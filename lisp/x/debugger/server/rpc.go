package server

import (
	"bytes"
	"encoding/json"
	"fmt"
	"github.com/go-delve/delve/pkg/logflags"
	"github.com/sirupsen/logrus"
	"io"
	"net/rpc"
	"net/rpc/jsonrpc"
	"reflect"
	"runtime"
	"sync"
	"unicode"
	"unicode/utf8"
)

func (s *RPCServer) Run() error {
	go func() {
		defer s.listener.Close()
		for {
			c, err := s.listener.Accept()
			if err != nil {
				s.log.Errorf("Error starting: %v", err)
				select {
				case <-s.stopChan:
					// We were supposed to exit, do nothing and return
					return
				default:
					logrus.Errorf("%s", err.Error())
					return
				}
			}

			go s.serveJSONCodec(c)
		}
	}()
	return nil
}

func suitableMethods(rcvr interface{}, methods map[string]*methodType, log *logrus.Entry) {
	typ := reflect.TypeOf(rcvr)
	rcvrv := reflect.ValueOf(rcvr)
	sname := reflect.Indirect(rcvrv).Type().Name()
	if sname == "" {
		log.Debugf("rpc.Register: no service name for type %s", typ)
		return
	}
	for m := 0; m < typ.NumMethod(); m++ {
		method := typ.Method(m)
		mname := method.Name
		mtype := method.Type
		// method must be exported
		if method.PkgPath != "" {
			continue
		}
		// Method needs three ins: (receive, *args, *reply) or (receiver, *args, *RPCCallback)
		if mtype.NumIn() != 3 {
			log.Warn("method", mname, "has wrong number of ins:", mtype.NumIn())
			continue
		}
		// First arg need not be a pointer.
		argType := mtype.In(1)
		if !isExportedOrBuiltinType(argType) {
			log.Warn(mname, "argument type not exported:", argType)
			continue
		}

		replyType := mtype.In(2)
		synchronous := replyType.String() != "service.RPCCallback"

		if synchronous {
			// Second arg must be a pointer.
			if replyType.Kind() != reflect.Ptr {
				log.Warn("method", mname, "reply type not a pointer:", replyType)
				continue
			}
			// Reply type must be exported.
			if !isExportedOrBuiltinType(replyType) {
				log.Warn("method", mname, "reply type not exported:", replyType)
				continue
			}

			// Method needs one out.
			if mtype.NumOut() != 1 {
				log.Warn("method", mname, "has wrong number of outs:", mtype.NumOut())
				continue
			}
			// The return type of the method must be error.
			if returnType := mtype.Out(0); returnType != typeOfError {
				log.Warn("method", mname, "returns", returnType.String(), "not error")
				continue
			}
		} else {
			// Method needs zero outs.
			if mtype.NumOut() != 0 {
				log.Warn("method", mname, "has wrong number of outs:", mtype.NumOut())
				continue
			}
		}
		methods[sname+"."+mname] = &methodType{method: method, ArgType: argType, ReplyType: replyType, Synchronous: synchronous, Rcvr: rcvrv}
	}
}

type methodType struct {
	method      reflect.Method
	Rcvr        reflect.Value
	ArgType     reflect.Type
	ReplyType   reflect.Type
	Synchronous bool
}

var typeOfError = reflect.TypeOf((*error)(nil)).Elem()

func isExportedOrBuiltinType(t reflect.Type) bool {
	for t.Kind() == reflect.Ptr {
		t = t.Elem()
	}
	// PkgPath will be non-empty even for an exported type,
	// so we need to check the type name as well.
	return isExported(t.Name()) || t.PkgPath() == ""
}

func isExported(name string) bool {
	rune, _ := utf8.DecodeRuneInString(name)
	return unicode.IsUpper(rune)
}

var invalidRequest = struct{}{}

func (s *RPCServer) sendResponse(sending *sync.Mutex, req *rpc.Request, resp *rpc.Response, reply interface{}, codec rpc.ServerCodec, errmsg string) {
	resp.ServiceMethod = req.ServiceMethod
	if errmsg != "" {
		resp.Error = errmsg
		reply = invalidRequest
	}
	resp.Seq = req.Seq
	sending.Lock()
	defer sending.Unlock()
	err := codec.WriteResponse(resp, reply)
	if err != nil {
		s.log.Error("writing response:", err)
	}
}

type internalError struct {
	Err   interface{}
	Stack []internalErrorFrame
}

type internalErrorFrame struct {
	Pc   uintptr
	Func string
	File string
	Line int
}

func newInternalError(ierr interface{}, skip int) *internalError {
	r := &internalError{ierr, nil}
	for i := skip; ; i++ {
		pc, file, line, ok := runtime.Caller(i)
		if !ok {
			break
		}
		fname := "<unknown>"
		fn := runtime.FuncForPC(pc)
		if fn != nil {
			fname = fn.Name()
		}
		r.Stack = append(r.Stack, internalErrorFrame{pc, fname, file, line})
	}
	return r
}

func (err *internalError) Error() string {
	var out bytes.Buffer
	fmt.Fprintf(&out, "Internal debugger error: %v\n", err.Err)
	for _, frame := range err.Stack {
		fmt.Fprintf(&out, "%s (%#x)\n\t%s:%d\n", frame.Func, frame.Pc, frame.File, frame.Line)
	}
	return out.String()
}

func (s *RPCServer) serveJSONCodec(conn io.ReadWriteCloser) {
	sending := new(sync.Mutex)
	codec := jsonrpc.NewServerCodec(conn)
	var req rpc.Request
	var resp rpc.Response
	for {
		req = rpc.Request{}
		err := codec.ReadRequestHeader(&req)
		if err != nil {
			if err != io.EOF {
				s.log.Error("rpc:", err)
			}
			break
		}

		mtype, ok := s.methods[req.ServiceMethod]
		if !ok {
			s.log.Errorf("rpc: can't find method %s", req.ServiceMethod)
			s.sendResponse(sending, &req, &rpc.Response{}, nil, codec, fmt.Sprintf("unknown method: %s", req.ServiceMethod))
			continue
		}

		var argv, replyv reflect.Value

		// Decode the argument value.
		argIsValue := false // if true, need to indirect before calling.
		if mtype.ArgType.Kind() == reflect.Ptr {
			argv = reflect.New(mtype.ArgType.Elem())
		} else {
			argv = reflect.New(mtype.ArgType)
			argIsValue = true
		}
		// argv guaranteed to be a pointer now.
		if err = codec.ReadRequestBody(argv.Interface()); err != nil {
			return
		}
		if argIsValue {
			argv = argv.Elem()
		}
		s.log.Infof("Received req for %s with arg %v", mtype.method.Name, argv)
		if mtype.Synchronous {
			if logflags.RPC() {
				argvbytes, _ := json.Marshal(argv.Interface())
				s.log.Debugf("<- %s(%T%s)", req.ServiceMethod, argv.Interface(), argvbytes)
			}
			replyv = reflect.New(mtype.ReplyType.Elem())
			function := mtype.method.Func
			var returnValues []reflect.Value
			var errInter interface{}
			func() {
				defer func() {
					if ierr := recover(); ierr != nil {
						errInter = newInternalError(ierr, 2)
					}
				}()
				returnValues = function.Call([]reflect.Value{mtype.Rcvr, argv, replyv})
				errInter = returnValues[0].Interface()
			}()

			errmsg := ""
			if errInter != nil {
				errmsg = errInter.(error).Error()
			}
			resp = rpc.Response{}
			if logflags.RPC() {
				replyvbytes, _ := json.Marshal(replyv.Interface())
				s.log.Debugf("-> %T%s error: %q", replyv.Interface(), replyvbytes, errmsg)
			}
			s.sendResponse(sending, &req, &resp, replyv.Interface(), codec, errmsg)
		} else {
			if logflags.RPC() {
				argvbytes, _ := json.Marshal(argv.Interface())
				s.log.Debugf("(async %d) <- %s(%T%s)", req.Seq, req.ServiceMethod, argv.Interface(), argvbytes)
			}
			function := mtype.method.Func
			ctl := &RPCCallback{s, sending, codec, req}
			go func() {
				defer func() {
					if ierr := recover(); ierr != nil {
						ctl.Return(nil, newInternalError(ierr, 2))
					}
				}()
				function.Call([]reflect.Value{mtype.Rcvr, argv, reflect.ValueOf(ctl)})
			}()
		}
	}
	codec.Close()
}

func (cb *RPCCallback) Return(out interface{}, err error) {
	errmsg := ""
	if err != nil {
		errmsg = err.Error()
	}
	var resp rpc.Response
	if logflags.RPC() {
		outbytes, _ := json.Marshal(out)
		cb.s.log.Debugf("(async %d) -> %T%s error: %q", cb.req.Seq, out, outbytes, errmsg)
	}
	cb.s.sendResponse(cb.sending, &cb.req, &resp, out, cb.codec, errmsg)
}

type RPCCallback struct {
	s       *RPCServer
	sending *sync.Mutex
	codec   rpc.ServerCodec
	req     rpc.Request
}

// Stop stops the JSON-RPC server.
func (s *RPCServer) Stop() error {
	if s.stopChan != nil {
		close(s.stopChan)
		s.stopChan = nil
	}
	s.listener.Close()
	return s.debugger.Complete()
}
