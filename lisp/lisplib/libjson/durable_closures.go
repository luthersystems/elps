// Copyright © 2026 The ELPS authors

package libjson

// Closures: a lambda with no global name is saved with its code and the
// frames it captured.
//
//	["~#closure",["PKG",ENV,CODE]]
//	ENV  = null | ["~#env",[ENV,["NAME",VALUE,...]]]      (or ~#obj / ~#ref of one)
//	CODE = ["~#code",[SEALED,FORMALS,BODY...]]            (or ~#obj / ~#ref of one)
//
// PKG is the package the lambda was defined in: its body resolves globals
// there when it is called, as a function restored by ~#fn does.  ENV is the
// innermost captured frame, and each frame names its parent; null is the
// root environment, whose names are globals.  A frame holds, in name order,
// the bindings the saved closures over it read: each name a closure's code
// names (lexically, nested lambdas included) is saved in the innermost
// frame that binds it, and code that names eval keeps its frames whole.
// Discovery computes these sets as a fixpoint before anything is counted,
// so the counting pass, the output and the decoder all see the same
// frames.  Frames that save nothing are left out of the chain.  A frame is
// an object, so two closures over one frame share it after a load, and a
// set! through one is seen by the other.  A closure is an object too, so a
// closure in its own frame (recursion) restores.
//
// CODE is the lambda's formals and body as code: each node keeps whether it
// is quoted, which decides whether the evaluator evaluates it.  A node is a
// typed scalar (an int, float, string or symbol), null (the empty list), a
// list ["~#list",[NODE...]] (a call form), or ["~#quote",NODE] (NODE quoted,
// as the reader's ' quotes it).  SEALED is true when the code is the
// reader's sealed program text, which the load seals again.  Code is an
// object, so closures made by one lambda form share it.
//
// A load evaluates nothing: it builds the frames with NewEnv and Put, and
// the lambda with LEnv.RestoreLambda.  A restored closure keeps the code it
// was saved with; a named function (~#fn) is the current definition of its
// name.

import (
	"bytes"
	"cmp"
	"errors"
	"fmt"
	"reflect"
	"slices"
	"strconv"
	"strings"
	"unicode/utf8"

	"github.com/luthersystems/elps/lisp"
)

// The closure extension tags.
const (
	tagClosure = "~#closure"
	tagEnv     = "~#env"
	tagCode    = "~#code"
	tagQuote   = "~#quote"
)

// errAnonymous is funName's answer for a function no global binds.
var errAnonymous = errors.New("durable json: cannot encode an anonymous function")

type (
	// closureKey identifies a closure by its function data, which every
	// header of it (FunRef copies) shares.
	closureKey struct{ p any }
	// frameKey identifies a captured frame.
	frameKey struct{ p *lisp.LEnv }
	// codeKey identifies a lambda's code: its formals header and the
	// headers of its body forms, which every closure made by one lambda
	// form shares.
	codeKey struct {
		formals *lisp.LVal
		body    string
	}
)

// binding is one saved frame binding.
type binding struct {
	value *lisp.LVal
	name  string
}

func codeKeyOf(f *lisp.LVal) codeKey {
	b := make([]byte, 0, 8*len(f.Cells))
	for _, c := range f.Cells[1:] {
		b = strconv.AppendUint(b, uint64(reflect.ValueOf(c).Pointer()), 16)
		b = append(b, ' ')
	}
	return codeKey{formals: f.Cells[0], body: string(b)}
}

// frameOf returns the innermost frame at or above env that saves a
// binding, or nil when there is none below the root.
func (e *durableEncoder) frameOf(env *lisp.LEnv) *lisp.LEnv {
	for ; env != nil && env.Parent() != nil; env = env.Parent() {
		if len(e.marked[env]) > 0 {
			return env
		}
	}
	return nil
}

// frameAll returns all of a frame's bindings by name, read once per dump.
func (e *durableEncoder) frameAll(env *lisp.LEnv) map[string]*lisp.LVal {
	if all, ok := e.frameVals[env]; ok {
		return all
	}
	all := make(map[string]*lisp.LVal, env.NumBindings())
	for name, v := range env.Bindings() {
		all[name] = v
	}
	e.frameVals[env] = all
	return all
}

// bindings returns the bindings a frame saves, in name order.
func (e *durableEncoder) bindings(env *lisp.LEnv) []binding {
	if bs, ok := e.frameBindings[env]; ok {
		return bs
	}
	bs := make([]binding, 0, len(e.marked[env]))
	for name := range e.marked[env] {
		bs = append(bs, binding{name: name, value: e.frameVals[env][name]})
	}
	slices.SortFunc(bs, func(a, b binding) int { return cmp.Compare(a.name, b.name) })
	e.frameBindings[env] = bs
	return bs
}

// freeNames is what a closure's code may read from its frames.
type freeNames struct {
	names   []string
	dynamic bool
}

// codeNames returns the names a closure's code may read lexically: every
// unqualified, non-keyword symbol anywhere in its formals and body,
// quoted or not, in nested lambdas too.  This over-approximates the free
// variables, which keeps it sound for any special form or macro call that
// names a variable in its arguments.  Code that names eval is dynamic: it
// may evaluate any symbol in the frames, so it keeps them whole.
func (e *durableEncoder) codeNames(f *lisp.LVal) freeNames {
	key := codeKeyOf(f)
	if fn, ok := e.codeFree[key]; ok {
		return fn
	}
	seen := map[string]bool{}
	var fn freeNames
	var walk func(v *lisp.LVal)
	walk = func(v *lisp.LVal) {
		switch v.Type {
		case lisp.LSymbol:
			name := v.Str
			if name == "eval" || name == "lisp:eval" {
				fn.dynamic = true
			}
			if name != "" && !strings.Contains(name, ":") && !seen[name] {
				seen[name] = true
				fn.names = append(fn.names, name)
			}
		case lisp.LSExpr, lisp.LQuote:
			for _, c := range v.Cells {
				walk(c)
			}
		case lisp.LInt, lisp.LFloat, lisp.LString, lisp.LBytes, lisp.LArray, lisp.LSortMap, lisp.LTaggedVal,
			lisp.LNative, lisp.LFun, lisp.LError, lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand,
			lisp.LInvalid, lisp.LTypeMax:
		}
	}
	for _, c := range f.Cells {
		walk(c)
	}
	slices.Sort(fn.names)
	e.codeFree[key] = fn
	return fn
}

// markClosure records, during discovery, the frame bindings a closure
// reads: each name its code names, in the innermost frame below the root
// that binds it, or every binding of every frame for dynamic code.  A
// binding marked for the first time has its value walked, so the closures
// it reaches mark theirs: the marks reach a fixpoint over the closures and
// frames by the time discovery ends.
func (e *durableEncoder) markClosure(v *lisp.LVal, depth int) error {
	fn := e.codeNames(v)
	var chain []*lisp.LEnv
	for env := v.LambdaEnv(); env != nil && env.Parent() != nil; env = env.Parent() {
		chain = append(chain, env)
	}
	mark := func(env *lisp.LEnv, name string, value *lisp.LVal) error {
		if e.marked[env][name] {
			return nil
		}
		if e.marked[env] == nil {
			e.marked[env] = map[string]bool{}
		}
		if name == "" || !utf8.ValidString(name) {
			return errors.New("durable json: a captured variable's name is empty or not UTF-8")
		}
		e.marked[env][name] = true
		if err := e.scan(value, depth+1); err != nil {
			return capturedError(name, err)
		}
		return nil
	}
	if fn.dynamic {
		for _, env := range chain {
			all := e.frameAll(env)
			names := make([]string, 0, len(all))
			for name := range all {
				names = append(names, name)
			}
			slices.Sort(names)
			for _, name := range names {
				if err := mark(env, name, all[name]); err != nil {
					return err
				}
			}
		}
		return nil
	}
	for _, name := range fn.names {
		for _, env := range chain {
			if value, ok := e.frameAll(env)[name]; ok {
				if err := mark(env, name, value); err != nil {
					return err
				}
				break
			}
		}
	}
	return nil
}

// scanFunValue resolves a function: a global name, or a closure.
func (e *durableEncoder) scanFunValue(v *lisp.LVal, depth int) error {
	k := funKey{v.Package(), v.FID()}
	name, ok := e.funs[k]
	if !ok {
		var err error
		name, err = e.funName(v)
		switch {
		case errors.Is(err, errAnonymous) && v.LambdaEnv() != nil:
			name = ""
		case err != nil:
			return err
		}
		e.funs[k] = name
	}
	if name != "" {
		return nil
	}
	return e.scanClosure(v, depth)
}

// scanClosure visits a closure: its frames and its code.
func (e *durableEncoder) scanClosure(v *lisp.LVal, depth int) error {
	key := closureKey{v.Native}
	e.refs[key]++
	if e.refs[key] > 1 {
		return e.revisit(nil, key)
	}
	if pkg := v.Package(); pkg == "" || !utf8.ValidString(pkg) || e.env.Runtime.Registry.Package(pkg) == nil {
		return errAnonymous
	}
	if err := e.scanDepth(depth); err != nil {
		return err
	}
	i := e.openObject(key)
	// Discovery checks the code (its size and depth) before it reads the
	// names in it.
	if e.discover {
		if err := e.scanCode(v, depth+1); err != nil {
			return err
		}
		if err := e.markClosure(v, depth); err != nil {
			return err
		}
	} else {
		if err := e.scanFrame(e.frameOf(v.LambdaEnv()), depth+1); err != nil {
			return err
		}
		if err := e.scanCode(v, depth+1); err != nil {
			return err
		}
	}
	e.closeNode(i)
	return nil
}

// scanFrame visits a frame (nil is the root): its parent and its
// bindings.  A refusal names the captured variable.
func (e *durableEncoder) scanFrame(env *lisp.LEnv, depth int) error {
	if err := e.countScan(1); err != nil {
		return err
	}
	if env == nil {
		return nil
	}
	key := frameKey{env}
	e.refs[key]++
	if e.refs[key] > 1 {
		return e.revisit(nil, key)
	}
	if err := e.scanDepth(depth); err != nil {
		return err
	}
	bs := e.bindings(env)
	i := e.openObject(key)
	if err := e.scanFrame(e.frameOf(env.Parent()), depth+1); err != nil {
		return err
	}
	for _, b := range bs {
		if err := e.countScan(1); err != nil {
			return err
		}
		if err := e.scan(b.value, depth+1); err != nil {
			return capturedError(b.name, err)
		}
	}
	e.closeNode(i)
	return nil
}

// capturedError puts a captured variable's name in front of a refusal, so
// the error gives the path to the refused value.
func capturedError(name string, err error) error {
	if errors.Is(err, ErrTypedLimit) {
		return err
	}
	return fmt.Errorf("durable json: captured variable %q: %s", name, strings.TrimPrefix(err.Error(), "durable json: "))
}

// scanCode visits a closure's code once per code object.
func (e *durableEncoder) scanCode(f *lisp.LVal, depth int) error {
	if err := e.countScan(1); err != nil {
		return err
	}
	key := codeKeyOf(f)
	e.refs[key]++
	if e.refs[key] > 1 {
		return e.revisit(nil, key)
	}
	if err := e.scanDepth(depth); err != nil {
		return err
	}
	i := e.openObject(key)
	for _, c := range f.Cells {
		if err := e.scanCodeNode(c, depth+1); err != nil {
			return err
		}
	}
	e.closeNode(i)
	return nil
}

// codeSealed reports whether a closure's code is sealed program text.
func codeSealed(f *lisp.LVal) bool {
	for _, c := range f.Cells {
		if !c.IsSealed() {
			return false
		}
	}
	return true
}

// scanCodeNode checks and counts one code node, as codeNode writes it.
func (e *durableEncoder) scanCodeNode(v *lisp.LVal, depth int) error {
	if v == nil {
		return errors.New("durable json: cannot encode a Go nil value")
	}
	switch {
	case v.Type == lisp.LQuote:
		if !v.IsQuoted() || len(v.Cells) != 1 || v.Cells[0] == nil || !v.Cells[0].IsQuoted() {
			return errors.New("durable json: a closure's code holds a malformed quote")
		}
		if err := e.countScan(1); err != nil {
			return err
		}
		if err := e.scanDepth(depth); err != nil {
			return err
		}
		return e.scanCodeNode(v.Cells[0], depth+1)
	case v.IsQuoted():
		if err := e.countScan(1); err != nil {
			return err
		}
		if err := e.scanDepth(depth); err != nil {
			return err
		}
		return e.scanCodeBody(v, depth+1)
	}
	return e.scanCodeBody(v, depth)
}

// scanCodeBody checks and counts a code node without its quote.
func (e *durableEncoder) scanCodeBody(v *lisp.LVal, depth int) error {
	if err := e.countScan(1); err != nil {
		return err
	}
	switch v.Type {
	case lisp.LInt, lisp.LFloat, lisp.LString, lisp.LSymbol:
		return nil
	case lisp.LSExpr:
		if len(v.Cells) == 0 {
			return nil
		}
		if err := e.scanDepth(depth); err != nil {
			return err
		}
		for _, c := range v.Cells {
			if err := e.scanCodeNode(c, depth+1); err != nil {
				return err
			}
		}
		return nil
	case lisp.LQuote, lisp.LArray, lisp.LSortMap, lisp.LBytes, lisp.LTaggedVal, lisp.LNative, lisp.LFun, lisp.LError,
		lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand, lisp.LInvalid, lisp.LTypeMax:
	}
	return fmt.Errorf("durable json: a closure's code holds a %v", v.Type)
}

// closure writes ["~#closure",["PKG",ENV,CODE]], shared or not.
func (e *durableEncoder) closure(v *lisp.LVal, depth int) error {
	key := closureKey{v.Native}
	body := func() error {
		if err := e.container(depth); err != nil {
			return err
		}
		pkg := v.Package()
		if err := e.reserve(jsonStringLen(pkg) + len(tagClosure) + 8); err != nil {
			return err
		}
		e.buf = append(e.buf, `["`+tagClosure+`",[`...)
		e.buf = appendJSONString(e.buf, pkg)
		e.buf = append(e.buf, ',')
		if err := e.frame(e.frameOf(v.LambdaEnv()), depth+1); err != nil {
			return err
		}
		e.buf = append(e.buf, ',')
		if err := e.code(v, depth+1); err != nil {
			return err
		}
		e.buf = append(e.buf, ']', ']')
		return e.grow()
	}
	if e.refs[key] >= 2 {
		return e.object(key, depth, body)
	}
	return body()
}

// frame writes a frame, or null for the root.
func (e *durableEncoder) frame(env *lisp.LEnv, depth int) error {
	if env == nil {
		return e.typedEncoder.value(lisp.Nil(), depth)
	}
	key := frameKey{env}
	body := func() error {
		if err := e.container(depth); err != nil {
			return err
		}
		e.buf = append(e.buf, `["`+tagEnv+`",[`...)
		if err := e.frame(e.frameOf(env.Parent()), depth+1); err != nil {
			return err
		}
		e.buf = append(e.buf, ',', '[')
		for k, b := range e.frameBindings[env] {
			if k > 0 {
				e.buf = append(e.buf, ',')
			}
			if err := e.count(); err != nil {
				return err
			}
			if err := e.reserve(jsonStringLen(b.name) + 1); err != nil {
				return err
			}
			e.buf = appendJSONString(e.buf, b.name)
			e.buf = append(e.buf, ',')
			if err := e.value(b.value, depth+1); err != nil {
				return err
			}
		}
		e.buf = append(e.buf, ']', ']', ']')
		return e.grow()
	}
	if e.refs[key] >= 2 {
		return e.object(key, depth, body)
	}
	return body()
}

// code writes ["~#code",[SEALED,FORMALS,BODY...]], shared or not.
func (e *durableEncoder) code(f *lisp.LVal, depth int) error {
	key := codeKeyOf(f)
	body := func() error {
		if err := e.container(depth); err != nil {
			return err
		}
		e.buf = append(e.buf, `["`+tagCode+`",[`...)
		if codeSealed(f) {
			e.buf = append(e.buf, "true"...)
		} else {
			e.buf = append(e.buf, "false"...)
		}
		for _, c := range f.Cells {
			e.buf = append(e.buf, ',')
			if err := e.codeNode(c, depth+1); err != nil {
				return err
			}
		}
		e.buf = append(e.buf, ']', ']')
		return e.grow()
	}
	if e.refs[key] >= 2 {
		return e.object(key, depth, body)
	}
	return body()
}

// codeNode writes one code node: ["~#quote",X] for a quoted node, where X
// is the node it quotes, else the node itself.
func (e *durableEncoder) codeNode(v *lisp.LVal, depth int) error {
	if v.Type != lisp.LQuote && !v.IsQuoted() {
		return e.codeBody(v, depth)
	}
	if err := e.container(depth); err != nil {
		return err
	}
	e.buf = append(e.buf, `["`+tagQuote+`",`...)
	var err error
	if v.Type == lisp.LQuote {
		err = e.codeNode(v.Cells[0], depth+1)
	} else {
		err = e.codeBody(v, depth+1)
	}
	if err != nil {
		return err
	}
	e.buf = append(e.buf, ']')
	return e.grow()
}

// codeBody writes a code node without its quote.
func (e *durableEncoder) codeBody(v *lisp.LVal, depth int) error {
	if v.Type != lisp.LSExpr {
		return e.typedEncoder.value(v, depth)
	}
	if len(v.Cells) == 0 {
		return e.typedEncoder.value(lisp.Nil(), depth)
	}
	if err := e.container(depth); err != nil {
		return err
	}
	e.buf = append(e.buf, `["`+tagList+`",[`...)
	for k, c := range v.Cells {
		if k > 0 {
			e.buf = append(e.buf, ',')
		}
		if err := e.codeNode(c, depth+1); err != nil {
			return err
		}
	}
	e.buf = append(e.buf, ']', ']')
	return e.grow()
}

// Decoding.

// closureValue reads ["PKG",ENV,CODE] after "~#closure",[ and rebuilds the
// closure.  Its header is defined first, so a captured frame can refer to
// it.
func (d *durableDecoder) closureValue(depth int) (*lisp.LVal, error) {
	h := &lisp.LVal{}
	d.define(h)
	s, err := d.rawString()
	if err != nil {
		return nil, err
	}
	pkg := string(s)
	if d.env.Runtime.Registry.Package(pkg) == nil {
		return nil, d.errorf("closure of unknown package %q", pkg)
	}
	if err = d.expect(','); err != nil {
		return nil, err
	}
	env, err := d.frameRef(depth + 1)
	if err != nil {
		return nil, err
	}
	if err = d.expect(','); err != nil {
		return nil, err
	}
	cells, err := d.codeRef(depth + 1)
	if err != nil {
		return nil, err
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	f := env.RestoreLambda(pkg, cells[0], cells[1:])
	if f.Type == lisp.LError {
		return nil, d.errorf("closure: %v", (*lisp.ErrorVal)(f).ErrorMessage())
	}
	*h = *f
	return h, nil
}

// root returns the root of the loading environment.
func (d *durableDecoder) root() *lisp.LEnv {
	env := d.env
	for env.Parent() != nil {
		env = env.Parent()
	}
	return env
}

// frameRef reads a frame position: null (the root), a frame, or a shared
// one.
func (d *durableDecoder) frameRef(depth int) (*lisp.LEnv, error) {
	if bytes.HasPrefix(d.b[d.i:], []byte("null")) {
		if err := d.count(); err != nil {
			return nil, err
		}
		d.i += 4
		return d.root(), nil
	}
	if !bytes.HasPrefix(d.b[d.i:], []byte(`["~#`)) {
		return nil, d.errorf("a closure's frame must be null, a frame or a shared one")
	}
	d.pos = posFrame
	v, err := d.value(depth)
	d.pos = posValue
	if err != nil {
		return nil, err
	}
	env, ok := v.Native.(*lisp.LEnv)
	if !ok || v.Type != lisp.LNative || env == nil {
		return nil, d.errorf("a frame that is its own ancestor")
	}
	return env, nil
}

// frameValue reads [PARENT,["NAME",VALUE,...]] after "~#env",[.  The frame
// is defined before its bindings are read, so a binding can refer to a
// closure over it.
func (d *durableDecoder) frameValue(depth int) (*lisp.LVal, error) {
	ph := &lisp.LVal{Type: lisp.LNative}
	d.define(ph)
	parent, err := d.frameRef(depth + 1)
	if err != nil {
		return nil, err
	}
	env := lisp.NewEnv(parent)
	ph.Native = env //elpsvet:allow-native the decoder's own placeholder for a frame object: it lives only in d.objs and is unwrapped by frameRef, so it never reaches a value LoadDurable returns, let alone a template
	if err = d.expect(','); err != nil {
		return nil, err
	}
	if err = d.expect('['); err != nil {
		return nil, err
	}
	var prev []byte
	for first := true; ; first = false {
		if d.peek() == ']' {
			if first {
				return nil, d.errorf("a frame with no bindings")
			}
			d.i++
			break
		}
		if !first {
			if err := d.expect(','); err != nil {
				return nil, err
			}
		}
		if err := d.count(); err != nil {
			return nil, err
		}
		name, err := d.rawString()
		if err != nil {
			return nil, err
		}
		if len(name) == 0 || !utf8.Valid(name) {
			return nil, d.errorf("a captured variable's name is empty or not UTF-8")
		}
		if prev != nil && bytes.Compare(prev, name) >= 0 {
			return nil, d.errorf("captured variables out of order or duplicated")
		}
		prev = append(prev[:0], name...)
		sym := lisp.Symbol(string(name))
		if err = d.expect(','); err != nil {
			return nil, err
		}
		v, err := d.value(depth + 1)
		if err != nil {
			return nil, err
		}
		if r := env.Put(sym, v); r.Type == lisp.LError {
			return nil, d.errorf("captured variable %q: %v", sym.Str, (*lisp.ErrorVal)(r).ErrorMessage())
		}
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	return ph, nil
}

// codeRef reads a code position: code, or shared code.  It returns the
// lambda's cells: formals, then body.
func (d *durableDecoder) codeRef(depth int) ([]*lisp.LVal, error) {
	if !bytes.HasPrefix(d.b[d.i:], []byte(`["~#`)) {
		return nil, d.errorf("a closure's code must be code or shared code")
	}
	d.pos = posCode
	v, err := d.value(depth)
	d.pos = posValue
	if err != nil {
		return nil, err
	}
	return v.Cells, nil
}

// codeValue reads [SEALED,FORMALS,BODY...] after "~#code",[.
func (d *durableDecoder) codeValue(depth int) (*lisp.LVal, error) {
	ph := lisp.QExpr(nil)
	d.define(ph)
	var sealed bool
	switch {
	case bytes.HasPrefix(d.b[d.i:], []byte("true,")):
		sealed = true
		d.i += 5
	case bytes.HasPrefix(d.b[d.i:], []byte("false,")):
		d.i += 6
	default:
		return nil, d.errorf("code must start with true or false and its formals")
	}
	var cells []*lisp.LVal
	for {
		c, err := d.codeNode(depth + 1)
		if err != nil {
			return nil, err
		}
		cells = append(cells, c)
		if d.peek() != ',' {
			break
		}
		d.i++
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	if f := cells[0]; f.Type != lisp.LSExpr || f.IsQuoted() {
		return nil, d.errorf("code formals must be a list")
	}
	if sealed {
		for _, c := range cells {
			c.SealAST()
		}
	} else if codeSealed(&lisp.LVal{Cells: cells}) {
		return nil, d.errorf("unsealed code that seals")
	}
	ph.Cells = cells
	return ph, nil
}

// codeNode reads one code node: a scalar, null, a list or a quote.
func (d *durableDecoder) codeNode(depth int) (*lisp.LVal, error) {
	switch {
	case bytes.HasPrefix(d.b[d.i:], []byte("null")):
		if err := d.count(); err != nil {
			return nil, err
		}
		d.i += 4
		return lisp.SExpr(nil), nil
	case bytes.HasPrefix(d.b[d.i:], []byte(`["`+tagQuote+`",`)):
		if err := d.count(); err != nil {
			return nil, err
		}
		if err := d.depth(depth); err != nil {
			return nil, err
		}
		d.i += len(tagQuote) + 4
		inner, err := d.codeNode(depth + 1)
		if err != nil {
			return nil, err
		}
		if err := d.expect(']'); err != nil {
			return nil, err
		}
		return lisp.Quote(inner), nil
	case bytes.HasPrefix(d.b[d.i:], []byte(`["`+tagList+`",[`)):
		if err := d.count(); err != nil {
			return nil, err
		}
		if err := d.depth(depth); err != nil {
			return nil, err
		}
		d.i += len(tagList) + 5
		var cells []*lisp.LVal
		for {
			c, err := d.codeNode(depth + 1)
			if err != nil {
				return nil, err
			}
			cells = append(cells, c)
			if d.peek() != ',' {
				break
			}
			d.i++
		}
		if err := d.expect(']'); err != nil {
			return nil, err
		}
		if err := d.expect(']'); err != nil {
			return nil, err
		}
		return lisp.SExpr(cells), nil
	case d.peek() == '[' || d.peek() == '{':
		return nil, d.errorf("code may hold only scalars, lists and quotes")
	}
	v, err := d.typedDecoder.value(depth)
	if err != nil {
		return nil, err
	}
	switch v.Type {
	case lisp.LInt, lisp.LFloat, lisp.LString, lisp.LSymbol:
		return v, nil
	case lisp.LSExpr, lisp.LArray, lisp.LSortMap, lisp.LBytes, lisp.LTaggedVal, lisp.LNative, lisp.LFun, lisp.LError, lisp.LQuote,
		lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand, lisp.LInvalid, lisp.LTypeMax:
	}
	return nil, d.errorf("code may hold only scalars, lists and quotes")
}
