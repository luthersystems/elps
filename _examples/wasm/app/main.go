// Copyright © 2018 The ELPS authors

package main

import (
	"fmt"
	"log"
	"os"
	"syscall/js"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

var env *lisp.LEnv

func LoadString(s string) *lisp.LVal {
	return env.LoadString("source", s)
}

func JSLoadString(vals []js.Value) {
	if len(vals) != 2 {
		log.Printf("invalid number argument: %v", len(vals))
		return
	}
	source := vals[0].String()
	callback := vals[1]
	lval := LoadString(source)
	out := lval.String()
	if lval.Type == lisp.LError {
		callback.Call("reject", js.ValueOf(out))
	} else {
		callback.Call("resolve", js.ValueOf(out))
	}
}

func main() {
	env = lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	rc := lisp.InitializeUserEnv(env)
	if !rc.IsNil() {
		fmt.Fprintln(os.Stderr, rc)
		os.Exit(1)
	}
	rc = lisplib.LoadLibrary(env)
	if !rc.IsNil() {
		fmt.Fprintln(os.Stderr, rc)
		os.Exit(1)
	}
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	if !rc.IsNil() {
		fmt.Fprintln(os.Stderr, rc)
		os.Exit(1)
	}

	// hook up "exported" functions

	done := make(chan struct{})
	var exportCallbacks []js.Callback
	export := func(name string, fn func([]js.Value)) js.Callback {
		cb := js.NewCallback(fn)
		js.Global().Set(name, cb)
		exportCallbacks = append(exportCallbacks, cb)
		return cb
	}
	export("LoadString", JSLoadString)
	export("GoKill", func([]js.Value) {
		close(done)
	})

	// initialization complete

	expr := lisp.SExpr([]*lisp.LVal{lisp.Symbol("debug-print"), lisp.String("hello wasm!?")})
	result := env.Eval(expr)
	fmt.Println(result)

	<-done
	for i := range exportCallbacks {
		exportCallbacks[i].Release()
	}
	exportCallbacks = nil
}
