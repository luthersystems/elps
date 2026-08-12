// Copyright © 2026 The ELPS authors

package main

import (
	"bytes"
	"context"
	"fmt"
	"runtime/debug"
	"time"

	lisp "github.com/luthersystems/elpsstock/lisp"
	lisplib "github.com/luthersystems/elpsstock/lisp/lisplib"
	parser "github.com/luthersystems/elpsstock/parser"
)

// evalStock runs src in the STOCK tree (origin/main) and reduces the result to
// a tree-independent Outcome.
//
// This file and eval_sealed.go are deliberate duplicates.  The two trees
// export the same API but as DIFFERENT Go types -- github.com/luthersystems/
// elps/lisp.LVal and github.com/luthersystems/elpsstock/lisp.LVal share no
// identity -- so no interface or generic can span them.  A copy per tree is
// the price of running both interpreters in one process, and it is a small
// price: any API drift between the trees shows up as a compile error here,
// which is itself a signal worth having.
func evalStock(src string) Outcome {
	var out Outcome
	out.Globals = map[string]string{}

	stderr := &bytes.Buffer{}
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env,
		lisp.WithStderr(stderr),
		lisp.WithMaxSteps(maxSteps),
		lisp.WithMaxTailIterations(maxTailIterations),
		lisp.WithMaximumPhysicalStackHeight(maxPhysicalHeight),
		lisp.WithMaxEvalNesting(maxEvalNesting),
		lisp.WithMaxAlloc(maxAlloc),
		lisp.WithMaxMacroExpansionDepth(maxMacroDepth),
	); rc.Type == lisp.LError {
		out.HardPanic = "InitializeUserEnv: " + rc.String()
		return out
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		out.HardPanic = "LoadLibrary: " + rc.String()
		return out
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		out.HardPanic = "InPackage: " + rc.String()
		return out
	}

	ctx, cancel := context.WithTimeout(context.Background(), evalDeadline)
	defer cancel()

	type done struct {
		v     *lisp.LVal
		steps int64
		pan   string
	}
	ch := make(chan done, 1)
	go func() {
		var d done
		defer func() {
			if r := recover(); r != nil {
				d.pan = fmt.Sprintf("%v\n%s", r, debug.Stack())
			}
			d.steps = env.Runtime.TotalSteps()
			ch <- d
		}()
		d.v = env.LoadStringContext(ctx, "prog", src)
	}()

	select {
	case d := <-ch:
		out.Steps = d.steps
		out.HardPanic = d.pan
		if d.pan != "" {
			return out
		}
		fillStock(&out, env, d.v)
	case <-time.After(watchdog):
		out.Timeout = true
		out.Starved = true
		return out
	}
	out.Stderr = normalize(stderr.String())
	return out
}

func fillStock(out *Outcome, env *lisp.LEnv, v *lisp.LVal) {
	defer func() {
		if r := recover(); r != nil {
			out.HardPanic = fmt.Sprintf("render: %v", r)
		}
	}()
	if v == nil {
		out.Type = "<nil>"
		return
	}
	out.Type = v.Type.String()
	out.Value = normalize(v.String())
	if v.Type == lisp.LError {
		out.IsError = true
		out.Cond = v.Str
		// A context-cancelled result is the deadline, i.e. wall clock.
		out.Starved = v.Str == "context-cancelled"
		out.InternalPanic = lisp.IsInternalPanic(v)
		if err := lisp.GoError(v); err != nil {
			out.Msg = normalize(err.Error())
		}
	}
	for _, name := range probeGlobals {
		out.Globals[name] = renderGlobalStock(env, name)
	}
}

func renderGlobalStock(env *lisp.LEnv, name string) (s string) {
	defer func() {
		if r := recover(); r != nil {
			s = fmt.Sprintf("<panic: %v>", r)
		}
	}()
	g := env.GetGlobal(lisp.Symbol(name))
	if g == nil {
		return "<nil>"
	}
	if g.Type == lisp.LError {
		return "<unbound>"
	}
	return normalize(g.String())
}
