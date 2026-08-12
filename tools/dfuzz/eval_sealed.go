// Copyright © 2026 The ELPS authors

package main

import (
	"bytes"
	"context"
	"fmt"
	"runtime/debug"
	"time"

	lisp "github.com/luthersystems/elps/lisp"
	lisplib "github.com/luthersystems/elps/lisp/lisplib"
	parser "github.com/luthersystems/elps/parser"
)

// evalSealed runs src in the SEALED tree (claude/exp-seal-tooling) and reduces the result to
// a tree-independent Outcome.
//
// See eval_stock.go for why this file is a deliberate duplicate of it: the
// two trees export the same API but as distinct Go types, so nothing can span
// them but a copy per tree.
func evalSealed(src string) Outcome {
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
		fillSealed(&out, env, d.v)
	case <-time.After(watchdog):
		out.Timeout = true
		return out
	}
	out.Stderr = normalize(stderr.String())
	return out
}

func fillSealed(out *Outcome, env *lisp.LEnv, v *lisp.LVal) {
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
		out.InternalPanic = lisp.IsInternalPanic(v)
		if err := lisp.GoError(v); err != nil {
			out.Msg = normalize(err.Error())
		}
	}
	for _, name := range probeGlobals {
		out.Globals[name] = renderGlobalSealed(env, name)
	}
}

func renderGlobalSealed(env *lisp.LEnv, name string) (s string) {
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
