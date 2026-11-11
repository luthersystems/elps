// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Establish the propagation contract independently of ErrorfAt: association,
// macroCall, evaluation and trace rendering must preserve a located error.
func TestReturnedErrorPreservesSubformLocation(t *testing.T) {
	for _, kind := range []string{"macro", "builtin"} {
		t.Run(kind, func(t *testing.T) {
			env := lisp.NewEnv(nil)
			require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env, lisp.WithReader(parser.NewReader()))))
			require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
			var returned *lisp.LVal
			def := elpsutil.FunctionDoc("fail-at", lisp.Formals("form"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				loc, ok := args.Cells[0].Source()
				require.True(t, ok)
				returned = lisp.Errorf("bad form: %s", args.Cells[0].Type)
				returned.SetSource(&loc)
				require.Nil(t, env.ErrorAssociate(returned))
				return returned
			}, "Reports an error at its argument.")
			if kind == "macro" {
				env.AddMacros(true, def)
			} else {
				env.AddBuiltins(true, def)
			}

			got := env.LoadString("located.lisp", "(progn\n (fail-at\n   42))")
			require.Equal(t, lisp.LError, got.Type)
			assert.Same(t, returned, got)
			const message = "located.lisp:3:4: fail-at: bad form: int"
			assert.Equal(t, message, lisp.GoError(got).Error())
			var trace bytes.Buffer
			_, err := (*lisp.ErrorVal)(got).WriteTrace(&trace)
			require.NoError(t, err)
			assert.Contains(t, trace.String(), message+"\nStack Trace [2 frames -- entrypoint last]:\n")
			assert.Contains(t, trace.String(), "height 1: located.lisp:2:2: user:fail-at")
			require.Nil(t, env.ErrorAssociate(got))
			require.NoError(t, lisp.GoError(env.LoadString("later.lisp", "(+ 1 2)")))
			assert.Equal(t, message, lisp.GoError(got).Error())
		})
	}
}

type errorfAtDebugger struct {
	entryRecordingDebugger
	enabled bool
	source  token.Location
}

func (d *errorfAtDebugger) IsEnabled() bool { return d.enabled }
func (d *errorfAtDebugger) OnError(env *lisp.LEnv, lerr *lisp.LVal) bool {
	d.source, _ = lerr.Source()
	return d.entryRecordingDebugger.OnError(env, lerr)
}

func TestEnvErrorfAt(t *testing.T) {
	for _, kind := range []string{"macro", "builtin"} {
		for _, target := range []string{"located", "nil", "unlocated", "synthetic"} {
			for _, debug := range []string{"none", "continue", "pause", "dormant"} {
				t.Run(kind+"/"+target+"/"+debug, func(t *testing.T) {
					env := lisp.NewEnv(nil)
					require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env, lisp.WithReader(parser.NewReader()))))
					require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
					def := elpsutil.FunctionDoc("fail-at", lisp.Formals("form"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
						form := args.Cells[0]
						switch target {
						case "nil":
							form = nil
						case "unlocated":
							form = lisp.Int(0)
						case "synthetic":
							form = lisp.Int(0)
							form.SetSource(&token.Location{File: "synthetic", Pos: -1})
						}
						callSite := env.Source()
						lerr := env.ErrorfAt(form, "bad form: %s", args.Cells[0].Type)
						assert.Equal(t, callSite, env.Source(), "ErrorfAt must not move the evaluator's location")
						return lerr
					}, "Reports an error at its argument.")
					if kind == "macro" {
						env.AddMacros(true, def)
					} else {
						env.AddBuiltins(true, def)
					}
					d := &errorfAtDebugger{enabled: debug == "continue" || debug == "pause"}
					d.pause = debug == "pause"
					if debug != "none" {
						env.Runtime.Debugger = d
					}
					got := env.LoadString("errorfat.lisp", "(progn\n (fail-at\n   42))")
					require.Equal(t, lisp.LError, got.Type)
					assert.Equal(t, "error", got.Str)
					line, col := 2, 2
					if target == "located" {
						line, col = 3, 4
					}
					assert.Equal(t, fmt.Sprintf("errorfat.lisp:%d:%d: fail-at: bad form: int", line, col), lisp.GoError(got).Error())
					if d.enabled {
						require.Len(t, d.observed, 1)
						assert.Same(t, got, d.observed[0])
						loc, ok := got.Source()
						require.True(t, ok)
						assert.Equal(t, loc, d.source, "OnError must see the final location")
					} else {
						assert.Empty(t, d.observed)
					}
					if d.pause {
						require.Len(t, d.waited, 1)
						assert.Same(t, got, d.waited[0])
					} else {
						assert.Empty(t, d.waited)
					}
				})
			}
		}
	}
}

func TestEnvErrorfAtCopiesLocation(t *testing.T) {
	env := lisp.NewEnv(nil)
	callSite := &token.Location{File: "call.lisp", Pos: 10, Line: 2, Col: 3}
	lisp.SetEnvLocForTest(env, callSite)
	for _, target := range []string{"located", "nil", "unlocated", "synthetic"} {
		t.Run(target, func(t *testing.T) {
			loc := &token.Location{File: "form.lisp", Pos: 0, Line: 1, Col: 1}
			form := lisp.Symbol("bad")
			form.SetSource(loc)
			want := *loc
			switch target {
			case "nil":
				form = nil
			case "unlocated":
				form.SetSource(nil)
			case "synthetic":
				loc.Pos = -1
			}
			if target != "located" {
				want = *callSite
			}
			got := env.ErrorfAt(form, "bad value: %d", 42)
			assert.NotSame(t, loc, lisp.SourceRefForTest(got))
			assert.NotSame(t, callSite, lisp.SourceRefForTest(got))
			loc.Line = 99
			oldLine := callSite.Line
			callSite.Line = 99
			actual, ok := got.Source()
			require.True(t, ok)
			assert.Equal(t, want, actual)
			callSite.Line = oldLine
		})
	}
	lisp.SetEnvLocForTest(env, nil)
	for _, form := range []*lisp.LVal{nil, lisp.Int(0)} {
		got := env.ErrorfAt(form, "bad value: %d", 42)
		_, ok := got.Source()
		assert.False(t, ok)
		assert.Equal(t, "<native code>: bad value: 42", lisp.GoError(got).Error())
	}
}
