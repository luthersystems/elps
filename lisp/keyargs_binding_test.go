// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
)

// keyArgsBindingCases pin how &key formals bind (luthersystems/elps#992).
// Each formals string holds the text after &key. want is the result, or the
// error text when isErr is true.
var keyArgsBindingCases = []struct {
	formals, body, call, want string
	isErr                     bool
}{
	{"a b", "(list a b)", ":a 1 :b 2", "'(1 2)", false},
	{"a b", "(list a b)", ":b 2 :a 1", "'(1 2)", false},
	{"a b", "(list a b)", ":b 2", "'(() 2)", false},
	{"a", "a", ":a 1 :a 2", "2", false},
	{"a", "a", ":a :b", ":b", false},
	{"a b c d", "(list a b c d)", ":d 4 :c 3 :b 2 :a 1 :c 5", "'(1 2 5 4)", false},
	{"a", "a", ":z 1 :y 2", "unrecognized keyword argument: z", true},
	{"a", "a", ":a 1 :z 2 :y 3", "unrecognized keyword argument: z", true},
	{"a", "a", ":a 1 :a 2 :y 3", "unrecognized keyword argument: y", true},
	{"a", "a", ":z 1 2 3", "argument is not a keyword: int", true},
	{"a", "a", ":z 1 'b 3", "argument is not a keyword: b", true},
	{"a", "a", ":a", "odd number of keyword arguments", true},
	{"a &rest b", "a", ":a 1", "control symbol at an invalid location: &key", true},
	{"a &rest b", "a", ":z 1", "control symbol at an invalid location: &key", true},
}

// keyArgsPadding returns n &key formals that no case passes.
func keyArgsPadding(n int) string {
	parts := make([]string, n)
	for i := range parts {
		parts[i] = fmt.Sprintf("pad%02d", i)
	}
	return strings.Join(parts, " ")
}

// TestKeyArgsBinding checks each case with few &key formals and again with
// 17 more. The two sizes take different code paths in bindFormalNext and
// must give the same result.
func TestKeyArgsBinding(t *testing.T) {
	env := keyArgsEnv(t)
	for _, pad := range []int{0, 17} {
		for _, tc := range keyArgsBindingCases {
			formals := strings.TrimSpace(keyArgsPadding(pad) + " " + tc.formals)
			src := fmt.Sprintf("((lambda (&key %s) %s) %s)", formals, tc.body, tc.call)
			t.Run(fmt.Sprintf("pad%d/%s", pad, src), func(t *testing.T) {
				v := env.LoadString("keyargs", src)
				if tc.isErr {
					if assert.Equal(t, lisp.LError, v.Type, "got %v", v) {
						assert.Contains(t, v.String(), tc.want)
					}
					return
				}
				if assert.NotEqual(t, lisp.LError, v.Type, "got %v", v) {
					assert.Equal(t, tc.want, v.String())
				}
			})
		}
	}
}

// TestKeyArgsBindingDuplicateFormal checks a builtin whose &key formals name
// the same symbol twice. lambda refuses such a list, but a Go builtin can
// declare one. The first formal takes the value and the second binds nil.
func TestKeyArgsBindingDuplicateFormal(t *testing.T) {
	for _, pad := range []int{0, 17} {
		formals := []string{lisp.KeyArgSymbol}
		if pad > 0 {
			formals = append(formals, strings.Fields(keyArgsPadding(pad))...)
		}
		formals = append(formals, "x", "x")
		env := keyArgsEnv(t)
		env.AddBuiltins(true, elpsutil.Function("dupkey", lisp.Formals(formals...),
			func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				n := len(args.Cells)
				return lisp.QExpr(args.Cells[n-2:])
			}))
		v := env.LoadString("keyargs", "(dupkey :x 1 :x 2)")
		if assert.NotEqual(t, lisp.LError, v.Type, "pad %d: got %v", pad, v) {
			assert.Equal(t, "'(2 ())", v.String(), "pad %d", pad)
		}
	}
}
