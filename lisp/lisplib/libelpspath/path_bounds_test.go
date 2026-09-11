// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestOutOfRangeWriteParity pins C-F2 (#657): a missing index must never
// replace its sequence with nil. Lists retain the existing in-place ban.
func TestOutOfRangeWriteParity(t *testing.T) {
	for _, op := range []struct {
		name         string
		copy, mutate lisp.LBuiltin
		set          bool
	}{
		{"set", BuiltinQuerySet, BuiltinQuerySetMutate, true},
		{"del", BuiltinQueryDelete, BuiltinQueryDeleteMutate, false},
		{"nil", BuiltinQueryNil, BuiltinQueryNilMutate, false},
	} {
		for _, list := range []bool{false, true} {
			for _, depth := range []int{0, 1, 3} {
				for _, index := range []int{2, 99, -3, -99} {
					t.Run(fmt.Sprintf("%s/list=%t/depth=%d/index=%d", op.name, list, depth, index), func(t *testing.T) {
						env := testEnv(t)
						build := func() (*lisp.LVal, []*lisp.LVal) {
							cells := []*lisp.LVal{mapHolding("value", lisp.Int(10)), lisp.Int(20)}
							doc := lisp.Vector(cells)
							if list {
								doc = lisp.QExpr(cells)
							}
							steps := make([]*lisp.LVal, 0, depth+1)
							for range depth {
								doc = mapHolding("lines", doc)
								doc.MapSet("id", lisp.Int(7))
								steps = append(steps, lisp.String("lines"))
							}
							steps = append(steps, lisp.Int(index))
							return doc, steps
						}
						doc, steps := build()
						before := doc.String()
						args := append([]*lisp.LVal{doc}, steps...)
						if op.set {
							args = append(args, lisp.Int(9))
						}
						result := op.copy(env, lisp.QExpr(args))
						require.NotEqual(t, lisp.LError, result.Type, "%v", result)
						assert.Equal(t, before, result.String(), "copy must preserve the entire document")
						assert.Equal(t, before, doc.String(), "copy must leave source untouched")
						require.NotSame(t, doc, result, "even a no-op copy owns its containers")
						// An in-place edit through the no-op copy must not reach the source.
						leafSteps := append([]*lisp.LVal{}, steps[:len(steps)-1]...)
						leafSteps = append(leafSteps, lisp.Int(0), lisp.String("value"), lisp.Int(88))
						changed := BuiltinQuerySetMutate(env, lisp.QExpr(append([]*lisp.LVal{result}, leafSteps...)))
						require.NotEqual(t, lisp.LError, changed.Type, "%v", changed)
						assert.Equal(t, before, doc.String())
						readArgs := append([]*lisp.LVal{result}, leafSteps[:len(leafSteps)-1]...)
						value := BuiltinQueryGet(env, lisp.QExpr(readArgs))
						require.Equal(t, lisp.LInt, value.Type, "%v", value)
						assert.Equal(t, 88, value.Int)
						original, mutSteps := build()
						mutArgs := append([]*lisp.LVal{original}, mutSteps...)
						if op.set {
							mutArgs = append(mutArgs, lisp.Int(9))
						}
						mutated := op.mutate(env, lisp.QExpr(mutArgs))
						if list {
							require.Equal(t, lisp.LError, mutated.Type)
							assert.Contains(t, mutated.String(), "in-place path operations require an array or sorted-map; got list")
						} else {
							require.NotEqual(t, lisp.LError, mutated.Type, "%v", mutated)
							assert.Equal(t, before, mutated.String())
							assert.Same(t, original, mutated)
						}
						assert.Equal(t, before, original.String())
					})
				}
			}
		}
	}
}

// TestArbitraryLeaves pins C-F4 (#657) at the exported builtin boundary.
func TestArbitraryLeaves(t *testing.T) {
	for _, leaf := range []struct {
		name  string
		value *lisp.LVal
	}{
		{"keyword", lisp.Symbol(":pending")},
		{"bytes", lisp.Bytes([]byte("ok"))},
		{"function", lisp.Fun("leaf", lisp.Formals(), func(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal { return lisp.Nil() })},
		{"native", lisp.Native(42)},
	} {
		t.Run(leaf.name, func(t *testing.T) {
			env := testEnv(t)
			doc := mapHolding("status", leaf.value)
			doc.MapSet("id", lisp.Int(7))
			for _, tc := range []struct {
				name  string
				steps []*lisp.LVal
				want  *lisp.LVal
			}{
				{"root", nil, doc},
				{"leaf", []*lisp.LVal{lisp.String("status")}, leaf.value},
				{"past-leaf", []*lisp.LVal{lisp.String("id")}, lisp.Int(7)},
			} {
				t.Run(tc.name, func(t *testing.T) {
					got := BuiltinQueryGet(env, lisp.QExpr(append([]*lisp.LVal{doc}, tc.steps...)))
					require.NotEqual(t, lisp.LError, got.Type, "%v", got)
					assert.Equal(t, tc.want.String(), got.String())
				})
			}
			for _, op := range []struct {
				name string
				run  lisp.LBuiltin
				set  bool
			}{
				{"set", BuiltinQuerySet, true}, {"set!", BuiltinQuerySetMutate, true},
				{"del", BuiltinQueryDelete, false}, {"del!", BuiltinQueryDeleteMutate, false},
				{"nil", BuiltinQueryNil, false}, {"nil!", BuiltinQueryNilMutate, false},
			} {
				t.Run(op.name, func(t *testing.T) {
					src := mapHolding("status", leaf.value)
					src.MapSet("id", lisp.Int(7))
					args := []*lisp.LVal{src, lisp.String("id")}
					if op.set {
						args = append(args, leaf.value)
					}
					got := op.run(env, lisp.QExpr(args))
					require.NotEqual(t, lisp.LError, got.Type, "%v", got)
					status, ok := got.Map().Get(lisp.String("status"))
					require.True(t, ok)
					assert.Same(t, leaf.value, status, "opaque leaves are shared by reference")
					if op.set {
						stored, ok := got.Map().Get(lisp.String("id"))
						require.True(t, ok)
						assert.Same(t, leaf.value, stored)
					} else {
						stored, exists := got.Map().Get(lisp.String("id"))
						if op.name == "del" || op.name == "del!" {
							assert.False(t, exists, "delete must remove an existing key")
						} else {
							require.True(t, exists, "nil must keep the key")
							assert.True(t, stored.IsNil(), "nil must clear the value: %v", stored)
						}
					}
				})
			}
			for _, step := range []*lisp.LVal{lisp.String("child"), lisp.Int(0), lisp.Symbol("*"), lisp.QExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(0)})} {
				for _, op := range []struct {
					name string
					run  lisp.LBuiltin
					set  bool
				}{
					{"get", BuiltinQueryGet, false}, {"set", BuiltinQuerySet, true}, {"set!", BuiltinQuerySetMutate, true},
					{"del", BuiltinQueryDelete, false}, {"del!", BuiltinQueryDeleteMutate, false},
					{"nil", BuiltinQueryNil, false}, {"nil!", BuiltinQueryNilMutate, false},
				} {
					t.Run("descend/"+step.String()+"/"+op.name, func(t *testing.T) {
						args := []*lisp.LVal{doc, lisp.String("status"), step}
						if op.set {
							args = append(args, lisp.Int(9))
						}
						got := op.run(env, lisp.QExpr(args))
						require.Equal(t, lisp.LError, got.Type, "%v", got)
						assert.False(t, lisp.IsInternalPanic(got))
						assert.Contains(t, got.String(), fmt.Sprintf("cannot index into %s at path status", leaf.value.Type))
					})
				}
			}
		})
	}
}

// TestLeafErrorLocations distinguishes the leaf's location from the failing
// next step, including keys that need quoting to avoid ambiguous paths.
func TestLeafErrorLocations(t *testing.T) {
	leaf := lisp.Symbol(":pending")
	for _, tc := range []struct {
		name     string
		doc      *lisp.LVal
		steps    []*lisp.LVal
		location string
	}{
		{"root", leaf, []*lisp.LVal{lisp.Int(0)}, "<root>"},
		{"nested", mapHolding("job", mapHolding("status", leaf)), []*lisp.LVal{lisp.String("job"), lisp.String("status"), lisp.String("child")}, "job.status"},
		{"index", mapHolding("jobs", lisp.Vector([]*lisp.LVal{leaf})), []*lisp.LVal{lisp.String("jobs"), lisp.Int(0), lisp.String("child")}, "jobs[0]"},
		{"punctuation", mapHolding("job.status", leaf), []*lisp.LVal{lisp.String("job.status"), lisp.Int(0)}, `["job.status"]`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			path, err := ArgsToPath(tc.steps)
			require.NoError(t, err)
			for _, run := range []func() (*lisp.LVal, error){
				func() (*lisp.LVal, error) { return path.Get(tc.doc) },
				func() (*lisp.LVal, error) { return path.Set(tc.doc, lisp.Int(9)) },
				func() (*lisp.LVal, error) { return path.Delete(tc.doc) },
				func() (*lisp.LVal, error) { return path.Nil(tc.doc) },
			} {
				_, err := run()
				require.EqualError(t, err, "cannot index into symbol at path "+tc.location)
			}
		})
	}
}
