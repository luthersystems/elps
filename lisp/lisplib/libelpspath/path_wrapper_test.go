// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func wrapTagged(v *lisp.LVal) *lisp.LVal {
	return &lisp.LVal{Type: lisp.LTaggedVal, Str: "user:box", Cells: []*lisp.LVal{v}}
}

func wrapQuote(v *lisp.LVal) *lisp.LVal {
	return lisp.Quote(lisp.Quote(v))
}

// TestWrapperCopyIsolation pins U9b: wrappers cannot hide source containers
// from copying writes, even though paths cannot index into the wrappers.
func TestWrapperCopyIsolation(t *testing.T) {
	for _, wrapper := range []struct {
		name string
		wrap func(*lisp.LVal) *lisp.LVal
	}{
		{"tagged", wrapTagged}, {"quote", wrapQuote},
		{"quote-tagged", func(v *lisp.LVal) *lisp.LVal { return wrapQuote(wrapTagged(v)) }},
	} {
		for _, op := range []struct {
			name string
			run  lisp.LBuiltin
			set  bool
		}{
			{"set", BuiltinQuerySet, true}, {"delete", BuiltinQueryDelete, false}, {"nil", BuiltinQueryNil, false},
		} {
			t.Run(wrapper.name+"/"+op.name, func(t *testing.T) {
				env := testEnv(t)
				inner := mapHolding("n", lisp.Int(1))
				wrapped := wrapper.wrap(inner)
				doc := mapHolding("wrapped", wrapped)
				doc.MapSet("k", lisp.Int(1))
				args := []*lisp.LVal{doc, lisp.String("k")}
				if op.set {
					args = append(args, lisp.Int(2))
				}
				result := op.run(env, lisp.QExpr(args))
				require.NotEqual(t, lisp.LError, result.Type, "%v", result)
				changed, exists := result.Map().Get(lisp.String("k"))
				switch op.name {
				case "set":
					require.True(t, exists)
					assert.Equal(t, "2", changed.String())
				case "delete":
					assert.False(t, exists)
				case "nil":
					require.True(t, exists)
					assert.True(t, changed.IsNil())
				}
				cp := callBuiltin(env, BuiltinQueryGet, result, lisp.String("wrapped"))
				require.Equal(t, wrapped.Type, cp.Type, "%v", cp)
				assert.Equal(t, wrapped.String(), cp.String())
				for src := wrapped; src.Type == lisp.LTaggedVal || src.Type == lisp.LQuote; src = src.Cells[0] {
					require.Equal(t, src.Type, cp.Type)
					assert.Equal(t, src.Str, cp.Str)
					assert.Equal(t, src.IsQuoted(), cp.IsQuoted())
					assert.NotSame(t, src, cp, "wrapper header must be rebuilt")
					cp = cp.Cells[0]
				}
				require.Equal(t, lisp.LSortMap, cp.Type)
				require.NoError(t, lisp.GoError(inner.MapSet("n", lisp.Int(99))))
				got, ok := cp.Map().Get(lisp.String("n"))
				require.True(t, ok)
				assert.Equal(t, "1", got.String(), "source mutation must not reach copy")
				require.NoError(t, lisp.GoError(cp.MapSet("n", lisp.Int(77))))
				got, ok = inner.Map().Get(lisp.String("n"))
				require.True(t, ok)
				assert.Equal(t, "99", got.String(), "copy mutation must not reach source")
			})
		}
	}
}

// TestWrapperCyclesRejected checks both public builtin validation and the
// independently guarded Go copy API, for cycles through either wrapper type.
func TestWrapperCyclesRejected(t *testing.T) {
	for _, wrapper := range []struct {
		name string
		wrap func(*lisp.LVal) *lisp.LVal
	}{
		{"tagged", wrapTagged}, {"quote", wrapQuote},
	} {
		t.Run(wrapper.name, func(t *testing.T) {
			doc := mapHolding("x", lisp.Int(1))
			doc.MapSet("self", wrapper.wrap(doc))
			require.ErrorIs(t, okSimpleType(doc), errCyclicValue)
			_, err := copyLVal(doc)
			require.ErrorIs(t, err, errCyclicValue)
			for _, op := range []struct {
				name string
				run  lisp.LBuiltin
				set  bool
			}{
				{"get", BuiltinQueryGet, false}, {"set", BuiltinQuerySet, true}, {"set!", BuiltinQuerySetMutate, true},
				{"delete", BuiltinQueryDelete, false}, {"delete!", BuiltinQueryDeleteMutate, false},
				{"nil", BuiltinQueryNil, false}, {"nil!", BuiltinQueryNilMutate, false},
			} {
				t.Run(op.name, func(t *testing.T) {
					args := []*lisp.LVal{doc, lisp.String("x")}
					if op.set {
						args = append(args, lisp.Int(2))
					}
					result := op.run(testEnv(t), lisp.QExpr(args))
					require.Equal(t, lisp.LError, result.Type)
					assert.False(t, lisp.IsInternalPanic(result))
					assert.Contains(t, result.String(), errCyclicValue.Error())
				})
			}
		})
	}
}

func TestWrappersRemainNonIndexable(t *testing.T) {
	for _, wrap := range []func(*lisp.LVal) *lisp.LVal{wrapTagged, wrapQuote} {
		wrapped := wrap(mapHolding("n", lisp.Int(1)))
		doc := mapHolding("wrapped", wrapped)
		path, err := ArgsToPath([]*lisp.LVal{lisp.String("wrapped"), lisp.String("n")})
		require.NoError(t, err)
		for _, run := range []func() (*lisp.LVal, error){
			func() (*lisp.LVal, error) { return path.Get(doc) },
			func() (*lisp.LVal, error) { return path.Set(doc, lisp.Int(2)) },
			func() (*lisp.LVal, error) { return path.Delete(doc) },
			func() (*lisp.LVal, error) { return path.Nil(doc) },
		} {
			_, err := run()
			require.EqualError(t, err, fmt.Sprintf("cannot index into %s at path wrapped", wrapped.Type))
		}
	}
}

func TestRangeReplacementTypeMessage(t *testing.T) {
	for _, run := range []lisp.LBuiltin{BuiltinQuerySet, BuiltinQuerySetMutate} {
		doc := mapHolding("a", lisp.Vector([]*lisp.LVal{lisp.Int(1)}))
		result := callBuiltin(testEnv(t), run, doc, lisp.String("a"),
			lisp.QExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(0), lisp.Int(1)}), lisp.Symbol(":kw"))
		require.Equal(t, lisp.LError, result.Type)
		assert.Equal(t, "range replacement must be a list or vector; got symbol", result.Cells[0].Str)
		assert.Equal(t, `(sorted-map "a" (vector 1))`, doc.String())
	}
}

// TestMalformedWrappersRejected pins the host API guard on both walks: a
// wrapper must contain exactly one non-nil value before Cells[0] is read.
func TestMalformedWrappersRejected(t *testing.T) {
	for _, wrapper := range []struct {
		name string
		wrap func(*lisp.LVal) *lisp.LVal
	}{{"tagged", wrapTagged}, {"quote", wrapQuote}} {
		for _, shape := range []struct {
			name  string
			cells []*lisp.LVal
		}{
			{"empty", nil},
			{"nil-child", []*lisp.LVal{nil}},
			{"extra-child", []*lisp.LVal{lisp.Int(1), lisp.Int(2)}},
		} {
			t.Run(wrapper.name+"/"+shape.name, func(t *testing.T) {
				v := wrapper.wrap(lisp.Int(1))
				v.Cells = shape.cells
				want := fmt.Sprintf("invalid %s wrapper: expected one value", v.Type)
				require.EqualError(t, okSimpleType(v), want)
				_, err := copyLVal(v)
				require.EqualError(t, err, want)
			})
		}
	}
}
