// Copyright © 2026 The ELPS authors

package libstring_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libstring"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func newStringAllocationEnv(t *testing.T, limit int) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env, lisp.WithReader(parser.NewReader()))))
	require.NoError(t, lisp.GoError(libstring.LoadPackage(env)))
	env.Runtime.MaxAlloc = limit
	return env
}

func assertStringAllocation(t *testing.T, got *lisp.LVal, want string, limit int) {
	t.Helper()
	require.False(t, lisp.IsInternalPanic(got), "%v", got)
	if len(want) > limit {
		require.Equal(t, lisp.LError, got.Type, "%v", got)
		assert.Contains(t, got.String(), "allocation")
		return
	}
	require.Equal(t, lisp.LString, got.Type, "%v", got)
	assert.Equal(t, want, got.Str)
}

func TestStringAllocationJoin(t *testing.T) {
	const limit = 8
	for _, size := range []int{limit - 1, limit, limit + 1} {
		t.Run(fmt.Sprint(size), func(t *testing.T) {
			env := newStringAllocationEnv(t, limit)
			tail := strings.Repeat("a", size-3)
			source := lisp.QExpr([]*lisp.LVal{lisp.String("é"), lisp.String(tail)})
			env.PutGlobal(lisp.Symbol("source"), source)
			got := env.LoadString("join-allocation.lisp", `(string:join source ":")`)
			assert.Equal(t, "é", source.Cells[0].Str)
			assert.Equal(t, tail, source.Cells[1].Str)
			assertStringAllocation(t, got, "é:"+tail, limit)
		})
	}
	for _, expr := range []string{`(string:join () "123456789")`, `(string:join '("") "123456789")`} {
		got := newStringAllocationEnv(t, limit).LoadString("join-allocation.lisp", expr)
		assertStringAllocation(t, got, "", limit)
	}
	for _, count := range []int{4, 5, 6} {
		t.Run(fmt.Sprintf("separator multiplier %d", count), func(t *testing.T) {
			env := newStringAllocationEnv(t, limit)
			cells := make([]*lisp.LVal, count)
			for i := range cells {
				cells[i] = lisp.String("")
			}
			env.PutGlobal(lisp.Symbol("source"), lisp.QExpr(cells))
			got := env.LoadString("join-allocation.lisp", `(string:join source "é")`)
			assertStringAllocation(t, got, strings.Repeat("é", count-1), limit)
		})
	}
}

func TestStringAllocationCaseConversion(t *testing.T) {
	const limit = 8
	for _, tc := range []struct {
		name, op, input, output string
	}{
		{"uppercase ASCII", "uppercase", "abcdefgh", "ABCDEFGH"},
		{"lowercase ASCII", "lowercase", "ABCDEFGH", "abcdefgh"},
		{"uppercase expands UTF8", "uppercase", "\u023f\u023faa", "\u2c7e\u2c7eAA"},
		{"lowercase expands UTF8", "lowercase", "\u023a\u023aAA", "\u2c65\u2c65aa"},
	} {
		for _, delta := range []int{-1, 0, 1} {
			t.Run(fmt.Sprintf("%s/%d", tc.name, delta), func(t *testing.T) {
				input, want := tc.input, tc.output
				if delta < 0 {
					input, want = input[:len(input)-1], want[:len(want)-1]
				} else if delta > 0 {
					input, want = input+"!", want+"!"
				}
				env := newStringAllocationEnv(t, limit)
				source := lisp.String(input)
				env.PutGlobal(lisp.Symbol("source"), source)
				got := env.LoadString("case-allocation.lisp", fmt.Sprintf(`(string:%s source)`, tc.op))
				assert.Equal(t, input, source.Str)
				assertStringAllocation(t, got, want, limit)
			})
		}
	}
	for _, tc := range []struct{ op, input, want string }{
		{"uppercase", strings.Repeat("ſ", 8), "SSSSSSSS"},
		{"lowercase", strings.Repeat("K", 8), "kkkkkkkk"},
		{"uppercase", "\xffa", "\ufffdA"},
		{"lowercase", "\xffA", "\ufffda"},
	} {
		t.Run(tc.op+" shrink or invalid UTF8", func(t *testing.T) {
			env := newStringAllocationEnv(t, limit)
			env.PutGlobal(lisp.Symbol("source"), lisp.String(tc.input))
			got := env.LoadString("case-allocation.lisp", fmt.Sprintf(`(string:%s source)`, tc.op))
			assertStringAllocation(t, got, tc.want, limit)
		})
	}
}

func TestStringAllocationSplit(t *testing.T) {
	const limit = 8
	for _, separator := range []string{",", ""} {
		for _, size := range []int{limit - 1, limit, limit + 1} {
			t.Run(fmt.Sprintf("%q/%d", separator, size), func(t *testing.T) {
				parts := make([]string, size)
				for i := range parts {
					parts[i] = "é"
				}
				input := strings.Join(parts, separator)
				env := newStringAllocationEnv(t, limit)
				source := lisp.String(input)
				env.PutGlobal(lisp.Symbol("source"), source)
				env.PutGlobal(lisp.Symbol("sep"), lisp.String(separator))
				got := env.LoadString("split-allocation.lisp", `(string:split source sep)`)
				require.False(t, lisp.IsInternalPanic(got), "%v", got)
				assert.Equal(t, input, source.Str)
				if size > limit {
					require.Equal(t, lisp.LError, got.Type, "%v", got)
					assert.Contains(t, got.String(), "allocation")
					return
				}
				require.Equal(t, lisp.LSExpr, got.Type, "%v", got)
				require.Len(t, got.Cells, size)
				for _, cell := range got.Cells {
					assert.Equal(t, "é", cell.Str)
				}
			})
		}
	}
	for _, tc := range []struct {
		input, sep string
		want       []string
	}{
		{"123456789", ",", []string{"123456789"}},
		{"a,", ",", []string{"a", ""}},
		{"", ",", []string{""}},
		{"", "", nil},
	} {
		t.Run(fmt.Sprintf("control %q/%q", tc.input, tc.sep), func(t *testing.T) {
			env := newStringAllocationEnv(t, limit)
			env.PutGlobal(lisp.Symbol("source"), lisp.String(tc.input))
			env.PutGlobal(lisp.Symbol("sep"), lisp.String(tc.sep))
			got := env.LoadString("split-allocation.lisp", `(string:split source sep)`)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.Equal(t, lisp.LSExpr, got.Type, "%v", got)
			require.Len(t, got.Cells, len(tc.want))
			for i, cell := range got.Cells {
				assert.Equal(t, tc.want[i], cell.Str)
			}
		})
	}
}

func TestStringAllocationCaseUnchangedAndInvalidUTF8(t *testing.T) {
	for _, tc := range []struct{ op, input string }{
		{"uppercase", "ABCDEFGHI"},
		{"lowercase", "abcdefghi"},
		{"uppercase", "\ufffd\ufffd\ufffd"},
		{"lowercase", "\ufffd\ufffd\ufffd"},
	} {
		t.Run(tc.op+tc.input, func(t *testing.T) {
			env := newStringAllocationEnv(t, 8)
			env.PutGlobal(lisp.Symbol("source"), lisp.String(tc.input))
			got := env.LoadString("case-allocation.lisp", fmt.Sprintf(`(string:%s source)`, tc.op))
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.Equal(t, lisp.LString, got.Type, "%v", got)
			assert.Equal(t, tc.input, got.Str, "unchanged valid text reuses existing storage")
		})
	}
	for _, op := range []string{"uppercase", "lowercase"} {
		for _, limit := range []int{2, 3, 4} {
			t.Run(fmt.Sprintf("%s invalid limit%d", op, limit), func(t *testing.T) {
				env := newStringAllocationEnv(t, limit)
				env.PutGlobal(lisp.Symbol("source"), lisp.String("\xff"))
				got := env.LoadString("case-allocation.lisp", fmt.Sprintf(`(string:%s source)`, op))
				assertStringAllocation(t, got, "\ufffd", limit)
			})
		}
	}
}

func TestStringAllocationRepeatWithoutCopy(t *testing.T) {
	for _, tc := range []struct {
		name, expr, want, wantErr string
	}{
		{"zero over cap", `(string:repeat source 0)`, "", ""},
		{"one over cap", `(string:repeat source 1)`, "123456789", ""},
		{"empty huge count", `(string:repeat "" huge-count)`, "", ""},
		{"two over cap", `(string:repeat source 2)`, "", "allocation"},
		{"two at cap", `(string:repeat "abcd" 2)`, "abcdabcd", ""},
		{"quoted input is unquoted", `(string:repeat (quote "x") 1)`, "x", ""},
		{"negative empty", `(string:repeat "" -1)`, "", "count is negative"},
		{"noninteger empty", `(string:repeat "" 0.0)`, "", "second argument is not an int"},
		{"nonstring zero", `(string:repeat 1 0)`, "", "first argument is not a string"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newStringAllocationEnv(t, 8)
			source := lisp.String("123456789")
			env.PutGlobal(lisp.Symbol("source"), source)
			env.PutGlobal(lisp.Symbol("huge-count"), lisp.Int(int(^uint(0)>>1)))
			got := env.LoadString("repeat-allocation.lisp", tc.expr)
			assert.Equal(t, "123456789", source.Str)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			if tc.wantErr != "" {
				require.Equal(t, lisp.LError, got.Type, "%v", got)
				assert.Contains(t, got.String(), tc.wantErr)
				return
			}
			require.Equal(t, lisp.LString, got.Type, "%v", got)
			assert.Equal(t, tc.want, got.Str)
			assert.False(t, got.IsQuoted(), "repeat returns an ordinary string even for quoted input")
		})
	}
}
