// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestSplitSymbolParts(t *testing.T) {
	for _, s := range []string{"", "name", "λ", "pkg:name", ":keyword", ":", "pkg:", "::", "a:b:c", ":a:b", "a:::b", "a\x00:b"} {
		t.Run(s, func(t *testing.T) {
			sym := Quote(Symbol(s))
			sym.source = &token.Location{File: "symbol", Line: 7}
			pieces := strings.Split(s, ":") // pre-optimization oracle
			ns, name, n := splitSymbolParts(s)
			require.Equal(t, len(pieces), n)
			got := SplitSymbol(sym)
			if n > 2 {
				assert.Equal(t, Errorf("illegal symbol: %q", s).String(), got.String())
				return
			}
			require.Equal(t, LSExpr, got.Type)
			require.True(t, got.quoted)
			require.Len(t, got.Cells, n)
			if n == 1 {
				assert.Empty(t, ns)
				assert.Equal(t, s, name)
				assert.Same(t, sym, got.Cells[0])
			} else {
				assert.Equal(t, pieces[0], ns)
				assert.Equal(t, pieces[1], name)
				assert.Equal(t, Symbol(ns), got.Cells[0])
				assert.Equal(t, Symbol(name), got.Cells[1])
			}
		})
	}
	assert.Equal(t, Errorf("not a symbol").String(), SplitSymbol(String("name")).String())
}

func TestGlobalSymbolParts(t *testing.T) {
	env := initSafetyTestEnv(t)
	value := Int(42)
	for _, s := range []string{"plain", "", "user:qualified", "user:"} {
		sym := Symbol(s)
		require.NotEqual(t, LError, env.PutGlobal(sym, value).Type)
		assert.Same(t, value, env.GetGlobal(sym))
		require.NotEqual(t, LError, env.PutGlobalFromLisp(sym, value).Type)
	}
	for _, s := range []string{":", ":keyword"} {
		sym := Symbol(s)
		assert.Same(t, sym, env.GetGlobal(sym))
		assert.Equal(t, env.Errorf("value cannot be assigned to a keyword: %s", s).String(), env.PutGlobal(sym, value).String())
	}
	for _, sym := range []*LVal{String("plain"), Symbol("a:b:c"), Symbol("::"), Symbol(":a:b")} {
		want := SplitSymbol(sym)
		require.Nil(t, env.ErrorAssociate(want))
		assert.Equal(t, want.String(), env.GetGlobal(sym).String())
		assert.Equal(t, want.String(), env.PutGlobal(sym, value).String())
	}
	for _, s := range []string{"name", "user:name", ":keyword", "", "::", "a:b:c"} {
		for _, quoted := range []bool{false, true} {
			sym := Symbol(s)
			sym.quoted = quoted
			got := opQualifiedSymbol(env, QExpr([]*LVal{sym}))
			pieces := SplitSymbol(sym)
			if pieces.Type == LError {
				require.Nil(t, env.ErrorAssociate(pieces))
				assert.Equal(t, pieces.String(), got.String())
			} else if pieces.Len() == 2 {
				if quoted {
					assert.Same(t, sym, got)
				} else {
					assert.Equal(t, Quote(sym), got)
				}
			} else {
				assert.Equal(t, Quote(Symbol("user:"+s)), got)
			}
		}
	}
}

func TestGeneratedNameFormatting(t *testing.T) {
	env := initSafetyTestEnv(t)
	for _, next := range []uint{0, 1, 9, 10, 255, 256, 9999999, 10000000, 99999999, 100000000, ^uint(0) >> 1, ^uint(0)} {
		env.Runtime.numenv = atomicCounter(next - 1)
		fun := env.Lambda(Formals(), nil)
		require.Equal(t, LFun, fun.Type)
		assert.Equal(t, fmt.Sprintf("_fun%d", next), fun.FID())
		env.Runtime.numsym = atomicCounter(next - 1)
		assert.Equal(t, fmt.Sprintf("gen%08d", next), env.Runtime.GenSym())
	}
}

func TestEvaluatorMarkerStorage(t *testing.T) {
	env := initSafetyTestEnv(t)
	expr := Int(42)
	term := env.Terminal(expr)
	require.Equal(t, LMarkTerminal, term.Type)
	assert.Same(t, env, term.Native)
	require.Len(t, term.Cells, 1)
	assert.Same(t, expr, term.Cells[0])
	macro := markMacExpand(expr)
	require.Equal(t, LMarkMacExpand, macro.Type)
	require.Len(t, macro.Cells, 1)
	assert.Same(t, expr, macro.Cells[0])
	fun, args := Symbol("fn"), QExpr([]*LVal{expr})
	mark := markTailRec(2, fun, args)
	require.Len(t, mark.Cells, 4)
	assert.False(t, decrementMarkTailRec(mark))
	assert.Equal(t, 2, mark.tailRecElided()) // the two counters must not alias
	assert.True(t, decrementMarkTailRec(mark))
	assert.Same(t, fun, mark.tailRecFun())
	assert.Same(t, args, mark.tailRecArgs())
	assert.Equal(t, 1, cap(term.Cells))
	assert.Equal(t, 1, cap(macro.Cells))
	assert.Equal(t, 4, cap(mark.Cells))
}

func TestEvalNestingCacheTracksDirectWrites(t *testing.T) {
	for _, rt := range []*Runtime{{}, StandardRuntime()} {
		for _, setting := range []int{0, 1, 12, -1, -7, 0, 4} {
			rt.MaxEvalNesting = setting
			limit := rt.MaxEvalNestingDepth()
			for _, depth := range []int{0, 1, 4, 12, 13, DefaultMaxEvalNesting, DefaultMaxEvalNesting + 1, int(^uint(0) >> 1)} {
				rt.evalNesting = depth
				assert.Equal(t, limit > 0 && depth > limit, rt.evalNestingExceeded(), "setting=%d depth=%d", setting, depth)
			}
		}
	}
}

func TestEvalRestoresNestingAfterPanic(t *testing.T) {
	env := initSafetyTestEnv(t)
	env.AddBuiltins(true, &langBuiltin{
		name: "nesting-panic", formals: Formals(), docs: "panic recovery test",
		fun: func(env *LEnv, _ *LVal) *LVal {
			require.Equal(t, 2, env.Runtime.EvalNesting())
			panic("nesting restore")
		},
	})
	call := SExpr([]*LVal{Symbol("identity"), SExpr([]*LVal{Symbol("nesting-panic")})})
	got := env.Eval(call)
	require.True(t, IsInternalPanic(got), "%v", got)
	assert.Zero(t, env.Runtime.EvalNesting())
	assert.Zero(t, env.Runtime.evalDepth)
	assert.Empty(t, env.Runtime.Stack.Frames)
	// Exercise eval's own recovery with a nonzero enclosing depth too.
	env.Runtime.evalNesting = 7
	got = env.eval(context.Background(), nil)
	require.True(t, IsInternalPanic(got))
	assert.Equal(t, 7, env.Runtime.EvalNesting())
	env.Runtime.evalNesting = 0
	assert.Equal(t, 42, env.Eval(Int(42)).Int)
	assert.Zero(t, env.Runtime.EvalNesting())
}
