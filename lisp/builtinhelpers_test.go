// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"reflect"
	"regexp"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest/oracle"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// helperDoc matches the first line of a helper's doc comment.
var helperDoc = regexp.MustCompile(`^(\w+) returns what the (\S+) builtin returns`)

// documentedHelpers returns, for each LEnv method in package lisp whose doc
// comment says it "returns what the X builtin returns", X and the method.
func documentedHelpers(t *testing.T) map[string]string {
	t.Helper()
	fset := token.NewFileSet()
	entries, err := os.ReadDir(".")
	require.NoError(t, err)
	found := map[string]string{}
	for _, e := range entries {
		name := e.Name()
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		f, err := parser.ParseFile(fset, name, nil, parser.ParseComments)
		require.NoError(t, err)
		for _, d := range f.Decls {
			fd, ok := d.(*ast.FuncDecl)
			if !ok || fd.Recv == nil || fd.Doc == nil || !fd.Name.IsExported() {
				continue
			}
			star, ok := fd.Recv.List[0].Type.(*ast.StarExpr)
			if !ok {
				continue
			}
			if id, isIdent := star.X.(*ast.Ident); !isIdent || id.Name != "LEnv" {
				continue
			}
			m := helperDoc.FindStringSubmatch(fd.Doc.Text())
			if m == nil || m[1] != fd.Name.Name {
				continue
			}
			if prev, dup := found[m[2]]; dup {
				t.Errorf("LEnv.%s and LEnv.%s both say they return what %s returns; the registry holds one", prev, fd.Name.Name, m[2])
			}
			found[m[2]] = fd.Name.Name
		}
	}
	return found
}

// TestBuiltinGoHelpersListEveryHelper checks the registry against the code:
// every documented helper is registered under its builtin, every registered
// builtin exists, and every registered method is an LEnv method.
func TestBuiltinGoHelpersListEveryHelper(t *testing.T) {
	documented := documentedHelpers(t)
	require.NotEmpty(t, documented)
	for builtin, method := range documented {
		h, ok := lisp.BuiltinGoHelper(builtin)
		if assert.True(t, ok, "LEnv.%s says it returns what %s returns, but builtinGoHelpers has no %q", method, builtin, builtin) {
			assert.Equal(t, method, h.Method, builtin)
		}
	}
	envType := reflect.TypeOf((*lisp.LEnv)(nil))
	for _, name := range lisp.BuiltinGoHelperNames() {
		assert.NotPanics(t, func() { lisp.BuiltinFunc(name) }, "builtinGoHelpers names %q, which is no builtin", name)
		h, _ := lisp.BuiltinGoHelper(name)
		assert.NotEqual(t, h.Method == "", h.Note == "", "%s: set Method or Note", name)
		if h.Method != "" {
			_, ok := envType.MethodByName(h.Method)
			assert.True(t, ok, "%s: LEnv has no method %s", name, h.Method)
		}
	}
	_, ok := lisp.BuiltinGoHelper("no-such-builtin")
	assert.False(t, ok)
}

// registeredBuiltins returns the builtin names that have a GoHelper.
func registeredBuiltins() []string {
	var names []string
	for _, b := range lisp.DefaultBuiltins() {
		if _, ok := lisp.BuiltinGoHelper(b.Name()); ok {
			names = append(names, b.Name())
		}
	}
	return names
}

var (
	lvalType  = reflect.TypeOf((*lisp.LVal)(nil))
	lvalsType = reflect.TypeOf([]*lisp.LVal(nil))
	boolType  = reflect.TypeOf(false)
	intType   = reflect.TypeOf(0)
)

// lvalHelper returns the method h names when it takes only *LVal arguments,
// the last one variadic or not, and returns *LVal, (bool, *LVal) or
// (int, *LVal): the shapes a test can call with any argument list.  It
// returns the number of fixed arguments too.
func lvalHelper(h lisp.GoHelper) (reflect.Method, int, bool) {
	m, ok := reflect.TypeOf((*lisp.LEnv)(nil)).MethodByName(h.Method)
	if !ok {
		return m, 0, false
	}
	ft := m.Type
	nfixed := ft.NumIn() - 1 // the receiver
	if ft.IsVariadic() {
		nfixed--
		if ft.In(ft.NumIn()-1) != lvalsType {
			return m, 0, false
		}
	}
	for i := 1; i <= nfixed; i++ {
		if ft.In(i) != lvalType {
			return m, 0, false
		}
	}
	switch {
	case ft.NumOut() == 1 && ft.Out(0) == lvalType:
	case ft.NumOut() == 2 && (ft.Out(0) == boolType || ft.Out(0) == intType) && ft.Out(1) == lvalType:
	default:
		return m, 0, false
	}
	return m, nfixed, true
}

// callHelper calls m on env with args and returns its result as the
// builtin returns it: a bool as a Lisp bool and an int as a Lisp int.
func callHelper(env *lisp.LEnv, m reflect.Method, args []*lisp.LVal) *lisp.LVal {
	in := []reflect.Value{reflect.ValueOf(env)}
	for _, a := range args {
		in = append(in, reflect.ValueOf(a))
	}
	out := m.Func.Call(in)
	last, _ := out[len(out)-1].Interface().(*lisp.LVal)
	if len(out) == 1 || last != nil {
		return last
	}
	if out[0].Type() == boolType {
		return lisp.Bool(out[0].Bool())
	}
	return lisp.Int(int(out[0].Int()))
}

// helperArgs returns the argument lists to try for a method with nfixed
// fixed arguments: every value of a type in each position, the later
// positions from a shorter list, and for a variadic method also no further
// argument and two more.  Each call builds fresh values, since a helper may
// change its arguments.
func helperArgs(nfixed int, variadic bool) [][]int {
	all := len(oracle.Values())
	short := min(all, 12)
	lists := [][]int{{}}
	for pos := range nfixed {
		n := all
		if pos > 0 {
			n = short
		}
		var next [][]int
		for _, l := range lists {
			for i := range n {
				next = append(next, append(append([]int(nil), l...), i))
			}
		}
		lists = next
	}
	if variadic {
		var more [][]int
		for _, l := range lists {
			more = append(more, l)
			for i := range short {
				more = append(more, append(append([]int(nil), l...), i, (i+1)%short))
			}
		}
		lists = more
	}
	return lists
}

// argValues builds fresh values for the indices of an argument list.
func argValues(idx []int) []*lisp.LVal {
	vals := oracle.Values()
	args := make([]*lisp.LVal, len(idx))
	for i, j := range idx {
		args[i] = vals[j].V
	}
	return args
}

// sameValue fails t unless got and want are the same error condition or
// equal values.  A value that holds an error value is not Equal to itself,
// so such a value is compared by how it prints.
func sameValue(t *testing.T, label string, got, want *lisp.LVal) {
	t.Helper()
	if !want.IsError() && !got.IsError() && got.Type == want.Type {
		if eq := got.Equal(want); !eq.IsError() && lisp.True(eq) {
			return
		}
		if got.String() == want.String() {
			return
		}
	}
	oracle.Same(t, label, got, want)
}

// TestBuiltinGoHelpersMatchBuiltins calls each registered method that takes
// only *LVal arguments with the arguments its builtin takes, from values of
// every type, and checks that it returns what CallBuiltin returns, and leaves
// its arguments as the builtin leaves them.  funcall and apply need the
// evaluator's stack; TestCallFunctionMatchesFuncall checks them.
func TestBuiltinGoHelpersMatchBuiltins(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	// A small limit keeps make-sequence over a wide range short, and checks
	// that each helper makes its builtin's allocation checks.
	env.Runtime.MaxAlloc = 64
	tested := 0
	for _, name := range registeredBuiltins() {
		if name == "funcall" || name == "apply" {
			continue
		}
		h, _ := lisp.BuiltinGoHelper(name)
		if h.Method == "" {
			continue
		}
		m, nfixed, ok := lvalHelper(h)
		if !ok {
			continue
		}
		tested++
		ref := lisp.BuiltinFunc(name)
		for _, idx := range helperArgs(nfixed, m.Type.IsVariadic()) {
			wantArgs, gotArgs := argValues(idx), argValues(idx)
			want := env.CallBuiltin(ref, wantArgs...)
			got := callHelper(env, m, gotArgs)
			label := name + "/" + argLabel(idx)
			sameValue(t, label, got, want)
			for i := range wantArgs {
				sameValue(t, label+"/arg", gotArgs[i], wantArgs[i])
			}
		}
	}
	assert.Greater(t, tested, 20, "the test reached too few helpers")
}

// TestBuiltinGoHelpersCancelled holds each method that
// TestBuiltinGoHelpersMatchBuiltins calls to its builtin when the context is
// done: each makes its one check before anything else.
func TestBuiltinGoHelpersCancelled(t *testing.T) {
	for _, name := range registeredBuiltins() {
		h, _ := lisp.BuiltinGoHelper(name)
		m, nfixed, ok := lvalHelper(h)
		if h.Method == "" || !ok || name == "funcall" || name == "apply" {
			continue
		}
		// Pass two variadic values, so a variadic helper reaches past its
		// fixed arguments.
		n := nfixed
		if m.Type.IsVariadic() {
			n += 2
		}
		idx := make([]int, n)
		ref := lisp.BuiltinFunc(name)
		oracle.Sweep(t, name,
			func(env *lisp.LEnv) *lisp.LVal { return callHelper(env, m, argValues(idx)) },
			func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(ref, argValues(idx)...) })
	}
}

func argLabel(idx []int) string {
	vals := oracle.Values()
	parts := make([]string, len(idx))
	for i, j := range idx {
		parts[i] = vals[j].Label
	}
	return strings.Join(parts, ",")
}

// TestAppendMatchesBuiltin checks LEnv.Append, which takes a SeqKind, against
// append with the type specifier each kind stands for, the refused kinds
// included.
func TestAppendMatchesBuiltin(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	env.Runtime.MaxAlloc = 64
	ref := lisp.BuiltinFunc("append")
	all := len(oracle.Values())
	for kind := lisp.SeqKind(0); kind <= lisp.SeqNil+1; kind++ {
		spec := lisp.Symbol(kind.String())
		if kind == lisp.SeqNil {
			spec = lisp.Nil()
		}
		for i := range all {
			for _, extra := range [][]int{{}, {i % 12}, {i % 12, (i + 1) % 12}} {
				idx := append([]int{i}, extra...)
				wantArgs, gotArgs := argValues(idx), argValues(idx)
				want := env.CallBuiltin(ref, append([]*lisp.LVal{spec}, wantArgs...)...)
				got := env.Append(kind, gotArgs[0], gotArgs[1:]...)
				label := kind.String() + "/" + argLabel(idx)
				sameValue(t, label, got, want)
				for j := range wantArgs {
					sameValue(t, label+"/arg", gotArgs[j], wantArgs[j])
				}
			}
		}
	}
	oracle.Sweep(t, "append",
		func(env *lisp.LEnv) *lisp.LVal { return env.Append(lisp.SeqList, lisp.Nil(), lisp.Int(1)) },
		func(env *lisp.LEnv) *lisp.LVal {
			return env.CallBuiltin(ref, lisp.Symbol("list"), lisp.Nil(), lisp.Int(1))
		})
}

// typePredicates is the Go each type predicate's Note names.
var typePredicates = map[string]func(v *lisp.LVal) bool{
	"nil?":          func(v *lisp.LVal) bool { return v.IsNil() },
	"true?":         lisp.True,
	"list?":         func(v *lisp.LVal) bool { return v.Type == lisp.LSExpr },
	"sorted-map?":   func(v *lisp.LVal) bool { return v.Type == lisp.LSortMap },
	"array?":        func(v *lisp.LVal) bool { return v.Type == lisp.LArray },
	"vector?":       func(v *lisp.LVal) bool { return v.Type == lisp.LArray && v.Cells[0].Len() == 1 },
	"bool?":         func(v *lisp.LVal) bool { return v.IsSymbol(lisp.TrueSymbol) || v.IsSymbol(lisp.FalseSymbol) },
	"number?":       func(v *lisp.LVal) bool { return v.IsNumeric() },
	"int?":          func(v *lisp.LVal) bool { return v.Type == lisp.LInt },
	"float?":        func(v *lisp.LVal) bool { return v.Type == lisp.LFloat },
	"symbol?":       func(v *lisp.LVal) bool { return v.Type == lisp.LSymbol },
	"string?":       func(v *lisp.LVal) bool { return v.Type == lisp.LString },
	"bytes?":        func(v *lisp.LVal) bool { return v.Type == lisp.LBytes },
	"tagged-value?": func(v *lisp.LVal) bool { return v.Type == lisp.LTaggedVal },
}

// TestTypePredicateNotes checks that every builtin of one argument whose
// name ends in "?" has a Note, and that the Go each Note names returns what
// the builtin returns for a value of every type.
func TestTypePredicateNotes(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	checked := 0
	for _, b := range lisp.DefaultBuiltins() {
		name := b.Name()
		if !strings.HasSuffix(name, "?") || b.Formals().Len() != 1 {
			continue
		}
		h, ok := lisp.BuiltinGoHelper(name)
		if !assert.True(t, ok, "the type predicate %s has no GoHelper", name) {
			continue
		}
		if h.Method != "" {
			continue // empty?, which LEnv.IsEmpty covers
		}
		checked++
		pred, ok := typePredicates[name]
		if !assert.True(t, ok, "typePredicates has no Go for %s (Note %q)", name, h.Note) {
			continue
		}
		ref := lisp.BuiltinFunc(name)
		for _, v := range oracle.Values() {
			want := env.CallBuiltin(ref, v.V)
			require.False(t, want.IsError(), name)
			assert.Equal(t, lisp.True(want), pred(v.V), "%s %s", name, v.Label)
		}
	}
	assert.Equal(t, len(typePredicates), checked, "a type predicate in typePredicates is no builtin of one argument")
}
