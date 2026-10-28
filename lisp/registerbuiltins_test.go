// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"bytes"
	"log"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// hiddenEnv returns registryEnv with the builtin hidden registered in
// regpkg by RegisterBuiltins (luthersystems/elps#907), and the exported Go
// macro call-hidden, which expands (call-hidden x) to a call whose head is
// this runtime's value of hidden.  That is how substrate's Go macros call a
// helper that has no name Lisp can reach.  hidden adds one to its argument,
// or raises an error when the argument is the symbol fail, so a test can
// read a stack frame.
func hiddenEnv(t *testing.T) (*lisp.LEnv, *lisp.LVal) {
	t.Helper()
	env := registryEnv(t)
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String("regpkg"))))
	fns := env.RegisterBuiltins(elpsutil.FunctionDoc("hidden", lisp.Formals("x"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		if x := args.Cells[0]; x.Type == lisp.LSymbol && x.Str == "fail" {
			return env.Errorf("hidden failed")
		}
		return lisp.Int(args.Cells[0].Int + 1)
	}, "doc for hidden"))
	require.Len(t, fns, 1)
	env.AddMacros(true, elpsutil.Function("call-hidden", lisp.Formals("x"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		fn := env.Runtime.Registry.RegisteredBuiltin("regpkg", "hidden")
		if fn == nil {
			return env.Errorf("hidden is not registered")
		}
		return lisp.SExpr([]*lisp.LVal{fn, args.Cells[0]})
	}))
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	return env, fns[0]
}

// A builtin RegisterBuiltins registers is in the registry but not in the
// package: Go finds it and a macro expansion calls it, Lisp cannot name it.
func TestRegisterBuiltinsUnbound(t *testing.T) {
	env, hidden := hiddenEnv(t)
	reg := env.Runtime.Registry
	assert.Same(t, hidden, reg.RegisteredBuiltin("regpkg", "hidden"))
	requireName(t, reg, hidden, "regpkg", "hidden")
	assert.Equal(t, "doc for hidden", hidden.Docstring())

	pkg := reg.Package("regpkg")
	_, bound := pkg.Symbol("hidden")
	assert.False(t, bound, "regpkg binds hidden")
	assert.NotContains(t, pkg.SymbolNames(), "hidden")
	assert.NotContains(t, pkg.Externals(), "hidden")
	_, bound = reg.Package(lisp.DefaultUserPackage).Symbol("hidden")
	assert.False(t, bound, "use-package imported hidden")

	// No Lisp path reaches it: unqualified, qualified, lisp:builtin.
	for _, src := range []string{`(hidden 1)`, `(regpkg:hidden 1)`, `(builtin 'regpkg:hidden)`} {
		v := env.LoadString("test", src)
		assert.Equal(t, lisp.LError, v.Type, "%s = %v", src, v)
	}

	// The expansion of a Go macro calls it.
	assert.Equal(t, "42", evalOK(t, env, `(regpkg:call-hidden 41)`))
}

// A stack frame names the builtin, and GetFunName logs nothing.  An
// unbound FunInPackage value, the old way to get such a helper, makes
// GetFunName log an error on each call.
func TestRegisterBuiltinsStackFrameName(t *testing.T) {
	env, hidden := hiddenEnv(t)
	assert.Equal(t, "hidden", env.GetFunName(hidden))
	var logs bytes.Buffer
	prev, flags := log.Writer(), log.Flags()
	log.SetOutput(&logs)
	t.Cleanup(func() { log.SetOutput(prev); log.SetFlags(flags) })
	v := env.LoadString("test", `(regpkg:call-hidden 'fail)`)
	require.Equal(t, lisp.LError, v.Type)
	var stack strings.Builder
	elpsutil.DumpErrorStack(&stack, lisp.GoError(v))
	assert.Contains(t, stack.String(), "regpkg:hidden")
	assert.NotContains(t, logs.String(), "GetFunName")
}

// A name that the package binds, or that a builtin is registered under,
// is refused, so one name never means two builtins.
func TestRegisterBuiltinsRefusesTakenNames(t *testing.T) {
	env, _ := hiddenEnv(t)
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String("regpkg"))))
	assert.PanicsWithValue(t, "symbol already defined: reg-fn", func() {
		env.RegisterBuiltins(constBuiltin("reg-fn", 1))
	})
	assert.PanicsWithValue(t, "builtin already registered: hidden", func() {
		env.RegisterBuiltins(constBuiltin("hidden", 1))
	})
	// The name of a bound builtin that was rebound to a lambda is still
	// registered.
	evalOK(t, env, `(set 'reg-fn (lambda (x) 0))`)
	assert.PanicsWithValue(t, "symbol already defined: reg-fn", func() {
		env.RegisterBuiltins(constBuiltin("reg-fn", 1))
	})
}

// A later bound registration of the name replaces the unbound one, as a
// later registration always does, and the replaced value is neither
// registered nor published.  Before #907 a registration saw a shadowing
// only through the package binding, which an unbound builtin does not
// have, so both values stayed published.
func TestRegisterBuiltinsReplacedByBinding(t *testing.T) {
	source, hidden := hiddenEnv(t)
	policy := lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true })
	lazy, err := lisp.NewTemplate(source, policy)
	require.NoError(t, err)
	vm, err := lazy.NewVM()
	require.NoError(t, err)
	for _, c := range []struct {
		env  *lisp.LEnv
		name string
	}{{source, "cold"}, {vm, "lazy VM"}} {
		t.Run(c.name, func(t *testing.T) {
			reg := c.env.Runtime.Registry
			old := reg.RegisteredBuiltin("regpkg", "hidden")
			require.NoError(t, lisp.GoError(c.env.InPackage(lisp.String("regpkg"))))
			c.env.AddBuiltins(false, constBuiltin("hidden", 5))
			require.NoError(t, lisp.GoError(c.env.InPackage(lisp.String(lisp.DefaultUserPackage))))
			requireNotRegistered(t, reg, old)
			if c.env == source {
				assert.False(t, lisp.RegistryHolds(source, hidden), "the registry keeps the replaced value")
			}
			cur := reg.RegisteredBuiltin("regpkg", "hidden")
			requireName(t, reg, cur, "regpkg", "hidden")
			assert.Equal(t, "5", evalOK(t, c.env, `(regpkg:hidden 0)`))
			asked := 0
			_, err := lisp.NewTemplate(c.env, lisp.TemplateWithBuiltinPolicy(func(v *lisp.LVal) bool {
				if v.Native == old.Native {
					asked++
				}
				return true
			}))
			require.NoError(t, err)
			assert.Zero(t, asked, "the policy was asked about the replaced value")
		})
	}
}

// A template publishes the builtin, a policy approves it by identity, and
// every kind of VM has its own value of it, which the macro calls.
func TestRegisterBuiltinsTemplate(t *testing.T) {
	source, hidden := hiddenEnv(t)
	_, err := lisp.NewTemplate(source, lisp.TemplateWithBuiltinPolicy(func(v *lisp.LVal) bool {
		return v.Native != hidden.Native
	}))
	require.Error(t, err, "the policy was not asked about hidden")
	assert.Contains(t, err.Error(), "registered builtin regpkg:hidden")

	policy := lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true })
	eager, err := lisp.NewTemplate(source, policy, lisp.TemplateWithEagerInstantiation())
	require.NoError(t, err)
	lazy, err := lisp.NewTemplate(source, policy)
	require.NoError(t, err)
	for _, c := range []struct {
		tmpl *lisp.Template
		name string
		opts []lisp.VMOption
	}{
		{eager, "eager", nil},
		{lazy, "lazy", nil},
		{lazy, "lazy prewarmed", []lisp.VMOption{lisp.VMWithPrewarm()}},
	} {
		t.Run(c.name, func(t *testing.T) {
			vm, err := c.tmpl.NewVM(c.opts...)
			require.NoError(t, err)
			reg := vm.Runtime.Registry
			own := reg.RegisteredBuiltin("regpkg", "hidden")
			require.NotNil(t, own)
			requireName(t, reg, own, "regpkg", "hidden")
			requireName(t, reg, hidden, "regpkg", "hidden")
			assert.Equal(t, "hidden", vm.GetFunName(own))
			_, bound := reg.Package("regpkg").Symbol("hidden")
			assert.False(t, bound)
			assert.Equal(t, "8", evalOK(t, vm, `(regpkg:call-hidden 7)`))
			v := vm.LoadString("test", `(regpkg:hidden 1)`)
			assert.Equal(t, lisp.LError, v.Type)
		})
	}
}
