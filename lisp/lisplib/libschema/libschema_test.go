package libschema_test

import (
	"bytes"
	"context"
	"log"
	"regexp"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

func TestPackage(t *testing.T) {
	r := &elpstest.Runner{}
	defer r.Close()
	r.RunTestFile(t, "libschema_test.lisp")
}

// TestMakeValidatorNeverWritesCallerPackage pins #736's rule for libschema: a
// prefixed library builtin must never write into the caller's package.
// s:deftype used to violate it -- it bound its validator as a global under
// the caller's own name, keyed off a string it never checked was already
// bound (env.Get on an LString always returned nil, so its own "Symbol
// already defined" guard never ran) -- and was removed for it. s:make-validator
// is the replacement and the sole entry point today; it must merely return the
// validator, leaving every binding decision (including a name that collides
// with a sealed package member, like "lisp:if" below) to the caller's own
// core `set`.
func TestMakeValidatorNeverWritesCallerPackage(t *testing.T) {
	for _, source := range []string{
		`(s:make-validator "lisp:if" s:int)`,
		`(in-package 'lisp) (s:make-validator "if" s:int)`,
		`(s:make-validator "untouched" s:int)`,
	} {
		t.Run(source, func(t *testing.T) {
			env := newSchemaEnv(t)
			got := env.LoadStringContext(context.Background(), "no-write.lisp", source)
			if got.Type == lisp.LError {
				t.Fatalf("s:make-validator must not itself error: %v", got)
			}
			// Nothing was bound anywhere: core `if` still resolves to the
			// sealed lisp package's own binding, and no global named
			// "untouched" exists in any package make-validator touched.
			got = env.LoadStringContext(context.Background(), "check.lisp", `(in-package 'user) (if true 'a 'b)`)
			if got.String() != "'a" {
				t.Errorf("core if must be untouched by make-validator: got %v, want 'a", got)
			}
			if fun := env.Get(lisp.Symbol("untouched")); !fun.IsNil() && fun.Type != lisp.LError {
				t.Errorf("s:make-validator must not bind a global: found %v", fun)
			}
		})
	}
}

// TestLenConstraintTypes pins the behaviour of the s:len* family across LType.
//
// The five constraints used to be five copies of the same type switch, each
// falling through silently for any type that is not a string, bytes or array.
// They now share constraintLen, which enumerates every LType so a new one has
// to make a choice. This test is the behavioural half of that guard: it fixes
// both the measured cases and -- importantly -- the "no length, so the
// constraint passes" cases, which include sorted-maps. If someone decides
// those should be measured, this test is what has to change first.
func TestLenConstraintTypes(t *testing.T) {
	cases := []struct {
		name  string
		def   string
		value string
		valid bool
	}{
		// Measured types: the constraint really does check.
		{"string len ok", `(set 'T (s:make-validator "T" s:string (s:len 3)))`, `"abc"`, true},
		{"string len wrong", `(set 'T (s:make-validator "T" s:string (s:len 3)))`, `"abcd"`, false},
		// "bytes" is not a make-validator base type, so bytes reach the
		// constraint through the "any" base.
		{"bytes len ok", `(set 'T (s:make-validator "T" "any" (s:len 3)))`, `(to-bytes "abc")`, true},
		{"bytes len wrong", `(set 'T (s:make-validator "T" "any" (s:len 3)))`, `(to-bytes "ab")`, false},
		{"array len ok", `(set 'T (s:make-validator "T" s:array (s:len 3)))`, `(vector 1 2 3)`, true},
		{"array len wrong", `(set 'T (s:make-validator "T" s:array (s:len 3)))`, `(vector 1 2)`, false},
		{"lengt ok", `(set 'T (s:make-validator "T" s:string (s:lengt 2)))`, `"abc"`, true},
		{"lengt too short", `(set 'T (s:make-validator "T" s:string (s:lengt 3)))`, `"abc"`, false},
		{"lengte ok", `(set 'T (s:make-validator "T" s:string (s:lengte 3)))`, `"abc"`, true},
		{"lengte too short", `(set 'T (s:make-validator "T" s:string (s:lengte 4)))`, `"abc"`, false},
		{"lenlt ok", `(set 'T (s:make-validator "T" s:string (s:lenlt 4)))`, `"abc"`, true},
		{"lenlt too long", `(set 'T (s:make-validator "T" s:string (s:lenlt 3)))`, `"abc"`, false},
		{"lenlte ok", `(set 'T (s:make-validator "T" s:string (s:lenlte 3)))`, `"abc"`, true},
		{"lenlte too long", `(set 'T (s:make-validator "T" s:string (s:lenlte 2)))`, `"abc"`, false},

		// Unmeasured types: no length, so the constraint passes whatever the
		// bound is. Locked in deliberately -- see constraintLen.
		{"int has no length", `(set 'T (s:make-validator "T" s:int (s:len 3)))`, `12345`, true},
		{"float has no length", `(set 'T (s:make-validator "T" "float" (s:len 3)))`, `1.5`, true},
		{"sorted-map has no length", `(set 'T (s:make-validator "T" s:sorted-map (s:len 3)))`, `(sorted-map "a" 1)`, true},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			env := newSchemaEnv(t)
			if rc := env.LoadStringContext(context.Background(), "len-def", c.def); rc.Type == lisp.LError {
				t.Fatalf("make-validator: %v", rc)
			}
			res := env.LoadStringContext(context.Background(), "len-validate",
				"(s:validate T "+c.value+")")
			isErr := res.Type == lisp.LError
			if c.valid && isErr {
				t.Fatalf("%s / %s: expected the constraint to pass, got error: %v", c.def, c.value, res)
			}
			if !c.valid && !isErr {
				t.Fatalf("%s / %s: expected the constraint to fail, got: %v", c.def, c.value, res)
			}
		})
	}
}

// newSchemaEnv bootstraps a complete env with the standard library
// (including libschema) loaded and the current package set to "user".
func newSchemaEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); !rc.IsNil() {
		t.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := lisplib.LoadLibrary(env); !rc.IsNil() {
		t.Fatalf("load-library: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); !rc.IsNil() {
		t.Fatalf("in-package: %v", rc)
	}
	return env
}

// captureLog redirects log.Default()'s output to a buffer for the
// duration of the test, restoring it on cleanup.
func captureLog(t *testing.T) *bytes.Buffer {
	t.Helper()
	var buf bytes.Buffer
	prevOutput := log.Writer()
	prevFlags := log.Flags()
	log.SetOutput(&buf)
	log.SetFlags(0)
	t.Cleanup(func() {
		log.SetOutput(prevOutput)
		log.SetFlags(prevFlags)
	})
	return &buf
}

// bugGetFunNameRe matches the specific BUG: log emitted by
// (*LEnv).GetFunName when it observes a package-less LFun. Anchored on
// the GetFunName prefix so unrelated future "BUG:" log messages in
// other code paths can't accidentally trip these regression checks.
var bugGetFunNameRe = regexp.MustCompile(`BUG: GetFunName:`)

// TestValidatorFunCallNoBugLog is the headline regression test for
// issue #271. libschema's s:make-validator returns a constructed validator
// LFun which the caller binds with core `set` (before #736, s:deftype bound
// it as a global symbol itself; see git history). Resolving that symbol
// and invoking it goes through env.funCall, which calls
// env.GetFunName(fun) before dispatch. Pre-fix the validator had an
// empty Package field, so GetFunName logged "BUG: ..." on every
// invocation. This test asserts the invariant directly (Package set on
// the validator LFun) and additionally asserts no BUG: GetFunName line
// appears on the default logger when the validator is funCall'd.
//
// We invoke the validator via env.FunCallContext directly (rather than
// via a Lisp `(Score ...)` expression) so the assertion is isolated to
// the funCall -> GetFunName path under test — it does not depend on
// libschema's internal argument-shape semantics.
func TestValidatorFunCallNoBugLog(t *testing.T) {
	t.Run("make-validator path", func(t *testing.T) {
		env := newSchemaEnv(t)

		// s:make-validator returns the validator instead of binding it.
		// Bind it ourselves so we can resolve and funCall via env.Get.
		if rc := env.LoadStringContext(context.Background(), "regression-271-mkv",
			`(set 'positive-int (s:make-validator "PositiveInt" "int" (s:gt 0)))`); rc.Type == lisp.LError {
			t.Fatalf("s:make-validator: %v", rc)
		}

		fun := env.Get(lisp.Symbol("positive-int"))
		if fun.Type != lisp.LFun {
			t.Fatalf("positive-int did not resolve to an LFun: %v", fun.Type)
		}
		if pkg := fun.Package(); pkg == "" {
			t.Fatalf("make-validator LFun has empty Package — issue #271 regression")
		}

		buf := captureLog(t)
		_ = env.FunCallContext(context.Background(), fun, lisp.QExpr([]*lisp.LVal{lisp.Int(1)}))
		if got := buf.String(); bugGetFunNameRe.MatchString(got) {
			t.Fatalf("unexpected BUG: GetFunName line during make-validator funCall:\n%s", got)
		}
	})
}

// TestGetFunNameBugLogStillFires is the positive control for
// TestValidatorFunCallNoBugLog. It constructs an LFun via the
// deprecated lisp.Fun (which intentionally leaves Package empty),
// binds it, invokes it, and asserts the BUG: GetFunName line *does*
// appear. This proves the safety-net log in (*LEnv).GetFunName is
// still armed — if a future refactor silently neutered the log (or
// the empty-Package error path in pkgFunName), this test would catch
// it and TestValidatorFunCallNoBugLog wouldn't go vacuously green.
func TestGetFunNameBugLogStillFires(t *testing.T) {
	env := newSchemaEnv(t)

	// Construct a package-less LFun via the deprecated constructor.
	// We intentionally use lisp.Fun (not FunInPackage) here — that's
	// the path whose detector we are verifying still fires.
	pkgless := lisp.Fun("regression-271-positive-control", lisp.Formals("x"), // deprecated constructor on purpose: we test the detector for its empty-Package path
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return lisp.Nil() })
	if pkgless.Package() != "" {
		t.Fatalf("test setup: expected package-less LFun, got Package=%q", pkgless.Package())
	}
	if rc := env.PutGlobal(lisp.Symbol("pkgless-victim"), pkgless); rc.Type == lisp.LError {
		t.Fatalf("put-global: %v", rc)
	}

	buf := captureLog(t)
	_ = env.FunCallContext(context.Background(), pkgless, lisp.QExpr([]*lisp.LVal{lisp.Int(0)}))
	if got := buf.String(); !bugGetFunNameRe.MatchString(got) {
		t.Fatalf("expected BUG: GetFunName line for package-less LFun but got none — safety net disarmed?\ncaptured output:\n%s", got)
	}
}
