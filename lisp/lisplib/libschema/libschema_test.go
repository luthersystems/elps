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
// issue #271. libschema's s:deftype binds the constructed validator
// LFun as a global symbol (see builtinDefType). Resolving that symbol
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
	t.Run("deftype path", func(t *testing.T) {
		env := newSchemaEnv(t)

		if rc := env.LoadStringContext(context.Background(), "regression-271-deftype",
			`(s:deftype "Score" "int" (s:gte 0))`); rc.Type == lisp.LError {
			t.Fatalf("s:deftype: %v", rc)
		}

		// env.Get returns the bound LFun without going through funCall.
		fun := env.Get(lisp.Symbol("Score"))
		if fun.Type != lisp.LFun {
			t.Fatalf("Score did not resolve to an LFun: %v", fun.Type)
		}
		// Direct invariant check: the validator's Package must be
		// non-empty regardless of any downstream funCall behavior.
		if pkg := fun.Package(); pkg == "" {
			t.Fatalf("validator LFun has empty Package — issue #271 regression")
		}

		buf := captureLog(t)
		_ = env.FunCallContext(context.Background(), fun, lisp.QExpr([]*lisp.LVal{lisp.Int(5)}))
		if got := buf.String(); bugGetFunNameRe.MatchString(got) {
			t.Fatalf("unexpected BUG: GetFunName line during validator funCall:\n%s", got)
		}
	})

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
	pkgless := lisp.Fun("regression-271-positive-control", lisp.Formals("x"), //nolint:staticcheck // SA1019: intentional use of deprecated constructor — we are testing the detector for its empty-Package path
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
