// Copyright © 2026 The ELPS authors

package elpstest_test

import (
	"context"
	"os"
	"os/exec"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/internal/templatepolicy"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libtesting"
)

// Each test here is a fork bug that shipped, written as the ForkCheck
// that fails on the tree it shipped in and passes on the tree that fixed
// it.  The program is the shape that reached the bug; the transactions
// are what a caller would run on a fork and observe diverging from a
// cold load.

// Issue #576: `(quasiquote (unquote a))` yields a second header on a's
// sorted-map.  Fork memoised copies per header, so the fork's a and b
// were two maps, and a write through one was invisible through the other
// — on the fork only.  Fixed in #587.
func TestForkCheck_SortedMapAliasAcrossHeaders(t *testing.T) {
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		Program: `
(set 'a (sorted-map "k" 1))
(set 'b (quasiquote (unquote a)))
(set 'both (list a b))
`,
		Tx: []string{
			`(assoc! a "y" 7) (get b "y")`,
			`(dissoc! b "k") (get a "k")`,
			`(assoc! (first both) "z" 1) (list (get (second both) "z") (get a "z"))`,
		},
	})
}

// Issue #576, second payload kind: the same two-header shape over a bytes
// value, which append! grows in place.  Fixed in #587.
func TestForkCheck_BytesAliasAcrossHeaders(t *testing.T) {
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		Program: `
(set 'a (to-bytes "abc"))
(set 'b (quasiquote (unquote a)))
`,
		Tx: []string{
			`(append! a 7) (length b)`,
			`(append! b 1 2) (length a)`,
		},
	})
}

// Per-VM host state needs no copying protocol: it is created after Fork.
type setupCounter struct{ n int }

// Native handles are bound per VM, not cloned from a template. Two Lisp
// headers must still address one accumulator, and mutations must stay local.
func TestForkCheck_NativeAliasAcrossHeaders(t *testing.T) {
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		Setup: func(env *lisp.LEnv) error {
			a := lisp.Native(&setupCounter{})
			b := *a // a second header over the same payload
			if rc := env.PutGlobal(lisp.Symbol("a"), a); rc.Type == lisp.LError {
				return lisp.GoError(rc)
			}
			if rc := env.PutGlobal(lisp.Symbol("b"), &b); rc.Type == lisp.LError {
				return lisp.GoError(rc)
			}
			bump := lisp.FunInPackage(lisp.DefaultUserPackage, "counter-bump", lisp.Formals("value"), func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				counter := args.Cells[0].Native.(*setupCounter)
				counter.n++
				return lisp.Int(counter.n)
			})
			if rc := env.PutGlobal(lisp.Symbol("counter-bump"), bump); rc.Type == lisp.LError {
				return lisp.GoError(rc)
			}
			return nil
		},
		Tx: []string{`(list (counter-bump a) (counter-bump b))`, `(list (counter-bump b) (counter-bump a))`},
	})
}

// These subprocesses intentionally misuse Setup or expose a shared host
// result. Opaque contents and results match, so only cross-VM identity checks
// can detect the violation. Neither payload implements NativeCloner.
func TestForkCheckRejectsSharedRequestNative(t *testing.T) {
	const childFlag = "ELPS_FORKCHECK_SHARED_NATIVE_CHILD"
	if mode := os.Getenv(childFlag); mode != "" {
		shared := &setupCounter{}
		// Dropping the setup handle during execution makes the pre-transaction
		// check necessary; the transaction case conversely needs the post-check.
		check := elpstest.ForkCheck{Tx: []string{`(set 'a ())`}}
		check.Setup = func(env *lisp.LEnv) error {
			if mode == "setup" {
				return lisp.GoError(env.PutGlobal(lisp.Symbol("a"), lisp.Native(shared)))
			}
			if mode == "annotation" || mode == "nested-annotation" {
				value := lisp.String("same result")
				value.Native = shared
				if mode == "nested-annotation" {
					value = lisp.QExpr([]*lisp.LVal{value})
					value.SealAST()
				}
				return lisp.GoError(env.PutGlobal(lisp.Symbol("a"), value))
			}
			fn := lisp.FunInPackage(lisp.DefaultUserPackage, "host-result", lisp.Formals(), func(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal { return lisp.Native(shared) })
			return lisp.GoError(env.PutGlobal(lisp.Symbol("host-result"), fn))
		}
		if mode == "transaction" {
			check.Tx = []string{`(set 'a (host-result))`}
		}
		elpstest.RunForkCheck(t, check)
		return
	}
	for _, mode := range []string{"setup", "transaction", "annotation", "nested-annotation"} {
		t.Run(mode, func(t *testing.T) {
			executable, err := os.Executable()
			if err != nil {
				t.Fatal(err)
			}
			ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
			defer cancel()
			// #nosec G204 -- os.Executable returns this test binary and the selector is fixed, not user-controlled.
			command := exec.CommandContext(ctx, executable, "-test.run=^TestForkCheckRejectsSharedRequestNative$")
			command.Env = append(os.Environ(), childFlag+"="+mode)
			output, err := command.CombinedOutput()
			if err == nil || !strings.Contains(string(output), "mutable payload(s) shared with tx[0] cold") {
				t.Fatalf("oracle did not identify cross-VM native sharing: error=%v\n%s", err, output)
			}
		})
	}
}

type immutableSetupValue struct{ n int }

func TestForkCheckAllowsDeclaredSharedSetupNative(t *testing.T) {
	shared := &immutableSetupValue{n: 7}
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		SharedSetupNative: func(value any) bool { return value == shared },
		Setup: func(env *lisp.LEnv) error {
			return lisp.GoError(env.PutGlobal(lisp.Symbol("constant"), lisp.Native(shared)))
		},
		Tx: []string{`constant`, `constant`},
	})
	if shared.n != 7 {
		t.Fatal("shared immutable setup value changed")
	}
}

type immutableCloningValue struct {
	templatepolicy.Marker
	n int
}

func (immutableCloningValue) CloneNative() any { panic("immutable values must not be cloned") }

type foreignImmutableCloningValue struct{ n int }

func (*foreignImmutableCloningValue) CloneNative() any {
	panic("policy-approved immutable must not be cloned")
}

func TestForkCheckAllowsImmutableNativeWithCloneMethod(t *testing.T) {
	for _, payload := range []any{immutableCloningValue{n: 7}, &immutableCloningValue{n: 7}, &foreignImmutableCloningValue{n: 7}} {
		elpstest.RunForkCheck(t, elpstest.ForkCheck{
			NewEnv: func() (*lisp.LEnv, error) {
				env, err := elpstest.NewForkCheckEnv()
				if err != nil {
					return nil, err
				}
				if rc := env.PutGlobal(lisp.Symbol("constant"), lisp.Native(payload)); rc.Type == lisp.LError {
					return nil, lisp.GoError(rc)
				}
				return env, nil
			},
			TemplateOptions: []lisp.TemplateOption{
				lisp.TemplateWithBuiltinPolicy(func(v *lisp.LVal) bool { return v.Builtin() != nil }),
				lisp.TemplateWithNativePolicy(func(value any) bool {
					switch value.(type) {
					case *immutableCloningValue, *foreignImmutableCloningValue:
						return value == payload
					default:
						return false
					}
				}),
			},
			Tx: []string{`constant`},
		})
	}
}

// Re-publication would hide mutations of plan-owned storage between requests.
// Count admission of an actual source builtin, not construction of child plans.
func TestForkCheckPublishesBaseTemplateOnce(t *testing.T) {
	var sourceBuiltin *lisp.LVal
	baseApprovals := 0
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		NewEnv: func() (*lisp.LEnv, error) {
			env, err := elpstest.NewForkCheckEnv()
			if err != nil {
				return nil, err
			}
			if sourceBuiltin == nil {
				sourceBuiltin = env.Get(lisp.Symbol("+"))
				if sourceBuiltin.Type != lisp.LFun {
					t.Fatalf("fixture builtin: %v", sourceBuiltin)
				}
			}
			return env, nil
		},
		TemplateOptions: []lisp.TemplateOption{lisp.TemplateWithBuiltinPolicy(func(v *lisp.LVal) bool {
			if v == sourceBuiltin {
				baseApprovals++
			}
			return v.Builtin() != nil
		})},
		Program: `(set 'state (sorted-map "n" 0))`,
		Tx:      []string{`(assoc! state "n" 1)`, `(assoc! state "n" 2)`},
	})
	if baseApprovals != 1 {
		t.Fatalf("base source admitted %d times, want one reusable template", baseApprovals)
	}
}

// Issue #579: a libschema validator minted on the template stopped being
// a validator in a fork, because its credential was the identity of a
// marker cell the fork had copied.  Fixed in #581.  The failing
// validation is included so an error stays an error of the same kind.
func TestForkCheck_SchemaValidatorCredential(t *testing.T) {
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		Program: `
(s:deftype "T" s:int)
(set 'anon (s:make-validator "Anon" s:int (s:gt 1)))
`,
		Tx: []string{
			`(s:validate T 3)`,
			`(s:validate anon 3)`,
			`(s:validate T "nope")`,
			`(s:deftype "U" s:string) (s:validate U "x")`,
			`(s:validate (s:make-validator "Fresh" s:string) "x")`,
		},
	})
}

// Issue #381 (fixed in #581): a fork shared the template's lisp testing
// suite, a Go accumulator held as a native global, so a test registered
// on a fork landed in the template.  The suite is an opaque native to the
// state and isolation oracles (rendered by type; no identity unless it
// is a NativeCloner, which the fix made it).  Parity sees the share
// because registering a name the template's suite already holds is an
// error: both transactions register "one", so on a shared suite the
// second fork to run it fails where the cold environment does not.
func TestForkCheck_TestingSuitePerFork(t *testing.T) {
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		Setup: func(env *lisp.LEnv) error {
			if rc := libtesting.LoadPackage(env); rc.Type == lisp.LError {
				return lisp.GoError(rc)
			}
			return lisp.GoError(env.LoadString("setup.lisp", `(use-package 'testing)`))
		},
		Tx: []string{
			`(test "one" (assert-equal 1 1))`,
			`(test "one" (assert-equal 2 2))`,
		},
	})
}

// Closure-captured state: the state a fork must copy and a transaction
// mutates through the closure, invisible from the package bindings except
// through the function.  A walker that stopped at the function header
// would pass a fork that shared the captured environment.
func TestForkCheck_ClosureState(t *testing.T) {
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		Program: `
(let ([outer (vector 0)] [box (sorted-map "n" 0)])
  (defun bump! () (append! outer 1) (assoc! box "n" (+ 1 (get box "n"))) ())
  (defun peek () (list (length outer) (get box "n"))))
(set 'shared (sorted-map "k" 1))
(defun share-through-closure () shared)
`,
		Tx: []string{
			`(bump!)`,
			`(bump!) (bump!) (peek)`,
			`(assoc! (share-through-closure) "k" 2) (get shared "k")`,
		},
	})
}

// The shapes the existing fork tests already pin, run through the
// harness so a regression in any of them shows up here with the same
// diagnostics: closures over mutable state, macros, labels mutual
// recursion, nested maps, bytes.
func TestForkCheck_LoadedProgram(t *testing.T) {
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		Program: `
(set 'counter-box (vector 0))
(defun make-adder (n) (lambda (x) (+ x n)))
(set 'add2 (make-adder 2))
(defmacro with-logging (expr) (quasiquote (progn (unquote expr))))
(defun handler (m) (get-default m "k" (with-logging (add2 40))))
(labels ([even? (n) (if (= n 0) true (odd? (- n 1)))]
         [odd? (n) (if (= n 0) false (even? (- n 1)))])
  (set 'evens even?))
(set 'config (sorted-map "a" 1 "b" (vector 1 2 3) "inner" (sorted-map "n" 0)))
(set 'blob (to-bytes "mutable-bytes"))
(set 'e (list))
`,
		Tx: []string{
			`(handler (sorted-map "x" 1))`,
			`(funcall evens 10)`,
			`(append! counter-box 99) (assoc! config "a" 100) (assoc! (get config "inner") "n" 5) (list counter-box config)`,
			`(set 'fv (append 'vector e 'fork)) (nth fv 0)`,
			`(append! blob 33) (length blob)`,
		},
	})
}
