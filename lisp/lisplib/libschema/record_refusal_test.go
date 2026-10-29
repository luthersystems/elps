// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// The runtime refusals of luthersystems/elps#908.  The record lints
// record-set-outside-method, record-check-writes and
// record-defmethod-not-toplevel report code that the runtime now refuses
// with record-error.  Every case runs on a cold-loaded VM and on each kind
// of template VM (eager, lazy, prewarmed), and the outcome must be the same
// on each: a refusal that only one kind gave would split a consensus.

// refusalSetup is the program every case runs against.
const refusalSetup = `
(set 'counter
  (s:make-record "counter"
    (s:has-key "n" s:int)
    :check (lambda (o) (when (< (o 'n) 0) (error 'wrong-type "negative")))
    (s:method get (o) (o 'n))
    (s:method inc (o) (s:set-field! o 'n (+ (o 'n) 1)) (o 'n))
    (s:method inc-in-lambda (o)
      (funcall (lambda () (s:set-field! o 'n (+ (o 'n) 1))))
      (o 'n))
    (s:method set-other (o other) (s:set-field! other 'n 9))
    (s:method writer (o) (lambda () (s:set-field! o 'n 100)))
    (s:method twice (o) (o 'inc) (o 'inc))
    (s:method add-method (o) (s:defmethod counter added (r) 1))))
(s:defmethod counter dec (o) (s:set-field! o 'n (- (o 'n) 1)) (o 'n))
(defun outside (o) (s:set-field! o 'n 5))
(s:defmethod counter via-helper (o) (outside o) (o 'n))
(set 'c (counter :n 0))

(set 'check-writes
  (s:make-record "check-writes"
    (s:has-key "n" s:int)
    :check (lambda (o) (when (> (o 'n) 0) (s:set-field! o 'n 0)))
    (s:method bump (o) (s:set-field! o 'n (+ (o 'n) 1)))))
(set 'check-calls-writer
  (s:make-record "check-calls-writer"
    (s:has-key "n" s:int)
    :check (lambda (o) (o 'reset))
    (s:method reset (o) (s:set-field! o 'n 0))))
(set 'check-reads
  (s:make-record "check-reads"
    (s:has-key "n" s:int)
    :check (lambda (o) (when (> (o 'get) 9) (error 'wrong-type "big")))
    (s:method get (o) (o 'n))
    (s:method bump (o) (s:set-field! o 'n (+ (o 'n) 1)) (o 'n))))
(set 'cw (check-writes :n 0))

(defun late () (s:defmethod counter late (o) 1))
`

// refusalCase is one expression and its printed outcome: the value, or
// "error:" and the condition.
type refusalCase struct {
	name, src, want string
}

var refusalCases = []refusalCase{
	// record-set-outside-method.
	{"set at top level", `(s:set-field! c 'n 5)`, "error:record-error"},
	{"set in a defun", `(outside c)`, "error:record-error"},
	{"set in an escaped lambda", `(funcall (c 'writer))`, "error:record-error"},
	{"set another object", `(c 'set-other (counter :n 1))`, "error:record-error"},
	{"refused set leaves the object", `(progn (ignore-errors (s:set-field! c 'n 5)) (c 'n))`, "0"},
	{"set in a method", `(c 'inc)`, "1"},
	{"set in s:defmethod", `(progn (c 'inc) (c 'dec))`, "0"},
	{"set in a lambda the method calls", `(c 'inc-in-lambda)`, "1"},
	{"set in a helper the method calls", `(c 'via-helper)`, "5"},
	{"nested method calls", `(c 'twice)`, "2"},
	{"set the receiver of another call", `(c 'set-other c)`, "9"},

	// record-check-writes.
	{"set in :check", `(check-writes :n 1)`, "error:record-error"},
	{"set in :check after a method", `(cw 'bump)`, "error:record-error"},
	{"refused :check write rolls back", `(progn (ignore-errors (cw 'bump)) (cw 'n))`, "0"},
	{"set in a method :check calls", `(check-calls-writer :n 1)`, "error:record-error"},
	{"read in :check", `(check-reads :n 1)`, "#<user:check-reads>"},
	{"method call with a reading :check", `((check-reads :n 1) 'bump)`, "2"},
	{"set in a validated :check", `(s:validate check-writes cw)`, "()"},

	// record-defmethod-not-toplevel.
	{"defmethod in a defun", `(late)`, "error:record-error"},
	{"defmethod in a lambda", `(funcall (lambda () (s:defmethod counter z (o) 1)))`, "error:record-error"},
	{"defmethod in a map callback", `(map 'list (lambda (x) (s:defmethod counter z (o) x)) '(1))`, "error:record-error"},
	{"defmethod in flet", `(flet ([f () (s:defmethod counter z (o) 1)]) (f))`, "error:record-error"},
	{"defmethod in a method", `(c 'add-method)`, "error:record-error"},
	{"defmethod in a handler", `(handler-bind ((x (lambda (&rest _)
		(handler-bind ((record-error (lambda (c &rest _) c))) (s:defmethod counter z (o) 1)))))
	  (error 'x "boom"))`, "'record-error"},
	// The refusal comes before an error in the signature.
	{"bad signature at top level", `(s:defmethod counter bad (o [x 5]) 1)`, "error:bad-arguments"},
	{"bad signature in a lambda", `(funcall (lambda () (s:defmethod counter bad (o [x 5]) 1)))`, "error:record-error"},
	{"refused defmethod adds nothing", `(progn (ignore-errors (late)) (ignore-errors (c 'late)))`, "()"},
	{"defmethod at top level", `(progn (s:defmethod counter top (o) 7) (c 'top))`, "7"},
	{"defmethod in let", `(let ([k 3]) (s:defmethod counter k3 (o) k) (c 'k3))`, "3"},
	{"defmethod in progn and when", `(progn (when true (s:defmethod counter w (o) 4)) (c 'w))`, "4"},
	{"defmethod in a call argument", `(progn (list (s:defmethod counter arg (o) 5)) (c 'arg))`, "5"},
	{"defmethod in flet body", `(flet ([f () 6]) (s:defmethod counter fb (o) (f)) (c 'fb))`, "6"},
	{"defmethod in load-string", `(progn (load-string "(s:defmethod counter ls (o) 8)") (c 'ls))`, "8"},
	{"defmethod from a top-level macro call", `(progn
	  (defmacro defm (n) (quasiquote (s:defmethod counter (unquote n) (o) 11)))
	  (defm mm)
	  (c 'mm))`, "11"},
	{"defmethod in ignore-errors", `(progn (ignore-errors (s:defmethod counter ie (o) 9)) (c 'ie))`, "9"},

	// The condition is a child of error.
	{"handler-bind on error", `(handler-bind ((error (lambda (c &rest _) c))) (s:set-field! c 'n 5))`, "'record-error"},
	{"handler-bind on record-error", `(handler-bind ((record-error (lambda (c &rest _) c))) (late))`, "'record-error"},
	{"condition-is?", `(condition-is? 'record-error 'error)`, "true"},
}

// refusalVM makes one VM of a kind with refusalSetup loaded.
type refusalVM struct {
	name string
	make func(t *testing.T) *lisp.LEnv
}

func loadRefusalSetup(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := newSchemaForkEnv(t)
	mustLoad(t, env, "setup.lisp", refusalSetup)
	return env
}

// refusalVMs returns the cold-loaded VM and the three kinds of template VM.
// The prewarmed VMs come after other VMs of the same template have read the
// program's globals, so prewarm has a set of values to build.
func refusalVMs(t *testing.T) []refusalVM {
	t.Helper()
	lazy := schemaTemplate(t, loadRefusalSetup(t))
	eager, err := lisp.NewTemplate(loadRefusalSetup(t),
		lisp.TemplateWithEagerInstantiation(),
		lisp.TemplateWithBuiltinPolicy(func(v *lisp.LVal) bool { return v.Builtin() != nil }))
	require.NoError(t, err)
	warm := schemaTemplate(t, loadRefusalSetup(t))
	for range 4 {
		vm, err := warm.NewVM()
		require.NoError(t, err)
		mustLoad(t, vm, "warm.lisp", `(list c counter check-writes cw outside late)`)
	}
	newVM := func(tmpl *lisp.Template, opts ...lisp.VMOption) func(t *testing.T) *lisp.LEnv {
		return func(t *testing.T) *lisp.LEnv {
			vm, err := tmpl.NewVM(opts...)
			require.NoError(t, err)
			return vm
		}
	}
	return []refusalVM{
		{"cold", loadRefusalSetup},
		{"eager", newVM(eager)},
		{"lazy", newVM(lazy)},
		{"prewarmed", newVM(warm, lisp.VMWithPrewarm())},
	}
}

// refusalOutcome evaluates src and prints its outcome.
func refusalOutcome(env *lisp.LEnv, src string) string {
	r := env.LoadString("case.lisp", src)
	if r.Type == lisp.LError {
		return "error:" + r.Str
	}
	return r.String()
}

func TestRecordRefusals(t *testing.T) {
	vms := refusalVMs(t)
	for _, tc := range refusalCases {
		t.Run(tc.name, func(t *testing.T) {
			for _, vm := range vms {
				assert.Equal(t, tc.want, refusalOutcome(vm.make(t), tc.src), vm.name)
			}
		})
	}
}

// TestRecordRefusalMessages pins the text of each refusal, which names the
// type and the rule.
func TestRecordRefusalMessages(t *testing.T) {
	cases := []struct{ src, want string }{
		{`(s:set-field! c 'n 5)`, "no method of user:counter runs on the object; change the field in a method of the type"},
		{`(check-writes :n 1)`, "the :check of user:check-writes runs on the object; :check must only read the object"},
		{`(late)`, "s:defmethod: method late of user:counter is added in a function call, not at load; write s:defmethod at the top level of a file"},
	}
	for _, vm := range refusalVMs(t) {
		for _, tc := range cases {
			r := vm.make(t).LoadString("case.lisp", tc.src)
			require.Equal(t, lisp.LError, r.Type, "%s %s", vm.name, tc.src)
			assert.Contains(t, (*lisp.ErrorVal)(r).ErrorMessage(), tc.want, "%s %s", vm.name, tc.src)
		}
	}
}

// refusalStepsGolden pins the steps of the allowed paths, as measured on
// origin/main before the refusals existed.  The refusals are Go checks that
// charge no step, so these must not change.  A change to any number is a
// consensus change for every embedder that meters steps.
var refusalStepsGolden = []struct {
	src   string
	steps int64
}{
	{`(c 'get)`, 14},
	{`(c 'inc)`, 24},
	{`(c 'dec)`, 24},
	{`(c 'inc-in-lambda)`, 28},
	{`(c 'via-helper)`, 22},
	{`(c 'twice)`, 43},
	{`(counter :n 1)`, 12},
	{`(check-reads :n 1)`, 15},
	{`((check-reads :n 1) 'bump)`, 41},
	{`(s:defmethod counter top (o) :replace true 7)`, 10},
	{`(let ([k 3]) (s:defmethod counter k3 (o) :replace true k))`, 13},
}

func TestRecordRefusalStepsGolden(t *testing.T) {
	for _, vm := range refusalVMs(t) {
		env := vm.make(t)
		for _, tc := range refusalStepsGolden {
			r := env.LoadStringContext(context.Background(), "steps.lisp", tc.src)
			require.NotEqual(t, lisp.LError, r.Type, "%s %s: %v", vm.name, tc.src, r)
			assert.Equal(t, tc.steps, env.Runtime.Steps(), "%s %s", vm.name, tc.src)
		}
	}
}

// The benchmarks of the paths the refusals check.  Compare them with
// benchstat against origin/main: the checks are a load and a type test on
// the object's call state, and s:defmethod reads the call stack once.
const refusalBenchSrc = `
(set 'counter
  (s:make-record "counter"
    (s:has-key "n" s:int)
    (s:method get (o) (o 'n))
    (s:method inc (o) (s:set-field! o 'n (+ (o 'n) 1)))))
(set 'checked
  (s:make-record "checked"
    (s:has-key "n" s:int)
    :check (lambda (o) (o 'n))
    (s:method inc (o) (s:set-field! o 'n (+ (o 'n) 1)))))
(set 'c (counter :n 0))
(set 'k (checked :n 0))
`

func benchRecordCall(b *testing.B, obj, op string) {
	env := newSchemaForkEnvB(b)
	if r := env.LoadString("bench.lisp", refusalBenchSrc); r.IsError() {
		b.Fatal(r)
	}
	fn := env.GetGlobal(lisp.Symbol(obj))
	args := lisp.QExpr([]*lisp.LVal{lisp.Symbol(op)})
	b.ReportAllocs()
	for b.Loop() {
		if r := env.FunCall(fn, args); r.IsError() {
			b.Fatal(r)
		}
	}
}

// BenchmarkRecordMethodRead is a method call that writes nothing.
func BenchmarkRecordMethodRead(b *testing.B) { benchRecordCall(b, "c", "get") }

// BenchmarkRecordMethodSet is a method call with one s:set-field!.
func BenchmarkRecordMethodSet(b *testing.B) { benchRecordCall(b, "c", "inc") }

// BenchmarkRecordMethodSetCheck is BenchmarkRecordMethodSet on a type with
// a :check, which the call runs.
func BenchmarkRecordMethodSetCheck(b *testing.B) { benchRecordCall(b, "k", "inc") }

// BenchmarkRecordDefmethod is a top-level s:defmethod that replaces a
// method.
func BenchmarkRecordDefmethod(b *testing.B) {
	env := newSchemaForkEnvB(b)
	if r := env.LoadString("bench.lisp", refusalBenchSrc); r.IsError() {
		b.Fatal(r)
	}
	b.ReportAllocs()
	for b.Loop() {
		if r := env.LoadString("defmethod.lisp", `(s:defmethod counter m (o) :replace true 1)`); r.IsError() {
			b.Fatal(r)
		}
	}
}
