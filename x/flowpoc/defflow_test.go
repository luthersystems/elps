package flowpoc

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// Stub storage: records are sorted-maps; nothing touches a ledger.
const stubs = `
(in-package 'flow)
(export 'goto 'vars 'var 'done 'machine)
(defun goto (label kind req opts vars)
  (sorted-map "goto" label "kind" kind "req" req "vars" vars))
(defun vars (&rest kv) (apply sorted-map kv))
(defun var (vs name) (get vs name))
(defun done (&rest kv) (sorted-map "done" (apply sorted-map kv)))
(defun machine (name opts clauses saved)
  (sorted-map "name" name "clauses" clauses "saved" saved))
(in-package 'user)
`

const kycFlow = `
(defun kyc-check (applicant) (list "EQUIFAX" applicant))
(set 'nexp 0)
(defmacro decided-by (who) (set! nexp (+ nexp 1)) (quasiquote (sorted-map "decided_by" (unquote who))))
(defflow kyc (applicant) (:msp "Org1MSP")
  (let ([result (await-reply 'check (kyc-check applicant) :retries 2)])
    (if (equal? (get result "status") "clear")
      (done :outcome "approved" :result (decided-by "auto"))
      (let ([d (await-input 'approval :from-msp "Org1MSP")])
        (done :outcome (if (get d "approved") "approved" "rejected")
              :result (decided-by (get d "reviewer")))))))
`

const signoffFlow = `
(defflow signoff (req)
  (let* ([quorum (get req "quorum")]
         [approved ()]
         [outcome ()])
    (while (and (< (length approved) quorum) (not outcome))
      (let* ([v (await-input 'vote :from-msp-in (get req "approvers"))]
             [who (get v "$from_msp")])
        (if (equal? (get v "decision") "reject")
          (set! outcome "rejected")
          (set! approved (cons who approved)))))
    (if outcome
      (done :outcome outcome)
      (progn (await-reply 'record (list "DMS" (get req "doc_ref") approved))
             (done :outcome "approved" :result (sorted-map "approved_by" approved))))))
`

func flowEnv(t testing.TB) *lisp.LEnv {
	env := newEnvLib(t, false) // no stdlib: libtesting holds a mutable native
	load(t, env, stubs)
	env.AddMacros(true, elpsutil.Function("defflow", lisp.Formals("name", "params", lisp.VarArgSymbol, "body"), Macro))
	return env
}

// run drives a flow: start, then feed each scripted message to whichever
// label the machine is waiting on.  Returns the done map and labels visited.
func run(t *testing.T, env *lisp.LEnv, flow string, startArg string, msgs []string) (*lisp.LVal, []string) {
	t.Helper()
	m := load(t, env, flow)
	env.Runtime.Package.Put(lisp.Symbol("$m"), m)
	rec := load(t, env, `(funcall (get (get $m "clauses") "start") `+startArg+`)`)
	var visited []string
	for _, msg := range msgs {
		require.Equal(t, lisp.LSortMap, rec.Type)
		env.Runtime.Package.Put(lisp.Symbol("$rec"), rec)
		label := load(t, env, `(get $rec "goto")`)
		require.Equal(t, lisp.LString, label.Type, "expected a wait, got %v", rec)
		visited = append(visited, label.Str)
		t.Logf("wait %s saved=%v", label.Str, load(t, env, `(keys (get $rec "vars"))`))
		rec = load(t, env, `(funcall (get (get $m "clauses") (get $rec "goto")) `+msg+` (get $rec "vars"))`)
	}
	env.Runtime.Package.Put(lisp.Symbol("$rec"), rec)
	return load(t, env, `(get $rec "done")`), visited
}

func TestKYC(t *testing.T) {
	env := flowEnv(t)
	d, v := run(t, env, `(progn `+kycFlow+` kyc)`, `"alice"`, []string{
		`(sorted-map "status" "refer")`,
		`(sorted-map "approved" true "reviewer" "bob")`,
	})
	require.Equal(t, []string{"check", "approval"}, v)
	require.Equal(t, `(sorted-map ':outcome "approved" ':result (sorted-map "decided_by" "bob"))`, d.String())
	t.Logf("expansion:\n%v", load(t, env, `(macroexpand '(defflow k2 (a) (let ([x (await-input 'l1)]) (done :x x a))))`))
}

func TestSignoff(t *testing.T) {
	env := flowEnv(t)
	d, v := run(t, env, `(progn `+signoffFlow+` signoff)`,
		`(sorted-map "quorum" 2 "approvers" '("A" "B" "C") "doc_ref" "d1")`, []string{
			`(sorted-map "decision" "approve" "$from_msp" "A")`,
			`(sorted-map "decision" "approve" "$from_msp" "B")`,
			`(sorted-map "ok" true)`,
		})
	require.Equal(t, []string{"vote", "vote", "record"}, v)
	require.Equal(t, `(sorted-map ':outcome "approved" ':result (sorted-map "approved_by" '("B" "A")))`, d.String())
	saved := load(t, env, `(get signoff "saved")`)
	t.Logf("saved: %v", saved)
	t.Logf("joins: %v", load(t, env, `(list signoff--j1 signoff--j2)`))
}

// TestNoReexpansion: clause lambdas are pre-expanded at defflow time, so
// calling a clause 10k times expands the user macro zero more times; and a
// template fork runs the compiled clauses without recompiling.
func TestNoReexpansion(t *testing.T) {
	env := flowEnv(t)
	load(t, env, kycFlow)
	n0 := load(t, env, `nexp`).Int
	load(t, env, `(dotimes (i 10000)
	  (funcall (get (get kyc "clauses") "approval") (sorted-map "approved" true "reviewer" "x") (sorted-map)))`)
	n1 := load(t, env, `nexp`).Int
	t.Logf("expansions at defflow: %d; during 10000 clause calls: %d", n0, n1-n0)
	require.Equal(t, 0, n1-n0)

	tpl, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	require.NoError(t, err)
	vm, err := tpl.NewVM()
	require.NoError(t, err)
	d := load(t, vm, `(get (funcall (get (get kyc "clauses") "approval") (sorted-map "approved" false "reviewer" "y") (sorted-map)) "done")`)
	require.Contains(t, d.String(), `"rejected"`)
	require.Equal(t, n1, load(t, vm, `nexp`).Int, "fork recompiled or re-expanded")
}
