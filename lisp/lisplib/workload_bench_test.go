// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"bytes"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

// BenchmarkWorkload measures realistic end-to-end programs rather than a
// single evaluator path.  Most other benchmarks in the repository are
// micro-benchmarks (one special operator, one builtin, one fork); these exist
// so an allocation or time change on the evaluator's hot paths can be judged
// by what it does to whole programs of the shapes embedders actually run:
//
//   - json:      decode a 60-item document, select, rebuild maps, re-encode.
//   - sortedmap: build and fold sorted-map indexes (assoc!, get-default,
//     foldl, dissoc!).
//   - string:    split, case-fold, trim, prefix tests and joins over 80 lines.
//   - recursion: closures with set!, compose, a depth-8 tree built and summed
//     with foldl, and a labels Ackermann.
//   - template:  per-request shape -- Template.NewVM plus LoadString of a
//     three-call JSON transaction against an application package.
//
// The eval workloads parse their call form once and evaluate it on every
// iteration; each iteration's result is compared with the first so a
// workload that stops doing its work fails instead of getting faster.

const wlJSONProgram = `
(set 'doc-src
  (let ((items (map 'vector
                    (lambda (i)
                      (sorted-map "id" i
                                  "name" (format-string "item-{}" i)
                                  "price" (* i 3)
                                  "tags" (vector "a" "b" (to-string i))
                                  "owner" (sorted-map "first" "Ada" "last" "L" "age" i)))
                    (make-sequence 0 60))))
    (json:dump-string (sorted-map "items" items "count" 60 "version" "1.2.3"))))

(defun json-pipeline ()
  (let* ((doc (json:load-string doc-src :exact-integers true))
         (items (get doc "items"))
         (sel (select 'vector (lambda (it) (= 0 (mod (get it "id") 2))) items))
         (out (map 'vector
                   (lambda (it)
                     (let ((m (sorted-map)))
                       (assoc! m "id" (get it "id"))
                       (assoc! m "label" (string:uppercase (get it "name")))
                       (assoc! m "total" (+ (get it "price") (length (get it "tags"))))
                       (assoc! m "who" (get (get it "owner") "first"))
                       m))
                   sel)))
    (length (json:dump-string (sorted-map "out" out "n" (length out))))))
`

const wlMapProgram = `
(defun build-index (n)
  (let ((idx (sorted-map)))
    (dotimes (i n)
      (let ((k (format-string "k{}" (mod (* i 7) 97))))
        (assoc! idx k (+ 1 (get-default idx k 0)))))
    idx))
(defun map-work ()
  (let* ((idx (build-index 400))
         (ks (keys idx))
         (m2 (foldl (lambda (acc k) (assoc acc k (* 2 (get idx k)))) (sorted-map) ks))
         (total (foldl (lambda (acc k) (+ acc (get m2 k))) 0 (keys m2))))
    (dotimes (i 50) (dissoc! m2 (format-string "k{}" i)))
    (+ total (length (keys m2)))))
`

const wlStringProgram = `
(set 'text (string:join (map 'list (lambda (i) (format-string "Line {}: the quick brown fox, jumps over; {}" i (* i i))) (make-sequence 0 80)) "\n"))
(defun string-work ()
  (let* ((lines (string:split text "\n"))
         (words (foldl (lambda (acc line)
                         (+ acc (length (string:split (string:trim-space (string:lowercase line)) " "))))
                       0 lines))
         (parts (map 'list (lambda (line)
                             (if (string:has-prefix? line "Line 1")
                               (string:uppercase line)
                               (concat 'string (string:trim-prefix line "Line ") "!")))
                     lines)))
    (+ words (length (string:join parts ",")))))
`

const wlRecursionProgram = `
(defun make-counter (start)
  (let ((n start))
    (lambda (d) (set! n (+ n d)) n)))
(defun tree (d) (if (= d 0) (list 1) (list (tree (- d 1)) (tree (- d 1)))))
(defun tree-sum (t)
  (cond ((nil? t) 0)
        ((list? t) (foldl (lambda (acc x) (+ acc (tree-sum x))) 0 t))
        (:else t)))
(defun compose (f g) (lambda (x) (f (g x))))
(defun rec-work ()
  (let* ((c (make-counter 0))
         (inc2 (compose (lambda (x) (+ x 1)) (lambda (x) (+ x 1)))))
    (dotimes (i 300) (funcall c (funcall inc2 i)))
    (labels ((ack (m n)
               (cond ((= m 0) (+ n 1))
                     ((= n 0) (ack (- m 1) 1))
                     (:else (ack (- m 1) (ack m (- n 1)))))))
      (+ (funcall c 0) (tree-sum (tree 8)) (ack 2 3)))))
`

// wlTemplateSetup is the "chaincode" loaded once into a template; each
// iteration forks a VM and runs a transaction against it.
const wlTemplateSetup = `
(in-package 'app)
(export 'handle)
(set 'state (sorted-map))
(defun validate (req)
  (and (sorted-map? req) (key? req "op") (key? req "key")))
(defun handle (raw)
  (let ((req (json:load-string raw)))
    (if (not (validate req))
      (json:dump-string (sorted-map "error" "bad"))
      (let ((op (get req "op")) (k (get req "key")))
        (cond ((string= op "put")
               (assoc! state k (get req "value"))
               (json:dump-string (sorted-map "ok" true "n" (length (keys state)))))
              (:else
               (json:dump-string (sorted-map "ok" true "value" (get-default state k ())))))))))
(in-package 'user)
`

func wlNewEnv(tb testing.TB) *lisp.LEnv {
	tb.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	env.Runtime.Stderr = &bytes.Buffer{}
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		tb.Fatal(rc)
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		tb.Fatal(rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		tb.Fatal(rc)
	}
	return env
}

func wlParse(tb testing.TB, env *lisp.LEnv, src string) *lisp.LVal {
	tb.Helper()
	exprs, err := env.Runtime.Reader.Read("call", strings.NewReader(src))
	if err != nil || len(exprs) != 1 {
		tb.Fatal(err)
	}
	return exprs[0]
}

func wlRun(b *testing.B, prog, call string) {
	env := wlNewEnv(b)
	if rc := env.LoadString("wl.lisp", prog); rc.Type == lisp.LError {
		b.Fatal(rc)
	}
	form := wlParse(b, env, call)
	first := env.Eval(form)
	if first.Type == lisp.LError {
		b.Fatal(first)
	}
	want := first.String()
	b.ReportAllocs()
	for b.Loop() {
		v := env.Eval(form)
		if v.Type == lisp.LError || v.String() != want {
			b.Fatalf("unexpected %v want %v", v, want)
		}
	}
	// The parsed call form is shared by every iteration.  Under the
	// elpscheck tag this re-fingerprints it (a free nil otherwise), so an
	// in-place write to the program fails the benchmark.
	if err := lisp.VerifySealedASTs(); err != nil {
		b.Fatalf("sealed AST verification failed after benchmark: %v", err)
	}
}

func BenchmarkWorkload(b *testing.B) {
	b.Run("json", func(b *testing.B) { wlRun(b, wlJSONProgram, "(json-pipeline)") })
	b.Run("sortedmap", func(b *testing.B) { wlRun(b, wlMapProgram, "(map-work)") })
	b.Run("string", func(b *testing.B) { wlRun(b, wlStringProgram, "(string-work)") })
	b.Run("recursion", func(b *testing.B) { wlRun(b, wlRecursionProgram, "(rec-work)") })
	b.Run("template", func(b *testing.B) {
		env := newTemplateTestEnv(b)
		env.Runtime.Stderr = &bytes.Buffer{}
		if rc := env.LoadString("app.lisp", wlTemplateSetup); rc.Type == lisp.LError {
			b.Fatal(rc)
		}
		tmpl, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(v *lisp.LVal) bool {
			return v.Builtin() != nil
		}))
		if err != nil {
			b.Fatal(err)
		}
		tx := `(app:handle "{\"op\":\"put\",\"key\":\"alpha\",\"value\":{\"x\":[1,2,3],\"y\":\"hello\"}}")
(app:handle "{\"op\":\"put\",\"key\":\"beta\",\"value\":42}")
(app:handle "{\"op\":\"get\",\"key\":\"alpha\"}")`
		// Every VM must answer the transaction identically: the handler's
		// state map is per VM, so a write leaking across forks shows up here
		// as a changed count rather than only as a changed number.
		run := func() string {
			vm, err := tmpl.NewVM()
			if err != nil {
				b.Fatal(err)
			}
			rc := vm.LoadString("tx.lisp", tx)
			if rc.Type == lisp.LError {
				b.Fatal(rc)
			}
			return rc.String()
		}
		want := run()
		b.ReportAllocs()
		for b.Loop() {
			if got := run(); got != want {
				b.Fatalf("transaction returned %s, want %s", got, want)
			}
		}
		// Template-loaded ASTs are shared by every VM the template mints.
		if err := lisp.VerifySealedASTs(); err != nil {
			b.Fatalf("sealed AST verification failed after benchmark: %v", err)
		}
	})
}
