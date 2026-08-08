// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/fuzzval"
	"github.com/luthersystems/elps/lisp"
)

// FuzzSchemaValidate builds a schema out of fuzzer-chosen constraint fragments
// and validates a fuzzer-generated value against it.
//
// libschema gets its own target rather than riding on FuzzApplyStdlib because
// its defects live in COMPOSITION, not in a single call. s:deftype takes a
// base type plus a variadic list of constraints; s:has-key, s:of,
// s:no-other-keys, s:not and s:when each take constraints of their own; and
// the value in a constraint slot is invoked through a private calling
// convention that no type check enforces. Applying s:has-key once with a
// generated argument reaches almost none of that. Assembling the schema from
// source and then validating a generated value reaches all of it.
//
// INVARIANTS
//
//  1. No internal-panic condition. Evaluation converts a Go panic into an
//     LError tagged CondInternalPanic (handler-bind is documented NOT to catch
//     it), so it is a first-class, greppable signal that host code has a
//     defect -- rather than a crash the harness has to catch by dying. Every
//     libschema defect this work fixed surfaced exactly this way.
//  2. No panic escapes at all (the belt to that braces: env.eval's recover is
//     a backstop, not a licence).
//  3. The shared singletons are unmutated.
//  4. Evaluation terminates, bounded by a context deadline plus a watchdog.
//
// NOT ASSERTED: whether validation passes or fails. Almost every generated
// schema is nonsense and rejecting it is correct. The property being fuzzed is
// "libschema always answers", not "libschema answers yes".
func FuzzSchemaValidate(f *testing.F) {
	// Sampled, not fully crossed: 66 fragments x 76 value seeds is 5,016
	// entries and each builds an environment, which measured ~3s added to
	// every `make test`. Sampling keeps every fragment and every value shape
	// in the corpus at ~1/12th the cost.
	valueSeeds := fuzzval.Seeds()
	for i := range len(schemaFragments) {
		f.Add(uint8(i), valueSeeds[i%len(valueSeeds)])     //nolint:gosec // G115: i < 256 fragments
		f.Add(uint8(i), valueSeeds[(i*7)%len(valueSeeds)]) //nolint:gosec // G115: as above
		f.Add(uint8(i), []byte{0x11, 0x22, 0x33, 0x44})    //nolint:gosec // G115: as above
	}
	for i, seed := range valueSeeds {
		f.Add(uint8(i%len(schemaFragments)), seed) //nolint:gosec // G115: modulo fragment count
	}

	f.Fuzz(func(t *testing.T, idx uint8, data []byte) {
		before := lisp.TakeSingletonSnapshot()

		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()

		env := newSchemaEnv(t)
		if rc := lisp.WithContext(ctx)(env); rc.Type == lisp.LError {
			t.Fatalf("context config: %v", rc)
		}
		if rc := lisp.WithMaxSteps(20000)(env); rc.Type == lisp.LError {
			t.Fatalf("step config: %v", rc)
		}

		gen := fuzzval.New(data, env)
		src := schemaFragments[int(idx)%len(schemaFragments)]

		// The value under test is bound as a Go value rather than spliced into
		// the source, so shapes with no source spelling (natives, tagged
		// values, multi-dimensional arrays, error values) reach the
		// validators. That is the whole reason for the value generator.
		if rc := env.PutGlobal(lisp.Symbol("subject"), gen.Value()); rc.Type == lisp.LError {
			t.Fatalf("bind subject: %v", rc)
		}
		if rc := env.PutGlobal(lisp.Symbol("subject2"), gen.Value()); rc.Type == lisp.LError {
			t.Fatalf("bind subject2: %v", rc)
		}

		done := make(chan *lisp.LVal, 1)
		panicked := make(chan any, 1)
		go func() {
			defer func() {
				if r := recover(); r != nil {
					panicked <- r
				}
			}()
			done <- env.LoadStringContext(ctx, "schema-fuzz", src)
		}()

		var res *lisp.LVal
		select {
		case res = <-done:
		case r := <-panicked:
			panic(r)
		case <-time.After(20 * time.Second):
			t.Fatalf("schema evaluation did not terminate despite a 5s deadline\n--- source ---\n%s", src)
		}

		if res == nil {
			t.Fatalf("evaluation returned a nil *LVal\n--- source ---\n%s", src)
		}
		if lisp.IsInternalPanic(res) {
			t.Fatalf("libschema panicked: %v\n--- source ---\n%s\n--- subject ---\n%s",
				res, src, env.GetGlobal(lisp.Symbol("subject")))
		}
		_ = res.String()

		if drift := before.Verify(); drift != "" {
			t.Fatalf("mutated the shared singleton %s\n--- source ---\n%s", drift, src)
		}
	})
}

// schemaFragments are the schema shapes fuzzed against a generated subject.
//
// They are written out rather than generated because a randomly assembled
// s-expression is overwhelmingly likely to be a syntax error, and a target
// that spends its budget on the parser is testing the wrong package (the
// parser has its own targets -- see internal/fuzzseed). What varies here is
// the VALUE flowing through a real schema, plus the constraint slot each
// fragment exercises.
//
// FOREIGN is substituted with an ordinary function, since a function landing
// in a constraint slot is the defect class this package had; the marker check
// must refuse it as a lisp error, never as a panic and never by silently
// skipping the clause.
var schemaFragments = buildSchemaFragments()

func buildSchemaFragments() []string {
	base := []string{
		`(s:deftype "T" s:int) (s:validate T subject)`,
		`(s:deftype "T" s:string) (s:validate T subject)`,
		`(s:deftype "T" "float") (s:validate T subject)`,
		`(s:deftype "T" s:number) (s:validate T subject)`,
		`(s:deftype "T" s:bool) (s:validate T subject)`,
		`(s:deftype "T" s:array) (s:validate T subject)`,
		`(s:deftype "T" s:sorted-map) (s:validate T subject)`,
		`(s:deftype "T" s:fun) (s:validate T subject)`,
		`(s:deftype "T" s:tagged-value) (s:validate T subject)`,
		`(s:deftype "T" "any") (s:validate T subject)`,
		`(s:deftype "T" "any" (s:in subject subject2)) (s:validate T subject)`,
		`(s:deftype "T" "any" (s:gt subject2)) (s:validate T subject)`,
		`(s:deftype "T" "any" (s:gte subject2)) (s:validate T subject)`,
		`(s:deftype "T" "any" (s:lt subject2)) (s:validate T subject)`,
		`(s:deftype "T" "any" (s:lte subject2)) (s:validate T subject)`,
		`(s:deftype "T" "any" (s:len subject2)) (s:validate T subject)`,
		`(s:deftype "T" "any" (s:lengt subject2)) (s:validate T subject)`,
		`(s:deftype "T" "any" (s:lenlte subject2)) (s:validate T subject)`,
		`(s:deftype "T" "any" s:positive) (s:validate T subject)`,
		`(s:deftype "T" "any" s:negative) (s:validate T subject)`,
		`(s:deftype "T" "any" s:is-true) (s:validate T subject)`,
		`(s:deftype "T" "any" s:is-false) (s:validate T subject)`,
		`(s:deftype "T" "any" s:is-truthy) (s:validate T subject)`,
		`(s:deftype "T" "any" s:is-falsy) (s:validate T subject)`,
		`(s:deftype "T" "any" (s:not s:positive)) (s:validate T subject)`,
		`(s:deftype "T" "any" (s:not FOREIGN)) (s:validate T subject)`,
		`(s:deftype "T" "any" (s:regexp "a.*b")) (s:validate T subject)`,
		`(s:deftype "T" s:array (s:of s:int)) (s:validate T subject)`,
		`(s:deftype "T" s:array (s:of FOREIGN)) (s:validate T subject)`,
		`(s:deftype "T" s:sorted-map (s:has-key "a" s:int)) (s:validate T subject)`,
		`(s:deftype "T" s:sorted-map (s:has-key "a" FOREIGN)) (s:validate T subject)`,
		`(s:deftype "T" s:sorted-map (s:may-have-key "a" s:string)) (s:validate T subject)`,
		`(s:deftype "T" s:sorted-map (s:no-other-keys (s:has-key "a" s:int))) (s:validate T subject)`,
		`(s:deftype "T" "any" (s:when "a" s:int "b" (s:gt 100))) (s:validate T subject)`,
		`(s:deftype "T" "any" (s:when "a" FOREIGN "b" (s:gt 100))) (s:validate T subject)`,
		`(s:deftype "T" "any" (s:when "a" s:int "b" FOREIGN)) (s:validate T subject)`,
		`(s:deftype "T" FOREIGN) (s:validate T subject)`,
		`(s:deftype "T" "any" FOREIGN) (s:validate T subject)`,
		`(s:validate subject subject2)`,
		`(s:validate (s:make-validator "T" s:int) subject)`,
		`(s:validate (s:make-validator "T" "any" FOREIGN) subject)`,
		// Nested composition: a validator used as a constraint on a
		// tagged-value's user data.
		`(deftype box (x) x)
		 (s:deftype "T" s:tagged-value "any" (s:not s:negative))
		 (s:validate T subject)`,
	}
	foreigners := []string{"identity", "(lambda (x) x)", "subject", "subject2"}
	out := make([]string, 0, len(base)*2)
	for _, b := range base {
		if !strings.Contains(b, "FOREIGN") {
			out = append(out, b)
			continue
		}
		for _, f := range foreigners {
			out = append(out, strings.ReplaceAll(b, "FOREIGN", f))
		}
	}
	return out
}
