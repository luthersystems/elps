// Copyright © 2026 The ELPS authors

package lint

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

// Keep the lint walk tied to ELPS evaluation rather than another Lisp's quote
// rules. Each case independently asserts the resulting map value before
// checking the diagnostic, so an interpreter error cannot masquerade as data.
func TestMutationChecks_QuoteRuntimeParity(t *testing.T) {
	for _, context := range []struct {
		name, prefix, suffix, message string
		analyzer                      *Analyzer
	}{
		{
			name:     "comparator",
			prefix:   `(set 'm (sorted-map)) (stable-sort (lambda (a b) `,
			suffix:   ` (< a b)) (list 2 1)) (get m "k")`,
			message:  "assoc! is called inside a comparator",
			analyzer: AnalyzerComparatorMutation,
		},
		{
			name:     "iteration",
			prefix:   `(set 'xs (list (sorted-map))) (map 'list (lambda (m) `,
			suffix:   ` m) xs) (get (first xs) "k")`,
			message:  "assoc! mutates the element m of map",
			analyzer: AnalyzerIterationMutation,
		},
	} {
		t.Run(context.name, func(t *testing.T) {
			for _, test := range []struct {
				name, body string
				mutated    bool
			}{
				{"explicit quote", `(quote (assoc! m "k" 1))`, false},
				{"reader quote", `'(assoc! m "k" 1)`, false},
				{"qualified quote", `(lisp:quote (assoc! m "k" 1))`, false},
				{"qualified quasiquote", `(lisp:quasiquote ((assoc! m "k" 1)))`, false},
				{"plain template", `(quasiquote ((assoc! m "k" 1)))`, false},
				{"unquote", `(quasiquote ((unquote (assoc! m "k" 1))))`, true},
				{"qualified template unquote", `(lisp:quasiquote ((unquote (assoc! m "k" 1))))`, true},
				{"nested template unquote", `(quasiquote (quasiquote ((unquote (assoc! m "k" 1)))))`, true},
				{"reader quoted unquote", `(quasiquote '(unquote (assoc! m "k" 1)))`, true},
				{"explicit quoted unquote", `(quasiquote (quote (unquote (assoc! m "k" 1))))`, true},
				{"unquote splicing", `(quasiquote ((unquote-splicing (list (assoc! m "k" 1)))))`, true},
				{"nested template splicing", `(quasiquote (quasiquote ((unquote-splicing (list (assoc! m "k" 1))))))`, true},
				// getUnquoteType recognizes only the bare marker, not a
				// qualified symbol that happens to end in the same name.
				{"qualified unquote is data", `(quasiquote ((lisp:unquote (assoc! m "k" 1))))`, false},
				{"qualified splicing is data", `(quasiquote ((lisp:unquote-splicing (list (assoc! m "k" 1)))))`, false},
			} {
				t.Run(test.name, func(t *testing.T) {
					source := context.prefix + test.body + context.suffix
					env := lisp.NewEnv(nil)
					env.Runtime.Reader = parser.NewReader()
					require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
					value := env.LoadString("quote-parity.lisp", source)
					require.NotEqual(t, lisp.LError, value.Type, "%s", value)
					want := 0
					if test.mutated {
						require.Equal(t, lisp.LInt, value.Type)
						require.Equal(t, 1, value.Int)
						want = 1
					} else {
						require.True(t, value.IsNil(), "%s", value)
					}
					diags := lintCheck(t, context.analyzer, source)
					require.Len(t, diags, want)
					if test.mutated {
						assertHasDiag(t, diags, context.message)
					}
				})
			}
		})
	}
}

// The same quote rules must govern discovery of the callback form itself,
// not just discovery of mutators within a previously discovered callback.
func TestMutationChecks_QuotedCallbackForms(t *testing.T) {
	for _, test := range []struct {
		name, form string
		analyzer   *Analyzer
	}{
		{"comparator", `(stable-sort (lambda (a b) (assoc! a "k" 1)) xs)`, AnalyzerComparatorMutation},
		{"iteration", `(map 'list (lambda (x) (assoc! x "k" 1)) xs)`, AnalyzerIterationMutation},
	} {
		t.Run(test.name, func(t *testing.T) {
			for _, quote := range []string{"quote", "lisp:quote", "quasiquote", "lisp:quasiquote"} {
				t.Run(quote, func(t *testing.T) {
					source := fmt.Sprintf("(%s %s)", quote, test.form)
					assertNoDiags(t, lintCheck(t, test.analyzer, source))
				})
			}
			source := fmt.Sprintf("(quasiquote (quasiquote ((unquote %s))))", test.form)
			require.Len(t, lintCheck(t, test.analyzer, source), 1)
		})
	}
}
