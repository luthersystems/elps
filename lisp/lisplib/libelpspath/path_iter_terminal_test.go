// Copyright © 2026 The ELPS authors

package libelpspath_test

import (
	"fmt"
	"regexp"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

// terminalIterDocs are documents made of vectors and sorted-maps only, so
// the in-place forms accept every write the copying forms make.  No
// container appears twice: an in-place write to a shared container shows in
// each place, and a copying write does not.
var terminalIterDocs = []struct{ name, doc string }{
	{"flat", `(vector 1 2 3)`},
	{"empty", `(vector)`},
	{"nested", `(vector (vector 1 2) (vector 3) (vector) 7)`},
	{"deep", `(vector (vector (vector 1 2) (vector 3)) (vector (vector) 4))`},
	{"map", `(sorted-map "a" (vector 1 2) "b" 3)`},
	{"maps", `(vector (sorted-map "a" (vector 1 2)) (sorted-map "a" (vector)) (sorted-map "b" 1))`},
}

// terminalIterPaths end in '*, the step the in-place forms ignored.
var terminalIterPaths = []string{
	`'*`,
	`'* '*`,
	`'* '* '*`,
	`0 '*`,
	`-1 '*`,
	`9 '*`,
	`"a" '*`,
	`'* "a" '*`,
	`'(range 0 2) '*`,
	`'(range 1) '*`,
	`'* '(range 0 1) '*`,
	`'* 0 '*`,
}

// errPrefix is the position and builtin name that start an error's text.
var errPrefix = regexp.MustCompile(`^test\.lisp:\d+:\d+: elpspath:\?[a-z]+!?: `)

// An in-place form whose path ends in '* writes each element, and leaves
// the document equal to the result of the copying form for the same path.
// The in-place forms did nothing for these paths and reported no error
// (luthersystems/elps#943).
func TestTerminalIteratorMutatesInPlace(t *testing.T) {
	ops := []struct{ name, value string }{
		{"?set", ` 0`},
		{"?set", ` (vector 8 9)`},
		{"?del", ``},
		{"?nil", ``},
	}
	for _, doc := range terminalIterDocs {
		for _, path := range terminalIterPaths {
			for _, op := range ops {
				name := fmt.Sprintf("%s/%s%s/%s", doc.name, op.name, op.value, path)
				t.Run(name, func(t *testing.T) {
					env := iterEnv(t, 0, "")
					eval := func(src string) string {
						t.Helper()
						// An error is compared by its text: the two forms
						// differ in name and source position.
						return errPrefix.ReplaceAllString(env.LoadString("test.lisp", src).String(), "error: ")
					}
					want := eval(fmt.Sprintf(`(elpspath:%s %s %s%s)`, op.name, doc.doc, path, op.value))
					// The in-place form returns the document it was given,
					// and that document holds the change.
					got := eval(fmt.Sprintf(`(let* ((d %s) (r (elpspath:%s! d %s%s)))
  (if (equal? r d) d (list 'returned r 'document d)))`, doc.doc, op.name, path, op.value))
					assert.Equal(t, want, got)
				})
			}
		}
	}
}

// The three calls of the issue report, with their results spelled out.
func TestTerminalIteratorMutatesInPlaceExamples(t *testing.T) {
	for _, tc := range []struct{ expr, want string }{
		{`(let ((v (vector 1 2 3))) (elpspath:?set! v '* 0) v)`, `(vector 0 0 0)`},
		{`(let ((v (vector 1 2 3))) (elpspath:?del! v '*) v)`, `(vector () () ())`},
		{`(let ((v (vector 1 2 3))) (elpspath:?nil! v '*) v)`, `(vector () () ())`},
		{`(let ((v (vector (vector 1 2) (vector 3) 7))) (elpspath:?set! v '* '* 0) v)`,
			`(vector (vector 0 0) (vector 0) 7)`},
		{`(let ((m (sorted-map "a" (vector 1 2)))) (elpspath:?nil! m "a" '*) m)`,
			`(sorted-map "a" (vector () ()))`},
		// The range step reads a window on the vector's own cells, so the
		// write lands in the vector.
		{`(let ((v (vector 1 2 3))) (elpspath:?set! v '(range 0 2) '* 9) v)`, `(vector 9 9 3)`},
		// An in-place write to a vector the document holds twice shows in
		// both places, as an index write does.  ?set copies each one.
		{`(let* ((row (vector 1 2)) (v (vector row row))) (elpspath:?set! v 0 '* 0) v)`,
			`(vector (vector 0 0) (vector 0 0))`},
		// The new value is stored by reference in every element, as ?set
		// stores it.
		{`(let* ((x (vector 1)) (v (vector 1 2))) (elpspath:?set! v '* x) (append! x 2) v)`,
			`(vector (vector 1 2) (vector 1 2))`},
	} {
		t.Run(tc.expr, func(t *testing.T) {
			env := iterEnv(t, 0, "")
			assert.Equal(t, tc.want, env.LoadString("test.lisp", tc.expr).String())
		})
	}
}

// A list's cells can be a program literal that every environment shares, so
// the in-place forms refuse to write them (errMutateList).  A last '* is a
// write of each cell, and is refused like an index step.  Under an outer
// iterator the refusal is that element's failure: the element stays as it
// was and the other elements are written.
func TestTerminalIteratorRefusesLists(t *testing.T) {
	const refused = "in-place path operations require an array or sorted-map; got list"
	for _, op := range []string{`?set! %s 0`, `?del! %s`, `?nil! %s`} {
		for _, target := range []string{
			`(list 1 2 3) '*`,
			`'(1 2 3) '*`,
			`(sorted-map "a" (list 1 2)) "a" '*`,
			`(vector (list 1 2)) 0 '*`,
			`(list 1 2 3) '(range 0 2) '*`,
		} {
			src := `(elpspath:` + fmt.Sprintf(op, target) + `)`
			t.Run(src, func(t *testing.T) {
				env := iterEnv(t, 0, "")
				got := env.LoadString("test.lisp", src)
				require.Equal(t, lisp.LError, got.Type, "%v", got)
				assert.Contains(t, got.String(), refused)
			})
		}
	}

	for _, tc := range []struct{ expr, want string }{
		{`(let ((v (vector (list 1 2) (vector 3)))) (elpspath:?set! v '* '* 0) v)`,
			`(vector '(1 2) (vector 0))`},
		{`(let ((v (vector (list 1 2) (vector 3)))) (elpspath:?del! v '* '*) v)`,
			`(vector '(1 2) (vector ()))`},
		{`(let ((v (vector (list 1 2) (vector 3)))) (elpspath:?nil! v '* '*) v)`,
			`(vector '(1 2) (vector ()))`},
		// A step below the iterator writes the elements' own containers,
		// not the list's cells, and stays allowed.
		{`(let ((l (list (sorted-map "a" 1) (sorted-map "a" 2)))) (elpspath:?set! l '* "a" 5) l)`,
			`'((sorted-map "a" 5) (sorted-map "a" 5))`},
	} {
		t.Run(tc.expr, func(t *testing.T) {
			env := iterEnv(t, 0, "")
			assert.Equal(t, tc.want, env.LoadString("test.lisp", tc.expr).String())
		})
	}
}

// A program literal list keeps its cells through a refused in-place write.
func TestTerminalIteratorLeavesLiteralListAlone(t *testing.T) {
	env := iterEnv(t, 0, `(defun lit () '(1 2 3))`)
	for _, src := range []string{
		`(elpspath:?set! (lit) '* 0)`,
		`(elpspath:?del! (lit) '*)`,
		`(elpspath:?nil! (lit) '*)`,
	} {
		got := env.LoadString("test.lisp", src)
		require.Equal(t, lisp.LError, got.Type, "%s: %v", src, got)
	}
	assert.Equal(t, `'(1 2 3)`, env.LoadString("test.lisp", `(lit)`).String())
}

// The errors a last '* raised before the fix are the same after it.
func TestTerminalIteratorKeepsItsErrors(t *testing.T) {
	for _, tc := range []struct{ expr, want string }{
		{`(elpspath:?set! () '* 0)`, "first argument is nil"},
		{`(elpspath:?del! () '*)`, "first argument is nil"},
		{`(elpspath:?nil! () '*)`, "first argument is nil"},
		{`(elpspath:?set! (sorted-map "a" 1) '* 0)`, "argument is not an array"},
		{`(elpspath:?set! 5 '* 0)`, "cannot index into int at path <root>"},
		{`(elpspath:?del! "s" '*)`, "cannot index into string at path <root>"},
	} {
		t.Run(tc.expr, func(t *testing.T) {
			env := iterEnv(t, 0, "")
			got := env.LoadString("test.lisp", tc.expr)
			require.Equal(t, lisp.LError, got.Type, "%v", got)
			assert.Contains(t, got.String(), tc.want)
		})
	}
}

// A last '* costs the steps it cost before it wrote anything: one unit of
// iterator work for each element, charged in steps past the allowance
// (budget.go).  The write itself is free, as an index write is.
func TestTerminalIteratorKeepsItsStepCharge(t *testing.T) {
	const excess = 1000
	const n = (1 << 20) + excess // iterWorkAllowance plus excess elements
	setup := fmt.Sprintf(`(set 'big (map 'vector identity (make-sequence 0 %d)))
(set 'small (vector 1))
()`, n)
	for _, op := range []string{
		`(elpspath:?set! %s '* 0)`,
		`(elpspath:?del! %s '*)`,
		`(elpspath:?nil! %s '*)`,
	} {
		t.Run(op, func(t *testing.T) {
			env := iterEnv(t, 1<<40, setup)
			// The two calls differ only in the vector, so the difference
			// is the iterator's charge.
			_, small := iterSteps(t, env, fmt.Sprintf(op, "small"))
			_, big := iterSteps(t, env, fmt.Sprintf(op, "big"))
			assert.Equal(t, int64(excess), big-small)
		})
	}
}
