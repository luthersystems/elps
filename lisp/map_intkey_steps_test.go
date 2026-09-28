// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// stringKeyProgram exercises every sorted-map builtin with string and symbol
// keys only.  Its result and step count were recorded before integer keys
// existed (issue #733); TestStringKeyMapsUnchangedByIntKeys pins both.
const stringKeyProgram = `
(set 'm (sorted-map "b" 1 'a 2 "c" (vector 1 2)))
(set 'm2 (assoc m "d" 4))
(assoc! m2 'e 5)
(set 'm3 (dissoc m2 "b"))
(dissoc! m3 'a)
(list m m2 m3 (keys m2) (get m2 'e) (key? m3 "b") (equal? m (copy m))
      (json:dump-string m2) (ignore-errors (sorted-map 1.5 1)))
`

// TestStringKeyMapsUnchangedByIntKeys pins that a program using only string
// and symbol keys produces the same result, and costs the same number of
// evaluation steps, as it did before integer keys were admitted (#733).
func TestStringKeyMapsUnchangedByIntKeys(t *testing.T) {
	env, err := (&elpstest.Runner{}).NewEnv(t)
	require.NoError(t, err)
	env.Runtime.SetStepBudget(1 << 40)
	before := env.Runtime.TotalSteps()
	got := env.LoadString("string-keys.lisp", stringKeyProgram)
	require.NotEqual(t, lisp.LError, got.Type, "%v", got)
	assert.Equal(t, `'((sorted-map 'a 2 "b" 1 "c" (vector 1 2)) `+
		`(sorted-map 'a 2 "b" 1 "c" (vector 1 2) "d" 4 'e 5) `+
		`(sorted-map "c" (vector 1 2) "d" 4 'e 5) '('a "b" "c" "d" 'e) 5 false true `+
		`"{\"a\":2,\"b\":1,\"c\":[1,2],\"d\":4,\"e\":5}" ())`, got.String())
	assert.Equal(t, int64(69), env.Runtime.TotalSteps()-before)
}
