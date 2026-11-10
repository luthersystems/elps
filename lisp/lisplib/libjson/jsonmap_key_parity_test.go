// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestDecodedMapAnswersLikeEqualSortedMap pins that a map json:load returns
// and the equal? sorted-map give the same answer to every operation that
// does not insert in place, for every key type (luthersystems/elps#943).
//
// A decoded map holds string keys only.  An int key is a valid sorted-map key
// that such a map cannot contain, so a read reports it absent and a delete
// does nothing, as the stock map does.  A caller cannot tell the two maps
// apart with equal?, so a read that raised on one of them only was a trap for
// code that takes a map from either source.
//
// A key no sorted-map takes (a float, a list) is an error on both maps.  The
// two messages differ, so the test compares the error condition and not the
// text.  assoc! with an int key stays refused on a decoded map
// (TestDecodedMapKeepsStringKeyPolicy), so it is not in the table.
func TestDecodedMapAnswersLikeEqualSortedMap(t *testing.T) {
	keys := []string{
		`1`, `0`, `-7`, `"a"`, `"zz"`, `'a`, `'zz`, `:a`, `"1"`,
		`1.0`, `(to-bytes "a")`, `()`, `'(1 2)`, `(vector 1)`,
		`(sorted-map)`, `true`, `(lambda () 1)`,
	}
	ops := []string{
		`(get m %s)`,
		`(key? m %s)`,
		`(get-default m %s "dflt")`,
		`(dissoc m %s)`,
		`(progn (dissoc! m %s) m)`,
		`(assoc m %s "new")`,
		`(match m ((sorted-map %s v) (list "hit" v)) (_ "miss"))`,
	}
	run := func(t *testing.T, mapExpr, expr string) string {
		env, err := (&elpstest.Runner{}).NewEnv(t)
		require.NoError(t, err)
		got := env.LoadString("parity.lisp", fmt.Sprintf(`(let ((m %s)) %s)`, mapExpr, expr))
		require.False(t, lisp.IsInternalPanic(got), "%v", got)
		if got.Type == lisp.LError {
			return "error"
		}
		return got.String()
	}
	const (
		decoded = `(json:load-string "{\"a\":1,\"1\":2}")`
		stock   = `(sorted-map "a" 1 "1" 2)`
	)
	for _, op := range ops {
		for _, key := range keys {
			expr := fmt.Sprintf(op, key)
			if strings.HasPrefix(op, "(match") && run(t, stock, expr) == "error" {
				continue // match takes a literal key only
			}
			t.Run(expr, func(t *testing.T) {
				assert.Equal(t, run(t, stock, expr), run(t, decoded, expr))
			})
		}
	}
}

// TestDecodedMapIntInsertRefusalIgnoresAllocationProbe pins that assoc! with
// an int key on a decoded map reports the map's refusal whether or not the
// map is at the allocation cap.  The cap probe reads the key first.  A
// decoded map reads an int key as absent, so the probe must not report
// growth for a key the insert refuses.
func TestDecodedMapIntInsertRefusalIgnoresAllocationProbe(t *testing.T) {
	const size = 64
	var doc strings.Builder
	doc.WriteString("{")
	for i := range size {
		if i > 0 {
			doc.WriteString(",")
		}
		fmt.Fprintf(&doc, `\"key%d\":%d`, i, i)
	}
	doc.WriteString("}")
	var messages []string
	for _, maxAlloc := range []int{size, 1 << 20} {
		env, err := (&elpstest.Runner{}).NewEnv(t)
		require.NoError(t, err)
		m := env.LoadString("probe.lisp", fmt.Sprintf(`(json:load-string "%s")`, doc.String()))
		require.Equal(t, lisp.LSortMap, m.Type, "%v", m)
		env.PutGlobal(lisp.Symbol("m"), m)
		env.Runtime.MaxAlloc = maxAlloc
		got := env.LoadString("probe.lisp", `(assoc! m 1 2)`)
		require.Equal(t, lisp.LError, got.Type, "%v", got)
		msg := env.Render(got)
		assert.Contains(t, msg, "sorted-map decoded from json cannot hold key with type 'int")
		messages = append(messages, msg)
	}
	assert.Equal(t, messages[1], messages[0])
}
