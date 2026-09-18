// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"fmt"
	"strconv"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

func TestDecodedMapSymbolKeys(t *testing.T) {
	for _, key := range []struct{ name, expr, spelling string }{
		{"symbol", `'a`, "a"},
		{"keyword", `:a`, ":a"},
	} {
		for _, nested := range []bool{false, true} {
			doc := fmt.Sprintf(`{%q:1}`, key.spelling)
			dumpedMap := fmt.Sprintf(`{%q:2}`, key.spelling)
			dumpedDoc := dumpedMap
			binding := "doc"
			if nested {
				doc = `{"outer":` + doc + `}`
				dumpedDoc = `{"outer":` + dumpedMap + `}`
				binding = `(get doc "outer")`
			}
			for _, op := range []struct{ name, body, want string }{
				{"get", `(get m %s)`, "1"},
				{"key?", `(key? m %s)`, "true"},
				{"get-default", `(get-default m %s (error 'unexpected-default))`, "1"},
				{"assoc!", `(progn (assoc! m %s 2) (get m ` + strconv.Quote(key.spelling) + `))`, "2"},
				{"dissoc!", `(progn (dissoc! m %s) (key? m ` + strconv.Quote(key.spelling) + `))`, "false"},
				{"missing", `(progn (dissoc! m %s) (list (get m ` + key.expr + `) (key? m ` + key.expr + `) (get-default m ` + key.expr + ` 42)))`, `'(() false 42)`},
				{"print", `(progn (assoc! m %s 2) m)`, fmt.Sprintf(`(sorted-map %q 2)`, key.spelling)},
				{"keys", `(progn (assoc! m %s 2) (keys m))`, fmt.Sprintf(`'(%q)`, key.spelling)},
				{"dump", `(progn (assoc! m %s 2) (json:dump-string doc))`, strconv.Quote(dumpedDoc)},
				{"round_trip", `(progn (assoc! m %s 2) (let ((loaded (json:load-string (json:dump-string m)))) (list (keys loaded) (get loaded ` + key.expr + `))))`, fmt.Sprintf(`'('(%q) 2)`, key.spelling)},
				{"insert", `(progn (dissoc! m ` + strconv.Quote(key.spelling) + `) (assoc! m %s 2) (json:dump-string m))`, strconv.Quote(dumpedMap)},
			} {
				t.Run(fmt.Sprintf("%s/nested=%t/%s", key.name, nested, op.name), func(t *testing.T) {
					env, err := (&elpstest.Runner{}).NewEnv(t)
					require.NoError(t, err)
					expr := fmt.Sprintf(`(let* ((doc (json:load-string %s)) (m %s)) %s)`, strconv.Quote(doc), binding, fmt.Sprintf(op.body, key.expr))
					got := env.LoadString("map-semantics.lisp", expr)
					require.NoError(t, lisp.GoError(got), "%s", expr)
					require.Equal(t, op.want, got.String())
				})
			}
		}
	}
}
