// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"encoding/json"
	"strconv"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// Read benchmarks on the downstream document shape.
//
// elpspath:? used to validate the whole document before every read, so a
// read cost O(document) however short its path (#960). The shape that hit it
// is a large, deep case document decoded from JSON, read many paths per call.
// These rows pin that a read's cost follows its path, not the document: the
// shallow and deep rows must stay flat as the client count grows, and the
// iterator row may grow only with the clients it fans out over. The
// preparsed row is the deep read with steps from parse-path, built once. The
// nested-get row is the floor, the same shallow read as plain map lookups, so
// the gap between it and the shallow row is visible in one table.
//
// The document is decoded with libjson so its maps and vectors are the types
// a phylum gets from json:load. Row names are compared base-vs-PR by
// cmd/benchgate, so keep them stable.

// readBenchClients are the document sizes. 1000 clients is the large arm; it
// stays cheap to build and, after #960, to read.
var readBenchClients = []int{10, 100, 1000}

func readBenchFees() []map[string]any {
	fees := make([]map[string]any, 20)
	for i := range fees {
		fees[i] = map[string]any{"code": "F" + strconv.Itoa(i), "amount": i * 10, "paid": i%2 == 0}
	}
	return fees
}

// readBenchDoc returns a case document with n clients, decoded from JSON.
func readBenchDoc(tb testing.TB, n int) *lisp.LVal {
	tb.Helper()
	details := map[string]any{}
	for _, f := range []string{"status", "type", "branch", "officer", "opened", "updated", "region", "channel", "product", "ref"} {
		details[f] = f + "-value"
	}
	clients := make([]any, n)
	for i := range clients {
		clients[i] = map[string]any{
			"id": i,
			"details": map[string]any{
				"first_name": "F" + strconv.Itoa(i),
				"last_name":  "L" + strconv.Itoa(i),
				"addresses": []any{
					map[string]any{"line1": "1 Main St", "city": "Town", "postcode": "AB1 2CD", "years": 3},
					map[string]any{"line1": "2 High St", "city": "City", "postcode": "EF3 4GH", "years": 5},
				},
			},
			"income": map[string]any{
				"employment": map[string]any{
					"employer": map[string]any{"name": "Acme", "address": map[string]any{"city": "Town"}},
					"salary":   50000 + i,
				},
			},
		}
	}
	props := make([]any, n/10+1)
	mortgages := make([]any, n/10+1)
	for i := range props {
		props[i] = map[string]any{"id": i, "value": 250000, "fees": readBenchFees()}
		mortgages[i] = map[string]any{"id": i, "amount": 200000, "fees": readBenchFees()}
	}
	raw, err := json.Marshal(map[string]any{
		"case":       map[string]any{"details": details},
		"clients":    clients,
		"properties": props,
		"mortgages":  mortgages,
	})
	require.NoError(tb, err)
	doc := libjson.Load(raw, false)
	require.NotEqual(tb, lisp.LError, doc.Type, "%v", doc)
	return doc
}

type readBenchCase struct {
	name  string
	steps func(n int) []*lisp.LVal
}

func readBenchCases() []readBenchCase {
	return []readBenchCase{
		{"shallow", func(int) []*lisp.LVal {
			return []*lisp.LVal{lisp.String("case"), lisp.String("details"), lisp.String("status")}
		}},
		{"deep", func(n int) []*lisp.LVal {
			return []*lisp.LVal{
				lisp.String("clients"), lisp.Int(n - 1), lisp.String("details"),
				lisp.String("addresses"), lisp.Int(1), lisp.String("postcode"),
			}
		}},
		{"deep-employer", func(n int) []*lisp.LVal {
			return []*lisp.LVal{
				lisp.String("clients"), lisp.Int(n - 1), lisp.String("income"),
				lisp.String("employment"), lisp.String("employer"), lisp.String("address"), lisp.String("city"),
			}
		}},
		{"iterator", func(int) []*lisp.LVal {
			return []*lisp.LVal{lisp.String("clients"), lisp.Symbol("*"), lisp.String("details"), lisp.String("last_name")}
		}},
	}
}

func readBenchEnv() *lisp.LEnv {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil
	return env
}

// nestedGet is the floor: the shallow read as plain map lookups, no path.
func nestedGet(doc *lisp.LVal, keys ...string) *lisp.LVal {
	v := doc
	for _, k := range keys {
		next, ok := v.Map().Get(lisp.String(k))
		if !ok {
			return lisp.Nil()
		}
		v = next
	}
	return v
}

func BenchmarkQueryGetCaseDocument(b *testing.B) {
	for _, n := range readBenchClients {
		doc := readBenchDoc(b, n)
		env := readBenchEnv()
		for _, tc := range readBenchCases() {
			args := lisp.QExpr(append([]*lisp.LVal{doc}, tc.steps(n)...))
			b.Run(tc.name+"/clients="+strconv.Itoa(n), func(b *testing.B) {
				if got := BuiltinQueryGet(env, args); got.Type == lisp.LError {
					b.Fatal(got)
				}
				b.ReportAllocs()
				for b.Loop() {
					_ = BuiltinQueryGet(env, args)
				}
			})
		}
		// preparsed is the table-driven pattern downstream: a selector parsed
		// once by parse-path, outside the loop, then read on every call.
		b.Run("preparsed/clients="+strconv.Itoa(n), func(b *testing.B) {
			sel := ".clients[" + strconv.Itoa(n-1) + "].details.addresses[1].postcode"
			steps := BuiltinParsePath(env, lisp.SExpr([]*lisp.LVal{lisp.String(sel)}))
			if steps.Type == lisp.LError {
				b.Fatal(steps)
			}
			args := lisp.QExpr(append([]*lisp.LVal{doc}, steps.Cells...))
			if got := BuiltinQueryGet(env, args); got.Type == lisp.LError {
				b.Fatal(got)
			}
			b.ReportAllocs()
			for b.Loop() {
				_ = BuiltinQueryGet(env, args)
			}
		})
		b.Run("nested-get/clients="+strconv.Itoa(n), func(b *testing.B) {
			if got := nestedGet(doc, "case", "details", "status"); got.IsNil() {
				b.Fatal("nested get missed")
			}
			b.ReportAllocs()
			for b.Loop() {
				_ = nestedGet(doc, "case", "details", "status")
			}
		})
	}
}

// TestQueryGetCostIndependentOfDocumentSize is the cheap, always-run twin of
// the benchmark: a shallow read allocates the same at 10 clients as at 1000.
// Under the old whole-document gate (#960) the large arm allocated more, so a
// regression fails go test here and not only the CI bench gate.
func TestQueryGetCostIndependentOfDocumentSize(t *testing.T) {
	allocs := func(n int) float64 {
		doc := readBenchDoc(t, n)
		env := readBenchEnv()
		args := lisp.QExpr([]*lisp.LVal{doc, lisp.String("case"), lisp.String("details"), lisp.String("status")})
		got := BuiltinQueryGet(env, args)
		require.NotEqual(t, lisp.LError, got.Type, "%v", got)
		require.Equal(t, `"status-value"`, got.String())
		return testing.AllocsPerRun(20, func() { _ = BuiltinQueryGet(env, args) })
	}
	assert.Equal(t, allocs(10), allocs(1000),
		"a shallow read must not allocate in proportion to the document")
}
