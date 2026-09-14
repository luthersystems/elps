// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestSortedMapLastWriteWinsELPS(t *testing.T) {
	for _, order := range []struct{ name, first, last, keys, printed string }{
		{"string_to_symbol", `"a"`, `'a`, `'('a)`, `(sorted-map 'a 2)`},
		{"symbol_to_string", `'a`, `"a"`, `'("a")`, `(sorted-map "a" 2)`},
	} {
		for _, operation := range []string{"sorted-map", "assoc", "assoc!"} {
			t.Run(order.name+"/"+operation, func(t *testing.T) {
				expr := fmt.Sprintf(`(sorted-map %s 1 %s 2)`, order.first, order.last)
				if operation != "sorted-map" {
					expr = fmt.Sprintf(`(%s (sorted-map %s 1) %s 2)`, operation, order.first, order.last)
				}
				elpstest.RunTestSuite(t, elpstest.TestSuite{{"last write wins", elpstest.TestSequence{
					{expr, order.printed, ""},
					{"(keys " + expr + ")", order.keys, ""},
				}}})
			})
		}
	}
}

func TestGetDefaultNil(t *testing.T) {
	elpstest.RunTestSuite(t, elpstest.TestSuite{{"nil and lazy default", elpstest.TestSequence{
		{`(get-default () "k" 42)`, `42`, ""},
		{`(get-default () 'k 42)`, `42`, ""},
		{`(get-default () :k 42)`, `42`, ""},
		{`(let ((calls 0)) (list (get-default (progn (set! calls (+ calls 1)) ()) (progn (set! calls (+ calls 1)) "k") (progn (set! calls (+ calls 1)) 42)) calls))`, `'(42 3)`, ""},
		{`(get-default (sorted-map "k" ()) "k" (error 'unexpected-default))`, `()`, ""},
		{`(get-default (sorted-map "k" false) "k" (error 'unexpected-default))`, `false`, ""},
		{`(handler-bind ((condition (lambda (&rest _) 'caught))) (get-default 1 "k" 42))`, `'caught`, ""},
	}}})
}
