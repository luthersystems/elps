// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestTaggedVals(t *testing.T) {
	tests := elpstest.TestSuite{
		{"type", elpstest.TestSequence{
			{`(type 'test)`, `'symbol`, ``},
			{`(type true)`, `'symbol`, ``},
			{`(type 1.0)`, `'float`, ``},
			{`(type 12)`, `'int`, ``},
			{`(type "")`, `'string`, ``},
			{`(type '())`, `'list`, ``},
			{`(type (list))`, `'list`, ``},
			{`(type (list 1 2 3))`, `'list`, ``},
			{`(type (vector 1 2 3))`, `'array`, ``},
		}},
		{"type?", elpstest.TestSequence{
			{`(type? 'symbol 'test)`, `true`, ``},
			{`(type? 'string true)`, `false`, ``},
			{`(type? 'int 1.0)`, `false`, ``},
			{`(type? 'int 12)`, `true`, ``},
			{`(type? 'list "")`, `false`, ``},
			{`(type? 'list '())`, `true`, ``},
			{`(type? 'nil (list))`, `false`, ``},
			{`(type? 'string (list 1 2 3))`, `false`, ``},
			{`(type? 'array (vector 1 2 3))`, `true`, ``},
		}},
		{"typedef", elpstest.TestSequence{
			{`(set 'object (new lisp:typedef 'user:object identity))`, `#{lisp:typedef '('user:object #<builtin>)}`, ``},
			{`(new user:object "hello")`, `#{user:object "hello"}`, ``},
			{`(new object "hello")`, `#{user:object "hello"}`, ``},
			{`(new 'object "hello")`, `#{user:object "hello"}`, ``},
			{`(set 'o (new object "hello"))`, `#{user:object "hello"}`, ``},
			{`(equal? o (new object "hello"))`, `true`, ``},
			{`(equal? o (new user:object "hello"))`, `true`, ``},
			{`(equal? o (new user:object "goodbye"))`, `false`, ``},
			{`(equal? (new object "hello") o)`, `true`, ``},
			{`(equal? (new user:object "hello") o)`, `true`, ``},
			{`(equal? (new user:object "goodbye") o)`, `false`, ``},
			{`(equal? o '())`, `false`, ``},
			{`(equal? '() o)`, `false`, ``},
			{`(equal? "hello" o)`, `false`, ``},
			{`(equal? o "hello")`, `false`, ``},
			{`(type o)`, `'user:object`, ``},
			{`(type? 'string o)`, `false`, ``},
			{`(type? 'object o)`, `false`, ``},
			{`(type? 'user:object o)`, `true`, ``},
			{`(type? 'user:what? o)`, `false`, ``},
			{`(tagged-value? o)`, `true`, ``},
			{`(user-data o)`, `"hello"`, ``},
			{`(tagged-value? "hello")`, `false`, ``},
			{`(user-data "hello")`, `test:1: lisp:user-data: argument is not a tagged value: 'string`, ``},
		}},
		{"constructor", elpstest.TestSequence{
			{`(set 'even (new lisp:typedef 'user:even (lambda (x) (if (int? x) (* x 2) (error 'type-error (format-string "argument is not an int: {}" (type x)))))))`, `#{lisp:typedef '('user:even (lambda (x) (if (int? x) (* x 2) (error 'type-error (format-string "argument is not an int: {}" (type x))))))}`, ``},
			{`(new even "hello")`, `test:1: type-error: argument is not an int: 'string`, ``},
			{`(new even 1)`, `#{user:even 2}`, ``},
		}},
		{"deftype", elpstest.TestSequence{
			{`(deftype none ())`, `'user:none`, ``},
			{`none`, `#{lisp:typedef '('user:none (lambda ()))}`, ``},
			{`(new none)`, `#{user:none ()}`, ``},
			{`(deftype some (x) x)`, `'user:some`, ``},
			{`some`, `#{lisp:typedef '('user:some (lambda (x) x))}`, ``},
			{`(new some "hello")`, `#{user:some "hello"}`, ``},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
