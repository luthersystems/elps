package profiler_test

import (
	_ "embed"
)

//go:embed test.lisp
var testLisp string

//go:embed longtest.lisp
var longTestLisp string
