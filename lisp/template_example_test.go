// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"

	"github.com/luthersystems/elps/lisp"
)

func ExampleTemplate() {
	source := lisp.NewEnv(nil)
	for _, result := range []*lisp.LVal{
		source.DefinePackage(lisp.Symbol("example")),
		source.InPackage(lisp.Symbol("example")),
		source.PutGlobal(lisp.Symbol("counter"), lisp.Int(1)),
	} {
		if result.Type == lisp.LError {
			panic(result)
		}
	}
	// Loading is complete. Nothing may evaluate or mutate source during this call.
	plan, err := lisp.NewTemplate(source)
	if err != nil {
		panic(err)
	}
	first, err := plan.NewVM()
	if err != nil {
		panic(err)
	}
	second, err := plan.NewVM()
	if err != nil {
		panic(err)
	}
	if result := first.PutGlobal(lisp.Symbol("counter"), lisp.Int(2)); result.Type == lisp.LError {
		panic(result)
	}
	fmt.Println(first.Get(lisp.Symbol("counter")).Int)
	fmt.Println(second.Get(lisp.Symbol("counter")).Int)
	fmt.Println(source.Get(lisp.Symbol("counter")).Int)
	// Output:
	// 2
	// 1
	// 1
}
