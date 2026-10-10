package idiomcase

import "github.com/luthersystems/elps/lisp"

// The Field hint needs no map check before the read.
func fieldsNoMapCheck(desc *lisp.LVal) (string, int) {
	s := desc.MapGetString("name") // want `lisp.Field\[string\]\(desc, "name"\) reads a string field in one call; it returns ok=false for a value that is not a map`
	if s.Type != lisp.LString {
		return "", 0
	}
	n := desc.MapGetString("count") // want `lisp.Field\[int\]\(desc, "count"\) reads an int field in one call`
	if n.Type != lisp.LInt {
		return s.Str, 0
	}
	return s.Str, n.Int
}

// A type check without the matching field read is not reported.
func fieldsKept(desc *lisp.LVal) (string, int) {
	s := desc.MapGetString("name")
	if s.Type != lisp.LString {
		return "", 0
	}
	n := desc.MapGetString("count")
	if n.Type != lisp.LInt {
		return "", 0
	}
	return n.Str, s.Int
}

func argName(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	collection, key := args.Cells[0], args.Cells[1] // want `a cell reader decodes collection: r := lisp.Cells\(args.Cells\).Read\(env\), then r.Name\(\), which takes a string or a symbol,`
	if collection.Type != lisp.LString && collection.Type != lisp.LSymbol {
		return env.Errorf("first argument is not a string: %v", collection.Type)
	}
	return key
}
