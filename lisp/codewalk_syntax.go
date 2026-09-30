// Copyright © 2026 The ELPS authors

package lisp

import "github.com/luthersystems/elps/internal/codewalk/hook"

func init() {
	w := CodeWalker{}
	hook.SyntaxOp = w.syntaxOperator
}

// syntaxOperator classifies raw heads, including quoted heads, without
// applying runtime resolution or the resolver's narrower source policy.
func (*CodeWalker) syntaxOperator(head *LVal) string {
	if head == nil || head.Type != LSymbol {
		return ""
	}
	if n := len(head.Str); n >= len(syntaxNameLengths) || !syntaxNameLengths[n] {
		return ""
	}
	return syntaxOperators[head.Str]
}

// Syntax recognizes both kernel spellings even on quoted heads; callers can
// narrow qualification and quoting according to their historical policy.
var syntaxOperators = func() map[string]string {
	operators := make(map[string]string, 2*len(formKinds)+2)
	for name := range formKinds {
		operators[name] = name
		operators[DefaultLangPackage+":"+name] = name
	}
	// These are template grammar, not special operators in code position.
	for _, name := range []string{"unquote", "unquote-splicing"} {
		operators[name] = name
	}
	return operators
}()

// Reject ordinary calls whose length cannot match before hashing their names.
var syntaxNameLengths = func() []bool {
	maxLength := 0
	for name := range syntaxOperators {
		maxLength = max(maxLength, len(name))
	}
	lengths := make([]bool, maxLength+1)
	for name := range syntaxOperators {
		lengths[len(name)] = true
	}
	return lengths
}()
