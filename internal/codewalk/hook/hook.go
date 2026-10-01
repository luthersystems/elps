// Copyright © 2026 The ELPS authors

// Package hook breaks the lisp/codewalk import cycle. Only lisp injects these
// adapters, and internal/codewalk consumes them after lisp initializes.
package hook

// Binding describes application-defined binding syntax. The caller validates
// both indices; a negative NameIndex means the form declares no name.
type Binding struct {
	NameIndex    int
	FormalsIndex int
}

// ScopeCategory describes the scope an internal source event opens.
type ScopeCategory uint8

const (
	ScopeFunction ScopeCategory = iota
	ScopeAnonymous
	ScopeLocal
	ScopeFunctions
	ScopeMacros
	ScopeLoop
	ScopeTest
)

// FormalsRole describes the use of a literal formals list.
type FormalsRole uint8

const (
	Parameters FormalsRole = iota
	Constructor
	Benchmark
)

// Node carries source-only metadata without depending on lisp. Both sides
// instantiate exactly this type, so the bridge does not copy each event.
type Node[V, E any] struct {
	Node     *V
	Owner    *V
	Formals  *V
	Binding  *V
	Init     *V
	Op       string
	Depth    int
	Head     bool
	Bound    bool
	Function bool
	Outer    bool
	Template bool
	Event    E
	Scope    ScopeCategory
	Role     FormalsRole
}

// Options carries source visitor policy across the import-cycle bridge.
type Options[V, E any] struct {
	Visit            func(*Node[V, E]) bool
	BindingForm      func(*V) *Binding
	Reference        func(*V)
	Form             func(*V, string, int) bool
	Formals          func(*Node[V, E])
	End              func(int)
	EndDepth         *int
	SkipLiterals     bool
	DeclarationsOnly bool
	SyntacticCalls   bool
}

// Walk holds the source adapter accepting a walker, Options, and a form.
// Its typed signature is recovered by internal/codewalk.
var Walk any

// Forms holds the form-only runtime visitor adapter.
var Forms any

// PackageForms holds func([]*lisp.LVal) []*lisp.LVal.
var PackageForms any

// Occurrences holds func(*lisp.CodeWalker, int, *lisp.LVal) bool.
var Occurrences any

// SyntaxOp holds the raw-syntax operator classifier. Visitors stay on the
// codewalk side of the bridge so their callbacks do not escape through any.
var SyntaxOp any
