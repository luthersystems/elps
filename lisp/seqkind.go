// Copyright © 2026 The ELPS authors

package lisp

import "strconv"

// SeqKind is a sequence type specifier for the Go forms of map, concat,
// zip, slice and append (LEnv.MapSeq, Concat, Zip, Slice and Append).
// SeqList, SeqVector, SeqString and SeqBytes stand for the symbols list,
// vector, string and bytes.  SeqNil stands for the type specifier ().
//
// A form given a kind its builtin refuses returns the error the builtin
// returns for the matching type specifier, at the same point in its checks:
//
//   - SeqNil to Concat, Zip, Slice or Append returns the builtin's first
//     check for a type specifier that is not a symbol, "first argument is
//     not a valid type specification: list" (concat and append) or "first
//     argument is not a valid type specifier: list" (zip and slice).
//   - Any other refused kind returns the builtin's error for an unknown
//     symbol, after the builtin's checks of the other arguments: "type
//     specifier is invalid: K" (map, zip and append) or "type specifier is
//     not valid: K" (concat and slice).  K is the kind's String.
//
// The zero SeqKind, and any value past the constants, is refused by every
// form, and K is then "SeqKind(n)".
type SeqKind uint8

const (
	// SeqList is 'list.  map, concat, zip, slice and append accept it.
	SeqList SeqKind = iota + 1
	// SeqVector is 'vector.  map, concat, zip, slice and append accept it.
	SeqVector
	// SeqString is 'string.  concat and slice accept it.
	SeqString
	// SeqBytes is 'bytes.  concat, slice and append accept it.
	SeqBytes
	// SeqNil is the type specifier ().  map accepts it: map calls fn for
	// its effects and returns ().  concat, zip, slice and append refuse it,
	// as they refuse a type specifier that is not a symbol.
	SeqNil
)

// String returns the symbol name of k ("list", "vector", "string" or
// "bytes"), "()" for SeqNil, or "SeqKind(n)" for any other value.
func (k SeqKind) String() string {
	switch k {
	case SeqList:
		return "list"
	case SeqVector:
		return "vector"
	case SeqString:
		return "string"
	case SeqBytes:
		return "bytes"
	case SeqNil:
		return "()"
	default:
		return "SeqKind(" + strconv.Itoa(int(k)) + ")"
	}
}

// seqKindOf returns the kind a type specifier symbol names, or 0 for a name
// no kind has.  It reads the names from String, so the two cannot differ.
func seqKindOf(name string) SeqKind {
	for k := SeqList; k <= SeqBytes; k++ {
		if k.String() == name {
			return k
		}
	}
	return 0
}

// seqSpec is a type specifier as the shared bodies of map, concat, zip and
// slice read it.  val is the builtin's argument, which the error messages
// print; it is nil for a Go form, whose messages print kind.
type seqSpec struct {
	val  *LVal
	kind SeqKind
}

// parseSeqSpec reads a builtin's type specifier argument.
func parseSeqSpec(v *LVal) seqSpec {
	switch {
	case v.IsNil():
		return seqSpec{val: v, kind: SeqNil}
	case v.Type == LSymbol:
		return seqSpec{val: v, kind: seqKindOf(v.Str)}
	default:
		return seqSpec{val: v}
	}
}

// goSeqSpec is the type specifier a Go form passes for kind.
func goSeqSpec(kind SeqKind) seqSpec {
	return seqSpec{kind: kind}
}

// isSymbol reports whether the specifier is a symbol, as the builtins' first
// check reads it.  A Go form's kind is a symbol unless it is SeqNil.
func (s seqSpec) isSymbol() bool {
	if s.val != nil {
		return s.val.Type == LSymbol
	}
	return s.kind != SeqNil
}

// typ is the type the builtins print when the specifier is not a symbol.
func (s seqSpec) typ() LType {
	if s.val != nil {
		return s.val.Type
	}
	return LSExpr // the type of ()
}

// String is what the builtins print for an invalid type specifier.
func (s seqSpec) String() string {
	if s.val != nil {
		return s.val.String()
	}
	return s.kind.String()
}
