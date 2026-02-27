package tree_sitter_elps

// #cgo CFLAGS: -std=c11 -fPIC
// #include "../../src/parser.c"
// #include "../../src/scanner.c"
import "C"

import "unsafe"

// Language returns the tree-sitter Language for ELPS.
func Language() unsafe.Pointer {
	return unsafe.Pointer(C.tree_sitter_elps())
}
