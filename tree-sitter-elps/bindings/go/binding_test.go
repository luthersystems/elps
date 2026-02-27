package tree_sitter_elps_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_elps "github.com/luthersystems/elps/tree-sitter-elps/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_elps.Language())
	if language == nil {
		t.Errorf("Error loading ELPS grammar")
	}
}
