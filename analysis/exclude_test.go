// Copyright © 2024 The ELPS authors

package analysis

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestMatchesExclude_BaseName(t *testing.T) {
	assert.True(t, MatchesExclude("project/build/shirocore.lisp", []string{"shirocore.lisp"}))
}

func TestMatchesExclude_DirectoryComponent(t *testing.T) {
	assert.True(t, MatchesExclude("project/build/output.lisp", []string{"build"}))
}

func TestMatchesExclude_FullPath(t *testing.T) {
	assert.True(t, MatchesExclude("shirocore.lisp", []string{"shirocore.lisp"}))
}

func TestMatchesExclude_GlobPattern(t *testing.T) {
	assert.True(t, MatchesExclude("gen/output.lisp", []string{"*.lisp"}))
}

func TestMatchesExclude_NoMatch(t *testing.T) {
	assert.False(t, MatchesExclude("src/main.lisp", []string{"shirocore.lisp", "build"}))
}

func TestMatchesExclude_EmptyPatterns(t *testing.T) {
	assert.False(t, MatchesExclude("anything.lisp", nil))
	assert.False(t, MatchesExclude("anything.lisp", []string{}))
}

func TestMatchesExclude_MultiplePatterns(t *testing.T) {
	patterns := []string{"generated.lisp", "build"}
	assert.True(t, MatchesExclude("out/generated.lisp", patterns))
	assert.True(t, MatchesExclude("build/lib.lisp", patterns))
	assert.False(t, MatchesExclude("src/lib.lisp", patterns))
}
