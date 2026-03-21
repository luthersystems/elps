// Copyright © 2024 The ELPS authors

package cmd

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFilterExcludes_ByName(t *testing.T) {
	paths := []string{
		"src/main.lisp",
		"src/shirocore.lisp",
		"lib/utils.lisp",
	}
	result := filterExcludes(paths, []string{"shirocore.lisp"})
	assert.Equal(t, []string{"src/main.lisp", "lib/utils.lisp"}, result)
}

func TestFilterExcludes_ByDirectory(t *testing.T) {
	paths := []string{
		"src/main.lisp",
		"build/output.lisp",
		"build/sub/deep.lisp",
		"lib/utils.lisp",
	}
	result := filterExcludes(paths, []string{"build"})
	assert.Equal(t, []string{"src/main.lisp", "lib/utils.lisp"}, result)
}

func TestFilterExcludes_GlobPattern(t *testing.T) {
	paths := []string{
		"src/main.lisp",
		"src/generated_foo.lisp",
		"src/generated_bar.lisp",
		"lib/utils.lisp",
	}
	result := filterExcludes(paths, []string{"generated_*"})
	assert.Equal(t, []string{"src/main.lisp", "lib/utils.lisp"}, result)
}

func TestFilterExcludes_MultiplePatterns(t *testing.T) {
	paths := []string{
		"src/main.lisp",
		"build/output.lisp",
		"src/shirocore.lisp",
		"lib/utils.lisp",
	}
	result := filterExcludes(paths, []string{"build", "shirocore.lisp"})
	assert.Equal(t, []string{"src/main.lisp", "lib/utils.lisp"}, result)
}

func TestFilterExcludes_NoMatches(t *testing.T) {
	paths := []string{
		"src/main.lisp",
		"lib/utils.lisp",
	}
	result := filterExcludes(paths, []string{"nonexistent"})
	assert.Equal(t, []string{"src/main.lisp", "lib/utils.lisp"}, result)
}

func TestFilterExcludes_EmptyExcludes(t *testing.T) {
	paths := []string{"src/main.lisp"}
	result := filterExcludes(paths, nil)
	assert.Equal(t, []string{"src/main.lisp"}, result)
}
