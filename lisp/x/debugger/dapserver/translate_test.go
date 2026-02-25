package dapserver

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestResolveSourcePath(t *testing.T) {
	tests := []struct {
		name       string
		path       string
		file       string
		sourceRoot string
		want       string
	}{
		{
			name:       "absolute path unchanged",
			path:       "/abs/path/file.lisp",
			file:       "file.lisp",
			sourceRoot: "/root",
			want:       "/abs/path/file.lisp",
		},
		{
			name:       "relative path resolved with source root",
			path:       "file.lisp",
			file:       "file.lisp",
			sourceRoot: "/my/project",
			want:       "/my/project/file.lisp",
		},
		{
			name:       "relative subdir path resolved",
			path:       "subdir/file.lisp",
			file:       "file.lisp",
			sourceRoot: "/my/project",
			want:       "/my/project/subdir/file.lisp",
		},
		{
			name:       "no source root returns relative path as-is",
			path:       "file.lisp",
			file:       "file.lisp",
			sourceRoot: "",
			want:       "file.lisp",
		},
		{
			name:       "empty path uses file as fallback",
			path:       "",
			file:       "main.lisp",
			sourceRoot: "/root",
			want:       "/root/main.lisp",
		},
		{
			name:       "both empty returns empty",
			path:       "",
			file:       "",
			sourceRoot: "/root",
			want:       "",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := resolveSourcePath(tt.path, tt.file, tt.sourceRoot)
			assert.Equal(t, tt.want, got)
		})
	}
}
