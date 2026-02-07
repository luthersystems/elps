// Copyright © 2024 The ELPS authors

package cmd

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// expandArgs expands arguments, resolving patterns ending with "/..." to all
// .lisp files found recursively under the given directory. Non-pattern
// arguments pass through unchanged.
func expandArgs(args []string) ([]string, error) {
	var out []string
	for _, arg := range args {
		if dir, ok := strings.CutSuffix(arg, "/..."); ok {
			if dir == "" {
				dir = "."
			}
			files, err := findLispFiles(dir)
			if err != nil {
				return nil, fmt.Errorf("expanding %s: %w", arg, err)
			}
			out = append(out, files...)
		} else {
			out = append(out, arg)
		}
	}
	return out, nil
}

func findLispFiles(root string) ([]string, error) {
	var files []string
	err := filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}
		if filepath.Ext(path) == ".lisp" {
			files = append(files, path)
		}
		return nil
	})
	return files, err
}
