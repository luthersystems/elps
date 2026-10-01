package main

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
)

// readGoldenCorpus reads name<TAB>bytes rows from every .txt file under dir.
// Compare the bytes literally: parsing JSON would hide escape, number text,
// whitespace and member-order changes. A missing directory is an empty
// surface (the base may predate the corpus; deleting the head still breaks).
func readGoldenCorpus(dir string) (map[string]string, error) {
	out := make(map[string]string)
	root, err := os.OpenRoot(dir)
	if os.IsNotExist(err) {
		return out, nil
	}
	if err != nil {
		return nil, err
	}
	defer func() { _ = root.Close() }()
	err = fs.WalkDir(root.FS(), ".", func(file string, entry fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if entry.IsDir() || filepath.Ext(file) != ".txt" {
			return nil
		}
		if !entry.Type().IsRegular() {
			return fmt.Errorf("%s/%s: golden file must be a regular file", dir, file)
		}
		// FS paths use forward slashes, so override keys are stable on Windows.
		b, err := root.ReadFile(file)
		if err != nil {
			return err
		}
		if len(b) == 0 {
			return nil
		}
		for i, row := range strings.Split(strings.TrimSuffix(string(b), "\n"), "\n") {
			name, text, ok := strings.Cut(row, "\t")
			if !ok || name == "" || strings.ContainsAny(name, " \t\r\n|:") || text == "" {
				return fmt.Errorf("%s/%s:%d: expected entry-name<TAB>nonempty bytes", dir, file, i+1)
			}
			key := file + ":" + name
			if _, duplicate := out[key]; duplicate {
				return fmt.Errorf("%s/%s:%d: duplicate golden entry %q", dir, file, i+1, name)
			}
			out[key] = text
		}
		return nil
	})
	return out, err
}

func goldenBreaks(base, head map[string]string) []brk {
	var out []brk
	for key, bytes := range base {
		now, ok := head[key]
		switch {
		case !ok:
			out = append(out, brk{"golden", key, "entry removed"})
		case now != bytes:
			out = append(out, brk{"golden", key, "bytes changed"})
		}
	}
	return out
}
