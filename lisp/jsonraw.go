// Copyright © 2026 The ELPS authors

package lisp

import jsonrawhook "github.com/luthersystems/elps/internal/jsonraw/hook"

func init() {
	// The repository-only decoder bridge exposes a fixed constructor, not a
	// map factory supplied by an embedder. Admission and instantiation still
	// operate directly on interpreter-owned jsonMap storage.
	jsonrawhook.Wrap = func(data map[string]any) *LVal {
		return SortedMapFromData(NewMapData(jsonMap(data)))
	}
}
