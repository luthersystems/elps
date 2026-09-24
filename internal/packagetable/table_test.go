// Copyright © 2026 The ELPS authors

package packagetable_test

import (
	"reflect"
	"slices"
	"testing"

	"github.com/luthersystems/elps/internal/packagetable"
)

func TestMapOwnership(t *testing.T) {
	input := map[string]int{"z": 2, "a": 1}
	table := packagetable.NewMap(input)
	input["z"] = 9
	table.Copy()["a"] = 9
	table.Keys()[0] = "corrupt"
	got := make(map[string]int)
	var keys []string
	for key, value := range table.All() {
		keys = append(keys, key)
		got[key] = value
	}
	if !reflect.DeepEqual(got, map[string]int{"a": 1, "z": 2}) || !slices.Equal(keys, []string{"a", "z"}) || table.Len() != 2 {
		t.Fatalf("snapshot changed: %v, keys %v", got, keys)
	}
	if value, ok := table.Lookup("a"); !ok || value != 1 {
		t.Fatalf("lookup changed: %v, %v", value, ok)
	}
	for key := range table.All() {
		if key != "a" {
			t.Fatal("iterator did not stop")
		}
		break
	}
}

func TestStringsOwnership(t *testing.T) {
	input := []string{"z", "a", "z"}
	table := packagetable.NewStrings(input)
	input[0] = "corrupt"
	table.Copy()[1] = "corrupt"
	if got := slices.Collect(table.All()); !slices.Equal(got, []string{"z", "a", "z"}) || table.Len() != 3 {
		t.Fatalf("snapshot changed: %v", got)
	}
	for value := range table.All() {
		if value != "z" {
			t.Fatal("iterator did not stop")
		}
		break
	}
}
