// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"reflect"
	"testing"
)

// Issue #639: compilation needs the descriptors, not a second allocation of
// sorted keys. Admission still establishes callback and rejection order.
func TestTemplateDescriptorsPreserveOrderWithOneAllocation(t *testing.T) {
	values := make(map[string]*LVal)
	metadata := make(map[string]string)
	leaves := []*LVal{Int(17), Int(29)}
	for n := range 32 {
		key := fmt.Sprintf("key-%02d", n)
		values[key] = leaves[n%len(leaves)]
		metadata[key] = "doc:" + key
	}
	c := templateCompiler{values: map[*LVal]int{leaves[0]: 1, leaves[1]: 2}, plan: templatePlan{values: make([]templateValue, 2)}}
	t.Run("bindings", func(t *testing.T) {
		var got []templateBinding
		allocs := testing.AllocsPerRun(100, func() { got = c.bindings(values) })
		if allocs > 1 {
			t.Fatalf("descriptor construction allocated %.0f objects, want at most the output slice", allocs)
		}
		seen := make(map[string]bool)
		for n, binding := range got {
			want, present := values[binding.name]
			if binding.name != fmt.Sprintf("key-%02d", n) || seen[binding.name] || !present || binding.value.index != c.values[want] || binding.value.shared != nil {
				t.Fatalf("binding lost its key, admitted identity or alias: %+v", binding)
			}
			seen[binding.name] = true
		}
		if len(seen) != len(values) || c.err != nil {
			t.Fatalf("bindings omitted values or rejected an admitted reference: count=%d error=%v", len(seen), c.err)
		}
	})
	t.Run("metadata", func(t *testing.T) {
		var got []templateStringPair
		allocs := testing.AllocsPerRun(100, func() { got = templateStringPairs(metadata) })
		if allocs > 1 {
			t.Fatalf("metadata construction allocated %.0f objects, want at most the output slice", allocs)
		}
		reconstructed := make(map[string]string)
		for n, entry := range got {
			if entry.key != fmt.Sprintf("key-%02d", n) {
				t.Fatalf("metadata insertion order changed at %d: %q", n, entry.key)
			}
			if _, duplicate := reconstructed[entry.key]; duplicate {
				t.Fatalf("duplicate metadata key %q", entry.key)
			}
			reconstructed[entry.key] = entry.value
		}
		if !reflect.DeepEqual(reconstructed, metadata) {
			t.Fatalf("metadata changed: %v", reconstructed)
		}
	})
}
