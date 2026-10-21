// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestVMValueLazyBuild checks that a lazy VM builds its map of VM values on
// the first VM value call and each value on its first read, and that the
// map drops its link to the lazy instance once no value is pending.
func TestVMValueLazyBuild(t *testing.T) {
	source := NewEnv(nil)
	require.False(t, InitializeUserEnv(source).IsError())
	for _, name := range []string{"a", "b", "c", "d"} {
		m := SortedMap()
		require.False(t, m.Map().Set(String(name), Int(1)).IsError())
		source.Runtime.SetVMValue(name, m)
	}
	tmpl, err := NewTemplate(source, TemplateWithBuiltinPolicy(func(*LVal) bool { return true }))
	require.NoError(t, err)
	vm, err := tmpl.NewVM()
	require.NoError(t, err)
	rt := vm.Runtime
	require.Nil(t, rt.vmValues.m, "NewVM built the map of VM values")
	require.NotNil(t, rt.vmValues.lazy)

	a, ok := rt.VMValue("a")
	require.True(t, ok)
	assert.Nil(t, rt.vmValues.lazy, "the map kept its builder after it was built")
	again, _ := rt.VMValue("a")
	assert.Same(t, a, again, "a second read built another copy")
	sm, ok := rt.vmValues.m.Map().mapBacking.(sortedmap)
	require.True(t, ok)
	require.NotNil(t, sm.lz)
	assert.Equal(t, 3, sm.lz.pending)

	rt.DeleteVMValue("b")
	rt.SetVMValue("c", Int(3))
	_, ok = rt.VMValue("b")
	assert.False(t, ok)
	_, ok = rt.VMValue("d")
	require.True(t, ok)
	assert.Nil(t, sm.lz.inst, "the map kept the lazy instance with nothing pending")

	// A VM that deletes a value it never read does not publish it.
	vm2, err := tmpl.NewVM()
	require.NoError(t, err)
	vm2.Runtime.DeleteVMValue("a")
	_, entries := vm2.Runtime.vmValueEntries()
	names := make([]string, len(entries))
	for i, e := range entries {
		names[i] = e.key
	}
	assert.Equal(t, []string{"b", "c", "d"}, names)
}
