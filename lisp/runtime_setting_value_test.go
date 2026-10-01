// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"sync"
	"testing"
	"unsafe"

	"github.com/luthersystems/elps/internal/templatepolicy"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type runtimeSettingVersion string

type runtimeSettingBytes []byte

type runtimeSettingImmutable struct {
	templatepolicy.Marker
	id      string
	version int
}

func settingValueTestValues() []struct {
	name  string
	value any
} {
	return []struct {
		name  string
		value any
	}{
		{"string", "loaded"},
		{"empty_string", ""},
		{"int", int(-7)},
		{"zero_int", int(0)},
		{"int8", int8(-8)},
		{"int16", int16(-16)},
		{"int32", int32(-32)},
		{"int64", int64(-64)},
		{"uint", uint(7)},
		{"uint8", uint8(8)},
		{"uint16", uint16(16)},
		{"uint32", uint32(32)},
		{"uint64", uint64(64)},
		{"bool", true},
		{"false_bool", false},
		{"float32", float32(1.25)},
		{"float64", float64(2.5)},
		{"complex64", complex64(1 + 2i)},
		{"complex128", complex128(3 + 4i)},
		{"named_string", runtimeSettingVersion("v1")},
		{"marked_struct", runtimeSettingImmutable{id: "loaded", version: 1}},
		{"bytes", []byte("loaded")},
	}
}

func assertRuntimeSettingValue(t *testing.T, rt *Runtime, name string, want any) {
	t.Helper()
	got, ok := rt.SettingValue(name)
	if assert.True(t, ok, "setting %q must be set", name) {
		assert.IsType(t, want, got, "setting %q must keep its dynamic type", name)
		assert.Equal(t, want, got, "setting %q", name)
	}
}

func assertNoRuntimeSettingValue(t *testing.T, rt *Runtime, name string) {
	t.Helper()
	got, ok := rt.SettingValue(name)
	assert.False(t, ok, "setting %q must be unset", name)
	assert.Nil(t, got, "unset setting %q must return nil", name)
}

func TestRuntimeSettingValueCold(t *testing.T) {
	for _, tc := range []struct {
		name string
		rt   *Runtime
	}{
		{"NewEnv", NewEnv(nil).Runtime},
		{"StandardRuntime", StandardRuntime()},
	} {
		t.Run(tc.name, func(t *testing.T) {
			assertNoRuntimeSettingValue(t, tc.rt, "missing")
			assertNoRuntimeSettingValue(t, tc.rt, "")
		})
	}
}

func TestRuntimeSettingValueRoundTrip(t *testing.T) {
	for _, tc := range settingValueTestValues() {
		t.Run(tc.name, func(t *testing.T) {
			rt := NewEnv(nil).Runtime
			require.NoError(t, rt.SetSettingValue("value", tc.value))
			assertRuntimeSettingValue(t, rt, "value", tc.value)
		})
	}
}

func TestRuntimeSettingValueBytes(t *testing.T) {
	t.Run("copy_on_set", func(t *testing.T) {
		rt := NewEnv(nil).Runtime
		input := []byte("loaded")
		require.NoError(t, rt.SetSettingValue("bytes", input))
		input[0] = 'X'
		assertRuntimeSettingValue(t, rt, "bytes", []byte("loaded"))
	})

	t.Run("copy_on_read", func(t *testing.T) {
		rt := NewEnv(nil).Runtime
		require.NoError(t, rt.SetSettingValue("bytes", []byte("loaded")))
		first, ok := rt.SettingValue("bytes")
		require.True(t, ok)
		require.IsType(t, []byte(nil), first)
		firstBytes := first.([]byte)
		require.Equal(t, []byte("loaded"), firstBytes)
		firstBytes[0] = 'X'
		second, ok := rt.SettingValue("bytes")
		require.True(t, ok)
		require.IsType(t, []byte(nil), second)
		secondBytes := second.([]byte)
		require.Equal(t, []byte("loaded"), secondBytes)
		secondBytes[1] = 'Y'
		assert.Equal(t, []byte("Xoaded"), firstBytes)
		assertRuntimeSettingValue(t, rt, "bytes", []byte("loaded"))
	})

	for _, tc := range []struct {
		name  string
		value []byte
	}{
		{"nil", nil},
		{"empty", []byte{}},
		{"empty_with_capacity", make([]byte, 0, 8)},
	} {
		t.Run(tc.name, func(t *testing.T) {
			rt := NewEnv(nil).Runtime
			require.NoError(t, rt.SetSettingValue("bytes", tc.value))
			got, ok := rt.SettingValue("bytes")
			require.True(t, ok)
			require.IsType(t, []byte(nil), got)
			assert.Len(t, got.([]byte), 0)
		})
	}
}

func TestRuntimeSettingValueRejectsTypes(t *testing.T) {
	text := "loaded"
	marked := runtimeSettingImmutable{id: "loaded", version: 1}
	for _, tc := range []struct {
		name  string
		value any
	}{
		{"nil", nil},
		{"pointer", new(int)},
		{"marked_pointer", &marked},
		{"nil_marked_pointer", (*runtimeSettingImmutable)(nil)},
		{"marker_pointer", &templatepolicy.Marker{}},
		{"lval_pointer", String("loaded")},
		{"nil_lval_pointer", (*LVal)(nil)},
		{"string_pointer", &text},
		{"nil_string_pointer", (*string)(nil)},
		{"bytes_pointer", &[]byte{1}},
		{"lval_value", *String("loaded")},
		{"unmarked_struct", struct{ Version int }{Version: 1}},
		{"struct_with_map", struct{ Data map[string]int }{Data: map[string]int{"version": 1}}},
		{"map", map[string]int{"version": 1}},
		{"nil_map", map[string]int(nil)},
		{"chan", make(chan int)},
		{"nil_chan", (chan int)(nil)},
		{"func", func() {}},
		{"nil_func", (func())(nil)},
		{"array", [2]byte{1, 2}},
		{"string_slice", []string{"loaded"}},
		{"nil_string_slice", []string(nil)},
		{"int_slice", []int{1}},
		{"named_byte_slice", runtimeSettingBytes{1, 2}},
		{"nil_named_byte_slice", runtimeSettingBytes(nil)},
		{"uintptr", uintptr(1)},
		{"unsafe_pointer", unsafe.Pointer(&text)},
		{"nil_unsafe_pointer", unsafe.Pointer(nil)},
	} {
		t.Run(tc.name, func(t *testing.T) {
			rt := NewEnv(nil).Runtime
			require.NoError(t, rt.SetSettingValue("existing", "original"))
			for _, name := range []string{"existing", "unset"} {
				err := rt.SetSettingValue(name, tc.value)
				if assert.Error(t, err, "setting %q must reject %T", name, tc.value) {
					assert.Contains(t, err.Error(), fmt.Sprintf("%q", name))
					assert.Contains(t, err.Error(), fmt.Sprintf("%T", tc.value))
				}
			}
			assertRuntimeSettingValue(t, rt, "existing", "original")
			assertNoRuntimeSettingValue(t, rt, "unset")
		})
	}
}

func TestRuntimeSettingValueDelete(t *testing.T) {
	rt := NewEnv(nil).Runtime
	rt.DeleteSettingValue("missing")
	assertNoRuntimeSettingValue(t, rt, "missing")
	require.NoError(t, rt.SetSettingValue("remove", "loaded"))
	require.NoError(t, rt.SetSettingValue("keep", int64(7)))
	assertRuntimeSettingValue(t, rt, "remove", "loaded")
	rt.DeleteSettingValue("remove")
	assertNoRuntimeSettingValue(t, rt, "remove")
	assertRuntimeSettingValue(t, rt, "keep", int64(7))
	rt.DeleteSettingValue("remove")
	assertNoRuntimeSettingValue(t, rt, "remove")
	assertRuntimeSettingValue(t, rt, "keep", int64(7))
	require.NoError(t, rt.SetSettingValue("remove", "restored"))
	assertRuntimeSettingValue(t, rt, "remove", "restored")
}

func TestRuntimeSettingValueNamespaces(t *testing.T) {
	rt := NewEnv(nil).Runtime
	rt.SetSetting("shared", true)
	assertNoRuntimeSettingValue(t, rt, "shared")
	require.NoError(t, rt.SetSettingValue("shared", "loaded"))
	assertRuntimeSettingValue(t, rt, "shared", "loaded")
	value, ok := rt.Setting("shared")
	assert.True(t, ok)
	assert.True(t, value)

	rt.SetSetting("shared", false)
	assertRuntimeSettingValue(t, rt, "shared", "loaded")
	require.NoError(t, rt.SetSettingValue("shared", int64(7)))
	assertRuntimeSettingValue(t, rt, "shared", int64(7))
	value, ok = rt.Setting("shared")
	assert.True(t, ok)
	assert.False(t, value)

	rt.DeleteSettingValue("shared")
	assertNoRuntimeSettingValue(t, rt, "shared")
	value, ok = rt.Setting("shared")
	assert.True(t, ok)
	assert.False(t, value)

	require.NoError(t, rt.SetSettingValue("value_only", "loaded"))
	assertRuntimeSettingValue(t, rt, "value_only", "loaded")
	value, ok = rt.Setting("value_only")
	assert.False(t, ok)
	assert.False(t, value)
}

func TestRuntimeSettingValueTemplate(t *testing.T) {
	source := NewEnv(nil)
	values := settingValueTestValues()
	for _, tc := range values {
		require.NoError(t, source.Runtime.SetSettingValue(tc.name, tc.value))
	}
	tmpl, err := NewTemplate(source)
	require.NoError(t, err)
	for i := range 3 {
		t.Run(fmt.Sprintf("vm_%d", i), func(t *testing.T) {
			vm, err := tmpl.NewVM()
			require.NoError(t, err)
			for _, tc := range values {
				assertRuntimeSettingValue(t, vm.Runtime, tc.name, tc.value)
			}
		})
	}
}

func TestRuntimeSettingValueForkIsolation(t *testing.T) {
	source := NewEnv(nil)
	require.NoError(t, source.Runtime.SetSettingValue("overwrite", "source"))
	require.NoError(t, source.Runtime.SetSettingValue("remove", "keep"))
	require.NoError(t, source.Runtime.SetSettingValue("bytes", []byte("source")))
	tmpl, err := NewTemplate(source)
	require.NoError(t, err)
	a, err := tmpl.NewVM()
	require.NoError(t, err)
	b, err := tmpl.NewVM()
	require.NoError(t, err)

	require.NoError(t, a.Runtime.SetSettingValue("new", "a"))
	assertRuntimeSettingValue(t, a.Runtime, "new", "a")
	require.NoError(t, a.Runtime.SetSettingValue("new", "a2"))
	require.NoError(t, a.Runtime.SetSettingValue("overwrite", int64(7)))
	require.NoError(t, a.Runtime.SetSettingValue("bytes", []byte("a")))
	a.Runtime.DeleteSettingValue("remove")
	assertRuntimeSettingValue(t, a.Runtime, "new", "a2")
	assertRuntimeSettingValue(t, a.Runtime, "overwrite", int64(7))
	assertRuntimeSettingValue(t, a.Runtime, "bytes", []byte("a"))
	assertNoRuntimeSettingValue(t, a.Runtime, "remove")

	c, err := tmpl.NewVM()
	require.NoError(t, err)
	for _, tc := range []struct {
		name string
		rt   *Runtime
	}{
		{"source", source.Runtime},
		{"vm_b", b.Runtime},
		{"vm_c", c.Runtime},
	} {
		t.Run(tc.name, func(t *testing.T) {
			assertRuntimeSettingValue(t, tc.rt, "overwrite", "source")
			assertRuntimeSettingValue(t, tc.rt, "remove", "keep")
			assertRuntimeSettingValue(t, tc.rt, "bytes", []byte("source"))
			assertNoRuntimeSettingValue(t, tc.rt, "new")
		})
	}
}

func TestRuntimeSettingValueSourceAfterPublication(t *testing.T) {
	source := NewEnv(nil)
	require.NoError(t, source.Runtime.SetSettingValue("overwrite", "published"))
	require.NoError(t, source.Runtime.SetSettingValue("remove", "published"))
	tmpl, err := NewTemplate(source)
	require.NoError(t, err)
	earlier, err := tmpl.NewVM()
	require.NoError(t, err)

	require.NoError(t, source.Runtime.SetSettingValue("overwrite", "source"))
	require.NoError(t, source.Runtime.SetSettingValue("new", "source"))
	source.Runtime.DeleteSettingValue("remove")
	assertRuntimeSettingValue(t, source.Runtime, "overwrite", "source")
	assertRuntimeSettingValue(t, source.Runtime, "new", "source")
	assertNoRuntimeSettingValue(t, source.Runtime, "remove")
	later, err := tmpl.NewVM()
	require.NoError(t, err)
	for _, vm := range []*LEnv{earlier, later} {
		assertRuntimeSettingValue(t, vm.Runtime, "overwrite", "published")
		assertRuntimeSettingValue(t, vm.Runtime, "remove", "published")
		assertNoRuntimeSettingValue(t, vm.Runtime, "new")
	}
}

func TestRuntimeSettingValueEmptyTemplate(t *testing.T) {
	source := NewEnv(nil)
	tmpl, err := NewTemplate(source)
	require.NoError(t, err)
	a, err := tmpl.NewVM()
	require.NoError(t, err)
	assertNoRuntimeSettingValue(t, a.Runtime, "local")
	require.NoError(t, a.Runtime.SetSettingValue("local", "a"))
	assertRuntimeSettingValue(t, a.Runtime, "local", "a")
	b, err := tmpl.NewVM()
	require.NoError(t, err)
	assertNoRuntimeSettingValue(t, b.Runtime, "local")
	assertNoRuntimeSettingValue(t, source.Runtime, "local")
}

func TestRuntimeSettingValueRepublish(t *testing.T) {
	for _, tc := range []struct {
		name    string
		written bool
	}{
		{"without_writes", false},
		{"with_writes", true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			source := NewEnv(nil)
			require.NoError(t, source.Runtime.SetSettingValue("inherited", runtimeSettingVersion("v1")))
			require.NoError(t, source.Runtime.SetSettingValue("remove", "keep"))
			require.NoError(t, source.Runtime.SetSettingValue("bytes", []byte("source")))
			tmpl, err := NewTemplate(source)
			require.NoError(t, err)
			vm, err := tmpl.NewVM()
			require.NoError(t, err)
			want := runtimeSettingVersion("v1")
			if tc.written {
				want = runtimeSettingVersion("v2")
				require.NoError(t, vm.Runtime.SetSettingValue("inherited", want))
				require.NoError(t, vm.Runtime.SetSettingValue("local", "vm"))
				vm.Runtime.DeleteSettingValue("remove")
			}
			republished, err := NewTemplate(vm)
			require.NoError(t, err)
			require.NoError(t, vm.Runtime.SetSettingValue("inherited", runtimeSettingVersion("v3")))
			vm.Runtime.DeleteSettingValue("local")
			for range 2 {
				fork, err := republished.NewVM()
				require.NoError(t, err)
				assertRuntimeSettingValue(t, fork.Runtime, "inherited", want)
				assertRuntimeSettingValue(t, fork.Runtime, "bytes", []byte("source"))
				if tc.written {
					assertRuntimeSettingValue(t, fork.Runtime, "local", "vm")
					assertNoRuntimeSettingValue(t, fork.Runtime, "remove")
				} else {
					assertNoRuntimeSettingValue(t, fork.Runtime, "local")
					assertRuntimeSettingValue(t, fork.Runtime, "remove", "keep")
				}
			}
			original, err := tmpl.NewVM()
			require.NoError(t, err)
			for _, rt := range []*Runtime{source.Runtime, original.Runtime} {
				assertRuntimeSettingValue(t, rt, "inherited", runtimeSettingVersion("v1"))
				assertRuntimeSettingValue(t, rt, "remove", "keep")
				assertNoRuntimeSettingValue(t, rt, "local")
			}
		})
	}
}

func TestRuntimeSettingValueConcurrentForks(t *testing.T) {
	source := NewEnv(nil)
	require.NoError(t, source.Runtime.SetSettingValue("id", "template"))
	require.NoError(t, source.Runtime.SetSettingValue("remove", "keep"))
	require.NoError(t, source.Runtime.SetSettingValue("bytes", []byte("template")))
	tmpl, err := NewTemplate(source)
	require.NoError(t, err)

	const workers = 16
	start := make(chan struct{})
	var wg, writes sync.WaitGroup
	wg.Add(workers)
	writes.Add(workers)
	for worker := range workers {
		go func() {
			defer wg.Done()
			<-start
			vm, err := tmpl.NewVM()
			if !assert.NoError(t, err, "worker %d", worker) {
				writes.Done()
				return
			}
			assertRuntimeSettingValue(t, vm.Runtime, "id", "template")
			assertRuntimeSettingValue(t, vm.Runtime, "remove", "keep")
			assertRuntimeSettingValue(t, vm.Runtime, "bytes", []byte("template"))
			assertNoRuntimeSettingValue(t, vm.Runtime, "local")
			assert.NoError(t, vm.Runtime.SetSettingValue("id", worker))
			assert.NoError(t, vm.Runtime.SetSettingValue("local", worker))
			assert.NoError(t, vm.Runtime.SetSettingValue("bytes", []byte{byte(worker)}))
			assertRuntimeSettingValue(t, vm.Runtime, "id", worker)
			vm.Runtime.DeleteSettingValue("remove")
			writes.Done()
			writes.Wait()
			assertRuntimeSettingValue(t, vm.Runtime, "id", worker)
			assertRuntimeSettingValue(t, vm.Runtime, "local", worker)
			assertRuntimeSettingValue(t, vm.Runtime, "bytes", []byte{byte(worker)})
			assertNoRuntimeSettingValue(t, vm.Runtime, "remove")
		}()
	}
	close(start)
	wg.Wait()

	vm, err := tmpl.NewVM()
	require.NoError(t, err)
	for _, rt := range []*Runtime{source.Runtime, vm.Runtime} {
		assertRuntimeSettingValue(t, rt, "id", "template")
		assertRuntimeSettingValue(t, rt, "remove", "keep")
		assertRuntimeSettingValue(t, rt, "bytes", []byte("template"))
		assertNoRuntimeSettingValue(t, rt, "local")
	}
}

func TestRuntimeValueSettingBooleanTemplateRegression(t *testing.T) {
	source := NewEnv(nil)
	value, ok := source.Runtime.Setting("enabled")
	assert.False(t, ok)
	assert.False(t, value)
	source.Runtime.SetSetting("enabled", true)
	source.Runtime.SetSetting("disabled", false)
	tmpl, err := NewTemplate(source)
	require.NoError(t, err)
	a, err := tmpl.NewVM()
	require.NoError(t, err)
	b, err := tmpl.NewVM()
	require.NoError(t, err)
	a.Runtime.SetSetting("enabled", false)
	a.Runtime.SetSetting("disabled", true)
	a.Runtime.SetSetting("local", true)
	source.Runtime.SetSetting("enabled", false)
	source.Runtime.SetSetting("after_publication", true)
	c, err := tmpl.NewVM()
	require.NoError(t, err)
	for _, vm := range []*LEnv{b, c} {
		value, ok := vm.Runtime.Setting("enabled")
		assert.True(t, ok)
		assert.True(t, value)
		value, ok = vm.Runtime.Setting("disabled")
		assert.True(t, ok)
		assert.False(t, value)
		for _, name := range []string{"local", "after_publication"} {
			value, ok = vm.Runtime.Setting(name)
			assert.False(t, ok)
			assert.False(t, value)
		}
	}
	value, ok = source.Runtime.Setting("local")
	assert.False(t, ok)
	assert.False(t, value)
	value, ok = source.Runtime.Setting("disabled")
	assert.True(t, ok)
	assert.False(t, value)

	fork, err := forkTestSnapshot(a)
	require.NoError(t, err)
	for _, rt := range []*Runtime{a.Runtime, fork.Runtime} {
		value, ok := rt.Setting("enabled")
		assert.True(t, ok)
		assert.False(t, value)
		for _, name := range []string{"disabled", "local"} {
			value, ok = rt.Setting(name)
			assert.True(t, ok)
			assert.True(t, value)
		}
	}

	empty, err := NewTemplate(NewEnv(nil))
	require.NoError(t, err)
	first, err := empty.NewVM()
	require.NoError(t, err)
	first.Runtime.SetSetting("local", true)
	next, err := empty.NewVM()
	require.NoError(t, err)
	value, ok = next.Runtime.Setting("local")
	assert.False(t, ok)
	assert.False(t, value)
}
