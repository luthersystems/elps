// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type vmCacheKey struct{}

// The VM cache is per Runtime: a template VM and another runtime start
// empty, and a write in one VM is seen by no other.
func TestVMCacheIsPerVM(t *testing.T) {
	env := templateTestEnv(t)
	rt := env.Runtime
	assert.Nil(t, rt.VMCache(vmCacheKey{}))

	entry := &struct{ n int }{n: 1}
	rt.SetVMCache(vmCacheKey{}, entry)
	assert.Same(t, entry, rt.VMCache(vmCacheKey{}))
	assert.Nil(t, rt.VMCache("other key"))

	tmpl, err := lisp.NewTemplate(env, templateCorePolicy())
	require.NoError(t, err)
	vm1, err := tmpl.NewVM()
	require.NoError(t, err)
	vm2, err := tmpl.NewVM()
	require.NoError(t, err)
	assert.Nil(t, vm1.Runtime.VMCache(vmCacheKey{}), "a template VM started with the source's cache")
	vm1.Runtime.SetVMCache(vmCacheKey{}, 2)
	assert.Equal(t, 2, vm1.Runtime.VMCache(vmCacheKey{}))
	assert.Nil(t, vm2.Runtime.VMCache(vmCacheKey{}), "a write in one VM reached another")
	assert.Same(t, entry, rt.VMCache(vmCacheKey{}), "a write in a VM reached the source")

	rt.SetVMCache(vmCacheKey{}, nil)
	assert.Nil(t, rt.VMCache(vmCacheKey{}))
}
