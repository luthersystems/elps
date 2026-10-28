// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/lisp/lisplib/libschema"
	"github.com/stretchr/testify/require"
)

// chainDepth is the number of nested objects in the chain tests.
const chainDepth = 5

// chainTypes returns the source of record types n1 ... nN in package tree.
// Each nK, K < N, has a field child of type nK+1.  nN has the int field v
// and, when strict, the int field w.  n1 also has a field extra of type
// s:any, which no record validator checks.  Each :check adds one to
// tree:checks.
func chainTypes(n int, strict bool) string {
	var b strings.Builder
	b.WriteString("(in-package 'tree)\n(set 'checks 0)\n")
	b.WriteString("(defun count-check (o) (set! checks (+ 1 checks)))\n")
	for k := n; k >= 1; k-- {
		fmt.Fprintf(&b, "(set 'n%d (s:make-record \"n%d\"", k, k)
		if k == n {
			b.WriteString(` (s:has-key "v" s:int)`)
			if strict {
				b.WriteString(` (s:has-key "w" s:int)`)
			}
		} else {
			fmt.Fprintf(&b, ` (s:has-key "child" n%d)`, k+1)
		}
		if k == 1 {
			b.WriteString(` (s:may-have-key "extra" s:any)`)
		}
		b.WriteString(" :check count-check))\n")
	}
	return b.String()
}

// chainValue returns the source of a chain of n objects.  The root's extra
// field holds the deepest object again and one more leaf, which only the
// walk of ValidateLoaded reaches.
func chainValue(n int) string {
	var b strings.Builder
	b.WriteString("(in-package 'tree)\n")
	fmt.Fprintf(&b, "(set 'leaf (n%d :v 1 :w 1))\n", n)
	fmt.Fprintf(&b, "(set 'other (n%d :v 2 :w 2))\n", n)
	b.WriteString("(set 'root leaf)\n")
	for k := n - 1; k >= 1; k-- {
		if k == 1 {
			b.WriteString("(set 'root (n1 :child root :extra (list leaf other)))\n")
			continue
		}
		fmt.Fprintf(&b, "(set 'root (n%d :child root))\n", k)
	}
	b.WriteString("(set 'checks 0)\nroot\n")
	return b.String()
}

// TestValidateLoadedChecksEachObjectOnce runs ValidateLoaded on a chain of
// nested objects.  Each object's :check runs once: the field validators of
// the ancestors and the walk of ValidateLoaded do not check an object
// again.
func TestValidateLoadedChecksEachObjectOnce(t *testing.T) {
	env := newSchemaForkEnv(t)
	evalString(t, env, chainTypes(chainDepth, true))
	root := evalString(t, env, chainValue(chainDepth))
	require.NoError(t, libschema.ValidateLoaded(env, root, root))
	// chainDepth objects in the chain, and other, which only the walk
	// reaches through the s:any field.
	require.Equal(t, chainDepth+1, evalString(t, env, `tree:checks`).Int)

	// Each call checks the objects again.
	require.NoError(t, libschema.ValidateLoaded(env, root))
	require.Equal(t, 2*(chainDepth+1), evalString(t, env, `tree:checks`).Int)

	// s:validate outside ValidateLoaded checks the nested objects as
	// before: one :check for each object in the chain.
	evalString(t, env, `(set 'tree:checks 0) (s:validate tree:n1 tree:root)`)
	require.Equal(t, chainDepth, evalString(t, env, `tree:checks`).Int)
}

// TestValidateLoadedNestedError loads a chain into a VM whose leaf type
// requires a field the saved leaf does not have.  The error is the one the
// root's field validators report.
func TestValidateLoadedNestedError(t *testing.T) {
	reg := recordRegistry(t)
	src := newSchemaForkEnv(t)
	evalString(t, src, chainTypes(chainDepth, false))
	root := evalString(t, src, strings.NewReplacer(" :w 1", "", " :w 2", "").Replace(chainValue(chainDepth)))
	b, err := libjson.DumpDurable(src, root, reg)
	require.NoError(t, err)

	dst := newSchemaForkEnv(t)
	evalString(t, dst, chainTypes(chainDepth, true))
	got, err := libjson.LoadDurable(dst, b, reg)
	require.NoError(t, err)
	err = libschema.ValidateLoaded(dst, got)
	require.Error(t, err)
	require.Equal(t, "test.lisp:8:1: wrong-type: tree:n1: Key child was of wrong type", err.Error())
}
