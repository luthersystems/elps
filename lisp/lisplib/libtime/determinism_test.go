// Copyright © 2026 The ELPS authors

package libtime_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// The time package must not offer builtins whose result depends on the wall
// clock or on goroutine timing: embedders such as chaincode need every
// evaluation of the same program to agree (luthersystems/elps#757).  Hosts
// that want a clock or a sleep register their own; libtime.BuiltinSleep
// stays exported for that.
func TestTimePackageHasNoNonDeterministicBuiltins(t *testing.T) {
	env := timeTestEnv(t)
	pkg := env.Runtime.Registry.Package("time")
	require.NotNil(t, pkg, "time package not loaded")
	for _, name := range []string{"utc-now", "time-elapsed", "sleep"} {
		assert.NotContains(t, pkg.Externals(), name, "time:%s is exported", name)
		_, ok := pkg.Symbol(name)
		assert.False(t, ok, "time:%s is bound", name)
	}
	// The deterministic surface survives.
	for _, name := range []string{"time-from", "time-add", "parse-rfc3339", "parse-duration"} {
		assert.Contains(t, pkg.Externals(), name)
	}
}
