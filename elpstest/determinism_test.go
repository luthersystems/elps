// Copyright © 2026 The ELPS authors

package elpstest_test

import (
	"encoding/hex"
	"io"
	"testing"
	"time"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

func TestStepClock(t *testing.T) {
	t.Parallel()
	start := time.Date(2024, time.March, 1, 12, 0, 0, 0, time.UTC)
	clock := elpstest.StepClock(start, time.Minute)
	require.Equal(t, start, clock())
	require.Equal(t, start.Add(time.Minute), clock())
	require.Equal(t, start.Add(2*time.Minute), clock())

	frozen := elpstest.StepClock(start, 0)
	require.Equal(t, start, frozen())
	require.Equal(t, start, frozen())
}

func readN(t *testing.T, r io.Reader, n int) []byte {
	t.Helper()
	buf := make([]byte, n)
	_, err := io.ReadFull(r, buf)
	require.NoError(t, err)
	return buf
}

// TestSeededEntropyGolden pins the byte stream: tests that record ids
// derived from a seed must see the same ids on every platform and release.
func TestSeededEntropyGolden(t *testing.T) {
	t.Parallel()
	require.Equal(t, seededEntropyGolden, hex.EncodeToString(readN(t, elpstest.SeededEntropy(1), 16)))
	first := readN(t, elpstest.SeededEntropy(7), 64)
	second := readN(t, elpstest.SeededEntropy(7), 64)
	require.Equal(t, first, second)
	require.NotEqual(t, readN(t, elpstest.SeededEntropy(7), 16), readN(t, elpstest.SeededEntropy(8), 16))

	// Reads of any size see one stream: 3+40+5 bytes equal one 48-byte read.
	whole := readN(t, elpstest.SeededEntropy(9), 48)
	r := elpstest.SeededEntropy(9)
	var parts []byte
	for _, n := range []int{3, 40, 5} {
		parts = append(parts, readN(t, r, n)...)
	}
	require.Equal(t, whole, parts)
}

// TestRuntimeDefaults pins that without a hook the
// runtime reads the real clock and crypto/rand.
func TestRuntimeDefaults(t *testing.T) {
	t.Parallel()
	env := lisp.NewEnv(nil)
	before := time.Now()
	require.False(t, env.Runtime.Now().Before(before))
	require.NotNil(t, env.Runtime.Random())
	require.Len(t, readN(t, env.Runtime.Random(), 8), 8)
}

func TestWithClockDrivesUTCNow(t *testing.T) {
	t.Parallel()
	start := time.Date(2030, time.June, 15, 8, 30, 0, 0, time.UTC)
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.True(t, lisp.InitializeUserEnv(env,
		lisp.WithClock(elpstest.StepClock(start, time.Hour)),
		lisp.WithEntropy(elpstest.SeededEntropy(1))).IsNil())
	require.True(t, lisplib.LoadLibrary(env).IsNil())
	require.True(t, env.InPackage(lisp.String(lisp.DefaultUserPackage)).IsNil())
	v := env.LoadString("clock.lisp", `(list (time:format-rfc3339 (time:utc-now)) (time:format-rfc3339 (time:utc-now)))`)
	require.Equal(t, `'("2030-06-15T08:30:00Z" "2030-06-15T09:30:00Z")`, v.String())
	require.Equal(t, seededEntropyGolden, hex.EncodeToString(readN(t, env.Runtime.Random(), 16)))
}

func TestTemplateVMsDoNotInheritClockOrEntropy(t *testing.T) {
	t.Parallel()
	start := time.Date(2030, time.June, 15, 8, 30, 0, 0, time.UTC)
	src := lisp.NewEnv(nil)
	src.Runtime.Reader = parser.NewReader()
	require.True(t, lisp.InitializeUserEnv(src,
		lisp.WithClock(elpstest.StepClock(start, 0)),
		lisp.WithEntropy(elpstest.SeededEntropy(1))).IsNil())
	tmpl, err := lisp.NewTemplate(src, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	require.NoError(t, err)

	plain, err := tmpl.NewVM()
	require.NoError(t, err)
	require.Nil(t, plain.Runtime.Clock)
	require.Nil(t, plain.Runtime.Entropy)

	vmStart := start.Add(time.Hour)
	vm, err := tmpl.NewVM(lisp.VMWithClock(elpstest.StepClock(vmStart, 0)),
		lisp.VMWithEntropy(elpstest.SeededEntropy(1)))
	require.NoError(t, err)
	require.Equal(t, vmStart, vm.Runtime.Now())
	require.Equal(t, seededEntropyGolden, hex.EncodeToString(readN(t, vm.Runtime.Random(), 16)))
}

func TestRunnerDeterminism(t *testing.T) {
	r := &elpstest.Runner{Determinism: &elpstest.Determinism{Step: time.Second, Seed: 1}}
	r.RunTestFile(t, "testdata/determinism/clock_test.lisp")

	// Every environment gets a fresh entropy stream from the same seed.
	a, err := r.NewEnv(t)
	require.NoError(t, err)
	b, err := r.NewEnv(t)
	require.NoError(t, err)
	require.Equal(t, seededEntropyGolden, hex.EncodeToString(readN(t, a.Runtime.Random(), 16)))
	require.Equal(t, seededEntropyGolden, hex.EncodeToString(readN(t, b.Runtime.Random(), 16)))
	require.Equal(t, elpstest.DefaultTestTime, b.Runtime.Now())
}

const seededEntropyGolden = "783825822a6f9e62da2190e828e4c9d2" // SHA-256(be64(1) || be64(0))[:16]
