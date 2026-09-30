// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"math"
	"math/rand/v2"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzval"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/require"
)

// All finite number text must match the real plain encoder, except the
// documented .0 suffix preserving float type. Large ints and nonfinite
// floats deliberately use Transit tags instead of JSON number text.
func checkTypedNumberText(t *testing.T, v *lisp.LVal) {
	t.Helper()
	plain, plainErr := libjson.Dump(v, false)
	typed, err := libjson.DumpTyped(v)
	require.NoError(t, err)
	want := string(plain)
	switch v.Type {
	case lisp.LInt:
		require.NoError(t, plainErr)
		if int64(v.Int) < -(1<<53) || int64(v.Int) > 1<<53 {
			want = `"~n` + want + `"`
		}
	case lisp.LFloat:
		switch {
		case math.IsNaN(v.Float):
			require.Error(t, plainErr)
			want = `"~zNaN"`
		case math.IsInf(v.Float, 1):
			require.Error(t, plainErr)
			want = `"~zINF"`
		case math.IsInf(v.Float, -1):
			require.Error(t, plainErr)
			want = `"~z-INF"`
		default:
			require.NoError(t, plainErr)
			if !strings.ContainsAny(want, ".e") {
				want += ".0"
			}
		}
	default:
		t.Fatalf("not a number: %v", v.Type)
	}
	require.Equal(t, want, string(typed), "value %s", v)
	back, err := libjson.LoadTyped(typed)
	require.NoError(t, err)
	require.Equal(t, v.Type, back.Type)
	if v.Type == lisp.LInt {
		require.Equal(t, v.Int, back.Int)
	} else if math.IsNaN(v.Float) {
		require.True(t, math.IsNaN(back.Float))
	} else {
		require.Equal(t, math.Float64bits(v.Float), math.Float64bits(back.Float))
	}
}

func checkTypedNumbersInValue(t *testing.T, v *lisp.LVal, seen map[*lisp.LVal]bool) {
	t.Helper()
	if v == nil || seen[v] {
		return
	}
	seen[v] = true
	if v.Type == lisp.LInt || v.Type == lisp.LFloat {
		checkTypedNumberText(t, v)
	}
	for _, c := range v.Cells {
		checkTypedNumbersInValue(t, c, seen)
	}
	if v.Type == lisp.LSortMap {
		for _, p := range v.MapEntries().Cells {
			checkTypedNumbersInValue(t, p, seen)
		}
	}
}

func TestTypedNumberTextPlainCorpus(t *testing.T) {
	// The numeric values in TestPlainGoldenDocument, plus notation cutoffs
	// and signed zero, subnormal, exact-integer and platform-int boundaries.
	for _, x := range []float64{
		0, math.Copysign(0, -1), 1, -1, 1.5, 0.1,
		1e-6, math.Nextafter(1e-6, 0), 1e-7, 1e20, math.Nextafter(1e21, 0), 1e21,
		1e23, 1e300, math.SmallestNonzeroFloat64, math.MaxFloat64,
		math.NaN(), math.Inf(1), math.Inf(-1),
	} {
		checkTypedNumberText(t, lisp.Float(x))
	}
	for _, n := range []int64{0, 1, -1, math.MinInt32, math.MaxInt32, 1<<53 - 1, 1 << 53, 1<<53 + 1, math.MinInt64, math.MaxInt64} {
		if n >= math.MinInt && n <= math.MaxInt {
			checkTypedNumberText(t, lisp.Int(int(n)))
		}
	}
	// Replay the same value seeds as FuzzDumpJSON and FuzzDumpExactIntegers.
	env := newJSONEnv(t)
	for _, seed := range fuzzval.Seeds() {
		checkTypedNumbersInValue(t, fuzzval.New(seed, env).Value(), make(map[*lisp.LVal]bool))
	}
	// Replay numeric values decoded from the plain decoder's saved corpus.
	files, err := filepath.Glob("testdata/fuzz/FuzzLoad*/*")
	require.NoError(t, err)
	require.NotEmpty(t, files)
	for _, file := range files {
		b, err := os.ReadFile(file) //nolint:gosec // G304: fixed repository fuzz corpus
		require.NoError(t, err)
		for _, line := range strings.Split(string(b), "\n") {
			if !strings.HasPrefix(line, "[]byte(") {
				continue
			}
			s, err := strconv.Unquote(strings.TrimSuffix(strings.TrimPrefix(line, "[]byte("), ")"))
			require.NoError(t, err)
			for _, exact := range []bool{false, true} {
				v := libjson.LoadWith([]byte(s), libjson.LoadOpts{ExactIntegers: exact})
				if v.Type != lisp.LError {
					checkTypedNumbersInValue(t, v, make(map[*lisp.LVal]bool))
				}
			}
		}
	}
}

func TestTypedNumberTextRandomFloat64(t *testing.T) {
	rng := rand.New(rand.NewPCG(751, 1)) //nolint:gosec // G404: deterministic property-test inputs, not security randomness
	for range 10000 {
		checkTypedNumberText(t, lisp.Float(math.Float64frombits(rng.Uint64())))
	}
}
