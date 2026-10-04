// Copyright © 2026 The ELPS authors

package libjson

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

type goldenJSONCase struct {
	fixture    goldenFixture
	name       string
	opts       []TypedOption
	failCharge int
}

func goldenJSONCases() []goldenJSONCase {
	fixtures := goldenFixtures()
	byName := make(map[string]goldenFixture)
	var cases []goldenJSONCase
	for _, f := range fixtures {
		byName[f.name] = f
		c := goldenJSONCase{fixture: f, name: "corpus/" + f.name}
		// Tree expansion is bounded before a 30-level DAG allocates a billion cells.
		if strings.HasPrefix(f.name, "shared-dag-30") {
			c.opts = []TypedOption{WithTypedMaxValues(4096), WithTypedMaxBytes(8192)}
		}
		if f.name == "cycle-before-boundary" {
			c.opts = []TypedOption{WithTypedMaxDepth(3)}
		}
		cases = append(cases, c)
	}
	add := func(name, fixture string, opts ...TypedOption) {
		cases = append(cases, goldenJSONCase{fixture: byName[fixture], name: name, opts: opts})
	}
	for _, kind := range []string{"list", "vector", "map", "tagged", "quote"} {
		for _, depth := range []int{3, 4} {
			add(fmt.Sprintf("depth-limit-3/%s-%d", kind, depth), fmt.Sprintf("depth-%s-%d", kind, depth), WithTypedMaxDepth(3))
		}
	}
	for _, n := range []int{0, -1} {
		for _, fixture := range []string{"int", "nil-list", "empty-vector", "vector", "map-empty", "tagged", "go-nil", "self-vector"} {
			add(fmt.Sprintf("depth-limit-%d/%s", n, fixture), fixture, WithTypedMaxDepth(n))
			add(fmt.Sprintf("value-limit-%d/%s", n, fixture), fixture, WithTypedMaxValues(n))
			add(fmt.Sprintf("container-byte-limit-%d/%s", n, fixture), fixture, WithTypedMaxBytes(n))
		}
	}
	for _, fixture := range []string{"vector", "list", "map-string-keys", "array-multi", "tagged", "wire-list", "wire-tagged", "wire-array"} {
		for _, n := range []int{1, 2, 3, 4, 5, 6, 7} {
			add(fmt.Sprintf("value-limit-%d/%s", n, fixture), fixture, WithTypedMaxValues(n))
		}
		for n := range 41 {
			add(fmt.Sprintf("byte-limit-%d/%s", n, fixture), fixture, WithTypedMaxBytes(n))
		}
	}
	for _, fixture := range []string{"int", "string", "bytes", "nil-list", "false", "string-escapes"} {
		add("value-boundary-1/"+fixture, fixture, WithTypedMaxValues(1))
		for n := range 65 {
			add(fmt.Sprintf("scalar-byte-limit-%d/%s", n, fixture), fixture, WithTypedMaxBytes(n))
		}
	}
	for _, fixture := range []string{"self-list", "self-vector", "self-map", "self-tagged", "branching-cycle"} {
		add("precedence-cycle-count-depth/"+fixture, fixture, WithTypedMaxDepth(1), WithTypedMaxValues(1))
		add("precedence-cycle-depth-bytes/"+fixture, fixture, WithTypedMaxDepth(1), WithTypedMaxBytes(1))
	}
	// These pairs reach both checks at the same value or inter-child event.
	for _, fixture := range []string{"vector", "map-string-keys", "tagged", "array-no-cells", "tagged-no-cell", "self-vector", "string-invalid-utf8", "go-nil"} {
		add("precedence-count-depth/"+fixture, fixture, WithTypedMaxValues(0), WithTypedMaxDepth(0))
		add("precedence-count-bytes/"+fixture, fixture, WithTypedMaxValues(0), WithTypedMaxBytes(0))
		add("precedence-depth-bytes/"+fixture, fixture, WithTypedMaxDepth(0), WithTypedMaxBytes(0))
		add("precedence-child-count-bytes/"+fixture, fixture, WithTypedMaxValues(2), WithTypedMaxBytes(2))
	}
	for _, fixture := range []string{"charge-long-string", "charge-chunks", "charge-map-key", "charge-escaped-string", "charge-bytes", "map-string-keys", "wire-tagged"} {
		for _, n := range []int{0, 1, 2, 3, 4} {
			cases = append(cases, goldenJSONCase{fixture: byName[fixture], name: fmt.Sprintf("charge-fail-%d/%s", n, fixture), failCharge: n})
		}
	}
	add("precedence-bytes-charge", "charge-long-string", WithTypedMaxBytes(1024))
	cases[len(cases)-1].failCharge = 1
	add("precedence-count-charge", "charge-chunks", WithTypedMaxValues(1))
	cases[len(cases)-1].failCharge = 1
	return cases
}

func TestValueWalkerGoldens(t *testing.T) {
	walkers := []struct {
		name string
		walk func(*lisp.LVal, ...TypedOption) (*lisp.LVal, []byte, error)
	}{
		{"tag", func(v *lisp.LVal, opts ...TypedOption) (*lisp.LVal, []byte, error) {
			out, err := Tag(v, opts...)
			return out, nil, err
		}},
		{"untag", func(v *lisp.LVal, opts ...TypedOption) (*lisp.LVal, []byte, error) {
			out, err := Untag(v, opts...)
			return out, nil, err
		}},
		{"canonize", func(v *lisp.LVal, opts ...TypedOption) (*lisp.LVal, []byte, error) {
			out, err := Canonize(v, opts...)
			return out, nil, err
		}},
		{"dump-typed", func(v *lisp.LVal, opts ...TypedOption) (*lisp.LVal, []byte, error) {
			out, err := DumpTyped(v, opts...)
			return nil, out, err
		}},
	}
	for _, walker := range walkers {
		t.Run(walker.name, func(t *testing.T) {
			var records []goldenRecord
			for _, c := range goldenJSONCases() {
				in, v := newGoldenInput(c.fixture)
				calls := 0
				opts := append([]TypedOption{}, c.opts...)
				opts = append(opts, WithTypedCharge(func(n int) error {
					calls++
					in.trace = append(in.trace, "charge:"+strconv.Itoa(n))
					if calls == c.failCharge {
						return goldenChargeError
					}
					return nil
				}))
				var failure *canonizeError
				r := goldenObserve(c.name, in, func() (*lisp.LVal, []byte, error) {
					out, b, err := walker.walk(v, opts...)
					errors.As(err, &failure)
					return out, b, err
				}, map[string]error{"typed-limit": ErrTypedLimit, "charge": goldenChargeError, "host": goldenHostError})
				if failure != nil {
					r.Case, r.Path = ":"+failure.caseName, failure.path
					r.ConditionData = []string{failure.message(), r.Case, r.Path}
				}
				records = append(records, r)
			}
			checkWalkerGolden(t, "walker-"+walker.name, records)
		})
	}
}

// The public transforms and the fused encoder agree on successful default encodings.
// Goldens retain their separate errors, limit order, aliases and callback traces.
func TestValueWalkerDifferential(t *testing.T) {
	for _, f := range goldenFixtures() {
		if strings.Contains(f.name, "shared-dag-30") || strings.HasPrefix(f.name, "host-map") || f.name == "cycle-before-boundary" {
			continue
		}
		t.Run(f.name, func(t *testing.T) {
			_, v := newGoldenInput(f)
			tagged, err := Tag(v)
			if err != nil {
				return
			}
			plain, err := Dump(tagged, false)
			if err != nil {
				t.Fatal(err)
			}
			_, fresh := newGoldenInput(f)
			typed, err := DumpTyped(fresh)
			if err != nil {
				t.Fatal(err)
			}
			if string(plain) != string(typed) {
				t.Fatalf("Tag + Dump: %s; DumpTyped: %s", plain, typed)
			}
			back, err := Untag(tagged)
			if err != nil {
				t.Fatal(err)
			}
			backBytes, err := DumpTyped(back)
			if err != nil {
				t.Fatal(err)
			}
			if string(backBytes) != string(typed) {
				t.Fatalf("Untag changed bytes: %s; want %s", backBytes, typed)
			}
			c, err := Canonize(v)
			if err != nil {
				return
			}
			canonical, err := DumpTyped(c)
			if err != nil {
				t.Fatal(err)
			}
			original, err := Dump(v, false)
			if err != nil {
				t.Fatal(err)
			}
			if string(canonical) != string(original) {
				t.Fatalf("Canonize changed plain bytes: %s; want %s", canonical, original)
			}
		})
	}
}

func TestCanonizeConditionGoldens(t *testing.T) {
	var records []goldenRecord
	for _, f := range goldenFixtures() {
		in, v := newGoldenInput(f)
		env := lisp.NewEnv(nil)
		if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
			t.Fatal(rc)
		}
		env.Runtime.MaxAlloc = 8192
		var result *lisp.LVal
		r := goldenObserve(f.name, in, func() (*lisp.LVal, []byte, error) {
			result = CanonizeBuiltin(env, lisp.SExpr([]*lisp.LVal{v}))
			return result, nil, lisp.GoError(result)
		}, map[string]error{"host": goldenHostError})
		if result != nil && result.Type == lisp.LError {
			r.Condition = result.Str
			for _, c := range result.Cells {
				if c == nil {
					r.ConditionData = append(r.ConditionData, "Go nil")
				} else {
					r.ConditionData = append(r.ConditionData, c.Str)
				}
			}
		}
		records = append(records, r)
	}
	checkWalkerGolden(t, "walker-canonize-condition", records)
}

// All DumpOpts combinations and resolved builtin keywords use the same corpus.
// Plain Serializer calls use a byte budget so the shared DAG fixtures finish.
func TestValueWalkerPlainBytesGolden(t *testing.T) {
	var records []goldenRecord
	for _, f := range goldenFixtures() {
		for bits := range 8 {
			opts := DumpOpts{StringNumbers: bits&1 != 0, Canonize: bits&2 != 0, Typed: bits&4 != 0}
			in, v := newGoldenInput(f)
			r := goldenObserve(fmt.Sprintf("serializer/sn=%t/canonize=%t/typed=%t/%s", opts.StringNumbers, opts.Canonize, opts.Typed, f.name), in, func() (*lisp.LVal, []byte, error) {
				s := DefaultSerializer()
				if !opts.Canonize && !opts.Typed {
					b, _, err := s.dumpLimit(v, opts.StringNumbers, 1024, encodeBudget{maxBytes: 8192}, encodeMeter{})
					return nil, b, err
				}

				b, err := s.DumpWith(v, opts)
				return nil, b, err
			}, map[string]error{"typed-limit": ErrTypedLimit, "host": goldenHostError, "depth": lisp.ValueDepthError(1024)})
			records = append(records, r)
		}
		for _, defaultSN := range []bool{false, true} {
			for _, sn := range []string{"omitted", "false", "true"} {
				for bits := range 4 {
					in, v := newGoldenInput(f)
					env := lisp.NewEnv(nil)
					if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
						t.Fatal(rc)
					}
					env.Runtime.MaxAlloc = 8192
					s := DefaultSerializer()
					s.UseStringNumbers = defaultSN
					arg := lisp.Nil()
					if sn != "omitted" {
						arg = lisp.Bool(sn == "true")
					}
					var result *lisp.LVal
					r := goldenObserve(fmt.Sprintf("dump-bytes/default-sn=%t/sn=%s/canonize=%t/typed=%t/%s", defaultSN, sn, bits&1 != 0, bits&2 != 0, f.name), in, func() (*lisp.LVal, []byte, error) {
						result = s.DumpBytesBuiltin(env, lisp.SExpr([]*lisp.LVal{v, arg, lisp.Bool(bits&1 != 0), lisp.Bool(bits&2 != 0)}))
						if result.Type == lisp.LError {
							return nil, nil, lisp.GoError(result)
						}
						return nil, result.Bytes(), nil
					}, map[string]error{"host": goldenHostError, "depth": lisp.ValueDepthError(1024)})
					if result != nil && result.Type == lisp.LError {
						r.Condition = result.Str
						for _, c := range result.Cells {
							r.ConditionData = append(r.ConditionData, c.Str)
						}
					}
					records = append(records, r)
				}
			}
		}
	}
	checkWalkerGolden(t, "walker-plain-bytes", records)
}
