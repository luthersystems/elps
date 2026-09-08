// Copyright © 2026 The ELPS authors

package fuzzval_test

import (
	"fmt"
	"reflect"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/fuzzval"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libtime"
)

// #633: retain host time.Time coverage independently of the actual private
// representation returned by libtime.Time. Both populations must cover the
// three bounded, host-independent zones during ordinary seed replay.
func TestGeneratorTimePopulations(t *testing.T) {
	ownedType := reflect.TypeOf(libtime.Time(time.Unix(1, 42).UTC()).Native)
	if ownedType == reflect.TypeOf(time.Time{}) {
		t.Fatal("premise: libtime.Time must own its native representation")
	}
	seen := map[string]map[string]bool{"raw": {}, "lisp": {}}
	for _, seed := range fuzzval.Seeds() {
		v := fuzzval.New(seed, nil).Value()
		if v.Type != lisp.LNative {
			continue
		}
		var population string
		if _, raw := v.Native.(time.Time); raw {
			population = "raw"
		} else if reflect.TypeOf(v.Native) == ownedType {
			population = "lisp"
		} else {
			continue
		}
		stamp, ok := libtime.Get(v)
		if !ok {
			t.Fatalf("%s seed %x rejected by libtime.Get", population, seed)
		}
		name, offset := stamp.Zone()
		seen[population][fmt.Sprintf("%s/%d", name, offset)] = true
		other := fuzzval.New(seed, nil).Value()
		repeated, ok := libtime.Get(other)
		if !ok || reflect.TypeOf(other.Native) != reflect.TypeOf(v.Native) || stamp.Format(time.RFC3339Nano) != repeated.Format(time.RFC3339Nano) || stamp.Location().String() != repeated.Location().String() {
			t.Fatalf("%s seed %x is not deterministic", population, seed)
		}
		mutationLocation := stamp.Location()
		if population == "raw" {
			// Get detaches even raw timestamps. Mutate the generated payload's
			// own header, otherwise a shared generator Location goes undetected.
			mutationLocation = v.Native.(time.Time).Location()
		}
		if mutationLocation != time.UTC {
			// Fresh headers prevent a mutation in one fuzz input from poisoning
			// later inputs. For owned natives this also checks Get's detached
			// output. Never mutate Go's process-global time.UTC object.
			wantStamp, wantLocation := stamp.Format(time.RFC3339Nano), stamp.Location().String()
			*mutationLocation = *time.FixedZone("Changed", 17)
			again, ok := libtime.Get(fuzzval.New(seed, nil).Value())
			if !ok || again.Format(time.RFC3339Nano) != wantStamp || again.Location().String() != wantLocation {
				t.Fatalf("%s seed %x changed after another input's Location mutation: %v, want %s in %q", population, seed, again, wantStamp, wantLocation)
			}
			if gotName, gotOffset := again.Zone(); gotName != name || gotOffset != offset {
				t.Fatalf("%s seed %x retained another input's Location: %s/%d", population, seed, gotName, gotOffset)
			}
		}
	}
	want := map[string]bool{"UTC/0": true, "/19800": true, "Fuzz/Fixed/-12600": true}
	for population, zones := range seen {
		if !reflect.DeepEqual(zones, want) {
			t.Errorf("%s time zones = %v, want %v", population, zones, want)
		}
	}
}

func TestGeneratorRawTimeSelectorCompatibility(t *testing.T) {
	// Preserve the original selector6 byte consumption, including the next
	// generated value; only reserved selector16 and zoned subtype26 change.
	g := fuzzval.New([]byte{17, 6, 0, 1, 42, 3, 0, 1}, nil)
	v := g.Value()
	stamp, ok := v.Native.(time.Time)
	if !ok || stamp != time.Unix(1, 42).UTC() {
		t.Fatalf("original raw selector changed: %v", v)
	}
	if next := g.Value(); next.Type != lisp.LInt || next.Int != 1 {
		t.Fatalf("original raw selector consumed another value's bytes: %v", next)
	}
}
