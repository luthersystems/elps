// Copyright © 2026 The ELPS authors

package fuzzfp_test

import (
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/fuzzfp"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libtime"
)

// #633: the oracle must see the private owned timestamp, not just its marker.
// Its raw-host negative control deliberately mutates a public Location header;
// the same mutation must leave the owned representation unchanged.
func TestFingerprintLispOwnedTime(t *testing.T) {
	location := time.FixedZone("Original", 19800)
	stamp := time.Unix(1710054000, 42).In(location)
	raw, owned := lisp.Native(stamp), libtime.Time(stamp)
	if _, exposed := owned.Native.(time.Time); exposed {
		t.Fatal("libtime.Time exposed its host timestamp instead of an owned value")
	}
	before := fuzzfp.Fingerprint(owned.Native)
	equal := libtime.Time(time.Unix(1710054000, 42).In(time.FixedZone("Original", 19800)))
	if before != fuzzfp.Fingerprint(equal.Native) {
		t.Fatal("equal independently owned times have different fingerprints")
	}
	for _, changed := range []time.Time{
		stamp.Add(time.Nanosecond),
		stamp.In(time.FixedZone("Renamed", 19800)),
		stamp.In(time.FixedZone("Original", 19801)),
	} {
		if before == fuzzfp.Fingerprint(libtime.Time(changed).Native) {
			t.Fatalf("fingerprint cannot see owned timestamp or timezone change: %v", changed)
		}
	}
	rawGuard, ownedGuard := fuzzfp.Watch(raw), fuzzfp.Watch(owned)
	if violation := rawGuard.Check(); violation != "" {
		t.Fatalf("clean raw native reported mutation: %s", violation)
	}
	*location = *time.FixedZone("Changed", -12600)
	if violation := rawGuard.Check(); !strings.Contains(violation, "Go value inside an LNative was mutated") {
		t.Fatalf("raw Location header overwrite escaped the native guard: %s", violation)
	}
	if violation := ownedGuard.Check(); violation != "" {
		t.Fatalf("source Location header mutation reached owned value: %s", violation)
	}
	read, ok := libtime.Get(owned)
	if !ok || read.Unix() != 1710054000 || read.Nanosecond() != 42 {
		t.Fatalf("owned timestamp changed: %v, %v", read, ok)
	}
	if name, offset := read.Zone(); name != "Original" || offset != 19800 {
		t.Fatalf("owned timezone changed: %s/%d", name, offset)
	}
	*read.Location() = *time.FixedZone("GetterChanged", 17)
	if violation := ownedGuard.Check(); violation != "" {
		t.Fatalf("Get leaked owned Location header: %s", violation)
	}
	owned.Native = libtime.Time(read.Add(time.Second)).Native //elps:mutates intentional native replacement for the independent fingerprint's negative control
	if violation := ownedGuard.Check(); !strings.Contains(violation, "Go value inside an LNative was mutated") {
		t.Fatalf("replacement with a different owned time escaped the native guard: %s", violation)
	}
}
