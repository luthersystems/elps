// Copyright © 2026 The ELPS authors

package libtime

import (
	"time"

	"github.com/luthersystems/elps/internal/templatepolicy"
	"github.com/luthersystems/elps/lisp"
)

// ownedTime is stored by value, never exposed as a writable pointer. The time
// and its initialized Location object are private. Location's unexported rule
// tables and cache referents are immutable after construction, so their backing
// storage may be shared. Tests pin the audited Go layouts and public methods.
type ownedTime struct {
	templatepolicy.Marker
	t time.Time
}

var _ templatepolicy.Immutable = ownedTime{}

// MarshalJSON preserves time.Time's encoding and validation. The returned
// bytes are newly allocated and cannot mutate the stored value.
func (t ownedTime) MarshalJSON() ([]byte, error) { return t.t.MarshalJSON() }

// MarshalText preserves time.Time's text encoding without exposing its storage.
func (t ownedTime) MarshalText() ([]byte, error) { return t.t.MarshalText() }

// detachTime copies the only publicly mutable object reachable from a Time.
// Initialize time.Local before copying: the lazy initializer recognizes only
// Go's original Local pointer, not a copied Location. No exported Location
// method mutates its private transition tables after initialization.
//
// Time.In intentionally drops monotonic clock metadata. ELPS native times are
// wall-clock values; timezone names, offsets and calendar rules are preserved.
// Callers must not concurrently mutate the supplied Location during capture.
func detachTime(t time.Time) time.Time {
	loc := t.Location()
	_ = loc.String()
	snapshot := *loc
	return t.In(&snapshot)
}

// borrowTime is for this package's read-only operations only. Never expose its
// Location or store the borrowed time in a newly admitted native without first
// taking ownership: the raw-host compatibility case may still have a Go owner.
func borrowTime(v *lisp.LVal) (time.Time, bool) {
	if t, ok := lisp.NativeValue[ownedTime](v); ok {
		return t.t, true
	}
	return lisp.NativeValue[time.Time](v)
}
