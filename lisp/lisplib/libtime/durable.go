// Copyright © 2026 The ELPS authors

package libtime

import (
	"fmt"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// Durable typed JSON names of the codecs this package declares.  An
// embedder that already saves documents under another name registers the
// codec with WithName.
const (
	DurableTimeName     = "elps:time"
	DurableDurationName = "elps:duration"
)

// DurableTimeCodec is the durable codec of time values (Time).  A time
// saves as its instant in UTC, RFC 3339 with nanoseconds, and loads in
// UTC.  The location is not saved, because its rules come from the host's
// timezone database, which peers do not share.
var DurableTimeCodec = libjson.DurableCodec[ownedTime]{
	Name:    DurableTimeName,
	Version: 1,
	Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) {
		t, ok := Get(v)
		if !ok {
			return nil, fmt.Errorf("%s: not a time value", DurableTimeName)
		}
		return lisp.String(t.UTC().Format(time.RFC3339Nano)), nil
	},
	Load: func(_ *lisp.LEnv, _ int, p *lisp.LVal) (*lisp.LVal, error) {
		if p.Type != lisp.LString {
			return nil, fmt.Errorf("%s: payload is not a string", DurableTimeName)
		}
		t, err := time.Parse(time.RFC3339Nano, p.Str)
		if err != nil {
			return nil, fmt.Errorf("%s: %w", DurableTimeName, err)
		}
		return Time(t.UTC()), nil
	},
}

// DurableDurationCodec is the durable codec of durations (Duration).  A
// duration saves as its integer number of nanoseconds.
var DurableDurationCodec = libjson.DurableCodec[time.Duration]{
	Name:    DurableDurationName,
	Version: 1,
	Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) {
		d, ok := GetDuration(v)
		if !ok {
			return nil, fmt.Errorf("%s: not a duration", DurableDurationName)
		}
		return lisp.Int(int(d)), nil
	},
	Load: func(_ *lisp.LEnv, _ int, p *lisp.LVal) (*lisp.LVal, error) {
		if p.Type != lisp.LInt {
			return nil, fmt.Errorf("%s: payload is not an integer", DurableDurationName)
		}
		return Duration(time.Duration(p.Int)), nil
	},
}
