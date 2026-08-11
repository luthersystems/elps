// Copyright © 2018 The ELPS authors

package libtime

import (
	"context"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DefaultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "time"

// LoadPackage adds the time package to env
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	prevPkg := env.Runtime.Package.Name
	defer env.InPackage(lisp.Symbol(prevPkg))
	name := lisp.Symbol(DefaultPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
	env.SetPackageDoc(`Time operations: parsing, formatting, comparison, and arithmetic
		on UTC timestamps and durations. Wraps Go's time package.`)
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

// Time creates an LVal representing the time t.
func Time(t time.Time) *lisp.LVal {
	return lisp.Native(t)
}

// GetTime gets a time.Time value from v and returns it.
func Get(v *lisp.LVal) (time.Time, bool) {
	t, ok := v.Native.(time.Time)
	return t, ok
}

// Duration returns an LVal representing duration d.
func Duration(d time.Duration) *lisp.LVal {
	return lisp.Native(d)
}

// GetDuration gets a time.Duration value from v and returns it.
func GetDuration(v *lisp.LVal) (time.Duration, bool) {
	d, ok := v.Native.(time.Duration)
	return d, ok
}

//elpsvet:allow package builtin table; formals reach a Runtime only through copyFormals at registration (lisp.LEnv.AddBuiltins)
var builtins = []*libutil.Builtin{
	libutil.FunctionDoc("utc-now", lisp.Formals(), BuiltinUTCNow,
		`Returns the current time in UTC as a native time value. Takes no
		arguments. Use format-rfc3339 or format-rfc3339-nano to convert
		the result to a string.`),
	libutil.FunctionDoc("parse-rfc3339", lisp.Formals("timestamp"), BuiltinParseRFC3339,
		`Parses an RFC 3339 timestamp string (e.g. "2023-01-15T10:30:00Z")
		and returns a native time value. Returns an error if the string
		is not valid RFC 3339 format. For nanosecond precision, use
		parse-rfc3339-nano.`),
	libutil.FunctionDoc("parse-rfc3339-nano", lisp.Formals("timestamp"), BuiltinParseRFC3339Nano,
		`Parses an RFC 3339 timestamp string with nanosecond precision
		(e.g. "2023-01-15T10:30:00.123456789Z") and returns a native
		time value. Returns an error if the string is not valid format.`),
	libutil.FunctionDoc("format-rfc3339", lisp.Formals("datetime"), BuiltinFormatRFC3339,
		`Formats a native time value as an RFC 3339 string with second
		precision (e.g. "2023-01-15T10:30:00Z"). Returns a string.
		The argument must be a native time value from utc-now or
		parse-rfc3339.`),
	libutil.FunctionDoc("format-rfc3339-nano", lisp.Formals("datetime"), BuiltinFormatRFC3339Nano,
		`Formats a native time value as an RFC 3339 string with nanosecond
		precision (e.g. "2023-01-15T10:30:00.123456789Z"). Returns a
		string. The argument must be a native time value.`),
	libutil.FunctionDoc("time=", lisp.Formals("a", "b"), BuiltinTimeEq,
		`Returns true if the two native time values represent the same
		instant. Both arguments must be native time values.`),
	libutil.FunctionDoc("time<", lisp.Formals("a", "b"), BuiltinTimeLT,
		`Returns true if time a is before time b. Both arguments must
		be native time values.`),
	libutil.FunctionDoc("time>", lisp.Formals("a", "b"), BuiltinTimeGT,
		`Returns true if time a is after time b. Both arguments must
		be native time values.`),
	libutil.FunctionDoc("time-add", lisp.Formals("datetime", "duration"), BuiltinTimeAdd,
		`Returns a new time value equal to datetime plus duration. The
		first argument must be a native time value and the second must
		be a native duration value (from parse-duration or time-from).`),
	libutil.FunctionDoc("time-elapsed", lisp.Formals("start"), BuiltinElapsed,
		`Returns the duration elapsed since start as a native duration
		value. Equivalent to (time-from start (utc-now)). The argument
		must be a native time value.`),
	libutil.FunctionDoc("time-from", lisp.Formals("start", "end"), BuiltinDurationBetween,
		`Returns the duration between start and end as a native duration
		value (end - start). Both arguments must be native time values.
		The result can be negative if end is before start.`),
	libutil.FunctionDoc("parse-duration", lisp.Formals("duration-string"), BuiltinParseDuration,
		`Parses a Go-style duration string and returns a native duration
		value. Valid units are "ns", "us" (or "µs"), "ms", "s", "m", "h".
		Examples: "1h30m", "500ms", "2h45m30s". Returns an error if the
		string cannot be parsed.`),
	libutil.FunctionDoc("duration-s", lisp.Formals("time-duration"), BuiltinDurationSeconds,
		`Returns the number of seconds in the duration as a float.
		The argument must be a native duration value.`),
	libutil.FunctionDoc("duration-ms", lisp.Formals("time-duration"), BuiltinDurationMS,
		`Returns the number of milliseconds in the duration as a float.
		The argument must be a native duration value.`),
	libutil.FunctionDoc("duration-ns", lisp.Formals("time-duration"), BuiltinDurationNS,
		`Returns the number of nanoseconds in the duration as an integer.
		The argument must be a native duration value. Returns an error
		if the value overflows an int.`),
	libutil.FunctionDoc("sleep", lisp.Formals("time-duration", lisp.KeyArgSymbol, "max"), BuiltinSleep,
		`Pauses execution for the specified duration. The argument must
		be a native duration value (from parse-duration). Returns nil.

		A sleep that cannot succeed is REFUSED IMMEDIATELY rather than
		blocked on. Two things refuse it, both before any time passes:

		  1. A duration longer than one hour raises
		     sleep-limit-exceeded. Pass :max with a longer duration to
		     allow it -- (time:sleep d :max m) -- which makes an
		     unusually long sleep explicit at the call site. The host
		     may set a ceiling that :max cannot exceed.
		  2. A duration that would outlast the evaluation's context
		     deadline raises context-cancelled. The sleep could not
		     have completed, so waiting out the remaining time would
		     only consume the budget the caller has left to react in.

		Once started, the pause is still interruptible: if the context
		is cancelled while sleeping, sleep wakes at that point and
		raises context-cancelled instead of returning nil.`),
}

func BuiltinUTCNow(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return Time(time.Now().UTC())
}

func BuiltinParseRFC3339(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	stamp := args.Cells[0]
	if stamp.Type != lisp.LString {
		return env.Errorf("argument is not a string: %v", stamp.Type)
	}
	t, err := time.Parse(time.RFC3339, stamp.Str)
	if err != nil {
		return env.Error(err)
	}
	return Time(t)
}

func BuiltinParseRFC3339Nano(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	stamp := args.Cells[0]
	if stamp.Type != lisp.LString {
		return env.Errorf("argument is not a string: %v", stamp.Type)
	}
	t, err := time.Parse(time.RFC3339Nano, stamp.Str)
	if err != nil {
		return env.Error(err)
	}
	return Time(t)
}

func BuiltinFormatRFC3339(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lt := args.Cells[0]
	if lt.Type != lisp.LNative {
		return env.Errorf("argument is not a time: %v", lt.Type)
	}
	t, ok := lt.Native.(time.Time)
	if !ok {
		return env.Errorf("argument is not a time: %v", lt)
	}
	return lisp.String(t.Format(time.RFC3339))
}

func BuiltinFormatRFC3339Nano(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lt := args.Cells[0]
	if lt.Type != lisp.LNative {
		return env.Errorf("argument is not a time: %v", lt.Type)
	}
	t, ok := lt.Native.(time.Time)
	if !ok {
		return env.Errorf("argument is not a time: %v", lt)
	}
	return lisp.String(t.Format(time.RFC3339Nano))
}

func BuiltinTimeEq(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != lisp.LNative {
		return env.Errorf("argument is not a time: %v", a.Type)
	}
	t1, ok := a.Native.(time.Time)
	if !ok {
		return env.Errorf("argument is not a time: %v", a)
	}
	t2, ok := b.Native.(time.Time)
	if !ok {
		return env.Errorf("argument is not a time: %v", b)
	}
	return lisp.Bool(t1.Equal(t2))
}

func BuiltinTimeLT(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != lisp.LNative {
		return env.Errorf("argument is not a time: %v", a.Type)
	}
	t1, ok := a.Native.(time.Time)
	if !ok {
		return env.Errorf("argument is not a time: %v", a)
	}
	t2, ok := b.Native.(time.Time)
	if !ok {
		return env.Errorf("argument is not a time: %v", b)
	}
	return lisp.Bool(t2.After(t1))
}

func BuiltinTimeGT(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != lisp.LNative {
		return env.Errorf("argument is not a time: %v", a.Type)
	}
	t1, ok := a.Native.(time.Time)
	if !ok {
		return env.Errorf("argument is not a time: %v", a)
	}
	t2, ok := b.Native.(time.Time)
	if !ok {
		return env.Errorf("argument is not a time: %v", b)
	}
	return lisp.Bool(t1.After(t2))
}

func BuiltinTimeAdd(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lt, ld := args.Cells[0], args.Cells[1]
	if lt.Type != lisp.LNative {
		return env.Errorf("argument is not a time: %v", lt.Type)
	}
	if ld.Type != lisp.LNative {
		return env.Errorf("argument is not a duration: %v", ld.Type)
	}
	t, ok := lt.Native.(time.Time)
	if !ok {
		return env.Errorf("argument is not a time: %v", lt)
	}
	d, ok := ld.Native.(time.Duration)
	if !ok {
		return env.Errorf("argument is not a time: %v", ld)
	}
	return Time(t.Add(d))
}

func BuiltinDurationBetween(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lt1, lt2 := args.Cells[0], args.Cells[1]
	if lt1.Type != lisp.LNative {
		return env.Errorf("argument is not a time: %v", lt1.Type)
	}
	if lt2.Type != lisp.LNative {
		return env.Errorf("argument is not a time: %v", lt2.Type)
	}
	t1, ok := lt1.Native.(time.Time)
	if !ok {
		return env.Errorf("argument is not a time: %v", lt1)
	}
	t2, ok := lt2.Native.(time.Time)
	if !ok {
		return env.Errorf("argument is not a time: %v", lt2)
	}
	return Duration(t2.Sub(t1))
}

func BuiltinElapsed(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lt1 := args.Cells[0]
	if lt1.Type != lisp.LNative {
		return env.Errorf("argument is not a time: %v", lt1.Type)
	}
	t1, ok := lt1.Native.(time.Time)
	if !ok {
		return env.Errorf("argument is not a time: %v", lt1)
	}
	return Duration(time.Since(t1))
}

func BuiltinParseDuration(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	v := args.Cells[0]
	if v.Type != lisp.LString {
		return env.Errorf("argument is not a string: %v", v.Type)
	}
	d, err := time.ParseDuration(v.Str)
	if err != nil {
		return env.Error(err)
	}
	return Duration(d)
}

// BulitinDurationSecods returns a float equal to the the number of seconds in
// the given duration.
func BuiltinDurationSeconds(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lt := args.Cells[0]
	if lt.Type != lisp.LNative {
		return env.Errorf("argument is not a duration: %v", lt.Type)
	}
	d, ok := lt.Native.(time.Duration)
	if !ok {
		return env.Errorf("argument is not a duration: %v", lt)
	}
	// TODO:  Check for overflow and use a method less prone to overflow.
	return lisp.Float(float64(d) / float64(time.Second))
}

// BuiltinDuriationNS returns a float equal to the the number of nanoseconds in
// the given duration.
func BuiltinDurationMS(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lt := args.Cells[0]
	if lt.Type != lisp.LNative {
		return env.Errorf("argument is not a duration: %v", lt.Type)
	}
	d, ok := lt.Native.(time.Duration)
	if !ok {
		return env.Errorf("argument is not a duration: %v", lt)
	}
	// TODO:  Check for overflow and use a method less prone to overflow.
	return lisp.Float(float64(d) / float64(time.Millisecond))
}

// BulitinDuriationNS returns an integer equal to the the number of nanoseconds
// in the given duration.
func BuiltinDurationNS(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lt := args.Cells[0]
	if lt.Type != lisp.LNative {
		return env.Errorf("argument is not a duration: %v", lt.Type)
	}
	d, ok := lt.Native.(time.Duration)
	if !ok {
		return env.Errorf("argument is not a duration: %v", lt)
	}
	if int64(int(d)) != int64(d) {
		return env.Errorf("duration is too large")
	}
	return lisp.Int(int(d))
}

// BuiltinSleep sleeps for the given duration before returning.
//
// The sleep is bounded by the evaluation's context (see LEnv.Context): it
// wakes early if the context is cancelled, and never sleeps past the
// context's deadline.  See sleepContext for the full rationale.
func BuiltinSleep(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lt, lmax := args.ReqArg(env, 0), args.KeyArg(1)
	if lt.Type == lisp.LError {
		return lt
	}
	if lt.Type != lisp.LNative {
		return env.Errorf("argument is not a duration: %v", lt.Type)
	}
	d, ok := lt.Native.(time.Duration)
	if !ok {
		return env.Errorf("argument is not a duration: %v", lt)
	}
	limit, lerr := sleepCap(env, lmax)
	if lerr != nil {
		return lerr
	}
	if limit > 0 && d > limit {
		return env.ErrorConditionf(lisp.CondSleepLimitExceeded,
			"sleep of %v exceeds the maximum %v"+
				" (pass :max to allow a longer sleep, subject to any host ceiling)",
			d, limit)
	}
	return sleepContext(env, d)
}

// sleepCap resolves the longest sleep this call may request, or 0 for no
// limit.  It returns a non-nil second value if the :max argument is unusable.
//
// Three inputs, in increasing authority:
//
//	lisp.DefaultMaxSleep   the cap when :max is absent
//	:max                   raises it for this call, at the program's discretion
//	Runtime.MaxSleep       the host's ceiling, which :max may not exceed
//
// The last is what makes this a bound rather than a suggestion: program
// source can relax the default but cannot relax the ceiling, so an untrusted
// program cannot grant itself an unbounded sleep.  See lisp.WithMaxSleep.
func sleepCap(env *lisp.LEnv, lmax *lisp.LVal) (time.Duration, *lisp.LVal) {
	ceiling := env.Runtime.MaxSleepCeiling()
	if lmax.IsNil() {
		limit := lisp.DefaultMaxSleep
		if ceiling > 0 && ceiling < limit {
			// A host ceiling below the default lowers the default too;
			// otherwise the default would quietly exceed the ceiling.
			limit = ceiling
		}
		return limit, nil
	}
	if lmax.Type != lisp.LNative {
		return 0, env.Errorf("max is not a duration: %v", lmax.Type)
	}
	m, ok := lmax.Native.(time.Duration)
	if !ok {
		return 0, env.Errorf("max is not a duration: %v", lmax)
	}
	if m <= 0 {
		// Reject rather than treat as "no limit": a non-positive :max most
		// likely came from arithmetic that underflowed, and reading it as
		// "unlimited" would turn a bug into the very unbounded sleep this
		// exists to prevent.
		return 0, env.Errorf("max is not a positive duration: %v", m)
	}
	if ceiling > 0 && m > ceiling {
		return 0, env.ErrorConditionf(lisp.CondSleepLimitExceeded,
			"max %v exceeds the host ceiling %v", m, ceiling)
	}
	return m, nil
}

// sleepContext pauses for d, bounded by env's context.
//
// WHY THIS IS NOT time.Sleep(d).  A plain time.Sleep blocks inside a single
// evaluation step, and no containment mechanism the interpreter offers can
// see it: MaxSteps is not consulted mid-builtin, MaxAlloc sees no allocation,
// the stack limits see no frames and no tail calls, and the context deadline
// is only observed BETWEEN steps.  A caller-supplied duration therefore had
// no upper bound at all -- "9223372036854775807ns" parses cleanly and blocks
// for roughly 292 years.  Downstream (luthersystems/substrate) the caller is
// customer-supplied phylum source running as chaincode, which made this the
// only known unbounded-execution shape no embedder configuration could stop.
// See issue #314.
//
// The fix makes the EXISTING context deadline authoritative rather than
// adding a new knob nobody sets: select on a timer and on ctx.Done(), and cap
// the wait at the remaining time before the deadline.
//
// The deadline cap is belt-and-braces alongside the ctx.Done() select, not a
// substitute for it.  The Context interface permits an implementation to
// report a Deadline while returning a nil Done channel; the stdlib's own
// types do not, but wrappers written by embedders can, and selecting on a nil
// channel blocks forever -- exactly the bug being fixed.  Capping the timer
// means the wait ends at the deadline either way.
//
// WHAT DOES NOT CHANGE.  When the context has neither a Done channel nor a
// deadline -- which is the default, context.Background(), for every embedder
// that uses Eval rather than EvalContext and never passes WithContext -- this
// is a plain time.Sleep with the historical behaviour, 292-year sleeps
// included.  Bounding that case would require a new default limit, which
// would break every program that legitimately sleeps longer than it.
func sleepContext(env *lisp.LEnv, d time.Duration) *lisp.LVal {
	if d <= 0 {
		// time.Sleep returns immediately for a non-positive duration.
		return lisp.Nil()
	}
	ctx := env.Context()
	done := ctx.Done()
	deadline, hasDeadline := ctx.Deadline()
	if done == nil && !hasDeadline {
		time.Sleep(d)
		return lisp.Nil()
	}
	if err := ctx.Err(); err != nil {
		return errContextCancelled(env, err)
	}
	// FAIL FAST rather than sleeping out a doomed wait.  When the deadline is
	// nearer than d the sleep provably cannot complete: the outcome is already
	// decided on entry, and the only question is whether the caller learns now
	// or after its remaining budget has been burned.  This used to wait and
	// then report (issue #338), which spent the caller's last chance to flush a
	// partial result or run cleanup on a call whose answer was already known.
	//
	// The objection to failing fast was that (while true (ignore-errors
	// (time:sleep 1))) stops being throttled and becomes a busy loop.  That is
	// real, but it is not this branch's doing: the loop is already unbounded
	// and MaxSteps is what bounds it.  Trading a caller's usable budget for a
	// throttle on catch-and-continue code is the wrong way round, and the
	// throttle was never durable anyway -- it lasted only until the deadline.
	if hasDeadline {
		if remaining := time.Until(deadline); remaining < d {
			return errContextCancelled(env, context.DeadlineExceeded)
		}
	}
	timer := time.NewTimer(d)
	defer timer.Stop()
	select {
	case <-timer.C:
	case <-done:
	}
	if err := ctx.Err(); err != nil {
		return errContextCancelled(env, err)
	}
	return lisp.Nil()
}

// errContextCancelled renders the interruption using the same condition and
// message shape the interpreter's own between-steps check produces, so a
// handler-bind on context-cancelled sees one consistent error.
func errContextCancelled(env *lisp.LEnv, err error) *lisp.LVal {
	return env.ErrorConditionf(lisp.CondContextCancelled, "context cancelled: %v", err)
}
