// Copyright © 2018 The ELPS authors

package libtime

import (
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "time"

// LoadPackage adds the time package to env
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	name := lisp.Symbol(DefaultPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
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

var builtins = []*libutil.Builtin{
	libutil.Function("utc-now", lisp.Formals(), BuiltinUTCNow),
	libutil.Function("parse-rfc3339", lisp.Formals("timestamp"), BuiltinParseRFC3339),
	libutil.Function("parse-rfc3339-nano", lisp.Formals("timestamp"), BuiltinParseRFC3339Nano),
	libutil.Function("format-rfc3339", lisp.Formals("datetime"), BuiltinFormatRFC3339),
	libutil.Function("format-rfc3339-nano", lisp.Formals("datetime"), BuiltinFormatRFC3339Nano),
	libutil.Function("time=", lisp.Formals("a", "b"), BuiltinTimeEq),
	libutil.Function("time<", lisp.Formals("a", "b"), BuiltinTimeLT),
	libutil.Function("time>", lisp.Formals("a", "b"), BuiltinTimeGT),
	libutil.Function("time-add", lisp.Formals("datetime", "duration"), BuiltinTimeAdd),
	libutil.Function("time-elapsed", lisp.Formals("start"), BuiltinElapsed),
	libutil.Function("time-from", lisp.Formals("start", "end"), BuiltinDurationBetween),
	libutil.Function("parse-duration", lisp.Formals("duration-string"), BuiltinParseDuration),
	libutil.Function("duration-s", lisp.Formals("time-duration"), BuiltinDurationSeconds),
	libutil.Function("duration-ms", lisp.Formals("time-duration"), BuiltinDurationMS),
	libutil.Function("duration-ns", lisp.Formals("time-duration"), BuiltinDurationNS),
	libutil.Function("sleep", lisp.Formals("time-duration"), BuiltinSleep),
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

// BulitinSleep sleeps for the given duration before returning.
func BuiltinSleep(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lt := args.Cells[0]
	if lt.Type != lisp.LNative {
		return env.Errorf("argument is not a duration: %v", lt.Type)
	}
	d, ok := lt.Native.(time.Duration)
	if !ok {
		return env.Errorf("argument is not a duration: %v", lt)
	}
	time.Sleep(d)
	return lisp.Nil()
}
