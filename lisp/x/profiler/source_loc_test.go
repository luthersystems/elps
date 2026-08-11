package profiler

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// TestGetSourceLocNativeFallback pins the historical attribution for
// functions with no recorded source location (builtins registered from Go):
// they report the synthetic "<native code>" location rather than nil.  A nil
// here regresses profiler output — callgrind's fl= lines are stateful, so a
// missing location silently attributes builtin cost to the previously
// emitted file, and the span annotators drop their source attributes.
func TestGetSourceLocNativeFallback(t *testing.T) {
	fun := lisp.Fun("<builtin-function ``test-fn''>", lisp.Formals(),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return lisp.Nil() })
	loc := getSourceLoc(fun)
	if loc == nil {
		t.Fatal("getSourceLoc returned nil for a builtin; want the <native code> location")
	}
	want := token.NativeLocation()
	if *loc != want {
		t.Fatalf("getSourceLoc = %+v, want %+v", *loc, want)
	}
}

// TestGetSourceLocRealLocation: a function whose formals carry a real parse
// location reports that location, copied (never the stored pointer).
func TestGetSourceLocRealLocation(t *testing.T) {
	formals := lisp.Formals("x")
	stored := &token.Location{File: "f.lisp", Pos: 3, Line: 2, Col: 1}
	formals.SetSource(stored)
	fun := lisp.Fun("<builtin-function ``test-fn''>", formals,
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return lisp.Nil() })
	loc := getSourceLoc(fun)
	if loc == nil {
		t.Fatal("getSourceLoc returned nil for a located function")
	}
	if loc == stored {
		t.Fatal("getSourceLoc returned the stored pointer; want a private copy")
	}
	if *loc != *stored {
		t.Fatalf("getSourceLoc = %+v, want %+v", *loc, *stored)
	}
}
