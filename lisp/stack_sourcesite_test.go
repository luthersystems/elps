// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/elpstest/oracle"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func siteLoc(file string, line, col int) *token.Location {
	return &token.Location{File: file, Path: file, Pos: line*100 + col, Line: line, Col: col}
}

func siteFrame(pkg, name string, src *token.Location) lisp.CallFrame {
	return lisp.CallFrame{Package: pkg, Name: name, Source: src}
}

// TestSourceSite checks each rule of SourceSite on frames built by hand,
// entrypoint first, as CallStack.Frames orders them.
func TestSourceSite(t *testing.T) {
	native := &token.Location{File: token.NativeFile, Pos: -1}

	tests := []struct {
		name   string
		frames []lisp.CallFrame
		want   string
	}{{
		name:   "no frames",
		frames: nil,
		want:   "",
	}, {
		name: "no frame with a source position",
		frames: []lisp.CallFrame{
			siteFrame("app", "load", nil),
			siteFrame("lisp", "builtin", native),
		},
		want: "",
	}, {
		name: "one frame with a source position",
		frames: []lisp.CallFrame{
			siteFrame("app", "load", nil),
			siteFrame("app", "now", siteLoc("main.lisp", 4, 9)),
		},
		want: "main.lisp:4:9",
	}, {
		name: "inner and reach at one position",
		frames: []lisp.CallFrame{
			siteFrame("app", "load", nil),
			siteFrame("lisp", "set", siteLoc("main.lisp", 4, 9)),
			siteFrame("app", "now", siteLoc("main.lisp", 4, 9)),
		},
		want: "main.lisp:4:9",
	}, {
		name: "reach follows inner",
		frames: []lisp.CallFrame{
			siteFrame("app", "load", nil),
			siteFrame("lisp", "handler-bind", siteLoc("main.lisp", 4, 9)),
			siteFrame("util", "now", siteLoc("main.lisp", 4, 66)),
			siteFrame("app", "now", siteLoc("core.lisp", 5, 29)),
		},
		want: "core.lisp:5:29 <- main.lisp:4:9",
	}, {
		name: "load-file starts a source stream",
		frames: []lisp.CallFrame{
			siteFrame("app", "load", nil),
			siteFrame("lisp", "load-file", siteLoc("main.lisp", 4, 1)),
			siteFrame("lisp", "handler-bind", siteLoc("a.lisp", 2, 9)),
			siteFrame("app", "now", siteLoc("core.lisp", 5, 29)),
		},
		want: "core.lisp:5:29 <- a.lisp:2:9",
	}, {
		name: "the innermost load-file decides",
		frames: []lisp.CallFrame{
			siteFrame("app", "load", nil),
			siteFrame("lisp", "load-file", siteLoc("main.lisp", 4, 1)),
			siteFrame("lisp", "load-file", siteLoc("a.lisp", 2, 1)),
			siteFrame("lisp", "handler-bind", siteLoc("b.lisp", 2, 9)),
			siteFrame("app", "now", siteLoc("core.lisp", 5, 29)),
		},
		want: "core.lisp:5:29 <- b.lisp:2:9",
	}, {
		name: "load-string starts a source stream",
		frames: []lisp.CallFrame{
			siteFrame("app", "load", nil),
			siteFrame("lisp", "load-string", siteLoc("main.lisp", 4, 1)),
			siteFrame("lisp", "handler-bind", siteLoc("gen.lisp", 1, 26)),
			siteFrame("app", "now", siteLoc("core.lisp", 5, 29)),
		},
		want: "core.lisp:5:29 <- gen.lisp:1:26",
	}, {
		name: "load-bytes starts a source stream",
		frames: []lisp.CallFrame{
			siteFrame("app", "load", nil),
			siteFrame("lisp", "load-bytes", siteLoc("main.lisp", 4, 1)),
			siteFrame("lisp", "handler-bind", siteLoc("gen.lisp", 1, 26)),
			siteFrame("app", "now", siteLoc("core.lisp", 5, 29)),
		},
		want: "core.lisp:5:29 <- gen.lisp:1:26",
	}, {
		name: "load-file of another package does not start a source stream",
		frames: []lisp.CallFrame{
			siteFrame("app", "load", nil),
			siteFrame("app", "load-file", siteLoc("main.lisp", 4, 1)),
			siteFrame("lisp", "handler-bind", siteLoc("main.lisp", 9, 3)),
			siteFrame("app", "now", siteLoc("core.lisp", 5, 29)),
		},
		want: "core.lisp:5:29 <- main.lisp:4:1",
	}, {
		name: "frames without a source position after the load are skipped",
		frames: []lisp.CallFrame{
			siteFrame("app", "load", nil),
			siteFrame("lisp", "load-file", siteLoc("main.lisp", 4, 1)),
			siteFrame("lisp", "native-thunk", native),
			siteFrame("lisp", "handler-bind", siteLoc("a.lisp", 2, 9)),
			siteFrame("app", "now", siteLoc("core.lisp", 5, 29)),
		},
		want: "core.lisp:5:29 <- a.lisp:2:9",
	}, {
		name: "inner is the only frame with a source position after the load",
		frames: []lisp.CallFrame{
			siteFrame("app", "load", nil),
			siteFrame("lisp", "load-file", siteLoc("main.lisp", 4, 1)),
			siteFrame("app", "now", siteLoc("a.lisp", 2, 9)),
		},
		want: "a.lisp:2:9",
	}}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			s := &lisp.CallStack{Frames: tt.frames}
			assert.Equal(t, tt.want, s.SourceSite())
		})
	}
}

func TestSourceSiteNilStack(t *testing.T) {
	var s *lisp.CallStack
	assert.Empty(t, s.SourceSite())
}

// TestSourceSiteLiveStack calls SourceSite from a builtin, through a
// load-string, on the stack an evaluation builds.
func TestSourceSiteLiveStack(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	var got string
	env.AddBuiltins(false, &testBuiltinDef{name: "site", formals: lisp.Formals(),
		fn: func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
			got = env.Runtime.Stack.SourceSite()
			return lisp.Nil()
		}})
	rc := env.LoadString("main.lisp", `(defun f () (site))
(load-string "(progn (f))" :name "gen.lisp")`)
	require.False(t, rc.IsError(), "%v", rc)
	assert.Equal(t, "main.lisp:1:13 <- gen.lisp:1:1", got)
}
