// Copyright © 2026 The ELPS authors

package analysis

import (
	"bytes"
	"path/filepath"
	"strings"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/internal/codewalk"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// RecordMethods maps the name of a record type to the methods that
// s:defmethod calls add to it, and each method to its signature after the
// receiver.  A nil signature means the formals cannot be read, or that two
// s:defmethod calls give the method different arities.
//
// The lint checks record-unknown-member and record-method-arity read it
// from Result.WorkspaceRecordMethods.  A type's s:make-record is often in
// one file and an s:defmethod for it in another.  Without the workspace's
// methods, a call to such a method reads as an unknown member.
//
// A type is keyed by its name without a package (RecordTypeKey).  Two
// packages with a type of one name share one method set; the checks then
// miss a finding, never report a false one.
type RecordMethods map[string]map[string]*Signature

// RecordMethodsByFile maps the path of each workspace file, as
// NormalizePath returns it, to the methods its s:defmethod calls add.
//
// The methods are kept per file so an editor can replace one file's
// methods when it is saved, and so the analysis of a file can leave out
// that file's own entries: they come from the file on disk, and the text
// being analyzed declares its methods itself.
type RecordMethodsByFile map[string]RecordMethods

// Except returns the methods of every file but file.
func (b RecordMethodsByFile) Except(file string) RecordMethods {
	skip := ""
	if file != "" {
		skip = NormalizePath(file)
	}
	var out RecordMethods
	for path, methods := range b {
		if path != skip {
			out = out.Merge(methods)
		}
	}
	return out
}

// With returns a copy of b in which file has the methods m, or has no
// entry when m is empty.  b is not changed: a Config that shares it may
// be in use by another analysis.
func (b RecordMethodsByFile) With(file string, m RecordMethods) RecordMethodsByFile {
	file = NormalizePath(file)
	out := make(RecordMethodsByFile, len(b)+1)
	for path, methods := range b {
		if path != file {
			out[path] = methods
		}
	}
	if len(m) > 0 {
		out[file] = m
	}
	return out
}

// RecordTypeKey returns the key of a record type's name in RecordMethods:
// the name without its package qualifier.  (s:defmethod shop:order ...) in
// one package and (set 'order ...) in another name one type.
func RecordTypeKey(name string) string {
	if i := strings.LastIndexByte(name, ':'); i >= 0 {
		return name[i+1:]
	}
	return name
}

// IsTestSourceFile reports whether path is a file only test runners load:
// a *_test.lisp file or a *_testhelpers.lisp file.  No production load or
// template holds what such a file defines.
func IsTestSourceFile(path string) bool {
	base := filepath.Base(path)
	return strings.HasSuffix(base, "_test.lisp") || strings.HasSuffix(base, "_testhelpers.lisp")
}

// ExtractFileRecordMethods returns the methods the s:defmethod calls of one
// file add, as the workspace prescan collects them.  An editor calls it
// when a file is saved.
func ExtractFileRecordMethods(source []byte, filename string) RecordMethods {
	if !mayAddRecordMethods(source, filename) {
		return nil
	}
	exprs, err := rdparser.New(token.NewScanner(filename, bytes.NewReader(source))).ParseProgram()
	if err != nil {
		return nil
	}
	return ScanRecordMethods(exprs)
}

// mayAddRecordMethods reports whether a file can add a method that the
// workspace knows.  A test file adds none: a method that only a test adds
// would hide an unknown member in a production file.  A file without the
// text "defmethod" holds no s:defmethod call, so it skips the walk.
func mayAddRecordMethods(source []byte, filename string) bool {
	return !IsTestSourceFile(filename) && bytes.Contains(source, []byte("defmethod"))
}

// recordDefmethod is the head of the call that adds a method to a record
// type.
const recordDefmethod = "s:defmethod"

// ScanRecordMethods returns the methods that the s:defmethod calls in exprs
// add.  A call is (s:defmethod TYPE NAME (recv formals...) body...), with
// TYPE and NAME symbols.  Only a call that runs at load counts
// (RecordDefmethods): an s:defmethod in a top-level let does, one in a
// defun body does not.  A call-time method is not part of the type a
// template publishes, so it must not make a member known.
func ScanRecordMethods(exprs []*lisp.LVal) RecordMethods {
	var out RecordMethods
	for _, top := range exprs {
		if !mentionsSymbol(top, recordDefmethod) {
			continue
		}
		for _, d := range RecordDefmethods(top) {
			c := d.Call.Cells
			if d.NotLoadScope != "" || len(c) < 4 || c[1].Type != lisp.LSymbol || c[2].Type != lisp.LSymbol {
				continue
			}
			if out == nil {
				out = make(RecordMethods)
			}
			out.add(RecordTypeKey(c[1].Str), c[2].Str, RecordMethodSignature(c[3]))
		}
	}
	return out
}

// Merge adds the methods of o to m.  It returns m, or a new map when m is
// nil and o is not empty.
func (m RecordMethods) Merge(o RecordMethods) RecordMethods {
	for typ, methods := range o {
		for name, sig := range methods {
			if m == nil {
				m = make(RecordMethods)
			}
			m.add(typ, name, sig)
		}
	}
	return m
}

// add records one method.  A method added twice with different arities
// gets a nil signature: the checks cannot know which call wins at run time.
func (m RecordMethods) add(typ, name string, sig *Signature) {
	methods := m[typ]
	if methods == nil {
		methods = make(map[string]*Signature)
		m[typ] = methods
	}
	if old, ok := methods[name]; ok && !sameArity(old, sig) {
		sig = nil
	}
	methods[name] = sig
}

// sameArity reports whether a and b accept the same numbers of arguments.
func sameArity(a, b *Signature) bool {
	if a == nil || b == nil {
		return a == b
	}
	return a.MinArity() == b.MinArity() && a.MaxArity() == b.MaxArity()
}

// RecordMethodFormals returns a method's formals, the receiver first, with
// each typed formal, [name type] (luthersystems/elps#905), replaced by its
// name.  It returns nil when formals is not a list with a receiver.
func RecordMethodFormals(formals *lisp.LVal) []*lisp.LVal {
	if formals == nil || formals.Type != lisp.LSExpr || len(formals.Cells) == 0 {
		return nil
	}
	names := make([]*lisp.LVal, 0, len(formals.Cells))
	for _, p := range formals.Cells {
		if p.Type == lisp.LSExpr && len(p.Cells) == 2 && p.Cells[0].Type == lisp.LSymbol {
			p = p.Cells[0]
		}
		names = append(names, p)
	}
	return names
}

// RecordMethodSignature returns the signature of a method's formals after
// the receiver, or nil when they are not a list with a receiver.
func RecordMethodSignature(formals *lisp.LVal) *Signature {
	names := RecordMethodFormals(formals)
	if names == nil {
		return nil
	}
	return &Signature{Params: lisp.ParseFormals(lisp.SExpr(names[1:]))}
}

// RecordDefmethod is one s:defmethod call in code position in a top-level
// form.
type RecordDefmethod struct {
	// Call is the s:defmethod call.
	Call *lisp.LVal
	// NotLoadScope is "" when the call runs at load.  Otherwise it names
	// the outermost form that keeps the call from running at load: the
	// head of a call or macro call around it, or the CodeWalker's name for
	// the function body that holds it (defun, lambda, flet, ...).
	NotLoadScope string
}

// maxRecordWalk bounds the lists RecordDefmethods walks in one top form.
// A call past the budget is not returned, so a check that reads the
// result misses a finding and makes no false one.
const maxRecordWalk = 1 << 16

// RecordDefmethods returns the s:defmethod calls in top, each with whether
// it runs at load.
//
// A template publishes a record type with its methods as they stand at
// publication, so only a s:defmethod that runs at load is part of the
// type.  A call runs at load when nothing around it defers it or hides it:
//
//   - A function body (defun, lambda, a flet or labels function) runs at
//     call time.
//   - The arguments of a function call or a macro call run when the callee
//     decides.  Macros are not expanded, so a macro such as an endpoint
//     definition counts as a call.  The value of a set or defconst runs at
//     load, so those two calls are the exception.
//   - Every other form the CodeWalker knows (let, let*, progn, if, when,
//     the body of flet and labels, ...) runs its parts at load.
//
// The rule reads only the walker's scope and call events, so it follows
// the walker's grammar for every special form.  One walk covers all calls
// in top.
func RecordDefmethods(top *lisp.LVal) []RecordDefmethod {
	// frames holds the forms around the walk position that defer it, each
	// with the depth of its event.  The walker reports everything inside a
	// form or scope deeper than the form or scope, so a frame ends at the
	// first event that is not deeper.
	type frame struct {
		where string
		depth int
	}
	var frames []frame
	var out []RecordDefmethod
	seen := make(map[*lisp.LVal]bool)
	where := func() string {
		if len(frames) == 0 {
			return ""
		}
		return frames[len(frames)-1].where
	}
	push := func(depth int, name string) {
		// The outermost form names the finding: it is the one to move.
		if outer := where(); outer != "" {
			name = outer
		}
		frames = append(frames, frame{where: name, depth: depth})
	}
	cw := &lisp.CodeWalker{KeepGoing: true, Visit: func(n *lisp.WalkNode) bool {
		for len(frames) > 0 && frames[len(frames)-1].depth >= n.Depth {
			frames = frames[:len(frames)-1]
		}
		switch n.Event {
		case lisp.WalkEnter:
			if n.Function {
				push(n.Depth, n.Op)
			}
		case lisp.WalkForm:
			if n.Op != "" {
				break
			}
			head := astutil.HeadSymbol(n.Node)
			if head == recordDefmethod && !seen[n.Node] {
				seen[n.Node] = true
				out = append(out, RecordDefmethod{Call: n.Node, NotLoadScope: where()})
			}
			if !loadScopeCall(head) {
				push(n.Depth, head)
			}
		case lisp.WalkRef, lisp.WalkSet, lisp.WalkBind, lisp.WalkDefine, lisp.WalkLiteral, lisp.WalkData, lisp.WalkLeave:
		}
		return true
	}}
	codewalk.Occurrences(cw, maxRecordWalk, top)
	return out
}

// loadScopeCall reports whether head is a call whose value argument runs
// at load: a set or a defconst.  Both are calls to the CodeWalker, which
// knows only special forms.
func loadScopeCall(head string) bool {
	switch strings.TrimPrefix(head, lisp.DefaultLangPackage+":") {
	case "set", "defconst":
		return true
	}
	return false
}

// mentionsSymbol reports whether form holds the symbol name.  It skips the
// call-site query for the forms that cannot hold a call, most of a usual
// file.
func mentionsSymbol(form *lisp.LVal, name string) bool {
	found := false
	astutil.Walk([]*lisp.LVal{form}, func(node, _ *lisp.LVal, _ int) {
		found = found || node.IsSymbol(name)
	})
	return found
}
