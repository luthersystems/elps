// Copyright © 2026 The ELPS authors

package analysis

import (
	"bytes"
	"path/filepath"
	"sort"
	"strings"
	"sync"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/internal/codewalk"
	"github.com/luthersystems/elps/internal/testforms"
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

// RecordMethodsByFile holds the methods that the s:defmethod calls of each
// workspace file add, and their merge.  It is immutable: With returns a
// new value, so a Config that shares one stays valid while an editor
// replaces a file's entry.  A nil *RecordMethodsByFile holds no file.
//
// The methods are kept per file so an editor can replace one file's
// methods when it is saved, and so the analysis of a file can leave out
// that file's own entries: they come from the file on disk, and the text
// being analyzed declares its methods itself.
//
// Test files (IsTestSourceFile) are kept too, apart from the others: their
// methods are known only in the analysis of a test file.  A production
// load never holds them, so they must not hide an unknown member in a
// production file; a test may use a method its *_testhelpers.lisp adds.
//
// The merges are made once, when the value is built.  Except then copies
// only the analyzed file's types, so an analysis per keystroke costs the
// size of its own file's entries, not of the workspace.
type RecordMethodsByFile struct {
	// files maps each path, as NormalizePath returns it, to its methods.
	files map[string]RecordMethods
	// prod merges the methods of the files that are not test files, and
	// all those of every file.
	prod, all RecordMethods
	// owners maps each type and method to the paths that add it.
	owners map[string]map[string][]string
	// paths caches NormalizePath of the names Except is given.  It is
	// shared by every value With derives, and safe for concurrent use.
	paths *sync.Map
}

// NewRecordMethodsByFile returns the methods of files, a map from each
// file's path to the methods it adds.  The paths are normalized here,
// once.  It returns nil when no file adds a method.
func NewRecordMethodsByFile(files map[string]RecordMethods) *RecordMethodsByFile {
	norm := make(map[string]RecordMethods, len(files))
	for path, m := range files {
		if len(m) > 0 {
			norm[NormalizePath(path)] = m
		}
	}
	return newRecordMethodsByFile(norm, nil)
}

// newRecordMethodsByFile builds the merges of files, whose keys are
// normalized.
func newRecordMethodsByFile(files map[string]RecordMethods, paths *sync.Map) *RecordMethodsByFile {
	if len(files) == 0 {
		return nil
	}
	if paths == nil {
		paths = new(sync.Map)
	}
	b := &RecordMethodsByFile{files: files, owners: make(map[string]map[string][]string), paths: paths}
	keys := make([]string, 0, len(files))
	for path := range files {
		keys = append(keys, path)
	}
	// A fixed order makes each owners list, and so each merge, the same
	// on every build.
	sort.Strings(keys)
	for _, path := range keys {
		m := files[path]
		b.all = b.all.Merge(m)
		if !IsTestSourceFile(path) {
			b.prod = b.prod.Merge(m)
		}
		for typ, methods := range m {
			byName := b.owners[typ]
			if byName == nil {
				byName = make(map[string][]string)
				b.owners[typ] = byName
			}
			for name := range methods {
				byName[name] = append(byName[name], path)
			}
		}
	}
	return b
}

// Len returns the number of files that add a method.
func (b *RecordMethodsByFile) Len() int {
	if b == nil {
		return 0
	}
	return len(b.files)
}

// Except returns the methods of every file but file, as the analysis of
// file sees them: the methods of the test files are left out unless file
// is a test file itself.  The result may be shared; the caller must not
// change it.
func (b *RecordMethodsByFile) Except(file string) RecordMethods {
	if b == nil {
		return nil
	}
	test := IsTestSourceFile(file)
	merged := b.prod
	if test {
		merged = b.all
	}
	path := b.normalized(file)
	own := b.files[path]
	if len(own) == 0 {
		return merged
	}
	// Copy the merge, and rebuild each method of file's own entries from
	// the other files that add it.
	out := make(RecordMethods, len(merged))
	for typ, methods := range merged {
		out[typ] = methods
	}
	for typ, methods := range own {
		kept := make(map[string]*Signature, len(merged[typ]))
		for name, sig := range merged[typ] {
			if _, mine := methods[name]; !mine {
				kept[name] = sig
			}
		}
		rebuilt := RecordMethods{typ: kept}
		for name := range methods {
			for _, p := range b.owners[typ][name] {
				if p != path && (test || !IsTestSourceFile(p)) {
					rebuilt.add(typ, name, b.files[p][typ][name])
				}
			}
		}
		if len(kept) == 0 {
			delete(out, typ)
		} else {
			out[typ] = kept
		}
	}
	return out
}

// normalized returns NormalizePath(file), from the cache when Except was
// given file before.
func (b *RecordMethodsByFile) normalized(file string) string {
	if file == "" {
		return ""
	}
	if _, ok := b.files[file]; ok {
		return file
	}
	if v, ok := b.paths.Load(file); ok {
		if s, ok := v.(string); ok {
			return s
		}
	}
	path := NormalizePath(file)
	b.paths.Store(file, path)
	return path
}

// With returns a value in which file has the methods m, or has no entry
// when m is empty.  b is not changed: a Config that shares it may be in
// use by another analysis.
func (b *RecordMethodsByFile) With(file string, m RecordMethods) *RecordMethodsByFile {
	file = NormalizePath(file)
	var paths *sync.Map
	files := make(map[string]RecordMethods, b.Len()+1)
	if b != nil {
		paths = b.paths
		for path, methods := range b.files {
			if path != file {
				files[path] = methods
			}
		}
	}
	if len(m) > 0 {
		files[file] = m
	}
	return newRecordMethodsByFile(files, paths)
}

// File returns the methods that file adds.
func (b *RecordMethodsByFile) File(file string) RecordMethods {
	if b == nil {
		return nil
	}
	return b.files[b.normalized(file)]
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
// when a file is saved.  A test file's methods are returned too;
// RecordMethodsByFile keeps them apart.
func ExtractFileRecordMethods(source []byte, filename string) RecordMethods {
	return fileRecordMethods(source, func() ([]*lisp.LVal, error) {
		return rdparser.New(token.NewScanner(filename, bytes.NewReader(source))).ParseProgram()
	})
}

// fileRecordMethods returns the methods one file adds.  A file without the
// text "defmethod" holds no s:defmethod call, so it is not parsed or
// walked.  parse returns the file's forms.
func fileRecordMethods(source []byte, parse func() ([]*lisp.LVal, error)) RecordMethods {
	if !bytes.Contains(source, []byte("defmethod")) {
		return nil
	}
	exprs, err := parse()
	if err != nil {
		return nil
	}
	return ScanRecordMethods(exprs)
}

// recordDefmethod is the head of the call that adds a method to a record
// type.
const recordDefmethod = "s:defmethod"

// ScanRecordMethods returns the methods that the s:defmethod calls in exprs
// add.  A call is (s:defmethod TYPE NAME (recv formals...) body...), with
// TYPE and NAME symbols.  Only a call that runs at load counts
// (RecordDefmethods): an s:defmethod in a top-level let does, one in a
// defun body does not.  A call-time method is not part of the type a
// template publishes, so it must not make a member known.  The macros
// that defer a call are the builtin ones and those exprs defines
// (RecordMacroTest with no scope).
func ScanRecordMethods(exprs []*lisp.LVal) RecordMethods {
	var out RecordMethods
	macros := RecordMacroTest(nil, exprs)
	for _, top := range exprs {
		if !astutil.MentionsSymbol([]*lisp.LVal{top}, isDefmethodName) {
			continue
		}
		for _, d := range RecordDefmethods(top, macros(top)) {
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
	// the innermost form that keeps the call from running at load: the
	// head of a macro call around it, or the CodeWalker's name for the
	// function body that holds it (defun, lambda, flet, ...).
	NotLoadScope string
}

// maxRecordWalk bounds the lists RecordDefmethods walks in one top form.
// A call past the budget is not returned, so a check that reads the
// result misses a finding and makes no false one.
const maxRecordWalk = 1 << 16

// RecordDefmethods returns the s:defmethod calls in top, each with whether
// it runs at load.  isMacro reports whether a call head defers its
// arguments: a macro or a test form (RecordMacroTest).  With isMacro nil,
// a head defers its arguments only when it names a test form.
//
// A template publishes a record type with its methods as they stand at
// publication, so only a s:defmethod that runs at load is part of the
// type.  A call runs at load when nothing around it defers it:
//
//   - A function body (defun, lambda, a flet or labels function) runs at
//     call time.
//   - A test body (test, test-let, test-let*, benchmark, benchmark-simple)
//     runs when the test runner runs the test, after the load.  A name
//     the file defines, or a local function binds, is not the test form.
//   - The arguments of a macro call run when the macro decides, if ever.
//     Macros are not expanded, so a macro such as an endpoint definition
//     defers its body.  defconst is a macro whose value runs at load, so
//     it is the exception, and a head a local function binds is that
//     function.
//   - The arguments of a function call run at once, before the call:
//     (set 'x (list (s:defmethod ...))) runs the s:defmethod at load.
//   - Every other form the CodeWalker knows (let, let*, progn, if, when,
//     the body of flet and labels, ...) runs its parts at load.
//
// A head isMacro does not know is taken for a function.  The prescan then
// counts the method as a member, and the check record-defmethod-not-toplevel
// does not report the call: a wrong guess misses a finding, and never
// reports a known method as an unknown member.
//
// The rule reads only the walker's scope and call events, so it follows
// the walker's grammar for every special form.  One walk covers all calls
// in top.
func RecordDefmethods(top *lisp.LVal, isMacro func(head string) bool) []RecordDefmethod {
	if isMacro == nil {
		// With no isMacro, a test form name is the test form.
		isMacro = func(head string) bool { return testforms.Name(head) != "" }
	}
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
		// The innermost frame names the finding: it is the form that
		// defers the call.
		if len(frames) == 0 {
			return ""
		}
		return frames[len(frames)-1].where
	}
	// call is the call form whose head the walk reports next, as a WalkRef
	// one deeper than the form.  Whether the head is a local function is
	// known only then.
	var call struct {
		head  string
		depth int
		ok    bool
	}
	cw := &lisp.CodeWalker{KeepGoing: true, Visit: func(n *lisp.WalkNode) bool {
		for len(frames) > 0 && frames[len(frames)-1].depth >= n.Depth {
			frames = frames[:len(frames)-1]
		}
		switch n.Event {
		case lisp.WalkEnter:
			if n.Function {
				frames = append(frames, frame{where: n.Op, depth: n.Depth})
				break
			}
			// The test runner runs a test or benchmark body after the
			// load, as a function body runs at call time.  The walker
			// reads the head as the form only when no local binds it;
			// isMacro says whether the program defines the name itself.
			if testforms.Name(n.Op) != "" && isMacro(astutil.HeadSymbol(n.Node)) {
				frames = append(frames, frame{where: n.Op, depth: n.Depth})
			}
		case lisp.WalkForm:
			call.ok = false
			if n.Op != "" {
				break
			}
			head := astutil.HeadSymbol(n.Node)
			if head == recordDefmethod && !seen[n.Node] {
				seen[n.Node] = true
				out = append(out, RecordDefmethod{Call: n.Node, NotLoadScope: where()})
			}
			if isMacro(head) && !loadScopeCall(head) {
				call.head, call.depth, call.ok = head, n.Depth, true
			}
		case lisp.WalkRef:
			// A head a local function binds names that function, not the
			// macro or test form, so its arguments run at once.
			if call.ok && n.Head && n.Depth == call.depth+1 && !n.Bound {
				frames = append(frames, frame{where: call.head, depth: call.depth})
			}
			call.ok = false
		case lisp.WalkSet, lisp.WalkBind, lisp.WalkDefine, lisp.WalkLiteral, lisp.WalkData, lisp.WalkLeave:
			call.ok = false
		}
		return true
	}}
	codewalk.Occurrences(cw, maxRecordWalk, top)
	return out
}

// loadScopeCall reports whether head is a call whose value argument runs
// at load although the call may be a macro: a set or a defconst.
func loadScopeCall(head string) bool {
	switch strings.TrimPrefix(head, lisp.DefaultLangPackage+":") {
	case "set", "defconst":
		return true
	}
	return false
}

// recordSchemaMacros are the libschema macros whose arguments hold
// method bodies.  The analysis does not load libschema, so it names them
// here.
var recordSchemaMacros = map[string]bool{
	"s:make-record": true, "s:method": true, recordDefmethod: true, "s:make-interface": true,
}

// builtinMacros returns the names of the lisp package's macros.
var builtinMacros = sync.OnceValue(func() map[string]bool {
	out := make(map[string]bool)
	for _, m := range lisp.DefaultMacros() {
		out[m.Name()] = true
	}
	return out
})

// RecordMacroTest returns, for a top-level form of exprs, the test
// RecordDefmethods uses for whether a call head in that form defers its
// arguments: a macro, or a test definition form (test, test-let,
// test-let*, benchmark, benchmark-simple, also lisp:- or
// testing:-qualified).
//
// One rule decides: a name the program defines itself means what that
// definition makes it, a macro or not; any other name means what the
// builtin of that name is.  With a scope, the scope says whether the name
// resolves to a user or imported definition, for every form.  Without
// one, as in the workspace prescan, the definitions are the top-level
// defun, defmacro, deftype and set forms of exprs, read once, each in the
// package the in-package forms before it select.  A bare head means the
// definition in the package of its own top-level form, and a qualified
// head the one in its package.  A local binding is not read here: the
// walk sees that the head is lexically bound (RecordDefmethods).  A name
// with no definition and no builtin macro or test form is taken for a
// function; see RecordDefmethods for why that is the safe guess.
func RecordMacroTest(scope *Scope, exprs []*lisp.LVal) func(top *lisp.LVal) func(head string) bool {
	if scope != nil {
		test := func(head string) bool {
			if known, ok := scopeMacro(scope, head); ok {
				return known
			}
			return builtinDefers(head)
		}
		return func(*lisp.LVal) func(string) bool { return test }
	}
	defs := make(map[string]SymbolKind)
	for _, def := range extractDefinitions(exprs) {
		defs[definitionKey(def.Package, def.Name)] = def.Kind
	}
	pkgs := topLevelPackages(exprs)
	return func(top *lisp.LVal) func(string) bool {
		pkg := lisp.DefaultUserPackage
		if p, ok := pkgs[top]; ok {
			pkg = p
		}
		return func(head string) bool {
			if recordSchemaMacros[head] {
				return true
			}
			key := definitionKey(pkg, head)
			if p, bare, ok := strings.Cut(head, ":"); ok && p != "" {
				key = definitionKey(p, bare)
			}
			if kind, ok := defs[key]; ok {
				return kind == SymMacro
			}
			return builtinDefers(head)
		}
	}
}

// scopeMacro reports whether head names a macro, from the user or
// imported definition scope resolves it to.  It returns false for ok when
// the program does not define the name.
func scopeMacro(scope *Scope, head string) (bool, bool) {
	if head == "" {
		return false, true
	}
	if recordSchemaMacros[head] {
		return true, true
	}
	name := strings.TrimPrefix(head, lisp.DefaultLangPackage+":")
	var sym *Symbol
	if pkg, bare, ok := strings.Cut(name, ":"); ok && pkg != "" {
		sym = scope.LookupInPackage(bare, pkg)
	} else {
		sym = scope.Lookup(name)
	}
	if sym != nil && (sym.Source != nil || sym.External) {
		return sym.Kind == SymMacro, true
	}
	return false, false
}

// builtinDefers reports whether head names a builtin macro or test form.
func builtinDefers(head string) bool {
	return testforms.Name(head) != "" || builtinMacros()[strings.TrimPrefix(head, lisp.DefaultLangPackage+":")]
}

// topLevelPackages maps each top-level form of exprs to the package the
// in-package forms before it select.
func topLevelPackages(exprs []*lisp.LVal) map[*lisp.LVal]string {
	pkgs := make(map[*lisp.LVal]string, len(exprs))
	pkg := lisp.DefaultUserPackage
	for _, expr := range exprs {
		pkgs[expr] = pkg
		if expr.Type == lisp.LSExpr && !expr.IsQuoted() && astutil.HeadSymbol(expr) == "in-package" {
			if name := scanPackageName(expr); name != "" {
				pkg = name
			}
		}
	}
	return pkgs
}

// isDefmethodName reports whether name is the head of a s:defmethod call.
func isDefmethodName(name string) bool { return name == recordDefmethod }
