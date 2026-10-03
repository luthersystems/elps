// Copyright © 2026 The ELPS authors

package libjson

// Closures: a lambda with no global name is saved with its code and the
// frames it captured.
//
//	["~#closure",["PKG",ENV,CODE]]
//	ENV  = null | ["~#env",[ENV,["NAME",VALUE,...]]]      (or ~#obj / ~#ref of one)
//	CODE = ["~#code",[FORMALS,BODY...]]                   (or ~#obj / ~#ref of one)
//
// PKG is the package the lambda was defined in: its body resolves globals
// there when it is called, as a function restored by ~#fn does.  ENV is the
// innermost captured frame, and each frame names its parent; null is the
// root environment, whose names are globals.  A frame holds, in name order,
// the bindings the saved closures over it read (see durable_scope.go):
// each name a closure's code names is saved in the innermost frame that
// binds it, and a dynamic closure (one whose code calls eval or a macro
// that is not audited) keeps its frames whole.  Discovery computes these
// sets as a fixpoint before anything is counted, so the counting pass, the
// output and the decoder all see the same frames.  Frames that save
// nothing are left out of the chain.  A frame is an object, so two
// closures over one frame share it after a load, and a set! through one is
// seen by the other.  A closure is an object too, so a closure in its own
// frame (recursion) restores.
//
// CODE is the lambda's formals and body as code: each node keeps whether it
// is quoted, which decides whether the evaluator evaluates it.  A node is a
// typed scalar (an int, float, string or symbol), null (the empty list), a
// list ["~#list",[NODE...]] (a call form), ["~#quote",NODE] (NODE quoted,
// as the reader's ' quotes it), or ["~#lit",NODE]: a sealed program
// literal, which the load seals again, node by node, so a literal a macro
// or quasiquote put inside new code stays protected.  Code is an object,
// so closures made by one lambda form share it, and a load restores them
// over one copy of it.
//
// A load evaluates nothing: it builds the frames with NewEnv and Put, and
// the lambda with LEnv.NewLambdaCode and LEnv.RestoreLambda.  A restored
// closure keeps the code it was saved with; a named function (~#fn) is the
// current definition of its name.

import (
	"cmp"
	"errors"
	"fmt"
	"reflect"
	"slices"
	"strconv"
	"strings"
	"unicode/utf8"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/lisp"
)

// The closure extension tags.
const (
	tagClosure = "~#closure"
	tagEnv     = "~#env"
	tagCode    = "~#code"
	tagQuote   = "~#quote"
)

// errAnonymous is funName's answer for a function no global binds.
var errAnonymous = errors.New("durable json: cannot encode an anonymous function")

type (
	// closureKey identifies a closure by its function data, which every
	// header of it (FunRef copies) shares.
	closureKey struct{ p any }
	// frameKey identifies a captured frame.
	frameKey struct{ p *lisp.LEnv }
	// codeKey identifies a lambda's code: its formals header and the
	// headers of its body forms, which every closure made by one lambda
	// form shares.
	codeKey struct {
		formals *lisp.LVal
		body    string
	}
)

// binding is one saved frame binding.
type binding struct {
	value *lisp.LVal
	name  string
}

// span is a range of cell addresses.
type span struct {
	start, end uintptr
	code       bool
}

// lambdaEnv returns the frame a lambda captured, or nil for a builtin, a
// macro or a special operator.
func lambdaEnv(v *lisp.LVal) *lisp.LEnv {
	if v.IsSpecialFun() {
		return nil
	}
	return funraw.Env(v)
}

// codeKeyOf returns a closure's code key, built once per closure.
func (e *durableEncoder) codeKeyOf(f *lisp.LVal) codeKey {
	ck := closureKey{f.Native}
	if k, ok := e.codeKeys[ck]; ok {
		return k
	}
	b := make([]byte, 0, 8*len(f.Cells))
	for _, c := range f.Cells[1:] {
		b = strconv.AppendUint(b, uint64(reflect.ValueOf(c).Pointer()), 16)
		b = append(b, ' ')
	}
	k := codeKey{formals: f.Cells[0], body: string(b)}
	e.codeKeys[ck] = k
	return k
}

// chainOf returns the frames below the root a lambda captured, innermost
// first.  The walk is bounded by the scope's work limit.
func (e *durableEncoder) chainOf(env *lisp.LEnv) ([]*lisp.LEnv, error) {
	var chain []*lisp.LEnv
	for ; env != nil && env.Parent() != nil; env = env.Parent() {
		if err := e.scope().step(); err != nil {
			return nil, err
		}
		chain = append(chain, env)
	}
	return chain, nil
}

// frameOf returns the innermost frame at or above env that saves a
// binding, or nil when there is none below the root.
func (e *durableEncoder) frameOf(env *lisp.LEnv) *lisp.LEnv {
	for ; env != nil && env.Parent() != nil; env = env.Parent() {
		if len(e.marked[env]) > 0 {
			return env
		}
	}
	return nil
}

// bindings returns the bindings a frame saves, in name order.
func (e *durableEncoder) bindings(env *lisp.LEnv) []binding {
	if bs, ok := e.frameBindings[env]; ok {
		return bs
	}
	bs := make([]binding, 0, len(e.marked[env]))
	for name, v := range e.marked[env] {
		bs = append(bs, binding{name: name, value: v})
	}
	slices.SortFunc(bs, func(a, b binding) int { return cmp.Compare(a.name, b.name) })
	e.frameBindings[env] = bs
	return bs
}

// scope returns the dump's name resolver.
func (e *durableEncoder) scope() *closureScope {
	if e.closureScope == nil {
		e.closureScope = newClosureScope(e.env, e.cfg)
	}
	return e.closureScope
}

// markClosure records, during discovery, the frame bindings a closure
// reads: each name its code names, in the innermost frame below the root
// that binds it, or every binding of every frame for a dynamic closure.
// A binding marked for the first time has its value walked, so the
// closures it reaches mark theirs: the marks reach a fixpoint over the
// closures and frames by the time discovery ends.
func (e *durableEncoder) markClosure(v *lisp.LVal, depth int) error {
	chain, err := e.chainOf(lambdaEnv(v))
	if err != nil {
		return err
	}
	sc := e.scope()
	names := sc.codeNames(e.codeKeyOf(v), v.Cells)
	reason, err := sc.dynamicReason(v.Cells, chain, v.Package())
	if err != nil {
		return err
	}
	mark := func(env *lisp.LEnv, name string, value *lisp.LVal) error {
		if _, ok := e.marked[env][name]; ok {
			return nil
		}
		if e.marked[env] == nil {
			e.marked[env] = map[string]*lisp.LVal{}
		}
		if name == "" || !utf8.ValidString(name) {
			return errors.New("durable json: a captured variable's name is empty or not UTF-8")
		}
		e.marked[env][name] = value
		if err := e.scan(value, depth+1); err != nil {
			if reason != "" {
				return wholeFrameError(reason, name, err)
			}
			return capturedError(name, err)
		}
		return nil
	}
	if reason != "" {
		for _, env := range chain {
			if e.whole[env] {
				continue
			}
			e.whole[env] = true
			all, err := e.wholeFrame(env)
			if err != nil {
				return err
			}
			for _, b := range all {
				if err := mark(env, b.name, b.value); err != nil {
					return err
				}
			}
		}
		return nil
	}
	for _, name := range names {
		for _, env := range chain {
			if err := sc.step(); err != nil {
				return err
			}
			if value, ok := funraw.Lookup(env, name); ok {
				if err := mark(env, name, value); err != nil {
					return err
				}
				break
			}
		}
	}
	return nil
}

// wholeFrame reads every binding of a frame a dynamic closure captured, in
// name order.  The bindings are reserved against the value limit, and
// charged as package names are (ceil(n/4)), before they are copied.
func (e *durableEncoder) wholeFrame(env *lisp.LEnv) ([]binding, error) {
	n := env.NumBindings()
	if n > e.cfg.maxValues {
		return nil, fmt.Errorf("%w: more than %d values", ErrTypedLimit, e.cfg.maxValues)
	}
	if e.cfg.charge != nil && n > 0 {
		if err := e.cfg.charge(funNameScanUnits(n)); err != nil {
			return nil, fmt.Errorf("durable json: captured frame of %d bindings: %w", n, err)
		}
	}
	all := make([]binding, 0, n)
	for name, v := range env.Bindings() {
		all = append(all, binding{name: name, value: v})
	}
	slices.SortFunc(all, func(a, b binding) int { return cmp.Compare(a.name, b.name) })
	return all, nil
}

// wholeFrameError refuses a dynamic closure whose frame holds a value that
// cannot be saved.
func wholeFrameError(reason, name string, err error) error {
	if errors.Is(err, ErrTypedLimit) {
		return err
	}
	return fmt.Errorf("durable json: closure body %s; its frames must be saved whole; captured variable %q cannot be saved: %s",
		reason, name, strings.TrimPrefix(err.Error(), "durable json: "))
}

// scanFunValue resolves a function: a global name, or a closure.
func (e *durableEncoder) scanFunValue(v *lisp.LVal, depth int) error {
	k := funKey{v.Package(), v.FID()}
	name, ok := e.funs[k]
	if !ok {
		var err error
		name, err = e.funName(v)
		switch {
		case errors.Is(err, errAnonymous) && lambdaEnv(v) != nil:
			name = ""
		case err != nil:
			return err
		}
		e.funs[k] = name
	}
	if name != "" {
		return nil
	}
	return e.scanClosure(v, depth)
}

// scanClosure visits a closure: its frames and its code.
func (e *durableEncoder) scanClosure(v *lisp.LVal, depth int) error {
	key := closureKey{v.Native}
	e.refs[key]++
	if e.refs[key] > 1 {
		return e.revisit(nil, key)
	}
	if pkg := v.Package(); pkg == "" || !utf8.ValidString(pkg) || e.env.Runtime.Registry.Package(pkg) == nil {
		return errAnonymous
	}
	if err := e.scanDepth(depth); err != nil {
		return err
	}
	i := e.openObject(key)
	// Discovery checks the code (its size and depth) before it reads the
	// names in it.
	if e.discover {
		if err := e.scanCode(v, depth+1); err != nil {
			return err
		}
		if err := e.markClosure(v, depth); err != nil {
			return err
		}
	} else {
		if err := e.scanFrame(e.frameOf(lambdaEnv(v)), depth+1); err != nil {
			return err
		}
		if err := e.scanCode(v, depth+1); err != nil {
			return err
		}
	}
	e.closeNode(i)
	return nil
}

// scanFrame visits a frame (nil is the root): its parent and its
// bindings.  A refusal names the captured variable.
func (e *durableEncoder) scanFrame(env *lisp.LEnv, depth int) error {
	if err := e.countScan(1); err != nil {
		return err
	}
	if env == nil {
		return nil
	}
	key := frameKey{env}
	e.refs[key]++
	if e.refs[key] > 1 {
		return e.revisit(nil, key)
	}
	if err := e.scanDepth(depth); err != nil {
		return err
	}
	bs := e.bindings(env)
	i := e.openObject(key)
	if err := e.scanFrame(e.frameOf(env.Parent()), depth+1); err != nil {
		return err
	}
	for _, b := range bs {
		if err := e.countScan(1); err != nil {
			return err
		}
		if err := e.scan(b.value, depth+1); err != nil {
			return capturedError(b.name, err)
		}
	}
	e.closeNode(i)
	return nil
}

// capturedError puts a captured variable's name in front of a refusal, so
// the error gives the path to the refused value.
func capturedError(name string, err error) error {
	if errors.Is(err, ErrTypedLimit) {
		return err
	}
	return fmt.Errorf("durable json: captured variable %q: %s", name, strings.TrimPrefix(err.Error(), "durable json: "))
}

// scanCode visits a closure's code once per code object.
func (e *durableEncoder) scanCode(f *lisp.LVal, depth int) error {
	if err := e.countScan(1); err != nil {
		return err
	}
	key := e.codeKeyOf(f)
	e.refs[key]++
	if e.refs[key] > 1 {
		return e.revisit(nil, key)
	}
	if err := e.scanDepth(depth); err != nil {
		return err
	}
	i := e.openObject(key)
	for _, c := range f.Cells {
		if err := e.scanCodeNode(c, false, depth+1); err != nil {
			return err
		}
	}
	e.closeNode(i)
	return nil
}

// litBoundary reports whether codeNode writes v as ["~#lit",…]: a sealed
// list or quote outside any sealed node.
func litBoundary(v *lisp.LVal, inLit bool) bool {
	if inLit || !v.IsSealed() {
		return false
	}
	return v.Type == lisp.LQuote || (v.Type == lisp.LSExpr && len(v.Cells) > 0)
}

// scanCodeNode checks and counts one code node, as codeNode writes it.
// inLit marks a node inside a sealed literal, which must be sealed too:
// the load seals a literal whole.  During discovery it also records the
// cells of each mutable code list, which must share storage with nothing
// else in the graph.
func (e *durableEncoder) scanCodeNode(v *lisp.LVal, inLit bool, depth int) error {
	if v == nil {
		return errors.New("durable json: cannot encode a Go nil value")
	}
	if inLit && !v.IsSealed() && (v.Type == lisp.LQuote || v.Type == lisp.LSExpr && len(v.Cells) > 0) {
		return errors.New("durable json: a closure's code holds a mutable list inside a sealed literal")
	}
	if litBoundary(v, inLit) {
		if err := e.countScan(1); err != nil {
			return err
		}
		if err := e.scanDepth(depth); err != nil {
			return err
		}
		return e.scanCodeNode(v, true, depth+1)
	}
	switch {
	case v.Type == lisp.LQuote:
		if !v.IsQuoted() || len(v.Cells) != 1 || v.Cells[0] == nil || !v.Cells[0].IsQuoted() {
			return errors.New("durable json: a closure's code holds a malformed quote")
		}
		if err := e.countScan(1); err != nil {
			return err
		}
		if err := e.scanDepth(depth); err != nil {
			return err
		}
		return e.scanCodeNode(v.Cells[0], inLit, depth+1)
	case v.IsQuoted():
		if err := e.countScan(1); err != nil {
			return err
		}
		if err := e.scanDepth(depth); err != nil {
			return err
		}
		return e.scanCodeBody(v, inLit, depth+1)
	}
	return e.scanCodeBody(v, inLit, depth)
}

// scanCodeBody checks and counts a code node without its quote.
func (e *durableEncoder) scanCodeBody(v *lisp.LVal, inLit bool, depth int) error {
	if err := e.countScan(1); err != nil {
		return err
	}
	switch v.Type {
	case lisp.LInt, lisp.LFloat, lisp.LString, lisp.LSymbol:
		return nil
	case lisp.LSExpr:
		if len(v.Cells) == 0 {
			return nil
		}
		if err := e.scanDepth(depth); err != nil {
			return err
		}
		if e.discover && !v.IsSealed() {
			start := cellAddr(v.Cells)
			e.codeRanges = append(e.codeRanges, span{start: start, end: start + uintptr(len(v.Cells))*cellSize, code: true})
		}
		for _, c := range v.Cells {
			if err := e.scanCodeNode(c, inLit, depth+1); err != nil {
				return err
			}
		}
		return nil
	case lisp.LQuote, lisp.LArray, lisp.LSortMap, lisp.LBytes, lisp.LTaggedVal, lisp.LNative, lisp.LFun, lisp.LError,
		lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand, lisp.LInvalid, lisp.LTypeMax:
	}
	return fmt.Errorf("durable json: a closure's code holds a %v", v.Type)
}

// checkCodeSharing refuses a mutable code list that shares cells with
// anything else the dump saves: a value, or another code list (or itself,
// reached twice).  Code and values restore as separate objects, so that
// sharing could not be kept.  Sealed code lists share nothing anyone can
// write.
func (e *durableEncoder) checkCodeSharing() error {
	if len(e.codeRanges) == 0 {
		return nil
	}
	spans := slices.Clone(e.codeRanges)
	for _, h := range e.headers {
		if c := e.holderCap(h); c > 0 {
			start := cellAddr(h.Cells)
			spans = append(spans, span{start: start, end: start + uintptr(c)*cellSize})
		}
	}
	slices.SortFunc(spans, func(a, b span) int { return cmp.Compare(a.start, b.start) })
	for i := 0; i < len(spans); {
		j, end, code := i+1, spans[i].end, spans[i].code
		for j < len(spans) && spans[j].start < end {
			end = max(end, spans[j].end)
			code = code || spans[j].code
			j++
		}
		if code && j-i > 1 {
			return errors.New("durable json: a closure's code shares a mutable list with another value; code restores as its own copy, so the sharing cannot be kept")
		}
		i = j
	}
	return nil
}

// closure writes ["~#closure",["PKG",ENV,CODE]], shared or not.
func (e *durableEncoder) closure(v *lisp.LVal, depth int) error {
	key := closureKey{v.Native}
	body := func() error {
		if err := e.container(depth); err != nil {
			return err
		}
		pkg := v.Package()
		if err := e.reserve(jsonStringLen(pkg) + len(tagClosure) + 8); err != nil {
			return err
		}
		e.buf = append(e.buf, `["`+tagClosure+`",[`...)
		e.buf = appendJSONString(e.buf, pkg)
		e.buf = append(e.buf, ',')
		if err := e.frame(e.frameOf(lambdaEnv(v)), depth+1); err != nil {
			return err
		}
		e.buf = append(e.buf, ',')
		if err := e.code(v, depth+1); err != nil {
			return err
		}
		e.buf = append(e.buf, ']', ']')
		return e.grow()
	}
	if e.refs[key] >= 2 {
		return e.object(key, depth, body)
	}
	return body()
}

// frame writes a frame, or null for the root.
func (e *durableEncoder) frame(env *lisp.LEnv, depth int) error {
	if env == nil {
		return e.typedEncoder.value(lisp.Nil(), depth)
	}
	key := frameKey{env}
	body := func() error {
		if err := e.container(depth); err != nil {
			return err
		}
		e.buf = append(e.buf, `["`+tagEnv+`",[`...)
		if err := e.frame(e.frameOf(env.Parent()), depth+1); err != nil {
			return err
		}
		e.buf = append(e.buf, ',', '[')
		for k, b := range e.frameBindings[env] {
			if k > 0 {
				e.buf = append(e.buf, ',')
			}
			if err := e.count(); err != nil {
				return err
			}
			if err := e.reserve(jsonStringLen(b.name) + 1); err != nil {
				return err
			}
			e.buf = appendJSONString(e.buf, b.name)
			e.buf = append(e.buf, ',')
			if err := e.value(b.value, depth+1); err != nil {
				return err
			}
		}
		e.buf = append(e.buf, ']', ']', ']')
		return e.grow()
	}
	if e.refs[key] >= 2 {
		return e.object(key, depth, body)
	}
	return body()
}

// code writes ["~#code",[FORMALS,BODY...]], shared or not.
func (e *durableEncoder) code(f *lisp.LVal, depth int) error {
	key := e.codeKeyOf(f)
	body := func() error {
		if err := e.container(depth); err != nil {
			return err
		}
		e.buf = append(e.buf, `["`+tagCode+`",[`...)
		for k, c := range f.Cells {
			if k > 0 {
				e.buf = append(e.buf, ',')
			}
			if err := e.codeNode(c, false, depth+1); err != nil {
				return err
			}
		}
		e.buf = append(e.buf, ']', ']')
		return e.grow()
	}
	if e.refs[key] >= 2 {
		return e.object(key, depth, body)
	}
	return body()
}

// codeNode writes one code node: ["~#lit",NODE] for a sealed literal,
// ["~#quote",X] for a quoted node, where X is the node it quotes, else the
// node itself.
func (e *durableEncoder) codeNode(v *lisp.LVal, inLit bool, depth int) error {
	if litBoundary(v, inLit) {
		if err := e.container(depth); err != nil {
			return err
		}
		e.buf = append(e.buf, `["`+tagLit+`",`...)
		if err := e.codeNode(v, true, depth+1); err != nil {
			return err
		}
		e.buf = append(e.buf, ']')
		return e.grow()
	}
	if v.Type != lisp.LQuote && !v.IsQuoted() {
		return e.codeBody(v, inLit, depth)
	}
	if err := e.container(depth); err != nil {
		return err
	}
	e.buf = append(e.buf, `["`+tagQuote+`",`...)
	var err error
	if v.Type == lisp.LQuote {
		err = e.codeNode(v.Cells[0], inLit, depth+1)
	} else {
		err = e.codeBody(v, inLit, depth+1)
	}
	if err != nil {
		return err
	}
	e.buf = append(e.buf, ']')
	return e.grow()
}

// codeBody writes a code node without its quote.
func (e *durableEncoder) codeBody(v *lisp.LVal, inLit bool, depth int) error {
	if v.Type != lisp.LSExpr {
		return e.typedEncoder.value(v, depth)
	}
	if len(v.Cells) == 0 {
		return e.typedEncoder.value(lisp.Nil(), depth)
	}
	if err := e.container(depth); err != nil {
		return err
	}
	e.buf = append(e.buf, `["`+tagList+`",[`...)
	for k, c := range v.Cells {
		if k > 0 {
			e.buf = append(e.buf, ',')
		}
		if err := e.codeNode(c, inLit, depth+1); err != nil {
			return err
		}
	}
	e.buf = append(e.buf, ']', ']')
	return e.grow()
}
