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
// root environment, which is never saved: a restored closure hangs off the
// loading root, and resolves names no saved frame binds there (the root's
// own scope, then PKG's globals).  A frame is saved whole, with
// every binding in name order: which names a closure's code may reach
// cannot be decided statically (eval and macros reach names the code does
// not spell), so nothing is dropped.  Frames with no bindings are left out
// of the chain.  A frame is an object, so two closures over one frame share
// it after a load, and a set! through one is seen by the other.  A closure
// is an object too, so a closure in its own frame (recursion) restores.
//
// CODE is the lambda's formals and body as code: each node keeps whether it
// is quoted, which decides whether the evaluator evaluates it.  A node is a
// typed scalar (an int, float, string or symbol), null (the empty list), a
// list ["~#list",[NODE...]] (a call form), ["~#quote",NODE] (NODE quoted,
// as the reader's ' quotes it), or ["~#lit",NODE]: a sealed program
// literal, which the load seals again, node by node, so a literal a macro
// or quasiquote put inside new code stays protected.  Code is an object
// identified by its cells: closures restored from one code object share
// them, so they write one code object again, and a load restores them over
// one copy of it.  Each closure lambda makes holds its own copy of the
// form's cells, so each writes its own code object.
//
// A load evaluates nothing: it builds the frames with NewEnv and Put, and
// the lambda with LEnv.NewLambdaCode and LEnv.RestoreLambda.  A restored
// closure keeps the code it was saved with; a named function (~#fn) is the
// current definition of its name.

import (
	"cmp"
	"errors"
	"fmt"
	"math"
	"slices"
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
	// codeKey identifies a lambda's code by its cells: the closures
	// restored from one code object share them.  A constant-size key, so
	// a closure costs the same however long its body is.
	codeKey struct {
		p **lisp.LVal
		n int
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

// codeKeyOf returns a closure's code key.
func codeKeyOf(f *lisp.LVal) codeKey {
	return codeKey{p: &f.Cells[0], n: len(f.Cells)}
}

// frameOf returns the innermost frame at or above env that has bindings,
// or nil when there is none below the root.  Each environment's answer is
// memoized for the whole dump, so a long chain of empty frames is walked
// once, not once per closure or pass.  Each environment walked for the
// first time is a step.  The steps are checked against frameWalkCap and
// charged one unit per bindingsPerUnit of them, each before the step it
// covers, so a chain past the cap or the budget stops where it crosses it.
func (e *durableEncoder) frameOf(env *lisp.LEnv) (*lisp.LEnv, error) {
	var walked []*lisp.LEnv
	var found *lisp.LEnv
	limit := frameWalkCap(e.cfg.maxValues)
	for ; env != nil && env.Parent() != nil; env = env.Parent() {
		if r, ok := e.nearestFrame[env]; ok {
			found = r
			break
		}
		if e.ancestorVisits >= limit {
			return nil, fmt.Errorf("%w: captured frame chains longer than %d frames", ErrTypedLimit, limit)
		}
		if e.cfg.charge != nil && e.ancestorVisits%bindingsPerUnit == 0 {
			if err := e.cfg.charge(1); err != nil {
				return nil, fmt.Errorf("durable json: captured frame chain: %w", err)
			}
		}
		e.ancestorVisits++
		walked = append(walked, env)
		if env.NumBindings() > 0 {
			found = env
			break
		}
	}
	for _, w := range walked {
		e.nearestFrame[w] = found
	}
	return found, nil
}

// frameWalkFactor bounds the environments a dump walks to find captured
// frames: at most frameWalkFactor per value of the value limit, plus
// frameWalkSlack.
const (
	frameWalkFactor = 16
	frameWalkSlack  = 1024
)

// frameWalkCap returns frameWalkFactor*maxValues + frameWalkSlack,
// saturated at math.MaxInt so a large value limit on a 32-bit platform
// cannot wrap it negative.
func frameWalkCap(maxValues int) int {
	if maxValues <= 0 {
		return frameWalkSlack
	}
	if maxValues > (math.MaxInt-frameWalkSlack)/frameWalkFactor {
		return math.MaxInt
	}
	return frameWalkFactor*maxValues + frameWalkSlack
}

// bindings returns a frame's bindings in name order, read once per dump.
// Before it copies them, it reserves them against the value limit, summed
// over every frame the dump reads: each binding is written, so a dump
// that reads more than the limit allows cannot succeed.
func (e *durableEncoder) bindings(env *lisp.LEnv) ([]binding, error) {
	if bs, ok := e.frameBindings[env]; ok {
		return bs, nil
	}
	n := env.NumBindings()
	e.frameReserved += n
	if e.frameReserved > e.cfg.maxValues {
		return nil, fmt.Errorf("%w: more than %d values", ErrTypedLimit, e.cfg.maxValues)
	}
	bs := make([]binding, 0, n)
	for name, v := range env.Bindings() {
		if name == "" || !utf8.ValidString(name) {
			return nil, errors.New("durable json: a captured variable's name is empty or not UTF-8")
		}
		bs = append(bs, binding{name: name, value: v})
	}
	slices.SortFunc(bs, func(a, b binding) int { return cmp.Compare(a.name, b.name) })
	e.frameBindings[env] = bs
	return bs, nil
}

// scanFunValue resolves a function: a registered builtin, a global name,
// or a closure.  An unregistered builtin is named by the bindings of its
// package that hold it.
func (e *durableEncoder) scanFunValue(v *lisp.LVal, depth int) error {
	if v.Builtin() != nil {
		if _, _, ok, err := e.registeredBuiltin(v); ok || err != nil {
			return err
		}
		_, err := e.unregisteredBuiltinName(v)
		return err
	}
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
	fr, err := e.frameOf(lambdaEnv(v))
	if err != nil {
		return err
	}
	if err := e.scanFrame(fr, depth+1); err != nil {
		return err
	}
	if err := e.scanCode(v, depth+1); err != nil {
		return err
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
	bs, err := e.bindings(env)
	if err != nil {
		return err
	}
	i := e.openObject(key)
	parent, err := e.frameOf(env.Parent())
	if err != nil {
		return err
	}
	if err := e.scanFrame(parent, depth+1); err != nil {
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
	key := codeKeyOf(f)
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
	e.codeVisits++
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
		fr, err := e.frameOf(lambdaEnv(v))
		if err != nil {
			return err
		}
		if err := e.frame(fr, depth+1); err != nil {
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
		parent, err := e.frameOf(env.Parent())
		if err != nil {
			return err
		}
		if err := e.frame(parent, depth+1); err != nil {
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
	key := codeKeyOf(f)
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
