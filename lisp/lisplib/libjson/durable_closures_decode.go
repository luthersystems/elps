// Copyright © 2026 The ELPS authors

package libjson

import (
	"bytes"
	"fmt"
	"unicode/utf8"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/lisp"
)

// decCode is a code object being read: its cells, and the lambda code
// NewLambdaCode validated once for every closure over it.
type decCode struct {
	code  *lisp.LambdaCode
	cells []*lisp.LVal
}

// decClosure is a restored closure, for the check at the end.
type decClosure struct {
	env  *lisp.LEnv
	code *decCode
	pkg  string
}

// decFrame is a restored frame and the names it binds, in order.
type decFrame struct {
	env   *lisp.LEnv
	names []string
}

// closureValue reads ["PKG",ENV,CODE] after "~#closure",[ and rebuilds the
// closure.  Its header is defined first, so a captured frame can refer to
// it.
func (d *durableDecoder) closureValue(depth int) (*lisp.LVal, error) {
	h := &lisp.LVal{}
	d.define(h)
	s, err := d.rawString()
	if err != nil {
		return nil, err
	}
	pkg := string(s)
	if d.env.Runtime.Registry.Package(pkg) == nil {
		return nil, d.errorf("closure of unknown package %q", pkg)
	}
	if err = d.expect(','); err != nil {
		return nil, err
	}
	env, err := d.frameRef(depth + 1)
	if err != nil {
		return nil, err
	}
	if err = d.expect(','); err != nil {
		return nil, err
	}
	code, err := d.codeRef(depth + 1)
	if err != nil {
		return nil, err
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	f := env.RestoreLambda(pkg, code.code)
	if f.Type == lisp.LError {
		return nil, d.errorf("closure: %v", (*lisp.ErrorVal)(f).ErrorMessage())
	}
	*h = *f
	d.closures = append(d.closures, decClosure{env: env, code: code, pkg: pkg})
	return h, nil
}

// root returns the root of the loading environment.
func (d *durableDecoder) root() *lisp.LEnv {
	env := d.env
	for env.Parent() != nil {
		env = env.Parent()
	}
	return env
}

// frameRef reads a frame position: null (the root), a frame, or a shared
// one.
func (d *durableDecoder) frameRef(depth int) (*lisp.LEnv, error) {
	if bytes.HasPrefix(d.b[d.i:], []byte("null")) {
		if err := d.count(); err != nil {
			return nil, err
		}
		d.i += 4
		return d.root(), nil
	}
	if !bytes.HasPrefix(d.b[d.i:], []byte(`["~#`)) {
		return nil, d.errorf("a closure's frame must be null, a frame or a shared one")
	}
	d.pos = posFrame
	v, err := d.value(depth)
	d.pos = posValue
	if err != nil {
		return nil, err
	}
	env, ok := v.Native.(*lisp.LEnv)
	if !ok || v.Type != lisp.LNative || env == nil {
		return nil, d.errorf("a frame that is its own ancestor")
	}
	return env, nil
}

// frameValue reads [PARENT,["NAME",VALUE,...]] after "~#env",[.  The frame
// is defined before its bindings are read, so a binding can refer to a
// closure over it.
func (d *durableDecoder) frameValue(depth int) (*lisp.LVal, error) {
	ph := &lisp.LVal{Type: lisp.LNative}
	d.define(ph)
	parent, err := d.frameRef(depth + 1)
	if err != nil {
		return nil, err
	}
	env := lisp.NewEnv(parent)
	ph.Native = env //elpsvet:allow-native the decoder's own placeholder for a frame object: it lives only in d.objs and is unwrapped by frameRef, so it never reaches a value LoadDurable returns, let alone a template
	if err = d.expect(','); err != nil {
		return nil, err
	}
	if err = d.expect('['); err != nil {
		return nil, err
	}
	fr := decFrame{env: env}
	var prev []byte
	for first := true; ; first = false {
		if d.peek() == ']' {
			if first {
				return nil, d.errorf("a frame with no bindings")
			}
			d.i++
			break
		}
		if !first {
			if err = d.expect(','); err != nil {
				return nil, err
			}
		}
		if err = d.count(); err != nil {
			return nil, err
		}
		name, err := d.rawString()
		if err != nil {
			return nil, err
		}
		if len(name) == 0 || !utf8.Valid(name) {
			return nil, d.errorf("a captured variable's name is empty or not UTF-8")
		}
		if prev != nil && bytes.Compare(prev, name) >= 0 {
			return nil, d.errorf("captured variables out of order or duplicated")
		}
		prev = append(prev[:0], name...)
		sym := lisp.Symbol(string(name))
		if err = d.expect(','); err != nil {
			return nil, err
		}
		v, err := d.value(depth + 1)
		if err != nil {
			return nil, err
		}
		if r := env.Put(sym, v); r.Type == lisp.LError {
			return nil, d.errorf("captured variable %q: %v", sym.Str, (*lisp.ErrorVal)(r).ErrorMessage())
		}
		fr.names = append(fr.names, sym.Str)
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	d.restoredFrames = append(d.restoredFrames, fr)
	return ph, nil
}

// codeRef reads a code position: code, or shared code.
func (d *durableDecoder) codeRef(depth int) (*decCode, error) {
	if !bytes.HasPrefix(d.b[d.i:], []byte(`["~#`)) {
		return nil, d.errorf("a closure's code must be code or shared code")
	}
	d.pos = posCode
	v, err := d.value(depth)
	d.pos = posValue
	if err != nil {
		return nil, err
	}
	code, ok := v.Native.(*decCode)
	if !ok || code == nil {
		return nil, d.errorf("a closure's code is its own ancestor")
	}
	return code, nil
}

// codeValue reads [FORMALS,BODY...] after "~#code",[ and validates the
// formals once, as lambda does, for every closure over this code.
func (d *durableDecoder) codeValue(depth int) (*lisp.LVal, error) {
	ph := &lisp.LVal{Type: lisp.LNative}
	d.define(ph)
	var cells []*lisp.LVal
	for {
		c, err := d.codeNode(depth+1, false)
		if err != nil {
			return nil, err
		}
		cells = append(cells, c)
		if d.peek() != ',' {
			break
		}
		d.i++
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	if cells[0].Type != lisp.LSExpr {
		return nil, d.errorf("code formals must be a list")
	}
	code, lerr := d.env.NewLambdaCode(cells[0], cells[1:])
	if lerr != nil {
		return nil, d.errorf("closure: %v", (*lisp.ErrorVal)(lerr).ErrorMessage())
	}
	ph.Native = &decCode{code: code, cells: cells} //elpsvet:allow-native the decoder's own placeholder for a code object: it lives only in d.objs and is unwrapped by codeRef, so it never reaches a value LoadDurable returns, let alone a template
	return ph, nil
}

// codeNode reads one code node: a scalar, null, a list, a quote, or a
// literal (["~#lit",NODE] outside another literal), which is sealed whole.
func (d *durableDecoder) codeNode(depth int, inLit bool) (*lisp.LVal, error) {
	switch {
	case bytes.HasPrefix(d.b[d.i:], []byte("null")):
		if err := d.count(); err != nil {
			return nil, err
		}
		d.i += 4
		return lisp.SExpr(nil), nil
	case bytes.HasPrefix(d.b[d.i:], []byte(`["`+tagLit+`",`)):
		if inLit {
			return nil, d.errorf("a literal inside a literal in code")
		}
		if err := d.count(); err != nil {
			return nil, err
		}
		if err := d.depth(depth); err != nil {
			return nil, err
		}
		d.i += len(tagLit) + 4
		inner, err := d.codeNode(depth+1, true)
		if err != nil {
			return nil, err
		}
		if err := d.expect(']'); err != nil {
			return nil, err
		}
		if inner.Type != lisp.LQuote && (inner.Type != lisp.LSExpr || len(inner.Cells) == 0) {
			return nil, d.errorf("a literal in code must be a nonempty list or a quote")
		}
		inner.SealAST()
		return inner, nil
	case bytes.HasPrefix(d.b[d.i:], []byte(`["`+tagQuote+`",`)):
		if err := d.count(); err != nil {
			return nil, err
		}
		if err := d.depth(depth); err != nil {
			return nil, err
		}
		d.i += len(tagQuote) + 4
		inner, err := d.codeNode(depth+1, inLit)
		if err != nil {
			return nil, err
		}
		if err := d.expect(']'); err != nil {
			return nil, err
		}
		return lisp.Quote(inner), nil
	case bytes.HasPrefix(d.b[d.i:], []byte(`["`+tagList+`",[`)):
		if err := d.count(); err != nil {
			return nil, err
		}
		if err := d.depth(depth); err != nil {
			return nil, err
		}
		d.i += len(tagList) + 5
		var cells []*lisp.LVal
		for {
			c, err := d.codeNode(depth+1, inLit)
			if err != nil {
				return nil, err
			}
			cells = append(cells, c)
			if d.peek() != ',' {
				break
			}
			d.i++
		}
		if err := d.expect(']'); err != nil {
			return nil, err
		}
		if err := d.expect(']'); err != nil {
			return nil, err
		}
		return lisp.SExpr(cells), nil
	case d.peek() == '[' || d.peek() == '{':
		return nil, d.errorf("code may hold only scalars, lists, quotes and literals")
	}
	v, err := d.typedDecoder.value(depth)
	if err != nil {
		return nil, err
	}
	switch v.Type {
	case lisp.LInt, lisp.LFloat, lisp.LString, lisp.LSymbol:
		return v, nil
	case lisp.LSExpr, lisp.LArray, lisp.LSortMap, lisp.LBytes, lisp.LTaggedVal, lisp.LNative, lisp.LFun, lisp.LError, lisp.LQuote,
		lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand, lisp.LInvalid, lisp.LTypeMax:
	}
	return nil, d.errorf("code may hold only scalars, lists, quotes and literals")
}

// checkClosures checks, once the document is read, that each frame holds
// exactly what DumpDurable saves: the names its closures read (see
// durable_scope.go), or every binding when a dynamic closure captured it,
// and nothing else.  A frame no closure reads from is rejected too.  It
// decides without evaluating anything, with the same resolution the dump
// uses, against the loading environment.
func (d *durableDecoder) checkClosures() error {
	if len(d.closures) == 0 {
		return nil
	}
	sc := newClosureScope(d.env, d.cfg)
	read := map[*lisp.LEnv]map[string]bool{}
	whole := map[*lisp.LEnv]bool{}
	for _, c := range d.closures {
		var chain []*lisp.LEnv
		for env := c.env; env != nil && env.Parent() != nil; env = env.Parent() {
			if err := sc.step(); err != nil {
				return err
			}
			chain = append(chain, env)
		}
		key := codeKey{formals: c.code.cells[0]}
		reason, err := sc.dynamicReason(c.code.cells, chain, c.pkg)
		if err != nil {
			return err
		}
		if reason != "" {
			for _, env := range chain {
				whole[env] = true
			}
			continue
		}
		for _, name := range sc.codeNames(key, c.code.cells) {
			for _, env := range chain {
				if err := sc.step(); err != nil {
					return err
				}
				if _, ok := funraw.Lookup(env, name); ok {
					if read[env] == nil {
						read[env] = map[string]bool{}
					}
					read[env][name] = true
					break
				}
			}
		}
	}
	for _, fr := range d.restoredFrames {
		if whole[fr.env] {
			continue
		}
		for _, name := range fr.names {
			if !read[fr.env][name] {
				return fmt.Errorf("durable json: captured variable %q is saved but no closure reads it", name)
			}
		}
	}
	return nil
}
