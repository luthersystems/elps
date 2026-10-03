// Copyright © 2026 The ELPS authors

package libjson

// Durable typed JSON (luthersystems/substrate#683).
//
// DumpDurable writes a value graph as typed JSON plus four extension tags,
// so that LoadDurable restores it with the same sharing: an object reached
// twice is written once and referenced after, a value that contains itself
// round-trips, native values go through registered codecs, and a function
// bound to a global is written by name.  The default typed JSON (DumpTyped,
// LoadTyped and the json: builtins) is not changed.
// docs/internals/durable-json.md specifies the format; the golden corpus in
// typedgolden/testdata/durable.txt freezes it.

import (
	"errors"
	"fmt"
	"reflect"
	"strconv"
	"unicode/utf8"

	"github.com/luthersystems/elps/lisp"
)

// DurableFormatVersion is the format version every durable document starts
// with.  It is frozen: LoadDurable rejects any other version.
const DurableFormatVersion = 1

// The durable extension tags.  Every typed JSON tag keeps its meaning.
const (
	tagDurable = "~#durable"
	tagObj     = "~#obj"
	tagRef     = "~#ref"
	tagNative  = "~#native"
	tagFn      = "~#fn"
)

// durablePrefix opens every document of DurableFormatVersion.
const durablePrefix = `["` + tagDurable + `",[1,`

// Identity keys.  Each kind of object has its own key type, so that an
// array's data list header can never be taken for a list header, and a
// native's pointer payload never for an LVal or a map.
type (
	listKey         struct{ p *lisp.LVal }
	arrayKey        struct{ p *lisp.LVal }
	taggedKey       struct{ p *lisp.LVal }
	nativeHeaderKey struct{ p *lisp.LVal }
	nativeKey       struct{ p any }
)

// durableIdentity returns the identity of an object (a value that can be
// shared), and false for any other value.  See the identity table in
// docs/internals/durable-json.md.
func durableIdentity(v *lisp.LVal) (any, bool) {
	switch v.Type {
	case lisp.LSExpr:
		if len(v.Cells) == 0 {
			return nil, false
		}
		return listKey{v}, true
	case lisp.LArray:
		if len(v.Cells) == 2 && v.Cells[1] != nil {
			return arrayKey{v.Cells[1]}, true
		}
		return arrayKey{v}, true
	case lisp.LSortMap:
		if m := v.Map(); m != nil {
			return m, true
		}
		return nil, false
	case lisp.LBytes:
		if p, ok := v.Native.(*[]byte); ok {
			return p, true
		}
		return nil, false
	case lisp.LTaggedVal:
		return taggedKey{v}, true
	case lisp.LNative:
		if v.Native != nil && reflect.TypeOf(v.Native).Kind() == reflect.Pointer {
			return nativeKey{v.Native}, true
		}
		return nativeHeaderKey{v}, true
	case lisp.LInt, lisp.LFloat, lisp.LString, lisp.LSymbol, lisp.LFun, lisp.LError, lisp.LQuote,
		lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand, lisp.LInvalid, lisp.LTypeMax:
	}
	return nil, false
}

// DumpDurable writes v as a durable typed JSON document:
// ["~#durable",[1,VALUE]].  VALUE is v's typed JSON with four extension
// tags.  An object (a nonempty list, a vector or array, a sorted map, a
// tagged value, bytes or a native) that is reached more than once is
// written once as ["~#obj",[ID,X]] and then as ["~#ref",ID], so LoadDurable
// restores the sharing and any cycle.  An object reached once is written
// exactly as DumpTyped writes it.  The same value graph always gives the
// same bytes.
//
// env resolves function names and is passed to the native codecs.  A
// native value is written through the codec reg holds for its Go type; reg
// may be nil when v holds no natives.  A function is written as
// ["~#fn","PKG:NAME"] when it is the current value of that global.
//
// Refused with an error: errors (condition values), anonymous and local
// functions, macros and special operators, natives with no codec, a native
// whose payload reaches the native or an object that encloses it, and every
// value DumpTyped refuses for a reason other than sharing.
//
// opts are typed JSON's limits and charge; see
// docs/internals/durable-json.md for what each one counts.  The byte and
// value limits never exceed env's per-operation allocation cap.  reg must be
// frozen.
func DumpDurable(env *lisp.LEnv, v *lisp.LVal, reg *DurableRegistry, opts ...TypedOption) ([]byte, error) {
	if env == nil {
		return nil, errors.New("durable json: DumpDurable needs an environment")
	}
	if err := reg.checkUsable(); err != nil {
		return nil, err
	}
	e := durableEncoder{
		typedEncoder: typedEncoder{cfg: durableConfig(env, opts)},
		env:          env,
		reg:          reg,
		refs:         map[any]int{},
		open:         map[any]int{},
		ids:          map[any]int{},
		saved:        map[any]savedNative{},
		dims:         map[*lisp.LVal]*lisp.LVal{},
		funs:         map[string]string{},
	}
	if err := e.scan(v, 0); err != nil {
		return nil, err
	}
	e.buf = make([]byte, 0, 256)
	e.buf = append(e.buf, durablePrefix...)
	if err := e.value(v, 0); err != nil {
		return nil, err
	}
	e.buf = append(e.buf, ']', ']')
	if err := e.grow(); err != nil {
		return nil, err
	}
	return e.buf, nil
}

// durableConfig is the configuration of one dump or load: opts, with the
// byte and value limits lowered to env's per-operation allocation cap.
func durableConfig(env *lisp.LEnv, opts []TypedOption) typedConfig {
	c := newTypedConfig(opts)
	limit := env.Runtime.MaxAllocBytes()
	c.maxBytes = min(c.maxBytes, limit)
	c.maxValues = min(c.maxValues, limit)
	return c
}

// chargeNative takes a codec's declared charge before a call.
func (c *typedConfig) chargeNative(e *nativeEntry) error {
	if e.charge == 0 || c.charge == nil {
		return nil
	}
	if err := c.charge(e.charge); err != nil {
		return fmt.Errorf("durable json: native %q: %w", e.name, err)
	}
	return nil
}

type durableEncoder struct {
	env *lisp.LEnv
	reg *DurableRegistry
	// refs counts the references to each object; scan fills it.
	refs map[any]int
	// open holds the objects on scan's current path, each with the native
	// depth at which scan entered it.
	open map[any]int
	// ids holds the id of each shared object value has written.
	ids map[any]int
	// saved holds each native's codec and payload; scan fills it.
	saved map[any]savedNative
	// dims holds the dims header of each array data list scan has seen.
	dims map[*lisp.LVal]*lisp.LVal
	// funs maps a function's FID to its "PKG:NAME".
	funs map[string]string
	// nativeNames is the stack of natives whose payloads scan is inside.
	nativeNames []string
	typedEncoder
	scanned int
	nextID  int
}

type savedNative struct {
	entry   *nativeEntry
	payload *lisp.LVal
}

func (e *durableEncoder) count() error {
	e.values++
	if e.values > e.cfg.maxValues {
		return fmt.Errorf("%w: more than %d values", ErrTypedLimit, e.cfg.maxValues)
	}
	return nil
}

// depthError reports a container at depth past the nesting limit.
func (c *typedConfig) depthError(depth int) error {
	if depth >= c.maxDepth {
		return fmt.Errorf("%w: nesting depth exceeds %d", ErrTypedLimit, c.maxDepth)
	}
	return nil
}

// scan is the first pass.  It counts the references to every object in the
// order value writes them, so value knows which objects to share.  It calls
// each native's codec once and resolves each function's name, so every
// refusal is reported before any output is written.  Traversal contract:
// depth-first; a list's cells, an array's cells, a map's values in member
// order, a tagged value's data and a native's payload, each object walked
// once; bounded by the configured depth and value limits.
func (e *durableEncoder) scan(v *lisp.LVal, depth int) error {
	if v == nil {
		return errors.New("durable json: cannot encode a Go nil value")
	}
	e.scanned++
	if e.scanned > e.cfg.maxValues {
		return fmt.Errorf("%w: more than %d values", ErrTypedLimit, e.cfg.maxValues)
	}
	key, shareable := durableIdentity(v)
	if shareable {
		e.refs[key]++
		if e.refs[key] > 1 {
			if nd, onPath := e.open[key]; onPath && nd < len(e.nativeNames) {
				return fmt.Errorf("durable json: native %q payload refers to a value that encloses the native",
					e.nativeNames[len(e.nativeNames)-1])
			}
			if v.Type == lisp.LArray && len(v.Cells) == 2 && e.dims[v.Cells[1]] != v.Cells[0] {
				return errors.New("durable json: two arrays share data with different dimensions")
			}
			return nil
		}
	}
	switch v.Type {
	case lisp.LInt, lisp.LFloat, lisp.LString, lisp.LBytes, lisp.LSymbol:
		return nil
	case lisp.LSExpr:
		if len(v.Cells) == 0 {
			return nil
		}
		if err := e.cfg.depthError(depth); err != nil {
			return err
		}
		e.open[key] = len(e.nativeNames)
		for _, c := range v.Cells {
			if err := e.scan(c, depth+1); err != nil {
				return err
			}
		}
	case lisp.LArray:
		_, cells, err := checkArray(v)
		if err != nil {
			return err
		}
		if err := e.cfg.depthError(depth); err != nil {
			return err
		}
		e.dims[v.Cells[1]] = v.Cells[0]
		e.open[key] = len(e.nativeNames)
		for _, c := range cells {
			if err := e.scan(c, depth+1); err != nil {
				return err
			}
		}
	case lisp.LSortMap:
		if err := e.cfg.depthError(depth); err != nil {
			return err
		}
		e.open[key] = len(e.nativeNames)
		pbase, keysMark, err := e.mapMembers(v)
		if err != nil {
			return err
		}
		// A nested map pushes members past these and releases them, so the
		// loop reads e.pairs by index.
		for i := pbase; i < len(e.pairs); i++ {
			if err := e.scan(e.pairs[i].val, depth+1); err != nil {
				return err
			}
		}
		e.releaseMembers(pbase, keysMark)
	case lisp.LTaggedVal:
		if err := checkTagged(v); err != nil {
			return err
		}
		if err := e.cfg.depthError(depth); err != nil {
			return err
		}
		e.open[key] = len(e.nativeNames)
		if err := e.scan(v.Cells[0], depth+1); err != nil {
			return err
		}
	case lisp.LNative:
		return e.scanNative(v, key, depth)
	case lisp.LFun:
		return e.scanFun(v)
	case lisp.LError:
		return errors.New("durable json: cannot encode an error")
	case lisp.LQuote:
		return errors.New("typed json: cannot encode a nested quote")
	case lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand, lisp.LInvalid, lisp.LTypeMax:
		return fmt.Errorf("durable json: cannot encode a %v", v.Type)
	}
	delete(e.open, key)
	return nil
}

// scanNative saves a native through its codec and scans the payload.
func (e *durableEncoder) scanNative(v *lisp.LVal, key any, depth int) error {
	entry := e.reg.entryFor(v.Native)
	if entry == nil {
		return fmt.Errorf("durable json: no codec registered for native type %T", v.Native)
	}
	if err := e.cfg.depthError(depth); err != nil {
		return err
	}
	if err := e.cfg.chargeNative(entry); err != nil {
		return err
	}
	payload, err := entry.codec.SaveNative(e.env, v)
	switch {
	case err != nil:
		return fmt.Errorf("durable json: native %q: %w", entry.name, err)
	case payload == nil:
		return fmt.Errorf("durable json: native %q: SaveNative returned no payload", entry.name)
	case payload.Type == lisp.LError:
		return fmt.Errorf("durable json: native %q: %v", entry.name, payload)
	}
	e.saved[key] = savedNative{entry: entry, payload: payload}
	e.open[key] = len(e.nativeNames)
	e.nativeNames = append(e.nativeNames, entry.name)
	if err := e.scan(payload, depth+1); err != nil {
		return err
	}
	e.nativeNames = e.nativeNames[:len(e.nativeNames)-1]
	delete(e.open, key)
	return nil
}

// scanFun resolves and caches a function's global name.
func (e *durableEncoder) scanFun(f *lisp.LVal) error {
	if _, ok := e.funs[f.FID()]; ok {
		return nil
	}
	name, err := durableFunName(e.env, f)
	if err != nil {
		return err
	}
	e.funs[f.FID()] = name
	return nil
}

// durableFunName returns "PKG:NAME" for a function that is the current value
// of that global, and an error for any other function.
func durableFunName(env *lisp.LEnv, f *lisp.LVal) (string, error) {
	if f.IsSpecialFun() {
		return "", errors.New("durable json: cannot encode a macro or special operator")
	}
	fid, pkgName := f.FID(), f.Package()
	anonymous := errors.New("durable json: cannot encode an anonymous function")
	if fid == "" || pkgName == "" {
		return "", anonymous
	}
	pkg := env.Runtime.Registry.Package(pkgName)
	if pkg == nil {
		return "", anonymous
	}
	name := pkg.GetFunName(fid)
	if name == "" {
		return "", anonymous
	}
	if !utf8.ValidString(pkgName) || !utf8.ValidString(name) {
		return "", errors.New("durable json: cannot encode a function name that is not valid UTF-8")
	}
	if g := pkg.Get(lisp.Symbol(name)); g.Type != lisp.LFun || g.FID() != fid {
		return "", fmt.Errorf("durable json: cannot encode function %s:%s: the global holds another value", pkgName, name)
	}
	return pkgName + ":" + name, nil
}

// checkTagged rejects a malformed tagged value, as DumpTyped does.
func checkTagged(v *lisp.LVal) error {
	if len(v.Cells) != 1 || v.Cells[0] == nil || v.Str == "" || !utf8.ValidString(v.Str) {
		return errors.New("typed json: malformed tagged value")
	}
	return nil
}

// value is the second pass: it writes v, sharing every object scan counted
// more than once.  Traversal contract: the order scan walks; a shared
// object is written in full at its first occurrence and as a reference
// after; bounded by the configured depth, value and byte limits.
func (e *durableEncoder) value(v *lisp.LVal, depth int) error {
	if v == nil {
		return errors.New("durable json: cannot encode a Go nil value")
	}
	key, shareable := durableIdentity(v)
	if !shareable || e.refs[key] < 2 {
		return e.body(v, key, depth)
	}
	if err := e.count(); err != nil {
		return err
	}
	if id, ok := e.ids[key]; ok {
		e.buf = append(e.buf, `["`+tagRef+`",`...)
		e.buf = strconv.AppendInt(e.buf, int64(id), 10)
		e.buf = append(e.buf, ']')
		return e.grow()
	}
	id := e.nextID
	e.nextID++
	e.ids[key] = id
	e.buf = append(e.buf, `["`+tagObj+`",[`...)
	e.buf = strconv.AppendInt(e.buf, int64(id), 10)
	e.buf = append(e.buf, ',')
	if err := e.body(v, key, depth); err != nil {
		return err
	}
	e.buf = append(e.buf, ']', ']')
	return e.grow()
}

// body writes v without an object wrapper.  Leaves are written by the
// typed encoder, so their bytes are typed JSON's.
func (e *durableEncoder) body(v *lisp.LVal, key any, depth int) error {
	switch v.Type {
	case lisp.LInt, lisp.LFloat, lisp.LString, lisp.LBytes, lisp.LSymbol:
		return e.typedEncoder.value(v, depth)
	case lisp.LSExpr:
		if len(v.Cells) == 0 {
			return e.typedEncoder.value(v, depth)
		}
		if err := e.container(depth); err != nil {
			return err
		}
		e.buf = append(e.buf, `["`+tagList+`",`...)
		if err := e.cells(v.Cells, depth); err != nil {
			return err
		}
		e.buf = append(e.buf, ']')
	case lisp.LArray:
		dims, cells, err := checkArray(v)
		if err != nil {
			return err
		}
		if err := e.container(depth); err != nil {
			return err
		}
		if len(dims) == 1 {
			return e.cells(cells, depth)
		}
		if err := e.arrayDims(dims); err != nil {
			return err
		}
		if err := e.cells(cells, depth); err != nil {
			return err
		}
		e.buf = append(e.buf, ']', ']')
	case lisp.LSortMap:
		return e.sortedMap(v, depth)
	case lisp.LTaggedVal:
		if err := checkTagged(v); err != nil {
			return err
		}
		if err := e.container(depth); err != nil {
			return err
		}
		if err := e.reserve(len(v.Str) + len(tagTagged) + 8); err != nil {
			return err
		}
		e.buf = append(e.buf, `["`+tagTagged+`",[`...)
		e.buf = appendJSONString(e.buf, v.Str)
		e.buf = append(e.buf, ',')
		if err := e.value(v.Cells[0], depth+1); err != nil {
			return err
		}
		e.buf = append(e.buf, ']', ']')
	case lisp.LNative:
		s, ok := e.saved[key]
		if !ok {
			return fmt.Errorf("durable json: native %T was not saved", v.Native)
		}
		if err := e.container(depth); err != nil {
			return err
		}
		if err := e.reserve(len(s.entry.name) + len(tagNative) + 8); err != nil {
			return err
		}
		e.buf = append(e.buf, `["`+tagNative+`",[`...)
		e.buf = appendJSONString(e.buf, s.entry.name)
		e.buf = append(e.buf, ',')
		e.buf = strconv.AppendInt(e.buf, int64(s.entry.version), 10)
		e.buf = append(e.buf, ',')
		if err := e.value(s.payload, depth+1); err != nil {
			return err
		}
		e.buf = append(e.buf, ']', ']')
	case lisp.LFun:
		name, ok := e.funs[v.FID()]
		if !ok {
			return errors.New("durable json: function was not resolved")
		}
		if err := e.count(); err != nil {
			return err
		}
		if err := e.reserve(len(name) + len(tagFn) + 6); err != nil {
			return err
		}
		e.buf = append(e.buf, `["`+tagFn+`",`...)
		e.buf = appendJSONString(e.buf, name)
		e.buf = append(e.buf, ']')
	case lisp.LError:
		return errors.New("durable json: cannot encode an error")
	case lisp.LQuote:
		return errors.New("typed json: cannot encode a nested quote")
	case lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand, lisp.LInvalid, lisp.LTypeMax:
		return fmt.Errorf("durable json: cannot encode a %v", v.Type)
	}
	return e.grow()
}

// container counts a container and checks its depth.
func (e *durableEncoder) container(depth int) error {
	if err := e.count(); err != nil {
		return err
	}
	return e.cfg.depthError(depth)
}

// cells writes a JSON array of values.
func (e *durableEncoder) cells(cells []*lisp.LVal, depth int) error {
	e.buf = append(e.buf, '[')
	for i, c := range cells {
		if i > 0 {
			e.buf = append(e.buf, ',')
		}
		if err := e.value(c, depth+1); err != nil {
			return err
		}
	}
	e.buf = append(e.buf, ']')
	return e.grow()
}

// sortedMap writes a map as typed JSON writes it, with durable values.
func (e *durableEncoder) sortedMap(v *lisp.LVal, depth int) error {
	if err := e.container(depth); err != nil {
		return err
	}
	if v.Len() > e.cfg.maxValues-e.values {
		return fmt.Errorf("%w: more than %d values", ErrTypedLimit, e.cfg.maxValues)
	}
	pbase, keysMark, err := e.mapMembers(v)
	if err != nil {
		return err
	}
	e.buf = append(e.buf, '{')
	for i := pbase; i < len(e.pairs); i++ {
		if i > pbase {
			e.buf = append(e.buf, ',')
		}
		if err := e.count(); err != nil {
			return err
		}
		p := e.pairs[i]
		e.buf = appendJSONString(e.buf, e.keys[p.ks:p.ke])
		e.buf = append(e.buf, ':')
		if err := e.value(p.val, depth+1); err != nil {
			return err
		}
	}
	e.buf = append(e.buf, '}')
	e.releaseMembers(pbase, keysMark)
	return e.grow()
}

// DurableRoot is one named value of a durable document of roots.
type DurableRoot struct {
	Value *lisp.LVal
	Name  string
}

// DumpDurableRoots writes named values as one durable document, so values
// shared between roots stay shared.  The roots are written in the order of
// the slice, and that order decides the object ids, so a caller that wants
// one encoding per set of names passes them in a fixed order (sorted, for
// example).  The value is the list of alternating names and values,
// ("name1" value1 "name2" value2 ...), or () for no roots.  Names must be
// nonempty, valid UTF-8 and distinct.  See DumpDurable for the rest.
func DumpDurableRoots(env *lisp.LEnv, roots []DurableRoot, reg *DurableRegistry, opts ...TypedOption) ([]byte, error) {
	cells := make([]*lisp.LVal, 0, 2*len(roots))
	seen := make(map[string]struct{}, len(roots))
	for _, r := range roots {
		if r.Name == "" || !utf8.ValidString(r.Name) {
			return nil, errors.New("durable json: a root name must be a nonempty UTF-8 string")
		}
		if _, dup := seen[r.Name]; dup {
			return nil, fmt.Errorf("durable json: root %q appears twice", r.Name)
		}
		seen[r.Name] = struct{}{}
		cells = append(cells, lisp.String(r.Name), r.Value)
	}
	return DumpDurable(env, lisp.QExpr(cells), reg, opts...)
}
