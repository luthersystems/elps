// Copyright © 2026 The ELPS authors

package libjson

import (
	"bytes"
	"errors"
	"fmt"
	"reflect"
	"slices"
	"strings"

	"github.com/luthersystems/elps/lisp"
)

// LoadDurable decodes a document DumpDurable wrote and rebuilds its value
// graph: each ["~#obj",[ID,X]] is one value, and every ["~#ref",ID] is that
// same value, so sharing and cycles come back as they were saved.  Natives
// are rebuilt by the codecs reg holds (reg may be nil when the document holds
// none), and ["~#fn","PKG:NAME"] is the function env's registry binds to
// that global now, which must be a regular function of package PKG.
//
// LoadDurable accepts only what DumpDurable writes.  It rejects every input
// LoadTyped rejects inside the value, a missing header or another format
// version, an object id out of sequence, an object no reference uses, a
// reference to an object not yet defined or to the object being defined, a
// native payload that reaches an unfinished object (directly or through
// finished ones), sharing inside the payload of a codec registered without
// WithSharedPayload, an unknown native name or version, and a function name
// that does not resolve to a regular function of its package.  A function
// may be named by any of its package's names for it, so such a document can
// re-encode to other bytes.  It never
// panics on malformed input; a native codec is called only with a fully
// restored payload.
//
// Values are freshly allocated, except functions, which are the current
// global bindings, and natives, which are whatever their codecs return.
// Memory is bounded by the input, as for LoadTyped.  The byte and value
// limits never exceed env's per-operation allocation cap.  Unlike
// LoadTyped, LoadDurable calls the charge function: ceil(n/1024) units for
// n input bytes before it decodes, and each codec's declared charge before
// each LoadNative call.  reg must be frozen.
func LoadDurable(env *lisp.LEnv, b []byte, reg *DurableRegistry, opts ...TypedOption) (*lisp.LVal, error) {
	if env == nil {
		return nil, errors.New("durable json: LoadDurable needs an environment")
	}
	if err := reg.checkUsable(); err != nil {
		return nil, err
	}
	d := durableDecoder{
		typedDecoder: typedDecoder{cfg: durableConfig(env, opts), b: b},
		env:          env,
		reg:          reg,
		pending:      -1,
		storage:      map[int]*decStorage{},
		views:        map[*lisp.LVal]*decView{},
		dataUsed:     map[*lisp.LVal]bool{},
	}
	if len(b) > d.cfg.maxBytes {
		return nil, fmt.Errorf("%w: input exceeds %d bytes", ErrTypedLimit, d.cfg.maxBytes)
	}
	if d.cfg.charge != nil {
		if err := d.cfg.charge(startedKiB(len(b))); err != nil {
			return nil, fmt.Errorf("durable json: %w", err)
		}
	}
	if !bytes.HasPrefix(b, []byte(durablePrefix)) {
		if bytes.HasPrefix(b, []byte(`["`+tagDurable+`",[`)) {
			return nil, errors.New("durable json: unsupported format version")
		}
		return nil, errors.New("durable json: not a durable document")
	}
	d.i = len(durablePrefix)
	v, err := d.value(0)
	if err != nil {
		return nil, err
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	if d.i != len(b) {
		return nil, d.errorf("trailing bytes")
	}
	if i := slices.Index(d.used, false); i >= 0 {
		return nil, fmt.Errorf("durable json: object %d is defined but never referenced", i)
	}
	if err := d.checkHolders(); err != nil {
		return nil, err
	}
	return v, nil
}

type durableDecoder struct {
	env *lisp.LEnv
	reg *DurableRegistry
	// storage holds each shared "~#cells" object by id, storages every
	// storage in document order, and views each view's header.  dataUsed
	// marks every header an array uses as its data, true when a vector
	// does.  arrays holds each array whose data is a holder of its own,
	// for the size check at the end.  emptyObjs holds each empty list
	// defined as an object.  See durable_views_decode.go.
	storage   map[int]*decStorage
	storages  []*decStorage
	views     map[*lisp.LVal]*decView
	dataUsed  map[*lisp.LVal]bool
	arrays    []holderArray
	emptyObjs []*lisp.LVal
	// objs holds each defined object.  A native's slot stays nil until its
	// codec returns.
	objs []*lisp.LVal
	// objNode holds each object's node in the cycle check, -1 for
	// storage.  nodeObj holds each node's object id, -1 for a cell of a
	// view.  A node is an object or a storage cell, numbered in the order
	// they open.
	objNode, nodeObj []int
	// open reports whether each node is still being read.
	open []bool
	// low holds, for each finished node, the smallest node that was still
	// open when it finished and that it reaches, or noLow.
	low []int
	// used reports whether a reference names each object.
	used []bool
	// frames holds, for each node and native payload being read, the
	// smallest open node its contents reach so far.
	frames []int
	// storageObj marks objects that are "~#cells" storage, which only a
	// view may refer to.
	storageObj []bool
	typedDecoder
	// pending is the id of the "~#obj" whose value is being read, until
	// that value's header is constructed; -1 when none.
	pending int
	// tree counts the payloads being read of codecs that do not keep
	// sharing; inside one, no "~#obj" or "~#ref" is allowed.
	tree int
	// dataPos marks that the next value is an array's data holder.
	dataPos bool
}

// define gives the pending object id its value.  Every container calls it
// right after it constructs its header and before it reads any child, so a
// reference inside the container resolves to it.
func (d *durableDecoder) define(v *lisp.LVal) {
	if d.pending >= 0 {
		d.objs[d.pending] = v
		d.pending = -1
	}
}

// reach records that the current contents reach object i, or what i
// reaches through finished objects, if that is still open.
func (d *durableDecoder) reach(i int) {
	i = resolveLow(i, d.open, d.low)
	if i != noLow && len(d.frames) > 0 && i < d.frames[len(d.frames)-1] {
		d.frames[len(d.frames)-1] = i
	}
}

func (d *durableDecoder) popFrame() int {
	low := d.frames[len(d.frames)-1]
	d.frames = d.frames[:len(d.frames)-1]
	return low
}

// openNode opens a node of the cycle check for object id, or for a
// storage cell when id is -1.
func (d *durableDecoder) openNode(id int) int {
	n := len(d.open)
	d.open = append(d.open, true)
	d.low = append(d.low, noLow)
	d.nodeObj = append(d.nodeObj, id)
	d.frames = append(d.frames, noLow)
	return n
}

// closeNode finishes node n and passes up the open nodes it reaches.
func (d *durableDecoder) closeNode(n int) {
	low := d.popFrame()
	d.open[n] = false
	if low < n {
		d.low[n] = low
		d.reach(low)
	}
}

func (d *durableDecoder) depth(depth int) error {
	return d.cfg.depthError(depth)
}

// value reads one durable value.
func (d *durableDecoder) value(depth int) (*lisp.LVal, error) {
	switch d.peek() {
	case '[':
		if err := d.count(); err != nil {
			return nil, err
		}
		return d.array(depth)
	case '{':
		if err := d.count(); err != nil {
			return nil, err
		}
		if err := d.depth(depth); err != nil {
			return nil, err
		}
		return d.object(depth)
	}
	v, err := d.typedDecoder.value(depth)
	if err != nil {
		return nil, err
	}
	if v.Type == lisp.LBytes {
		d.define(v)
	}
	return v, nil
}

// array reads a vector or a tagged form, after its '[' was counted.
func (d *durableDecoder) array(depth int) (*lisp.LVal, error) {
	d.i++
	if bytes.HasPrefix(d.b[d.i:], []byte(`"~#`)) {
		return d.tagged(depth)
	}
	if err := d.depth(depth); err != nil {
		return nil, err
	}
	n := lisp.Int(0)
	data := lisp.QExpr(nil)
	v := &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{lisp.QExpr([]*lisp.LVal{n}), data}}
	d.define(v)
	cells, err := d.elements(depth)
	if err != nil {
		return nil, err
	}
	// A vector written inline has no spare capacity (DumpDurable writes
	// one with spare capacity as a view).
	data.Cells = cells[:len(cells):len(cells)]
	n.Int = len(cells)
	return v, nil
}

// elements reads values up to and including the closing ']' and returns
// them in a fresh slice of exact length.
func (d *durableDecoder) elements(depth int) ([]*lisp.LVal, error) {
	base := len(d.stack)
	defer func() {
		clear(d.stack[base:])
		d.stack = d.stack[:base]
	}()
	if d.peek() == ']' {
		d.i++
		return []*lisp.LVal{}, nil
	}
	for {
		v, err := d.value(depth + 1)
		if err != nil {
			return nil, err
		}
		d.stack = append(d.stack, v)
		switch d.peek() {
		case ',':
			d.i++
		case ']':
			d.i++
			return slices.Clone(d.stack[base:]), nil
		default:
			return nil, d.errorf("expected ',' or ']'")
		}
	}
}

// index reads a nonnegative int.
func (d *durableDecoder) index() (int, error) {
	if c := d.peek(); c != '-' && (c < '0' || c > '9') {
		return 0, d.errorf("expected an integer")
	}
	n, err := d.number()
	if err != nil {
		return 0, err
	}
	if n.Type != lisp.LInt || n.Int < 0 {
		return 0, d.errorf("expected a nonnegative integer")
	}
	return n.Int, nil
}

func (d *durableDecoder) tagged(depth int) (*lisp.LVal, error) {
	var tag string
	for _, t := range [...]string{tagList, tagArray, tagTagged, tagObj, tagRef, tagNative, tagFn, tagView} {
		if bytes.HasPrefix(d.b[d.i:], []byte(`"`+t+`",`)) {
			tag = t
			break
		}
	}
	if tag == "" {
		return nil, d.errorf("unknown tag")
	}
	d.i += len(tag) + 3
	dataPos := d.dataPos
	d.dataPos = false
	if dataPos && tag != tagObj && tag != tagRef && tag != tagView {
		return nil, d.errorf("array data in its own form must be a shared object, a reference or a view")
	}
	var v *lisp.LVal
	var err error
	switch tag {
	case tagRef:
		v, err = d.ref()
	case tagFn:
		v, err = d.function()
	case tagObj:
		v, err = d.objectDef(depth)
	case tagView:
		if err = d.depth(depth); err != nil {
			return nil, err
		}
		if err = d.expect('['); err != nil {
			return nil, err
		}
		v, err = d.view(depth)
	case tagList, tagArray, tagTagged, tagNative:
		if err = d.depth(depth); err != nil {
			return nil, err
		}
		if err = d.expect('['); err != nil {
			return nil, err
		}
		switch tag {
		case tagList:
			v, err = d.list(depth)
		case tagArray:
			v, err = d.multiArray(depth)
		case tagTagged:
			v, err = d.taggedValue(depth)
		default:
			v, err = d.native(depth)
		}
	}
	if err != nil {
		return nil, err
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	return v, nil
}

// list reads the cells of a tagged list after their '['.
func (d *durableDecoder) list(depth int) (*lisp.LVal, error) {
	v := lisp.QExpr(nil)
	d.define(v)
	cells, err := d.elements(depth)
	if err != nil {
		return nil, err
	}
	if len(cells) == 0 {
		return nil, d.errorf("empty list must be null")
	}
	// The list may be a vector's data, whose capacity DumpDurable writes:
	// one written as a list has none to spare.
	v.Cells = cells[:len(cells):len(cells)]
	return v, nil
}

// taggedValue reads ["type-name",data] after its '['.
func (d *durableDecoder) taggedValue(depth int) (*lisp.LVal, error) {
	s, err := d.rawString()
	if err != nil {
		return nil, err
	}
	if len(s) == 0 {
		return nil, d.errorf("tagged value with an empty type")
	}
	v := &lisp.LVal{Type: lisp.LTaggedVal, Str: string(s)}
	d.define(v)
	if err = d.expect(','); err != nil {
		return nil, err
	}
	inner, err := d.value(depth + 1)
	if err != nil {
		return nil, err
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	v.Cells = []*lisp.LVal{inner}
	return v, nil
}

// multiArray reads [[dims...],[cells...]] after its '['; rank is not 1.
func (d *durableDecoder) multiArray(depth int) (*lisp.LVal, error) {
	dimList, data := lisp.QExpr(nil), lisp.QExpr(nil)
	v := &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{dimList, data}}
	d.define(v)
	if err := d.expect('['); err != nil {
		return nil, err
	}
	dims, err := d.dims()
	if err != nil {
		return nil, err
	}
	total, zero := 1, false
	for _, n := range dims {
		if n.Type != lisp.LInt || n.Int < 0 {
			return nil, d.errorf("invalid array dimension")
		}
		switch {
		case n.Int == 0:
			zero = true
		case total > len(d.b)/n.Int:
			total = len(d.b) + 1 // more cells than the input can hold
		default:
			total *= n.Int
		}
	}
	if zero {
		total = 0
	}
	if err = d.expect(','); err != nil {
		return nil, err
	}
	if bytes.HasPrefix(d.b[d.i:], []byte(`["~#`)) {
		// Data with an identity of its own: a shared list, a reference to
		// one, or a view.  Any rank.  The holder may still be open (an
		// array inside its own data), so its size is checked at the end.
		h, herr := d.dataHolder(depth, len(dims) == 1)
		if herr != nil {
			return nil, herr
		}
		d.arrays = append(d.arrays, holderArray{data: h, size: total, at: d.i})
		if err = d.expect(']'); err != nil {
			return nil, err
		}
		dimList.Cells = dims
		v.Cells[1] = h
		return v, nil
	}
	if len(dims) == 1 {
		return nil, d.errorf("vector written as a tagged array")
	}
	if err = d.expect('['); err != nil {
		return nil, err
	}
	cells, err := d.elements(depth)
	if err != nil {
		return nil, err
	}
	if len(cells) != total {
		return nil, d.errorf("array contents do not match its dimensions")
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	dimList.Cells = dims
	data.Cells = cells[:len(cells):len(cells)]
	return v, nil
}

// dims reads the dimension list of a tagged array, through its ']'.  Each
// dimension is a counted JSON integer, read on the scalar path, so no tag
// (and no codec) can run in a dimension.
func (d *durableDecoder) dims() ([]*lisp.LVal, error) {
	var dims []*lisp.LVal
	if d.peek() == ']' {
		d.i++
		return []*lisp.LVal{}, nil
	}
	for {
		if err := d.count(); err != nil {
			return nil, err
		}
		var n *lisp.LVal
		switch c := d.peek(); {
		case c == '-' || c >= '0' && c <= '9':
			var err error
			if n, err = d.number(); err != nil {
				return nil, err
			}
		case c == '"':
			// A dimension past 2^53 is a "~n" string.  stringValue reads
			// scalars only; anything but an int is refused below.
			s, err := d.rawString()
			if err != nil {
				return nil, err
			}
			if n, err = d.stringValue(s); err != nil {
				return nil, err
			}
		default:
			return nil, d.errorf("invalid array dimension")
		}
		if n.Type != lisp.LInt {
			return nil, d.errorf("invalid array dimension")
		}
		dims = append(dims, n)
		switch d.peek() {
		case ',':
			d.i++
		case ']':
			d.i++
			return dims, nil
		default:
			return nil, d.errorf("expected ',' or ']'")
		}
	}
}

// objectDef reads [ID,X] after "~#obj",.  ID must be the next id and X a
// value that can be shared.
func (d *durableDecoder) objectDef(depth int) (*lisp.LVal, error) {
	if d.tree > 0 {
		return nil, d.errorf("a payload of a codec that does not keep sharing holds a shared object")
	}
	if d.pending >= 0 {
		return nil, d.errorf("object definition inside another definition")
	}
	if err := d.expect('['); err != nil {
		return nil, err
	}
	id, err := d.index()
	if err != nil {
		return nil, err
	}
	if id != len(d.objs) {
		return nil, d.errorf("object id %d out of sequence", id)
	}
	if err = d.expect(','); err != nil {
		return nil, err
	}
	d.objs = append(d.objs, nil)
	d.used = append(d.used, false)
	d.storageObj = append(d.storageObj, false)
	node := d.openNode(id)
	d.objNode = append(d.objNode, node)
	d.pending = id
	var v *lisp.LVal
	if empty := `["` + tagList + `",[]]`; bytes.HasPrefix(d.b[d.i:], []byte(empty)) {
		// An array's shared data list may be empty; checkHolders checks
		// that an array uses it.
		if err = d.count(); err != nil {
			return nil, err
		}
		d.i += len(empty)
		v = lisp.QExpr([]*lisp.LVal{})
		d.define(v)
		d.emptyObjs = append(d.emptyObjs, v)
	} else if v, err = d.value(depth); err != nil {
		return nil, err
	}
	if d.pending >= 0 {
		return nil, d.errorf("an object must be a list, vector, array, map, tagged value, bytes or native")
	}
	d.closeNode(node)
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	return v, nil
}

// ref reads ID after "~#ref", (the ']' is left for the caller).
func (d *durableDecoder) ref() (*lisp.LVal, error) {
	if d.tree > 0 {
		return nil, d.errorf("a payload of a codec that does not keep sharing holds a reference")
	}
	id, err := d.index()
	if err != nil {
		return nil, err
	}
	switch {
	case id >= len(d.objs):
		return nil, d.errorf("reference to undefined object %d", id)
	case id == d.pending:
		return nil, d.errorf("an object must be a list, vector, array, map, tagged value, bytes or native, not a reference")
	case d.storageObj[id]:
		return nil, d.errorf("reference to storage object %d outside a view", id)
	case d.objs[id] == nil:
		return nil, d.errorf("native payload refers to object %d, which encloses the native", id)
	}
	d.reach(d.objNode[id])
	d.used[id] = true
	return d.objs[id], nil
}

// function reads "PKG:NAME" after "~#fn", and resolves it.
func (d *durableDecoder) function() (*lisp.LVal, error) {
	s, err := d.rawString()
	if err != nil {
		return nil, err
	}
	name := string(s)
	pkgName, sym, ok := strings.Cut(name, ":")
	if !ok || pkgName == "" || sym == "" {
		return nil, d.errorf("invalid function name %q", name)
	}
	pkg := d.env.Runtime.Registry.Package(pkgName)
	if pkg == nil {
		return nil, d.errorf("function %s: unknown package", name)
	}
	f, ok := pkg.Symbol(sym)
	if !ok || f.Type != lisp.LFun || f.IsSpecialFun() {
		return nil, d.errorf("function %s: the global is not a regular function", name)
	}
	if f.Package() != pkgName {
		return nil, d.errorf("function %s: the global holds a function of package %s", name, f.Package())
	}
	return f, nil
}

// native reads ["NAME",VERSION,PAYLOAD] after its '[' and calls the codec.
func (d *durableDecoder) native(depth int) (*lisp.LVal, error) {
	id := d.pending
	d.pending = -1
	s, err := d.rawString()
	if err != nil {
		return nil, err
	}
	name := string(s)
	entry := d.reg.entryNamed(name)
	if entry == nil {
		return nil, d.errorf("no codec registered for native %q", name)
	}
	if err = d.expect(','); err != nil {
		return nil, err
	}
	version, err := d.index()
	if err != nil {
		return nil, err
	}
	if version < 1 || version > entry.version {
		return nil, d.errorf("native %q: unsupported version %d", name, version)
	}
	if err = d.expect(','); err != nil {
		return nil, err
	}
	if !entry.shared {
		d.tree++
	}
	d.frames = append(d.frames, noLow)
	payload, err := d.value(depth + 1)
	if err != nil {
		return nil, err
	}
	if !entry.shared {
		d.tree--
	}
	// The payload must reach no object that is still open: every such
	// object encloses this native, and LoadNative must see a finished
	// payload.
	if low := d.popFrame(); low != noLow {
		if obj := d.nodeObj[low]; obj >= 0 {
			return nil, d.errorf("native %q payload refers to object %d, which encloses the native", name, obj)
		}
		return nil, d.errorf("native %q payload refers to a view cell that encloses the native", name)
	}
	if err = d.expect(']'); err != nil {
		return nil, err
	}
	if err = d.cfg.chargeNative(entry); err != nil {
		return nil, err
	}
	v, err := entry.codec.LoadNative(d.env, version, payload)
	switch {
	case err != nil:
		return nil, fmt.Errorf("durable json: native %q: %w", name, err)
	case v == nil || v.Type != lisp.LNative || reflect.TypeOf(v.Native) != entry.typ:
		return nil, fmt.Errorf("durable json: native %q: LoadNative did not return a native %v", name, entry.typ)
	}
	if id >= 0 {
		d.objs[id] = v
	}
	return v, nil
}

// object reads a JSON object into a sorted map.
func (d *durableDecoder) object(depth int) (*lisp.LVal, error) {
	d.i++
	m := lisp.SortedMap()
	d.define(m)
	if d.peek() == '}' {
		d.i++
		return m, nil
	}
	var prev []byte
	size := 0
	for {
		if err := d.count(); err != nil {
			return nil, err
		}
		s, err := d.rawString()
		if err != nil {
			return nil, err
		}
		if prev != nil && bytes.Compare(prev, s) >= 0 {
			return nil, d.errorf("members out of order or duplicated")
		}
		// s aliases scratch the value below may reuse.
		prev = append(prev[:0], s...)
		k, err := d.key(prev)
		if err != nil {
			return nil, err
		}
		if err = d.expect(':'); err != nil {
			return nil, err
		}
		v, err := d.value(depth + 1)
		if err != nil {
			return nil, err
		}
		if r := m.MapSetLVal(k, v); r.Type == lisp.LError {
			return nil, d.errorf("%s", r.Str)
		}
		size++
		if m.Len() != size {
			// A string and a symbol of one spelling are one key.
			return nil, d.errorf("two members name one key")
		}
		switch d.peek() {
		case ',':
			d.i++
		case '}':
			d.i++
			return m, nil
		default:
			return nil, d.errorf("expected ',' or '}'")
		}
	}
}

// LoadDurableRoots decodes a document DumpDurableRoots wrote and returns its
// roots in the order they were written.  It rejects a document whose value
// is not a list of distinct names and values, and one whose root list is a
// shared object (DumpDurableRoots builds that list fresh, so nothing can
// refer to it).  See LoadDurable for the rest.
func LoadDurableRoots(env *lisp.LEnv, b []byte, reg *DurableRegistry, opts ...TypedOption) ([]DurableRoot, error) {
	if bytes.HasPrefix(b, []byte(durablePrefix+`["`+tagObj+`",`)) {
		return nil, errors.New("durable json: the root list is a shared object")
	}
	v, err := LoadDurable(env, b, reg, opts...)
	if err != nil {
		return nil, err
	}
	if v.Type != lisp.LSExpr || len(v.Cells)%2 != 0 {
		return nil, errors.New("durable json: not a list of named roots")
	}
	roots := make([]DurableRoot, 0, len(v.Cells)/2)
	seen := make(map[string]struct{}, len(v.Cells)/2)
	for i := 0; i < len(v.Cells); i += 2 {
		name := v.Cells[i]
		if name.Type != lisp.LString || name.Str == "" {
			return nil, errors.New("durable json: a root name is not a nonempty string")
		}
		if _, dup := seen[name.Str]; dup {
			return nil, fmt.Errorf("durable json: root %q appears twice", name.Str)
		}
		seen[name.Str] = struct{}{}
		roots = append(roots, DurableRoot{Name: name.Str, Value: v.Cells[i+1]})
	}
	return roots, nil
}
