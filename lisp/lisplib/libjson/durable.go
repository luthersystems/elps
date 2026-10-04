// Copyright © 2026 The ELPS authors

package libjson

// Durable typed JSON (luthersystems/substrate#683).
//
// DumpDurable writes a value graph as typed JSON plus extension tags, so
// that LoadDurable restores it with the same sharing: an object reached
// twice is written once and referenced after, a value that contains itself
// round-trips, native values go through registered codecs, a function
// bound to a global is written by name, lists and arrays over shared
// storage are views of it, program literals stay literals and error values
// keep their condition and data.  The default typed JSON (DumpTyped,
// LoadTyped and the json: builtins) is not changed.
// docs/internals/durable-json.md specifies the format; the golden corpus in
// typedgolden/testdata/durable.txt freezes it.

import (
	"errors"
	"fmt"
	"math"
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
	tagBuiltin = "~#builtin"
)

// durablePrefix opens every document of DurableFormatVersion.
const durablePrefix = `["` + tagDurable + `",[1,`

// Identity keys.  Each kind of object has its own key type, so that an
// array's data list header can never be taken for a list, and a native's
// pointer payload never for an LVal or a map.  Keys are compared for
// equality only; no order is ever taken from them.
type (
	// listKey is a nonempty list's cells: their first element's address and
	// their length.  Headers over the same cells are one list, because a
	// write through one (stable-sort) is seen through the other.
	listKey struct {
		p **lisp.LVal
		n int
	}
	// arrayKey is an array's dims and data headers.
	arrayKey        struct{ dims, data *lisp.LVal }
	taggedKey       struct{ p *lisp.LVal }
	nativeHeaderKey struct{ p *lisp.LVal }
	nativeKey       struct{ p any }
	// nativeRefKey is a native whose payload is a Go map, channel or
	// unsafe pointer: its type and the address it refers to.
	nativeRefKey struct {
		t reflect.Type
		p uintptr
	}
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
		return listKey{&v.Cells[0], len(v.Cells)}, true
	case lisp.LArray:
		if len(v.Cells) == 2 {
			return arrayKey{v.Cells[0], v.Cells[1]}, true
		}
		return arrayKey{v, nil}, true
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
		if v.Native == nil {
			return nativeHeaderKey{v}, true
		}
		switch t := reflect.TypeOf(v.Native); t.Kind() {
		case reflect.Pointer:
			return nativeKey{v.Native}, true
		case reflect.Map, reflect.Chan, reflect.UnsafePointer:
			// A nil map, channel or pointer has address 0; distinct nil
			// natives are then distinct headers.
			if p := reflect.ValueOf(v.Native).Pointer(); p != 0 {
				return nativeRefKey{t, p}, true
			}
			return nativeHeaderKey{v}, true
		default:
			return nativeHeaderKey{v}, true
		}
	case lisp.LError:
		return errorKey{v}, true
	case lisp.LInt, lisp.LFloat, lisp.LString, lisp.LSymbol, lisp.LFun, lisp.LQuote,
		lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand, lisp.LInvalid, lisp.LTypeMax:
	}
	return nil, false
}

// DumpDurable writes v as a durable typed JSON document:
// ["~#durable",[1,VALUE]].  VALUE is v's typed JSON with extension tags.
// An object (a nonempty list, a vector or array, a sorted map, a tagged
// value, bytes, an error or a native) that is reached more than once is
// written once as ["~#obj",[ID,X]] and then as ["~#ref",ID], so LoadDurable
// restores the sharing and any cycle.  An object reached once is written
// exactly as DumpTyped writes it.  The same value graph always gives the
// same bytes.
//
// env resolves function names and is passed to the native codecs.  A
// native value is written through the codec reg holds for its Go type; reg
// may be nil when v holds no natives.  A registered builtin is written as
// ["~#builtin",["PKG","NAME"]], the package and name env's registry
// registered it under (lisp.PackageRegistry.RegisteredBuiltinName),
// whatever its names bind.  A Lisp function, or a builtin no registration
// names, that a global binds is written as ["~#fn","PKG:NAME"]: PKG is its
// defining package and NAME the first name, in sorted order, under which
// PKG binds it.
//
// Lists and arrays whose cells share storage (a list and its tail, a slice
// of a vector, arrays over one data list) keep that sharing: the storage is
// written once and each value as a view of it (see durable_views.go).
// A program literal (a sealed list) is written as ["~#lit",X] and restores
// as a literal that the mutators refuse.
//
// An error value is written as ["~#error",["CONDITION",[DATA...]]] and
// restores as an error with the same condition and data, without its call
// stack or source location (see durable_errors.go).
//
// A lambda no global binds (a closure) is written with its code and the
// frames it captured (see durable_closures.go).
//
// Refused with an error: internal panics, errors whose condition is empty
// or not UTF-8, builtins no global binds, macros and special operators
// (also when a closure captured one), natives with no codec, a native whose payload reaches the
// native or an object that encloses it (directly or through finished
// objects), a shared value in the payload of a codec registered without
// WithSharedPayload, and every value DumpTyped refuses for a reason other
// than sharing.
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
	return newDurableEncoder(env, reg, durableConfig(env, opts)).dump(v)
}

// dump writes the document for v.
func (e *durableEncoder) dump(v *lisp.LVal) ([]byte, error) {
	// Discovery finds every holder of cells, so storage shared by several
	// can be grouped before the counting pass walks it once.  It also
	// saves every native and names every function, once.
	e.discover, e.scanning = true, true
	if err := e.scan(v, 0); err != nil {
		return nil, err
	}
	if err := e.groupHolders(); err != nil {
		return nil, err
	}
	if err := e.checkCodeSharing(); err != nil {
		return nil, err
	}
	e.resetScan()
	if err := e.scan(v, 0); err != nil {
		return nil, err
	}
	e.scanning = false
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
	// order is the first-visit index of each object scan has seen.
	order map[any]int
	// tree holds the objects scan first visited inside the payload of a
	// codec that does not keep sharing, with that codec's name.
	tree map[any]string
	// ids holds the id of each shared object value has written.
	ids map[any]int
	// saved holds each native's codec and payload; scan fills it.
	saved map[any]savedNative
	// funs maps a Lisp function's package and FID to its "PKG:NAME".
	funs map[funKey]string
	// builtinNames maps an unregistered builtin's function data to its
	// "PKG:NAME".  A builtin is named by identity, never by FID: two
	// builtins of one package can share an FID.
	builtinNames map[any]string
	// funIndex holds, per package, the FID to first-name index read once
	// per dump.
	funIndex map[string]map[string]string
	// dataHeaders marks every array data header discovery found, and
	// appendable those a vector uses.  recorded marks the holder headers
	// in headers.  views places each grouped holder.  See
	// durable_views.go.
	dataHeaders map[*lisp.LVal]bool
	appendable  map[*lisp.LVal]bool
	recorded    map[*lisp.LVal]bool
	views       map[any]viewInfo
	// literal holds the holders whose headers are program literals.
	literal map[any]bool
	// walked links each cell address discovery walked to the next
	// address; see unwalked.
	walked map[uintptr]uintptr
	// frameBindings holds each captured frame's bindings in name order,
	// and frameReserved how many bindings the dump has reserved against
	// the value limit; codeRanges the cells of mutable code lists.  See
	// durable_closures.go.
	frameBindings map[*lisp.LEnv][]binding
	// nearestFrame memoizes frameOf; ancestorVisits and codeVisits count
	// environments walked and code nodes scanned, for the bound and the
	// tests.
	nearestFrame map[*lisp.LEnv]*lisp.LEnv
	// low holds, by first-visit index, for each object scan has finished,
	// the smallest first-visit index of an object that was still open when
	// it finished and that it reaches, or noLow.  An open object is one scan
	// is inside.  resolve follows these links through finished objects.
	low []int
	// isOpen reports, by first-visit index, whether scan is inside that
	// object.
	isOpen []bool
	// frames holds, for each open object, the smallest first-visit index
	// of an open object its contents reach so far (noLow if none).
	frames []int
	// treeNames is the stack of no-sharing codecs whose payloads scan is
	// inside.
	treeNames []string
	// headers holds the holder headers discovery found, in walk order,
	// and storages each group's storage.
	headers    []*lisp.LVal
	storages   []storageInfo
	codeRanges []span
	typedEncoder
	ancestorVisits int
	codeVisits     int
	frameReserved  int
	scanned        int
	nextID         int
	// discover marks the first walk, which only finds holders, saves
	// natives and names functions.
	discover bool
}

type funKey struct{ pkg, fid string }

// noLow marks a frame or object that reaches no open object.
const noLow = math.MaxInt

func newDurableEncoder(env *lisp.LEnv, reg *DurableRegistry, cfg typedConfig) *durableEncoder {
	return &durableEncoder{
		typedEncoder:  typedEncoder{cfg: cfg, durable: true},
		env:           env,
		reg:           reg,
		refs:          map[any]int{},
		order:         map[any]int{},
		tree:          map[any]string{},
		ids:           map[any]int{},
		saved:         map[any]savedNative{},
		funs:          map[funKey]string{},
		builtinNames:  map[any]string{},
		dataHeaders:   map[*lisp.LVal]bool{},
		appendable:    map[*lisp.LVal]bool{},
		recorded:      map[*lisp.LVal]bool{},
		walked:        map[uintptr]uintptr{},
		frameBindings: map[*lisp.LEnv][]binding{},
		nearestFrame:  map[*lisp.LEnv]*lisp.LEnv{},

		views:   map[any]viewInfo{},
		literal: map[any]bool{},
	}
}

// resetScan clears the counting state discovery left, keeping its saved
// natives, function names and holder groups.
func (e *durableEncoder) resetScan() {
	e.discover = false
	clear(e.refs)
	clear(e.order)
	clear(e.tree)
	e.low, e.isOpen, e.frames, e.treeNames = nil, nil, nil, nil
	e.scanned, e.scanKeyBytes = 0, 0
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

func (e *durableEncoder) countScan(n int) error {
	e.scanned += n
	if e.scanned > e.cfg.maxValues {
		return fmt.Errorf("%w: more than %d values", ErrTypedLimit, e.cfg.maxValues)
	}
	return nil
}

// discoveryMaxDepth bounds discovery's recursion when the nesting limit is
// lower.  Discovery meets a container by a different path than the counting
// pass and the output (it walks holders' lengths, before their storage is
// grouped), so it may meet it deeper; the nesting limit is applied by the
// counting pass, which walks exactly what is written.  This bound only
// keeps discovery's recursion finite.
const discoveryMaxDepth = 1 << 16

// scanDepth checks a container's depth during a scan: against the nesting
// limit in the counting pass, against discoveryMaxDepth (or the limit, if
// higher) in discovery.
func (e *durableEncoder) scanDepth(depth int) error {
	if e.discover && depth < max(discoveryMaxDepth, e.cfg.maxDepth) {
		return nil
	}
	return e.cfg.depthError(depth)
}

// depthError reports a container at depth past the nesting limit.
func (c *typedConfig) depthError(depth int) error {
	if depth >= c.maxDepth {
		return fmt.Errorf("%w: nesting depth exceeds %d", ErrTypedLimit, c.maxDepth)
	}
	return nil
}

// resolveLow follows low-links from object i through finished objects to
// the outermost open object i reaches, or noLow.  Links only point to
// objects visited earlier, so the walk ends.  It then points every finished
// object on the way at the result (path compression), which stays correct:
// if that object later finishes, the walk continues through its own link.
func resolveLow(i int, open []bool, low []int) int {
	r := i
	for r != noLow && !open[r] {
		r = low[r]
	}
	for i != noLow && !open[i] && low[i] != r {
		i, low[i] = low[i], r
	}
	return r
}

// reach records that the current contents reach object i, or what i
// reaches through finished objects, if that is still open.
func (e *durableEncoder) reach(i int) {
	i = resolveLow(i, e.isOpen, e.low)
	if i != noLow && len(e.frames) > 0 && i < e.frames[len(e.frames)-1] {
		e.frames[len(e.frames)-1] = i
	}
}

// openNode opens a node of the cycle check: an object, or a cell of view
// storage.
func (e *durableEncoder) openNode() int {
	i := len(e.isOpen)
	e.isOpen = append(e.isOpen, true)
	e.low = append(e.low, noLow)
	e.frames = append(e.frames, noLow)
	return i
}

// openObject marks a first-visited object open.
func (e *durableEncoder) openObject(key any) int {
	i := e.openNode()
	e.order[key] = i
	if len(e.treeNames) > 0 {
		e.tree[key] = e.treeNames[len(e.treeNames)-1]
	}
	return i
}

// closeNode finishes a node: it records the open nodes the node reaches,
// other than itself and its contents, and passes them up.
func (e *durableEncoder) closeNode(i int) int {
	low := e.frames[len(e.frames)-1]
	e.frames = e.frames[:len(e.frames)-1]
	e.isOpen[i] = false
	if low >= i {
		low = noLow
	}
	e.low[i] = low
	e.reach(low)
	return low
}

// scan is the first pass.  It counts the references to every object in the
// order value writes them, so value knows which objects to share.  It calls
// each native's codec once and resolves each function's name, so every
// refusal is reported before any output is written.  Traversal contract:
// depth-first; a list's cells, an array's cells, a map's values in member
// order, a tagged value's data and a native's payload, each object walked
// once; bounded by the configured depth and value limits, counting map keys
// and array dimensions, before anything is copied.
func (e *durableEncoder) scan(v *lisp.LVal, depth int) error {
	if v == nil {
		return errors.New("durable json: cannot encode a Go nil value")
	}
	if err := e.countScan(1); err != nil {
		return err
	}
	if e.discover && v.Type == lisp.LSExpr && len(v.Cells) > 0 {
		e.recordHeader(v)
	}
	key, shareable := e.identity(v)
	if shareable {
		e.refs[key]++
		if e.refs[key] > 1 {
			return e.revisit(v, key)
		}
	}
	switch v.Type {
	case lisp.LInt, lisp.LFloat, lisp.LString, lisp.LSymbol:
		return nil
	case lisp.LBytes:
		e.openObject(key)
		e.closeNode(e.order[key])
		return nil
	case lisp.LSExpr:
		if !shareable {
			return nil
		}
		if err := e.scanDepth(depth); err != nil {
			return err
		}
		e.noteLiteral(key, v)
		return e.scanHolder(key, v.Cells, depth)
	case lisp.LArray:
		dims, cells, err := checkArray(v)
		if err != nil {
			return err
		}
		if err := e.scanDepth(depth); err != nil {
			return err
		}
		if len(dims) != 1 {
			if err := e.countScan(len(dims)); err != nil {
				return err
			}
		}
		_ = cells
		if e.discover {
			e.recordData(v.Cells[1], len(dims) == 1)
		}
		i := e.openObject(key)
		if err := e.scanData(v.Cells[1], depth); err != nil {
			return err
		}
		e.closeNode(i)
	case lisp.LSortMap:
		if err := e.scanDepth(depth); err != nil {
			return err
		}
		// Keys count as values.  Check them before the members are copied.
		if err := e.countScan(v.Len()); err != nil {
			return err
		}
		// Each member writes at least four bytes ("":0 and a comma or
		// brace), so the member scratch is bounded before it is collected.
		if e.scanKeyBytes+4*v.Len() > e.cfg.maxBytes {
			return fmt.Errorf("%w: encoding exceeds %d bytes", ErrTypedLimit, e.cfg.maxBytes)
		}
		i := e.openObject(key)
		pbase, keysMark, err := e.mapMembers(v)
		if err != nil {
			return err
		}
		// A nested map pushes members past these and releases them, so the
		// loop reads e.pairs by index.
		for j := pbase; j < len(e.pairs); j++ {
			if err := e.scan(e.pairs[j].val, depth+1); err != nil {
				return err
			}
		}
		e.releaseMembers(pbase, keysMark)
		e.closeNode(i)
	case lisp.LTaggedVal:
		if err := checkTagged(v); err != nil {
			return err
		}
		if err := e.scanDepth(depth); err != nil {
			return err
		}
		i := e.openObject(key)
		if err := e.scan(v.Cells[0], depth+1); err != nil {
			return err
		}
		e.closeNode(i)
	case lisp.LNative:
		return e.scanNative(v, key, depth)
	case lisp.LFun:
		return e.scanFunValue(v, depth)
	case lisp.LError:
		return e.scanError(v, key, depth)
	case lisp.LQuote:
		return errors.New("typed json: cannot encode a nested quote")
	case lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand, lisp.LInvalid, lisp.LTypeMax:
		return fmt.Errorf("durable json: cannot encode a %v", v.Type)
	}
	return nil
}

// revisit handles a second or later reference to an object.  Discovery
// only walks, so it checks nothing.
func (e *durableEncoder) revisit(_ *lisp.LVal, key any) error {
	if e.discover {
		return nil
	}
	if len(e.treeNames) > 0 {
		return treeSharingError(e.treeNames[len(e.treeNames)-1])
	}
	if name, ok := e.tree[key]; ok {
		return treeSharingError(name)
	}
	if i, ok := e.order[key]; ok {
		e.reach(i)
	}
	return nil
}

func treeSharingError(name string) error {
	return fmt.Errorf("durable json: native %q payload shares a value, and its codec does not keep sharing", name)
}

// scanNative saves a native through its codec and scans the payload.  A
// payload that reaches the native, or any object that encloses it, is
// refused: the decoder could not give LoadNative a finished payload.
func (e *durableEncoder) scanNative(v *lisp.LVal, key any, depth int) error {
	entry := e.reg.entryFor(v.Native)
	if entry == nil {
		return fmt.Errorf("durable json: no codec registered for native type %T", v.Native)
	}
	if err := e.scanDepth(depth); err != nil {
		return err
	}
	// Discovery saves each native once; the counting pass reuses it.
	if _, ok := e.saved[key]; !ok {
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
	}
	payload := e.saved[key].payload
	i := e.openObject(key)
	if !entry.shared {
		e.treeNames = append(e.treeNames, entry.name)
	}
	if err := e.scan(payload, depth+1); err != nil {
		return err
	}
	if !entry.shared {
		e.treeNames = e.treeNames[:len(e.treeNames)-1]
	}
	if !e.discover && e.frames[len(e.frames)-1] <= i {
		return fmt.Errorf("durable json: native %q payload refers to a value that encloses the native", entry.name)
	}
	e.closeNode(i)
	return nil
}

// bindingsPerUnit is how many bindings one charge unit pays for when a
// package's function names are read.  One elps evaluation step costs about
// 175 ns (BenchmarkDurableFunctionName's host: a dotimes loop of 50,004
// steps runs in 8.8 ms), and reading one binding costs 22 ns on a cold
// environment and up to 100 ns on a template VM, so four bindings cost
// about one step.
const bindingsPerUnit = 4

// funNameScanUnits is the charge for reading n bindings: ceil(n/4).
func funNameScanUnits(n int) int { return (n + bindingsPerUnit - 1) / bindingsPerUnit }

// funName returns "PKG:NAME" for a Lisp function (builtins go through
// registeredBuiltin and unregisteredBuiltinName): PKG is the function's
// defining package, and NAME is the first name, in sorted order, that PKG
// binds to a function with the same package and FID.  An FID is unique
// within its package, so the package and FID identify the function.  Each
// package's names are read once per dump (Package.FunNamesByFID, which
// materializes no lazy binding), and the read is charged ceil(n/4) units
// for n bindings before it starts.
func (e *durableEncoder) funName(f *lisp.LVal) (string, error) {
	if f.IsSpecialFun() {
		return "", errors.New("durable json: cannot encode a macro or special operator")
	}
	fid, pkgName := f.FID(), f.Package()
	anonymous := errAnonymous
	if fid == "" || pkgName == "" {
		return "", anonymous
	}
	index, ok := e.funIndex[pkgName]
	if !ok {
		pkg := e.env.Runtime.Registry.Package(pkgName)
		if pkg == nil {
			return "", anonymous
		}
		// Charged before the scan, so a step budget or a cancelled
		// context stops it.  The work scales with the package's bindings,
		// not with the saved graph.
		if n := pkg.NumBindings(); e.cfg.charge != nil && n > 0 {
			if err := e.cfg.charge(funNameScanUnits(n)); err != nil {
				return "", fmt.Errorf("durable json: function names of package %s: %w", pkgName, err)
			}
		}
		index, _ = pkg.FunNamesByFID()
		if e.funIndex == nil {
			e.funIndex = map[string]map[string]string{}
		}
		e.funIndex[pkgName] = index
	}
	name, ok := index[fid]
	if !ok {
		return "", anonymous
	}
	if !utf8.ValidString(pkgName) || !utf8.ValidString(name) {
		return "", errors.New("durable json: cannot encode a function name that is not valid UTF-8")
	}
	return pkgName + ":" + name, nil
}

// registeredBuiltin returns the package and name of a registered builtin
// (lisp.PackageRegistry.RegisteredBuiltinName), with ok true.  ok is false
// for a builtin no registration names.  The form depends only on the value:
// the bindings of its names play no part.
func (e *durableEncoder) registeredBuiltin(f *lisp.LVal) (string, string, bool, error) {
	if f.IsSpecialFun() {
		return "", "", false, errors.New("durable json: cannot encode a macro or special operator")
	}
	pkg, name, ok := e.env.Runtime.Registry.RegisteredBuiltinName(f)
	if !ok {
		return "", "", false, nil
	}
	if !utf8.ValidString(pkg) || !utf8.ValidString(name) {
		return "", "", false, errors.New("durable json: cannot encode a function name that is not valid UTF-8")
	}
	return pkg, name, true, nil
}

// unregisteredBuiltinName returns "PKG:NAME" for a builtin no registration
// names: PKG is its package and NAME the first name, in sorted order, under
// which PKG binds this function itself (lisp.Package.FirstNameOf).  Each
// such function is looked up once per dump, and the lookup is charged
// ceil(n/4) units for n bindings before it starts.
func (e *durableEncoder) unregisteredBuiltinName(f *lisp.LVal) (string, error) {
	if name, ok := e.builtinNames[f.Native]; ok {
		return name, nil
	}
	pkgName := f.Package()
	if pkgName == "" {
		return "", errAnonymous
	}
	pkg := e.env.Runtime.Registry.Package(pkgName)
	if pkg == nil {
		return "", errAnonymous
	}
	if n := pkg.NumBindings(); e.cfg.charge != nil && n > 0 {
		if err := e.cfg.charge(funNameScanUnits(n)); err != nil {
			return "", fmt.Errorf("durable json: function names of package %s: %w", pkgName, err)
		}
	}
	name, ok, _ := pkg.FirstNameOf(f)
	if !ok {
		return "", errAnonymous
	}
	if !utf8.ValidString(pkgName) || !utf8.ValidString(name) {
		return "", errors.New("durable json: cannot encode a function name that is not valid UTF-8")
	}
	full := pkgName + ":" + name
	e.builtinNames[f.Native] = full
	return full, nil
}

// builtin writes ["~#builtin",["PKG","NAME"]].  It counts one value, as
// ["~#fn",...] does.
func (e *durableEncoder) builtin(pkg, name string) error {
	if err := e.count(); err != nil {
		return err
	}
	if err := e.reserve(jsonStringLen(pkg) + jsonStringLen(name) + len(tagBuiltin) + 8); err != nil { // ["…",[…,…]]
		return err
	}
	e.buf = append(e.buf, `["`+tagBuiltin+`",[`...)
	e.buf = appendJSONString(e.buf, pkg)
	e.buf = append(e.buf, ',')
	e.buf = appendJSONString(e.buf, name)
	e.buf = append(e.buf, ']', ']')
	return e.grow()
}

// namedFunction writes ["~#fn","PKG:NAME"].  It counts one value.
func (e *durableEncoder) namedFunction(name string) error {
	if err := e.count(); err != nil {
		return err
	}
	if err := e.reserve(jsonStringLen(name) + len(tagFn) + 5); err != nil { // ["…",…]
		return err
	}
	e.buf = append(e.buf, `["`+tagFn+`",`...)
	e.buf = appendJSONString(e.buf, name)
	e.buf = append(e.buf, ']')
	return e.grow()
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
	key, shareable := e.identity(v)
	if !shareable || e.refs[key] < 2 {
		return e.body(v, key, shareable, depth)
	}
	return e.object(key, depth, func() error { return e.body(v, key, shareable, depth) })
}

// body writes v without an object wrapper.  Leaves are written by the
// typed encoder, so their bytes are typed JSON's.
func (e *durableEncoder) body(v *lisp.LVal, key any, shareable bool, depth int) error {
	switch v.Type {
	case lisp.LInt, lisp.LFloat, lisp.LString, lisp.LBytes, lisp.LSymbol:
		return e.typedEncoder.value(v, depth)
	case lisp.LSExpr:
		if !shareable {
			return e.typedEncoder.value(v, depth)
		}
		return e.holderBody(key, v.Cells, depth)
	case lisp.LArray:
		dims, cells, err := checkArray(v)
		if err != nil {
			return err
		}
		if err := e.container(depth); err != nil {
			return err
		}
		if e.separateData(v.Cells[1]) {
			// The data has an identity of its own: write it as a holder
			// after the dimensions, whatever the rank.
			if err := e.arrayDims(dims); err != nil {
				return err
			}
			if err := e.holder(holderKey{v.Cells[1]}, cells, depth); err != nil {
				return err
			}
			e.buf = append(e.buf, ']', ']')
			return e.grow()
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
		if err := e.reserve(jsonStringLen(v.Str) + len(tagTagged) + 6); err != nil {
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
		if err := e.reserve(jsonStringLen(s.entry.name) + len(tagNative) + 6); err != nil {
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
		if v.Builtin() != nil {
			pkg, name, ok, err := e.registeredBuiltin(v)
			switch {
			case err != nil:
				return err
			case ok:
				return e.builtin(pkg, name)
			}
			full, ok := e.builtinNames[v.Native]
			if !ok {
				return errors.New("durable json: function was not resolved")
			}
			return e.namedFunction(full)
		}
		name, ok := e.funs[funKey{v.Package(), v.FID()}]
		if !ok {
			return errors.New("durable json: function was not resolved")
		}
		if name == "" {
			return e.closure(v, depth)
		}
		return e.namedFunction(name)
	case lisp.LError:
		if err := e.errorBody(v, depth); err != nil {
			return err
		}
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
		if err := e.reserve(jsonStringLen(e.keys[p.ks:p.ke])); err != nil {
			return err
		}
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
	if env == nil {
		return nil, errors.New("durable json: DumpDurableRoots needs an environment")
	}
	if err := reg.checkUsable(); err != nil {
		return nil, err
	}
	// Each root is at least two values: check before allocating for them.
	if cfg := durableConfig(env, opts); len(roots) > cfg.maxValues/2 {
		return nil, fmt.Errorf("%w: more than %d values", ErrTypedLimit, cfg.maxValues)
	}
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
