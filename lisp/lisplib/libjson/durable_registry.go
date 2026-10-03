// Copyright © 2026 The ELPS authors

package libjson

import (
	"encoding/json"
	"errors"
	"fmt"
	"reflect"
	"slices"
	"strings"
	"unicode/utf8"

	"github.com/luthersystems/elps/lisp"
)

// NativeCodec saves and restores the native values of one Go type in
// durable typed JSON.  Register it with RegisterNative or
// DurableRegistry.Register.
//
// A codec must follow this contract, because peers that save or restore one
// value must agree byte for byte:
//
//   - Deterministic: the result depends only on the arguments.  No clock,
//     random source, Go map iteration order or pointer value.
//   - No transaction context: no ledger read or write, no logging that a
//     result depends on, and no context value that the arguments do not
//     carry.  Everything a codec needs is in the native or the payload.
//   - No caching that changes a result: LoadNative returns a new native on
//     every call.  A zero-size pointer payload can share an address with
//     another, so give a pointer type a field.
//   - Graph identity: a codec keeps the identity of its payload or refuses
//     sharing.  By default elps refuses a payload that holds a shared
//     object; a codec whose SaveNative returns the exact payload values
//     LoadNative received registers with WithSharedPayload.
//   - Charged: the declared charge (WithNativeCharge) is taken before each
//     call.  Work that grows with the input beyond it is charged by the
//     codec through env.ChargeSteps, and a failed charge is returned as the
//     error.
//   - Versioned: LoadNative reads every version from 1 to the registered
//     one.  Change the payload shape only with a new version.
type NativeCodec interface {
	// SaveNative returns the payload for v, a native value of the
	// registered Go type.  The payload is any value DumpDurable can write,
	// natives and named functions included.  DumpDurable calls SaveNative
	// once per native object in one dump.
	SaveNative(env *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error)
	// LoadNative rebuilds a native value from a payload that SaveNative
	// returned at version (1 to the registered version).  The result must
	// be a native value of the registered Go type.  The payload is fully
	// restored: it holds no object that is still being loaded.
	LoadNative(env *lisp.LEnv, version int, payload *lisp.LVal) (*lisp.LVal, error)
}

// NativeFuncs is a NativeCodec built from two functions.
type NativeFuncs struct {
	Save func(env *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error)
	Load func(env *lisp.LEnv, version int, payload *lisp.LVal) (*lisp.LVal, error)
}

// SaveNative calls f.Save.
func (f NativeFuncs) SaveNative(env *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) {
	return f.Save(env, v)
}

// LoadNative calls f.Load.
func (f NativeFuncs) LoadNative(env *lisp.LEnv, version int, payload *lisp.LVal) (*lisp.LVal, error) {
	return f.Load(env, version, payload)
}

// DurableRegistry maps native Go types to their codecs and stable names.
// The zero value is not usable; call NewDurableRegistry.  Register every
// codec when the environment is built, then call Freeze.  DumpDurable and
// LoadDurable refuse a registry that is not frozen, and a frozen registry
// refuses Register, so the codecs cannot change while documents are written
// or read.  A frozen registry is safe for concurrent use.  Registration
// order never changes a document's bytes.
type DurableRegistry struct {
	byType map[reflect.Type]*nativeEntry
	byName map[string]*nativeEntry
	frozen bool
}

type nativeEntry struct {
	typ     reflect.Type
	codec   NativeCodec
	name    string
	version int
	charge  int
	shared  bool
}

// NativeOption configures one registration.
type NativeOption func(*nativeEntry)

// WithSharedPayload declares that the codec keeps the identity of its
// payload: SaveNative returns the same payload values LoadNative was given
// (for example a native that holds its payload and returns it).  Only such a
// codec may have a payload that shares an object with the rest of the
// graph, or within itself.  Without this option, DumpDurable and LoadDurable
// refuse a payload that holds a shared object, because the codec would
// drop the sharing on the next save.
func WithSharedPayload() NativeOption {
	return func(e *nativeEntry) { e.shared = true }
}

// maxNativeVersion is the largest version a document can carry: versions
// are written as plain JSON numbers.
const maxNativeVersion = maxExactInt

// WithNativeCharge declares the units charged before each SaveNative and
// LoadNative call of the codec, through the WithTypedCharge function of the
// dump or load (default 0).  Use it for a codec's fixed cost, such as
// reopening a B+-tree.
func WithNativeCharge(units int) NativeOption {
	return func(e *nativeEntry) { e.charge = units }
}

// NewDurableRegistry returns an empty registry.
func NewDurableRegistry() *DurableRegistry {
	return &DurableRegistry{
		byType: map[reflect.Type]*nativeEntry{},
		byName: map[string]*nativeEntry{},
	}
}

// Register adds codec c for native values whose payload (LVal.Native) has Go
// type typ.  name is written into every document and must never change for
// that type; qualify it, for example "substrate:bptree".  version is the
// payload version DumpDurable writes, starting at 1.  Register returns an
// error for an empty or invalid name, a version below 1, a nil type or
// codec, and a type or name that is already registered.
func (r *DurableRegistry) Register(typ reflect.Type, name string, version int, c NativeCodec, opts ...NativeOption) error {
	switch {
	case r == nil || r.byType == nil:
		return errors.New("durable json: registry is not initialized; use NewDurableRegistry")
	case r.frozen:
		return fmt.Errorf("durable json: cannot register %q: the registry is frozen", name)
	case typ == nil:
		return errors.New("durable json: cannot register a nil type")
	case c == nil:
		return fmt.Errorf("durable json: cannot register a nil codec for %q", name)
	case name == "" || !utf8.ValidString(name):
		return errors.New("durable json: a native name must be a nonempty UTF-8 string")
	case version < 1:
		return fmt.Errorf("durable json: native %q: version %d is below 1", name, version)
	case int64(version) > maxNativeVersion:
		return fmt.Errorf("durable json: native %q: version %d is above 2^53", name, version)
	}
	switch typ.Kind() {
	case reflect.Func, reflect.Slice, reflect.Interface:
		return fmt.Errorf("durable json: native %q: a %v payload has no identity; register a pointer type", name, typ.Kind())
	default:
	}
	if !namedOrPointerToNamed(typ) {
		return fmt.Errorf("durable json: native %q: type %v is not named; declare a named type for it", name, typ)
	}
	if prev, ok := r.byType[typ]; ok {
		return fmt.Errorf("durable json: type %v is already registered as %q", typ, prev.name)
	}
	if _, ok := r.byName[name]; ok {
		return fmt.Errorf("durable json: name %q is already registered", name)
	}
	e := &nativeEntry{typ: typ, codec: c, name: name, version: version}
	for _, o := range opts {
		o(e)
	}
	if e.charge < 0 {
		return fmt.Errorf("durable json: native %q: negative charge %d", name, e.charge)
	}
	r.byType[typ] = e
	r.byName[name] = e
	return nil
}

// RegisterNative registers codec c for native payloads of Go type T.  See
// DurableRegistry.Register.
func RegisterNative[T any](r *DurableRegistry, name string, version int, c NativeCodec, opts ...NativeOption) error {
	return r.Register(reflect.TypeFor[T](), name, version, c, opts...)
}

// Freeze ends registration.  Call it once every codec is registered.
func (r *DurableRegistry) Freeze() {
	if r != nil {
		r.frozen = true
	}
}

// Frozen reports whether Freeze was called.
func (r *DurableRegistry) Frozen() bool { return r != nil && r.frozen }

// Fingerprint describes the registered codecs as JSON: an array of
// {"name","type","shape","version","charge","shared"} objects sorted by
// name.  type is the package-qualified Go type name.  shape is its
// complete structure (see typeShape), because two types declared inside
// functions of one Go package can share a name.  Only two such types with
// one name and an identical complete structure are not told apart; declare
// codec types at package level.  Peers compare it to check that they hold
// the same registry.
func (r *DurableRegistry) Fingerprint() string {
	if r == nil {
		return "[]"
	}
	type row struct {
		Name    string `json:"name"`
		Type    string `json:"type"`
		Shape   string `json:"shape"`
		Version int    `json:"version"`
		Charge  int    `json:"charge"`
		Shared  bool   `json:"shared"`
	}
	rows := make([]row, 0, len(r.byName))
	for name, e := range r.byName {
		rows = append(rows, row{Name: name, Version: e.version, Charge: e.charge, Shared: e.shared, Type: qualifiedTypeName(e.typ), Shape: typeShape(e.typ, true)})
	}
	slices.SortFunc(rows, func(a, b row) int { return strings.Compare(a.Name, b.Name) })
	b, err := json.Marshal(rows)
	if err != nil {
		// Strings, ints and bools always marshal.
		panic(err)
	}
	return string(b)
}

// namedOrPointerToNamed reports whether t is a named type or a chain of
// pointers to one.  Only such types have a name that identifies them, so
// only they are registered.
func namedOrPointerToNamed(t reflect.Type) bool {
	for t.Kind() == reflect.Pointer && t.Name() == "" {
		t = t.Elem()
	}
	return t.Name() != ""
}

// typeShape describes t's complete structure: its kind and, by kind,
// channel direction, array length, map key and element, struct fields
// (name, package path, embedding, tag and type), function parameters,
// results and variadic flag, and interface methods (name, package path and
// signature).  An unnamed component is described by its own shape; unnamed
// types cannot refer to themselves, so that recursion ends.  A named
// component is described by its qualified name, except the named type
// behind the registered type's pointers, which is expanded once.
func typeShape(t reflect.Type, top bool) string {
	// top stays true through the pointers of the registered type.
	ptr := top && t.Kind() == reflect.Pointer
	comp := func(c reflect.Type) string {
		switch {
		case c.Name() == "":
			return typeShape(c, ptr)
		case ptr && c.PkgPath() != "":
			// The named type behind the registered pointers: expand it once.
			return qualifiedTypeName(c) + "=" + typeShape(c, false)
		default:
			return qualifiedTypeName(c)
		}
	}
	list := func(n int, at func(int) string) string {
		parts := make([]string, n)
		for i := range parts {
			parts[i] = at(i)
		}
		return strings.Join(parts, ",")
	}
	switch k := t.Kind(); k {
	case reflect.Pointer:
		return "ptr(" + comp(t.Elem()) + ")"
	case reflect.Chan:
		return "chan(" + t.ChanDir().String() + "," + comp(t.Elem()) + ")"
	case reflect.Map:
		return "map(" + comp(t.Key()) + "," + comp(t.Elem()) + ")"
	case reflect.Array:
		return fmt.Sprintf("array(%d,%s)", t.Len(), comp(t.Elem()))
	case reflect.Slice:
		return "slice(" + comp(t.Elem()) + ")"
	case reflect.Struct:
		return "struct(" + list(t.NumField(), func(i int) string {
			f := t.Field(i)
			return fmt.Sprintf("%q %q %t %q %s", f.Name, f.PkgPath, f.Anonymous, string(f.Tag), comp(f.Type))
		}) + ")"
	case reflect.Func:
		return fmt.Sprintf("func(in(%s),out(%s),variadic=%t)",
			list(t.NumIn(), func(i int) string { return comp(t.In(i)) }),
			list(t.NumOut(), func(i int) string { return comp(t.Out(i)) }),
			t.IsVariadic())
	case reflect.Interface:
		return "interface(" + list(t.NumMethod(), func(i int) string {
			m := t.Method(i)
			return fmt.Sprintf("%q %q %s", m.Name, m.PkgPath, comp(m.Type))
		}) + ")"
	default:
		return k.String()
	}
}

// qualifiedTypeName names a registered type with its full package path, so
// two types of one short name in different packages differ.  Register
// admits only named types and pointers to them.
func qualifiedTypeName(t reflect.Type) string {
	if t.Kind() == reflect.Pointer && t.Name() == "" {
		return "*" + qualifiedTypeName(t.Elem())
	}
	if t.PkgPath() != "" {
		return t.PkgPath() + "." + t.Name()
	}
	return t.Name()
}

// checkUsable rejects a registry DumpDurable or LoadDurable cannot use.
func (r *DurableRegistry) checkUsable() error {
	if r != nil && !r.frozen {
		return errors.New("durable json: the registry is not frozen; call Freeze after registration")
	}
	return nil
}

func (r *DurableRegistry) entryFor(native any) *nativeEntry {
	if r == nil || native == nil {
		return nil
	}
	return r.byType[reflect.TypeOf(native)]
}

func (r *DurableRegistry) entryNamed(name string) *nativeEntry {
	if r == nil {
		return nil
	}
	return r.byName[name]
}
