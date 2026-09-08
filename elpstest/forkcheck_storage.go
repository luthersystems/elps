// Copyright © 2026 The ELPS authors

package elpstest

import (
	"fmt"
	"hash/fnv"
	"reflect"
	"runtime"
	"sort"
	"strings"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/lisp"
)

// One physical identity namespace covers both VM-owned storage and direct
// native exposures. Eligibility for native sharing is separate census data.
type nativePayloadIdentity struct {
	kind    reflect.Kind
	address uintptr
}

func oraclePointerID(pointer any) nativePayloadIdentity {
	return nativePayloadIdentity{reflect.Pointer, reflect.ValueOf(pointer).Pointer()}
}

// These are identities of actual retained storage, not an interpreter's view
// declaration or Go function code addresses. Native functions and references
// nested inside structs need host observations; they cannot be inferred here.
// A zero-size element has no independently writable storage identity.
func nativeReferenceIDs(value any) []nativePayloadIdentity {
	rv := reflect.ValueOf(value)
	if !rv.IsValid() {
		return nil
	}
	switch rv.Kind() { //nolint:exhaustive // only directly identifiable reference kinds belong here
	case reflect.Pointer:
		if !rv.IsNil() && rv.Type().Elem().Size() > 0 {
			return []nativePayloadIdentity{{reflect.Pointer, rv.Pointer()}}
		}
	case reflect.Map, reflect.Chan:
		if !rv.IsNil() {
			return []nativePayloadIdentity{{rv.Kind(), rv.Pointer()}}
		}
	case reflect.Slice:
		if rv.Cap() == 0 || rv.Type().Elem().Size() == 0 {
			return nil
		}
		full := rv.Slice(0, rv.Cap())
		ids := make([]nativePayloadIdentity, full.Len())
		for i := range ids {
			ids[i] = nativePayloadIdentity{reflect.Pointer, full.Index(i).Addr().Pointer()}
		}
		return ids
	}
	return nil
}

func mutableOracleHeader(v *lisp.LVal) bool {
	return !v.IsSealed() && v != lisp.Nil() && v != lisp.Bool(true) && v != lisp.Bool(false)
}

func readonlyOracleCells(v *lisp.LVal) bool {
	if v.IsSealed() {
		return true
	}
	if v.Type != lisp.LFun || cap(v.Cells) != len(v.Cells) {
		return false
	}
	for _, child := range v.Cells {
		if child != nil && !child.IsSealed() {
			return false
		}
	}
	return true
}

func oracleOwnedIDs(v *lisp.LVal) []nativePayloadIdentity {
	var ids []nativePayloadIdentity
	if mutableOracleHeader(v) {
		ids = append(ids, oraclePointerID(v))
	}
	if v.IsSealed() {
		return ids
	}
	if id, ok := payloadIdentity(v); ok && v.Type != lisp.LNative {
		ids = append(ids, nativeReferenceIDs(id)...)
	}
	if v.Type == lisp.LSortMap {
		ids = append(ids, oracleMapBackingIDs(v.Map())...)
	}
	if !readonlyOracleCells(v) {
		full := v.Cells[:cap(v.Cells)]
		for i := range full {
			ids = append(ids, oraclePointerID(&full[i]))
		}
	}
	if v.Type == lisp.LBytes {
		data := v.Bytes()
		full := data[:cap(data)]
		for i := range full {
			ids = append(ids, oraclePointerID(&full[i]))
		}
	}
	return ids
}

func oracleNativePayload(v *lisp.LVal) any {
	if v.Type == lisp.LNative {
		return v.Native
	}
	return oracleNativeAnnotation(v)
}

func oracleValueIDs(v *lisp.LVal) []any {
	var ids []any
	for _, id := range oracleOwnedIDs(v) {
		ids = append(ids, id)
	}
	for _, id := range nativeReferenceIDs(oracleNativePayload(v)) {
		ids = append(ids, id)
	}
	return ids
}

// Every header is visited independently even when two headers share a payload:
// their lengths, capacities and outgoing edges can differ. Visit repeats before
// cutting recursion so aliases are represented without exponential traversal.
func walkOracleGraph(env *lisp.LEnv, value func(*lisp.LVal, string), environment func(*lisp.LEnv, string)) {
	seen := make(map[*lisp.LVal]bool)
	envs := make(map[*lisp.LEnv]bool)
	var walk func(*lisp.LVal, string)
	var walkEnv func(*lisp.LEnv, string)
	walk = func(v *lisp.LVal, path string) {
		if v == nil {
			return
		}
		value(v, path)
		if seen[v] {
			return
		}
		seen[v] = true
		if v.Type == lisp.LSortMap {
			if md := v.Map(); md != nil {
				for _, key := range md.Keys().Cells {
					child, _ := md.Get(key)
					walk(child, path+"/"+key.String())
				}
			}
		}
		for i, child := range v.Cells[:cap(v.Cells)] {
			walk(child, fmt.Sprintf("%s/%d", path, i))
		}
		if v.Type == lisp.LFun {
			walkEnv(funraw.Env(v), path+"/env")
			walk(funraw.Captures(v), path+"/captures")
		}
	}
	walkEnv = func(e *lisp.LEnv, path string) {
		if e == nil {
			return
		}
		environment(e, path)
		if envs[e] {
			return
		}
		envs[e] = true
		keys, values := sortedBindings(e)
		for _, key := range keys {
			walk(values[key], path+"/"+key)
		}
		walkEnv(e.Parent(), path+"/parent")
	}
	roots(env, func(pkg, name string, v *lisp.LVal) { walk(v, pkg+":"+name) })
}

// Sealed identity is an implementation detail of the parse cache. Memoize
// bottom-up content digests so shared and independently parsed equal subtrees
// compare equally without expanding a diamond graph once per incoming path.
// Cycles use an active-path marker rather than a mutable identity ordinal.
type sealedOracleState struct {
	memo         map[*lisp.LVal]uint64
	active       map[*lisp.LVal]bool
	renderNative func(any) string
}

func (s *sealedOracleState) digest(v *lisp.LVal) uint64 {
	if v == nil {
		return 0
	}
	if digest, found := s.memo[v]; found {
		return digest
	}
	if s.active[v] {
		return 1
	}
	if s.memo == nil {
		s.memo = make(map[*lisp.LVal]uint64)
		s.active = make(map[*lisp.LVal]bool)
	}
	s.active[v] = true
	// The canonical shallow fingerprint includes private quote/splice flags
	// and source metadata. It never walks the original children here.
	header := *v
	header.Cells = nil
	h := fnv.New64a()
	_, _ = fmt.Fprintf(h, "%x/%t/%d/%d:", lisp.SealedASTFingerprint([]*lisp.LVal{&header}), v.Cells == nil, len(v.Cells), cap(v.Cells))
	if annotation := oracleNativeAnnotation(v); annotation != nil {
		_, _ = fmt.Fprintf(h, "annotation(%T)", annotation)
		if s.renderNative != nil {
			_, _ = fmt.Fprintf(h, "%q", s.renderNative(annotation))
		}
	}
	for _, child := range v.Cells[:cap(v.Cells)] {
		_, _ = fmt.Fprintf(h, "%x;", s.digest(child))
	}
	digest := h.Sum64()
	delete(s.active, v)
	s.memo[v] = digest
	return digest
}

// Physical addresses are meaningful only while their owners remain live. Keep
// identity maps and their retainers together through every comparison (#625).
type oracleCensus struct {
	ids      map[interface{}]string
	natives  map[nativePayloadIdentity]any
	retained []any // Keep snapshot addresses alive even after bindings/backings change.
}

func newOracleCensus(env *lisp.LEnv) oracleCensus {
	ids := make(map[interface{}]string)
	natives := make(map[nativePayloadIdentity]any)
	retained := []any{env}
	owned := make(map[nativePayloadIdentity]bool)
	record := func(id any, path string) {
		if _, found := ids[id]; !found {
			ids[id] = path
		}
	}
	walkOracleGraph(env, func(v *lisp.LVal, path string) {
		// Keep copies of slice headers, not only the mutable LVal/bytes box:
		// an append or cell replacement can detach their former backing.
		retained = append(retained, v, v.Cells, v.Native)
		if v.Type == lisp.LBytes {
			retained = append(retained, v.Bytes())
		}
		if v.Type == lisp.LSortMap && v.Map() != nil {
			// Copy the embedded backing interface: a host can replace the
			// exported MapData wrapper wholesale after this observation.
			retained = append(retained, *v.Map())
		}
		for _, id := range oracleOwnedIDs(v) {
			record(id, path)
			owned[id] = true
		}
		payload := oracleNativePayload(v)
		for _, id := range nativeReferenceIDs(payload) {
			record(id, path)
			natives[id] = payload
		}
	}, func(e *lisp.LEnv, path string) {
		retained = append(retained, e)
		id := oraclePointerID(e)
		record(id, path)
		owned[id] = true
	})
	// A native declaration cannot make VM-owned mutable storage immutable.
	// Apply this after the whole census so root order cannot restore sharing
	// exemption metadata through another exposure of the same address.
	for id := range owned {
		delete(natives, id)
	}
	return oracleCensus{ids: ids, natives: natives, retained: retained}
}

// Intersect before applying sharing policies: an immutable native in one VM
// must not conceal a writable VM-owned exposure of the same storage in another.
func oracleSharedPayloads(a, b *lisp.LEnv, allowNative func(nativePayloadIdentity, any) bool) []string {
	return sharedOracleCensuses(newOracleCensus(a), newOracleCensus(b), allowNative)
}

func sharedOracleCensuses(a, b oracleCensus, allowNative func(nativePayloadIdentity, any) bool) []string {
	var shared []string
	for id, path := range a.ids {
		if _, found := b.ids[id]; !found {
			continue
		}
		if native, ok := id.(nativePayloadIdentity); ok && allowNative != nil {
			aValue, aNative := a.natives[native]
			bValue, bNative := b.natives[native]
			if aNative && bNative && allowNative(native, aValue) && allowNative(native, bValue) {
				continue
			}
		}
		shared = append(shared, path)
	}
	sort.Strings(shared)
	runtime.KeepAlive(a.retained)
	runtime.KeepAlive(b.retained)
	return shared
}

func oracleStorageSignature(env *lisp.LEnv) string {
	var out strings.Builder
	ids := make(map[any]int)
	write := func(id any) {
		n, found := ids[id]
		if !found {
			n = len(ids)
			ids[id] = n
		}
		fmt.Fprintf(&out, " #%d", n)
	}
	walkOracleGraph(env, func(v *lisp.LVal, _ string) {
		valueIDs := oracleValueIDs(v)
		if len(valueIDs) == 0 {
			return
		}
		out.WriteString("storage:")
		for _, id := range valueIDs {
			write(id)
		}
		out.WriteByte('\n')
	}, func(e *lisp.LEnv, _ string) {
		out.WriteString("storage:")
		write(oraclePointerID(e))
		out.WriteByte('\n')
	})
	return out.String()
}

// A TextLoader/host can attach Native annotations to ordinary values after
// publication. They are not necessarily LNative and may be under sealed nodes.
// Distinguish annotations from the interpreter-owned payload types.
func oracleNativeAnnotation(v *lisp.LVal) any {
	switch v.Type {
	case lisp.LFun, lisp.LSortMap, lisp.LBytes, lisp.LError, lisp.LNative:
		return nil
	default:
		return v.Native
	}
}
