// Copyright © 2026 The ELPS authors

package lisp

import (
	"cmp"
	"errors"
	"fmt"
	"reflect"
	"slices"
)

// A template plan represents mutable VM state with immutable descriptors.
// Mutable references are one-based indices; only admitted immutable values
// have shared pointers. Destinations are allocated before references are fixed,
// so aliases and cycles need no discovery during instantiation.
type templateRef struct {
	shared *LVal
	index  int
}
type templateBinding struct {
	name  string
	value templateRef
}
type templateValue struct {
	header  LVal
	view    templateView
	payload int
	kind    uint8
	hasView bool
}

const (
	templatePlain uint8 = iota
	templateFunction
	templateMapPayload
	templateBytePayload
	templateNativePayload
)

type templateEnv struct {
	bindings []templateBinding
	parent   int
	id       uint
}
type templateFunctionData struct {
	values   templateRef
	code     func(*LEnv, *LVal, *LVal) *LVal
	header   funData
	env      int
	captures int
}
type templateMap struct {
	backing int // one-based; zero is a nil implementation
	nilData bool
}
type templateKeyType struct {
	key  string
	kind keytype
}
type templateMapBacking struct {
	entries []templateBinding
	types   []templateKeyType
	json    bool
}
type templateMapIdentity struct {
	values, types uintptr
	json          bool
}
type templateBytes struct {
	view           templateView
	nonnil, backed bool
}
type templateStringPair struct{ key, value string }
type templatePackage struct {
	name, doc            string
	bindings             []templateBinding
	funNames, symbolDocs []templateStringPair
	externals            []string
}
type templatePlan struct {
	values            []templateValue
	envs              []templateEnv
	functions         []templateFunctionData
	maps              []templateMap
	mapBackings       []templateMapBacking
	bytes             []templateBytes
	byteBackings      [][]byte
	natives           []any
	cells             [][]templateRef
	packages          []templatePackage
	runtime           templateRuntime
	root, numCaptures int
}
type templateCompiler struct {
	err         error
	storage     *templateStorage
	values      map[*LVal]int
	envs        map[*LEnv]int
	sealed      map[*LVal]bool
	functions   map[*funData]int
	maps        map[*MapData]int
	mapBackings map[templateMapIdentity]int
	bytes       map[*[]byte]int
	natives     map[any]int
	cellsReady  []bool
	plan        templatePlan
}

// compileTemplate consumes the quiescent source and temporary storage inventory.
// No source identity maps, queues, environments, packages or mutable backings
// escape this call. storage.bytes already owns independent byte-group copies.
func compileTemplate(env *LEnv, inventory *templateInventory) (templatePlan, error) {
	storage := inventory.storage()
	// Admission assigned every private identity before compilation. Reuse that
	// closed index: compilation cannot discover an unvalidated graph edge.
	c := templateCompiler{storage: storage, values: inventory.values, envs: inventory.envs, sealed: inventory.sealed,
		functions: make(map[*funData]int), maps: make(map[*MapData]int, len(inventory.maps)), bytes: make(map[*[]byte]int, len(inventory.byteSeen)), natives: make(map[any]int),
		mapBackings: make(map[templateMapIdentity]int), cellsReady: make([]bool, len(storage.cells))}
	c.plan.values = make([]templateValue, len(inventory.valueQueue))
	c.plan.envs = make([]templateEnv, len(inventory.envQueue))
	c.plan.maps = make([]templateMap, 0, len(inventory.maps))
	c.plan.bytes = make([]templateBytes, 0, len(inventory.byteSeen))
	c.plan.packages = make([]templatePackage, 0, len(env.Runtime.Registry.packages))
	c.plan.runtime = snapshotTemplateRuntime(env.Runtime)
	c.plan.cells = make([][]templateRef, len(storage.cells))
	c.plan.byteBackings = storage.bytes
	c.plan.root = c.env(env)
	for _, name := range sortedTemplateKeys(env.Runtime.Registry.packages) {
		pkg := env.Runtime.Registry.packages[name]
		c.plan.packages = append(c.plan.packages, templatePackage{
			name: pkg.Name, doc: pkg.Doc, bindings: c.bindings(pkg.symbols),
			funNames: templateStringPairs(pkg.funNames), symbolDocs: templateStringPairs(pkg.symbolDocs),
			externals: append([]string(nil), pkg.externals...),
		})
	}
	for index, env := range inventory.envQueue {
		c.plan.envs[index] = templateEnv{id: env.ID, parent: c.env(env.parent), bindings: c.bindings(env.scope)}
	}
	for index, source := range inventory.valueQueue {
		value, err := c.value(source)
		if err != nil {
			return templatePlan{}, err
		}
		c.plan.values[index] = value
	}
	if c.err != nil {
		return templatePlan{}, c.err
	}
	return c.plan, nil
}

func templateStringPairs(values map[string]string) []templateStringPair {
	if len(values) == 0 {
		return nil
	}
	out := make([]templateStringPair, 0, len(values))
	for key, value := range values {
		out = append(out, templateStringPair{key: key, value: value})
	}
	slices.SortFunc(out, func(a, b templateStringPair) int { return cmp.Compare(a.key, b.key) })
	return out
}

func (c *templateCompiler) ref(v *LVal) templateRef {
	if v == nil || isSingleton(v) {
		return templateRef{shared: v}
	}
	if v.sealed && sealableNodeType(v.Type) {
		if c.sealed[v] {
			return templateRef{shared: v}
		}
	} else if index := c.values[v]; index > 0 && index <= len(c.plan.values) {
		return templateRef{index: index}
	}
	if c.err == nil {
		c.err = errors.New("template: value missing admitted identity")
	}
	return templateRef{}
}
func (c *templateCompiler) env(env *LEnv) int {
	if env == nil {
		return 0
	}
	if index := c.envs[env]; index > 0 && index <= len(c.plan.envs) {
		return index
	}
	if c.err == nil {
		c.err = errors.New("template: environment missing admitted identity")
	}
	return 0
}
func (c *templateCompiler) bindings(values map[string]*LVal) []templateBinding {
	// Sort the final descriptors, avoiding temporary keys and a second map
	// lookup per entry. Preserve the existing lexicographic descriptor order,
	// and therefore the order in which each instance populates its maps.
	out := make([]templateBinding, 0, len(values))
	for name, value := range values {
		out = append(out, templateBinding{name: name, value: c.ref(value)})
	}
	sortTemplateBindings(out)
	return out
}

func sortTemplateBindings(bindings []templateBinding) {
	slices.SortFunc(bindings, func(a, b templateBinding) int { return cmp.Compare(a.name, b.name) })
}
func (c *templateCompiler) function(fd *funData) int {
	if index, ok := c.functions[fd]; ok {
		return index
	}
	index := len(c.plan.functions)
	c.functions[fd] = index
	function := templateFunctionData{header: *fd, env: c.env(fd.env)}
	function.header.env = nil
	// Definition locations affect errors at function-body entry. Unlike the
	// transient environment register, retain an owned immutable snapshot (#624).
	function.header.loc = copyLocation(fd.loc)
	function.header.captures = nil
	if fd.captures != nil {
		c.plan.numCaptures++
		function.captures = c.plan.numCaptures
		function.values = c.ref(fd.captures.values)
		function.code = fd.captures.code
		function.header.builtin = nil
	}
	c.plan.functions = append(c.plan.functions, function)
	return index
}
func (c *templateCompiler) value(v *LVal) (templateValue, error) {
	header := &LVal{}
	*header = *v
	header.Native = nil
	header.macroExpansion = nil
	out := templateValue{}
	switch v.Type {
	case LFun:
		out.kind = templateFunction
		out.payload = c.function(v.Native.(*funData))
	case LNative:
		out.kind = templateNativePayload
		out.payload = c.native(v.Native)
	default:
		switch payload := v.Native.(type) {
		case nil:
		case *MapData:
			out.kind = templateMapPayload
			index, err := c.mapData(payload)
			if err != nil {
				return out, err
			}
			out.payload = index
		case *[]byte:
			out.kind = templateBytePayload
			out.payload = c.byteData(payload)
		default:
			out.kind = templateNativePayload
			out.payload = c.native(payload)
		}
	}
	if view, ok := c.storage.cellViews[v]; ok {
		out.hasView = true
		out.view = view
		header.Cells = nil
		if !c.cellsReady[view.storage] {
			c.cellsReady[view.storage] = true
			source := c.storage.cells[view.storage]
			refs := make([]templateRef, len(source))
			for i, value := range source {
				refs[i] = c.ref(value)
			}
			c.plan.cells[view.storage] = refs
		}
	} else if cap(v.Cells) > 0 {
		// Only a function's immutable body/formals have unindexed backing.
		if v.Type != LFun || cap(v.Cells) != len(v.Cells) {
			return out, errors.New("template: cells missing admitted storage identity")
		}
		for _, value := range v.Cells {
			if ref := c.ref(value); ref.index != 0 {
				return out, errors.New("template: mutable cells missing storage identity")
			}
		}
		header.Cells = v.Cells[:len(v.Cells):len(v.Cells)]
	} else if v.Cells != nil {
		// Preserve nil-versus-empty observations by host callbacks without
		// retaining a source array hidden behind a zero-capacity view.
		header.Cells = []*LVal{}
	} else {
		header.Cells = nil
	}
	out.header = *header
	return out, nil
}
func (c *templateCompiler) native(payload any) int {
	memo := payload != nil && reflect.TypeOf(payload).Kind() == reflect.Pointer
	if memo {
		if index, ok := c.natives[payload]; ok {
			return index
		}
	}
	index := len(c.plan.natives)
	c.plan.natives = append(c.plan.natives, payload)
	if memo {
		c.natives[payload] = index
	}
	return index
}
func (c *templateCompiler) byteData(payload *[]byte) int {
	if index, ok := c.bytes[payload]; ok {
		return index
	}
	index := len(c.plan.bytes)
	c.bytes[payload] = index
	view, backed := c.storage.byteViews[payload]
	if cap(*payload) > 0 && !backed {
		if c.err == nil {
			c.err = errors.New("template: bytes missing admitted storage identity")
		}
		return 0
	}
	c.plan.bytes = append(c.plan.bytes, templateBytes{view: view, backed: backed, nonnil: *payload != nil})
	return index
}
func (c *templateCompiler) mapData(source *MapData) (int, error) {
	if index, ok := c.maps[source]; ok {
		return index, nil
	}
	index := len(c.plan.maps)
	c.maps[source] = index
	c.plan.maps = append(c.plan.maps, templateMap{})
	if source == nil {
		c.plan.maps[index].nilData = true
		return index, nil
	}
	if source.mapBacking == nil {
		return index, nil
	}
	var id templateMapIdentity
	var backing templateMapBacking
	switch sourceMap := source.mapBacking.(type) {
	case sortedmap:
		id = templateMapIdentity{values: reflect.ValueOf(sourceMap.m).Pointer(), types: reflect.ValueOf(sourceMap.tm).Pointer()}
		if existing := c.mapBackings[id]; existing != 0 {
			c.plan.maps[index].backing = existing
			return index, nil
		}
		backing.entries = c.bindings(sourceMap.m)
		backing.types = make([]templateKeyType, 0, len(sourceMap.tm))
		for key, kind := range sourceMap.tm {
			backing.types = append(backing.types, templateKeyType{key: key, kind: kind})
		}
		slices.SortFunc(backing.types, func(a, b templateKeyType) int { return cmp.Compare(a.key, b.key) })
	case jsonMap:
		id = templateMapIdentity{values: reflect.ValueOf(sourceMap).Pointer(), json: true}
		if existing := c.mapBackings[id]; existing != 0 {
			c.plan.maps[index].backing = existing
			return index, nil
		}
		backing.json = true
		backing.entries = make([]templateBinding, 0, len(sourceMap))
		for key, value := range sourceMap {
			backing.entries = append(backing.entries, templateBinding{name: key, value: c.ref(jsonMapLVal(value))})
		}
		sortTemplateBindings(backing.entries)
	default:
		return index, fmt.Errorf("template: map backing %T is not interpreter-owned", source.mapBacking)
	}
	c.plan.mapBackings = append(c.plan.mapBackings, backing)
	c.mapBackings[id] = len(c.plan.mapBackings)
	c.plan.maps[index].backing = len(c.plan.mapBackings)
	return index, nil
}

type templateInstance struct {
	values       []*LVal
	envs         []*LEnv
	functions    []*funData
	captures     []*builtinCaptures
	maps         []*MapData
	bytes        []*[]byte
	cells        [][]*LVal
	byteBackings [][]byte
}

// Individual objects preserve ordinary escape lifetimes. In particular, a
// scalar does not retain function headers or environments from a shared slab.
func templateObjects[T any](n int) []*T {
	refs := make([]*T, n)
	for i := range refs {
		refs[i] = new(T)
	}
	return refs
}
func (i *templateInstance) ref(ref templateRef) *LVal {
	if ref.index == 0 {
		return ref.shared
	}
	return i.values[ref.index-1]
}
func (i *templateInstance) env(index int) *LEnv {
	if index == 0 {
		return nil
	}
	return i.envs[index-1]
}

func (p *templatePlan) instantiate(opts []VMOption) (*LEnv, error) {
	var config vmConfig
	for _, opt := range opts {
		if opt == nil {
			return nil, errors.New("template: nil VM option")
		}
		opt(&config)
	}
	rt := p.runtime.newRuntime(config)
	instance := templateInstance{
		values: templateObjects[LVal](len(p.values)), envs: templateObjects[LEnv](len(p.envs)),
		functions: templateObjects[funData](len(p.functions)), captures: templateObjects[builtinCaptures](p.numCaptures),
		maps: templateObjects[MapData](len(p.maps)), bytes: templateObjects[[]byte](len(p.bytes)),
		cells: make([][]*LVal, len(p.cells)), byteBackings: make([][]byte, len(p.byteBackings)),
	}
	for group, refs := range p.cells {
		instance.cells[group] = make([]*LVal, len(refs))
		for slot, ref := range refs {
			instance.cells[group][slot] = instance.ref(ref)
		}
	}
	for group, source := range p.byteBackings {
		instance.byteBackings[group] = append([]byte(nil), source...)
	}
	for index, bytes := range p.bytes {
		if bytes.backed {
			v := bytes.view
			*instance.bytes[index] = instance.byteBackings[v.storage][v.offset : v.offset+v.length : v.offset+v.capacity]
		} else if bytes.nonnil {
			*instance.bytes[index] = []byte{}
		}
	}
	// Admission accepts only immutable native payloads. Even a payload also
	// implementing NativeCloner is shared; construction runs no host clone code.
	for _, payload := range p.natives {
		checkNativeAffinity(rt, payload)
	}
	for index, env := range p.envs {
		instance.envs[index].ID = env.id
		instance.envs[index].Runtime = rt
		instance.envs[index].parent = instance.env(env.parent)
		instance.envs[index].scope = make(map[string]*LVal, len(env.bindings))
		for _, binding := range env.bindings {
			instance.envs[index].scope[binding.name] = instance.ref(binding.value)
		}
	}
	for index, function := range p.functions {
		data := instance.functions[index]
		*data = function.header
		data.env = instance.env(function.env)
		if function.captures != 0 {
			captures := instance.captures[function.captures-1]
			*captures = builtinCaptures{values: instance.ref(function.values), code: function.code}
			data.captures = captures
			data.builtin = captures.call
		}
	}
	mapBackings := make([]Map, len(p.mapBackings))
	for index, backing := range p.mapBackings {
		if backing.json {
			m := make(jsonMap, len(backing.entries))
			mapBackings[index] = m
			for _, entry := range backing.entries {
				m[entry.name] = instance.ref(entry.value)
			}
		} else {
			m := sortedmap{m: make(map[string]*LVal, len(backing.entries)), tm: make(typemap, len(backing.types))}
			mapBackings[index] = m
			for _, kind := range backing.types {
				m.tm[kind.key] = kind.kind
			}
			for _, entry := range backing.entries {
				m.m[entry.name] = instance.ref(entry.value)
			}
		}
	}
	for index, m := range p.maps {
		if m.nilData {
			instance.maps[index] = nil
		} else if m.backing != 0 {
			instance.maps[index].mapBacking = mapBackings[m.backing-1]
		}
	}
	// Every identity already exists; patching these headers closes all cycles.
	for index, value := range p.values {
		out := &LVal{}
		*out = value.header
		if value.hasView {
			v := value.view
			out.Cells = instance.cells[v.storage][v.offset : v.offset+v.length : v.offset+v.capacity]
		}
		switch value.kind {
		case templateFunction:
			out.Native = instance.functions[value.payload]
		case templateMapPayload:
			out.Native = instance.maps[value.payload]
		case templateBytePayload:
			out.Native = instance.bytes[value.payload]
		case templateNativePayload:
			//elpsvet:allow-native instantiation replays a payload the PLAN already holds: templateInventory.native admitted it at publication (a marked immutable struct value, a scalar, or one the embedder approved), and checkNativeAffinity re-checks it above, so this store opens no channel the audit did not already take
			out.Native = p.natives[value.payload]
		}
		*instance.values[index] = *out //elps:mutates templateObjects allocated these private destinations for this instance; no source or published VM points to them
	}
	for _, pkg := range p.packages {
		out := &Package{Name: pkg.name, Doc: pkg.doc, symbols: make(map[string]*LVal, len(pkg.bindings)), funNames: make(map[string]string, len(pkg.funNames))}
		for _, binding := range pkg.bindings {
			out.symbols[binding.name] = instance.ref(binding.value)
		}
		for _, name := range pkg.funNames {
			out.funNames[name.key] = name.value
		}
		if len(pkg.symbolDocs) > 0 {
			out.symbolDocs = make(map[string]string, len(pkg.symbolDocs))
			for _, doc := range pkg.symbolDocs {
				out.symbolDocs[doc.key] = doc.value
			}
		}
		if len(pkg.externals) > 0 {
			out.externals = append([]string(nil), pkg.externals...)
		}
		rt.Registry.packages[out.Name] = out
	}
	if p.runtime.hasCurrentPackage {
		rt.Package = rt.Registry.packages[p.runtime.currentPackage]
	}
	root := instance.env(p.root)
	if config.ctx != nil {
		root.evalCtx = config.ctx
	}
	return root, nil
}
