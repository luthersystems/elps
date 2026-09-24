// Copyright © 2026 The ELPS authors

package lisp

import (
	"errors"
	"fmt"
	"reflect"
	"slices"
	"sort"
	"strings"

	"github.com/luthersystems/elps/internal/templatepolicy"
)

// Template is a privately owned construction plan for a loaded environment.
// Construct it with NewTemplate and instantiate independent VMs with NewVM.
// Mutable storage is copied once per backing object, including overlapping
// list and byte views. Transitively sealed program structure remains shared.
//
// The source must be quiescent during NewTemplate: no active evaluation and no
// concurrent mutation of its reachable state (see [NewTemplate]). Afterwards its
// mutable values may change without changing the plan. Environment, runtime and
// package state is represented by owned descriptors, not source objects.
// Concurrent NewVM calls are supported.
//
// Sharing immutable Go objects can retain their enclosing backing allocations,
// including inaccessible elements outside a slice's capacity. Thus Template
// does not guarantee garbage collection of the source VM when host-provided
// shared storage or services indirectly retain it (see the package documentation).
//
// As with Program, embedders must not write exported fields of sealed values
// or function formals/body vectors that contain shared sealed code.
// Reader, SourceLibrary, LoadCache and Stderr retain their shared-service contracts.
// Approved Go callbacks and native implementations remain trusted code: arbitrary
// hidden Go references cannot be discovered or isolated by the interpreter.
type Template struct {
	plan templatePlan
}

// TemplateOption configures admission of a loaded environment.
type TemplateOption func(*templateConfig)

type templateConfig struct {
	builtinPolicy func(*LVal) bool
	nativePolicy  func(any) bool
	frozen        map[string]bool
	eager         bool
}

// TemplateWithEagerInstantiation makes every NewVM build the template's whole
// value graph up front, as releases before lazy instantiation did. By default
// a VM materializes each package binding, sorted-map entry and everything
// they reach on first use, which is observably identical but costs only what
// a VM touches. Eager instantiation is the escape hatch for a host that reads
// one VM from several goroutines at once (lazy materialization turns reads
// into writes) or that wants every allocation paid at NewVM.
func TemplateWithEagerInstantiation() TemplateOption {
	return func(c *templateConfig) { c.eager = true }
}

// TemplateWithBuiltinPolicy explicitly approves legacy Go builtin code for
// sharing. The predicate must attest that a callback captures no mutable VM data:
// it uses only immutable configuration, concurrency-safe host services, and the
// environment/arguments of its call. Shared services must not carry VM-local or
// request-bound state; callbacks obtain that state from each call's environment.
// ELPS's internal captured builtins expose VM state explicitly and need no such
// approval. Embedders attach mutable host state to each instance's context.
// A package name or a reflect function address is not evidence of this property.
// ELPS cannot inspect a Go closure or method receiver. Returning true for every
// builtin trusts every installed callback; it does not verify their captures.
// Embedders must audit their registrations and run ownership checks on host code.
// The policy runs during publication, not during each NewVM call.
func TemplateWithBuiltinPolicy(approve func(*LVal) bool) TemplateOption {
	return func(c *templateConfig) { c.builtinPolicy = approve }
}

// TemplateWithNativePolicy explicitly approves foreign immutable native payloads
// provided by the embedder. It is an assertion of transitive
// immutability, not a request to clone mutable data. A NativeCloner declaration
// does not make a payload admissible, and no clone callback is invoked. Mutable
// VM references belong in explicit builtin captures; host services are attached
// separately to each instance. Known diagnostic payloads (CallStack, *CallStack,
// and byte-header aliases into the live runtime stack) are always rejected,
// before this policy is consulted.
func TemplateWithNativePolicy(approve func(any) bool) TemplateOption {
	return func(c *templateConfig) { c.nativePolicy = approve }
}

// TemplateWithFrozenPackages freezes the named packages at publication: each
// is shared until its first write, and a write thaws a private copy for that
// VM. A frozen package's symbol, function-name, documentation and export
// tables are built once and shared by every VM the template mints, so NewVM
// does not copy them; each VM gets only its own values for the package's
// bindings. Rebinding a name the package already binds -- set, set! or defun
// of an existing global, including to a new function or closure -- writes
// only that VM's value for it and does not thaw; the function-name entry such
// a write records is kept per VM too. Any other write in a VM -- a new name,
// export, use-package into it, a docstring, or another Go Package mutator --
// copies that one package's tables into the VM and then applies the write
// exactly as it would in an unfrozen package. Other packages, other VMs and the template
// are unaffected, so every program behaves as it does without the option.
// Values bound in a frozen package remain per-VM copies with the usual
// template semantics, and mutating a value never thaws its package. Reads,
// lookups and iteration are unchanged. Naming a package the source does not
// register makes NewTemplate fail. Options accumulate.
func TemplateWithFrozenPackages(names ...string) TemplateOption {
	return func(c *templateConfig) {
		if c.frozen == nil {
			c.frozen = make(map[string]bool, len(names))
		}
		for _, name := range names {
			c.frozen[name] = true
		}
	}
}

// NewTemplate validates env and takes a private snapshot of its mutable state.
// It rejects partially sealed trees, unknown value types, and opaque Go state
// without an explicit immutability contract. In particular, a sealed
// ancestor cannot conceal a mutable descendant (issue #621).
// Retained CallStack or *CallStack payloads, including typed nil, are rejected
// under every value type and cannot be approved by a sharing policy. The live
// Runtime.Stack.GoStack must be nil, and no value may retain its byte-slice
// header, even while nil. Stack-free saved errors and errors generated by a VM
// after construction remain supported.
//
// The source must remain quiescent for this entire call: no evaluation in
// progress, no active call frames or condition handlers, and no concurrent
// mutation of its reachable state by another goroutine or host callback. The
// caller provides this exclusion. Rejecting recorded active evaluation state
// does not lock the source or synchronize with concurrent writers.
//
// Storage discovery uses numeric address arithmetic only at construction to
// identify overlapping backing ranges. It assumes Go's non-moving heap while
// retained source slices remain live. Numeric addresses are never converted
// back to pointers or dereferenced; memory is accessed through ordinary slices.
// The result stores ordinary Go slices and integer offsets, not source addresses.
//
// Initialization must itself be suitable for replay: transaction context, time,
// randomness and external effects must be bound per VM when cold loads do so.
// Iterative admission is bounded by the source Runtime.ValueDepthLimit,
// including closure environments, visible cells and hidden capacity tails.
func NewTemplate(env *LEnv, opts ...TemplateOption) (*Template, error) {
	if env == nil || env.Runtime == nil || env.Runtime.Registry == nil {
		return nil, errors.New("template: nil environment, runtime or registry")
	}
	if err := checkTemplateQuiescent(env.Runtime); err != nil {
		return nil, fmt.Errorf("template: %w", err)
	}
	var config templateConfig
	for _, opt := range opts {
		if opt == nil {
			return nil, errors.New("template: nil option")
		}
		opt(&config)
	}
	for _, name := range sortedTemplateKeys(config.frozen) {
		if env.Runtime.Registry.packages[name] == nil {
			return nil, fmt.Errorf("template: frozen package %q is not registered", name)
		}
	}
	input := newTemplateInventory(config)
	if err := input.scan(env); err != nil {
		return nil, err
	}
	plan, err := compileTemplate(env, input)
	if err != nil {
		return nil, err
	}
	return &Template{plan: plan}, nil
}

// NewVM constructs an independent VM from the template. Mutable values use
// independent allocations, so retaining one returned scalar or byte view does
// not retain unrelated VM storage. Context and stderr may be set per instance.
//
// Unless the template was published with TemplateWithEagerInstantiation, the
// VM is built lazily: package bindings, sorted-map entries and what they reach
// are created on first use, once per VM. A VM must therefore be used by one
// goroutine at a time, including reads such as symbol lookups and map gets
// made by the host; hand it between goroutines only with a happens-before
// edge (a channel send, a mutex). Checked builds (-tags elpscheck) panic when
// two goroutines materialize in one VM concurrently. Any number of goroutines
// may call NewVM on one Template concurrently. A retained sorted map or
// package with unmaterialized entries keeps the whole VM alive, as a retained
// function or environment always has, until its last entry is materialized;
// a fully read map retains only itself.
func (t *Template) NewVM(opts ...VMOption) (*LEnv, error) {
	if t == nil || t.plan.root == 0 {
		return nil, errors.New("template: uninitialized template")
	}
	checkPackageBases(t.plan.packages)
	return t.plan.instantiate(opts)
}

// checkTemplateQuiescent rejects source state left inside an evaluation. The
// caller must also prevent concurrent evaluation throughout publication; this
// check does not synchronize with another goroutine evaluating the source.
func checkTemplateQuiescent(rt *Runtime) error {
	if rt.Stack == nil {
		return errors.New("runtime has no call stack")
	}
	// Recovered panics record their trace on an error's stack copy, never the
	// live runtime stack. Even empty nonnil storage violates that boundary.
	if rt.Stack.GoStack != nil {
		return errors.New("retained diagnostic stack: live runtime GoStack must be nil")
	}
	if n := len(rt.Stack.Frames); n != 0 {
		return fmt.Errorf("source not quiescent: call stack height %d", n)
	}
	if rt.evalDepth != 0 {
		return fmt.Errorf("source not quiescent: eval depth %d", rt.evalDepth)
	}
	if n := len(rt.conditionStack); n != 0 {
		return fmt.Errorf("source not quiescent: %d pending condition handlers", n)
	}
	return nil
}

type templateInventory struct {
	config      templateConfig
	values      map[*LVal]int // positive private index; zero is an admitted shared root
	envs        map[*LEnv]int
	sealed      map[*LVal]bool
	maps        map[*MapData]bool
	byteSeen    map[*[]byte]bool
	runtime     *Runtime
	valueQueue  []*LVal
	envQueue    []*LEnv
	cells       []templateCellSpan
	bytes       []templateByteSpan
	sharedCells []templateCellSpan
	next        templateFrame
}

func newTemplateInventory(config templateConfig) *templateInventory {
	return &templateInventory{
		config: config,
		values: make(map[*LVal]int), envs: make(map[*LEnv]int),
		sealed: make(map[*LVal]bool), maps: make(map[*MapData]bool),
		byteSeen: make(map[*[]byte]bool),
	}
}

func (s *templateInventory) scan(env *LEnv) error {
	s.runtime = env.Runtime
	packages := sortedTemplateKeys(env.Runtime.Registry.packages)
	// Validate root identity before traversing any values or invoking policy.
	// Otherwise a differently named/unregistered current package could be
	// silently normalized or excluded from the published graph.
	for _, name := range packages {
		pkg := env.Runtime.Registry.packages[name]
		if pkg == nil {
			return fmt.Errorf("template: nil package %q", name)
		}
		if pkg.Name != name {
			return fmt.Errorf("template: registry key %q does not match package name %q", name, pkg.Name)
		}
	}
	if current := env.Runtime.Package; current != nil && env.Runtime.Registry.packages[current.Name] != current {
		return fmt.Errorf("template: current package %q is not registered by identity", current.Name)
	}
	if err := s.env(env); err != nil {
		return err
	}
	for _, name := range packages {
		pkg := env.Runtime.Registry.packages[name]
		for _, symbol := range pkg.SymbolNames() {
			value, _ := pkg.lookup(symbol)
			if err := s.val(value); err != nil {
				return fmt.Errorf("template: %s:%s: %w", name, symbol, err)
			}
		}
	}
	return s.checkSharedStorage()
}

func sortedTemplateKeys[V any](m map[string]V) []string {
	keys := make([]string, 0, len(m))
	for key := range m {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	return keys
}

// templateVisit names a node without a closure or an addressable result slot.
type templateVisit struct {
	v    *LVal
	env  *LEnv
	kind uint8
}

const (
	templateVisitValue uint8 = iota
	templateVisitEnv
	templateVisitShared
)

// templateFrame keeps one continuation per ancestor. Children are consumed in
// admission order: captures, environment, map entries, then capacity cells.
// Keys are needed for deterministic policy order; diagnostic text is not built
// until an error occurs. The source pointers here are cursors, not new memos.
type templateFrame struct {
	capture, shared                     *LVal
	env, scope                          *LEnv
	entries                             []templateMapEntry
	smallEntries                        [4]templateMapEntry
	cells                               []*LVal
	nkeys                               int
	index                               int
	stage                               uint8
	visitCapture, visitEnv, sharedCells bool
}

// templateMapEntry is one key/value pair a frame took from a scope or a map
// backing when the walk ENTERED it.
//
// The value is snapshotted with the key rather than read back out of the
// backing when the walk reaches it, which is the copier's contract (see the
// comment on copier.cells) applied to admission: a walk admits the entries a
// container held when the walker entered it. Admission runs embedder policy
// callbacks -- TemplateWithNativePolicy, TemplateWithBuiltinPolicy -- and a
// callback holding the source environment can write the very map under
// admission. Reading live, a callback could delete an entry before the walk
// reached it and have a value admitted that was never inspected, and for a
// jsonMap backing the deleted key's `backing[key].(*LVal)` asserted on a nil
// interface and took the walk down with it.
//
// The pair replaces the frame's key list rather than sitting beside it, so a
// map or scope of any size still costs the ONE slice the key list cost.
type templateMapEntry struct {
	v   *LVal
	key string
}

// Keep tiny entry sets inside the by-value frame. A slice into this array
// would escape when the continuation grows, so select each entry by index.
func (f *templateFrame) entry(i int) templateMapEntry {
	if f.entries != nil {
		return f.entries[i]
	}
	return f.smallEntries[i]
}

func (f *templateFrame) key(i int) string { return f.entry(i).key }

// allocEntries returns empty storage for n entries, inside the frame when it
// fits. The caller appends exactly n and passes the result to commitEntries.
func (f *templateFrame) allocEntries(n int) []templateMapEntry {
	f.nkeys = n
	if n > len(f.smallEntries) {
		f.entries = make([]templateMapEntry, 0, n)
		return f.entries
	}
	return f.smallEntries[:0]
}

// commitEntries installs the filled storage and orders it by key, so that the
// policy callbacks a walk makes run in an order that depends only on what the
// container held and not on Go's map iteration order.
func (f *templateFrame) commitEntries(entries []templateMapEntry) {
	if f.entries != nil {
		f.entries = entries
	}
	slices.SortFunc(entries, func(a, b templateMapEntry) int {
		return strings.Compare(a.key, b.key)
	})
}

// setTemplateFrameEntries snapshots an LVal-valued scope or map backing.
func setTemplateFrameEntries(f *templateFrame, values map[string]*LVal) {
	entries := f.allocEntries(len(values))
	for key, v := range values {
		entries = append(entries, templateMapEntry{key: key, v: v})
	}
	f.commitEntries(entries)
}

func (f *templateFrame) empty() bool {
	return !f.visitCapture && f.shared == nil && !f.visitEnv && f.nkeys == 0 && len(f.cells) == 0
}

func (f *templateFrame) child() (templateVisit, bool) {
	for {
		switch f.stage {
		case 0:
			f.stage++
			if f.shared != nil {
				return templateVisit{v: f.shared, kind: templateVisitShared}, true
			}
		case 1:
			f.stage++
			if f.visitCapture {
				return templateVisit{v: f.capture}, true
			}
		case 2:
			f.stage++
			if f.visitEnv && f.scope == nil {
				return templateVisit{env: f.env, kind: templateVisitEnv}, true
			}
		case 3:
			if f.index < f.nkeys {
				entry := f.entry(f.index)
				f.index++
				return templateVisit{v: entry.v}, true
			}
			f.index = 0
			f.stage++
		case 4:
			f.stage++
			if f.scope != nil {
				return templateVisit{env: f.env, kind: templateVisitEnv}, true
			}
		case 5:
			if f.index < len(f.cells) {
				v := f.cells[f.index]
				f.index++
				kind := templateVisitValue
				if f.sharedCells {
					kind = templateVisitShared
				}
				return templateVisit{v: v, kind: kind}, true
			}
			return templateVisit{}, false
		}
	}
}

// wrapTemplatePath formats the active ancestors only on rejection, then wraps
// the leaf once. Per-ancestor wrapping retains quadratic diagnostic text.
// Keep at most 32 scope/capture entries and 1024 bytes of path (excluding the
// omission marker and leaf error), so deep graphs and long names stay bounded.
func wrapTemplatePath(pending []templateFrame, err error) error {
	var path strings.Builder
	shown, omitted := 0, 0
	for i := range pending {
		f := &pending[i]
		var prefix, name string
		switch {
		case f.stage == 2 && f.visitCapture:
			name = "builtin captures"
		case f.stage == 3 && f.scope != nil && f.index > 0:
			prefix, name = "scope ", f.key(f.index-1)
		default:
			continue
		}
		if omitted > 0 || shown >= 32 || len(prefix)+len(name)+2 > 1024-path.Len() {
			omitted++
			continue
		}
		path.WriteString(prefix)
		path.WriteString(name)
		path.WriteString(": ")
		shown++
	}
	if omitted > 0 {
		fmt.Fprintf(&path, "... (%d more): ", omitted)
	}
	if path.Len() == 0 {
		return err
	}
	return fmt.Errorf("%s%w", path.String(), err)
}

func (s *templateInventory) node(visit templateVisit) error {
	s.next = templateFrame{}
	switch visit.kind {
	case templateVisitEnv:
		return s.envNode(visit.env)
	case templateVisitShared:
		return s.sharedNode(visit.v)
	default:
		return s.valNode(visit.v)
	}
}

func (s *templateInventory) walk(visit templateVisit) error {
	// A leaf or memo hit must not initialize any continuation storage.
	if err := s.node(visit); err != nil {
		return err
	}
	if s.next.empty() {
		return nil
	}
	pending := make([]templateFrame, 0, 16)
	pending = append(pending, s.next)
	var err error
	for len(pending) > 0 {
		f := &pending[len(pending)-1]
		child, ok := f.child()
		if !ok {
			*f = templateFrame{}
			pending = pending[:len(pending)-1]
			continue
		}
		if len(pending) >= s.runtime.ValueDepthLimit() {
			err = ValueDepthError(s.runtime.ValueDepthLimit())
			break
		}
		if err = s.node(child); err != nil {
			break
		}
		if !s.next.empty() {
			pending = append(pending, s.next)
		}
	}
	s.next = templateFrame{}
	if err != nil {
		return wrapTemplatePath(pending, err)
	}
	return err
}

func (s *templateInventory) env(env *LEnv) error {
	return s.walk(templateVisit{env: env, kind: templateVisitEnv})
}
func (s *templateInventory) val(v *LVal) error {
	return s.walk(templateVisit{v: v})
}

func (s *templateInventory) envNode(env *LEnv) error {
	if env == nil {
		return nil
	}
	if _, seen := s.envs[env]; seen {
		return nil
	}
	if env.Runtime != s.runtime {
		return errors.New("captured environment belongs to another runtime")
	}
	s.envs[env] = len(s.envQueue) + 1
	s.envQueue = append(s.envQueue, env)
	s.next = templateFrame{scope: env, env: env.parent, visitEnv: true}
	setTemplateFrameEntries(&s.next, env.scope)
	return nil
}

// The known diagnostic category is independent of the LVal header and of host
// approval policies. Keep both mutable and sealed admission on this boundary.
func (s *templateInventory) checkDiagnosticPayload(payload any) error {
	switch payload := payload.(type) {
	case CallStack, *CallStack:
		return errors.New("retained diagnostic stack is not supported by templates")
	case *[]byte:
		if s.runtime != nil && s.runtime.Stack != nil && payload == &s.runtime.Stack.GoStack {
			// Identity matters even for a nil/zero-capacity slice: appending via
			// this header can otherwise change IsInternalPanic's marker (#629).
			return errors.New("retained diagnostic stack: byte header aliases live runtime GoStack")
		}
	}
	return nil
}

func (s *templateInventory) sharedNode(v *LVal) error {
	if v == nil || s.sealed[v] {
		return nil
	}
	if err := s.checkDiagnosticPayload(v.Native); err != nil {
		return err
	}
	if !v.sealed || !sealableNodeType(v.Type) || v.Native != nil {
		return fmt.Errorf("sealed graph reaches mutable or opaque %s", v.Type)
	}
	// Debug metadata can reach unsealed macro arguments, including closures
	// over the source VM. The shared header cannot drop that edge as a mutable
	// descriptor does, so it must not be admitted as immutable syntax (#621).
	if v.macroExpansion != nil {
		return errors.New("sealed graph contains macro expansion metadata")
	}
	s.sealed[v] = true
	if cap(v.Cells) > 0 {
		s.sharedCells = append(s.sharedCells, newTemplateCellSpan(v))
	}
	// A Go caller can reslice up to capacity. SealAST visits visible children
	// only, so the seal bit is not evidence about a hidden capacity tail.
	s.next.cells = v.Cells[:cap(v.Cells)]
	s.next.sharedCells = true
	return nil
}

func (s *templateInventory) valNode(v *LVal) error {
	if v == nil {
		return nil
	}
	// Saved diagnostics can alias frames, locations and ordinary byte storage.
	// Reject the category, including typed nil and non-error headers, before
	// either a sharing policy or a header-specific path can admit it (#629).
	if err := s.checkDiagnosticPayload(v.Native); err != nil {
		return err
	}
	if _, seen := s.values[v]; seen {
		return nil
	}
	if v.sealed {
		s.values[v] = 0
		s.next.shared = v
		return nil
	}
	if isSingleton(v) {
		s.values[v] = 0
	} else {
		s.values[v] = len(s.valueQueue) + 1
		s.valueQueue = append(s.valueQueue, v)
	}
	if v.Type <= LInvalid || v.Type >= LMarkTerminal {
		return fmt.Errorf("unsupported value type %s", v.Type)
	}
	switch v.Type {
	case LFun:
		fd, ok := v.Native.(*funData)
		if !ok || fd == nil {
			return errors.New("function has no function data")
		}
		if fd.captures != nil {
			s.next.capture, s.next.visitCapture = fd.captures.values, true
		} else if fd.builtin != nil && (s.config.builtinPolicy == nil || !s.config.builtinPolicy(v)) {
			return fmt.Errorf("builtin %s:%s has no template sharing declaration", fd.pkg, fd.fid)
		}
		s.next.env, s.next.visitEnv = fd.env, true
	case LNative:
		if err := s.native(v.Native); err != nil {
			return err
		}
	default:
		switch payload := v.Native.(type) {
		case nil:
		case *MapData:
			if err := s.mapData(payload); err != nil {
				return err
			}
		case *[]byte:
			if payload == nil {
				return errors.New("nil byte payload")
			}
			if !s.byteSeen[payload] {
				s.byteSeen[payload] = true
				if cap(*payload) > 0 {
					s.bytes = append(s.bytes, newTemplateByteSpan(payload))
				}
			}
		default:
			if err := s.native(payload); err != nil {
				return err
			}
		}
	}
	// Function bodies whose cells are all shared have immutable backing under
	// the existing function construction contract. Keep that fast path.
	sharedCells := v.Type == LFun && cap(v.Cells) == len(v.Cells)
	if sharedCells {
		for _, child := range v.Cells {
			if child != nil && !child.sealed {
				sharedCells = false
				break
			}
		}
	}
	if cap(v.Cells) > 0 && !sharedCells {
		s.cells = append(s.cells, newTemplateCellSpan(v))
		// Capacity can expose additional cells through a later append or host
		// reslice. Validate and remap those references as part of the storage.
		s.next.cells = v.Cells[:cap(v.Cells)]
	} else {
		if cap(v.Cells) > 0 {
			s.sharedCells = append(s.sharedCells, newTemplateCellSpan(v))
		}
		s.next.cells = v.Cells
	}
	return nil
}

// A mutable wrapper over shared program storage is not made safe by copying
// only that wrapper: it changes the cold program's aliases. Reject this shape
// at admission, including wrappers over a function's shared body/formals.
func (s *templateInventory) checkSharedStorage() error {
	slices.SortFunc(s.cells, compareTemplateCellSpans)
	if len(s.cells) == 0 || len(s.sharedCells) == 0 {
		return nil
	}
	// Shared program spans usually greatly outnumber mutable spans. Index
	// the mutable side instead of sorting the whole shared program again.
	// Prefix maxima retain earlier containing spans when views are nested.
	maxEnds := make([]uintptr, len(s.cells))
	var end uintptr
	for i, span := range s.cells {
		end = max(end, span.end)
		maxEnds[i] = end
	}
	for _, shared := range s.sharedCells {
		// Half-open spans overlap iff a mutable start is before shared.end
		// and that prefix contains an end strictly after shared.start.
		i := sort.Search(len(s.cells), func(i int) bool { return s.cells[i].start >= shared.end })
		if i > 0 && maxEnds[i-1] > shared.start {
			return errors.New("template: mutable cells backing overlaps shared program storage")
		}
	}
	return nil
}

func (s *templateInventory) native(payload any) error {
	if payload == nil {
		return nil
	}
	// Pointer method sets inherit value markers, but a caller can replace a
	// whole pointee even when all its fields are private. Only audited struct
	// values receive automatic admission; pointer forms require host policy.
	if _, ok := payload.(templatepolicy.Immutable); ok && reflect.TypeOf(payload).Kind() == reflect.Struct {
		return nil
	}
	// Only actual scalar values have no hidden reference graph. A value struct
	// containing a map or slice is not made immutable by being held by value.
	switch reflect.TypeOf(payload).Kind() {
	case reflect.Bool, reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64,
		reflect.Float32, reflect.Float64, reflect.Complex64, reflect.Complex128, reflect.String:
		return nil
	case reflect.Invalid, reflect.Uintptr, reflect.Array, reflect.Chan, reflect.Func,
		reflect.Interface, reflect.Map, reflect.Pointer, reflect.Slice, reflect.Struct, reflect.UnsafePointer:
		// These kinds may hide mutable references or process-local identities.
	}
	if s.config.nativePolicy != nil && s.config.nativePolicy(payload) {
		return nil
	}
	return fmt.Errorf("native %T has no template immutability declaration", payload)
}

func (s *templateInventory) mapData(data *MapData) error {
	if data == nil || s.maps[data] {
		return nil
	}
	s.maps[data] = true
	switch backing := data.mapBacking.(type) {
	case nil:
		return nil
	case sortedmap:
		// Keys and type flags are Go scalars, not source value identities.
		// Avoid manufacturing temporary keys/pairs solely to discard them.
		// Republishing a lazily instantiated VM: materialize pending entries
		// before the direct table read.
		backing.forceAll()
		setTemplateFrameEntries(&s.next, backing.m)
	case jsonMap:
		if backing == nil {
			return errors.New("nil JSON map is not writable")
		}
		// Reject malformed decoder storage before invoking admission
		// callbacks, and snapshot in the same pass: the type assertion and
		// the value the walk later admits then read the backing once,
		// together, before any callback can change it.
		entries := s.next.allocEntries(len(backing))
		// Which malformed entry is REPORTED must not depend on Go's map
		// iteration order, so the scan notes the offender with the smallest
		// key rather than the first one it happens to meet, which is the
		// entry the old key-sorted validation pass named.
		var bad templateMapEntry
		var badPayload any
		malformed := false
		for key, x := range backing {
			v, ok := x.(*LVal)
			if !ok && (!malformed || key < bad.key) {
				bad, badPayload, malformed = templateMapEntry{key: key}, x, true
			}
			entries = append(entries, templateMapEntry{key: key, v: v})
		}
		if malformed {
			return fmt.Errorf("JSON map entry %q is not an LVal: %T", bad.key, badPayload)
		}
		s.next.commitEntries(entries)
	default:
		return fmt.Errorf("map backing %T is not interpreter-owned", data.mapBacking)
	}
	return nil
}
