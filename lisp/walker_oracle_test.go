// Copyright © 2026 The ELPS authors

package lisp

import (
	"encoding/json"
	"errors"
	"fmt"
	"reflect"
	"slices"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/fmtmeta"
	"github.com/luthersystems/elps/parser/token"
)

// Issues #598 and #600: one behavioral oracle drives the real entry points,
// including the debugger-only stamp path. Keep this private: detach and the
// stamper are kernel operations, not new embedding APIs.
//
// The contract is NOT identical across walkers. Copying preserves container
// headers/payloads, but not immutable leaf identity or overlapping cell slots.
// The stamper shares value payloads and may duplicate acyclic syntax headers;
// it owns only the newly stamped syntax/metadata. Templates preserve cell slots
// and share only admitted immutable code/natives. Existing field/registry source
// scans complement these finite behavioral graphs; this is not a claim to model
// arbitrary Go payloads or every evaluator state.
type oracleWalker struct {
	name, owners string
	makeCopies   func(*LVal) (*LVal, *LVal, error)
	stamps       bool
	template     bool
}

func oracleWalkers() []oracleWalker {
	copyTwice := func(copyValue func(*LVal) (*LVal, error)) func(*LVal) (*LVal, *LVal, error) {
		return func(v *LVal) (*LVal, *LVal, error) {
			a, err := copyValue(v)
			if err != nil {
				return nil, nil, err
			}
			b, err := copyValue(v)
			return a, b, err
		}
	}
	return []oracleWalker{
		{name: "copy", owners: "detacher", makeCopies: copyTwice(func(v *LVal) (*LVal, error) {
			cp := builtinCopy(NewEnv(nil), SExpr([]*LVal{v}))
			return cp, GoError(cp)
		})},
		{name: "detach", owners: "detacher", makeCopies: copyTwice((*LVal).detach)},
		{name: "LVal.Copy", owners: "copier", makeCopies: copyTwice(func(v *LVal) (*LVal, error) {
			cp := v.Copy()
			return cp, GoError(cp)
		})},
		{name: "macro stamp", owners: "macroStamper", stamps: true, makeCopies: copyTwice(func(v *LVal) (*LVal, error) {
			loc := &token.Location{File: "oracle-call.lisp", Pos: 17, Line: 3, Col: 4}
			ctx := &macroExpansionContext{Name: "oracle-macro", CallSite: loc, Args: []*LVal{v}}
			return stampMacroExpansion(v, loc, ctx, StandardRuntime()), nil
		})},
		{name: "Template.NewVM", owners: "templateInventory templateCompiler", template: true, makeCopies: oracleTemplateCopies},
	}
}

func oracleTemplateCopies(v *LVal) (*LVal, *LVal, error) {
	return oracleTemplateCopiesWithOptions(v)
}

func oracleTemplateCopiesWithOptions(v *LVal, opts ...TemplateOption) (*LVal, *LVal, error) {
	env := NewEnv(nil)
	env.Runtime.Package = env.Runtime.Registry.DefinePackage("user")
	if rc := env.PutGlobal(Symbol("oracle-value"), v); rc.Type == LError {
		return nil, nil, GoError(rc)
	}
	// No builtin or foreign native approval: this isolates the graph walker
	// without paying for a stdlib bootstrap on every fuzz input.
	tmpl, err := NewTemplate(env, opts...)
	if err != nil {
		return nil, nil, err
	}
	a, err := tmpl.NewVM()
	if err != nil {
		return nil, nil, err
	}
	b, err := tmpl.NewVM()
	if err != nil {
		return nil, nil, err
	}
	return a.Get(Symbol("oracle-value")), b.Get(Symbol("oracle-value")), nil
}

type oracleNative struct{ n int }

func (v *oracleNative) CloneNative() interface{} { return &oracleNative{n: v.n} }

// Equal-content distinct payloads are deliberate: an oracle that compares only
// values, or assumes every equal value aliases, must fail the negative controls.
func oracleGraph(seed byte, native bool) *LVal {
	m := SortedMap()
	m.Map().Set(String("n"), Int(7))
	n := SortedMap()
	n.Map().Set(String("n"), Int(7))
	b := Bytes([]byte{7, seed})
	m.Map().Set(String("bytes"), b)
	n.Map().Set(String("bytes"), Bytes([]byte{7, seed}))
	leaf := SExpr([]*LVal{m, Quote(m), n, b, Quote(b), Bytes([]byte{7, seed})})
	if native {
		p := Native(&oracleNative{n: 7})
		leaf.Cells = append(leaf.Cells, p, Quote(p), Native(&oracleNative{n: 7}))
	}
	scalarMap := SortedMap()
	scalarMap.Map().Set(String("n"), Int(7))
	root := SExpr([]*LVal{leaf, leaf, Array(nil, []*LVal{m, b}), SExpr([]*LVal{Int(9)}), scalarMap, Array(nil, []*LVal{Int(13)})})
	for i := range int(seed % 4) {
		root.Cells = append(root.Cells, SExpr([]*LVal{Int(i), leaf}))
	}
	// A syntax back-edge forces the stamper's memoized cyclic rerun. The
	// non-cyclic seeds exercise its optimistic, non-memoized fast path too.
	if seed&4 != 0 {
		root.Cells = append(root.Cells, root)
	}
	root.quoted = seed&8 != 0
	root.spliced = seed&16 != 0
	root.source = &token.Location{File: "oracle-source.lisp", Pos: -1, Line: 7, Col: 2}
	root.meta = &fmtmeta.Meta{OriginalText: "oracle", TrailingComment: &token.Token{
		Text: "; retained comment", Source: &token.Location{File: "comment.lisp", Line: 7, Col: 10},
	}}
	root.macroExpansion = &macroExpansionInfo{
		macroExpansionContext: &macroExpansionContext{
			Name: "old", Args: []*LVal{leaf},
			CallSite: &token.Location{File: "old-call.lisp", Pos: 1, EndCol: 3},
			DefSite:  &token.Location{File: "old-def.lisp", Pos: 2, EndCol: 5},
		}, ID: 91,
	}
	return root
}

type oracleNode struct {
	v       *LVal
	path    string
	state   string
	payload any
}

// Traverse paths independently on each arm, stopping back-edges only on the
// active path. A lost alias therefore cannot hide nodes behind the SOURCE's
// visited set. Bounds turn an incorrectly unrolled cycle into a witness.
func oracleNodes(root *LVal) ([]oracleNode, error) {
	var out []oracleNode
	active := make(map[*LVal]bool)
	var visit func(*LVal, string) error
	visit = func(v *LVal, path string) error {
		if len(out) >= 512 || len(path) > 1024 {
			return fmt.Errorf("graph traversal exceeded bound at %s", path)
		}
		if v == nil {
			out = append(out, oracleNode{path: path, state: "nil"})
			return nil
		}
		// JSON walks token/location fields by value, not pointer address: an
		// in-place metadata-only write must change the saved observation.
		metadata, err := json.Marshal([]any{v.source, v.meta})
		if err != nil {
			return err
		}
		state := fmt.Sprintf("%d/%d/%q/%d/%g/%t/%t/%t/%s", v.Type, v.FunType, v.Str, v.Int, v.Float, v.quoted, v.spliced, v.sealed, metadata)
		if info := v.macroExpansion; info != nil {
			locations, err := json.Marshal([]any{info.CallSite, info.DefSite})
			if err != nil {
				return err
			}
			state += fmt.Sprintf("/macro=%p/%p/%d/%s/%s", info, info.macroExpansionContext, info.ID, info.Name, locations)
			var args strings.Builder
			for _, arg := range info.Args {
				fmt.Fprintf(&args, "/arg=%p", arg)
			}
			state += args.String()
		}
		var payload any
		switch p := v.Native.(type) {
		case *MapData, *[]byte:
			payload = p
		case *oracleNative:
			payload = p
			state += fmt.Sprintf("/native=%d", p.n)
		default:
			// Annotations are observed under EVERY header tag, not only
			// LNative. Kernel map/bytes dispatch above still reaches cells.
			state += fmt.Sprintf("/native=%T:%v", p, p)
		}
		if v.Type == LBytes {
			state += fmt.Sprintf("/bytes=%v", v.Bytes())
		}
		out = append(out, oracleNode{v: v, path: path, state: state, payload: payload})
		if active[v] {
			return nil
		}
		active[v] = true
		defer delete(active, v)
		for i, c := range v.Cells {
			if err := visit(c, fmt.Sprintf("%s/c%d", path, i)); err != nil {
				return err
			}
		}
		if v.Type == LSortMap {
			entries := sortedMapEntries(v.Map())
			if entries.Type == LError {
				return GoError(entries)
			}
			for _, pair := range entries.Cells {
				if err := visit(pair.Cells[1], fmt.Sprintf("%s/m%q", path, pair.Cells[0].Str)); err != nil {
					return err
				}
			}
		}
		return nil
	}
	err := visit(root, "root")
	return out, err
}

func oracleSnapshot(root *LVal) (string, error) {
	nodes, err := oracleNodes(root)
	var out strings.Builder
	for _, n := range nodes {
		fmt.Fprintf(&out, "%s:%p:%p:%s\n", n.path, n.v, n.payload, n.state)
	}
	return out.String(), err
}

func oracleCompare(w oracleWalker, source, copyValue *LVal) error {
	src, err := oracleNodes(source)
	if err != nil {
		return err
	}
	dst, err := oracleNodes(copyValue)
	if err != nil {
		return err
	}
	if len(src) != len(dst) {
		return fmt.Errorf("topology: %d source paths, %d output paths", len(src), len(dst))
	}
	for i, s := range src {
		d := dst[i]
		if s.path != d.path || (s.v == nil) != (d.v == nil) {
			return fmt.Errorf("topology at %s", s.path)
		}
		if s.v == nil {
			continue
		}
		a, b := s.v, d.v
		if len(a.Cells) != len(b.Cells) {
			return fmt.Errorf("cell arity at %s", s.path)
		}
		if a.Type != b.Type || a.FunType != b.FunType || a.Int != b.Int || a.Float != b.Float || a.Str != b.Str || a.quoted != b.quoted || a.spliced != b.spliced {
			return fmt.Errorf("value/flags at %s", s.path)
		}
		if !w.stamps && b.macroExpansion != nil {
			return fmt.Errorf("retained macro metadata at %s", s.path)
		}
		if !w.stamps && (!reflect.DeepEqual(a.source, b.source) || !reflect.DeepEqual(a.meta, b.meta)) {
			return fmt.Errorf("source/format metadata at %s", s.path)
		}
		if w.stamps && a != b && needsStamp(a) {
			info := b.macroExpansion
			if info == nil || info.Name != "oracle-macro" || info.ID <= 0 || b.source == nil || b.source.File != "oracle-call.lisp" || b.source.Pos != 17 {
				return fmt.Errorf("missing descendant debug stamp at %s", s.path)
			}
		}
		if a.Type == LBytes && !slices.Equal(a.Bytes(), b.Bytes()) {
			return fmt.Errorf("byte contents at %s", s.path)
		}
		if p, ok := a.Native.(*oracleNative); ok {
			q, ok := b.Native.(*oracleNative)
			if !ok || p.n != q.n {
				return fmt.Errorf("native contents at %s", s.path)
			}
		}
		if !w.stamps && (len(a.Cells) > 0 || a.Native != nil) && a == b {
			return fmt.Errorf("header ownership at %s", s.path)
		}
		if s.payload != nil {
			if (s.payload == d.payload) != w.stamps {
				return fmt.Errorf("payload ownership at %s", s.path)
			}
		}
		for j := range i {
			if s.payload != nil && src[j].payload != nil && (s.payload == src[j].payload) != (d.payload == dst[j].payload) {
				return fmt.Errorf("payload alias at %s and %s", s.path, src[j].path)
			}
			// Leaf identity and stamp DAG identity are not contracts.
			if !w.stamps && (len(a.Cells) > 0 || a.Native != nil) && (a == src[j].v) != (b == dst[j].v) {
				return fmt.Errorf("header alias at %s and %s", s.path, src[j].path)
			}
		}
	}
	return nil
}

func oracleCheck(w oracleWalker, source *LVal) error {
	before, err := oracleSnapshot(source)
	if err != nil {
		return err
	}
	a, b, err := w.makeCopies(source)
	if err != nil {
		return err
	}
	after, err := oracleSnapshot(source)
	if err != nil {
		return fmt.Errorf("source traversal after construction: %w", err)
	}
	if before != after {
		return errors.New("source changed during construction")
	}
	for _, result := range []*LVal{a, b} {
		if err := oracleCompare(w, source, result); err != nil {
			return err
		}
	}
	if w.stamps {
		if a == source || a.macroExpansion == nil || a.macroExpansion.Name != "oracle-macro" || a.macroExpansion.ID != 1 || a.source == nil || a.source.File != "oracle-call.lisp" {
			return errors.New("debug stamp path did not execute")
		}
		if a.macroExpansion == b.macroExpansion || a.source == b.source {
			return errors.New("sibling expansions share writable metadata")
		}
		first, _ := oracleNodes(a) // Both traversals were validated above.
		second, _ := oracleNodes(b)
		original, _ := oracleNodes(source)
		for i, n := range first {
			if n.v != nil && n.v != original[i].v && n.v.macroExpansion != nil && n.v.macroExpansion == second[i].v.macroExpansion {
				return fmt.Errorf("sibling expansions share descendant metadata at %s", n.path)
			}
		}
		return nil // Shared value payloads are intentional; never mutate them here.
	}
	// All three directions: copy -> source, copy -> sibling, source -> copies.
	// Snapshot each unaffected arm BEFORE every write; do not compare two copies
	// with one another and accidentally accept both leaking in the same way.
	arms := []*LVal{a, b, source}
	for i, arm := range arms {
		var old [3]string
		for j, other := range arms {
			old[j], err = oracleSnapshot(other)
			if err != nil {
				return fmt.Errorf("snapshot arm %d before writes to arm %d: %w", j, i, err)
			}
		}
		nodes, err := oracleNodes(arm)
		if err != nil {
			return fmt.Errorf("probe census for arm %d before writes: %w", i, err)
		}
		dimensions := make(map[*LVal]bool)
		for _, n := range nodes {
			if n.v != nil && n.v.Type == LArray {
				dimensions[n.v.Cells[0]] = true
			}
		}
		writes := 0
		// Template source/format metadata is private immutable program state
		// and shared by contract, including with the source (field policy).
		// Do not invent an in-place kernel write as a VM operation. Copy/detach
		// instead own mutable metadata, so exercise all directions there.
		if !w.template {
			if arm.source != nil {
				arm.source.Line++
			}
			if arm.meta != nil && arm.meta.TrailingComment != nil {
				arm.meta.TrailingComment.Source.Line++
			}
		} else {
			// SetSource replaces a mutable header's location; it does not
			// write the frozen location object shared by the plan.
			arm.SetSource(&token.Location{File: "probe.lisp", Line: 100 + i})
		}
		for _, n := range nodes {
			if n.v == nil {
				continue
			}
			if n.v.Type == LSExpr && len(n.v.Cells) > 0 && !dimensions[n.v] && !n.v.IsSealed() {
				// Lists and array backing lists: write slots, not just the
				// values stored in them. Take the census first so disconnecting
				// a graph edge cannot silently remove later probe sites.
				n.v.Cells[0] = Int(200 + i)
				writes++
			}
			switch p := n.payload.(type) {
			case *MapData:
				p.Set(String("probe"), Int(100+i))
				writes++
			case *[]byte:
				(*p)[0] = byte(100 + i)
				writes++
			case *oracleNative:
				p.n = 100 + i
				writes++
			}
		}
		if writes == 0 {
			return errors.New("isolation probe performed no writes")
		}
		for j, other := range arms {
			now, err := oracleSnapshot(other)
			if err != nil {
				return fmt.Errorf("snapshot arm %d after writes to arm %d: %w", j, i, err)
			}
			if j != i && now != old[j] {
				return fmt.Errorf("isolation: arm %d changed arm %d", i, j)
			}
			if j == i && now == old[j] {
				return errors.New("isolation probe did not change its target")
			}
		}
	}
	return nil
}

func TestWalkerBehaviorOracle(t *testing.T) {
	for _, w := range oracleWalkers() {
		t.Run(w.name, func(t *testing.T) {
			for _, seed := range []byte{0, 3, 4, 8, 16, 31} {
				if err := oracleCheck(w, oracleGraph(seed, !w.template)); err != nil {
					t.Fatalf("seed %d: %v", seed, err)
				}
			}
		})
	}
}

func TestWalkerBehaviorOracleCoversRegistry(t *testing.T) {
	var registered, driven []string
	nodes, err := oracleNodes(oracleGraph(0, true))
	if err != nil {
		t.Fatal(err)
	}
	exercised := make(map[payloadKind]bool)
	for _, n := range nodes {
		switch n.payload.(type) {
		case *MapData:
			exercised[payloadSortedMap] = true
		case *[]byte:
			exercised[payloadBytes] = true
		case *oracleNative:
			exercised[payloadNative] = true
		}
	}
	for _, m := range registeredWalkerMemos() {
		registered = append(registered, m.Walker)
		for _, kind := range m.Payloads {
			if !exercised[kind] {
				t.Errorf("%s memoizes %s but the behavioral graph does not exercise it", m.Walker, kind)
			}
		}
	}
	for _, w := range oracleWalkers() {
		driven = append(driven, strings.Fields(w.owners)...)
	}
	slices.Sort(registered)
	slices.Sort(driven)
	driven = slices.Compact(driven)
	if !reflect.DeepEqual(registered, driven) {
		t.Fatalf("registered walkers %v != behaviorally driven walkers %v", registered, driven)
	}
}

func FuzzWalkerBehaviorOracle(f *testing.F) {
	for i := range oracleWalkers() {
		for _, seed := range []byte{0, 4, 31} {
			f.Add(byte(i), []byte{seed, 0, 17, 34, 51})
		}
	}
	f.Fuzz(func(t *testing.T, which byte, data []byte) {
		walkers := oracleWalkers()
		w := walkers[int(which)%len(walkers)]
		var seed byte
		if len(data) > 0 {
			seed = data[0]
		}
		root := oracleGraph(seed, !w.template)
		payloads := root.Cells[0].Cells
		// A bounded graph program: choose alias groups, distinct headers,
		// nesting and array edges independently. It is not evaluator fuzzing
		// and cannot consume an unbounded execution budget.
		for _, op := range data[:min(len(data), 8)] {
			v := payloads[int(op)%len(payloads)]
			switch op >> 4 & 3 {
			case 1:
				v = Quote(v)
			case 2:
				v = SExpr([]*LVal{v, v})
			case 3:
				v = Array(nil, []*LVal{v})
			}
			root.Cells = append(root.Cells, v)
		}
		if err := oracleCheck(w, root); err != nil {
			t.Fatalf("%s program %x: %v", w.name, data[:min(len(data), 8)], err)
		}
	})
}
