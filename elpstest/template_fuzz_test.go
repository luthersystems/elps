// Copyright © 2026 The ELPS authors

package elpstest

import (
	"errors"
	"fmt"
	"reflect"
	"slices"
	"strconv"
	"strings"
	"sync"
	"testing"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/lisp/lisplib/libschema"
	"github.com/luthersystems/elps/parser"
)

// FuzzForkParity exercises #625's governing property, not merely a copying
// mechanism: n independent cold loads versus n instances of ONE published plan.
// Inputs select a bounded, always-valid program and multiple transactions per VM.
// There are no skip/reject paths. Every transaction has an independently computed
// result/state observation and host-event trace, including deliberately raised
// errors. Each input selects one lazy, eagerly interleaved, or concurrent
// schedule: running all three per execution can exceed the fuzz worker watchdog.
// Ordinary tests replay the seed/schedule pairs not selected by this target.
//
// The historical #601 shapes remain: cdr/rest/slice views, in-place sorting,
// vector append through a view, headers created inside a transaction, map aliases,
// shared lexical scopes, and two-hop forks. Explicit captures/cycles, JSON key
// policy, native request state and model-pinned errors extend that coverage.
func FuzzForkParity(f *testing.F) {
	for _, seed := range templateParitySeeds() {
		f.Add(seed)
	}
	f.Fuzz(func(t *testing.T, data []byte) {
		g := generateTemplateParity(data)
		schedule := templateParitySchedule(data)
		if err := runTemplateParity(g, schedule, nil); err != nil {
			t.Fatalf("schedule=%s script=%x: %v\nprogram:\n%s", schedule, data, err, g.program)
		}
	})
}

var templateParitySchedules = []string{"lazy", "interleaved", "concurrent"}

func templateParitySchedule(data []byte) string {
	if len(data) == 0 {
		return templateParitySchedules[0]
	}
	// Do not strip or prepend selector bytes: existing minimized inputs must
	// still generate exactly the same program and transaction sequence.
	return templateParitySchedules[int(data[len(data)-1])%len(templateParitySchedules)]
}

func templateParityComplementarySchedules(data []byte) []string {
	var schedules []string
	for _, schedule := range templateParitySchedules {
		if schedule != templateParitySchedule(data) {
			schedules = append(schedules, schedule)
		}
	}
	return schedules
}

const templateParityActions = 18

type templateParityModel struct {
	xs                                   []int
	view                                 []int
	mapN, closure, capture, raw, allowed int
	jsonN                                int
	bytes                                string
	hostBytes                            []byte
	literal                              string
	graphNodes, graphEdges               [3]int
}

func (m templateParityModel) clone() templateParityModel {
	m.xs, m.view, m.hostBytes = slices.Clone(m.xs), slices.Clone(m.view), slices.Clone(m.hostBytes)
	return m
}

func parityInts(ns []int) []*lisp.LVal {
	vals := make([]*lisp.LVal, len(ns))
	for i, n := range ns {
		vals[i] = lisp.Int(n)
	}
	return vals
}

func (m templateParityModel) observation() *lisp.LVal {
	var edges, twoHop [3]int
	for i, edge := range m.graphEdges {
		edges[i] = m.graphNodes[edge]
		twoHop[i] = m.graphNodes[m.graphEdges[edge]]
	}
	return lisp.QExpr([]*lisp.LVal{
		lisp.Int(m.mapN), lisp.Int(m.mapN), lisp.Int(m.mapN),
		lisp.String(m.bytes), lisp.String(m.bytes),
		lisp.QExpr(parityInts(m.xs)), lisp.QExpr(parityInts(m.xs[1:])),
		lisp.QExpr(parityInts(m.xs[:2])), lisp.QExpr(parityInts(m.xs[1:])),
		lisp.Vector(parityInts([]int{7, 8, 9})), lisp.Vector(parityInts(m.view)),
		lisp.Int(m.closure), lisp.Int(m.capture), lisp.Int(m.capture),
		lisp.String(string(m.hostBytes[:3])), lisp.String(string(m.hostBytes[2:5])),
		lisp.Int(m.raw), lisp.Int(m.raw), lisp.Int(m.allowed), lisp.Nil(),
		lisp.Int(m.jsonN), lisp.Int(m.jsonN), lisp.String(m.literal),
		lisp.QExpr(parityInts(m.graphNodes[:])), lisp.QExpr(parityInts(edges[:])), lisp.QExpr(parityInts(twoHop[:])),
	})
}

type templateParityTx struct {
	source, condition, message string
	observation                *lisp.LVal
	effects                    []int
	action                     int
	native                     bool
}

type templateParityCase struct {
	program string
	initial templateParityModel
	tx      [][]templateParityTx
	hops    int
}

func generateTemplateParity(data []byte) templateParityCase {
	// Bound work even for a huge fuzz input; the remaining bytes are irrelevant.
	data = data[:min(len(data), 128)]
	byteAt := func(i int) int {
		if len(data) == 0 {
			return 0
		}
		return int(data[i%len(data)])
	}
	initial := templateParityModel{
		xs:   []int{30 + byteAt(3)%20, 10 + byteAt(4)%20, 20 + byteAt(5)%20},
		view: []int{7}, mapN: byteAt(6) % 50, closure: byteAt(7) % 50,
		capture: byteAt(8) % 50, raw: byteAt(9) % 50, allowed: byteAt(10) % 50,
		jsonN: byteAt(11) % 50, bytes: "ab", hostBytes: []byte("ABCDEF"),
		literal:    fmt.Sprintf("_fun%d/_validation_fun_%d", byteAt(12), byteAt(13)),
		graphNodes: [3]int{10, 20, 30}, graphEdges: [3]int{byteAt(14) % 3, byteAt(15) % 3, byteAt(64) % 3},
	}
	g := templateParityCase{initial: initial, hops: 1 + byteAt(2)%2}
	g.program = fmt.Sprintf(`
(set 'a (sorted-map "n" %d))
(set 'map-alias (quasiquote (unquote a))) (assoc! a "self" map-alias)
(set 'ba (to-bytes "ab")) (set 'bb (quasiquote (unquote ba)))
(set 'xs (list %d %d %d)) (set 'tail (cdr xs))
(set 'middle (slice 'list xs 0 2)) (set 'rest-view (rest xs))
(set 'vec (vector 7 8 9)) (set 'append-view (slice 'vector vec 0 1))
(let ((cell (vector %d)))
  (defun closure-add (n) (append! cell n) (foldl + 0 cell))
  (defun closure-read () (foldl + 0 cell)))
(set 'allowed (sorted-map "k" %d)) (set 'check (s:in allowed))
(set 'doc (json:load-string %s :exact-integers true))
(set 'doc-alias (quasiquote (unquote doc)))
(set 'literal %s)
(defun parity-observe ()
  (list (get a "n") (get map-alias "n") (get (get a "self") "n")
    (to-string ba) (to-string bb) (copy xs) (copy tail) (copy middle) (copy rest-view)
    (copy vec) (copy append-view) (closure-read) (captured-read) (captured-read-again)
    (to-string host-a) (to-string host-b) raw-header raw-alias (get allowed "k")
    (s:validate check allowed) (get doc "n") (get doc-alias "n") literal
    (list (get graph0 "n") (get graph1 "n") (get graph2 "n"))
    (list (get (get graph0 "edge") "n") (get (get graph1 "edge") "n") (get (get graph2 "edge") "n"))
    (list (get (get (get graph0 "edge") "edge") "n")
          (get (get (get graph1 "edge") "edge") "n")
          (get (get (get graph2 "edge") "edge") "n"))))
`, initial.mapN, initial.xs[0], initial.xs[1], initial.xs[2], initial.closure, initial.allowed,
		strconv.Quote(fmt.Sprintf(`{"n":%d}`, initial.jsonN)), strconv.Quote(initial.literal))
	// Each input independently chooses three real edges: 27 initial graphs,
	// including self cycles, a three-cycle and shared targets. All nodes exist
	// before edges are wired, so every topology is a valid loadable program.
	for i, value := range initial.graphNodes {
		g.program += fmt.Sprintf("(set 'graph%d (sorted-map \"n\" %d))\n", i, value)
	}
	for i, edge := range initial.graphEdges {
		g.program += fmt.Sprintf("(assoc! graph%d \"edge\" graph%d)\n", i, edge)
	}
	n, steps := 2+byteAt(0)%2, 3+byteAt(1)%5
	g.tx = make([][]templateParityTx, n)
	for vm := range n {
		m := initial.clone()
		var effects []int
		for step := range steps {
			action := byteAt(16+2*(vm*steps+step)) % templateParityActions
			arg := 1 + byteAt(17+2*(vm*steps+step))%40
			event := 1000*vm + 100*step + arg
			effects = append(effects, event)
			tx := templateParityTx{action: action, effects: slices.Clone(effects)}
			var mutation string
			switch action {
			case 0:
				mutation = "()"
			case 1:
				m.mapN = arg
				mutation = fmt.Sprintf(`(set 'tx-alias (quasiquote (unquote a))) (assoc! tx-alias "n" %d)`, arg)
			case 2:
				m.bytes += string(byte(64 + arg))
				mutation = fmt.Sprintf(`(append! bb %d)`, 64+arg)
			case 3:
				slices.Sort(m.xs)
				mutation = `(stable-sort < xs)`
			case 4:
				slices.Sort(m.xs[1:])
				slices.Reverse(m.xs[1:])
				mutation = `(stable-sort > tail)`
			case 5:
				// Lisp slice views clamp capacity: append grows this view,
				// without overwriting the source vector's later slots.
				m.view = append(m.view, arg)
				mutation = fmt.Sprintf(`(append! append-view %d)`, arg)
			case 6:
				m.closure += arg
				mutation = fmt.Sprintf(`(closure-add %d)`, arg)
			case 7:
				m.capture += arg
				mutation = fmt.Sprintf(`(captured-add %d)`, arg)
			case 8:
				m.hostBytes[2] = byte(64 + arg)
				mutation = fmt.Sprintf(`(overlap-write %d)`, 64+arg)
			case 9:
				m.raw += arg
				mutation = fmt.Sprintf(`(header-add %d)`, arg)
			case 10:
				m.allowed = arg
				mutation = fmt.Sprintf(`(assoc! allowed "k" %d)`, arg)
			case 11:
				m.jsonN = arg
				mutation = fmt.Sprintf(`(assoc! doc-alias "n" %d)`, arg)
			case 12:
				tx.condition, tx.message = "fuzz-condition", fmt.Sprintf("_fun%d/_validation_fun_%d", event, arg)
				mutation = fmt.Sprintf(`(error 'fuzz-condition %s)`, strconv.Quote(tx.message))
			case 13:
				tx.condition = "error"
				mutation = `(get doc 'n)`
			case 14:
				tx.condition = "failed-constraint"
				mutation = `(s:validate check (sorted-map "k" -1))`
			case 15:
				tx.native = true
				mutation = `host-state`
			case 16:
				node := arg % len(m.graphNodes)
				m.graphNodes[m.graphEdges[node]] = arg
				mutation = fmt.Sprintf(`(assoc! (get graph%d "edge") "n" %d)`, node, arg)
			case 17:
				node, edge := (arg/3)%len(m.graphNodes), arg%len(m.graphNodes)
				m.graphEdges[node] = edge
				mutation = fmt.Sprintf(`(assoc! graph%d "edge" graph%d)`, node, edge)
			}
			handle := "host-state"
			if step%2 != 0 {
				handle = "host-alias"
			}
			tx.source = fmt.Sprintf("(emit %s %d) %s", handle, event, mutation)
			if tx.condition == "" && !tx.native {
				tx.source += " (parity-observe)"
			}
			tx.observation = m.observation()
			g.tx[vm] = append(g.tx[vm], tx)
		}
	}
	return g
}

type templateParityTrust map[string]bool

func (trusted templateParityTrust) option(env *lisp.LEnv) lisp.TemplateOption {
	instances := make(map[any]bool)
	roots(env, func(pkg, name string, v *lisp.LVal) {
		if trusted[pkg+"\x00"+name] && v.Type == lisp.LFun && v.Builtin() != nil {
			instances[v.Native] = true
		}
	})
	return lisp.TemplateWithBuiltinPolicy(func(v *lisp.LVal) bool { return instances[v.Native] })
}

func buildTemplateParity(g templateParityCase) (*lisp.LEnv, templateParityTrust, error) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	for _, load := range []func(*lisp.LEnv) *lisp.LVal{func(env *lisp.LEnv) *lisp.LVal { return lisp.InitializeUserEnv(env) }, libjson.LoadPackage, libschema.LoadPackage} {
		if rc := load(env); rc.Type == lisp.LError {
			return nil, nil, lisp.GoError(rc)
		}
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		return nil, nil, lisp.GoError(rc)
	}
	// Approve exact instances of this fixed trusted bootstrap, before loading
	// generated source. No package-name or reflect/code-pointer allowlist.
	trusted := make(templateParityTrust)
	roots(env, func(pkg, name string, v *lisp.LVal) {
		if v.Type == lisp.LFun && v.Builtin() != nil {
			trusted[pkg+"\x00"+name] = true
		}
	})
	if rc := env.LoadString("program.lisp", g.program); rc.Type == lisp.LError {
		return nil, nil, lisp.GoError(rc)
	}
	put := func(name string, v *lisp.LVal) error { return lisp.GoError(env.PutGlobal(lisp.Symbol(name), v)) }
	capture := lisp.SortedMap()
	if rc := capture.MapSet("n", lisp.Int(g.initial.capture)); rc.Type == lisp.LError {
		return nil, nil, lisp.GoError(rc)
	}
	if rc := capture.MapSet("self", capture); rc.Type == lisp.LError {
		return nil, nil, lisp.GoError(rc)
	}
	for _, name := range []string{"captured-read", "captured-read-again", "captured-add"} {
		formals := lisp.Formals()
		if name == "captured-add" {
			formals = lisp.Formals("n")
		}
		fn := funraw.NewCapturedBuiltin(funraw.CapturedBuiltin{Package: lisp.DefaultUserPackage, FID: name,
			Formals: formals, Captures: capture,
			Eval: func(_ *lisp.LEnv, args, values *lisp.LVal) *lisp.LVal {
				if len(args.Cells) != 0 {
					if rc := values.MapSet("n", lisp.Int(values.MapGet("n").Int+args.Cells[0].Int)); rc.Type == lisp.LError {
						return rc
					}
				}
				return values.MapGet("n")
			}})
		if err := put(name, fn); err != nil {
			return nil, nil, err
		}
		if rc := capture.MapSet(name, fn); rc.Type == lisp.LError {
			return nil, nil, lisp.GoError(rc)
		}
	}
	backing := slices.Clone(g.initial.hostBytes)
	first, second := lisp.Bytes(backing[:3]), lisp.Bytes(backing[2:5])
	header := lisp.Int(g.initial.raw)
	for name, v := range map[string]*lisp.LVal{"host-a": first, "host-b": second, "raw-header": header, "raw-alias": header, "hidden-state": lisp.Int(19)} {
		if err := put(name, v); err != nil {
			return nil, nil, err
		}
	}
	for _, entry := range []struct {
		name    string
		capture *lisp.LVal
		eval    func(*lisp.LEnv, *lisp.LVal, *lisp.LVal) *lisp.LVal
	}{
		{"overlap-write", first, func(_ *lisp.LEnv, args, values *lisp.LVal) *lisp.LVal {
			values.Bytes()[2] = byte(args.Cells[0].Int) //elps:mutates explicit host mutation of an owned, admitted byte capture
			return lisp.Nil()
		}},
		{"header-add", header, func(_ *lisp.LEnv, args, values *lisp.LVal) *lisp.LVal {
			values.Int += args.Cells[0].Int //elps:mutates explicit host mutation of an owned, admitted scalar capture
			return lisp.Nil()
		}},
	} {
		fn := funraw.NewCapturedBuiltin(funraw.CapturedBuiltin{Package: lisp.DefaultUserPackage, FID: entry.name,
			Formals: lisp.Formals("n"), Captures: entry.capture, Eval: entry.eval})
		if err := put(entry.name, fn); err != nil {
			return nil, nil, err
		}
	}
	return env, trusted, nil
}

type templateParityHost struct{ events []int }

func setupTemplateParity(env *lisp.LEnv) error {
	host := &templateParityHost{}
	for _, name := range []string{"host-state", "host-alias"} {
		if rc := env.PutGlobal(lisp.Symbol(name), lisp.Native(host)); rc.Type == lisp.LError {
			return lisp.GoError(rc)
		}
	}
	// This ordinary Go callback and its mutable native arguments are installed
	// only after publication. They are never smuggled into an immutable plan.
	emit := lisp.FunInPackage(lisp.DefaultUserPackage, "emit", lisp.Formals("state", "n"),
		func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			host := args.Cells[0].Native.(*templateParityHost)
			host.events = append(host.events, args.Cells[1].Int)
			return lisp.Nil()
		})
	return lisp.GoError(env.PutGlobal(lisp.Symbol("emit"), emit))
}

func templateParityEffects(env *lisp.LEnv) []int {
	return env.Get(lisp.Symbol("host-state")).Native.(*templateParityHost).events
}

func renderTemplateParityNative(value any) string {
	if host, ok := value.(*templateParityHost); ok {
		return fmt.Sprint(host.events)
	}
	// Other fixture natives are explicitly immutable schema tags; their type
	// is already emitted by the generic renderer and there is no mutable state.
	return ""
}

// templateParityFault changes only the substituted constructor or request hook.
// Negative controls exercise the same runner as the fuzz target; a broken
// constructor that slips past it fails a fixed test rather than looking green.
type templateParityFault func(role string, source, vm *lisp.LEnv) error

func runTemplateParity(g templateParityCase, schedule string, fault templateParityFault) error {
	if len(g.tx) < 2 || (g.hops != 1 && g.hops != 2) {
		return errors.New("invalid case: require multiple VMs and one or two hops")
	}
	for _, seq := range g.tx {
		if len(seq) < 2 || len(seq) != len(g.tx[0]) {
			return errors.New("invalid case: require equal-length multi-transaction sequences")
		}
	}
	if !slices.Contains(templateParitySchedules, schedule) {
		return fmt.Errorf("invalid case: unknown schedule %q", schedule)
	}
	state := func(env *lisp.LEnv) string { return envStateWithNative(env, renderTemplateParityNative) }
	result := func(v *lisp.LVal) string { return renderResultWithNative(v, renderTemplateParityNative) }
	source, opts, err := buildTemplateParity(g)
	if err != nil {
		return fmt.Errorf("template/load: %w", err)
	}
	plan, err := lisp.NewTemplate(source, opts.option(source))
	if err != nil {
		return fmt.Errorf("template/publish: %w", err)
	}
	pristineState, pristineAlias := state(source), aliasSignature(source)
	sources := []*lisp.LEnv{source}
	if g.hops == 2 {
		anchor, err := plan.NewVM()
		if err != nil {
			return fmt.Errorf("anchor/fork: %w", err)
		}
		if fault != nil {
			if err := fault("anchor", source, anchor); err != nil {
				return err
			}
		}
		sources = append(sources, anchor)
		// The grammar never replaces bootstrap bindings. Follow those known
		// roots into the first instance and approve its exact copied headers;
		// original funData identities intentionally do not survive a fork.
		plan, err = lisp.NewTemplate(anchor, opts.option(anchor))
		if err != nil {
			return fmt.Errorf("anchor/publish: %w", err)
		}
	}
	checkPristine := func(role string, env *lisp.LEnv) error {
		if state(env) != pristineState {
			return fmt.Errorf("%s/state differs from pristine source", role)
		}
		if aliasSignature(env) != pristineAlias {
			return fmt.Errorf("%s/alias differs from pristine source", role)
		}
		return nil
	}
	take := func(role string) (*lisp.LEnv, error) {
		vm, err := plan.NewVM()
		if err != nil {
			return nil, fmt.Errorf("%s/fork: %w", role, err)
		}
		if fault != nil {
			if err := fault(role, source, vm); err != nil {
				return nil, err
			}
		}
		if err := checkPristine(role, vm); err != nil {
			return nil, err
		}
		return vm, nil
	}
	type pair struct {
		cold, fork   *lisp.LEnv
		state, alias string
		effects      []int
	}
	pairs := make([]pair, len(g.tx))
	makePair := func(i int) error {
		cold, _, err := buildTemplateParity(g)
		if err != nil {
			return fmt.Errorf("cold[%d]/load: %w", i, err)
		}
		if fault != nil {
			if err := fault("cold", source, cold); err != nil {
				return fmt.Errorf("cold[%d]/load: %w", i, err)
			}
		}
		vm, err := take("initial")
		if err != nil {
			return err
		}
		for _, env := range []*lisp.LEnv{cold, vm} {
			if err := setupTemplateParity(env); err != nil {
				return err
			}
		}
		if fault != nil {
			if err := fault("setup", source, vm); err != nil {
				return err
			}
		}
		pairs[i] = pair{cold: cold, fork: vm, state: state(cold), alias: aliasSignature(cold)}
		return nil
	}
	checkPrivate := func() error {
		all := slices.Clone(sources)
		for _, p := range pairs {
			if p.cold != nil {
				all = append(all, p.cold, p.fork)
			}
		}
		censuses := make([]oracleCensus, len(all))
		for i, env := range all {
			censuses[i] = newOracleCensus(env)
		}
		allowNative := func(_ nativePayloadIdentity, value any) bool {
			return oracleDeclaredImmutable(value)
		}
		for i, a := range censuses {
			for _, b := range censuses[:i] {
				if shared := sharedOracleCensuses(a, b, allowNative); len(shared) != 0 {
					return fmt.Errorf("private/shared: %v", shared)
				}
			}
		}
		return nil
	}
	checkIdle := func() error {
		for _, env := range sources {
			if err := checkPristine("source", env); err != nil {
				return err
			}
		}
		for i, p := range pairs {
			if p.cold != nil && state(p.fork) != p.state {
				return fmt.Errorf("sibling[%d]/state changed without its transaction", i)
			}
			if p.cold != nil && aliasSignature(p.fork) != p.alias {
				return fmt.Errorf("sibling[%d]/alias changed without its transaction", i)
			}
			if p.cold != nil && (!slices.Equal(templateParityEffects(p.cold), p.effects) || !slices.Equal(templateParityEffects(p.fork), p.effects)) {
				return fmt.Errorf("sibling[%d]/effects changed without its transaction", i)
			}
		}
		_, err := take("later")
		return err
	}
	step := func(i, j int) error {
		p, tx := &pairs[i], g.tx[i][j]
		filename := fmt.Sprintf("vm%d-tx%d.lisp", i, j)
		want := p.cold.LoadString(filename, tx.source)
		got := p.fork.LoadString(filename, tx.source)
		for _, arm := range []struct {
			name  string
			value *lisp.LVal
			env   *lisp.LEnv
		}{{"cold", want, p.cold}, {"fork", got, p.fork}} {
			if tx.native {
				if arm.value.Type != lisp.LNative || arm.value.Native != arm.env.Get(lisp.Symbol("host-state")).Native {
					return fmt.Errorf("native-result/model %s: result is not this VM's host handle", arm.name)
				}
				if !slices.Equal(arm.value.Native.(*templateParityHost).events, tx.effects) {
					return fmt.Errorf("native-result/model %s: contents differ", arm.name)
				}
			} else if tx.condition != "" {
				if arm.value.Type != lisp.LError || arm.value.Str != tx.condition {
					return fmt.Errorf("result/model %s vm=%d tx=%d: got %v, want condition %s", arm.name, i, j, arm.value, tx.condition)
				}
				if tx.message != "" && (len(arm.value.Cells) != 1 || arm.value.Cells[0].Type != lisp.LString || arm.value.Cells[0].Str != tx.message) {
					return fmt.Errorf("error/model %s: got %v, want literal %q", arm.name, arm.value, tx.message)
				}
			} else if !lisp.True(arm.value.Equal(tx.observation)) {
				return fmt.Errorf("result/model %s vm=%d tx=%d: got %v, want %v", arm.name, i, j, arm.value, tx.observation)
			}
		}
		if result(got) != result(want) {
			return fmt.Errorf("result/cold vm=%d tx=%d: %s != %s", i, j, result(got), result(want))
		}
		for _, env := range []*lisp.LEnv{p.cold, p.fork} {
			if observed := env.LoadString("observe.lisp", `(parity-observe)`); !lisp.True(observed.Equal(tx.observation)) {
				return fmt.Errorf("observation/model vm=%d tx=%d: got %v, want %v", i, j, observed, tx.observation)
			}
			if !slices.Equal(templateParityEffects(env), tx.effects) {
				return fmt.Errorf("effects/model vm=%d tx=%d: got %v, want %v", i, j, templateParityEffects(env), tx.effects)
			}
		}
		if state(p.fork) != state(p.cold) {
			return fmt.Errorf("state/cold vm=%d tx=%d: %s", i, j, diffLines(state(p.cold), state(p.fork)))
		}
		if aliasSignature(p.fork) != aliasSignature(p.cold) {
			return fmt.Errorf("alias/cold vm=%d tx=%d", i, j)
		}
		p.state, p.alias = state(p.cold), aliasSignature(p.cold)
		p.effects = tx.effects
		return nil
	}
	if schedule != "lazy" {
		if schedule == "concurrent" && fault == nil {
			// Cold loads and repeated Fork calls themselves run concurrently.
			// Fault controls construct serially so their deliberate writes to
			// the source cannot create a race inside the test injection.
			var wg sync.WaitGroup
			errs := make([]error, len(pairs))
			for i := range pairs {
				wg.Add(1)
				go func() {
					defer wg.Done()
					errs[i] = makePair(i)
				}()
			}
			wg.Wait()
			for _, err := range errs {
				if err != nil {
					return err
				}
			}
		} else {
			for i := range pairs {
				if err := makePair(i); err != nil {
					return err
				}
			}
		}
		if err := checkPrivate(); err != nil {
			return err
		}
	}
	if schedule == "concurrent" {
		var wg sync.WaitGroup
		errs := make([]error, len(pairs))
		start := make(chan struct{})
		for i := range pairs {
			wg.Add(1)
			go func() {
				defer wg.Done()
				<-start
				for j := range g.tx[i] {
					if err := step(i, j); err != nil {
						errs[i] = err
						return
					}
				}
			}()
		}
		close(start)
		wg.Wait()
		for _, err := range errs {
			if err != nil {
				return err
			}
		}
	} else {
		for outer := range len(g.tx[0]) * len(pairs) {
			i, j := outer/len(g.tx[0]), outer%len(g.tx[0])
			if schedule == "interleaved" {
				i, j = outer%len(pairs), outer/len(pairs)
			}
			if pairs[i].cold == nil {
				if err := makePair(i); err != nil {
					return err
				}
				if err := checkPrivate(); err != nil {
					return err
				}
			}
			if err := step(i, j); err != nil {
				return err
			}
			if err := checkIdle(); err != nil {
				return err
			}
		}
	}
	if err := checkIdle(); err != nil {
		return err
	}
	if err := checkPrivate(); err != nil {
		return err
	}
	// Mutating the original environment after publication must not alter the
	// saved plan or any existing instance, in either direction of ownership.
	if rc := source.Get(lisp.Symbol("a")).MapSet("n", lisp.Int(999)); rc.Type == lisp.LError {
		return lisp.GoError(rc)
	}
	if _, err := take("after-source-write"); err != nil {
		return err
	}
	for i, p := range pairs {
		if state(p.fork) != p.state || aliasSignature(p.fork) != p.alias {
			return fmt.Errorf("source-write changed fork[%d]", i)
		}
	}
	return nil
}

func templateParitySeeds() [][]byte {
	seeds := [][]byte{nil, {255}}
	for action := range templateParityActions {
		seed := make([]byte, 58)
		seed[0], seed[1], seed[2] = byte(action%2), byte(action%5), byte(action%2)
		for i := 16; i < len(seed); i += 2 {
			seed[i], seed[i+1] = byte(action), byte(i)
		}
		seeds = append(seeds, seed)
	}
	for _, edges := range [][3]byte{{0, 0, 0}, {1, 2, 0}, {0, 1, 2}, {2, 2, 1}} {
		seeds = append(seeds, templateParityTopologySeed(edges))
	}
	return seeds
}

func templateParityTopologySeed(edges [3]byte) []byte {
	seed := make([]byte, 65)
	seed[14], seed[15], seed[64] = edges[0], edges[1], edges[2]
	for i := 16; i < 64; i += 2 {
		seed[i] = 16 // write 1 through graph1's chosen edge
	}
	seed[18], seed[19] = 17, 4 // then rewire graph1 to graph2 (arg=5)
	return seed
}

func TestTemplateParityInputReallyChangesGraphEdges(t *testing.T) {
	seen := make(map[[3]int]bool)
	for a := range byte(3) {
		for b := range byte(3) {
			for c := range byte(3) {
				edges := [3]int{int(a), int(b), int(c)}
				g := generateTemplateParity(templateParityTopologySeed([3]byte{a, b, c}))
				if g.initial.graphEdges != edges {
					t.Fatalf("input edges=%v generated=%v", edges, g.initial.graphEdges)
				}
				seen[g.initial.graphEdges] = true
				env, _, err := buildTemplateParity(g)
				if err != nil {
					t.Fatal(err)
				}
				for node, target := range edges {
					from := env.Get(lisp.Symbol(fmt.Sprintf("graph%d", node)))
					to := env.Get(lisp.Symbol(fmt.Sprintf("graph%d", target)))
					if from.MapGet("edge").Map() != to.Map() {
						t.Fatalf("input edges=%v: emitted edge %d does not reach node %d's actual storage", edges, node, target)
					}
				}
				if err := setupTemplateParity(env); err != nil {
					t.Fatal(err)
				}
				// Pin the consequence of the first mutation independently of
				// the generator's model: graph1's target becomes exactly 1.
				want := []int{10, 20, 30}
				want[edges[1]] = 1
				got := env.LoadString("graph-write.lisp", g.tx[0][0].source)
				if got.Type == lisp.LError || len(got.Cells) != 26 || !lisp.True(got.Cells[23].Equal(lisp.QExpr(parityInts(want)))) {
					t.Fatalf("input edges=%v: through-edge mutation got %v, want node values %v", edges, got, want)
				}
				if got := env.LoadString("graph-rewire.lisp", g.tx[0][1].source); got.Type == lisp.LError {
					t.Fatal(got)
				}
				if env.Get(lisp.Symbol("graph1")).MapGet("edge").Map() != env.Get(lisp.Symbol("graph2")).Map() {
					t.Fatalf("input edges=%v: transaction did not rewire graph1 to graph2", edges)
				}
			}
		}
	}
	if len(seen) != 27 {
		t.Fatalf("generator lost graph topology choices: got %d, want 27", len(seen))
	}
}

func TestTemplateParitySeedsCoverHistoricalShapes(t *testing.T) {
	seen := make(map[int]bool)
	hops := make(map[int]bool)
	for _, data := range templateParitySeeds() {
		g := generateTemplateParity(data)
		hops[g.hops] = true
		if len(g.tx) < 2 || len(g.tx[0]) < 3 {
			t.Fatal("generator lost multi-VM transaction sequences")
		}
		for _, shape := range []string{"(cdr xs)", "(rest xs)", "(slice 'list", "(slice 'vector", "(quasiquote (unquote a))", "(defun closure-add", "(defun closure-read", "(s:in allowed)"} {
			if !strings.Contains(g.program, shape) {
				t.Fatalf("generator lost historical shape %s", shape)
			}
		}
		for _, seq := range g.tx {
			for _, tx := range seq {
				seen[tx.action] = true
			}
		}
	}
	if len(seen) != templateParityActions || !hops[1] || !hops[2] {
		t.Fatalf("corpus lost action/hop coverage: actions=%v hops=%v", seen, hops)
	}
}

// Go's ordinary test runner already executes FuzzForkParity's seeds. Replay
// only the other schedules here: each of the original 24 x 3 pairs still runs
// once, instead of twice, without charging all 72 pairs to the 30s fuzz budget.
func TestTemplateParitySeedComplementarySchedules(t *testing.T) {
	for i, data := range templateParitySeeds() {
		for _, schedule := range templateParityComplementarySchedules(data) {
			t.Run(fmt.Sprintf("seed%d/%s", i, schedule), func(t *testing.T) {
				if err := runTemplateParity(generateTemplateParity(data), schedule, nil); err != nil {
					t.Fatal(err)
				}
			})
		}
	}
}

func TestTemplateParityScheduleSelectionPreservesPrograms(t *testing.T) {
	if got := templateParitySchedule(nil); got != "lazy" {
		t.Fatalf("empty input schedule = %s, want lazy", got)
	}
	// The generator reads at most 128 bytes. A selector beyond that boundary
	// must reach every schedule without changing any modeled transaction.
	data := make([]byte, 129)
	want := generateTemplateParity(data)
	for selector := range 256 {
		data[128] = byte(selector)
		if got := templateParitySchedule(data); got != templateParitySchedules[selector%3] {
			t.Fatalf("selector %d: schedule = %s", selector, got)
		}
		got := generateTemplateParity(data)
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("selector %d changed the generated case", selector)
		}
	}
	selected := make(map[string]bool)
	for _, data := range templateParitySeeds() {
		selected[templateParitySchedule(data)] = true
		counts := map[string]int{templateParitySchedule(data): 1}
		for _, schedule := range templateParityComplementarySchedules(data) {
			counts[schedule]++
		}
		if !reflect.DeepEqual(counts, map[string]int{"lazy": 1, "interleaved": 1, "concurrent": 1}) {
			t.Fatalf("seed %x: fuzz + complementary schedules must cover each pair exactly once: %v", data, counts)
		}
	}
	for _, schedule := range templateParitySchedules {
		if !selected[schedule] {
			t.Fatalf("fuzz seed corpus never selects %s", schedule)
		}
	}
}

func TestTemplateParityRejectsBrokenConstructors(t *testing.T) {
	for _, tc := range []struct {
		name, needle string
		fault        templateParityFault
	}{
		{"dealiased-view", "alias", func(role string, _, vm *lisp.LEnv) error {
			if role == "initial" {
				view := vm.Get(lisp.Symbol("tail"))
				view.Cells = slices.Clone(view.Cells) //elps:mutates deliberately broken constructor for the oracle's negative control
			}
			return nil
		}},
		{"later-fork-corruption", "later/state", func(role string, _, vm *lisp.LEnv) error {
			if role == "later" {
				return lisp.GoError(vm.Get(lisp.Symbol("a")).MapSet("n", lisp.Int(-1)))
			}
			return nil
		}},
		{"capture-corruption", "state", func(role string, _, vm *lisp.LEnv) error {
			if role == "initial" {
				return lisp.GoError(funraw.Captures(vm.Get(lisp.Symbol("captured-read"))).MapSet("n", lisp.Int(-1)))
			}
			return nil
		}},
		{"shared-byte-backing", "private/shared", func(role string, source, vm *lisp.LEnv) error {
			if role == "initial" {
				for _, name := range []string{"host-a", "host-b"} {
					*vm.Get(lisp.Symbol(name)).Native.(*[]byte) = source.Get(lisp.Symbol(name)).Bytes() //elps:mutates deliberately broken storage copy for the isolation oracle
				}
			}
			return nil
		}},
		{"shared-scalar-header", "private/shared", func(role string, source, vm *lisp.LEnv) error {
			if role == "initial" {
				// Inject below PutGlobal's checked-build ownership guard: this
				// control must prove the independent oracle detects a bad copy,
				// not merely that a public write can reject shared ownership.
				pkg := vm.Runtime.Registry.Package(lisp.DefaultUserPackage)
				for _, name := range []string{"raw-header", "raw-alias", "header-add"} {
					if rc := pkg.Put(lisp.Symbol(name), source.Get(lisp.Symbol(name))); rc.Type == lisp.LError {
						return lisp.GoError(rc)
					}
				}
			}
			return nil
		}},
		{"source-corruption", "source/state", func(role string, source, _ *lisp.LEnv) error {
			if role == "initial" {
				return lisp.GoError(source.Get(lisp.Symbol("a")).MapSet("n", lisp.Int(-1)))
			}
			return nil
		}},
		{"fork-refusal", "injected refusal", func(role string, _, _ *lisp.LEnv) error {
			if role == "initial" {
				return errors.New("injected refusal")
			}
			return nil
		}},
		{"cold-load-refusal", "cold[0]/load", func(role string, _, _ *lisp.LEnv) error {
			if role == "cold" {
				return errors.New("injected cold-load refusal after the source succeeded")
			}
			return nil
		}},
		{"cold-load-asymmetry", "result/model cold", func(role string, _, vm *lisp.LEnv) error {
			if role == "cold" {
				return lisp.GoError(vm.Get(lisp.Symbol("a")).MapSet("n", lisp.Int(-1)))
			}
			return nil
		}},
		{"raise-asymmetry", "result/model fork", func(role string, _, vm *lisp.LEnv) error {
			if role == "setup" {
				broken := lisp.FunInPackage(lisp.DefaultUserPackage, "emit", lisp.Formals("state", "n"),
					func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Error(errors.New("unexpected rejection")) })
				return lisp.GoError(vm.PutGlobal(lisp.Symbol("emit"), broken))
			}
			return nil
		}},
		{"ordinary-string-corruption", "result/model", func(role string, _, vm *lisp.LEnv) error {
			if role == "setup" {
				return lisp.GoError(vm.PutGlobal(lisp.Symbol("literal"), lisp.String("_fun1/_validation_fun_0")))
			}
			return nil
		}},
		{"lost-host-effects", "effects/model", func(role string, _, vm *lisp.LEnv) error {
			if role == "setup" {
				noop := lisp.FunInPackage(lisp.DefaultUserPackage, "emit", lisp.Formals("state", "n"),
					func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Nil() })
				return lisp.GoError(vm.PutGlobal(lisp.Symbol("emit"), noop))
			}
			return nil
		}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			for _, schedule := range templateParitySchedules {
				err := runTemplateParity(generateTemplateParity(nil), schedule, tc.fault)
				if err == nil || !strings.Contains(err.Error(), tc.needle) {
					t.Fatalf("%s: broken constructor escaped its intended oracle; got %v, want %s", schedule, err, tc.needle)
				}
			}
		})
	}
}

func TestTemplateParityRejectsVacuousCase(t *testing.T) {
	for _, g := range []templateParityCase{{}, {tx: make([][]templateParityTx, 2), hops: 1}} {
		if err := runTemplateParity(g, "lazy", nil); err == nil || !strings.Contains(err.Error(), "invalid case") {
			t.Fatalf("vacuous case accepted: %v", err)
		}
	}
}

func TestTemplateParityConcurrentStateChannelRejectsHiddenWrite(t *testing.T) {
	fault := func(role string, _, vm *lisp.LEnv) error {
		if role != "setup" {
			return nil
		}
		// The transaction's value, expected observations and host effects are
		// and alias topology are all correct. Only the whole-state channel
		// can see this write to an existing, independently allocated scalar.
		emit := lisp.FunInPackage(lisp.DefaultUserPackage, "emit", lisp.Formals("state", "n"),
			func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				host := args.Cells[0].Native.(*templateParityHost)
				host.events = append(host.events, args.Cells[1].Int)
				env.Get(lisp.Symbol("hidden-state")).Int = 20 //elps:mutates deliberate private scalar-content corruption for the whole-state oracle
				return lisp.Nil()
			})
		return lisp.GoError(vm.PutGlobal(lisp.Symbol("emit"), emit))
	}
	err := runTemplateParity(generateTemplateParity(nil), "concurrent", fault)
	if err == nil || !strings.Contains(err.Error(), "state/cold") {
		t.Fatalf("concurrent-only hidden write escaped whole-state comparison: %v", err)
	}
}

func TestTemplateParitySecondHopReallyPublishesTheAnchor(t *testing.T) {
	g := generateTemplateParity(nil)
	g.hops = 2
	for _, schedule := range templateParitySchedules {
		anchors := 0
		fault := func(role string, _, vm *lisp.LEnv) error {
			if role != "anchor" {
				return nil
			}
			anchors++
			return lisp.GoError(vm.Get(lisp.Symbol("a")).MapSet("n", lisp.Int(-1)))
		}
		err := runTemplateParity(g, schedule, fault)
		if anchors != 1 || err == nil || !strings.Contains(err.Error(), "initial/state") {
			t.Fatalf("%s: corrupt second-hop publication escaped: anchors=%d error=%v", schedule, anchors, err)
		}
	}
}

func TestTemplateParityHistoricalSortLiteral(t *testing.T) {
	g := generateTemplateParity(templateParitySeeds()[2+3])
	if !slices.Equal(g.initial.xs, []int{30, 10, 20}) {
		t.Fatalf("historical seed no longer emits the original unsorted list: %v", g.initial.xs)
	}
	if got := g.tx[0][0].observation.Cells[6]; !lisp.True(got.Equal(lisp.QExpr([]*lisp.LVal{lisp.Int(20), lisp.Int(30)}))) {
		t.Fatalf("historical cold view expectation: got %v, want '(20 30)", got)
	}
	for _, schedule := range templateParitySchedules {
		if err := runTemplateParity(g, schedule, nil); err != nil {
			t.Fatal(err)
		}
	}
}
