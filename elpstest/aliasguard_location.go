// Copyright © 2026 The ELPS authors

package elpstest

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"strings"
	"time"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// The location channel: spooky action at a distance.
//
// # Why locations are not compared directly
//
// LEnv.loc is not state an environment owns, it is the evaluator's location
// register: eval rebinds it to the node it is about to evaluate, on every
// step.  A fork deliberately starts with an EMPTY register (lisp/fork.go,
// issue #440), so a template and its fork legitimately hold different
// locations and comparing them would report the drop Fork exists to do.
//
// So the guard compares their OBSERVABLE CONSEQUENCES instead.  The register
// is observable through exactly one surface: error rendering.  A step limit,
// an evaluation-nesting limit and a context cancellation all raise through
// env.ErrorConditionf, which stamps env.loc into the error's rendered text,
// into Source(), and into the call-stack frames — which is precisely the
// surface the differential battery of PR #578 swept.  If a location bleeds
// from one environment into another, an error rendered somewhere else
// changes; if it does not bleed, the rendering is byte-identical.
//
// The forcing device is a countdown context rather than a step budget,
// because a countdown carries no accumulated runtime state and can therefore
// be replayed exactly.  It is the same device lisp/funloc_test.go uses to
// walk the cancellation point across an evaluation one checkLimits call at a
// time.
//
// # The three properties
//
//  1. NO CROSS-ENVIRONMENT BLEED.  Stamp a unique sentinel location on one
//     reachable environment; the rendering of an error raised elsewhere must
//     be byte-identical to the baseline taken before the stamp.  Swept over
//     every reachable environment.
//  2. FORK DROPS EVALUATOR LOCATIONS.  No environment reachable in a fork
//     carries a location register inherited from the template.
//     lisp/fork_metadata_test.go's TestForkDropsEvaluatorLocation pins one
//     instance of this against the lexical chain and the registry's direct
//     function bindings; this generalises it to every environment reachable
//     through a container.
//  3. DEFINITION-SITE SNAPSHOTS STAY FROZEN.  A budget error that trips at a
//     function-body entry reports the function's DEFINITION site, not
//     whatever its defining environment's live register happens to hold
//     (PR #578 review finding F1, fixed by funData.loc).  Stated as a
//     property: mutating the defining environment's register changes
//     nothing, which is property 1 aimed at the environment that would
//     otherwise be read.
//
// Property 3 is what makes property 1 non-vacuous: the check asserts up
// front that the baseline rendering really does name a definition site, so
// a probe that stopped tripping at a body entry fails loudly instead of
// passing by reaching nothing.

// countdownContext reports itself cancelled once Err has been probed more
// than N times, which walks the cancellation point across an evaluation one
// checkLimits call at a time.  A real cancelled context trips at the first
// check; this one trips at a chosen one, and replays exactly.
type countdownContext struct {
	n int
	i int
}

func (c *countdownContext) Deadline() (time.Time, bool) { return time.Time{}, false }
func (c *countdownContext) Done() <-chan struct{}       { return nil }
func (c *countdownContext) Value(any) any               { return nil }
func (c *countdownContext) Err() error {
	c.i++
	if c.i > c.n {
		return context.Canceled
	}
	return nil
}

// LocationCheck describes one run of the location oracle.
type LocationCheck struct {
	// NewEnv builds the environment.  Nil means NewForkCheckEnv.
	NewEnv func() (*lisp.LEnv, error)
	// Program is loaded into that environment.  It should define at least
	// one closure over a non-root scope, so the sweep has an environment to
	// stamp that is not the global one.
	Program string
	// Probe is the expression whose evaluation is interrupted.  It must
	// raise under the countdown.
	Probe string
	// Trip is the countdown's trip point: the number of limit checks that
	// pass before cancellation.  Pick one that lands at a function-body
	// entry, so the baseline names a definition site (see WantSite).
	Trip int
	// Fork produces the environment the fork properties are checked
	// against.  Nil means (*lisp.LEnv).Fork.  It exists so a deliberately
	// broken reference fork can be driven through the same oracle, which
	// is how the guard proves it can still see each historical failure
	// mode (aliasguard_broken_test.go).
	Fork func(*lisp.LEnv) (*lisp.LEnv, error)
	// WantSite is a substring the baseline rendering must contain — the
	// definition site the budget error should report.  It is the
	// anti-vacuity assertion: without it a probe that stopped tripping
	// where it was aimed would still pass.
	WantSite string
	// MaxEnvironments bounds the sweep, which rebuilds the whole
	// environment once per stamped environment.  Zero means
	// DefaultMaxEnvironments.
	//
	// Exceeding it does NOT silently shorten the sweep: the check reports
	// a partial-coverage witness naming this field, because a truncated
	// sweep is a sweep that can miss a leak on the environments it never
	// reached.  Raise it -- at a superlinear cost in environment rebuilds,
	// measured under DefaultMaxEnvironments -- for a program that leaves
	// more scopes than the default covers: a dispatch table of forty
	// handlers leaves forty-two.
	MaxEnvironments int
	// Repro is attached to every witness.
	Repro string
}

// maxEnvs is the check's environment cap, defaulted.
func (c LocationCheck) maxEnvs() int {
	if c.MaxEnvironments > 0 {
		return c.MaxEnvironments
	}
	return DefaultMaxEnvironments
}

// fork applies the check's fork walker, defaulting to (*lisp.LEnv).Fork.
func (c LocationCheck) fork(env *lisp.LEnv) (*lisp.LEnv, error) {
	if c.Fork != nil {
		return c.Fork(env)
	}
	return env.Fork()
}

// RunLocationCheck runs the location oracle and reports each witness.
func RunLocationCheck(t TestingTB, c LocationCheck) {
	t.Helper()
	got, err := CheckLocations(c)
	if err != nil {
		t.Fatalf("location check: %v", err)
		return
	}
	for _, w := range got {
		t.Errorf("%s", w)
	}
}

// rendering is everything a raised error says about where it happened.
type rendering struct {
	text   string
	source string
	frames []string
}

func (r rendering) String() string {
	return fmt.Sprintf("%s | Source()=%s | frames=[%s]", r.text, r.source, strings.Join(r.frames, "; "))
}

func (r rendering) equal(o rendering) bool {
	if r.text != o.text || r.source != o.source || len(r.frames) != len(o.frames) {
		return false
	}
	for i := range r.frames {
		if r.frames[i] != o.frames[i] {
			return false
		}
	}
	return true
}

func renderError(v *lisp.LVal) rendering {
	r := rendering{text: v.String(), source: "-"}
	if loc, ok := v.Source(); ok {
		r.source = fmt.Sprintf("%s:%d:%d", loc.File, loc.Line, loc.Col)
	}
	if st := v.CallStack(); st != nil {
		for _, fr := range st.Frames {
			if fr.Source != nil {
				r.frames = append(r.frames, fmt.Sprintf("%s@%s:%d:%d", fr.Name, fr.Source.File, fr.Source.Line, fr.Source.Col))
			} else {
				r.frames = append(r.frames, fr.Name+"@-")
			}
		}
	}
	return r
}

// sentinelLocation is the location stamped onto an environment under test.
// Its file name cannot occur in any program the guard loads, so its
// appearance anywhere in a rendering is unambiguous evidence of a bleed.
func sentinelLocation(i int) *token.Location {
	name := fmt.Sprintf("location-sentinel-%d.lisp", i)
	return &token.Location{File: name, Path: name, Line: 90000 + i, Col: 7, Pos: 1}
}

// stampLocation writes loc into env's evaluator location register the only
// way an outside caller can: by evaluating a node that carries it.  eval
// sets env.loc = v.source on entry, which is exactly the register the
// budget errors read.  The node is minted fresh on every call because the
// checked-build ownership table adopts a value for the Runtime that first
// evaluates it.
func stampLocation(env *lisp.LEnv, loc *token.Location) error {
	v := lisp.Int(0)
	v.SetSource(loc)
	if res := env.Eval(v); res.Type == lisp.LError {
		return lisp.GoError(res)
	}
	return nil
}

// CheckLocations runs the location oracle and returns one witness per
// failed property.
func CheckLocations(c LocationCheck) ([]Witness, error) {
	build := func() (*lisp.LEnv, error) {
		newEnv := c.NewEnv
		if newEnv == nil {
			newEnv = NewForkCheckEnv
		}
		env, err := newEnv()
		if err != nil {
			return nil, err
		}
		if rc := env.LoadString("program.lisp", c.Program); rc.Type == lisp.LError {
			return nil, lisp.GoError(rc)
		}
		return env, nil
	}
	run := func(env *lisp.LEnv) (rendering, error) {
		res := env.LoadStringContext(&countdownContext{n: c.Trip}, "probe.lisp", c.Probe)
		if res.Type != lisp.LError {
			return rendering{}, fmt.Errorf("the probe did not raise at trip point %d: %v", c.Trip, res)
		}
		return renderError(res), nil
	}

	base, err := build()
	if err != nil {
		return nil, err
	}
	baseline, err := run(base)
	if err != nil {
		return nil, err
	}
	var out []Witness
	if c.WantSite != "" && !strings.Contains(baseline.text, c.WantSite) {
		// Property 3 stated directly: a budget error that trips at a
		// function-body entry must report the function's DEFINITION site.
		// It is a witness rather than a harness error so that the sweep
		// below still runs — a call environment reading its captured
		// environment's LIVE register (PR #578's F1) fails both this and
		// the bleed sweep, and seeing both is what tells an operator the
		// two are the same defect.
		out = append(out, Witness{
			Walker:   "location",
			Property: "a budget error at a function-body entry reports the definition site",
			Baseline: c.WantSite,
			Observed: baseline.String(),
			Detail: "the call environment is not carrying the definition-site snapshot the function " +
				"captured (funData.loc); it is reporting whatever register it inherited",
			Repro: c.Repro,
		})
	}
	// Replay determinism: the same probe on the same environment must
	// render identically, or every comparison below is noise.
	if again, err := run(base); err != nil {
		return nil, err
	} else if !again.equal(baseline) {
		return nil, fmt.Errorf("the probe does not replay: %s vs %s", baseline, again)
	}

	// Property 1 (and, at a body-entry trip point, property 3): stamping any
	// reachable environment changes nothing.  Each sweep step gets a fresh
	// environment, because a stamped register cannot be restored to "absent"
	// from outside the kernel.
	nEnv, truncated, err := countReachableEnvs(build, c.maxEnvs())
	if err != nil {
		return nil, err
	}
	if truncated {
		out = append(out, truncationWitness(c, "the location sweep covers every reachable environment"))
	}
	if nEnv == 0 {
		return nil, errors.New("the program leaves no reachable environment to stamp; the sweep would be vacuous")
	}
	for i := range nEnv {
		env, err := build()
		if err != nil {
			return nil, err
		}
		envs, _ := reachableEnvs(env, c.maxEnvs())
		if i >= len(envs) {
			break
		}
		if err := stampLocation(envs[i].env, sentinelLocation(i)); err != nil {
			return nil, fmt.Errorf("stamping %s: %w", envs[i].path, err)
		}
		got, err := run(env)
		if err != nil {
			return nil, err
		}
		if !got.equal(baseline) {
			out = append(out, Witness{
				Walker:   "location",
				Property: "a location stamped on one environment is invisible to an error raised elsewhere",
				Leak:     envs[i].path,
				Baseline: baseline.String(),
				Observed: got.String(),
				Detail: fmt.Sprintf("stamped %s on the environment reached at %s",
					sentinelLocation(i).File, envs[i].path),
				Repro: c.Repro,
			})
		}
	}

	// Property 2: a fork carries no evaluator location at all.  The
	// template's environments are STAMPED first, so the assertion cannot
	// pass by there being nothing to inherit — which is how it would pass
	// on a core environment, whose load leaves every register empty
	// (LoadString saves and restores the caller's register).
	fresh, err := build()
	if err != nil {
		return nil, err
	}
	stampedTemplate := 0
	freshEnvs, freshTruncated := reachableEnvs(fresh, c.maxEnvs())
	if freshTruncated {
		out = append(out, truncationWitness(c, "the fork sweep covers every reachable environment"))
	}
	for i, e := range freshEnvs {
		if err := stampLocation(e.env, sentinelLocation(2000+i)); err != nil {
			return nil, err
		}
		stampedTemplate++
	}
	if err := stampLocation(fresh, sentinelLocation(2999)); err != nil {
		return nil, err
	}
	if fresh.Source() == nil {
		return nil, errors.New("the template's evaluator location register is empty after being stamped; the fork assertions would pass vacuously")
	}
	fork, err := c.fork(fresh)
	if err != nil {
		return nil, err
	}
	if got := fork.Source(); got != nil {
		out = append(out, Witness{
			Walker:   "Fork",
			Property: "a fork starts with an empty evaluator location register",
			Leak:     "<the fork's own environment>",
			Baseline: "<none>",
			Observed: fmt.Sprintf("%s:%d:%d", got.File, got.Line, got.Col),
			Repro:    c.Repro,
		})
	}
	forkEnvs, _ := reachableEnvs(fork, c.maxEnvs())
	for _, e := range forkEnvs {
		if loc := e.env.Source(); loc != nil {
			out = append(out, Witness{
				Walker:   "Fork",
				Property: "a fork starts with an empty evaluator location register",
				Leak:     e.path,
				Baseline: "<none>",
				Observed: fmt.Sprintf("%s:%d:%d", loc.File, loc.Line, loc.Col),
				Detail: fmt.Sprintf("%d template environment(s) were stamped with a location-sentinel-2xxx.lisp location before the fork",
					stampedTemplate+1),
				Repro: c.Repro,
			})
		}
	}

	// Property 4: the same, across forks.  A sentinel stamped inside fork A
	// must be invisible to a transaction on fork B and on the template.
	cross, err := crossForkLocationWitnesses(c, build, run, baseline)
	if err != nil {
		return nil, err
	}
	return append(out, cross...), nil
}

// crossForkLocationWitnesses stamps every environment of one fork and
// asserts that a sibling fork and the template still render an identical
// error.  This is the transaction-isolation form of the location property:
// substrate runs every transaction on its own fork, so a location that
// crossed from one fork to another is one customer transaction's source
// position appearing in another's error.
//
// A FORK HAS ITS OWN BASELINE, and it is not the template's.  Fork drops
// the location register of every environment it remaps, and funData.loc
// with it (lisp/fork.go), so a budget error that trips at a function-body
// entry reports the definition site on the template and the call site on a
// fork — a documented difference, not a bleed.  Comparing a fork against
// the template's rendering would report that difference on every run; the
// baseline for the fork arms is therefore taken on a PRISTINE fork of an
// unstamped template, and the template arm keeps the template's.
func crossForkLocationWitnesses(c LocationCheck, build func() (*lisp.LEnv, error), run func(*lisp.LEnv) (rendering, error), tmplBaseline rendering) ([]Witness, error) {
	pristine, err := build()
	if err != nil {
		return nil, err
	}
	pristineFork, err := c.fork(pristine)
	if err != nil {
		return nil, err
	}
	forkBaseline, err := run(pristineFork)
	if err != nil {
		return nil, err
	}

	tmpl, err := build()
	if err != nil {
		return nil, err
	}
	forkA, err := c.fork(tmpl)
	if err != nil {
		return nil, err
	}
	forkB, err := c.fork(tmpl)
	if err != nil {
		return nil, err
	}
	stamped := 0
	forkAEnvs, forkATruncated := reachableEnvs(forkA, c.maxEnvs())
	for i, e := range forkAEnvs {
		if err := stampLocation(e.env, sentinelLocation(1000+i)); err != nil {
			return nil, err
		}
		stamped++
	}
	if stamped == 0 {
		return nil, errors.New("the fork left no environment to stamp; the cross-fork sweep would be vacuous")
	}
	var out []Witness
	if forkATruncated {
		out = append(out, truncationWitness(c, "the cross-fork sweep covers every reachable environment"))
	}
	for _, arm := range []struct {
		name string
		env  *lisp.LEnv
		want rendering
	}{
		{"a sibling fork", forkB, forkBaseline},
		{"the template", tmpl, tmplBaseline},
	} {
		got, err := run(arm.env)
		if err != nil {
			return nil, err
		}
		if !got.equal(arm.want) {
			out = append(out, Witness{
				Walker:   "Fork",
				Property: "a location stamped inside one fork is invisible to " + arm.name,
				Baseline: arm.want.String(),
				Observed: got.String(),
				Detail: fmt.Sprintf("every environment of a sibling fork (%d of them) was stamped with a location-sentinel-1xxx.lisp location",
					stamped),
				Repro: c.Repro,
			})
		}
	}
	return out, nil
}

// ReachableEnvironments returns every environment reachable from env's
// package bindings and lexical chain, excluding the global/root
// environment, in a deterministic order.  It is exported because an
// embedder auditing its own template wants the same enumeration, and
// because the guard-on-the-guard tests build a deliberately broken fork by
// stamping the environments a real fork must leave empty.
//
// It enumerates at most DefaultMaxEnvironments.  The second return reports
// whether that limit was reached, i.e. whether the enumeration is PARTIAL —
// an embedder auditing its own template must not read a short list as
// "this is everything", which is the same silent cliff the sweep itself
// used to have.
func ReachableEnvironments(env *lisp.LEnv) ([]*lisp.LEnv, bool) {
	return ReachableEnvironmentsN(env, DefaultMaxEnvironments)
}

// ReachableEnvironmentsN is ReachableEnvironments with an explicit cap, so
// a caller who raised a check's MaxEnvironments can enumerate to the same
// depth.  Without it the helper the docs send an embedder to would keep
// truncating at the default after they had raised the cap everywhere else.
func ReachableEnvironmentsN(env *lisp.LEnv, limit int) ([]*lisp.LEnv, bool) {
	// A non-positive limit means the default, matching
	// LocationCheck.MaxEnvironments.  Without the clamp the slice that
	// applies the limit panics, and this is an exported entry point.
	if limit <= 0 {
		limit = DefaultMaxEnvironments
	}
	reached, truncated := reachableEnvs(env, limit)
	out := make([]*lisp.LEnv, 0, len(reached))
	for _, e := range reached {
		out = append(out, e.env)
	}
	return out, truncated
}

// StampEvaluatorLocation writes loc into env's evaluator location register,
// by evaluating a node that carries it — the only route an outside caller
// has, and the same one eval itself takes.  Exported for the same reason as
// ReachableEnvironments: the negative controls need to build a fork that
// carries a location a real fork drops.
func StampEvaluatorLocation(env *lisp.LEnv, loc *token.Location) error {
	return stampLocation(env, loc)
}

// reachedEnv is one environment the walk found, with the path that reached
// it.
type reachedEnv struct {
	env  *lisp.LEnv
	path string
}

// DefaultMaxEnvironments bounds the location sweep, which rebuilds the
// whole environment once per stamped environment.  A loaded standard
// library leaves a handful.
//
// A program that leaves more does not get a quietly shortened sweep.  It
// gets a partial-coverage witness, because truncation and a clean result
// are indistinguishable to a reader otherwise: with forty let-bound
// closures the forty-first environment is never stamped, so a fork
// carrying a stale location THERE is invisible while the identical bug on
// the first environment is caught.  That is a coverage cliff at a size real
// programs reach — substrate's router shape is a dispatch table of handlers
// — and it was silent until the adversarial review of #599 proved it.
//
// The value is 128 rather than a tighter number because the guard's own
// motivating workload is substrate's router, a dispatch table of handlers,
// and a cap an ordinary program trips is a failure that is not a bug --
// which trains an embedder to raise the cap reflexively, the opposite of
// what a loud signal is for.  At 24 a dispatch table of 23 handlers
// already truncated (n handlers leave n+2 environments).  The cap costs
// nothing when it is not reached; the sweep rebuilds the whole environment
// once per stamped environment,
// so its cost grows superlinearly in the environment count.  Measured on a
// 4-core box, best of four full sweeps at MaxEnvironments 4096: 24
// environments 34ms, 62 environments 136ms, 128 environments 480ms.  That
// is 5.3x the environments for 14x the time, about n^1.6.  Reproduce it by
// timing CheckLocations over manyScopesProgram(n-2) with the cap raised
// past n; absolute numbers move with the machine, and the exponent is the
// part to plan against.
//
// Raise it per check with LocationCheck.MaxEnvironments.
const DefaultMaxEnvironments = 128

// reachableEnvs enumerates every environment reachable from the package
// bindings and from env's own lexical chain, in a deterministic order,
// EXCLUDING the global/root environment.  The root is excluded because it
// is the environment the probe itself is evaluated in, so eval rebinds its
// register before the probe raises and stamping it can prove nothing; the
// environments that matter are the ones a call never passes through.
//
// This is the general form of lisp/fork_metadata_test.go's forkedEnvs,
// which walks the lexical chain and the registry's direct function
// bindings; this one also reaches a closure parked inside a container.
// reachableEnvs enumerates up to limit environments.  The second return is
// true when the walk stopped at the limit, meaning the enumeration is
// PARTIAL and anything past it was never examined.
func reachableEnvs(env *lisp.LEnv, limit int) ([]reachedEnv, bool) {
	var out []reachedEnv
	// Enumerate one PAST the limit so "truncated" can mean "more than the
	// limit exist" rather than "the walk met another value after reaching
	// the limit".  The distinction matters: a program whose environment
	// count exactly EQUALS the cap was enumerated completely, and
	// reporting it as partial made the witness's own remediation fail —
	// raise the cap to the count you just measured, and it still says
	// partial.
	probe := oneMore(limit) // saturating: see oneMore for the MaxInt trap
	seenV := map[*lisp.LVal]bool{}
	seenE := map[*lisp.LEnv]bool{}
	var walk func(v *lisp.LVal, path string)
	var walkEnv func(e *lisp.LEnv, path string)
	walk = func(v *lisp.LVal, path string) {
		if v == nil || seenV[v] {
			return
		}
		if len(out) >= probe {
			return
		}
		seenV[v] = true
		switch v.Type {
		case lisp.LFun:
			walkEnv(funraw.Env(v), path+"/env")
		case lisp.LSortMap:
			if md := v.Map(); md != nil {
				for _, k := range md.Keys().Cells {
					val, _ := md.Get(k)
					walk(val, path+"/"+k.String())
				}
			}
		default:
			for i, c := range v.Cells {
				walk(c, fmt.Sprintf("%s/%d", path, i))
			}
		}
	}
	walkEnv = func(e *lisp.LEnv, path string) {
		if e == nil || seenE[e] || e.Parent() == nil {
			return
		}
		if len(out) >= probe {
			return
		}
		seenE[e] = true
		out = append(out, reachedEnv{env: e, path: path})
		keys, vals := sortedBindings(e)
		for _, k := range keys {
			walk(vals[k], path+"/"+k)
		}
		walkEnv(e.Parent(), path+"/parent")
	}
	roots(env, func(pkg, name string, v *lisp.LVal) {
		walk(v, pkg+":"+name)
	})
	walkEnv(env, "<env>")
	sort.SliceStable(out, func(i, j int) bool { return out[i].path < out[j].path })
	truncated := len(out) > limit
	if truncated {
		out = out[:limit]
	}
	return out, truncated
}

func countReachableEnvs(build func() (*lisp.LEnv, error), limit int) (int, bool, error) {
	env, err := build()
	if err != nil {
		return 0, false, err
	}
	envs, truncated := reachableEnvs(env, limit)
	return len(envs), truncated, nil
}

// truncationWitness reports a sweep that stopped at the environment cap.
// It is a FAILURE, not a note: a partial sweep and a clean sweep are
// indistinguishable to a reader, so the cliff is made loud.  Every
// truncation site routes through here so the wording and the remediation
// cannot drift apart.  See DefaultMaxEnvironments.
func truncationWitness(c LocationCheck, property string) Witness {
	return Witness{
		Walker:   "location",
		Property: property,
		// Leak renders as "leaked payload reachable at:", so it holds a
		// path, not a sentence.  There is no leaked payload here -- the
		// finding is absence of coverage -- so it names the region that
		// went unexamined instead.
		Leak:     fmt.Sprintf("<environments %d..n, never examined>", c.maxEnvs()+1),
		Baseline: "every reachable environment examined",
		Observed: fmt.Sprintf("the sweep stopped at MaxEnvironments=%d", c.maxEnvs()),
		Detail: fmt.Sprintf("This program leaves more than %d reachable environments, so the sweep is "+
			"PARTIAL: a stale location on an environment past the cap is never stamped and so cannot be "+
			"detected, while the identical leak on an environment before it would be. Raise "+
			"LocationCheck.MaxEnvironments (the cost is one environment rebuild per environment, and "+
			"grows superlinearly in the count) or narrow the program.", c.maxEnvs()),
		Repro: c.Repro,
	}
}
