// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"io"
	"maps"
	"time"
)

// VMOption configures one VM constructed by [Template.NewVM].
type VMOption func(*vmConfig)

type vmConfig struct {
	ctx        context.Context
	stderr     io.Writer
	stepBudget int64
	prewarm    bool
}

// VMWithPrewarm builds, during NewVM, every template value that any earlier
// VM of the same template has used, instead of on first use. A host that
// creates VMs ahead of demand (a pool filled by a background goroutine) uses
// it to move that work off the request path. The set is learned from use: it
// only grows, and is bounded by the template's size, so a value used once by
// any VM is built by every later prewarmed VM. Results are identical with or
// without it; it has no effect on a TemplateWithEagerInstantiation template.
func VMWithPrewarm() VMOption {
	return func(c *vmConfig) { c.prewarm = true }
}

// VMWithContext binds the new VM's evaluation context. The source context is
// never inherited. Per-call LEnv context methods remain available afterwards.
func VMWithContext(ctx context.Context) VMOption {
	return func(c *vmConfig) { c.ctx = ctx }
}

// VMWithStepBudget installs a shared step budget of n steps on the new VM's
// Runtime, as Runtime.SetStepBudget does: every top-level evaluation on the
// VM draws from it, and exhausting it raises CondStepBudgetExceeded. A
// template never carries a budget from its source runtime. n <= 0 leaves the
// VM unlimited.
func VMWithStepBudget(n int64) VMOption {
	return func(c *vmConfig) { c.stepBudget = n }
}

// VMWithStderr overrides the new VM's diagnostic writer. Without an override,
// VMs share the publication-time writer, which must support concurrent use if
// multiple VMs write to it.
func VMWithStderr(w io.Writer) VMOption {
	return func(c *vmConfig) { c.stderr = w }
}

// templateRuntime is configuration by value, not a retained Runtime. Reader,
// Library, LoadCache and Stderr are host-provided shared services with the same
// concurrency contracts as on independently constructed environments.
type templateRuntime struct {
	reader                 Reader
	library                SourceLibrary
	loadCache              LoadCache
	settings               map[string]bool
	values                 map[string]any
	stderr                 io.Writer
	currentPackage         string
	languagePackage        string
	maxAlloc               int
	maxMacroExpansionDepth int
	maxValueDepth          int
	maxEvalNesting         int
	maxSleep               time.Duration
	maxSteps               int64
	numenv                 uint64
	numsym                 uint64
	maxHeightLogical       int
	maxHeightPhysical      int
	maxTailIterations      int
	hasCurrentPackage      bool
	legacyKeywordFormals   bool
}

func snapshotTemplateRuntime(rt *Runtime) templateRuntime {
	c := templateRuntime{
		reader: rt.Reader, library: rt.Library, loadCache: rt.LoadCache, stderr: rt.Stderr,
		languagePackage: rt.Registry.Lang, maxAlloc: rt.MaxAlloc, maxMacroExpansionDepth: rt.MaxMacroExpansionDepth,
		maxValueDepth: rt.MaxValueDepth, maxEvalNesting: rt.MaxEvalNesting, maxSleep: rt.MaxSleep, maxSteps: rt.maxSteps, numenv: rt.numenv.Load(), numsym: rt.numsym.Load(),
		maxHeightLogical: rt.Stack.MaxHeightLogical, maxHeightPhysical: rt.Stack.MaxHeightPhysical, maxTailIterations: rt.Stack.MaxTailIterations,
		legacyKeywordFormals: rt.LegacyKeywordFormals,
	}
	if len(rt.settings) > 0 {
		c.settings = maps.Clone(rt.settings)
	}
	if len(rt.values) > 0 {
		c.values = maps.Clone(rt.values)
	}
	if rt.Package != nil {
		c.currentPackage = rt.Package.Name
		c.hasCurrentPackage = true
	}
	return c
}

// Every VM starts with empty evaluation/condition stacks, location registers,
// counters for evaluation work, context, debugger and profiler. Identifier
// counters continue past publication so inherited function/gensym IDs stay unique.
func (c templateRuntime) newRuntime(opts vmConfig, packages int) *Runtime {
	rt := &Runtime{
		Registry: &PackageRegistry{packages: make(map[string]*Package, packages), Lang: c.languagePackage}, Stderr: c.stderr,
		Stack:  &CallStack{MaxHeightLogical: c.maxHeightLogical, MaxHeightPhysical: c.maxHeightPhysical, MaxTailIterations: c.maxTailIterations},
		Reader: c.reader, Library: c.library, LoadCache: c.loadCache,
		MaxAlloc: c.maxAlloc, MaxMacroExpansionDepth: c.maxMacroExpansionDepth,
		MaxValueDepth: c.maxValueDepth, MaxEvalNesting: c.maxEvalNesting, MaxSleep: c.maxSleep, maxSteps: c.maxSteps,
		LegacyKeywordFormals: c.legacyKeywordFormals,
	}
	rt.numenv.Store(c.numenv)
	rt.numsym.Store(c.numsym)
	if c.settings != nil {
		rt.settings = maps.Clone(c.settings)
	}
	if c.values != nil {
		rt.values = c.values
		rt.valuesShared = true
	}
	// The VM's environments are built by the planner rather than by
	// NewEnvRuntime, so bind the fresh registry to the fresh runtime here:
	// admission into this VM must read THIS runtime's value-depth limit, and
	// the source runtime is not reachable from the plan at all.
	bindRegistryRuntime(rt)
	if opts.stderr != nil {
		rt.Stderr = opts.stderr
	}
	return rt
}
