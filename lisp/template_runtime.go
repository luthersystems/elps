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
	ctx     context.Context
	stderr  io.Writer
	prewarm bool
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
	stderr                 io.Writer
	currentPackage         string
	languagePackage        string
	maxAlloc               int
	maxMacroExpansionDepth int
	maxValueDepth          int
	maxEvalNesting         int
	maxSleep               time.Duration
	maxSteps               int64
	numenv                 atomicCounter
	numsym                 atomicCounter
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
		maxValueDepth: rt.MaxValueDepth, maxEvalNesting: rt.MaxEvalNesting, maxSleep: rt.MaxSleep, maxSteps: rt.maxSteps, numenv: rt.numenv, numsym: rt.numsym,
		maxHeightLogical: rt.Stack.MaxHeightLogical, maxHeightPhysical: rt.Stack.MaxHeightPhysical, maxTailIterations: rt.Stack.MaxTailIterations,
		legacyKeywordFormals: rt.LegacyKeywordFormals,
	}
	if len(rt.settings) > 0 {
		c.settings = maps.Clone(rt.settings)
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
func (c templateRuntime) newRuntime(opts vmConfig) *Runtime {
	rt := &Runtime{
		Registry: NewRegistry(), Stderr: c.stderr,
		Stack:  &CallStack{MaxHeightLogical: c.maxHeightLogical, MaxHeightPhysical: c.maxHeightPhysical, MaxTailIterations: c.maxTailIterations},
		Reader: c.reader, Library: c.library, LoadCache: c.loadCache,
		MaxAlloc: c.maxAlloc, MaxMacroExpansionDepth: c.maxMacroExpansionDepth,
		MaxValueDepth: c.maxValueDepth, MaxEvalNesting: c.maxEvalNesting, MaxSleep: c.maxSleep, maxSteps: c.maxSteps, numenv: c.numenv, numsym: c.numsym,
		LegacyKeywordFormals: c.legacyKeywordFormals,
	}
	rt.Registry.Lang = c.languagePackage
	if c.settings != nil {
		rt.settings = maps.Clone(c.settings)
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
