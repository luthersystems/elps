// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"io"
	"time"
)

// VMOption configures one VM constructed by [Template.NewVM].
type VMOption func(*vmConfig)

type vmConfig struct {
	ctx    context.Context
	stderr io.Writer
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
	stderr                 io.Writer
	currentPackage         string
	languagePackage        string
	maxAlloc               int
	maxMacroExpansionDepth int
	maxEvalNesting         int
	maxSleep               time.Duration
	maxSteps               int64
	numenv                 atomicCounter
	numsym                 atomicCounter
	maxHeightLogical       int
	maxHeightPhysical      int
	maxTailIterations      int
	hasCurrentPackage      bool
}

func snapshotTemplateRuntime(rt *Runtime) templateRuntime {
	c := templateRuntime{
		reader: rt.Reader, library: rt.Library, loadCache: rt.LoadCache, stderr: rt.Stderr,
		languagePackage: rt.Registry.Lang, maxAlloc: rt.MaxAlloc, maxMacroExpansionDepth: rt.MaxMacroExpansionDepth,
		maxEvalNesting: rt.MaxEvalNesting, maxSleep: rt.MaxSleep, maxSteps: rt.maxSteps, numenv: rt.numenv, numsym: rt.numsym,
		maxHeightLogical: rt.Stack.MaxHeightLogical, maxHeightPhysical: rt.Stack.MaxHeightPhysical, maxTailIterations: rt.Stack.MaxTailIterations,
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
		MaxEvalNesting: c.maxEvalNesting, MaxSleep: c.maxSleep, maxSteps: c.maxSteps, numenv: c.numenv, numsym: c.numsym,
	}
	rt.Registry.Lang = c.languagePackage
	if opts.stderr != nil {
		rt.Stderr = opts.stderr
	}
	return rt
}
