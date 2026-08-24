// Copyright © 2026 The ELPS authors

package lisp

// Package admission: what a registry does with an externally built package
// (issue #524).
//
// PackageRegistry.AddPackage is the second exported surface with the shape
// issue #394 found on the Program constructors: a caller hands the kernel a
// container of *LVals that the caller still holds pointers to, and the
// registry — which is the interpreter state of a Runtime — starts serving
// them.  #523 fixed the Program half with a single admission point
// (newProgram: checkLoaderExpr, sealed-throughout fast path, private
// Copy()+SealAST, reject the unsealable).  This file is the same idea sized
// for a different contract.
//
// # Why AddPackage cannot reuse newProgram's rule
//
// A Program holds parse output and nothing else, so "seal it or refuse it"
// is a complete rule: everything a Reader may legally return is either a
// syntax node (sealable) or a mistake (rejectable).  A package's symbol
// table is the opposite — it holds whatever an environment binds, and the
// legitimate contents include values the seal deliberately declines to
// cover:
//
//   - Go builtins (LFun over an LBuiltin), which are the entire point of an
//     embedder's package;
//   - lisp-defined functions, whose captured environment cannot be copied
//     and cannot be frozen;
//   - natives, sorted-maps, arrays and byte strings — reference types by
//     design, which SealAST refuses to mark because the evaluator's mutating
//     builtins write them;
//   - runtime data of every other shape.
//
// "Seal everything" would freeze storage the evaluator mutates, and "reject
// the unsealable" would reject every real package: the two in-repo consumers
// (cmd/doc.go's `--registry` merge and mcpserver's per-request docEnv, which
// is how substrate's `shirotester doc/lsp/mcp` shares a booted shiro
// registry) are packages of builtins and closures, which is to say packages
// made almost entirely of values no seal can cover.
//
// # The rule, per value class
//
// AddPackage registers a private SNAPSHOT of the package: a fresh *Package
// carrying the same name, doc, export list, per-symbol docs and function-name
// table, whose symbol table is built by classifying every bound value.
//
//	value class                                  admission
//	-------------------------------------------  --------------------------
//	singletons (Nil, true, false)                shared by reference
//	sealed throughout (parse output, quoted      shared by reference
//	  literals, values derived from them)          (the sanctioned share)
//	sealable-throughout but NOT sealed:          private Copy() + SealAST
//	  runtime-built lists, symbols, strings,       (the #524 hazard class)
//	  and numbers — "code-like trees"
//	everything else: functions, natives,         shared by reference,
//	  maps, arrays, bytes, errors, tagged          custody transferred
//	  values, and mixed trees holding one
//
// The middle row is the hazard the issue names.  A code-like tree that is not
// sealed is fresh mutable storage the caller still aliases: `stable-sort` in
// the registry's Runtime rewrites it under the caller (and under every other
// registry the same package was added to), and a write through the caller's
// retained pointer rewrites what the Runtime evaluates.  It is the
// substrate#378 corruption class with a package for a vehicle instead of a
// parse cache.  Copy severs the alias; SealAST makes the registry's copy
// safe to share onward — with the environments that `use-package` it, and
// with the checked-mode census, which from that moment holds the copy's
// fingerprint and reports any in-place write to it (lisp/
// seal_check_elpscheck.go).
//
// The sealed fast path is load-bearing, not an optimization detail: values
// that came from evaluating literals are ALREADY sealed, so a package built
// by loading lisp source is admitted with its bindings shared by reference,
// exactly as a Program of already-sealed parser output is.  Copying them
// would fork every literal in the registry for no gain.
//
// The last row is where this admission is deliberately weaker than
// newProgram's, and saying so plainly is the point of writing it down.  A
// function value, a native or a sorted-map is admitted BY REFERENCE and stays
// aliased: nothing here makes it safe for the caller to keep mutating it, and
// nothing here makes it safe to evaluate the same closure under two Runtimes.
// What AddPackage promises for those values is custody transfer, not
// isolation — the caller must stop writing them.  Rejecting them was
// considered and would break every real consumer; copying them is either
// impossible (a closure's captured *LEnv) or a semantic change (reference
// types are reference types on purpose).  The compensating controls are the
// ones that already exist: closure-free builtins are provably stateless and
// exempt from ownership checking, and everything else is covered by the
// checked-mode ownership table, which panics when one mutable value is used
// by two Runtimes.
//
// # Sibling mutators
//
// The audit that came with #524 covers every exported member of
// PackageRegistry and Package that stores a caller-supplied *LVal:
//
//   - AddPackage — admits, as above.
//   - Package.Put / Package.Update — bind one value into one package.  They
//     are the write path every `set` reaches through LEnv.PutGlobal, so an
//     admission walk there would tax the interpreter's hot path to protect a
//     transfer that is not happening: Put binds a value into the environment
//     that is already evaluating it, and LEnv.Put/PutGlobal already take the
//     checked-mode ownership sighting.  A Go caller that reaches around the
//     environment to Put into ANOTHER Runtime's package is doing the thing
//     AddPackage's snapshot exists to prevent, and the docs say so.
//   - Package.Export / Exports — names only, and Exports already copies its
//     argument slice before sorting.
//   - PackageRegistry.DefinePackage — builds a fresh package; no caller value
//     is stored.
//
// The read side is the residual: PackageRegistry.Package hands out the
// registry's live *Package, which is how the doc/LSP/MCP merges enumerate a
// booted registry in the first place, and a caller can Put through it.  That
// is the same residual the seal design records for exported LVal fields
// (docs/sealed-ast.md §2.7): the boundary stops accidental sharing, not a
// caller that goes looking for interpreter state.

// admitPackage returns the private snapshot of p that a registry stores.
// See the file comment for the per-class rule; the traversal decisions are
// in admitSymbolValue.
//
// The snapshot is taken by the calling goroutine and reads p's maps, so it
// carries the same requirement every other read of a *Package does: no other
// goroutine may be writing p at the time (issue #397).
func admitPackage(p *Package) *Package {
	adm := &Package{
		Name:       p.Name,
		Doc:        p.Doc,
		symbols:    make(map[string]*LVal, len(p.symbols)),
		symbolDocs: make(map[string]string, len(p.symbolDocs)),
		funNames:   make(map[string]string, len(p.funNames)),
	}
	if len(p.externals) > 0 {
		adm.externals = make([]string, len(p.externals))
		copy(adm.externals, p.externals)
	}
	for name, v := range p.symbols {
		adm.symbols[name] = admitSymbolValue(v)
	}
	for name, doc := range p.symbolDocs {
		adm.symbolDocs[name] = doc
	}
	// funNames is keyed by FID and admission never copies a function value
	// (functions are not sealable), so every recorded FID still names the
	// value the snapshot holds.  Carrying it over keeps stack traces and
	// error messages naming the same functions they did before the transfer.
	for fid, name := range p.funNames {
		adm.funNames[fid] = name
	}
	return adm
}

// admitSymbolValue returns the value a registry binds for one of an admitted
// package's symbols: v itself when v is already safe to share, and a private
// sealed copy when v is a code-like tree the caller may still be writing.
func admitSymbolValue(v *LVal) *LVal {
	// Cheapest question first, and the one that answers most bindings in a
	// real package: a function, native, map, array or byte string is not a
	// class the seal covers, so there is nothing to copy and nothing to
	// freeze.  It is admitted by reference under the custody-transfer half
	// of the contract.
	if v == nil || !sealableNodeType(v.Type) {
		return v
	}
	sealed, sealable := classifySymbolValue(v, cycleGuard{state: new(cycleState)})
	if sealed || !sealable {
		// Sealed throughout: the sanctioned share (immutability, not
		// confinement, is what protects it).  Not sealable throughout: a
		// list holding a function or a native, which cannot be frozen and
		// whose interior cannot be copied faithfully — reference, as above.
		return v
	}
	// The hazard class.  Copy severs every alias the caller retained (cells,
	// locations and format metadata are all detached) and clears the sealed
	// flag on the fresh storage; SealAST then freezes the copy and records
	// its fingerprint with the checked-mode census.  classifySymbolValue has
	// already established that every node of the tree is one SealAST marks,
	// so the copy is sealed throughout when this returns.
	cp := v.Copy()
	cp.SealAST()
	return cp
}

// classifySymbolValue walks v once and reports whether it is sealed
// throughout (admit by reference) and whether it is sealable throughout
// (eligible for the copy-and-seal treatment).  A node whose type SealAST
// declines to mark makes the whole value unsealable, and stops the walk
// there rather than descending into a reference type's backing.
//
// Unlike program.go's firstUnsealed, this walks values rather than parse
// output, so it cannot assume a tree: a program can store a container inside
// itself (issue #390), and Copy() on such a value would not terminate.  A
// cycle therefore reports "neither", which lands the value in the
// by-reference row where no copy is attempted.
func classifySymbolValue(v *LVal, g cycleGuard) (sealed, sealable bool) {
	if v == nil || !sealableNodeType(v.Type) {
		return false, false
	}
	g, cyclic := g.descend(v)
	if cyclic {
		return false, false
	}
	if g.tracking() {
		defer g.ascend(v)
	}
	sealed, sealable = v.IsSealed(), true
	for _, c := range v.Cells {
		cellSealed, cellSealable := classifySymbolValue(c, g)
		sealed = sealed && cellSealed
		sealable = sealable && cellSealable
		if !sealed && !sealable {
			// Neither answer can change from here: both are conjunctions.
			return false, false
		}
	}
	return sealed, sealable
}
