// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"encoding/binary"
	"hash/maphash"
	"iter"
	"unsafe"
)

type packageBaseCheck struct {
	fingerprint *uint64
}

// publish records the tables after construction and before any VM can see them.
func (b *packageBase) publish() {
	fingerprint := b.fingerprint()
	b.check.fingerprint = &fingerprint
}

// fingerprintSeed is fixed for the process: a fingerprint is only compared
// with one taken earlier in the same process.
var fingerprintSeed = maphash.MakeSeed()

// fingerprint includes table boundaries, counts, keys, values and export order.
// Each string is written with its length first, so separators are
// unambiguous even for arbitrary Go-side names.
//
// Template.NewVM runs this for every frozen package of every VM, so it is
// most of the cost of an elpscheck test run (substrate's elpscheck suite
// passed its CI time limit once fidNames joined the fingerprint).  So it
// avoids the costly steps of the first version, which wrote every entry
// with fmt's %q into a SHA-256 in key order:
//   - maphash in place of SHA-256.  This is a check against a bug, not an
//     attacker, so a 64-bit hash is enough.
//   - A length prefix in place of quoting, which scanned every rune of
//     every doc string.
//   - The maps are hashed entry by entry and the entry hashes summed, which
//     is independent of iteration order, so no keys are sorted.
func (b *packageBase) fingerprint() uint64 {
	var h maphash.Hash
	h.SetSeed(fingerprintSeed)
	writeInt(&h, len(b.bindings))
	writeInt(&h, b.pending)
	for _, binding := range b.bindings {
		writeString(&h, binding.name)
		writeInt(&h, binding.value.index)
		// The shared value's address, not its contents: a frozen binding must
		// keep pointing at the same template value.
		writeInt(&h, int(uintptr(unsafe.Pointer(binding.value.shared))))
	}
	writeInt(&h, b.index.Len())
	writeInt(&h, int(sumEntries(b.index.Unordered(), func(e *maphash.Hash, v int) { writeInt(e, v) })))
	writeInt(&h, b.funNames.Len())
	writeInt(&h, int(sumEntries(b.funNames.Unordered(), writeString)))
	// The pairs in the table's own key-then-value order, so the list
	// boundaries and every element are in the hash.
	writeInt(&h, b.fidNames.Len())
	for pair := range b.fidNames.All() {
		writeString(&h, pair.Key)
		writeString(&h, pair.Value)
	}
	writeInt(&h, b.symbolDocs.Len())
	writeInt(&h, int(sumEntries(b.symbolDocs.Unordered(), writeString)))
	writeInt(&h, b.externals.Len())
	for value := range b.externals.All() {
		writeString(&h, value)
	}
	return h.Sum64()
}

// sumEntries returns the sum of the hashes of a map's entries, which does
// not depend on the order the entries are visited in.
func sumEntries[V any](entries iter.Seq2[string, V], writeValue func(*maphash.Hash, V)) uint64 {
	var sum uint64
	var e maphash.Hash
	e.SetSeed(fingerprintSeed)
	for key, value := range entries {
		e.Reset()
		writeString(&e, key)
		writeValue(&e, value)
		sum += e.Sum64()
	}
	return sum
}

func writeInt(h *maphash.Hash, n int) {
	var b [8]byte
	binary.LittleEndian.PutUint64(b[:], uint64(n))
	_, _ = h.Write(b[:])
}

func writeString(h *maphash.Hash, s string) {
	writeInt(h, len(s))
	_, _ = h.WriteString(s)
}

// checkPackageBases is also the in-package test helper for checking integrity
// after exercising a VM, without having to instantiate another one.
func checkPackageBases(packages []templatePackage) {
	for _, pkg := range packages {
		if b := pkg.base; b != nil && (b.check.fingerprint == nil || *b.check.fingerprint != b.fingerprint()) {
			panic("template: frozen package " + pkg.name + ": shared packageBase tables changed after publication")
		}
	}
}
