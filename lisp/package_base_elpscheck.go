// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"crypto/sha256"
	"fmt"
)

type packageBaseCheck struct {
	fingerprint *[sha256.Size]byte
}

// publish records the tables after construction and before any VM can see them.
func (b *packageBase) publish() {
	fingerprint := b.fingerprint()
	b.check.fingerprint = &fingerprint
}

// fingerprint includes table boundaries, counts, keys, values and export order.
// Quoted strings make separators unambiguous even for arbitrary Go-side names.
// Hash writes cannot fail; their results are deliberately ignored.
func (b *packageBase) fingerprint() [sha256.Size]byte {
	h := sha256.New()
	_, _ = fmt.Fprintf(h, "index:%d\n", b.index.Len())
	for key, value := range b.index.All() {
		_, _ = fmt.Fprintf(h, "%q:%d\n", key, value)
	}
	_, _ = fmt.Fprintf(h, "funNames:%d\n", b.funNames.Len())
	for key, value := range b.funNames.All() {
		_, _ = fmt.Fprintf(h, "%q:%q\n", key, value)
	}
	_, _ = fmt.Fprintf(h, "symbolDocs:%d\n", b.symbolDocs.Len())
	for key, value := range b.symbolDocs.All() {
		_, _ = fmt.Fprintf(h, "%q:%q\n", key, value)
	}
	_, _ = fmt.Fprintf(h, "externals:%d\n", b.externals.Len())
	for value := range b.externals.All() {
		_, _ = fmt.Fprintf(h, "%q\n", value)
	}
	var sum [sha256.Size]byte
	copy(sum[:], h.Sum(nil))
	return sum
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
