// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package lisp

// Production builds pay no fingerprint storage or NewVM traversal cost.
type packageBaseCheck struct{}

func (b *packageBase) publish() { _ = b.check }

func checkPackageBases(_ []templatePackage) {}
