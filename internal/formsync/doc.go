// Copyright © 2018 The ELPS authors

// Package formsync holds no code.  Its tests guard the hand-maintained
// mirrors of lisp.DefaultSpecialOps() against drift; see formsync_test.go.
//
// This file exists so the package has a non-test Go file: `go build ./...`
// and `go vet ./...` are fine without one, but building the package by name
// fails with "no non-test Go files".
package formsync
