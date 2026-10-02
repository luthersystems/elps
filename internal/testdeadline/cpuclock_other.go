// Copyright © 2026 The ELPS authors

//go:build !linux

package testdeadline

// threadCPU falls back to wall time where this package has no portable way to
// read a thread's CPU time.  CI runs the suite on Linux.
func threadCPU() cpuClock { return wallClock() }

// processCPU falls back to wall time, as threadCPU does.
func processCPU(int) cpuClock { return wallClock() }
