// Copyright © 2026 The ELPS authors

package testdeadline

import (
	"bytes"
	"errors"
	"fmt"
	"os"
	"strconv"
	"syscall"
	"time"
)

// userHZ is the unit of the utime and stime fields of /proc/<pid>/stat.
// Linux fixes it at 100 for user space on every architecture, whatever the
// kernel's internal tick rate.
const userHZ = 100

// threadCPU returns a clock that reads the CPU time of the calling OS thread.
// The caller must hold the thread with runtime.LockOSThread for as long as the
// clock is read; the clock itself may be read from any goroutine.
func threadCPU() cpuClock {
	path := fmt.Sprintf("/proc/self/task/%d/stat", syscall.Gettid())
	return procClock(path)
}

// processCPU returns a clock that reads the CPU time of process pid, summed
// over all of its threads.
func processCPU(pid int) cpuClock {
	return procClock(fmt.Sprintf("/proc/%d/stat", pid))
}

// procClock reads utime+stime from a /proc stat file.  If the file cannot be
// read on the first call -- no /proc, or a sandbox that hides it -- the clock
// falls back to wall time, which is what every caller used before CPU time.
func procClock(path string) cpuClock {
	if _, err := readStatCPU(path); err != nil {
		return wallClock()
	}
	return func() (time.Duration, bool) {
		d, err := readStatCPU(path)
		return d, err == nil
	}
}

// readStatCPU parses utime and stime out of a /proc/<pid>/stat or
// /proc/<pid>/task/<tid>/stat file.
func readStatCPU(path string) (time.Duration, error) {
	b, err := os.ReadFile(path) //nolint:gosec // a /proc path built from this process's own pid or tid
	if err != nil {
		return 0, err
	}
	// The command name in field 2 is parenthesised and may hold spaces or
	// parentheses, so count fields from the LAST closing parenthesis.
	i := bytes.LastIndexByte(b, ')')
	if i < 0 {
		return 0, errors.New("malformed stat: " + path)
	}
	// Fields after the name start at field 3 (state); utime is field 14 and
	// stime field 15.
	f := bytes.Fields(b[i+1:])
	if len(f) < 13 {
		return 0, errors.New("short stat: " + path)
	}
	utime, err := strconv.ParseInt(string(f[11]), 10, 64)
	if err != nil {
		return 0, err
	}
	stime, err := strconv.ParseInt(string(f[12]), 10, 64)
	if err != nil {
		return 0, err
	}
	return time.Duration(utime+stime) * time.Second / userHZ, nil
}
