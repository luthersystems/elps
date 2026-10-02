// Copyright © 2026 The ELPS authors

package cmd

import (
	"fmt"
	"os"
	"os/exec"
	"syscall"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/testdeadline"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"golang.org/x/sys/unix"
)

func TestReplSignalRestoresTerminal(t *testing.T) {
	master, err := os.OpenFile("/dev/ptmx", os.O_RDWR|syscall.O_NOCTTY, 0)
	if err != nil {
		t.Skipf("PTY unavailable: %v", err)
	}
	t.Cleanup(func() { _ = master.Close() })
	fd := int(master.Fd())
	require.NoError(t, unix.IoctlSetPointerInt(fd, unix.TIOCSPTLCK, 0))
	number, err := unix.IoctlGetInt(fd, unix.TIOCGPTN)
	require.NoError(t, err)
	slave, err := os.OpenFile(fmt.Sprintf("/dev/pts/%d", number), os.O_RDWR|syscall.O_NOCTTY, 0) //#nosec G304 -- kernel-provided PTY number
	require.NoError(t, err)
	t.Cleanup(func() { _ = slave.Close() })
	slaveFD := int(slave.Fd())
	original, err := unix.IoctlGetTermios(slaveFD, unix.TCGETS)
	require.NoError(t, err)
	require.NotZero(t, original.Lflag&unix.ICANON)
	bin, err := os.Executable()
	require.NoError(t, err)
	// Backstop, not a short wall-clock deadline (#789): the regressions
	// here are an input wait that does not end, which uses no CPU.
	ctx, cancel := testdeadline.Backstop(t)
	defer cancel()
	cmd := exec.CommandContext(ctx, bin, "-test.run=^TestReplTerminalHelper$") //nolint:gosec // current test executable
	cmd.Env = append(os.Environ(), "HOME="+t.TempDir(), "TERM=xterm", "ELPS_TEST_TERMINAL_HELPER=1")
	cmd.Stdin, cmd.Stdout, cmd.Stderr = slave, slave, slave
	require.NoError(t, cmd.Start())
	defer func() {
		_ = cmd.Process.Kill()
		if cmd.ProcessState == nil {
			_ = cmd.Wait()
		}
	}()

	// Observe raw mode before signalling, so cancellation happens during an
	// interactive input wait. Keep the slave open to inspect it after exit.
	rawMode := func() bool {
		state, termErr := unix.IoctlGetTermios(slaveFD, unix.TCGETS)
		return termErr == nil && state.Lflag&(unix.ICANON|unix.ECHO) == 0
	}
	for !rawMode() {
		select {
		case <-ctx.Done():
			t.Fatal("REPL did not enter raw mode")
		case <-time.After(10 * time.Millisecond):
		}
	}
	require.NoError(t, cmd.Process.Signal(syscall.SIGTERM))
	require.Error(t, cmd.Wait())
	require.NoError(t, ctx.Err(), "signal did not stop input wait")
	assert.Equal(t, 1, cmd.ProcessState.ExitCode())
	restored, err := unix.IoctlGetTermios(slaveFD, unix.TCGETS)
	require.NoError(t, err)
	assert.Equal(t, original, restored, "terminal settings must be restored before stdin closes")
}

func TestReplTerminalHelper(t *testing.T) {
	if os.Getenv("ELPS_TEST_TERMINAL_HELPER") != "1" {
		return
	}
	// Make stdin pollable so Close releases fd 0 while a read is pending.
	// A blocking stdin can defer the actual close and mask the ordering bug.
	// Do this in the child because os/exec makes inherited files blocking.
	require.NoError(t, unix.SetNonblock(0, true))
	os.Stdin = os.NewFile(0, "/dev/stdin")
	rootCmd.SetArgs([]string{"repl"})
	Execute()
}
