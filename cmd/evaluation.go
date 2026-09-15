// Copyright © 2026 The ELPS authors

package cmd

import (
	"context"
	"os"
	"os/signal"
	"syscall"
	"time"
)

// evaluationContext cancels on the first SIGINT/SIGTERM and force-exits on
// the second signal, even if a native call cannot observe cancellation.
// The caller must keep the handler installed until output and cleanup finish.
func evaluationContext(parent context.Context, timeout time.Duration) (context.Context, func()) {
	ctx, cancel := context.WithCancel(parent)
	deadlineCancel := func() {}
	if timeout > 0 {
		ctx, deadlineCancel = context.WithTimeout(ctx, timeout)
	}
	signals := make(chan os.Signal, 2)
	signal.Notify(signals, os.Interrupt, syscall.SIGTERM)
	done := make(chan struct{})
	stopped := make(chan struct{})
	go func() {
		defer close(stopped)
		interrupted := false
		for {
			select {
			case sig := <-signals:
				if interrupted {
					if sig == syscall.SIGTERM {
						os.Exit(143)
					}
					os.Exit(130)
				}
				interrupted = true
				cancel()
			case <-done:
				return
			}
		}
	}()
	return ctx, func() {
		signal.Stop(signals)
		close(done)
		<-stopped
		deadlineCancel()
		cancel()
	}
}
