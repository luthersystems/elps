// Copyright © 2026 The ELPS authors

package elpstest

import (
	"context"
	"os"
	"os/exec"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/templatepolicy"
	"github.com/luthersystems/elps/lisp"
)

func TestOracleDeclaredImmutableRequiresStructValue(t *testing.T) {
	for _, tc := range []struct {
		name  string
		value any
		want  bool
	}{
		{"marked-value", oracleImmutablePayload{n: 17}, true},
		{"marked-pointer", &oracleImmutablePayload{n: 17}, false},
		{"typed-nil-pointer", (*oracleImmutablePayload)(nil), false},
		{"zero-size-value", templatepolicy.Marker{}, true},
		{"zero-size-pointer", &templatepolicy.Marker{}, false},
		{"unmarked-value", struct{}{}, false},
		{"nil", nil, false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			if got := oracleDeclaredImmutable(tc.value); got != tc.want {
				t.Fatalf("automatic sharing for %T = %v, want %v", tc.value, got, tc.want)
			}
		})
	}
}

// #635: a pointer inherits the marker's method but not the value's ownership.
// Setup runs after publication, so this proves the oracle itself rejects an
// undeclared shared pointer rather than relying on NewTemplate admission.
func TestForkCheckRejectsSharedMarkerPointer(t *testing.T) {
	const childFlag = "ELPS_ORACLE_SHARED_MARKER_POINTER_CHILD"
	if os.Getenv(childFlag) == "1" {
		shared := &oracleImmutablePayload{n: 17}
		RunForkCheck(t, ForkCheck{
			Setup: func(env *lisp.LEnv) error {
				return lisp.GoError(env.PutGlobal(lisp.Symbol("shared-marker"), lisp.Native(shared)))
			},
			Tx: []string{"shared-marker"},
		})
		return
	}
	executable, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()
	// #nosec G204 -- this test binary and fixed selector contain no user input.
	command := exec.CommandContext(ctx, executable, "-test.run=^TestForkCheckRejectsSharedMarkerPointer$")
	command.Env = append(os.Environ(), childFlag+"=1")
	output, err := command.CombinedOutput()
	if err == nil || !strings.Contains(string(output), "mutable payload(s) shared with tx[0] cold") {
		t.Fatalf("marker pointer escaped the intended sharing oracle: error=%v\n%s", err, output)
	}
}

func TestTemplateParityRejectsSharedMarkerPointer(t *testing.T) {
	for _, schedule := range templateParitySchedules {
		t.Run(schedule, func(t *testing.T) {
			if err := runTemplateParity(generateTemplateParity(nil), schedule, nil); err != nil {
				t.Fatalf("healthy constructor failed before injecting marker pointer: %v", err)
			}
			shared := &oracleImmutablePayload{n: 17}
			fault := func(role string, _, env *lisp.LEnv) error {
				// Both arms observe identical values and within-VM aliases.
				// Only physical cross-VM sharing distinguishes this defect.
				if role == "cold" || role == "setup" {
					return lisp.GoError(env.PutGlobal(lisp.Symbol("shared-marker"), lisp.Native(shared)))
				}
				return nil
			}
			err := runTemplateParity(generateTemplateParity(nil), schedule, fault)
			if err == nil || !strings.Contains(err.Error(), "private/shared") {
				t.Fatalf("marker pointer escaped its intended parity isolation oracle: %v", err)
			}
		})
	}
}
