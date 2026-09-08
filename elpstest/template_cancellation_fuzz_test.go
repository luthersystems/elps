// Copyright © 2026 The ELPS authors

package elpstest

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// FuzzTemplateCancellationParity extends #624's definition-site regression
// across generated lexical scopes, handler frames, depths and cancellation
// points. Construction and successful requests are pinned to a Go arithmetic
// model; cancellation must have exactly the cold VM's result and error location.
// The same published template serves multiple independent request contexts.
func FuzzTemplateCancellationParity(f *testing.F) {
	for _, seed := range []struct{ point, shape uint16 }{{0, 0}, {3, 0}, {6, 0}, {24, 1}, {48, 6}, {256, 7}, {65535, 65535}} {
		f.Add(seed.point, seed.shape)
	}
	f.Fuzz(func(t *testing.T, point, shape uint16) {
		if _, err := runTemplateCancellationParity(int(point)%257, int(shape), nil); err != nil {
			t.Fatal(err)
		}
	})
}

// Each VM owns its context. Err's deterministic countdown moves the exact
// interruption point without clocks, sleeps or cross-goroutine scheduler races.
type templateParityCountdown struct {
	remaining int
}

func (*templateParityCountdown) Deadline() (time.Time, bool) { return time.Time{}, false }
func (*templateParityCountdown) Done() <-chan struct{}       { return nil }
func (*templateParityCountdown) Value(any) any               { return nil }
func (c *templateParityCountdown) Err() error {
	c.remaining--
	if c.remaining < 0 {
		return context.Canceled
	}
	return nil
}

func runTemplateCancellationParity(point, shape int, fault func(*lisp.LEnv) error) (string, error) {
	depth, arg := 1+shape%6, 1+(shape/6)%19
	want := arg
	var program strings.Builder
	for i := range depth {
		bias := 1 + (shape+i)%13
		want += bias
		body := "(+ x bias)"
		if i > 0 {
			body = fmt.Sprintf("(step%d (+ x bias))", i-1)
		}
		fmt.Fprintf(&program, "(let ((bias %d)) (defun step%d (x) %s))\n", bias, i, body)
	}
	program.WriteString("(defun recover (condition &rest args) -999)\n")
	call := fmt.Sprintf("(step%d %d)", depth-1, arg)
	if shape%2 != 0 {
		call = "(handler-bind ([never-raised recover]) " + call + ")"
	}
	build := func() (*lisp.LEnv, templateParityTrust, error) {
		env := lisp.NewEnv(nil)
		env.Runtime.Reader = parser.NewReader()
		if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
			return nil, nil, lisp.GoError(rc)
		}
		trusted := make(templateParityTrust)
		roots(env, func(pkg, name string, v *lisp.LVal) {
			if v.Type == lisp.LFun && v.Builtin() != nil {
				trusted[pkg+"\x00"+name] = true
			}
		})
		if rc := env.LoadString("definitions.lisp", program.String()); rc.Type == lisp.LError {
			return nil, nil, lisp.GoError(rc)
		}
		return env, trusted, nil
	}
	source, trusted, err := build()
	if err != nil {
		return "", fmt.Errorf("source/load: %w", err)
	}
	plan, err := lisp.NewTemplate(source, trusted.option(source))
	if err != nil {
		return "", fmt.Errorf("source/publish: %w", err)
	}
	pristine := envState(source)
	var outcome string
	for vm := range 2 {
		cold, _, err := build()
		if err != nil {
			return "", fmt.Errorf("cold[%d]/load: %w", vm, err)
		}
		fork, err := plan.NewVM()
		if err != nil {
			return "", fmt.Errorf("fork[%d]: %w", vm, err)
		}
		if fault != nil {
			if err := fault(fork); err != nil {
				return "", err
			}
		}
		for _, env := range []*lisp.LEnv{cold, fork} {
			if got := env.LoadString("successful.lisp", call); got.Type != lisp.LInt || got.Int != want {
				return "", fmt.Errorf("success/model: got %v, want %d", got, want)
			}
			if got := env.LoadString("previous.lisp", "()"); !got.IsNil() {
				return "", fmt.Errorf("previous request: got %v, want nil", got)
			}
		}
		coldResult := cold.LoadStringContext(&templateParityCountdown{remaining: point}, "request.lisp", call)
		forkResult := fork.LoadStringContext(&templateParityCountdown{remaining: point}, "request.lisp", call)
		// Named functions make the entire diagnostic deterministic: do not
		// normalize user text or locations to manufacture agreement.
		if coldResult.Type != forkResult.Type || coldResult.String() != forkResult.String() {
			return "", fmt.Errorf("cancel point=%d shape=%d vm=%d: cold=%s; template=%s", point, shape, vm, coldResult, forkResult)
		}
		if coldResult.Type != lisp.LError && (coldResult.Type != lisp.LInt || coldResult.Int != want) {
			return "", fmt.Errorf("cancel/model: unexpected non-error result %v, want %d", coldResult, want)
		}
		if coldResult.Type == lisp.LError && coldResult.Str != lisp.CondContextCancelled {
			return "", fmt.Errorf("cancel/model: got condition %q, want %q", coldResult.Str, lisp.CondContextCancelled)
		}
		outcome = coldResult.String()
		// Cancellation must not poison a subsequent request or the saved plan.
		for _, env := range []*lisp.LEnv{cold, fork} {
			if got := env.LoadString("after.lisp", call); got.Type != lisp.LInt || got.Int != want {
				return "", fmt.Errorf("after cancellation/model: got %v, want %d", got, want)
			}
		}
		if envState(cold) != envState(fork) || envState(source) != pristine {
			return "", errors.New("cancellation changed cold/template/source state")
		}
	}
	return outcome, nil
}

func TestTemplateCancellationParityReachesDefinitionAndRequestSites(t *testing.T) {
	sites := make(map[string]bool)
	for point := range 48 {
		outcome, err := runTemplateCancellationParity(point, 7, nil)
		if err != nil {
			t.Fatal(err)
		}
		for _, site := range []string{"definitions.lisp:", "request.lisp:"} {
			if strings.HasPrefix(outcome, site) {
				sites[site] = true
			}
		}
	}
	if !sites["definitions.lisp:"] || !sites["request.lisp:"] {
		t.Fatalf("cancellation sweep missed required error sites: %v", sites)
	}
	outcome, err := runTemplateCancellationParity(256, 0, nil)
	if err != nil || outcome != "2" {
		t.Fatalf("late cancellation must actually finish the modelled request: got %q, %v; want 2", outcome, err)
	}
}

func TestTemplateCancellationParityRejectsChangedDefinitionLocation(t *testing.T) {
	// Preserve arithmetic exactly while introducing the historical fault's
	// observable consequence: a different definition-site diagnostic.
	fault := func(vm *lisp.LEnv) error {
		return lisp.GoError(vm.LoadString("wrong-definition.lisp", "(defun step0 (x) (+ x 1))"))
	}
	_, err := runTemplateCancellationParity(3, 0, fault)
	if err == nil || !strings.Contains(err.Error(), "cancel point=") || !strings.Contains(err.Error(), "wrong-definition.lisp:") {
		t.Fatalf("changed definition location escaped exact cancellation parity: %v", err)
	}
}
