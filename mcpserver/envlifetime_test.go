package mcpserver

import (
	"context"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/modelcontextprotocol/go-sdk/mcp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// envTracker is a RequestEnvFactory that records how many environments it has
// handed out and how many have been released. It stands in for an embedder
// whose environments own external resources (substrate's shirotester mcp backs
// each env with an in-memory Badger DB, see issue #307): the counters are the
// observable proxy for "the resource was reclaimed".
//
// release deliberately does NOT guard with sync.Once — a double release shows
// up as released > created rather than being silently swallowed.
type envTracker struct {
	mu       sync.Mutex
	created  int
	released int
	live     int
	maxLive  int
	ctxs     []context.Context
}

func (tr *envTracker) newEnv(ctx context.Context) (*lisp.LEnv, func(), error) {
	env, err := lisplib.NewDocEnv()
	if err != nil {
		return nil, nil, err
	}
	tr.mu.Lock()
	tr.created++
	tr.live++
	if tr.live > tr.maxLive {
		tr.maxLive = tr.live
	}
	tr.ctxs = append(tr.ctxs, ctx)
	tr.mu.Unlock()

	return env, func() {
		tr.mu.Lock()
		defer tr.mu.Unlock()
		tr.released++
		tr.live--
	}, nil
}

func (tr *envTracker) counts() (created, released, live, maxLive int) {
	tr.mu.Lock()
	defer tr.mu.Unlock()
	return tr.created, tr.released, tr.live, tr.maxLive
}

func (tr *envTracker) contexts() []context.Context {
	tr.mu.Lock()
	defer tr.mu.Unlock()
	return append([]context.Context(nil), tr.ctxs...)
}

// testRegistry returns a stdlib package registry. Servers built with an env
// factory pass it so New does not fall back to building a default doc env,
// which would short-circuit the factory for the doc tool. This mirrors how
// embedders wire the server (see cmd/mcp.go).
func testRegistry(t *testing.T) *lisp.PackageRegistry {
	t.Helper()
	env, err := lisplib.NewDocEnv()
	require.NoError(t, err)
	return env.Runtime.Registry
}

// TestRequestEnvFactoryReleasesEveryEnv is the regression test for #307: every
// environment the factory hands to a tool must be released by the time that
// tool call returns, so nothing accumulates across a session.
func TestRequestEnvFactoryReleasesEveryEnv(t *testing.T) {
	calls := []struct {
		name string
		tool string
		args func(t *testing.T, root string) map[string]any
	}{
		{"doc", "doc", func(*testing.T, string) map[string]any {
			return map[string]any{"query": "map"}
		}},
		{"doc-batch", "doc", func(*testing.T, string) map[string]any {
			return map[string]any{"queries": []string{"map", "foldl"}}
		}},
		{"eval", "eval", func(*testing.T, string) map[string]any {
			return map[string]any{"expression": "(+ 1 2)"}
		}},
		{"eval-batch", "eval", func(*testing.T, string) map[string]any {
			return map[string]any{"expressions": []string{"(+ 1 1)", "(+ 2 2)"}}
		}},
		{"test", "test", func(t *testing.T, root string) map[string]any {
			t.Helper()
			path := filepath.Join(root, "lifetime_test.lisp")
			writeTestFile(t, path, `(test "adds" (assert= 3 (+ 1 2)))`)
			return map[string]any{"path": path}
		}},
	}

	for _, tc := range calls {
		t.Run(tc.name, func(t *testing.T) {
			tr := &envTracker{}
			root := t.TempDir()
			session, serverSession := connectTestServer(t, New(
				WithRegistry(testRegistry(t)),
				WithRequestEnvFactory(tr.newEnv),
				WithWorkspaceRoot(root),
			))
			defer closeClientSession(t, session)
			defer closeServerSession(t, serverSession)

			params := &mcp.CallToolParams{Name: tc.tool, Arguments: tc.args(t, root)}

			const iterations = 3
			for range iterations {
				res, err := session.CallTool(context.Background(), params)
				require.NoError(t, err)
				require.False(t, res.IsError)

				// Checked after every call, not just at the end: the point of
				// the release handle is that nothing survives its request.
				created, released, live, _ := tr.counts()
				assert.Positive(t, created, "tool must use the env factory")
				assert.Equal(t, created, released,
					"every env handed out must be released by the time the call returns")
				assert.Zero(t, live, "no env may outlive its request")
			}
		})
	}
}

// TestRequestEnvFactoryBatchEvalReleasesEachEnv checks the batch path holds at
// most one environment at a time. Batch eval builds one env per expression, so
// a lifetime tied to the request (rather than to the env) would pin all of them
// until the request ended.
func TestRequestEnvFactoryBatchEvalReleasesEachEnv(t *testing.T) {
	tr := &envTracker{}
	session, serverSession := connectTestServer(t, New(
		WithRegistry(testRegistry(t)),
		WithRequestEnvFactory(tr.newEnv),
		WithWorkspaceRoot(t.TempDir()),
	))
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "eval",
		Arguments: map[string]any{
			"expressions": []string{"(+ 1 1)", "(+ 2 2)", "(+ 3 3)", "(+ 4 4)"},
		},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)

	got := decodeStructured[EvalResponse](t, res)
	require.Len(t, got.Batch, 4)

	created, released, live, maxLive := tr.counts()
	assert.Equal(t, 4, created, "batch eval isolates each expression in its own env")
	assert.Equal(t, created, released)
	assert.Zero(t, live)
	assert.Equal(t, 1, maxLive,
		"each batch env must be released before the next is built")
}

// TestRequestEnvFactoryReceivesRequestContext checks the factory is handed the
// live request context, so an embedder can key cleanup off cancellation or use
// it to correlate an env with its request.
func TestRequestEnvFactoryReceivesRequestContext(t *testing.T) {
	tr := &envTracker{}
	session, serverSession := connectTestServer(t, New(
		WithRegistry(testRegistry(t)),
		WithRequestEnvFactory(tr.newEnv),
		WithWorkspaceRoot(t.TempDir()),
	))
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name:      "doc",
		Arguments: map[string]any{"query": "map"},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)

	ctxs := tr.contexts()
	require.Len(t, ctxs, 1)
	require.NotNil(t, ctxs[0])

	// The request context is cancelled once the request finishes, which is what
	// makes it usable as a per-request lifetime signal. Cancellation happens
	// after the response is written, so poll rather than assert immediately.
	deadline := time.Now().Add(2 * time.Second)
	for ctxs[0].Err() == nil && time.Now().Before(deadline) {
		time.Sleep(time.Millisecond)
	}
	assert.Error(t, ctxs[0].Err(), "request context should be cancelled once the request completes")
}

// TestWithEnvFactoryStillSupported pins the backward-compatibility promise: the
// deprecated option keeps working, and a factory with no release handle does
// not panic.
func TestWithEnvFactoryStillSupported(t *testing.T) {
	var calls int
	session, serverSession := connectTestServer(t, New(
		WithRegistry(testRegistry(t)),
		WithEnvFactory(func() (*lisp.LEnv, error) {
			calls++
			return lisplib.NewDocEnv()
		}),
		WithWorkspaceRoot(t.TempDir()),
	))
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	for _, name := range []string{"doc", "eval"} {
		args := map[string]any{"query": "map"}
		if name == "eval" {
			args = map[string]any{"expression": "(+ 1 2)"}
		}
		res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
			Name:      name,
			Arguments: args,
		})
		require.NoError(t, err)
		require.False(t, res.IsError, name)
	}
	assert.Equal(t, 2, calls, "legacy factory must still back doc and eval")
}

// TestWithRequestEnvFactoryOverridesWithEnvFactory documents that the two
// options write the same slot, so the last one applied wins.
func TestWithRequestEnvFactoryOverridesWithEnvFactory(t *testing.T) {
	var legacy, current int
	srv := New(
		WithRegistry(testRegistry(t)),
		WithEnvFactory(func() (*lisp.LEnv, error) {
			legacy++
			return lisplib.NewDocEnv()
		}),
		WithRequestEnvFactory(func(context.Context) (*lisp.LEnv, func(), error) {
			current++
			env, err := lisplib.NewDocEnv()
			return env, nil, err
		}),
	)
	session, serverSession := connectTestServer(t, srv)
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name:      "doc",
		Arguments: map[string]any{"query": "map"},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)
	assert.Zero(t, legacy)
	assert.Equal(t, 1, current)
}

// TestWithDocEnvSharesOneEnvForDoc covers the third improvement in #307: doc is
// a read-only symbol lookup, so an embedder can supply one shared env for it
// instead of paying for a fresh env per call.
func TestWithDocEnvSharesOneEnvForDoc(t *testing.T) {
	shared, err := lisplib.NewDocEnv()
	require.NoError(t, err)

	tr := &envTracker{}
	srv := New(
		WithRegistry(testRegistry(t)),
		WithDocEnv(shared),
		WithRequestEnvFactory(tr.newEnv),
		WithWorkspaceRoot(t.TempDir()),
	)
	session, serverSession := connectTestServer(t, srv)
	defer closeClientSession(t, session)
	defer closeServerSession(t, serverSession)

	for range 5 {
		res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
			Name:      "doc",
			Arguments: map[string]any{"query": "map"},
		})
		require.NoError(t, err)
		require.False(t, res.IsError)
		got := decodeStructured[DocResponse](t, res)
		require.True(t, got.Found)
	}

	created, _, _, _ := tr.counts()
	assert.Zero(t, created, "doc must reuse the shared env, not the request factory")

	// Unlike WithEnv, WithDocEnv must not redirect the diagnostics path.
	assert.Nil(t, srv.service.env, "WithDocEnv must not populate the diagnostics env")
	assert.Same(t, shared, srv.service.sharedDocEnv)

	// eval still gets isolated per-request envs from the factory.
	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name:      "eval",
		Arguments: map[string]any{"expression": "(+ 1 2)"},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)
	created, released, live, _ := tr.counts()
	assert.Equal(t, 1, created)
	assert.Equal(t, created, released)
	assert.Zero(t, live)
}

// TestWithDocEnvTakesPrecedenceOverWithEnv pins the documented precedence order
// for the doc tool.
func TestWithDocEnvTakesPrecedenceOverWithEnv(t *testing.T) {
	docOnly, err := lisplib.NewDocEnv()
	require.NoError(t, err)
	diagnostics, err := lisplib.NewDocEnv()
	require.NoError(t, err)

	srv := New(WithEnv(diagnostics), WithDocEnv(docOnly))
	env, release, err := srv.service.docEnv(context.Background())
	require.NoError(t, err)
	require.NotNil(t, release)
	release()
	assert.Same(t, docOnly, env)
	assert.Same(t, diagnostics, srv.service.env)
}

// TestRequestEnvFactoryErrorSkipsRelease pins the error contract: a factory that
// fails owns its own cleanup and the server must not call the release handle.
func TestRequestEnvFactoryErrorSkipsRelease(t *testing.T) {
	var released int
	boom := assert.AnError
	srv := New(WithRequestEnvFactory(func(context.Context) (*lisp.LEnv, func(), error) {
		return nil, func() { released++ }, boom
	}))

	_, release, err := srv.service.newTestEnv(context.Background())
	require.ErrorIs(t, err, boom)
	require.NotNil(t, release, "release must be safe to defer even on error")
	release()
	assert.Zero(t, released, "the server must not call release when the factory errors")
}
