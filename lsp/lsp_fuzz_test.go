// Copyright © 2026 The ELPS authors

package lsp

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"reflect"
	"regexp"
	"sort"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/internal/fuzzwatch"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// Fuzzing the LANGUAGE SERVER as a session: a sequence of requests against
// documents and cursor positions the fuzzer chose.
//
// # Why this package, and where issue #318's premise needed correcting
//
// #318 lists lsp/ as a top unfuzzed target on the grounds that it "processes
// untrusted documents from an editor, incrementally, with its own
// document-sync state machine". The first and third clauses hold. The
// SECOND DOES NOT: server.go pins TextDocumentSyncKindFull, and
// textDocumentDidChange takes the last content change as the whole document.
// There is no incremental-edit application here and therefore none of the
// range-splicing arithmetic that makes incremental sync interesting. Anyone
// picking up this bullet expecting to fuzz an edit-splicer will not find one.
//
// The real case for this package is different, and stronger than the one
// recorded:
//
//  1. THE POSITIONS ARE UNTRUSTED, NOT JUST THE TEXT. Every request carries a
//     client-supplied line/character, and the server converts between LSP's
//     0-based positions, ELPS's 1-based token.Location, and byte offsets into
//     strings.Split(content, "\n") -- by hand, in position.go and in each
//     feature file. That arithmetic is where out-of-range slicing lives, and
//     no other fuzz target in this repository supplies a position at all.
//
//  2. IT IS THE ONLY WAY TO REACH analysis/ AND lint/ FROM A FUZZ TARGET.
//     Both are on #318's unfuzzed list. didOpen runs parse -> analysis ->
//     every registered lint analyzer synchronously, so one session target
//     covers three of that list's packages.
//
//  3. THE THREAT MODEL IS REAL AND UNMEDIATED. `elps lsp` runs against
//     whatever repository the developer opened. There is no sandbox between a
//     hostile .lisp file and this code.
//
// # The recover problem, which is this target's IsInternalPanic
//
// FuzzEval needs lisp.IsInternalPanic because lisp.eval launders every Go
// panic into an ordinary-looking *LVal. The LSP layer has the same hazard in a
// different shape: it installs FOUR blanket `recover()`s that turn a panic
// into a silent no-op. A naive harness would drive the server, observe no
// crash, and prove nothing.
//
//	site                                   how this target handles it
//	-------------------------------------  --------------------------------
//	didChange's debounce closure           NEUTRALISED. The harness stops the
//	  (diagnostics.go, time.AfterFunc)     timer and runs the closure's body
//	                                       itself, so a panic in the debounced
//	                                       analysis propagates to the fuzzer
//	                                       instead of being swallowed 300ms
//	                                       later on a timer goroutine.
//	debouncedReanalyze's timer             NEUTRALISED the same way (that one
//	  (watched_files.go)                   has NO recover, so left alone it is
//	                                       an unattributable process crash on
//	                                       a goroutine -- worse than swallowed).
//	buildWorkspaceIndex                    DETECTED. It has no early return, so
//	  (server.go)                          its final `s.analysisCfg = cfg` runs
//	                                       unless it panicked. The harness
//	                                       asserts analysisCfg != nil after
//	                                       ensureWorkspaceIndex, which is a
//	                                       non-forgeable marker of a recovered
//	                                       panic in exactly the same sense
//	                                       IsInternalPanic is. Not a new idea:
//	                                       testServerWithWorkspaceIndex in
//	                                       lsp_test.go already guards this way,
//	                                       "fail fast if index build silently
//	                                       panicked".
//	EnvMacroExpander.ExpandMacro           DETECTED, since analysis/ grew the
//	  (analysis/expander.go)               marker this row used to say it
//	                                       lacked. Its recover still sets
//	                                       result = nil -- also the ordinary
//	                                       "not a macro" answer, so the RETURN
//	                                       VALUE still cannot carry the fact --
//	                                       but the expander now counts calls
//	                                       that failed to complete, keyed off a
//	                                       local in ExpandMacro's own frame that
//	                                       only a normal return sets.
//	                                       checkExpansionPanics reads that count
//	                                       through analysis.PanicReporter. Same
//	                                       family of non-forgeable marker as
//	                                       IsInternalPanic and the analysisCfg
//	                                       check above.
//
// Everything else runs with no recover between the handler body and the fuzz
// function, because the harness calls the server's handler methods directly
// rather than through glsp's dispatcher. So for the synchronous request path
// the assertion is the strongest kind available: an ordinary Go panic, which
// `go test -fuzz` attributes to the input that caused it and writes to
// testdata. Nothing has to be inferred from a return value.
//
// # What is deliberately kept out of the harness's reach
//
// URIs ARE FIXED CONSTANTS, NEVER FUZZER-DERIVED. workspaceDidChangeWatchedFiles
// and didSave both funnel into updateFileRefs, which does os.ReadFile(uriToPath(uri)).
// A fuzzer-controlled URI is a file-read primitive pointed at the machine
// running the tests. The two session URIs name a directory that does not
// exist, so the read fails and the function returns early -- the same
// principle as FuzzEval leaving Runtime.Library nil so load-file cannot reach
// the disk. The cost is that updateFileRefs' body past the read is not
// covered here.
//
// NO WORKSPACE ROOT. initialize is called with RootURI and RootPath unset, so
// PrescanWorkspace and ScanWorkspaceRefs -- which walk a directory tree -- are
// skipped. A fuzz target must not depend on, or wander through, the checkout
// it happens to be running in. What that scan FEEDS is not skipped: the
// preamble it would have collected is taken from document B instead, filtered
// by the same six head symbols, and loaded through the real
// LoadWorkspaceMacros. See workspacePreamble for why that is load-bearing
// rather than decorative -- without it, env.MacroCall is never reached and the
// analysis-time evaluation path this target claims to cover is not covered at
// all. ScanWorkspaceRefs has no such substitute, so cfg.WorkspaceRefs is
// always empty here and the cross-file reference branches in references,
// rename and call hierarchy go unexercised.
//
// SINGLE GOROUTINE. Every timer the server arms is cancelled and its body run
// inline (see the table above), and initialized's `go s.ensureWorkspaceIndex()`
// is defused by completing the sync.Once during setup. A crasher that does not
// reproduce from its own testdata file is worthless.
const (
	// Session shape. Two documents, because cross-document state --
	// the definition index, the workspace ref index, reanalyzeOpenDocuments'
	// walk over docs.All() -- does not exist with one.
	fuzzURIA = "file:///elps-fuzz-nonexistent/a.lisp"
	fuzzURIB = "file:///elps-fuzz-nonexistent/b.lisp"

	// maxOps bounds a session. Each op can trigger a full parse + analyse +
	// lint of both documents, so an unbounded script would let one input
	// spend the entire deadline.
	//
	// It must stay >= len(ops), or allOpsScript -- the seed that runs every
	// operation once -- is silently truncated and the tail of the table stops
	// being seeded. TestLSPFuzzScriptReachesEveryOp asserts that.
	maxOps = 48

	// bytesPerOp is how much script one op consumes: opcode, two position
	// bytes, and a flags byte.
	bytesPerOp = 4

	// Document size cap. Not a performance dodge -- it is what makes
	// analyzeAndPublish's over-limit branch reachable at all, while leaving
	// every realistic mutator input (far under 16KiB) on the full analysis
	// path.
	fuzzMaxDocBytes = 16384

	// Budgets for the injected LEnv. An env is injected on purpose: it is what
	// sets cfg.MacroExpander, and analysis-time macro expansion CALLS
	// env.MacroCall, i.e. it evaluates macro bodies while merely analysing a
	// document. That is arbitrary evaluation reachable from an opened file, so
	// it needs the same budgets FuzzEval applies to deliberate evaluation.
	fuzzEnvMaxSteps          = 200_000
	fuzzEnvMaxTailIterations = 10_000
	fuzzEnvMaxPhysHeight     = 2_000
	fuzzEnvMaxAlloc          = 1_000_000
	fuzzEnvMacroDepth        = 100
	fuzzEnvMaxEvalNesting    = 20_000

	// watchdogTimeout is scheduled time, not wall clock (see internal/fuzzwatch).
	lspWatchdogTimeout = 30 * time.Second
)

// session is one fuzzer-driven LSP conversation. It owns a freshly built
// Server, so no state crosses inputs.
type session struct {
	srv     *Server
	ctx     *glsp.Context
	content map[string]string

	// lastItems is the most recent prepareCallHierarchy result. Feeding it
	// back into the incoming/outgoing calls requests is what a real client
	// does, and it is the only way those two handlers see an item that
	// resolves to anything.
	lastItems []protocol.CallHierarchyItem

	// executed records which ops ran, for the coverage guard below.
	executed map[string]bool

	// marshalErr holds the first notification payload that would not
	// serialise. Notifications are sent from deep inside the server, so the
	// failure is recorded and asserted by the caller.
	marshalErr error
	marshalCtx string

	// tokenErr holds the first semantic token whose range did not lie inside
	// the document it was computed for (elps#428). Like marshalErr it is
	// recorded rather than raised, because the op that produced it has no way
	// to fail the input on its own.
	tokenErr error

	// renameErr holds the first rename edit whose range did not cover the
	// identifier being renamed (elps#463). Recorded, not raised, for the same
	// reason as tokenErr.
	renameErr error
}

// opArgs is the fuzzer's four bytes for one operation, already interpreted.
type opArgs struct {
	uri   string
	line  int
	col   int
	flags byte
	a, b  byte
}

// lspOp binds an opcode to the protocol.Handler field it drives, so the
// coverage guard can prove the two sets agree.
type lspOp struct {
	name    string
	handler string // protocol.Handler field name, or "" for wrapper-only methods
	method  string // LSP method string for wrapper-dispatched ops, else ""
	run     func(s *session, a opArgs)
}

// ops is the dispatch table. ADDING A HANDLER TO server.go WITHOUT ADDING AN
// OP HERE IS A CI FAILURE -- see TestLSPFuzzDrivesEveryWiredHandler.
//
// ORDER IS LOAD-BEARING for the seeds, though not for the fuzzer.
// allOpsScript walks this table in order and runs each op exactly once, so any
// op that REMOVES OR REPLACES the document content has to sit at the end or it
// starves everything after it. That was got wrong twice, both times caught by
// measuring rather than by reading:
//
//   - didClose at index 3 left every later op querying a document that had just
//     been dropped from the store. collectFoldingRanges, collectSemanticTokens,
//     buildSignatureHelp and buildSelectionRangeChain measured 0% from the
//     whole seed corpus.
//   - didChange at index 1 then did the same thing more quietly: with
//     allOpsScript's uniform argument bytes it takes the truncating branch of
//     editedContent and leaves document A as the single byte "(", which parses
//     to nothing. Package-wide mean per-function coverage from the seeds fell
//     from 66.7% to 48.8% and every op still "ran".
//
// The fuzzer is free to interleave these however it likes -- a request racing
// an edit is the interesting case, and finding it is the mutator's job rather
// than the seeds'.
var ops = []lspOp{
	{name: "didOpen", handler: "TextDocumentDidOpen", run: (*session).opDidOpen},
	{name: "didSave", handler: "TextDocumentDidSave", run: (*session).opDidSave},
	{name: "hover", handler: "TextDocumentHover", run: (*session).opHover},
	{name: "definition", handler: "TextDocumentDefinition", run: (*session).opDefinition},
	{name: "completion", handler: "TextDocumentCompletion", run: (*session).opCompletion},
	{name: "references", handler: "TextDocumentReferences", run: (*session).opReferences},
	{name: "highlight", handler: "TextDocumentDocumentHighlight", run: (*session).opHighlight},
	{name: "documentSymbol", handler: "TextDocumentDocumentSymbol", run: (*session).opDocumentSymbol},
	{name: "rename", handler: "TextDocumentRename", run: (*session).opRename},
	{name: "prepareRename", handler: "TextDocumentPrepareRename", run: (*session).opPrepareRename},
	{name: "formatting", handler: "TextDocumentFormatting", run: (*session).opFormatting},
	{name: "signatureHelp", handler: "TextDocumentSignatureHelp", run: (*session).opSignatureHelp},
	{name: "codeAction", handler: "TextDocumentCodeAction", run: (*session).opCodeAction},
	{name: "foldingRange", handler: "TextDocumentFoldingRange", run: (*session).opFoldingRange},
	{name: "semanticTokens", handler: "TextDocumentSemanticTokensFull", run: (*session).opSemanticTokens},
	{name: "selectionRange", handler: "TextDocumentSelectionRange", run: (*session).opSelectionRange},
	{name: "linkedEditing", handler: "TextDocumentLinkedEditingRange", run: (*session).opLinkedEditing},
	{name: "prepareCallHierarchy", handler: "TextDocumentPrepareCallHierarchy", run: (*session).opPrepareCallHierarchy},
	{name: "incomingCalls", handler: "CallHierarchyIncomingCalls", run: (*session).opIncomingCalls},
	{name: "outgoingCalls", handler: "CallHierarchyOutgoingCalls", run: (*session).opOutgoingCalls},
	{name: "watchedFiles", handler: "WorkspaceDidChangeWatchedFiles", run: (*session).opWatchedFiles},
	{name: "workspaceSymbol", handler: "WorkspaceSymbol", run: (*session).opWorkspaceSymbol},
	{name: "inlayHint", method: "textDocument/inlayHint", run: (*session).opInlayHint},
	{name: "setTrace", handler: "SetTrace", run: (*session).opSetTrace},
	{name: "initialized", handler: "Initialized", run: (*session).opInitialized},
	// Content-replacing and session-ending operations last; see the note above
	// the table.
	{name: "didChange", handler: "TextDocumentDidChange", run: (*session).opDidChange},
	{name: "didClose", handler: "TextDocumentDidClose", run: (*session).opDidClose},
	{name: "shutdown", handler: "Shutdown", run: (*session).opShutdown},
	{name: "exit", handler: "Exit", run: (*session).opExit},
}

// setupHandlers are driven once by newSession rather than by an opcode, with
// the reason each is not fuzzer-steerable. The coverage guard treats these as
// covered; the list may shrink but must not grow without a reason.
var setupHandlers = map[string]string{
	"Initialize": "driven once by newSession with RootURI/RootPath unset;" +
		" a fuzzer-supplied root would make the target walk the machine's filesystem",
}

// ---------------------------------------------------------------------------
// session construction
// ---------------------------------------------------------------------------

// preambleHeads are the top-level forms PrescanWorkspace collects into
// Prescan.Preamble and LoadWorkspaceMacros then EVALUATES. Copied from the
// switch in analysis/workspace.go scanFileFull, so the harness replays what
// production would replay and not a superset of it.
var preambleHeads = map[string]bool{
	"in-package": true, "use-package": true, "export": true,
	"defmacro": true, "defun": true, "set": true,
}

// workspacePreamble extracts from src the forms a workspace scan would have
// collected out of a neighbouring file.
//
// This is what makes env.MacroCall reachable, and only a measurement showed it
// was needed. Injecting an env sets cfg.MacroExpander, which looks sufficient
// for analysis-time macro EVALUATION to be covered. Coverage said otherwise:
// EnvMacroExpander.ExpandMacro ran at 74%, and its last three statements --
// the ones that actually call MacroCall -- ran at 0%. The analyser consults
// the expander only when the head symbol is a known macro or is unknown
// entirely, and a stdlib macro is neither (it resolves to an ordinary builtin
// symbol) while a macro defined in the document under analysis is not in the
// env at all. The only macro that is BOTH present in the env AND unknown to
// the analyser is one that came from another workspace file -- exactly what
// buildWorkspaceIndex loads through LoadWorkspaceMacros.
//
// So document B doubles as the neighbouring workspace file, and its preamble
// is loaded before the index is built, in the order buildWorkspaceIndex uses.
// That reaches the sharpest edge in the whole surface: merely OPENING a
// document evaluates a macro body that came off disk. It is also why the
// injected env carries FuzzEval's budgets.
func workspacePreamble(src []byte) []*lisp.LVal {
	sc := token.NewScanner("workspace", bytes.NewReader(src))
	res := rdparser.New(sc).ParseProgramFaultTolerant()
	var preamble []*lisp.LVal
	for _, expr := range res.Exprs {
		if expr.Type != lisp.LSExpr || expr.Quoted || len(expr.Cells) == 0 {
			continue
		}
		if preambleHeads[astutil.HeadSymbol(expr)] {
			preamble = append(preamble, expr)
		}
	}
	return preamble
}

// newSession builds a server with an injected environment and completes the
// initialize handshake. Returns nil if the environment could not be built,
// which the caller treats as a harness failure rather than a finding.
func newSession(preamble []*lisp.LVal) (*session, error) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	// Runtime.Library stays nil so load-file cannot reach the disk, exactly as
	// in FuzzEval. Stderr is discarded: the analyser may evaluate macro bodies
	// that print.
	if rc := lisp.InitializeUserEnv(env,
		lisp.WithStderr(discard{}),
		lisp.WithMaxSteps(fuzzEnvMaxSteps),
		lisp.WithMaxTailIterations(fuzzEnvMaxTailIterations),
		lisp.WithMaximumPhysicalStackHeight(fuzzEnvMaxPhysHeight),
		lisp.WithMaxAlloc(fuzzEnvMaxAlloc),
		lisp.WithMaxMacroExpansionDepth(fuzzEnvMacroDepth),
		lisp.WithMaxEvalNesting(fuzzEnvMaxEvalNesting),
	); rc.Type == lisp.LError {
		return nil, fmt.Errorf("InitializeUserEnv: %v", rc)
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		return nil, fmt.Errorf("LoadLibrary: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		return nil, fmt.Errorf("InPackage: %v", rc)
	}

	s := &session{
		content:  map[string]string{},
		executed: map[string]bool{},
	}
	s.srv = New(
		WithEnv(env),
		WithRegistry(env.Runtime.Registry),
		WithMaxDocumentBytes(fuzzMaxDocBytes),
	)
	// exit() calls exitFn, which defaults to os.Exit. Stubbing it is what lets
	// the exit handler be driven like any other rather than exempted.
	s.srv.exitFn = func(int) {}

	s.ctx = &glsp.Context{
		Notify: func(method string, params any) {
			s.checkMarshal("notification "+method, params)
		},
		// Call stays nil so initialized does not spawn its client-callback
		// goroutine. Nothing in the harness answers a client request.
	}

	// Stand in for the LoadWorkspaceMacros call inside buildWorkspaceIndex,
	// which the harness cannot trigger for real because it deliberately has no
	// workspace root. Same position in the order: preamble first, then the
	// index build creates the EnvMacroExpander over the resulting env.
	//
	// This EVALUATES fuzzer-derived forms -- that is what production does with
	// the preamble, and the reason the env is budgeted.
	_ = analysis.LoadWorkspaceMacros(env, preamble)

	// No RootURI and no RootPath: the workspace scan is skipped entirely.
	res, err := s.srv.initialize(s.ctx, &protocol.InitializeParams{})
	if err != nil {
		return nil, fmt.Errorf("initialize: %w", err)
	}
	s.checkMarshal("initialize result", res)

	// Complete the index build synchronously BEFORE anything can arm the
	// goroutine in initialized: sync.Once then makes that goroutine a no-op,
	// and the session stays single-goroutine.
	s.srv.ensureWorkspaceIndex()
	if err := s.checkIndexBuilt(); err != nil {
		return nil, err
	}
	return s, nil
}

// discard is an io.Writer that drops everything, so a macro body that prints
// during analysis-time expansion does not spam the fuzzing log.
type discard struct{}

func (discard) Write(p []byte) (int, error) { return len(p), nil }

// checkIndexBuilt is the swallowed-panic detector for buildWorkspaceIndex.
//
// That function ends with `s.analysisCfg = cfg` and contains no return
// statement, so once ensureWorkspaceIndex' sync.Once has fired, a nil
// analysisCfg means control left the function early -- which it can only do
// through the `defer func() { _ = recover() }()` at the top. The blanket
// recover makes the panic invisible; this makes its EFFECT visible.
func (s *session) checkIndexBuilt() error {
	s.srv.analysisCfgMu.RLock()
	cfg := s.srv.analysisCfg
	s.srv.analysisCfgMu.RUnlock()
	if cfg == nil {
		return errors.New("buildWorkspaceIndex left analysisCfg nil after ensureWorkspaceIndex;" +
			" that function has no early return, so it panicked and its blanket recover()" +
			" swallowed the panic")
	}
	return nil
}

// checkExpansionPanics is the swallowed-panic detector for analysis-time macro
// expansion, which the table above used to list as NOT COVERED.
//
// Analysing a document CALLS env.MacroCall on macros that came off disk, so a
// macro body is evaluated while a file is merely open in an editor. The recover
// around that returns nil, and nil is also the answer for "not a macro" -- the
// answer the analyser gets for almost every head symbol it asks about -- so no
// return value could ever have distinguished the two.
//
// analysis.EnvMacroExpander now counts calls that did not complete, keyed off a
// local variable in ExpandMacro's own frame. Nothing reachable from lisp, and
// nothing an embedder holds, can set that local; the count is unexported and
// only ever increases. Reading it here is the same move as asserting
// analysisCfg != nil after ensureWorkspaceIndex, one layer down.
func (s *session) checkExpansionPanics() error {
	s.srv.analysisCfgMu.RLock()
	cfg := s.srv.analysisCfg
	s.srv.analysisCfgMu.RUnlock()
	if cfg == nil {
		return nil // already reported by checkIndexBuilt
	}
	reporter, ok := cfg.MacroExpander.(analysis.PanicReporter)
	if !ok {
		return nil
	}
	n := reporter.ExpansionPanics()
	if n == 0 {
		return nil
	}
	detail := ""
	if exp, ok := cfg.MacroExpander.(*analysis.EnvMacroExpander); ok {
		if rec := exp.LastExpansionPanic(); rec != nil {
			detail = fmt.Sprintf("\n  macro:   %s\n  package: %s\n  value:   %v\n  stack:\n%s",
				rec.Macro, rec.Package, rec.Value, rec.GoStack)
		}
	}
	return fmt.Errorf(
		"EnvMacroExpander swallowed %d panic(s) during analysis-time macro expansion."+
			" A document merely being open evaluated a macro body that crashed host"+
			" code, and ExpandMacro's recover() turned that into the same nil it"+
			" returns for \"not a macro\"%s", n, detail)
}

func (s *session) checkMarshal(what string, v any) {
	if v == nil || s.marshalErr != nil {
		return
	}
	if _, err := json.Marshal(v); err != nil {
		s.marshalErr = err
		s.marshalCtx = what
	}
}

// ---------------------------------------------------------------------------
// script decoding
// ---------------------------------------------------------------------------

// decode turns four script bytes into an operation and its arguments.
//
// Positions are derived two ways on purpose. The modulo form lands mostly
// inside the document (so requests resolve to real symbols and reach the
// interesting code), while flagRawPos hands the raw byte through unclamped and
// flagWildPos multiplies it far past any document -- both of which are what a
// misbehaving or malicious client sends, and where the index arithmetic in
// position.go and the feature files is actually tested.
const (
	flagDocB    byte = 1 << 0
	flagRawPos  byte = 1 << 1
	flagWildPos byte = 1 << 2
)

func (s *session) decode(chunk []byte) (lspOp, opArgs) {
	op := ops[int(chunk[0])%len(ops)]
	flags := chunk[3]

	uri := fuzzURIA
	if flags&flagDocB != 0 {
		uri = fuzzURIB
	}

	lines := strings.Split(s.content[uri], "\n")
	width := 1
	for _, ln := range lines {
		if len(ln) > width {
			width = len(ln)
		}
	}

	var line, col int
	switch {
	case flags&flagWildPos != 0:
		// Absurd but legal: UInteger is 32 bits, and clients do send
		// end-of-file sentinels shaped like this.
		line = 1<<20 + int(chunk[1])
		col = 1<<20 + int(chunk[2])
	case flags&flagRawPos != 0:
		line = int(chunk[1])
		col = int(chunk[2])
	default:
		// +2 so just-past-the-end is reachable without being the common case.
		line = int(chunk[1]) % (len(lines) + 2)
		col = int(chunk[2]) % (width + 2)
	}

	return op, opArgs{uri: uri, line: line, col: col, flags: flags, a: chunk[1], b: chunk[2]}
}

func (a opArgs) pos() protocol.Position {
	return protocol.Position{Line: safeUint(a.line), Character: safeUint(a.col)}
}

func (a opArgs) ident() protocol.TextDocumentIdentifier {
	return protocol.TextDocumentIdentifier{URI: a.uri}
}

func (a opArgs) tdpp() protocol.TextDocumentPositionParams {
	return protocol.TextDocumentPositionParams{TextDocument: a.ident(), Position: a.pos()}
}

// word slices a short string out of the fuzzer's own bytes, so rename targets
// and symbol queries are mutator-steerable rather than constant.
func word(src []byte, a, b byte) string {
	if len(src) == 0 {
		return ""
	}
	start := int(a) % len(src)
	n := int(b)%16 + 1
	end := start + n
	if end > len(src) {
		end = len(src)
	}
	return string(src[start:end])
}

// ---------------------------------------------------------------------------
// operations
// ---------------------------------------------------------------------------

func (s *session) opDidOpen(a opArgs) {
	text := s.content[a.uri]
	_ = s.srv.textDocumentDidOpen(s.ctx, &protocol.DidOpenTextDocumentParams{
		TextDocument: protocol.TextDocumentItem{
			URI: a.uri, LanguageID: "lisp", Version: 1, Text: text,
		},
	})
}

// editedContent is what didChange installs. It has to differ from what is
// already open, or the change path is a no-op dressed as an edit: the first
// version of this harness re-sent the identical text, so the version counter
// moved but the content never did, and none of the stale-analysis machinery --
// the cached-analysis invalidation, the publishedVersion guard,
// analyzeAndPublish's two version comparisons -- was ever put under strain.
//
// Three shapes, chosen by the fuzzer: swap in the other document's text
// (turning A into B is what makes cross-document symbol state go stale), or
// truncate, or duplicate. Truncation matters most for positions -- a request
// aimed at line 40 of a document that just shrank to 3 lines is the classic
// editor race, and it is what the server has to survive.
func (s *session) editedContent(a opArgs) string {
	cur := s.content[a.uri]
	switch a.b % 3 {
	case 0:
		if a.uri == fuzzURIA {
			return s.content[fuzzURIB]
		}
		return s.content[fuzzURIA]
	case 1:
		if cur == "" {
			return ""
		}
		return cur[:int(a.a)%len(cur)]
	default:
		return cur + "\n" + cur
	}
}

func (s *session) opDidChange(a opArgs) {
	text := s.editedContent(a)
	// Keep the harness's own view in step, so later ops derive their positions
	// from the text the server actually holds.
	s.content[a.uri] = text
	_ = s.srv.textDocumentDidChange(s.ctx, &protocol.DidChangeTextDocumentParams{
		TextDocument: protocol.VersionedTextDocumentIdentifier{
			TextDocumentIdentifier: a.ident(),
			Version:                protocol.Integer(a.a) + 2,
		},
		ContentChanges: []any{protocol.TextDocumentContentChangeEventWhole{Text: text}},
	})
	s.runDebounced(a.uri)
}

// runDebounced cancels the timer didChange just armed and performs its body
// inline. See the recover table in the header: left alone, that closure runs
// the whole analyse+lint pass 300ms later on a timer goroutine underneath a
// blanket recover, so any panic in it is both unattributable and invisible.
func (s *session) runDebounced(uri string) {
	s.srv.debounceMu.Lock()
	if t, ok := s.srv.debounce[uri]; ok {
		t.Stop()
		delete(s.srv.debounce, uri)
	}
	s.srv.debounceMu.Unlock()

	if d := s.srv.docs.Get(uri); d != nil {
		s.srv.analyzeAndPublish(d)
	}
}

func (s *session) opDidSave(a opArgs) {
	_ = s.srv.textDocumentDidSave(s.ctx, &protocol.DidSaveTextDocumentParams{
		TextDocument: a.ident(),
	})
}

func (s *session) opDidClose(a opArgs) {
	_ = s.srv.textDocumentDidClose(s.ctx, &protocol.DidCloseTextDocumentParams{
		TextDocument: a.ident(),
	})
}

func (s *session) opHover(a opArgs) {
	res, err := s.srv.textDocumentHover(s.ctx, &protocol.HoverParams{TextDocumentPositionParams: a.tdpp()})
	s.record("hover", res, err)
}

func (s *session) opDefinition(a opArgs) {
	res, err := s.srv.textDocumentDefinition(s.ctx, &protocol.DefinitionParams{TextDocumentPositionParams: a.tdpp()})
	s.record("definition", res, err)
}

func (s *session) opCompletion(a opArgs) {
	var cctx *protocol.CompletionContext
	if a.flags&(1<<3) != 0 {
		trigger := string([]byte{"(:"[int(a.a)%2]})
		cctx = &protocol.CompletionContext{
			TriggerKind:      protocol.CompletionTriggerKindTriggerCharacter,
			TriggerCharacter: &trigger,
		}
	}
	res, err := s.srv.textDocumentCompletion(s.ctx, &protocol.CompletionParams{
		TextDocumentPositionParams: a.tdpp(),
		Context:                    cctx,
	})
	s.record("completion", res, err)
}

func (s *session) opReferences(a opArgs) {
	res, err := s.srv.textDocumentReferences(s.ctx, &protocol.ReferenceParams{
		TextDocumentPositionParams: a.tdpp(),
		Context:                    protocol.ReferenceContext{IncludeDeclaration: a.flags&(1<<4) != 0},
	})
	s.record("references", res, err)
}

func (s *session) opHighlight(a opArgs) {
	res, err := s.srv.textDocumentDocumentHighlight(s.ctx, &protocol.DocumentHighlightParams{
		TextDocumentPositionParams: a.tdpp(),
	})
	s.record("documentHighlight", res, err)
}

func (s *session) opDocumentSymbol(a opArgs) {
	res, err := s.srv.textDocumentDocumentSymbol(s.ctx, &protocol.DocumentSymbolParams{TextDocument: a.ident()})
	s.record("documentSymbol", res, err)
}

func (s *session) opRename(a opArgs) {
	res, err := s.srv.textDocumentRename(s.ctx, &protocol.RenameParams{
		TextDocumentPositionParams: a.tdpp(),
		NewName:                    word([]byte(s.content[a.uri]), a.a, a.b),
	})
	s.record("rename", res, err)
	if err == nil && res != nil {
		s.checkRenameSpans(a, res)
	}
}

// checkRenameSpans is the CONTENT assertion for textDocument/rename, and the
// second exception to "NOT asserted: that any request returns a particular
// result" -- checkTokenSpans above being the first, and the note there explains
// why an exception is admissible at all.
//
// It does not say which symbol should be renamed, how many edits there should
// be, or which files they should touch. It says that whatever the server chose
// to replace, the range it chose has to COVER THE WHOLE IDENTIFIER. A rename is
// the one request in the protocol whose answer is applied to the user's file
// unread, so a range that is off by even one byte is not a cosmetic error the
// way a wrong hover range is: it is a silent edit of a program into a different
// program.
//
// elps#463. token.TokenEnd derived EndCol by counting RUNES onto Scanner's byte
// Col, and elpsToLSPRange prefers EndCol whenever it is set, so on an
// identifier containing a multi-byte rune every rename edit was short by
// len-runeCount bytes. Renaming "éx" to "zz" replaced "é" and left "x", giving
// "zzx" -- a valid program that is not the user's. Where the identifier's LAST
// rune was multi-byte the leftover was a bare continuation byte and the file
// stopped being valid UTF-8 at all.
//
// THE ORACLE is prepareRename, which the server answers from the same symbol
// lookup rename uses and which returns the symbol's name as its Placeholder.
// Asking it at the same position turns "is this range right?" into a comparison
// against text the server itself named, with no second implementation of symbol
// resolution in the test to disagree about the answer. Where prepareRename
// declines -- a builtin, no symbol, an external with no source -- there is no
// oracle and nothing is checked; that costs coverage only on inputs where
// rename should have returned nothing anyway.
//
// Only edits against documents the SERVER holds are checked. A cross-file edit
// may name a workspace file the harness never opened, and its content is not
// available to compare against.
//
// UNITS: byte offsets, as everywhere else in this package (position.go splits
// on "\n" and slices by byte). elps#464 is that LSP 3.16 actually specifies
// UTF-16 code units and this server neither negotiates positionEncoding nor
// converts; this assertion is deliberately the weaker, server-internal
// property, which is a real property regardless of how #464 is answered -- a
// range whose two ends are counted differently is broken before any encoding
// question arises.
func (s *session) checkRenameSpans(a opArgs, res *protocol.WorkspaceEdit) {
	if s.renameErr != nil || res.Changes == nil {
		return
	}
	pr, err := s.srv.textDocumentPrepareRename(s.ctx, &protocol.PrepareRenameParams{
		TextDocumentPositionParams: a.tdpp(),
	})
	if err != nil || pr == nil {
		return
	}
	rwp, ok := pr.(*protocol.RangeWithPlaceholder)
	if !ok || rwp.Placeholder == "" {
		return
	}
	name := rwp.Placeholder

	for uri, edits := range res.Changes {
		doc := s.srv.docs.Get(uri)
		if doc == nil {
			continue // a workspace file the harness never opened
		}
		doc.mu.Lock()
		content := doc.Content
		doc.mu.Unlock()
		lines := strings.Split(content, "\n")

		for _, e := range edits {
			line, start, end := int(e.Range.Start.Line), int(e.Range.Start.Character), int(e.Range.End.Character)
			if int(e.Range.End.Line) != line {
				s.renameErr = fmt.Errorf("rename edit [%d:%d..%d:%d) on %s spans two lines; an identifier does not",
					e.Range.Start.Line, start, e.Range.End.Line, end, uri)
				return
			}
			if line >= len(lines) || start < 0 || end < start || end > len(lines[line]) {
				s.renameErr = fmt.Errorf("rename edit [%d:%d..%d) on %s is not inside the document (%d lines)",
					line, start, end, uri, len(lines))
				return
			}
			if covered := lines[line][start:end]; covered != name {
				s.renameErr = fmt.Errorf("rename edit [%d:%d..%d) on %s covers %q, but the identifier being renamed is %q:"+
					" applying this edit replaces part of a name and leaves the rest (#463)",
					line, start, end, uri, covered, name)
				return
			}
		}
	}
}

func (s *session) opPrepareRename(a opArgs) {
	res, err := s.srv.textDocumentPrepareRename(s.ctx, &protocol.PrepareRenameParams{TextDocumentPositionParams: a.tdpp()})
	s.record("prepareRename", res, err)
}

func (s *session) opFormatting(a opArgs) {
	res, err := s.srv.textDocumentFormatting(s.ctx, &protocol.DocumentFormattingParams{
		TextDocument: a.ident(),
		Options: protocol.FormattingOptions{
			"tabSize":      protocol.Integer(a.a%8) + 1,
			"insertSpaces": a.b%2 == 0,
		},
	})
	s.record("formatting", res, err)
}

func (s *session) opSignatureHelp(a opArgs) {
	res, err := s.srv.textDocumentSignatureHelp(s.ctx, &protocol.SignatureHelpParams{
		TextDocumentPositionParams: a.tdpp(),
	})
	s.record("signatureHelp", res, err)
}

// opCodeAction feeds back the diagnostics the server itself published, which
// is what a real client sends and the only way the publishedDiagnostics
// lookup (the glsp IntegerOrString round-trip workaround) is exercised.
func (s *session) opCodeAction(a opArgs) {
	var diags []protocol.Diagnostic
	if d := s.srv.docs.Get(a.uri); d != nil {
		d.mu.Lock()
		diags = append(diags, d.publishedDiagnostics...)
		d.mu.Unlock()
	}
	end := protocol.Position{Line: safeUint(a.line + int(a.b)%3), Character: safeUint(a.col + int(a.a)%5)}
	res, err := s.srv.textDocumentCodeAction(s.ctx, &protocol.CodeActionParams{
		TextDocument: a.ident(),
		Range:        protocol.Range{Start: a.pos(), End: end},
		Context:      protocol.CodeActionContext{Diagnostics: diags},
	})
	s.record("codeAction", res, err)
}

func (s *session) opFoldingRange(a opArgs) {
	res, err := s.srv.textDocumentFoldingRange(s.ctx, &protocol.FoldingRangeParams{TextDocument: a.ident()})
	s.record("foldingRange", res, err)
}

func (s *session) opSemanticTokens(a opArgs) {
	res, err := s.srv.textDocumentSemanticTokensFull(s.ctx, &protocol.SemanticTokensParams{TextDocument: a.ident()})
	s.record("semanticTokens", res, err)
	if err == nil && res != nil {
		s.checkTokenSpans(a.uri, res.Data)
	}
}

// checkTokenSpans is the one CONTENT assertion this target makes, and the
// exception to "NOT asserted: that any request returns a particular result".
//
// It is admissible here because it does not say what the answer should be. It
// says the answer has to be about THIS DOCUMENT: LSP 3.16 defines a semantic
// token as (deltaLine, deltaStartChar, length) over the document text, so a
// token whose range runs past the end of its line names no text at all, and no
// document and no cursor position can make that the right response. It is
// checkable from the response alone -- decode the deltas back to absolute
// triples and compare against the content the harness sent -- which is why it
// belongs on the fuzzer's arbitrary documents rather than only on the handful
// of shapes a table test can name.
//
// The bound is a BYTE count, matching how every other line index in this
// package is computed (position.go splits content on "\n" and slices by byte).
// A client counting UTF-16 code units would be stricter still; this checks the
// weaker property the server itself claims to satisfy.
//
// elps#428: the reader's synthesized #^ and #' heads were reported with the
// length of their DESUGARED NAME ("lisp:expr", "lisp:function") over a
// two-column span, so "#'foo" told the editor to decorate 13 characters of a
// 5-character line. This assertion was left out of the target when that defect
// was filed, because with it in place the target was red on its own seeds.
//
// COVERAGE, ADDED FOR elps#449.  Bounds alone was too weak to be the whole
// property.  It asks that a token name SOME text in the document; it does not
// ask that it name the RIGHT text, and both defects in elps#449 were strictly
// in bounds -- a quoted symbol's token slid one character left onto the ' and
// stopped one character short of the symbol, and a string's length was measured
// on the DECODED value so `"x\ty"` got five characters of a six-character
// literal.  Every such token satisfies the bound and misinforms the editor.
//
// coverageDefect (semantic_tokens_span_test.go) is the stronger question, asked
// of the same decoded triples: does the text a token claims look like the atom
// it is classifying?  It is still checkable from the response alone and still
// says nothing about which answer is right -- a token may be a variable or a
// function, but it may not start on reader punctuation, span two atoms, or stop
// in the middle of a string literal.  One definition of the property, shared
// with the table tests next door, so the fuzzer and they cannot drift.
func (s *session) checkTokenSpans(uri string, data []protocol.UInteger) {
	if s.tokenErr != nil {
		return
	}
	// The SERVER's copy, not the harness's mirror of it: the tokens were
	// computed from that, so anything else would be checking the wrong text.
	doc := s.srv.docs.Get(uri)
	if doc == nil {
		return
	}
	doc.mu.Lock()
	content := doc.Content
	doc.mu.Unlock()

	lines := strings.Split(content, "\n")
	prevLine, prevChar := 0, 0
	for i := 0; i+4 < len(data); i += 5 {
		line := prevLine + int(data[i])
		char := int(data[i+1])
		if data[i] == 0 {
			char += prevChar
		}
		length := int(data[i+2])
		prevLine, prevChar = line, char

		if line >= len(lines) {
			s.tokenErr = fmt.Errorf("semantic token [%d:%d,+%d) is on line %d of a %d-line document (%s)",
				line, char, length, line, len(lines), uri)
			return
		}
		// A CRLF document keeps its "\r" in the line; the editor does too, so
		// it is part of the line for this purpose.
		if char < 0 || length < 0 || char+length > len(lines[line]) {
			s.tokenErr = fmt.Errorf("semantic token [%d:%d,+%d) overruns line %d, which is %d bytes: %q (%s)",
				line, char, length, line, len(lines[line]), lines[line], uri)
			return
		}
		cov := tokenCoverage{
			tok: rawToken{
				line: line, startChar: char, length: length,
				tokenType: int(data[i+3]),
			},
			covered: lines[line][char : char+length],
		}
		if msg := coverageDefect(cov, lines[line]); msg != "" {
			s.tokenErr = fmt.Errorf("semantic token [%d:%d,+%d) covers %q: %s (%s)",
				line, char, length, cov.covered, msg, uri)
			return
		}
	}
}

func (s *session) opSelectionRange(a opArgs) {
	res, err := s.srv.textDocumentSelectionRange(s.ctx, &protocol.SelectionRangeParams{
		TextDocument: a.ident(),
		Positions: []protocol.Position{
			a.pos(),
			{Line: safeUint(a.col), Character: safeUint(a.line)},
		},
	})
	s.record("selectionRange", res, err)
}

func (s *session) opLinkedEditing(a opArgs) {
	res, err := s.srv.textDocumentLinkedEditingRange(s.ctx, &protocol.LinkedEditingRangeParams{
		TextDocumentPositionParams: a.tdpp(),
	})
	s.record("linkedEditingRange", res, err)
}

func (s *session) opPrepareCallHierarchy(a opArgs) {
	res, err := s.srv.textDocumentPrepareCallHierarchy(s.ctx, &protocol.CallHierarchyPrepareParams{
		TextDocumentPositionParams: a.tdpp(),
	})
	s.record("prepareCallHierarchy", res, err)
	if len(res) > 0 {
		s.lastItems = res
	}
}

// callItem returns an item for the call-hierarchy follow-ups: the real one
// from a previous prepare when there is one, else a synthetic item, because a
// client that never got a prepare result can still send this request.
func (s *session) callItem(a opArgs) protocol.CallHierarchyItem {
	if len(s.lastItems) > 0 {
		return s.lastItems[int(a.a)%len(s.lastItems)]
	}
	rng := protocol.Range{Start: a.pos(), End: a.pos()}
	return protocol.CallHierarchyItem{
		Name:           word([]byte(s.content[a.uri]), a.a, a.b),
		Kind:           protocol.SymbolKindFunction,
		URI:            a.uri,
		Range:          rng,
		SelectionRange: rng,
	}
}

func (s *session) opIncomingCalls(a opArgs) {
	res, err := s.srv.callHierarchyIncomingCalls(s.ctx, &protocol.CallHierarchyIncomingCallsParams{Item: s.callItem(a)})
	s.record("incomingCalls", res, err)
}

func (s *session) opOutgoingCalls(a opArgs) {
	res, err := s.srv.callHierarchyOutgoingCalls(s.ctx, &protocol.CallHierarchyOutgoingCallsParams{Item: s.callItem(a)})
	s.record("outgoingCalls", res, err)
}

func (s *session) opWatchedFiles(a opArgs) {
	_ = s.srv.workspaceDidChangeWatchedFiles(s.ctx, &protocol.DidChangeWatchedFilesParams{
		Changes: []protocol.FileEvent{
			{URI: a.uri, Type: protocol.UInteger(a.a%3) + 1},
			// A non-.lisp URI exercises the suffix filter's skip branch.
			{URI: a.uri + ".txt", Type: protocol.UInteger(a.b%3) + 1},
		},
	})
	s.runWatchedReanalyze()
}

// runWatchedReanalyze does for debouncedReanalyze what runDebounced does for
// didChange's timer. This one matters more: its closure has NO recover, so a
// panic in reanalyzeOpenDocuments would kill the process from a timer
// goroutine with no input attached to it.
func (s *session) runWatchedReanalyze() {
	s.srv.debounceMu.Lock()
	if t, ok := s.srv.debounce["__watched_files__"]; ok {
		t.Stop()
		delete(s.srv.debounce, "__watched_files__")
	}
	s.srv.debounceMu.Unlock()
	s.srv.reanalyzeOpenDocuments()
}

func (s *session) opWorkspaceSymbol(a opArgs) {
	res, err := s.srv.workspaceSymbol(s.ctx, &protocol.WorkspaceSymbolParams{
		Query: word([]byte(s.content[a.uri]), a.a, a.b),
	})
	s.record("workspaceSymbol", res, err)
}

func (s *session) opInlayHint(a opArgs) {
	end := protocol.Position{Line: safeUint(a.line + int(a.b)), Character: safeUint(a.col)}
	res, err := s.srv.textDocumentInlayHint(&InlayHintParams{
		TextDocument: a.ident(),
		Range:        protocol.Range{Start: a.pos(), End: end},
	})
	s.record("inlayHint", res, err)
}

func (s *session) opSetTrace(a opArgs) {
	_ = s.srv.setTrace(s.ctx, &protocol.SetTraceParams{Value: protocol.TraceValue(word([]byte("offmessagesverbose"), a.a, a.b))})
}

func (s *session) opInitialized(_ opArgs) {
	// ctx.Call is nil, so no client-callback goroutine, and the workspace
	// index sync.Once has already fired, so `go s.ensureWorkspaceIndex()`
	// returns immediately.
	_ = s.srv.initialized(s.ctx, &protocol.InitializedParams{})
}

func (s *session) opShutdown(_ opArgs) {
	_ = s.srv.shutdown(s.ctx)
}

func (s *session) opExit(_ opArgs) {
	// exitFn is stubbed in newSession; without that this would take the test
	// process down.
	_ = s.srv.exit(s.ctx)
}

// record checks that whatever the handler produced can actually be sent. A
// response glsp cannot marshal breaks the client connection, and walking the
// whole result is also how a corrupt nested value gets touched -- the same job
// res.String() does in FuzzDebugEval.
func (s *session) record(what string, res any, err error) {
	if err != nil {
		return
	}
	s.checkMarshal("response "+what, res)
}

// ---------------------------------------------------------------------------
// driver
// ---------------------------------------------------------------------------

// runSession executes a whole script. Any panic in a handler escapes from here
// straight into the fuzzing engine: nothing on this path recovers.
func runSession(srcA, srcB, script []byte) error {
	s, err := newSession(workspacePreamble(srcB))
	if err != nil {
		return err
	}
	s.content[fuzzURIA] = string(srcA)
	s.content[fuzzURIB] = string(srcB)

	n := len(script) / bytesPerOp
	if n > maxOps {
		n = maxOps
	}
	for i := range n {
		op, args := s.decode(script[i*bytesPerOp : (i+1)*bytesPerOp])
		s.executed[op.name] = true
		op.run(s, args)
		if s.marshalErr != nil {
			return fmt.Errorf("%s did not marshal: %w", s.marshalCtx, s.marshalErr)
		}
		if s.tokenErr != nil {
			return s.tokenErr
		}
		if s.renameErr != nil {
			return s.renameErr
		}
	}
	// Any timer still armed would fire after the session is over, on a
	// goroutine belonging to no input.
	_ = s.srv.shutdown(s.ctx)
	if err := s.checkIndexBuilt(); err != nil {
		return err
	}
	return s.checkExpansionPanics()
}

// fatalf is the subset of *testing.T the harness needs, so the same driver
// serves the fuzz target and the ordinary corpus tests.
type fatalf interface {
	Helper()
	Fatalf(format string, args ...interface{})
	Skipf(format string, args ...interface{})
}

// runSessionBudgeted runs a session on its own goroutine under the watchdog.
//
// A hang is a real possibility rather than a formality: analysis-time macro
// expansion evaluates macro bodies, and lint walks an AST the fuzzer shaped.
// The interpreter budgets bound the first; nothing but this bounds the second.
func runSessionBudgeted(t fatalf, srcA, srcB, script []byte) {
	t.Helper()

	done := make(chan error, 1)
	go func() { done <- runSession(srcA, srcB, script) }()

	budget := fuzzwatch.New(lspWatchdogTimeout)
	wait := budget.Total()
	for {
		select {
		case err := <-done:
			if err != nil {
				t.Fatalf("%v\n--- doc A (%d bytes) ---\n%q\n--- doc B (%d bytes) ---\n%q"+
					"\n--- script (%d bytes) ---\n%q",
					err, len(srcA), srcA, len(srcB), srcB, len(script), script)
			}
			return
		case <-time.After(wait):
			verdict, more, report := budget.Check()
			switch verdict {
			case fuzzwatch.Continue:
				wait = more
			case fuzzwatch.Inconclusive:
				t.Skipf("no verdict: the process was starved throughout (%s)", report)
				return
			default:
				t.Fatalf("the LSP session did not terminate within %s of SCHEDULED time (%s)."+
					" Analysis-time macro expansion runs under interpreter budgets, but nothing"+
					" bounds a loop in analysis or lint themselves"+
					"\n--- doc A (%d bytes) ---\n%q\n--- doc B (%d bytes) ---\n%q"+
					"\n--- script (%d bytes) ---\n%q",
					budget.Total(), report, len(srcA), srcA, len(srcB), srcB, len(script), script)
				return
			}
		}
	}
}

// ---------------------------------------------------------------------------
// the target
// ---------------------------------------------------------------------------

// allOpsScript is a script that runs every op once, in table order, against
// document A with in-range positions. Used as a seed and by the coverage
// guard.
func allOpsScript() []byte {
	script := make([]byte, 0, len(ops)*bytesPerOp)
	for i := range ops {
		script = append(script, byte(i), 1, 1, 0)
	}
	return script
}

// opcode looks an operation up by name so seed scripts do not hard-code table
// indices. They did at first, and reordering the table to fix the didClose
// problem above silently repointed every one of them at a different handler --
// the seeds still ran, still passed, and no longer seeded what their comments
// claimed.
func opcode(name string) byte {
	for i, op := range ops {
		if op.name == name {
			return byte(i)
		}
	}
	panic("no such fuzz op: " + name)
}

// script assembles [op, a, b, flags] quads from named operations.
func script(steps ...[4]any) []byte {
	out := make([]byte, 0, len(steps)*bytesPerOp)
	for _, st := range steps {
		out = append(out,
			opcode(st[0].(string)),
			byte(st[1].(int)),
			byte(st[2].(int)),
			st[3].(byte),
		)
	}
	return out
}

// FuzzLSPSession drives a fuzzer-chosen sequence of LSP requests against
// fuzzer-chosen documents and cursor positions.
//
// Asserted, and only this:
//
//  1. No Go panic escapes a request handler. Nothing recovers between the
//     handler body and the fuzz function, so this is an ordinary crash the
//     engine attributes to its input -- no marker value needed. The two timer
//     closures the server would otherwise run asynchronously are run inline so
//     they are covered by the same statement.
//  2. buildWorkspaceIndex did not swallow a panic (checkIndexBuilt).
//  3. The session terminates (the watchdog).
//  4. Every response and every notification marshals to JSON, because
//     everything here is on its way to a client over a JSON-RPC connection.
//
// NOT asserted: that any request succeeds, returns a particular result, or
// even finds the document. A request against a closed document, a position
// past end-of-file and a rename to a syntactically invalid symbol are all
// things a real client sends, and returning nothing is the right answer.
func FuzzLSPSession(f *testing.F) {
	add := func(a, b string, sc []byte) { f.Add([]byte(a), []byte(b), sc) }

	// A document has to be opened before most requests do anything, so every
	// hand-written seed starts by opening both.
	openBoth := []([4]any){
		{"didOpen", 1, 1, byte(0)},
		{"didOpen", 1, 1, flagDocB},
	}
	withOpen := func(rest ...[4]any) []byte {
		return script(append(append([]([4]any){}, openBoth...), rest...)...)
	}

	// Real lisp, so requests resolve to real symbols and the analysis and lint
	// paths run on well-formed input rather than only on noise.
	const defs = `(in-package 'demo)
(use-package 'lisp)
(defun add (a b) "add two" (+ a b))
(defmacro twice (x) (quasiquote (+ (unquote x) (unquote x))))
(defun caller (n) (add n (twice n)))
(export 'add)`
	const uses = `(in-package 'user)
(use-package 'demo)
(defun main () (add 1 2))`

	for _, sc := range [][]byte{
		allOpsScript(),
		// Cursor on a symbol: the three requests an editor fires constantly.
		withOpen([4]any{"hover", 3, 8, byte(0)}, [4]any{"definition", 3, 8, byte(0)},
			[4]any{"completion", 3, 8, byte(0)}),
		withOpen([4]any{"rename", 3, 8, byte(0)}, [4]any{"prepareRename", 3, 8, byte(0)}),
		// Call hierarchy: prepare, then follow it up both ways with the item
		// prepare returned.
		withOpen([4]any{"prepareCallHierarchy", 3, 8, byte(0)},
			[4]any{"incomingCalls", 0, 0, byte(0)}, [4]any{"outgoingCalls", 0, 0, byte(0)}),
		withOpen([4]any{"didChange", 2, 2, byte(0)}, [4]any{"didChange", 2, 2, flagDocB}),
		withOpen([4]any{"watchedFiles", 0, 0, byte(0)}, [4]any{"workspaceSymbol", 1, 4, byte(0)}),
		withOpen([4]any{"hover", 200, 200, flagRawPos}), // way past end of file
		withOpen([4]any{"hover", 7, 7, flagWildPos}),    // absurd position
		// Every position request against a document that was just closed.
		withOpen([4]any{"didClose", 0, 0, byte(0)}, [4]any{"hover", 1, 1, byte(0)},
			[4]any{"semanticTokens", 1, 1, byte(0)}, [4]any{"foldingRange", 1, 1, byte(0)}),
	} {
		add(defs, uses, sc)
	}

	// Text shapes worth starting from. The parser corpus already covers what
	// breaks a parser; these are chosen for what breaks POSITION arithmetic:
	// no trailing newline, CRLF, a lone CR, tabs, and multi-byte runes -- the
	// server indexes lines by BYTE, so a rune boundary is where a byte-index
	// slice goes wrong.
	add("", "", allOpsScript())
	add("\n\n\n", "", allOpsScript())
	add("(defun f (x) x)", "", allOpsScript())
	add("(defun f (x) x)\r\n(f 1)\r\n", "", allOpsScript())
	add("(defun f (x) x)\r(f 1)", "", allOpsScript())
	add("(defun éèê (x) x)\n(éèê 1)", "", allOpsScript())

	// More NON-ASCII IDENTIFIERS, for checkRenameSpans (elps#463).  The one
	// above already reaches the assertion, but it is a single shape: three
	// two-byte runes.  These vary what the defect's arithmetic keys on -- how
	// many bytes are lost is len(name)-runeCount(name), and whether the result
	// is merely a WRONG program or is not valid UTF-8 at all depends on whether
	// the identifier's LAST rune is multi-byte.
	//
	// Seeded deliberately rather than left to mutation, for the reason PR #462
	// had to hand-seed its escaped-string case: an assertion no seed reaches is
	// not an assertion, and random bytes rarely land on well-formed multi-byte
	// UTF-8 inside an identifier the analyser will resolve.
	add("(defun éx (a) a)\n(éx 1)", "", allOpsScript())                 // leading multi-byte
	add("(defun xé (a) a)\n(xé 1)", "", allOpsScript())                 // TRAILING: leftover is a bare continuation byte
	add("(defun 加算 (a b) (+ a b))\n(加算 1 2)", "", allOpsScript())       // three-byte runes
	add("(defun 𝛼𝛽 (a) a)\n(𝛼𝛽 1)", "", allOpsScript())                 // four-byte runes, outside the BMP
	add("(set λ 1)\nλ", "", allOpsScript())                             // a variable, not a function
	add("(defun f (é) (g é))\n(defun g (x) x)", "", allOpsScript())     // ASCII name on a non-ASCII line
	add("(in-package 'é)\n(defun é:f () 1)\n(é:f)", "", allOpsScript()) // non-ASCII package qualifier
	// Cross-file: the definition is in A and the reference in B, so the rename
	// has to produce edits for a document the cursor is not in.
	add("(in-package 'demo)\n(defun éx (a) a)\n(export 'éx)",
		"(in-package 'user)\n(use-package 'demo)\n(éx 1)", allOpsScript())
	add("\t\t(defun f (x)\n\t\tx)", "", allOpsScript())
	add("(defun f (x", "", allOpsScript())
	add(";; only a comment", "", allOpsScript())
	add(defs, uses, allOpsScript())

	// The reader's PREFIX SHORTHANDS, which checkTokenBounds exists for
	// (elps#428): #^e and #'f desugar to (lisp:expr e) and (lisp:function f),
	// and the head symbol the reader synthesizes carries a 9- or 13-byte name
	// over 2 bytes of source.
	//
	// Seeded here rather than relied on from fuzzseed, which does not reach the
	// property despite containing the syntax. Of its five prefix seeds three
	// fail to parse ("#^", "#' ", and fifty nested "#^"), and the other two --
	// "#^(a (b))" and "#^(+ % 1)" -- are each exactly len("lisp:expr") = 9
	// characters, so the over-long token ends exactly at the end of the line
	// and the bound holds by arithmetic accident. An assertion no seed reaches
	// is not an assertion.
	add("#'foo", "", allOpsScript())
	add("(defun g () #'g)", "", allOpsScript())
	add("(f #^a)", "", allOpsScript())
	add("'#^0", "", allOpsScript())
	add("(list #'car\n      #'cdr)", "", allOpsScript())
	add("(lisp:function foo)", "", allOpsScript()) // longhand: not synthesized

	// The shapes the COVERAGE half of checkTokenSpans exists for (elps#449).
	//
	// The quoted-atom half is already reached without these: `uses` above is
	// "(in-package 'user)\n(use-package 'demo)\n...", and on 5ef6106 each of
	// those quoted symbols covers "'use" and "'dem" respectively. The ESCAPED
	// STRING half is reached by nothing in the corpus -- no seed here or in
	// fuzzseed contains a string literal with an escape in it, and a literal
	// without one has len(v.Str)+2 exactly right -- so the property would have
	// held by the corpus never asking. Seeded rather than left to the mutator,
	// which has to produce a balanced pair of quotes AND a backslash escape
	// inside it before the length is ever wrong.
	add(`(f "x\ty")`, "", allOpsScript())
	add(`(f "a\nb" c)`, "", allOpsScript()) // escaped \n, not a newline
	add(`(f """raw""" c)`, "", allOpsScript())
	add("(f \"\"\"a\nb\"\"\" c)", "", allOpsScript()) // genuinely multi-line
	add("(list 'a 'b)", "", allOpsScript())           // one-character quoted symbols
	add("' a", "", allOpsScript())                    // whitespace after the prefix
	add("'\na", "", allOpsScript())                   // and a newline after it

	// Document B as the WORKSPACE FILE that defines a macro and document A as
	// the file that calls it. This is the only shape that reaches
	// env.MacroCall -- evaluation of a macro body during mere analysis -- so
	// without these three the deepest path this target claims is seeded by
	// nothing. See workspacePreamble.
	const wsMacro = "(in-package 'demo)\n(defmacro twice (x) (quasiquote (+ (unquote x) (unquote x))))"
	add("(in-package 'demo)\n(twice (twice 3))", wsMacro, allOpsScript())
	// A macro that expands to a call to itself: bounded by the analyser's own
	// maxMacroExpansionDepth rather than by anything the harness sets.
	add("(in-package 'demo)\n(loopy 1)", "(in-package 'demo)\n(defmacro loopy (x) (quasiquote (loopy (unquote x))))", allOpsScript())
	// A macro whose body errors when expanded.
	add("(in-package 'demo)\n(boom 1)", "(in-package 'demo)\n(defmacro boom (x) (error 'macro-boom \"x\"))", allOpsScript())

	// Parser-adversarial text, reused: those inputs are small by construction
	// (see fuzzseed.Adversarial's own note on seed size) and a fault-tolerant
	// parse of them yields the partial and malformed ASTs that analysis, lint
	// and every position lookup then have to survive. LispSources() is
	// deliberately NOT used -- for the same reason evalseed.go avoids it, a
	// large seed throttles every generation descended from it.
	for _, seed := range fuzzseed.Adversarial() {
		add(string(seed), "", allOpsScript())
	}

	f.Fuzz(func(t *testing.T, srcA, srcB, sc []byte) {
		runSessionBudgeted(t, srcA, srcB, sc)
	})
}

// ---------------------------------------------------------------------------
// guards
// ---------------------------------------------------------------------------

// TestLSPFuzzDrivesEveryWiredHandler is the guard that keeps this target
// honest, and the analogue of TestDebuggerChangesTheEvalPath.
//
// FuzzLSPSession is only worth its budget if it actually reaches the request
// surface. The way it would silently stop doing so is not a change in lsp/ at
// all -- it is a NEW HANDLER being wired into protocol.Handler with no opcode
// added here. Nothing about that is visible: the target still passes, still
// burns ten minutes a night, and quietly covers one method less.
//
// So the wired set is read back off the server by reflection rather than
// listed, and every non-nil handler must be driven by an op or carry a written
// reason in setupHandlers.
func TestLSPFuzzDrivesEveryWiredHandler(t *testing.T) {
	t.Parallel()

	covered := map[string]bool{}
	for _, op := range ops {
		if op.handler != "" {
			covered[op.handler] = true
		}
	}
	for name := range setupHandlers {
		covered[name] = true
	}

	srv := New()
	// Addressed via pointer: protocol.Handler embeds a sync.Mutex, and
	// reflect.ValueOf on the value would copy it (go vet copylocks).
	h := reflect.ValueOf(&srv.handler).Elem()
	var missing []string
	var wired int
	for i := range h.NumField() {
		// protocol.Handler carries bookkeeping fields (a mutex, a bool)
		// alongside the handler funcs; only the funcs are handlers.
		if h.Field(i).Kind() != reflect.Func || h.Field(i).IsNil() {
			continue
		}
		wired++
		name := h.Type().Field(i).Name
		if !covered[name] {
			missing = append(missing, name)
		}
	}

	if wired == 0 {
		t.Fatal("no handlers are wired into protocol.Handler; the reflection above" +
			" is reading the wrong value and this guard is asserting nothing")
	}
	if len(missing) > 0 {
		sort.Strings(missing)
		t.Fatalf("these LSP handlers are wired into the server but no FuzzLSPSession op drives them: %v."+
			"\nAdd an op to the `ops` table (or, if it genuinely cannot be fuzzed, an entry in"+
			" setupHandlers saying why). Without one, FuzzLSPSession silently covers less than"+
			" it appears to.", missing)
	}

	// The wrapper dispatches methods that are NOT protocol.Handler fields, so
	// reflection cannot see them. Read them out of the source text instead --
	// cheap, and it catches a new case arm at CI time.
	src, err := os.ReadFile("handler_wrapper.go")
	if err != nil {
		t.Fatalf("cannot read handler_wrapper.go: %v", err)
	}
	methods := regexp.MustCompile(`case "([^"]+)":`).FindAllStringSubmatch(string(src), -1)
	if len(methods) == 0 {
		t.Fatal("no `case \"...\":` arms found in handler_wrapper.go; this half of the" +
			" guard has stopped reading what it thinks it is reading")
	}
	byMethod := map[string]bool{}
	for _, op := range ops {
		if op.method != "" {
			byMethod[op.method] = true
		}
	}
	for _, m := range methods {
		if !byMethod[m[1]] {
			t.Errorf("handlerWrapper dispatches %q but no FuzzLSPSession op drives it", m[1])
		}
	}
}

// TestLSPFuzzScriptReachesEveryOp is the complement: the ops table can be
// complete and still unreachable, if the decoder cannot emit some opcode or an
// op's body returns before doing anything. This runs allOpsScript and checks
// that every entry actually executed.
func TestLSPFuzzScriptReachesEveryOp(t *testing.T) {
	t.Parallel()

	if maxOps < len(ops) {
		t.Fatalf("maxOps is %d but the table has %d operations: runSession would truncate"+
			" allOpsScript and stop seeding the tail of the table", maxOps, len(ops))
	}

	s, err := newSession(workspacePreamble([]byte("(defmacro ws (x) (quasiquote (+ (unquote x) 1)))")))
	if err != nil {
		t.Fatalf("newSession: %v", err)
	}
	s.content[fuzzURIA] = "(defun f (x) x)\n(f 1)\n"
	s.content[fuzzURIB] = "(defun g (y) y)\n"

	script := allOpsScript()
	for i := 0; i+bytesPerOp <= len(script); i += bytesPerOp {
		op, args := s.decode(script[i : i+bytesPerOp])
		s.executed[op.name] = true
		op.run(s, args)
	}

	var missed []string
	for _, op := range ops {
		if !s.executed[op.name] {
			missed = append(missed, op.name)
		}
	}
	if len(missed) > 0 {
		t.Fatalf("allOpsScript did not reach these ops: %v -- the decoder cannot emit"+
			" every opcode, so part of the table is dead weight", missed)
	}
	if s.marshalErr != nil {
		t.Fatalf("%s did not marshal: %v", s.marshalCtx, s.marshalErr)
	}
	if s.tokenErr != nil {
		t.Fatalf("%v", s.tokenErr)
	}
	if s.renameErr != nil {
		t.Fatalf("%v", s.renameErr)
	}
}

// TestLSPFuzzSeedsTerminate is the false-positive guard: every seeded session
// must reach a verdict under the harness. A seed that hangs would not fail
// loudly -- it would make every generation descended from it useless.
func TestLSPFuzzSeedsTerminate(t *testing.T) {
	t.Parallel()

	seeds := map[string][]byte{
		"empty":       []byte(""),
		"comment":     []byte(";; only a comment"),
		"unclosed":    []byte("(defun f (x"),
		"crlf":        []byte("(defun f (x) x)\r\n(f 1)\r\n"),
		"multibyte":   []byte("(defun éèê (x) x)\n(éèê 1)"),
		"macro-heavy": []byte("(defmacro m (x) (quasiquote (+ (unquote x) 1)))\n(m (m (m 1)))"),
	}
	for i, seed := range fuzzseed.Adversarial() {
		seeds[fmt.Sprintf("adversarial/%d", i)] = seed
	}

	for name, seed := range seeds {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			runSessionBudgeted(t, seed, seed, allOpsScript())
		})
	}
}

// TestLSPFuzzDetectsSwallowedExpansionPanic proves checkExpansionPanics is
// wired to the object the analyser actually uses, and that it fires.
//
// The detector is worth exactly nothing if it reads a different expander from
// the one buildWorkspaceIndex installed, and that mistake is invisible: the
// check would return nil forever and every input would pass. So this reaches
// into the built config, makes THAT expander swallow a real panic (a nil form
// dereferences inside ExpandMacro), and requires the session-level check to
// report it.
func TestLSPFuzzDetectsSwallowedExpansionPanic(t *testing.T) {
	s, err := newSession(nil)
	if err != nil {
		t.Fatalf("newSession: %v", err)
	}

	s.srv.analysisCfgMu.RLock()
	cfg := s.srv.analysisCfg
	s.srv.analysisCfgMu.RUnlock()
	if cfg == nil {
		t.Fatal("no analysis config after ensureWorkspaceIndex")
	}
	exp, ok := cfg.MacroExpander.(*analysis.EnvMacroExpander)
	if !ok {
		t.Fatalf("buildWorkspaceIndex installed a %T, not an *analysis.EnvMacroExpander;"+
			" checkExpansionPanics reads analysis.PanicReporter, so if the new"+
			" expander does not implement it this target silently stops detecting"+
			" swallowed panics", cfg.MacroExpander)
	}

	if err := s.checkExpansionPanics(); err != nil {
		t.Fatalf("a fresh session already reports a swallowed panic: %v", err)
	}

	// A nil form panics inside ExpandMacro. The recover swallows it and returns
	// nil -- exactly what "not a macro" returns -- so only the count shows it.
	if v := exp.ExpandMacro(nil, "user"); v != nil {
		t.Fatalf("ExpandMacro(nil) returned %v, expected the recovered nil", v)
	}
	err = s.checkExpansionPanics()
	if err == nil {
		t.Fatal("checkExpansionPanics did not report a swallowed panic in the very" +
			" expander buildWorkspaceIndex installed; the detector is not wired to" +
			" the object the analyser uses")
	}
	if !strings.Contains(err.Error(), "swallowed") {
		t.Errorf("unexpected error text: %v", err)
	}
}
