// Copyright © 2024 The ELPS authors

package lsp

import (
	"slices"
	"strings"
	"sync"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// Document represents an open text document tracked by the LSP server.
type Document struct {
	mu          sync.Mutex
	URI         string
	Version     int32
	Content     string
	ast         []*lisp.LVal
	analysis    *analysis.Result
	parseErrors []error

	// overLimit records that the content exceeded the store's size limit at
	// the last Open or Change, so it was stored without being parsed (ast and
	// parseErrors are nil). analyzeAndPublish reports it to the client
	// instead of analysing. See parse.
	overLimit bool

	// publishedVersion tracks the latest version whose diagnostics were
	// successfully published. Used to discard stale debounced results.
	publishedVersion int32

	// publishedDiagnostics caches the last set of diagnostics published
	// for this document. Used by code actions to look up the original
	// diagnostic Code (analyzer name), working around a glsp bug where
	// IntegerOrString.Value is lost during JSON round-trip from the client.
	publishedDiagnostics []protocol.Diagnostic
}

// DocumentSnapshot is an immutable copy of a document's state at a point
// in time. Slice fields are shallow-copied so the snapshot remains safe
// even if the document's slices are replaced by a concurrent parse().
type DocumentSnapshot struct {
	URI         string
	Version     int32
	Content     string
	AST         []*lisp.LVal
	ParseErrors []error
}

// Snapshot returns an immutable copy of the document's current state.
// Slice fields are copied so the snapshot is safe to use after the
// document's lock is released.
func (d *Document) Snapshot() DocumentSnapshot {
	d.mu.Lock()
	defer d.mu.Unlock()
	return DocumentSnapshot{
		URI:         d.URI,
		Version:     d.Version,
		Content:     d.Content,
		AST:         slices.Clone(d.ast),
		ParseErrors: slices.Clone(d.parseErrors),
	}
}

// parse parses the document content and caches the AST using the
// fault-tolerant parser. This recovers partial ASTs and collects all
// parse errors in a single pass.
//
// A document larger than maxBytes (when maxBytes > 0) is left unparsed: the
// AST and parse errors are cleared and nothing is read. The size limit exists
// to bound the work a single document can cost the server, and parsing is the
// first and largest part of that work -- the AST is on the order of a hundred
// bytes per source byte, and it is built on every didOpen and didChange before
// analyzeAndPublish ever consults the limit. Checking only there left a client
// able to make the server parse a document of any size on every keystroke
// (FuzzLSPSession/36ec6d2dd72dfd1f: a 1-byte document doubled by each of 48
// didChange notifications was still being parsed at 64 MiB / 10 GiB of heap
// when the watchdog fired, with analysis correctly skipped every time).
func (d *Document) parse(maxBytes int) {
	d.overLimit = maxBytes > 0 && len(d.Content) > maxBytes
	if d.overLimit {
		d.ast = nil
		d.parseErrors = nil
		return
	}
	s := token.NewScanner(uriToPath(d.URI), strings.NewReader(d.Content))
	p := rdparser.New(s)
	result := p.ParseProgramFaultTolerant()
	d.ast = result.Exprs
	d.parseErrors = result.Errors
}

// analyze runs semantic analysis on the cached AST.
func (d *Document) analyze(cfg *analysis.Config) {
	if d.ast == nil {
		return
	}
	d.analysis = analysis.Analyze(d.ast, cfg)
}

// DocumentStore manages open documents with thread-safe access.
type DocumentStore struct {
	mu   sync.RWMutex
	docs map[string]*Document

	// maxBytes is the document size above which Open and Change store the
	// content without parsing it. 0 means no limit. Set from the server's
	// maxDocumentBytes so the limit bounds parsing as well as analysis; see
	// Document.parse.
	maxBytes int
}

// NewDocumentStore creates an empty document store.
func NewDocumentStore() *DocumentStore {
	return &DocumentStore{docs: make(map[string]*Document)}
}

// Open adds a document to the store and parses it.
func (s *DocumentStore) Open(uri string, version int32, content string) *Document {
	doc := &Document{
		URI:     uri,
		Version: version,
		Content: content,
	}
	doc.parse(s.maxBytes)
	s.mu.Lock()
	s.docs[uri] = doc
	s.mu.Unlock()
	return doc
}

// Change updates a document's content (full sync) and re-parses it.
func (s *DocumentStore) Change(uri string, version int32, content string) *Document {
	s.mu.Lock()
	doc, ok := s.docs[uri]
	if !ok {
		doc = &Document{URI: uri}
		s.docs[uri] = doc
	}
	s.mu.Unlock()

	doc.mu.Lock()
	doc.Version = version
	doc.Content = content
	doc.parse(s.maxBytes)
	// Clear cached analysis; it will be rebuilt on next request.
	doc.analysis = nil
	doc.mu.Unlock()
	return doc
}

// Close removes a document from the store.
func (s *DocumentStore) Close(uri string) {
	s.mu.Lock()
	delete(s.docs, uri)
	s.mu.Unlock()
}

// Get retrieves a document by URI. Returns nil if not found.
func (s *DocumentStore) Get(uri string) *Document {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return s.docs[uri]
}

// All returns a snapshot of all open documents.
func (s *DocumentStore) All() []*Document {
	s.mu.RLock()
	defer s.mu.RUnlock()
	docs := make([]*Document, 0, len(s.docs))
	for _, doc := range s.docs {
		docs = append(docs, doc)
	}
	return docs
}
