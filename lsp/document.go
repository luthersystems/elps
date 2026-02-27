// Copyright © 2024 The ELPS authors

package lsp

import (
	"strings"
	"sync"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
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
}

// parse parses the document content and caches the AST using the
// fault-tolerant parser. This recovers partial ASTs and collects all
// parse errors in a single pass.
func (d *Document) parse() {
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
	doc.parse()
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
	doc.parse()
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
