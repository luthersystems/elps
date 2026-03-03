// Copyright © 2024 The ELPS authors

package lsp

import (
	"encoding/json"

	"github.com/tliron/glsp"
)

// handlerWrapper wraps a glsp.Handler to intercept LSP 3.17 methods
// (like textDocument/inlayHint) that the protocol_3_16 handler doesn't
// know about. Unrecognized methods are forwarded to the inner handler.
type handlerWrapper struct {
	inner  glsp.Handler
	server *Server
}

func (w *handlerWrapper) Handle(ctx *glsp.Context) (any, bool, bool, error) {
	switch ctx.Method {
	case "textDocument/inlayHint":
		var params InlayHintParams
		if err := json.Unmarshal(ctx.Params, &params); err != nil {
			return nil, true, false, err
		}
		result, err := w.server.textDocumentInlayHint(&params)
		return result, true, true, err
	default:
		return w.inner.Handle(ctx)
	}
}
