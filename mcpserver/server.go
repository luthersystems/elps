package mcpserver

import (
	"context"
	"log/slog"

	"github.com/luthersystems/elps/analysis/perf"
	"github.com/luthersystems/elps/lint"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/modelcontextprotocol/go-sdk/mcp"
)

const (
	defaultImplementationName    = "elps-mcp"
	defaultImplementationVersion = "0.1.0"
)

type Option func(*Server)

type Server struct {
	registry        *lisp.PackageRegistry
	env             *lisp.LEnv
	workspaceRoot   string
	perfConfig      *perf.Config
	excludePatterns []string
	impl            *mcp.Implementation
	instructions    string
	logger          *slog.Logger
	registrars      []func(*mcp.Server) error

	service  *service
	server   *mcp.Server
	tools    []ToolDescriptor
	buildErr error
}

func WithRegistry(reg *lisp.PackageRegistry) Option {
	return func(s *Server) { s.registry = reg }
}

func WithEnv(env *lisp.LEnv) Option {
	return func(s *Server) { s.env = env }
}

func WithWorkspaceRoot(root string) Option {
	return func(s *Server) { s.workspaceRoot = root }
}

func WithPerfConfig(cfg *perf.Config) Option {
	return func(s *Server) { s.perfConfig = clonePerfConfig(cfg) }
}

func WithImplementation(impl *mcp.Implementation) Option {
	return func(s *Server) { s.impl = impl }
}

func WithInstructions(instructions string) Option {
	return func(s *Server) { s.instructions = instructions }
}

// WithExcludes sets glob patterns for files to skip during workspace scanning.
func WithExcludes(patterns []string) Option {
	return func(s *Server) { s.excludePatterns = patterns }
}

func WithLogger(logger *slog.Logger) Option {
	return func(s *Server) {
		if logger != nil {
			s.logger = logger
		}
	}
}

func WithToolRegistrar(register func(*mcp.Server) error) Option {
	return func(s *Server) {
		if register != nil {
			s.registrars = append(s.registrars, register)
		}
	}
}

func New(opts ...Option) *Server {
	s := &Server{
		perfConfig: clonePerfConfig(nil),
		logger:     slog.Default(),
	}
	for _, opt := range opts {
		opt(s)
	}
	if s.impl == nil {
		s.impl = &mcp.Implementation{
			Name:    defaultImplementationName,
			Version: defaultImplementationVersion,
		}
	}
	if s.env == nil && s.registry == nil {
		env, err := lisplib.NewDocEnv()
		if err != nil {
			s.buildErr = err
		} else {
			s.env = env
		}
	}

	s.service = newService(serviceConfig{
		registry:        s.registry,
		env:             s.env,
		workspaceRoot:   s.workspaceRoot,
		perfConfig:      s.perfConfig,
		excludePatterns: s.excludePatterns,
		linter:          &lint.Linter{Analyzers: lint.DefaultAnalyzers()},
		logger:          s.logger,
	})
	s.server = mcp.NewServer(s.impl, &mcp.ServerOptions{
		Instructions: s.instructions,
		Logger:       s.logger,
	})
	s.registerCoreTools()
	for _, register := range s.registrars {
		if err := register(s.server); err != nil {
			s.buildErr = err
			break
		}
	}
	if s.buildErr == nil {
		s.buildErr = s.syncToolDescriptors()
	}
	return s
}

func (s *Server) RunStdio(ctx context.Context) error {
	if s.buildErr != nil {
		return s.buildErr
	}
	return s.server.Run(ctx, &mcp.StdioTransport{})
}

func (s *Server) MCPServer() *mcp.Server {
	return s.server
}

func (s *Server) registerTool(name, description string) {
	s.tools = append(s.tools, ToolDescriptor{Name: name, Description: description})
}

// syncToolDescriptors rebuilds the tools list by introspecting the MCP
// server. This is necessary because third-party registrars (WithToolRegistrar)
// add tools directly to the mcp.Server without going through registerTool,
// so the only way to capture the full tool list is to read it back.
//
// TODO: simplify if the MCP go-sdk adds a server-side ListRegisteredTools API.
func (s *Server) syncToolDescriptors() error {
	ctx := context.Background()
	serverTransport, clientTransport := mcp.NewInMemoryTransports()
	serverSession, err := s.server.Connect(ctx, serverTransport, nil)
	if err != nil {
		return err
	}
	defer func() {
		_ = serverSession.Close()
	}()

	client := mcp.NewClient(&mcp.Implementation{
		Name:    defaultImplementationName + "-introspect",
		Version: defaultImplementationVersion,
	}, nil)
	clientSession, err := client.Connect(ctx, clientTransport, nil)
	if err != nil {
		return err
	}
	defer func() {
		_ = clientSession.Close()
	}()

	result, err := clientSession.ListTools(ctx, nil)
	if err != nil {
		return err
	}

	descriptors := make([]ToolDescriptor, 0, len(result.Tools))
	for _, tool := range result.Tools {
		descriptors = append(descriptors, ToolDescriptor{
			Name:        tool.Name,
			Description: tool.Description,
		})
	}
	s.tools = descriptors
	return nil
}

func (s *Server) registerCoreTools() {
	s.registerTool("describe_server", "Describe server metadata, capabilities, and supported tools. Returns server name, version, workspace root, and a list of all registered tools with descriptions.")
	mcp.AddTool(s.server, &mcp.Tool{
		Name:        "describe_server",
		Description: "Describe server metadata, capabilities, and supported tools. Returns server name, version, workspace root, and a list of all registered tools with descriptions.",
	}, func(ctx context.Context, _ *mcp.CallToolRequest, _ DescribeServerInput) (*mcp.CallToolResult, DescribeServerResponse, error) {
		return nil, DescribeServerResponse{
			Name:                 s.impl.Name,
			Version:              s.impl.Version,
			ReadOnly:             true,
			DefaultWorkspaceRoot: s.service.workspaceRoot,
			Capabilities:         append([]ToolDescriptor(nil), s.tools...),
		}, nil
	})

	s.registerTool("help", "Return a usage guide covering coordinates, paths, content overrides, scoping, result limiting, lint analyzers, and perf rules.")
	mcp.AddTool(s.server, &mcp.Tool{Name: "help", Description: "Return a usage guide covering coordinates, paths, content overrides, scoping, result limiting, lint analyzers, and perf rules."}, s.service.helpTool)

	s.registerTool("hover", "Look up symbol information (name, kind, signature, docs, definition location) at a 0-based line/character position in a file. Supports content override for unsaved buffers.")
	mcp.AddTool(s.server, &mcp.Tool{Name: "hover", Description: "Look up symbol information (name, kind, signature, docs, definition location) at a 0-based line/character position in a file. Supports content override for unsaved buffers."}, s.service.hoverTool)

	s.registerTool("definition", "Find where a symbol at a given position is defined. Returns the file path and range, or a virtual location for builtins.")
	mcp.AddTool(s.server, &mcp.Tool{Name: "definition", Description: "Find where a symbol at a given position is defined. Returns the file path and range, or a virtual location for builtins."}, s.service.definitionTool)

	s.registerTool("references", "Find all references to a symbol across the current file and workspace. Supports include_declaration and limit parameters.")
	mcp.AddTool(s.server, &mcp.Tool{Name: "references", Description: "Find all references to a symbol across the current file and workspace. Supports include_declaration and limit parameters."}, s.service.referencesTool)

	s.registerTool("document_symbols", "List all top-level symbol definitions (functions, macros, variables, types) in a single document. Supports content override.")
	mcp.AddTool(s.server, &mcp.Tool{Name: "document_symbols", Description: "List all top-level symbol definitions (functions, macros, variables, types) in a single document. Supports content override."}, s.service.documentSymbolsTool)

	s.registerTool("workspace_symbols", "Search for top-level symbol definitions across all .lisp files in a workspace. Supports query filtering and limit. Does not return Go-registered builtins.")
	mcp.AddTool(s.server, &mcp.Tool{Name: "workspace_symbols", Description: "Search for top-level symbol definitions across all .lisp files in a workspace. Supports query filtering and limit. Does not return Go-registered builtins."}, s.service.workspaceSymbolsTool)

	s.registerTool("diagnostics", "Collect parse errors and lint diagnostics for a file, inline content, or entire workspace. Supports severity filtering and max_files limit.")
	mcp.AddTool(s.server, &mcp.Tool{Name: "diagnostics", Description: "Collect parse errors and lint diagnostics for a file, inline content, or entire workspace. Supports severity filtering and max_files limit."}, s.service.diagnosticsTool)

	s.registerTool("perf_issues", "Run ELPS performance analysis and return structured issues with traces. Use paths to scope, top to limit output, rules to filter by rule ID.")
	mcp.AddTool(s.server, &mcp.Tool{Name: "perf_issues", Description: "Run ELPS performance analysis and return structured issues with traces. Use paths to scope, top to limit output, rules to filter by rule ID."}, s.service.perfIssuesTool)

	s.registerTool("call_graph", "Build a structured call graph showing functions and caller-callee edges. Use paths to scope and top to limit to highest-cost functions.")
	mcp.AddTool(s.server, &mcp.Tool{Name: "call_graph", Description: "Build a structured call graph showing functions and caller-callee edges. Use paths to scope and top to limit to highest-cost functions."}, s.service.callGraphTool)

	s.registerTool("hotspots", "Return the highest-cost functions from performance analysis. Requires top parameter. Use paths to scope the analysis.")
	mcp.AddTool(s.server, &mcp.Tool{Name: "hotspots", Description: "Return the highest-cost functions from performance analysis. Requires top parameter. Use paths to scope the analysis."}, s.service.hotspotsTool)
}

func clonePerfConfig(cfg *perf.Config) *perf.Config {
	if cfg == nil {
		return perf.DefaultConfig()
	}
	out := *cfg
	if cfg.ExpensiveFunctions != nil {
		out.ExpensiveFunctions = append([]string(nil), cfg.ExpensiveFunctions...)
	}
	if cfg.LoopKeywords != nil {
		out.LoopKeywords = append([]string(nil), cfg.LoopKeywords...)
	}
	if cfg.FunctionCosts != nil {
		out.FunctionCosts = make(map[string]int, len(cfg.FunctionCosts))
		for k, v := range cfg.FunctionCosts {
			out.FunctionCosts[k] = v
		}
	}
	if cfg.Rules != nil {
		out.Rules = append([]string(nil), cfg.Rules...)
	}
	if cfg.ExcludeFiles != nil {
		out.ExcludeFiles = append([]string(nil), cfg.ExcludeFiles...)
	}
	return &out
}
