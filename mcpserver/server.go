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
	registry      *lisp.PackageRegistry
	env           *lisp.LEnv
	workspaceRoot string
	perfConfig    *perf.Config
	impl          *mcp.Implementation
	instructions  string
	logger        *slog.Logger
	registrars    []func(*mcp.Server) error

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
		registry:      s.registry,
		env:           s.env,
		workspaceRoot: s.workspaceRoot,
		perfConfig:    s.perfConfig,
		linter:        &lint.Linter{Analyzers: lint.DefaultAnalyzers()},
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
	s.registerTool("describe_server", "Describe server metadata and supported tools")
	mcp.AddTool(s.server, &mcp.Tool{
		Name:        "describe_server",
		Description: "Describe server metadata and supported tools",
	}, func(ctx context.Context, _ *mcp.CallToolRequest, _ DescribeServerInput) (*mcp.CallToolResult, DescribeServerResponse, error) {
		return nil, DescribeServerResponse{
			Name:                 s.impl.Name,
			Version:              s.impl.Version,
			ReadOnly:             true,
			DefaultWorkspaceRoot: s.service.workspaceRoot,
			Capabilities:         append([]ToolDescriptor(nil), s.tools...),
		}, nil
	})

	s.registerTool("hover", "Look up symbol information at a file position")
	mcp.AddTool(s.server, &mcp.Tool{Name: "hover", Description: "Look up symbol information at a file position"}, s.service.hoverTool)

	s.registerTool("definition", "Find the definition of a symbol at a file position")
	mcp.AddTool(s.server, &mcp.Tool{Name: "definition", Description: "Find the definition of a symbol at a file position"}, s.service.definitionTool)

	s.registerTool("references", "Find references for a symbol at a file position")
	mcp.AddTool(s.server, &mcp.Tool{Name: "references", Description: "Find references for a symbol at a file position"}, s.service.referencesTool)

	s.registerTool("document_symbols", "List top-level symbols in a document")
	mcp.AddTool(s.server, &mcp.Tool{Name: "document_symbols", Description: "List top-level symbols in a document"}, s.service.documentSymbolsTool)

	s.registerTool("workspace_symbols", "Search top-level symbols across a workspace")
	mcp.AddTool(s.server, &mcp.Tool{Name: "workspace_symbols", Description: "Search top-level symbols across a workspace"}, s.service.workspaceSymbolsTool)

	s.registerTool("diagnostics", "Collect parse and lint diagnostics for a file or workspace")
	mcp.AddTool(s.server, &mcp.Tool{Name: "diagnostics", Description: "Collect parse and lint diagnostics for a file or workspace"}, s.service.diagnosticsTool)

	s.registerTool("perf_issues", "Run ELPS performance analysis and return structured issues")
	mcp.AddTool(s.server, &mcp.Tool{Name: "perf_issues", Description: "Run ELPS performance analysis and return structured issues"}, s.service.perfIssuesTool)

	s.registerTool("call_graph", "Build a structured call graph from ELPS performance analysis")
	mcp.AddTool(s.server, &mcp.Tool{Name: "call_graph", Description: "Build a structured call graph from ELPS performance analysis"}, s.service.callGraphTool)

	s.registerTool("hotspots", "Return the highest-cost functions from ELPS performance analysis")
	mcp.AddTool(s.server, &mcp.Tool{Name: "hotspots", Description: "Return the highest-cost functions from ELPS performance analysis"}, s.service.hotspotsTool)
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
