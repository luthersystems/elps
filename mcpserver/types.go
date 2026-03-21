package mcpserver

// ToolDescriptor describes an MCP tool exposed by the server.
type ToolDescriptor struct {
	Name        string `json:"name"`
	Description string `json:"description,omitempty"`
}

// Position is a 0-based line/character cursor position.
type Position struct {
	Line      int `json:"line"`
	Character int `json:"character"`
}

// Range is a start/end span in a source document.
type Range struct {
	Start Position `json:"start"`
	End   Position `json:"end"`
}

// Location identifies a source position, optionally virtual (e.g. for builtins).
type Location struct {
	Path         string `json:"path,omitempty"`
	Line         int    `json:"line"`
	Character    int    `json:"character"`
	EndLine      int    `json:"end_line"`
	EndCharacter int    `json:"end_character"`
	Virtual      bool   `json:"virtual,omitempty"`
	VirtualID    string `json:"virtual_id,omitempty"`
}

// DescribeServerInput is the (empty) input for the describe_server tool.
type DescribeServerInput struct{}

// DescribeServerResponse describes server metadata and capabilities.
type DescribeServerResponse struct {
	Name                 string           `json:"name"`
	Version              string           `json:"version"`
	ReadOnly             bool             `json:"read_only"`
	DefaultWorkspaceRoot string           `json:"default_workspace_root,omitempty"`
	Capabilities         []ToolDescriptor `json:"capabilities"`
}

// FileQueryInput is common input for tools that query a position in a file.
type FileQueryInput struct {
	Path          string  `json:"path"`
	Line          int     `json:"line"`
	Character     int     `json:"character"`
	Content       *string `json:"content,omitempty"`
	WorkspaceRoot *string `json:"workspace_root,omitempty"`
}

// DocumentQueryInput is input for tools that query an entire document.
type DocumentQueryInput struct {
	Path          string  `json:"path"`
	Content       *string `json:"content,omitempty"`
	WorkspaceRoot *string `json:"workspace_root,omitempty"`
}

// WorkspaceSymbolsInput is input for the workspace_symbols tool.
type WorkspaceSymbolsInput struct {
	Query         string  `json:"query"`
	WorkspaceRoot *string `json:"workspace_root,omitempty"`
	Limit         int     `json:"limit,omitempty"`
	Offset        int     `json:"offset,omitempty"`
}

// DiagnosticsInput is input for the diagnostics tool.
type DiagnosticsInput struct {
	Path             *string `json:"path,omitempty"`
	Content          *string `json:"content,omitempty"`
	WorkspaceRoot    *string `json:"workspace_root,omitempty"`
	IncludeWorkspace bool    `json:"include_workspace,omitempty"`
	MaxFiles         int     `json:"max_files,omitempty"`
	Offset           int     `json:"offset,omitempty"`
	Severity         *string `json:"severity,omitempty"`
}

// PerfToolConfig allows overriding performance analysis settings per-request.
type PerfToolConfig struct {
	ExpensiveFunctions      []string       `json:"expensive_functions,omitempty"`
	LoopKeywords            []string       `json:"loop_keywords,omitempty"`
	FunctionCosts           map[string]int `json:"function_costs,omitempty"`
	SuppressionPrefix       string         `json:"suppression_prefix,omitempty"`
	HotPathThreshold        int            `json:"hot_path_threshold,omitempty"`
	ScalingWarningThreshold int            `json:"scaling_warning_threshold,omitempty"`
	ScalingErrorThreshold   int            `json:"scaling_error_threshold,omitempty"`
}

// PerfSelectionInput is input for the perf_issues, call_graph, and hotspots tools.
type PerfSelectionInput struct {
	WorkspaceRoot *string         `json:"workspace_root,omitempty"`
	Paths         []string        `json:"paths,omitempty"`
	Rules         []string        `json:"rules,omitempty"`
	IncludeTests  bool            `json:"include_tests,omitempty"`
	Top           int             `json:"top,omitempty"`
	Config        *PerfToolConfig `json:"config,omitempty"`
}

// HoverResponse is the result of a hover query.
type HoverResponse struct {
	SymbolName string    `json:"symbol_name,omitempty"`
	Kind       string    `json:"kind,omitempty"`
	Signature  string    `json:"signature,omitempty"`
	Doc        string    `json:"doc,omitempty"`
	DefinedIn  *Location `json:"defined_in,omitempty"`
	Markdown   string    `json:"markdown,omitempty"`
	Found      bool      `json:"found"`
}

// DefinitionResponse is the result of a go-to-definition query.
type DefinitionResponse struct {
	Found    bool      `json:"found"`
	Location *Location `json:"location,omitempty"`
}

// ReferencesInput is input for the references tool.
type ReferencesInput struct {
	Path               string  `json:"path"`
	Line               int     `json:"line"`
	Character          int     `json:"character"`
	Content            *string `json:"content,omitempty"`
	WorkspaceRoot      *string `json:"workspace_root,omitempty"`
	IncludeDeclaration bool    `json:"include_declaration,omitempty"`
	Limit              int     `json:"limit,omitempty"`
	Offset             int     `json:"offset,omitempty"`
}

// ReferencesResponse is the result of a find-references query.
type ReferencesResponse struct {
	SymbolName string     `json:"symbol_name,omitempty"`
	References []Location `json:"references"`
	Truncated  bool       `json:"truncated,omitempty"`
	Total      int        `json:"total,omitempty"`
}

// DocumentSymbol describes a top-level symbol in a document.
type DocumentSymbol struct {
	Name   string `json:"name"`
	Kind   string `json:"kind"`
	Detail string `json:"detail,omitempty"`
	Path   string `json:"path"`
	Range  Range  `json:"range"`
}

// DocumentSymbolsResponse is the result of a document symbols query.
type DocumentSymbolsResponse struct {
	Symbols []DocumentSymbol `json:"symbols"`
}

// WorkspaceSymbol describes a symbol found across the workspace.
type WorkspaceSymbol struct {
	Name    string `json:"name"`
	Kind    string `json:"kind"`
	Package string `json:"package,omitempty"`
	Path    string `json:"path"`
	Range   Range  `json:"range"`
}

// WorkspaceSymbolsResponse is the result of a workspace symbols query.
type WorkspaceSymbolsResponse struct {
	Symbols   []WorkspaceSymbol `json:"symbols"`
	Truncated bool              `json:"truncated,omitempty"`
	Total     int               `json:"total,omitempty"`
}

// Diagnostic is a parse or lint diagnostic for a source location.
type Diagnostic struct {
	Source   string `json:"source,omitempty"`
	Code     string `json:"code,omitempty"`
	Severity string `json:"severity"`
	Message  string `json:"message"`
	Range    Range  `json:"range"`
}

// FileDiagnostics groups diagnostics for a single file.
type FileDiagnostics struct {
	Path        string       `json:"path"`
	Diagnostics []Diagnostic `json:"diagnostics"`
}

// DiagnosticsResponse is the result of a diagnostics query.
type DiagnosticsResponse struct {
	Files      []FileDiagnostics `json:"files"`
	Truncated  bool              `json:"truncated,omitempty"`
	TotalFiles int               `json:"total_files,omitempty"`
}

// TraceEntry is a single entry in a performance issue trace.
type TraceEntry struct {
	Function string    `json:"function"`
	Location *Location `json:"location,omitempty"`
	Note     string    `json:"note,omitempty"`
}

// PerfIssue describes a performance issue found by analysis.
type PerfIssue struct {
	Rule        string       `json:"rule"`
	Severity    string       `json:"severity"`
	Message     string       `json:"message"`
	Function    string       `json:"function"`
	Path        string       `json:"path,omitempty"`
	Location    *Location    `json:"location,omitempty"`
	Details     []string     `json:"details,omitempty"`
	Fingerprint string       `json:"fingerprint,omitempty"`
	Trace       []TraceEntry `json:"trace,omitempty"`
}

// SolvedFunctionSummary summarizes a function's resolved performance cost.
type SolvedFunctionSummary struct {
	Name         string    `json:"name"`
	Path         string    `json:"path,omitempty"`
	Location     *Location `json:"location,omitempty"`
	LocalCost    int       `json:"local_cost"`
	TotalScore   int       `json:"total_score"`
	ScalingOrder int       `json:"scaling_order"`
	InCycle      bool      `json:"in_cycle"`
}

// PerfIssuesResponse is the result of a perf_issues query.
type PerfIssuesResponse struct {
	Issues      []PerfIssue             `json:"issues"`
	Solved      []SolvedFunctionSummary `json:"solved,omitempty"`
	Truncated   bool                    `json:"truncated,omitempty"`
	TotalIssues int                     `json:"total_issues,omitempty"`
}

// CallGraphFunction describes a function node in a call graph.
type CallGraphFunction struct {
	Name         string    `json:"name"`
	Path         string    `json:"path,omitempty"`
	Location     *Location `json:"location,omitempty"`
	LocalCost    int       `json:"local_cost"`
	MaxLoopDepth int       `json:"max_loop_depth"`
}

// CallGraphEdge describes a caller-callee edge in a call graph.
type CallGraphEdge struct {
	Caller      string    `json:"caller"`
	Callee      string    `json:"callee"`
	Location    *Location `json:"location,omitempty"`
	LoopDepth   int       `json:"loop_depth"`
	InLoop      bool      `json:"in_loop"`
	IsExpensive bool      `json:"is_expensive"`
}

// CallGraphResponse is the result of a call_graph query.
type CallGraphResponse struct {
	Functions      []CallGraphFunction `json:"functions"`
	Edges          []CallGraphEdge     `json:"edges"`
	Truncated      bool                `json:"truncated,omitempty"`
	TotalFunctions int                 `json:"total_functions,omitempty"`
	TotalEdges     int                 `json:"total_edges,omitempty"`
}

// HotspotsResponse is the result of a hotspots query.
type HotspotsResponse struct {
	Functions []SolvedFunctionSummary `json:"functions"`
}

// HelpInput is the (empty) input for the help tool.
type HelpInput struct{}

// HelpResponse is the result of the help tool.
type HelpResponse struct {
	Content string `json:"content"`
}
