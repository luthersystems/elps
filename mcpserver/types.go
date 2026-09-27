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
	Path          string  `json:"path" jsonschema:"File path: a bare filename for a file in the workspace root, or an absolute path. Do not prefix the workspace directory name."`
	Line          int     `json:"line" jsonschema:"0-based line number (LSP convention: editor line 1 is 0)."`
	Character     int     `json:"character" jsonschema:"0-based character offset within the line."`
	Content       *string `json:"content,omitempty" jsonschema:"Unsaved buffer content to analyze instead of the file on disk; path still supplies the workspace context."`
	WorkspaceRoot *string `json:"workspace_root,omitempty" jsonschema:"Workspace root for this call; defaults to the server startup root. A new root triggers a fresh directory scan."`
}

// DocumentQueryInput is input for tools that query an entire document.
type DocumentQueryInput struct {
	Path          string  `json:"path" jsonschema:"File path: a bare filename for a file in the workspace root, or an absolute path. Do not prefix the workspace directory name."`
	Content       *string `json:"content,omitempty" jsonschema:"Unsaved buffer content to analyze instead of the file on disk; path still supplies the workspace context."`
	WorkspaceRoot *string `json:"workspace_root,omitempty" jsonschema:"Workspace root for this call; defaults to the server startup root. A new root triggers a fresh directory scan."`
	Limit         int     `json:"limit,omitempty" jsonschema:"Maximum number of results; when results are trimmed the response sets truncated and a total count."`
	Offset        int     `json:"offset,omitempty" jsonschema:"Number of results to skip, for pagination with limit."`
}

// WorkspaceSymbolsInput is input for the workspace_symbols tool.
type WorkspaceSymbolsInput struct {
	Query         string  `json:"query" jsonschema:"Case-insensitive substring matched against symbol names and package-qualified names; empty matches every symbol."`
	WorkspaceRoot *string `json:"workspace_root,omitempty" jsonschema:"Workspace root for this call; defaults to the server startup root. A new root triggers a fresh directory scan."`
	Limit         int     `json:"limit,omitempty" jsonschema:"Maximum number of results; when results are trimmed the response sets truncated and a total count."`
	Offset        int     `json:"offset,omitempty" jsonschema:"Number of results to skip, for pagination with limit."`
}

// DiagnosticsInput is input for the diagnostics tool.
type DiagnosticsInput struct {
	Path             *string `json:"path,omitempty" jsonschema:"File to check; omit it and pass content to check a buffer, reported as <stdin>."`
	Content          *string `json:"content,omitempty" jsonschema:"Source to check instead of the file on disk."`
	WorkspaceRoot    *string `json:"workspace_root,omitempty" jsonschema:"Workspace root for this call; defaults to the server startup root. A new root triggers a fresh directory scan."`
	IncludeWorkspace bool    `json:"include_workspace,omitempty" jsonschema:"Load the other files of the workspace so symbols defined there resolve; without it they read as undefined."`
	MaxFiles         int     `json:"max_files,omitempty" jsonschema:"Maximum number of files to report on."`
	Offset           int     `json:"offset,omitempty" jsonschema:"Number of results to skip, for pagination with limit."`
	Severity         *string `json:"severity,omitempty" jsonschema:"Only return diagnostics of this severity: error, warning or info. Any other value is an invalid_input error."`
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
	WorkspaceRoot *string         `json:"workspace_root,omitempty" jsonschema:"Workspace root for this call; defaults to the server startup root. A new root triggers a fresh directory scan."`
	Paths         []string        `json:"paths,omitempty" jsonschema:"Files or directories to analyze. The whole workspace is analyzed when empty, so pass paths to keep output small."`
	Rules         []string        `json:"rules,omitempty" jsonschema:"Rule IDs to keep (PERF001 to PERF004, UNKNOWN001). Filters issues and solved functions on perf_issues; no effect on call_graph."`
	IncludeTests  bool            `json:"include_tests,omitempty" jsonschema:"Include *_test.lisp files, which are excluded by default."`
	Top           int             `json:"top,omitempty" jsonschema:"Keep only the N highest-cost entries. Required (greater than zero) for hotspots."`
	Config        *PerfToolConfig `json:"config,omitempty" jsonschema:"Advanced cost-model overrides (function costs, thresholds, loop keywords). The defaults suit normal use."`
}

// HoverResponse is the result of a hover query.
type HoverResponse struct {
	SymbolName string        `json:"symbol_name,omitempty"`
	Kind       string        `json:"kind,omitempty"`
	Signature  string        `json:"signature,omitempty"`
	Doc        string        `json:"doc,omitempty"`
	DefinedIn  *Location     `json:"defined_in,omitempty"`
	Markdown   string        `json:"markdown,omitempty"`
	Found      bool          `json:"found"`
	Meta       *ResponseMeta `json:"_meta,omitempty"`
}

// DefinitionResponse is the result of a go-to-definition query.
type DefinitionResponse struct {
	Found    bool          `json:"found"`
	Location *Location     `json:"location,omitempty"`
	Meta     *ResponseMeta `json:"_meta,omitempty"`
}

// ReferencesInput is input for the references tool.
type ReferencesInput struct {
	Path               string  `json:"path" jsonschema:"File path: a bare filename for a file in the workspace root, or an absolute path. Do not prefix the workspace directory name."`
	Line               int     `json:"line" jsonschema:"0-based line number (LSP convention: editor line 1 is 0)."`
	Character          int     `json:"character" jsonschema:"0-based character offset within the line."`
	Content            *string `json:"content,omitempty" jsonschema:"Unsaved buffer content to analyze instead of the file on disk; path still supplies the workspace context."`
	WorkspaceRoot      *string `json:"workspace_root,omitempty" jsonschema:"Workspace root for this call; defaults to the server startup root. A new root triggers a fresh directory scan."`
	IncludeDeclaration bool    `json:"include_declaration,omitempty" jsonschema:"Also return the declaration itself among the references."`
	Limit              int     `json:"limit,omitempty" jsonschema:"Maximum number of results; when results are trimmed the response sets truncated and a total count."`
	Offset             int     `json:"offset,omitempty" jsonschema:"Number of results to skip, for pagination with limit."`
}

// ReferencesResponse is the result of a find-references query.
type ReferencesResponse struct {
	SymbolName string        `json:"symbol_name,omitempty"`
	References []Location    `json:"references"`
	Truncated  bool          `json:"truncated,omitempty"`
	Total      int           `json:"total,omitempty"`
	Meta       *ResponseMeta `json:"_meta,omitempty"`
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
	Symbols   []DocumentSymbol `json:"symbols"`
	Truncated bool             `json:"truncated,omitempty"`
	Total     int              `json:"total,omitempty"`
	Meta      *ResponseMeta    `json:"_meta,omitempty"`
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
	Meta      *ResponseMeta     `json:"_meta,omitempty"`
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
	Meta       *ResponseMeta     `json:"_meta,omitempty"`
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
	Solved      []SolvedFunctionSummary `json:"solved"`
	Truncated   bool                    `json:"truncated,omitempty"`
	TotalIssues int                     `json:"total_issues,omitempty"`
	Meta        *ResponseMeta           `json:"_meta,omitempty"`
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
	Meta           *ResponseMeta       `json:"_meta,omitempty"`
}

// HotspotsResponse is the result of a hotspots query.
type HotspotsResponse struct {
	Functions []SolvedFunctionSummary `json:"functions"`
	Meta      *ResponseMeta           `json:"_meta,omitempty"`
}

// HelpInput is the (empty) input for the help tool.
type HelpInput struct{}

// HelpResponse is the result of the help tool.
type HelpResponse struct {
	Content string `json:"content"`
}

// FormatInput is input for the format tool.
type FormatInput struct {
	Path          string  `json:"path,omitempty" jsonschema:"File to format; or pass content instead."`
	Content       *string `json:"content,omitempty" jsonschema:"Source to format instead of a file."`
	IndentSize    int     `json:"indent_size,omitempty" jsonschema:"Indentation width in spaces; the formatter default when unset or 0."`
	CheckOnly     bool    `json:"check_only,omitempty" jsonschema:"Only report whether formatting would change the source; the formatted text is not returned."`
	WorkspaceRoot *string `json:"workspace_root,omitempty" jsonschema:"Workspace root for this call; defaults to the server startup root. A new root triggers a fresh directory scan."`
}

// FormatResponse is the result of the format tool.
type FormatResponse struct {
	Formatted string        `json:"formatted"`
	Changed   bool          `json:"changed"`
	Meta      *ResponseMeta `json:"_meta,omitempty"`
}

// LintInput is input for the lint tool.
type LintInput struct {
	Path          string   `json:"path,omitempty" jsonschema:"File to lint; or pass content instead."`
	Content       *string  `json:"content,omitempty" jsonschema:"Source to lint instead of the file on disk."`
	WorkspaceRoot *string  `json:"workspace_root,omitempty" jsonschema:"Workspace root for this call; defaults to the server startup root. A new root triggers a fresh directory scan."`
	Checks        []string `json:"checks,omitempty" jsonschema:"Analyzer names to run (the help tool lists them); every analyzer when empty. An unknown name is an invalid_input error."`
	Severity      *string  `json:"severity,omitempty" jsonschema:"Only return diagnostics of this severity: error, warning or info. Any other value is an invalid_input error."`
	Limit         int      `json:"limit,omitempty" jsonschema:"Maximum number of results; when results are trimmed the response sets truncated and a total count."`
	Offset        int      `json:"offset,omitempty" jsonschema:"Number of results to skip, for pagination with limit."`
}

// LintResponse is the result of the lint tool.
type LintResponse struct {
	Diagnostics []Diagnostic  `json:"diagnostics"`
	Truncated   bool          `json:"truncated,omitempty"`
	Total       int           `json:"total,omitempty"`
	Meta        *ResponseMeta `json:"_meta,omitempty"`
}

// DocInput is input for the doc tool.
type DocInput struct {
	Query   string   `json:"query,omitempty" jsonschema:"Name to look up: a function, macro, operator or package, optionally qualified (math:sin)."`
	Queries []string `json:"queries,omitempty" jsonschema:"Several names to look up in one call; each gets its own result."`
	Package bool     `json:"package,omitempty" jsonschema:"Treat the query as a package name and list its symbols."`
}

// DocResult is the result of a single doc lookup in batch mode.
type DocResult struct {
	Query   string     `json:"query"`
	Found   bool       `json:"found"`
	Symbol  *DocSymbol `json:"symbol,omitempty"`
}

// DocResponse is the result of the doc tool.
type DocResponse struct {
	Found   bool          `json:"found"`
	Symbol  *DocSymbol    `json:"symbol,omitempty"`
	Package *DocPackage   `json:"package,omitempty"`
	Batch   []DocResult   `json:"batch,omitempty"`
	Meta    *ResponseMeta `json:"_meta,omitempty"`
}

// DocSymbol describes a documented symbol.
type DocSymbol struct {
	Name    string       `json:"name"`
	Kind    string       `json:"kind"`
	Doc     string       `json:"doc,omitempty"`
	Formals *DocFormals  `json:"formals,omitempty"`
}

// DocFormals describes a function's parameter list.
type DocFormals struct {
	Required []string `json:"required"`
	Optional []string `json:"optional,omitempty"`
	Rest     string   `json:"rest,omitempty"`
	Keys     []string `json:"keys,omitempty"`
}

// DocPackage describes a package for documentation.
type DocPackage struct {
	Name    string      `json:"name"`
	Doc     string      `json:"doc,omitempty"`
	Symbols []DocSymbol `json:"symbols"`
}

// TestInput is input for the test tool.
type TestInput struct {
	Path          string  `json:"path" jsonschema:"Test file to run (a *_test.lisp file); or pass content instead."`
	Content       *string `json:"content,omitempty" jsonschema:"Test source to run instead of the file on disk."`
	WorkspaceRoot *string `json:"workspace_root,omitempty" jsonschema:"Workspace root for this call; defaults to the server startup root. A new root triggers a fresh directory scan."`
}

// TestResult describes a single test outcome.
type TestResult struct {
	Name   string `json:"name"`
	Passed bool   `json:"passed"`
	Error  string `json:"error,omitempty"`
}

// TestResponse is the result of the test tool.
type TestResponse struct {
	Path    string        `json:"path"`
	Tests   []TestResult  `json:"tests"`
	Passed  int           `json:"passed"`
	Failed  int           `json:"failed"`
	Total   int           `json:"total"`
	Meta    *ResponseMeta `json:"_meta,omitempty"`
}

// EvalInput is input for the eval tool.
type EvalInput struct {
	Expression  string   `json:"expression,omitempty" jsonschema:"ELPS source to evaluate; may contain several forms, and the value of the last one is returned. Set this or expressions."`
	Expressions []string `json:"expressions,omitempty" jsonschema:"several independent sources, each evaluated in its own fresh environment with its own result. Set this or expression."`
}

// EvalResult is the result of evaluating a single expression.
type EvalResult struct {
	Expression string `json:"expression"`
	Value      string `json:"value,omitempty"`
	Error      string `json:"error,omitempty"`
}

// EvalResponse is the result of the eval tool.
type EvalResponse struct {
	Value   string        `json:"value"`
	Results []string      `json:"results,omitempty"`
	Batch   []EvalResult  `json:"batch,omitempty"`
	Error   string        `json:"error,omitempty"`
	Meta    *ResponseMeta `json:"_meta,omitempty"`
}

// ResponseMeta provides metadata about the tool response.
type ResponseMeta struct {
	WorkspaceRoot string `json:"workspace_root,omitempty"`
	ElapsedMs     int64  `json:"elapsed_ms"`
	FileCount     int    `json:"file_count,omitempty"`
}
