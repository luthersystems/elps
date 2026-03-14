package mcpserver

type ToolDescriptor struct {
	Name        string `json:"name"`
	Description string `json:"description,omitempty"`
}

type Position struct {
	Line      int `json:"line"`
	Character int `json:"character"`
}

type Range struct {
	Start Position `json:"start"`
	End   Position `json:"end"`
}

type Location struct {
	Path         string `json:"path,omitempty"`
	Line         int    `json:"line"`
	Character    int    `json:"character"`
	EndLine      int    `json:"end_line"`
	EndCharacter int    `json:"end_character"`
	Virtual      bool   `json:"virtual,omitempty"`
	VirtualID    string `json:"virtual_id,omitempty"`
}

type DescribeServerInput struct{}

type DescribeServerResponse struct {
	Name                 string           `json:"name"`
	Version              string           `json:"version"`
	ReadOnly             bool             `json:"read_only"`
	DefaultWorkspaceRoot string           `json:"default_workspace_root,omitempty"`
	Capabilities         []ToolDescriptor `json:"capabilities"`
}

type FileQueryInput struct {
	Path          string  `json:"path"`
	Line          int     `json:"line"`
	Character     int     `json:"character"`
	Content       *string `json:"content,omitempty"`
	WorkspaceRoot *string `json:"workspace_root,omitempty"`
}

type DocumentQueryInput struct {
	Path          string  `json:"path"`
	Content       *string `json:"content,omitempty"`
	WorkspaceRoot *string `json:"workspace_root,omitempty"`
}

type WorkspaceSymbolsInput struct {
	Query         string  `json:"query"`
	WorkspaceRoot *string `json:"workspace_root,omitempty"`
}

type DiagnosticsInput struct {
	Path             *string `json:"path,omitempty"`
	Content          *string `json:"content,omitempty"`
	WorkspaceRoot    *string `json:"workspace_root,omitempty"`
	IncludeWorkspace bool    `json:"include_workspace,omitempty"`
}

type PerfToolConfig struct {
	ExpensiveFunctions      []string       `json:"expensive_functions,omitempty"`
	LoopKeywords            []string       `json:"loop_keywords,omitempty"`
	FunctionCosts           map[string]int `json:"function_costs,omitempty"`
	SuppressionPrefix       string         `json:"suppression_prefix,omitempty"`
	HotPathThreshold        int            `json:"hot_path_threshold,omitempty"`
	ScalingWarningThreshold int            `json:"scaling_warning_threshold,omitempty"`
	ScalingErrorThreshold   int            `json:"scaling_error_threshold,omitempty"`
}

type PerfSelectionInput struct {
	WorkspaceRoot *string         `json:"workspace_root,omitempty"`
	Paths         []string        `json:"paths,omitempty"`
	Rules         []string        `json:"rules,omitempty"`
	IncludeTests  bool            `json:"include_tests,omitempty"`
	Top           int             `json:"top,omitempty"`
	Config        *PerfToolConfig `json:"config,omitempty"`
}

type HoverResponse struct {
	SymbolName string    `json:"symbol_name,omitempty"`
	Kind       string    `json:"kind,omitempty"`
	Signature  string    `json:"signature,omitempty"`
	Doc        string    `json:"doc,omitempty"`
	DefinedIn  *Location `json:"defined_in,omitempty"`
	Markdown   string    `json:"markdown,omitempty"`
	Found      bool      `json:"found"`
}

type DefinitionResponse struct {
	Found    bool      `json:"found"`
	Location *Location `json:"location,omitempty"`
}

type ReferencesInput struct {
	Path               string  `json:"path"`
	Line               int     `json:"line"`
	Character          int     `json:"character"`
	Content            *string `json:"content,omitempty"`
	WorkspaceRoot      *string `json:"workspace_root,omitempty"`
	IncludeDeclaration bool    `json:"include_declaration,omitempty"`
}

type ReferencesResponse struct {
	SymbolName string     `json:"symbol_name,omitempty"`
	References []Location `json:"references"`
}

type DocumentSymbol struct {
	Name   string `json:"name"`
	Kind   string `json:"kind"`
	Detail string `json:"detail,omitempty"`
	Path   string `json:"path"`
	Range  Range  `json:"range"`
}

type DocumentSymbolsResponse struct {
	Symbols []DocumentSymbol `json:"symbols"`
}

type WorkspaceSymbol struct {
	Name    string `json:"name"`
	Kind    string `json:"kind"`
	Package string `json:"package,omitempty"`
	Path    string `json:"path"`
	Range   Range  `json:"range"`
}

type WorkspaceSymbolsResponse struct {
	Symbols []WorkspaceSymbol `json:"symbols"`
}

type Diagnostic struct {
	Source   string `json:"source,omitempty"`
	Code     string `json:"code,omitempty"`
	Severity string `json:"severity"`
	Message  string `json:"message"`
	Range    Range  `json:"range"`
}

type FileDiagnostics struct {
	Path        string       `json:"path"`
	Diagnostics []Diagnostic `json:"diagnostics"`
}

type DiagnosticsResponse struct {
	Files []FileDiagnostics `json:"files"`
}

type TraceEntry struct {
	Function string    `json:"function"`
	Location *Location `json:"location,omitempty"`
	Note     string    `json:"note,omitempty"`
}

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

type SolvedFunctionSummary struct {
	Name         string    `json:"name"`
	Path         string    `json:"path,omitempty"`
	Location     *Location `json:"location,omitempty"`
	LocalCost    int       `json:"local_cost"`
	TotalScore   int       `json:"total_score"`
	ScalingOrder int       `json:"scaling_order"`
	InCycle      bool      `json:"in_cycle"`
}

type PerfIssuesResponse struct {
	Issues []PerfIssue             `json:"issues"`
	Solved []SolvedFunctionSummary `json:"solved,omitempty"`
}

type CallGraphFunction struct {
	Name         string    `json:"name"`
	Path         string    `json:"path,omitempty"`
	Location     *Location `json:"location,omitempty"`
	LocalCost    int       `json:"local_cost"`
	MaxLoopDepth int       `json:"max_loop_depth"`
}

type CallGraphEdge struct {
	Caller      string    `json:"caller"`
	Callee      string    `json:"callee"`
	Location    *Location `json:"location,omitempty"`
	LoopDepth   int       `json:"loop_depth"`
	InLoop      bool      `json:"in_loop"`
	IsExpensive bool      `json:"is_expensive"`
}

type CallGraphResponse struct {
	Functions []CallGraphFunction `json:"functions"`
	Edges     []CallGraphEdge     `json:"edges"`
}

type HotspotsResponse struct {
	Functions []SolvedFunctionSummary `json:"functions"`
}
