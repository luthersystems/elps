// Copyright © 2024 The ELPS authors

package perf

import (
	"os"
	"path/filepath"

	"gopkg.in/yaml.v3"
)

// Config controls the behavior of the performance analyzer.
type Config struct {
	// ExpensiveFunctions are glob patterns matching function names considered
	// expensive (e.g., "db-*", "http-*"). Calls to these functions receive
	// an additional cost penalty.
	ExpensiveFunctions []string `yaml:"expensive_functions"`

	// ExpensiveCost is the extra cost added per call to an expensive function.
	ExpensiveCost int `yaml:"expensive_cost"`

	// LoopKeywords are symbols that introduce loop iteration in ELPS
	// (e.g., "dotimes", "map", "foldl"). Embedders may append
	// domain-specific iteration forms via config.
	LoopKeywords []string `yaml:"loop_keywords"`

	// LoopMultiplier is the assumed number of iterations per loop level.
	// Used to amplify costs inside loops.
	LoopMultiplier int `yaml:"loop_multiplier"`

	// MaxScore is the PERF001 threshold: functions with TotalScore above
	// this value are flagged as hot paths.
	MaxScore int `yaml:"max_score"`

	// MaxAcceptableOrder is the PERF002 warning threshold: functions with
	// ScalingOrder >= this value receive a warning.
	MaxAcceptableOrder int `yaml:"max_acceptable_order"`

	// ScalingErrorThreshold is the PERF002 error threshold: functions with
	// ScalingOrder >= this value receive an error.
	ScalingErrorThreshold int `yaml:"scaling_error_threshold"`

	// MaxRecursionOrder caps the scaling order assigned to recursive functions.
	MaxRecursionOrder int `yaml:"max_recursion_order"`

	// FunctionCosts provides per-function cost overrides. Keys are function
	// names (exact match), values are the base cost for a single call.
	// Overrides the default cost of 1. Embedders use this to assign
	// domain-specific weights (e.g., {"statedb:put": 100}).
	FunctionCosts map[string]int `yaml:"function_costs"`

	// SuppressionPrefix is the comment text that disables analysis for a
	// function. Defaults to "elps-analyze-disable". Embedders can
	// override this (e.g., "substrate-analyze-disable").
	SuppressionPrefix string `yaml:"suppression_prefix"`

	// AmplificationCausesScaling enables N+1 detection mode. When true,
	// calling an expensive function inside a loop adds +1 to the caller's
	// scaling order beyond the loop depth contribution. This surfaces the
	// classic N+1 query pattern more aggressively.
	AmplificationCausesScaling bool `yaml:"amplification_causes_scaling"`

	// Rules filters which rules to run. When empty, all rules run.
	// Valid values: "PERF001", "PERF002", "PERF003", "PERF004", "UNKNOWN001".
	Rules []string `yaml:"rules"`

	// ExcludeFiles are file glob patterns excluded from CLI-oriented analysis
	// workflows. The analyzer core remains file-agnostic.
	ExcludeFiles []string `yaml:"exclude_files"`

	// IncludeTests opts back into including *_test.lisp files in CLI-oriented
	// analysis workflows.
	IncludeTests bool `yaml:"include_tests"`
}

// DefaultConfig returns a Config with sensible defaults.
func DefaultConfig() *Config {
	return &Config{
		ExpensiveFunctions: []string{"db-*", "put-state", "get-state", "http-*"},
		ExpensiveCost:      50,
		LoopKeywords: []string{
			"dotimes", "map", "foldl", "foldr",
			"select", "reject",
		},
		LoopMultiplier:        20,
		MaxScore:              100000,
		MaxAcceptableOrder:    2,
		ScalingErrorThreshold: 3,
		MaxRecursionOrder:     5,
		SuppressionPrefix:     "elps-analyze-disable",
	}
}

// LoadConfigFile reads a YAML config file and returns a Config with
// defaults for any unset fields. Returns DefaultConfig() if the file
// does not exist.
func LoadConfigFile(path string) (*Config, error) {
	data, err := os.ReadFile(path) //nolint:gosec // config file path from user
	if err != nil {
		if os.IsNotExist(err) {
			return DefaultConfig(), nil
		}
		return nil, err
	}
	cfg := DefaultConfig()
	if err := yaml.Unmarshal(data, cfg); err != nil {
		return nil, err
	}
	return cfg, nil
}

// FindConfigFile searches for .elps-analyze.yaml starting from dir and
// walking up to the filesystem root. Returns the path if found, or empty
// string if not found.
func FindConfigFile(dir string) string {
	const name = ".elps-analyze.yaml"
	for {
		p := filepath.Join(dir, name)
		if _, err := os.Stat(p); err == nil {
			return p
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return ""
		}
		dir = parent
	}
}
