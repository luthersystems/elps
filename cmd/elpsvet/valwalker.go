// Copyright © 2026 The ELPS authors

package main

import (
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	"golang.org/x/tools/go/analysis"
)

// valWalkerAnalyzer requires an audit for each new value walker.
// A function must dispatch on LType and either recur or push LVal pointers in a loop.
// Dispatch includes typed switch tags, tagless LType comparisons, LVal.Type conditions, and lisp.ShapeOf calls.
// Recursion follows the package's static calls, concrete methods, and local closures.
// Closure diagnostics name the enclosing declared function. Each function reports once.
// Loop detection recognises append to slices of *lisp.LVal, including ... and named slices.
// It also recognises indexed writes to those slices and append to frame slices with LVal fields.
//
// Limits: cross-package calls, interface calls, and method values are not followed.
// Function parameters, aliases, aggregate closures, reflection, and unsafe calls are not followed.
// Custom stack Push methods and pointer-free index queues are not recognised as pushes.
// No data-flow proof connects a pushed pointer or recursive call to the dispatched value.
// A clean run excludes new walkers of these forms. It does not prove that no walker exists.
// Dispatch and stack pushes must occur in the same function. Split helper drivers can bypass this rule.
// Package-level closures are not inspected. Closure bindings use their last literal assignment, without control-flow analysis.
// Tagless cases require a comparison with an LType operand. Boolean dispatch helpers are not followed.
// An audited walker is accepted by a reason in valueWalkerFunctions (elps's own walkers)
// or by `//elpsvet:allow-valwalker <reason of at least three words>` in the doc comment of
// the declared function the diagnostic names. The marker covers closures that function owns.
// It is the only way for a module that runs elpsvalwalker over its own code to record an audit.
// Inside the elps module the marker is ignored: elps keeps one mechanism, the table, whose
// rows are classified, counted and pinned by TestValueWalkerAllowlistReasons in one place.
// A marker in a body, trailing a line, above a closure, or with a shorter reason does not count.
var valWalkerAnalyzer = &analysis.Analyzer{
	Name: "elpsvalwalker",
	Doc:  "flag new LType-dispatching recursive or stack-based walkers; static calls only, unless audited by //elpsvet:allow-valwalker <reason> or a valueWalkerFunctions row",
	Run:  runValWalker,
}

// valueWalkerFunctions names existing walkers and their traversal contracts.
var valueWalkerFunctions = map[string]string{ //nolint:gosec // G101: function names and audit reasons contain no credentials.
	"github.com/luthersystems/elps/lisp/lisplib/libjson.tagWalker.object":                  "hand-rolled JSON walker: limits, paths and callback order pinned by goldens",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.tagWalker.value":                   "hand-rolled JSON walker: limits, paths and callback order pinned by goldens",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.canonWalker.nativeMap":             "hand-rolled JSON walker: limits, paths and callback order pinned by goldens",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.canonWalker.sortedMap":             "hand-rolled JSON walker: limits, paths and callback order pinned by goldens",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.canonWalker.value":                 "hand-rolled JSON walker: limits, paths and callback order pinned by goldens",
	"github.com/luthersystems/elps/analysis.analyzer.expandPackageForms":                   "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/analysis.scanFileFull":                                  "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/analysis.walkLoadFile":                                  "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/astutil.ClassifyNodes":                                  "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/astutil.ExportNames":                                    "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/elpstest.RunBenchmark":                                  "oracle: constructs benchmark arguments and checks evaluator results",
	"github.com/luthersystems/elps/elpstest.aliasWalker.value":                             "oracle: checks value storage independently of production traversal",
	"github.com/luthersystems/elps/elpstest.stateWalker.value":                             "oracle: checks value storage independently of production traversal",
	"github.com/luthersystems/elps/elpstest.walkOracleGraph":                               "oracle: checks value storage independently of production traversal",
	"github.com/luthersystems/elps/elpsutil.templateCompiler.compile":                      "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/elpsutil.templateNode.expand":                           "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/formatter.printer.tryPrefixForm":                        "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/formatter.printer.writeCompactExpr":                     "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/formatter.printer.writeExpr":                            "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/formatter.printer.writeQuote":                           "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/internal/codewalk.Syntax":                               "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/internal/fuzzfp.Guard.walk":                             "oracle: checks value storage independently of production traversal",
	"github.com/luthersystems/elps/internal/fuzzval.Gen.arrayND":                           "oracle: constructs bounded test graphs and validates generated children",
	"github.com/luthersystems/elps/internal/fuzzval.Gen.fun":                               "oracle: constructs bounded test graphs and validates generated children",
	"github.com/luthersystems/elps/internal/fuzzval.Gen.sortMap":                           "oracle: constructs bounded test graphs and validates generated children",
	"github.com/luthersystems/elps/internal/fuzzval.Gen.tagged":                            "oracle: constructs bounded test graphs and validates generated children",
	"github.com/luthersystems/elps/lint.checkLispBindings":                                 "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lint.evaluatedSyntax.code":                              "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lint.invalidExportLiteral":                              "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lint.letRecursionState.visit":                           "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lint.mentionsSymbol":                                    "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lint.mentionsUnquoted":                                  "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lint.nodeMentionsRethrowContext":                        "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lint.walkLambdaListCalls":                               "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lint.walkRethrowTemplate":                               "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/internal/codewalk.syntax":                               "syntax walker: the raw source traversal behind codewalk.Syntax",
	"github.com/luthersystems/elps/lisp.CodeWalker.sourceCalls":                            "syntax walker: source calls for the internal formals event, quote and quasiquote skipped",
	"github.com/luthersystems/elps/lisp.Array":                                             "hot path: evaluates forms or constructs sequences with LVal slices",
	"github.com/luthersystems/elps/lisp.CodeWalker.call":                                   "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lisp.CodeWalker.compound":                               "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lisp.CodeWalker.flet":                                   "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lisp.CodeWalker.formals":                                "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lisp.CodeWalker.runtimeForm":                            "syntax walker: dispatches code forms with quote, lexical scope, and macro expansion rules",
	"github.com/luthersystems/elps/lisp.CodeWalker.scanPackage":                            "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lisp.CodeWalker.sourceValue":                            "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lisp.CodeWalker.special":                                "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lisp.CodeWalker.template":                               "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lisp.CodeWalker.templateList":                           "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lisp.LEnv.call":                                         "hot path: preserves evaluator or copy identity and allocation contracts",
	"github.com/luthersystems/elps/lisp.LEnv.callBuiltin":                                  "hot path: preserves evaluator or copy identity and allocation contracts",
	"github.com/luthersystems/elps/lisp.LEnv.evalUnchecked":                                "hot path: preserves evaluator or copy identity and allocation contracts",
	"github.com/luthersystems/elps/lisp.LEnv.evalSExpr":                                    "hot path: preserves evaluator or copy identity and allocation contracts",
	"github.com/luthersystems/elps/lisp.LEnv.evalSExprCells":                               "hot path: preserves evaluator or copy identity and allocation contracts",
	"github.com/luthersystems/elps/lisp.LEnv.funCall":                                      "hot path: preserves evaluator or copy identity and allocation contracts",
	"github.com/luthersystems/elps/lisp.LEnv.macroCall":                                    "hot path: preserves evaluator or copy identity and allocation contracts",
	"github.com/luthersystems/elps/lisp.LEnv.specialOpCall":                                "hot path: preserves evaluator or copy identity and allocation contracts",
	"github.com/luthersystems/elps/lisp.LVal.Len":                                          "hot path: reads one container and its storage metadata without graph traversal",
	"github.com/luthersystems/elps/lisp.LVal.equalIter":                                    "hot path: preserves evaluator or copy identity and allocation contracts",
	"github.com/luthersystems/elps/lisp.LVal.equalShallow":                                 "hot path: preserves evaluator or copy identity and allocation contracts",
	"github.com/luthersystems/elps/lisp.LVal.mapRange":                                     "hot path: reads one container and its storage metadata without graph traversal",
	"github.com/luthersystems/elps/lisp.builtinCompose":                                    "hot path: evaluates forms or constructs sequences with LVal slices",
	"github.com/luthersystems/elps/lisp.builtinErrorStack":                                 "hot path: evaluates forms or constructs sequences with LVal slices",
	"github.com/luthersystems/elps/lisp.builtinMakeSequence":                               "hot path: evaluates forms or constructs sequences with LVal slices",
	"github.com/luthersystems/elps/lisp.builtinMap":                                        "hot path: evaluates forms or constructs sequences with LVal slices",
	"github.com/luthersystems/elps/lisp.builtinReject":                                     "hot path: evaluates forms or constructs sequences with LVal slices",
	"github.com/luthersystems/elps/lisp.builtinReverse":                                    "hot path: evaluates forms or constructs sequences with LVal slices",
	"github.com/luthersystems/elps/lisp.builtinSelect":                                     "hot path: evaluates forms or constructs sequences with LVal slices",
	"github.com/luthersystems/elps/lisp.builtinZip":                                        "hot path: evaluates forms or constructs sequences with LVal slices",
	"github.com/luthersystems/elps/lisp.checkContainerDepth":                               "hot path: preserves storage depth and sharing checks",
	"github.com/luthersystems/elps/lisp.classifySymbolValue":                               "syntax walker: validates sealed source storage before package publication",
	"github.com/luthersystems/elps/lisp.containsCycle":                                     "specialized traversal: renderer cycle probes use an independent colouring pass",
	"github.com/luthersystems/elps/lisp.convertContainer":                                  "specialized traversal: conversion keeps distinct memo weights and array policy",
	"github.com/luthersystems/elps/lisp.copier.copy":                                       "hot path: drives the copy frames and preserves graph identity",
	"github.com/luthersystems/elps/lisp.copier.copyNode":                                   "hot path: preserves evaluator or copy identity and allocation contracts",
	"github.com/luthersystems/elps/lisp.copier.mapData":                                    "hot path: preserves evaluator or copy identity and allocation contracts",
	"github.com/luthersystems/elps/lisp.detacher.detach":                                   "specialized traversal: detachment copies storage and native payloads with its own memo",
	"github.com/luthersystems/elps/lisp.detacher.detachNode":                               "specialized traversal: detachment dispatch is separate from its frame driver",
	"github.com/luthersystems/elps/lisp.exportArgs":                                        "syntax walker: validates export declarations and their symbol lists",
	"github.com/luthersystems/elps/lisp.findAndUnquote":                                    "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lisp.firstUnsealed":                                     "syntax walker: validates sealed source storage before package publication",
	"github.com/luthersystems/elps/lisp.genSymLevelWalk":                                   "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lisp.loaderWalk.check":                                  "syntax walker: validates reader trees and counts quoted evaluation work",
	"github.com/luthersystems/elps/lisp.makeByteSeq":                                       "hot path: evaluates forms or constructs sequences with LVal slices",
	"github.com/luthersystems/elps/lisp.opAssert":                                          "hot path: evaluates forms or constructs sequences with LVal slices",
	"github.com/luthersystems/elps/lisp.opHandlerBind":                                     "hot path: evaluates forms or constructs sequences with LVal slices",
	"github.com/luthersystems/elps/lisp.opLet":                                             "hot path: evaluates forms or constructs sequences with LVal slices",
	"github.com/luthersystems/elps/lisp.renderChildren":                                    "specialized traversal: bounded rendering uses multiple passes and cycle probes",
	"github.com/luthersystems/elps/lisp.sealChildren":                                      "syntax walker: seals source cells with a slice cursor per ancestor",
	"github.com/luthersystems/elps/lisp.sealFP.walk":                                       "oracle: fingerprints sealed storage independently of production traversal",
	"github.com/luthersystems/elps/lisp.sourceExprFormalNames":                             "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lisp.stampMacroExpansion":                               "syntax walker: stamps generated syntax while preserving caller metadata",
	"github.com/luthersystems/elps/lisp.templateCompiler.value":                            "specialized traversal: template storage walks include environments and capacity tails",
	"github.com/luthersystems/elps/lisp.templateInventory.val":                             "specialized traversal: admission registers identity before walking environments and storage",
	"github.com/luthersystems/elps/lisp.threadValue":                                       "hot path: evaluates forms or constructs sequences with LVal slices",
	"github.com/luthersystems/elps/lisp.validateExportArgs":                                "syntax walker: validates export declarations and their symbol lists",
	"github.com/luthersystems/elps/lisp.valueRenderer.errorMessage":                        "specialized traversal: bounded rendering uses multiple passes and cycle probes",
	"github.com/luthersystems/elps/lisp.valueRenderer.nested":                              "specialized traversal: bounded rendering uses multiple passes and cycle probes",
	"github.com/luthersystems/elps/lisp.valueRenderer.value":                               "specialized traversal: bounded rendering uses multiple passes and cycle probes",
	"github.com/luthersystems/elps/lisp/lisplib/libelpspath.copyContainer":                 "hand-rolled path walker: operation budget, height memo and off-path copying preserve aliases",
	"github.com/luthersystems/elps/lisp/lisplib/libelpspath.copySeqOffPath":                "hand-rolled path walker: operation budget, height memo and off-path copying preserve aliases",
	"github.com/luthersystems/elps/lisp/lisplib/libelpspath.iterPath.delete":               "hand-rolled path walker: selected-path accessors and mutating iterators follow addressed children",
	"github.com/luthersystems/elps/lisp/lisplib/libelpspath.iterPath.get":                  "hand-rolled path walker: selected-path accessors and mutating iterators follow addressed children",
	"github.com/luthersystems/elps/lisp/lisplib/libelpspath.iterPath.null":                 "hand-rolled path walker: selected-path accessors and mutating iterators follow addressed children",
	"github.com/luthersystems/elps/lisp/lisplib/libelpspath.iterPath.set":                  "hand-rolled path walker: selected-path accessors and mutating iterators follow addressed children",
	"github.com/luthersystems/elps/lisp/lisplib/libelpspath.okSimpleContainerContents":     "hand-rolled path walker: delayed cycle tracking and width-weighted memo bound validation work",
	"github.com/luthersystems/elps/lisp/lisplib/libelpspath.okSimpleContainerTypeGuarded":  "hand-rolled path walker: delayed cycle tracking and width-weighted memo bound validation work",
	"github.com/luthersystems/elps/lisp/lisplib/libelpspath.okSimpleTypeGuarded":           "hand-rolled path walker: delayed cycle tracking and width-weighted memo bound validation work",
	"github.com/luthersystems/elps/lisp/lisplib/libelpspath.rangePath.nilMutate":           "hand-rolled path walker: selected-path accessors and mutating iterators follow addressed children",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.Serializer.DumpStringBuiltin":      "specialized traversal: serializer adapters construct values and delegate encoding or conversion",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.Serializer.convertValue":           "specialized traversal: conversion keeps distinct memo weights and array policy",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.Serializer.dumpModeBuiltin":        "specialized traversal: serializer adapters construct values and delegate encoding or conversion",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.Serializer.loadInterfaceOpts":      "specialized traversal: serializer adapters construct values and delegate encoding or conversion",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.encoder.encodeDeepValue":           "hot path: plain JSON encoding emits bytes with recursive traversal",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.LoadDurableRoots":                  "specialized traversal: checks the top-level cells of one decoded root list, with no recursion",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableDecoder.dataHolder":         "specialized traversal: parses one array's data holder from JSON bytes and checks it is a list header",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableDecoder.codeNode":           "specialized traversal: parses one closure code node from JSON bytes and constructs it; scalars are checked by type, no value graph is walked",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableEncoder.scanCodeNode":       "hand-rolled JSON walker: durable closure traversal, each object once, depth and value limits, pinned by closure tests",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableEncoder.scanCodeBody":       "hand-rolled JSON walker: durable closure traversal, each object once, depth and value limits, pinned by closure tests",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableEncoder.codeNode":           "hand-rolled JSON walker: durable closure traversal, each object once, depth and value limits, pinned by closure tests",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableEncoder.codeBody":           "hand-rolled JSON walker: durable closure traversal, each object once, depth and value limits, pinned by closure tests",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableDecoder.closureValue":       "hand-rolled JSON walker: durable closure traversal, each object once, depth and value limits, pinned by closure tests",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableDecoder.frameRef":           "hand-rolled JSON walker: durable closure traversal, each object once, depth and value limits, pinned by closure tests",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableDecoder.codeValue":          "specialized traversal: parses one closure's code from JSON bytes through codeNode and checks its formals header",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableDecoder.frameValue":         "specialized traversal: parses one captured frame from JSON bytes, each binding value read once by value",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.closureScope.codeNames":            "specialized traversal: collects the symbols of one closure code, already bounded in size and depth by scanCode or codeNode, once per code object",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.closureScope.dynamicReason":        "specialized traversal: resolves the symbols of one closure code, already bounded in size and depth, each lookup a bounded step",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableDecoder.dims":               "specialized traversal: parses one array's dimension list from JSON bytes, scalars only, no recursion",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableDecoder.multiArray":         "specialized traversal: parses JSON bytes and constructs values rather than walking a value graph",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableDecoder.native":             "specialized traversal: parses JSON bytes and constructs values rather than walking a value graph",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableDecoder.object":             "specialized traversal: parses JSON bytes and constructs values rather than walking a value graph",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableDecoder.value":              "specialized traversal: parses JSON bytes and constructs values rather than walking a value graph",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableEncoder.body":               "hand-rolled JSON walker: durable second pass in scan's order, each shared object written once then referenced, depth, value and byte limits, pinned by durable goldens",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableEncoder.scan":               "hand-rolled JSON walker: durable first pass, depth-first in the write order, each object walked once, depth and value limits, pinned by durable goldens",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.durableEncoder.scanNative":         "hand-rolled JSON walker: durable first pass over one native payload, codec called once per native, depth and value limits, pinned by durable goldens",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.typedEncoder.mapMembers":           "hand-rolled JSON walker: collects one map's members in canonical order without recursion; member order pinned by goldens",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.typedDecoder.multiArray":           "specialized traversal: parses JSON bytes and constructs values rather than walking a value graph",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.typedDecoder.object":               "specialized traversal: parses JSON bytes and constructs values rather than walking a value graph",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.typedEncoder.array":                "hand-rolled JSON walker: limits, paths and callback order pinned by goldens",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.typedEncoder.sortedMap":            "hand-rolled JSON walker: limits, paths and callback order pinned by goldens",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.typedEncoder.value":                "hand-rolled JSON walker: limits, paths and callback order pinned by goldens",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.untagWalker.object":                "hand-rolled JSON walker: limits, paths and callback order pinned by goldens",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.untagWalker.tagged":                "hand-rolled JSON walker: limits, paths and callback order pinned by goldens",
	"github.com/luthersystems/elps/lisp/lisplib/libjson.untagWalker.value":                 "hand-rolled JSON walker: limits, paths and callback order pinned by goldens",
	"github.com/luthersystems/elps/lisp/lisplib/libschema.builtinArrayOf":                  "hot path: schema callbacks construct and apply recursive validation constraints",
	"github.com/luthersystems/elps/lisp/lisplib/libschema.builtinCheckTaggedVal":           "hot path: schema callbacks construct and apply recursive validation constraints",
	"github.com/luthersystems/elps/lisp/lisplib/libschema.builtinHasKey":                   "hot path: schema callbacks construct and apply recursive validation constraints",
	"github.com/luthersystems/elps/lisp/lisplib/libschema.builtinMayHaveKey":               "hot path: schema callbacks construct and apply recursive validation constraints",
	"github.com/luthersystems/elps/lisp/lisplib/libschema.builtinNoOtherKeys":              "hot path: schema callbacks construct and apply recursive validation constraints",
	"github.com/luthersystems/elps/lisp/lisplib/libschema.builtinWhen":                     "hot path: schema callbacks construct and apply recursive validation constraints",
	"github.com/luthersystems/elps/lisp/lisplib/libschema.getHandler":                      "hot path: schema callbacks construct and apply recursive validation constraints",
	"github.com/luthersystems/elps/lisp/x/debugger/dapserver.exprWalk.hasUserFunCall":      "syntax walker: inspects source calls for debugger stepping",
	"github.com/luthersystems/elps/lisp/x/debugger/dapserver.handler.collectStepInTargets": "syntax walker: inspects source calls for debugger stepping",
	"github.com/luthersystems/elps/lisp/x/debugger/dapserver.handler.onVariables":          "specialized traversal: debugger expands one value level per request",
	"github.com/luthersystems/elps/lsp.collectFoldingRanges":                               "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lsp.collectSemanticTokens":                              "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lsp.lambdaHeadAt":                                       "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lsp.nodeChainAt":                                        "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lsp.walkForHints":                                       "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/lsp.walkNodeForCall":                                    "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/minifier.collectQuotedSymbols":                          "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/minifier.firstDynamicEvaluation":                        "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/minifier.firstGlobalFallback":                           "syntax walker: raw forms keep macro templates and quoted data in the fallback scan",
	"github.com/luthersystems/elps/minifier.literalExportArgument":                         "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/minifier.recordQualifiedReferences":                     "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/parser/rdparser.Parser.ParseConsExpression":             "syntax walker: visits source forms with syntax and quote rules",
	"github.com/luthersystems/elps/parser/rdparser.Parser.ParseList":                       "syntax walker: visits source forms with syntax and quote rules",
}

type valueWalkFunc struct {
	body     *ast.BlockStmt
	owner    *ast.FuncDecl
	calls    []*valueWalkFunc
	dispatch bool
	push     bool
}

type valueWalkGraph struct {
	pass     *analysis.Pass
	funcs    map[*types.Func]*valueWalkFunc
	lits     map[*ast.FuncLit]*valueWalkFunc
	closures map[types.Object]*valueWalkFunc
	nodes    []*valueWalkFunc
}

const (
	valWalkerAllowMarker   = "elpsvet:allow-valwalker"
	valWalkerAllowMinWords = 3
	elpsModulePath         = "github.com/luthersystems/elps"
)

// hasJustifiedValWalkerAllow reports whether a function's doc comment carries
// a reasoned allow-valwalker. Only a // line comment counts. Text after a
// " // " is not part of the reason (an analysistest "// want" or padding),
// but a "//" inside a word, as in a URL, is.
func hasJustifiedValWalkerAllow(cg *ast.CommentGroup) bool {
	if cg == nil {
		return false
	}
	for _, c := range cg.List {
		text, ok := strings.CutPrefix(c.Text, "//")
		if !ok {
			continue
		}
		if i := strings.Index(text, " //"); i >= 0 {
			text = text[:i]
		}
		if justifiedAllow(text, valWalkerAllowMarker, valWalkerAllowMinWords) {
			return true
		}
	}
	return false
}

// valWalkerInElpsModule reports whether a package belongs to the elps module,
// where the marker is ignored. It uses the module path when the driver knows
// it, so a separate module named github.com/luthersystems/elps/x keeps its
// markers, and falls back to the package path otherwise (analysistest's
// GOPATH fixtures).
func valWalkerInElpsModule(module, pkg string) bool {
	if module != "" {
		return module == elpsModulePath
	}
	return pkg == elpsModulePath || strings.HasPrefix(pkg, elpsModulePath+"/")
}

func runValWalker(pass *analysis.Pass) (any, error) {
	g := &valueWalkGraph{pass: pass, funcs: map[*types.Func]*valueWalkFunc{}, lits: map[*ast.FuncLit]*valueWalkFunc{}, closures: map[types.Object]*valueWalkFunc{}}
	for _, file := range pass.Files {
		if strings.HasSuffix(pass.Fset.Position(file.Pos()).Filename, "_test.go") {
			continue
		}
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil {
				continue
			}
			node := &valueWalkFunc{body: fn.Body, owner: fn}
			obj, _ := pass.TypesInfo.Defs[fn.Name].(*types.Func)
			g.funcs[obj] = node
			g.nodes = append(g.nodes, node)
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				if lit, ok := n.(*ast.FuncLit); ok {
					child := &valueWalkFunc{body: lit.Body, owner: fn}
					g.lits[lit] = child
					g.nodes = append(g.nodes, child)
				}
				return true
			})
		}
	}
	for _, node := range g.nodes {
		ast.Inspect(node.body, func(n ast.Node) bool {
			switch n := n.(type) {
			case *ast.AssignStmt:
				if len(n.Lhs) == len(n.Rhs) {
					for i, rhs := range n.Rhs {
						g.bindClosure(n.Lhs[i], rhs)
					}
				}
			case *ast.ValueSpec:
				if len(n.Names) == len(n.Values) {
					for i, rhs := range n.Values {
						g.bindClosure(n.Names[i], rhs)
					}
				}
			}
			return true
		})
	}
	for _, node := range g.nodes {
		g.scan(node)
	}
	reported := map[*ast.FuncDecl]bool{}
	module := ""
	if pass.Module != nil {
		module = pass.Module.Path
	}
	elpsModule := valWalkerInElpsModule(module, pass.Pkg.Path())
	for _, node := range g.nodes {
		if !node.dispatch || !node.push && !valueWalkRecursive(node, node, map[*valueWalkFunc]bool{}) {
			continue
		}
		fn := node.owner
		name := pass.Pkg.Path() + "." + funcDeclName(pass, fn)
		if valueWalkerFunctions[name] != "" || reported[fn] || !elpsModule && hasJustifiedValWalkerAllow(fn.Doc) {
			continue
		}
		reported[fn] = true
		pass.Reportf(fn.Name.Pos(), "value walker %s dispatches on lisp.LType and walks children; audit its traversal contract with //elpsvet:allow-valwalker <reason> in its doc comment (a valueWalkerFunctions row inside elps)", name)
	}
	return nil, nil
}

func (g *valueWalkGraph) bindClosure(lhs, rhs ast.Expr) {
	id, ok := ast.Unparen(lhs).(*ast.Ident)
	if !ok {
		return
	}
	lit, ok := ast.Unparen(rhs).(*ast.FuncLit)
	if !ok {
		return
	}
	obj := g.pass.TypesInfo.ObjectOf(id)
	if obj != nil {
		g.closures[obj] = g.lits[lit]
	}
}

func (g *valueWalkGraph) callee(call *ast.CallExpr) *valueWalkFunc {
	if lit, ok := ast.Unparen(call.Fun).(*ast.FuncLit); ok {
		return g.lits[lit]
	}
	if id, ok := ast.Unparen(call.Fun).(*ast.Ident); ok {
		if closure := g.closures[g.pass.TypesInfo.ObjectOf(id)]; closure != nil {
			return closure
		}
	}
	if sel, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr); ok {
		if selection := g.pass.TypesInfo.Selections[sel]; selection != nil {
			recv := selection.Recv()
			if ptr, ok := recv.Underlying().(*types.Pointer); ok {
				recv = ptr.Elem()
			}
			if _, ok := recv.Underlying().(*types.Interface); ok {
				return nil
			}
		}
	}
	return g.funcs[calleeFunc(g.pass, call)]
}

func (g *valueWalkGraph) scan(node *valueWalkFunc) {
	pass := g.pass
	ast.Inspect(node.body, func(n ast.Node) bool {
		switch n := n.(type) {
		case *ast.FuncLit:
			return false
		case *ast.SwitchStmt:
			if n.Tag != nil && isLispNamed(pass.TypesInfo.TypeOf(n.Tag), "LType") {
				node.dispatch = true
			} else if n.Tag == nil {
				for _, stmt := range n.Body.List {
					arm, ok := stmt.(*ast.CaseClause)
					if !ok {
						continue
					}
					for _, cond := range arm.List {
						if valueWalkLTypeComparison(pass, cond) {
							node.dispatch = true
						}
					}
				}
			}
		case *ast.IfStmt:
			ast.Inspect(n.Cond, func(expr ast.Node) bool {
				sel, ok := expr.(*ast.SelectorExpr)
				if ok {
					if selection := pass.TypesInfo.Selections[sel]; selection != nil && isLValTypeField(selection.Obj()) {
						node.dispatch = true
					}
				}
				return true
			})
		case *ast.CallExpr:
			if fn := calleeFunc(pass, n); fn != nil && fn.Pkg() != nil && fn.Pkg().Path() == lispPkgPath && fn.Name() == "ShapeOf" {
				node.dispatch = true
			}
			if child := g.callee(n); child != nil {
				node.calls = append(node.calls, child)
			}
		case *ast.ForStmt, *ast.RangeStmt:
			if valueWalkLoopPush(pass, n) {
				node.push = true
			}
		}
		return true
	})
}

func valueWalkLTypeComparison(pass *analysis.Pass, cond ast.Expr) bool {
	found := false
	ast.Inspect(cond, func(n ast.Node) bool {
		if _, ok := n.(*ast.FuncLit); ok {
			return false
		}
		binary, ok := n.(*ast.BinaryExpr)
		if !ok {
			return true
		}
		switch binary.Op {
		case token.EQL, token.NEQ, token.LSS, token.LEQ, token.GTR, token.GEQ:
			if isLispNamed(pass.TypesInfo.TypeOf(binary.X), "LType") ||
				isLispNamed(pass.TypesInfo.TypeOf(binary.Y), "LType") {
				found = true
			}
		default:
			return true
		}
		return !found
	})
	return found
}

func valueWalkRecursive(start, current *valueWalkFunc, seen map[*valueWalkFunc]bool) bool {
	if seen[current] {
		return false
	}
	seen[current] = true
	for _, child := range current.calls {
		if child == start || valueWalkRecursive(start, child, seen) {
			return true
		}
	}
	return false
}

func valueWalkLoopPush(pass *analysis.Pass, loop ast.Node) bool {
	push := false
	ast.Inspect(loop, func(n ast.Node) bool {
		if _, ok := n.(*ast.FuncLit); ok {
			return false
		}
		switch n := n.(type) {
		case *ast.CallExpr:
			id, ok := ast.Unparen(n.Fun).(*ast.Ident)
			if !ok {
				break
			}
			builtin, ok := pass.TypesInfo.Uses[id].(*types.Builtin)
			if ok && builtin.Name() == "append" && len(n.Args) > 1 && valueWalkStackType(pass.TypesInfo.TypeOf(n.Args[0])) {
				push = true
			}
		case *ast.AssignStmt:
			if n.Tok != token.ASSIGN {
				break
			}
			for _, lhs := range n.Lhs {
				if index, ok := ast.Unparen(lhs).(*ast.IndexExpr); ok && valueWalkStackType(pass.TypesInfo.TypeOf(index.X)) {
					push = true
				}
			}
		}
		return true
	})
	return push
}

func valueWalkStackType(t types.Type) bool {
	if t == nil {
		return false
	}
	slice, ok := t.Underlying().(*types.Slice)
	if !ok {
		return false
	}
	if isLValPtr(slice.Elem()) {
		return true
	}
	if st, ok := slice.Elem().Underlying().(*types.Struct); ok {
		for i := range st.NumFields() {
			if isLValPtr(st.Field(i).Type()) {
				return true
			}
		}
	}
	return false
}
