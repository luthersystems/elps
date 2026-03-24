// ELPS Extension for VS Code
//
// Provides:
// - LSP client: spawns `elps lsp --stdio` for language features
// - DAP client: spawns `elps debug --stdio` or attaches to a running DAP server

const vscode = require("vscode");
const fs = require("fs");
const path = require("path");
const os = require("os");
const { LanguageClient } = require("vscode-languageclient/node");

let client = null;

// --- Helpers ---

// Resolve the elps binary path. If the user has set elps.path to something
// other than the default "elps", use that. Otherwise, search common Go
// install locations since VS Code on macOS doesn't inherit shell PATH.
function getElpsPath() {
  const configured = vscode.workspace.getConfiguration("elps").get("path", "elps");
  if (configured !== "elps") {
    return configured;
  }

  const home = os.homedir();
  const candidates = [
    path.join(home, "go", "bin", "elps"),
    path.join(home, ".local", "bin", "elps"),
    "/usr/local/bin/elps",
    "/opt/homebrew/bin/elps",
  ];

  // Also check GOPATH/bin and GOBIN if set in the process environment.
  if (process.env.GOBIN) {
    candidates.unshift(path.join(process.env.GOBIN, "elps"));
  } else if (process.env.GOPATH) {
    candidates.unshift(path.join(process.env.GOPATH, "bin", "elps"));
  }

  for (const candidate of candidates) {
    try {
      fs.accessSync(candidate, fs.constants.X_OK);
      return candidate;
    } catch {
      // not found, try next
    }
  }

  // Fall back to bare "elps" and hope it's on PATH.
  return "elps";
}

// --- DAP ---

class ElpsDebugAdapterFactory {
  createDebugAdapterDescriptor(session) {
    const config = session.configuration;

    // Attach mode: connect to an already-running DAP server over TCP.
    if (config.request === "attach") {
      const host = config.host || "localhost";
      const port = config.port || 4711;
      return new vscode.DebugAdapterServer(port, host);
    }

    // Launch mode: spawn the elps binary as a child process.
    const elpsPath = config.elpsPath || getElpsPath();
    const args = ["debug", "--stdio"];

    if (config.stopOnEntry) {
      args.push("--stop-on-entry");
    }
    if (config.rootDir) {
      args.push("--root-dir", config.rootDir);
    }

    args.push(config.program);

    return new vscode.DebugAdapterExecutable(elpsPath, args);
  }
}

// --- LSP ---

async function startLSP(context) {
  const config = vscode.workspace.getConfiguration("elps");
  if (!config.get("lsp.enable", true)) {
    return;
  }

  const elpsPath = getElpsPath();

  // elps lsp defaults to stdio, so --stdio is not needed.
  const serverOptions = {
    run: { command: elpsPath, args: ["lsp"] },
    debug: { command: elpsPath, args: ["lsp"] },
  };

  const clientOptions = {
    documentSelector: [{ scheme: "file", language: "elps" }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.lisp"),
    },
    outputChannelName: "ELPS Language Server",
  };

  client = new LanguageClient(
    "elps",
    "ELPS Language Server",
    serverOptions,
    clientOptions,
  );

  try {
    await client.start();
  } catch (err) {
    vscode.window
      .showWarningMessage(
        `ELPS language server failed to start. Is '${elpsPath}' installed? (${err.message})`,
        "Open Settings",
      )
      .then((choice) => {
        if (choice === "Open Settings") {
          vscode.commands.executeCommand(
            "workbench.action.openSettings",
            "elps.path",
          );
        }
      });
  }
}

async function stopLSP() {
  if (client) {
    await client.stop();
    client = null;
  }
}

// --- Activation ---

async function activate(context) {
  // DAP
  context.subscriptions.push(
    vscode.debug.registerDebugAdapterDescriptorFactory(
      "elps",
      new ElpsDebugAdapterFactory(),
    ),
  );

  // LSP
  await startLSP(context);

  // Restart LSP when relevant settings change.
  context.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration(async (e) => {
      if (
        e.affectsConfiguration("elps.path") ||
        e.affectsConfiguration("elps.lsp.enable")
      ) {
        await stopLSP();
        await startLSP(context);
      }
    }),
  );
}

async function deactivate() {
  await stopLSP();
}

module.exports = { activate, deactivate };
