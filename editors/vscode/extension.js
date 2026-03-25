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
let extensionContext = null;

// --- Helpers ---

// Resolve the elps binary path. Priority:
// 1. User-configured elps.path setting (if changed from default)
// 2. Bundled binary in the extension's bin/ directory (platform-specific packages)
// 3. Common Go install locations (GOBIN, GOPATH, ~/go/bin, etc.)
// 4. Bare "elps" on PATH
function getElpsPath() {
  const configured = vscode.workspace.getConfiguration("elps").get("path", "elps");
  if (configured !== "elps") {
    return configured;
  }

  // Bundled binary (platform-specific extension package).
  if (extensionContext) {
    const ext = process.platform === "win32" ? ".exe" : "";
    const bundled = path.join(extensionContext.extensionPath, "bin", "elps" + ext);
    try {
      fs.accessSync(bundled, fs.constants.X_OK);
      return bundled;
    } catch {
      // Not bundled (universal package) — fall through to auto-discovery.
    }
  }

  // Auto-discovery in common Go install locations.
  const home = os.homedir();
  const ext = process.platform === "win32" ? ".exe" : "";
  const candidates = [
    path.join(home, "go", "bin", "elps" + ext),
    path.join(home, ".local", "bin", "elps" + ext),
    "/usr/local/bin/elps",
    "/opt/homebrew/bin/elps",
  ];

  if (process.env.GOBIN) {
    candidates.unshift(path.join(process.env.GOBIN, "elps" + ext));
  } else if (process.env.GOPATH) {
    candidates.unshift(path.join(process.env.GOPATH, "bin", "elps" + ext));
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
  extensionContext = context;

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
