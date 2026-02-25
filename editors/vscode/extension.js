// ELPS Debug Adapter Extension for VS Code
//
// This extension registers a DebugAdapterDescriptorFactory that handles
// two modes:
// - launch: spawns `elps debug --stdio` as a child process
// - attach: connects to a running DAP server over TCP

const vscode = require("vscode");

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
    const elpsPath = config.elpsPath || "elps";
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

function activate(context) {
  const factory = new ElpsDebugAdapterFactory();
  context.subscriptions.push(
    vscode.debug.registerDebugAdapterDescriptorFactory("elps", factory)
  );
}

function deactivate() {}

module.exports = { activate, deactivate };
