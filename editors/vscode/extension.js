// ELPS Debug Adapter Extension for VS Code
//
// This extension registers a DebugAdapterDescriptorFactory that spawns
// `elps debug --stdio` as a child process, connecting VS Code's DAP
// client to the ELPS debugger.

const vscode = require("vscode");

class ElpsDebugAdapterFactory {
  createDebugAdapterDescriptor(session) {
    const config = session.configuration;
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
