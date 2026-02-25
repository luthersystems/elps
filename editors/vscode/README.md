# ELPS Debug — VS Code Extension

A minimal VS Code debug adapter extension for the [ELPS](https://github.com/luthersystems/elps) Lisp interpreter.

## Prerequisites

Build the `elps` binary and ensure it's on your PATH:

```bash
cd /path/to/elps
make
export PATH="$PWD:$PATH"
```

## Install

From the `editors/vscode/` directory:

```bash
# Option 1: Symlink into VS Code extensions directory
ln -s "$PWD" ~/.vscode/extensions/elps-debug

# Option 2: Package and install with vsce
npx @vscode/vsce package
code --install-extension elps-debug-0.1.0.vsix
```

## Usage

1. Open a `.lisp` file in VS Code.
2. Create a `.vscode/launch.json` (or use the auto-generated snippet):

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "type": "elps",
      "request": "launch",
      "name": "Debug ELPS",
      "program": "${file}",
      "stopOnEntry": true
    }
  ]
}
```

3. Set breakpoints by clicking the gutter.
4. Press F5 to start debugging.

## Configuration

| Attribute     | Type    | Default              | Description                              |
|---------------|---------|----------------------|------------------------------------------|
| `program`     | string  | `${file}`            | Path to the `.lisp` file to debug        |
| `stopOnEntry` | boolean | `true`               | Pause before the first expression        |
| `rootDir`     | string  | `${workspaceFolder}` | Source root for file resolution           |
| `elpsPath`    | string  | `"elps"`             | Path to the `elps` binary                |

## Features

- Line breakpoints
- Conditional breakpoints (Lisp expressions)
- Hit count breakpoints (`>N`, `>=N`, `==N`, `%N`)
- Log points (message templates with `{expr}` interpolation)
- Function breakpoints
- Exception breakpoints
- Step into / over / out
- Variable inspection (local and package scopes)
- Watch expressions
- Debug console evaluation
- Pause / continue
