# ELPS DAP for JetBrains IDEs

Requires the [LSP4IJ](https://plugins.jetbrains.com/plugin/23257-lsp4ij) plugin
which includes DAP client support.

## Setup

1. Install the **LSP4IJ** plugin from the JetBrains Marketplace
2. Go to **Settings > Languages & Frameworks > LSP4IJ > DAP**
3. Add a new DAP server configuration:

| Field | Value |
|-------|-------|
| Name | ELPS Debug |
| Command | `elps debug --stdio` |
| File types | `*.lisp`, `*.elps` |

## Launch Configuration

Create a Run Configuration:

1. **Run > Edit Configurations > + > DAP**
2. Set:
   - **DAP Server**: ELPS Debug
   - **Request**: launch
   - **Program**: `$FilePath$`
   - **Stop on Entry**: true

## Attach Configuration

To connect to a running ELPS DAP server:

1. **Run > Edit Configurations > + > DAP**
2. Set:
   - **DAP Server**: ELPS Debug
   - **Request**: attach
   - **Host**: localhost
   - **Port**: 4711

Start the DAP server in your Go application, then run the attach
configuration in the IDE.

## Alternative: External Tool

If LSP4IJ is not available, you can configure ELPS as an External Tool:

1. **Settings > Tools > External Tools > +**
2. Set **Program** to `elps`, **Arguments** to `debug --stdio $FilePath$`
3. Use this for non-DAP debugging (output only, no breakpoints)
