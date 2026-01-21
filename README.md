# Nix Workspace Manager (Racket Edition)

Nix Workspace Manager is a CLI tool built in Racket designed to interactively create, manage, and generate Nix configuration files. It functions as a structured editor, allowing users to navigate and manipulate an Abstract Syntax Tree (AST) representing Nix expressions through a shell-like interface.

## Features

*   **Interactive REPL**: A shell interface to navigate the configuration structure.
*   **AST Manipulation**: Create Nix sets, lists, let bindings, and comments dynamically.
*   **Buffer Management**: Edit multiple Nix files simultaneously within a session.
*   **Code Generation**: Compiles the internal AST into valid Nix syntax.
*   **Undo History**: Supports undoing changes to the state.

## Prerequisites

*   Racket (ensure it is installed and available in your PATH).
*   **Optional**: Nix (for a reproducible development environment).

## Development Environment (Nix)

To enable arrow key navigation and history, this project requires the `readline` library (specifically `libedit`). We provide a Nix environment that sets this up automatically.

**Using Nix Flakes (Recommended):**
```bash
nix develop
```

**Using Legacy Nix:**
```bash
nix-shell
```

Once inside the environment, you can run the interpreter with full line-editing support.

## Running the Application

To ensure full compatibility with the development environment (including arrow key support and autocompletion), run the application directly using the Racket source:

```bash
racket interpreter.rkt
```

> **Note:** Running the source inside the Nix environment (`nix develop` or `nix-shell`) is the recommended way to use this tool, as it correctly handles the `libedit` dependencies for line editing.

## Development Environment (Nix)

## Usage

Once inside the REPL, the following commands are available:

### Navigation

*   `ls`: List children (keys/indices) of the current node.
*   `cat`: Print the Nix source code of the current node.
*   `cd <path>`: Navigate into a key or index.
*   `back` or `..`: Go up one level.
*   `top` or `/`: Go to the root of the document.

### Editing

*   `mkset <key>`: Create a new Set at the specified key.
*   `mklist <key>`: Create a new List at the specified key.
*   `mklet [key]`: Create a `let ... in ...` expression (virtual directories: `bindings`, `body`). If the key is omitted, it replaces the current node.
*   `set <key> <value>`: Set a value. Quotes matter: `"foo"` is a string, `foo` is a raw identifier, `123` is a number, `true` is a boolean.
*   `push <value>`: Append a value to a list (follows same typing rules as `set`).
*   `rm <key>`: Delete a key from a Set or an index from a List.
*   `comment <text>`: Add a comment to the current node.

### File Management

*   `edit <filename>`: Switch to or open a new buffer (creates file if not exists).
*   `set-root <set|list>`: Reset the current buffer's root to an empty Set or List.
*   `save`: Save the current buffer to disk.
*   `save as <filename>`: Save as a new file.
*   `ls files`: List open buffers.
*   `clear`: Wipe the current state (same as `set-root set`).

### Templates

*   `init flake`: Scaffold a basic Nix flake.
*   `init shell`: Scaffold a `buildInputs` shell environment.

## Project Structure

*   `interpreter.rkt`: Entry point; implements the REPL and parses user commands.
*   `manager.rkt`: Handles state management, buffer switching, and AST traversal logic.
*   `compiler.rkt`: Converts internal AST structs into formatted Nix source code.
*   `structs.rkt`: Defines Racket structs for Nix language constructs.
