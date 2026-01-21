# Nix Workspace Manager (Racket Edition)

The **Nix Workspace Manager** is a GUI tool built in Racket designed to interactively create, manage, and generate Nix configuration files. It provides a modern, user-friendly interface to manage Nix expressions, simplifying the process for both beginners and experts.

## Project Goals

1.  **GUI-Driven Management**: Manage Nix configuration files (`default.nix`, `flake.nix`, etc.) primarily through a Graphical User Interface.
2.  **Beginner Accessibility**: Abstract away complex syntax and provide a guided editing environment.
3.  **Advanced Search**: Built-in search feature for `nixpkgs` packages and NixOS options.
4.  **Interactive "Structured" Editing**: An edit view that allows manual editing while keeping the graphical AST structure perfectly in sync.

## The Nix Editor GUI

The Nix Editor provides a comprehensive environment for managing Nix expressions:

*   **Action Toolbar**: A streamlined command center for global actions:
    *   **Undo**: Quickly revert the last change.
    *   **Path Display**: Shows the currently selected node's path.
    *   **Search Toggle**: Show or hide the advanced search panel.
*   **Structure Tree (Left Pane)**: A text-based interactive tree view with a powerful context menu.
    *   **Context Menu (Right-Click)**: Access structural manipulations directly on nodes:
        *   **Add Child**: Add new Sets, Lists, or Scalar values.
        *   **Update**: Rename keys or modify values.
        *   **Wrap/Unwrap Scope**: Manage `let ... in` blocks.
        *   **Wrap Lambda**: Wrap expressions in functions.
        *   **Comment & Delete**: Metadata management and node removal.
    *   Visualizes the hierarchy and supports folding/unfolding.
    *   Synchronizes instantly with the code editors.*   **Whole Source View (Right Top)**: A full-featured dark-mode code editor.
    *   Displays the complete compiled Nix file.
    *   Supports manual editing with automatic background parsing and tree updates.
    *   Real-time synchronization: clicking or moving the cursor in the code automatically selects the corresponding node in the tree.
*   **Node Source View (Right Bottom)**: A focused, read-only preview of the Nix code for the currently selected tree node.

## Prerequisites

*   **Racket**: Ensure it is installed and available in your PATH.
*   **Nix (Optional)**: For a reproducible development environment and package management.

## Development Environment (Nix)

To enable arrow key navigation and history for the CLI component, this project utilizes the `readline` library (specifically `libedit`). We provide a Nix environment that sets this up automatically.

**Using Nix Flakes (Recommended):**
```bash
nix develop
```

**Using Legacy Nix:**
```bash
nix-shell
```

## Getting Started

### Running the GUI
The main way to use the application is through the GUI:
```bash
racket gui.rkt
```

### Running the CLI
For a terminal-based structured editing experience:
```bash
racket interpreter.rkt
```

## Architecture

*   **`gui.rkt`**: Main window initialization and layout management.
*   **`manager.rkt`**: Core state engine and AST traversal.
*   **`compiler.rkt`**: Nix code generation with source-mapping support.
*   **`parser.rkt`**: Recursive-descent parser for real-time manual edit support.
*   **`actions.rkt`**: Toolbar handlers and UI synchronization logic.
*   **`theme.rkt`**: Centralized styling and dark mode configuration.
*   **`interpreter.rkt`**: Legacy CLI entry point; implements the REPL and parses user commands.

## CLI Usage

If using the `interpreter.rkt` CLI, the following commands are available:

### Navigation
*   `ls`: List children.
*   `cat`: Print Nix source of current node.
*   `cd <path>`: Navigate into a key/index.
*   `back` / `..`: Go up.
*   `top` / `/`: Go to root.

### Editing
*   `mkset <key>`: Create a Set.
*   `mklist <key>`: Create a List.
*   `mklet [key]`: Create `let ... in`.
*   `set <key> <value>`: Set value (e.g., `set foo "bar"`).
*   `push <value>`: Append to list.
*   `rm <key>`: Delete key/index.
*   `comment <text>`: Add comment.

### File Management
*   `edit <filename>`: Open/create buffer.
*   `save`: Save buffer.
*   `ls files`: List buffers.
