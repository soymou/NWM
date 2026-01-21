# Nix Workspace Manager (Racket Edition)

## Project Goals

The core objective of this project is to simplify the management of Nix configuration files through a modern, user-friendly interface. Our primary goals are:

1.  **GUI-Driven Management**: Manage Nix configuration files (`default.nix`, `flake.nix`, etc.) primarily through a robust Graphical User Interface.
2.  **Beginner Accessibility**: Ensure the tool is accessible to Nix beginners by abstracting away complex syntax and providing a guided editing environment.
3.  **Advanced Search**: Provide a built-in search feature that allows users to quickly search for packages in `nixpkgs` or browse available NixOS options.
4.  **Clean and Intuitive Design**: Maintain a clean, logical, and intuitive GUI that prioritizes ease of use and user productivity.
5.  **Interactive "Structured" Editing**: Provide an additional interactive edit view (Whole Source View) that allows users to manually edit Nix files while keeping the graphical AST structure perfectly in sync.
6.  **Code Quality and Modularity**: Prioritize clean, understandable Racket code and a highly modular architecture to ensure the project is easy to maintain and extend.

## The Nix Editor GUI

The Nix Editor provides a comprehensive environment for managing Nix expressions:

*   **Action Toolbar**: A centralized command center for structural manipulations:
    *   **Add Child**: Unified menu to add new Sets, Lists, or Scalar values.
    *   **Update**: Rename keys or modify values via simple dialog popups.
    *   **Wrap/Unwrap Scope**: Easily encapsulate logic in `let ... in` blocks or strip them away.
    *   **Wrap Lambda**: Wrap any expression in a function `{ args }:`.
    *   **Comment & Delete**: Metadata management and safe node removal.
*   **Structure Tree (Left Pane)**: A text-based interactive tree view.
    *   Visualizes the hierarchy of the Nix configuration.
    *   Supports folding/unfolding of containers.
    *   Synchronizes instantly with the code editors.
*   **Whole Source View (Right Top)**: A full-featured dark-mode code editor.
    *   Displays the complete compiled Nix file.
    *   Supports manual editing with automatic background parsing and tree updates.
    *   Real-time synchronization: clicking or moving the cursor in the code automatically selects the corresponding node in the tree.
*   **Node Source View (Right Bottom)**: A focused, read-only preview of the Nix code for the currently selected tree node.

## Architecture

*   **`manager.rkt`**: Core state engine and AST traversal.
*   **`compiler.rkt`**: Nix code generation with source-mapping support.
*   **`parser.rkt`**: Recursive-descent parser for real-time manual edit support.
*   **`actions.rkt`**: Toolbar handlers and UI synchronization logic.
*   **`theme.rkt`**: Centralized styling and dark mode configuration.
*   **`gui.rkt`**: Main window initialization and layout management.

## Getting Started

### Running the GUI
```bash
racket gui.rkt
```

### Running the CLI
```bash
racket interpreter.rkt
```
