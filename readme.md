
# Flora

PoC for a practical [structural editor](https://en.wikipedia.org/wiki/Structure_editor).

## Installation

See the [Dockerfile](Dockerfile) for the system dependencies required.

## Keybindings

| **Key** | **Function** | **Mode** | **Type** |
|---|---|---|---|
| <kbd>↑</kbd><kbd>↓</kbd><kbd>←</kbd><kbd>→</kbd> | Pan around | Normal | Meta |
| <kbd>Ctrl</kbd><kbd>f</kbd> | Page down | Normal | Meta |
| <kbd>Ctrl</kbd><kbd>b</kbd> | Page up | Normal | Meta |
| <kbd>q</kbd> | Quit | Normal | Meta |
| <kbd>k</kbd> | Parent | Normal | Movement |
| <kbd>j</kbd> | First child | Normal | Movement |
| <kbd>l</kbd> | Next sibling | Normal | Movement |
| <kbd>h</kbd> | Previous sibling | Normal | Movement |
| <kbd>n</kbd> | Next hole | Normal | Movement |
| <kbd>N</kbd> | Previous hole | Normal | Movement |
| <kbd>u</kbd> | Undo | Normal | Editing |
| <kbd>i</kbd> | Insert | Normal | Meta |
| <kbd>Esc</kbd> | Normal | Insert | Meta |
| <kbd>Tab</kbd> / <kbd>Enter</kdb> | Commit completion | Insert | Editing |
| Edit text | Narrow completions | Insert | Editing |

## Design

This is a general-purpose syntax tree editor for _existing_ programming languages.

- Syntax trees are untyped, but are annotated with semantic information to generate and validate completions.
- Language support is ad hoc (i.e. OCaml code taking the AST as input) and compiled into the final executable. Using dynamically-linked plugins is planned. LSP integration is another option.
- Code layout is hand-rolled. Ideally we'd use [existing formatters](https://github.com/ocaml-ppx/ocamlformat).
- Parsing uses available libraries in the ecosystem. [Tree-sitter](https://github.com/tree-sitter/tree-sitter) is a nice language-agnostic parser that might be worth binding to.
- Renders to [Unix terminals](https://github.com/pqwy/notty). Other front-ends are certainly possible.
- The full set of editing operations is still being figured out.
