# Emacs configuration

## Overview

Personal Emacs config using `use-package`, tree-sitter, and eglot for LSP.

## Key files

- `init.el` — single-file configuration

## Language servers

- Python: `basedpyright-langserver` (installed via `uv`)
- TypeScript/JavaScript: `typescript-language-server` (installed via `npm`)
- Bash: `bash-language-server`
- Dockerfile: `docker-langserver`

## Packages

Installed via `package.el` from MELPA. Run `M-x package-install` or add a `use-package` block to `init.el`.

## Conventions

- All code and strings in `init.el` must be in English
- Functions and variables are prefixed with `marcus-`
