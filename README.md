## Emacs config

My personal Emacs configuration. Clone into `~/.emacs.d/`.

### Structure

Single-file config in `init.el` using `use-package`.

### Setup

Run the setup script to install all dependencies:

```
./setup.sh
```

Fonts and tree-sitter grammars are installed automatically on first launch. Run `M-x marcus-check-dependencies` to see any missing dependencies.
