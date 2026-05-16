## Emacs config

My personal Emacs configuration. Clone into `~/.emacs.d/`.

### Structure

Single-file config in `init.el` using `use-package`.

### Setup

Run the setup script to install all dependencies:

```
./setup.sh
```

After first launch, run in Emacs:

- `M-x all-the-icons-install-fonts`
- `M-x treesit-install-language-grammar` (for each language)

Or run `M-x marcus-check-dependencies` to see what's missing.
