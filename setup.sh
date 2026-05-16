#!/bin/bash

set -e

case "$(uname -s)" in
  Darwin)
    echo "Installing Homebrew packages..."
    brew install ripgrep coreutils
    ;;
  Linux)
    echo "Installing system packages..."
    sudo apt-get update -q
    sudo apt-get install -y ripgrep
    ;;
  *)
    echo "Unsupported OS: $(uname -s)"
    exit 1
    ;;
esac

echo "Installing language servers..."
uv tool install basedpyright
npm install -g typescript-language-server bash-language-server dockerfile-language-server-nodejs

echo "Done. Start Emacs and run M-x marcus-check-dependencies to verify."
