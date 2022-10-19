#!/bin/bash

mkdir -p ~/.local/bin
export COURSIER_BIN_DIR=~/.local/bin/

curl -fLo cs https://git.io/coursier-cli-"$(uname | tr LD ld)"

chmod +x cs
./cs install cs ammonite bloop giter8 sbt scala scalafmt
./cs java --jvm adoptium:17 --setup

mkdir -p ~/.zsh/completion
cs --completions zsh > ~/.zsh/completion/cs
echo 'fpath=(~/.zsh/completion $fpath)' >> ~/.zshrc
echo 'autoload -Uz compinit ; compinit' >> ~/.zshrc

rm cs
