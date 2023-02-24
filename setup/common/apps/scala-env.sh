#!/bin/bash

mkdir -p ~/.local/bin
export COURSIER_BIN_DIR=~/.local/bin/

curl -fL https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz | gzip -d > cs

#curl -fLo cs https://git.io/coursier-cli-"$(uname | tr LD ld)"

chmod +x cs
./cs install cs ammonite bloop giter8 sbt scala scalafmt
./cs java --jvm adoptium:17 --setup

rm cs
