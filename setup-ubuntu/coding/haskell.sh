#!/bin/sh

add_haskell_tools() {
  curl -sSL https://get.haskellstack.org/ | sh # installs stack
  stack install hfmt
}

add_haskell_tools
