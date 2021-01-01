#!/bin/bash

#stack install hlint

(
git clone https://github.com/haskell/haskell-ide-engine --recursive
cd haskell-ide-engine && ./install.hs hie
)
rm -rf haskell-ide-engine
