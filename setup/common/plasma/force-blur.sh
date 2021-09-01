#!/bin/bash

git clone https://github.com/esjeon/kwin-forceblur.git
(cd kwin-forceblur

plasmapkg2 --type kwinscript -i .

mkdir -p ~/.local/share/kservices5/
cp ~/.local/share/kwin/scripts/forceblur/metadata.desktop ~/.local/share/kservices5/forceblur.desktop
)
rm -rf kwin-forceblur

