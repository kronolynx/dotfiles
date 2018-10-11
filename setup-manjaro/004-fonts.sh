#!/bin/bash

apps=(
noto-fonts-emoji
noto-fonts
nerd-fonts-complete
#./install-app.sh adobe-source-code-pro-fonts
# to select emojis
emojione-picker-git
# patched fonts
nerd-fonts-source-code-pro
otf-fira-code
)

./install-app.sh ${apps[*]}
