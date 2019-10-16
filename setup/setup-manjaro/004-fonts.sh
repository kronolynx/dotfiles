#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls

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

$SCRIPTPATH/helpers/pprint.sh "Installing fonts" 
$SCRIPTPATH/helpers/install-app.sh ${apps[*]}
