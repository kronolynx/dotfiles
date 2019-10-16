#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls

apps=(
# pipes bash screen saver
bash-pipes
# matrix screen saver
cmatrix
# asciiiquarium screen saver
asciiquarium
)

$SCRIPTPATH/helpers/pprint.sh "Installing terminal decorations" 
$SCRIPTPATH/helpers/install-app.sh ${apps[*]}
