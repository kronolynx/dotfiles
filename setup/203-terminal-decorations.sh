#!/bin/bash
apps=(
# pipes bash screen saver
bash-pipes
# matrix screen saver
cmatrix
# asciiiquarium screen saver
asciiquarium
)

for app in ${apps[*]}; do
    ./install-app.sh $app
done
