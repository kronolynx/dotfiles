#!/bin/bash
apps=(
# google-chrome 
google-chrome
# chromium
chromium
# firefox
firefox
# dropbox
dropbox
# keepassxc
keepassxc
# note taking
boostnote-bin
)

for app in ${apps[*]}; do
    ./install-app.sh $app
done

