#!/bin/bash

chmod +x *.sh
chmod +x apps/*.sh

sudo apt update && sudo apt upgrade

./000-base-apps.sh
./001-base-apps-ppa.sh
./002-base-apps-deb.sh
./003-base-apps-github.sh
./004-base-apps-snap.sh
./010-display-manager.sh
./020-install-xmonad.sh
#./030-coding-env.sh
