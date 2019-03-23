#!/bin/bash

chmod +x *.sh
chmod +x apps/*.sh


sudo apt update && sudo apt upgrade
./000-base-apps.sh
./001-install-xmonad.sh
#./002-coding-env.sh
./003-cli-apps.sh
