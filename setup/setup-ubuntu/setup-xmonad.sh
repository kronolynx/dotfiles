#!/bin/bash

find -type f -iname "*.sh" -exec chmod +x {} \;

$PWD/000-base.sh
$PWD/010-xmonad-desktop.sh
$PWD/020-display-manager.sh

