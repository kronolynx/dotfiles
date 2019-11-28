#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls

apps=(
  fish
  fisherman
)

$SCRIPTPATH/../helpers/install-app.sh ${apps[*]}

# run fisherman to install plugins
fish
fisher
