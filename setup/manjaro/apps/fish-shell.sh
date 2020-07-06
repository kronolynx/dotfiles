#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls
COMMON="$(dirname $(dirname $SCRIPTPATH))/common"

apps=(
  fish
  fisherman
)

$COMMON/helpers/install-app.sh ${apps[*]}

# run fisherman to install plugins
fish
fisher
