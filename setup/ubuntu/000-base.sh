#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls
COMMON="$(dirname $SCRIPTPATH)/common"

apps=(
  neovim
  software-properties-common
  git # version control system
  firefox
  build-essential # base devel
  curl # URL retrieval utility
  net-tools # NET-3 networking toolkit
  openssl # secure sockets layer (security)
  p7zip # zip
  rxvt-unicode # terminal emulator
  ubuntu-drivers-common
  wget # URL retrieval utility
)

$COMMON/helpers/pprint.sh "Installing base apps" 
$SCRIPTPATH/helpers/install-app.sh ${apps[*]}
