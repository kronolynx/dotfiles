#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls
COMMON="$(dirname $(dirname $SCRIPTPATH))/common"

install_app() {
  package=$1
  #----------------------------------------------------------------------------------
  
  # checking if application is already installed or else install with aur helpers
  if sudo snap install $package &> /dev/null; then
    
    $COMMON/helpers/pprint.sh "$package is already installed" "yellow"
    
  else
    $COMMON/helpers/pprint.sh "Can't install $package, Error: snap not available" "red"
  fi
}

for app in ${@}; do
  install_app $app
done
