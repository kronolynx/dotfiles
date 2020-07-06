#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls
COMMON="$(dirname $SCRIPTPATH)/common"

debs=(
  # add here links to direct downloads to .debs to install
  https://www.dropbox.com/download?dl=packages/ubuntu/dropbox_2019.02.14_amd64.deb
  https://download.opensuse.org/repositories/home:/manuelschneid3r/xUbuntu_19.04/amd64/albert_0.16.1_amd64.deb
)

$COMMON/helpers/pprint.sh "Installing debs" 
$SCRIPTPATH/helpers/install-deb.sh ${debs[*]}
