#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls

debs=(
  # add here links to direct downloads to .debs to install
  https://www.dropbox.com/download?dl=packages/ubuntu/dropbox_2019.02.14_amd64.deb
)

$SCRIPTPATH/helpers/pprint.sh "Installing debs" 
$SCRIPTPATH/helpers/install_deb.sh ${deb[*]}
