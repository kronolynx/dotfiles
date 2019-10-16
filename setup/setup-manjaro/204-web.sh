#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls

apps=(
  # browser
  vivaldi
  vivaldi-codecs-ffmpeg-extra-bin
  vivaldi-widevine
  # google-chrome
  google-chrome
  # chromium
  chromium
  # firefox
  firefox
  # mega sync
  megasync
  # keepassxc
  keepassxc
)

$SCRIPTPATH/helpers/pprint.sh "Installing web" 
$SCRIPTPATH/helpers/install-app.sh ${apps[*]}