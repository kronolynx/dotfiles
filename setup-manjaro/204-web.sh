#!/bin/bash
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

./install-app.sh ${apps[*]}
