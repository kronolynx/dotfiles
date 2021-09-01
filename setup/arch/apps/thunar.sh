#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls

apps=(
  thunar
  thunar-volman
  thunar-media-tags-plugin
  thunar-archive-plugin
  file-roller
  xarchiver
  # for thunar thumbnails
  tumbler
  tumbler-extra-thumbnailers
  # Video thumbnails
  ffmpegthumbnailer
  # PDF thumbnails
  poppler-glib
  libgsf
  gvfs
  catfish
  mlocate
  # export files to pdf
  # installs libre office fresh
  unoconv
  # subtitles
  periscope
  # display messages
  zenity
)

$SCRIPTPATH/../helpers/install-app.sh ${apps[*]}
