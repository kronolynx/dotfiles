#!/bin/bash

apps=(
thunar
thunar-volman
thunar-media-tags-plugin
thunar-archive-plugin
file-roller
xarchiver
# for thunar thumbnails
tumbler
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

../install-app.sh ${apps[*]}
