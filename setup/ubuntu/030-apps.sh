#!/bin/bash

accessories=(
  cmatrix # simulates the display from "The Matrix"
  galculator # scientific calculator
)

development=(
  emacs # GNU Emacs editor
  git # version control system
  highlight # Universal source code to formatted text converter
  entr # Run arbitrary commands when files change
  silversearcher-ag #  very fast grep-like program, alternative to ack-grep
)

education=(

)

graphics=(
  gimp # GNU Image Manipulation Program
  inkscape # vector-based drawing program
  nomacs # image viewer with capability of syncing multiple instances
)

internet=(
  chromium-browser # Chromium web browser, open-source version of Chrome
)

multimedia=(
  cmus # Terminal music player
  mkchromecast # Cast Audio/Video to your Google Cast and Sonos Devices
  mps-youtube # Terminal based YouTube jukebox with playlist management
  mpv # video player based on MPlayer/mplayer2
  openshot # create and edit videos and movies
  simplescreenrecorder # Feature rich screen recorder
  smplayer # Complete front-end for MPlayer and mpv
  vlc # multimedia player and streamer
  youtube-dl # downloader of videos from YouTube and other sites
)

office=(
  evince # Document (PostScript, PDF) viewer
  libreOffice # office productivity suite
)

other=(
  ansiweather # Weather in your terminal, with ANSI colors and Unicode symbols
  qalc # calculator for the terminal  command (qalc)
  ranger # console file manager
  teensy-loader-cli # Ergodox flashing
)

system=(
  arand # front end for Xrandr (screen related)
  atool # tool for managing file archives of various types
  build-essential # base devel
  caca-utils # text mode graphics utilities
  catfish # File searching tool which is configurable via the command line
  cups # printing system
  curl # URL retrieval utility
  dconf-editor # simple configuration storage system - graphical editor
  dmidecode # SMBIOS/DMI table decoder
  fasd # quick access to files and directories
  fdupes # duplicate file finder
  ffmpegthumbnailer # fast and lightweight video thumbnailer
  glances # Curses-based monitoring tool
  gparted # partition editor
  hardinfo # Displays system information
  hddtemp # hard drive temperature monitoring utility
  htop #  interactive process viewer
  inxi # full featured system information script
  libpam-u2f # yubico U2F (2 factor authentication)
  lm-sensors # utilities to read temperature/voltage/fan sensors
  mediainfo # command-line utility for reading information from audio/video files
  mlocate # quickly find files on the filesystem based on their name
  ncdu # disk usage analizer
  neofetch # system info
  net-tools # NET-3 networking toolkit
  openssl # secure sockets layer (security)
  p7zip # zip
  poppler-utils # PDF utilities (based on Poppler)
  pulseaudio # sound server
  qt5ct # Qt5 Configuration Utility
  redshift # Adjusts the color temperature of your screen
  rxvt-unicode # terminal emulator
  sane # scanner graphical frontends
  simple-scan # Simple Scanning Utility
  sysstat # system performance tools for Linux
  tldr # to display examples in man pages e.g. tldr cups
  trash-cli # command line trashcan
  tumbler # D-Bus thumbnailing service
  unclutter # hides the mouse cursor in X after a period of inactivity
  unoconv # converter between LibreOffice document formats
  vnstat # console-based network traffic monitor
  w3m
  wget # URL retrieval utility
  xarchiver
  xbacklight # RandR-based backlight control
  xdg-user-dirs # tool to manage well known user directories
  xserver-xorg-input-all # fix keyboard not working
  xserver-xorg-input-synaptics # touchpad
  yad # tool for creating graphical dialogs from shell scripts
  zenity # Display graphical dialog boxes from shell scripts
)

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls
COMMON="$(dirname $SCRIPTPATH)/common"

$COMMON/helpers/pprint.sh "accessories"
$SCRIPTPATH/helpers/install-app.sh ${accessories[*]}

$COMMON/helpers/pprint.sh "development" 
$SCRIPTPATH/helpers/install-app.sh ${development[*]}

$COMMON/helpers/pprint.sh "education" 
$SCRIPTPATH/helpers/install-app.sh ${education[*]}

$COMMON/helpers/pprint.sh "graphics"
$SCRIPTPATH/helpers/install-app.sh ${graphics[*]}

$COMMON/helpers/pprint.sh "internet"
$SCRIPTPATH/helpers/install-app.sh ${internet[*]}

$COMMON/helpers/pprint.sh "multimedia"
$SCRIPTPATH/helpers/install-app.sh ${multimedia[*]}

$COMMON/helpers/pprint.sh "office"
$SCRIPTPATH/helpers/install-app.sh ${office[*]}

$COMMON/helpers/pprint.sh "other"
$SCRIPTPATH/helpers/install-app.sh ${other[*]}

$COMMON/helpers/pprint.sh "system"
$SCRIPTPATH/helpers/install-app.sh ${system[*]}
