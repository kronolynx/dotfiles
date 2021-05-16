#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls
COMMON="$(dirname $SCRIPTPATH)/common"

# make scripts in current directory executable
find $SCRIPTPATH -type f -iname "*.sh" -exec chmod +x {} \;

base=(
  # base devel (make automake, gcc, binutils, fakeroot)
  base-devel
  # version control system
  git
  # URL retrieval utility
  curl
  # secure sockets layer (security)
  #openssl
  zsh
)

media=(
  vlc
  smplayer
  smplayer-skins
  smplayer-themes

  # Cast Audio/Video to your Google Cast and Sonos Devices
  mkchromecast
  # Free radio streaming software with more than 20,000 radio stations
  odio-appimage
  # ebook management
  calibre
)


cli=(
  # to display current song
  playerctl
  # interactive process viewer
  htop
  # vim like file manager
  ranger
  w3m
  ffmpegthumbnailer
  # command like trashcan
  trash-cli
  # dish usage analizer
  ncdu
  # duplicate file finder
  fdupes
  # quick access to files and directories
  fasd
  # interactively kill process
  fkill
  # terminal info
  neofetch
  # fast and user-friendly alternative to find
  fd
  # cat replacement with color syntax
  bat
  # Search tool
  ripgrep
  # command-line fuzzy finder
  fzf
  # run commands when file changes
  entr
  # cross shell prompt
  starship
  # ls replacement
  exa
)

cli_media=(
  # music player
  cmus
  # Terminal based YouTube jukebox with playlist management
  mps-youtube
  # download youtube videos
  youtube-dl
  # Simple screen recorder with an easy to use interface (gif)
  peek
)

misc=(
  chromium
  brave
  google-chrome
  dropbox
  slack-desktop
  # Small commandline tool to configure devices (set elecom buttons)
  # chinese input
  ibus
  ibus-anthy
  ibus-libpinyin
  
  xdotool
)

coding=(
  #jdk8-openjdk
  #coursier
  visual-studio-code-bin
  docker
  docker-compose
  socat
  # # code color higlight
  highlight
  # # Latex
  # # texlive-most
  meld
  # # The Glasgow Haskell Compiler
  # #ghc
  # # The Haskell Tool Stack
  # #stack
  # # git pager
  # git-delta
  
  asbru-cm # connection manager
  #cassandra
  cqlsh
  # amazon web services
  aws-cli
)

install_tmux() {
  $SCRIPTPATH/helpers/install-app.sh tmux

  if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
  fi
}


themes=(
  beautyline
  #adapta-gtk-theme
  #papirus-icon-theme
  noto-fonts               
  noto-fonts-emoji         
  ttf-liberation
  ttf-opensans
  noto-fonts-cjk
  noto-fonts-extra
  asian-fonts
  #nerd-fonts-noto-sans-mono
  ttf-iosevka
  #nerd-fonts-complete
  #gtk-engine-murrine
  #qt5ct
  #kvantum-qt5
)

$SCRIPTPATH/helpers/install-app.sh ${base[*]}
$SCRIPTPATH/helpers/install-app.sh ${cli[*]}
$SCRIPTPATH/helpers/install-app.sh ${cli_media[*]}
$SCRIPTPATH/helpers/install-app.sh ${media[*]}
$SCRIPTPATH/helpers/install-app.sh ${misc[*]}
$SCRIPTPATH/helpers/install-app.sh ${coding[*]}
$SCRIPTPATH/helpers/install-app.sh ${themes[*]}
