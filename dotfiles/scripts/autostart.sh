#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

#(sleep 2; run $HOME/.config/polybar/launch.sh) &
compton --config $HOME/.config/compton/compton.conf &
run compton &
run xfce4-power-manager &
run clipit &
run dropbox &
