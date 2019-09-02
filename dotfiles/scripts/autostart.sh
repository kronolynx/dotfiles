#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}
if ! pgrep "trayer" > /dev/null
then
  trayer --edge top --align right --widthtype request --expand true --SetDockType true --SetPartialStrut false --transparent true --alpha 0 --tint 0x2F343F --expand true --heighttype pixel --height 24 --monitor 0 --padding 1 --distance 4 --distancefrom top &
fi
compton --config $HOME/.config/compton/compton.conf &

# set x cursor
# https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Setting_the_X_cursor
xsetroot -cursor_name left_ptr
run compton &
run xfce4-power-manager &
run dropbox start &

if [ -f autostart_work.sh ]; then
  ./autostart_work.sh
fi
