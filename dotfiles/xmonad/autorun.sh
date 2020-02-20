#!/usr/bin/env bash

## run (only once) processes which spawn with the same name
function run {
   if (command -v $1 && ! pgrep $1); then
     $@&
   fi
}

## run (only once) processes which spawn with different name
if (command -v gnome-keyring-daemon && ! pgrep gnome-keyring-d); then
    gnome-keyring-daemon --daemonize --login &
fi
if (command -v start-pulseaudio-x11 && ! pgrep pulseaudio); then
    start-pulseaudio-x11 &
fi
if (command -v /usr/lib/mate-polkit/polkit-mate-authentication-agent-1 && ! pgrep polkit-mate-aut) ; then
    /usr/lib/mate-polkit/polkit-mate-authentication-agent-1 &
fi
if (command -v  xfce4-power-manager && ! pgrep xfce4-power-man) ; then
    xfce4-power-manager &
fi


if ! pgrep "trayer" > /dev/null
then
  trayer --edge top --align right --widthtype request --expand true --SetDockType true --SetPartialStrut false --transparent true --alpha 0 --tint 0x1D1F28 --expand true --heighttype pixel --height 22 --monitor 0 --padding 1 --distance 4 --distancefrom top &
fi

run thunar --daemon
run urxvtd -q -o -f
run feh --bg-scale ~/.wallpapers/girl-anime.jpg

run compton --shadow-exclude '!focused'

# set x cursor
# https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Setting_the_X_cursor
run xsetroot -cursor_name left_ptr

$HOME/.scripts/keyboard.sh &
$HOME/.scripts/monitor.sh &

if [ -f ~/.scripts/autostart_work.sh ]; then
  ~/.scripts/autostart_work.sh
fi

run xautolock -time 7 -locker lock
