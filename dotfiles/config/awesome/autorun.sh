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

run thunar --daemon
run emacs --daemon
run urxvtd -q -o -f
run feh --bg-scale ~/.wallpapers/girl-anime.jpg

run compton --shadow-exclude '!focused'

$HOME/.scripts/keyboard.sh &
$HOME/.scripts/monitor.sh &

if [ -f ~/.scripts/autostart_work.sh ]; then
  ~/.scripts/autostart_work.sh
fi

run xautolock -time 7 -locker lock
