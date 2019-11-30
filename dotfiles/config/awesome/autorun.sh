#!/usr/bin/env bash


$HOME/.scripts/monitor.sh &
$HOME/.scripts/keyboard.sh &

if (! pgrep thunar); then
    thunar --daemon &
fi

if (! pgrep daemon); then
    emacs --daemon &
fi
if (! pgrep urxvtd); then
    urxvtd -q -o -f
fi
if (! pgrep feh); then
    feh --bg-scale ~/.wallpapers/girl-anime.jpg &
fi

if (! pgrep compton); then
    compton --shadow-exclude '!focused' &
fi

if (! pgrep xautolock); then
    xautolock -time 7 -locker lock &
fi

if [ -f ~/.scripts/autostart_work.sh ]; then
  ~/.scripts/autostart_work.sh
fi
