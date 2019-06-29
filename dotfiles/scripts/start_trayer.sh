#!/bin/sh

if pgrep "trayer" > /dev/null
then
   killall trayer
   trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request --transparent true --alpha 0 --tint 0x000000 --height 22 --padding 2
   #trayer --edge top --align right --widthtype request --expand true --SetDockType true --SetPartialStrut true --transparent true --alpha 0 --tint 0x1A1918 --heighttype pixel --height 23 --monitor 0 --padding 1
fi
