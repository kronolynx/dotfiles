#!/bin/bash
# Get the maximum volume of any pulseaudio sink channel
# amixer get Master | egrep -o "[0-9]+%"
mute=$(amixer -D pulse get Master | awk  '/'Front' 'Left:' 'Playback'/  {print $6}')
vol=$(amixer -D pulse get Master | awk -F'[]%[]' '/%/ {if ($7 == "off") { print "MM" } else { print $2 }}' | head -n 1)

if [ "$vol" -le "30" -a "$mute" = "[on]" ]
	then echo "<fc=blue>奄 $vol% </fc>"
elif [ "$vol" -le "60" -a "$mute" = "[on]" ]
	then echo "<fc=green>奔 $vol% </fc>"
elif [ "$vol" -le "90" -a "$mute" = "[on]" ]
	then echo "<fc=orange>墳 $vol% </fc>"
elif [ "$vol" -gt "90" -a "$mute" = "[on]" ]
	then echo "<fc=red>墳 $vol% </fc>"
elif [ "$mute" = "[off]" ]
	then echo "<fc=grey>婢 $vol% </fc>" 
else
	echo  "<fc=red>墳 $vol% </fc>"
fi

exit 0

