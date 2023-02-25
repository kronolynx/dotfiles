#!/bin/bash
sinkName=$(pacmd info | grep 'Default sink' | awk '{print $4}')
sinkNumber=$(pacmd info | grep "sink:.*$sinkName" | awk '{print $2}')

getVolume() {
 volumeInput=$(pactl list sinks)
 currentVolume=$(echo "${volumeInput#*Sink #$sinkNumber}" | grep -E 'V.*-left' | grep -oE '[0-9]+%' | tail -n 1)
 notify-send "Volume: $currentVolume"
}

case "$1" in
 up) pactl set-sink-volume @DEFAULT_SINK@ +5%; getVolume;;
 down) pactl set-sink-volume @DEFAULT_SINK@ -5%; getVolume;;
 toggle) muted=$(pactl list sinks | grep 'Mute: yes')
       if [ -z "$muted" ]; then
        pactl set-sink-mute @DEFAULT_SINK@ 1
        notify-send "Volume: Muted"
       else
        pactl set-sink-mute @DEFAULT_SINK@ 0
        getVolume
       fi;;
 list) getVolume;;
esac

if [[ "$1" =~ [0-9]+ ]]; then
 pactl set-sink-volume @DEFAULT_SINK@ "$1%"; getVolume
fi
