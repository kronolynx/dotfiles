#!/bin/bash
hdmi=$(xrandr | grep "HDMI. connected" | cut -f 1 -d " ")

case $hdmi in
    HDMI1)
        xrandr --output "$hdmi"  --mode 1920x1080 --pos 0x0 --rotate normal --output eDP1 --primary --mode 1920x1080 --pos 1920x0 --rotate normal
        xbacklight = 60
        ;;
    HDMI2)
        xrandr --output "$hdmi"  --mode 1920x1080 --pos 1920x0 --rotate normal --output eDP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal
        xbacklight = 60
        ;;
        *)
        xrandr --output HDMI1 --off --output HDMI2 --off --output eDP1 --auto
        xbacklight = 30
esac

