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
        xrandr --output HDMI-0 --off --output DP-0 --mode 1920x1080 --pos 0x0 --rotate normal --output DP-1 --off --output DP-2 --off --output DP-3 --off --output DP-4 --off
        xbacklight = 2
esac
