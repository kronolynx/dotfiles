#!/bin/bash
intern=eDP1
extern=HDMI1

if xrandr | grep "$extern disconnected"; then
    xrandr --output "$extern" --off --output "$intern" --auto
	xbacklight = 30
else
	xrandr --output "$extern"  --mode 1920x1080 --pos 0x0 --rotate normal --output eDP1 --primary --mode 1920x1080 --pos 1920x0 --rotate normal

	xbacklight = 60
fi
