#!/bin/bash
if synclient -l | grep "TouchpadOff .*=.*0" ; then
    synclient TouchpadOff=1 ;
    notify-send -u low -i mouse "Trackpad disabled"
else
    synclient TouchpadOff=0 ;
    notify-send -u low -i mouse "Trackpad enabled"
fi
