#!/bin/sh

case "$DESKTOP_SESSION" in
    xfce)
        LOCK=("xflock4")
        LOGOUT=("xfce4-session-logout" "--logout")
        SUSPEND=("xfce4-session-logout" "--suspend")
        HiBERNATE=("xfce4-session-logout" "--hibernate")
        REBOOT=("xfce4-session-logout" "--reboot")
        SHUTDOWN=("xfce4-session-logout" "--halt")
        ;;
    xmonad*)
        LOCK=("~/.local/bin/lock")
        LOGOUT=("pkill" "-u" "$USER")
        SUSPEND=("~/.local/bin/lock" "&&" "systemctl" "suspend")
        HiBERNATE=("lock" "&&" "systemctl" "hibernate")
        REBOOT=("systemctl" "reboot")
        SHUTDOWN=("systemctl" "poweroff")
        ;;
    *)
        echo "Unknown commands for $DESKTOP_SESSION"
        exit 2
        
esac


case "$1" in
    lock)
        "${LOCK[@]}"
        ;;
    screenoff)
        ~/.local/bin/lock && xset dpms force off
        ;;
    logout)
        "${LOGOUT[@]}"
        ;;
    suspend)
        "${SUSPEND[@]}"
        ;;
    hibernate)
        "${HIBERNATE[@]}"
        ;;
    reboot)
        "${REBOOT[@]}"
        ;;
    shutdown)
        "${SHUTDOWN[@]}"
        ;;
    *)
        echo "Usage: $0 {lock|logout|suspend|hibernate|reboot|shutdown}"
        exit 2
esac

exit 0
