#!/bin/sh

case "$1" in
    lock)
        ~/.local/bin/lock
        ;;
    logout)
        i3-msg exit
        ;;
    suspend)
        ~/.local/bin/lock && systemctl suspend
        ;;
    hibernate)
        lock && systemctl hibernate
        ;;
    reboot)
        systemctl reboot
        ;;
    shutdown)
        systemctl poweroff
        ;;
    *)
        echo "Usage: $0 {lock|logout|suspend|hibernate|reboot|shutdown}"
        exit 2
esac

exit 0
