#!/bin/bash

case "$1" in
  lock)
    lock
    ;;
  logout)
    if  pgrep --exact awesome &> /dev/null; then
      echo 'awesome.quit()' | awesome-client
    elif pgrep --exact i3 &> /dev/null; then
      i3-msg exit
    fi
    ;;
  restart)
    systemctl reboot
    ;;
  shutdown)
    systemctl poweroff
    ;;
  suspend)
    lock && systemctl suspend
    ;;
  reboot)
    systemctl reboot
    ;;
  hibernate)
    lock && systemctl hibernate
    ;;
  *) 
    echo "Usage: $0 {lock|logout|suspend|hibernate|reboot|shutdowon}"
    exit 2
esac
