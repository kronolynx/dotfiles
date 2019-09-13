#!/bin/bash

shutdownChoice=$(zenity --height=300 --print-column="2" --list --text "would you like to log out?" --column "" --column "" 1 "Lock" 2 "Suspend" 3 "Restart" 4 "Shut down" 5 "Hibernate" 6 "Log out")

if [ "$shutdownChoice" = "Lock" ]; then
 lock
elif [ "$shutdownChoice" = "Log out" ]; then
  if  pgrep --exact awesome &> /dev/null; then
    echo 'awesome.quit()' | awesome-client
  elif pgrep --exact i3 &> /dev/null; then
    i3-msg exit
  fi
elif [ "$shutdownChoice" = "Restart" ]; then
 systemctl reboot
elif [ "$shutdownChoice" = "Shut down" ]; then
 systemctl poweroff
elif [ "$shutdownChoice" = "Suspend" ]; then
 lock && systemctl suspend
elif [ "$shutdownChoice" = "Hibernate" ]; then
 lock && systemctl hibernate
else
 echo Cancelled
fi
