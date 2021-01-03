#!/bin/bash

timestamp() {
  date "+%Y_%m_%d_%H%M%S"
}

SCREEN_DIR=~/Pictures/screenshots

[ -d $SCREEN_DIR ] || mkdir -p $SCREEN_DIR

TARGET="$SCREEN_DIR/$(timestamp).png"

case "$1" in
  area)
   notify-send 'Select Area' # TODO fix message captured by screenshot
   sleep 0.5
   import $TARGET && notify-send 'Area captured'
  ;;
  window)
    CURRENT=`xprop -root | grep "_NET_ACTIVE_WINDOW(WINDOW)" | awk '{print $NF}'`
    import -window $CURRENT -screen $TARGET && notify-send 'Focused window captured'
  ;;
  root)
    import -window "root" $TARGET && notify-send 'Desktop captured'
  ;;
  *)
    echo "Usage: \n screen_shot.sh area|window|root \n"
    exit 1
    ;;
esac
