#! /bin/sh
eval $(xdotool getmouselocation --shell)
width=200
height=200
datefmt="+%a %d.%m.%Y %H:%M:%S"

OPTIND=1
while getopts ":f:W:H:" opt; do
    case $opt in
        f) datefmt="$OPTARG" ;;
        W) width="$OPTARG" ;;
        H) height="$OPTARG" ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            exit 1
            ;;
        :)
            echo "Option -$OPTARG requires an argument." >&2
            exit 1
            ;;
    esac
done


# the position of the upper left corner of the popup
posX=`expr $X - 100` # TODO find screen size and keep the calendar within boundaries
posY=31

yad --calendar \
    --width=$width --height=$height \
    --undecorated --fixed \
    --close-on-unfocus --no-buttons \
    --posx=$posX --posy=$posY
	    
date "$datefmt"
