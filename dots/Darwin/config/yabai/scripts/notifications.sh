#!/bin/bash

res=$(lsappinfo -all list | grep "label\"=")

BELL=􀋚
BELL_DOT=􀝗

if [[ "$res" == *"label\"=\"\""* ]]; then
    echo "$BELL"
else
    echo "$BELL_DOT"
fi
