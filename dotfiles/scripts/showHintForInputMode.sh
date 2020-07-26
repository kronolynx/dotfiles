#!/usr/bin/env bash

mode_name=$1
fg_color=$2
bg_color=$3
bar_height=$4
 
screen_size=$(xdpyinfo | awk '/dimensions:/ { print $2; exit }')
screen_width=$(echo "${screen_size}" | cut -f1 -dx)
screen_height=$(echo "${screen_size}" | cut -f2 -dx)
 
y=0
x=0
 
font="Source Code Pro Medium:size=12"
#PERSFLAGS="onstart=grabkeys,uncollapse;entertitle=uncollapse,grabkeys;enterslave=grabkeys;leaveslave=ungrabkeys;leavetitle=ungrabkeys;key_Escape=ungrabkeys,exit"
dzen_flags="onstart=uncollapse"
 
# $MODE_NAME ($2 , $3 , $4 , $5, $LINE_HEIGHT, $X, $Y, $W
(echo "$mode_name"; cat) | dzen2 -fn "${font}" -bg "${bg_color}" -fg "${fg_color}" -h "${bar_height}" -x "${x}" -y "$((y - bar_height))" -w "${screen_width}" -e "${dzen_flags}" -title-name "dzen_input_mode"
