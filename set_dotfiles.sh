#!/bin/bash

## this script copies all the dotfiles into home


# dotfiles dir
dotfiles="dotfiles"
# backup specified dotfiles
./backup_dotfiles.sh

# WARNING any file existing in dotfiles will override the corresponding file in home

files=$(ls $dotfiles)

for f in ${files[*]}; do
    if [ -d $dotfiles/$f ]; then
        mkdir -p $HOME/.$f
        cp --remove-destination -r $dotfiles/$f/* $HOME/.$f/
    else
        file="${f##*/}"
        cp --remove-destination $dotfiles/$file $HOME/.$file
    fi
done

echo "dotfiles copied"


# reload xresources
xrdb ~/.Xresources
