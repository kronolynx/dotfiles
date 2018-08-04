#!/bin/bash

## this script backups and then copies all the dotfiles into home


# origin of dotfiles
# if no console argument is provided the origin is set to dotfiles
dotfiles_origin=${@:-dotfiles}


# backup location  e.g. backup-20180804164746
backup_dir="backup-$(date +'%Y%m%d')$(date +'%H%M%S')"

# backup each file in dotfiles_origin that exist in home
shopt -s globstar
for file in $dotfiles_origin/**
do
    # check that the file exist
    if [ -f $file ]; then
        f=${file#${dotfiles_origin}/}
        home_file=$HOME/.$f
        # check that the file exist in home
        if [ -f $home_file ]; then
            destination_dir=$backup_dir/$(dirname $f)
            mkdir -p $destination_dir
            cp --remove-destination -r $home_file $backup_dir/$f
        fi
    fi
done

# copy each file from the origin in to home overriding the original one
files=$(ls $dotfiles_origin)
for f in ${files[*]}; do
    if [ -d $dotfiles_origin/$f ]; then
        mkdir -p $HOME/.$f
        cp --remove-destination -r $dotfiles_origin/$f/* $HOME/.$f/
    else
        file="${f##*/}"
        cp --remove-destination $dotfiles_origin/$file $HOME/.$file
    fi
done

echo "dotfiles copied"


# reload xresources
#xrdb ~/.Xresources
