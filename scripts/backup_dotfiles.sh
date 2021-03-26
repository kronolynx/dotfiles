#!/bin/bash

DOT_DIR=dotfiles
## this script makes a backup of the specified dot files

echo -e ""
echo -e "################################################################"
echo -e "############## \e[32m Copying dotfiles \e[36m '$DOT_DIR' \e[0m"
echo -e "################################################################"
echo -e ""

if ! command -v rsync &> /dev/null
then
    echo "Error: rsync could not be found"
    exit
fi

if [ -d $DOT_DIR ]; then
    backup="backup-$(date +'%Y%m%d')$(date +'%H%M%S')"
    cp -r $DOT_DIR $backup
fi


for f in "$@" ; do
    file_or_dir=$HOME/.$f
    if [ -f $file_or_dir ]; then
        mkdir -p $DOT_DIR/$(dirname $f)
        rsync --no-links $file_or_dir $DOT_DIR/$f
    elif [ -d $file_or_dir ]; then
        mkdir -p $DOT_DIR/$f
        rsync -r --no-links --delete-during $file_or_dir/* $DOT_DIR/$f
    fi
done