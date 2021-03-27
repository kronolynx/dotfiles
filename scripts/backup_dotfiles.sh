#!/bin/bash

DOT_DIR=dotfiles
## this script makes a backup of the specified dot files

echo -e ""
echo -e "################################################################"
echo -e "############## \e[32m Copying dotfiles \e[36m '$DOT_DIR' \e[0m"
echo -e "################################################################"
echo -e ""

if [ -d $DOT_DIR ]; then
    backup="backup-$(date +'%Y%m%d')$(date +'%H%M%S')"
    cp -r $DOT_DIR $backup
else
    mkdir -p $DOT_DIR
fi

copy_files() {
    for f in "$@" ; do
        local file_or_dir=$HOME/.$f
        if [ -f $file_or_dir ] && [ ! -L $file_or_dir ]; then
            mkdir -p $DOT_DIR/$(dirname $f)
            echo -e "\e[32mcopying\e[0m  => $f"
            cp --remove-destination $file_or_dir $DOT_DIR/$f
        elif [ -f $f ] && [ ! -L $f ]; then
            next_file=$(echo $f | cut -d "." -f 2-)
            copy_files $next_file
        elif [ -d $file_or_dir ]; then
            next_files=($file_or_dir/*)
            copy_files $next_files
        else 
            echo -e "\e[33mskipping\e[0m => $file_or_dir"
        fi
    done
}

copy_files "$@" 