#!/bin/bash
sudo add-apt-repository ppa:neovim-ppa/stable -y
sudo apt update -y
sudo apt install neovim -y

mkdir -p ~/.config/nvim

# import existing configuration
# ln -s ~/.vimrc ~/.config/nvim/init.vim
# or create one from current
# cp ~/.vimrc ~/.config/nvim/init.vim

 mkdir -p ~/.vim/undo
 mkdir -p ~/.vim/backup
