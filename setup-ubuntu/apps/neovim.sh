#!/bin/bash
sudo add-apt-repository ppa:neovim-ppa/stable -y
sudo apt update -y
sudo apt install neovim -y

mkdir -p ~/.config/nvim

curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# import existing configuration
# ln -s ~/.vimrc ~/.config/nvim/init.vim
# or create one from current
 cp ~/.vimrc ~/.config/nvim/init.vim
