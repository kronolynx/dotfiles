#!/bin/bash

sudo apt install cmake pkg-config libfreetype6-dev libfontconfig1-dev xclip

git clone https://github.com/jwilm/alacritty.git
cd alacritty

if [ ! -d "$HOME/.cargo" ]; then
  curl https://sh.rustup.rs -sSf | sh
fi

source $HOME/.cargo/env

rustup override set stable
rustup update stable

cargo build --release

# desktop entry
sudo cp target/release/alacritty ~/.local/bin # or anywhere else in $PATH
sudo cp extra/logo/alacritty-term.svg /usr/share/pixmaps/Alacritty.svg
sudo desktop-file-install extra/linux/alacritty.desktop
sudo update-desktop-database

cargo install cargo-deb --force
cargo deb --install

mkdir -p ${ZDOTDIR:-~}/.zsh_functions
echo 'fpath+=${ZDOTDIR:-~}/.zsh_functions' >> ${ZDOTDIR:-~}/.zshrc

cp extra/completions/_alacritty ${ZDOTDIR:-~}/.zsh_functions/_alacritty

sudo tic -xe alacritty,alacritty-direct extra/alacritty.info

cd ..
rm -rf alacritty
