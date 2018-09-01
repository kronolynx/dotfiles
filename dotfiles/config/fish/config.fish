fish_vi_key_bindings

set -Ux EDITOR vim
set -Ux VISUAL vim

set -gx fish_greeting ''

# programming
source ~/.asdf/asdf.fish
set -gx PATH ~/.cargo/bin $PATH
#set -gx PATH ~/.local/bin $PATH

alias vi=vim
# search in yay
alias S="yay -Ss "
# install with yay
alias I="yay -Syu "
alias mirrors="sudo pacman-mirrors --fasttrack 15; sudo pacman -Syyu"
alias swapk="setxkbmap -model pc105 -layout dvorak,es -option grp:alt_shift_toggle,caps:backspace"

alias v='f -e vim'
alias o='a -e xdg-open'
