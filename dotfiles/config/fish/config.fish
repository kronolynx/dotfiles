fish_vi_key_bindings

set -Ux EDITOR "emacsclient -nw -c emacs"
set -Ux VISUAL "emacsclient -nw -c emacs"

set -gx fish_greeting ''

# programming
#source ~/.asdf/asdf.fish
#set -gx PATH ~/.cargo/bin $PATH
set -gx PATH ~/.local/bin $PATH

# console emacs
alias em="emacsclient -nw -c -a ecamcs"
alias vi=vim
# # search in yay
# alias S="yay -Ss "
# # install with yay
# alias I="yay -Syu "
# alias mirrors="sudo pacman-mirrors --fasttrack 15; sudo pacman -Syyu"
# alias swapk="setxkbmap -model pc105 -layout dvorak,us -option grp:alt_shift_toggle,caps:backspace"

# apt alias
alias S="apt search "
alias U="sudo apt update; and sudo apt upgrade"
alias I="sudo apt install "
alias R="sudo apt remove "

alias g=git
alias F="cat ~/.config/fish/config.fish | grep 'alias'"
function get_port
    lsof -i ":$argv"
end
alias P="get_port " #"lsof -i :" #add port e.g. 8080

alias tkd="tmux list-sessions | grep -v attached | cut -d: -f1 |  xargs -t -n1 tmux kill-session -t"
alias tk="tmux kill-session -t "

function mk_cd
    mkdir -p "$argv"; and cd "$argv"
end
alias md=mk_cd

alias v='f -e vim'
alias o='a -e xdg-open'
alias cat=bat
alias untar='tar -sxvf '
alias wget='wget -c '
alias ipe='curl ipinfo.io/ip'

# bobthefish theme
set -g theme_nerd_fonts yes
set -g theme_display_vi yes
set -g theme_color_scheme dracula
