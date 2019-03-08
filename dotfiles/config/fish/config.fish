fish_vi_key_bindings
set fish_plugins autojump vi-mode
set -gx TERM rxvt-256color
set -Ux EDITOR "emacsclient -nw -c emacs"
set -Ux VISUAL "emacsclient -nw -c emacs"

set -gx PATH ~/.local/bin $PATH

# console emacs
alias em="emacsclient -nw -c -a emacs"
alias emc="emacsclient -c -a emacs"
alias vi=vim

# apt alias
alias S="apt search "
alias U="sudo apt update; and sudo apt upgrade"
alias I="sudo apt install "
alias R="sudo apt remove "
function g --wraps git
  git $argv;
end
alias LF="cat ~/.config/fish/config.fish | grep 'alias'"
alias SF="source ~/.config/fish/config.fish"
function get_port
    lsof -i ":$argv"
end
alias P="get_port " #"lsof -i :" #add port e.g. 8080

alias tkd="tmux list-sessions | grep -v attached | cut -d: -f1 |  xargs -t -n1 tmux kill-session -t"
alias tk="tmux kill-session -t "

alias swapk="setxkbmap -model pc105 -layout dvorak,es -option grp:alt_shift_toggle,caps:backspace"

# kill any process listening on the port given e.g: kp 8080
function kp
  kill -9 (lsof -t -i:$argv) 2>/dev/null; and echo "Process on port $argv killed" ;or echo "Nothing listening on port $argv"
end
alias v='f -e vim'
alias o='a -e xdg-open'
alias cat=bat
alias ls=lsd
alias l='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias lt='ls --tree'
alias untar='tar -sxvf '
alias wget='wget -c '
alias ipe='curl ipinfo.io/ip'
# make a directory and cd into it
function md
    mkdir -p "$argv"; and cd "$argv"
end

set -U fish_key_bindings fish_vi_key_bindings
set -U budspencer_nogreeting
