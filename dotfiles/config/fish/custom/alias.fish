##########################################################
##########    Aliases
##########################################################

#replace system tools

if command -v bat >/dev/null 2>&1
  alias old_cat=bat
  alias cat=bat # replace cat with bat
end
if command -v exa >/dev/null 2>&1
  alias old_ls=ls
  alias ls=exa # improved ls
end

# if command -v nvim >/dev/null 2>&1
#   alias old_vim=vim
#   alias vim=nvim
# end
alias vin="vim -u NONE" # vim no config

alias CAPS="xdotool key Caps_Lock"

alias rg="rg --follow" # follow symlinks

alias emx="emacsclient -t --alternate-editor='nvim'"
alias emc="emacsclient -c -a emacs"
alias cm="cmus"
alias co="code ."
alias cl="clear && printf '\e[3J'"
alias chmox='chmod +x'
alias fv='f -e vim'
alias l='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias lp="lsof -i :" # e.g. lp 8080 # which app is using a port
alias lt='ls --tree'
alias o='a -e xdg-open'
alias r='ranger --choosedir=$HOME/.rangerdir; set LASTDIR (cat $HOME/.rangerdir); cd "$LASTDIR"'
alias rm="rm -i"
alias tk="tmux kill-session -t "
alias tkd="tmux list-sessions | grep -v attached | cut -d: -f1 |  xargs -t -n1 tmux kill-session -t"
alias untar='tar -sxvf '
alias v="vim"
alias wget='wget -c '
alias bashi='bash -s interactive'
alias vm=vifm
alias nv=nvim

# to create more alias for different distro operations
# https://wiki.archlinux.org/index.php/Pacman/Rosetta

# source and edit
alias EF="vim ~/.config/fish/config.fish"
alias EG="vim ~/.gitrc"
alias ET="vim ~/.tmux.conf"
alias EV="vim ~/.vimrc"
alias LF="cat ~/.config/fish/custom/aliases.fish | grep -v '^#' | grep 'alias'"
alias SF="source ~/.config/fish/config.fish"
alias SV="source ~/.vimrc"
alias SX="xrdb ~/.Xresources"

alias screenOff="xset dpms force off"

alias nordc="nordvpn c p2p && nordvpn s killswitch on && nordvpn s autoconnect on"
alias nordd="nordvpn s killswitch off && nordvpn s autoconnect off && nordvpn d"

# networking. ip address, dig, dns
alias ipdig="dig +short myip.opendns.com @resolver1.opendns.com"
alias dig="dig +nocmd any +multiline +noall +answer"

# file size
alias fs="stat -f \"%z bytes\""


# json manipulation
# https://github.com/tomnomnom/gron
alias norg="gron --ungron"
alias ungron="gron --ungron"
