##########################################################
##########    Aliases
##########################################################

#replace system tools

if command -v bat >/dev/null 2>&1
    alias old_cat=bat
    alias cat=bat # replace cat with bat
end
if command -v eza >/dev/null 2>&1
    alias ls="eza --icons --group-directories-first --color=auto" # improved ls
end

alias nn='nvim'
alias no="LSP_ENABLED=false nvim"
alias vin="vim -u NONE" # vim no config

alias CAPS="xdotool key Caps_Lock"

alias rg="rg --follow" # follow symlinks
alias nov='Applications/Neovide.app/Contents/MacOS/neovide'
alias emx="emacsclient -t --alternate-editor='nvim'"
alias emc="emacsclient -c -a emacs"
alias cm="cmus"
alias co="code ."
alias copilot='gh copilot'
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
alias tk="tmux kill-session -t "
alias tkd="tmux list-sessions | grep -v attached | cut -d: -f1 |  xargs -t -n1 tmux kill-session -t"
alias untar='tar -sxvf '
alias v="vim"
alias wget='wget -c '
alias bashi='bash -s interactive'
alias zshi='zsh -s interactive'
alias vm=vifm
alias nv=nvim
alias nn=nvim
alias nf='nvim (fzf)'
alias nov='neovide --frame=none'

# to create more alias for different distro operations
# https://wiki.archlinux.org/index.php/Pacman/Rosetta

# source and edit
alias EF="nvim ~/.config/fish/config.fish"
alias EG="nvim ~/.gitrc"
alias ET="nvim ~/.tmux.conf"
alias LF="cat ~/.config/fish/custom/aliases.fish | grep -v '^#' | grep 'alias'"
alias SF="source ~/.config/fish/config.fish"
alias SX="xrdb ~/.Xresources"

alias screenOff="xset dpms force off"

# networking. ip address, dig, dns
alias ipdig="dig +short myip.opendns.com @resolver1.opendns.com"
alias dig="dig +nocmd any +multiline +noall +answer"

# file size
alias fs="stat -f \"%z bytes\""

alias g='git'

alias jv17="set -x JAVA_HOME (/usr/libexec/java_home -v17)"
alias jv21="set -x JAVA_HOME (/usr/libexec/java_home -v21)"

fnm env --use-on-cd --shell fish | source
