fish_vi_key_bindings

set -Ux EDITOR nvim
set -Ux VISUAL nvim

set -gx fish_greeting ''

# programming
source ~/.asdf/asdf.fish
set -gx PATH ~/.cargo/bin $PATH
set -gx PATH ~/.gem/ruby/2.5.0/bin $PATH
set -gx PATH ~/.local/bin $PATH 

alias vi=nvim
alias vim=nvim
alias search="yay -Ss "
alias install="yay -Syu "
# alias monitor="xrandr | grep 'HDMI1 connected' && xrandr --output eDP1 --auto --primary --output HDMI1 --auto --above eDP1"
#alias swapk="setxkbmap -option caps:swapescape"
#alias swapk="setxkbmap -modiel pc105 -layout dvorak,es -option grp:alt_shift_toggle,caps:swapescape"
alias swapk="setxkbmap -model pc105 -layout dvorak,es -option grp:alt_shift_toggle,caps:backspace"

alias mp3=yo-mp3
alias vid=yo-vid
alias mirrorupdate="sudo pacman-mirrors -gm rank"

alias crop='k2pdfopt -mode fw -c -ls- '

alias v='f -e nvim' 
alias o='a -e xdg-open'

#set -U fish_user_paths $HOME/.asdf/installs/python/anaconda2-5.0.1/bin $fish_user_paths
#source (conda info --root)/etc/fish/conf.d/conda.fish

# tabtab source for electron-forge package
# uninstall by removing these lines or running `tabtab uninstall electron-forge`
[ -f /home/krono/Workspace/js/electron-vue-typescript-starter/node_modules/tabtab/.completions/electron-forge.fish ]; and . /home/krono/Workspace/js/electron-vue-typescript-starter/node_modules/tabtab/.completions/electron-forge.fish
