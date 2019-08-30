fish_vi_key_bindings
set fish_plugins autojump vi-mode
set -gx TERM rxvt-256color

set -gx PATH ~/.local/bin $PATH 

alias vim=nvim
alias vi=vim

if test -e "$HOME/.config/fish/custom/aliases.fish"
  source $HOME/.config/fish/custom/aliases.fish
end
if test -e "$HOME/.config/fish/custom/theme.fish"

  source $HOME/.config/fish/custom/theme.fish
end
if test -e "$HOME/.config/fish/custom/functions.fish"
  source $HOME/.config/fish/custom/functions.fish
end
if test -e "$HOME/.config/fish/custom/work.fish"
  source $HOME/.config/fish/custom/work.fish
end



