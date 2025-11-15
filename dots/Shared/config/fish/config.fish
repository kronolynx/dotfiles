# Nothing to do if not inside an interactive shell.
if not status is-interactive
    return 0
end

set -x GITHUB_ENTERPRISE_URL "https://evolution-github-copilot.ghe.com"
# Figure out which operating system we're in.
set -l os (uname)

fish_add_path "/Users/jortiz/.nvim/bin"

if command -q -v nvim
    set -gx EDITOR nvim
    set -gx VISUAL nvim
else if command -q -v vim
    set -gx EDITOR vim
    set -gx VISUAL vim
else
    set -gx EDITOR vi
    set -gx VISUAL vi
end

##########################################################
########## Plugins
##########################################################

# install fisher and plugins if not installed
if not functions -q fisher
    echo "Installing fisher"
    eval curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
    source ~/.config/fish/functions/fisher.fish
    eval fisher update
end

##########################################################
##########
##########################################################
# Add completions from stuff installed with Homebrew.
if test "$os" = Darwin && command -q -v brew
    set -x JAVA_HOME (/usr/libexec/java_home -v21)

    if test -d (brew --prefix)"/share/fish/completions"
        set -p fish_complete_path (brew --prefix)/share/fish/completions
    end
    if test -d (brew --prefix)"/share/fish/vendor_completions.d"
        set -p fish_complete_path (brew --prefix)/share/fish/vendor_completions.d
    end
end

set fish_plugins autojump vi-mode

# Remove the gretting message.
set -U fish_greeting

# Vi mode.
set -g fish_key_bindings fish_vi_key_bindings
# force cursor shape in wezterm https://github.com/wez/wezterm/issues/2781#issuecomment-1324143452
set fish_vi_force_cursor 1
set fish_cursor_default block
set fish_cursor_insert line
set fish_cursor_replace_one underscore

set -gx GPG_TTY (tty)

set -gx SHELL (which fish)

if command -q -v fzf
    set -gx FZF_BASE (which fzf)
    set -gx FZF_CTRL_T_COMMAND "command find -L \$dir -type f 2> /dev/null | sed '1d; s#^\./##'"
    set -gx FZF_DEFAULT_OPTS '--height 40% --layout=reverse --border --bind shift-up:preview-up,shift-down:preview-down,ctrl-u:preview-half-page-up,ctrl-d:preview-half-page-down'
    # fzf shell integration:
    fzf --fish | source
end

set -gx fifc_editor nvim

bind \cp history-search-backward # ctrl-p
bind \cn history-search-forward # ctrl-n

##########################################################
##########    Source
##########################################################

# if type -q bass
#     if test -f ~/.config/shared_env.sh
#         bass source ~/.config/shared_env.sh
#     end
# end

if test -e "$HOME/.config/fish/custom/functions.fish"
    source $HOME/.config/fish/custom/functions.fish
end

if test -e "$HOME/.config/fish/custom/dev.fish"
    source $HOME/.config/fish/custom/dev.fish
end

if test -e "$HOME/.config/fish/custom/abbr.fish"
    source $HOME/.config/fish/custom/abbr.fish
end

if test -e "$HOME/.config/fish/custom/alias.fish"
    source $HOME/.config/fish/custom/alias.fish
end

if test -e "$HOME/.config/fish/custom/work.fish"
    source $HOME/.config/fish/custom/work.fish
end

# if command -q -v direnv
#     direnv hook fish | source
# end

if command -q -v zoxide
    zoxide init fish | source
end

if command -q -v fnm
    fnm env --use-on-cd --shell fish | source
end

# if command -q -v kubectl
#     kubectl completion fish | source
# end

if command -q -v atuin
    atuin init fish --disable-up-arrow | source
end

if command -q -v carapace
    set -Ux CARAPACE_BRIDGES 'zsh,fish,bash,inshellisense' # optional
    carapace _carapace | source
end

# # Attach to tmux on start if not nested session or inside nvim, vscode or intellij
if command -q -v tmux
    and not set -q TMUX
    and not string match -r "vscode|JetBrains" $TERM_PROGRAM
    and not set -q NVIM
    tmux new-session -As dev
end

##########################################################
##########    Keybindings
##########################################################
# Replace !! by the previous command.
bind -M insert ! bind_bang
# Copy/paste.
bind yy fish_clipboard_copy
bind p fish_clipboard_paste

##########################################################
##########    Theme
##########################################################
#if command -v oh-my-posh >/dev/null 2>&1
#    oh-my-posh init fish | source
#else
if command -q -v starship
    starship init fish | source
end

#set --global kubectl

## tide configure --auto --style=Lean --prompt_colors='16 colors' --show_time='24-hour format' --lean_prompt_height='Two lines' --prompt_connection=Disconnected --prompt_spacing=Compact --icons='Few icons' --transient=Yes
# https://github.com/IlanCosman/tide/blob/7818abcbc600372418b1f8a931306b1d694bd009/functions/tide/configure/configs/lean_16color.fish#L62

# set -g tide_right_prompt_items status jobs rustc kubectl
# set -g tide_left_prompt_items context pwd git java node cmd_duration newline time character
#
# set -g tide_pwd_color_dirs blue
# set -g tide_pwd_color_anchors blue
# set -g tide_kubectl_color brblack
# set -g tide_time_color brcyan
# set -g tide_git_color_branch brblue
# set -g tide_git_color_branch brcyan
#
# set -g tide_pwd_style short
# set -g tide_pwd_truncation_length 1
# set -g tide_pwd_display truncate_to_last

# ## pure
# set --universal pure_enable_k8s true
# set --universal pure_shorten_prompt_current_directory_length 1
# set --universal pure_truncate_prompt_current_directory_keeps 0
