#!/bin/bash

apps=(
# code documentation
zeal
# code color higlight
highlight
# visual studio code
visual-studio-code-bin
# atom editor
atom
# Manage all your JetBrains Projects and Tools
jetbrains-toolbox
# Latex
texlive-most
)

./install-app.sh ${apps[*]}

set_vim() {
    ./install-app.sh vim

    if [ ! -d "$HOME/.vim/plugged" ]; then
        curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
            https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    fi
    mkdir -p $HOME/.vim/backup
    mkdir -p $HOME/.vim/undo
}

set_tmux() {
    ./install-app.sh tmux

    if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
        git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    fi
}

# spacemacs editor
spacemacs () {
    ./install-app.sh emacs
    if [ ! -d "$HOME/.emacs.d" ]; then
        git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
    else
        echo "Can't install spacemacs: .emacs.d already exist"
    fi
}



vs-code-ext() {
code --install-extension robertohuertasm.vscode-icons
code --install-extension eg2.tslint
code --install-extension donjayamanne.python-extension-pack
code --install-extension oderwat.indent-rainbow
code --install-extension vscodevim.vim
code --install-extension rust-lang.rust
code --install-extension be5invis.toml # TOML language support
code --install-extension webfreak.debug # GDB, LLDB & Mago-MI Debugger support for VSCode
code --install-extension vadimcn.vscode-lldb # Debug your native code with LLDB
code --install-extension eamodio.gitlens # Git supercharged
code --install-extension christian-kohler.path-intellisense
code --install-extension EditorConfig.editorconfig
code --install-extension eg2.tslint
code --install-extension formulahendry.code-runner
code --install-extension hookyqr.beautify
code --install-extension joelday.docthis
code --install-extension mgmcdermott.vscode-language-babel
code --install-extension milkmidi.vs-code-quick-require
code --install-extension msjsdiag.debugger-for-chrome
code --install-extension robertohuertasm.vscode-icons
code --install-extension robinbentley.sass-indented
code --install-extension shinnn.stylelint
code --install-extension steoates.autoimport
code --install-extension wix.vscode-import-cost
}

atom-ext() {
apm install vim-mode-plus
}
set_vim
spacemacs
set_tmux
vs-code-ext
