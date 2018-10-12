#!/bin/bash
if [ -f ~/.zshrc ]; then
  SHELL_NAME="zshrc"
else
  SHELL_NAME="bashrc"
fi
SHELL_FILE="${HOME}/.${SHELL_NAME}"

add_node() {
        if ! pacman -Qi nvm &> /dev/null; then
            ./install-app.sh nvm
            ./install-app.sh yarn
        fi

  if ! command grep -qc '/nvm.sh' "$SHELL_FILE"; then
    echo "Appending nvm source string to zsh"

                command printf "\\n# Set up Node Version Manager
\\nexport NVM_DIR=\"\$HOME/.nvm\"                            # You can change this if you want.
\\nexport NVM_SOURCE=\"/usr/share/nvm\"                     # The AUR package installs it to here.
\\n[ -s \"\$NVM_SOURCE/nvm.sh\" ] && \\. \"\$NVM_SOURCE/nvm.sh\"  # Load NVM" >> "$SHELL_FILE"

  fi
        source $SHELL_FILE
        nvm install --lts
}
add_rust() {
  if [ ! -d "$HOME/.cargo" ]; then
    echo "Installing rust"
    curl https://sh.rustup.rs -sSf | sh
    source $HOME/.cargo/env
  fi
}
add_rustup(){
  echo "rustup"
  rustup self update
  rustup update
  rustup component add rust-analysis
  rustup component add rls-preview
  rustup component add rust-src
  rustup run nightly cargo install clippy
  rustup run nightly cargo install racer
}

add_asdf() {
    if [ ! -d "$HOME/.asdf" ]; then
      echo "asdf"
      git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.5.0
    fi
    if ! command grep -qc '/asdf.sh' "$SHELL_FILE"; then
      echo "Appending asdf source string to zsh"
      echo -e '\n# asdf' >> "$SHELL_FILE"
      echo -e '. $HOME/.asdf/asdf.sh' >> "$SHELL_FILE"
      echo -e '. $HOME/.asdf/completions/asdf.bash' >> "$SHELL_FILE"
      source "$SHELL_FILE"
    fi
}

asdf_plugins() {
    echo -e "asdf plugins"
    #asdf plugin-add python https://github.com/tuvistavie/asdf-python.git
    asdf plugin-add mongodb https://github.com/sylph01/asdf-mongodb.git
    #asdf plugin-add postgres https://github.com/smashedtoatoms/asdf-postgres.git
    #asdf plugin-add lua https://github.com/Stratus3D/asdf-lua.git
    #asdf plugin-add R https://github.com/iroddis/asdf-R.git
    #asdf plugin-add ruby https://github.com/asdf-vm/asdf-ruby.git
    #asdf plugin-add redis https://github.com/smashedtoatoms/asdf-redis.git
}
add_scala() {
  ./install-app.sh jdk8-openjdk
  ./install-app.sh sbt
  if [ ! -f "/usr/local/bin/amm" ]; then
    echo -e "Installing ammonite"
    sudo sh -c '(echo "#!/usr/bin/env sh" && curl -L https://github.com/lihaoyi/Ammonite/releases/download/1.1.2/2.12-1.1.2) > /usr/local/bin/amm && chmod +x /usr/local/bin/amm'
  fi
}

add_node
add_rust
add_rustup
add_asdf
asdf_plugins
add_scala