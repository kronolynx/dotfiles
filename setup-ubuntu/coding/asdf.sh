#!/bin/bash

if [ -f ~/.zshrc ]; then
  SHELL_NAME="zshrc"
else
  SHELL_NAME="bashrc"
fi
SHELL_FILE="${HOME}/.${SHELL_NAME}"

add_asdf() {
  if [ ! -d "$HOME/.asdf" ]; then
    echo "asdf"
    git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.7.3
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


add_asdf
asdf_plugins
