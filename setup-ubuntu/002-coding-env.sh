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
    git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.6.0
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

add_docker() {
  sudo apt install \
       apt-transport-https \
       ca-certificates \
       curl \
       software-properties-common
  curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
  sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"

  sudo apt update
  sudo apt install docker-ce
}

add_java() {
  sudo add-apt-repository ppa:webupd8team/java -y
  sudo apt update
  ./install-app.sh oracle-java8-installer
  ./install-app.sh oracle-java8-set-default
  # ./install-app.sh openjdk-8-jdk
  # ./install-app.sh openjdk-8-source
  # ./install-app.sh sbt
}

add_sbt() {
  echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
  sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
  ./install-app.sh sbt
}

add_ammonite() {
  if [ ! -f "/usr/local/bin/amm" ]; then
    echo -e "Installing ammonite"
    sudo sh -c '(echo "#!/usr/bin/env sh" && curl -L https://github.com/lihaoyi/Ammonite/releases/download/1.6.5/2.12-1.6.5) > /usr/local/bin/amm && chmod +x /usr/local/bin/amm'
  fi
}

add_haskell_tools() {
  curl -sSL https://get.haskellstack.org/ | sh
  stack install hfmt
}

extras() {
  sudo apt install zeal
}

add_java
add_sbt
add_ammonite
#add_asdf
#add_docker
#extras
