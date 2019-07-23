#!/bin/sh

add_java_sdk() {
  if ! java -version 2>&1 | grep -q "OpenJDK Runtime"; then
    echo "########## Installin openjdk-8 ##########"
    sudo apt install openjdk-8-jdk -y
    echo "to choose java version run: sudo update-alternatives --config java"
    echo ""
  fi
}

add_sbt() {
  if ! [ -x "$(command -v sbt)" ]; then
    echo "########## Installin openjdk-8 ##########"
    echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
    sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
    sudo apt install sbt -y
  fi
}

add_ammonite() {
  if [ -x "$(command -v amm)" ]; then
    echo -e "Updating ammonite (run as 'amm')"
  else
    echo -e "Installing ammonite (run as 'amm')"
  fi

  mkdir -p temp
  cd temp
  curl -s "https://api.github.com/repos/lihaoyi/Ammonite/releases/latest" \
    | grep -E "2.12-1.{1,8}\"$" \
    | cut -d : -f 2,3 \
    | tr -d \" \
    | wget -qi -

  sudo mv 2.12-1* /usr/local/bin/amm && chmod +x /usr/local/bin/amm

  cd ..
  rmdir temp
}

add_java_sdk
add_sbt
add_ammonite
