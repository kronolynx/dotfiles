#!/bin/sh

add_java_sdk() {
  JAVA_VERSION=$(java -version 2>&1 | grep "version" | cut -d ' ' -f 1,3)
  RUNTIME=$(echo $JAVA_VERSION | awk '{print $1}')
  VERSION=$(echo $JAVA_VERSION | awk '{print $2}' | tr -d \" | cut -c1-3)

  if [ "$VERSION" != "1.8" -o "$RUNTIME" != "openjdk" ]; then
    echo "##########\e[32m Installing openjdk-8 \e[0m##########"
    sudo apt install openjdk-8-jdk -y
    echo "##########\e[33m to choose java version run: sudo update-alternatives --config java \e[0m##########"
    echo ""
  fi
}

add_sbt() {
  if ! [ -x "$(command -v sbt)" ]; then
    echo "##########\e[32m Installing sbt \e[0m##########"
    echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
    sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
    sudo apt install sbt -y
  fi
}

add_ammonite() {
  if [ -x "$(command -v amm)" ]; then
    echo "##########\e[32m Updating ammonite (run as 'amm') \e[0m##########"
  else
    echo "##########\e[32m Installing ammonite (run as 'amm') \e[0m##########"
  fi

  mkdir -p temp
  (cd temp
  curl -s "https://api.github.com/repos/lihaoyi/Ammonite/releases/latest" \
    | grep -E "2.12-1.{1,8}\"$" \
    | cut -d : -f 2,3 \
    | tr -d \" \
    | wget -qi -

  sudo mv 2.12-1* /usr/local/bin/amm && chmod +x /usr/local/bin/amm)

  rm -rf temp
}

add_java_sdk
add_sbt
add_ammonite
