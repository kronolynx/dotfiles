#!/bin/bash

# Usage
# call this script passing one or multiple app names
# $ ./install-app.sh git
# $ ./install-app.sh git ssh firefox

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls
COMMON="$(dirname $(dirname $SCRIPTPATH))/common"

install_app() {
    package=$1 # name of the package to install

    if dpkg -s $package &> /dev/null; then
        $COMMON/helpers/pprint.sh "$package is already installed" "yellow"
    else
        sudo apt install -y $package &&
        $COMMON/helpers/pprint.sh "Installed $package" "blue" ||
        $COMMON/helpers/pprint.sh "Failed to install $package" "red"
    fi
}

for app in ${@}; do
    install_app $app
done
