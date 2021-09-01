
#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls
COMMON="$(dirname $SCRIPTPATH)/common"

# make scripts in current directory executable
find $SCRIPTPATH -type f -iname "*.sh" -exec chmod +x {} \;

coding=(
  jdk8-openjdk
  # # Latex
  # # texlive-most
  meld
  # a shell extension that manages your environment
  direnv-bin
  # # The Glasgow Haskell Compiler
  # #ghc
  # # The Haskell Tool Stack
  # #stack
  # # git pager
  # git-delta

  asbru-cm # connection manager
  #cassandra
  cqlsh
)

docker = (
  docker
  docker-compose
  socat # remote
)

misc = (
  aws-cli
  aws-iam-authenticator-bin
)

$SCRIPTPATH/helpers/install-app.sh ${coding[*]}
$SCRIPTPATH/helpers/install-app.sh ${docker[*]}

$SCRIPTPATH/../common/apps/scala-env.sh