#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls
echo $SCRIPTPATH
COMMON="$(dirname $SCRIPTPATH)/common"
echo $COMMONS

# make scripts in current directory executable
find $SCRIPTPATH -type f -iname "*.sh" -exec chmod +x {} \;
find $COMMON -type f -iname "*.sh" -exec chmod +x {} \;

base=(

)

media=(

)


cli=(

)

cli_media=(

)

misc=(

)

coding=(
  code
  postman
)

$COMMON/helpers/pprint.sh "Installing Snaps"
$SCRIPTPATH/helpers/install-app.sh ${base[*]}
$SCRIPTPATH/helpers/install-app.sh ${cli[*]}
$SCRIPTPATH/helpers/install-app.sh ${cli_media[*]}
$SCRIPTPATH/helpers/install-app.sh ${media[*]}
$SCRIPTPATH/helpers/install-app.sh ${misc[*]}
$SCRIPTPATH/helpers/install-app.sh ${coding[*]}
