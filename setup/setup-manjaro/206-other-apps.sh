#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls

apps=(
redshift
)

$SCRIPTPATH/helpers/pprint.sh "Installing base apps" 
$SCRIPTPATH/helpers/install-app.sh ${apps[*]}

