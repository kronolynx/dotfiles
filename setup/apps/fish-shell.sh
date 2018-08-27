#!/bin/bash

apps=(
fish
fisherman
)

../install-app.sh ${apps[*]}

# run fisherman to install plugins
fisher
