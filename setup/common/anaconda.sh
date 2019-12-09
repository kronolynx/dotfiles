#!/bin/bash

ANACONDA="$(wget -O - https://www.anaconda.com/distribution/ 2>/dev/null | sed -ne 's@.*\(https:\/\/repo\.anaconda\.com\/archive\/Anaconda3-.*-Linux-x86_64\.sh\)\">64-Bit (x86) Installer.*@\1@p')"

SCRIPT="$(basename $ANACONDA)"


(cd /tmp
 curl $ANACONDA -o $SCRIPT
 # -b  Batch mode with no PATH modifications to ~/.bashrc. Assumes that you agree to the license agreement. Does not edit the .bashrc or .bash_profile files.
 # -p  Installation prefix/path.
 bash $SCRIPT -b -p $HOME/.anaconda
)
