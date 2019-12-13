#!/bin/bash

if command -v code >/dev/null 2>&1; then
  case $1 in
    install)
      if [ -f code-ext ]; then
	while read ext; do
	  code --install-extension "$ext"
	done < code-ext
      else
	echo "code extensions file not found"
      fi
      ;;
    *)
      echo "Backing extension list to 'code-ext'"
      echo "To install backed up extensions run: $0 install"
      code --list-extensions --show-versions > code-ext
      ;;
  esac
else
  echo "code not installed"
fi
