#!/bin/bash

width=80
inner_width=58
normal="\e[0m"

case "${2:-cyan}" in
  "red")     color="\e[31m" ;;
  "green")   color="\e[32m" ;;
  "yellow")  color="\e[33m" ;;
  "blue")    color="\e[34m" ;;
  "magenta") color="\e[35m" ;;
  "cyan")    color="\e[36m" ;;
  "gray")    color="\e[37m" ;;
esac

generate_padding () {
  printf "$1"'%.0s' $(eval "echo {1.."$(($2))"}");
}

side_padding=$(generate_padding "#" 10)
full_padding=$(generate_padding "#" $width)

split_long_text() {
  echo "TODO: split text to keep it centered with less than 58 characters per line"
}

center_text() {
  let "even = ${#1} % 2"
  let "padding = ($inner_width - ${#1}) / 2"
  leftp=$(generate_padding " " $padding)
  rightp=$(generate_padding " " $(expr $padding + $even))
  printf "${side_padding}${leftp}${color} $1 ${normal}${rightp}${side_padding}"
}

pprint() {
  printf "${full_padding}\n"
  printf "$(center_text "$1")\n"
  printf "${full_padding}\n"
}

pprint "$1"
