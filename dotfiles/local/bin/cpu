#!/bin/bash

# CPU Temperature

temp=$(sensors | awk '/^Core 0/ {print $3}')

echo $temp

exit 0
