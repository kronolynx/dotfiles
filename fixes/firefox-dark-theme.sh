#!/bin/bash

echo "Fix firefox for dark themes"

# Firefox must have started at least once. The directory will not exist otherwise.
firefox &
sleep 1
killall firefox


# css to fix firefox for dark themes
read -r -d '' FFIX <<'EOF'
input:not(.urlbar-input):not(.textbox-input):not(.form-control):not([type='checkbox']):not([type='radio']), textarea, select {
-moz-appearance: none !important;
background-color: #eee;
color: #111;
}

#downloads-indicator-counter {
color: white;
}
EOF
# save present working directory
CUR_DIR=$PWD

# create folder for css in firefox profile and cd into it
cd ~/.mozilla/firefox/*.default && mkdir -p "chrome" && cd "chrome"
# create css file
echo "$FFIX" > userContent.css

# return to the saved working directory
cd $CUR_DIR


