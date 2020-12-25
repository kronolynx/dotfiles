#!/bin/bash

PACMAN_CONF=/etc/pacman.conf

if grep -q "#.*Color" "$PACMAN_CONF"; then
  echo "Enable color"
  sed -i 's/#.*Color.*/Color/' "$PACMAN_CONF" 
fi

if ! grep -q "ILoveCandy" "$PACMAN_CONF"; then
  echo "Enable I love candy"
  # /a insert line after match /i insert line before line
  sed -i '/.*Color.*/a ILoveCandy' "$PACMAN_CONF"
fi

if ! grep -q "chaotic-aur" "$PACMAN_CONF"; then
  echo "Enable chaotic-aur"
  # EOF to expand variables, 'EOF' without
cat << 'EOF' >> "$PACMAN_CONF" 
[chaotic-aur]
# Brazil
Server = https://lonewolf.pedrohlc.com/$repo/$arch
# USA
Server = https://builds.garudalinux.org/repos/$repo/$arch
Server = https://repo.kitsuna.net/$arch
# Netherlands
Server = https://chaotic.tn.dedyn.io/$arch
# Spain
Server = https://repo.jkanetwork.com/repo/$repo/$arch
# Germany
Server = http://chaotic.bangl.de/$repo/$arch
# Korea
Server = https://mirror.maakpain.kro.kr/garuda/$repo/$arch
EOF
fi
