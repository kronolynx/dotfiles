git clone https://github.com/kronolynx/kwin-extra-keys.git
(cd kwin-extra-keys/
plasmapkg2 --type kwinscript -i .

mkdir -p ~/.local/share/kservices5
ln -s ~/.local/share/kwin/scripts/kwin-extra-keys/metadata.desktop ~/.local/share/kservices5/kwin-extra-keys.desktop
)

rm -rf kwin-extra-keys
