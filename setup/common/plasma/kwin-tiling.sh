git clone https://github.com/kwin-scripts/kwin-tiling.git
(cd kwin-tiling/
plasmapkg2 --type kwinscript -i .

mkdir -p ~/.local/share/kservices5
ln -s ~/.local/share/kwin/scripts/kwin-script-tiling/metadata.desktop ~/.local/share/kservices5/kwin-script-tiling.desktop
)

rm -rf kwin-tiling
