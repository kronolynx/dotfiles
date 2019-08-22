# Johann's dotfiles

These are my dotfiles :heart:

## Installation
First clone this repository
`git clone https://github.com/kronolynx/dotfiles.git`

### Symlink
For each file in dotfiles creates a symlink in the corresponding location at home and backup the file if already present into `backup-yyyymmddhhmmss` where `yyyy` is the year, `mm` month, `dd` day, `hhmmss` hours, minutes and seconds.

```bash
./symlink.sh             # uses default dir dotfiles
./symlink.sh path_to_dir # uses the given path
```


### Removal
There's a script to restore the backup which removes all the symlinks and copies dotfiles back into home.

```bash
./restore_dotfiles.sh             # uses default dir dotfiles
./restore_dotfiles.sh path_to_dir # uses the given path
```

the script has 2 flags which can be set to besides removing the symlinks, copy files not present in home and override existing files.

### Backup
The script does a back up of the dotfiles specified in a bash list, the scripts removes the dot part from the name e.g:
```bash
dotfiles=(
vimrc                   # to copy ~/.vimrc
config/xmobar/xmobar.hs # to copy ~/.config/xmobar/xmobar.hs
config/ranger           # to copy every file in ~/.config/ranger
)
```

## Desktop setup
TODO