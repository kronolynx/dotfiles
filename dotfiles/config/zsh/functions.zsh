# ls after cd and if the directory doesn't exist try at home
function cd() {
    if ! test -d $@ && test -d "$HOME/$@"; then
      builtin cd "$HOME/$@" && ls -G
    else
      builtin cd "$@" && ls -G
    fi
}

# MISC: mkdir && cd
function mcd()
{
    test -z "$1" && echo mkcd: no path given && return
    test -d "$1" && print "mkcd: Directory $1 already exists"
    mkdir -p -- "$1"
    cd -- "$1"
}


# Coloured man pages using less as pager
man_color() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;36m") \
        man "$@"
}

# Hex to dec
function h2d () {
  HEXV=$(echo $1 | tr 'a-z' 'A-Z')
  echo "obase=10; ibase=16; $HEXV" | bc
}
# Dec to Hex
function d2h () {
  echo "obase=16; ibase=10; $1" | bc
}
# Dec to Bin
function d2b () {
  echo "obase=2; ibase=10; $1" | bc
}
# Bin to Dec
function b2d () {
  echo "obase=10; ibase=2; $1" | bc
}
# Hex to Bin
function h2b () {
  HEXV=$(echo $1 | tr 'a-z' 'A-Z')
  echo "obase=2; ibase=16; $HEXV" | bc
}
# Bin to Hex
function b2d () {
  echo "obase=16; ibase=2; $1" | bc
}

# Download mp3 files from youtube
function mp3download {
    if [ "$#" -eq 1 ]
    then
	youtube-dl -xi --audio-format mp3 -f bestaudio --prefer-ffmpeg -o "%(title)s.%(ext)s" $1
    elif [ "$#" -eq 2 ]
    then
	youtube-dl -xi -u $1 --audio-format mp3 -f bestaudio --prefer-ffmpeg -o "%(title)s.%(ext)s" $2
    else
	echo "Wrong number of arguments:\nvalid arguments\n\nURL\n\nor\n\nemail URL\n\n** URL of a playlist or video"
    fi
}

# Douwnload youtube videos
function videodownload {
    if [ "$#" -eq 1 ]
    then
	youtube-dl -i -o "%(title)s.%(ext)s" "$1a"
    elif [ "$#" -eq 2 ]
    then # user then video
	youtube-dl -iu "$1" -o "%(title)s.%(ext)s" $2
    else
	echo "Wrong number of arguments:\nvalid arguments\n\nURL\n\nor\n\nemail URL\n\n** URL of a playlist or video"
    fi
}


# removes commands containing argument from history or the last command
function rmh() {
    if [ $# -eq 0 ]; then
        sed -i '$d' ~/.zhistory # removes current commad
        sed -i '$d' ~/.zhistory # removes last line
    else
        sed -i "/$1/d" ~/.zhistory # removes lines that contain the given argument
        sed -i '$d' ~/.zhistory # removes current commad
    fi
}


# The following functions come from http://strcat.de/dotfiles/dot.zshfunctions
# super powerful ls
# MISC: Another ls(1)
function lr() {
    zparseopts -D -E S=S t=t r=r h=h U=U l=l F=F d=d
    local sort="sort -t/ -k2" # by name (default)
    local numfmt="cat"
    local long='s:[^/]* /::; s:^\./\(.\):\1:;' # strip detail
    local classify=''
    [ -n $F ]] && classify='/^d/s:$:/:; /^-[^ ]*x/s:$:*:;' # dir/ binary*
    [ -n $l ]] && long='s: /\./\(.\): \1:; s: /\(.\): \1:;' # show detail
    [ -n $S ]] && sort="sort -n -k5" # by size
    [ -n $r ]] && sort+=" -r" # reverse
    [ -n $t ]] && sort="sort -k6" && { [[ -n $r ]] || sort+=" -r" } # by date
    [ -n $U ]] && sort=cat # no sort, live output
    [ -n $h ]] && numfmt="numfmt --field=5 --to=iec --padding=6" # human fmt
    [ -n $d ]] && set -- "$@" -prune # don't enter dirs
    find "$@" -printf "%M %2n %u %g %9s %TY-%Tm-%Td %TH:%TM /%p -> %l\n" | $=sort | $=numfmt | sed '/^[^l]/s/ -> $//; '$classify' '$long
}

# search by file contents
# MISC: foobar
function g() {
    emulate -LR zsh
    local p=$argv[-1]
    [[ -d $p ]] && { p=$p/; argv[-1]=(); } || p=''
    grep --exclude "*~" --exclude "*.o" --exclude "tags" \
        --exclude-dir .bzr --exclude-dir .git --exclude-dir .hg --exclude-dir .svn \
        --exclude-dir CVS --exclude-dir RCS --exclude-dir _darcs \ --exclude-dir _build \
        -r -P ${@:?regexp missing} $p
}

lsd() { ls -d ${^~*:-*}(/) }

# printf '%-20s %s\n' ${(kv)options}
showoptions() {
    local k
    zmodload -i zsh/parameter
    for k in ${(ok)options}; do
        printf "%-20s\t%s\n" $k ${options[$k]}
    done
}

# Create a /overview/ of all available function()'s; the description for
# each funtion() *must* be the first line above the string `function'!
# Otherwise it wont work.
# Display all function()'s with her descriptions
function funlist()
{
    grep -B 1 "^function" $HOME/.zsh/zshfunctions | \
        grep -v "^\-\-$" | \
        awk '{ if(/^#/) { gsub(/^#[:space:]*/, ""); ht=$0 }; getline; gsub(/ ?\(\)/, ":"); printf("-> %-20s %s\n", $2, ht); }' | \
        sort -u -k 3
}

# show directory stack and ask for a dir to switch to
function dstack() {
    emulate -L zsh
    autoload -U colors
    local color=$fg_bold[blue]
    integer i=0
    dirs -p | while read dir
do
    local num="${$(printf "%-4d " $i)/ /.}"
    printf " %s  $color%s$reset_color\n" $num $dir
    (( i++ ))
done
integer dir=-1
read -r 'dir?Jump to directory: ' || return
(( dir == -1 )) && return
if (( dir < 0 || dir >= i ))
then
    echo d: no such directory stack entry: $dir
    return 1
fi
cd ~$dir
}

# grep(1)'ing $HISTFILE
histgrep () { fc -fl -m "*(#i)$1*" 1 | grep -i --color $1 }

# other version for "ls"
zls () {
    zmodload zsh/stat
    local -a st
    local name dev inode mode nlink uid gid rdev
    local size atmim mtime ctme blksize blocks link
    [[ $# -eq 0 ]] && set -- *
    zstat -nsLA st "$@"
    for name dev inode mode nlink uid gid rdev size atmim mtime ctme blksize blocks link in "$st[@]"
    do print -r -- "$mode $nlink ${(r:8:)uid} ${(r:8:)gid} ${(l:9:)size} $name"
    done
}

# Create a directory like "year-month-day" (i. e. 2007-07-16)
function mdate()
{
    mkdir `date +%F`
    cd `date +%F`
}

# eheheh :>
# MISC: lets sing ;-)
function littlelamb() {
    beep -f 10
    if [ $? = 0 ]; then
        echo 'Marry Had A Little Lamb'
        beep -f 466.2 -l 250 -D 20 -n -f 415.3 -l 250 -D 20 -n -f 370.0 -l 250 -D 20 -n -f 415.3 -l 250 -D 20 -n -f 466.2 -l 250 -r 2 -d 0 -D 20 -n -f 466.2 -l 500 -n -f 10 -l 20
        echo 'Little Lamb, Little Lamb'
        beep -f 415.3 -l 250 -r 2 -d 0 -D 20 -n -f 415.3 -l 500 -D 20 -n -f 466.2 -l 250 -D 20 -n -f 568.8 -l 250 -D 20 -n -f 568.8 -l 500 -n -f 10 -l 20
        echo 'Marry Had A Little Lamb'
        beep -f 466.2 -l 250 -D 20 -n -f 415.3 -l 250 -D 20 -n -f 370.0 -l 250 -D 20 -n -f 415.3 -l 250 -D 20 -n -f 466.2 -l 250 -r 2 -d 0 -D 20 -n -f 466.2 -l 250 -n -f 10 -l 20
        echo 'Whose Fleece Was White As Snow'
        beep -f 415.3 -l 250 -r 3 -D 20 -n -f 466.2 -l 250 -D 20 -n -f 415.3 -l 250 -D 20 -n -f 370.0 -l 500
    fi
}

# MISC: Display current directory as a 'tree'.
if [ ! -x  $(which tree > /dev/null 2>&1) ]; then
    function tree() { find . | sed -e 's/[^\/]*\//|----/g' -e 's/---- |/    |/g' | $PAGER }
fi

# MISC: rfc2396 url encoding (by Bart Schaefer)
function urlencode() {
    setopt localoptions extendedglob
    input=( ${(s::)1} )
    print ${(j::)input/(#b)([^A-Za-z0-9_.--version\'\(\)-])/%$(([##16]#match))}
}


# MISC: Globbing is simple? Sure .. See zshexpn(1) /Glob Qualifiers for details and come back ;)
function H-Glob()
{
    echo -e "
    /      directories
    .      plain files
    @      symbolic links
    =      sockets
    p      named pipes (FIFOs)
    *      executable plain files (0100)
    %      device files (character or block special)
    %b     block special files
    %c     character special files
    r      owner-readable files (0400)
    w      owner-writable files (0200)
    x      owner-executable files (0100)
    A      group-readable files (0040)
    I      group-writable files (0020)
    E      group-executable files (0010)
    R      world-readable files (0004)
    W      world-writable files (0002)
    X      world-executable files (0001)
    s      setuid files (04000)
    S      setgid files (02000)
    t      files with the sticky bit (01000)
    print *(m-1)          # Dateien, die vor bis zu einem Tag modifiziert wurden.
    print *(a1)           # Dateien, auf die vor einem Tag zugegriffen wurde.
    print *(@)            # Nur Links
    print *(Lk+50)        # Dateien die ueber 50 Kilobytes grosz sind
    print *(Lk-50)        # Dateien die kleiner als 50 Kilobytes sind
    print **/*.c          # Alle *.c - Dateien unterhalb von \$PWD
    print **/*.c~file.c   # Alle *.c - Dateien, aber nicht 'file.c'
    print (foo|bar).*     # Alle Dateien mit 'foo' und / oder 'bar' am Anfang
    print *~*.*           # Nur Dateien ohne '.' in Namen
    chmod 644 *(.^x)      # make all non-executable files publically readable
    print -l *(.c|.h)     # Nur Dateien mit dem Suffix '.c' und / oder '.h'
    print **/*(g:users:)  # Alle Dateien/Verzeichnisse der Gruppe >users<
    echo /proc/*/cwd(:h:t:s/self//) # Analog zu >ps ax | awk '{print $1}'<"
}

# MISC: Making the right decisions is hard :>
function helpme()
{
    print "Please wait.. i'll think about.."
    for i in 1 2 3; do echo -ne "."; sleep 0.3; done
    if [ $RANDOM -gt $RANDOM ]
    then
        print "Yes\!"
    else
        print "No\!"
    fi
}

# PROG: colorizing the output of make
if [[ -x ~/bin/makefilter ]]
then
    make() { command make "$@" |& makefilter }
fi

# SYS: /gitize/ $PWD
function gitize() { git init && git add . && git commit -a -m"Initial commit" && git gc }

# LOCA: print current settings of LC_*
function plocale()
{
    print LC_ALL=$LC_ALL
    print LANG=$LANG
    print LC_CTYPE=$LC_CTYPE
    print LC_NUMERIC=$LC_NUMERIC
    print LC_TIME=$LC_TIME
    print LC_COLLATE=$LC_COLLATE
    print LC_MONETARY=$LC_MONETARY
    print LC_MESSAGES=$LC_MESSAGES
    print LC_PAPER=$LC_PAPER
    print LC_NAME=$LC_NAME
    print LC_ADDRESS=$LC_ADDRESS
    print LC_TELEPHONE=$LC_TELEPHONE
    print LC_MEASUREMENT=$LC_MEASUREMENT
    print LC_IDENTIFICATION=$LC_IDENTIFICATION
}

# PROG: invoke this every time when u change .zshrc to recompile it.
function src()
{
    autoload -U zrecompile
    [ -f ~/.zshrc ] && zrecompile -p ~/.zshrc
    [ -f ~/.zcompdump ] && zrecompile -p ~/.zcompdump
    [ -f ~/.zcompdump ] && zrecompile -p ~/.zcompdump
    [ -f ~/.zshrc.zwc.old ] && command rm -f ~/.zshrc.zwc.old
    [ -f ~/.zcompdump.zwc.old ] && command rm -f ~/.zcompdump.zwc.old
    source ~/.zshrc
}


# PERL: Print the binary equivalent of a word (and back)
function bew() { perl -le 'print unpack "B*","'$1'"' }
function web() { perl -le 'print pack "B*","'$1'"' }

# PERL: Print the hex equivalent of a word (and back)
function hew() { perl -le 'print unpack "H*","'$1'"' }
function weh() { perl -le 'print pack "H*","'$1'"' }

# HELP: /Quick read/ help-files of Vim
function vimhelp () { vim -c "help $1" -c on -c "au! VimEnter *" }

# SEARCH: plap foo -- list all programs with prefix "foo":
function plap()
{
    if [[ $# = 0 ]]
    then
        echo "Usage:    $0 program"
        echo "Example:  $0 zsh"
        echo "Lists all occurrences of program in the current PATH."
    else
        ls -l ${^path}/*$1*(*N)
    fi
}

# SEARCH: function for searching for acronyms and stuff
function w00t()
{
    case "$1" in
        -s) =grep -i "^$2" ~/.abk.txt
            ;;
        -a) echo "$2">> ~/.abk.txt
            ;;
        *)
            echo "Usage $0 [-s] [-a]"
            echo "		-s (Acronym)  Search for an acronym in the db"
            echo "		-a (String)   Add an acronym+description to the db"
            echo "Examples:"
            echo "			$0 -s STFU"
            echo "			$0 -a STFU - Shut the fuck up"
    esac
}

# PROG: aDisplay the size of all dirs at $PWD
function dirsize()
{
    if [ -z $1 ]; then
        dir="."
    else
        dir=$1
    fi
    find $dir -type d -maxdepth 1 -mindepth 1 -exec du -sh '{}' \; 2>/dev/null \
        | perl -pe "s/\t.*\/(.*)$/\t$(echo '\033[01;32m')\1$(echo '\033[0m')/gi"
    echo
    echo "Total: " $(du -sh $dir 2>/dev/null | awk '{print $1}')
}

# MISC: Use vim to convert plaintext to HTML
function 2html() { vim -u NONE -n -c ':syntax on' -c ':so $VIMRUNTIME/syntax/2html.vim' -c ':wqa' $1 > /dev/null 2> /dev/null }

# ARCHIVE: Create a tarball from given directory
function create-archive()
{
    local archive_name
    archive_name="$1.tar.gz"
    archive_name=${archive_name/\//}
    tar cvfz "$archive_name" "$1"
    echo "Created archive $archive_name"
}

# SEARCH: Show the path from a symlink to its ultimate source.
function folsym() {
    if [[ -e $1 || -h $1 ]]; then
        file=$1
    else
        file=`which $1`
    fi
    if
        if [[ -e $file || -L $file ]]; then
            if [[ -L $file ]]; then
                echo `ls -ld $file | perl -ane 'print $F[7]'` '->'
                folsym `perl -le '$file = $ARGV[0];
                $dest = readlink $file;
                if ($dest !~ m{^/}) {
                    $file =~ s{(/?)[^/]*$}{$1$dest};
                } else {
                $file = $dest;
            }
            $file =~ s{/{2,}}{/}g;
            while ($file =~ s{[^/]+/\.\./}{}) {
                ;
            }
            $file =~ s{^(/\.\.)+}{};
            print $file' $file`
        else
            ls -d $file
        fi
    else
        echo $file
    fi
}

# MISC: Use 'view' to read manpages, if u want colors, regex - search, ... like vi(m).
function vman() { man $1 | col -b | view -c 'map q :q<CR>' -c 'hi StatusLine ctermbg=green| set ft=man nomod nolist' - }

# SEARCH: search for various types or README file in dir and display them in $PAGER
function readme()
{
    local files
    files=(./(#i)*(read*me|lue*m(in|)ut)*(ND))
    if (($#files))
    then
        $PAGER $files
    else
        print 'No README files. Please lart \$MAINTAINER!'
    fi
}

# MISC: add a "load-level"
function load()
{
    LOAD=`print ${${$(=uptime)[11]}:gs/,//}`
    case $LOAD {
        0.0*)     llevel="relax.."   ;;
        0.[123]*) llevel="normal."   ;;
        0.[456]*) llevel="verspannt"   ;;
        0.[789]*) llevel="WTF?!"  ;;
        1*)       llevel="dangerous!" ;;
        2*)       llevel="HELP ME!!!111!"   ;;
        [3-9]*)   llevel="He's dead jim!"   ;;
        *)        return         ;;
    }
    echo "$llevel"
}


# MISC: a quick hack for GNU Emacs && emacsclient
function e()
{
    if [ "$1" = "" ]; then
        echo "No file specified you have, think before you must."
    else
        if emacsclient -n "$1" >/dev/null 2>&1; then
            echo "Alrite, opened $1 in the Holy Emacs."
        else
            echo "There's no Holy Emacs running here.. starting.."
            exec emacs "$1" &
        fi
    fi
}

# nconvert()    convert numbers between different bases
function nconvert() {
    if (( ${#@} < 2 )) ; then
        printf 'usage: nconvert CONVERSION_CODE NUMBER\n'
        printf '    eg.: nconvert dh 42\n'
        printf '      returns '\''0x2A'\'' - decimal 42 converted to hex.\n'
        printf ' available conversion chars: d, h, o and b\n'
        return 2
    fi

    setopt localoptions noksharrays
    local from=${1[1]} to=${1[2]}
    local -i from_c to_c num=$2

    case ${from} in
        (h) (( from_c = 16 )) ;;
        (d) (( from_c = 10 )) ;;
        (o) (( from_c =  8 )) ;;
        (b) (( from_c =  2 )) ;;
        (*)
            printf 'Unknown from code: (%s)\n' ${from}
            return 1
            ;;
    esac
    case ${to} in
        (h) (( to_c = 16 )) ;;
        (d) (( to_c = 10 )) ;;
        (o) (( to_c =  8 )) ;;
        (b) (( to_c =  2 )) ;;
        (*)
            printf 'Unknown to code: (%s)\n' ${to}
            return 1
            ;;
    esac

    echo $(( [#${to_c}] ${from_c}#${num} ))
    return 0
}

# List all files, long format, colorized, permissions in octal
function la(){
    ls -l  "$@" | awk '
    {
        k=0;
        for (i=0;i<=8;i++)
            k+=((substr($1,i+2,1)~/[rwx]/) *2^(8-i));
            if (k)
                printf("%0o ",k);
                printf(" %9s  %3s %2s %5s  %6s  %s %s %s\n", $3, $6, $7, $8, $5, $9,$10, $11);
            }'
}
