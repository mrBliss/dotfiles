# Wiki Lookups
wiki() {
    dig +short txt $1.wp.dg.cx
}

# mkdir, cd into it
mkcd() {
    mkdir -p "$*"
    cd "$*"
}

# Connect to KULeuven
kulssh() {
    ssh -A -X -C -c blowfish -t s0202013@st.cs.kuleuven.be ssh -A s0202013@$1.cs.kotnet.kuleuven.be
}

# Extract files
extract() {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2) tar xjf $1    ;;
            *.tar.gz)  tar xzf $1    ;;
            *.bz2)     bunzip2 $1    ;;
            *.rar)     rar x $1      ;;
            *.gz)      gunzip $1     ;;
            *.tar)     tar xf $1     ;;
            *.tbz2)    tar xjf $1    ;;
            *.tgz)     tar xzf $1    ;;
            *.zip)     unzip $1      ;;
            *.Z)       uncompress $1 ;;
            *)         echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# Usage: zipls <archive>
# Description: view archive without unpack
zipls() {
    if [ -f $1 ]; then
        case $1 in
            *.tar.bz2)     tar -jtf $1 ;;
            *.tar.gz)      tar -ztf $1 ;;
            *.tar)         tar -tf $1  ;;
            *.tgz)         tar -ztf $1 ;;
            *.zip)         unzip -l $1 ;;
            *.jar)         unzip -l $1 ;;
            *.rar)         rar vb $1   ;;
            *)             echo "'$1' cannot be viewed via show-archive()" ;;
        esac
    else
        echo "'$1' is not a valid archive"
    fi
}

# Usage: zip <file> (<type>)
# Smart archive creator
zipit() {
    emulate -L zsh
    if [[ -n $2 ]] ; then
        case $2 in
            tgz | tar.gz)   tar -zcvf$1.$2 $1 ;;
            tbz2 | tar.bz2) tar -jcvf$1.$2 $1 ;;
            tar.Z)          tar -Zcvf$1.$2 $1 ;;
            tar)            tar -cvf$1.$2  $1 ;;
            gz | gzip)      gzip           $1 ;;
            bz2 | bzip2)    bzip2          $1 ;;
            *)
                echo "Error: $2 is not a valid compression type"
                ;;
        esac
    else
        zipit $1 tar.gz
    fi
}


# Back up a file. Usage "bu filename.txt"
bu() {
    cp $1 ${1}-`date +%Y%m%d%H%M`.backup
}

# grep for a process
psg() {
    FIRST=`echo $1 | sed -e 's/^\(.\).*/\1/'`
    REST=`echo $1 | sed -e 's/^.\(.*\)/\1/'`
    ps aux | grep "[$FIRST]$REST"
}

# cd, then ls
cl() {
    cd $1
    ls
}

# Go N directories up
up() {
    local arg=${1:-1};
    while [ ${arg} -gt 0 ]; do
        cd .. >&/dev/null;
        arg=$((${arg} - 1));
    done
}


# map() takes a command and a list of files, then runs the command on each
# file. Functional zsh!

map() {
    f=$argv[1]
    argv[1]=""
    echo $@ | xargs --no-run-if-empty --max-args=1 ${=f}
}

# Kill emacs and remove server file in /tmp
killem () {
    killall emacs
    rm -rf /tmp/emacs*
}

# Use emacs' woman command as man viewer
women () {
    emacsclient -t -e "(woman \"$1\")'"
}