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
    ssh -YCc blowfish -t s0202013@st.cs.kuleuven.be ssh -A s0202013@$1.cs.kotnet.kuleuven.be
}

# Connect to KULeuven from within Kotnet
kotssh() {
    ssh -YCc blowfish s0202013@$1.cs.kotnet.kuleuven.be
}

# Establish a SOCKS proxy on port 8080 for the given SSH host
sshproxy() {
    ssh -C -c blowfish -D 8080 -N "$1"
}

# Extract files
extract() {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2) tar xjf $1    ;;
            *.tar.gz)  tar xzf $1    ;;
            *.bz2)     bunzip2 $1    ;;
            *.rar)     unrar x $1    ;;
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


# The syntax is map COMMAND: ITEM1 ITEM2 ITEM3 ...
# The space following the colon is required. Space preceding it is optional.
map(){
    local command
    if [ $# -lt 2 ] || [[ ! "$@" =~ :[[:space:]] ]];then
        echo "Invalid syntax." >&2; return 1
    fi
    until [[ $1 =~ : ]]; do
        command="$command $1"; shift
    done
    command="$command ${1%:}"; shift
    for i in "$@"; do
        eval "${command//\\/\\\\} \"${i//\\/\\\\}\""
    done
}


# Kill emacs and remove server file in /tmp
killem() {
    killall emacs
    rm -rf /tmp/emacs*
}

# Use emacs' woman command as man viewer
women() {
    emacsclient -t -e "(woman \"$1\")'"
}

# Reattach a screen session. If multiple, presents a menu for
# choosing.
reattach() {
    OPTS=`screen -ls | grep "[0-9]\." | while read line ; do echo "$line" | sed -e 's/\s/_/g' ; done`

    case $(echo $OPTS | wc -w) in
        0)
            echo -e "\nNo screen sessions open\n"
            ;;
        1)
            SESSION=$OPTS
            echo -e "\nAttaching to only available screen"
            ;;
        *)
            echo -e "\nPick a screen session"
            select opt in $OPTS ; do
                SESSION=$opt
                break;
            done
            ;;
    esac

    screen -x $(echo $SESSION | sed -e 's/\..*//')

}

# Starts a simple (Python) webserver in the current directory on port
# 8000. Tries python2 first (ArchLinux), then python.
serve() {
    type -a python2 &>/dev/null && python2 -m SimpleHTTPServer || python -m SimpleHTTPServer
}

# Reset current directory to sensible permissions
fixperms() {
    find . -type d -print0 | xargs -0 chmod 755
    find . -type f -print0 | xargs -0 chmod 644
}

# Backup a remote svn repository.
# Usage: backup_svn svn+ssh://url.to.re/mote/repo /home/user/path/to/local/destination
# Example: backup_svn svn+ssh://repocs/repos/swop-groep-01 /home/thomas/Documents/SO/backup
backup_svn() {
    svnadmin create "$2"
    echo '#!/bin/bash' > "$2/hooks/pre-revprop-change"
    chmod +x "$2/hooks/pre-revprop-change"
    svnsync init "file:///$2" "$1"
    svnsync sync "file:///$2"
}

# Open a file as root in Emacs
sem() {
    filename=$1
    without_beg_slash="${1##/}"
    if [[ $without_beg_slash == $1 ]];then
        filename="${PWD%//}/$1"
    fi
    $EDITOR "/sudo:root@localhost:$1"
}

# Merge PDFs
pdfmerge() {
    pdftk $* cat output merged.pdf
}

# Copy over ssh-id (w/o dependencies)
authme() {
  ssh "$1" 'mkdir -p ~/.ssh && cat >> ~/.ssh/authorized_keys' < ~/.ssh/id_rsa.pub
}

# Create an SSH tunnel

ssh_tunnel() {
  if [ $# -eq 3 ]; then
    user=$1
    host=$2
    localPort=$3
    remotePort=$3
  elif [ $# -eq 4 ]; then
      user=$1
      host=$2
      localPort=$3
      remotePort=$4
  else
      echo -n "User: "; read user
      echo -n "host: "; read host
      echo -n "Distant host: "; read remotePort
      echo -n "Local port: "; read localPort
  fi
  ssh -N -f $user@$host -L ${localPort}:${host}:${remotePort}
}
