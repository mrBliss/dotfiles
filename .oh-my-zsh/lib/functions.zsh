# Wiki Lookups
wiki() {
    dig +short txt $1.wp.dg.cx
}

# mkdir, cd into it
mkcd () {
    mkdir -p "$*"
    cd "$*"
}

# Connect to KULeuven
kulssh () {
    ssh -t s0202013@st.cs.kuleuven.be ssh s0202013@$1.cs.kotnet.kuleuven.be
}

# Extract files
extract () {
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

# Back up a file. Usage "bu filename.txt"
bu () {
    cp $1 ${1}-`date +%Y%m%d%H%M`.backup
}

# grep for a process
psg () {
    FIRST=`echo $1 | sed -e 's/^\(.\).*/\1/'`
    REST=`echo $1 | sed -e 's/^.\(.*\)/\1/'`
    ps aux | grep "[$FIRST]$REST"
}

# cd, then ls
cl () {
    cd $1
    ls
}