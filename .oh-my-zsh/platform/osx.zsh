# Mac OS X specific options

# Needed for ssh (ProxyCommand)
export SHELL=/bin/zsh

# Use Cocoa Emacs
alias em='emacsclient -t -a emacsserver'
alias emg='emacsclient -c -n -a emacsserver-gui'

# Not a typo, emacsclientt is a script that calls emacsclient -t
export EDITOR='emacsclientt'
export ALTERNATE_EDITOR='emacsserver'

# Environment variables
export JAVA_HOME=/Library/Java/Home
export PATH=~/.bin:~/.cabal/bin:/usr/local/bin:/usr/local/share/npm/bin:$PATH
export NODE_PATH=/usr/local/lib/node:/usr/local/lib/node_modules

# A secure tunnel to a KUL workstation, to be used in public hot
# spots of the KUL
alias kultunnel='ssh -C -c blowfish -D 8080 -N gent.cs.kotnet.kuleuven.be'

# QuickLook alias (usage: ql filename; C-c)
alias ql='qlmanage -p 2>/dev/null'

# View 'man' in Preview.app
pman() {
    man -t "${1}" | open -f -a /Applications/Preview.app/
}

# View output in Preview.app with the preview command (usage: .. | preview)
alias preview='groff -Tps > /tmp/tmp.ps && open -a Preview /tmp/tmp.ps'

# homebrew aliases
alias br='brew'
alias bri='brew install'
alias brs='brew search'
alias bru='brew update'
alias brup='brew upgrade'
alias brrm='brew uninstall'
alias bro='brew outdated'
alias brdeps='brew deps'
alias bruses='brew uses'


# Easy way to mount my samba share
mnt_win() {
    if [ -z "$2" ]; then
        USR="Thomas"
    else
        USR="$2"
    fi

    if [ -d "/Volumes/$1/" ]; then
        echo "Already mounted"
    else
        # Windows share on 192.168.1.3
        echo -n "Password for the share: "
        stty -echo
        read password
        stty echo
        echo ""
        if [ ! -d "/Volumes/$1" ]; then
            mkdir "/Volumes/$1"
        fi
        mount_smbfs "//$USR:$password@192.168.1.3/$1" "/Volumes/$1"
    fi

}

alias dhcpstart='sudo /bin/launchctl load -w /System/Library/LaunchDaemons/bootps.plist'
alias dhcpstop='sudo /bin/launchctl unload -w /System/Library/LaunchDaemons/bootps.plist'

# Fancy ls: ls++
alias lll='~/.bin/ls--/ls++'

# Seriously ncmpcpp?
alias nc=ncmpcpp

# Stream music with VLC
alias streamvlc='/Applications/VLC.app/Contents/MacOS/VLC -I ncurses http://localhost:8000'

# Load mac specific tmux config
alias tmux='tmux -f ~/.tmux.mac.conf'

# UTF-8 locale
export LANG="en_GB.UTF-8"
export LC_CTYPE="en_GB.UTF-8"
