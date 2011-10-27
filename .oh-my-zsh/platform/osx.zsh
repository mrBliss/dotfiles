# Mac OS X specific options

# Needed for ssh (ProxyCommand)
export SHELL=/bin/zsh

# Use Cocoa Emacs
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
alias emd='/Applications/Emacs.app/Contents/MacOS/Emacs --daemon'

# Use emacsclient in a terminal window or nano
export EDITOR='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t'
export ALTERNATE_EDITOR='/Applications/Emacs.app/Contents/MacOS/Emacs --daemon; /Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t -a ""'

# Environment variables
export JAVA_HOME=/Library/Java/Home
export PATH=/usr/local/bin:/usr/local/sbin:~/.cljr/bin:$PATH
export NODE_PATH=/usr/local/lib/node:/usr/local/lib/node_modules

# Git pull from other repositories
alias gitpulllin='git pull ssh://thomas@lucy/~ master'
alias gitpullwin='git pull file:///Volumes/dotfiles/ master'

# A secure tunnel to a KUL workstation, to be used in public hot
# spots of the KUL
alias kultunnel='ssh -C -c blowfish -D 8080 -N gent.cs.kotnet.kuleuven.be'

# QuickLook alias (usage: ql filename; C-c)
alias ql='qlmanage -p 2>/dev/null'

# View 'man' in Preview.app
pman () {
    man -t "${1}" | open -f -a /Applications/Preview.app/
}

# View output in Preview.app with the preview command (usage: .. | preview)
alias preview='groff -Tps > /tmp/tmp.ps && open -a Preview /tmp/tmp.ps'


# Load autojump
source /usr/local/Cellar/autojump/17/etc/autojump.zsh

# homebrew aliases
alias br='brew'
alias bri='brew install'
alias brs='brew search'
alias bru='brew update'
alias brrm='brew uninstall'
alias bro='brew outdated'
alias brdeps='brew deps'
alias bruses='brew uses'
# TODO upgrade?


# Easy way to mount my samba share
mnt_win () {
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
    mount_smbfs "//Thomas:$password@192.168.1.3/$1" "/Volumes/$1"
  fi
}
