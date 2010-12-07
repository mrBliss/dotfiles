# Mac OS X specific options

# Needed for ssh (ProxyCommand)
export SHELL=/bin/zsh

# Use Cocoa Emacs
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'

# Use emacsclient in a terminal window or nano
export EDITOR='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t -a ""'
export ALTERNATE_EDITOR='/Applications/Emacs.app/Contents/MacOS/Emacs --daemon; emacsclient -t'

# Environment variables
export JAVA_HOME=/Library/Java/Home/
export PATH=/usr/local/bin:/usr/local/sbin:~/.cljr/bin:$PATH

# Git pull from other repositories
alias gitpulllin='git pull ssh://thomas@lucy/~ master'
alias gitpullwin='git pull file:///Volumes/dotfiles/ master'

# QuickLook alias (usage: ql filename; C-c)
alias ql='qlmanage -p 2>/dev/null'

# View 'man' in Preview.app
pman () {
    man -t "${1}" | open -f -a /Applications/Preview.app/
}

# View output in Preview.app with the preview command (usage: .. | preview)
alias preview='groff -Tps > /tmp/tmp.ps && open -a Preview /tmp/tmp.ps'

# Easy way to mount my samba share
mnt_win () {
        if [ -d "/Volumes/dotfiles/" ]; then
            echo "Already mounted"
        else
        # Windows share on 192.168.1.3
            echo -n "Password for the share: "
            stty -echo
            read password
            stty echo
            echo ""
            if [ ! -d "/Volumes/dotfiles" ]; then
                mkdir /Volumes/dotfiles
            fi
            mount_smbfs //Thomas:$password@192.168.1.3/dotfiles /Volumes/dotfiles
        fi
    }

# Man with colors
export MANPAGER=most