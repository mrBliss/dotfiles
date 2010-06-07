# Linux specific options

# I don't like your Aptitude!
alias apti='sudo aptitude install'
alias aptrm='sudo aptitude remove'
alias aptut='sudo aptitude update'
alias aptud='sudo aptitude upgrade'
alias apts='aptitude search'

# Git pull from other repositories
alias gitpullwin='git pull file:///mnt/dotfiles/ master'
alias gitpullosx='git pull file:///mnt/dotfiles2/ master'

# Easy way to mount the OS X samba share on 192.168.1.9
mnt_osx () {
    echo -n "Password for the OS X share: "
    stty -echo
    read password
    stty echo
    echo ""
    if [ ! -d "/mnt/dotfiles2" ]; then
        sudo mkdir /mnt/dotfiles2
    elif [ -f "/mnt/dotfiles2/.emacs" ]; then
        echo "Already mounted"
    else
        sudo smbmount //192.168.1.9/Thomas /mnt/dotfiles2 -o username=Thomas,password=$password,uid=1000,mask=000
    fi
}

# Easy way to mount the Windows samba share on 192.168.1.3
mnt_win () {
    echo -n "Password for the Windows share: "
    stty -echo
    read password
    stty echo
    echo ""
    if [ ! -d "/mnt/dotfiles" ]; then
        sudo mkdir /mnt/dotfiles
    elif [ -f "/mnt/dotfiles/.emacs" ]; then
        echo "Already mounted"
    else
        sudo smbmount //192.168.1.3/dotfiles /mnt/dotfiles -o username=Thomas,password=$password,uid=1000,mask=000
    fi
}