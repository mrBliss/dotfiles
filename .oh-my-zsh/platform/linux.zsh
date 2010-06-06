# Linux specific options

alias apti='sudo aptitude install'
alias aptrm='sudo aptitude remove'
alias aptut='sudo aptitude update'
alias aptud='sudo aptitude upgrade'
alias apts='aptitude search'

# Git pull from other repositories
alias gitpullwin='git pull file:///mnt/dotfiles/ master'
alias gitpullosx='git pull file:///mnt/dotfiles2/ master'

# Easy way to mount my samba shares
mnt_samba () {
    # Windows share on 192.168.1.3
    # the OS X share on 192.168.1.9
    if [ "$1" = "win" ]; then
        digit=3
        dotdigit=''
        share='dotfiles'
    elif [ "$1" = "osx" ]; then
        digit=9
        dotdigit=2
        share='Thomas'
    fi

    if [ -z "$digit" ]; then
        echo "Choose between win and osx"
    else
        echo -n "Password for the share: "
        stty -echo
        read password
        stty echo
        echo ""
        sudo smbmount //192.168.1.$digit/$share /mnt/dotfiles$dotdigit -o username=Thomas,password=$password,uid=1000,mask=000
    fi
}