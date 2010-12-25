# Cygwin specific options

# Use emacsclient
export EDITOR='emacsclient -t -a ""'
export ALTERNATE_EDITOR='emacs --daemon; emacsclient -t'

# Needed for ssh (ProxyCommand)
export SHELL=/usr/bin/zsh

# Change colours
echo -e "\e]P0000000"
echo -e "\e]P1FF0000"
echo -e "\e]P2A6E22E"
echo -e "\e]P3FD971F"
echo -e "\e]P40074E8"
echo -e "\e]P5F92672"
echo -e "\e]P666D9EF"
echo -e "\e]P7F8F8F0"
echo -e "\e]P8444444"
echo -e "\e]P9960050"
echo -e "\e]PA61CE3C"
echo -e "\e]PBFBDE2D"
echo -e "\e]PC6D9CBE"
echo -e "\e]PDAE59AB"
echo -e "\e]PE89BDFF"
echo -e "\e]PFCCCDDD"

# Keychain
eval `keychain --eval --nogui -Q -q id_rsa`

# Git pull from other repositories
alias gitpulllin='git pull ssh://thomas@lucy/~ master'
alias gitpullwin='git pull file:///cygdrive/c/Users/Thomas/AppData/Roaming master'
alias gitpullcyg='git pull file:///home/Thomas/ master'

# Shorthand for cd'ing to the 'other' ~
alias cdapp='cd /cygdrive/c/Users/Thomas/AppData/Roaming/'

# Shorthand for cd'ing to the desktop
alias cddesk='cd /cygdrive/c/Users/Thomas/Desktop/'
