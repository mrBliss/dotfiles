# Cygwin specific options

# Use emacsclient
export EDITOR='emacsclient -t -a ""'
export ALTERNATE_EDITOR='emacs --daemon; emacsclient -t'

# Change colours
echo -e "\e]P0000000"
echo -e "\e]P1FF0000"
echo -e "\e]P2A6E22E"
echo -e "\e]P3FD971F"
echo -e "\e]P40074e8"
echo -e "\e]P5F92672"
echo -e "\e]P666D9EF"
echo -e "\e]P7F8F8F0"
echo -e "\e]P8444444"
echo -e "\e]P9960050"
echo -e "\e]Pa61ce3c"
echo -e "\e]Pbfbde2d"
echo -e "\e]Pc6d9cbe"
echo -e "\e]Pdcc0000"
echo -e "\e]Pe89bdff"
echo -e "\e]Pfcccddd"


# Keychain
eval `keychain --eval --nogui -Q -q id_rsa`

# Git pull from other repositories
alias gitpulllin='git pull ssh://thomas@lucy/~ master'
alias gitpullwin='git pull file:///cygdrive/c/Users/Thomas/AppData/Roaming master'
alias gitpullcyg='git pull file:///home/Thomas/ master'

# Shorthand for cd'ing into AppData
alias cdapp='cd /cygdrive/c/Users/Thomas/AppData/Roaming/'