# Cygwin specific options

# Use emacsclient
export EDITOR='emacsclient -t -a ""'
export ALTERNATE_EDITOR='emacs --daemon; emacsclient -t'

# Change colours
echo -e "\e]P0272822"
echo -e "\e]P1FF0000"
echo -e "\e]P2A6E22E"
echo -e "\e]P3FD971F"
echo -e "\e]P4AE81FF"
echo -e "\e]P5F92672"
echo -e "\e]P666D9EF"
echo -e "\e]P7F8F8F0"
echo -e "\e]P875715E"
# echo -e "\e]P949483E"
# echo -e "\e]PaE6DB74"

# Keychain
eval `keychain --eval --nogui -Q -q id_rsa`

# Git pull from other repositories
alias gitpulllin='git pull ssh://thomas@lucy/~ master'
alias gitpullwin='git pull file:///cygdrive/c/Users/Thomas/AppData/Roaming master'
alias gitpullcyg='git pull file:///home/Thomas/ master'

# Shorthand for cd'ing into AppData
alias cdapp='cd /cygdrive/c/Users/Thomas/AppData/Roaming/'