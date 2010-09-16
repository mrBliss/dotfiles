# Cygwin specific options

# Use emacsclient
export EDITOR='emacsclient -t -a ""'
export ALTERNATE_EDITOR='emacs --daemon; emacsclient -t'

# Change colours
echo -e "\e]P0000000"
echo -e "\e]P1FF0000"
echo -e "\e]P2A6E32D"
echo -e "\e]P3FC951E"
echo -e "\e]P4C48DFF"
echo -e "\e]P5FA2573"
echo -e "\e]P667D9F0"
echo -e "\e]P7F2F2F2"

# Keychain
eval `keychain --eval --nogui -Q -q id_rsa`

# Git pull from other repositories
alias gitpulllin='git pull ssh://thomas@lucy/~ master'
alias gitpullwin='git pull file:///cygdrive/c/Users/Thomas/AppData/Roaming master'
alias gitpullcyg='git pull file:///home/Thomas/ master'

# Disable JLine for cljr
export DISABLE_JLINE="true"