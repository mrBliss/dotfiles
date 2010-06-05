# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# Super user
alias s='sudo'

# Show history
alias history='fc -l 1'

# List direcory contents
alias l='ls -lAh'
alias ll='ls -lAh'
alias sl=ls # often screw this up

# Directory navigation
alias .='pwd'
alias ..='cd ..'
alias ...='cd ../..'
alias cd..='cd ..'
alias cd...='cd ../..'
alias cd....='cd ../../..'
alias cd.....='cd ../../../..'
alias cd/='cd /'

# Directory creation and deletion
alias md='mkdir -p'
alias rd=rmdir

# Correction
setopt correct_all
alias man='nocorrect man'
alias mv='nocorrect mv'
alias mysql='nocorrect mysql'
alias mkdir='nocorrect mkdir'
alias gist='nocorrect gist'
alias heroku='nocorrect heroku'

# Emacs
alias em='emacsclient -t -a ""'

# Make things safer with these aliases
alias mv='mv -i'
alias cp='cp -i'