# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# Super user
alias s='sudo'

# Show history
alias history='fc -l 1'

# List direcory contents
alias l='ls -lAFh'
alias ll='ls -lAFh'
alias sl='ls -lAFh'

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
alias rmr='rm -r'

# Emacs
alias em='emacsclient -t -a ""'

# Make things safer with these aliases
alias mv='mv -i'
alias cp='cp -i'

# Directory size
alias size='du -hs'

# Find the largest directories
alias space='find . -maxdepth 1 -mindepth 1 -print0  | xargs -0 -n1 du -ks | sort -rn | head -16 | cut -f2 | xargs -i du -hs {}'

# Corrections
setopt correct_all
alias man='nocorrect man'
alias mv='nocorrect mv'
alias mysql='nocorrect mysql'
alias mkdir='nocorrect mkdir'
alias gist='nocorrect gist'

