## smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

## file rename magick
bindkey "^[m" copy-prev-shell-word

## jobs
setopt long_list_jobs

## pager
export PAGER=less
export LC_CTYPE=en_US.UTF-8

## terminal support
# case "$TERM" in
#   xterm*|rxvt*)
#     preexec () {
#       print -Pn "\e]0;%n@%m: $1\a"  # xterm
#     }
#     precmd () {
#       print -Pn "\e]0;%n@%m: %~\a"  # xterm
#     }
#     ;;
#   screen*)
#     preexec () {
#       local CMD=${1[(wr)^(*=*|sudo|ssh|-*)]}
#       echo -ne "\ek$CMD\e\\"
#       print -Pn "\e]0;%n@%m: $1\a"  # xterm
#     }
#     precmd () {
#       echo -ne "\ekzsh\e\\"
#       print -Pn "\e]0;%n@%m: %~\a"  # xterm
#     }
#     ;;
# esac

## fixme, i duplicated this in xterms - oops
# function title {
#   if [[ $TERM == "screen" ]]; then
#     # Use these two for GNU Screen:
#     print -nR $'\033k'$1$'\033'\\\

#     print -nR $'\033]0;'$2$'\a'
#   elif [[ $TERM == "xterm" || $TERM == "rxvt" ]]; then
#     # Use this one instead for XTerms:
#     print -nR $'\033]0;'$*$'\a'
#   fi
# }

function precmd {
  # title zsh "$PWD"
}

function preexec {
  emulate -L zsh
  local -a cmd; cmd=(${(z)1})
  # title $cmd[1]:t "$cmd[2,-1]"
}

function zsh_stats() {
  history | awk '{print $2}' | sort | uniq -c | sort -rn | head
}
