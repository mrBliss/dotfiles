# # No fancy prompt for dumb shells (TRAMP) via ssh
if [ "${TERM}" = "dumb" ];
then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    PS1='$ '
else
    # Enable 256 colors
    export TERM=xterm-256color

    # ls colors
    autoload colors; colors;
    export LSCOLORS="Gxfxcxdxbxegedabagacad"

     # Enable ls colors
    if [ "$DISABLE_LS_COLORS" != "true" ]
    then
    # Find the option for using colors in ls, depending on the
    # version: Linux or BSD
        ls --color -d . &>/dev/null 2>&1 && alias ls='ls --color=tty' || alias ls='ls -G'
    fi

    setopt no_beep
    setopt multios

    if [[ x$WINDOW != x ]]
    then
        SCREEN_NO="%B$WINDOW%b "
    else
        SCREEN_NO=""
    fi

    # git theming default: Variables for theming the git info prompt
    ZSH_THEME_GIT_PROMPT_PREFIX="git:(" # Prefix at the beginning of
                                        # the prompt, before the
                                        # branch name
    ZSH_THEME_GIT_PROMPT_SUFFIX=")"    # At the very end of the prompt
    ZSH_THEME_GIT_PROMPT_DIRTY="*"     # Text to display if the branch is dirty
    ZSH_THEME_GIT_PROMPT_CLEAN=""      # Text to display if the branch is clean

    # Apply theming defaults
    export PS1="%n@%m:%~%# "
    # Setup the prompt with pretty colors
    setopt prompt_subst
    # Load the theme
    source "$ZSH/themes/$ZSH_THEME.zsh-theme"

    # Don't display the RPROMPT in Emacs (ansi-term)
    if [ "$EMACS" != '' ];
    then
        RPROMPT=''
    fi

fi
