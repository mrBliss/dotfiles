function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}

if [[ "$SSH_CONNECTION" != "" ]]
then
        PROMPT='%{$fg_bold[magenta]%}%m%f:%{$fg_no_bold[magenta]%}$(collapse_pwd)%{$fg[cyan]%}> '
else
        PROMPT='%{$fg[green]%}%m%f:%{$fg[magenta]%}$(collapse_pwd)%{$fg[cyan]%}>%f '
fi

if [ "$USER" = "s0202013" ]
then
        RPROMPT=' %(?,%{$fg[green]%}:%)%f,%{$fg[red]%}:(%f)'
else
        RPROMPT=' %(?,%{$fg[green]%}:%)%f,%{$fg[red]%}:(%f)'
fi
