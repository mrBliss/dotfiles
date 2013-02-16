function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}

if [[ "$SSH_CONNECTION" != "" ]]
then
    # No lambda: often no support for unicode
    PROMPT='%{$fg[yellow]%}%m%f:%{$fg[magenta]%}$(collapse_pwd) %{$fg[blue]>%}%f '
else
    PROMPT='%{$fg_bold[cyan]%}%m%f:%{$fg[magenta]%}$(collapse_pwd) %{$fg_no_bold[blue]λ%}%f '
fi

# RPROMPT=' %(?,%{$fg_bold[green]%}:%)%f,%{$fg_bold[red]%}:(%f) %{$reset_color%}'

