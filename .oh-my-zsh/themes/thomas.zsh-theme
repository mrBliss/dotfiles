function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}

if [[ "$SSH_CONNECTION" != "" ]]
then
        PROMPT='%{$fg_bold[magenta]%}%m%{$reset_colors%}:%{$fg_no_bold[magenta]%}$(collapse_pwd)%{$fg[cyan]%}> '
else
        PROMPT='%{$fg[green]%}%m%{$reset_colors%}:%{$fg[magenta]%}$(collapse_pwd)%{$fg[cyan]%}> '
fi

if [ "$USER" = "s0202013" ]
then
        RPROMPT='%{$fg[cyan]%}%n %(?,%{$fg[green]%}:%)%{$reset_color%},%{$fg[red]%}:(%{$reset_color%})'
else
        RPROMPT='%{$fg[blue]%}%n %(?,%{$fg[green]%}:%)%{$reset_color%},%{$fg[red]%}:(%{$reset_color%})'
fi
