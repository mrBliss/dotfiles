function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}

PROMPT='%{$fg[green]%}%m%{$reset_colors%}:%{$fg[magenta]%}$(collapse_pwd)%{$fg[cyan]%}> '

if [ "$USER" = "s0202013" ]
then
        RPROMPT='%{$fg[cyan]%}%n %(?,%{$fg[green]%}:%)%{$reset_color%},%{$fg[red]%}:(%{$reset_color%})'
else
        RPROMPT='%{$fg[blue]%}%n %(?,%{$fg[green]%}:%)%{$reset_color%},%{$fg[red]%}:(%{$reset_color%})'
fi
