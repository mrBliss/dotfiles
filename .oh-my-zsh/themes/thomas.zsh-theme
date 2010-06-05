function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}

PROMPT='%{$fg[magenta]%}%n%{$reset_color%}@%{$fg[green]%}%m%{$reset_colors%}:%{$fg[yellow]%}$(collapse_pwd)%{$fg[cyan]%}➜  '

RPROMPT='%(?,%{$fg[green]%}:%)%{$reset_color%},%{$fg[red]%}:(%{$reset_color%})'
