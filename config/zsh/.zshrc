if [[ -e $HOME/.zsh/manjaro-zsh-config ]]; then
    source $HOME/.zsh/manjaro-zsh-config
fi

setopt PROMPT_SUBST
setopt interactivecomments

alias @="sudo"
alias cr="cp -rfv"
alias e="emacs -nw"
alias E="emacs"
alias ll="ls -laFs --color=auto"
alias rr="rm -irfv"

export PATH=$PATH:$HOME/.bin:$HOME/.local/bin:$HOME/.platformio/penv/bin
export EDITOR=/usr/bin/emacs
export BROWSER=/usr/bin/firefox

precmd() {
    echo
}

PROMPT='    %F{yellow}$(rndchar)%f %F{magenta}%Bλ%f%b '
RPROMPT='%F{blue}%B%5~%f%b   '

source $HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
