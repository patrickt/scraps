# Append to a history file (~/.zhistory)
setopt appendhistory
# Auto-cd to directories if the only input provided is a directory.
setopt autocd
# Auto-expand substitution strings in the prompt
setopt prompt_subst
# Enable auto push/pop tracking, a la a browser
setopt auto_pushd

# Set up the vcs_info module to work with git, svn, and git-svn
zstyle ':vcs_info:*' stagedstr '%F{28}●'
zstyle ':vcs_info:*' unstagedstr '%F{11}●'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' formats ' [%F{green}%b%c%u%F{blue}] '

# Load VCS info
autoload -Uz vcs_info

PROMPT='%F{blue}${vcs_info_msg_0_}%F{blue}%(?/%F{blue}/%F{red})% %F{white}%% '
export RPS1="(%~)"
export EDITOR="/Applications/Emacs.app/Contents/MacOS/Emacs"

alias g="git"
alias o="open"
alias pbpatch="pbpaste | patch"
alias kx="killall -KILL Xcode"
alias :r='$(history -p !!)'

export CLICOLOR=1

if [[ `uname` = "Linux" ]] {
     bindkey "^[[1;3C" forward-word
     bindkey "^[[1;3D" backward-word
     export EDITOR="emacsclient"
     export GOPATH="/home/patrick/.go.d"
     alias ls="ls --color"
}

precmd() {
  if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
        zstyle ':vcs_info:*' formats ' [%F{green}%b%c%u%F{blue}] '
    } else {
        zstyle ':vcs_info:*' formats ' [%F{green}%b%c%u%F{red}●%F{blue}] '
    }
    vcs_info
    print -Pn "\e]0;$PWD\a";
}

export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin

HISTFILE=~/.zhistory
HISTSIZE=1000
SAVEHIST=1000

if [ -f /usr/local/erl/activate ]
then source /usr/local/erl/activate
fi

if [ -f /usr/bin/keychain ]
then keychain id_rsa
fi

export CC=clang

