# Don't do anything if not running interactively
[[ $- != *i* ]] && return
# Run tmux if not already in it AND we're connected through SSH
# [[ -z "$TMUX" ]] && [[ ! -z $SSH_CONNECTION  ]] && exec tmux

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh
ZSH_THEME="agnoster"
plugins=(brew git pip)

export DEFAULT_USER="thblt"; # ZSH themes uses this to simplify prompt.

# Variables

export EDITOR="emacsclient -a ''"
alias e="${EDITOR} --no-wait" # Shorthand 
alias ee="${EDITOR} --create-frame --no-wait" # Shorthand 

export GREP_COLOR=31
alias grep='grep --color=auto'
export SSH_AUTH_SOCK=~/.ssh/ssh-agent.socket

# Misc aliases

alias fuck='sudo $(fc -ln -1)' # 'sudo $(history -p \!\!)' is bash-only
alias namo="ssh thblt@namo.thb.lt"
alias k9="ssh thblt@k9.thb.lt"
alias bc="bc -l"

function mkcd() {
if mkdir -p "$@"
	then cd "$@"
fi
}

# Moving between directories

alias bd=". bd -s"
alias ..="cd .."
alias ..2="cd ../.."
alias ..3="cd ../../.."
alias ..4="cd ../../../.."
alias ..5="cd ../../../../.."

# Virtualenvwrapper

if command -v virtualenvwrapper.sh > /dev/null; then
	export WORKON_HOME=~/.virtualenvs
	virtualenvwrapper.sh
fi

# Load oh my zsh
if [ ! -d "$HOME/.oh-my-zsh" ]; then
	git clone https://github.com/robbyrussell/oh-my-zsh .oh-my-zsh
fi
	
DISABLE_AUTO_UPDATE="true"
source $ZSH/oh-my-zsh.sh

# oh my zsh overrides. Must be *below* the source command

# Case-insensitive completion *only* when there's no case sensitive match.
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
source ~/.profile
