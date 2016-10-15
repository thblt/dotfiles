# Don't do anything if not running interactively
[[ $- != *i* ]] && return

# Install antigen if needed
if [ ! -d "$HOME/.antigen" ]; then
	git clone https://github.com/zsh-users/antigen.git $HOME/.antigen
fi

source ~/.antigen/antigen.zsh

antigen use oh-my-zsh

antigen bundle common-aliases
antigen bundle fancy-ctrl-z
antigen bundle git
antigen bundle pip

antigen theme agnoster

antigen apply

# Variables

export DEFAULT_USER="thblt"; # ZSH themes uses this to simplify prompt.
export EDITOR="emacsclient -ca ''"
alias e="${EDITOR} --no-wait" # Shorthand 
alias ee="${EDITOR} --create-frame --no-wait" # Shorthand 

export GREP_COLOR=31
alias grep='grep --color=auto'

# Misc aliases

alias bc="bc -l"
alias fuck='sudo $(fc -ln -1)' # 'sudo $(history -p \!\!)' is bash-only
alias zbarcam="LD_PRELOAD=/usr/lib/libv4l/v4l1compat.so zbarcam" 
# SSH host
alias namo="ssh thblt@namo.thb.lt"
alias k9="ssh thblt@k9.thb.lt"

function mkcd() {
if mkdir -p "$@"
	then cd "$"@
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
	source `which virtualenvwrapper.sh`
fi

# Case-insensitive completion *only* when there's no case sensitive match.
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
source ~/.profile
