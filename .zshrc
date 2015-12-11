# Load .profile for common setings

source .profile

# Identify computer
# This allows sharing this .zshrc across machines

is_mac=false;
is_linux=false;
is_headless=false;

if [[ `hostname` == Rudiger* ]]; then
	is_mac=true;
elif [[ `hostname` == thblap* ]]; then
	is_mac=true;
elif [[ `hostname` == namo* ]]; then
	is_linux=true;
	is_headless=true;
elif [[ `hostname` == k9* ]]; then
	is_linux=true;
	is_headless=true;
else
	echo "!!!\tUnknown machine!";
fi;

# Read secrets (not in version control)
source ~/.secrets

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh
ZSH_THEME="agnoster"
plugins=(brew git pip)

export DEFAULT_USER="thblt"; # ZSH themes uses this to simplify prompt.

# Variables

# if $is_headless; then
if [ `command -v emacs` ]; then
	export EDITOR="emacsclient --no-wait -a emacs"
else
	export EDITOR="vim"
fi;
alias e=${EDITOR} # Shorthand 

export GREP_COLOR=31
alias grep='grep --color=auto'

# Aliases

alias fuck='sudo $(fc -ln -1)' # 'sudo $(history -p \!\!)' is bash-only
alias namo="ssh thblt@namo.thb.lt"
alias k9="ssh thblt@k9.thb.lt"
alias bc="bc -l"
# mkcd

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

export WORKON_HOME=~/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh


DISABLE_AUTO_UPDATE="true"
source $ZSH/oh-my-zsh.sh

# oh my zsh overrides. Must be *below* the source command

# Case-insensitive completion *only* when there's no case sensitive match.
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
