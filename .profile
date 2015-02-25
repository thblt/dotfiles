# Identify computer
# This allows sharing this .profile across machines

is_mac=false;
is_linux=false;
is_headless=false;

HOSTNAME=

if [[ `hostname` == "Rudiger.local" ]]; then
	is_mac=true;
elif [[ `hostname` = "thblap.local" ]]; then
	is_mac=true;
elif [[ `hostname` = "namo.thb.lt" ]]; then
	is_linux=true;
	is_headless=true;
elif [[ `hostname` = "k9.thb.lt" ]]; then
	is_linux=true;
	is_headless=true;
else
	echo "!!!\tUnknown machine!";
fi;

# Read secrets (not in version control)

source ~/.secrets

# Variables

if ! $is_headless; then
    export DEFAULT_USER="thblt";
fi;

if $is_headless; then
    export EDITOR="vim "
elif $is_mac; then
    export EDITOR="mvim"
else
    export EDITOR="gvim"
fi;

export GREP_COLOR=31
alias grep='grep --color=auto'

# Aliases

alias e=${EDITOR}
alias irc="irssi -n thblt -c irc.freenode.net -w \"${_irc_password}\""
alias fuck='sudo $(history -p \!\!)' 
alias bd='. ~/.bd -s'

alias namo="ssh thblt@namo.thb.lt"
alias k9="ssh thblt@k9.thb.lt"

# mkcd

function mkcd() { 
if mkdir "$@" 
	then cd "$@"
fi
}

# bd

alias bd="~/.dotfiles/bd -s"

alias ..="cd .."
alias ..2="cd ../.."
alias ..3="cd ../../.."
alias ..4="cd ../../../.."
alias ..5="cd ../../../../.."

# path stuff

if ! $is_linux; then
	PATH="/Library/Frameworks/Python.framework/Versions/3.4/bin:${PATH}"
	PATH="~/Documents/Coderies/bin:${PATH}"
	PATH="${Qt5_ROOT}/bin/:${PATH}"
	export PATH
fi;

