# Identify computer
# This allows sharing this .profile across machines

is_mac=false;
is_linux=false;
is_headless=false;

if [ $HOSTNAME = "Rudiger.local" ]; then
	is_mac=true;
elif [ $HOSTNAME = "thblap.local" ]; then
	is_mac=:true;
elif [ $HOSTNAME = "namo.thb.lt" ]; then
	is_linux=true;
	is_headless=true;
elif [ $HOSTNAME = "k9.thb.lt" ]; then
	is_linux=true;
	is_headless=true;
else
	echo "!!!\tUnknown machine!";
fi;

# Read secrets (not in version control)

source ~/.secrets

if ! $is_linux; then
	export Qt5_ROOT="/Users/thblt/Documents/Coderies/lib/Qt/5.4/clang_64"
	export Qt5_DIR="${Qt5_ROOT}/lib/cmake/Qt5/"
	export _qt5gui_OPENGL_INCLUDE_DIR="/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.10.sdk/System/Library/Frameworks/OpenGL.framework/Headers"
	export BOOST_ROOT="/Users/thblt/Documents/Coderies/lib/Boost/boost_1_57_0/"
	export Boost_INCLUDE_DIR="/Users/thblt/Documents/Coderies/lib/Boost/boost_1_57_0/boost/"
fi;

# Variables

if $is_headless; then
    export EDITOR="vim -f"
elif $is_mac; then
    export EDITOR="mvim -f"
else
    export EDITOR="gvim -f"
fi;

# Aliases

alias e=${EDITOR}
alias irc="irssi -n thblt -c irc.freenode.net -w \"${_irc_password}\""
alias fuck='sudo $(history -p \!\!)' 
alias bd='. ~/.bd -s'

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
