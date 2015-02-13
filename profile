# Identify computer
# This allows sharing this .profile across machines
if [ $HOSTNAME = "Rudiger.local" ]; then
	is_mac = 1;
	is_rudiger = 1
elif [ $HOSTNAME = "thblap.local" ]; then
	is_mac=1;
else
	echo "!!!\tUnknown machine!";
fi;

# Read secrets (not in version control)

source ~/.secrets
:
if ! ${is_linux}; then
	export Qt5_ROOT="/Users/thblt/Documents/Coderies/lib/Qt/5.4/clang_64"
	export Qt5_DIR="${Qt5_ROOT}/lib/cmake/Qt5/"
	export _qt5gui_OPENGL_INCLUDE_DIR="/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.10.sdk/System/Library/Frameworks/OpenGL.framework/Headers"
	export BOOST_ROOT="/Users/thblt/Documents/Coderies/lib/Boost/boost_1_57_0/"
	export Boost_INCLUDE_DIR="/Users/thblt/Documents/Coderies/lib/Boost/boost_1_57_0/boost/"
fi;

# Variables

export EDITOR="gvim -w"
export tkacz="/Users/thblt/Documents/Coderies/workspace/Tkacz"

# Aliases

alias irc="irssi -n thblt -c irc.freenode.net -w \"i${_irc_password}\""

if [ $is_rudiger ]; then
	alias namo="ssh 192.168.0.13"
	
else
	alias namo="ssh namo.thb.lt" 
fi

alias k9="ssh k9.thb.lt"
alias fuck='sudo $(history -p \!\!)' 

# mkcd

function mkcd() { 
if mkdir "$@" 
	then cd "$@"
fi
}

# bd

alias bd=". bd -s"

# ../../..…

alias ..="cd .."
alias ..2="cd ../.."
alias ..3="cd ../../.."
alias ..4="cd ../../../.."
alias ..5="cd ../../../../.."

# $PATH

PATH="/Library/Frameworks/Python.framework/Versions/3.4/bin:${PATH}"
PATH="~/Documents/Coderies/bin:${PATH}"
PATH="${Qt5_ROOT}/bin/:${PATH}"
export PATH
