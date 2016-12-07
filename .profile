# $PATH
if [ -e /usr/libexec/path_helper ]; then
	# Darwin/OSX utility to determine system path
	eval `/usr/libexec/path_helper`
fi
export GOPATH=$HOME/.go
export PATH="${PATH}:${HOME}/.bin:${HOME}/.local/bin:$GOPATH/bin"
