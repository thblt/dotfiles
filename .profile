# $PATH
if [ -e /usr/libexec/path_helper ]; then
	# Darwin/OSX utility to determine system path
	eval `/usr/libexec/path_helper`
fi
export PATH="${HOME}/.bin:${PATH}:${HOME}/.cabal/bin"
