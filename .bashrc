# This file is used:
#	- in vim with gui, where the default shell is bash and not zsh, because zsh 
#	  requires much more terminal emulation capacity than what MacVim (at least) can
#     provide.
#   - It is sourced from .zshrc so path is set in only one place.
if [ -e /usr/libexec/path_helper ]; then
	eval `/usr/libexec/path_helper`
fi
export PATH="${HOME}/bin:${PATH}"
