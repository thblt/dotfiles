export GOPATH=$HOME/.go
export PATH="$PATH:/bin:/usr/bin:/usr/local/bin:${HOME}/.local/bin:$GOPATH/bin:$HOME/.cargo/bin"

if [ -e /usr/libexec/path_helper ]; then
    # Darwin/OSX utility to determine system path
    eval $(/usr/libexec/path_helper)
fi

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
    . $HOME/.nix-profile/etc/profile.d/nix.sh;
fi
