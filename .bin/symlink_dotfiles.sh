#!/bin/bash

cd $HOME

declare -a things=(
    .Xresources
    .bin
	.cig	
    .emacs.d
    .gitconfig
    .gitignore_global
    .gtkrc-2.0
    .keynavrc
    .mbsyncrc
    .profile
    .tmux.conf
    .vim
    .Xmodmap.`hostname`
    .xmonad
    .xsessionrc
    .zshrc
)

for t in "${things[@]}"; do
    if [ -e .dotfiles/$t ]; then
        ln -s .dotfiles/$t .
    else
        echo "No source file \"$t\", skipping."
    fi
done

mkdir .config 2> /dev/null
cd .config
for t in `ls ../.dotfiles/.config`; do
	ln -s ../.dotfiles/.config/$t .
done
