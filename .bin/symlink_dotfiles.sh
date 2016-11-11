#!/bin/bash

cd $HOME

declare -a things=(
    .Xresources
    .bin
    .emacs.d
    .gitconfig
    .gitignore_global
    .gtkrc-2.0
    .keynavrc
    .mbsyncrc
    .profile
    .tmux.conf
    .vim
    .xmonad
    .xsessionrc
    .zshrc
)

for t in "${things[@]}"; do
    ln -s .dotfiles/$t .
done

mkdir .config 2> /dev/null
cd .config
for t in `ls ../.dotfiles/.config`; do
	ln -s ../.dotfiles/.config/$t .
done
