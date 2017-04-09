#!/bin/sh
cd ~/.xmonad

stack build
if [ $? -eq 0 ]; then
    xmonad --restart
    notify-send -u low XMonad 'Recompiled and restarted.'
else
    notify-send -u critical "XMonad recompilation failed" $(cat ~/.xmonad/xmonad.errors)
fi
