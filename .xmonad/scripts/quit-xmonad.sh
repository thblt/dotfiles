#!/bin/bash

# This script is used to properly quit/shutdown/etc when using XMonad
# or any other standalone window manager.

# Nicely terminates all windows, and, if everything's fine, terminate
# XMonad.

wait_for_termination() {
	# Returns 0 after the window ID $1 doesn't exists anymore (ie,
	# doesn't appear in the output of wmctrl -l). If window still
	# exists after $2 seconds, returns -1.

    end=$(($SECONDS+$2))
    
	while [ $SECONDS -lt $end ]; do
		if [[ -z `wmctrl -l | grep "^$1\s"` ]]; then
			return 0;
		fi
        sleep .05
	done
    return -1
}

for win in $(wmctrl -l | awk '{print $1}'); do
	wmctrl -ic $win;
	wait_for_termination $win 10;
    if [[ $? != 0 ]]; then
        notify-send -u critical "Cannot quit" "At least an application couldn't be closed."
        exit -1
    fi
done


# Last sanity check
if [[ -z `wmctrl -l` ]]; then
	# kill $PPID doesn't seem to work on Debian (?).  This works, but may be dangerous in a multi-user environment.
	killall xmonad-x86_64-linux
else
	notify-send -u normal "Quit failed" "Relaunching termination script."
	~/.xmonad/quit-xmonad.sh 
fi
