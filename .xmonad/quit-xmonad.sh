#!/bin/sh

# Nicely terminates all windows, and, if everything's fine, terminate XMonad.

wait_for_termination() {
	# Returns 0 after the window ID provided as $1 doesn't
	# exists anymore (ie, doesn't appear in the output of
	# wmctrl -l). If window still exists after $2 seconds,
	# returns -1.

    end=$(($SECONDS+$2))
    
	while [ $SECONDS -lt $end ]; do
#        wmctrl -l | grep "^$1\s"
		if [[ -z `wmctrl -l | grep "^$1\s"` ]]; then
##			notify-send -u low "Window <$1> was terminated.";
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
#	killall xmonad-x86_64-linux
	kill $PPID
else
	notify-send -u normal "Quit failed" "Relaunching termination script."
    ~/.xmonad/quit-xmonad.sh 
fi
