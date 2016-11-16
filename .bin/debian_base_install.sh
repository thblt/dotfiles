#!/bin/sh

add() {
    packages="$packages $@"
}

remove() {
    removed_packages="$removed_packages $@"
}

packages=""

# Drivers and hardware
add firmware-linux-free firmware-linux-nonfree smartmontools
# Misc system utilities
add file less man-db powertop psmisc sudo wipe
# Networking fundamentals
add ca-certificates curl dnsutils lynx rsync wget whois
# Debian/APT utilities
add apt-file aptitude debian-keyring netselect-apt
#                     ^ Need this to verify release ISOs
# Shell and terminal multiplexing
add fish tmux zsh
#   ^ Better out of the box than zsh, cool for root or new users.
# Compression
add zip unzip

###############################################################
# At this point, the base system is complete.                 #
# Need to run this in a server? Wrap everything below in a if #
###############################################################

# Base programming
add build-essential git python-setuptools python-pip python python3 virtualenvwrapper nodejs npm
# Some deps
add cmake libclang-dev sqlite3
#                      ^ for helm-dash/counsel-dash 
#   ^-----^ emacs irony-mode

#
# Desktop environment
# 

# Tools for computers with an attached keyboard and screen
add fbset 

# Xorg
add xserver-xorg-core xserver-xorg-input-libinput x11-xserver-utils \
     mesa-utils mesa-va-drivers mesa-vdpau-drivers \
	 libgl1-mesa-dri

add xfonts-base # XMonad decorations fail without this.
add gnome-themes-standard gtk2-engines-pixbuf # pixbuf required for Adwaita on GTK2 apps
add desktop-base # Wallpaper and Plymouth themes
# Window and compositing manager
add xmonad libghc-xmonad-contrib-dev libghc-xmonad-contrib-doc libghc-dbus-dev
add compton 
# Display manager & session locker
add lightdm-gtk-greeter light-locker \ 
add policykit-1 # LightDM depends on this to handle shutdown/reboot/etc   
# Dbus
add dbus-x11
# Notification system
add dunst libnotify-bin
# Audio
add alsa-base alsa-utils

# Printing
add cups
add printer-driver-brlaser
# add printer-driver-splix # For the Samsung - doesn't work

# Misc X-desktop utilities
add feh scrot suckless-tools synapse udiskie wmctrl xsel
# Font manager and fonts
add font-manager fonts-dejavu fonts-roboto

#
# Desktop (not necessarily X) tools
#

# Terminal emulator
add rxvt-unicode-256color
# Browsers
add chromium chromium-l10n firefox-esr firefox-esr-l10n-fr torbrowser-launcher 
# Text editors (with CLI versions as well)
add emacs24 emacs25 vim vim-gtk
# Text editor tools
add stylish-haskell
# Spell checking
add aspell aspell-fr
# Email client
add maildir-utils mu4e isync
# File managers
add mc pcmanfm
# Media player
add vlc
# Crypto
add gpa keychain
# Bad office suite
add libreoffice-calc libreoffice-writer libreoffice-gtk3 libreoffice-l10n-fr
# The texlive monster and tex utilities
add texlive-base texlive-lang-french texlive-xetex lyx
# Misc
add barcode qrencode zbar-tools
add qalculate-gtk

if [ "anna" = `hostname` ]; then
    >&2 echo "I'm running on Anna."
	add acpid
	add network-manager rfkill # TODO move to a "laptop" group
	add task-laptop # Should have been installed automatically
	add xserver-xorg-video-intel  
	add firmware-brcm80211 # Wifi
    # TODO apt suggests for powertop: cpufrequtils laptop-mode-tools
elif [ "rudiger" = `hostname` ]; then
    >&2 echo "I'm running on Rudiger."
	add xserver-xorg-video-radeon
fi

if [ "$1" = "list" ]; then
    echo $packages | tr " " "\n" | sort -u
    exit 0
fi

echo
echo "I'm about to install"
echo "--------------------"
echo
echo $packages
echo
echo "I'm about to remove and PURGE"
echo "-----------------------------"
echo $removed_packages
echo
echo "Please review the FULL output above and press enter"
echo "to proceed or C-c to abort."
read dummy

apt autoremove --purge $removed_packages
apt install $packages
