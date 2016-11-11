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
add curl dnsutils fbset file less powertop psmisc sudo wget whois wipe
# Debian/APT utilities
add apt-file aptitude netselect-apt
# Shell
add fish zsh
# Encryption keys management
add keychain
# Stuff for programming
add build-essential git python-setuptools python-pip python python3 virtualenvwrapper nodejs npm
# Printing!
add cups
add printer-driver-splix # For the Samsung
add printer-driver-brlaser
# Auto mounting
add udiskie
# Compression
add zip unzip

#
# Desktop environment
# 

# Xorg
add xserver-xorg-core xserver-xorg-input-libinput x11-xserver-utils \
     mesa-utils mesa-va-drivers mesa-vdpau-drivers \
	 libgl1-mesa-dri
add xfonts-base # XMonad decorations fail without this.

add gnome-themes-standard gtk2-engines-pixbuf # pixbuf required for Adwaita on GTK2 apps

add desktop-base # Wallpaper and Plymouth themes

# Display manager & session locker
add lightdm-gtk-greeter light-locker \
# Dbus
add dbus-x11
# Notification system
add libnotify-bin
# Make some noise!
add alsa-base alsa-utils
# Window manager
add xmonad libghc-xmonad-contrib-dev libghc-xmonad-contrib-doc libghc-dbus-dev
# Compositing manager
add compton 
# Misc desktop utilities
add dunst feh gpa scrot suckless-tools synapse wmctrl xsel
add barcode qrencode zbar-tools
# Font manager and fonts
add font-manager fonts-roboto

#
# Desktop (not necessarily X) tools
#

# Terminal emulator and multiplexer
add rxvt-unicode-256color tmux
# Browsers
add chromium chromium-l10n firefox-esr firefox-esr-l10n-fr lynx torbrowser-launcher 
# Text editors (with CLI versions as well)
add emacs24 emacs25 vim vim-gtk
# Text editor tools
add stylish-haskell
# ... with spell checking
add aspell aspell-fr
# Email client
add maildir-utils mu4e isync
# File managers
add mc
# Media player
add vlc
# Bad office suite
add libreoffice-calc libreoffice-writer libreoffice-gtk3 libreoffice-l10n-fr

# And the texlive monster and tex utilities
add texlive-base texlive-lang-french texlive-xetex lyx

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

apt install $packages
apt autoremove --purge $removed_packages
