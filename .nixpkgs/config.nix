{ pkgs }:
{
  allowUnfree = true;

  chromium = {
              enablePepperFlash = true;
  };

  packageOverrides = pkgs: rec {

                                thblt-alacritty = pkgs.stdenv.lib.overrideDerivation pkgs.alacritty (oldAttrs : {
                                    src = pkgs.fetchgit {
                                                         url = https://github.com/thblt/alacritty.git;
                                                         rev = "4fb4110e40af2a64793284c3394045fdb74af1a9";
                                                         sha256 =  "1z7vni4dwsa5zngi8xy6pqbl3rcsfvlq6k7v7v9841c7nrhjfnxzcd";
                                    };
                                  });

                                emacs = pkgs.stdenv.lib.overrideDerivation pkgs.emacs (oldAttrs : {
                                    version = "26.1RC1";
                                    src = pkgs.fetchurl {
                                                         url = "ftp://alpha.gnu.org/gnu/emacs/pretest/emacs-26.1-rc1.tar.xz";
                                                         sha256 = "6594e668de00b96e73ad4f168c897fe4bca7c55a4caf19ee20eac54b62a05758";
                                    };
                                    patches = [];
                                  });

                                # * Package list

                                all = with pkgs; buildEnv {
                                  name = "all";
                                  paths =
                                    [

                                     # ** Shell

                                     tmux


                                     # ** Common system utilities

                                     htop
                                     p7zip
                                     tree
                                     wget
                                     whois
                                     zip unzip

                                     # ** Less common system utilities

                                     bc
                                     graphviz
                                     pandoc
                                     udiskie

                                     # ** Crypto

                                     gnupg1compat
                                     gpa
                                     pass
                                     pinentry

                                     # ** X11 and X utilities

                                     arandr
                                     compton
                                     dmenu
                                     dunst
                                     feh
                                     libnotify
                                     lightlocker
                                     powerline-fonts
                                     scrot
                                     wmctrl
                                     xorg.xbacklight
                                     xorg.xev
                                     xsel

                                     # *** Apps

                                     alacritty
                                     browserpass
                                     chromium
                                     firefox-bin
                                     libreoffice
                                     transmission-gtk
                                     vlc

                                     # *** Fonts

                                     opensans-ttf
                                     roboto
                                     symbola

                                     # *** Icon/cursor themes

                                     gnome3.adwaita-icon-theme # For large mouse pointers

                                     # ** Emacs and friends

                                     emacs
                                     isync
                                     mu
                                     aspell
                                     aspellDicts.fr
                                     aspellDicts.en

                                     hunspell
                                     hunspellDicts.fr-any

                                     # ** Programming tools

                                     python36
                                     stack

                                     nix-prefetch-scripts

                                     ripgrep
                                     git
                                     meld

                                     # ** *TeX

                                     asymptote
                                     lyx
                                     texlive.biber
                                     texlive.combined.scheme-full
                                     ];
                                };
  };}
