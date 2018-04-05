    {
  allowUnfree = true;

  chromium = {
    enablePepperFlash = true;
  };

      packageOverrides = pkgs_: with pkgs_; {

        # * Packages overrides
          # ** Emacs 25

          emacs = stdenv.lib.overrideDerivation emacs (oldAttrs : {
              src = fetchurl {
                url = "mirror://gnu/emacs/emacs-25.3.tar.xz";
                sha256 = "253ac5e7075e594549b83fd9ec116a9dc37294d415e2f21f8ee109829307c00b";
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

             zip
             unzip
             wget
             whois
             tree
             p7zip

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
             xorg.xev
             xsel

             # *** Apps

             chromium
             latest.firefox-bin
             browserpass
             libreoffice
             rxvt_unicode-with-plugins
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
      };
    }
