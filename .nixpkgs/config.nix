{ pkgs }:
{
  allowUnfree = true;

  #chromium.enablePepperFlash = true;
  #oraclejdk.accept_license = true;

  packageOverrides = pkgs:
  rec {
  #   emacs = pkgs.stdenv.lib.overrideDerivation pkgs.emacs (oldAttrs : {
  #     version = "26.1RC1";
  #     src = pkgs.fetchurl {
  #       url = "ftp://alpha.gnu.org/gnu/emacs/pretest/emacs-26.1-rc1.tar.xz";
  #       sha256 = "6594e668de00b96e73ad4f168c897fe4bca7c55a4caf19ee20eac54b62a05758";
  #     };
  #     patches = [];
  #   });

    # * Package list

      all = with pkgs; buildEnv {
      name = "all";
      paths =
        [

        # ** Shell

        tmux

        # ** Common system utilities

        acpi lm_sensors
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
        gimp
        jabref
        imagemagick
        inkscape
        libreoffice
        scantailor-advanced
        transmission-gtk
        vlc
        zotero

        # *** Fonts

        liberation-fonts
        opensans-ttf
        roboto
        symbola

        # *** Icon/cursor themes

        gnome3.adwaita-icon-theme # For large mouse pointers

        # ** Emacs and friends

        emacs
        isync
        aspell
        aspellDicts.fr
        aspellDicts.en

        hunspell
        hunspellDicts.fr-any

        # ** Programming tools

        # *** Language-independent

        git
        meld
        nix-prefetch-scripts
        ripgrep

        # *** Go

        go

        # *** Haskell

        haskellPackages.apply-refact hlint haskellPackages.hoogle stack
        python36

        # *** Python

        # *** Lisps

        racket
        chez

        # *** Rust

        cargo
        rustfmt

        # ** *TeX

        asymptote
        lyx
        texlive.biber
        texlive.combined.scheme-full
      ];
    };
  };
}
