{ pkgs }:
{
  allowUnfree = true;

  #chromium.enablePepperFlash = true;
  #oraclejdk.accept_license = true;

  packageOverrides = pkgs: rec
  {
    myZotero = pkgs.callPackage "/home/thblt/.nixpkgs/zotero.nix" {};
    emacs = pkgs.emacs.overrideAttrs (oldAttrs: rec {
      name = "emacs-${version}${versionModifier}";
      version = "26.2";
      versionModifier = "";
      src = pkgs.fetchurl {
        url = "mirror://gnu/emacs/${name}.tar.xz";
        sha256 = "151ce69dbe5b809d4492ffae4a4b153b2778459de6deb26f35691e1281a9c58e";
      };
      patches = [];
    });

    # * Package list

    all = with pkgs; buildEnv
    {
      name = "all";
      paths =
        [

          # ** Shell

          tmux

          # ** Common system utilities

          acpi lm_sensors
          bind
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
          xclip
          xorg.xbacklight
          xorg.xev
          xsel

          # *** Apps

          alacritty
          browserpass
          chromium
          firefox-bin
          gimp
          hugo
          jabref
          imagemagick
          inkscape
          libreoffice
          scantailor-advanced
          thunderbird
          tor-browser-bundle
          transmission-gtk
          vlc
          myZotero

          # *** Fonts

          #liberation-fonts
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
          #texlive.biber
          texlive.combined.scheme-full
        ];
    };
  };
}
