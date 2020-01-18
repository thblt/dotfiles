{ pkgs }:
{
  allowUnfree = true;

  #chromium.enablePepperFlash = true;
  #oraclejdk.accept_license = true;

  packageOverrides = pkgs: rec
  {
    # myZotero = pkgs.callPackage "/home/thblt/.nixpkgs/zotero.nix" {};

    emacsPrime = with pkgs; stdenv.lib.overrideDerivation
      (pkgs.emacs.override {
        srcRepo = true;
        withGTK2 = false;
        withGTK3 = false;
      }) (attrs: rec {
        name = "emacs-${version}${versionModifier}";
        version = "HEAD";
        versionModifier = "";
        src = builtins.fetchGit {
          url = "https://git.savannah.gnu.org/git/emacs.git";
          rev = "d36adb544d984b91c70f6194da01344e4b2b6fc9";
        };
        autoconf = true;
        automake = true;
        texinfo = true;

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
          unrar
          wget
          whois
          zip unzip

          # ** Less common system utilities

          bc
          graphviz
          # pandoc
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
          xclip
          xorg.xbacklight
          xorg.xev
          xsel

          # *** Apps

          alacritty
          browserpass
          chromium
          evince
          firefox-bin
          gimp
          hugo
          jabref
          imagemagick
          inkscape
          libreoffice
          gnome3.nautilus
          qrencode
          scantailor-advanced
          scribus
          thunderbird
          #tor-browser-bundle
          transmission-gtk
          vlc
          youtube-dl
          zotero

          # *** Fonts

          #liberation-fonts
          opensans-ttf
          roboto
          symbola

          # *** Icon/cursor themes

          gnome3.adwaita-icon-theme # For large mouse pointers

          # ** Emacs and friends

          emacsPrime
          isync
          aspell
          aspellDicts.fr
          aspellDicts.en

          hunspell
          hunspellDicts.fr-any

          # ** Programming tools

          # *** Language-independent

          gitFull
          gitAndTools.git-hub
          meld
          nix-prefetch-scripts
          ripgrep

          # *** Go

          go

          # *** Haskell

          haskellPackages.apply-refact
          hlint
          haskellPackages.hoogle
          stack

          # *** Python

          python36

          # *** Lisps

          racket
          chez

          # *** Rust

          rustup
          # ^ provides rustfmt

          # ** *TeX

          asymptote
          lyx
          #texlive.biber
          texlive.combined.scheme-full
        ];
    };
  };
}
