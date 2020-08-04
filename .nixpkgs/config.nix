{ pkgs }:
{
  allowUnfree = true;

  #chromium.enablePepperFlash = true;
  #oraclejdk.accept_license = true;

  packageOverrides = pkgs: rec
  {
    # myZotero = pkgs.callPackage "/home/thblt/.nixpkgs/zotero.nix" {};

    emacsPrime = (pkgs.emacs.override {
      srcRepo = true;
      withGTK2 = false;
      withGTK3 = false;
    }).overrideAttrs ({name, version, versionModifier, ...}: {
      name = "emacs-${version}${versionModifier}";
      version = "HEAD";
      versionModifier = "";
      src = builtins.fetchGit {
        url = "https://git.savannah.gnu.org/git/emacs.git";
        rev = "9dccaf8a5cdb10dae597345ec3741475477a7d97";
      };
      # configureFlags = configureFlags ++ ["--with-imagemagick"];
      # buildInputs = buildInputs ++ [ pkgs.imagemagick ];
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

          # ** Less common utilities

          bc
          graphviz
          udiskie

          # ** Crypto

          gnupg1compat
          pass
          pinentry

          # ** X11 and X utilities

          # *** Apps

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
          transmission-gtk
          vlc
          youtube-dl
          zotero

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
          cargo
          rustc
          rustfmt
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
