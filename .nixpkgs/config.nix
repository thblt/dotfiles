{
  allowUnfree = true;

  packageOverrides = pkgs_: with pkgs_; {

    emacs = stdenv.lib.overrideDerivation emacs (oldAttrs : {
        src = fetchurl {
          url = "mirror://gnu/emacs/emacs-25.2.tar.xz";
          sha256 = "59b55194c9979987c5e9f1a1a4ab5406714e80ffcfd415cc6b9222413bc073fa";
        };
        patches = [];
      });

    all = with pkgs; buildEnv {
      name = "all";
      paths = [
               zsh
               tmux

               tree

               emacs
               isync
               mu

               gnupg
               gpa
               pass
               pinentry



               arandr
               fzf
               ripgrep
               git
               meld

               dmenu
               dunst
               libnotify               xsel

               zip
               unzip
               p7zip
               bc
               firefoxWrapper
               chromium
               python27
               python3
               gcc
               libreoffice
               vlc
               rxvt_unicode-with-plugins
               wmctrl

	       feh

	       compton

               krita
               gimp
               blender

			stack
               ];
    };
  };
}
