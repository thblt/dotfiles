with (import <nixpkgs> {});

let ghc = haskell.packages.ghc802;
in
stdenv.mkDerivation {
  name = "xmonad-testing";

  buildInputs = [
    # GHC and friends:
    (ghc.ghcWithPackages (p: with p; [
      cabal-install
      packdeps
    ]))

   pkgconfig
    autoconf
    xorg.libX11
    xorg.libXext
    xorg.libXft
    xorg.libXinerama
    xorg.libXpm
    xorg.libXrandr
    xorg.libXrender

    gnupg # sign tags and releases
    rsync # Needed by xmonad-web/gen-docs.sh
  ];

  LD_LIBRARY_PATH = "${xorg.libXinerama}/lib:${xorg.libXext}/lib:${xorg.libX11}/lib:${xorg.libXrandr}/lib";

}
