{ pkgs ? import
    (fetchTarball {
      url =
        "https://github.com/NixOS/nixpkgs/archive/db6e089456cdddcd7e2c1d8dac37a505c797e8fa.tar.gz";
      sha256 = "sha256-i/QZI5qM4eYw2wWGd/OjruBx53FqUPq+sdAWmyIQ0ws=";
    })
    { }
, ghc ? "ghc8104"
}:
pkgs.mkShell {
  nativeBuildInputs = [
    (pkgs.haskell.packages."${ghc}".ghcWithPackages (ps:
      with ps; [
        ghc-paths
        haskell-language-server
        th-desugar
      ]))
  ];
}
