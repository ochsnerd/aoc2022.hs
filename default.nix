{ pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (haskellPackages: with haskellPackages; [
      haskell-language-server
      split
      search-algorithms
      hashable
      unordered-containers
    ]))
  ];
}
