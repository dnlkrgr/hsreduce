with import <nixpkgs> { };
pkgs.haskellPackages.developPackage {
  root = ./.;
}
