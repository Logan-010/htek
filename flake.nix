{
  description = "Haskell flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };

          hs = pkgs.haskellPackages.override {
            overrides = hself: hsuper: {
              # Skip library test suites and their test-only dependencies.
              mkDerivation = args: hsuper.mkDerivation (args // { doCheck = false; });
              crypton = hself.callHackageDirect {
                pkg = "crypton";
                ver = "2.2.0";
                sha256 = "sha256-LFI49aRKsgxf5B4qixL/BA1eafRXZ+I0zPXbsxRznkM=";
              } { };
              text = hself.callHackage "text" "2.1.4" { };
              ram = hself.callHackageDirect {
                pkg = "ram";
                ver = "0.22.1";
                sha256 = "sha256-viwoo1u5jRGO2pTzQLlJ/PgkLmxeafTg8DDGTylpcPo=";
              } { };
              time = hself.callHackageDirect {
                pkg = "time";
                ver = "1.16.0.1";
                sha256 = "sha256-AEo/DrQAQr+QfWqHMVTh31jCj/crAWFHaNO5j65YJJM=";
              } { };
            };
          };
        in
        {
          default = hs.callCabal2nix "htek" ./. { };
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              # Compiler and build tools
              haskellPackages.ghc
              cabal-install

              # Language server
              haskell-language-server

              # Formatting
              ormolu
              haskellPackages.cabal-fmt

              # Linting
              hlint

              # Utilities
              haskellPackages.ghcid
              git
              pkg-config
            ];
          };
        }
      );
    };
}
