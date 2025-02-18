{
  description = "A flake for Haskell development";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    hsPkgs = pkgs.haskellPackages;
  in {
    # A package that compiles main.hs into an executable
    packages.${system}.haskell-app = pkgs.stdenv.mkDerivation {
      pname = "haskell-app";
      version = "0.1.0";
      src = ./.;
      buildInputs = [ hsPkgs.ghc ];
      # The build phase compiles main.hs using GHC.
      phases = [ "buildPhase" "installPhase" ];
      buildPhase = ''
        ghc -O2 -o haskell-app main.hs
      '';
      installPhase = ''
        mkdir -p $out/bin
        cp haskell-app $out/bin/
      '';
    };

    # Set the default package to the haskell-app executable.
    defaultPackage.${system} = self.packages.${system}.haskell-app;

    # A dev shell that provides GHC for interactive development.
    devShells.${system}.haskell = pkgs.mkShell {
      buildInputs = [ hsPkgs.ghc ];
    };
  };
}
