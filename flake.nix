{
  description = "A Haskell port of the Rich library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskellPackages;
        projectName = "h-rich";
      in
      {
        packages.default = haskellPackages.callCabal2nix projectName ./h-rich { };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.default ];
          buildInputs = with haskellPackages; [
            haskell-language-server
            cabal-install
            ghcid
          ] ++ (with pkgs; [
            git
          ]);
          withHoogle = true;
        };
      });
}
