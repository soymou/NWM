{
  description = "Nix Workspace Manager Development Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            racket
            libedit
            ncurses
            manix  # For NixOS options search
          ];

          shellHook = ''
            export LD_LIBRARY_PATH="${pkgs.libedit}/lib:${pkgs.ncurses}/lib:$LD_LIBRARY_PATH"
            echo "Nix Workspace Manager environment loaded."
          '';
        };
      }
    );
}
