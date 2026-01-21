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
          ];

          shellHook = ''
            export LD_LIBRARY_PATH="${pkgs.libedit}/lib:${pkgs.ncurses}/lib:$LD_LIBRARY_PATH"
            export LIBRARY_PATH="${pkgs.libedit}/lib:${pkgs.ncurses}/lib:$LIBRARY_PATH"
            echo "Racket + Readline (via libedit) environment loaded."
          '';
        };
      }
    );
}
