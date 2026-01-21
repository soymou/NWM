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
            gtk3
            gsettings-desktop-schemas
          ];

          shellHook = ''
            export LD_LIBRARY_PATH="${pkgs.libedit}/lib:${pkgs.ncurses}/lib:$LD_LIBRARY_PATH"
            export XDG_DATA_DIRS="${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}:$XDG_DATA_DIRS"
            echo "Nix Workspace Manager environment loaded."
          '';
        };
      }
    );
}
