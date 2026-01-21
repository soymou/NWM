{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name = "racket-env";

  buildInputs = with pkgs; [
    racket
    libedit
    ncurses
  ];

  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.libedit}/lib:${pkgs.ncurses}/lib:$LD_LIBRARY_PATH"
    echo "Nix Workspace Manager environment loaded."
  '';
}
