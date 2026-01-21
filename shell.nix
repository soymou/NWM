{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name = "racket-env";

  buildInputs = with pkgs; [
    racket
    libedit
    ncurses
  ];

  shellHook = ''
    echo "Welcome to the Nix Workspace Manager Dev Environment"
    
    # Ensure Racket can find libedit and ncurses for readline support
    export LD_LIBRARY_PATH="${pkgs.libedit}/lib:${pkgs.ncurses}/lib:$LD_LIBRARY_PATH"
    export LIBRARY_PATH="${pkgs.libedit}/lib:${pkgs.ncurses}/lib:$LIBRARY_PATH"
  '';
}
