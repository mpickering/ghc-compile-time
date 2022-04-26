let pkgs = import <nixpkgs> {};
in with pkgs;
let
  my-python-packages = python-packages: with python-packages; [
    graph-tool
    networkx
    matplotlib
    pydot
    # other python packages you want
  ];
  python-with-my-packages = python3.withPackages my-python-packages;
in mkShell { buildInputs = [python-with-my-packages ffmpeg vlc]; }
