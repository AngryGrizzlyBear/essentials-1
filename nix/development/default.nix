{ config, pkgs, ... }:
{
  environment = {

    # OCaml has trouble finding pthread. So we set PTHREAD_LIBS to give iet a heads up
    # ** currently doesnt set PTHREAD_LIBS at all **
    variables = {
      EDITOR = "nvim";
      PTHREAD_CFLAGS = "-I${pkgs.glibc}/include/";
      PTHREAD_LIBS = "-L${pkgs.glibc}/lib/";
    };

    systemPackages = with pkgs; [
      bar-xft                       # Lightweight xcb based bar
      cabal-install                 # Haskell package installer
      coreutils                     # GNU coreutils
      cmake                         # CMake tool
      curl                          # Tool for general HTTP, FTP, etc work
      freetype                      # Library to render fonts required by some packages
      gcc                           # GNU Compiler Collection
      ghc                           # Haskell compiler
      glibc                         # Glibc
      gnumake                       # GNU Make
      git                           # Source control
      libev                         # Event loop library
      libpng                        # PNG library
      m4                            # GNU Macro Processor
      neovim                        # Better vim
      ocaml                         # OCaml language
      ocamlPackages.ocaml_batteries # OCaml batteries included
      ocamlPackages.merlin          # OCaml editor enhancement
      ocamlPackages.findlib         # OCaml findlib
      ocamlPackages.lambdaTerm      # Lambda Term Library
      ocamlPackages.ocaml_lwt       # OCaml LWT Library - Saves time otherwise spent fighting with opam
      ocamlPackages.ocaml_oasis     # OASIS Project Architecture Tool for OCaml
      ocamlPackages.utop            # Much better top-level for OCaml
      opam                          # OCaml package manager
      pkgconfig
      python3                       # Python 3
      python34Packages.setuptools   # Python setuptools
      silver-searcher               # Better than grep
      vim                           # Regular ole' vim
      wget                          # Standard GNU package for HTTP, FTP, etc file retrieval
    ];
  };
}
