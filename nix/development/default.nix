{ config, pkgs, ... }:
{
  environment = {
    # LIBEV_LIBS is used to specify where libev is so opam can find it.
    variables = {
      EDITOR = "nvim";
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
      gnumake                       # GNU Make
      git                           # Source control
      libev                         # Event loop library
      libpng                        # PNG library
      m4                            # GNU Macro Processor
      neovim                        # Better vim
      ocaml                         # OCaml language
      ocamlPackages.findlib         # OCaml findlib
      ocamlPackages.ocaml_lwt       # OCaml LWT Library - Saves time otherwise spent fighting with opam
      ocamlPackages.ocaml_oasis     # OASIS Project Architecture Tool for OCaml
      ocamlPackages.utop            # Much better top-level for OCaml
      opam                          # OCaml package manager
      python3                       # Python 3
      python34Packages.setuptools   # Python setuptools
      silver-searcher               # Better than grep
      vim                           # Regular ole' vim
      wget                          # Standard GNU package for HTTP, FTP, etc file retrieval
    ];
  };
}
