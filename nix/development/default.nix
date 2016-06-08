{ config, pkgs, ... }:
{
  environment = {
    variables = {
      EDITOR = "nvim";
    };

    systemPackages = with pkgs; [
      bar-xft                     # Lightweight xcb based bar
      cabal-install               # Haskell package installer
      curl                        # Tool for general HTTP, FTP, etc work
      freetype                    # Library to render fonts required by some packages
      gcc                         # GNU Compiler Collection
      ghc                         # Haskell compiler
      git                         # Source control
      libpng                      # PNG library
      neovim                      # Better vim
      python3                     # Python 3
      python34Packages.setuptools # Python setuptools
      silver-searcher             # Better than grep
      vim                         # Regular ole' vim
      wget                        # Standard GNU package for HTTP, FTP, etc file retrieval
    ];
  };
}
