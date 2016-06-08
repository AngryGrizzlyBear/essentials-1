{ config, pkgs, ... }:
{
  environment = { 
    variables = {
      EDITOR = "nvim";
    };

    systemPackages = with pkgs; [
      bar-xft                 # Lightweight xcb based bar
      cabal-install           # Haskell package installer
      curl                    # Tool for general HTTP, FTP, etc work 
      ghc                     # Haskell Compiler
      git                     # Source control
      neovim                  # Better vim
      python3                 # Python 3
      silver-searcher         # Better than grep
      vim                     # Regular ole' vim
      wget                    # Standard GNU package for HTTP, FTP, etc file retrieval
    ];
  };
}
