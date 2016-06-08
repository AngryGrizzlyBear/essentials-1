{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    rxvt_unicode-with-plugins         # Terminal emulator
    tmux                              # Terminal multiplexer
    tree                              # Better file tree
    zsh                               # ZShell
    xclip                             # Command line clipboard tool
  ];

  programs.zsh.enable = true;
}

