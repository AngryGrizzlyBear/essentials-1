{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    byzanz                      # Screen recording software
    calibre                     # Ebook reader
    chromium                    # Browser
    gtk                         # For GTK+ themes
    htop                        # System monitor
    libreoffice                 # Libreoffice
    mplayer                     # Video player
    nox                         # Better nix package search
    unzip                       # Unzipper
    xdotool                     # Diagnostic tool for mouse and keyboard
  ];

  nixpkgs.config = {
    chromium = {
      enablePepperFlash = true;
      enablePepperPdf = true;
    };
  };
}
