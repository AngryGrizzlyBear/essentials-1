{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    byzanz                      # Screen recording software
    chromium                    # Browser
    gtk                         # For GTK+ themes
    htop                        # System monitor
    mplayer                     # Video player
    xdotool                     # Diagnostic tool for mouse and keyboard
  ];

  nixpkgs.config = {
    chromium = {
      enablePepperFlash = true;
      enablePepperPdf = true;
    };
  };
}
