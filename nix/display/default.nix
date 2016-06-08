{ config, pkgs, ... }:
{
  imports = [
    ./terminal.nix
  ];

  environment.systemPackages = with pkgs; [
    arandr                    # GUI frontend for xrandr
    compton                   # Windowing system
    dmenu                     # Application launcher
    feh                       # Wallpaper manager
    python3Packages.udiskie   # Automatically mount USB drives
    scrot                     # Screenshot software
    unclutter                 # Mouse hider
    xlibs.xmodmap             # keyboard reconfiguration
    xmonad-with-packages      # Tiling window manager
    xorg.xbacklight           # Backlight manager
  ];

  # services.xserver.videoDriver = 

  services = {
    udisks2.enable = true;    

    xserver = {
      enableTCP = false;
      exportConfiguration = false;
      enable = true;
      layout = "us";

      displayManager = {
        sessionCommands = ''
          xset r rate 300 30
          unclutter -grab &
          udiskie &
	  feh --bg-fill ~/bgs/dark_night.jpg &
          compton --config /dev/null &
        '';
      };

      desktopManager = {
        xterm.enable = false;
        xfce.enable = false;
      };

      windowManager = {
        xmonad.enable = true;
        xmonad.enableContribAndExtras = true;
        default = "xmonad";
      };
    };
  };
}
