{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    pulseaudioFull              # Audio
    spotify                     # Spotify
  ];

  hardware = {
    pulseaudio.enable = true;
  };
}
