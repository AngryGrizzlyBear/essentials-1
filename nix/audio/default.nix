{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    pulseaudioFull              # Audio	
  ];

  hardware = {
    pulseaudio.enable = true;
  };
}
