{ config, pkgs, ... }: 
{
  environment.systemPackages = with pkgs; [
    xorg.xf86inputsynaptics
  ];

  services.xserver = {
    synaptics = {
      enable = true;
      twoFingerScroll = true;
    };
  };
}
