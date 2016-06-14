{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    nethack
    steam
    # tibia
  ];
}
