{ config, pkgs, ... }:
{
  users.extraUsers.taylor = {
    isNormalUser = true;
    home = "/home/taylor";
    description = "taylor";
    extraGroups = [ "wheel" "networkmanager" ];
    shell = "/run/current-system/sw/bin/zsh";
    uid = 1000;
  };
}
