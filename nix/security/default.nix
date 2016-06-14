{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    physlock
  ];

  # Set sudo to use user home
  security.sudo.extraConfig = ''
    Defaults !always_set_home
    Defaults env_keep+="HOME"
  '';

  services = {
    # Configure physlock
    physlock = {
      enable = true;
      user = "taylor";
      lockOn = {
        suspend = true;
        hibernate = true;
      };
    };
  };
}
