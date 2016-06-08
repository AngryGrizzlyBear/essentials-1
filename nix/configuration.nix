{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./display
      ./audio
      ./input
      ./users
      ./security
      ./development
      ./games
      ./comms
      ./software
    ];

  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  nixpkgs.config = {
    allowUnfree = true;
  };

  boot.initrd.luks.devices = [
    { name = "root"; device = "/dev/sda3"; }
  ];

  boot.initrd.luks.cryptoModules = [ "aes" "sha256" "sha1" "cbc" ];

  networking.hostName = "ares"; # Define your hostname.
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  
  i18n = {
    consoleFont = "inconsolata";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "US/Pacific";

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";

}
