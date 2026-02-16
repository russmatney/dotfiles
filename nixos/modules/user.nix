{ config, pkgs, lib, ... }:

{
  imports = [
    (import "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/release-25.11.tar.gz}/nixos")
  ];

  users = {
    mutableUsers = true;
    # users.default = {
    #   home = "/home/${toString config.users.users.default.name}";
    #   extraGroups = ["wheel" "networkmanager" "docker" "video"];
    #   isNormalUser = true;
    # };
    users.russ = {
      home = "/home/russ";
      extraGroups = [ "networkmanager" "wheel" "docker" "video" ];
      isNormalUser = true;
      shell = pkgs.zsh;
    };
  };

  home-manager = {
    useUserPackages = true;
    # users.default = import ./home.nix;
    users.russ = import ./home.nix;
  };

  nix.settings.trusted-users = [
    "root"
    # "${toString config.users.users.default.name}"
    "russ"
  ];
}
