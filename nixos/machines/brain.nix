# Machine-specific configuration for brain
{ config, pkgs, ... }:

{
  # Machine role: [Define role - e.g., server, always-on services]

  imports = [
    /etc/nixos/hardware-configuration.nix
  ];

  environment.systemPackages = with pkgs; [
    # Add brain-specific packages here
  ];

  services = {
    # Brain-specific services
  };

  environment.sessionVariables = {
    # MACHINE_ROLE = "server";
  };
}
