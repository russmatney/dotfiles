# Machine-specific configuration for brain
{ config, pkgs, ... }:

{
  # Machine role: [Define role - e.g., server, always-on services]
  imports = [
    # <nixos-hardware/framework/desktop/amd-ai-300-series>

    /etc/nixos/hardware-configuration.nix
  ];

  # hardware = {
  #   modules = [ "amd-ai-300-series" ];
  # };

  environment.systemPackages = with pkgs; [
    # Add brain-specific packages here
  ];

  services = {
    # Brain-specific services
    # services.fwupd.enable = true;
  };

  environment.sessionVariables = {
    # MACHINE_ROLE = "server";
  };
}
