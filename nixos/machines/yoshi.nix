# Machine-specific configuration for yoshi
{ config, pkgs, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix
  ];

  # Machine role: Primary development workstation
  # Hardware: Framework 16-inch AMD

  # Machine-specific packages
  environment.systemPackages = with pkgs; [
    # Add yoshi-specific packages here
  ];

  # Machine-specific services
  services = {
    # Example: Enable specific services for this machine
  };

  # Performance tuning
  # powerManagement.cpuFreqGovernor = "performance";

  # Machine-specific environment variables
  environment.sessionVariables = {
    # MACHINE_ROLE = "dev-primary";
  };

  # Any other yoshi-specific configuration
}
