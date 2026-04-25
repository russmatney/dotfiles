# Machine-specific configuration for walt
{ config, pkgs, ... }:

{
  # Machine role: [Define role - e.g., media server, secondary workstation]

  environment.systemPackages = with pkgs; [
    # Add walt-specific packages here
  ];

  services = {
    # Walt-specific services
  };

  environment.sessionVariables = {
    # MACHINE_ROLE = "media";
  };
}
