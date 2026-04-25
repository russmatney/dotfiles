# Machine-specific configuration for wordnerd
{ config, pkgs, ... }:

{
  # Machine role: [Define role - e.g., writing/content creation]

  environment.systemPackages = with pkgs; [
    # Add wordnerd-specific packages here
  ];

  services = {
    # Wordnerd-specific services
  };

  environment.sessionVariables = {
    # MACHINE_ROLE = "content";
  };
}
