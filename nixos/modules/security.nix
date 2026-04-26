{ config, pkgs, ... }:

{

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryPackage = pkgs.pinentry-gnome3;
  };

  # environment.variables = {
  #   USER_GPG_ID = "";
  # };

  environment.systemPackages = with pkgs; [
    gnupg
    gnutls
    keychain
    openssh
    openssl
  ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
}
