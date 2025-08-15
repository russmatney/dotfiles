{ config, lib, pkgs, ... }:

{
  # packages
  environment.systemPackages = with pkgs; [
    # editor
    emacs
    neovide
    neovim
    nodePackages.prettier
    vim
  ];

  ## services ############################

  services.lorri.enable = true;

  services.emacs = {
      enable = true;
      package = pkgs.emacs;
  };
}
