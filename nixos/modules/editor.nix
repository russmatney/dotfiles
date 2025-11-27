{ config, lib, pkgs, ... }:

{

  # add-unstable-packages = final: _prev: {
  #   unstable = import nixpkgs-unstable {
  #     system = "x86_64-linux";
  #   };
  # };

  # nixpkgs.overlays = [add-unstable-packages];

  # packages
  environment.systemPackages = with pkgs; [
    # editor
    emacs-pgtk
    neovide
    neovim
    nodePackages.prettier
    vim

    claude-code
    # unstable.code-cursor
    code-cursor
  ];

  ## services ############################

  services.lorri.enable = true;

  services.emacs = {
      enable = true;
      package = pkgs.emacs-pgtk;
  };
}
