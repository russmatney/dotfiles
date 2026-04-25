{ config, lib, pkgs, inputs, ... }:

let
  peon-ping-pkg = inputs.peon-ping.packages.${pkgs.system}.default;
  peon-share = "${peon-ping-pkg}/share/peon-ping";
in

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

    peon-ping-pkg
  ];

  # Wire peon-ping hooks into Claude Code (~/.claude/hooks/peon-ping/).
  # Symlinks are recreated on every nixos-rebuild so they always point to
  # the current store path after a peon-ping update.
  system.activationScripts.peonPingClaudeHooks = {
    text = ''
      HOOKS_DIR="/home/russ/.claude/hooks/peon-ping"
      mkdir -p "$HOOKS_DIR/scripts"
      ln -sf "${peon-share}/peon.sh"                         "$HOOKS_DIR/peon.sh"
      ln -sf "${peon-share}/scripts/hook-handle-use.sh"      "$HOOKS_DIR/scripts/hook-handle-use.sh"
      ln -sf "${peon-share}/scripts/hook-handle-rename.sh"   "$HOOKS_DIR/scripts/hook-handle-rename.sh"
    '';
  };

  ## services ############################

  services.lorri.enable = true;

  services.emacs = {
      enable = true;
      package = pkgs.emacs-pgtk;
  };
}
