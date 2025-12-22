{config, pkgs, lib, ...}:

{
  # boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  # clean /tmp
  boot.tmp.cleanOnBoot = true;

  # Make camera work in Skype, Zoom, etc
  # boot.extraModulePackages = with config.boot.kernelPackages;
  #   [ v4l2loopback.out ];
  # boot.extraModprobeConfig = ''
  #   # exclusive_caps: Skype, Zoom, Teams etc. will only show device when actually streaming
  #   # card_label: Name of virtual camera, how it'll show up in Skype, Zoom, Teams
  #   # https://github.com/umlaeute/v4l2loopback
  #   options v4l2loopback exclusive_caps=1 card_label="Virtual Camera"
  # '';

  # environment.systemPackages = [
  #   pkgs.linuxKernel.packages.linux_6_18.v4l2loopback
  # ];

  # networking
  networking.networkmanager.enable = true;

  # timezone
  time.timeZone = "America/New_York";

  # internationalisation
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;

  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" ];

      substituters = [
        "https://cache.iog.io"
        "https://nix-community.cachix.org"
        "https://hyprland.cachix.org"
        # "https://cuda-maintainers.cachix.org"
      ];
      trusted-substituters = [
        "https://cache.iog.io"
        "https://nix-community.cachix.org"
        # "https://cuda-maintainers.cachix.org"
      ];
      trusted-public-keys = [
        "cache.iog.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        # "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
      ];
    };
    # nixPath =
    #   [
    #     "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
    #     "nixos-config=/etc/nixos/configuration.nix"
    #     "/nix/var/nix/profiles/per-user/root/channels"
    #   ];
  };

}
