# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }: {
  imports = [ ./hardware-configuration.nix ];
  nixpkgs.config = {
    allowUnfree = true;
    pulseaudio = true;
    packageOverrides = pkgs: {
      nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
        inherit pkgs;
      };
    };
  };
  boot = {
    kernelPackages = pkgs.linuxPackages_zen;
    loader = {
      grub = {
        enable = true;
        version = 2;
        device = "/dev/sda";
        useOSProber = true;
      };
    };
    blacklistedKernelModules = [ "snd_pcsp" ];
  };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      noto-fonts-cjk
      noto-fonts
      terminus_font
      font-awesome-ttf
      ubuntu_font_family
    ];
  };

  networking.hostName = "nixos";

  time.timeZone = "Europe/Moscow";

  networking.useDHCP = false;
  networking.interfaces.enp7s0.useDHCP = true;

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  services = {
    xserver = {
      enable = true;
      displayManager = {
        sddm.enable = true;
        defaultSession = "none+awesome";
      };
      desktopManager = {
        xterm.enable = false;
      };
      windowManager = {
        awesome = {
          enable = true;
          luaModules = with pkgs.luaPackages; [
            luarocks
            luadbi-mysql
          ];
        };
        i3 = {
          enable = true;
          package = pkgs.i3-gaps;
        };
      };
      videoDrivers = [ "nvidia" ];
    };
    flatpak.enable = true;
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  sound.enable = true;
  hardware = {
    opengl = {
      enable = true;
      driSupport32Bit = true;
      extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
    };
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
      support32Bit = true;
    };
  };

  users.users = {
    vivek = {
      isNormalUser = true;
      extraGroups = [
        "wheel"
        "audio"
      ];
      shell = pkgs.fish;
    };
    vel = {
      isNormalUser = true;
      extraGroups = [
        "wheel"
        "audio"
      ];
      shell = pkgs.fish;
    };
  };

  xdg.portal.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # environment.systemPackages = with pkgs; [
  #   wget vim
  #   firefox
  # ];
  programs.dconf.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}

