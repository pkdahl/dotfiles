{ config, lib, pkgs, ... }:
{
  system.defaults.dock.autohide = true;
  system.defaults.dock.orientation = "left";
  system.defaults.NSGlobalDomain.NSDocumentSaveNewDocumentsToCloud = false;
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode = true;
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode2 = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [ pkgs.nix
      pkgs.nix-repl

      pkgs.git
      pkgs.gnupg
      pkgs.mosh
      pkgs.wget

      pkgs.isync
      pkgs.mu

      pkgs.ghc
      pkgs.cabal2nix
      pkgs.cabal-install
    ];

  # Create /etc/bashrc that loads the nix-darwin environment.
  # programs.bash.enable = true;
  programs.zsh.enable = true;

  # Recreate /run/current-system symlink after boot.
  services.activate-system.enable = true;

  # environment.variables.GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  # environment.variables.SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";

  nixpkgs.config.allowUnfree = true;
}
