# ~/.nixpkgs/config.nix

{
  allowBroken = true;
  allowUnfree = true;

  packageOverrides = pkgs: with pkgs; rec {

    elm-env = buildEnv {
      name = "elm-env";
      paths = [
        elmPackages.elm
        elmPackages.elm-compiler
        elmPackages.elm-format
        elmPackages.elm-make
        elmPakcages.elm-packages
        elmPackages.elm-reactor
        elmPackages.elm-repl
      ];
    };

    ghc-env = buildEnv {
      name = "ghc-env";
      paths = [
        ghc
        cabal2nix
        cabal-install
      ];
    };

    shell-tools-env = buildEnv {
      name = "shell-tools-env";
      paths = [
        git
        gnupg
        mosh
      ];
    };

    nix-tools-env = buildEnv {
      name = "nix-tools-env";
      paths = [
        nix-repl
        nix-zsh-completions
      ];
    };

  };

}
