{
  description = "My flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixvim = {
      url = "github:nix-community/nixvim";
    };
    org-babel.url = "github:emacs-twist/org-babel";
    flake-parts.url = "github:hercules-ci/flake-parts";
    brew-nix = {
      url = "github:BatteredBunny/brew-nix";
      inputs.nix-darwin.follows = "nix-darwin";
      inputs.brew-api.follows = "brew-api";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    brew-api = {
      url = "github:BatteredBunny/brew-api";
      flake = false;
    };
    llm-agents.url = "github:numtide/llm-agents.nix";
    arto.url = "github:arto-app/Arto";
    agent-skills-nix = {
      url = "github:Kyure-A/agent-skills-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
    emacs-skills = {
      url = "github:xenodium/emacs-skills";
      flake = false;
    };
    nippo = {
      url = "github:nwiizo/nippo";
      flake = false;
    };
    suiko = {
      url = "github:nwiizo/suiko";
      flake = false;
    };
    ponytail = {
      url = "github:DietrichGebert/ponytail";
      flake = false;
    };
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # エージェントの書き込みを OS のサンドボックス (Apple Seatbelt / Landlock) で縛る。
    cage = {
      url = "github:Warashi/cage";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # PreToolUse hook で危険な読み取り・コマンド実行をブロックし、代替手段を提示する。
    guard-and-guide = {
      url = "github:kawarimidoll/guard-and-guide";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Claude Code の transcript と設定を集計し、利用状況や失敗傾向を診断する。
    cclens = {
      url = "github:lambdalisue/cclens";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    flake-parts,
    nixpkgs,
    nix-darwin,
    home-manager,
    emacs-overlay,
    nixvim,
    org-babel,
    brew-nix,
    llm-agents,
    arto,
    nixos-wsl,
    agent-skills-nix,
    emacs-skills,
    nippo,
    suiko,
    ponytail,
    cage,
    guard-and-guide,
    cclens,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} ({self, ...}: let
      darwinSystem = "aarch64-darwin";
      nixosSystem = "x86_64-linux";
      username = "kei";
      workUsername = "kei.moriyama";
      homeModules = [
        ./home-manager/default.nix
        agent-skills-nix.homeManagerModules.default
      ];
      mkDarwinSpecialArgs = username: isWork: {
        inherit
          nixpkgs
          home-manager
          emacs-overlay
          nixvim
          org-babel
          username
          isWork
          brew-nix
          llm-agents
          arto
          nixos-wsl
          emacs-skills
          nippo
          suiko
          ponytail
          cage
          cclens
          guard-and-guide
          ;
        system = darwinSystem;
        inherit (home-manager.lib) homeManagerConfiguration;
      };
      darwinSpecialArgs = mkDarwinSpecialArgs username false;
      mkDarwinConfiguration = username: isWork: let
        specialArgs = mkDarwinSpecialArgs username isWork;
      in
        nix-darwin.lib.darwinSystem {
          system = darwinSystem;
          inherit specialArgs;
          modules = [
            ./hosts/darwin/default.nix
            home-manager.darwinModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = false;
                useUserPackages = true;
                extraSpecialArgs = specialArgs;
                users.${username} = {
                  imports = homeModules;
                };
              };
            }
          ];
        };
      nixosSpecialArgs = {
        inherit
          nixpkgs
          home-manager
          emacs-overlay
          nixvim
          org-babel
          username
          brew-nix
          llm-agents
          arto
          nixos-wsl
          emacs-skills
          nippo
          suiko
          ponytail
          cage
          cclens
          guard-and-guide
          ;
        isWork = false;
        system = nixosSystem;
        inherit (home-manager.lib) homeManagerConfiguration;
      };
    in {
      systems = [darwinSystem nixosSystem];

      perSystem = {pkgs, ...}: {
        apps.update = {
          type = "app";
          program = toString (pkgs.writeShellScript "update-script" ''
            set -e
            config="''${1:-my-config}"
            echo "Updating nix-darwin and home-manager ($config)..."
            sudo env HOME="$HOME" USER="$USER" LOGNAME="$LOGNAME" \
              nix run nix-darwin -- switch --flake ${self.outPath}#"$config"
            echo "update complete"
          '');
        };

        formatter = pkgs.alejandra;
      };

      flake = {
        darwinConfigurations.my-config = mkDarwinConfiguration username false;
        darwinConfigurations.work-config = mkDarwinConfiguration workUsername true;

        homeConfigurations.myHomeConfig = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            system = darwinSystem;
            config.allowUnfreePredicate = pkg:
              builtins.elem (nixpkgs.lib.getName pkg) [
                "copilot-language-server"
              ];
          };
          extraSpecialArgs = darwinSpecialArgs;
          modules = homeModules;
        };

        nixosConfigurations.my-config = nixpkgs.lib.nixosSystem {
          system = nixosSystem;
          specialArgs = nixosSpecialArgs;
          modules = [
            nixos-wsl.nixosModules.wsl
            ./hosts/nixos-wsl/default.nix
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = nixosSpecialArgs;
                users.${username} = {
                  imports = homeModules;
                };
              };
            }
          ];
        };
      };
    });
}
