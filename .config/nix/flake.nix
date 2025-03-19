{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
   neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
  };

  outputs = { self, nixpkgs, neovim-nightly-overlay }: {
	packages.aarch64-darwin.my-packages = nixpkgs.legacyPackages.aarch64-darwin.buildEnv {
   name="my-packages";
		paths = [
		nixpkgs.legacyPackages.aarch64-darwin.git
        nixpkgs.legacyPackages.aarch64-darwin.curl
		neovim-nightly-overlay.packages.aarch64-darwin.neovim
		];
	};
  };
}
