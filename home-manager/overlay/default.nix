{
  emacs-overlay,
  # neovim-nightly-overlay,
  brew-nix,
}: [
  (import emacs-overlay)
  # (import neovim-nightly-overlay)
  brew-nix.overlays.default
  # mailutils 3.21 does not build on aarch64-darwin (its sieve modules need
  # `-undefined dynamic_lookup`, and libmu_auth/virtual.c calls the glibc-only
  # `fgetpwent`, absent from the macOS SDK). Emacs only pulls mailutils in for
  # `movemail`, so build Emacs against its own bundled movemail instead. This
  # keeps auctex/Emacs buildable without depending on the broken package.
  (final: prev:
    prev.lib.optionalAttrs prev.stdenv.hostPlatform.isDarwin {
      emacs = prev.emacs.override {withMailutils = false;};
    })
]
