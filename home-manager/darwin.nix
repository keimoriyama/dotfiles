{pkgs}:
with pkgs; [
  brewCasks.zoom
  brewCasks.skim
  brewCasks.docker-desktop
  brewCasks.chatgpt
  brewCasks.ollama-app
  brewCasks.alt-tab
  (brewCasks.steam.overrideAttrs
    (oldAttrs: {
      src = pkgs.fetchurl {
        url = builtins.head oldAttrs.src.urls;
        hash = "sha256-X1VnDJGv02A6ihDYKhedqQdE/KmPAQZkeJHudA6oS6M=";
      };
    }))
]
