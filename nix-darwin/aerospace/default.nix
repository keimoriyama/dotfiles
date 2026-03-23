{pkgs}: let
  # emacsScratchpadToggle = emacsLib.mkScratchpadToggle {
  #   windowManager = "aerospace";
  # };
in {
  services.aerospace = {
    enable = true;
    settings = {
      enable-normalization-flatten-containers = true;
      enable-normalization-opposite-orientation-for-nested-containers = true;
      accordion-padding = 0;
      on-focused-monitor-changed = ["move-mouse monitor-lazy-center"];
      exec-on-workspace-change = [
        "/bin/bash"
        "-c"
        "sketchybar --trigger aerospace_workspace_change FOCUSED_WORKSPACE=$(/run/current-system/sw/bin/aerospace list-workspaces --focused)"
      ];

      gaps = {
        inner = {
          horizontal = 0;
          vertical = 0;
        };
        outer = {
          left = 0;
          bottom = 0;
          top = 10;
          right = 0;
        };
      };

      mode = {
        main = {
          binding = {
            alt-h = "focus left";
            alt-l = "focus right";

            alt-shift-h = "move left";
            alt-shift-l = "move right";

            alt-shift-space = "layout floating tiling";

            alt-1 = "workspace 1";
            alt-2 = "workspace 2";
            alt-3 = "workspace 3";
            alt-4 = "workspace 4";
            alt-5 = "workspace 5";
            alt-6 = "workspace 6";
            alt-7 = "workspace 7";
            alt-8 = "workspace 8";
            alt-9 = "workspace 9";
            alt-0 = "workspace 10";
            alt-e = "workspace e";
            alt-c = "workspace c";
            alt-s = "workspace s";
            alt-w = "workspace w";

            alt-shift-1 = [
              "move-node-to-workspace 1"
              "workspace 1"
            ];
            alt-shift-2 = [
              "move-node-to-workspace 2"
              "workspace 2"
            ];
            alt-shift-3 = [
              "move-node-to-workspace 3"
              "workspace 3"
            ];
            alt-shift-4 = [
              "move-node-to-workspace 4"
              "workspace 4"
            ];
            alt-shift-5 = [
              "move-node-to-workspace 5"
              "workspace 5"
            ];
            alt-shift-6 = [
              "move-node-to-workspace 6"
              "workspace 6"
            ];
            alt-shift-7 = [
              "move-node-to-workspace 7"
              "workspace 7"
            ];
            alt-shift-8 = [
              "move-node-to-workspace 8"
              "workspace 8"
            ];
            alt-shift-9 = [
              "move-node-to-workspace 9"
              "workspace 9"
            ];
            alt-shift-0 = [
              "move-node-to-workspace 10"
              "workspace 10"
            ];

            alt-r = "mode resize";

            # Emacs Scratchpad Toggle (like NixOS Mod+I)
            # alt-i = "exec-and-forget ${emacsScratchpadToggle}";
          };
        };

        resize = {
          binding = {
            h = "resize width -50";
            j = "resize height +50";
            k = "resize height -50";
            l = "resize width +50";
            enter = "mode main";
            esc = "mode main";
          };
        };
      };

      workspace-to-monitor-force-assignment = {
        "1" = "main";
        "2" = "main";
        "3" = "main";
        "4" = "main";
        "5" = "secondary";
        "6" = "secondary";
        "7" = "secondary";
        "8" = "secondary";
        "9" = "secondary";
        "10" = "secondary";
        e = "main";
        c = "main";
        s = "main";
        w = "main";
      };

      on-window-detected = [
        # FloatingEmacs scratchpad (kitty with specific title)
        {
          "if".app-id = "com.apple.Mail";
          # "if".window-title-regex-substring = "FloatingEmacs";
          run = ["layout floating"];
        }
        {
          "if".app-id = "com.github.wez.wezterm";
          run = ["move-node-to-workspace w"];
        }
        {
          "if".app-id = "org.gnu.Emacs";
          run = ["move-node-to-workspace e"];
        }
        {
          "if".app-id = "com.google.Chrome";
          run = [
            # "layout floating"
            "move-node-to-workspace c"
          ];
        }
        {
          "if".app-id = "com.tinyspeck.slackmacgap";
          run = ["move-node-to-workspace s"];
        }
      ];
    };
  };
}
