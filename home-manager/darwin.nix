{pkgs}:
with pkgs; [
  brewCasks.zoom
  brewCasks.skim
  # brewCasks.docker-desktop
  # brewCasks.chatgpt
  # brewCasks.ollama-app
  # brewCasks.codex
  # The claude desktop cask ships a `bin/claude` wrapper that collides with the
  # claude-code CLI's `bin/claude`. Lower its priority so the CLI wins the bin/
  # while the desktop .app bundle (a non-conflicting path) is still installed.
  (lib.lowPrio brewCasks.claude)
  # brewCasks.notion-calendar
]
