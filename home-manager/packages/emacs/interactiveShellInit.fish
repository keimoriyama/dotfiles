set -gx MOCWORD_DATA $HOME/.local/mocword-data/mocword.sqlite
set -gx DPP_PATH $HOME/.cache/dpp
set -gx LSP_USE_PLISTS 1
set -gx HYDRA_FULL_ERROR 1
set -gx NIX_USER_CONF_FILES $XDG_CONFIG_HOME/nix/nix.conf:$$XDG_CONFIG_HOME/nix/local.conf;
set -gx TYPST_FONT_PATHS $HOME/Library/Application Support/Adobe/.User Owned Fonts/:$HOME/Library/Application Support/Adobe/CoreSync/plugins/livetype/.r/