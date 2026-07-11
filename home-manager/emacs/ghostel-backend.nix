{
  pkgs,
  sources,
}:
# Builds the Ghostel dynamic module from source with Zig and installs it into
# the user's cache dir so Emacs can load it at runtime.
pkgs.writeShellApplication {
  name = "build-ghostel-backend";
  runtimeInputs = [
    pkgs.coreutils
    pkgs.zig_0_15
  ];
  text = ''
    set -euo pipefail

    module_dir="''${GHOSTEL_MODULE_DIR:-''${XDG_CACHE_HOME:-$HOME/.cache}/ghostel}"
    build_dir="$(mktemp -d)"
    trap 'rm -rf "$build_dir"' EXIT

    case "$(uname -s)" in
      Darwin) module_suffix=dylib ;;
      *) module_suffix=so ;;
    esac

    cp -R ${sources.ghostel.src} "$build_dir/ghostel"
    chmod -R u+w "$build_dir/ghostel"
    cd "$build_dir/ghostel"

    export HOME="$build_dir/home"
    export ZIG_GLOBAL_CACHE_DIR="$build_dir/zig-global-cache"
    export ZIG_LOCAL_CACHE_DIR="$build_dir/zig-local-cache"

    if [[ "$(uname -s)" == Darwin && -z "''${SDKROOT:-}" ]] && command -v xcrun >/dev/null 2>&1; then
      sdkroot="$(xcrun --sdk macosx --show-sdk-path)"
      export SDKROOT="$sdkroot"
    fi

    zig build -Doptimize=ReleaseFast -Dcpu=baseline

    mkdir -p "$module_dir"
    install -m 755 "ghostel-module.$module_suffix" "$module_dir/"
    install -m 644 ghostel-module.version "$module_dir/"

    printf 'Installed Ghostel backend to %s\n' "$module_dir"
  '';
}
