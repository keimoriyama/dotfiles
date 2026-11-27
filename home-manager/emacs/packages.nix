{
  pkgs,
  epkgs,
  sources,
}: let
  parquetExplorerSource = sources.emacs-parquet-explorer;
  parquetExplorerVersion = "0.1.0-unstable-${parquetExplorerSource.date}";
  parquetExplorerUiSource = pkgs.applyPatches {
    name = "emacs-parquet-explorer-ui-source";
    src = parquetExplorerSource.src;
    patches = [
      ./emacs-parquet-explorer-zstd.diff
      ./emacs-parquet-explorer-nested.diff
      ./emacs-parquet-explorer-utf8.diff
    ];
  };
  parquetExplorerWasmClang = pkgs.rustc.llvmPackages.clang-unwrapped;
  parquetExplorerUi = pkgs.rustPlatform.buildRustPackage {
    pname = "emacs-parquet-explorer-ui";
    version = parquetExplorerVersion;
    src = parquetExplorerUiSource;
    cargoRoot = "ui";
    cargoLock.lockFile = "${parquetExplorerUiSource}/ui/Cargo.lock";
    buildInputs = [pkgs.zstd];
    nativeBuildInputs = [
      pkgs.binaryen
      pkgs.pkg-config
      pkgs.rustc.llvmPackages.lld
      pkgs.wasm-pack
      # Must match wasm-bindgen in ui/Cargo.lock.
      pkgs.wasm-bindgen-cli_0_2_122
    ];
    CC_wasm32_unknown_unknown = "${parquetExplorerWasmClang}/bin/clang";
    CFLAGS_wasm32_unknown_unknown = let
      majorVersion = pkgs.lib.versions.major parquetExplorerWasmClang.version;
    in "-isystem ${pkgs.lib.getLib pkgs.rustc.llvmPackages.libclang}/lib/clang/${majorVersion}/include -resource-dir ${pkgs.lib.getLib parquetExplorerWasmClang}/lib/clang/${majorVersion}";
    doCheck = false;
    buildPhase = ''
      runHook preBuild
      export CARGO_NET_OFFLINE=true
      export HOME="$TMPDIR/home"
      export XDG_CACHE_HOME="$TMPDIR/cache"
      mkdir -p "$HOME" "$XDG_CACHE_HOME"
      cd ui
      install -D -m 644 ${pkgs.ipafont}/share/fonts/truetype/ipag.ttf assets/ipag.ttf
      cargo build --release --bins
      # Native binaries use Nix's zstd.  WASM must build zstd-sys's bundled C source.
      unset ZSTD_SYS_USE_PKG_CONFIG
      wasm-pack build --target web --release --mode no-install
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      install -D -m 755 target/release/parquet_filter "$out/bin/parquet_filter"
      install -D -m 755 target/release/parquet_to_csv "$out/bin/parquet_to_csv"
      cp -R pkg "$out/pkg"
      runHook postInstall
    '';
  };
in {
  yatex = epkgs.melpaBuild {
    pname = "yatex";
    version = "0.0.1";
    src = sources.emacs-yatex.src;
    patches = [./yatexlib.diff];
    ignoreCompilationError = true;
  };
  nano-modeline = epkgs.melpaBuild {
    pname = "nano-modeline";
    src = sources.emacs-nano-modeline.src;
    version = "0.0.1";
  };
  typst-ts-mode = epkgs.melpaBuild {
    pname = "typst-ts-mode";
    src = sources.emacs-typst-ts-mode.src;
    version = "0.0.1";
    # LIMITATION: upstream marks `define-compilation-mode` with an autoload
    # cookie, so the whole form is copied into the autoloads file and runs
    # before compile.el is loaded (void-function define-compilation-mode).
    postPatch = ''
      sed -i '/^;;;###autoload$/{N;/\n(define-compilation-mode/s/^;;;###autoload\n//}' typst-ts-compile.el
    '';
  };
  eglot-x = epkgs.melpaBuild {
    pname = "eglot-x";
    src = sources.emacs-eglot-x.src;
    version = "0.0.1";
    packageRequires = [
      epkgs.project
      epkgs.eglot
      epkgs.xref
    ];
  };
  agent-shell-notifications = epkgs.melpaBuild {
    pname = "agent-shell-notifications";
    src = sources.emacs-agent-shell-notifications.src;
    version = "0.1.0";
    # The knockknock provider requires the knockknock package, which isn't
    # packaged in nixpkgs, so byte-compiling it would fail. We use a custom
    # macOS provider anyway (libnotify needs D-Bus, absent on darwin), so drop
    # it along with the test file rather than disabling error checking wholesale.
    preBuild = ''
      rm -f agent-shell-notifications-knockknock.el agent-shell-notifications-tests.el
    '';
    packageRequires = [epkgs.agent-shell];
  };
  agent-shell-attention = epkgs.melpaBuild {
    pname = "agent-shell-attention";
    src = sources.emacs-agent-shell-attention.src;
    version = "0.0.5";
    packageRequires = [epkgs.agent-shell];
  };
  agent-shell-tramp = epkgs.melpaBuild {
    pname = "agent-shell-tramp";
    src = sources.emacs-agent-shell-tramp.src;
    version = "0.2.0";
    preBuild = ''
      rm -f agent-shell-tramp-tests.el
    '';
    packageRequires = [
      epkgs.agent-shell
      epkgs.acp
    ];
  };
  agent-shell-dashboard = epkgs.melpaBuild {
    pname = "agent-shell-dashboard";
    src = sources.emacs-agent-shell-dashboard.src;
    version = "0.1.0";
    packageRequires = [
      epkgs.agent-shell
      # Optional at runtime — (require 'modus-themes nil t) — but needed so
      # byte-compilation resolves its faces. projectile stays out: it's only
      # reached through fboundp guards.
      epkgs.modus-themes
    ];
  };
  arto = epkgs.melpaBuild {
    pname = "arto";
    src = sources.emcas-arto.src;
    version = "0.0.1";
  };
  rsync-mode = epkgs.melpaBuild {
    pname = "rsync-mode";
    src = ./rsync-mode;
    version = "0.1.0";
    packageRequires = [epkgs.spinner];
  };
  emacs-egui = epkgs.melpaBuild {
    pname = "emacs-egui";
    version = "0.1.0-unstable-${sources.emacs-egui.date}";
    src = sources.emacs-egui.src;
    patches = [./emacs-egui-utf8.diff];
    files = ''("lisp/*.el")'';
    postPatch = ''
      # marimo's official renderer and its chunks use the .mjs extension.
      substituteInPlace lisp/emacs-egui.el \
        --replace-fail '("js"   "text/javascript; charset=utf-8")' '((or "js" "mjs") "text/javascript; charset=utf-8")'
    '';
    meta = {
      description = "Web and native runtimes for egui applications in Emacs";
      homepage = "https://github.com/nohzafk/emacs-egui";
      license = pkgs.lib.licenses.mit;
    };
  };
  emacs-parquet-explorer = epkgs.melpaBuild (finalAttrs: {
    pname = "emacs-parquet-explorer";
    version = parquetExplorerVersion;
    src = parquetExplorerSource.src;
    # The prebuilt sidecar cannot be rebuilt inside the read-only Nix store.
    patches = [./emacs-parquet-explorer-sidecar.diff];
    files = ''("lisp/*.el")'';
    postPatch = ''
      # package-build moves lisp/*.el to the package root. Keep the UI and
      # binaries below that same root at runtime; emacs-egui is installed separately.
      substituteInPlace lisp/emacs-parquet-explorer.el \
        --replace-fail '"../emacs-egui/lisp/"' '"emacs-egui/lisp/"' \
        --replace-fail '"../ui/"' '"ui/"' \
        --replace-fail '(expand-file-name "../" emacs-parquet-explorer--dir)' 'emacs-parquet-explorer--dir' \
        --replace-fail '(manifest-path (expand-file-name "ui/Cargo.toml" emacs-parquet-explorer--dir))' '(export-binary (expand-file-name "ui/target/release/parquet_to_csv" emacs-parquet-explorer--dir))' \
        --replace-fail ':command (list "cargo" "run" "--release" "--manifest-path" manifest-path "--bin" "parquet_to_csv" "--" expanded-input expanded-output)' ':command (list export-binary expanded-input expanded-output)'
    '';
    preBuild = ''
      export EMACSLOADPATH="$EMACSLOADPATH:$PWD/emacs-egui/lisp"
    '';
    postInstall = ''
      LISPDIR="$out/share/emacs/site-lisp/elpa/${finalAttrs.ename}-${finalAttrs.melpaVersion}"
      mkdir -p "$LISPDIR/ui/target/release"
      install -m 644 ui/index.html "$LISPDIR/ui/index.html"
      cp -R ${parquetExplorerUi}/pkg "$LISPDIR/ui/pkg"
      install -m 755 ${parquetExplorerUi}/bin/parquet_filter "$LISPDIR/ui/target/release/parquet_filter"
      install -m 755 ${parquetExplorerUi}/bin/parquet_to_csv "$LISPDIR/ui/target/release/parquet_to_csv"
    '';
    meta = {
      description = "GPU-accelerated Parquet file explorer for Emacs";
      homepage = "https://github.com/nohzafk/emacs-parquet-explorer";
      license = pkgs.lib.licenses.mit;
      platforms = pkgs.lib.platforms.darwin;
    };
  });
}
