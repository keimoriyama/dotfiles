{
  pkgs,
  sources,
  neomacs,
}: let
  # EMACSLOADPATH を揃えるためだけに使う。本家の Emacs 設定と同じ derivation。
  emacsPkgs = import ../emacs/emacs-with-packages.nix {inherit pkgs sources;};

  # neomacs は GNU Emacs 互換を名乗るので bin/emacs・bin/emacsclient と
  # share/emacs/site-lisp の互換ファイルを置く。どれも本家 Emacs と同じパスに
  # なり home.packages の buildEnv で衝突するため落とす。neomacs は neomacs
  # コマンドとして使い、emacs の名前は本家に譲る。
  neomacsBase = neomacs.overrideAttrs (prev: {
    postInstall =
      (prev.postInstall or "")
      + ''
        rm "$out/bin/emacs" "$out/bin/emacsclient"
        rm "$out/share/emacs/site-lisp/site-start.el" \
           "$out/share/emacs/site-lisp/subdirs.el"
        rmdir "$out/share/emacs/site-lisp" "$out/share/emacs"
      '';
  });

  # EMACSLOADPATH だけはビルド時に本家の Emacs へ問い合わせて埋めるので、
  # プレースホルダにしてある。
  neomacsInfoPlist = pkgs.writeText "neomacs-Info.plist" ''
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
      "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
      <key>CFBundleName</key>
      <string>neomacs</string>
      <key>CFBundleDisplayName</key>
      <string>Neomacs</string>
      <key>CFBundleExecutable</key>
      <string>neomacs</string>
      <key>CFBundleIdentifier</key>
      <string>org.neomacs</string>
      <key>CFBundleVersion</key>
      <string>${neomacsBase.version}</string>
      <key>CFBundleShortVersionString</key>
      <string>${neomacsBase.version}</string>
      <key>CFBundlePackageType</key>
      <string>APPL</string>
      <key>CFBundleInfoDictionaryVersion</key>
      <string>6.0</string>
      <key>LSMinimumSystemVersion</key>
      <string>12.0</string>
      <key>NSHighResolutionCapable</key>
      <true/>
      <key>LSEnvironment</key>
      <dict>
        <key>NEOMACS_RUNTIME_ROOT</key>
        <string>${neomacsBase}/share/neomacs</string>
        <key>EMACSLOADPATH</key>
        <string>@EMACSLOADPATH@</string>
      </dict>
    </dict>
    </plist>
  '';

  # neomacs は emacs-31.1 のフォークなので ~/.emacs.d/init.el はそのまま読む。
  # 足りないのはパッケージで、それらは Nix が EMACSLOADPATH で渡す場所にしか
  # 存在しない。本家のラッパーが設定している値をそのまま渡して load-path を
  # 揃える。末尾の空要素が neomacs 自身の lisp を残すので、同梱の elisp は
  # 本家のものではなく neomacs のものが使われる。
  # emacsPkgs への依存をラッパー層に閉じ込めておかないと、elisp パッケージを
  # 足すたびに neomacs の Rust ビルドがやり直しになる。
  #
  # あわせて macOS 用の .app も組む。upstream の Nix パッケージは
  # $out/bin にバイナリを置くだけで、バンドルを作るのは Linux 向けの
  # デスクトップアセット生成と cargo ベースのリリーススクリプトだけなので、
  # このままでは LaunchServices に登録されず AeroSpace からも見えない。
  neomacsPackage =
    pkgs.runCommand "${neomacsBase.name}-emacs-config" {
      nativeBuildInputs = [pkgs.makeWrapper];
    } ''
      loadPath="$(${emacsPkgs}/bin/emacs -Q --batch \
        --eval '(princ (or (getenv "EMACSLOADPATH") ""))')"

      mkdir -p "$out/bin"
      for entry in ${neomacsBase}/*; do
        name="$(basename "$entry")"
        if [ "$name" != bin ]; then
          ln -s "$entry" "$out/$name"
        fi
      done
      for entry in ${neomacsBase}/bin/*; do
        ln -s "$entry" "$out/bin/$(basename "$entry")"
      done

      rm "$out/bin/neomacs"
      makeWrapper "${neomacsBase}/bin/neomacs" "$out/bin/neomacs" \
        --set EMACSLOADPATH "$loadPath"

      ${pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isDarwin ''
        app="$out/Applications/Neomacs.app"
        mkdir -p "$app/Contents/MacOS"

        # Emacs.app と同じ理由で、ここには実体のバイナリを置く。ラッパー
        # スクリプトを置くと LaunchServices が起動したプロセスをバンドルへ
        # 紐付けられず、アクセシビリティ API を使う AeroSpace から見えなくなる。
        # ラッパーが渡していた環境変数は Info.plist の LSEnvironment で渡す。
        cp "${neomacsBase}/bin/.neomacs-wrapped" "$app/Contents/MacOS/neomacs"
        cp "${neomacsBase}/bin/neomacsclient" "$app/Contents/MacOS/neomacsclient"
        # dump イメージの探索は実行ファイルの隣が最初の段なので、そこへ置く。
        cp "${neomacsBase}/bin/neomacs.pdump" "$app/Contents/MacOS/neomacs.pdump"
        chmod u+w "$app/Contents/MacOS/neomacs" \
          "$app/Contents/MacOS/neomacsclient" \
          "$app/Contents/MacOS/neomacs.pdump"

        install -m 0644 ${neomacsInfoPlist} "$app/Contents/Info.plist"
        substituteInPlace "$app/Contents/Info.plist" \
          --replace-fail @EMACSLOADPATH@ "$loadPath"
      ''}
    '';
in {
  home.packages = [neomacsPackage];
}
