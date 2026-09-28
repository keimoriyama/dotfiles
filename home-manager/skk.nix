{
  pkgs,
  lib,
  sources,
  ...
}: let
  jisyoL = "${pkgs.skkDictionaries.l}/share/skk/SKK-JISYO.L";

  # macSKKはUTF-8辞書しか安定して読めないため、EUC-JPのSKK-JISYO.Lを変換しておく。
  skk-jisyo-utf8 = pkgs.runCommand "SKK-JISYO.L.utf8" {} ''
    ${pkgs.libiconv}/bin/iconv -f EUC-JP -t UTF-8 \
      "${jisyoL}" \
      | sed '1s/coding: euc-jp/coding: utf-8/' > $out
  '';
in {
  home.packages = [
    (pkgs.callPackage ./yaskkserv2 {inherit sources;})
  ];

  home.activation =
    {
      skkDictionary = lib.hm.dag.entryAfter ["linkGeneration"] ''
        $DRY_RUN_CMD /bin/mkdir -p "$HOME/.skk-dict"
        $DRY_RUN_CMD /usr/bin/install -m644 \
          "${jisyoL}" \
          "$HOME/.skk-dict/SKK-JISYO.L"
      '';
    }
    // lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
      # macSKKはサンドボックスアプリのため/nix/storeへのsymlinkを辿れない。
      # コンテナ内へ実ファイルとしてコピーする必要がある。
      # 他アプリのコンテナへの書き込みは macOS の App Data 保護で拒否されることがある。
      # 辞書の更新に失敗しても switch 全体は止めず、警告だけ出す。
      macskkFiles = lib.hm.dag.entryAfter ["writeBoundary"] ''
        container="$HOME/Library/Containers/net.mtgto.inputmethod.macSKK/Data/Documents"
        if ! {
          $DRY_RUN_CMD /bin/mkdir -p "$container/Dictionaries" "$container/Settings" &&
          $DRY_RUN_CMD /usr/bin/install -m644 \
            "${skk-jisyo-utf8}" \
            "$container/Dictionaries/SKK-JISYO.L.utf8" &&
          $DRY_RUN_CMD /usr/bin/install -m644 \
            "${./macskk/kana-rule.conf}" \
            "$container/Settings/kana-rule.conf"
        }; then
          warnEcho "macSKK のコンテナに書き込めませんでした。辞書と kana-rule.conf は更新されていません。"
        fi
      '';
    };
}
