---
name: acl-paper-review
description: ACL / EMNLP など ARR (ACL Rolling Review) に投稿する NLP 論文の原稿 (PDF・テキスト) を、投稿前に査読者の視点で点検する。論文の種類 (手法・データ資源・分析・サーベイ/ポジションなど) を判定したうえで、デスクリジェクトの要件、ARR 査読書式に沿った内容面の評価、同種の採択論文の統計と比べた構成・文体の点検を行い、優先度付きの改善点と想定スコアを返す。ARR 以外の会議の論文や、論文以外の文書には使用しない。
---

# ACL 投稿論文のレビュー

## 振る舞い

1. 次のことを確認する。原稿から判断できない点はユーザーに尋ねる。
   - 原稿の場所。投稿版の PDF しかない場合は、LaTeX ソースもあるかを尋ねる (手順 4)。
   - 種別 (long / short)
   - 段階 (投稿版 / camera-ready)。以降の判定はこれで分岐する。
     - 投稿版: 本文の上限は long 8 / short 4 ページ。匿名性を確認し、Acknowledgments があれば違反とする。
     - camera-ready: 上限は 1 ページ増える (long 9 / short 5)。匿名性の確認は不要で、Acknowledgments はあってよい。
   - 再投稿かどうか
2. 原稿を最初から最後まで読む。
3. 論文の種類を決める。
   - `references/paper-types.md` の区分 (method / resource / analysis / survey・position / その他) から、主張の中心になる種類を選ぶ。
   - 種類があいまいな場合は、ユーザーが ARR で申告する予定の contribution type を尋ねる。
4. `scripts/compare_style.py` を実行し、同じ種類の採択論文と比べた構成と文体の指標を得る。

   ```sh
   uv run <skill-dir>/scripts/compare_style.py <paper.pdf> \
     --type <method|resource|analysis|survey_position> --title "<タイトル>" \
     --stage <submission|camera-ready> --length <long|short>
   ```

   - 投稿版の PDF は余白に行番号がある。スクリプトは行番号を取り除くが、レイアウト解析が段をまたいで見出しや段落を融合させることが残る。
     - LaTeX ソースがあれば、`\usepackage[review]{acl}` を `\usepackage[preprint]{acl}` に変えてコンパイルし直した PDF を渡す。
     - ソースがなく、出力冒頭に行番号の警告が出た場合は、章の検出漏れと、段落・文の長さの指標を割り引いて読む。章立ては原稿を読んで自分で確かめる。
   - 投稿版では、本文の語数と段落数の分布を 8/9 に縮めて比べる。比較対象が camera-ready 版のため。
   - short 論文では分量の指標を比べない。分布は long 論文だけから作っている。文体の指標は参考にしてよい。
   - `--type` を省くと、スクリプトがタイトルと Abstract から種類を推定する。推定は約 8 割しか当たらないので、手順 3 で決めた種類を渡す。
   - 統計がない種類 (再現研究、理論など) は、最も近い種類を渡す。

5. `references/submission-rules.md` に沿って、デスクリジェクトにつながる項目を確認する。ページ数、Limitations、匿名性、付録の使い方、引用の実在性が対象。
   - ページ数の判定には、スクリプトが出す「本文の終わり」と上限の判定を使う。図や表だけのページなど判定が怪しい場合は、PDF を見て確かめる。
   - スクリプトが「Limitations が References の後にある」と出したら、配置の違反として扱う。
   - 引用の実在性は、疑わしい文献だけを ACL Anthology や Web で照合する。
6. `references/review-criteria.md` の観点と、`references/paper-types.md` のうちその種類で問われる点を使って、内容を評価する。中心はここで、次の 3 点を突き合わせる。
   - Abstract と Introduction で主張していること
   - 実験・分析が実際に示していること
   - Limitations が認めていること
7. `references/paper-types.md` の典型的な構成、`references/style-norms.md`、スクリプトの出力を見て、構成と文体のうち査読者が違和感を持ちそうな箇所を拾う。
   - 構成は同じ種類の典型と比べる。たとえば survey に Related Work の章がないこと、analysis の章立てが RQ に沿っていることは、問題にしない。
   - 統計の外れ値は、内容上の理由があれば問題にしない。
8. Responsible NLP Checklist の各項目について、原稿内に対応する記述があるかを確認する。
9. 「出力」の形式で報告する。

## 判断の原則

- 指摘には必ず場所 (節・表・図・段落) を添え、本文の該当箇所を短く引用する。
- 弱点 (Weakness) と提案 (Suggestion) を分ける。主張を支えるのに必要な追加実験だけを弱点に入れ、必要な理由を書く。
- `review-criteria.md` の H1–H17 (避けるべき論法) を弱点の根拠にしない。ただし、実際の査読者がそれを持ち出しそうな箇所は「先回りの手当て」として挙げる。
- 文体の指摘は内容の指摘より優先度を低くする。英語の誤りは、理解を妨げるものだけを弱点に入れる。
- スコアは、ARR の査読者が今の原稿を読んだ場合の見立てとして示す。根拠となる弱点と対応させ、幅を持たせてよい (例: Soundness 3–3.5)。

## 出力

次の順に Markdown で書く。

1. **要約** (3〜5 文): 論文の種類と、論文が何を主張し、どう示しているか。
2. **デスクリジェクトの確認**: 各項目の 可 / 要確認 / 違反。違反と要確認には理由を書く。
3. **想定スコア**: Soundness / Excitement / Overall / Reproducibility と、それぞれの根拠。
4. **強み**: 査読者が評価しそうな点。
5. **改善点** (優先度順、番号付き): 各項目に次の 4 つを書く。
   - 優先度 (高 / 中 / 低)
   - 場所
   - 問題 (対応する観点の記号。例: R4, M2)
   - 具体的な直し方
6. **想定される査読コメントと先回りの手当て**
7. **Checklist の不足**: 対応する記述が見つからない項目。
8. **構成・文体**: スクリプトの出力のうち、対応が必要なものだけ。

## 知識

- `references/paper-types.md` — 論文の種類ごとの典型的な構成、採択論文の統計、査読で問われる点。
- `references/review-criteria.md` — ARR 査読書式のスコア定義、評価の観点 (G / M / R / T)、避けるべき論法 (H1–H17)。
- `references/submission-rules.md` — デスクリジェクトの要件と Responsible NLP Checklist。
- `references/style-norms.md` — 採択論文の構成・文体の基準と注意したい語。
- `references/norms.json` — 採択論文の指標の分位点 (全体と種類別)。`compare_style.py` が読む。
- `scripts/compare_style.py` — 原稿を採択論文の分布と比べる。PDF とテキストを受け付ける。
- `scripts/build_norms.py` — コーパスの特徴量から `norms.json` を作り直す。
- `scripts/features.py`, `scripts/extract.py` — 特徴量の抽出と PDF からのテキスト抽出。

## 制約

- 投稿版の PDF の行番号は取り除くが、レイアウト解析の崩れまでは直せない。正確に測るには preprint モードで作り直した PDF を使う。
- 論文の種類はキーワード規則で推定している。種類別の分布にも約 2 割の誤分類が混ざっているため、種類の間の差は実際より小さめに出る。
- 章の分類は章タイトル中のキーワードで判定している。手法名だけの章は "other" になり、割合の指標に入らない。
- 規定の出典は 2026-10-17 時点のもの。サイクルごとに改定されるので、締切前に CFP を確認する。
