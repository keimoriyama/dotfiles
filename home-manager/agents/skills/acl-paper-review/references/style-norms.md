# 採択論文の構成と文体の基準

ACL long と EMNLP main の 2022–2026 年の採択論文 11,011 本から集計した。数値の分布 (分位点) は `norms.json` にあり、2024 年以降の 7,630 本から作っている。`scripts/compare_style.py` を実行すると、対象論文の値がこの分布のどこに入るかを出力する。

ここに書いたのは全種類を合わせた基準である。手法・データ資源・分析・サーベイなど、種類ごとの違いは `paper-types.md` にまとめた。たとえばサーベイには Related Work の章がほぼなく、参考文献は約 120 本ある。`norms.json` には種類別の分布 (`by_type`) も入っている。

分布はあくまで採択論文の記述統計で、採択されるための条件ではない。外れ値はそれだけでは問題にならず、「査読者が違和感を持つかもしれない箇所」の候補として扱う。

## 構成

- 本文は約 3,800 語 (p25–p75: 3,500–4,200)、約 50 段落、番号付きの章は 5〜7 個。いずれも本文 9 ページの camera-ready 版の値で、8 ページの投稿版では 1 割ほど少ないのが普通。
- 最も多い骨格:
  - Introduction → Related Work → Method → Experiments (→ Analysis) → Conclusion
  - その後に Limitations → (Ethics) → References → 付録 A, B, C…
- Related Work:
  - 約 6 割の論文が第 2 章に置き、約 2 割が結論の直前に置く。後ろに置く論文は減っている。
  - 長さは 3 段落・本文の約 10%。テーマごとに 1 段落ずつ書き、段落の頭にテーマを太字で置くことが多い。
- 手法の章: 汎用の名前 ("Method") の代わりに、提案手法の名前を章のタイトルにすることが多い。
- 実験の章: 本文の約 3 割。"Datasets / Baselines / Metrics / Implementation Details / Main Results" と小節や段落見出しで区切るのが定型。
- 結論の章: 1〜2 段落、本文の約 3%。Future Work は結論の中で触れる。
- Limitations: ほぼ全論文にあり、140 語前後。
- 付録: 97% の論文にあり、中央値で約 2,000 語。本文を短くして詳細を付録に回す傾向が年々強まっている。ただし評価に必要なものは本文に置く規定がある (submission-rules.md)。
- 参考文献は約 50 本 (p5–p95: 28–92)。

## Introduction

- 7〜8 段落、約 600 語、本文の約 16%。
- 典型的な流れ:
  1. 背景
  2. 既存手法の課題 ("However, ...")
  3. 本論文の提案 ("In this paper, we propose ...")
  4. 手法の要点
  5. 結果の要約 (数値を入れる)
  6. 貢献の箇条書き
- 2026 年の論文では、半数以上が貢献を箇条書きにしている。

## Abstract とタイトル

- Abstract は 1 段落、約 160 語。
  - 背景 → 課題 → 提案 ("we propose / introduce") → 結果 → (公開) の順に書く。
  - 具体的な数値を入れる Abstract は約 1/4。
- タイトルは約 10 語。
  - 「手法名: 説明」の形が約 4 割で、コロンを使うタイトルは 2026 年に 7 割を超えた。
  - 疑問形は約 5%。

## 文体

- 1 文は平均 21〜23 語。40 語以上の文は約 7%。
- 段落は約 67 語 (3 文前後)。
- 主語は "we" で、能動態で書くのが基本。文頭の語は We (15%) が最も多く、The、In、This、For、To が続く。
  - よく使う動詞: use, propose, evaluate, introduce, conduct, observe, find, compare。
- 逆接は "However" が中心。"In contrast" で対比し、"Specifically" で直前の文を具体化する。
- 括弧による補足が多い (1,000 語あたり約 26 個)。引用、略語の定義、数値の補足に使う。
- 縮約形 (don't, it's) は使わない。疑問文は研究課題 (RQ) を示すときに限られる。

## 注意したい語

- 次の語は 2024 年に急増し、その後減った。LLM が書いた文章の目印として受け取られやすい。
  - delve, showcase, intricate, pivotal
  - tapestry, realm, meticulous, seamless (この 4 語は集計していない一般的な目印)
- 次の語は採択論文でも多数派が使っており、使うこと自体は問題ない。ただし多用すると目立つ。
  - notably, comprehensive, leverage, underscore
- 強調語 (significantly, substantially, consistently, remarkably, dramatically) は 2022 年から 2026 年に 2 倍に増えた。
  - "significantly" を統計的検定なしに使うと、R4 (過剰な主張) と R5 (有意性) の指摘を受けやすい。
