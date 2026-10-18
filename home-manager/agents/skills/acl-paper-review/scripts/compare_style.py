# /// script
# requires-python = ">=3.13"
# dependencies = ["pymupdf>=1.28.2", "pymupdf-layout>=1.28.2", "pymupdf4llm>=1.28.2"]
# ///
"""投稿予定の論文を、ACL / EMNLP 採択論文の分布と比べて Markdown で報告する。

usage: uv run compare_style.py <paper.pdf | paper.txt>
    [--type method|resource|analysis|survey_position] [--title "..."]
    [--stage submission|camera-ready] [--length long|short]
"""

import argparse
import json
import re
import tempfile
from pathlib import Path

from features import PAPER_GROUPS, classify, features, paper_group, summary

HERE = Path(__file__).resolve().parent
DEFAULT_NORMS = HERE.parent / "references" / "norms.json"

LABELS = {
    "body_words": "本文の語数",
    "body_paras": "本文の段落数",
    "n_sections": "番号付きの章数",
    "n_subsections": "小節 (x.y) の数",
    "intro_paras": "Introduction の段落数",
    "intro_words": "Introduction の語数",
    "abstract_words": "Abstract の語数",
    "references": "参考文献数 (推定)",
    "appendix_words": "付録の語数",
    "limitations_words": "Limitations の語数",
    "para_words_median": "段落の語数 (中央値)",
    "sent_len_mean": "文の平均語数",
    "long_sent_ratio": "40 語以上の文の割合",
    "share_introduction": "本文に占める割合: Introduction",
    "share_related_work": "本文に占める割合: Related Work",
    "share_method": "本文に占める割合: Method",
    "share_experiment": "本文に占める割合: Experiments",
    "share_analysis": "本文に占める割合: Analysis",
    "share_conclusion": "本文に占める割合: Conclusion",
    "per1k_we": "1000 語あたり: we",
    "per1k_passive": "1000 語あたり: 受動態 (近似)",
    "per1k_hedge": "1000 語あたり: hedge (may, might, suggest など)",
    "per1k_booster": "1000 語あたり: 強調語 (significantly など)",
    "per1k_however": "1000 語あたり: however",
    "per1k_notably": "1000 語あたり: notably",
    "per1k_leverage": "1000 語あたり: leverage",
    "per1k_comprehensive": "1000 語あたり: comprehensive",
    "per1k_underscore": "1000 語あたり: underscore",
    "per1k_crucial": "1000 語あたり: crucial",
    "per1k_robust": "1000 語あたり: robust",
}
# LLM が書いた文章の目印として査読者に受け取られやすい語。採択論文での使用は 2024 年をピークに減っている。
MARKED_WORDS = re.compile(
    r"\b(delv(e|es|ed|ing)|showcas(e|es|ed|ing)|intricate|pivotal|underscor(e|es|ed|ing)|tapestry|realm|meticulous(ly)?|seamless(ly)?|paramount)\b",
    re.IGNORECASE,
)
END_OF_CONTENT = re.compile(
    r"^(\d+\.?\s+)?(Limitations|Ethics Statement|Ethical Considerations|Acknowledge?ments?|References)$"
)
# 自分のコードやデータへのリンクと、自分の過去の研究への言及。他人のリポジトリへのリンクは拾わない。
NON_ANONYMOUS = re.compile(
    r"\b(our|the) (code|data|dataset|benchmark|models?|implementation|resources?)\b[^.]{0,80}?(github\.com|huggingface\.co|https?://)\S+"
    r"|\b(available|released?|publicly)\b[^.]{0,40}?(github\.com|huggingface\.co)\S+"
    r"|\b(in )?our (previous|prior|earlier) (work|paper|study)\b",
    re.IGNORECASE,
)
# 匿名化サービスのリンクは問題にしない。
ANONYMIZED = re.compile(r"anonymous|4open\.science", re.IGNORECASE)
# 本文のページ上限 (Limitations、Ethics、参考文献、付録は数えない)。camera-ready は 1 ページ増える。
PAGE_LIMIT = {
    ("long", "submission"): 8,
    ("long", "camera-ready"): 9,
    ("short", "submission"): 4,
    ("short", "camera-ready"): 5,
}
# ページ数に比例する指標。比較対象の採択論文は camera-ready なので、投稿版では上限の比で分布を縮める。
PAGE_BOUND_KEYS = ("body_words", "body_paras")
# 分布は long 論文だけから作っているので、short 論文では分量の指標を比べない。
AMOUNT_KEYS = (
    "body_words",
    "body_paras",
    "n_sections",
    "n_subsections",
    "intro_paras",
    "intro_words",
    "references",
    "appendix_words",
)


def band(value: float, q: dict[str, float]) -> str:
    if value < q["p5"]:
        return "**下位 5% 未満**"
    if value > q["p95"]:
        return "**上位 5% 超**"
    if value < q["p25"]:
        return "やや少ない (p5–p25)"
    if value > q["p75"]:
        return "やや多い (p75–p95)"
    return "典型的 (p25–p75)"


def fmt(key: str, v: float) -> str:
    if key.startswith("share_") or key == "long_sent_ratio":
        return f"{100 * v:.1f}%"
    if key.startswith("per1k_") or key == "sent_len_mean":
        return f"{v:.2f}"
    return f"{v:.0f}"


def end_headings(pdf: Path) -> tuple[list[tuple[int, int, float, str]], int]:
    """本文の後に来る見出しを、PDF 上の読み順 (ページ, 段, 高さ) に並べて返す。

    抽出テキストの順序はレイアウト解析が段の順を取り違えることがあるので、位置で並べる。
    要素は (ページ番号, 段 (0: 左, 1: 右), 上からの位置 (%), 見出し)。
    """
    import pymupdf

    found = []
    with pymupdf.open(pdf) as doc:
        for page in doc:
            for x0, y0, x1, y1, text, *_ in page.get_text("blocks"):
                lines = [line.strip() for line in text.strip().splitlines()]
                # 投稿版では行番号が見出しと同じブロックに入ることがあるので、番号だけの行を飛ばす。
                head = next((line for line in lines if not line.isdigit()), "")
                if END_OF_CONTENT.match(head):
                    col = 1 if x0 > page.rect.width / 2 else 0
                    found.append(
                        (page.number + 1, col, 100 * y0 / page.rect.height, head)
                    )
        return sorted(found), doc.page_count


def content_end(
    headings: list[tuple[int, int, float, str]], n_pages: int, limit: int
) -> str:
    """本文 (ページ制限の対象) が何ページ目のどこで終わるかと、上限に収まるかを返す。"""
    if not headings:
        return "Limitations / References の見出しが見つからない"
    n, col, pos, head = headings[0]
    # 次のページの先頭で始まるなら、本文は前のページで終わっている。
    end_page = n - 1 if pos < 12 and col == 0 else n
    verdict = (
        f"**上限 {limit} ページを超過**"
        if end_page > limit
        else f"上限 {limit} ページ以内"
    )
    return (
        f"「{head}」が {n} ページ目の{'右段' if col else '左段'}・上から {pos:.0f}% の位置で始まる"
        f" (全 {n_pages} ページ)。本文は {end_page} ページ目で終わる: {verdict}"
    )


def limitations_after_references(headings: list[tuple[int, int, float, str]]) -> bool:
    names = [re.sub(r"^\d+\.?\s+", "", h[3]) for h in headings]
    return (
        "Limitations" in names
        and "References" in names
        and names.index("Limitations") > names.index("References")
    )


def snippets(text: str, pattern: re.Pattern[str], limit: int = 10) -> list[str]:
    out = []
    for m in pattern.finditer(text):
        s = max(0, m.start() - 60)
        out.append("…" + " ".join(text[s : m.end() + 60].split()) + "…")
        if len(out) >= limit:
            break
    return out


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("paper", type=Path)
    parser.add_argument("--norms", type=Path, default=DEFAULT_NORMS)
    parser.add_argument(
        "--type",
        choices=PAPER_GROUPS,
        help="論文の種類。省略時はタイトルと Abstract から推定する",
    )
    parser.add_argument("--title", default="", help="種類の推定に使うタイトル")
    parser.add_argument(
        "--stage",
        choices=("submission", "camera-ready"),
        default="submission",
        help="原稿の段階 (default: submission)",
    )
    parser.add_argument(
        "--length",
        choices=("long", "short"),
        default="long",
        help="論文の種別 (default: long)",
    )
    args = parser.parse_args()
    limit = PAGE_LIMIT[(args.length, args.stage)]
    norms = json.loads(args.norms.read_text())

    if args.paper.suffix.lower() == ".pdf":
        from extract import extract_text

        text, n_line_numbers = extract_text(args.paper)
        with tempfile.NamedTemporaryFile(
            "w", suffix=".txt", delete=False, encoding="utf-8"
        ) as f:
            f.write(text)
        feat = features(Path(f.name))
        Path(f.name).unlink()
    else:
        text = args.paper.read_text(encoding="utf-8")
        feat = features(args.paper)
        n_line_numbers = 0
    values = summary(feat)
    meta = norms["meta"]
    if args.type:
        group, how = args.type, "指定"
    else:
        group = paper_group(
            classify(args.title or feat["front"], feat["abstract_text"])
        )
        how = "タイトルと Abstract から推定"
    type_norms = norms["by_type"][group]

    print(f"# 形式・文体の比較: {args.paper.name}\n")
    print(f"論文の種類: **{group}** ({how})。違っていれば --type で指定し直す。\n")
    print(
        f"比較対象: {meta['since']} 年以降の {', '.join(meta['tracks'])} 採択論文のうち {group} の {meta['n_by_type'][group]} 本"
        f" (全体 {meta['n_papers']} 本)。\n"
    )
    if n_line_numbers:
        print(
            f"> 投稿版の行番号を {n_line_numbers} 個検出し、取り除いた。"
            "それでもレイアウト解析が段をまたいで見出しや段落を融合させることがあり、"
            "章の検出漏れ、段落数の過少、文の長さの過大が起きうる。"
            "LaTeX ソースがあれば `\\usepackage[preprint]{acl}` でコンパイルし直した PDF で測り直すと正確になる。\n"
        )
    scale = 1.0
    if args.length == "short":
        print(
            "> short 論文: 比較対象は long 論文だけなので、分量の指標 (語数・段落数・章数など) は比べない。\n"
        )
    elif args.stage == "submission":
        scale = (
            PAGE_LIMIT[("long", "submission")] / PAGE_LIMIT[("long", "camera-ready")]
        )
        print(
            "> 投稿版: 比較対象の採択論文は camera-ready (本文 9 ページ) なので、"
            "本文の語数と段落数の分布は 8/9 に縮めて比べる。\n"
        )
    is_pdf = args.paper.suffix.lower() == ".pdf"
    if is_pdf:
        headings, n_pages = end_headings(args.paper)
        print(f"- 本文の終わり: {content_end(headings, n_pages, limit)}")
    print(
        f"- 検出した章: {len(feat['sections'])} 個 / 小節: {feat['n_sub']} 個 / 付録の節: {len(feat['appendix_titles'])} 個"
    )
    for key, label in [
        ("limitations", "Limitations"),
        ("ethics", "Ethics"),
        ("acknowledgments", "Acknowledgments"),
        ("references", "References"),
    ]:
        print(
            f"- {label} 節: {'あり' if feat['special'].get(key) or (key == 'limitations' and feat['limitations_words']) else 'なし'}"
        )
    # テキストだけでは段の読み順を取り違えて誤検出するので、PDF の位置で判定する。
    if is_pdf and limitations_after_references(headings):
        print(
            "- **Limitations が References の後にある**: 規定では結論の後・References の前に置く"
        )
    print(
        f"- Introduction で貢献を明示: {'あり' if feat['intro_contrib'] else 'なし'} / 箇条書き: {'あり' if feat['intro_bullets'] else 'なし'}"
    )

    print("\n## 章立て\n")
    print("| # | タイトル | 分類 | 語数 | 段落 | 小節 |")
    print("| --- | --- | --- | --- | --- | --- |")
    for s in feat["sections"]:
        print(
            f"| {s['n']} | {s['title']} | {s['cat']} | {s['words']} | {s['paras']} | {s['subs']} |"
        )
    if feat["appendix_titles"]:
        print("\n付録: " + " / ".join(feat["appendix_titles"]))

    print("\n## 指標\n")
    print(
        f"| 指標 | この論文 | 同種 ({group}) p5 / p25 / p50 / p75 / p95 | 同種の中での位置 | 全体 p50 |"
    )
    print("| --- | --- | --- | --- | --- |")
    for key, label in LABELS.items():
        q = type_norms.get(key)
        if q is None or (args.length == "short" and key in AMOUNT_KEYS):
            continue
        if key in PAGE_BOUND_KEYS:
            q = {p: v * scale for p, v in q.items()}
        v = values.get(key, 0.0)
        qs = " / ".join(fmt(key, q[p]) for p in ("p5", "p25", "p50", "p75", "p95"))
        pos = (
            band(v, q)
            if key in values
            else ("該当章なし" if key.startswith("share_") else "未検出")
        )
        overall = norms["norms"].get(key)
        print(
            f"| {label} | {fmt(key, v)} | {qs} | {pos} | {fmt(key, overall['p50']) if overall else '-'} |"
        )

    marked = snippets(text, MARKED_WORDS)
    print("\n## 目立つ語\n")
    if marked:
        print("LLM 由来と受け取られやすい語の使用箇所 (最大 10 件):\n")
        for s in marked:
            print(f"- {s}")
    else:
        print("該当なし。")

    if args.stage == "submission":
        anon = [s for s in snippets(text, NON_ANONYMOUS) if not ANONYMIZED.search(s)]
        print("\n## 匿名性の確認候補\n")
        if anon:
            for s in anon:
                print(f"- {s}")
        else:
            print("該当なし。")
        if feat["special"].get("acknowledgments"):
            print("- **Acknowledgments 節がある**: 投稿版では書かない")

    contractions = snippets(text, re.compile(r"\b\w+(n[’']t|[’']re|[’']ve|[’']ll)\b"))
    if contractions:
        print("\n## 縮約形\n")
        for s in contractions:
            print(f"- {s}")


if __name__ == "__main__":
    main()
