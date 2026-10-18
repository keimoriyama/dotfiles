"""論文テキストから構成・文体の特徴量を取り出す。

コーパス全体の特徴量を作るとき:
  uv run features.py <text_dir> <papers.jsonl> <features.jsonl>
"""

import json
import re
import sys
from collections import Counter
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

TOP_RE = re.compile(r"^(\d{1,2})\.?\s+([A-Z][^\n]{0,80})$")
SUB_RE = re.compile(r"^(\d{1,2})\.(\d{1,2})\.?\s+([A-Z][^\n]{0,100})$")
SUBSUB_RE = re.compile(r"^\d{1,2}\.\d{1,2}\.\d{1,2}\.?\s+[A-Z][^\n]{0,100}$")
APPX_RE = re.compile(r"^([A-H])(\.\d+)?\.?\s+([A-Z][^\n]{0,80})$")
UNNUMBERED = {
    "abstract": re.compile(r"^abstract$", re.IGNORECASE),
    "limitations": re.compile(r"^(\d+\s+)?limitations?$", re.IGNORECASE),
    "ethics": re.compile(
        r"^(\d+\s+)?(ethics? statement|ethical (considerations?|statement|impact)|broader impact)s?$",
        re.IGNORECASE,
    ),
    "acknowledgments": re.compile(r"^acknowle?dge?(ment|ments)?s?$", re.IGNORECASE),
    "references": re.compile(r"^references$", re.IGNORECASE),
}
ABBREV = re.compile(
    r"\b(et al|e\.g|i\.e|cf|vs|Fig|Figs|Sec|Secs|Eq|Eqs|Tab|App|No|resp|approx|etc)\.",
)
SENT_SPLIT = re.compile(r"(?<=[.!?])\s+(?=[A-Z\"“(])")
WORD_RE = re.compile(r"[A-Za-z][A-Za-z'-]*")

LEXICON = {
    "we": r"\bwe\b",
    "our": r"\b(our|ours)\b",
    "us": r"\bus\b",
    "i_first": r"\bI\b(?!\.)",
    "passive": r"\b(is|are|was|were|be|been|being)\s+(\w+ly\s+)?\w+(ed|en)\b",
    "hedge": r"\b(may|might|could|possibly|potentially|likely|suggests?|appears? to|seems? to)\b",
    "booster": r"\b(significantly|substantially|clearly|consistently|remarkably|dramatically)\b",
    "novel": r"\bnovel\b",
    "sota": r"\bstate[- ]of[- ]the[- ]art\b|\bSOTA\b",
    "however": r"\bhowever\b",
    "moreover": r"\bmoreover\b",
    "furthermore": r"\bfurthermore\b",
    "additionally": r"\badditionally\b",
    "notably": r"\bnotably\b",
    "specifically": r"\bspecifically\b",
    "in_contrast": r"\bin contrast\b",
    "leverage": r"\bleverag(e|es|ed|ing)\b",
    "delve": r"\bdelv(e|es|ed|ing)\b",
    "crucial": r"\bcrucial(ly)?\b",
    "comprehensive": r"\bcomprehensive(ly)?\b",
    "robust": r"\brobust(ly|ness)?\b",
    "intricate": r"\bintricate\b",
    "pivotal": r"\bpivotal\b",
    "showcase": r"\bshowcas(e|es|ed|ing)\b",
    "underscore": r"\bunderscor(e|es|ed|ing)\b",
    "llm": r"\bLLMs?\b|\blarge language models?\b",
    "bert": r"\bBERT\b",
    "contraction": r"\b\w+(n't|'re|'ve|'ll)\b",
    "question": r"\?",
    "colon_semicolon": r";",
    "parenthesis": r"\(",
    "e_g": r"\be\.g\.",
    "i_e": r"\bi\.e\.",
}
LEXICON_RE = {
    k: re.compile(v, re.IGNORECASE if k not in {"i_first", "llm", "bert"} else 0)
    for k, v in LEXICON.items()
}

CATEGORIES = [
    ("introduction", r"introduction"),
    ("related_work", r"related work|prior work|previous work|literature"),
    (
        "background",
        r"background|preliminar|problem (formulation|definition|statement)|task (definition|formulation)",
    ),
    ("method", r"method|approach|model|framework|proposed|architecture|our "),
    ("data", r"data|corpus|benchmark|annotation|collection"),
    ("experiment", r"experiment|setup|setting|evaluation"),
    ("results", r"result|performance"),
    ("analysis", r"analys|discussion|ablation|case stud|error"),
    ("conclusion", r"conclusion|concluding|summary|future work"),
]
CATEGORY_RE = [(name, re.compile(p, re.IGNORECASE)) for name, p in CATEGORIES]


def categorize(title: str) -> str:
    for name, pat in CATEGORY_RE:
        if pat.search(title):
            return name
    return "other"


def sentences(text: str) -> list[str]:
    protected = ABBREV.sub(lambda m: m.group(0).replace(".", "<D>"), text)
    return [s.replace("<D>", ".") for s in SENT_SPLIT.split(protected) if s.strip()]


def words(text: str) -> list[str]:
    return WORD_RE.findall(text)


def features(path: Path) -> dict:
    paras = [
        p.strip() for p in path.read_text(encoding="utf-8").split("\n\n") if p.strip()
    ]
    sections: list[dict] = []
    current: dict | None = None
    special: dict[str, int] = {}
    n_sub = n_subsub = 0
    body_paras: list[str] = []
    phase = "front"  # front -> abstract -> body -> refs
    appendix_titles: list[str] = []
    appendix_words = 0
    limitations_words = 0
    in_limitations = False
    # Ethics と Acknowledgments の段落は、直前の章 (多くは Conclusion) に数えない。
    in_backmatter = False
    refs_years = 0
    abstract_text: list[str] = []

    for p in paras:
        is_header = False
        for key, pat in UNNUMBERED.items():
            if len(p) < 60 and pat.match(p):
                special[key] = special.get(key, 0) + 1
                is_header = True
                in_limitations = key == "limitations"
                in_backmatter = key in ("ethics", "acknowledgments")
                if key == "abstract" and phase == "front":
                    phase = "abstract"
                elif key == "references":
                    phase = "refs"
                break
        if is_header:
            continue
        if phase == "refs":
            m = APPX_RE.match(p)
            if m and len(p) < 90 and not p.endswith("."):
                in_limitations = in_backmatter = False
                if m.group(2) is None:
                    appendix_titles.append(m.group(3))
                continue
            if in_limitations:
                limitations_words += len(words(p))
                continue
            refs_years += len(re.findall(r"\b(19|20)\d\d[a-z]?\.", p))
            if appendix_titles:
                appendix_words += len(words(p))
            continue
        m = TOP_RE.match(p)
        if m and len(p) < 90 and not p.endswith((".", ",")) and int(m.group(1)) <= 15:
            in_limitations = bool(UNNUMBERED["limitations"].match(m.group(2)))
            in_backmatter = False
            if in_limitations:
                continue
            last = sections[-1]["n"] if sections else 0
            n = int(m.group(1))
            # 抽出で見出しが 1 つ欠けても続きを拾えるよう、2 つ先まで許す。
            if last < n <= last + 2:
                phase = "body"
                current = {
                    "n": n,
                    "title": m.group(2),
                    "cat": categorize(m.group(2)),
                    "words": 0,
                    "paras": 0,
                    "subs": 0,
                }
                sections.append(current)
                continue
        if SUB_RE.match(p) and len(p) < 110 and not p.endswith("."):
            n_sub += 1
            if current:
                current["subs"] += 1
            continue
        if SUBSUB_RE.match(p) and len(p) < 110:
            n_subsub += 1
            continue
        w = len(words(p))
        if phase == "abstract":
            abstract_text.append(p)
        elif phase == "body":
            if in_limitations:
                limitations_words += w
                continue
            if in_backmatter or current is None or w < 15:
                continue
            current["words"] += w
            current["paras"] += 1
            body_paras.append(p)

    body = "\n".join(body_paras)
    body_words = words(body)
    sents = sentences(body)
    sent_lens = [len(words(s)) for s in sents if len(words(s)) >= 3]
    first_words = Counter()
    for s in sents:
        ws = words(s)
        if len(ws) >= 3:
            first_words[" ".join(ws[:1])] += 1
    lex = {k: len(r.findall(body)) for k, r in LEXICON_RE.items()}
    we_verbs = Counter(
        m.group(1).lower()
        for m in re.finditer(
            r"\b[Ww]e\s+(?:also\s+|further\s+|then\s+|first\s+)?([a-z]+)", body
        )
    )
    intro = next((s for s in sections if s["cat"] == "introduction"), None)
    intro_text = (
        body_paras[: intro["paras"]]
        if intro and sections and sections[0] is intro
        else []
    )
    intro_joined = "\n".join(intro_text)

    return {
        "id": path.stem,
        "sections": sections,
        "n_sub": n_sub,
        "n_subsub": n_subsub,
        "special": special,
        "appendix_titles": appendix_titles,
        "appendix_words": appendix_words,
        "limitations_words": limitations_words,
        "refs_years": refs_years,
        "abstract_words_text": len(words(" ".join(abstract_text))),
        "abstract_text": " ".join(abstract_text),
        "front": paras[0] if paras else "",
        "body_words": len(body_words),
        "body_paras": len(body_paras),
        "para_words": [len(words(p)) for p in body_paras],
        "n_sents": len(sent_lens),
        "sent_lens": sent_lens,
        "lex": lex,
        "first_words": dict(first_words.most_common(30)),
        "we_verbs": dict(we_verbs.most_common(30)),
        "intro_paras": len(intro_text),
        "intro_words": len(words(intro_joined)),
        "intro_contrib": bool(
            re.search(
                r"contributions?\s+(of this (paper|work)\s+)?(are|is|can be|as follows)|we (make|summarize) (the following|our) contributions|main contributions",
                intro_joined,
                re.IGNORECASE,
            )
        ),
        "intro_bullets": intro_joined.count("•")
        + len(re.findall(r"(?:^|\n)\s*[-–]\s", intro_joined)),
        "intro_in_this": bool(
            re.search(r"\bin this (paper|work|study)\b", intro_joined, re.IGNORECASE)
        ),
        "github": "github.com" in body
        or "github.com" in path.read_text(encoding="utf-8"),
    }


SUMMARY_CATEGORIES = (
    "introduction",
    "related_work",
    "method",
    "experiment",
    "analysis",
    "conclusion",
)
SUMMARY_LEXICON = (
    "we",
    "passive",
    "hedge",
    "booster",
    "however",
    "notably",
    "leverage",
    "comprehensive",
    "underscore",
    "crucial",
    "robust",
)


def summary(feat: dict) -> dict[str, float]:
    """比較に使う論文単位の指標。値が取れない指標は含めない。"""
    out: dict[str, float] = {
        "body_words": feat["body_words"],
        "body_paras": feat["body_paras"],
        "n_sections": len(feat["sections"]),
        "n_subsections": feat["n_sub"],
        "intro_paras": feat["intro_paras"],
        "intro_words": feat["intro_words"],
        "abstract_words": feat["abstract_words_text"],
        "references": feat["refs_years"],
        "appendix_words": feat["appendix_words"],
        "limitations_words": feat["limitations_words"],
    }
    # 0 は検出に失敗したか節がないことを表すので、分布に混ぜない。
    out = {k: v for k, v in out.items() if v}
    if feat["para_words"]:
        out["para_words_median"] = sorted(feat["para_words"])[
            len(feat["para_words"]) // 2
        ]
    if feat["sent_lens"]:
        out["sent_len_mean"] = sum(feat["sent_lens"]) / len(feat["sent_lens"])
        out["long_sent_ratio"] = sum(x >= 40 for x in feat["sent_lens"]) / len(
            feat["sent_lens"]
        )
    total = sum(s["words"] for s in feat["sections"])
    if total:
        for cat in SUMMARY_CATEGORIES:
            # その章がない論文は分布に含めず、ある論文の中での割合を見る。
            if words_in_cat := sum(
                s["words"] for s in feat["sections"] if s["cat"] == cat
            ):
                out[f"share_{cat}"] = words_in_cat / total
    if feat["body_words"]:
        for k in SUMMARY_LEXICON:
            out[f"per1k_{k}"] = 1000 * feat["lex"][k] / feat["body_words"]
    return out


PAPER_TYPES = ("survey", "position", "resource", "analysis", "method")
# 比較用の分布を作るときの区分。position は数が少ないので survey とまとめる。
PAPER_GROUPS = ("method", "resource", "analysis", "survey_position")
_I = re.IGNORECASE
_SURVEY_TITLE = re.compile(
    r"\bsurvey\b|\bsystematic (literature )?review\b|: an? (critical |systematic )?review\b|^a review of\b",
    _I,
)
_POSITION = re.compile(
    r"^position:|\bposition paper\b|\bthis (paper|work) argues\b", _I
)
_RESOURCE_TITLE = re.compile(
    r"\b(benchmark|dataset|corpus|corpora|treebank|test ?suite|testbed|knowledge graph|lexicon)s?\b|^benchmarking\b|: benchmarking\b|[a-z\d-]bench\b",
    _I,
)
_RESOURCE_ABS = re.compile(
    r"\bwe (introduce|present|construct|build|release|create|curate|collect)\s+(\S+\s+){0,4}?(a|an|the first)\s+(\S+\s+){0,5}?(benchmark|dataset|corpus|treebank|test ?suite)\b",
    _I,
)
_ANALYSIS_TITLE = re.compile(
    r"^(do|does|did|can|could|is|are|was|were|why|what|which|who|should|will|would|how (do|does|did|can|could|much|many|well|far|good|robust|reliable|faithful|sensitive)|on the|towards understanding|understanding|probing|analy[sz]ing|investigating|revisiting|rethinking|examining|disentangling|dissecting|measuring|quantifying|evaluating)\b|"
    r"\?|\b(an? (empirical|systematic|comprehensive|large-scale|controlled|cross-lingual|comparative) (study|analysis|investigation|evaluation)|"
    r"lessons from|the (role|effect|impact|limits?) of|a case study|an analysis of)\b",
    _I,
)
_ANALYSIS_ABS = re.compile(
    r"\b(we|this (paper|work|study))\s+(systematically\s+|empirically\s+|comprehensively\s+|thoroughly\s+)?(investigates?|analy[sz]es?|examines?|stud(y|ies)|probes?)\s+(whether|how|why|the extent|what)\b",
    _I,
)
_PROPOSE = re.compile(
    r"\b(we|this (paper|work|study)) (propose|introduce|present|develop|design)s?\b\s+(?!(a|an|the first|the)\s+(\S+\s+){0,4}?(benchmark|dataset|corpus|study|analysis)\b)",
    _I,
)


def classify(title: str, abstract: str) -> str:
    """タイトルと Abstract から論文の種類を推定する。

    LIMITATION: キーワード規則による推定。データセットと手法を同時に提案する論文は、
    タイトルにデータセット系の語があれば resource、なければ method になる。
    """
    head = " ".join(abstract.split()[:150])
    if _SURVEY_TITLE.search(title):
        return "survey"
    if _POSITION.search(title) or _POSITION.search(head):
        return "position"
    if _RESOURCE_TITLE.search(title) or (
        _RESOURCE_ABS.search(head)
        and not _PROPOSE.search(head.replace(_RESOURCE_ABS.search(head).group(0), ""))
    ):
        return "resource"
    if _ANALYSIS_TITLE.search(title) or (
        _ANALYSIS_ABS.search(head) and not _PROPOSE.search(head)
    ):
        return "analysis"
    return "method"


def paper_group(paper_type: str) -> str:
    return "survey_position" if paper_type in ("survey", "position") else paper_type


def main() -> None:
    text_dir, meta_path, out_path = map(Path, sys.argv[1:4])
    meta = {}
    with meta_path.open() as f:
        for line in f:
            r = json.loads(line)
            meta[r["id"]] = r
    paths = sorted(text_dir.glob("*.txt"))
    with ProcessPoolExecutor() as pool, out_path.open("w") as out:
        for feat in pool.map(features, paths, chunksize=50):
            m = meta.get(feat["id"], {})
            feat["title"] = m.get("title", "")
            feat["abstract"] = m.get("abstract") or ""
            feat["year"] = int(feat["id"].split(".")[0])
            feat["track"] = feat["id"].split(".")[1]
            out.write(json.dumps(feat, ensure_ascii=False) + "\n")


if __name__ == "__main__":
    main()
