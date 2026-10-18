"""PDF から本文テキストを抽出する。

acl-anthology-crawler の extract と同じ処理に、投稿版 (review モード) の PDF の
余白に入る行番号を取り除く処理を加えている。
"""

import re
from pathlib import Path
from typing import Any

import pymupdf
import pymupdf.layout  # import するとレイアウト解析モデルが有効になる
from pymupdf4llm.helpers.document_layout import parse_document

TEXT_BOX_CLASSES = frozenset(
    {"title", "section-header", "text", "list-item", "footnote"}
)
# レイアウト解析が text に誤分類したキャプションを落とす。
CAPTION_RE = re.compile(r"^(Figure|Fig\.|Table)\s*\d+\s*[:.]")
# スパン間の隙間がフォントサイズのこの割合を超えたら空白とみなす。
SPACE_GAP_RATIO = 0.1
# acl.sty の review モードは lineno で 3 桁にゼロ埋めした行番号を lightgray で描く。
LINE_NUMBER_COLOR = 0xBFBFBF
LINE_NUMBER_RE = re.compile(r"\d{3,4}")
# 段落の 1 行目の字下げ (ACL スタイルでは約 11pt) とみなす最小の幅。
INDENT_MIN = 5.0

LineNumber = tuple[str, pymupdf.Rect]


def line_numbers(page: pymupdf.Page) -> list[LineNumber]:
    """ページ上の行番号の文字列と位置を返す。"""
    marks = []
    for block in page.get_text("dict")["blocks"]:
        for line in block.get("lines", []):
            for span in line["spans"]:
                text = span["text"].strip()
                if span["color"] == LINE_NUMBER_COLOR and LINE_NUMBER_RE.fullmatch(
                    text
                ):
                    marks.append((text, pymupdf.Rect(span["bbox"])))
    return marks


def strip_line_numbers(
    spans: list[dict[str, Any]], marks: list[LineNumber]
) -> list[dict[str, Any]]:
    """スパンから行番号を取り除く。

    レイアウト解析は、行番号を隣の単語と同じスパンに融合させる ("Background084")。
    行番号と位置が重なるスパンから、その番号の文字列を取り除く。
    """
    out = []
    for span in spans:
        text = span["text"]
        rect = pymupdf.Rect(span["bbox"])
        for number, mark in marks:
            if number not in text or not rect.intersects(mark):
                continue
            stripped = text.strip()
            if stripped.endswith(number):
                text = stripped[: -len(number)]
            elif stripped.startswith(number):
                text = stripped[len(number) :]
            else:
                text = text.replace(number, " ", 1)
        if text.strip():
            out.append({**span, "text": text.strip()})
    return out


def line_text(spans: list[dict[str, Any]]) -> str:
    """スパンを連結する。PDF に空白文字がなく座標だけで単語が離れている場合も空白を補う。"""
    text = ""
    prev_x1 = None
    for span in spans:
        if (
            prev_x1 is not None
            and span["bbox"][0] - prev_x1 > span["size"] * SPACE_GAP_RATIO
        ):
            text += " "
        text += span["text"]
        prev_x1 = span["bbox"][2]
    return text.strip()


def append_fragment(text: str, fragment: str, separator: str) -> str:
    """text の後ろに fragment をつなぐ。

    fragment が小文字で始まり text の文が終わっていなければ、同じ文の続きとみなして
    空白でつなぐ。行末のハイフンはそのとき外す。それ以外は separator で区切る。
    """
    if not text:
        return fragment
    if fragment[0].islower():
        # LIMITATION: "state-of-the-art" のような複合語が行末で割れた場合もハイフンを外してしまう。
        if text.endswith("-"):
            return text[:-1] + fragment
        if not text.endswith((".", "!", "?", ":")):
            return text + " " + fragment
    return text + separator + fragment


def box_paragraphs(
    textlines: list[dict[str, Any]], marks: list[LineNumber]
) -> list[str]:
    """ボックスの行を段落にまとめる。

    行番号のあるページでは、レイアウト解析が複数の段落を 1 つのボックスにまとめてしまう。
    その場合は 1 行目の字下げで段落を分ける。行番号のないページは 1 ボックス 1 段落とする。
    """
    lines = []
    for line in textlines:
        spans = strip_line_numbers(line["spans"], marks) if marks else line["spans"]
        if text := line_text(spans):
            lines.append((min(s["bbox"][0] for s in spans), text))
    if not lines:
        return []
    left = min(x0 for x0, _ in lines)
    paragraphs = [""]
    for x0, text in lines:
        if marks and paragraphs[-1] and x0 - left > INDENT_MIN:
            paragraphs.append("")
        paragraphs[-1] = append_fragment(paragraphs[-1], text, " ")
    return [" ".join(p.split()) for p in paragraphs if p]


def extract_text(pdf_path: Path) -> tuple[str, int]:
    """本文テキストと、取り除いた行番号の数を返す。"""
    with pymupdf.open(pdf_path) as doc:
        marks = [line_numbers(page) for page in doc]
        parsed = parse_document(
            doc, embed_images=False, write_images=False, use_ocr=False
        )
    text = ""
    footnotes = []
    for page_marks, page in zip(marks, parsed.pages, strict=True):
        for box in page.boxes:
            if box.boxclass not in TEXT_BOX_CLASSES or not box.textlines:
                continue
            for paragraph in box_paragraphs(box.textlines, page_marks):
                if CAPTION_RE.match(paragraph):
                    continue
                if box.boxclass == "footnote":
                    footnotes.append(paragraph)
                else:
                    text = append_fragment(text, paragraph, "\n\n")
    # 脚注を本文の途中に挟むと、段やページをまたぐ文が分断されるので末尾にまとめる。
    return "\n\n".join([text, *footnotes]) + "\n", sum(len(m) for m in marks)
