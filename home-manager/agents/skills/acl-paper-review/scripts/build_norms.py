"""コーパスの特徴量から、指標ごとの分位点を norms.json に書く。

全体の分布と、論文の種類 (features.PAPER_GROUPS) ごとの分布を書く。

usage: uv run build_norms.py <features.jsonl> <norms.json> [--since 2024]
"""

import argparse
import json
import statistics as st
from collections import Counter, defaultdict
from pathlib import Path

from features import classify, paper_group, summary

PERCENTILES = (5, 25, 50, 75, 95)


def quantiles(values: dict[str, list[float]]) -> dict[str, dict[str, float]]:
    out = {}
    for k, xs in values.items():
        if len(xs) < 20:
            continue
        qs = st.quantiles(xs, n=100, method="inclusive")
        out[k] = {f"p{p}": round(qs[p - 1], 4) for p in PERCENTILES}
    return out


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("features", type=Path)
    parser.add_argument("out", type=Path)
    parser.add_argument("--since", type=int, default=2024)
    args = parser.parse_args()

    values: dict[str, list[float]] = defaultdict(list)
    by_group: dict[str, dict[str, list[float]]] = defaultdict(lambda: defaultdict(list))
    groups: Counter = Counter()
    tracks = set()
    with args.features.open() as f:
        for line in f:
            feat = json.loads(line)
            if (
                feat["year"] < args.since
                or len(feat["sections"]) < 3
                or feat["body_words"] < 1000
            ):
                continue
            group = paper_group(classify(feat["title"], feat["abstract"]))
            groups[group] += 1
            tracks.add(feat["track"])
            for k, v in summary(feat).items():
                values[k].append(v)
                by_group[group][k].append(v)
    meta = {
        "n_papers": sum(groups.values()),
        "n_by_type": dict(groups.most_common()),
        "since": args.since,
        "tracks": sorted(tracks),
    }
    out = {
        "meta": meta,
        "norms": quantiles(values),
        "by_type": {g: quantiles(v) for g, v in by_group.items()},
    }
    args.out.write_text(json.dumps(out, indent=2, ensure_ascii=False) + "\n")


if __name__ == "__main__":
    main()
