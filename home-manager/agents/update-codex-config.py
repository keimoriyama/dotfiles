"""Merge a writable root without replacing app-managed Codex settings."""

import os
import sys
from pathlib import Path

import tomlkit


def update_config(path: Path, writable_root: str) -> None:
    if path.exists():
        document = tomlkit.parse(path.read_text(encoding="utf-8"))
    else:
        document = tomlkit.document()

    workspace_write = document.get("sandbox_workspace_write")
    if workspace_write is None:
        workspace_write = tomlkit.table()
        document["sandbox_workspace_write"] = workspace_write

    writable_roots = workspace_write.get("writable_roots")
    if writable_roots is None:
        writable_roots = tomlkit.array()
        workspace_write["writable_roots"] = writable_roots

    if writable_root in writable_roots:
        return
    writable_roots.append(writable_root)

    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.tmp")
    temporary.write_text(tomlkit.dumps(document), encoding="utf-8")
    os.replace(temporary, path)


def main() -> None:
    if len(sys.argv) != 3:
        raise SystemExit("usage: update-codex-config CONFIG WRITABLE_ROOT")
    update_config(Path(sys.argv[1]), sys.argv[2])


if __name__ == "__main__":
    main()
