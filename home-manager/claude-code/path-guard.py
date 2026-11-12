"""ファイル系ツールがホーム配下で触れる範囲を ~/ghq と ~/.claude に限る。

Claude Code の PreToolUse hook として stdin から JSON を受け取る。
Bash は sandbox.filesystem が同じ範囲を強制するので、ここでは扱わない。
ホームの外 (/nix, /tmp など) は制限しない。
"""

import json
import os
import sys

from pydantic import BaseModel

HOME = os.path.realpath(os.path.expanduser("~"))
ALLOWED_ROOTS = [
    os.path.realpath(os.path.join(HOME, "ghq")),
    os.path.realpath(os.path.join(HOME, ".claude")),
]
GLOB_CHARS = "*?[{"


class ToolInput(BaseModel):
    file_path: str | None = None
    notebook_path: str | None = None
    path: str | None = None
    pattern: str | None = None


class HookInput(BaseModel):
    tool_name: str
    cwd: str
    tool_input: ToolInput


def resolve(raw: str, cwd: str) -> str:
    # symlink を辿った先で判定し、~/ghq 内のリンク経由の迂回を防ぐ。
    return os.path.realpath(os.path.join(cwd, os.path.expanduser(raw)))


def is_within(path: str, root: str) -> bool:
    return path == root or path.startswith(root + os.sep)


def is_denied(path: str) -> bool:
    if not is_within(path, HOME):
        return False
    return not any(is_within(path, root) for root in ALLOWED_ROOTS)


def glob_base(pattern: str) -> str | None:
    """Glob の pattern が絶対パスなら、ワイルドカードより前の部分を返す。"""
    if not pattern.startswith(("/", "~")):
        return None
    cut = min(
        (i for i, c in enumerate(pattern) if c in GLOB_CHARS),
        default=len(pattern),
    )
    return os.path.dirname(pattern[:cut]) or "/"


def targets(hook: HookInput) -> list[str]:
    ti = hook.tool_input
    raws = [p for p in (ti.file_path, ti.notebook_path, ti.path) if p]
    if hook.tool_name in ("Glob", "Grep") and ti.path is None:
        raws.append(hook.cwd)
    if hook.tool_name == "Glob" and ti.pattern:
        base = glob_base(ti.pattern)
        if base:
            raws.append(base)
    return [resolve(raw, hook.cwd) for raw in raws]


def main() -> None:
    hook = HookInput.model_validate_json(sys.stdin.read())
    denied = [p for p in targets(hook) if is_denied(p)]
    if not denied:
        return
    reason = (
        f"Access to {denied[0]} is prohibited. Under the home directory, "
        "file tools may only touch ~/ghq and ~/.claude. "
        "Work inside ~/ghq, or ask the user for the content you need."
    )
    print(
        json.dumps(
            {
                "hookSpecificOutput": {
                    "hookEventName": "PreToolUse",
                    "permissionDecision": "deny",
                    "permissionDecisionReason": reason,
                }
            }
        )
    )


main()
