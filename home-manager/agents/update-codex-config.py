"""Merge Nix-managed values without replacing app-managed Codex settings."""

import os
import sys
from pathlib import Path

import tomlkit
from pydantic import BaseModel, ConfigDict, TypeAdapter
from tomlkit.items import Table


class McpServer(BaseModel):
    model_config = ConfigDict(extra="forbid", frozen=True)

    command: str
    args: list[str] = []


McpServers = TypeAdapter(dict[str, McpServer])


def ensure_table(parent: Table | tomlkit.TOMLDocument, key: str) -> Table:
    table = parent.get(key)
    if table is None:
        table = tomlkit.table()
        parent[key] = table
    return table


def add_writable_root(
    document: tomlkit.TOMLDocument, writable_root: str
) -> None:
    workspace_write = ensure_table(document, "sandbox_workspace_write")
    writable_roots = workspace_write.get("writable_roots")
    if writable_roots is None:
        writable_roots = tomlkit.array()
        workspace_write["writable_roots"] = writable_roots
    if writable_root not in writable_roots:
        writable_roots.append(writable_root)


def set_mcp_servers(
    document: tomlkit.TOMLDocument, servers: dict[str, McpServer]
) -> None:
    """Overwrite only the named servers; servers added by hand survive."""
    mcp_servers = ensure_table(document, "mcp_servers")
    for name, server in servers.items():
        table = tomlkit.table()
        table["command"] = server.command
        table["args"] = server.args
        mcp_servers[name] = table


def update_config(
    path: Path, writable_root: str, servers: dict[str, McpServer]
) -> None:
    original = path.read_text(encoding="utf-8") if path.exists() else ""
    document = tomlkit.parse(original)

    add_writable_root(document, writable_root)
    set_mcp_servers(document, servers)

    updated = tomlkit.dumps(document)
    if updated == original:
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.tmp")
    temporary.write_text(updated, encoding="utf-8")
    os.replace(temporary, path)


def main() -> None:
    if len(sys.argv) != 4:
        raise SystemExit(
            "usage: update-codex-config CONFIG WRITABLE_ROOT MCP_SERVERS_JSON"
        )
    servers = McpServers.validate_json(sys.argv[3])
    update_config(Path(sys.argv[1]), sys.argv[2], servers)


if __name__ == "__main__":
    main()
