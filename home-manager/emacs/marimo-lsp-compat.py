"""Run marimo-lsp with compatibility for marimo 0.26."""

from __future__ import annotations

import inspect
from collections.abc import Iterable

import msgspec
from marimo._ast.app import InternalApp
from marimo._ast.cell import CellConfig
from marimo._runtime.commands import ExecuteCellCommand, ExecuteCellsCommand
from marimo._types.ids import CellId_t
from marimo_lsp import api as lsp_api
from marimo_lsp import main as run_server


def _with_data(
    self: InternalApp,
    *,
    cell_ids: Iterable[CellId_t],
    codes: Iterable[str],
    names: Iterable[str],
    configs: Iterable[CellConfig],
) -> InternalApp:
    self.apply_data(
        cell_ids=cell_ids,
        codes=codes,
        names=names,
        configs=configs,
    )
    return self


def _patch_execute_cells_command() -> None:
    if "cell_ids" in inspect.signature(ExecuteCellsCommand).parameters:
        return

    class CompatibleExecuteCellsCommand(ExecuteCellsCommand, tag="execute-cells"):
        execution_requests: list[ExecuteCellCommand] = msgspec.field(
            default_factory=list
        )
        cell_ids: list[CellId_t] = msgspec.field(default_factory=list)
        codes: list[str] = msgspec.field(default_factory=list)

        def __post_init__(self) -> None:
            if self.cell_ids or self.codes:
                self.execution_requests = [
                    ExecuteCellCommand(cell_id=cell_id, code=code)
                    for cell_id, code in zip(self.cell_ids, self.codes, strict=True)
                ]
            super().__post_init__()

    lsp_api._KernelExecuteCellsCommand = CompatibleExecuteCellsCommand


def main() -> None:
    if not hasattr(InternalApp, "with_data"):
        InternalApp.with_data = _with_data
    _patch_execute_cells_command()
    run_server()


if __name__ == "__main__":
    main()
