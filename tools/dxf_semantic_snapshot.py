#!/usr/bin/env python3
"""Create and compare compact, reviewable semantic DXF regression oracles.

The ordinary DXF comparator needs two complete drawings.  Full LEparagliding
regression drawings are deliberately not checked into the repository because
they are several megabytes each.  This helper stores the normalized ENTITIES
model used by :mod:`dxf_semantic_diff` in a compressed JSON snapshot instead.

Snapshots retain every entity type, layer, CAD colour, group-code occurrence,
polyline vertex, and coordinate.  Entity order, numeric text formatting,
comments, and volatile handles have already been removed by the normalizer.
Comparison remains tolerance-aware and reports the same actionable missing,
unexpected, and changed-entity diagnostics as the full-DXF comparator.
"""

from __future__ import annotations

import argparse
import gzip
import json
import sys
from collections import Counter
from pathlib import Path
from typing import Any, Sequence

from dxf_semantic_diff import (
    ComparisonResult,
    DxfParseError,
    SemanticEntity,
    compare_entities,
    load_ascii_dxf,
)


SCHEMA = "leparagliding-dxf-semantic-v1"


def _entity_record(entity: SemanticEntity) -> list[Any]:
    return [
        entity.kind,
        [[code, list(values)] for code, values in entity.fields],
    ]


def _record_sort_key(record: list[Any]) -> str:
    return json.dumps(record, ensure_ascii=True, separators=(",", ":"))


def _field_first(entity: SemanticEntity, name: str) -> object | None:
    values = entity.field_map().get(name, ())
    return values[0] if values else None


def create_snapshot(dxf_path: Path | str, snapshot_path: Path | str) -> None:
    """Write a deterministic gzip-compressed semantic snapshot."""

    entities = load_ascii_dxf(dxf_path)
    records = sorted((_entity_record(entity) for entity in entities), key=_record_sort_key)
    kind_counts = Counter(entity.kind for entity in entities)
    layer_counts = Counter(str(_field_first(entity, "8")) for entity in entities)
    color_counts = Counter(str(_field_first(entity, "62")) for entity in entities)
    document = {
        "schema": SCHEMA,
        "entity_count": len(entities),
        "kind_counts": dict(sorted(kind_counts.items())),
        "layer_counts": dict(sorted(layer_counts.items())),
        "color_counts": dict(sorted(color_counts.items())),
        "entities": records,
    }

    target = Path(snapshot_path)
    target.parent.mkdir(parents=True, exist_ok=True)
    # mtime=0 makes regeneration byte-for-byte deterministic.
    with target.open("wb") as raw_output:
        with gzip.GzipFile(filename="", mode="wb", fileobj=raw_output, mtime=0) as output:
            output.write(
                json.dumps(
                    document,
                    ensure_ascii=True,
                    separators=(",", ":"),
                    sort_keys=True,
                ).encode("utf-8")
            )


def load_snapshot(snapshot_path: Path | str) -> tuple[SemanticEntity, ...]:
    """Load and validate a semantic snapshot."""

    path = Path(snapshot_path)
    try:
        with gzip.open(path, "rt", encoding="utf-8") as input_file:
            document = json.load(input_file)
    except (OSError, UnicodeError, json.JSONDecodeError) as exc:
        raise DxfParseError(f"cannot read semantic snapshot {path}: {exc}") from exc

    if not isinstance(document, dict) or document.get("schema") != SCHEMA:
        raise DxfParseError(f"{path}: unsupported semantic snapshot schema")
    records = document.get("entities")
    if not isinstance(records, list):
        raise DxfParseError(f"{path}: semantic snapshot has no entity list")

    entities: list[SemanticEntity] = []
    try:
        for record in records:
            kind, raw_fields = record
            fields = tuple(
                (str(code), tuple(values)) for code, values in raw_fields
            )
            entities.append(SemanticEntity(str(kind), fields))
    except (TypeError, ValueError) as exc:
        raise DxfParseError(f"{path}: malformed semantic entity record") from exc

    if document.get("entity_count") != len(entities):
        raise DxfParseError(f"{path}: semantic entity count is inconsistent")
    return tuple(entities)


def compare_snapshot(
    snapshot_path: Path | str,
    actual_dxf_path: Path | str,
    *,
    abs_tol: float = 1.0e-6,
    rel_tol: float = 0.0,
    max_differences: int = 20,
) -> ComparisonResult:
    """Compare a generated DXF with a stored normalized snapshot."""

    return compare_entities(
        load_snapshot(snapshot_path),
        load_ascii_dxf(actual_dxf_path),
        abs_tol=abs_tol,
        rel_tol=rel_tol,
        max_differences=max_differences,
    )


def _argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Create or compare a compressed semantic DXF oracle."
    )
    subparsers = parser.add_subparsers(dest="command", required=True)

    create = subparsers.add_parser("create", help="create a snapshot from a DXF")
    create.add_argument("dxf", type=Path)
    create.add_argument("snapshot", type=Path)

    compare = subparsers.add_parser("compare", help="compare a snapshot with a DXF")
    compare.add_argument("snapshot", type=Path)
    compare.add_argument("actual", type=Path)
    compare.add_argument("--abs-tol", type=float, default=1.0e-6)
    compare.add_argument("--rel-tol", type=float, default=0.0)
    compare.add_argument("--max-differences", type=int, default=20)
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    args = _argument_parser().parse_args(argv)
    try:
        if args.command == "create":
            create_snapshot(args.dxf, args.snapshot)
            entities = load_snapshot(args.snapshot)
            print(f"Wrote DXF semantic snapshot: {len(entities)} entities")
            return 0

        result = compare_snapshot(
            args.snapshot,
            args.actual,
            abs_tol=args.abs_tol,
            rel_tol=args.rel_tol,
            max_differences=args.max_differences,
        )
    except (DxfParseError, ValueError) as exc:
        print(f"DXF semantic snapshot error: {exc}", file=sys.stderr)
        return 2

    if result.equivalent:
        print(
            f"DXF semantic snapshot match: {result.actual_count} entities "
            f"(abs_tol={args.abs_tol:g}, rel_tol={args.rel_tol:g})"
        )
        return 0

    print(
        f"DXF semantic snapshot mismatch: expected {result.expected_count} "
        f"entities, actual {result.actual_count}"
    )
    for difference in result.differences:
        print(f"  {difference}")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
