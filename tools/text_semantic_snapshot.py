#!/usr/bin/env python3
"""Create and compare semantic snapshots of LEparagliding text deliverables.

The report and line-list writers use fixed human-readable layouts.  This
normalizer deliberately ignores line endings, blank-line pagination, runs of
horizontal whitespace, and numeric text formatting.  It preserves nonblank
record order, all text and punctuation, and every numeric value.  Numeric
comparison uses explicit caller-supplied absolute and relative tolerances.

Exact normalized-file hashes remain in the CMake regression scripts as a
separate byte-stability gate; this helper adds an independently inspectable
semantic gate rather than replacing or weakening those hashes.
"""

from __future__ import annotations

import argparse
import difflib
import gzip
import json
import math
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Sequence


SCHEMA = "leparagliding-text-semantic-v1"
SUPPORTED_FORMATS = frozenset({"lep-out", "lines"})
NUMBER_PATTERN = re.compile(
    r"(?<![A-Za-z0-9_])"
    r"[-+]?(?:\d+\.\d*|\.\d+|\d+)(?:[EeDd][-+]?\d+)?"
    r"(?![A-Za-z0-9_])"
)


class TextSemanticError(ValueError):
    """Raised when a text deliverable or semantic snapshot is invalid."""


@dataclass(frozen=True)
class SemanticTextRecord:
    """One ordered nonblank output record with normalized text and numbers."""

    text_segments: tuple[str, ...]
    numbers: tuple[float, ...]

    @property
    def structure(self) -> tuple[str, ...]:
        return self.text_segments


@dataclass(frozen=True)
class TextComparisonResult:
    equivalent: bool
    expected_count: int
    actual_count: int
    differences: tuple[str, ...]


def _normalize_text(value: str) -> str:
    return re.sub(r"[ \t\f\v]+", " ", value).strip()


def parse_text_deliverable(
    text: str, source: str = "<memory>"
) -> tuple[SemanticTextRecord, ...]:
    """Normalize a report or line-list while retaining ordered semantics."""

    records: list[SemanticTextRecord] = []
    for line_number, line in enumerate(text.splitlines(), start=1):
        if not line.strip():
            continue
        text_segments: list[str] = []
        numbers: list[float] = []
        cursor = 0
        for match in NUMBER_PATTERN.finditer(line):
            text_segments.append(_normalize_text(line[cursor : match.start()]))
            token = match.group(0).replace("D", "E").replace("d", "e")
            try:
                number = float(token)
            except ValueError as exc:
                raise TextSemanticError(
                    f"{source}:{line_number}: invalid numeric token {token!r}"
                ) from exc
            if not math.isfinite(number):
                raise TextSemanticError(
                    f"{source}:{line_number}: non-finite numeric token {token!r}"
                )
            numbers.append(number)
            cursor = match.end()
        text_segments.append(_normalize_text(line[cursor:]))
        records.append(SemanticTextRecord(tuple(text_segments), tuple(numbers)))
    return tuple(records)


def load_text_deliverable(path: Path | str) -> tuple[SemanticTextRecord, ...]:
    source = Path(path)
    try:
        text = source.read_text(encoding="utf-8-sig")
    except UnicodeDecodeError:
        text = source.read_text(encoding="cp1252")
    except OSError as exc:
        raise TextSemanticError(f"cannot read {source}: {exc}") from exc
    return parse_text_deliverable(text, str(source))


def _record_document(record: SemanticTextRecord) -> dict[str, Any]:
    return {"text": list(record.text_segments), "numbers": list(record.numbers)}


def create_snapshot(
    output_format: str, source_path: Path | str, snapshot_path: Path | str
) -> None:
    """Create a deterministic compressed semantic text oracle."""

    if output_format not in SUPPORTED_FORMATS:
        raise TextSemanticError(f"unsupported text output format {output_format!r}")
    records = load_text_deliverable(source_path)
    document = {
        "schema": SCHEMA,
        "format": output_format,
        "record_count": len(records),
        "numeric_value_count": sum(len(record.numbers) for record in records),
        "records": [_record_document(record) for record in records],
    }
    target = Path(snapshot_path)
    target.parent.mkdir(parents=True, exist_ok=True)
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


def load_snapshot(
    snapshot_path: Path | str,
) -> tuple[str, tuple[SemanticTextRecord, ...]]:
    """Load and validate a compressed semantic text oracle."""

    path = Path(snapshot_path)
    try:
        with gzip.open(path, "rt", encoding="utf-8") as input_file:
            document = json.load(input_file)
    except (OSError, UnicodeError, json.JSONDecodeError) as exc:
        raise TextSemanticError(f"cannot read semantic snapshot {path}: {exc}") from exc
    if not isinstance(document, dict) or document.get("schema") != SCHEMA:
        raise TextSemanticError(f"{path}: unsupported semantic snapshot schema")
    output_format = document.get("format")
    if output_format not in SUPPORTED_FORMATS:
        raise TextSemanticError(f"{path}: invalid semantic output format")
    raw_records = document.get("records")
    if not isinstance(raw_records, list):
        raise TextSemanticError(f"{path}: semantic snapshot has no record list")
    records: list[SemanticTextRecord] = []
    try:
        for raw_record in raw_records:
            segments = tuple(str(value) for value in raw_record["text"])
            numbers = tuple(float(value) for value in raw_record["numbers"])
            if len(segments) != len(numbers) + 1:
                raise ValueError("text/number arity differs")
            if not all(math.isfinite(value) for value in numbers):
                raise ValueError("non-finite snapshot number")
            records.append(SemanticTextRecord(segments, numbers))
    except (KeyError, TypeError, ValueError) as exc:
        raise TextSemanticError(f"{path}: malformed semantic text record") from exc
    if document.get("record_count") != len(records):
        raise TextSemanticError(f"{path}: semantic record count is inconsistent")
    numeric_count = sum(len(record.numbers) for record in records)
    if document.get("numeric_value_count") != numeric_count:
        raise TextSemanticError(f"{path}: semantic numeric count is inconsistent")
    return str(output_format), tuple(records)


def _within_tolerance(expected: float, actual: float, abs_tol: float, rel_tol: float) -> bool:
    return abs(expected - actual) <= abs_tol + rel_tol * max(
        abs(expected), abs(actual)
    )


def _render_record(record: SemanticTextRecord) -> str:
    pieces: list[str] = []
    for index, segment in enumerate(record.text_segments):
        if segment:
            pieces.append(segment)
        if index < len(record.numbers):
            pieces.append(f"<{record.numbers[index]:.12g}>")
    return " ".join(pieces)


def compare_records(
    expected: Sequence[SemanticTextRecord],
    actual: Sequence[SemanticTextRecord],
    *,
    abs_tol: float = 1.0e-6,
    rel_tol: float = 0.0,
    max_differences: int = 20,
) -> TextComparisonResult:
    """Compare ordered semantic records with tolerant numeric values."""

    if abs_tol < 0.0 or rel_tol < 0.0:
        raise ValueError("tolerances must be non-negative")
    if max_differences < 1:
        raise ValueError("max_differences must be positive")
    matcher = difflib.SequenceMatcher(
        None,
        [record.structure for record in expected],
        [record.structure for record in actual],
        autojunk=False,
    )
    messages: list[str] = []
    for tag, expected_start, expected_end, actual_start, actual_end in matcher.get_opcodes():
        if tag == "equal":
            for offset in range(expected_end - expected_start):
                expected_record = expected[expected_start + offset]
                actual_record = actual[actual_start + offset]
                for number_index, (left, right) in enumerate(
                    zip(expected_record.numbers, actual_record.numbers), start=1
                ):
                    if not _within_tolerance(left, right, abs_tol, rel_tol):
                        allowed = abs_tol + rel_tol * max(abs(left), abs(right))
                        messages.append(
                            f"record {expected_start + offset + 1} number {number_index}: "
                            f"expected {left:.12g}, actual {right:.12g}, "
                            f"delta {abs(left - right):.6g} > {allowed:.6g}; "
                            f"{_render_record(expected_record)}"
                        )
            continue
        for index in range(expected_start, expected_end):
            messages.append(
                f"missing record {index + 1}: {_render_record(expected[index])}"
            )
        for index in range(actual_start, actual_end):
            messages.append(
                f"unexpected record {index + 1}: {_render_record(actual[index])}"
            )

    displayed = messages[:max_differences]
    if len(messages) > max_differences:
        displayed.append(f"... {len(messages) - max_differences} more differences")
    return TextComparisonResult(
        equivalent=not messages,
        expected_count=len(expected),
        actual_count=len(actual),
        differences=tuple(displayed),
    )


def compare_snapshot(
    snapshot_path: Path | str,
    actual_path: Path | str,
    *,
    abs_tol: float = 1.0e-6,
    rel_tol: float = 0.0,
    max_differences: int = 20,
) -> TextComparisonResult:
    _, expected = load_snapshot(snapshot_path)
    return compare_records(
        expected,
        load_text_deliverable(actual_path),
        abs_tol=abs_tol,
        rel_tol=rel_tol,
        max_differences=max_differences,
    )


def _argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Create or compare a semantic LEparagliding text oracle."
    )
    subparsers = parser.add_subparsers(dest="command", required=True)
    create = subparsers.add_parser("create", help="create a reviewed snapshot")
    create.add_argument("format", choices=sorted(SUPPORTED_FORMATS))
    create.add_argument("source", type=Path)
    create.add_argument("snapshot", type=Path)
    compare = subparsers.add_parser("compare", help="compare output with a snapshot")
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
            create_snapshot(args.format, args.source, args.snapshot)
            output_format, records = load_snapshot(args.snapshot)
            print(
                f"Wrote {output_format} semantic snapshot: {len(records)} records"
            )
            return 0
        result = compare_snapshot(
            args.snapshot,
            args.actual,
            abs_tol=args.abs_tol,
            rel_tol=args.rel_tol,
            max_differences=args.max_differences,
        )
    except (TextSemanticError, ValueError) as exc:
        print(f"Text semantic snapshot error: {exc}", file=sys.stderr)
        return 2
    if result.equivalent:
        print(
            f"Text semantic snapshot match: {result.actual_count} records "
            f"(abs_tol={args.abs_tol:g}, rel_tol={args.rel_tol:g})"
        )
        return 0
    print(
        f"Text semantic snapshot mismatch: expected {result.expected_count} "
        f"records, actual {result.actual_count}"
    )
    for difference in result.differences:
        print(f"  {difference}")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
