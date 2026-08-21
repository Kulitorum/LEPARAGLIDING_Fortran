#!/usr/bin/env python3
"""Check reviewed LEparagliding 3.28-to-3.29 output compatibility.

The historical 3.28 source is compiled and run separately by the CMake
regression harness.  This tool compares that run with the maintained program
without requiring byte-identical DXF serialization.  The complete 3D drawing
and line list remain compatible.  The report comparison excludes only the
reviewed 3.29 additions and corrections documented by ``project_report``.

The main 2D manufacturing drawing is deliberately outside this cross-version
comparison: safety repairs and corrected panel geometry changed it materially.
Its complete current semantics are protected by the reviewed 3.29 DXF oracle.
"""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence

from dxf_semantic_diff import DxfParseError, compare_entities, load_ascii_dxf
from text_semantic_snapshot import (
    SemanticTextRecord,
    TextComparisonResult,
    TextSemanticError,
    compare_records,
    load_text_deliverable,
    parse_text_deliverable,
)


VERSION_HEADER = re.compile(
    r"(LABORATORI D'ENVOL PARAGLIDING)\s+(3\.(?:28|29))"
)
JONC_TYPE = re.compile(
    r"^(\s*Jonc\s+\d+)\s+type\s+\d+\s+(sup\s+.*)$"
)
ZONE_CORRECTION = re.compile(r"^(?:z3|f13|z4)\s+\d+")
CHI_HEADER = re.compile(r"^Rib\s+phi\s+chi$")
CHI_ROW = re.compile(r"^\d+\s+[-+]?\d")
SPECIAL_CODE_ROW = re.compile(
    r"^\d+\s+(1291|1341|1146|1351|1352|1353|2000|3001)$"
)
PLAN_B_SPECIAL_CODES = (1291, 1341, 1146, 1351, 1352, 1353, 2000, 3001)


class CompatibilityError(ValueError):
    """Raised when an output cannot be used by the reviewed compatibility gate."""


@dataclass(frozen=True)
class ReportProjection:
    """Stable report records plus counts for each explicitly approved delta."""

    records: tuple[SemanticTextRecord, ...]
    edition_dates: int
    total_height_rows: int
    jonc_rows: int
    jonc_type_labels: int
    corrected_zone_rows: int
    chi_rows: int
    special_codes: tuple[int, ...]


def _read_text(path: Path | str) -> str:
    source = Path(path)
    try:
        return source.read_text(encoding="utf-8-sig")
    except UnicodeDecodeError:
        return source.read_text(encoding="cp1252")
    except OSError as exc:
        raise CompatibilityError(f"cannot read {source}: {exc}") from exc


def project_report(text: str, *, expected_version: str) -> ReportProjection:
    """Return the report semantics that must remain compatible.

    The projection permits precisely these reviewed changes:

    * the 3.28-to-3.29 version label and new edition-date record;
    * the corrected total-height report value;
    * explicit ``type`` labels on Jonc records;
    * corrected z3/f13/z4 panel-length measurements;
    * corrected ``chi`` values; and
    * the expanded list of the eight already-counted special codes.

    All other ordered report text and numeric values remain in the comparison.
    """

    projected_lines: list[str] = []
    version_headers = 0
    edition_dates = 0
    total_height_rows = 0
    jonc_rows = 0
    jonc_type_labels = 0
    corrected_zone_rows = 0
    chi_rows = 0
    special_codes: list[int] = []
    in_chi_table = False

    for line in text.splitlines():
        stripped = line.strip()

        version_match = VERSION_HEADER.search(line)
        if version_match:
            version_headers += 1
            actual_version = version_match.group(2)
            if actual_version != expected_version:
                raise CompatibilityError(
                    f"expected report version {expected_version}, found {actual_version}"
                )
            line = VERSION_HEADER.sub(r"\1 VERSION", line, count=1)
            stripped = line.strip()

        if stripped.startswith("Edition date:"):
            edition_dates += 1
            continue
        if stripped.startswith("Total height hcp"):
            total_height_rows += 1
            continue

        if stripped.startswith("Jonc "):
            jonc_rows += 1
            jonc_match = JONC_TYPE.match(line)
            if jonc_match:
                jonc_type_labels += 1
                line = f"{jonc_match.group(1)} {jonc_match.group(2)}"
                stripped = line.strip()

        if ZONE_CORRECTION.match(stripped):
            corrected_zone_rows += 1
            continue

        if CHI_HEADER.match(stripped):
            in_chi_table = True
            projected_lines.append(line)
            continue
        if in_chi_table:
            if not stripped:
                # Blank records are non-semantic; stay in the table until the
                # next nonblank row identifies its end.
                continue
            if CHI_ROW.match(stripped):
                chi_rows += 1
                continue
            in_chi_table = False

        special_match = SPECIAL_CODE_ROW.match(stripped)
        if special_match:
            special_codes.append(int(special_match.group(1)))
            continue

        projected_lines.append(line)

    if version_headers != 1:
        raise CompatibilityError(
            f"expected one {expected_version} report header, found {version_headers}"
        )

    return ReportProjection(
        records=parse_text_deliverable("\n".join(projected_lines)),
        edition_dates=edition_dates,
        total_height_rows=total_height_rows,
        jonc_rows=jonc_rows,
        jonc_type_labels=jonc_type_labels,
        corrected_zone_rows=corrected_zone_rows,
        chi_rows=chi_rows,
        special_codes=tuple(special_codes),
    )


def _validate_plan_b_projections(
    legacy: ReportProjection, current: ReportProjection
) -> None:
    """Ensure the allowlist matched the reviewed Plan B report structures."""

    expected_common = {
        "total-height rows": (legacy.total_height_rows, current.total_height_rows, 1),
        "Jonc rows": (legacy.jonc_rows, current.jonc_rows, 8),
        "corrected zone rows": (
            legacy.corrected_zone_rows,
            current.corrected_zone_rows,
            36,
        ),
        "chi rows": (legacy.chi_rows, current.chi_rows, 12),
    }
    for label, (legacy_count, current_count, expected_count) in expected_common.items():
        if legacy_count != expected_count or current_count != expected_count:
            raise CompatibilityError(
                f"reviewed {label} changed: expected {expected_count}, "
                f"found legacy={legacy_count}, current={current_count}"
            )

    if legacy.edition_dates != 0 or current.edition_dates != 1:
        raise CompatibilityError(
            "reviewed edition-date delta changed: expected legacy=0, current=1"
        )
    if legacy.jonc_type_labels != 0 or current.jonc_type_labels != 8:
        raise CompatibilityError(
            "reviewed Jonc type-label delta changed: expected legacy=0, current=8"
        )
    if legacy.special_codes or current.special_codes != PLAN_B_SPECIAL_CODES:
        raise CompatibilityError(
            "reviewed special-code expansion changed: expected no legacy rows and "
            f"current={PLAN_B_SPECIAL_CODES}, found legacy={legacy.special_codes}, "
            f"current={current.special_codes}"
        )


def compare_reports(
    legacy_path: Path | str,
    current_path: Path | str,
    *,
    abs_tol: float = 1.0e-9,
    max_differences: int = 20,
) -> TextComparisonResult:
    """Compare reports after validating the reviewed Plan B delta allowlist."""

    legacy = project_report(_read_text(legacy_path), expected_version="3.28")
    current = project_report(_read_text(current_path), expected_version="3.29")
    _validate_plan_b_projections(legacy, current)
    return compare_records(
        legacy.records,
        current.records,
        abs_tol=abs_tol,
        rel_tol=0.0,
        max_differences=max_differences,
    )


def _format_failure(label: str, differences: Sequence[str]) -> str:
    body = "\n".join(f"  {difference}" for difference in differences)
    return f"{label} compatibility mismatch:\n{body}"


def check_output_directories(
    legacy_directory: Path | str,
    current_directory: Path | str,
    *,
    abs_tol: float = 1.0e-9,
    max_differences: int = 20,
) -> tuple[str, ...]:
    """Run all approved 3.28 compatibility checks and return summaries."""

    legacy = Path(legacy_directory)
    current = Path(current_directory)
    required = ("lep-3d.dxf", "lines.txt", "lep-out.txt")
    for directory, label in ((legacy, "legacy"), (current, "current")):
        for name in required:
            path = directory / name
            if not path.is_file():
                raise CompatibilityError(f"missing {label} compatibility output: {path}")

    dxf_result = compare_entities(
        load_ascii_dxf(legacy / "lep-3d.dxf"),
        load_ascii_dxf(current / "lep-3d.dxf"),
        abs_tol=abs_tol,
        rel_tol=0.0,
        max_differences=max_differences,
    )
    if not dxf_result.equivalent:
        raise CompatibilityError(
            _format_failure("3D DXF", dxf_result.differences)
        )

    lines_result = compare_records(
        load_text_deliverable(legacy / "lines.txt"),
        load_text_deliverable(current / "lines.txt"),
        abs_tol=abs_tol,
        rel_tol=0.0,
        max_differences=max_differences,
    )
    if not lines_result.equivalent:
        raise CompatibilityError(
            _format_failure("line list", lines_result.differences)
        )

    report_result = compare_reports(
        legacy / "lep-out.txt",
        current / "lep-out.txt",
        abs_tol=abs_tol,
        max_differences=max_differences,
    )
    if not report_result.equivalent:
        raise CompatibilityError(
            _format_failure("projected report", report_result.differences)
        )

    return (
        f"3D DXF: {dxf_result.actual_count} entities compatible",
        f"line list: {lines_result.actual_count} semantic records compatible",
        f"projected report: {report_result.actual_count} semantic records compatible",
    )


def _argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Compare isolated 3.28 Plan B outputs with maintained 3.29"
    )
    parser.add_argument("legacy_directory", type=Path)
    parser.add_argument("current_directory", type=Path)
    parser.add_argument("--abs-tol", type=float, default=1.0e-9)
    parser.add_argument("--max-differences", type=int, default=20)
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    args = _argument_parser().parse_args(argv)
    if args.abs_tol < 0.0:
        print("3.28 compatibility error: tolerances must be non-negative", file=sys.stderr)
        return 2
    if args.max_differences < 1:
        print("3.28 compatibility error: max-differences must be positive", file=sys.stderr)
        return 2
    try:
        summaries = check_output_directories(
            args.legacy_directory,
            args.current_directory,
            abs_tol=args.abs_tol,
            max_differences=args.max_differences,
        )
    except (CompatibilityError, DxfParseError, TextSemanticError, ValueError) as exc:
        print(f"3.28 compatibility error: {exc}", file=sys.stderr)
        return 1

    print("3.28 Plan B compatibility confirmed")
    for summary in summaries:
        print(f"  {summary}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
