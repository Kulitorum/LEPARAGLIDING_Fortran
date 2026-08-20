#!/usr/bin/env python3
"""Compare the model-space entities in two ASCII DXF files semantically.

The comparator is intended for geometry regression tests.  It ignores numeric
text formatting, entity order, comments, and volatile entity/owner handles.
Entity type, layer, colour, group-code structure, integer/string attributes,
and floating-point values are compared.  Floating-point values use a caller
supplied absolute and relative tolerance.

Only the ENTITIES section is compared.  A classic POLYLINE and its VERTEX
records are treated as one ordered compound entity; the order of vertices
inside a polyline remains significant.

Exit status is 0 for a semantic match, 1 for a semantic difference, and 2 for
invalid input or command-line usage.
"""

from __future__ import annotations

import argparse
import bisect
import math
import sys
from collections import Counter, defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Mapping, Sequence


# Handles are allocation details rather than drawing semantics.  Group 330 is
# the entity's owner handle; entity handle codes 5 and 105 are also volatile.
IGNORED_CODES = frozenset({5, 105, 330, 999})


class DxfParseError(ValueError):
    """Raised when an input is not a usable ASCII DXF document."""


@dataclass(frozen=True)
class SemanticEntity:
    """A normalized entity represented by group-code occurrence sequences."""

    kind: str
    fields: tuple[tuple[str, tuple[object, ...]], ...]

    def field_map(self) -> dict[str, tuple[object, ...]]:
        return dict(self.fields)


@dataclass(frozen=True)
class ComparisonResult:
    equivalent: bool
    expected_count: int
    actual_count: int
    differences: tuple[str, ...]


def _is_float_code(code: int) -> bool:
    return (
        10 <= code <= 59
        or 110 <= code <= 149
        or 210 <= code <= 239
        or 460 <= code <= 469
        or 1010 <= code <= 1059
    )


def _is_integer_code(code: int) -> bool:
    return (
        60 <= code <= 99
        or 160 <= code <= 179
        or 270 <= code <= 299
        or 370 <= code <= 459
        or 1060 <= code <= 1071
    )


def _parse_value(code: int, text: str, source: str, line_number: int) -> object:
    value = text.strip()
    try:
        if _is_float_code(code):
            number = float(value.replace("D", "E").replace("d", "e"))
            if not math.isfinite(number):
                raise DxfParseError(
                    f"{source}:{line_number}: non-finite numeric value {value!r}"
                )
            return number
        if _is_integer_code(code):
            return int(value)
    except ValueError as exc:
        raise DxfParseError(
            f"{source}:{line_number}: invalid value {value!r} for group {code}"
        ) from exc
    return value


def _read_pairs(text: str, source: str) -> list[tuple[int, object]]:
    lines = text.splitlines()
    if lines and lines[0].startswith("\ufeff"):
        lines[0] = lines[0].lstrip("\ufeff")
    if len(lines) % 2:
        raise DxfParseError(f"{source}: odd number of lines in ASCII DXF")

    pairs: list[tuple[int, object]] = []
    for index in range(0, len(lines), 2):
        try:
            code = int(lines[index].strip())
        except ValueError as exc:
            raise DxfParseError(
                f"{source}:{index + 1}: invalid group code {lines[index]!r}"
            ) from exc
        pairs.append(
            (code, _parse_value(code, lines[index + 1], source, index + 2))
        )
    return pairs


def _entities_section(
    pairs: Sequence[tuple[int, object]], source: str
) -> list[tuple[str, list[tuple[int, object]]]]:
    raw_entities: list[tuple[str, list[tuple[int, object]]]] = []
    in_entities = False
    found_entities = False
    index = 0

    while index < len(pairs):
        code, value = pairs[index]
        if code == 0 and value == "SECTION":
            if index + 1 >= len(pairs) or pairs[index + 1][0] != 2:
                raise DxfParseError(f"{source}: SECTION has no group-2 name")
            in_entities = pairs[index + 1][1] == "ENTITIES"
            found_entities = found_entities or in_entities
            index += 2
            continue
        if in_entities and code == 0 and value == "ENDSEC":
            in_entities = False
            index += 1
            continue
        if in_entities and code == 0:
            kind = str(value).upper()
            body: list[tuple[int, object]] = []
            index += 1
            while index < len(pairs) and pairs[index][0] != 0:
                body.append(pairs[index])
                index += 1
            raw_entities.append((kind, body))
            continue
        index += 1

    if not found_entities:
        raise DxfParseError(f"{source}: no ENTITIES section found")
    return raw_entities


def _normalized_fields(
    pairs: Iterable[tuple[int, object]],
    *,
    prefix: str = "",
    entity_defaults: bool = False,
) -> dict[str, tuple[object, ...]]:
    values: dict[str, list[object]] = defaultdict(list)
    for code, value in pairs:
        if code not in IGNORED_CODES:
            values[f"{prefix}{code}"].append(value)

    if entity_defaults:
        # Omitted layer and colour have these meanings in DXF.  Normalizing the
        # explicit forms avoids false regressions from writer implementation.
        values.setdefault(f"{prefix}8", ["0"])
        values.setdefault(f"{prefix}62", [256])  # BYLAYER
    return {key: tuple(items) for key, items in values.items()}


def parse_ascii_dxf(text: str, source: str = "<memory>") -> tuple[SemanticEntity, ...]:
    """Parse and normalize the ENTITIES section of an ASCII DXF document."""

    raw = _entities_section(_read_pairs(text, source), source)
    entities: list[SemanticEntity] = []
    index = 0
    while index < len(raw):
        kind, body = raw[index]
        fields = _normalized_fields(body, entity_defaults=True)

        if kind == "POLYLINE":
            vertex_index = 0
            index += 1
            while index < len(raw) and raw[index][0] == "VERTEX":
                vertex_kind, vertex_body = raw[index]
                assert vertex_kind == "VERTEX"
                fields.update(
                    _normalized_fields(vertex_body, prefix=f"v{vertex_index}:")
                )
                vertex_index += 1
                index += 1
            fields["vertex-count"] = (vertex_index,)
            if index < len(raw) and raw[index][0] == "SEQEND":
                index += 1
            entities.append(SemanticEntity(kind, tuple(sorted(fields.items()))))
            continue

        entities.append(SemanticEntity(kind, tuple(sorted(fields.items()))))
        index += 1

    return tuple(entities)


def load_ascii_dxf(path: Path | str) -> tuple[SemanticEntity, ...]:
    path = Path(path)
    try:
        text = path.read_text(encoding="utf-8-sig")
    except UnicodeDecodeError:
        # Legacy DXFs commonly use the codepage declared in their header.
        text = path.read_text(encoding="cp1252")
    except OSError as exc:
        raise DxfParseError(f"cannot read {path}: {exc}") from exc
    return parse_ascii_dxf(text, str(path))


def _structure_key(entity: SemanticEntity) -> tuple[object, ...]:
    structure: list[object] = [entity.kind]
    for code, values in entity.fields:
        structure.append(code)
        structure.append(
            tuple("<number>" if isinstance(value, float) else value for value in values)
        )
    return tuple(structure)


def _numeric_vector(entity: SemanticEntity) -> tuple[float, ...]:
    return tuple(
        value
        for _, values in entity.fields
        for value in values
        if isinstance(value, float)
    )


def _within_tolerance(expected: float, actual: float, abs_tol: float, rel_tol: float) -> bool:
    return abs(expected - actual) <= abs_tol + rel_tol * max(
        abs(expected), abs(actual)
    )


def _vectors_match(
    expected: Sequence[float],
    actual: Sequence[float],
    abs_tol: float,
    rel_tol: float,
) -> bool:
    return len(expected) == len(actual) and all(
        _within_tolerance(left, right, abs_tol, rel_tol)
        for left, right in zip(expected, actual)
    )


def _candidate_radius(value: float, abs_tol: float, rel_tol: float) -> float:
    if rel_tol >= 1.0:
        return math.inf
    # Conservative solution for a tolerance scaled by max(abs(a), abs(b)).
    return (abs_tol + rel_tol * abs(value)) / (1.0 - rel_tol)


def _match_numeric_entities(
    expected: Sequence[SemanticEntity],
    actual: Sequence[SemanticEntity],
    abs_tol: float,
    rel_tol: float,
) -> tuple[list[int], list[int]]:
    """Return unmatched indices using maximum bipartite tolerance matching."""

    expected_vectors = [_numeric_vector(entity) for entity in expected]
    actual_vectors = [_numeric_vector(entity) for entity in actual]

    # Remove exact duplicates first.  This makes the common large-DXF case fast
    # and prevents a dense graph for repeated identical entities.
    actual_by_vector: dict[tuple[float, ...], list[int]] = defaultdict(list)
    for actual_index, vector in enumerate(actual_vectors):
        actual_by_vector[vector].append(actual_index)
    pre_matched_expected: set[int] = set()
    pre_matched_actual: set[int] = set()
    for expected_index, vector in enumerate(expected_vectors):
        candidates = actual_by_vector.get(vector)
        if candidates:
            pre_matched_expected.add(expected_index)
            pre_matched_actual.add(candidates.pop())

    remaining_expected = [
        index for index in range(len(expected)) if index not in pre_matched_expected
    ]
    remaining_actual = [
        index for index in range(len(actual)) if index not in pre_matched_actual
    ]

    if not expected_vectors or (expected_vectors and not expected_vectors[0]):
        count = min(len(remaining_expected), len(remaining_actual))
        return remaining_expected[count:], remaining_actual[count:]

    sorted_actual = sorted(
        ((actual_vectors[index][0], index) for index in remaining_actual),
        key=lambda item: item[0],
    )
    first_values = [item[0] for item in sorted_actual]
    adjacency: dict[int, list[int]] = {}
    for expected_index in remaining_expected:
        vector = expected_vectors[expected_index]
        radius = _candidate_radius(vector[0], abs_tol, rel_tol)
        start = bisect.bisect_left(first_values, vector[0] - radius)
        end = bisect.bisect_right(first_values, vector[0] + radius)
        adjacency[expected_index] = [
            actual_index
            for _, actual_index in sorted_actual[start:end]
            if _vectors_match(vector, actual_vectors[actual_index], abs_tol, rel_tol)
        ]

    # Augmenting paths provide a maximum matching, avoiding false failures when
    # several close entities have overlapping tolerance windows.
    owner: dict[int, int] = {}

    def augment(expected_index: int, visited: set[int]) -> bool:
        for actual_index in adjacency[expected_index]:
            if actual_index in visited:
                continue
            visited.add(actual_index)
            if actual_index not in owner or augment(owner[actual_index], visited):
                owner[actual_index] = expected_index
                return True
        return False

    matched_expected = set(pre_matched_expected)
    for expected_index in remaining_expected:
        if augment(expected_index, set()):
            matched_expected.add(expected_index)
    matched_actual = set(pre_matched_actual) | set(owner)
    return (
        [index for index in range(len(expected)) if index not in matched_expected],
        [index for index in range(len(actual)) if index not in matched_actual],
    )


def _field_value(entity: SemanticEntity, code: str) -> object | None:
    values = entity.field_map().get(code, ())
    return values[0] if values else None


def _describe(entity: SemanticEntity) -> str:
    fields = entity.field_map()
    parts = [
        entity.kind,
        f"layer={_field_value(entity, '8')!r}",
        f"color={_field_value(entity, '62')!r}",
    ]
    coordinates: list[str] = []
    for code, values in entity.fields:
        base_code = code.split(":")[-1]
        try:
            group_code = int(base_code)
        except ValueError:
            continue
        if 10 <= group_code <= 59:
            coordinates.extend(f"{code}={value:.9g}" for value in values if isinstance(value, float))
        if len(coordinates) >= 8:
            break
    if coordinates:
        parts.append("coords(" + ", ".join(coordinates[:8]) + ")")
    if "vertex-count" in fields:
        parts.append(f"vertices={fields['vertex-count'][0]}")
    return " ".join(parts)


def _numeric_difference(
    expected: SemanticEntity,
    actual: SemanticEntity,
    abs_tol: float,
    rel_tol: float,
) -> str:
    expected_vector = _numeric_vector(expected)
    actual_vector = _numeric_vector(actual)
    field_names = [
        f"{code}[{occurrence}]"
        for code, values in expected.fields
        for occurrence, value in enumerate(values)
        if isinstance(value, float)
    ]
    violations: list[str] = []
    for name, left, right in zip(field_names, expected_vector, actual_vector):
        if not _within_tolerance(left, right, abs_tol, rel_tol):
            allowed = abs_tol + rel_tol * max(abs(left), abs(right))
            violations.append(
                f"{name}: expected {left:.12g}, actual {right:.12g}, "
                f"delta {abs(left - right):.6g} > {allowed:.6g}"
            )
    return "; ".join(violations[:4])


def compare_entities(
    expected: Sequence[SemanticEntity],
    actual: Sequence[SemanticEntity],
    *,
    abs_tol: float = 1.0e-6,
    rel_tol: float = 0.0,
    max_differences: int = 20,
) -> ComparisonResult:
    """Compare two normalized entity sequences as tolerance-aware multisets."""

    if abs_tol < 0.0 or rel_tol < 0.0:
        raise ValueError("tolerances must be non-negative")
    if max_differences < 1:
        raise ValueError("max_differences must be positive")

    expected_groups: dict[tuple[object, ...], list[SemanticEntity]] = defaultdict(list)
    actual_groups: dict[tuple[object, ...], list[SemanticEntity]] = defaultdict(list)
    for entity in expected:
        expected_groups[_structure_key(entity)].append(entity)
    for entity in actual:
        actual_groups[_structure_key(entity)].append(entity)

    messages: list[str] = []
    for key in sorted(set(expected_groups) | set(actual_groups), key=repr):
        expected_group = expected_groups.get(key, [])
        actual_group = actual_groups.get(key, [])
        unmatched_expected, unmatched_actual = _match_numeric_entities(
            expected_group, actual_group, abs_tol, rel_tol
        )

        paired = min(len(unmatched_expected), len(unmatched_actual))
        for pair_index in range(paired):
            left = expected_group[unmatched_expected[pair_index]]
            right = actual_group[unmatched_actual[pair_index]]
            detail = _numeric_difference(left, right, abs_tol, rel_tol)
            messages.append(
                f"changed: {_describe(left)} -> {_describe(right)}"
                + (f"; {detail}" if detail else "")
            )
        for group_index in unmatched_expected[paired:]:
            messages.append(f"missing: {_describe(expected_group[group_index])}")
        for group_index in unmatched_actual[paired:]:
            messages.append(f"unexpected: {_describe(actual_group[group_index])}")

    # A metadata change produces different structure keys, hence separate
    # missing/unexpected messages that expose layer and colour in the summary.
    truncated = len(messages) > max_differences
    displayed = messages[:max_differences]
    if truncated:
        displayed.append(f"... {len(messages) - max_differences} more differences")
    return ComparisonResult(
        equivalent=not messages,
        expected_count=len(expected),
        actual_count=len(actual),
        differences=tuple(displayed),
    )


def compare_files(
    expected_path: Path | str,
    actual_path: Path | str,
    *,
    abs_tol: float = 1.0e-6,
    rel_tol: float = 0.0,
    max_differences: int = 20,
) -> ComparisonResult:
    return compare_entities(
        load_ascii_dxf(expected_path),
        load_ascii_dxf(actual_path),
        abs_tol=abs_tol,
        rel_tol=rel_tol,
        max_differences=max_differences,
    )


def _argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Compare ASCII DXF ENTITIES semantically, ignoring entity order, "
            "numeric formatting, comments, and volatile handles."
        )
    )
    parser.add_argument("expected", type=Path, help="reference DXF")
    parser.add_argument("actual", type=Path, help="generated DXF")
    parser.add_argument(
        "--abs-tol",
        type=float,
        default=1.0e-6,
        help="absolute floating-point tolerance (default: 1e-6)",
    )
    parser.add_argument(
        "--rel-tol",
        type=float,
        default=0.0,
        help="relative floating-point tolerance (default: 0)",
    )
    parser.add_argument(
        "--max-differences",
        type=int,
        default=20,
        help="maximum difference lines to print (default: 20)",
    )
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    args = _argument_parser().parse_args(argv)
    try:
        result = compare_files(
            args.expected,
            args.actual,
            abs_tol=args.abs_tol,
            rel_tol=args.rel_tol,
            max_differences=args.max_differences,
        )
    except (DxfParseError, ValueError) as exc:
        print(f"DXF comparison error: {exc}", file=sys.stderr)
        return 2

    if result.equivalent:
        print(
            f"DXF semantic match: {result.expected_count} entities "
            f"(abs_tol={args.abs_tol:g}, rel_tol={args.rel_tol:g})"
        )
        return 0

    print(
        f"DXF semantic mismatch: expected {result.expected_count} entities, "
        f"actual {result.actual_count}"
    )
    for difference in result.differences:
        print(f"  {difference}")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
