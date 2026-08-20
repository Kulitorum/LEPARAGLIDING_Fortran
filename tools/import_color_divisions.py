#!/usr/bin/env python3
"""Convert open DXF artwork divisions into LEP section 15/16 records.

The importer intentionally has no third-party dependencies.  It reads ASCII
DXF LINE and LWPOLYLINE entities, intersects the selected open division path
with the flattened wing's rib-reference lines, and reports the chord
percentage at every crossing.  It never rewrites the designer's DXF, so all
source layers, entity colors, hatches, and construction geometry are retained.
"""

from __future__ import annotations

import argparse
import math
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Iterator, Sequence, TextIO


Point = tuple[float, float]


@dataclass(frozen=True)
class DxfEntity:
    """One entity and its group-code/value pairs from the ENTITIES section."""

    kind: str
    pairs: tuple[tuple[int, str], ...]

    def values(self, code: int) -> list[str]:
        return [value for pair_code, value in self.pairs if pair_code == code]

    def first(self, code: int, default: str = "") -> str:
        values = self.values(code)
        return values[0] if values else default

    @property
    def layer(self) -> str:
        return self.first(8, "0")

    @property
    def color(self) -> int | None:
        value = self.first(62)
        return abs(int(value)) if value else None


@dataclass(frozen=True)
class DivisionPath:
    """An open artwork path plus the DXF metadata that identifies it."""

    points: tuple[Point, ...]
    layer: str
    color: int | None
    source_kind: str

    @property
    def span(self) -> float:
        xs = [point[0] for point in self.points]
        return max(xs) - min(xs)

    @property
    def length(self) -> float:
        return sum(distance(a, b) for a, b in zip(self.points, self.points[1:]))


@dataclass(frozen=True)
class RibCrossing:
    """Intersection of an artwork division and one chord-reference line."""

    point: Point
    chord_percent: float
    reference_length: float


def parse_args(argv: Sequence[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Sample an open division line in a flattened 2D wing DXF and "
            "write paste-ready LEparagliding section 15 or 16 records."
        )
    )
    parser.add_argument("dxf", type=Path, help="ASCII DXF containing the artwork")
    parser.add_argument(
        "--surface",
        choices=("extrados", "intrados"),
        required=True,
        help="target LEP surface (section 15 or 16)",
    )
    parser.add_argument("--division-layer", default="0")
    parser.add_argument("--division-color", type=int)
    parser.add_argument("--rib-layer", default="default")
    parser.add_argument("--rib-color", type=int, default=5)
    parser.add_argument(
        "--side",
        choices=("positive", "negative", "either"),
        default="positive",
        help="semi-wing to import; LEP section records describe one semi-wing",
    )
    parser.add_argument(
        "--chord-origin",
        choices=("top", "bottom", "first", "second"),
        default="top",
        help="reference-line endpoint corresponding to 100 percent in sections 15/16",
    )
    parser.add_argument("--boundary-id", type=int, default=1)
    parser.add_argument("--start-rib", type=int, default=1)
    parser.add_argument("--precision", type=int, default=4)
    parser.add_argument(
        "--snap-percent",
        type=float,
        default=0.01,
        help="snap derived percentages to this increment (0 disables snapping)",
    )
    parser.add_argument("--join-tolerance", type=float, default=1.0e-4)
    parser.add_argument("--min-rib-length", type=float, default=1.0)
    parser.add_argument(
        "--rib-axis-ratio",
        type=float,
        default=2.0,
        help="minimum abs(dY)/abs(dX) used to reject non-rib construction lines",
    )
    parser.add_argument("--output", type=Path, help="output file (stdout by default)")
    return parser.parse_args(argv)


def iter_group_pairs(stream: TextIO) -> Iterator[tuple[int, str]]:
    line_number = 0
    while True:
        code_line = stream.readline()
        if code_line == "":
            return
        line_number += 1
        value_line = stream.readline()
        if value_line == "":
            raise ValueError(f"truncated DXF after group code on line {line_number}")
        line_number += 1
        try:
            code = int(code_line.strip())
        except ValueError as error:
            raise ValueError(f"invalid DXF group code on line {line_number - 1}") from error
        yield code, value_line.strip()


def iter_entities(path: Path) -> Iterator[DxfEntity]:
    """Yield entities from an ASCII DXF without loading the whole file."""

    with path.open("r", encoding="cp1252", errors="replace", newline="") as stream:
        in_entities = False
        section_pending = False
        current_kind: str | None = None
        current_pairs: list[tuple[int, str]] = []

        for code, value in iter_group_pairs(stream):
            if not in_entities:
                if code == 0 and value == "SECTION":
                    section_pending = True
                elif section_pending and code == 2:
                    in_entities = value == "ENTITIES"
                    section_pending = False
                continue

            if code == 0:
                if current_kind is not None:
                    yield DxfEntity(current_kind, tuple(current_pairs))
                if value == "ENDSEC":
                    return
                current_kind = value
                current_pairs = []
            elif current_kind is not None:
                current_pairs.append((code, value))


def float_values(entity: DxfEntity, code: int) -> list[float]:
    try:
        return [float(value) for value in entity.values(code)]
    except ValueError as error:
        raise ValueError(f"invalid coordinate in {entity.kind} on layer {entity.layer!r}") from error


def entity_matches(entity: DxfEntity, layer: str, color: int | None) -> bool:
    return entity.layer.casefold() == layer.casefold() and (
        color is None or entity.color == abs(color)
    )


def line_points(entity: DxfEntity) -> tuple[Point, Point]:
    return (
        (float(entity.first(10)), float(entity.first(20))),
        (float(entity.first(11)), float(entity.first(21))),
    )


def lwpolyline_points(entity: DxfEntity) -> tuple[Point, ...]:
    xs = float_values(entity, 10)
    ys = float_values(entity, 20)
    if len(xs) != len(ys):
        raise ValueError(f"unpaired LWPOLYLINE coordinates on layer {entity.layer!r}")
    return tuple(zip(xs, ys))


def path_is_open(entity: DxfEntity) -> bool:
    flags = int(entity.first(70, "0"))
    return flags & 1 == 0


def distance(a: Point, b: Point) -> float:
    return math.hypot(b[0] - a[0], b[1] - a[1])


def points_close(a: Point, b: Point, tolerance: float) -> bool:
    return distance(a, b) <= tolerance


def join_line_segments(
    segments: list[tuple[Point, Point]], tolerance: float
) -> list[tuple[Point, ...]]:
    """Greedily join a small set of artwork LINE entities by shared endpoints."""

    remaining = list(segments)
    paths: list[tuple[Point, ...]] = []
    while remaining:
        first, second = remaining.pop()
        path = [first, second]
        changed = True
        while changed:
            changed = False
            for index, (start, end) in enumerate(remaining):
                if points_close(path[-1], start, tolerance):
                    path.append(end)
                elif points_close(path[-1], end, tolerance):
                    path.append(start)
                elif points_close(path[0], end, tolerance):
                    path.insert(0, start)
                elif points_close(path[0], start, tolerance):
                    path.insert(0, end)
                else:
                    continue
                remaining.pop(index)
                changed = True
                break
        paths.append(tuple(path))
    return paths


def side_matches(points: Sequence[Point], side: str, tolerance: float) -> bool:
    xs = [point[0] for point in points]
    if side == "positive":
        return min(xs) >= -tolerance and max(xs) > tolerance
    if side == "negative":
        return max(xs) <= tolerance and min(xs) < -tolerance
    return True


def collect_geometry(
    path: Path,
    division_layer: str,
    division_color: int | None,
    rib_layer: str,
    rib_color: int | None,
    side: str,
    join_tolerance: float,
) -> tuple[DivisionPath, list[tuple[Point, Point]]]:
    division_paths: list[DivisionPath] = []
    division_segments: list[tuple[Point, Point]] = []
    ribs: list[tuple[Point, Point]] = []

    for entity in iter_entities(path):
        if entity.kind == "LINE":
            endpoints = line_points(entity)
            if entity_matches(entity, division_layer, division_color):
                division_segments.append(endpoints)
            if entity_matches(entity, rib_layer, rib_color):
                ribs.append(endpoints)
        elif (
            entity.kind == "LWPOLYLINE"
            and entity_matches(entity, division_layer, division_color)
            and path_is_open(entity)
        ):
            points = lwpolyline_points(entity)
            if len(points) >= 2:
                division_paths.append(
                    DivisionPath(points, entity.layer, entity.color, entity.kind)
                )

    for points in join_line_segments(division_segments, join_tolerance):
        if len(points) >= 2:
            division_paths.append(
                DivisionPath(points, division_layer, division_color, "LINE chain")
            )

    candidates = [
        candidate
        for candidate in division_paths
        if side_matches(candidate.points, side, join_tolerance)
    ]
    if not candidates:
        raise ValueError(
            "no matching open LINE/LWPOLYLINE division path was found; "
            "check --division-layer, --division-color, and --side"
        )
    selected = max(candidates, key=lambda candidate: (candidate.span, candidate.length))
    return selected, ribs


def cross(a: Point, b: Point) -> float:
    return a[0] * b[1] - a[1] * b[0]


def subtract(a: Point, b: Point) -> Point:
    return a[0] - b[0], a[1] - b[1]


def segment_intersection(
    first_a: Point,
    first_b: Point,
    second_a: Point,
    second_b: Point,
    tolerance: float,
) -> tuple[Point, float] | None:
    """Return intersection and fraction along the second segment."""

    first_delta = subtract(first_b, first_a)
    second_delta = subtract(second_b, second_a)
    denominator = cross(first_delta, second_delta)
    if abs(denominator) <= tolerance:
        return None
    separation = subtract(second_a, first_a)
    first_fraction = cross(separation, second_delta) / denominator
    second_fraction = cross(separation, first_delta) / denominator
    if not (-tolerance <= first_fraction <= 1.0 + tolerance):
        return None
    if not (-tolerance <= second_fraction <= 1.0 + tolerance):
        return None
    point = (
        first_a[0] + first_fraction * first_delta[0],
        first_a[1] + first_fraction * first_delta[1],
    )
    return point, max(0.0, min(1.0, second_fraction))


def orient_rib(a: Point, b: Point, origin: str) -> tuple[Point, Point]:
    if origin == "first":
        return a, b
    if origin == "second":
        return b, a
    if origin == "top":
        return (a, b) if a[1] >= b[1] else (b, a)
    return (a, b) if a[1] <= b[1] else (b, a)


def collect_crossings(
    division: DivisionPath,
    ribs: Iterable[tuple[Point, Point]],
    side: str,
    chord_origin: str,
    tolerance: float,
    min_rib_length: float,
    rib_axis_ratio: float,
) -> list[RibCrossing]:
    crossings: list[RibCrossing] = []
    for rib_a, rib_b in ribs:
        delta_x = rib_b[0] - rib_a[0]
        delta_y = rib_b[1] - rib_a[1]
        rib_length = distance(rib_a, rib_b)
        if rib_length < min_rib_length:
            continue
        if abs(delta_y) < rib_axis_ratio * max(abs(delta_x), tolerance):
            continue

        origin, opposite = orient_rib(rib_a, rib_b, chord_origin)
        for division_a, division_b in zip(division.points, division.points[1:]):
            intersection = segment_intersection(
                division_a, division_b, origin, opposite, tolerance
            )
            if intersection is None:
                continue
            point, fraction_from_origin = intersection
            if side == "positive" and point[0] < -tolerance:
                continue
            if side == "negative" and point[0] > tolerance:
                continue
            crossings.append(
                RibCrossing(
                    point,
                    100.0 * (1.0 - fraction_from_origin),
                    rib_length,
                )
            )
            break

    # A vertex shared by two division segments and duplicate central-rib lines
    # can report the same crossing more than once.  Retain the longest reference
    # line at each position because it is the complete chord construction line.
    crossings.sort(key=lambda crossing: (abs(crossing.point[0]), crossing.point[0]))
    unique: list[RibCrossing] = []
    for crossing in crossings:
        duplicate_index = next(
            (
                index
                for index, existing in enumerate(unique)
                if points_close(existing.point, crossing.point, 10.0 * tolerance)
            ),
            None,
        )
        if duplicate_index is None:
            unique.append(crossing)
        elif crossing.reference_length > unique[duplicate_index].reference_length:
            unique[duplicate_index] = crossing

    unique.sort(key=lambda crossing: abs(crossing.point[0]))
    return unique


def format_section(
    args: argparse.Namespace,
    division: DivisionPath,
    crossings: Sequence[RibCrossing],
) -> str:
    section_number = 15 if args.surface == "extrados" else 16
    surface_title = "Extrados" if args.surface == "extrados" else "Intrados"
    color_text = "BYLAYER" if division.color is None else str(division.color)
    lines = [
        "*************************************************************",
        f"*\t{section_number}. {surface_title} colors | {args.dxf.name}",
        f"* DXF division: layer={division.layer!r} ACI={color_text} "
        f"{division.source_kind}; ribs: layer={args.rib_layer!r} "
        f"ACI={args.rib_color}; source unchanged",
        str(len(crossings)),
    ]
    value_format = f"{{:.{args.precision}f}}"
    for offset, crossing in enumerate(crossings):
        rib_number = args.start_rib + offset
        chord_percent = crossing.chord_percent
        if args.snap_percent > 0.0:
            chord_percent = (
                round(chord_percent / args.snap_percent) * args.snap_percent
            )
        lines.append(f"{rib_number}\t1")
        lines.append(
            f"{args.boundary_id}\t{value_format.format(chord_percent)}\t0.0"
        )
    return "\n".join(lines) + "\n"


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_args(sys.argv[1:] if argv is None else argv)
    if args.start_rib < 0:
        raise ValueError("--start-rib must be nonnegative")
    if args.join_tolerance <= 0.0:
        raise ValueError("--join-tolerance must be positive")

    division, ribs = collect_geometry(
        args.dxf,
        args.division_layer,
        args.division_color,
        args.rib_layer,
        args.rib_color,
        args.side,
        args.join_tolerance,
    )
    crossings = collect_crossings(
        division,
        ribs,
        args.side,
        args.chord_origin,
        args.join_tolerance,
        args.min_rib_length,
        args.rib_axis_ratio,
    )
    if len(crossings) < 2:
        raise ValueError(
            f"only {len(crossings)} rib crossing(s) found; verify the rib layer/color "
            "and that the division line crosses the flattened chord references"
        )

    output_text = format_section(args, division, crossings)
    if args.output:
        args.output.write_text(output_text, encoding="utf-8", newline="\n")
    else:
        sys.stdout.write(output_text)
    print(
        f"Imported {len(crossings)} rib crossings from {division.source_kind} "
        f"on layer {division.layer!r} (ACI {division.color}).",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(2)
