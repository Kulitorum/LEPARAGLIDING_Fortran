"""Dependency-free tests for tools/dxf_semantic_diff.py."""

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


REPOSITORY_ROOT = Path(__file__).resolve().parents[1]
TOOLS_DIRECTORY = REPOSITORY_ROOT / "tools"
sys.path.insert(0, str(TOOLS_DIRECTORY))

from dxf_semantic_diff import compare_entities, parse_ascii_dxf  # noqa: E402
from dxf_semantic_snapshot import (  # noqa: E402
    compare_snapshot,
    create_snapshot,
    load_snapshot,
)
from text_semantic_snapshot import (  # noqa: E402
    compare_records as compare_text_records,
    compare_snapshot as compare_text_snapshot,
    create_snapshot as create_text_snapshot,
    parse_text_deliverable,
)


def dxf(*entities: str) -> str:
    return "\n".join(("0", "SECTION", "2", "ENTITIES", *entities, "0", "ENDSEC", "0", "EOF", ""))


REFERENCE_LINE = "\n".join(
    (
        "0", "LINE", "5", "AB", "8", "color_seams", "62", "3",
        "10", "0.0", "20", "1.0", "30", "0.0",
        "11", "10.0", "21", "2.0", "31", "0.0",
    )
)

REFERENCE_CIRCLE = "\n".join(
    (
        "0", "CIRCLE", "8", "marks", "62", "1",
        "10", "5.0", "20", "6.0", "30", "0.0", "40", "0.5",
    )
)

REFERENCE_POLYLINE = "\n".join(
    (
        "0", "POLYLINE", "8", "cut", "62", "4", "66", "1", "70", "0",
        "0", "VERTEX", "8", "cut", "10", "0.0", "20", "0.0", "30", "0.0",
        "0", "VERTEX", "8", "cut", "10", "1.0", "20", "2.0", "30", "0.0",
        "0", "SEQEND",
    )
)


class SemanticDxfComparisonTests(unittest.TestCase):
    def test_order_format_handles_and_small_numeric_drift_are_ignored(self) -> None:
        expected = parse_ascii_dxf(dxf(REFERENCE_LINE, REFERENCE_CIRCLE))
        actual_circle = REFERENCE_CIRCLE.replace("5.0", "5.0004", 1)
        actual_line = (
            REFERENCE_LINE.replace("AB", "FFFF")
            .replace("10.0", "10.0004")
            .replace("1.0", "1.0003")
        )
        actual = parse_ascii_dxf(dxf(actual_circle, actual_line))

        result = compare_entities(expected, actual, abs_tol=0.001)

        self.assertTrue(result.equivalent, result.differences)
        self.assertEqual(2, result.expected_count)

    def test_coordinate_regression_has_actionable_failure(self) -> None:
        expected = parse_ascii_dxf(dxf(REFERENCE_LINE))
        actual = parse_ascii_dxf(dxf(REFERENCE_LINE.replace("10.0", "10.25")))

        result = compare_entities(expected, actual, abs_tol=0.01)

        self.assertFalse(result.equivalent)
        report = "\n".join(result.differences)
        self.assertIn("changed: LINE", report)
        self.assertIn("11[0]", report)
        self.assertIn("delta 0.25", report)

    def test_layer_and_colour_changes_are_detected(self) -> None:
        expected = parse_ascii_dxf(dxf(REFERENCE_LINE))
        modified = REFERENCE_LINE.replace("color_seams", "wrong_layer").replace(
            "\n3\n10\n", "\n5\n10\n"
        )
        actual = parse_ascii_dxf(dxf(modified))

        result = compare_entities(expected, actual)

        self.assertFalse(result.equivalent)
        report = "\n".join(result.differences)
        self.assertIn("layer='color_seams'", report)
        self.assertIn("color=3", report)
        self.assertIn("layer='wrong_layer'", report)
        self.assertIn("color=5", report)

    def test_cli_exit_statuses_support_regression_jobs(self) -> None:
        tool = TOOLS_DIRECTORY / "dxf_semantic_diff.py"
        with tempfile.TemporaryDirectory() as directory:
            expected_path = Path(directory) / "expected.dxf"
            actual_path = Path(directory) / "actual.dxf"
            expected_path.write_text(dxf(REFERENCE_LINE), encoding="utf-8")
            actual_path.write_text(
                dxf(REFERENCE_LINE.replace("10.0", "10.0004")), encoding="utf-8"
            )

            passing = subprocess.run(
                [sys.executable, str(tool), str(expected_path), str(actual_path), "--abs-tol", "0.001"],
                check=False,
                capture_output=True,
                text=True,
            )
            failing = subprocess.run(
                [sys.executable, str(tool), str(expected_path), str(actual_path), "--abs-tol", "0.0001"],
                check=False,
                capture_output=True,
                text=True,
            )

        self.assertEqual(0, passing.returncode, passing.stderr)
        self.assertIn("DXF semantic match", passing.stdout)
        self.assertEqual(1, failing.returncode, failing.stderr)
        self.assertIn("DXF semantic mismatch", failing.stdout)

    def test_compressed_snapshot_retains_all_semantics(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            dxf_path = Path(directory) / "reference.dxf"
            snapshot_path = Path(directory) / "reference.semantic.json.gz"
            actual_path = Path(directory) / "actual.dxf"
            dxf_path.write_text(dxf(REFERENCE_LINE, REFERENCE_CIRCLE), encoding="utf-8")
            actual_path.write_text(
                dxf(
                    REFERENCE_CIRCLE.replace("5.0", "5.0004", 1),
                    REFERENCE_LINE.replace("AB", "FFFF"),
                ),
                encoding="utf-8",
            )

            create_snapshot(dxf_path, snapshot_path)
            snapshot_entities = load_snapshot(snapshot_path)
            result = compare_snapshot(snapshot_path, actual_path, abs_tol=0.001)

        self.assertEqual(2, len(snapshot_entities))
        self.assertTrue(result.equivalent, result.differences)

    def test_snapshot_detects_layer_colour_topology_and_coordinates(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            dxf_path = Path(directory) / "reference.dxf"
            snapshot_path = Path(directory) / "reference.semantic.json.gz"
            actual_path = Path(directory) / "actual.dxf"
            dxf_path.write_text(dxf(REFERENCE_LINE), encoding="utf-8")
            create_snapshot(dxf_path, snapshot_path)
            actual_path.write_text(
                dxf(
                    REFERENCE_LINE.replace("color_seams", "wrong_layer")
                    .replace("\n3\n10\n", "\n5\n10\n")
                    .replace("10.0", "10.25")
                ),
                encoding="utf-8",
            )

            result = compare_snapshot(snapshot_path, actual_path, abs_tol=0.01)

        self.assertFalse(result.equivalent)
        report = "\n".join(result.differences)
        self.assertIn("layer='color_seams'", report)
        self.assertIn("color=3", report)
        self.assertIn("layer='wrong_layer'", report)
        self.assertIn("color=5", report)

    def test_snapshot_detects_polyline_vertex_topology(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            dxf_path = Path(directory) / "reference.dxf"
            snapshot_path = Path(directory) / "reference.semantic.json.gz"
            actual_path = Path(directory) / "actual.dxf"
            dxf_path.write_text(dxf(REFERENCE_POLYLINE), encoding="utf-8")
            create_snapshot(dxf_path, snapshot_path)
            second_vertex = "\n".join(
                ("0", "VERTEX", "8", "cut", "10", "1.0", "20", "2.0", "30", "0.0", "0")
            )
            actual_path.write_text(
                dxf(REFERENCE_POLYLINE.replace(second_vertex, "0")),
                encoding="utf-8",
            )

            result = compare_snapshot(snapshot_path, actual_path)

        self.assertFalse(result.equivalent)
        report = "\n".join(result.differences)
        self.assertIn("vertices=2", report)
        self.assertIn("vertices=1", report)


class SemanticTextComparisonTests(unittest.TestCase):
    REPORT = """\
LABORATORI D'ENVOL PARAGLIDING 3.29

Scale: 1.00000
Flat area = 15.97 m2 171.9 ft2
Ribs number = 24
"""

    LINES = """\
Plan A
1 1A1 35.0 cm - type 1 Riser 25.00 mm
2 2A1 321.0 cm - type 2 Line275 1.90 mm
"""

    def test_whitespace_blank_lines_and_numeric_format_are_ignored(self) -> None:
        expected = parse_text_deliverable(self.REPORT)
        actual = parse_text_deliverable(
            "LABORATORI   D'ENVOL PARAGLIDING 3.2900\r\n"
            "Scale:\t1D0\r\n\r\n"
            "Flat area = 15.97000 m2\t171.900 ft2\r\n"
            "Ribs number = 24.0\r\n"
        )

        result = compare_text_records(expected, actual, abs_tol=1.0e-9)

        self.assertTrue(result.equivalent, result.differences)

    def test_numeric_tolerance_is_explicit_and_actionable(self) -> None:
        expected = parse_text_deliverable(self.REPORT)
        actual = parse_text_deliverable(self.REPORT.replace("15.97", "15.9705"))

        passing = compare_text_records(expected, actual, abs_tol=0.001)
        failing = compare_text_records(expected, actual, abs_tol=0.0001)

        self.assertTrue(passing.equivalent, passing.differences)
        self.assertFalse(failing.equivalent)
        self.assertIn("delta 0.0005", "\n".join(failing.differences))

    def test_text_and_record_order_remain_semantic(self) -> None:
        expected = parse_text_deliverable(self.LINES)
        changed_text = parse_text_deliverable(self.LINES.replace("Riser", "Wrong"))
        reordered = parse_text_deliverable(
            "\n".join(reversed(self.LINES.splitlines()))
        )

        text_result = compare_text_records(expected, changed_text)
        order_result = compare_text_records(expected, reordered)

        self.assertFalse(text_result.equivalent)
        self.assertIn("Riser", "\n".join(text_result.differences))
        self.assertFalse(order_result.equivalent)

    def test_compressed_text_snapshot_round_trip_and_comparison(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory) / "lines.txt"
            actual = Path(directory) / "actual-lines.txt"
            snapshot = Path(directory) / "lines.semantic.json.gz"
            source.write_text(self.LINES, encoding="utf-8")
            actual.write_text(
                self.LINES.replace("35.0", "35.000000").replace("  ", "   "),
                encoding="utf-8",
            )
            create_text_snapshot("lines", source, snapshot)

            result = compare_text_snapshot(snapshot, actual, abs_tol=1.0e-9)

        self.assertTrue(result.equivalent, result.differences)


if __name__ == "__main__":
    unittest.main()
