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


if __name__ == "__main__":
    unittest.main()
