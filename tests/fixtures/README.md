# Test fixtures

## Color importer

`swoop2-color-overlay-sample.dxf` is a minimal coordinate-only extract created
from Pere Casellas' public
[Swoop2 reference archive](https://laboratoridenvol.com/projects/Swoop2/data/20260820%20Swoop2.zip).
It contains four flattened chord-reference lines and four vertices from the
open intrados division polyline—no profiles, panel patterns, plans, hatches, or
raster artwork.

The expected percentages are the section-16 values in the same project:
48.00%, 54.46%, 61.00%, and 61.00%. The full 25 MB design DXF remains external
and was used only for local end-to-end acceptance testing.

## Even-cell Swoop2 calculation

`even-cell-swoop2/` contains `20260820 Swoop2/lep/leparagliding.txt` and
`20260820 Swoop2/lep/gnuReflex.txt` from the same author-supplied public archive,
retrieved 2026-08-20 and included at the author's direction as regression data.
Their upstream SHA-256 values are respectively
`4510679a1c17655af3ee3a0023853ee5024f3cc05a903990c18635b6fa9e8cb6` and
`fbdd99584c4d7739a8c7ed11d40ae0707947b568fbef0b00da67894d0ffc4b0c`;
the repository copies differ only by one terminal newline. The design declares
50 cells and uses a collapsed, zero-thickness central cell, so its 26 stored
half-wing rib rows exercise the even-cell identity and panel-zero paths.

The `even_cell_regression` CTest runs the complete 3.29 calculation, requires
the collapsed-center diagnostic and declared 51-rib/50-cell report, rejects any
parity warning or non-finite DXF geometry, and compares all five principal
text/DXF outputs with reviewed, line-ending-normalized SHA-256 oracles.
Generated outputs, including the approximately 20 MB main DXF, are deliberately
omitted from the repository.
