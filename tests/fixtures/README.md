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

## Classic-tension gnuA3 calculation

`classic-skin-gnua/` contains the published gnuA3 preset from the maintained
LeParagliding `resources/presets/gnuA3` tree. The configuration identifies the
2021-07-24 gnuA3-25 design; its public source is
<https://www.laboratoridenvol.com/projects/gnu-A3/leparagliding.txt>. The
vendored SHA-256 values are:

- `leparagliding.txt`: `d8239309fcbc7cc5a88b174bb69b99f09623dabe78e8e108922a3d76dfc2d380`
- `gnua.txt`: `dc4548baf611877a7ea5ef9b028450c62f657e37a38d83fcdefd617133d7a9f9`
- `gnuat.txt`: `11d84f2f34de2bf0260a00c021becb3cb261dbaedf436afdef262425d6f4787b`

The design declares 28 cells and 29 ribs, enables section-29 3D shaping, and
sets section 31 to zero so the complete run exercises classic skin tension.
`classic_skin_regression` verifies that mode before execution, rejects
non-finite DXF geometry, checks the reported counts, and compares all five
principal outputs with reviewed, line-ending-normalized SHA-256 oracles.
`disabled_shaping_regression` derives a second input in its isolated build
directory by replacing section 29 with its documented zero value. It verifies
the one-based no-cut group and zero-initialized shaping-influence path under
runtime bounds checking, while leaving the authored fixture immutable.
