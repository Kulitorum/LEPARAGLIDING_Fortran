# Color divisions from a flattened 2D wing

LEparagliding now has a first production-oriented color workflow for the way
Pere Casellas actually designs artwork: a fully flattened 2D wing with simple
open division lines, usually running from the left rib of a panel to the right.

The implementation deliberately builds on sections 15 and 16 instead of
introducing a second panel model. It has two stages:

1. `tools/import_color_divisions.py` samples an authored DXF division line at
   the flattened rib references and writes paste-ready section 15 or 16 data.
2. `src/main/15_colors.inc` maps matching boundary IDs onto the developed
   upper or lower panels and adds the internal sewing line, both allowances,
   and inset matching marks.

The reference case is the author's Swoop2 project:
[20260820 Swoop2.zip](https://laboratoridenvol.com/projects/Swoop2/data/20260820%20Swoop2.zip).

## Confirmed design rules

The August 20, 2026 review established these rules for the first iteration:

- Artwork is normally designed on the fully flattened 2D wing. An inflated or
  arched reference is not required for the normal workflow.
- Artwork uses open division lines, not closed colored polygons.
- Designs are usually simple straight cuts across one panel from rib to rib.
- Existing layers and AutoCAD colors have construction meaning and must remain
  unchanged.
- Both pieces adjacent to an internal color boundary receive a seam allowance.
- Matching marks are required on the adjacent construction and should be moved
  about 1.0--1.2 mm inward where possible so they remain hidden in the wing.
- Compensation for the final arched flight shape is a possible later feature,
  not part of this simple 2D-first workflow.

The maintained default matching-mark inset is 1.1 mm.

## DXF import

The importer is a dependency-free ASCII DXF reader. It currently accepts open
`LWPOLYLINE` entities and connected `LINE` entities. The longest matching path
on the selected semi-wing is intersected with the selected rib-reference
lines. Each intersection is projected along that chord reference and converted
to LEP's section-15/16 percentage convention.

Example using the Swoop2 intrados artwork line:

```powershell
python tools/import_color_divisions.py `
  "20260818 Swoop2 2D.dxf" `
  --surface intrados `
  --division-layer 0 `
  --division-color 6 `
  --rib-layer default `
  --rib-color 5 `
  --output swoop2-section16.txt
```

On the supplied Swoop2 DXF this recovers all 26 records: 48.00% at the center,
54.46% at rib 2, and 61.00% over the remaining ribs. Percentages are snapped to
0.01% by default to remove insignificant noise from four-decimal CAD geometry;
use `--snap-percent 0` to disable this.

The tool does not rewrite the DXF. Its output comments record the original
division layer, entity ACI color, rib layer, and rib ACI color. Hatches, images,
piece colors, and every construction entity stay in the designer's source file.
This is important because colors and layers already carry manufacturing meaning.

The importer defaults to the positive semi-wing because sections 15 and 16
describe one semi-wing. Use `--side negative` for artwork drawn only on the
opposite half, and verify the generated rib order before pasting the block into
`leparagliding.txt`.

## Section 15/16 semantics

Classic section records are rib based:

```text
number_of_ribs_with_boundaries
rib_number  boundaries_on_this_rib
boundary_id  chord_percent  off_rib_offset
```

For example:

```text
4
1  1
1  48.00  0.0
2  1
1  54.46  0.0
3  1
1  61.00  0.0
4  1
1  61.00  0.0
```

The first value on each boundary row is its stable boundary ID. Equal IDs on
ribs `i` and `i+1` define one open division across developed panel `i`. This
supports more than one straight division per panel without relying on row order.
The current production construction accepts on-rib records (`off_rib_offset =
0`). The historical `-2` panel-input mode is still drawn in the planform but is
not silently reinterpreted as the new manufacturing construction.

## Developed-panel output

Stage 8 first creates the complete outer panel geometry. Stage 15 then adds only
the missing internal color construction:

| DXF layer | Contents |
|---|---|
| `color_seams` | Finished internal sewing line shared by both pieces |
| `color_allowance` | Two parallel cut edges, one allowance on each side |
| `color_marks` | Paired registration crosses inset inside both adjacent cut edges |

Upper construction retains ACI 4 and lower construction retains ACI 30, matching
the existing section-15/16 design-view convention. No entity on `default`,
`vents`, `triangles`, `mcircles`, or an author-defined layer is moved,
recolored, or deleted.

The allowance width comes from the normal upper/lower sewing allowance in
section 6. Input sewing widths are millimetres; the drawing engine converts them
to LEP's centimetre model coordinates. In the Swoop2 verification run, the
configured 10 mm produces two lines exactly 1.0 model unit from each sewing
line.

## NaN repair

The original color-mark routine calculated slopes twice: once on the normalized
profile and once on the developed edge. Repeated X coordinates could therefore
produce `0/0`, which is why the Swoop2 input warned that sections 15/16 caused a
NaN problem.

`leparagliding_color_geometry` now finds the containing profile segment and
applies its interpolation fraction directly to the developed edge. It handles
either traversal direction, repeated coordinates, exact vertices, invalid
ranges, and non-finite inputs without emitting geometry. The supplied Swoop2
project changes from 12 NaN coordinates in `leparagliding.dxf` to zero.

Color construction now obtains its extrados/intake/intrados ranges from the
validated `profile_topology` model. Raw `np(:,*)` column arithmetic is confined
to the legacy adapter. The `.dat` loader now rebuilds the contour and its named
boundaries after moving or inserting intake endpoints, including the shared
intake/intrados endpoint. This matters for Swoop2, whose reference profiles use
the `.dat` path.

## Validation

Four focused tests accompany the existing end-to-end suite:

- `color_geometry` checks interpolation, repeated profile coordinates, seam
  offsets, and the 1.1 mm inward mark calculation.
- `color_division_import` checks the importer against a small coordinate-only
  fixture derived from Swoop2 and its known section-16 values.
- `domain_model` checks named topology, unequal adjacent profile
  discretization, transactional production-panel snapshots, and the exact
  neutral-segment representation used by the stage-8 dual-run.
- `profile_data` exercises exact, shifted-to-`j`, shifted-to-`j+1`, and inserted
  `.dat` intake endpoints, then checks the rebuilt counts and shared indices.

The complete author-supplied Swoop2 DXF was also used locally as the end-to-end
acceptance case. It imports 26 rib crossings and the full calculation produces
50 internal seam lines, 100 allowance edges, 200 mark strokes, and no NaNs.
The 25 MB reference archive is intentionally not copied into this repository.

## Current boundary and next work

This iteration automates straight open divisions and produces manufacturing
construction on the existing developed panels. It does not yet extract and
nested-layout each adjacent color region as an independent closed CAD object,
nor does it assign a fabric/material record to each side of a boundary.

The next useful steps are:

1. agree on a small side-assignment record that maps every boundary ID to the
   two piece colors/materials while retaining source DXF layer and ACI metadata;
2. split the existing outer panel path at the internal construction and emit
   independently selectable closed piece paths;
3. add labels containing panel, surface, boundary, side, color, and material;
4. support several connected straight divisions and validate junction order;
5. compare the generated pieces with Pere's finished Swoop2 cutting layout;
6. only then investigate curved divisions and optional arch compensation.

The simple 2D workflow should remain the default even if those advanced modes
are added.
