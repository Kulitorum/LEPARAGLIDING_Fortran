# Data-model refactor: inventory and migration design

Status: implementation in progress; phases 0--1 complete, phase 2 staged,
phase 3 complete at the real-panel neutral boundary, and the first Phase-4
lower-intrados production-shaping and terminal-reformat checkpoints complete

Evidence baseline: typed adapter/color, topology, neutral terminal-boundary,
authoritative extrados/intake/intrados, and typed regular plus physical-terminal
lower-intrados shaping/reformat checkpoints

Primary scope: profiles, ribs, the spatial wing, flattened panels, production
edges, and their immediate consumers

## Purpose

The goal is to make the construction of the wing in space and in two dimensions
explicit in the program's types and names. This is not a mechanical rename. The
same legacy arrays currently contain input data, intermediate transforms,
manufacturing geometry, feature-specific workspaces, and temporary
accumulators. The numbered includes all execute in one program scope and their
order is a numerical dependency (`src/leparagliding.f:86-113`). A safe refactor
must therefore change one semantic boundary at a time and prove equivalence at
each boundary.

This document records what can be established from the code, marks uncertain
interpretations as such, proposes the target model, and defines a phased
migration with objective exit criteria.

## Implementation checkpoint: 2026-08-20

The first migration boundary is implemented alongside this plan:

- `src/leparagliding_domain_model.f90` defines validated normalized-profile,
  spatial-rib, production-panel, and color-division types.
- Named compatibility constants quarantine legacy slots 2 and 9:12.
- Transactional adapters copy normalized profiles, spatial `x/y/z` ribs, and
  complete production-panel sewing/cut edges without exposing partially valid
  objects.
- The section-15/16 color construction is the first production consumer: it
  now reads named profiles and sewing edges from `production_panel_2d` rather
  than indexing slots 2, 9, and 10 directly.
- `tests/test_domain_model.f90` covers validation, independent adjacent-profile
  topology, panel zero, non-finite rejection, and adapter ownership.
- `tools/dxf_semantic_diff.py` adds a tolerance-aware DXF geometry oracle for
  later numerical migrations.

### Second checkpoint: topology, rib roles, and neutral development

The next migration boundary is now implemented and dual-running:

- `profile_topology` replaces raw `np(:,1:6)` at typed boundaries with named,
  validated extrados, intake, and intrados ranges.
- The `.dat` producer now rebuilds the contour when intake endpoints are moved
  or inserted, records their actual output indices, and initializes the shared
  intake/intrados endpoint. It previously left `np(:,5)` indeterminate and
  could report `j` after moving a boundary to `j+1`.
- Color construction no longer indexes any `np` column. Its production-panel
  adapter supplies both normalized profiles and their named topology while
  retaining the legacy behavior for unequal adjacent discretizations.
- `rib_identity` distinguishes authored centerline/center-adjacent/interior/
  wingtip ribs, finite-center symmetry mirrors, collapsed-center aliases, and
  the nonphysical extrapolated tip support. The legacy `cencell >= 0.01` rule
  remains authoritative for central-panel activity; declared cell parity is a
  diagnostic, while source and placement stations are validated after stage 4.
- `neutral_panel_2d` owns exact lower/higher segment starts and ends for one
  surface and exposes the intake-only support segment separately from the
  intrados segment that shares its legacy array index.
- Stage 8 initially dual-ran extrados lengths and widths through the
  neutral-panel model and stopped on disagreement while retaining the legacy
  assignments as the numerical authority.

The dual-run revealed that the higher developed edge is only approximately
chained: repeated triangulation can leave small gaps between one segment end
and the next segment start. The typed object therefore preserves exact segment
endpoints and reports maximum join gaps instead of silently merging points into
a mathematically continuous polyline. This distinction is essential for
bit-for-bit legacy length comparisons.

### Third checkpoint: pure extrados development and typed metrics

The extrados migration now crosses its first producer boundary:

- A maintained even-cell Swoop2 fixture runs the complete 50-cell design,
  asserts its collapsed center, and freezes all five principal outputs.
- Stage 6 snapshots generated center row 0 and physical rows 1 through `nribss`
  into `spatial_rib_geometry_3d`; the extrapolated tip-support row remains an
  explicit legacy-only role.
- `develop_extrados_panel` is a pure, transactional stage-7 developer. It
  returns exact lower/higher segment chains plus named `pa`--`pf` source
  distances while preserving the historical expression order and one-argument
  arctangent. It requires matching extrados indices, not identical whole-profile
  discretization, so adjacent ribs may use different intake/intrados counts.
- Stage 7 dual-runs that routine for every regular extrados panel and compares
  all eight endpoint scalars and six source distances per quadrilateral.
- Stage-8 extrados contour lengths and widths are now assigned from typed
  neutral panels after the typed and legacy results agree. Its adapter likewise
  validates compatibility per surface rather than across unrelated ranges.

The odd-cell Plan B outputs and Swoop2 geometry/line outputs are unchanged. The
Swoop2 report oracle changed only to correct its displayed counts from 52/51 to
the declared 51 ribs and 50 cells.

### Fourth checkpoint: authoritative extrados write-back and classic tension

The checked producer boundary is now active for the exact regular extrados
slice:

- After every stage-7 legacy/typed endpoint and source-distance comparison
  succeeds, `write_legacy_extrados_panel` publishes the typed exact segment
  starts and ends into the corresponding `pl*/pr*` slice. The adapter validates
  panel ownership, both adjacent extrados topologies, array shapes and bounds,
  and, at that checkpoint, separation from the reserved scratch index before
  making any write. The reservation was removed by the later intrados
  checkpoint.
- The pure typed extrados developer is therefore the final writer for the
  segment indices it owns. The legacy calculation remains a comparison oracle
  during this migration, rather than the final source of those values.
- A realistic classic-tension (`k31d=0`) full-output fixture now covers the
  non-single-surface stage-8 path and freezes all five principal outputs.
- When section 29 shaping is disabled, ribs are assigned to its declared
  one-based no-cut group. Parsing rejects incomplete or inconsistent group
  coverage, the stage-6 lookup and 3D shaping consumer check bounds
  defensively, and a derived full-output gnuA3 run covers the disabled mode.

This authority is intentionally not broader than the adapter's exact extrados
range. At this checkpoint, intake/vent support, intrados development, the
point-499 scratch convention, and the dummy/tip-support row remained
legacy-owned.

### Fifth checkpoint: authoritative intake and explicit support

The next safe surface boundary is now active without widening regular panel
ownership:

- `develop_intake_panel` reuses the pure distance-preserving quadrilateral
  engine for matching intake ranges. Exact contour segments occupy
  `first:last-1`; the legacy look-ahead quadrilateral at `last` is returned only
  through the named post-surface support fields.
- Stage 7 dual-runs every endpoint and all six independent source distances for
  both the contour and support. `write_legacy_intake_panel` then publishes the
  agreeing typed values for rows `0:nribss-1`, after validating shapes, bounds,
  topology, ownership, and the distinct contour/support ranges.
- The support at intake `first=np(i,2)` supplies the new-skin-tension extrados
  look-ahead; the support at intake `last` supplies the intake/intrados tangent.
  Both are explicit cross-surface dependencies rather than extrados ownership.
- Stage-8 lower and wingtip-higher intake lengths are assigned from exact typed
  segments after agreement with the legacy sums.

At this checkpoint, intrados and scratch index 499 remained outside the intake
boundary. No typed panel was created for row `nribss`.

### Sixth checkpoint: physical terminal comparison edges

The stage-8 terminal boundary now has explicit geometry and provenance without
inventing a panel beyond the wingtip:

- `neutral_boundary_edge_2d` owns one exact surface edge, its join diagnostic,
  its physical boundary-rib index, and the real source-panel index. Intake
  boundaries also retain their explicit post-surface support and support join
  gap.
- `derive_neutral_boundary_edge` transactionally copies the higher edge of the
  final real `neutral_panel_2d`. `neutral_boundary_edge_length` measures its
  exact segments without closing legacy reconstruction gaps or including the
  optional support segment.
- Stage 8 uses these boundary objects for the physical wingtip extrados and
  intake comparison lengths. Both originate from the higher side of panel
  `nribss-1`; row `nribss` is not interpreted as a panel and no outward geometry
  is fabricated.

### Seventh checkpoint: authoritative retained intrados

The last regular neutral surface now crosses the typed producer boundary
without inventing a terminal panel:

- `develop_intrados_panel` reuses the pure quadrilateral engine for the exact
  intrados segment range `first:last-1`. It starts in the intrados coordinate
  frame and has no dependency on a reserved legacy point.
- Stage 7 develops and dual-compares every real panel `0:nribss-1` before intake
  publishes its post-surface support over the shared legacy index. All eight
  endpoint scalars and the six independent source distances must agree.
- The agreeing typed intrados panels are retained across vent processing.
  After the vent consumers release intake support, stage 8 transactionally
  publishes each retained panel through `write_legacy_intrados_panel` and then
  runs the existing shaping paths.
- Stage-8 intrados lower/higher lengths and widths are assigned from typed
  panels or the typed terminal boundary after comparison with the legacy
  calculations.
- The point-499 save/restore and the associated topology collision restriction
  are removed. Unit coverage accepts a 500-point topology and proves that its
  real final intrados segment can occupy index 499. This establishes capacity
  for the typed topology and neutral adapters only; it does not certify every
  later legacy algorithm for arbitrary 500-point profiles.

Typed intrados ownership stops at `nribss-1`. Row `nribss` remains a physical
terminal boundary/non-panel and is deliberately untouched by regular-panel
write-back.

### Seventh checkpoint: Section-31 laws and lower-intrados shaping

The first Phase-4 slice now owns new-law lower-intrados production geometry for
every real panel while retaining the existing calculation as a comparison
oracle:

- `leparagliding_skin_tension` defines a validated, transactional
  `skin_tension_law`. The Section-31 adapter selects intrados columns 3/4,
  reverses authored trailing-edge-to-leading-edge positions into increasing
  developed-contour order, and requires finite, strictly increasing 0--100
  percent coverage with nonnegative overwidths.
- `evaluate_skin_tension_offset` scales the normalized positions by contour
  length and overwidth by panel width. It deliberately preserves the promoted
  default-REAL `1.001` inclusive upper-bound factor and last-matching-interval
  behavior of overlapping legacy intervals.
- For `k31d=1`, stage 8 dual-compares and publishes typed offsets on the lower
  intrados side of panels `0:nribss-1`. The final contour point is included: it
  owns an offset and shaped point even though the final real segment is
  `last-1` and there is no outgoing segment at `last`.
- `leparagliding_panel_shaping` defines transactional `shaped_panel_side_2d`
  results and a pure side-parameterized kernel. The first integrated boundary
  publishes the agreeing lower-intrados sewing and cut coordinates to legacy
  slots 9 and 11 for remaining consumers.
- The kernel retains the legacy lower/higher normal signs, horizontal-initial-
  point convention, incoming-segment endpoint bias at joins, and promoted
  default-REAL `0.1` millimetre allowance conversion. These are explicit
  manufacturing-file compatibility rules, not accidental cleanup targets.
- `production_boundary_edge_2d` separately represents the physical wingtip.
  For the `k31d=1` intrados path, its neutral source is the higher edge of panel
  `nribss-1`, but its production orientation is the lower/outward normal.
  A checked neutral adapter defines the previously unproduced terminal
  `pl1/pl2` oracle; typed distances, offsets, sewing coordinates, and cut
  coordinates must agree before the production adapter writes only row
  `nribss` slots 9/11. It cannot represent or write terminal slots 10/12.

The unit suite now contains 15 registered tests, including focused
`skin_tension` normalization/evaluation coverage and `panel_shaping` coverage
for all quadrants, axis-aligned segments, endpoint bias, allowance conversion,
validation, and transactional failure. Full Plan B and even-cell regressions
exercise the integrated `k31d=1` path; the classic fixture continues to protect
`k31d=0`.

### Eighth checkpoint: terminal intrados length matching

`leparagliding_panel_reformat` now owns the exact physical-terminal intrados
range after the optional `ndif=1000` length match for `k31d=1`:

- `boundary_length_match_control` names the original contour length, target rib
  length, control fraction, and the independently validated measurement and
  reconstruction indices. Keeping two indices explicitly preserves the legacy
  `jirr`/`jirl` behavior rather than hiding it behind one ambiguous variable.
- `reformat_terminal_intrados_boundary` is pure and transactional. It retains
  the absolute-angle/four-quadrant reconstruction and the real-exponent distance
  expressions. Its accumulator, corrected length, and scale are explicitly
  `real32`, documenting the old undeclared `dist2/dist3/distk` variables instead
  of allowing implicit typing to decide their kind.
- Stage 8 retains the pre-reformat typed boundary, runs the old calculation as
  an oracle, compares every exact-range sewing coordinate, and publishes the
  typed result only after agreement. Each cut point receives the same
  displacement as its sewing point, preserving the established allowance vector
  which the old slot-9-only rewrite stranded in slot 11.
- The legacy `intrados_first-1` extrapolation is deliberately not claimed by an
  intrados boundary. `preceding_join_support_2d` represents that cross-surface
  point, its preceding sewing anchor, and its cut point. The typed reformatter
  reproduces `anchor + anchor - support`, translates the cut point by the same
  displacement, and checked-writes only that exact slot-9/11 pair after legacy
  agreement.

Focused unit coverage includes expansion, fractional shrink, distinct start
indices, all quadrant signs, provenance, cut-vector retention, and transactional
failures. Plan B and Swoop2 execute the integrated new-tension path; the runtime
dual comparison supplies a terminal numeric oracle because downstream drawing
loops do not expose that row in the reviewed output hashes.

## Terminology used in this plan

The names below describe coordinate domains, not presentation views:

- **Normalized profile**: the signed two-dimensional airfoil contour, initially
  expressed as fractions of chord and then as percentages.
- **Rib-local profile**: the normalized contour scaled to a rib chord, before
  placement in the full spatial wing.
- **Spatial rib**: one complete profile placed in the wing's three-dimensional
  coordinate system.
- **Neutral development**: the chained two-dimensional quadrilaterals obtained
  from spatial distances before skin-tension shaping.
- **Production panel**: the tensioned developed panel, including sewing and cut
  edges, but excluding drawing-sheet translation.
- **Sheet layout**: translation, mirroring, labels, and duplicated print/laser
  placement applied only while emitting a drawing.

“Extrados” and “intrados” are retained because those are the source and file
format terms. “Left” and “right” mean the lower and higher rib-index sides of a
panel in this document; the physical viewing convention needs confirmation from
Pere.

## What the current program proves

### Execution and ownership

`declarations.inc` is included before every executable stage, and stages 3
through 23 follow in a fixed order (`src/leparagliding.f:93-113`). There is no
stage-owned state: every numbered include can read or overwrite every declared
array. The declarations themselves explicitly call this a shared legacy schema
and advise that new state belong in typed modules
(`src/main/declarations.inc:1-6`).

The practical blast radius is large:

- `rib` is referenced by input, drawing, profile construction, panel shaping,
  singular-point, line, brake, internal-rib, reporting, and 3D-output stages.
- `np` controls loops in stages 6, 7, 8, 9, 10, 11, 12, 16, 18, and 21.
- `u/v/w` stores unrelated domains used by stages 6, 8, 9, 10, 11, 12, 14,
  15, 16, and 21.
- `pl*/pr*` is produced in stage 7, transformed in stage 8, and also consumed
  extensively by internal-rib construction in stage 16.

This is why a repository-wide textual rename is unsafe: a name such as `u`
does not identify a domain without its third subscript and the current stage.

### Length and angle units

The strongest code evidence is that working geometry is intended to be in
centimetres:

- Planform coordinates are multiplied by the dimensionless wing scale `xwf`
  immediately after input (`src/main/04_data_reading.inc:95-103`).
- A sewing allowance read in millimetres is divided by 10 before being applied
  to rib or panel geometry (`src/main/06_airfoil_geometry.inc:316-321` and
  `src/main/08_skin_tension.inc:14-16`).
- The report labels spatial centre-of-mass coordinates and cell width as `cm`
  (`src/main/18_text_output.inc:113-115` and `src/main/18_text_output.inc:162`).
- Profile fractions are multiplied by chord to enter the same working length
  domain (`src/main/06_airfoil_geometry.inc:144-150`).

`xkf` and `xwf` are separate inputs (`src/main/04_data_reading.inc:26-34`).
`xwf` scales wing geometry, while `xkf` scales fixed drawing-sheet locations,
as shown by the panel layout origins in `src/main/07_panel_development.inc:94-97`.
The target model must not store either drawing scale or sheet origin in a
geometric object.

Input angles are degrees. They are converted locally to radians for
trigonometry (`src/main/06_airfoil_geometry.inc:136-140`). The target model
should make that conversion once at the input boundary and suffix any remaining
scalar names with `_deg` or `_rad`.

There are inconsistent later uses of `xwf`, including expressions that appear
to scale already-scaled rib values. Those expressions must be preserved during
the compatibility phases and investigated separately; they are not evidence
for introducing a second physical length unit.

## Legacy schema inventory

### `rib(0:100,500)`

The array is declared at `src/main/declarations.inc:216`. It combines at least
four categories that should not share a type: user input, derived rib geometry,
panel measurements, and scratch accumulators.

#### Core planform and profile columns

| Column | Proposed name | Unit/domain | Producer | Principal consumers/evidence |
|---:|---|---|---|---|
| 1 | `source_rib_number` | identifier stored as real | section 1 input | Read with the geometry row (`src/main/04_data_reading.inc:68-77`); not safe as an array index without conversion. |
| 2 | `planform_station` | working length, planform | section 1 input, then `*xwf` | Planform drawing and span calculations; scaling is at `src/main/04_data_reading.inc:95-100`. Exact physical axis name needs confirmation. |
| 3 | `leading_edge_position` | working length, planform/chordwise | section 1 input, then `*xwf` | Added to scaled profile U to form spatial Y (`src/main/06_airfoil_geometry.inc:180-183`). |
| 4 | `trailing_edge_position` | working length, planform/chordwise | section 1 input, then `*xwf` | Used with column 3 to derive chord (`src/main/04_data_reading.inc:102-103`). |
| 5 | `chord_length` | working length | derived as column 4 minus 3 | Scales normalized profiles (`src/main/06_airfoil_geometry.inc:148-150`) and converts percentages to lengths. |
| 6 | `spatial_station_x_prime` | working length | section 1 input, then `*xwf` | Used in absolute spatial X placement (`src/main/06_airfoil_geometry.inc:180-183`). The legacy header calls it `x'`; the aerodynamic meaning is uncertain. |
| 7 | `spatial_height` | working length | section 1 input, then `*xwf` | Used in absolute spatial Z placement (`src/main/06_airfoil_geometry.inc:180-183`). |
| 8 | `washin_angle_deg` | degrees | copied or derived from wash-in mode | Converted to radians before rib-local rotation (`src/main/06_airfoil_geometry.inc:136-155`). |
| 9 | `rib_plane_angle_deg` | degrees | section 1 input | Used for the next spatial rotation (`src/main/06_airfoil_geometry.inc:173-178`). Legacy comments call it `beta`. |
| 10 | `washin_pivot_percent` | percent chord | section 1 input | Divided by 100 and multiplied by chord for the rotation pivot (`src/main/06_airfoil_geometry.inc:152-156`). A zero input is changed to `0.01` (`src/main/04_data_reading.inc:82-85`). |
| 11 | `intake_start_percent` | percent chord, signed convention | section 2 input | Used by profile reformatting to insert the intake boundary (`src/procedures/profile_data.inc:64-101`). |
| 12 | `intake_end_percent` | percent chord | section 2 input | Used by profile reformatting for the other intake boundary (`src/procedures/profile_data.inc:120-166`). |
| 14 | `cell_open_flag` | integer-like flag stored as real | section 2 input | Both adjacent ribs are tested to decide whether a cell is closed (`src/main/07_panel_development.inc:179-183`). |
| 15 | `anchor_count` | integer-like count stored as real | section 3 input | Controls A--F iteration (`src/main/06_airfoil_geometry.inc:275-311`). |
| 16:21 | `anchor_percent(1:6)` | percent chord | section 3 input | Converted to chord lengths and interpolated on profiles (`src/main/04_data_reading.inc:2476-2487`; `src/main/09_singular_rib_points.inc:14-43`). |
| 22 | `extrados_cell_width` | working length | stage 8 | Measured between neutral developed sides around an extrados mid-index (`src/main/08_skin_tension.inc:71-77`). |
| 23 | `extrados_contour_length` | working length | stage 8 | Accumulated from neutral developed segments (`src/main/08_skin_tension.inc:22-28`, `53-57`). |
| 24 | `intrados_cell_width` | working length | stage 8 | Measured at an intrados mid-index (`src/main/08_skin_tension.inc:79-80`, `90-98`). |
| 25 | `intrados_contour_length` | working length | stage 8 | Accumulated from neutral developed segments (`src/main/08_skin_tension.inc:30-36`, `59-63`). |
| 26 | `intake_contour_length` | working length | stage 8 | Accumulated over the intake range (`src/main/08_skin_tension.inc:38-44`, `65-68`). |
| 50 | `profile_vertical_displacement` | working length | section 2 input, later `*xwf` | Subtracted before rotation and restored afterward (`src/main/06_airfoil_geometry.inc:148-150`, `189-190`). The design intent of “unloaded rib descent” needs confirmation. |
| 51 | `input_washin_angle_deg` | degrees | section 1 input | Copied to column 8 for wash-in mode 0 (`src/main/04_data_reading.inc:105-111`). |
| 55:56 | unresolved section-2 parameters | mixed/unknown | section 2 input | Read beside airfoil configuration (`src/main/04_data_reading.inc:149-165`); column 56 is later applied as a percentage of mean chord in a planform construction (`src/main/05_graphic_design.inc:398-402`). Pere should name both. |
| 66:70 | `anchor_chord_length(1:5)` | working length | data post-processing | Derived from columns 16:20 (`src/main/04_data_reading.inc:2476-2487`). |
| 110:115 | `anchor_profile_u(1:6)` | rib-local working length | stage 6 | Interpolated anchor U coordinates (`src/main/06_airfoil_geometry.inc:281-301`). |
| 120:125 | `anchor_profile_v(1:6)` | rib-local working length | stage 6 | Interpolated anchor V coordinates in the same block. |
| 130:135 | `te_to_anchor_contour_length(1:6)` | working length | stage 6 | Accumulated along the lower contour (`src/main/06_airfoil_geometry.inc:293-301`). |
| 160 | `profile_height_scale` | dimensionless | later input section | Multiplies normalized V while reading profiles (`src/main/06_airfoil_geometry.inc:37-41`). |
| 165 | feature/control value, unresolved | unknown | input and later geometry | Widely shared by stages 4, 5, 6, 8, 9, 11, and 21; it must be named only after tracing its input section with Pere. |
| 169 | `shaping_group_index` | integer-like index stored as real | input | Converted to `ng` by 3D shaping (`src/procedures/geometry_3d.inc:39-40`). |
| 190:193 | four rib/panel side lengths | working length | stage 11 | Explicitly initialized and measured for both sides of an extrados panel (`src/main/11_panel_lengths.inc:84-119`). |
| 194:195 | `extrados_left/right_length_ratio` | dimensionless | stage 11 | Ratios of production-panel length to source-rib length (`src/main/11_panel_lengths.inc:118-119`). |
| 196:199 | four rib/panel side lengths | working length | stage 11 | Intrados equivalents (`src/main/11_panel_lengths.inc:123-133`). |
| 200:201 | `intrados_left/right_length_ratio` | dimensionless | stage 11 | Intrados ratios (`src/main/11_panel_lengths.inc:134-135`). |
| 250 | `profile_rotation_z_deg` | degrees | section 1 input | Converted to radians in spatial construction (`src/main/06_airfoil_geometry.inc:136-140`). |
| 251 | `profile_rotation_pivot_percent` | percent chord | section 1 input | Converts to a chordwise pivot length (`src/main/06_airfoil_geometry.inc:139-140`). |

#### Columns that must become locals rather than fields

Columns 30:39 are derived lengths and ratios for corresponding panel/rib sides
(`src/main/11_panel_lengths.inc:18-67`). Columns 40:45 are repeatedly zeroed and
reused as partial-length accumulators inside mark loops
(`src/main/11_panel_lengths.inc:210-229`). Columns 107:108 are also overwritten
as interpolation scratch while locating each anchor mark
(`src/main/11_panel_lengths.inc:1073-1086`). These values are not rib identity;
they should become named local records such as `side_length_metrics` and
`polyline_position`.

The declaration comment currently labels both columns 33 and 34 as the rib
intrados length (`src/main/declarations.inc:39-45`), while executable stage 11
shows 33 = left panel, 34 = rib, and 35 = right panel
(`src/main/11_panel_lengths.inc:20-25`). Executable code is the evidence used in
this plan.

The remaining high-numbered columns support individual features and are not
safe to carry into a general `rib` type wholesale. Each feature migration must
either give its columns a focused type or leave them behind a legacy adapter.

### `np(0:100,9)`

`np` is declared at `src/main/declarations.inc:262`. Only columns 1:6 have a
confirmed meaning; 7:9 have no literal executable references in the inspected
3.29 source and should not be reproduced in the new model without evidence.

| Column | Proposed name | Meaning and evidence |
|---:|---|---|
| 1 | `point_count` | Total contour points, read from `.txt` profiles (`src/main/06_airfoil_geometry.inc:28-39`) or produced by `.dat` reformatting (`src/procedures/profile_data.inc:54-58`, `163-166`). |
| 2 | `extrados_end_index` | Last extrados point and first intake point. Stage 7 uses extrados segments `1:np(2)-1` (`src/main/07_panel_development.inc:300-308`). |
| 3 | `intake_point_count` | Number of intake points. The last intake point is `np(2)+np(3)-1` (`src/main/06_airfoil_geometry.inc:30-35`). |
| 4 | `intrados_point_count` | Read/reformatted lower-surface count (`src/main/06_airfoil_geometry.inc:30-33`; `src/procedures/profile_data.inc:163-166`). It is mostly redundant once explicit index ranges exist. |
| 5 | `intake_end_index` | Derived as column 2 + column 3 - 1 (`src/main/06_airfoil_geometry.inc:35`). |
| 6 | `leading_edge_index` | Index nearest `(0,0)` in the original normalized contour (`src/main/06_airfoil_geometry.inc:55-67`). |

Confirmed contour partitions are:

```text
extrados points: 1 ... extrados_end_index
intake points:   extrados_end_index ... intake_end_index
intrados points: intake_end_index ... point_count
```

The endpoints are shared between adjacent partitions. Segment loops therefore
end one point before the inclusive point bound. For example, stage 7 uses
`1:np(2)-1`, `np(2):np(2)+np(3)-2`, and
`np(2)+np(3)-1:np(1)-1` (`src/main/07_panel_development.inc:20`, `193`,
`374`). The target must represent these as inclusive `index_range` values so
callers cannot mix point counts with end indices.

### `u/v/w(0:100,500,99)`

The arrays are declared together at `src/main/declarations.inc:224`. The first
subscript is sometimes a rib and sometimes a panel, the second is usually a
profile point, and the third is a semantic slot. `w` is meaningful only for
three-dimensional slots. The same slot number does not guarantee that all three
arrays form a point.

#### Confirmed core slots

| Slot | Proposed typed value | Domain/unit | Producer and evidence |
|---:|---|---|---|
| 1 | `normalized_profile_fraction` | rib-local 2D, fraction of chord | Profile input writes U/V (`src/main/06_airfoil_geometry.inc:37-41`); `.dat` handling may insert intake points (`src/procedures/profile_data.inc:54-58`, `163-166`). |
| 2 | `normalized_profile_percent` | rib-local 2D, percent chord | Stage 6 multiplies slot 1 by 100 (`src/main/06_airfoil_geometry.inc:142-146`). This is the profile domain used by the color mapper (`src/main/15_colors.inc:20-24`). |
| 3 | `scaled_profile_2d` | rib-local 2D, working length | U/V are multiplied by chord and V is temporarily displaced (`src/main/06_airfoil_geometry.inc:148-150`). This is also the source rib pattern used in stage 11 (`src/main/11_panel_lengths.inc:41-42`, `55-56`). |
| 4 | `washin_rotated_profile_3d_components` | intermediate local 3D, working length | Wash-in and Z rotation write U/V/W (`src/main/06_airfoil_geometry.inc:152-166`). |
| 5 | `rib_frame_components_3d` | intermediate local 3D, working length | The next rotation writes U/V/W before absolute placement (`src/main/06_airfoil_geometry.inc:168-183`). |
| 6 | `singular_profile_points` | rib-local 2D, working length | Indices 1:6 are anchors and 7:8 are intake boundaries (`src/main/09_singular_rib_points.inc:12-43`, `96-125`). It is not a full contour. |
| 7 | `left_shaping_law` | panel-side workspace, working length | U accumulates distance along the neutral left edge while V stores the local overwidth (`src/main/08_skin_tension.inc:117-146`). |
| 8 | `right_shaping_law` | panel-side workspace, working length | Right-edge equivalent (`src/main/08_skin_tension.inc:259-300`). Slots 7/8 are not planar point coordinates even though they use U/V. |
| 9 | `production_left_sewing_edge` | developed panel 2D, working length | Shaping offsets neutral `pl*` geometry into the left edge (`src/main/08_skin_tension.inc:188-223`). It is the lower-rib-index side for panel `i`. |
| 10 | `production_right_sewing_edge` | developed panel 2D, working length | Right-side equivalent from `pr*` geometry (`src/main/08_skin_tension.inc:333-365`). |
| 11 | `production_left_cut_edge` | developed panel 2D, working length | Offset from slot 9 by the configured seam allowance (`src/main/08_skin_tension.inc:219-223`). The legacy declaration calls this a sewing border; manufacturing terminology should be confirmed. |
| 12 | `production_right_cut_edge` | developed panel 2D, working length | Offset from slot 10 (`src/main/08_skin_tension.inc:361-365`). |
| 14:15 | `end_extension_construction_left/right` | developed 2D, working length | Calculated for leading/trailing ends; panel-edge procedures document the role (`src/procedures/panel_edges.inc:1-13`). |
| 16 | `rib_external_cut_contour` | rib-local 2D, working length | Stage 6 offsets slot 3 by the rib sewing allowance (`src/main/06_airfoil_geometry.inc:316-377`). |
| 17:20 | transformed suspension/brake singular points | mixed local then spatial 3D, working length | Stage 12 rotates slot-6 points through 17/18, places them in 19, and saves/restores 20 (`src/main/12_lines.inc:157-220`, `329-361`). These belong to line geometry, not profile storage. |
| 29,30,32,33,35 | panel-reformat scratch copies | developed 2D, working length | Stage 8 copies and rewrites edge coordinates during length matching (`src/main/08_skin_tension.inc:542-561`, `832-884`, `915-965`, `2201-2352`). They should be local work arrays. |
| 43 | `reformatted_rib_contour` | rib-local/developed comparison 2D, working length | Initialized from an averaged, scaled normalized profile (`src/main/09_singular_rib_points.inc:430`) and then length-adjusted in place (`src/main/09_singular_rib_points.inc:525-588`). |
| 44 | `panel_midline_contour` | developed 2D, working length | Average of panel slots 9 and 10 (`src/main/09_singular_rib_points.inc:438`). |
| 45 | `rib_reformat_candidate` | local scratch 2D | Temporary point written and copied back into slot 43 (`src/main/09_singular_rib_points.inc:525-533`, `581-588`). |
| 46 | `reformatted_rib_cut_contour` | rib pattern 2D, working length | Offset from slot 43 by rib allowance (`src/main/09_singular_rib_points.inc:635-706`). |
| 47 | `spatial_skin` | global 3D, working length | Copies `x/y/z` before 3D shaping (`src/main/08_skin_tension.inc:2653-2664`). |
| 48 | `spatial_panel_mid_surface` | global 3D, working length | Mean of adjacent slot-47 ribs (`src/procedures/geometry_3d.inc:41-47`). |
| 49 | `spatial_ballooned_mid_surface` | global 3D, working length | Offset from slot 48 using the solved ballooning height (`src/procedures/geometry_3d.inc:144-188`). |
| 53 | `panel_mid_surface_local` | panel-local 3D, working length | Slot 48 transformed into a local frame (`src/procedures/geometry_3d.inc:600-627`). |
| 54 | `ballooned_mid_surface_local` | panel-local 3D, working length | Slot 49 transformed into the same frame (`src/procedures/geometry_3d.inc:629-634`). |
| 55 | `spatial_panel_normal_reference` | global 3D, working length | A point 10 working units normal to the median-profile plane (`src/procedures/geometry_3d.inc:132-141`). |
| 69:70 | `assembled_developed_left/right` | developed panel 2D, working length | Stage 8 reconstructs full-contour edges from extrados, intake, and intrados family arrays (`src/main/08_skin_tension.inc:2667-2697`). |
| 71:72 | rib-boundary work contours | rib-local 2D, working length | Stage 8 repeatedly copies slot 3 and creates an allowance offset for minirib/cut work (`src/main/08_skin_tension.inc:2797-2812`). Exact feature ownership must be confirmed before naming more narrowly. |

Slots 50/51 save shaping-law values, but an in-code comment already questions
their meaning (`src/main/08_skin_tension.inc:425-426`). They remain explicitly
unresolved. Slots not listed above must not become anonymous members of a new
type. They stay in the compatibility store until their owning feature is
migrated and tested.

### `x/y/z`, `xx/yy/zz`, and axis meaning

`x/y/z(0:100,500)` is declared at `src/main/declarations.inc:246`. Stage 6
places each rotated profile using:

```text
x = rib(:,6) - w(:,:,5)
y = rib(:,3) + u(:,:,5)
z = rib(:,7) - v(:,:,5)
```

(`src/main/06_airfoil_geometry.inc:180-187`). These are the authoritative
unshaped spatial skin coordinates consumed by neutral panel development. Stage
7 reconstructs every flattened quadrilateral from distances between adjacent
`x/y/z` profiles (`src/main/07_panel_development.inc:300-322`).

`xx/yy/zz(1,500)` stores the mirrored central profile used for the special
panel between rib 0 and rib 1 (`src/main/06_airfoil_geometry.inc:262-269` and
`src/main/07_panel_development.inc:14-34`). It should not survive as a separate
coordinate type: the target spatial-wing collection should contain the virtual
rib explicitly, with metadata marking why it exists.

Rib 0 is not unused padding. Its profile is copied/mirrored from rib 1 and its
absolute X is negated (`src/main/06_airfoil_geometry.inc:222-260`). A second
virtual rib at `nribss+1` is synthesized for feature calculations
(`src/main/04_data_reading.inc:129-140`; `src/main/06_airfoil_geometry.inc:69-78`).
The target model therefore needs `rib_role = physical | symmetry_virtual |
tip_extrapolated`, rather than an invariant that all indices are physical or
start at one.

The exact aerodynamic labels for global X, Y, and Z are not inferred here. The
target initially retains `spatial_x/y/z`; Pere should confirm whether these are
spanwise, chordwise, and vertical in the intended sign convention.

### Neutral panel development: `pl*/pr*`

The eight arrays are declared at `src/main/declarations.inc:270-273` and form
four 2D points per spatial quadrilateral:

| Legacy pair | Proposed name | Meaning |
|---|---|---|
| `pl1x/pl1y` | `left_edge(point j)` | Lower-rib-index side at the start of contour segment `j`. |
| `pl2x/pl2y` | `left_edge(point j+1)` | Same side at the end of the segment. |
| `pr1x/pr1y` | `right_edge(point j)` | Higher-rib-index side at the start. |
| `pr2x/pr2y` | `right_edge(point j+1)` | Same side at the end. |

Stage 7 derives six distances from one spatial quadrilateral, reconstructs the
four planar corners, then advances `pl2` to the next `pl1`
(`src/main/07_panel_development.inc:300-355`). For normal panels, panel `i` lies
between spatial ribs `i` and `i+1` (`src/main/07_panel_development.inc:300-322`).
The first implementation tested the natural hypothesis that these corners form
two canonical continuous polylines. The stage-8 dual-run disproved that exact
invariant: on the higher-index edge, independently reconstructed
quadrilaterals can leave small numerical gaps between `pr2(:,j)` and
`pr1(:,j+1)`. Collapsing either endpoint changes the legacy contour length.
`neutral_panel_2d` therefore owns exact segment starts and ends, plus a
point-oriented view and measured maximum join gaps. A future authoritative
stage-7 algorithm may choose a continuous representation, but that would be an
approved numerical change rather than a storage-only refactor.

Index 499 was historically used as a hidden save location for the
intake/intrados junction. Retaining typed intrados panels across vent processing
removed that scratch state and its save/restore loops. Index 499 is now ordinary
segment capacity and can hold real neutral geometry when a 500-point topology
requires it; the downstream legacy pipeline still needs separate capacity
audits before arbitrary maximum-size inputs can be claimed as supported.

Stage 7 accesses both ribs using one loop topology and `np(i,*)`
(`src/main/07_panel_development.inc:300-322`, `368-388`). This proves the current
algorithm assumes corresponding point indices across adjacent profiles. Whether
all valid inputs guarantee identical topology is a question for Pere; until
answered, the adapter must reject or explicitly remap mismatched topology rather
than silently pairing different profile points.

### Production panel families: `uf/vf`, `ufe/vfe`, and peers

These arrays are declared at `src/main/declarations.inc:225-233`. They repeat
the slot convention of the main `u/v` arrays, mostly slots 9:12 and 14:25, but
their first two indices have been repacked for surface fragments and drawing
operations.

| Family | Confirmed role | Evidence/status |
|---|---|---|
| `uf/vf` | General-purpose assembled panel buffer passed to drawing, end-extension, arc, and mark procedures. | Populated from main slots 9:12 in several surface paths (`src/main/08_skin_tension.inc:1147-1154`, `1569-1576`, `2541-2548`). It is overwritten repeatedly and is not an authoritative model. |
| `ufe/vfe` | Extrados production-panel fragment. | Saved beside `uf/vf` in the extrados path (`src/main/08_skin_tension.inc:1156-1163`). |
| `ufv/vfv` | Intake/vent production-panel fragment. | Saved from the intake point range (`src/main/08_skin_tension.inc:1578-1585`). |
| `ufi/vfi` | Intrados production-panel fragment. | Saved from the intrados path (`src/main/08_skin_tension.inc:2550-2557`). |
| `ufa/vfa`, `ufb/vfb`, `ufc/vfc` | Translation/rotation and assembly scratch buffers. | A vent fragment is translated, rotated, translated again, and appended using these buffers (`src/main/08_skin_tension.inc:3510-3550`, `3791-3840`). Names `a/b/c` carry no domain meaning. |
| `uft/vft` | Temporary endpoint/transition splice buffer. | It selects points from extrados or vent fragments before reassembly (`src/main/08_skin_tension.inc:4312-4328`). Exact feature terminology needs confirmation. |
| `ufr/vfr` | Arc/reformatted drawing buffer. | Passed to mark and arc routines (`src/procedures/pattern_marks.inc:114-118`, `1070-1084`); ownership is feature-specific and should remain local. |
| slots 9/10 | Sewing or nominal production edges. | Drawing code emits them as the two panel sides (`src/procedures/panel_edges.inc:182-206`). |
| slots 11/12 | Allowance/cut edges. | Drawing code emits these with a different CAD color (`src/procedures/panel_edges.inc:200-206`). |
| slots 14/15 and 24/25 | End-extension construction and intersections/corners. | Defined by `extpoints` (`src/procedures/panel_edges.inc:1-13`, `34-35`). |
| slot 50 | metadata/count-like storage in a real coordinate array | Used by several assembly paths; must be audited before migration. It should not be a coordinate member. |

The target model should have one owned `production_panel_2d` with named edges
and optional vent/end features. Short-lived rotation and splice buffers should
be allocatable local variables. Drawing procedures should accept a read-only
panel plus a separate `layout_transform`, not a mutable full-size `uf/vf`
workspace.

### Procedure interfaces

Legacy procedure signatures expose the storage layout instead of the domain:

- `datair(i,rib,np,u,v)` receives the full rib matrix, count matrix, and all 99
  profile slots merely to load one profile (`src/procedures/profile_data.inc:1-17`).
- `xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)` receives full arrays although the
  calculation uses one rib and point (`src/procedures/profile_data.inc:320-370`).
- `panels3d` receives mutable full `rib/np/u/v/w` stores and documents several
  slots in prose (`src/procedures/geometry_3d.inc:1-30`).
- `tessella` likewise receives every slot and returns a four-dimensional array
  (`src/procedures/geometry_utilities.inc:350-365`).
- `dpanelc` and related procedures accept a mutable generic `uf/vf` store even
  though they draw one panel (`src/procedures/panel_edges.inc:182-206`).
- The color routine has `implicit none` and `intent`. It receives named profile
  topology plus the full legacy `u/v` stores at its compatibility boundary,
  but all numeric slot access is confined to `copy_legacy_production_panel`.

The newer free-form color geometry demonstrates the desired interface style:
private module state, `real64`, assumed-shape arrays, `intent`, finite checks,
and descriptive arguments (`src/leparagliding_color_geometry.f90:6-15`,
`38-52`). The data-model migration should extend this pattern while replacing
raw parallel arrays with domain objects.

## Proposed target model

The following types are conceptual names. Exact Fortran spelling may evolve,
but their ownership boundaries and invariants should not.

```fortran
type :: point_2d
  real(real64) :: u_cm, v_cm
end type

type :: segment_2d
  type(point_2d) :: start, finish
end type

type :: point_3d
  real(real64) :: x_cm, y_cm, z_cm
end type

type :: index_range
  integer :: first, last
end type

type :: profile_topology
  integer :: point_count
  type(index_range) :: extrados
  type(index_range) :: intake
  type(index_range) :: intrados
  integer :: leading_edge_index
end type

type :: normalized_profile_2d
  integer :: source_rib_index
  type(profile_topology) :: topology
  real(real64), allocatable :: chord_fraction(:)
  real(real64), allocatable :: height_fraction(:)
end type

type :: rib_definition
  integer :: source_number
  integer :: role
  real(real64) :: planform_station_cm
  real(real64) :: leading_edge_cm, trailing_edge_cm, chord_cm
  real(real64) :: spatial_station_cm, spatial_height_cm
  real(real64) :: washin_rad, rib_plane_angle_rad
  real(real64) :: washin_pivot_fraction
  real(real64) :: profile_rotation_z_rad, profile_rotation_pivot_fraction
  real(real64) :: profile_vertical_displacement_cm
  real(real64) :: intake_start_fraction, intake_end_fraction
  logical :: cell_open
  real(real64), allocatable :: anchor_fraction(:)
end type

type :: spatial_rib_3d
  integer :: rib_index
  integer :: role
  type(point_3d), allocatable :: skin(:)
end type

type :: neutral_panel_2d
  integer :: panel_index, lower_rib_index, higher_rib_index
  integer :: surface
  ! Inspection views only: the following segment start wins at a join.
  type(point_2d), allocatable :: lower_start_biased_view(:)
  type(point_2d), allocatable :: higher_start_biased_view(:)
  ! Exact segments are authoritative for measurements and migration checks.
  type(segment_2d), allocatable :: lower_index_segments(:)
  type(segment_2d), allocatable :: higher_index_segments(:)
  real(real64) :: maximum_lower_join_gap
  real(real64) :: maximum_higher_join_gap
  logical :: has_post_intake_support
  type(segment_2d) :: lower_post_intake_support
  type(segment_2d) :: higher_post_intake_support
end type

type :: production_panel_2d
  integer :: panel_index, lower_rib_index, higher_rib_index
  integer :: surface
  type(point_2d), allocatable :: sewing_edge_lower(:)
  type(point_2d), allocatable :: sewing_edge_higher(:)
  type(point_2d), allocatable :: cut_edge_lower(:)
  type(point_2d), allocatable :: cut_edge_higher(:)
  ! Named optional end, intake, and shaping features follow.
end type

type :: layout_transform_2d
  type(point_2d) :: translation
  real(real64) :: rotation_rad
  logical :: mirror_v
end type

type :: color_division
  integer :: boundary_id, panel_index, surface
  real(real64) :: lower_rib_chord_fraction
  real(real64) :: higher_rib_chord_fraction
end type
```

The first production-shaping types are now implemented in focused modules:

- `skin_tension_law` in `leparagliding_skin_tension`
- `shaped_panel_side_2d` in `leparagliding_panel_shaping`
- `boundary_length_match_control` in `leparagliding_panel_reformat`

Additional focused types should be introduced when their owning stage is
migrated:

- `seam_allowance` and `panel_end_extension`
- `anchor_definition`, `anchor_on_profile`, and `anchor_on_panel`
- `spatial_panel_surface` for the median/ballooned geometry
- `panel_side_metrics` for lengths and ratios now in `rib(:,30:45,190:201)`
- feature types for vents, miniribs, H/V ribs, rods, holes, and reinforcements
- `wing_model` as an owning aggregate after the components are stable

Use integer enums or small validated wrappers for `surface` and `rib_role`.
Do not put sheet coordinates, DXF layer/color, or output-unit numbers into
geometric types.

## Required invariants

These invariants should be executable validation procedures, not comments only.

### Profile topology

- Every coordinate is finite.
- Both normalized coordinate arrays have `point_count` elements.
- `point_count >= 2` and all ranges lie within `1:point_count`.
- Extrados/intake and intake/intrados share exactly one endpoint.
- `leading_edge_index` is in range and corresponds to the closest normalized
  sample to `(0,0)`, matching current stage-6 behavior.
- Chord fractions are finite and normally in `[0,1]`; signed height is allowed.
- An algorithm that pairs adjacent ribs by point index must first prove their
  topology is compatible or run an explicit remapping operation.

### Rib and spatial wing

- Physical chord is positive within a documented tolerance.
- Angles have one internal unit: radians.
- Input percentages are converted once to fractions.
- Every physical rib has one profile and one spatial skin with matching
  topology.
- Virtual ribs are explicit, validated entities with a source/reference rib;
  they are not inferred from array index alone.
- Spatial coordinates contain no sheet translation or drawing scale.

### Neutral development

- Panel adjacency is explicit: a panel names both source ribs.
- Both exact 2D segment arrays are finite and compatible with their source
  profiles; they are authoritative for lengths and comparisons.
- Consecutive legacy corners are not assumed to join. The exact
  `pl2(j)`--`pl1(j+1)` and `pr2(j)`--`pr1(j+1)` gaps are retained and their
  maxima reported. Start-biased point views exist for inspection only.
- Every reconstructed 2D quadrilateral preserves all available spatial edge
  and diagonal distances within an agreed tolerance.
- Extrados, intake, and intrados ranges retain their shared endpoints; no
  reserved array element such as 499 is part of the object.

### Production panels

- Sewing and cut edge pairs are finite and have explicit surface ownership.
- Shaping is applied to neutral geometry before seam allowances.
- Allowance input is in millimetres at the external interface and converted to
  centimetres once.
- Cut-edge distance from its sewing edge equals the configured allowance within
  tolerance, except at explicitly modeled corner treatments.
- Panel zero is allowed when the wing topology calls for the central panel.
- Geometry is layout-independent. Applying a `layout_transform_2d` must not
  mutate the stored panel.

### Color divisions and output

- A division is an open boundary with one chord fraction at each adjacent rib.
- Boundary identifiers are stable within a surface.
- Mapping is performed against normalized profiles and production sewing edges,
  never against sheet-translated coordinates.
- Each successful division produces one seam, two allowance edges, and paired
  marks with the configured inward offset.
- Existing non-color layer and CAD-color semantics remain unchanged.

## Migration strategy

The strategy is a strangler migration: typed objects are introduced alongside
the current arrays, consumers move to the types, producers dual-run and compare,
and only then is the legacy storage removed. Every phase must leave the main
program runnable.

### Phase 0 — Protect the numerical baseline

1. Freeze representative inputs and capture semantic output oracles for 3.28
   compatibility, 3.29 features, Swoop2 colors, odd/even central cells,
   open/closed cells, single-surface profiles, miniribs, and H/V/VH ribs.
2. Compare DXF by entities, layer, CAD color, topology, and coordinates with a
   tolerance; do not require byte order or handles to match.
3. Capture `lep-out.txt` and `lines.txt` semantically.
4. Run debug builds with bounds, undefined-value, floating-point, and backtrace
   checks where the compiler supports them.

Exit criterion: each following phase can demonstrate “no unapproved semantic
change” automatically.

### Phase 1 — Establish names and adapters without changing calculations

Implementation status: complete for profile topology, normalized/spatial ribs,
rib identities, neutral panels, production edges, and color divisions. Layout
transforms and the wider `rib_definition` remain future types because their
final axis/field vocabulary still needs confirmation.

1. Add the foundational point, range, topology, normalized-profile, spatial-rib,
   neutral-panel, production-panel, and layout-transform types in free-form
   modules using `real64` and `implicit none`.
2. Add named constants only for confirmed legacy slots and columns.
3. Implement checked copy adapters from legacy arrays to typed objects. Keep
   adapters in one module and forbid new direct numeric-slot access elsewhere.
4. Add validators for every invariant above, including panel zero and virtual
   ribs.
5. Add unit tests for adapters, invalid counts, non-finite coordinates, shared
   surface endpoints, and mismatched adjacent topology.

Exit criterion: typed snapshots of every profile, spatial rib, neutral panel,
and production panel compare exactly with their legacy source arrays.

### Phase 2 — Migrate read-only production consumers first

Implementation status: color construction uses typed production panels.
Stage-8 extrados, intake, and intrados length/width calculations dual-run
through exact typed neutral geometry, then assign the agreeing typed values as
authoritative. Lower-intrados `k31d=1` offsets and slots 9/11 now pass through a
typed Phase-4 authority boundary; remaining panel drawing, mark routines,
surfaces, sides, and special shaping paths still use legacy storage.

1. Change color construction to accept a typed normalized-profile pair and
   `production_panel_2d` rather than full `np/u/v` arrays.
2. Change panel drawing and mark routines to accept read-only named edges plus a
   `layout_transform_2d`.
3. Move panel-side length measurement into pure functions returning
   `panel_side_metrics`; stop using `rib(:,40:45)` as loop scratch.

This phase exercises the model in useful production code while producer
authority changes only at exact, checked compatibility boundaries.

Exit criterion: color entities, panel outlines, marks, and measured lengths
match the phase-0 oracles; the migrated consumers contain no numeric `u/v` slot
references.

### Phase 3 — Own neutral 2D development

Implementation status: complete at the real-panel neutral-authority boundary.
The pure shared quadrilateral developer, exact dual-run, and checked write-back
cover regular extrados, intake, and intrados geometry. Intake's post-surface
support is explicit; retained typed intrados panels survive that support's vent
consumers; physical terminal edges are derived from the final real panel; and
the point-499 scratch convention is gone.

1. Extract one pure `develop_panel_strip(spatial_left, spatial_right, topology)`
   routine from stage 7. **Complete through the shared engine for all three
   regular surfaces.**
2. Return exact lower/higher segment chains in `neutral_panel_2d`; decide
   explicitly whether closing measured legacy join gaps is an approved change.
   **Complete; exact starts/ends and measured gaps are retained without closing
   them.**
3. Dual-run it beside the legacy `pl*/pr*` calculation and compare every point
   plus the six source distances for every quadrilateral. **Complete for all
   real extrados, intake, and intrados panels, including intake support.**
4. Initially write the typed result back through an adapter so stage 8 and
   stage 16 continue unchanged. **Complete for every exact regular surface
   slice; intrados publication is intentionally deferred until after vents.**
5. Replace the special `xx/yy/zz` path with an explicit symmetry-virtual spatial
   rib. **Complete at the typed producer boundary; the old calculation remains
   only as the dual-run comparison oracle.**

Exit criterion: typed neutral development is authoritative, legacy `pl*/pr*`
is adapter output only, and no point-499 scratch convention remains in the
neutral-development pipeline.

Phase 4 now owns new-law lower-intrados regular panels and the separate physical
terminal intrados production edge through its exact-range `ndif=1000` length
match. Broader regular-row reformat, distortion, surface, side, vent, and
special-path migration follows.
Retiring the duplicate stage-7 comparison oracle can follow once downstream
boundaries have equivalent semantic regression coverage.

### Phase 4 — Own production panel shaping

Implementation status: the `k31d=1` lower-intrados side is typed-authoritative
for real panels `0:nribss-1`, including its final contour point. The distinct
physical terminal boundary at row `nribss` is also typed-authoritative through
the initial production handoff and the exact-range `ndif=1000` length match: it
owns intrados offsets and slots 9/11 without inventing a panel or terminal slots
10/12. Section-31 laws, shaped sewing/cut sides, terminal length matching, and
the preceding join support are owned by focused pure modules; stage 8 compares
them with the old calculations before publishing compatibility slots.
Regular-row reformats, distortion, and all other production surfaces/sides
remain to be migrated.

1. Extract `skin_tension_law` parsing and evaluation from slots 7/8.
   **Complete for Section-31 intrados laws and the lower side of every real
   panel.**
2. Implement one side-shaping routine parameterized by side and surface instead
   of separate copy/pasted extrados/intrados blocks. **The shared pure kernel is
   complete and integrated for the lower intrados regular-panel side and the
   separately typed terminal intrados boundary.**
3. Produce sewing and cut edges directly in `production_panel_2d`.
   **In progress: the first boundary produces validated
   `shaped_panel_side_2d` and publishes slots 9/11 transactionally; aggregation
into a complete production panel remains.**
4. Model vents, end extensions, and special reformat paths as named optional
   features; delete `ufa/ufb/ufc/uft` workspaces as each path migrates.
   **In progress: exact-range terminal intrados `ndif=1000` length matching and
   its separate preceding join support are typed and authoritative; regular
   rows and distortion remain.**
5. Keep a write-back adapter for remaining internal-rib consumers.

Exit criterion: stage 8 no longer writes slots 7:12 for migrated surfaces;
allowance and shaping invariants pass for all fixtures; production drawings are
semantically unchanged.

### Phase 5 — Own spatial profile construction

1. Parse section-1/2 values into `rib_definition` and
   `normalized_profile_2d`, converting units at the boundary.
2. Replace the chained numbered slots 3/4/5 and duplicated `xyzt` path with one
   explicit transformation from a normalized profile and rib definition to
   `spatial_rib_3d`.
3. Generate symmetry and extrapolated virtual ribs through named constructors.
4. Dual-run against `x/y/z` and compare each spatial point before stage 7.

Exit criterion: the typed spatial wing is authoritative; `x/y/z`, slots 3:5,
and `xx/yy/zz` are compatibility output only.

### Phase 6 — Migrate 3D shaping, anchors, and structural features

1. Replace slots 47:55 and 69:72 with `spatial_panel_surface` and local-frame
   objects.
2. Move anchors and intake singular points out of slot 6 and rib columns
   66:70/110:135 into focused types.
3. Migrate stage 16 one feature at a time: H/V/VH ribs, junctions, rods, holes,
   reinforcements, and miniribs. Each feature owns its input definition,
   calculated geometry, and output views.
4. Migrate suspension and brake points from slots 17:20 to the line model.

Exit criterion: no active calculation outside the compatibility module accepts
the raw `rib/np/u/v/w/pl*/pr*/uf*` stores.

### Phase 7 — Remove compatibility storage and includes

1. Replace `declarations.inc` with an owning `wing_model` plus run/output
   configuration.
2. Turn each numbered include into a module procedure with explicit typed input
   and output.
3. Remove unused columns, slots, magic point indices, and fixed maximum shapes.
4. Drop `-fallow-argument-mismatch` once all interfaces are explicit; make the
   warning build mandatory in CI.

Exit criterion: the legacy matrices are gone, no executable `.inc` file shares
mutable program scope, and the full regression matrix passes.

## Questions requiring Pere's confirmation

These are not blockers for adapters, but they are blockers for final public
names and for deleting the corresponding legacy representations.

1. What are the physical names and positive directions of `rib(:,2)`,
   `rib(:,3:4)`, `rib(:,6)`, `rib(:,7)` and final `x/y/z`? In particular, what
   does legacy `x'` mean geometrically?
2. Are centimetres the canonical internal length unit for every wing and panel
   coordinate? What exact promises do `xwf` and `xkf` make, and are any values
   deliberately scaled twice?
3. Is `rib(:,50)` a manufacturing displacement, an unloaded-profile descent,
   or another aerodynamic correction? What should columns 55 and 56 be called?
4. Does “left/right panel edge” mean lower/higher rib index, port/starboard, or
   the operator's view on the flattened sheet? May internal names use
   `lower_index_edge/higher_index_edge` until this is settled?
5. Must all neighboring profile files have identical point counts and matching
   extrados/intake/intrados indices? If not, what is the intended remapping rule
   before panel development?
6. The code now proves that rib 0 is a finite-center symmetry mirror or a
   collapsed-center alias, while `nribss+1` is extrapolated tip support.
   Declared odd/even parity is only a consistency diagnostic. Should a finite
   panel 0 always be exposed in manufacturing output, and should the collapsed
   alias ever appear outside calculations?
7. In manufacturing terminology, are slots 9/10 the sewing lines and slots
   11/12 the cut edges, or should they be named nominal edge/sewing allowance
   edge?
8. Does “fully flattened 2D wing” refer to the planform artwork reference, the
   neutral distance-preserving development, or the tensioned production panels?
   Current colors are authored in normalized chord space and mapped to slots
   9/10 after skin tension (`src/main/15_colors.inc:20-25`).
9. Which later `rib` columns are durable design properties and which are merely
   cached report values? Column 165 and the slot-71/72 feature are the highest
   priority unknowns because they cross multiple stages.
10. May the output ordering of DXF entities change when semantic content is
    equivalent, or do downstream CAD scripts depend on order as well as layers
    and colors?
11. Are the small `pr2(:,j)` to `pr1(:,j+1)` gaps in neutral development an
    accepted consequence of independent quadrilateral flattening, or should an
    authoritative stage-7 rewrite force a continuous higher-index edge?

## Definition of done

The broader data-model refactor is complete only when all of the following are
true:

- Domain types distinguish normalized profiles, spatial ribs, neutral
  developments, production panels, sheet layout, and structural features.
- Every public field and argument has a domain name, unit, coordinate frame,
  ownership rule, and validation invariant.
- Percentages and degrees are converted once at input; internal geometry uses
  `real64`, centimetres, fractions, and radians consistently.
- Virtual ribs and the central panel are explicit model entities.
- No production calculation directly indexes a numeric `rib` column, `np`
  column, or `u/v/w` slot. Numeric references exist only in a quarantined
  compatibility adapter, then disappear when that adapter is removed.
- No routine receives a full global array when it operates on one rib, panel,
  profile, or polyline. All module procedures use `implicit none`, explicit
  `intent`, and assumed-shape or domain-type arguments.
- Stage-local accumulators and transform buffers are local variables, not hidden
  columns or reserved point indices.
- Spatial and developed geometry never contains sheet placement offsets.
- Duplicate extrados/intrados and left/right algorithms have one parameterized
  implementation wherever their mathematics is the same.
- The regression suite covers 3.28 compatibility, all supported 3.29 features,
  Swoop2 colors, odd/even wings, profile-format variants, and structural options.
- Semantic DXF comparison proves layers, CAD colors, entity topology, and
  coordinates within an agreed tolerance; report and line outputs also have
  semantic oracles.
- Normal, warning, runtime-check, and bounds-check builds pass without NaN,
  infinity, out-of-bounds access, uninitialized state, or argument mismatch.
- Pere has approved the domain vocabulary and the answers to the questions
  above, and those answers are recorded next to the relevant types.

## First implementation slice

The first shippable slice—phases 0 and 1 plus the color portion of phase 2—is
complete. It has a deliberately narrow numerical surface:

1. Introduce validated `profile_topology`, `normalized_profile_2d`,
   `spatial_rib_3d`, and `production_panel_2d` snapshots.
2. Support physical and virtual rib roles and allow panel index zero.
3. Copy normalized slot 2, spatial `x/y/z`, and production slots 9:12 through
   checked adapters.
4. Pass typed profiles and production edges into the already robust color
   interpolation and construction code.
5. Add adapter and semantic-DXF tests before changing stage-6/7/8 producers.

That slice produced immediate naming and interface improvements in a real
manufacturing workflow while keeping the existing geometry calculations as the
comparison oracle for the deeper stages. The subsequent neutral-panel
checkpoint dual-ran stage-8 extrados metrics before making the agreeing typed
values authoritative.

The producer-authority checkpoints completed:

1. write the typed extrados result back through a checked adapter and make the
   pure developer authoritative for its exact regular extrados segment slice;
2. add a classic, non-single-surface (`k31d=0`) full-output fixture;
3. map disabled section-29 shaping to its one-based no-cut group and guard
   shaping-table lookups against invalid group indices; and
4. make typed intake contour segments and explicit post-intake support the
   final regular-row values after exact dual-run agreement; and
5. represent physical terminal surface comparison edges as the higher side of
   the last real panel, with no fabricated row-`nribss` panel; and
6. retain dual-compared typed intrados panels across vent processing, publish
   them after intake support is released, make their stage-8 metrics
   authoritative, and remove the magic point-499 save/restore and collision
   rule without modifying terminal row `nribss`; and
7. normalize Section-31 intrados laws, make their lower-side `k31d=1` offsets
   authoritative for every real panel and final contour point after comparison,
   then shape and publish the agreeing typed slot-9/11 contours; and
8. define `production_boundary_edge_2d`, seed the legacy terminal intrados
   neutral oracle from the final real panel, independently check every terminal
   distance/offset/coordinate, publish only row-`nribss` slots 9/11, and stop
   the old right-side call from fabricating slots 10/12; and
9. model the terminal intrados `ndif=1000` length match with explicit source and
   target lengths plus separate measurement/reconstruction indices, preserve
   its implicit single-precision scale, dual-compare every exact-range sewing
   point, and publish the agreeing sewing/cut pair transactionally; and
10. represent the preceding intake/intrados join support separately from the
    physical terminal surface, dual-compare its historical extrapolation, and
    publish its paired sewing/cut point transactionally.

The next slice should begin regular-row `ndif` length matching and five-pass
distortion correction.
Terminal extrados and the remaining regular sides/surfaces follow.
Later shaping-cut bounds exposed by the Chooca-15 preset still need a
mixed-profile full-output fixture.
