# LEparagliding 3.29 — maintainer guide

LEparagliding is Pere Casellas' fixed-input paraglider and parachute design
program. This repository now combines Pere's version 3.29 "Jardins" release
with the structural refactor and safety repairs previously applied to 3.28.

The upstream 3.29 implementation was compared against both the original 3.28
source and this repository's corrected 3.28 implementation. Its new behavior
was then integrated into the existing modules and include-file boundaries,
rather than replacing the repaired code wholesale.

## Version 3.29 support

This tree implements the additions described in Pere's 3.29 update notes:

- optional section 38, which configures holes in H/V ribs of types 5/15 and
  6/16 and in miniribs of type 9;
- optional section 39, which reads horizontal and vertical drawing-position
  factors for V-rib types 1 through 6 (types 1, 5, and 6 are active in 3.29);
- type 4 extrados and type 5 intrados longitudinal nylon rods;
- special codes 2005, 2006, and 3002, while retaining all earlier section-37
  codes;
- UTF-8 design text converted to DXF R12 `\U+XXXX` escapes; and
- separate, labelled nylon-rod columns in the generated notes table.

Inputs that end after section 37 remain valid, so existing 3.28 designs do not
need empty section-38 and section-39 blocks. When the new blocks are present,
the parser validates controls, row counts, supported types, and parameter
ranges before geometry generation.

The integration also repairs defects found in the upstream 3.29 paths:

- every section-37 code is retained instead of only the final row;
- the requested HVR hole row is retained, and active position types 1, 5, and
  6 use the intended factors;
- type 5 rod length is accumulated in the correct report column;
- type 5 seam construction cannot use zero or negative profile indices;
- ellipse output is one bounds-safe closed DXF polyline, with no duplicate line
  representation or write past the point buffer;
- minirib parity is preserved rather than calculated and immediately reset;
- the special-wingtip angle has one consistent radians convention;
- profile-derived tip geometry is created only after every source profile is
  loaded; and
- optional STL/XFLR5 directories are created correctly on Windows and POSIX
systems.

## Flattened-wing color divisions

The first automated color-construction workflow is now implemented for simple
open division lines drawn on a fully flattened 2D wing:

- [`tools/import_color_divisions.py`](tools/import_color_divisions.py) reads
  ASCII DXF `LINE`/`LWPOLYLINE` artwork without external packages and produces
  section 15 (extrados) or 16 (intrados) rib records;
- matching boundary IDs on consecutive ribs become straight internal sewing
  lines on the developed panel;
- both adjacent pieces receive the configured upper/lower seam allowance;
- paired matching marks are inset 1.1 mm from both internal cut edges; and
- new construction is isolated on `color_seams`, `color_allowance`, and
  `color_marks`, preserving all existing layers and CAD colors.

This also fixes the section-15/16 slope interpolation that produced NaNs in the
author's Swoop2 project. The complete reference changes from 12 NaN DXF
coordinates to zero and its ACI-6 artwork polyline imports all 26 known rib
values exactly. See [`COLORS.md`](COLORS.md) for the workflow, command example,
format contract, limitations, and next steps.

## Refactored foundation

The original 36,252-line 3.28 program placed the main calculation, procedures,
global mark state, parsing, geometry, and DXF writing in one fixed-form file.
The maintained structure now:

- splits the main calculation into 21 numbered files under `src/main/`;
- groups procedures by responsibility under `src/procedures/`;
- puts legacy procedures behind `leparagliding_procedures`, giving calls
  explicit compiler-checked interfaces;
- replaces the repeated `/markstypes/` COMMON block with a typed module;
- owns vector, plane, and rotation concepts in `leparagliding_geometry`;
- owns robust color-edge interpolation and seam offsets in
  `leparagliding_color_geometry`;
- separates named profile topology, central-panel-aware rib identities,
  normalized/spatial ribs, exact neutral-development segments, and flattened
  production panels in `leparagliding_domain_model`, with transactional checked
  legacy-array adapters;
- dual-runs stage-8 extrados lengths and widths through the typed neutral-panel
  model while the reviewed legacy calculation remains authoritative;
- parses new HVR settings into typed, initialized configuration objects in
  `leparagliding_hvr_config`; and
- keeps new module code in free-form Fortran with `implicit none` while the
  legacy numerical sequence remains fixed-form.

The historical `leparagliding3.28.f.zip` and
`build-baseline-src/leparagliding.f` sources remain comparison references.

## Safety repairs retained from 3.28

The earlier safety work remains in place. In particular, profile loading is
capacity-checked, reported negative/zero array-index paths are guarded, the
leading-edge sample is selected deterministically, optional results and
interpolation fallbacks are initialized, the fifth line level is reachable,
and V-rib coordinate buffers hold the 121 samples written by the algorithm.
The `.dat` profile path now rebuilds the contour when an intake boundary must be
inserted, records the actual output indices when a nearby source point is used,
and initializes the shared intake/intrados endpoint before any junction or
typed-topology consumer reads it. The old routine could leave `np(:,5)`
indeterminate and report the wrong index for a boundary moved to source point
`j+1`.

The 3.29 sample supplied by Pere and the repository tests complete with all GNU
Fortran runtime checks enabled. The legacy main program still produces
lower-priority warnings such as precision conversions, exact REAL comparisons,
unused variables, and old-style implicit typing. Address those incrementally:
changes in expression type or order can change numerical output.

## Build and test

Requirements:

- GNU Fortran (`gfortran`)
- CMake 3.20 or newer
- a build tool supported by CMake

Python 3 is optional. When available, CMake enables the DXF color-import and
semantic-DXF regression tests in addition to the Fortran tests.

GNU/Linux:

```sh
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build --parallel
ctest --test-dir build --output-on-failure
```

Windows with MinGW:

```powershell
cmake -S . -B build -G "MinGW Makefiles" -DCMAKE_BUILD_TYPE=Release
cmake --build build --parallel
ctest --test-dir build --output-on-failure
```

For a checked diagnostic build:

```sh
cmake -S . -B build-check -DCMAKE_BUILD_TYPE=Debug \
  -DLEPARAGLIDING_ENABLE_WARNINGS=ON \
  -DLEPARAGLIDING_RUNTIME_CHECKS=ON
cmake --build build-check --parallel
ctest --test-dir build-check --output-on-failure
```

Eight isolated tests are registered:

1. `domain_model` checks named profile partitions, odd/even and virtual rib
   roles, spatial and production snapshots, exact neutral segments, panel 0,
   hidden intake support/scratch semantics, the point-499 collision guard, and
   transactional adapters.
2. `profile_data` checks exact, shifted, and inserted `.dat` intake boundaries
   and verifies the rebuilt contour's topology identities.
3. `color_geometry` checks robust color-edge interpolation, repeated profile
   coordinates, both seam offsets, and the 1.1 mm inward mark calculation.
4. `dxf_semantic_diff` checks the dependency-free, tolerance-aware DXF geometry
   comparator (registered when Python 3 is available).
5. `color_division_import` compares DXF import with a Swoop2-derived section-16
   oracle (registered when Python 3 is available).
6. `plan_b_regression` compares all five principal outputs with the reviewed
   3.29 baseline, after normalizing line endings.
7. `profile_capacity_guard` verifies that an oversized 501-point profile is
   rejected before it can overrun the legacy arrays.
8. `version_329_features` exercises rod types 4 and 5, section-38 hole types,
   all new special codes, section-39 positioning, and UTF-8 DXF output; it also
   rejects NaN or infinity in the main DXF.

Test runs use copies below the build directory and do not overwrite the inputs
in `Plan B Parakite/`.

### Running another design

The program uses fixed filenames and resolves profile files relative to the
current working directory. A run directory must contain `leparagliding.txt`
and every profile file named by its section 2. Run in a copy or a separate
working directory because output files there are overwritten.

## Source map

```text
CMakeLists.txt
cmake/
  run_color_import_test.cmake      Swoop2-derived DXF import oracle
  run_plan_b_regression.cmake       complete-output regression
  run_profile_capacity_check.cmake oversized-profile rejection
  run_329_features.cmake           focused 3.29 feature coverage
src/
  leparagliding.f                   main program and ordered section includes
  leparagliding_color_geometry.f90  color-edge interpolation and seam offsets
  leparagliding_domain_model.f90    typed wing domains and legacy adapters
  leparagliding_hvr_config.f90      typed sections 38/39 parser and lookup
  leparagliding_procedures.f        explicit interface facade
  leparagliding_geometry.f90        vector, plane, rotation, transforms
  leparagliding_mark_types.f90      shared mark drawing configuration
  main/                             numbered main-program calculation stages
  procedures/                       procedures grouped by responsibility
tests/
  fixtures/                         minimal reference geometry
  expected/                         reviewed focused-test output
tools/
  dxf_semantic_diff.py              tolerant semantic DXF comparison
  import_color_divisions.py         open DXF division -> section 15/16
Plan B Parakite/                    regression input supplied by the designer
```

Important procedure groups:

| File | Responsibility |
|---|---|
| `dxf_output.inc` | DXF primitives, ellipses, UTF-8 text, start/end records |
| `color_construction.inc` | internal color seams, allowances, and inset marks |
| `geometry_2d.inc` | redistribution, intersections, flattening, HVR holes |
| `panel_edges.inc` | panel boundaries, arcs, vents, panel variants |
| `junctions.inc` | junction and longitudinal nylon-rod geometry |
| `offsets_reinforcements.inc` | offsets, straps, mylars, interpolation |
| `geometry_3d.inc` | 3D panels, planes, local/global geometry |
| `pattern_marks.inc` | print, alignment, and Romano marks |
| `profile_data.inc` | profile reading, remapping, coordinate transforms |
| `geometry_utilities.inc` | distances, arcs, interpolation, tessellation |
| `interpolation.inc` | equal-distance polyline interpolation |
| `file_cleanup.inc` | generated-file text post-processing |
| `transformations.inc` | 2D local-to-global transformation |

## Maintenance rules

1. Run `plan_b_regression` before and after each coherent numerical change.
2. Add new numerical code in focused free-form `.f90` modules with
   `implicit none`, explicit `intent`, and an explicit real kind.
3. Keep parsing, validation, and geometry separate.
4. Prefer the existing typed geometry objects over new unrelated coordinate
   helper conventions.
5. Migrate one legacy procedure at a time, with focused coverage before
   changing implicit types or argument declarations.
6. Do not reorder floating-point expressions as cosmetic cleanup; the output
   regression is intentionally sensitive to numerical drift.

The numbered main files remain includes deliberately. They preserve the
original scope and calculation order without introducing a huge argument list
or a new untested global-data object. New independent state should live in a
focused module or derived type rather than extending `declarations.inc`.

The coordinate domains and input/output contract of every ordered main include
are summarized in [`docs/geometry-pipeline.md`](docs/geometry-pipeline.md).
The complete schema inventory, staged migration, open terminology questions,
and exit criteria are in
[`docs/data-model-refactor.md`](docs/data-model-refactor.md).

## Further color work

Straight open divisions and their manufacturing construction are implemented.
Independent closed piece extraction, material assignment, multi-boundary
junctions, curved artwork, and optional arch compensation remain future work;
their deliberately 2D-first sequence is recorded in [`COLORS.md`](COLORS.md).
