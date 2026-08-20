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

Three isolated end-to-end tests are registered:

1. `plan_b_regression` compares all five principal outputs with the reviewed
   3.29 baseline, after normalizing line endings.
2. `profile_capacity_guard` verifies that an oversized 501-point profile is
   rejected before it can overrun the legacy arrays.
3. `version_329_features` exercises rod types 4 and 5, section-38 hole types,
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
  run_plan_b_regression.cmake       complete-output regression
  run_profile_capacity_check.cmake oversized-profile rejection
  run_329_features.cmake           focused 3.29 feature coverage
src/
  leparagliding.f                   main program and ordered section includes
  leparagliding_hvr_config.f90      typed sections 38/39 parser and lookup
  leparagliding_procedures.f        explicit interface facade
  leparagliding_geometry.f90        vector, plane, rotation, transforms
  leparagliding_mark_types.f90      shared mark drawing configuration
  main/                             numbered main-program calculation stages
  procedures/                       procedures grouped by responsibility
Plan B Parakite/                    regression input supplied by the designer
```

Important procedure groups:

| File | Responsibility |
|---|---|
| `dxf_output.inc` | DXF primitives, ellipses, UTF-8 text, start/end records |
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

## Future design work

The proposed AutoCAD-based workflow for automatic color-piece division is
recorded in [`COLORS.md`](COLORS.md). It is research for a future feature, not
part of the implemented 3.29 update.
