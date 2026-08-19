# LEparagliding 3.28 — maintainer guide

LEparagliding is Pere Casellas' fixed-input paraglider and parachute design
program. This repository contains a behavior-preserving structural refactor of
version 3.28, "Jardins".

This pass deliberately does **not** implement the new designer features yet.
Its purpose is to make those changes safer: automatic color-piece division,
profile resampling and `.dat` support, consistency checks, and data
autocompletion are the next layer of work.

## What changed

The original 36,252-line `leparagliding.f` compiled and ran, but the main
program, all 84 procedures, global mark state, input parsing, geometry, and DXF
writing occupied one file. Procedure calls had no explicit interfaces, and one
COMMON block repeated the same declarations in nine places.

The refactor made these changes without changing the Plan B wing result:

- Split the main calculation into 21 files named after the existing numbered
  sections under `src/main/`.
- Split the procedures into 12 subject-based files under `src/procedures/`.
- Put the legacy procedures behind the `leparagliding_procedures` module. The
  compiler can now check every procedure call. With the diagnostic flags used
  during this work, implicit-interface warnings fell from 1,793 to zero.
- Replaced `/markstypes/`, the only COMMON block, with the typed
  `leparagliding_mark_types` module.
- Added the modern `leparagliding_geometry` module. It owns the 2D/3D vector,
  plane, and rotation concepts used by the migrated geometry helpers.
- Consolidated repeated POINT, CIRCLE, 3D LINE, and TEXT DXF writers behind
  private helpers.
- Added explicit `implicit none` declarations to the new modules and migrated
  helpers. The remaining legacy sections retain implicit typing until each can
  be migrated with a focused regression test.
- Normalized source text to UTF-8 while retaining fixed-form Fortran for the
  legacy calculation.
- Added a CMake build and an end-to-end regression test using the full Plan B
  Parakite wing.

The original source archive, `leparagliding3.28.f.zip`, remains in the root as
the historical reference.

## Build and test

Requirements:

- GNU Fortran (`gfortran`)
- CMake 3.20 or newer
- A build tool supported by CMake

On GNU/Linux:

```sh
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build --parallel
ctest --test-dir build --output-on-failure
```

On Windows with MinGW:

```powershell
cmake -S . -B build -G "MinGW Makefiles" -DCMAKE_BUILD_TYPE=Release
cmake --build build --parallel
ctest --test-dir build --output-on-failure
```

The test runs in the build directory. It never overwrites files in
`Plan B Parakite/`. It checks the program's completion message and the complete
normalized contents of:

- `leparagliding.dxf`
- `lep-3d.dxf`
- `lep-out.txt`
- `lines.txt`
- `run-log.txt`

All five files match the output of the untouched 3.28 source byte-for-byte on
the Windows/GNU Fortran baseline. The test normalizes line endings before
hashing so the same oracle can be used on GNU/Linux.

### Running another design

The program uses fixed filenames and resolves profile files relative to its
current working directory. A run directory must contain:

- the design as `leparagliding.txt`;
- every profile file named by section 2 of that design.

Run in a copy or a separate working directory. The five output files listed
above are created or overwritten in that directory.

The safest example is the regression test itself:

```powershell
ctest --test-dir build --output-on-failure
```

Its isolated files are left under `build/tests/plan-b-regression/` for
inspection.

## Source map

```text
CMakeLists.txt
cmake/
  run_plan_b_regression.cmake    isolated end-to-end test
src/
  leparagliding.f                named program and ordered section includes
  leparagliding_procedures.f     explicit interface facade for legacy routines
  leparagliding_geometry.f90     vector, plane, rotation, and transforms
  leparagliding_mark_types.f90   shared mark drawing configuration
  main/                          numbered main-program calculation sections
  procedures/                    procedures grouped by responsibility
Plan B Parakite/                 complete regression input supplied by designer
```

The important procedure groups are:

| File | Responsibility |
|---|---|
| `dxf_output.inc` | DXF primitives, marks, text, and DXF start/end records |
| `geometry_2d.inc` | redistribution, intersections, flattening, axes, angles |
| `panel_edges.inc` | panel boundaries, arcs, vents, and panel variants |
| `junctions.inc` | junction geometry and junction pattern generation |
| `offsets_reinforcements.inc` | offsets, straps, mylars, interpolation |
| `geometry_3d.inc` | 3D panels, planes, local/global geometry |
| `pattern_marks.inc` | print marks, alignment marks, and Romano marks |
| `profile_data.inc` | profile file reading, remapping, coordinate transforms |
| `geometry_utilities.inc` | distances, arcs, polyline interpolation, tessellation |
| `interpolation.inc` | equal-distance polyline interpolation |
| `file_cleanup.inc` | post-processing of NaN text in generated files |
| `transformations.inc` | 2D local-to-global transformation |

### Why the numbered main files are includes

The original main program has hundreds of local scalars and large arrays shared
by all calculation sections. Immediately turning every section into a
subroutine would require a huge argument list or an untested global data object,
either of which could change implicit types, array layout, or numerical
evaluation order.

The numbered include files are therefore an intentional intermediate boundary:
they preserve the exact scope and calculation order while making each design
stage independently findable. New code should not add more unrelated state to
`declarations.inc`. Prefer a focused module or a derived type with a clear
owner.

## Where the requested features belong

| Requested feature | Primary extension points | Notes |
|---|---|---|
| Automatic division of colored pieces | `main/15_colors.inc`, section 15 parsing in `main/04_data_reading.inc`, panel generation in `main/07_panel_development.inc` and `main/08_skin_tension.inc` | Define one color-boundary representation before changing panel output. |
| Smooth profile point increase/reduction | `procedures/interpolation.inc`, `procedures/profile_data.inc` | Build and unit-test a profile resampler here; do not embed another interpolation loop in input parsing. |
| Optional `.dat` profiles | `procedures/profile_data.inc`, profile-name parsing in `main/04_data_reading.inc` | Add format detection and normalize every format into one internal profile representation. |
| Cross-section consistency checks | after `main/04_data_reading.inc` and before `main/05_graphic_design.inc` | Checks should report section and field names, then stop before geometry generation on invalid data. |
| Data autocompletion | a new input/model module called from the same post-read boundary | Keep defaults separate from validation so an author can see which values were supplied and which were inferred. |

## Rules for safe follow-up changes

1. Run `plan_b_regression` before editing and after every coherent change.
2. Put new numerical code in free-form `.f90` modules with `implicit none`,
   explicit `intent`, and `real(real64)`.
3. Keep file parsing separate from geometry. Parse into one representation,
   validate it, then calculate.
4. Prefer `vector_2d`, `vector_3d`, `plane_3d`, and `rotation_2d` over adding
   another set of unrelated `x1/y1/z1` helpers.
5. Migrate one legacy procedure at a time. Add declarations and tests before
   removing its implicit typing.
6. Do not “clean up” floating-point expression order casually. The exact-output
   regression is intentionally sensitive to numerical drift.

## Known legacy risks still to resolve

The structural refactor exposes problems; it does not guess at fixes that could
silently alter a wing. A diagnostic GNU Fortran build still reports 1,438
warnings, mostly implicit single-precision temporaries/conversions and direct
REAL equality comparisons. It also identifies these higher-priority defects:

- Some profile arrays are declared for 500 points while `datair` can read up to
  1,000 (`src/procedures/profile_data.inc`).
- Several loops access `j-1` or `j-2` while their arrays have a lower bound of
  1 (`pattern_marks.inc` and `geometry_3d.inc`).
- Section 8 contains paths that can access rib index `-1` even though the lower
  bound is `0`.
- A runtime-checked Plan B build stops when `np(i,6)` is zero and is used as the
  second index of `u`, whose declared lower bound is `1`, in
  `main/06_airfoil_geometry.inc`.
- The compiler reports several possibly uninitialized legacy values, including
  `dyy`, `ii`, `len1`, `len2`, `nployr`, `xlen`, and `xlenco`.

The normal legacy-compatible build completes Plan B because bounds checking was
not historically enabled. These issues should be fixed with targeted test cases
for the affected design options, not by globally changing array bounds: changing
an explicit-shape bound also changes array strides and can alter every caller.

For investigation, GNU Fortran runtime checks can be enabled with:

```sh
cmake -S . -B build-check -DLEPARAGLIDING_RUNTIME_CHECKS=ON
cmake --build build-check --parallel
```

At present, the Plan B regression is expected to expose the known bound error
under that diagnostic configuration. Use the normal Release configuration for
the behavior-preserving baseline.
