# LEparagliding 3.28 — maintainer guide

LEparagliding is Pere Casellas' fixed-input paraglider and parachute design
program. This repository contains a structural refactor of version 3.28,
"Jardins", followed by targeted repairs to the unsafe legacy paths found by
compiler diagnostics and runtime checks.

This pass deliberately does **not** implement the new designer features yet.
Its purpose is to make those changes safer: automatic color-piece division,
profile resampling and `.dat` support, consistency checks, and data
autocompletion are the next layer of work.

## What changed

The original 36,252-line `leparagliding.f` compiled and ran, but the main
program, all legacy procedures, global mark state, input parsing, geometry, and
DXF writing occupied one file. Procedure calls had no explicit interfaces, and
one COMMON block repeated the same declarations in nine places.

The initial refactor made these changes without changing the Plan B wing
result:

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

The subsequent safety pass:

- makes profile loading capacity-safe and rejects profiles over 500 points with
  a clear diagnostic instead of writing past an array;
- removes the identified negative and zero index paths in profile, panel,
  mark, rib, and 3D geometry calculations;
- chooses the leading-edge sample nearest the profile origin, so a valid
  profile no longer needs an exact floating-point `(0, 0)` point;
- initializes optional-section results and interpolation fallbacks before use;
- restores the reachable fifth line level and sizes the V-rib coordinate
  buffers for the 121 samples the algorithm writes; and
- repairs the incomplete `arc3parc` compatibility routine and legacy spelling
  mistakes that selected the wrong variables.

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

The five files match the reviewed 3.28 safety baseline. They are not all
byte-for-byte identical to the untouched program: replacing invalid-memory
behavior and selecting a deterministic leading-edge point intentionally changes
parts of `leparagliding.dxf` and `lep-out.txt`. The test normalizes line endings
before hashing so the same oracle can be used on GNU/Linux.

A second test, `profile_capacity_guard`, builds a synthetic 501-point profile
from the Plan B case and verifies that it is rejected safely.

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
  run_plan_b_regression.cmake    isolated end-to-end output test
  run_profile_capacity_check.cmake  oversized-profile rejection test
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

## Procedure documentation

Every procedure is documented where it is defined: 89 legacy subroutines and
six typed geometry functions. Modules, public derived types, their components,
and the main program also have summaries. The comments use Doxygen/FORD-style
Fortran markup:

```fortran
!> One-sentence purpose and any important side effects.
!! @param[in] input Meaning, units, valid range, or indexing convention.
!! @param[out] output Meaning of the returned value.
!! @param[in,out] state What is read and what is changed.
!! @return Function result and its interpretation.
!! @note Preconditions or legacy behavior that callers must understand.
```

Each formal argument has its own `@param` entry, including compatibility
arguments that the current implementation does not use. For legacy routines,
the documented `[in]`, `[out]`, and `[in,out]` directions describe observed
behavior; they are not yet compiler-enforced `intent` declarations. Add those
declarations only while migrating a routine with a focused regression test.

Keep documentation next to the implementation and update it in the same change
whenever a signature, coordinate-slot convention, output unit, or side effect
changes.

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

## Legacy safety status

The high-priority risks formerly listed here are resolved. In particular,
profile capacity is checked, all reported `j-1`/`j-2` and rib `-1` paths are
guarded, the leading-edge index is always valid, and the named uninitialized or
misspelled values have been repaired. A warning-enabled Release build now
reports no possibly-uninitialized values and no statically provable array-bound
violations. The full Plan B case also completes with all GNU Fortran runtime
checks enabled.

The legacy main program still produces many lower-priority warnings, chiefly
implicit single-precision-to-double-precision conversions, exact REAL
comparisons, unused variables, and old-style implicit typing. These are cleanup
work rather than known memory-safety failures. Address them incrementally with
focused tests because changing expression types or order can change numerical
output.

Run the same diagnostic configuration with:

```sh
cmake -S . -B build-check -DCMAKE_BUILD_TYPE=Release \
  -DLEPARAGLIDING_ENABLE_WARNINGS=ON \
  -DLEPARAGLIDING_RUNTIME_CHECKS=ON
cmake --build build-check --parallel
ctest --test-dir build-check --output-on-failure
```
