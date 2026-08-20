# Automatic color-piece division — design research

This document records the proposed workflow for importing artwork and dividing
paraglider panels into pieces of different colors. It is a design note, not an
implemented feature. The workflow should be reviewed with Pere Casellas and
other designers before its data format or geometry rules are finalized.

## Repository checkpoint

Last reviewed on 2026-08-20 after the maintained codebase was updated to
LEparagliding 3.29. Version 3.29 adds HVR holes and positioning, nylon-rod
types 4 and 5, special codes 2005/2006/3002, and improved DXF R12 output. None
of those additions implements the automatic color-piece workflow proposed
here; this document remains future design research.

The current normal and runtime-check test suites cover the 3.29 baseline,
profile-capacity rejection, and the new 3.29 feature paths. A future color
implementation should extend those tests rather than replace their baselines.

## Goal

Allow a designer to place vector artwork on a wing using a familiar CAD
workflow, then let LEparagliding automatically:

1. project the artwork onto the selected upper or lower surface;
2. transfer every color boundary to the developed fabric panels;
3. split the affected panels into manufacturable colored pieces;
4. add seam allowances, matching marks, labels, and material information; and
5. generate cutting DXFs and a colored inflated-wing preview.

The first version should work with AutoCAD 2002 and should not require a new
interactive LEparagliding GUI.

## What the current program supports

The present implementation is a useful starting point but does not perform
automatic artwork division:

- `src/main/04_data_reading.inc` reads manually specified extrados and
  intrados color data from sections 15 and 16.
- The classic representation records color positions on selected ribs.
- The newer `-2` representation records straight panel cuts using positions on
  the two ribs bordering a panel.
- `src/main/05_graphic_design.inc` displays these marks or straight cuts on the
  planform.
- `src/main/15_colors.inc` converts classic rib positions into cross marks on
  developed panels.
- `src/procedures/dxf_output.inc` writes R12-compatible ASCII DXF primitives,
  legacy polylines, AutoCAD Color Index values, and UTF-8 text escapes.

There is currently no DXF reader, general polygon representation, region
topology, automatic panel splitting, or windowed user interface. Arbitrary
curved artwork should not be forced into the existing fixed-size
`npc*`/`xpac*` arrays.

## Proposed designer workflow

LEparagliding generates a dedicated `artwork-template.dxf`. The designer then:

1. opens the template in AutoCAD;
2. draws or imports artwork over the registered wing reference;
3. moves, scales, rotates, and mirrors the artwork as required;
4. places each fabric color on a separate named layer;
5. saves an ASCII DXF; and
6. asks LEparagliding to generate the colored pieces.

This uses AutoCAD as the placement interface. A dedicated GUI can be added
later without changing the projection and division engine.

## Reference geometry: a flat-span inflated wing

The recommended reference is neither a completely flat fabric outline nor the
fully arched flight shape. It is a virtual **flat-span inflated wing**:

- rib stations are arranged across the flat span with no spanwise arch;
- every rib retains its inflated airfoil section;
- upper and lower skins retain their chordwise curvature and cell shaping; and
- extrados and intrados are treated as separate projection targets.

This creates three distinct geometry domains:

| Domain | Purpose |
|---|---|
| Artwork plane | Where the designer positions the 2D DXF artwork. |
| Flat-span inflated surface | Where artwork is projected and associated with cells, ribs, and surface coordinates. |
| Developed fabric panels | Where color boundaries become cutting geometry, seams, and marks. |

The mapping is therefore:

```text
2D artwork
    -> orthogonal projection onto the flat-span inflated surface
    -> cell/panel surface coordinates
    -> developed fabric-panel coordinates
    -> separate colored cutting pieces
```

For the upper surface, projection travels from above toward the extrados. For
the lower surface, it travels from below toward the intrados. Top and bottom
must have separate registered design views and layer namespaces.

This reference accounts for chordwise inflation without making the designer
compensate for the final spanwise arch. Artwork designed this way may still look
slightly different on the fully arched flight shape, so the final output should
include a 3D preview.

Possible future projection modes are:

- flat fabric or span/chord mapping;
- flat-span inflated projection (recommended default); and
- fully inflated 3D projection from a selected viewing direction.

## `artwork-template.dxf`

The template should be a clean production input, not the existing pattern DXF
with all construction output mixed together. It should contain:

- upper and lower flat-span inflated references;
- leading and trailing edges;
- rib, cell, and relevant panel boundaries;
- wing centerline and orientation labels;
- three non-collinear registration markers with stable identifiers;
- an explicit calibration bar and declared working units; and
- instructions and example artwork layers.

Suggested reference layers:

```text
LEP_REFERENCE_TOP
LEP_REFERENCE_BOTTOM
LEP_REFERENCE_RIBS
LEP_REFERENCE_PANELS
LEP_REGISTRATION
LEP_NOTES
```

Reference layers are ignored as artwork during import. They should be locked by
default where the DXF format and CAD application allow it.

Three registration markers allow the importer to detect translation, rotation,
uniform scaling, and mirroring. They also prevent old, effectively unitless DXF
workflows from silently manufacturing a wrongly scaled wing. A mismatched wing
identity or non-uniformly transformed reference should be rejected with a clear
diagnostic.

## DXF compatibility

The initial importer should accept ASCII DXF saved in AutoCAD R12 or AutoCAD
2000 format, both suitable for an AutoCAD 2002 workflow. Direct DWG import is
out of scope.

Recommended first entity subset:

- `LINE`;
- legacy `POLYLINE`/`VERTEX`;
- `LWPOLYLINE`;
- `ARC`;
- `CIRCLE`; and
- closed polylines defining colored regions.

Arcs and circles can be tessellated to an accuracy specified in physical units.
Blocks, inserts, splines, hatches, text outlines, dimensions, and raster images
can be considered later. Unsupported entities must be reported rather than
silently discarded.

Closed regions are preferable because they identify both the boundary and the
area belonging to a color. Open division lines do not state which color lies on
each side; supporting them would require region seed points or another explicit
side-assignment rule.

## Color and material identification

The importer can read AutoCAD Color Index values, including entity colors and
colors inherited from layers. However, CAD display color alone should not be
the authoritative manufacturing identity.

The recommended convention is one named layer per fabric color or material:

```text
TOP_RED
TOP_WHITE
TOP_BLUE
BOTTOM_RED
BOTTOM_WHITE
LOGO_BLACK
```

Layer names provide stable semantic identifiers. AutoCAD colors provide useful
preview defaults. A later GUI or configuration section can map imported layers
to actual cloth names, supplier codes, weights, and display colors.

The importer will also need explicit rules for:

- the base color outside all artwork regions;
- overlapping regions and drawing priority;
- nested regions and holes;
- coincident or nearly coincident boundaries;
- minimum manufacturable piece size; and
- whether identical colors separated only by construction geometry are merged.

## Projection and division algorithm

A robust implementation should proceed as follows:

1. Parse the supported DXF entities without modifying wing geometry.
2. Resolve layers, colors, closed regions, and curve tessellation.
3. Verify the three registration markers, wing identity, units, orientation,
   scale, and top/bottom assignment.
4. Build triangulated upper and lower flat-span inflated reference surfaces.
5. Project artwork curves onto the selected surface.
6. Locate each projected point in a surface triangle and retain its cell,
   panel, and barycentric or equivalent surface coordinates.
7. Clip every colored region against the applicable cell and panel domains.
8. Map clipped boundaries through the existing panel-development geometry.
9. Construct planar region topology for each developed panel.
10. Split the original panel into separate colored pieces.
11. Offset internal color boundaries according to the selected seam rule.
12. Add matching marks, piece labels, orientation, side, color, and material.
13. Export manufacturing DXFs and a colored 3D preview.

Projection and panel development should use an explicit surface mapping. A
single global flat projection is not sufficient because an inflated skin is not
globally developable without distortion.

## Seam and manufacturing rules

Automatic geometric division is only useful if the generated pieces express
the intended assembly method. Before implementation, the author should confirm:

- whether a color boundary is the finished seam centerline or one piece edge;
- whether both neighboring pieces receive an allowance;
- the default and per-material allowance widths;
- allowance behavior at panel edges and multi-color junctions;
- matching-mark type, spacing, and numbering;
- minimum corner radius and minimum strip width;
- preferred labels and layer names in the cutting DXF; and
- whether pieces of the same color should be grouped for nesting.

These rules should be data, not hard-coded drawing calls.

## Software architecture

The geometry feature should be implemented as a deterministic, headless engine
first. AutoCAD remains the initial placement interface.

New code should use focused free-form Fortran modules and derived types for:

- DXF artwork entities;
- named colors and materials;
- polygon rings and region topology;
- reference-surface triangles and mappings;
- projected artwork curves;
- divided panel pieces; and
- seam and marking policies.

The feature should not add more unrelated arrays to
`src/main/declarations.inc`. File parsing, geometric projection, region
operations, panel mapping, manufacturing policy, and DXF output should remain
separate stages.

If an interactive application is added later, it should be a separate frontend
over this engine. Artwork movement can then be rendered immediately without
rerunning Fortran for every mouse movement; the calculation engine runs when
the user requests validation, division, or a rebuilt 3D preview.

## Proposed outputs

The feature may produce:

- `artwork-template.dxf` — registered CAD placement template;
- `colored-panels.dxf` — complete divided cutting layout;
- optional per-color or per-material DXFs;
- a division report listing pieces, colors, areas, and warnings; and
- a colored 3D preview in a suitable mesh or CAD format.

Output should remain compatible with the author's existing AutoCAD 2002
workflow unless a newer format is explicitly selected.

## Validation and tests

The implementation needs focused tests in addition to the existing Plan B and
3.29 feature regressions:

- DXF registration with unchanged, translated, rotated, scaled, and mirrored
  templates;
- rejection of non-uniform scaling and mismatched wing templates;
- top-only, bottom-only, and combined artwork;
- one straight boundary across several cells;
- curved, nested, and holed regions;
- a boundary passing exactly through a rib or panel vertex;
- multi-color junctions and extremely small pieces;
- correct seam allowance on both adjacent pieces;
- round-trip stability of generated templates; and
- preservation of all non-color Plan B outputs.

A real artwork DXF supplied by Pere should become the primary end-to-end color
regression case.

## Questions for Pere and other designers

The following decisions should be confirmed before coding:

1. Does the flat-span inflated reference match the way artwork is normally
   designed?
2. Are designs normally expressed as closed colored shapes or open division
   lines?
3. Should manufacturing identity come from layer names, AutoCAD colors, or
   both?
4. What are the exact seam-allowance and matching-mark rules at internal color
   boundaries?
5. How should overlaps, holes, tiny pieces, and multi-color junctions be
   handled?
6. Should artwork be authoritative on the flat-span reference, or should a
   future option compensate for appearance on the fully arched wing?
7. Can one or more existing color-design DXFs and their finished cutting files
   be provided as reference cases?

## Suggested implementation sequence

After the workflow is approved:

1. obtain representative artwork and manufacturing examples;
2. specify the template, layer, registration, unit, and seam conventions;
3. generate the flat-span inflated reference mesh and template DXF;
4. implement and test the restricted ASCII DXF importer;
5. implement projection and panel-coordinate mapping;
6. implement polygon clipping, region topology, and panel splitting;
7. implement allowances, marks, labels, reports, and cutting output;
8. add the colored inflated-wing preview; and
9. evaluate whether a dedicated interactive frontend is still needed.
