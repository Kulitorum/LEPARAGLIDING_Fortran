# Geometry pipeline and include-file contracts

The numbered `.inc` files are ordered stages of one legacy main program. They
share state declared in `src/main/declarations.inc`, so their order is part of
the numerical model rather than a formatting choice. Each main include now has
a contract header describing what it consumes and produces; procedures under
`src/procedures/` document their parameters at the routine definition.

## Coordinate domains

Several arrays reuse short historical names. The domain is determined by the
array and its slot, not by the variable letter alone:

| State | Meaning |
|---|---|
| `rib(i,*)` | Semi-wing rib parameters: station, LE/TE, chord, arch, angles, anchors, and derived lengths |
| `u/v(i,j,1)` | Original normalized profile coordinates |
| `u/v(i,j,2)` | Profile coordinates normalized to percentages |
| `x/y/z(i,j)` | Absolute spatial skin coordinates after scale, arch, wash-in, and rotations |
| `wing_spatial_ribs(i)` | Validated, independently owned snapshot of physical spatial rib `i` |
| `pl*/pr*(panel,j)` | Neutral 2D development of the four quadrilateral corners |
| `neutral_extrados_panel` | Exact typed extrados segments for the current regular panel; after comparison these are written to the owned legacy extrados slice |
| `u/v(panel,j,9)` | Tensioned/developed left skin edge |
| `u/v(panel,j,10)` | Tensioned/developed right skin edge |
| `u/v(panel,j,11:12)` | Left/right sewing-border coordinates |
| `seppix(panel)` | Horizontal placement of a developed panel in the output sheet |

The most important distinction is between `x/y/z`, which is spatial wing
geometry, and developed `u/v` slots 9--12, which are manufacturing geometry.
A color division is authored against the flattened design reference, stored as
a chord percentage at ribs, then mapped onto developed `u/v` panel edges.

## Ordered main stages

| Include | Contract |
|---|---|
| `declarations.inc` | Declares shared schema and documents legacy array slots |
| `03_initialization.inc` | Initializes defaults, run state, and fixed I/O units |
| `04_data_reading.inc` | Parses sections 1--39, validates options, scales geometry, derives placement |
| `05_graphic_design.inc` | Draws planform/vault references and design annotations |
| `06_airfoil_geometry.inc` | Loads profiles, constructs normalized plus absolute 3D geometry, then snapshots physical spatial ribs |
| `07_panel_development.inc` | Flattens consecutive 3D quadrilaterals, exactly compares the legacy and pure typed regular-extrados results, then publishes the typed segments through a checked adapter |
| `08_skin_tension.inc` | Makes agreeing typed regular-extrados metrics authoritative, then applies legacy shaping and creates edges, borders, vents, and layouts |
| `09_singular_rib_points.inc` | Resolves anchors, intake limits, and named construction points |
| `10_calage.inc` | Calculates aerodynamic reference angles and balance geometry |
| `11_panel_lengths.inc` | Measures corresponding sides and places assembly marks |
| `12_lines.inc` | Expands suspension topology and calculates line geometry |
| `14_brakes.inc` | Expands brake topology and distribution |
| `15_colors.inc` | Adds internal color seams, allowances, and inset registration marks |
| `16_internal_ribs.inc` | Builds H/V/VH parts, junctions, rods, holes, and reinforcements |
| `17_equilibrium.inc` | Optionally solves force/moment equilibrium |
| `18_text_output.inc` | Writes the detailed design report |
| `19_lines_output.inc` | Writes the line schedule |
| `20_line_labels.inc` | Labels the completed 2D line drawing |
| `21_3d_dxf.inc` | Writes spatial DXF geometry and surface output |
| `22_notes.inc` | Adds title blocks, legends, and calculated notes |
| `23_finish.inc` | Terminates files, closes units, and reports deliverables |

## Panel and color flow

```text
section 1 rib planform + section 2 profiles
  -> normalized profile u/v
  -> absolute spatial x/y/z
  -> validated spatial_rib_geometry_3d snapshots
  -> quadrilateral distances between ribs
  -> pure typed extrados development compared with neutral pl*/pr* coordinates
  -> checked write-back of typed exact segments to the owned extrados slice
  -> typed-authoritative extrados length/width metrics
  -> tensioned left/right u/v slots 9/10
  -> outer sewing/cutting geometry in slots 11/12 and panel routines
  -> section 15/16 chord percentages
  -> internal color seams, allowances, and marks
```

The color stage intentionally runs after skin tension. Moving it earlier would
map artwork onto the neutral strip instead of the actual production panel.

The typed authority boundary currently ends at those exact regular-extrados
segments and their agreeing metrics. Stage-8 intake/vent support at
`j=np(i,2)`, intrados geometry, point 499 used as legacy scratch, and the
dummy/tip-support row remain legacy-owned; the adapter does not synthesize a
typed panel for them.

When section 29 shaping is disabled, data reading selects its declared
one-based no-cut group, validates that every rib has a declared group, and
zero-initializes no-cut influence values. The stage-6 mapper and
zone-of-influence routine also validate group bounds before indexing the legacy
shaping tables. A derived gnuA3 full run covers this disabled mode. Separately,
the authored gnuA3 classic-tension (`k31d=0`) regression exercises the
non-single-surface stage-8 path with shaping enabled.

## Naming rule for new work

New module code uses descriptive names such as `boundary_percent`,
`panel_offset_x`, and `mark_inset_mm`. Legacy loop letters remain where changing
them would create a broad numerical diff, but touched logic should stop reusing
one letter for rib, panel, point, and record indices. New independent state
belongs in focused free-form modules with `implicit none`, explicit `intent`,
and an explicit real kind.
