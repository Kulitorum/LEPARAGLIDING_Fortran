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
| `neutral_intake_panel` | Exact typed intake segments plus explicit post-intake support for the current regular panel |
| `neutral_intrados_panels(0:nribss-1)` | Retained exact typed intrados panels, developed before intake overwrites the shared support index and checked-written after vent processing |
| `terminal_extrados_edge`, `terminal_intake_edge`, `terminal_intrados_edge` | Physical wingtip comparison edges derived from the higher side of the final real panel; these are boundaries, not fabricated panels |
| `typed_intrados_tension_laws(0:nribss)` | Validated Section-31 intrados boundary laws, normalized into increasing developed-contour direction |
| `u(panel,j,7)`, `v(panel,j,7)` | Intrados developed-contour distance and its `k31d=1` skin-tension offset |
| `typed_intrados_lower_side` | Sewing and cut contours shaped from the exact lower side of the current real intrados panel |
| `typed_terminal_intrados_offsets` | One typed skin-tension offset for each point on the physical terminal intrados contour |
| `typed_terminal_intrados_production_edge` | Physical wingtip sewing/cut boundary; owns row-`nribss` slots 9/11 only and cannot contain slots 10/12 |
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
| `04_data_reading.inc` | Parses sections 1--39, validates options, scales geometry, derives placement, and normalizes Section-31 intrados laws |
| `05_graphic_design.inc` | Draws planform/vault references and design annotations |
| `06_airfoil_geometry.inc` | Loads profiles, constructs normalized plus absolute 3D geometry, then snapshots physical spatial ribs |
| `07_panel_development.inc` | Flattens consecutive 3D quadrilaterals, exactly compares legacy and pure typed regular extrados/intake/intrados results, publishes extrados/intake, and retains intrados across vent processing |
| `08_skin_tension.inc` | Makes agreeing typed surface metrics authoritative, processes vents, checked-writes retained intrados, and makes typed `k31d=1` lower-intrados offsets plus sewing/cut contours authoritative for regular panels and the physical terminal boundary before continuing legacy edges, borders, and layouts |
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
  -> typed-authoritative extrados length/width metrics, including the final
     real panel's higher edge as the physical wingtip comparison boundary
  -> pure typed intake development plus explicit post-intake support
  -> checked intake write-back and typed-authoritative intake lengths, with the
     same explicit physical wingtip boundary
  -> pure typed intrados development and exact dual comparison before intake
     support overwrites the shared legacy index
  -> retained typed intrados panels and typed-authoritative length/width metrics
  -> vent processing while the intake support remains published
  -> checked intrados write-back after vents
  -> normalized Section-31 law evaluation for each lower-intrados contour point
  -> typed side shaping from exact neutral segments, compared with legacy shaping
  -> checked write-back of regular-panel offsets and slot-9/11 sewing/cut edges
  -> separately checked physical-terminal offsets and slot-9/11 boundary
  -> tensioned left/right u/v slots 9/10
  -> outer sewing/cutting geometry in slots 11/12 and panel routines
  -> section 15/16 chord percentages
  -> internal color seams, allowances, and marks
```

The color stage intentionally runs after skin tension. Moving it earlier would
map artwork onto the neutral strip instead of the actual production panel.

The typed authority boundary covers exact extrados, intake, and intrados
segments for real panels `0:nribss-1`, the explicitly separated post-intake
support, and their agreeing stage-8 length/width metrics. Intrados panels are
retained until the vent consumers release that support, then published directly
to their real segment indices. There is no point-499 save/restore or topology
collision rule: a 500-point topology may legitimately store its final segment
at index 499, as covered by the domain-model unit test. This is a neutral-model
and adapter guarantee, not a claim that every downstream legacy algorithm is
ready for arbitrary 500-point profiles. Both `.dat` and count-prefixed `.txt`
readers nevertheless reject point 501 before it can index fixed storage, and
the new-tension intrados accumulator stops at the final real segment `last-1`.

For the first Phase-4 slice, `leparagliding_skin_tension` converts Section 31
from its authored trailing-edge-to-leading-edge order into increasing 0--100
percent developed-contour order. Intrados uses source columns 3/4. Its pure
evaluator scales position by contour length and overwidth by panel width. It
retains the historical promoted default-REAL `1.001` inclusive upper-bound
factor and lets the last matching interval win when adjacent intervals overlap.
For `k31d=1`, these typed offsets are authoritative on the lower intrados side
of every real panel `0:nribss-1`, including the final contour point even though
that point has no outgoing neutral segment.

`leparagliding_panel_shaping` then uses those offsets and exact neutral segment
endpoints to produce a typed sewing contour and its cut allowance. Compatibility
includes the legacy lower/higher normal signs, the special horizontal initial
point, incoming-segment endpoint ownership at joins, and the promoted
default-REAL `0.1` millimetre-to-model allowance factor. Stage 8 compares every
typed coordinate with `puntslat` before publishing slots 9 and 11. Other
surfaces and sides remain on their existing paths.

Stage 8 derives each physical wingtip comparison edge from the higher side of
panel `nribss-1`. Row `nribss` remains a terminal boundary/non-panel; typed
regular-panel development and write-back do not synthesize geometry beyond the
final rib. For the `k31d=1` intrados path, a checked neutral-boundary adapter
now seeds the exact terminal `pl1/pl2` segments which the legacy loop previously
read without a Stage-7 producer. Typed traversal checks every cumulative
distance and law offset, `shape_neutral_boundary_edge` applies the lower/outward
normal to the retained higher neutral source, and
`write_legacy_production_boundary` publishes only exact-range slots 9/11.
There is no terminal panel and no slots 10/12. The later optional `ndif`
reformatter still mutates slot 9 as a compatibility consumer; it is a separate
future ownership boundary.

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
