# Semantic DXF regression snapshots

These gzip-compressed JSON files are the normalized `ENTITIES` models for the
reviewed Plan B, even-cell Swoop2, classic-skin gnuA3, and section-29-disabled
gnuA3 outputs. They retain every entity type, layer, CAD colour, group-code
occurrence, ordered polyline vertex, and coordinate while omitting formatting,
entity order, comments, and volatile handles.

The full generated drawings range from roughly 8 to 23 MB and remain build
artifacts. The compact snapshots let the full-output CTest regressions use the
same tolerance-aware semantic comparison as `tools/dxf_semantic_diff.py`
without checking those drawings into source control. Existing normalized file
hashes remain as an additional exact-output guard.

Snapshots are generated only from a reviewed output, for example:

```sh
python tools/dxf_semantic_snapshot.py create \
  build/tests/plan-b-regression/leparagliding.dxf \
  tests/expected/dxf/plan-b-leparagliding.semantic.json.gz
```

Regenerate an oracle only after reviewing the reported entity, layer, colour,
topology, and coordinate changes. Running the regression against an unreviewed
new output must not be used to approve that output.
