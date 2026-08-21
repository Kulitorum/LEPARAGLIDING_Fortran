# Compare a generated DXF with its compact normalized ENTITIES oracle.
#
# Python remains optional for building LEparagliding.  When it is available,
# every full-output regression invokes the same tolerance-aware comparison as
# tools/dxf_semantic_diff.py.  Raw-file hashes remain as a stricter stability
# guard, while this check independently proves entity types, layers, CAD
# colours, group-code topology, polyline vertices, and coordinates.

find_program(LEPARAGLIDING_SEMANTIC_PYTHON NAMES python3 python)
get_filename_component(LEPARAGLIDING_REPOSITORY_ROOT
    "${CMAKE_CURRENT_LIST_DIR}/.." ABSOLUTE)
set(LEPARAGLIDING_SEMANTIC_DXF_TOOL
    "${LEPARAGLIDING_REPOSITORY_ROOT}/tools/dxf_semantic_snapshot.py")
set(LEPARAGLIDING_SEMANTIC_DXF_DIRECTORY
    "${LEPARAGLIDING_REPOSITORY_ROOT}/tests/expected/dxf")

function(leparagliding_assert_dxf_semantics oracle_name actual_dxf)
  if(NOT LEPARAGLIDING_SEMANTIC_PYTHON)
    message(STATUS
        "Python was not found; semantic DXF oracle skipped for ${actual_dxf}")
    return()
  endif()

  set(snapshot
      "${LEPARAGLIDING_SEMANTIC_DXF_DIRECTORY}/${oracle_name}.semantic.json.gz")
  if(NOT EXISTS "${snapshot}")
    message(FATAL_ERROR "Semantic DXF oracle is missing: ${snapshot}")
  endif()
  if(NOT EXISTS "${actual_dxf}")
    message(FATAL_ERROR "Generated DXF is missing: ${actual_dxf}")
  endif()

  execute_process(
      COMMAND "${LEPARAGLIDING_SEMANTIC_PYTHON}"
          "${LEPARAGLIDING_SEMANTIC_DXF_TOOL}"
          compare "${snapshot}" "${actual_dxf}"
          --abs-tol 1e-9
          --rel-tol 0
      RESULT_VARIABLE semantic_result
      OUTPUT_VARIABLE semantic_stdout
      ERROR_VARIABLE semantic_stderr)

  if(NOT semantic_result EQUAL 0)
    message(FATAL_ERROR
        "Semantic DXF regression failed for ${actual_dxf}:\n"
        "${semantic_stdout}${semantic_stderr}")
  endif()
  string(STRIP "${semantic_stdout}" semantic_summary)
  message(STATUS "${semantic_summary}")
endfunction()
