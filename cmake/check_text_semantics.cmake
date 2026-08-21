# Compare generated human-readable outputs with normalized semantic oracles.
# Exact hashes remain in each regression and continue to be enforced.

find_program(LEPARAGLIDING_TEXT_SEMANTIC_PYTHON NAMES python3 python)
get_filename_component(LEPARAGLIDING_TEXT_REPOSITORY_ROOT
    "${CMAKE_CURRENT_LIST_DIR}/.." ABSOLUTE)
set(LEPARAGLIDING_TEXT_SEMANTIC_TOOL
    "${LEPARAGLIDING_TEXT_REPOSITORY_ROOT}/tools/text_semantic_snapshot.py")
set(LEPARAGLIDING_TEXT_SEMANTIC_DIRECTORY
    "${LEPARAGLIDING_TEXT_REPOSITORY_ROOT}/tests/expected/text")

function(leparagliding_assert_text_semantics oracle_name actual_text)
  if(NOT LEPARAGLIDING_TEXT_SEMANTIC_PYTHON)
    message(STATUS
        "Python was not found; semantic text oracle skipped for ${actual_text}")
    return()
  endif()

  set(snapshot
      "${LEPARAGLIDING_TEXT_SEMANTIC_DIRECTORY}/${oracle_name}.semantic.json.gz")
  if(NOT EXISTS "${snapshot}")
    message(FATAL_ERROR "Semantic text oracle is missing: ${snapshot}")
  endif()
  if(NOT EXISTS "${actual_text}")
    message(FATAL_ERROR "Generated text output is missing: ${actual_text}")
  endif()

  # The reports print values at fixed decimal precision.  A 1e-9 absolute
  # tolerance ignores representation-only changes while rejecting any change
  # visible at the authored output precision.  Relative tolerance is disabled.
  execute_process(
      COMMAND "${LEPARAGLIDING_TEXT_SEMANTIC_PYTHON}"
          "${LEPARAGLIDING_TEXT_SEMANTIC_TOOL}"
          compare "${snapshot}" "${actual_text}"
          --abs-tol 1e-9
          --rel-tol 0
      RESULT_VARIABLE semantic_result
      OUTPUT_VARIABLE semantic_stdout
      ERROR_VARIABLE semantic_stderr)

  if(NOT semantic_result EQUAL 0)
    message(FATAL_ERROR
        "Semantic text regression failed for ${actual_text}:\n"
        "${semantic_stdout}${semantic_stderr}")
  endif()
  string(STRIP "${semantic_stdout}" semantic_summary)
  message(STATUS "${semantic_summary}")
endfunction()
