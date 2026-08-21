# Build and run the immutable original 3.28 monolith as an isolated Plan B
# compatibility oracle.  It must not be linked with maintained procedure
# sources: the historical file already contains the complete program.

find_program(LEPARAGLIDING_COMPATIBILITY_PYTHON NAMES python3 python)
find_program(LEPARAGLIDING_LEGACY_GFORTRAN NAMES gfortran)
get_filename_component(LEPARAGLIDING_COMPATIBILITY_ROOT
    "${CMAKE_CURRENT_LIST_DIR}/.." ABSOLUTE)
set(LEPARAGLIDING_328_ARCHIVE
    "${LEPARAGLIDING_COMPATIBILITY_ROOT}/leparagliding3.28.f.zip")
set(LEPARAGLIDING_328_ARCHIVE_SHA256
    "c9ee08d64c722389f134e4c5923c10324a6bd17c4d2c3c0da216d39a2b2707eb")
set(LEPARAGLIDING_328_SOURCE_SHA256
    "f1f377e0c71d80176410e55e2bd4f3c1369ccf7a34baf80b90fb62b0ef529bd8")
set(LEPARAGLIDING_328_COMPARATOR
    "${LEPARAGLIDING_COMPATIBILITY_ROOT}/tools/check_328_compatibility.py")

function(leparagliding_assert_328_compatibility case_dir current_work_dir)
  if(NOT LEPARAGLIDING_COMPATIBILITY_PYTHON)
    message(STATUS "Python was not found; isolated 3.28 oracle skipped")
    return()
  endif()
  if(NOT LEPARAGLIDING_LEGACY_GFORTRAN)
    message(STATUS "GNU Fortran was not found; isolated 3.28 oracle skipped")
    return()
  endif()
  if(NOT EXISTS "${LEPARAGLIDING_328_ARCHIVE}")
    message(FATAL_ERROR
        "Original 3.28 source archive is missing: ${LEPARAGLIDING_328_ARCHIVE}")
  endif()
  if(NOT EXISTS "${LEPARAGLIDING_328_COMPARATOR}")
    message(FATAL_ERROR
        "3.28 compatibility comparator is missing: ${LEPARAGLIDING_328_COMPARATOR}")
  endif()

  file(SHA256 "${LEPARAGLIDING_328_ARCHIVE}" legacy_archive_hash)
  if(NOT legacy_archive_hash STREQUAL LEPARAGLIDING_328_ARCHIVE_SHA256)
    message(FATAL_ERROR
        "Original 3.28 source archive changed; compatibility oracle requires review\n"
        "expected ${LEPARAGLIDING_328_ARCHIVE_SHA256}\n"
        "actual   ${legacy_archive_hash}")
  endif()

  set(legacy_root "${current_work_dir}/legacy-328")
  set(legacy_source_dir "${legacy_root}/source")
  set(legacy_run "${legacy_root}/run")
  file(MAKE_DIRECTORY "${legacy_source_dir}" "${legacy_run}")
  file(ARCHIVE_EXTRACT
      INPUT "${LEPARAGLIDING_328_ARCHIVE}"
      DESTINATION "${legacy_source_dir}")
  set(legacy_source "${legacy_source_dir}/leparagliding.f")
  if(NOT EXISTS "${legacy_source}")
    message(FATAL_ERROR
        "Original 3.28 archive does not contain leparagliding.f")
  endif()

  # The tracked archive contains the exact build-baseline-src/leparagliding.f
  # supplied for comparison.  Hash normalized text as a second provenance
  # check so the result is independent of source line endings.
  file(READ "${legacy_source}" legacy_source_text)
  string(REPLACE "\r\n" "\n" legacy_source_text "${legacy_source_text}")
  string(REPLACE "\r" "\n" legacy_source_text "${legacy_source_text}")
  string(SHA256 legacy_source_hash "${legacy_source_text}")
  if(NOT legacy_source_hash STREQUAL LEPARAGLIDING_328_SOURCE_SHA256)
    message(FATAL_ERROR
        "Original 3.28 source changed; compatibility oracle requires review\n"
        "expected ${LEPARAGLIDING_328_SOURCE_SHA256}\n"
        "actual   ${legacy_source_hash}")
  endif()

  if(WIN32)
    set(legacy_program "${legacy_root}/leparagliding-328.exe")
  else()
    set(legacy_program "${legacy_root}/leparagliding-328")
  endif()

  execute_process(
      COMMAND "${LEPARAGLIDING_LEGACY_GFORTRAN}"
          -std=legacy
          -ffixed-line-length-none
          -fallow-argument-mismatch
          "${legacy_source}"
          -o "${legacy_program}"
      RESULT_VARIABLE legacy_compile_result
      OUTPUT_FILE "${legacy_root}/compile.stdout.txt"
      ERROR_FILE "${legacy_root}/compile.stderr.txt")
  if(NOT legacy_compile_result EQUAL 0)
    file(READ "${legacy_root}/compile.stderr.txt" legacy_compile_stderr)
    message(FATAL_ERROR
        "Original 3.28 source failed to compile with exit code "
        "${legacy_compile_result}:\n${legacy_compile_stderr}")
  endif()

  foreach(input_file leparagliding.txt gnuReflex_SN.txt gnuReflexC174.txt)
    if(NOT EXISTS "${case_dir}/${input_file}")
      message(FATAL_ERROR "3.28 Plan B input is missing: ${case_dir}/${input_file}")
    endif()
    file(COPY_FILE "${case_dir}/${input_file}" "${legacy_run}/${input_file}")
  endforeach()

  execute_process(
      COMMAND "${legacy_program}"
      WORKING_DIRECTORY "${legacy_run}"
      RESULT_VARIABLE legacy_run_result
      OUTPUT_FILE "${legacy_root}/console.stdout.txt"
      ERROR_FILE "${legacy_root}/console.stderr.txt")
  if(NOT legacy_run_result EQUAL 0)
    file(READ "${legacy_root}/console.stderr.txt" legacy_run_stderr)
    message(FATAL_ERROR
        "Original 3.28 Plan B run failed with exit code ${legacy_run_result}:\n"
        "${legacy_run_stderr}")
  endif()
  file(READ "${legacy_root}/console.stdout.txt" legacy_run_stdout)
  string(FIND "${legacy_run_stdout}" "OK, paraglider calculated!"
      legacy_success_position)
  if(legacy_success_position EQUAL -1)
    message(FATAL_ERROR "Original 3.28 Plan B run did not report success")
  endif()

  execute_process(
      COMMAND "${LEPARAGLIDING_COMPATIBILITY_PYTHON}"
          "${LEPARAGLIDING_328_COMPARATOR}"
          "${legacy_run}" "${current_work_dir}"
          --abs-tol 1e-9
      RESULT_VARIABLE compatibility_result
      OUTPUT_VARIABLE compatibility_stdout
      ERROR_VARIABLE compatibility_stderr)
  if(NOT compatibility_result EQUAL 0)
    message(FATAL_ERROR
        "Original 3.28 compatibility regression failed:\n"
        "${compatibility_stdout}${compatibility_stderr}")
  endif()
  string(STRIP "${compatibility_stdout}" compatibility_summary)
  message(STATUS "${compatibility_summary}")
endfunction()
