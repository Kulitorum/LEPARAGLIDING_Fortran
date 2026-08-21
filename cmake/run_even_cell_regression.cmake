foreach(required_variable PROGRAM CASE_DIR WORK_DIR)
  if(NOT DEFINED ${required_variable} OR "${${required_variable}}" STREQUAL "")
    message(FATAL_ERROR "${required_variable} is required")
  endif()
endforeach()

include("${CMAKE_CURRENT_LIST_DIR}/check_dxf_semantics.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/check_text_semantics.cmake")

if(NOT EXISTS "${PROGRAM}")
  message(FATAL_ERROR "LEparagliding executable does not exist: ${PROGRAM}")
endif()

get_filename_component(work_name "${WORK_DIR}" NAME)
if(NOT work_name STREQUAL "even-cell-swoop2-regression")
  message(FATAL_ERROR "Refusing to clean unexpected regression directory: ${WORK_DIR}")
endif()

file(REMOVE_RECURSE "${WORK_DIR}")
file(MAKE_DIRECTORY "${WORK_DIR}")

foreach(input_file leparagliding.txt gnuReflex.txt)
  if(NOT EXISTS "${CASE_DIR}/${input_file}")
    message(FATAL_ERROR "Even-cell fixture input is missing: ${input_file}")
  endif()
  file(COPY "${CASE_DIR}/${input_file}" DESTINATION "${WORK_DIR}")
endforeach()

file(READ "${CASE_DIR}/leparagliding.txt" fixture_input)
string(ASCII 9 tab_character)
string(REPLACE "${tab_character}" " " normalized_fixture_input
    "${fixture_input}")
string(REGEX MATCH "1000 +1[.]0" ndif_enabled "${normalized_fixture_input}")
if(ndif_enabled STREQUAL "")
  message(FATAL_ERROR
      "Even-cell fixture must retain ndif=1000 and xndif=1.0 coverage")
endif()

execute_process(
    COMMAND "${PROGRAM}"
    WORKING_DIRECTORY "${WORK_DIR}"
    RESULT_VARIABLE program_result
    OUTPUT_FILE "${WORK_DIR}/console.stdout.txt"
    ERROR_FILE "${WORK_DIR}/console.stderr.txt")

if(NOT program_result EQUAL 0)
  file(READ "${WORK_DIR}/console.stderr.txt" program_stderr)
  message(FATAL_ERROR
      "Even-cell run failed with exit code ${program_result}:\n${program_stderr}")
endif()

file(READ "${WORK_DIR}/console.stdout.txt" program_stdout)
string(FIND "${program_stdout}" "OK, paraglider calculated!" success_position)
if(success_position EQUAL -1)
  message(FATAL_ERROR "Even-cell run did not print its success message")
endif()
string(FIND "${program_stdout}"
    "Warning: declared cell parity and central width differ" parity_warning)
if(NOT parity_warning EQUAL -1)
  message(FATAL_ERROR "Even-cell fixture produced a rib-parity warning")
endif()

# These hashes come from the author-supplied Swoop2 3.29 design. Text is
# normalized so GNU Fortran on GNU/Linux and Windows shares one oracle without
# storing the generated 20 MB drawing in the repository.
set(expected_outputs
    leparagliding.dxf 03e1aae3d1a6238727f4b1dbb2a7b95c232f0e88b06d7d531f11b0f5b74de449
    lep-3d.dxf        961cbd9adde5c108dab847a1a84b03efcf1d53f17f8f2d9afb06dcbbf037b381
    lep-out.txt       4203a62d9fbc8beb46db708f17a822646b6f83a64ada4b0237463accbdc005b9
    lines.txt         8a5eb58f2c09454d2972448a5861183b550b8a799c75377d906bceb51c4091cf
    run-log.txt       b1c1460636e82feeed30d63d7bfdba556ff2a606d48112e8e978fcecc452df4f)

list(LENGTH expected_outputs output_list_length)
math(EXPR last_output_index "${output_list_length} - 1")
set(changed_outputs "")

foreach(index RANGE 0 ${last_output_index} 2)
  math(EXPR hash_index "${index} + 1")
  list(GET expected_outputs ${index} output_name)
  list(GET expected_outputs ${hash_index} expected_hash)
  set(output_path "${WORK_DIR}/${output_name}")

  if(NOT EXISTS "${output_path}")
    message(FATAL_ERROR "Expected even-cell output was not created: ${output_name}")
  endif()

  if(output_name STREQUAL "leparagliding.dxf")
    leparagliding_assert_dxf_semantics(
        "even-cell-leparagliding" "${output_path}")
  elseif(output_name STREQUAL "lep-3d.dxf")
    leparagliding_assert_dxf_semantics("even-cell-lep-3d" "${output_path}")
  elseif(output_name STREQUAL "lep-out.txt")
    leparagliding_assert_text_semantics("even-cell-lep-out" "${output_path}")
  elseif(output_name STREQUAL "lines.txt")
    leparagliding_assert_text_semantics("even-cell-lines" "${output_path}")
  endif()

  file(READ "${output_path}" output_text)
  string(REPLACE "\r\n" "\n" output_text "${output_text}")
  string(REPLACE "\r" "\n" output_text "${output_text}")
  if(output_name MATCHES "\\.dxf$")
    string(TOLOWER "${output_text}" lowercase_output)
    string(FIND "${lowercase_output}" "nan" nan_position)
    string(FIND "${lowercase_output}" "infinity" infinity_position)
    if(NOT nan_position EQUAL -1 OR NOT infinity_position EQUAL -1)
      message(FATAL_ERROR
          "Even-cell output contains non-finite geometry: ${output_name}")
    endif()
  endif()
  string(SHA256 actual_hash "${output_text}")

  if(NOT actual_hash STREQUAL expected_hash)
    string(APPEND changed_outputs
        "\n${output_name}\n"
        "  expected ${expected_hash}\n"
        "  actual   ${actual_hash}\n")
  endif()
endforeach()

if(NOT changed_outputs STREQUAL "")
  message(FATAL_ERROR "Even-cell outputs changed:${changed_outputs}")
endif()

file(READ "${WORK_DIR}/lep-out.txt" calculation_report)
string(FIND "${calculation_report}"
    "Zero-thickness central cell" collapsed_center_position)
if(collapsed_center_position EQUAL -1)
  message(FATAL_ERROR
      "Even-cell fixture did not report its collapsed central cell")
endif()
string(REGEX MATCH "Ribs number  =[ \t]+51" expected_rib_count
    "${calculation_report}")
string(REGEX MATCH "Cells number =[ \t]+50" expected_cell_count
    "${calculation_report}")
if(expected_rib_count STREQUAL "" OR expected_cell_count STREQUAL "")
  message(FATAL_ERROR
      "Even-cell report did not preserve the declared 51-rib/50-cell counts")
endif()

message(STATUS "Even-cell Swoop2 outputs match the reviewed 3.29 baseline")
