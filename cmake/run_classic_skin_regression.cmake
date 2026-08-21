foreach(required_variable PROGRAM CASE_DIR WORK_DIR)
  if(NOT DEFINED ${required_variable} OR "${${required_variable}}" STREQUAL "")
    message(FATAL_ERROR "${required_variable} is required")
  endif()
endforeach()

include("${CMAKE_CURRENT_LIST_DIR}/check_dxf_semantics.cmake")

if(NOT EXISTS "${PROGRAM}")
  message(FATAL_ERROR "LEparagliding executable does not exist: ${PROGRAM}")
endif()

get_filename_component(work_name "${WORK_DIR}" NAME)
if(DISABLE_SHAPING)
  set(expected_work_name "disabled-shaping-regression")
else()
  set(expected_work_name "classic-skin-regression")
endif()
if(NOT work_name STREQUAL expected_work_name)
  message(FATAL_ERROR "Refusing to clean unexpected regression directory: ${WORK_DIR}")
endif()

file(REMOVE_RECURSE "${WORK_DIR}")
file(MAKE_DIRECTORY "${WORK_DIR}")

foreach(input_file
    leparagliding.txt
    gnua.txt
    gnuat.txt)
  if(NOT EXISTS "${CASE_DIR}/${input_file}")
    message(FATAL_ERROR "Classic-skin fixture input is missing: ${input_file}")
  endif()
  file(COPY "${CASE_DIR}/${input_file}" DESTINATION "${WORK_DIR}")
endforeach()

# This fixture is the reviewed gnuA3 preset from the maintained
# LeParagliding/resources/presets/gnuA3 tree. Its source SHA-256 values are:
#   leparagliding.txt D8239309FCBC7CC5A88B174BB69B99F09623DABE78E8E108922A3D76DFC2D380
#   gnua.txt          DC4548BAF611877A7EA5EF9B028450C62F657E37A38D83FCDEFD617133D7A9F9
#   gnuat.txt         11D84F2F34DE2BF0260A00C021BECB3CB261DBAEDF436AFDEF262425D6F4787B
file(READ "${CASE_DIR}/leparagliding.txt" fixture_input)
string(ASCII 9 tab_character)
string(REPLACE "${tab_character}" " " normalized_fixture_input
    "${fixture_input}")
string(REGEX MATCH "1000 +1[.]0" ndif_enabled "${normalized_fixture_input}")
if(ndif_enabled STREQUAL "")
  message(FATAL_ERROR
      "Classic fixture must retain ndif=1000 and xndif=1.0 coverage")
endif()
if(DISABLE_SHAPING)
  string(FIND "${fixture_input}" "*       29. 3D SHAPING" section_29_start)
  string(FIND "${fixture_input}"
      "*       30. AIRFOIL THICKNESS MODIFICATION" section_30_start)
  if(section_29_start EQUAL -1 OR section_30_start EQUAL -1 OR
      section_30_start LESS_EQUAL section_29_start)
    message(FATAL_ERROR "Could not isolate gnuA3 section 29 for the disabled-shaping test")
  endif()
  string(SUBSTRING "${fixture_input}" 0 ${section_29_start} input_prefix)
  string(SUBSTRING "${fixture_input}" ${section_30_start} -1 input_suffix)
  set(disabled_section_29
      "*       29. 3D SHAPING\n*******************************************************\n0\n*******************************************************\n")
  string(CONCAT fixture_input
      "${input_prefix}" "${disabled_section_29}" "${input_suffix}")
  file(WRITE "${WORK_DIR}/leparagliding.txt" "${fixture_input}")
endif()
string(REGEX MATCH
    "\\*[\t ]*31\\. NEW SKIN TENSION MODULE[\t ]*[\r\n]+\\*+[\r\n]+[\t ]*0[\t ]*[\r\n]"
    section_31_disabled "${fixture_input}")
if(section_31_disabled STREQUAL "")
  message(FATAL_ERROR
      "Classic skin-tension fixture must have section 31's first data value set to 0")
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
      "Classic skin-tension run failed with exit code ${program_result}:\n${program_stderr}")
endif()

file(READ "${WORK_DIR}/console.stdout.txt" program_stdout)
string(FIND "${program_stdout}" "OK, paraglider calculated!" success_position)
if(success_position EQUAL -1)
  message(FATAL_ERROR "Classic skin-tension run did not print its success message")
endif()
string(FIND "${program_stdout}"
    "Warning: declared cell parity and central width differ" parity_warning)
if(NOT parity_warning EQUAL -1)
  message(FATAL_ERROR "Classic skin-tension fixture produced a parity warning")
endif()

# These hashes are from the reviewed 28-cell gnuA3 preset running classic skin
# tension. Text is line-ending normalized for GNU Fortran runs on GNU/Linux and
# Windows. The derived mode replaces section 29 with its documented single zero
# while retaining the immutable authored geometry and profiles.
if(DISABLE_SHAPING)
  set(expected_outputs
      leparagliding.dxf 6e1c19add31f09726357da547ada52253e67905cdb8220e18c5aef6b4d0438a6
      lep-3d.dxf        d0926481e05c220a86f8495a6fe83670f00dc1ef964194f5941f11dc883d2287
      lep-out.txt       85a89aec246e3bb5c56ad66354a3259539e3e227fe178877f1ab146b7699f4d3
      lines.txt         2b15d495205c5bc7a1b30c7e6ac90ac0bfc4fce9bba62d58e18616d28667cc0d
      run-log.txt       22d784defdc48084de718118733ad6c32904edb2fdcde449f10daa81938cb9ea)
else()
  set(expected_outputs
      leparagliding.dxf 6e1c19add31f09726357da547ada52253e67905cdb8220e18c5aef6b4d0438a6
      lep-3d.dxf        f0eb18191ba120ca8506cf4a6f752a5a86b26e2e9b52c48952227c9fc9eb895a
      lep-out.txt       85a89aec246e3bb5c56ad66354a3259539e3e227fe178877f1ab146b7699f4d3
      lines.txt         2b15d495205c5bc7a1b30c7e6ac90ac0bfc4fce9bba62d58e18616d28667cc0d
      run-log.txt       22d784defdc48084de718118733ad6c32904edb2fdcde449f10daa81938cb9ea)
endif()

list(LENGTH expected_outputs output_list_length)
math(EXPR last_output_index "${output_list_length} - 1")
set(changed_outputs "")

foreach(index RANGE 0 ${last_output_index} 2)
  math(EXPR hash_index "${index} + 1")
  list(GET expected_outputs ${index} output_name)
  list(GET expected_outputs ${hash_index} expected_hash)
  set(output_path "${WORK_DIR}/${output_name}")

  if(NOT EXISTS "${output_path}")
    message(FATAL_ERROR
        "Expected classic skin-tension output was not created: ${output_name}")
  endif()

  if(DISABLE_SHAPING)
    set(semantic_prefix "disabled-shaping")
  else()
    set(semantic_prefix "classic-skin")
  endif()
  if(output_name STREQUAL "leparagliding.dxf")
    leparagliding_assert_dxf_semantics(
        "${semantic_prefix}-leparagliding" "${output_path}")
  elseif(output_name STREQUAL "lep-3d.dxf")
    leparagliding_assert_dxf_semantics(
        "${semantic_prefix}-lep-3d" "${output_path}")
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
          "Classic skin-tension output contains non-finite geometry: ${output_name}")
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
  message(FATAL_ERROR "Classic skin-tension outputs changed:${changed_outputs}")
endif()

file(READ "${WORK_DIR}/lep-out.txt" calculation_report)
string(REGEX MATCH "Ribs number  =[ \t]+29" expected_rib_count
    "${calculation_report}")
string(REGEX MATCH "Cells number =[ \t]+28" expected_cell_count
    "${calculation_report}")
if(expected_rib_count STREQUAL "" OR expected_cell_count STREQUAL "")
  message(FATAL_ERROR
      "Classic skin-tension report did not preserve the declared 29-rib/28-cell counts")
endif()

if(DISABLE_SHAPING)
  message(STATUS "Disabled-shaping outputs match the reviewed k29d=0 baseline")
else()
  message(STATUS "Classic k31d=0 outputs match the reviewed 3.29 baseline")
endif()
