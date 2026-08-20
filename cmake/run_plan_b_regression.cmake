foreach(required_variable PROGRAM CASE_DIR WORK_DIR)
  if(NOT DEFINED ${required_variable} OR "${${required_variable}}" STREQUAL "")
    message(FATAL_ERROR "${required_variable} is required")
  endif()
endforeach()

if(NOT EXISTS "${PROGRAM}")
  message(FATAL_ERROR "LEparagliding executable does not exist: ${PROGRAM}")
endif()

get_filename_component(work_name "${WORK_DIR}" NAME)
if(NOT work_name STREQUAL "plan-b-regression")
  message(FATAL_ERROR "Refusing to clean unexpected regression directory: ${WORK_DIR}")
endif()

file(REMOVE_RECURSE "${WORK_DIR}")
file(MAKE_DIRECTORY "${WORK_DIR}")

foreach(input_file leparagliding.txt gnuReflex_SN.txt gnuReflexC174.txt)
  if(NOT EXISTS "${CASE_DIR}/${input_file}")
    message(FATAL_ERROR "Plan B input is missing: ${CASE_DIR}/${input_file}")
  endif()
  file(COPY_FILE "${CASE_DIR}/${input_file}" "${WORK_DIR}/${input_file}")
endforeach()

execute_process(
    COMMAND "${PROGRAM}"
    WORKING_DIRECTORY "${WORK_DIR}"
    RESULT_VARIABLE program_result
    OUTPUT_FILE "${WORK_DIR}/console.stdout.txt"
    ERROR_FILE "${WORK_DIR}/console.stderr.txt")

if(NOT program_result EQUAL 0)
  file(READ "${WORK_DIR}/console.stderr.txt" program_stderr)
  message(FATAL_ERROR
      "Plan B run failed with exit code ${program_result}:\n${program_stderr}")
endif()

file(READ "${WORK_DIR}/console.stdout.txt" program_stdout)
string(FIND "${program_stdout}" "OK, paraglider calculated!" success_position)
if(success_position EQUAL -1)
  message(FATAL_ERROR "Plan B run did not print its success message")
endif()

# Hash the text after normalizing line endings so GNU/Linux and Windows can
# share the same regression oracle.
set(expected_outputs
    leparagliding.dxf cc3c0365a73aba5fb6d096743d32a1016b5483c92c82e41f24b847226b6923dc
    lep-3d.dxf        dfc3917d4c402ebb7c8543c2fb0e636f8a4845f6f83397ae1fa61496a2612958
    lep-out.txt       0c1f8a9f844ae94fccaaa9351c2d174caa3e01cd7ebee670ecef048f1d503c65
    lines.txt         0e9cc3879f81d77e3909918f4391e786af34ed22d9c5c3acf483a5dcf6da6cc8
    run-log.txt       24c2d35b5b762d0dcb5c4191564787ccad93726680dcd25765e3a6f1eb641d4c)

list(LENGTH expected_outputs output_list_length)
math(EXPR last_output_index "${output_list_length} - 1")
set(changed_outputs "")

foreach(index RANGE 0 ${last_output_index} 2)
  math(EXPR hash_index "${index} + 1")
  list(GET expected_outputs ${index} output_name)
  list(GET expected_outputs ${hash_index} expected_hash)
  set(output_path "${WORK_DIR}/${output_name}")

  if(NOT EXISTS "${output_path}")
    message(FATAL_ERROR "Expected output was not created: ${output_name}")
  endif()

  file(READ "${output_path}" output_text)
  string(REPLACE "\r\n" "\n" output_text "${output_text}")
  string(REPLACE "\r" "\n" output_text "${output_text}")
  string(SHA256 actual_hash "${output_text}")

  if(NOT actual_hash STREQUAL expected_hash)
    string(APPEND changed_outputs
        "\n${output_name}\n"
        "  expected ${expected_hash}\n"
        "  actual   ${actual_hash}\n")
  endif()
endforeach()

if(NOT changed_outputs STREQUAL "")
  message(FATAL_ERROR "Plan B outputs changed:${changed_outputs}")
endif()

message(STATUS "Plan B outputs match the reviewed 3.29 baseline")
