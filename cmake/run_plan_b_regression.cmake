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
    leparagliding.dxf 836176f70755e5343be1a2a285123c14f45b447b79fba81a7ce3aa5fa3af0125
    lep-3d.dxf        26543ac41008ca7f3d7df739238b124fa60953517b7703da998635457c1fc1ee
    lep-out.txt       7f236a9a65d680a51b72adcef1889f40d98649da8ff3fa154b505c46f917fa80
    lines.txt         0e9cc3879f81d77e3909918f4391e786af34ed22d9c5c3acf483a5dcf6da6cc8
    run-log.txt       e85d948c4bc287854e49b73a4f4bb56bc34607d58435bbac5a5726e4a5791bae)

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

message(STATUS "Plan B outputs match the corrected 3.28 safety baseline")
