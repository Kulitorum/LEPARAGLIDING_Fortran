foreach(required_variable PROGRAM CASE_DIR WORK_DIR)
  if(NOT DEFINED ${required_variable} OR "${${required_variable}}" STREQUAL "")
    message(FATAL_ERROR "${required_variable} is required")
  endif()
endforeach()

if(NOT EXISTS "${PROGRAM}")
  message(FATAL_ERROR "LEparagliding executable does not exist: ${PROGRAM}")
endif()

get_filename_component(work_name "${WORK_DIR}" NAME)
if(NOT work_name STREQUAL "profile-capacity-check")
  message(FATAL_ERROR "Refusing to clean unexpected test directory: ${WORK_DIR}")
endif()

file(REMOVE_RECURSE "${WORK_DIR}")
file(MAKE_DIRECTORY "${WORK_DIR}")

file(READ "${CASE_DIR}/leparagliding.txt" design_text)
string(REPLACE "gnuReflex_SN.txt" "over-capacity.dat" design_text
    "${design_text}")
string(REPLACE "gnuReflexC174.txt" "over-capacity.dat" design_text
    "${design_text}")
file(WRITE "${WORK_DIR}/leparagliding.txt" "${design_text}")

# The profile reader accepts a title followed by coordinates. Its caller owns
# storage for 500 points, so point 501 must be rejected before any copy occurs.
string(REPEAT "0.500000 0.000000\n" 501 profile_points)
file(WRITE "${WORK_DIR}/over-capacity.dat"
    "LEparagliding profile-capacity regression\n${profile_points}")

execute_process(
    COMMAND "${PROGRAM}"
    WORKING_DIRECTORY "${WORK_DIR}"
    RESULT_VARIABLE program_result
    OUTPUT_FILE "${WORK_DIR}/console.stdout.txt"
    ERROR_FILE "${WORK_DIR}/console.stderr.txt")

if(program_result EQUAL 0)
  message(FATAL_ERROR "An oversized profile was accepted unexpectedly")
endif()

file(READ "${WORK_DIR}/console.stdout.txt" program_stdout)
string(FIND "${program_stdout}" "ERROR: profile has" error_position)
if(error_position EQUAL -1)
  file(READ "${WORK_DIR}/console.stderr.txt" program_stderr)
  message(FATAL_ERROR
      "Oversized profile failed for the wrong reason:\n${program_stderr}")
endif()

message(STATUS "Profile capacity guard rejected 501 points before copying")
