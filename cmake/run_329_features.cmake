foreach(required_variable PROGRAM CASE_DIR WORK_DIR)
  if(NOT DEFINED ${required_variable} OR "${${required_variable}}" STREQUAL "")
    message(FATAL_ERROR "${required_variable} is required")
  endif()
endforeach()

if(NOT EXISTS "${PROGRAM}")
  message(FATAL_ERROR "LEparagliding executable does not exist: ${PROGRAM}")
endif()

get_filename_component(work_name "${WORK_DIR}" NAME)
if(NOT work_name STREQUAL "version-329-features")
  message(FATAL_ERROR "Refusing to clean unexpected test directory: ${WORK_DIR}")
endif()

file(REMOVE_RECURSE "${WORK_DIR}")
file(MAKE_DIRECTORY "${WORK_DIR}")

foreach(profile_file gnuReflex_SN.txt gnuReflexC174.txt)
  file(COPY_FILE "${CASE_DIR}/${profile_file}"
                 "${WORK_DIR}/${profile_file}")
endforeach()

file(READ "${CASE_DIR}/leparagliding.txt" design_text)
string(REPLACE "\r\n" "\n" design_text "${design_text}")
string(REPLACE "\r" "\n" design_text "${design_text}")

# Exercise the two new long-rod types with the same valid group data as the
# original Plan B shark-nose block.
set(old_rod_block [=[2
1
1 3
1
1 1 8
5.0   11.0    1.2   2.0
3.0   5.0     1.5   0.8
4.5   4.0     2.0   0.7
15  20    1.0   2.2
2.5   32    10   25]=])
set(new_rod_block [=[2
2
1 4
1
1 1 8
23.5  29.5     1.1   2.0
63.0  70.0     1.1   2.0
0.0    9.35    6.3   9.35
2 5
1
1 1 8
23.5  29.5     1.1   2.0
63.0  70.0     1.1   2.0
0.0    9.35    6.3   9.35]=])
string(FIND "${design_text}" "${old_rod_block}" rod_block_position)
if(rod_block_position EQUAL -1)
  message(FATAL_ERROR "Could not locate the Plan B nylon-rod fixture")
endif()
string(REPLACE "${old_rod_block}" "${new_rod_block}" design_text
               "${design_text}")

# Add all section-37 codes introduced in 3.29, plus optional sections 38/39.
string(REPLACE "\n1\n8\n1291" "\n1\n11\n1291" design_text
               "${design_text}")
string(REPLACE "\"Plan B\"" "\"Plan Bé\"" design_text "${design_text}")
string(APPEND design_text [=[
2005 1
2006 1
3002 24
*******************************************************
*       38. HVR HOLES
*******************************************************
1
3
1 16 1 2 70.0 50.0 0.5 0.0 0.0
2 15 1 2 45.0 25.0 0.6 0.0 0.0
3  9 1 2 70.0  0.6 -0.15 15.0 0.0
*******************************************************
*       39. HVR POSITION PARAMETERS
*******************************************************
1
6
vrib_type1 0.8 1.0
vrib_type2 1.0 1.0
vrib_type3 1.0 1.0
vrib_type4 1.0 1.0
vrib_type5 0.9 1.0
vrib_type6 0.9 1.0
]=])
file(WRITE "${WORK_DIR}/leparagliding.txt" "${design_text}")

execute_process(
    COMMAND "${PROGRAM}"
    WORKING_DIRECTORY "${WORK_DIR}"
    RESULT_VARIABLE program_result
    OUTPUT_FILE "${WORK_DIR}/console.stdout.txt"
    ERROR_FILE "${WORK_DIR}/console.stderr.txt")

if(NOT program_result EQUAL 0)
  file(READ "${WORK_DIR}/console.stderr.txt" program_stderr)
  message(FATAL_ERROR
      "3.29 feature run failed with exit code ${program_result}:\n${program_stderr}")
endif()

file(READ "${WORK_DIR}/console.stdout.txt" program_stdout)
foreach(required_message
    "38-HVR holes read"
    "39-HVR position parameters read"
    "OK, paraglider calculated!")
  string(FIND "${program_stdout}" "${required_message}" message_position)
  if(message_position EQUAL -1)
    message(FATAL_ERROR "3.29 run omitted: ${required_message}")
  endif()
endforeach()

file(READ "${WORK_DIR}/lep-out.txt" report_text)
foreach(required_report_text
    "type  4"
    "type  5"
    "2005"
    "2006"
    "3002")
  string(FIND "${report_text}" "${required_report_text}" report_position)
  if(report_position EQUAL -1)
    message(FATAL_ERROR "3.29 report omitted: ${required_report_text}")
  endif()
endforeach()

file(READ "${WORK_DIR}/leparagliding.dxf" dxf_text)
string(FIND "${dxf_text}" "\\U+00E9" utf8_position)
if(utf8_position EQUAL -1)
  message(FATAL_ERROR "UTF-8 brand text was not escaped for DXF R12")
endif()
foreach(required_dxf_text "AC1009" "POLYLINE")
  string(FIND "${dxf_text}" "${required_dxf_text}" dxf_position)
  if(dxf_position EQUAL -1)
    message(FATAL_ERROR "3.29 DXF omitted: ${required_dxf_text}")
  endif()
endforeach()

string(TOLOWER "${dxf_text}" dxf_text_lower)
foreach(invalid_number "nan" "infinity")
  string(FIND "${dxf_text_lower}" "${invalid_number}" invalid_position)
  if(NOT invalid_position EQUAL -1)
    message(FATAL_ERROR "3.29 DXF contains ${invalid_number}")
  endif()
endforeach()

message(STATUS "LEparagliding 3.29 parser, rods, holes, and UTF-8 path passed")
