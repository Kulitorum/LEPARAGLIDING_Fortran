if(NOT DEFINED PYTHON OR NOT DEFINED IMPORTER OR NOT DEFINED FIXTURE OR
   NOT DEFINED EXPECTED OR NOT DEFINED OUTPUT)
  message(FATAL_ERROR "Color importer test requires PYTHON, IMPORTER, FIXTURE, EXPECTED, and OUTPUT")
endif()

get_filename_component(output_directory "${OUTPUT}" DIRECTORY)
file(MAKE_DIRECTORY "${output_directory}")

execute_process(
  COMMAND "${PYTHON}" "${IMPORTER}" "${FIXTURE}"
      --surface intrados
      --division-layer 0
      --division-color 6
      --rib-layer default
      --rib-color 5
      --output "${OUTPUT}"
  RESULT_VARIABLE import_result
  OUTPUT_VARIABLE import_stdout
  ERROR_VARIABLE import_stderr)

if(NOT import_result EQUAL 0)
  message(FATAL_ERROR "Color importer failed (${import_result}):\n${import_stdout}\n${import_stderr}")
endif()

file(READ "${OUTPUT}" actual)
file(READ "${EXPECTED}" expected)
if(NOT actual STREQUAL expected)
  message(FATAL_ERROR "Color importer output differs from Swoop2 sample oracle\n--- actual ---\n${actual}\n--- expected ---\n${expected}")
endif()
