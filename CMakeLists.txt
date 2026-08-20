cmake_minimum_required(VERSION 3.20)

project(LEparagliding VERSION 3.29 LANGUAGES Fortran)

option(LEPARAGLIDING_ENABLE_WARNINGS "Enable additional compiler warnings" OFF)
option(LEPARAGLIDING_RUNTIME_CHECKS "Enable GNU Fortran runtime checks" OFF)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules")

set(LEPARAGLIDING_MAIN_FRAGMENTS
    src/main/declarations.inc
    src/main/03_initialization.inc
    src/main/04_data_reading.inc
    src/main/05_graphic_design.inc
    src/main/06_airfoil_geometry.inc
    src/main/07_panel_development.inc
    src/main/08_skin_tension.inc
    src/main/09_singular_rib_points.inc
    src/main/10_calage.inc
    src/main/11_panel_lengths.inc
    src/main/12_lines.inc
    src/main/14_brakes.inc
    src/main/15_colors.inc
    src/main/16_internal_ribs.inc
    src/main/17_equilibrium.inc
    src/main/18_text_output.inc
    src/main/19_lines_output.inc
    src/main/20_line_labels.inc
    src/main/21_3d_dxf.inc
    src/main/22_notes.inc
    src/main/23_finish.inc)

set(LEPARAGLIDING_PROCEDURE_FRAGMENTS
    src/procedures/dxf_output.inc
    src/procedures/geometry_2d.inc
    src/procedures/panel_edges.inc
    src/procedures/junctions.inc
    src/procedures/offsets_reinforcements.inc
    src/procedures/geometry_3d.inc
    src/procedures/pattern_marks.inc
    src/procedures/profile_data.inc
    src/procedures/geometry_utilities.inc
    src/procedures/interpolation.inc
    src/procedures/file_cleanup.inc
    src/procedures/transformations.inc)

set_source_files_properties(
    ${LEPARAGLIDING_MAIN_FRAGMENTS}
    ${LEPARAGLIDING_PROCEDURE_FRAGMENTS}
    PROPERTIES HEADER_FILE_ONLY TRUE)

add_executable(leparagliding
    src/leparagliding_hvr_config.f90
    src/leparagliding_mark_types.f90
    src/leparagliding_geometry.f90
    src/leparagliding_procedures.f
    src/leparagliding.f
    ${LEPARAGLIDING_MAIN_FRAGMENTS}
    ${LEPARAGLIDING_PROCEDURE_FRAGMENTS})

target_include_directories(leparagliding PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/src")

if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  target_compile_options(leparagliding PRIVATE
      -std=legacy
      -ffixed-line-length-none
      -fallow-argument-mismatch)

  if(LEPARAGLIDING_ENABLE_WARNINGS)
    target_compile_options(leparagliding PRIVATE
        -Wall
        -Wextra
        -Wimplicit-interface
        -Wsurprising)
  endif()

  if(LEPARAGLIDING_RUNTIME_CHECKS)
    target_compile_options(leparagliding PRIVATE -fcheck=all -fbacktrace)
  endif()
endif()

include(CTest)

if(BUILD_TESTING)
  add_test(
      NAME plan_b_regression
      COMMAND "${CMAKE_COMMAND}"
          "-DPROGRAM=$<TARGET_FILE:leparagliding>"
          "-DCASE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/Plan B Parakite"
          "-DWORK_DIR=${CMAKE_CURRENT_BINARY_DIR}/tests/plan-b-regression"
          -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/run_plan_b_regression.cmake")

  add_test(
      NAME profile_capacity_guard
      COMMAND "${CMAKE_COMMAND}"
          "-DPROGRAM=$<TARGET_FILE:leparagliding>"
          "-DCASE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/Plan B Parakite"
          "-DWORK_DIR=${CMAKE_CURRENT_BINARY_DIR}/tests/profile-capacity-check"
          -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/run_profile_capacity_check.cmake")

  add_test(
      NAME version_329_features
      COMMAND "${CMAKE_COMMAND}"
          "-DPROGRAM=$<TARGET_FILE:leparagliding>"
          "-DCASE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/Plan B Parakite"
          "-DWORK_DIR=${CMAKE_CURRENT_BINARY_DIR}/tests/version-329-features"
          -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/run_329_features.cmake")
endif()
