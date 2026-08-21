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
    src/procedures/color_construction.inc
    src/procedures/geometry_2d.inc
    src/procedures/panel_edges.inc
    src/procedures/junctions.inc
    src/procedures/offsets_reinforcements.inc
    src/procedures/geometry_3d.inc
    src/procedures/pattern_marks.inc
    src/procedures/profile_data.inc
    src/procedures/geometry_utilities.inc)

set_source_files_properties(
    ${LEPARAGLIDING_MAIN_FRAGMENTS}
    ${LEPARAGLIDING_PROCEDURE_FRAGMENTS}
    PROPERTIES HEADER_FILE_ONLY TRUE)

add_executable(leparagliding
    src/leparagliding_hvr_config.f90
    src/leparagliding_mark_types.f90
    src/leparagliding_geometry.f90
    src/leparagliding_domain_model.f90
    src/leparagliding_spatial_geometry.f90
    src/leparagliding_anchor_geometry.f90
    src/leparagliding_structural_geometry.f90
    src/leparagliding_neutral_development.f90
    src/leparagliding_skin_tension.f90
    src/leparagliding_panel_shaping.f90
    src/leparagliding_panel_reformat.f90
    src/leparagliding_color_geometry.f90
    src/leparagliding_transformations.f90
    src/leparagliding_file_cleanup.f90
    src/leparagliding_polyline_interpolation.f90
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
  add_executable(test_domain_model
      tests/test_domain_model.f90
      src/leparagliding_domain_model.f90)

  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(test_domain_model PRIVATE -std=f2008 -Wall -Wextra)
    if(LEPARAGLIDING_RUNTIME_CHECKS)
      target_compile_options(test_domain_model PRIVATE -fcheck=all -fbacktrace)
    endif()
  endif()

  add_test(NAME domain_model COMMAND test_domain_model)

  add_executable(test_spatial_geometry
      tests/test_spatial_geometry.f90
      src/leparagliding_domain_model.f90
      src/leparagliding_spatial_geometry.f90)

  set_target_properties(test_spatial_geometry PROPERTIES
      Fortran_MODULE_DIRECTORY
          "${CMAKE_CURRENT_BINARY_DIR}/test-spatial-geometry-modules")
  target_include_directories(test_spatial_geometry PRIVATE
      "${CMAKE_CURRENT_BINARY_DIR}/test-spatial-geometry-modules")

  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(test_spatial_geometry PRIVATE
        -std=f2008 -Wall -Wextra)
    if(LEPARAGLIDING_RUNTIME_CHECKS)
      target_compile_options(test_spatial_geometry PRIVATE
          -fcheck=all -fbacktrace)
    endif()
  endif()

  add_test(NAME spatial_geometry COMMAND test_spatial_geometry)

  add_executable(test_anchor_geometry
      tests/test_anchor_geometry.f90
      src/leparagliding_domain_model.f90
      src/leparagliding_spatial_geometry.f90
      src/leparagliding_anchor_geometry.f90)

  set_target_properties(test_anchor_geometry PROPERTIES
      Fortran_MODULE_DIRECTORY
          "${CMAKE_CURRENT_BINARY_DIR}/test-anchor-geometry-modules")
  target_include_directories(test_anchor_geometry PRIVATE
      "${CMAKE_CURRENT_BINARY_DIR}/test-anchor-geometry-modules")

  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(test_anchor_geometry PRIVATE
        -std=f2008 -Wall -Wextra)
    if(LEPARAGLIDING_RUNTIME_CHECKS)
      target_compile_options(test_anchor_geometry PRIVATE
          -fcheck=all -fbacktrace)
    endif()
  endif()

  add_test(NAME anchor_geometry COMMAND test_anchor_geometry)

  add_executable(test_structural_geometry
      tests/test_structural_geometry.f90
      src/leparagliding_domain_model.f90
      src/leparagliding_spatial_geometry.f90
      src/leparagliding_structural_geometry.f90)

  set_target_properties(test_structural_geometry PROPERTIES
      Fortran_MODULE_DIRECTORY
          "${CMAKE_CURRENT_BINARY_DIR}/test-structural-geometry-modules")
  target_include_directories(test_structural_geometry PRIVATE
      "${CMAKE_CURRENT_BINARY_DIR}/test-structural-geometry-modules")

  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(test_structural_geometry PRIVATE
        -std=f2008 -Wall -Wextra)
    if(LEPARAGLIDING_RUNTIME_CHECKS)
      target_compile_options(test_structural_geometry PRIVATE
          -fcheck=all -fbacktrace)
    endif()
  endif()

  add_test(NAME structural_geometry COMMAND test_structural_geometry)

  add_executable(test_transformations
      tests/test_transformations.f90
      src/leparagliding_geometry.f90
      src/leparagliding_transformations.f90)

  set_target_properties(test_transformations PROPERTIES
      Fortran_MODULE_DIRECTORY
          "${CMAKE_CURRENT_BINARY_DIR}/test-transformations-modules")
  target_include_directories(test_transformations PRIVATE
      "${CMAKE_CURRENT_BINARY_DIR}/test-transformations-modules")

  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(test_transformations PRIVATE
        -std=f2008 -Wall -Wextra)
    if(LEPARAGLIDING_RUNTIME_CHECKS)
      target_compile_options(test_transformations PRIVATE
          -fcheck=all -fbacktrace)
    endif()
  endif()

  add_test(NAME transformations COMMAND test_transformations)

  add_executable(test_file_cleanup
      tests/test_file_cleanup.f90
      src/leparagliding_file_cleanup.f90)

  set_target_properties(test_file_cleanup PROPERTIES
      Fortran_MODULE_DIRECTORY
          "${CMAKE_CURRENT_BINARY_DIR}/test-file-cleanup-modules")
  target_include_directories(test_file_cleanup PRIVATE
      "${CMAKE_CURRENT_BINARY_DIR}/test-file-cleanup-modules")

  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(test_file_cleanup PRIVATE
        -std=f2008 -Wall -Wextra)
    if(LEPARAGLIDING_RUNTIME_CHECKS)
      target_compile_options(test_file_cleanup PRIVATE
          -fcheck=all -fbacktrace)
    endif()
  endif()

  add_test(NAME file_cleanup COMMAND test_file_cleanup)

  add_executable(test_polyline_interpolation
      tests/test_polyline_interpolation.f90
      src/leparagliding_polyline_interpolation.f90)

  set_target_properties(test_polyline_interpolation PROPERTIES
      Fortran_MODULE_DIRECTORY
          "${CMAKE_CURRENT_BINARY_DIR}/test-polyline-interpolation-modules")
  target_include_directories(test_polyline_interpolation PRIVATE
      "${CMAKE_CURRENT_BINARY_DIR}/test-polyline-interpolation-modules")

  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(test_polyline_interpolation PRIVATE
        -std=f2008 -Wall -Wextra)
    if(LEPARAGLIDING_RUNTIME_CHECKS)
      target_compile_options(test_polyline_interpolation PRIVATE
          -fcheck=all -fbacktrace)
    endif()
  endif()

  add_test(NAME polyline_interpolation COMMAND test_polyline_interpolation)

  add_executable(test_neutral_development
      tests/test_neutral_development.f90
      src/leparagliding_domain_model.f90
      src/leparagliding_neutral_development.f90)

  set_target_properties(test_neutral_development PROPERTIES
      Fortran_MODULE_DIRECTORY
          "${CMAKE_CURRENT_BINARY_DIR}/test-neutral-development-modules")
  target_include_directories(test_neutral_development PRIVATE
      "${CMAKE_CURRENT_BINARY_DIR}/test-neutral-development-modules")

  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(test_neutral_development PRIVATE
        -std=f2008 -Wall -Wextra)
    if(LEPARAGLIDING_RUNTIME_CHECKS)
      target_compile_options(test_neutral_development PRIVATE
          -fcheck=all -fbacktrace)
    endif()
  endif()

  add_test(NAME neutral_development COMMAND test_neutral_development)

  add_executable(test_skin_tension
      tests/test_skin_tension.f90
      src/leparagliding_domain_model.f90
      src/leparagliding_skin_tension.f90)

  set_target_properties(test_skin_tension PROPERTIES
      Fortran_MODULE_DIRECTORY
          "${CMAKE_CURRENT_BINARY_DIR}/test-skin-tension-modules")
  target_include_directories(test_skin_tension PRIVATE
      "${CMAKE_CURRENT_BINARY_DIR}/test-skin-tension-modules")

  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(test_skin_tension PRIVATE
        -std=f2008 -Wall -Wextra)
    if(LEPARAGLIDING_RUNTIME_CHECKS)
      target_compile_options(test_skin_tension PRIVATE
          -fcheck=all -fbacktrace)
    endif()
  endif()

  add_test(NAME skin_tension COMMAND test_skin_tension)

  add_executable(test_panel_shaping
      tests/test_panel_shaping.f90
      src/leparagliding_domain_model.f90
      src/leparagliding_panel_shaping.f90)

  set_target_properties(test_panel_shaping PROPERTIES
      Fortran_MODULE_DIRECTORY
          "${CMAKE_CURRENT_BINARY_DIR}/test-panel-shaping-modules")
  target_include_directories(test_panel_shaping PRIVATE
      "${CMAKE_CURRENT_BINARY_DIR}/test-panel-shaping-modules")

  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(test_panel_shaping PRIVATE
        -std=f2008 -Wall -Wextra)
    if(LEPARAGLIDING_RUNTIME_CHECKS)
      target_compile_options(test_panel_shaping PRIVATE
          -fcheck=all -fbacktrace)
    endif()
  endif()

  add_test(NAME panel_shaping COMMAND test_panel_shaping)

  add_executable(test_panel_reformat
      tests/test_panel_reformat.f90
      src/leparagliding_domain_model.f90
      src/leparagliding_panel_reformat.f90)

  set_target_properties(test_panel_reformat PROPERTIES
      Fortran_MODULE_DIRECTORY
          "${CMAKE_CURRENT_BINARY_DIR}/test-panel-reformat-modules")
  target_include_directories(test_panel_reformat PRIVATE
      "${CMAKE_CURRENT_BINARY_DIR}/test-panel-reformat-modules")

  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(test_panel_reformat PRIVATE
        -std=f2008 -Wall -Wextra)
    if(LEPARAGLIDING_RUNTIME_CHECKS)
      target_compile_options(test_panel_reformat PRIVATE
          -fcheck=all -fbacktrace)
    endif()
  endif()

  add_test(NAME panel_reformat COMMAND test_panel_reformat)

  add_executable(test_profile_data
      tests/test_profile_data.f90
      src/leparagliding_hvr_config.f90
      src/leparagliding_mark_types.f90
      src/leparagliding_geometry.f90
      src/leparagliding_domain_model.f90
      src/leparagliding_color_geometry.f90
      src/leparagliding_transformations.f90
      src/leparagliding_file_cleanup.f90
      src/leparagliding_polyline_interpolation.f90
      src/leparagliding_procedures.f
      ${LEPARAGLIDING_PROCEDURE_FRAGMENTS})

  set_target_properties(test_profile_data PROPERTIES
      Fortran_MODULE_DIRECTORY
          "${CMAKE_CURRENT_BINARY_DIR}/test-profile-data-modules")
  target_include_directories(test_profile_data PRIVATE
      "${CMAKE_CURRENT_SOURCE_DIR}/src"
      "${CMAKE_CURRENT_BINARY_DIR}/test-profile-data-modules")

  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(test_profile_data PRIVATE
        -std=legacy
        -ffixed-line-length-none
        -fallow-argument-mismatch)
    if(LEPARAGLIDING_ENABLE_WARNINGS)
      target_compile_options(test_profile_data PRIVATE
          -Wall
          -Wextra
          -Wimplicit-interface
          -Wsurprising)
    endif()
    if(LEPARAGLIDING_RUNTIME_CHECKS)
      target_compile_options(test_profile_data PRIVATE -fcheck=all -fbacktrace)
    endif()
  endif()

  add_test(NAME profile_data COMMAND test_profile_data)

  add_executable(test_color_geometry
      tests/test_color_geometry.f90
      src/leparagliding_color_geometry.f90)

  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(test_color_geometry PRIVATE -std=f2008 -Wall -Wextra)
    if(LEPARAGLIDING_RUNTIME_CHECKS)
      target_compile_options(test_color_geometry PRIVATE -fcheck=all -fbacktrace)
    endif()
  endif()

  add_test(NAME color_geometry COMMAND test_color_geometry)

  find_package(Python3 COMPONENTS Interpreter QUIET)
  if(Python3_Interpreter_FOUND)
    add_test(
        NAME dxf_semantic_diff
        COMMAND "${Python3_EXECUTABLE}"
            "${CMAKE_CURRENT_SOURCE_DIR}/tests/test_dxf_semantic_diff.py")

    add_test(
        NAME color_division_import
        COMMAND "${CMAKE_COMMAND}"
            "-DPYTHON=${Python3_EXECUTABLE}"
            "-DIMPORTER=${CMAKE_CURRENT_SOURCE_DIR}/tools/import_color_divisions.py"
            "-DFIXTURE=${CMAKE_CURRENT_SOURCE_DIR}/tests/fixtures/swoop2-color-overlay-sample.dxf"
            "-DEXPECTED=${CMAKE_CURRENT_SOURCE_DIR}/tests/expected/swoop2-section16-sample.txt"
            "-DOUTPUT=${CMAKE_CURRENT_BINARY_DIR}/tests/swoop2-section16-sample.txt"
            -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/run_color_import_test.cmake")
  endif()

  add_test(
      NAME plan_b_regression
      COMMAND "${CMAKE_COMMAND}"
          "-DPROGRAM=$<TARGET_FILE:leparagliding>"
          "-DCASE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/Plan B Parakite"
          "-DWORK_DIR=${CMAKE_CURRENT_BINARY_DIR}/tests/plan-b-regression"
          -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/run_plan_b_regression.cmake")

  add_test(
      NAME even_cell_regression
      COMMAND "${CMAKE_COMMAND}"
          "-DPROGRAM=$<TARGET_FILE:leparagliding>"
          "-DCASE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/tests/fixtures/even-cell-swoop2"
          "-DWORK_DIR=${CMAKE_CURRENT_BINARY_DIR}/tests/even-cell-swoop2-regression"
          -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/run_even_cell_regression.cmake")

  add_test(
      NAME classic_skin_regression
      COMMAND "${CMAKE_COMMAND}"
          "-DPROGRAM=$<TARGET_FILE:leparagliding>"
          "-DCASE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/tests/fixtures/classic-skin-gnua"
          "-DWORK_DIR=${CMAKE_CURRENT_BINARY_DIR}/tests/classic-skin-regression"
          -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/run_classic_skin_regression.cmake")

  add_test(
      NAME disabled_shaping_regression
      COMMAND "${CMAKE_COMMAND}"
          "-DPROGRAM=$<TARGET_FILE:leparagliding>"
          "-DCASE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/tests/fixtures/classic-skin-gnua"
          "-DWORK_DIR=${CMAKE_CURRENT_BINARY_DIR}/tests/disabled-shaping-regression"
          -DDISABLE_SHAPING=ON
          -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/run_classic_skin_regression.cmake")

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
