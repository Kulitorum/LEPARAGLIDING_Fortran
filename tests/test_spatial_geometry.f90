program test_spatial_geometry
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan
  use leparagliding_domain_model, only : index_range, normalized_profile_2d, &
      rib_identity, rib_role_physical_centerline, &
      rib_role_physical_interior, rib_role_physical_wingtip, &
      rib_role_symmetry_centerline_alias, &
      rib_role_symmetry_mirror_physical, &
      rib_role_tip_extrapolated_support, spatial_rib_geometry_3d
  use leparagliding_spatial_geometry
  implicit none

  real(real64), parameter :: tolerance = 2.0e-12_real64
  real(real64), parameter :: pi = acos(-1.0_real64)
  real(real64) :: legacy_rib(0:5, 300), short_legacy_rib(0:5, 250)
  type(rib_definition) :: definition, saved_definition
  type(rib_local_point_2d) :: local_point
  type(point_3d) :: spatial_point, saved_point
  type(rib_identity) :: identity
  logical :: valid
  character(len=160) :: message

  definition = make_center_definition()
  call require(definition%is_valid(), 'valid rib definition rejected')
  call require(local_point%is_valid(), 'default local point is not finite')
  call require(spatial_point%is_valid(), 'default spatial point is not finite')

  ! The Stage-4 adapter owns the degrees/percent conversion boundary.  The
  ! numeric column references below are an independent legacy-schema oracle.
  legacy_rib = 0.0_real64
  call fill_legacy_rib_row(legacy_rib, 1)
  identity = definition%identity
  call copy_legacy_rib_definition(legacy_rib, identity, 1, definition, &
      valid, message)
  call require(valid, 'authored legacy definition rejected: '//trim(message))
  call require(len_trim(message) == 0, 'successful adapter returned a message')
  call require(definition%identity%legacy_index == 1, &
      'authored identity was not retained')
  call require(definition%source_profile_number == 1, &
      'authored source profile was not retained')
  call require_close(definition%planform_station_cm, 4.5_real64, &
      'planform station adapter')
  call require_close(definition%leading_edge_position_cm, 10.0_real64, &
      'leading-edge adapter')
  call require_close(definition%trailing_edge_position_cm, 20.0_real64, &
      'trailing-edge adapter')
  call require_close(definition%chord_length_cm, 10.0_real64, &
      'chord adapter')
  call require_close(definition%spatial_station_cm, 3.0_real64, &
      'spatial station adapter')
  call require_close(definition%spatial_height_cm, 5.0_real64, &
      'spatial height adapter')
  call require_close(definition%washin_angle_rad, 12.0_real64 * pi / &
      180.0_real64, 'wash-in degree conversion')
  call require_close(definition%rib_plane_angle_rad, -7.5_real64 * pi / &
      180.0_real64, 'rib-plane degree conversion')
  call require_close(definition%washin_pivot_fraction, 0.25_real64, &
      'wash-in pivot percent conversion')
  call require_close(definition%intake_start_fraction, -0.03_real64, &
      'signed intake-start percent conversion')
  call require_close(definition%intake_end_fraction, 0.17_real64, &
      'intake-end percent conversion')
  call require_close(definition%profile_vertical_displacement_cm, &
      0.6_real64, 'profile displacement adapter')
  call require_close(definition%profile_height_scale, 0.95_real64, &
      'profile height-scale adapter')
  call require_close(definition%profile_rotation_z_rad, 23.0_real64 * pi / &
      180.0_real64, 'profile-Z degree conversion')
  call require_close(definition%profile_rotation_pivot_fraction, &
      0.6_real64, 'profile-Z pivot percent conversion')
  call require(definition%cell_open, 'open-cell flag adapter')

  ! Generated rows take their profile provenance from rib_identity and retain
  ! their already-adjusted Stage-4 signs and placement values.
  call fill_legacy_rib_row(legacy_rib, 0)
  legacy_rib(0, 6) = -3.0_real64
  legacy_rib(0, 8) = -12.0_real64
  legacy_rib(0, 9) = 7.5_real64
  legacy_rib(0, 14) = 0.0_real64
  legacy_rib(0, 250) = -23.0_real64
  identity%legacy_index = 0
  identity%role = rib_role_symmetry_mirror_physical
  identity%profile_source_index = 1
  identity%placement_anchor_index = 1
  call copy_legacy_rib_definition(legacy_rib, identity, 1, definition, &
      valid, message)
  call require(valid, 'symmetry legacy definition rejected: '//trim(message))
  call require(definition%identity%legacy_index == 0, &
      'symmetry identity was not retained')
  call require_close(definition%spatial_station_cm, -3.0_real64, &
      'symmetry station sign')
  call require_close(definition%washin_angle_rad, -12.0_real64 * pi / &
      180.0_real64, 'symmetry wash-in sign')
  call require(.not. definition%cell_open, 'closed-cell flag adapter')

  call fill_legacy_rib_row(legacy_rib, 4)
  legacy_rib(4, 6) = 14.0_real64
  identity%legacy_index = 4
  identity%role = rib_role_tip_extrapolated_support
  identity%profile_source_index = 2
  identity%placement_anchor_index = 3
  call copy_legacy_rib_definition(legacy_rib, identity, 2, definition, &
      valid, message)
  call require(valid, 'tip-support legacy definition rejected: '//trim(message))
  call require(definition%source_profile_number == 2, &
      'tip-support profile provenance was not retained')
  call require_close(definition%spatial_station_cm, 14.0_real64, &
      'tip-support placement adapter')

  ! Bounds, finite values, flags, and provenance all fail before publication.
  saved_definition = definition
  identity%legacy_index = 6
  identity%profile_source_index = 4
  identity%placement_anchor_index = 5
  call copy_legacy_rib_definition(legacy_rib, identity, 4, definition, &
      valid, message)
  call require(.not. valid, 'out-of-bounds legacy row accepted')
  call require_same_definition(definition, saved_definition, &
      'row-bounds failure changed destination')

  short_legacy_rib = 0.0_real64
  identity%legacy_index = 1
  identity%role = rib_role_physical_centerline
  identity%profile_source_index = 1
  identity%placement_anchor_index = 1
  call copy_legacy_rib_definition(short_legacy_rib, identity, 1, &
      definition, valid, message)
  call require(.not. valid, 'short legacy row accepted')
  call require_same_definition(definition, saved_definition, &
      'column-bounds failure changed destination')

  legacy_rib(1, 7) = ieee_value(0.0_real64, ieee_quiet_nan)
  call copy_legacy_rib_definition(legacy_rib, identity, 1, definition, &
      valid, message)
  call require(.not. valid, 'non-finite legacy definition accepted')
  call require(len_trim(message) > 0, 'adapter failure has no diagnostic')
  call require_same_definition(definition, saved_definition, &
      'non-finite failure changed destination')
  call fill_legacy_rib_row(legacy_rib, 1)

  call copy_legacy_rib_definition(legacy_rib, identity, 2, definition, &
      valid, message)
  call require(.not. valid, 'mismatched profile provenance accepted')
  call require_same_definition(definition, saved_definition, &
      'provenance failure changed destination')

  legacy_rib(1, 14) = 2.0_real64
  call copy_legacy_rib_definition(legacy_rib, identity, 1, definition, &
      valid, message)
  call require(.not. valid, 'invalid cell-open flag accepted')
  call require_same_definition(definition, saved_definition, &
      'cell-open failure changed destination')
  call fill_legacy_rib_row(legacy_rib, 1)

  identity%role = 0
  call copy_legacy_rib_definition(legacy_rib, identity, 1, definition, &
      valid, message)
  call require(.not. valid, 'invalid identity accepted by adapter')
  call require_same_definition(definition, saved_definition, &
      'identity failure changed destination')

  definition = make_center_definition()

  ! With no rotations, the local chord direction enters global Y, local height
  ! is subtracted from global Z, and the spatial station supplies global X.
  local_point%chordwise_cm = 2.5_real64
  local_point%height_cm = 1.0_real64
  call transform_adjusted_rib_local_point(definition, local_point, &
      spatial_point, valid, message)
  call require(valid, 'identity transform rejected: '//trim(message))
  call require_close(spatial_point%x_cm, 3.0_real64, 'identity spatial X')
  call require_close(spatial_point%y_cm, 12.5_real64, 'identity spatial Y')
  call require_close(spatial_point%z_cm, 4.0_real64, 'identity spatial Z')

  ! The public kernel accepts the point entering wash-in.  It must not apply
  ! the definition's displacement a second time.
  definition%profile_vertical_displacement_cm = 4.0_real64
  call transform_adjusted_rib_local_point(definition, local_point, &
      spatial_point, valid, message)
  call require(valid, 'explicitly adjusted point rejected')
  call require_close(spatial_point%z_cm, 4.0_real64, &
      'transform reapplied profile displacement')
  local_point%height_cm = -3.0_real64
  call transform_adjusted_rib_local_point(definition, local_point, &
      spatial_point, valid, message)
  call require(valid, 'displacement-adjusted point rejected')
  call require_close(spatial_point%z_cm, 8.0_real64, &
      'adjusted local height did not control spatial Z')

  ! Wash-in uses the named chord pivot and retains the historical sign order.
  definition = make_center_definition()
  definition%washin_angle_rad = 0.5_real64 * pi
  definition%washin_pivot_fraction = 0.2_real64
  local_point%chordwise_cm = 4.0_real64
  local_point%height_cm = 2.0_real64
  call transform_adjusted_rib_local_point(definition, local_point, &
      spatial_point, valid, message)
  call require(valid, 'wash-in transform rejected')
  call require_close(spatial_point%x_cm, 3.0_real64, 'wash-in spatial X')
  call require_close(spatial_point%y_cm, 14.0_real64, 'wash-in spatial Y')
  call require_close(spatial_point%z_cm, 7.0_real64, 'wash-in spatial Z')

  ! The legacy local-Z rotation is about its own chordwise pivot.
  definition = make_center_definition()
  definition%profile_rotation_z_rad = 0.5_real64 * pi
  definition%profile_rotation_pivot_fraction = 0.3_real64
  local_point%chordwise_cm = 5.0_real64
  local_point%height_cm = 2.0_real64
  call transform_adjusted_rib_local_point(definition, local_point, &
      spatial_point, valid, message)
  call require(valid, 'profile-Z transform rejected')
  call require_close(spatial_point%x_cm, 1.0_real64, 'profile-Z spatial X')
  call require_close(spatial_point%y_cm, 13.0_real64, 'profile-Z spatial Y')
  call require_close(spatial_point%z_cm, 3.0_real64, 'profile-Z spatial Z')

  ! Rib-plane rotation retains the Stage-6 X/Z signs.
  definition = make_center_definition()
  definition%rib_plane_angle_rad = 0.5_real64 * pi
  local_point%chordwise_cm = 5.0_real64
  local_point%height_cm = 2.0_real64
  call transform_adjusted_rib_local_point(definition, local_point, &
      spatial_point, valid, message)
  call require(valid, 'rib-plane transform rejected')
  call require_close(spatial_point%x_cm, 5.0_real64, &
      'rib-plane spatial X')
  call require_close(spatial_point%y_cm, 15.0_real64, &
      'rib-plane spatial Y')
  call require_close(spatial_point%z_cm, 5.0_real64, &
      'rib-plane spatial Z')

  ! Freeze one compound, non-axis-aligned Stage-6 transform as a numeric
  ! oracle so later algebraic simplification cannot reorder the calculation.
  definition = make_center_definition()
  definition%leading_edge_position_cm = -3.5_real64
  definition%trailing_edge_position_cm = 13.75_real64
  definition%chord_length_cm = 17.25_real64
  definition%spatial_station_cm = 8.75_real64
  definition%spatial_height_cm = 12.5_real64
  definition%washin_angle_rad = 13.5_real64 * pi / 180.0_real64
  definition%washin_pivot_fraction = 0.37_real64
  definition%profile_rotation_z_rad = -21.25_real64 * pi / 180.0_real64
  definition%profile_rotation_pivot_fraction = 0.61_real64
  definition%rib_plane_angle_rad = 34.0_real64 * pi / 180.0_real64
  local_point%chordwise_cm = 11.125_real64
  local_point%height_cm = -2.875_real64
  call transform_adjusted_rib_local_point(definition, local_point, &
      spatial_point, valid, message)
  call require(valid, 'compound transform rejected')
  call require_close(spatial_point%x_cm, 6.507647943119603_real64, &
      'compound spatial X')
  call require_close(spatial_point%y_cm, 6.836386347979504_real64, &
      'compound spatial Y')
  call require_close(spatial_point%z_cm, 15.69499488277239_real64, &
      'compound spatial Z')

  ! Signed intake and out-of-range pivot fractions are compatibility values,
  ! not reasons to reject an otherwise finite legacy definition.
  definition%intake_start_fraction = -0.03_real64
  definition%profile_rotation_pivot_fraction = 1.2_real64
  call require(definition%is_valid(), &
      'signed intake or extrapolated pivot was rejected')

  ! Generated-row identities remain explicit and valid at this foundation.
  definition = make_center_definition()
  definition%identity%legacy_index = 0
  definition%identity%role = rib_role_symmetry_mirror_physical
  definition%identity%profile_source_index = 1
  definition%identity%placement_anchor_index = 1
  call require(definition%is_valid(), 'symmetry definition rejected')
  definition%identity%legacy_index = 4
  definition%identity%role = rib_role_tip_extrapolated_support
  definition%identity%profile_source_index = 2
  definition%identity%placement_anchor_index = 3
  definition%source_profile_number = 2
  call require(definition%is_valid(), 'tip-support definition rejected')

  ! Invalid input is transactional: neither the object nor a failed transform
  ! can leak a partially updated point.
  definition = make_center_definition()
  call require(definition%is_valid(), 'baseline for invalid checks')

  definition%source_profile_number = 0
  call require(.not. definition%is_valid(), 'zero source profile number accepted')
  definition = make_center_definition()
  definition%chord_length_cm = 0.0_real64
  call require(.not. definition%is_valid(), 'zero chord accepted')
  definition = make_center_definition()
  definition%trailing_edge_position_cm = 19.0_real64
  call require(.not. definition%is_valid(), 'inconsistent chord accepted')
  definition = make_center_definition()
  definition%profile_height_scale = 0.0_real64
  call require(definition%is_valid(), 'zero-height wingtip profile rejected')

  definition = make_center_definition()
  local_point%chordwise_cm = ieee_value(0.0_real64, ieee_quiet_nan)
  local_point%height_cm = 1.0_real64
  call require(.not. local_point%is_valid(), 'non-finite local point accepted')
  saved_point%x_cm = 71.0_real64
  saved_point%y_cm = 72.0_real64
  saved_point%z_cm = 73.0_real64
  spatial_point = saved_point
  call transform_adjusted_rib_local_point(definition, local_point, &
      spatial_point, valid, message)
  call require(.not. valid, 'non-finite local transform accepted')
  call require(len_trim(message) > 0, 'failed transform has no diagnostic')
  call require_close(spatial_point%x_cm, saved_point%x_cm, &
      'failed transform changed X')
  call require_close(spatial_point%y_cm, saved_point%y_cm, &
      'failed transform changed Y')
  call require_close(spatial_point%z_cm, saved_point%z_cm, &
      'failed transform changed Z')

  spatial_point%x_cm = ieee_value(0.0_real64, ieee_quiet_nan)
  call require(.not. spatial_point%is_valid(), 'non-finite spatial point accepted')

  call test_complete_profile_construction()

  write (*, '(A)') 'PASS: typed spatial-geometry foundation'

contains

  function make_center_definition() result(candidate)
    type(rib_definition) :: candidate

    candidate%identity%legacy_index = 1
    candidate%identity%role = rib_role_physical_centerline
    candidate%identity%profile_source_index = 1
    candidate%identity%placement_anchor_index = 1
    candidate%source_profile_number = 1
    candidate%planform_station_cm = 0.0_real64
    candidate%leading_edge_position_cm = 10.0_real64
    candidate%trailing_edge_position_cm = 20.0_real64
    candidate%chord_length_cm = 10.0_real64
    candidate%spatial_station_cm = 3.0_real64
    candidate%spatial_height_cm = 5.0_real64
    candidate%washin_angle_rad = 0.0_real64
    candidate%rib_plane_angle_rad = 0.0_real64
    candidate%washin_pivot_fraction = 0.0_real64
    candidate%profile_rotation_z_rad = 0.0_real64
    candidate%profile_rotation_pivot_fraction = 0.0_real64
    candidate%profile_vertical_displacement_cm = 0.0_real64
    candidate%intake_start_fraction = 0.1_real64
    candidate%intake_end_fraction = 0.2_real64
    candidate%profile_height_scale = 1.0_real64
    candidate%cell_open = .true.
  end function make_center_definition

  function make_normalized_profile(rib_index) result(profile)
    integer, intent(in) :: rib_index
    type(normalized_profile_2d) :: profile

    profile%rib_index = rib_index
    profile%topology%point_count = 6
    profile%topology%extrados = index_range(1, 3)
    profile%topology%intake = index_range(3, 4)
    profile%topology%intrados = index_range(4, 6)
    profile%topology%leading_edge_index = 3
    profile%chord_percent = [100.0_real64, 50.0_real64, 0.0_real64, &
        10.0_real64, 50.0_real64, 100.0_real64]
    profile%height_percent = [0.0_real64, 5.0_real64, 0.0_real64, &
        -2.0_real64, -4.0_real64, 0.0_real64]
  end function make_normalized_profile

  subroutine test_complete_profile_construction()
    type(normalized_profile_2d) :: profile
    type(rib_definition) :: source_definition, anchor_definition
    type(rib_definition) :: generated_definition, saved_definition
    type(spatial_rib_geometry_3d) :: geometry, source_geometry
    type(spatial_rib_geometry_3d) :: saved_geometry, invalid_geometry
    type(rib_identity) :: generated_identity, invalid_identity
    real(real64) :: legacy_x(0:5, 10), legacy_y(0:5, 10)
    real(real64) :: legacy_z(0:5, 10), short_legacy_z(0:5, 9)
    real(real64) :: saved_legacy_x(0:5, 10)
    logical :: construction_valid
    character(len=160) :: construction_message

    profile = make_normalized_profile(1)
    call require(profile%is_valid(), 'complete-profile test profile invalid')
    source_definition = make_center_definition()
    source_definition%profile_vertical_displacement_cm = 1.0_real64
    call build_spatial_rib_geometry(profile, source_definition, geometry, &
        construction_valid, construction_message)
    call require(construction_valid, 'complete profile rejected: '// &
        trim(construction_message))
    call require(geometry%rib_index == 1, 'complete profile target index')
    call require(all(abs(geometry%x - 3.0_real64) <= tolerance), &
        'complete profile X coordinates')
    call require(all(abs(geometry%y - [20.0_real64, 15.0_real64, &
        10.0_real64, 11.0_real64, 15.0_real64, 20.0_real64]) <= &
        tolerance), 'complete profile Y coordinates')
    call require(all(abs(geometry%z - [6.0_real64, 5.5_real64, &
        6.0_real64, 6.2_real64, 6.4_real64, 6.0_real64]) <= tolerance), &
        'complete profile displacement/Z coordinates')

    saved_geometry = geometry
    profile%rib_index = 2
    call build_spatial_rib_geometry(profile, source_definition, geometry, &
        construction_valid, construction_message)
    call require(.not. construction_valid, &
        'mismatched complete-profile provenance accepted')
    call require_same_geometry(geometry, saved_geometry, &
        'failed complete-profile build changed destination')

    profile = make_normalized_profile(1)
    source_definition = make_center_definition()
    source_definition%planform_station_cm = 4.5_real64
    source_definition%rib_plane_angle_rad = 0.2_real64
    source_definition%profile_rotation_z_rad = -0.3_real64
    source_definition%profile_vertical_displacement_cm = 1.25_real64
    source_definition%profile_height_scale = 0.85_real64
    generated_identity%legacy_index = 0
    generated_identity%role = rib_role_symmetry_mirror_physical
    generated_identity%profile_source_index = 1
    generated_identity%placement_anchor_index = 1
    call build_symmetry_rib_definition(source_definition, generated_identity, &
        generated_definition, construction_valid, construction_message)
    call require(construction_valid, 'symmetry definition rejected: '// &
        trim(construction_message))
    call require_close(generated_definition%planform_station_cm, -4.5_real64, &
        'symmetry planform station')
    call require_close(generated_definition%spatial_station_cm, -3.0_real64, &
        'symmetry spatial station')
    call require_close(generated_definition%rib_plane_angle_rad, -0.2_real64, &
        'symmetry rib-plane angle')
    call require_close(generated_definition%profile_rotation_z_rad, &
        0.3_real64, 'symmetry profile-Z angle')
    call require_close(generated_definition%profile_vertical_displacement_cm, &
        1.25_real64, 'symmetry source displacement')
    call require_close(generated_definition%profile_height_scale, 0.85_real64, &
        'symmetry source height scale')

    saved_definition = generated_definition
    invalid_identity = generated_identity
    invalid_identity%role = rib_role_physical_centerline
    call build_symmetry_rib_definition(source_definition, invalid_identity, &
        generated_definition, construction_valid, construction_message)
    call require(.not. construction_valid, &
        'nonsymmetry identity accepted by symmetry constructor')
    call require_same_definition(generated_definition, saved_definition, &
        'failed symmetry definition changed destination')

    generated_identity%role = rib_role_symmetry_centerline_alias
    call build_symmetry_rib_definition(source_definition, generated_identity, &
        generated_definition, construction_valid, construction_message)
    call require(construction_valid, 'centerline-alias definition rejected')
    call build_spatial_rib_geometry(profile, source_definition, &
        source_geometry, construction_valid, construction_message)
    call require(construction_valid, 'symmetry source geometry rejected')
    call build_symmetry_spatial_rib(source_geometry, generated_definition, &
        geometry, construction_valid, construction_message)
    call require(construction_valid, 'symmetry geometry rejected: '// &
        trim(construction_message))
    call require(all(abs(geometry%x + source_geometry%x) <= tolerance), &
        'symmetry geometry X mirror')
    call require(all(abs(geometry%y - source_geometry%y) <= tolerance), &
        'symmetry geometry changed Y')
    call require(all(abs(geometry%z - source_geometry%z) <= tolerance), &
        'symmetry geometry changed Z')

    saved_geometry = geometry
    invalid_geometry = source_geometry
    invalid_geometry%rib_index = 2
    call build_symmetry_spatial_rib(invalid_geometry, generated_definition, &
        geometry, construction_valid, construction_message)
    call require(.not. construction_valid, &
        'wrong-source symmetry geometry accepted')
    call require_same_geometry(geometry, saved_geometry, &
        'failed symmetry geometry changed destination')

    source_definition = make_center_definition()
    source_definition%identity%legacy_index = 2
    source_definition%identity%role = rib_role_physical_interior
    source_definition%identity%profile_source_index = 2
    source_definition%identity%placement_anchor_index = 2
    source_definition%source_profile_number = 2
    source_definition%planform_station_cm = 8.0_real64
    source_definition%spatial_station_cm = 10.0_real64
    source_definition%spatial_height_cm = 4.0_real64
    source_definition%profile_vertical_displacement_cm = 0.75_real64
    anchor_definition = source_definition
    anchor_definition%identity%legacy_index = 3
    anchor_definition%identity%role = rib_role_physical_wingtip
    anchor_definition%identity%profile_source_index = 3
    anchor_definition%identity%placement_anchor_index = 3
    anchor_definition%source_profile_number = 3
    anchor_definition%planform_station_cm = 12.0_real64
    anchor_definition%spatial_station_cm = 15.0_real64
    anchor_definition%spatial_height_cm = 7.0_real64
    generated_identity%legacy_index = 4
    generated_identity%role = rib_role_tip_extrapolated_support
    generated_identity%profile_source_index = 2
    generated_identity%placement_anchor_index = 3
    call build_tip_support_rib_definition(source_definition, &
        anchor_definition, generated_identity, generated_definition, &
        construction_valid, construction_message)
    call require(construction_valid, 'tip-support definition rejected: '// &
        trim(construction_message))
    call require_close(generated_definition%spatial_station_cm, 20.0_real64, &
        'tip-support extrapolated station')
    call require_close(generated_definition%spatial_height_cm, 10.0_real64, &
        'tip-support extrapolated height')
    call require(generated_definition%source_profile_number == 2, &
        'tip-support source profile')
    call require_close(generated_definition%profile_vertical_displacement_cm, &
        0.75_real64, 'tip-support source displacement')

    saved_definition = generated_definition
    invalid_identity = generated_identity
    invalid_identity%placement_anchor_index = 2
    call build_tip_support_rib_definition(source_definition, &
        anchor_definition, invalid_identity, generated_definition, &
        construction_valid, construction_message)
    call require(.not. construction_valid, &
        'invalid tip-support provenance accepted')
    call require_same_definition(generated_definition, saved_definition, &
        'failed tip-support definition changed destination')

    profile = make_normalized_profile(2)
    call build_tip_support_spatial_rib(profile, generated_definition, &
        geometry, construction_valid, construction_message)
    call require(construction_valid, 'tip-support geometry rejected: '// &
        trim(construction_message))
    call require(geometry%rib_index == 4, 'tip-support geometry target index')
    call require(all(abs(geometry%x - 20.0_real64) <= tolerance), &
        'tip-support geometry placement station')

    legacy_x = -9.0_real64
    legacy_y = -8.0_real64
    legacy_z = -7.0_real64
    call write_legacy_spatial_rib_geometry(geometry, legacy_x, legacy_y, &
        legacy_z, construction_valid, construction_message)
    call require(construction_valid, 'typed spatial publication rejected: '// &
        trim(construction_message))
    call require(all(abs(legacy_x(4, 1:6) - geometry%x) <= tolerance), &
        'typed spatial publication X')
    call require(all(abs(legacy_x(4, 7:10) + 9.0_real64) <= 0.0_real64), &
        'typed spatial publication changed unused suffix')
    call require(all(abs(legacy_x(3, :) + 9.0_real64) <= 0.0_real64), &
        'typed spatial publication changed another row')

    saved_legacy_x = legacy_x
    invalid_geometry = geometry
    invalid_geometry%rib_index = 6
    call write_legacy_spatial_rib_geometry(invalid_geometry, legacy_x, &
        legacy_y, legacy_z, construction_valid, construction_message)
    call require(.not. construction_valid, &
        'out-of-bounds typed spatial rib published')
    call require(all(abs(legacy_x - saved_legacy_x) <= 0.0_real64), &
        'failed spatial publication changed destination')
    short_legacy_z = -6.0_real64
    call write_legacy_spatial_rib_geometry(geometry, legacy_x, legacy_y, &
        short_legacy_z, construction_valid, construction_message)
    call require(.not. construction_valid, &
        'mismatched legacy spatial shapes accepted')
    call require(all(abs(legacy_x - saved_legacy_x) <= 0.0_real64), &
        'shape failure changed spatial destination')
  end subroutine test_complete_profile_construction

  subroutine fill_legacy_rib_row(legacy, rib_index)
    real(real64), intent(inout) :: legacy(0:,:)
    integer, intent(in) :: rib_index

    legacy(rib_index, 2) = 4.5_real64
    legacy(rib_index, 3) = 10.0_real64
    legacy(rib_index, 4) = 20.0_real64
    legacy(rib_index, 5) = 10.0_real64
    legacy(rib_index, 6) = 3.0_real64
    legacy(rib_index, 7) = 5.0_real64
    legacy(rib_index, 8) = 12.0_real64
    legacy(rib_index, 9) = -7.5_real64
    legacy(rib_index, 10) = 25.0_real64
    legacy(rib_index, 11) = -3.0_real64
    legacy(rib_index, 12) = 17.0_real64
    legacy(rib_index, 14) = 1.0_real64
    legacy(rib_index, 50) = 0.6_real64
    legacy(rib_index, 160) = 0.95_real64
    legacy(rib_index, 250) = 23.0_real64
    legacy(rib_index, 251) = 60.0_real64
  end subroutine fill_legacy_rib_row

  subroutine require_same_definition(actual, expected, diagnostic)
    type(rib_definition), intent(in) :: actual, expected
    character(len=*), intent(in) :: diagnostic

    call require(actual%identity%legacy_index == &
        expected%identity%legacy_index .and. &
        actual%identity%role == expected%identity%role .and. &
        actual%identity%profile_source_index == &
            expected%identity%profile_source_index .and. &
        actual%identity%placement_anchor_index == &
            expected%identity%placement_anchor_index .and. &
        actual%source_profile_number == expected%source_profile_number .and. &
        (actual%cell_open .eqv. expected%cell_open), diagnostic)
    call require(all(abs([ &
        actual%planform_station_cm, actual%leading_edge_position_cm, &
        actual%trailing_edge_position_cm, actual%chord_length_cm, &
        actual%spatial_station_cm, actual%spatial_height_cm, &
        actual%washin_angle_rad, actual%rib_plane_angle_rad, &
        actual%washin_pivot_fraction, actual%profile_rotation_z_rad, &
        actual%profile_rotation_pivot_fraction, &
        actual%profile_vertical_displacement_cm, &
        actual%intake_start_fraction, actual%intake_end_fraction, &
        actual%profile_height_scale] - [ &
        expected%planform_station_cm, expected%leading_edge_position_cm, &
        expected%trailing_edge_position_cm, expected%chord_length_cm, &
        expected%spatial_station_cm, expected%spatial_height_cm, &
        expected%washin_angle_rad, expected%rib_plane_angle_rad, &
        expected%washin_pivot_fraction, expected%profile_rotation_z_rad, &
        expected%profile_rotation_pivot_fraction, &
        expected%profile_vertical_displacement_cm, &
        expected%intake_start_fraction, expected%intake_end_fraction, &
        expected%profile_height_scale]) <= 0.0_real64), diagnostic)
  end subroutine require_same_definition

  subroutine require_same_geometry(actual, expected, diagnostic)
    type(spatial_rib_geometry_3d), intent(in) :: actual, expected
    character(len=*), intent(in) :: diagnostic

    call require(actual%rib_index == expected%rib_index, diagnostic)
    call require(allocated(actual%x) .and. allocated(actual%y) .and. &
        allocated(actual%z), diagnostic)
    call require(all(shape(actual%x) == shape(expected%x)) .and. &
        all(shape(actual%y) == shape(expected%y)) .and. &
        all(shape(actual%z) == shape(expected%z)), diagnostic)
    call require(all(abs(actual%x - expected%x) <= 0.0_real64) .and. &
        all(abs(actual%y - expected%y) <= 0.0_real64) .and. &
        all(abs(actual%z - expected%z) <= 0.0_real64), &
        diagnostic)
  end subroutine require_same_geometry

  subroutine require(condition, diagnostic)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: diagnostic

    if (.not. condition) then
      write (*, '(2A)') 'FAIL: ', trim(diagnostic)
      error stop 1
    end if
  end subroutine require

  subroutine require_close(actual, expected, diagnostic)
    real(real64), intent(in) :: actual, expected
    character(len=*), intent(in) :: diagnostic

    call require(abs(actual - expected) <= &
        tolerance * (1.0_real64 + abs(expected)), diagnostic)
  end subroutine require_close

end program test_spatial_geometry
