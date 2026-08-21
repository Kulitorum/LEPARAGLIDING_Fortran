program test_spatial_geometry
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan
  use leparagliding_domain_model, only : rib_identity, &
      rib_role_physical_centerline, rib_role_symmetry_mirror_physical, &
      rib_role_tip_extrapolated_support
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
  call require(.not. definition%is_valid(), 'zero profile scale accepted')

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
