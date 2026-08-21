program test_spatial_geometry
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan
  use leparagliding_domain_model, only : rib_role_physical_centerline, &
      rib_role_symmetry_mirror_physical, rib_role_tip_extrapolated_support
  use leparagliding_spatial_geometry
  implicit none

  real(real64), parameter :: tolerance = 2.0e-12_real64
  real(real64), parameter :: pi = acos(-1.0_real64)
  type(rib_definition) :: definition
  type(rib_local_point_2d) :: local_point
  type(point_3d) :: spatial_point, saved_point
  logical :: valid
  character(len=160) :: message

  definition = make_center_definition()
  call require(definition%is_valid(), 'valid rib definition rejected')
  call require(local_point%is_valid(), 'default local point is not finite')
  call require(spatial_point%is_valid(), 'default spatial point is not finite')

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
  call require(definition%is_valid(), 'tip-support definition rejected')

  ! Invalid input is transactional: neither the object nor a failed transform
  ! can leak a partially updated point.
  definition = make_center_definition()
  call require(definition%is_valid(), 'baseline for invalid checks')

  definition%source_rib_number = 0
  call require(.not. definition%is_valid(), 'zero source number accepted')
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
    candidate%source_rib_number = 1
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
