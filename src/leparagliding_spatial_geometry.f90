! SPDX-License-Identifier: GPL-3.0-or-later
!
!> Define and transform the named rib geometry used to place a wing in space.
!!
!! This module is the Phase-5 foundation for replacing legacy `rib` columns
!! and `u/v/w` transform slots.  It deliberately has no dependency on the
!! numbered main-program includes and performs no compatibility-array writes.
module leparagliding_spatial_geometry
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
  use leparagliding_domain_model, only : rib_identity
  implicit none
  private

  real(real64), parameter :: chord_tolerance = 1.0e-10_real64

  !> One finite point in the legacy model's global spatial coordinate system.
  !!
  !! The provisional X/Y/Z names are retained until their aerodynamic axis
  !! names and positive directions are confirmed by the original author.
  type, public :: point_3d
    real(real64) :: x_cm = 0.0_real64
    real(real64) :: y_cm = 0.0_real64
    real(real64) :: z_cm = 0.0_real64
  contains
    procedure :: is_valid => point_3d_is_valid
  end type point_3d

  !> One point in the rib-local profile plane, in model length units.
  !!
  !! `height_cm` is the value entering the wash-in rotation.  Callers placing
  !! skin points must therefore subtract `profile_vertical_displacement_cm`
  !! before invoking `transform_adjusted_rib_local_point`.  Making that policy
  !! explicit prevents this kernel from silently changing the different
  !! displacement order historically used by Stage-12 singular points.
  type, public :: rib_local_point_2d
    real(real64) :: chordwise_cm = 0.0_real64
    real(real64) :: height_cm = 0.0_real64
  contains
    procedure :: is_valid => rib_local_point_is_valid
  end type rib_local_point_2d

  !> Named definition of one physical or generated legacy rib row.
  !!
  !! The fields quarantine the current meanings of the Stage-6 source columns:
  !! planform station (2), leading/trailing edge and chord (3:5), provisional
  !! spatial station/height (6:7), rotations (8:10,250:251), profile vertical
  !! displacement (50), intake limits (11:12), and profile height scale (160).
  !! Angles are radians and percentages have already become fractions.
  type, public :: rib_definition
    type(rib_identity) :: identity
    integer :: source_rib_number = 0
    real(real64) :: planform_station_cm = 0.0_real64
    real(real64) :: leading_edge_position_cm = 0.0_real64
    real(real64) :: trailing_edge_position_cm = 0.0_real64
    real(real64) :: chord_length_cm = 0.0_real64
    real(real64) :: spatial_station_cm = 0.0_real64
    real(real64) :: spatial_height_cm = 0.0_real64
    real(real64) :: washin_angle_rad = 0.0_real64
    real(real64) :: rib_plane_angle_rad = 0.0_real64
    real(real64) :: washin_pivot_fraction = 0.0_real64
    real(real64) :: profile_rotation_z_rad = 0.0_real64
    real(real64) :: profile_rotation_pivot_fraction = 0.0_real64
    real(real64) :: profile_vertical_displacement_cm = 0.0_real64
    real(real64) :: intake_start_fraction = 0.0_real64
    real(real64) :: intake_end_fraction = 0.0_real64
    real(real64) :: profile_height_scale = 1.0_real64
    logical :: cell_open = .false.
  contains
    procedure :: is_valid => rib_definition_is_valid
  end type rib_definition

  public :: transform_adjusted_rib_local_point

contains

  !> Test whether all three global spatial coordinates are finite.
  pure logical function point_3d_is_valid(point) result(valid)
    class(point_3d), intent(in) :: point

    valid = ieee_is_finite(point%x_cm) .and. &
        ieee_is_finite(point%y_cm) .and. ieee_is_finite(point%z_cm)
  end function point_3d_is_valid

  !> Test whether both rib-local coordinates are finite.
  pure logical function rib_local_point_is_valid(point) result(valid)
    class(rib_local_point_2d), intent(in) :: point

    valid = ieee_is_finite(point%chordwise_cm) .and. &
        ieee_is_finite(point%height_cm)
  end function rib_local_point_is_valid

  !> Validate a named rib definition without imposing unproved design limits.
  !!
  !! Intake positions and rotation pivots are required to be finite but are
  !! not clamped to [0,1]: the legacy input permits a signed intake convention,
  !! and authoritative migration must not reject established extrapolations.
  pure logical function rib_definition_is_valid(definition) result(valid)
    class(rib_definition), intent(in) :: definition
    real(real64) :: expected_chord, scale

    valid = .false.
    if (.not. definition%identity%is_valid()) return
    if (definition%source_rib_number < 1) return
    if (.not. all(ieee_is_finite([ &
        definition%planform_station_cm, &
        definition%leading_edge_position_cm, &
        definition%trailing_edge_position_cm, &
        definition%chord_length_cm, definition%spatial_station_cm, &
        definition%spatial_height_cm, definition%washin_angle_rad, &
        definition%rib_plane_angle_rad, &
        definition%washin_pivot_fraction, &
        definition%profile_rotation_z_rad, &
        definition%profile_rotation_pivot_fraction, &
        definition%profile_vertical_displacement_cm, &
        definition%intake_start_fraction, &
        definition%intake_end_fraction, &
        definition%profile_height_scale]))) return
    if (definition%chord_length_cm <= 0.0_real64) return
    if (definition%profile_height_scale <= 0.0_real64) return

    expected_chord = definition%trailing_edge_position_cm - &
        definition%leading_edge_position_cm
    scale = 1.0_real64 + abs(definition%trailing_edge_position_cm) + &
        abs(definition%leading_edge_position_cm) + &
        abs(definition%chord_length_cm)
    if (abs(expected_chord - definition%chord_length_cm) > &
        chord_tolerance * scale) return
    valid = .true.
  end function rib_definition_is_valid

  !> Transform one displacement-adjusted rib-local point into wing space.
  !!
  !! The expression sequence intentionally matches Stage 6 exactly:
  !! wash-in about its chord pivot, rotation in the legacy local Z plane,
  !! rib-plane rotation, then absolute X/Y/Z placement.  A candidate is built
  !! locally so invalid input or non-finite output leaves `spatial_point`
  !! unchanged.
  !!
  !! @param[in] definition Valid named rib placement and rotation values.
  !! @param[in] local_point Rib-local point entering the wash-in rotation.
  !! @param[inout] spatial_point Transactional global-coordinate destination.
  !! @param[out] valid True only when a finite point was produced.
  !! @param[out] message Empty on success; validation diagnostic on failure.
  pure subroutine transform_adjusted_rib_local_point(definition, local_point, &
      spatial_point, valid, message)
    type(rib_definition), intent(in) :: definition
    type(rib_local_point_2d), intent(in) :: local_point
    type(point_3d), intent(inout) :: spatial_point
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(point_3d) :: candidate
    real(real64) :: washin_pivot_cm, rotation_pivot_cm
    real(real64) :: washin_u, washin_v
    real(real64) :: rotated_u, rotated_v, rotated_w
    real(real64) :: rib_frame_u, rib_frame_v, rib_frame_w

    valid = .false.
    message = ''
    if (.not. definition%is_valid()) then
      message = 'rib definition is invalid'
      return
    end if
    if (.not. local_point%is_valid()) then
      message = 'rib-local point is invalid'
      return
    end if

    washin_pivot_cm = definition%washin_pivot_fraction * &
        definition%chord_length_cm
    washin_u = (local_point%chordwise_cm - washin_pivot_cm) * &
        cos(definition%washin_angle_rad) + local_point%height_cm * &
        sin(definition%washin_angle_rad) + washin_pivot_cm
    washin_v = (-local_point%chordwise_cm + washin_pivot_cm) * &
        sin(definition%washin_angle_rad) + local_point%height_cm * &
        cos(definition%washin_angle_rad)

    rotation_pivot_cm = definition%chord_length_cm * &
        definition%profile_rotation_pivot_fraction
    rotated_w = -washin_u * sin(definition%profile_rotation_z_rad) + &
        rotation_pivot_cm * sin(definition%profile_rotation_z_rad)
    rotated_u = washin_u * cos(definition%profile_rotation_z_rad) + &
        rotation_pivot_cm * &
        (1.0_real64 - cos(definition%profile_rotation_z_rad))
    rotated_v = washin_v

    rib_frame_w = -rotated_w * cos(definition%rib_plane_angle_rad) - &
        rotated_v * sin(definition%rib_plane_angle_rad)
    rib_frame_u = rotated_u
    rib_frame_v = -rotated_w * sin(definition%rib_plane_angle_rad) + &
        rotated_v * cos(definition%rib_plane_angle_rad)

    candidate%x_cm = definition%spatial_station_cm - rib_frame_w
    candidate%y_cm = definition%leading_edge_position_cm + rib_frame_u
    candidate%z_cm = definition%spatial_height_cm - rib_frame_v
    if (.not. candidate%is_valid()) then
      message = 'spatial transform produced a non-finite point'
      return
    end if

    spatial_point = candidate
    valid = .true.
  end subroutine transform_adjusted_rib_local_point

end module leparagliding_spatial_geometry
