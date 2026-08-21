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
  use leparagliding_domain_model, only : normalized_profile_2d, &
      rib_identity, rib_role_symmetry_centerline_alias, &
      rib_role_symmetry_mirror_physical, rib_role_tip_extrapolated_support, &
      spatial_rib_geometry_3d
  implicit none
  private

  real(real64), parameter :: chord_tolerance = 1.0e-10_real64
  real(real64), parameter :: integer_value_tolerance = 1.0e-10_real64
  real(real64), parameter :: pi = 4.0_real64 * atan(1.0_real64)
  integer, parameter :: legacy_planform_station_column = 2
  integer, parameter :: legacy_leading_edge_position_column = 3
  integer, parameter :: legacy_trailing_edge_position_column = 4
  integer, parameter :: legacy_chord_length_column = 5
  integer, parameter :: legacy_spatial_station_column = 6
  integer, parameter :: legacy_spatial_height_column = 7
  integer, parameter :: legacy_washin_angle_degrees_column = 8
  integer, parameter :: legacy_rib_plane_angle_degrees_column = 9
  integer, parameter :: legacy_washin_pivot_percent_column = 10
  integer, parameter :: legacy_intake_start_percent_column = 11
  integer, parameter :: legacy_intake_end_percent_column = 12
  integer, parameter :: legacy_cell_open_flag_column = 14
  integer, parameter :: legacy_profile_vertical_displacement_column = 50
  integer, parameter :: legacy_profile_height_scale_column = 160
  integer, parameter :: legacy_profile_rotation_z_degrees_column = 250
  integer, parameter :: legacy_profile_rotation_pivot_percent_column = 251
  integer, parameter :: required_legacy_rib_column_count = &
      legacy_profile_rotation_pivot_percent_column

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
    integer :: source_profile_number = 0
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

  public :: copy_legacy_rib_definition
  public :: build_spatial_rib_geometry
  public :: build_symmetry_rib_definition
  public :: build_symmetry_spatial_rib
  public :: build_tip_support_rib_definition
  public :: build_tip_support_spatial_rib
  public :: transform_adjusted_rib_local_point
  public :: write_legacy_spatial_rib_geometry

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
  !! Profile height scale is likewise only required to be finite because the
  !! maintained Plan B wingtip deliberately uses zero to collapse its height.
  pure logical function rib_definition_is_valid(definition) result(valid)
    class(rib_definition), intent(in) :: definition
    real(real64) :: expected_chord, scale

    valid = .false.
    if (.not. definition%identity%is_valid()) return
    if (definition%source_profile_number < 1) return
    if (definition%source_profile_number /= &
        definition%identity%profile_source_index) return
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

    expected_chord = definition%trailing_edge_position_cm - &
        definition%leading_edge_position_cm
    scale = 1.0_real64 + abs(definition%trailing_edge_position_cm) + &
        abs(definition%leading_edge_position_cm) + &
        abs(definition%chord_length_cm)
    if (abs(expected_chord - definition%chord_length_cm) > &
        chord_tolerance * scale) return
    valid = .true.
  end function rib_definition_is_valid

  !> Copy one Stage-4 legacy rib row into a named, canonical definition.
  !!
  !! The legacy length values have already received the run's `xwf` unit
  !! conversion before this adapter is called.  This boundary converts degree
  !! angles to radians and percentage values to fractions exactly once.
  !! `source_profile_number` is supplied independently because generated rows
  !! obtain their profile from `rib_identity`, rather than inferring ownership
  !! from the row index.  Failure leaves `definition` unchanged.
  !!
  !! @param[in] legacy_rib Stage-4 legacy rib matrix with a zero-based row.
  !! @param[in] identity Valid authored or generated identity for that row.
  !! @param[in] source_profile_number Profile source recorded by the caller.
  !! @param[inout] definition Transactional typed destination.
  !! @param[out] valid True only when a complete valid definition was copied.
  !! @param[out] message Empty on success; validation diagnostic on failure.
  pure subroutine copy_legacy_rib_definition(legacy_rib, identity, &
      source_profile_number, definition, valid, message)
    real(real64), intent(in) :: legacy_rib(0:,:)
    type(rib_identity), intent(in) :: identity
    integer, intent(in) :: source_profile_number
    type(rib_definition), intent(inout) :: definition
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(rib_definition) :: candidate
    real(real64) :: cell_open_value
    integer :: cell_open_integer, rib_index

    valid = .false.
    message = ''
    if (.not. identity%is_valid()) then
      message = 'rib identity is invalid'
      return
    end if
    rib_index = identity%legacy_index
    if (rib_index < 0 .or. rib_index > ubound(legacy_rib, 1)) then
      message = 'rib identity is outside the legacy row bounds'
      return
    end if
    if (ubound(legacy_rib, 2) < required_legacy_rib_column_count) then
      message = 'legacy rib row lacks required spatial-definition columns'
      return
    end if
    if (source_profile_number < 1 .or. &
        source_profile_number /= identity%profile_source_index) then
      message = 'source profile disagrees with rib identity provenance'
      return
    end if
    if (.not. all(ieee_is_finite([ &
        legacy_rib(rib_index, legacy_planform_station_column), &
        legacy_rib(rib_index, legacy_leading_edge_position_column), &
        legacy_rib(rib_index, legacy_trailing_edge_position_column), &
        legacy_rib(rib_index, legacy_chord_length_column), &
        legacy_rib(rib_index, legacy_spatial_station_column), &
        legacy_rib(rib_index, legacy_spatial_height_column), &
        legacy_rib(rib_index, legacy_washin_angle_degrees_column), &
        legacy_rib(rib_index, legacy_rib_plane_angle_degrees_column), &
        legacy_rib(rib_index, legacy_washin_pivot_percent_column), &
        legacy_rib(rib_index, legacy_intake_start_percent_column), &
        legacy_rib(rib_index, legacy_intake_end_percent_column), &
        legacy_rib(rib_index, legacy_cell_open_flag_column), &
        legacy_rib(rib_index, &
            legacy_profile_vertical_displacement_column), &
        legacy_rib(rib_index, legacy_profile_height_scale_column), &
        legacy_rib(rib_index, &
            legacy_profile_rotation_z_degrees_column), &
        legacy_rib(rib_index, &
            legacy_profile_rotation_pivot_percent_column)]))) then
      message = 'legacy rib definition contains a non-finite value'
      return
    end if

    cell_open_value = legacy_rib(rib_index, legacy_cell_open_flag_column)
    cell_open_integer = nint(cell_open_value)
    if (abs(cell_open_value - real(cell_open_integer, real64)) > &
        integer_value_tolerance .or. cell_open_integer < 0 .or. &
        cell_open_integer > 1) then
      message = 'legacy cell-open flag is not zero or one'
      return
    end if

    candidate%identity = identity
    candidate%source_profile_number = source_profile_number
    candidate%planform_station_cm = &
        legacy_rib(rib_index, legacy_planform_station_column)
    candidate%leading_edge_position_cm = &
        legacy_rib(rib_index, legacy_leading_edge_position_column)
    candidate%trailing_edge_position_cm = &
        legacy_rib(rib_index, legacy_trailing_edge_position_column)
    candidate%chord_length_cm = &
        legacy_rib(rib_index, legacy_chord_length_column)
    candidate%spatial_station_cm = &
        legacy_rib(rib_index, legacy_spatial_station_column)
    candidate%spatial_height_cm = &
        legacy_rib(rib_index, legacy_spatial_height_column)
    candidate%washin_angle_rad = &
        legacy_rib(rib_index, legacy_washin_angle_degrees_column) * pi / &
        180.0_real64
    candidate%rib_plane_angle_rad = &
        legacy_rib(rib_index, legacy_rib_plane_angle_degrees_column) * pi / &
        180.0_real64
    candidate%washin_pivot_fraction = &
        legacy_rib(rib_index, legacy_washin_pivot_percent_column) / &
        100.0_real64
    candidate%profile_rotation_z_rad = &
        legacy_rib(rib_index, &
            legacy_profile_rotation_z_degrees_column) * pi / 180.0_real64
    candidate%profile_rotation_pivot_fraction = &
        legacy_rib(rib_index, &
            legacy_profile_rotation_pivot_percent_column) / 100.0_real64
    candidate%profile_vertical_displacement_cm = &
        legacy_rib(rib_index, &
            legacy_profile_vertical_displacement_column)
    candidate%intake_start_fraction = &
        legacy_rib(rib_index, legacy_intake_start_percent_column) / &
        100.0_real64
    candidate%intake_end_fraction = &
        legacy_rib(rib_index, legacy_intake_end_percent_column) / &
        100.0_real64
    candidate%profile_height_scale = &
        legacy_rib(rib_index, legacy_profile_height_scale_column)
    candidate%cell_open = cell_open_integer == 1
    if (.not. candidate%is_valid()) then
      message = 'legacy rib values do not form a valid definition'
      return
    end if

    definition = candidate
    valid = .true.
  end subroutine copy_legacy_rib_definition

  !> Build the explicit generated center/symmetry-row definition from rib one.
  !!
  !! The legacy row is not consulted because its late-scaled displacement and
  !! height-scale fields are incomplete.  Every field instead comes from the
  !! complete authored source definition, with only the four established
  !! mirror operations applied: planform/spatial station, rib-plane angle, and
  !! profile-Z rotation change sign.  Failure leaves `definition` unchanged.
  pure subroutine build_symmetry_rib_definition(source_definition, identity, &
      definition, valid, message)
    type(rib_definition), intent(in) :: source_definition
    type(rib_identity), intent(in) :: identity
    type(rib_definition), intent(inout) :: definition
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(rib_definition) :: candidate

    valid = .false.
    message = ''
    if (.not. source_definition%is_valid()) then
      message = 'symmetry source definition is invalid'
      return
    end if
    if (.not. source_definition%identity%is_authored_physical()) then
      message = 'symmetry source is not an authored physical rib'
      return
    end if
    if (.not. identity%is_valid() .or. identity%legacy_index /= 0) then
      message = 'symmetry identity is invalid or is not row zero'
      return
    end if
    if (identity%role /= rib_role_symmetry_mirror_physical .and. &
        identity%role /= rib_role_symmetry_centerline_alias) then
      message = 'row-zero identity does not have a symmetry role'
      return
    end if
    if (identity%profile_source_index /= &
        source_definition%identity%legacy_index .or. &
        identity%placement_anchor_index /= &
        source_definition%identity%legacy_index) then
      message = 'symmetry provenance disagrees with its authored source'
      return
    end if

    candidate = source_definition
    candidate%identity = identity
    candidate%source_profile_number = identity%profile_source_index
    candidate%planform_station_cm = -source_definition%planform_station_cm
    candidate%spatial_station_cm = -source_definition%spatial_station_cm
    candidate%rib_plane_angle_rad = &
        -source_definition%rib_plane_angle_rad
    candidate%profile_rotation_z_rad = &
        -source_definition%profile_rotation_z_rad
    if (.not. candidate%is_valid()) then
      message = 'generated symmetry definition failed validation'
      return
    end if

    definition = candidate
    valid = .true.
  end subroutine build_symmetry_rib_definition

  !> Build the generated support definition beyond the physical wingtip.
  !!
  !! The authored profile source supplies chord/profile/rotation fields and
  !! the physical wingtip supplies the placement anchor.  Spatial station and
  !! height are extrapolated through those two authored ribs in the same order
  !! as legacy Stage 4.  This avoids reading incomplete generated columns 50
  !! and 160 while retaining their named source provenance transactionally.
  pure subroutine build_tip_support_rib_definition(source_definition, &
      placement_anchor_definition, identity, definition, valid, message)
    type(rib_definition), intent(in) :: source_definition
    type(rib_definition), intent(in) :: placement_anchor_definition
    type(rib_identity), intent(in) :: identity
    type(rib_definition), intent(inout) :: definition
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(rib_definition) :: candidate

    valid = .false.
    message = ''
    if (.not. source_definition%is_valid() .or. &
        .not. placement_anchor_definition%is_valid()) then
      message = 'tip-support source or placement definition is invalid'
      return
    end if
    if (.not. source_definition%identity%is_authored_physical() .or. &
        .not. placement_anchor_definition%identity%is_authored_physical()) then
      message = 'tip-support inputs must be authored physical ribs'
      return
    end if
    if (.not. identity%is_valid() .or. &
        identity%role /= rib_role_tip_extrapolated_support) then
      message = 'tip-support identity is invalid or has the wrong role'
      return
    end if
    if (identity%profile_source_index /= &
        source_definition%identity%legacy_index .or. &
        identity%placement_anchor_index /= &
        placement_anchor_definition%identity%legacy_index) then
      message = 'tip-support provenance disagrees with its authored inputs'
      return
    end if
    if (placement_anchor_definition%identity%legacy_index /= &
        source_definition%identity%legacy_index + 1 .or. &
        identity%legacy_index /= &
        placement_anchor_definition%identity%legacy_index + 1) then
      message = 'tip-support source, anchor, and generated rows are not adjacent'
      return
    end if

    candidate = source_definition
    candidate%identity = identity
    candidate%source_profile_number = identity%profile_source_index
    candidate%spatial_station_cm = &
        placement_anchor_definition%spatial_station_cm + &
        (placement_anchor_definition%spatial_station_cm - &
        source_definition%spatial_station_cm)
    candidate%spatial_height_cm = &
        placement_anchor_definition%spatial_height_cm + &
        (placement_anchor_definition%spatial_height_cm - &
        source_definition%spatial_height_cm)
    if (.not. candidate%is_valid()) then
      message = 'generated tip-support definition failed validation'
      return
    end if

    definition = candidate
    valid = .true.
  end subroutine build_tip_support_rib_definition

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

  !> Construct one complete spatial rib from a normalized source profile.
  !!
  !! `normalized_profile_2d` is the Stage-6 slot-2 boundary: its percentages
  !! already include any profile-file-specific height modification made while
  !! reading slot 1.  The routine therefore converts percentages to local
  !! chord lengths exactly once, subtracts the named vertical displacement,
  !! and applies `transform_adjusted_rib_local_point` point by point.  The
  !! candidate owns newly allocated one-based arrays and is published only
  !! after every point and the complete geometry validate.
  pure subroutine build_spatial_rib_geometry(profile, definition, geometry, &
      valid, message)
    type(normalized_profile_2d), intent(in) :: profile
    type(rib_definition), intent(in) :: definition
    type(spatial_rib_geometry_3d), intent(inout) :: geometry
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(spatial_rib_geometry_3d) :: candidate
    type(rib_local_point_2d) :: local_point
    type(point_3d) :: spatial_point
    logical :: point_valid
    character(len=len(message)) :: point_message
    integer :: point_index

    valid = .false.
    message = ''
    if (.not. profile%is_valid()) then
      message = 'normalized source profile is invalid'
      return
    end if
    if (.not. definition%is_valid()) then
      message = 'rib definition is invalid'
      return
    end if
    if (profile%rib_index /= definition%source_profile_number) then
      message = 'normalized profile disagrees with source provenance'
      return
    end if

    candidate%rib_index = definition%identity%legacy_index
    allocate(candidate%x(profile%topology%point_count), &
        candidate%y(profile%topology%point_count), &
        candidate%z(profile%topology%point_count))
    do point_index = 1, profile%topology%point_count
      local_point%chordwise_cm = definition%chord_length_cm * &
          (profile%chord_percent(point_index) / 100.0_real64)
      local_point%height_cm = definition%chord_length_cm * &
          (profile%height_percent(point_index) / 100.0_real64) - &
          definition%profile_vertical_displacement_cm
      call transform_adjusted_rib_local_point(definition, local_point, &
          spatial_point, point_valid, point_message)
      if (.not. point_valid) then
        message = 'profile point transform failed: '//trim(point_message)
        return
      end if
      candidate%x(point_index) = spatial_point%x_cm
      candidate%y(point_index) = spatial_point%y_cm
      candidate%z(point_index) = spatial_point%z_cm
    end do
    if (.not. candidate%is_valid()) then
      message = 'constructed spatial rib failed validation'
      return
    end if

    geometry = candidate
    valid = .true.
  end subroutine build_spatial_rib_geometry

  !> Mirror an authored spatial rib into explicit generated row zero.
  !!
  !! Legacy Stage 6 mirrors the final absolute X coordinate and retains Y/Z;
  !! it does not run the generated row through the rotation chain a second
  !! time.  The definition is used to validate named source/target provenance.
  pure subroutine build_symmetry_spatial_rib(source_geometry, definition, &
      geometry, valid, message)
    type(spatial_rib_geometry_3d), intent(in) :: source_geometry
    type(rib_definition), intent(in) :: definition
    type(spatial_rib_geometry_3d), intent(inout) :: geometry
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(spatial_rib_geometry_3d) :: candidate

    valid = .false.
    message = ''
    if (.not. source_geometry%is_valid()) then
      message = 'symmetry source geometry is invalid'
      return
    end if
    if (.not. definition%is_valid() .or. &
        definition%identity%legacy_index /= 0) then
      message = 'symmetry target definition is invalid'
      return
    end if
    if (definition%identity%role /= rib_role_symmetry_mirror_physical .and. &
        definition%identity%role /= rib_role_symmetry_centerline_alias) then
      message = 'symmetry target definition has the wrong role'
      return
    end if
    if (source_geometry%rib_index /= definition%source_profile_number .or. &
        source_geometry%rib_index /= &
        definition%identity%placement_anchor_index) then
      message = 'symmetry geometry disagrees with source provenance'
      return
    end if

    candidate%rib_index = definition%identity%legacy_index
    candidate%x = -source_geometry%x
    candidate%y = source_geometry%y
    candidate%z = source_geometry%z
    if (.not. candidate%is_valid()) then
      message = 'generated symmetry geometry failed validation'
      return
    end if

    geometry = candidate
    valid = .true.
  end subroutine build_symmetry_spatial_rib

  !> Construct the extrapolated tip-support geometry from its named profile.
  !!
  !! The definition constructor has already resolved complete profile-source
  !! fields and extrapolated placement, so this wrapper enforces the generated
  !! role before delegating to the exact complete-profile construction path.
  pure subroutine build_tip_support_spatial_rib(profile, definition, geometry, &
      valid, message)
    type(normalized_profile_2d), intent(in) :: profile
    type(rib_definition), intent(in) :: definition
    type(spatial_rib_geometry_3d), intent(inout) :: geometry
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    valid = .false.
    message = ''
    if (.not. definition%is_valid() .or. &
        definition%identity%role /= rib_role_tip_extrapolated_support) then
      message = 'tip-support target definition is invalid'
      return
    end if
    call build_spatial_rib_geometry(profile, definition, geometry, valid, &
        message)
  end subroutine build_tip_support_spatial_rib

  !> Publish one validated typed spatial rib to compatibility X/Y/Z arrays.
  !!
  !! Shape, zero-based row bounds, and point capacity are checked before any
  !! assignment.  Consequently failure leaves all three legacy arrays
  !! unchanged, while success writes only the target row's populated prefix.
  pure subroutine write_legacy_spatial_rib_geometry(geometry, legacy_x, &
      legacy_y, legacy_z, valid, message)
    type(spatial_rib_geometry_3d), intent(in) :: geometry
    real(real64), intent(inout) :: legacy_x(0:,:), legacy_y(0:,:)
    real(real64), intent(inout) :: legacy_z(0:,:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    integer :: point_count, rib_index

    valid = .false.
    message = ''
    if (.not. geometry%is_valid()) then
      message = 'typed spatial rib is invalid'
      return
    end if
    if (.not. all(shape(legacy_x) == shape(legacy_y)) .or. &
        .not. all(shape(legacy_x) == shape(legacy_z))) then
      message = 'legacy X/Y/Z array shapes differ'
      return
    end if
    rib_index = geometry%rib_index
    if (rib_index < 0 .or. rib_index > ubound(legacy_x, 1)) then
      message = 'typed rib index is outside the legacy spatial array'
      return
    end if
    point_count = size(geometry%x)
    if (point_count > size(legacy_x, 2)) then
      message = 'typed rib point count exceeds legacy spatial capacity'
      return
    end if

    legacy_x(rib_index, 1:point_count) = geometry%x
    legacy_y(rib_index, 1:point_count) = geometry%y
    legacy_z(rib_index, 1:point_count) = geometry%z
    valid = .true.
  end subroutine write_legacy_spatial_rib_geometry

end module leparagliding_spatial_geometry
