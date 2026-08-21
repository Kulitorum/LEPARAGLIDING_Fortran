! SPDX-License-Identifier: GPL-3.0-or-later
!
!> Give suspension-anchor inputs and their base geometry named ownership.
!!
!! Legacy Section 3 stores the active A--E count in rib column 15, six chord
!! percentages in columns 16:21, and several later results in offset column
!! groups.  Only A--E belong to the authored count; column 21 is the separate
!! F/brake chord position.  This module retains precisely the active A--E
!! prefix, carries explicit generated-rib provenance through `rib_identity`,
!! and can either resolve a base point independently or take a checked,
!! read-only snapshot of the distinct legacy producers.
!!
!! The coordinate names intentionally stop at demonstrated frames.  The
!! profile point is the Stage-9 rib-local plane (legacy slot 6), and the
!! spatial point is the absolute Stage-12.3 X/Y/Z result (legacy slot 19).
!! Stage-12.4 brake displacement and single-surface virtual-anchor policies are
!! later modifiers and are not silently folded into this base geometry.
module leparagliding_anchor_geometry
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
  use leparagliding_domain_model, only : geometry_values_are_close, &
      normalized_profile_2d, rib_identity, &
      rib_role_symmetry_centerline_alias, &
      rib_role_symmetry_mirror_physical
  use leparagliding_spatial_geometry, only : point_3d, rib_definition, &
      rib_local_point_2d
  implicit none
  private

  integer, parameter, public :: maximum_authored_anchor_count = 5
  integer, parameter :: legacy_anchor_count_column = 15
  integer, parameter :: legacy_first_anchor_percent_column = 16
  integer, parameter :: legacy_brake_percent_column = 21
  integer, parameter :: legacy_first_chord_position_column = 66
  integer, parameter :: legacy_first_local_chord_column = 111
  integer, parameter :: legacy_first_local_height_column = 121
  integer, parameter :: legacy_first_te_distance_column = 131
  integer, parameter :: legacy_local_anchor_slot = 6
  integer, parameter :: legacy_spatial_anchor_slot = 19
  integer, parameter :: required_legacy_rib_column_count = 135
  real(real64), parameter :: integer_tolerance = 1.0e-10_real64

  !> One active authored A--E chord position from Section 3.
  type, public :: anchor_chord_definition
    integer :: ordinal = 0
    real(real64) :: chord_fraction = 0.0_real64
  contains
    procedure :: is_valid => anchor_chord_definition_is_valid
  end type anchor_chord_definition

  !> Active anchor definitions for one physical or generated rib row.
  !!
  !! `authored_anchor_count` controls only A--E.  F is retained separately as
  !! `brake_chord_fraction` because the legacy solver uses it even when it is
  !! outside the active suspension-anchor prefix.  The nested identity records
  !! whether the row is authored, mirrored at the center, or tip-generated.
  type, public :: rib_anchor_definition
    type(rib_identity) :: identity
    integer :: authored_anchor_count = 0
    type(anchor_chord_definition), allocatable :: anchors(:)
    real(real64) :: brake_chord_fraction = 0.0_real64
  contains
    procedure :: is_valid => rib_anchor_definition_is_valid
  end type rib_anchor_definition

  !> One base anchor retaining every demonstrated legacy geometry producer.
  !!
  !! Requested chord, Stage-6 intrados mark, Stage-9 profile point, and
  !! Stage-12 spatial point are intentionally separate: some terminal profiles
  !! do not give the Stage-6 search an interpolation segment, so equality among
  !! these values would be an unsupported semantic assumption.
  !!
  !! `trailing_edge_intrados_distance_cm` is the Stage-6 contour distance used
  !! to place matching marks on flattened intrados edges.  It corresponds to
  !! the active portion of legacy columns 131:135.
  type, public :: resolved_anchor_point
    integer :: ordinal = 0
    real(real64) :: requested_chordwise_cm = 0.0_real64
    type(rib_local_point_2d) :: profile_point
    type(rib_local_point_2d) :: intrados_mark_point
    real(real64) :: trailing_edge_intrados_distance_cm = 0.0_real64
    type(point_3d) :: spatial_point
  contains
    procedure :: is_valid => resolved_anchor_point_is_valid
  end type resolved_anchor_point

  !> Base A--E anchor geometry for one rib, preserving source provenance.
  type, public :: resolved_rib_anchors
    type(rib_identity) :: identity
    type(resolved_anchor_point), allocatable :: anchors(:)
  contains
    procedure :: is_valid => resolved_rib_anchors_is_valid
  end type resolved_rib_anchors

  public :: copy_legacy_rib_anchor_definition
  public :: build_generated_rib_anchor_definition
  public :: rib_anchor_definition_matches_legacy
  public :: resolve_rib_anchors
  public :: place_resolved_rib_anchors
  public :: build_symmetry_resolved_anchors
  public :: copy_legacy_resolved_rib_anchors
  public :: resolved_rib_anchors_match_legacy
  public :: write_legacy_resolved_anchor_spatial_points

contains

  !> Validate one A--E ordinal and its chord fraction.
  pure logical function anchor_chord_definition_is_valid(anchor) result(valid)
    class(anchor_chord_definition), intent(in) :: anchor

    valid = anchor%ordinal >= 1 .and. &
        anchor%ordinal <= maximum_authored_anchor_count .and. &
        ieee_is_finite(anchor%chord_fraction) .and. &
        anchor%chord_fraction >= 0.0_real64 .and. &
        anchor%chord_fraction <= 1.0_real64
  end function anchor_chord_definition_is_valid

  !> Validate a compact active-prefix definition without inspecting stale slots.
  pure logical function rib_anchor_definition_is_valid(definition) result(valid)
    class(rib_anchor_definition), intent(in) :: definition
    integer :: anchor_index

    valid = .false.
    if (.not. definition%identity%is_valid()) return
    if (definition%authored_anchor_count < 0 .or. &
        definition%authored_anchor_count > maximum_authored_anchor_count) return
    if (.not. allocated(definition%anchors)) return
    if (size(definition%anchors) /= definition%authored_anchor_count) return
    if (.not. ieee_is_finite(definition%brake_chord_fraction)) return
    if (definition%brake_chord_fraction < 0.0_real64 .or. &
        definition%brake_chord_fraction > 1.0_real64) return
    do anchor_index = 1, definition%authored_anchor_count
      if (.not. definition%anchors(anchor_index)%is_valid()) return
      if (definition%anchors(anchor_index)%ordinal /= anchor_index) return
    end do
    valid = .true.
  end function rib_anchor_definition_is_valid

  !> Validate finite base anchor geometry and its non-negative TE distance.
  pure logical function resolved_anchor_point_is_valid(anchor) result(valid)
    class(resolved_anchor_point), intent(in) :: anchor

    valid = anchor%ordinal >= 1 .and. &
        anchor%ordinal <= maximum_authored_anchor_count .and. &
        ieee_is_finite(anchor%requested_chordwise_cm) .and. &
        anchor%profile_point%is_valid() .and. &
        anchor%intrados_mark_point%is_valid() .and. &
        ieee_is_finite(anchor%trailing_edge_intrados_distance_cm) .and. &
        anchor%trailing_edge_intrados_distance_cm >= 0.0_real64 .and. &
        anchor%spatial_point%is_valid()
  end function resolved_anchor_point_is_valid

  !> Validate a resolved collection while preserving the rib identity.
  pure logical function resolved_rib_anchors_is_valid(resolved) result(valid)
    class(resolved_rib_anchors), intent(in) :: resolved
    integer :: anchor_index

    valid = .false.
    if (.not. resolved%identity%is_valid()) return
    if (.not. allocated(resolved%anchors)) return
    if (size(resolved%anchors) > maximum_authored_anchor_count) return
    do anchor_index = 1, size(resolved%anchors)
      if (.not. resolved%anchors(anchor_index)%is_valid()) return
      if (resolved%anchors(anchor_index)%ordinal /= anchor_index) return
    end do
    valid = .true.
  end function resolved_rib_anchors_is_valid

  !> Copy one legacy Section-3 row into a checked compact definition.
  !!
  !! The adapter reads only the active A--E prefix and the independent F/brake
  !! position.  Inactive percentage columns are deliberately ignored because
  !! established files can retain nonzero values beyond column 15's count.
  !! Failure leaves `definition` unchanged.
  pure subroutine copy_legacy_rib_anchor_definition(legacy_rib, identity, &
      definition, valid, message)
    real(real64), intent(in) :: legacy_rib(0:,:)
    type(rib_identity), intent(in) :: identity
    type(rib_anchor_definition), intent(inout) :: definition
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(rib_anchor_definition) :: candidate
    real(real64) :: count_value, percent_value
    integer :: anchor_count, anchor_index, rib_index

    valid = .false.
    message = ''
    if (.not. identity%is_valid()) then
      message = 'rib identity is invalid'
      return
    end if
    rib_index = identity%legacy_index
    if (rib_index < 0 .or. rib_index > ubound(legacy_rib, 1)) then
      message = 'rib identity is outside legacy anchor row bounds'
      return
    end if
    if (ubound(legacy_rib, 2) < legacy_brake_percent_column) then
      message = 'legacy rib row lacks Section-3 anchor columns'
      return
    end if
    count_value = legacy_rib(rib_index, legacy_anchor_count_column)
    if (.not. ieee_is_finite(count_value)) then
      message = 'legacy anchor count is non-finite'
      return
    end if
    anchor_count = nint(count_value)
    if (abs(count_value - real(anchor_count, real64)) > integer_tolerance .or. &
        anchor_count < 0 .or. anchor_count > maximum_authored_anchor_count) then
      message = 'legacy anchor count is not an integer from zero through five'
      return
    end if

    candidate%identity = identity
    candidate%authored_anchor_count = anchor_count
    allocate(candidate%anchors(anchor_count))
    do anchor_index = 1, anchor_count
      percent_value = legacy_rib(rib_index, &
          legacy_first_anchor_percent_column + anchor_index - 1)
      if (.not. ieee_is_finite(percent_value) .or. &
          percent_value < 0.0_real64 .or. percent_value > 100.0_real64) then
        message = 'active legacy anchor percentage is outside zero to 100'
        return
      end if
      candidate%anchors(anchor_index)%ordinal = anchor_index
      candidate%anchors(anchor_index)%chord_fraction = &
          percent_value / 100.0_real64
    end do
    percent_value = legacy_rib(rib_index, legacy_brake_percent_column)
    if (.not. ieee_is_finite(percent_value) .or. &
        percent_value < 0.0_real64 .or. percent_value > 100.0_real64) then
      message = 'legacy brake percentage is outside zero to 100'
      return
    end if
    candidate%brake_chord_fraction = percent_value / 100.0_real64
    if (.not. candidate%is_valid()) then
      message = 'copied rib anchor definition failed validation'
      return
    end if

    definition = candidate
    valid = .true.
  end subroutine copy_legacy_rib_anchor_definition

  !> Copy authored anchor data into a generated row with explicit provenance.
  !!
  !! Section 3 obtains row 0 from row 1 and tip support from nribss-1.  The
  !! target identity's `profile_source_index` names that source directly, so
  !! no generated row is inferred from numeric adjacency.  Failure is
  !! transactional.
  pure subroutine build_generated_rib_anchor_definition(source, &
      generated_identity, definition, valid, message)
    type(rib_anchor_definition), intent(in) :: source
    type(rib_identity), intent(in) :: generated_identity
    type(rib_anchor_definition), intent(inout) :: definition
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(rib_anchor_definition) :: candidate

    valid = .false.
    message = ''
    if (.not. source%is_valid()) then
      message = 'generated anchor source is invalid'
      return
    end if
    if (.not. generated_identity%is_valid() .or. &
        generated_identity%is_authored_physical()) then
      message = 'generated anchor target identity is invalid or authored'
      return
    end if
    if (source%identity%legacy_index /= &
        generated_identity%profile_source_index) then
      message = 'generated anchor source disagrees with target provenance'
      return
    end if

    candidate = source
    candidate%identity = generated_identity
    if (.not. candidate%is_valid()) then
      message = 'generated rib anchor definition failed validation'
      return
    end if
    definition = candidate
    valid = .true.
  end subroutine build_generated_rib_anchor_definition

  !> Compare every retained definition field with its legacy Section-3 row.
  pure subroutine rib_anchor_definition_matches_legacy(definition, legacy_rib, &
      matches, message)
    type(rib_anchor_definition), intent(in) :: definition
    real(real64), intent(in) :: legacy_rib(0:,:)
    logical, intent(out) :: matches
    character(len=*), intent(out) :: message

    real(real64) :: count_value
    integer :: anchor_index, rib_index

    matches = .false.
    message = ''
    if (.not. definition%is_valid()) then
      message = 'typed rib anchor definition is invalid'
      return
    end if
    rib_index = definition%identity%legacy_index
    if (rib_index < 0 .or. rib_index > ubound(legacy_rib, 1) .or. &
        ubound(legacy_rib, 2) < legacy_brake_percent_column) then
      message = 'legacy Section-3 row is unavailable'
      return
    end if
    count_value = legacy_rib(rib_index, legacy_anchor_count_column)
    if (.not. geometry_values_are_close(count_value, &
        real(definition%authored_anchor_count, real64))) then
      message = 'typed and legacy authored anchor counts differ'
      return
    end if
    do anchor_index = 1, definition%authored_anchor_count
      if (.not. geometry_values_are_close( &
          definition%anchors(anchor_index)%chord_fraction * 100.0_real64, &
          legacy_rib(rib_index, legacy_first_anchor_percent_column + &
              anchor_index - 1))) then
        write(message, '(A,I0)') 'typed/legacy anchor percentage differs at ', &
            anchor_index
        return
      end if
    end do
    if (.not. geometry_values_are_close( &
        definition%brake_chord_fraction * 100.0_real64, &
        legacy_rib(rib_index, legacy_brake_percent_column))) then
      message = 'typed and legacy brake percentages differ'
      return
    end if
    matches = .true.
  end subroutine rib_anchor_definition_matches_legacy

  !> Resolve all active A--E points from named profile and rib definitions.
  !!
  !! The intrados interpolation and trailing-edge accumulation follow the
  !! Stage-6/9 contour order.  Spatial placement applies the named Stage-12
  !! transform, including its anchor-specific displacement order.
  !! Failure leaves `resolved` unchanged.
  pure subroutine resolve_rib_anchors(profile, spatial_definition, &
      anchor_definition, resolved, valid, message)
    type(normalized_profile_2d), intent(in) :: profile
    type(rib_definition), intent(in) :: spatial_definition
    type(rib_anchor_definition), intent(in) :: anchor_definition
    type(resolved_rib_anchors), intent(inout) :: resolved
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(resolved_rib_anchors) :: candidate
    type(point_3d) :: spatial_point
    real(real64), allocatable :: local_chord(:), local_height(:)
    real(real64) :: denominator, target_chord, interpolation_fraction
    real(real64) :: distance
    logical :: point_valid, segment_found
    character(len=len(message)) :: point_message
    integer :: anchor_index, contour_index, distance_index
    integer :: intrados_first, intrados_last

    valid = .false.
    message = ''
    if (.not. profile%is_valid()) then
      message = 'normalized anchor source profile is invalid'
      return
    end if
    if (.not. spatial_definition%is_valid()) then
      message = 'anchor spatial rib definition is invalid'
      return
    end if
    if (.not. anchor_definition%is_valid()) then
      message = 'rib anchor definition is invalid'
      return
    end if
    if (spatial_definition%identity%legacy_index /= &
        anchor_definition%identity%legacy_index .or. &
        profile%rib_index /= spatial_definition%source_profile_number) then
      message = 'anchor profile, placement, and Section-3 provenance disagree'
      return
    end if

    allocate(local_chord(profile%topology%point_count), &
        local_height(profile%topology%point_count))
    local_chord = spatial_definition%chord_length_cm * &
        profile%chord_fraction
    local_height = spatial_definition%chord_length_cm * &
        profile%height_fraction
    intrados_first = profile%topology%intrados%first
    intrados_last = profile%topology%intrados%last

    candidate%identity = anchor_definition%identity
    allocate(candidate%anchors(anchor_definition%authored_anchor_count))
    do anchor_index = 1, anchor_definition%authored_anchor_count
      target_chord = spatial_definition%chord_length_cm * &
          anchor_definition%anchors(anchor_index)%chord_fraction
      segment_found = .false.
      do contour_index = intrados_last, intrados_first + 1, -1
        if (local_chord(contour_index - 1) <= target_chord .and. &
            local_chord(contour_index) > target_chord .and. &
            local_chord(contour_index) >= 0.0_real64) then
          denominator = local_chord(contour_index) - &
              local_chord(contour_index - 1)
          if (abs(denominator) <= tiny(denominator)) then
            message = 'anchor interpolation encountered duplicate chord values'
            return
          end if
          interpolation_fraction = (target_chord - &
              local_chord(contour_index - 1)) / denominator
          candidate%anchors(anchor_index)%ordinal = anchor_index
          candidate%anchors(anchor_index)%requested_chordwise_cm = target_chord
          candidate%anchors(anchor_index)%profile_point%chordwise_cm = &
              target_chord
          candidate%anchors(anchor_index)%profile_point%height_cm = &
              local_height(contour_index - 1) + interpolation_fraction * &
              (local_height(contour_index) - &
                  local_height(contour_index - 1))
          candidate%anchors(anchor_index)%intrados_mark_point = &
              candidate%anchors(anchor_index)%profile_point

          distance = 0.0_real64
          do distance_index = intrados_last, contour_index + 1, -1
            distance = distance + hypot( &
                local_chord(distance_index) - &
                    local_chord(distance_index - 1), &
                local_height(distance_index) - &
                    local_height(distance_index - 1))
          end do
          distance = distance + hypot( &
              local_chord(contour_index) - target_chord, &
              local_height(contour_index) - &
                  candidate%anchors(anchor_index)%profile_point%height_cm)
          candidate%anchors(anchor_index)% &
              trailing_edge_intrados_distance_cm = distance

          call transform_stage12_anchor_profile_point(spatial_definition, &
              candidate%anchors(anchor_index)%profile_point, spatial_point, &
              point_valid, point_message)
          if (.not. point_valid) then
            message = 'anchor spatial transform failed: '//trim(point_message)
            return
          end if
          candidate%anchors(anchor_index)%spatial_point = spatial_point
          segment_found = .true.
          exit
        end if
      end do
      if (.not. segment_found) then
        write(message, '(A,I0)') &
            'active anchor has no intrados interpolation segment at ', &
            anchor_index
        return
      end if
    end do
    if (.not. candidate%is_valid()) then
      message = 'resolved rib anchor collection failed validation'
      return
    end if

    resolved = candidate
    valid = .true.
  end subroutine resolve_rib_anchors

  !> Rebuild only the absolute spatial points of an existing anchor collection.
  !!
  !! Stage 9 and Stage 6 currently remain the compatibility producers for the
  !! rib-local profile point, intrados mark point, and trailing-edge distance.
  !! This constructor retains those independent values but replaces every
  !! Stage-12.3 spatial point through the named anchor transform.  That policy
  !! is deliberately distinct from skin placement: legacy Stage 12 subtracts
  !! the profile displacement after the wash-in rotation and only from its
  !! local-height result.
  !!
  !! The operation is transactional.  Invalid provenance or any failed point
  !! transform leaves `placed` unchanged.
  pure subroutine place_resolved_rib_anchors(source, spatial_definition, &
      placed, valid, message)
    type(resolved_rib_anchors), intent(in) :: source
    type(rib_definition), intent(in) :: spatial_definition
    type(resolved_rib_anchors), intent(inout) :: placed
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(resolved_rib_anchors) :: candidate
    type(point_3d) :: spatial_point
    logical :: point_valid
    character(len=len(message)) :: point_message
    integer :: anchor_index

    valid = .false.
    message = ''
    if (.not. source%is_valid()) then
      message = 'source resolved anchors are invalid'
      return
    end if
    if (.not. spatial_definition%is_valid()) then
      message = 'anchor spatial rib definition is invalid'
      return
    end if
    if (source%identity%legacy_index /= &
        spatial_definition%identity%legacy_index .or. &
        source%identity%profile_source_index /= &
        spatial_definition%identity%profile_source_index .or. &
        source%identity%placement_anchor_index /= &
        spatial_definition%identity%placement_anchor_index .or. &
        source%identity%role /= spatial_definition%identity%role) then
      message = 'resolved anchors and spatial definition provenance disagree'
      return
    end if

    candidate = source
    do anchor_index = 1, size(candidate%anchors)
      call transform_stage12_anchor_profile_point(spatial_definition, &
          source%anchors(anchor_index)%profile_point, spatial_point, &
          point_valid, point_message)
      if (.not. point_valid) then
        write(message, '(A,I0,2A)') &
            'anchor spatial transform failed at active ordinal ', &
            anchor_index, ': ', trim(point_message)
        return
      end if
      candidate%anchors(anchor_index)%spatial_point = spatial_point
    end do
    if (.not. candidate%is_valid()) then
      message = 'placed rib anchor collection failed validation'
      return
    end if

    placed = candidate
    valid = .true.
  end subroutine place_resolved_rib_anchors

  !> Apply the exact legacy Stage-12.3 transform ordering to one anchor point.
  !!
  !! Skin geometry subtracts profile displacement before wash-in.  Singular
  !! anchor points instead subtract it from the rotated local-height component;
  !! sharing the skin kernel would therefore move both local components when
  !! wash-in is nonzero.  Keeping this small policy-specific kernel beside the
  !! anchor model makes that historical distinction explicit.
  pure subroutine transform_stage12_anchor_profile_point(definition, &
      profile_point, spatial_point, valid, message)
    type(rib_definition), intent(in) :: definition
    type(rib_local_point_2d), intent(in) :: profile_point
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
      message = 'anchor spatial rib definition is invalid'
      return
    end if
    if (.not. profile_point%is_valid()) then
      message = 'anchor profile point is invalid'
      return
    end if

    washin_pivot_cm = definition%washin_pivot_fraction * &
        definition%chord_length_cm
    washin_u = (profile_point%chordwise_cm - washin_pivot_cm) * &
        cos(definition%washin_angle_rad) + profile_point%height_cm * &
        sin(definition%washin_angle_rad) + washin_pivot_cm
    washin_v = (-profile_point%chordwise_cm + washin_pivot_cm) * &
        sin(definition%washin_angle_rad) + profile_point%height_cm * &
        cos(definition%washin_angle_rad) - &
        definition%profile_vertical_displacement_cm

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
      message = 'anchor spatial transform produced a non-finite point'
      return
    end if

    spatial_point = candidate
    valid = .true.
  end subroutine transform_stage12_anchor_profile_point

  !> Mirror physical row-1 base anchors into generated legacy row zero.
  !!
  !! Stage 12 mirrors only absolute X and preserves Y/Z.  Rib-local positions
  !! and intrados distances remain those of the explicitly named source row.
  pure subroutine build_symmetry_resolved_anchors(source, target_definition, &
      resolved, valid, message)
    type(resolved_rib_anchors), intent(in) :: source
    type(rib_anchor_definition), intent(in) :: target_definition
    type(resolved_rib_anchors), intent(inout) :: resolved
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(resolved_rib_anchors) :: candidate
    integer :: anchor_index

    valid = .false.
    message = ''
    if (.not. source%is_valid() .or. .not. target_definition%is_valid()) then
      message = 'symmetry anchor source or target is invalid'
      return
    end if
    if (target_definition%identity%role /= &
        rib_role_symmetry_mirror_physical .and. &
        target_definition%identity%role /= &
        rib_role_symmetry_centerline_alias) then
      message = 'symmetry anchor target has the wrong generated role'
      return
    end if
    if (source%identity%legacy_index /= &
        target_definition%identity%profile_source_index .or. &
        size(source%anchors) /= target_definition%authored_anchor_count) then
      message = 'symmetry anchor source disagrees with target provenance'
      return
    end if

    candidate = source
    candidate%identity = target_definition%identity
    do anchor_index = 1, size(candidate%anchors)
      candidate%anchors(anchor_index)%spatial_point%x_cm = &
          -source%anchors(anchor_index)%spatial_point%x_cm
    end do
    if (.not. candidate%is_valid()) then
      message = 'generated symmetry anchors failed validation'
      return
    end if
    resolved = candidate
    valid = .true.
  end subroutine build_symmetry_resolved_anchors

  !> Snapshot the complete legacy base-anchor boundary transactionally.
  !!
  !! The Stage-6 intrados mark point and Stage-9 slot-6 profile point are kept
  !! separately.  They normally agree, but collapsing/terminal profiles can
  !! leave one legacy producer without an interpolation segment; preserving
  !! both prevents a typed adapter from inventing geometry at that edge case.
  pure subroutine copy_legacy_resolved_rib_anchors(definition, legacy_rib, &
      legacy_u, legacy_v, legacy_w, resolved, valid, message)
    type(rib_anchor_definition), intent(in) :: definition
    real(real64), intent(in) :: legacy_rib(0:,:)
    real(real64), intent(in) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    real(real64), intent(in) :: legacy_w(0:,:,:)
    type(resolved_rib_anchors), intent(inout) :: resolved
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(resolved_rib_anchors) :: candidate
    integer :: anchor_index, rib_index

    valid = .false.
    message = ''
    if (.not. definition%is_valid()) then
      message = 'typed Section-3 definition is invalid'
      return
    end if
    rib_index = definition%identity%legacy_index
    if (rib_index < 0 .or. rib_index > ubound(legacy_rib, 1) .or. &
        ubound(legacy_rib, 2) < required_legacy_rib_column_count .or. &
        rib_index > ubound(legacy_u, 1) .or. &
        size(legacy_u, 2) < definition%authored_anchor_count .or. &
        ubound(legacy_u, 3) < legacy_spatial_anchor_slot .or. &
        .not. all(shape(legacy_u) == shape(legacy_v)) .or. &
        .not. all(shape(legacy_u) == shape(legacy_w))) then
      message = 'legacy resolved-anchor storage is incomplete or inconsistent'
      return
    end if

    candidate%identity = definition%identity
    allocate(candidate%anchors(definition%authored_anchor_count))
    do anchor_index = 1, definition%authored_anchor_count
      candidate%anchors(anchor_index)%ordinal = anchor_index
      candidate%anchors(anchor_index)%requested_chordwise_cm = &
          legacy_rib(rib_index, legacy_first_chord_position_column + &
              anchor_index - 1)
      candidate%anchors(anchor_index)%profile_point%chordwise_cm = &
          legacy_u(rib_index, anchor_index, legacy_local_anchor_slot)
      candidate%anchors(anchor_index)%profile_point%height_cm = &
          legacy_v(rib_index, anchor_index, legacy_local_anchor_slot)
      candidate%anchors(anchor_index)%intrados_mark_point%chordwise_cm = &
          legacy_rib(rib_index, legacy_first_local_chord_column + &
              anchor_index - 1)
      candidate%anchors(anchor_index)%intrados_mark_point%height_cm = &
          legacy_rib(rib_index, legacy_first_local_height_column + &
              anchor_index - 1)
      candidate%anchors(anchor_index)%trailing_edge_intrados_distance_cm = &
          legacy_rib(rib_index, legacy_first_te_distance_column + &
              anchor_index - 1)
      candidate%anchors(anchor_index)%spatial_point%x_cm = &
          legacy_u(rib_index, anchor_index, legacy_spatial_anchor_slot)
      candidate%anchors(anchor_index)%spatial_point%y_cm = &
          legacy_v(rib_index, anchor_index, legacy_spatial_anchor_slot)
      candidate%anchors(anchor_index)%spatial_point%z_cm = &
          legacy_w(rib_index, anchor_index, legacy_spatial_anchor_slot)
    end do
    if (.not. candidate%is_valid()) then
      message = 'legacy resolved-anchor snapshot failed validation'
      return
    end if
    resolved = candidate
    valid = .true.
  end subroutine copy_legacy_resolved_rib_anchors

  !> Compare every typed base-anchor value with the legacy producer stores.
  !!
  !! The active offset groups are 66:70 (chord positions), 111:115 and
  !! 121:125 (intrados local point), and 131:135 (TE contour distance).
  !! Columns 110, 116:120, and 126:130 are not active members of those groups.
  !! Slot 18 is a Stage-12 intermediate and has no downstream reader; slot 19
  !! is the absolute spatial point consumed by the line topology.
  pure subroutine resolved_rib_anchors_match_legacy(resolved, definition, &
      legacy_rib, legacy_u, legacy_v, legacy_w, matches, message)
    type(resolved_rib_anchors), intent(in) :: resolved
    type(rib_anchor_definition), intent(in) :: definition
    real(real64), intent(in) :: legacy_rib(0:,:)
    real(real64), intent(in) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    real(real64), intent(in) :: legacy_w(0:,:,:)
    logical, intent(out) :: matches
    character(len=*), intent(out) :: message

    integer :: anchor_index, rib_index
    matches = .false.
    message = ''
    if (.not. resolved%is_valid() .or. .not. definition%is_valid()) then
      message = 'typed resolved anchors or definition is invalid'
      return
    end if
    if (resolved%identity%legacy_index /= definition%identity%legacy_index .or. &
        size(resolved%anchors) /= definition%authored_anchor_count) then
      message = 'resolved anchors disagree with Section-3 definition'
      return
    end if
    rib_index = resolved%identity%legacy_index
    if (rib_index < 0 .or. rib_index > ubound(legacy_rib, 1) .or. &
        ubound(legacy_rib, 2) < required_legacy_rib_column_count .or. &
        rib_index > ubound(legacy_u, 1) .or. &
        size(legacy_u, 2) < size(resolved%anchors) .or. &
        ubound(legacy_u, 3) < legacy_spatial_anchor_slot .or. &
        .not. all(shape(legacy_u) == shape(legacy_v)) .or. &
        .not. all(shape(legacy_u) == shape(legacy_w))) then
      message = 'legacy resolved-anchor storage is incomplete or inconsistent'
      return
    end if

    do anchor_index = 1, size(resolved%anchors)
      if (.not. geometry_values_are_close( &
          resolved%anchors(anchor_index)%requested_chordwise_cm, &
          legacy_rib(rib_index, legacy_first_chord_position_column + &
              anchor_index - 1)) .or. &
          .not. geometry_values_are_close( &
          resolved%anchors(anchor_index)%intrados_mark_point%chordwise_cm, &
          legacy_rib(rib_index, legacy_first_local_chord_column + &
              anchor_index - 1)) .or. &
          .not. geometry_values_are_close( &
          resolved%anchors(anchor_index)%profile_point%chordwise_cm, &
          legacy_u(rib_index, anchor_index, legacy_local_anchor_slot)) .or. &
          .not. geometry_values_are_close( &
          resolved%anchors(anchor_index)%intrados_mark_point%height_cm, &
          legacy_rib(rib_index, legacy_first_local_height_column + &
              anchor_index - 1)) .or. &
          .not. geometry_values_are_close( &
          resolved%anchors(anchor_index)%profile_point%height_cm, &
          legacy_v(rib_index, anchor_index, legacy_local_anchor_slot)) .or. &
          .not. geometry_values_are_close( &
          resolved%anchors(anchor_index)%trailing_edge_intrados_distance_cm, &
          legacy_rib(rib_index, legacy_first_te_distance_column + &
              anchor_index - 1)) .or. &
          .not. geometry_values_are_close( &
          resolved%anchors(anchor_index)%spatial_point%x_cm, &
          legacy_u(rib_index, anchor_index, legacy_spatial_anchor_slot)) .or. &
          .not. geometry_values_are_close( &
          resolved%anchors(anchor_index)%spatial_point%y_cm, &
          legacy_v(rib_index, anchor_index, legacy_spatial_anchor_slot)) .or. &
          .not. geometry_values_are_close( &
          resolved%anchors(anchor_index)%spatial_point%z_cm, &
          legacy_w(rib_index, anchor_index, legacy_spatial_anchor_slot))) then
        write(message, '(A,I0)') &
            'typed/legacy resolved anchor differs at active ordinal ', &
            anchor_index
        return
      end if
    end do
    matches = .true.
  end subroutine resolved_rib_anchors_match_legacy

  !> Publish only active A--E absolute points to legacy Stage-12 slot 19.
  !!
  !! This narrow compatibility writer intentionally leaves intermediate slots
  !! 17/18 and the independent Stage-6/9 producers untouched.  Bounds and
  !! shapes are checked before the first assignment so failure cannot leave a
  !! partially updated legacy boundary.
  pure subroutine write_legacy_resolved_anchor_spatial_points(resolved, &
      legacy_u, legacy_v, legacy_w, valid, message)
    type(resolved_rib_anchors), intent(in) :: resolved
    real(real64), intent(inout) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    real(real64), intent(inout) :: legacy_w(0:,:,:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    integer :: anchor_index, rib_index

    valid = .false.
    message = ''
    if (.not. resolved%is_valid()) then
      message = 'resolved anchors are invalid'
      return
    end if
    rib_index = resolved%identity%legacy_index
    if (rib_index < 0 .or. rib_index > ubound(legacy_u, 1) .or. &
        size(legacy_u, 2) < size(resolved%anchors) .or. &
        ubound(legacy_u, 3) < legacy_spatial_anchor_slot .or. &
        .not. all(shape(legacy_u) == shape(legacy_v)) .or. &
        .not. all(shape(legacy_u) == shape(legacy_w))) then
      message = 'legacy spatial-anchor storage is incomplete or inconsistent'
      return
    end if

    do anchor_index = 1, size(resolved%anchors)
      legacy_u(rib_index, anchor_index, legacy_spatial_anchor_slot) = &
          resolved%anchors(anchor_index)%spatial_point%x_cm
      legacy_v(rib_index, anchor_index, legacy_spatial_anchor_slot) = &
          resolved%anchors(anchor_index)%spatial_point%y_cm
      legacy_w(rib_index, anchor_index, legacy_spatial_anchor_slot) = &
          resolved%anchors(anchor_index)%spatial_point%z_cm
    end do
    valid = .true.
  end subroutine write_legacy_resolved_anchor_spatial_points

end module leparagliding_anchor_geometry
