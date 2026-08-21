! SPDX-License-Identifier: GPL-3.0-or-later
!
!> Typed post-shaping length matching for physical production boundaries.
!!
!! The legacy `ndif=1000` path rescales a selected run of sewing-edge
!! segments so its length approaches the corresponding rib contour.  It uses
!! one index to measure the correction run and a second index to reconstruct
!! it.  Those indices are intentionally distinct here: current designs depend
!! on that historical selection and changing it requires a separately reviewed
!! geometry policy.
!!
!! The old terminal path overwrote only sewing slot 9.  This module carries the
!! already constructed cut contour with the same point displacement, keeping
!! the terminal sewing/cut pair coherent without inventing a panel or higher
!! terminal side.
module leparagliding_panel_reformat
  use, intrinsic :: iso_fortran_env, only : int64, real32, real64
  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
  use leparagliding_domain_model, only : production_boundary_edge_2d, &
      surface_intrados, legacy_production_lower_sewing_slot, &
      legacy_production_higher_sewing_slot, &
      legacy_production_lower_cut_slot, legacy_production_higher_cut_slot
  implicit none
  private

  real(real64), parameter :: half_pi = &
      2.0_real64*atan(1.0_real64)
  ! The Stage-8 expressions use `**2.` (a real exponent), not integer
  ! squaring.  Preserve that slower, slightly different rounding contract.
  real(real64), parameter :: legacy_square_exponent = &
      real(2.0, real64)

  !> Inputs controlling one legacy-compatible boundary length match.
  !!
  !! Indices use the original global contour numbering.  The measurement
  !! index partitions the pre-reformat contour for the scale calculation; the
  !! reconstruction index is the unchanged anchor from which points are
  !! rebuilt toward `contour_first_index`.
  type, public :: boundary_length_match_control
    integer :: measurement_start_index = 0
    integer :: reconstruction_start_index = 0
    real(real64) :: source_contour_length = 0.0_real64
    real(real64) :: target_contour_length = 0.0_real64
    real(real64) :: correction_fraction = 0.0_real64
  contains
    procedure :: is_valid_for => boundary_length_match_control_is_valid_for
  end type boundary_length_match_control

  !> One compatibility point immediately before a terminal surface range.
  !!
  !! The `ndif=1000` right-side path extrapolates `contour_first_index-1`
  !! using the preceding point.  This point belongs to the intake/intrados join
  !! treatment rather than the exact intrados edge, so it is represented
  !! separately instead of widening `production_boundary_edge_2d`.
  type, public :: preceding_join_support_2d
    integer :: boundary_rib_index = -1
    integer :: source_panel_index = -1
    integer :: boundary_contour_first_index = 0
    integer :: anchor_point_index = 0
    integer :: support_point_index = 0
    real(real64) :: anchor_sewing_u = 0.0_real64
    real(real64) :: anchor_sewing_v = 0.0_real64
    real(real64) :: sewing_u = 0.0_real64
    real(real64) :: sewing_v = 0.0_real64
    real(real64) :: cut_u = 0.0_real64
    real(real64) :: cut_v = 0.0_real64
  contains
    procedure :: is_valid => preceding_join_support_is_valid
  end type preceding_join_support_2d

  !> Exact production contour owned by one side of a regular extrados panel.
  !!
  !! Stage 8 stores this edge in legacy slots 9/11 at `panel_index`.  The
  !! point after `contour_last_index` is an intake-side support point and is
  !! deliberately excluded: the historical extrapolation of that point stays
  !! in the fixed-form caller until its cross-surface ownership is modelled.
  type :: regular_extrados_edge_2d
    integer :: panel_index = -1
    integer :: contour_first_index = 1
    integer :: contour_last_index = -1
    real(real64), allocatable :: sewing_u(:)
    real(real64), allocatable :: sewing_v(:)
    real(real64), allocatable :: cut_u(:)
    real(real64), allocatable :: cut_v(:)
  contains
    procedure :: is_valid => regular_extrados_edge_is_valid
  end type regular_extrados_edge_2d

  public :: reformat_terminal_intrados_boundary
  public :: reformat_legacy_regular_lower_extrados_row
  public :: reformat_legacy_regular_higher_extrados_row
  public :: apply_legacy_regular_extrados_distortion_pass
  public :: copy_legacy_preceding_join_support
  public :: reformat_preceding_join_support
  public :: write_legacy_preceding_join_support

contains

  !> Validate one bounded regular extrados production edge.
  pure logical function regular_extrados_edge_is_valid(edge)
    class(regular_extrados_edge_2d), intent(in) :: edge

    integer :: point_count

    regular_extrados_edge_is_valid = .false.
    if (edge%panel_index < 0) return
    if (edge%contour_first_index /= 1) return
    if (edge%contour_last_index < edge%contour_first_index) return
    point_count = edge%contour_last_index - edge%contour_first_index + 1
    if (point_count < 2) return
    if (.not. allocated(edge%sewing_u) .or. &
        .not. allocated(edge%sewing_v) .or. &
        .not. allocated(edge%cut_u) .or. &
        .not. allocated(edge%cut_v)) return
    if (any([size(edge%sewing_u), size(edge%sewing_v), &
        size(edge%cut_u), size(edge%cut_v)] /= point_count)) return
    if (.not. all(ieee_is_finite(edge%sewing_u)) .or. &
        .not. all(ieee_is_finite(edge%sewing_v)) .or. &
        .not. all(ieee_is_finite(edge%cut_u)) .or. &
        .not. all(ieee_is_finite(edge%cut_v))) return
    regular_extrados_edge_is_valid = .true.
  end function regular_extrados_edge_is_valid

  !> Reformat one regular lower extrados row through a checked typed boundary.
  !!
  !! This is the migration boundary for the forward `ndif=1000` loop.  It
  !! snapshots the exact extrados contour from slots 9/11, calculates a typed
  !! candidate, independently evaluates the historical loop as an oracle, and
  !! publishes the candidate only when every sewing coordinate is bit-exact.
  !! The established cut contour is intentionally unchanged, matching the
  !! production contract before this migration.  The caller still owns the
  !! historical extrapolation at `contour_last_index+1`.
  !!
  !! Rejected requests are transactional and leave both legacy arrays intact.
  pure subroutine reformat_legacy_regular_lower_extrados_row(panel_index, &
      contour_last_index, reconstruction_start_index, &
      source_contour_length, target_contour_length, correction_fraction, &
      legacy_u, legacy_v, segment_angle, segment_distance, segment_sign_u, &
      segment_sign_v, valid, message)
    integer, intent(in) :: panel_index, contour_last_index
    integer, intent(in) :: reconstruction_start_index
    real(real64), intent(in) :: source_contour_length, target_contour_length
    real(real64), intent(in) :: correction_fraction
    real(real64), intent(inout) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    real(real64), intent(inout) :: segment_angle(0:), segment_distance(0:)
    real(real64), intent(inout) :: segment_sign_u(0:), segment_sign_v(0:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    call reformat_legacy_regular_extrados_row(panel_index, &
        contour_last_index, reconstruction_start_index, &
        source_contour_length, target_contour_length, correction_fraction, &
        legacy_production_lower_sewing_slot, &
        legacy_production_lower_cut_slot, legacy_u, legacy_v, segment_angle, &
        segment_distance, segment_sign_u, segment_sign_v, valid, message)
  end subroutine reformat_legacy_regular_lower_extrados_row

  !> Reformat the higher edge of one real extrados panel.
  !!
  !! The legacy caller stores this edge at the real panel's row in slots
  !! 10/12.  The physical tip is therefore the higher edge of the final real
  !! panel, not a fabricated row beyond it.  As on the lower wrapper, the
  !! following intake-support point remains outside this routine's ownership.
  pure subroutine reformat_legacy_regular_higher_extrados_row(panel_index, &
      contour_last_index, reconstruction_start_index, &
      source_contour_length, target_contour_length, correction_fraction, &
      legacy_u, legacy_v, segment_angle, segment_distance, segment_sign_u, &
      segment_sign_v, valid, message)
    integer, intent(in) :: panel_index, contour_last_index
    integer, intent(in) :: reconstruction_start_index
    real(real64), intent(in) :: source_contour_length, target_contour_length
    real(real64), intent(in) :: correction_fraction
    real(real64), intent(inout) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    real(real64), intent(inout) :: segment_angle(0:), segment_distance(0:)
    real(real64), intent(inout) :: segment_sign_u(0:), segment_sign_v(0:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    call reformat_legacy_regular_extrados_row(panel_index, &
        contour_last_index, reconstruction_start_index, &
        source_contour_length, target_contour_length, correction_fraction, &
        legacy_production_higher_sewing_slot, &
        legacy_production_higher_cut_slot, legacy_u, legacy_v, segment_angle, &
        segment_distance, segment_sign_u, segment_sign_v, valid, message)
  end subroutine reformat_legacy_regular_higher_extrados_row

  !> Shared typed/oracle boundary for either side of a real extrados panel.
  pure subroutine reformat_legacy_regular_extrados_row(panel_index, &
      contour_last_index, reconstruction_start_index, &
      source_contour_length, target_contour_length, correction_fraction, &
      sewing_slot, cut_slot, legacy_u, legacy_v, segment_angle, &
      segment_distance, segment_sign_u, segment_sign_v, valid, message)
    integer, intent(in) :: panel_index, contour_last_index
    integer, intent(in) :: reconstruction_start_index, sewing_slot, cut_slot
    real(real64), intent(in) :: source_contour_length, target_contour_length
    real(real64), intent(in) :: correction_fraction
    real(real64), intent(inout) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    real(real64), intent(inout) :: segment_angle(0:), segment_distance(0:)
    real(real64), intent(inout) :: segment_sign_u(0:), segment_sign_v(0:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(regular_extrados_edge_2d) :: source, typed_candidate
    type(regular_extrados_edge_2d) :: legacy_oracle
    real(real64), allocatable :: typed_angle(:), typed_distance(:)
    real(real64), allocatable :: typed_sign_u(:), typed_sign_v(:)
    real(real64), allocatable :: oracle_angle(:), oracle_distance(:)
    real(real64), allocatable :: oracle_sign_u(:), oracle_sign_v(:)
    integer :: point_count

    valid = .false.
    message = ''
    if (any(shape(legacy_u) /= shape(legacy_v))) then
      message = 'legacy U/V regular-row source shapes differ'
      return
    end if
    if (panel_index < lbound(legacy_u, 1) .or. &
        panel_index > ubound(legacy_u, 1)) then
      message = 'regular-row panel index is outside legacy storage'
      return
    end if
    if (contour_last_index < 2 .or. &
        contour_last_index > ubound(legacy_u, 2)) then
      message = 'regular-row extrados contour is outside legacy storage'
      return
    end if
    if (sewing_slot < lbound(legacy_u, 3) .or. &
        sewing_slot > ubound(legacy_u, 3) .or. &
        cut_slot < lbound(legacy_u, 3) .or. &
        cut_slot > ubound(legacy_u, 3)) then
      message = 'legacy array has no selected regular production slots'
      return
    end if
    if (reconstruction_start_index < 1 .or. &
        reconstruction_start_index >= contour_last_index) then
      message = 'regular-row reconstruction start is outside the contour'
      return
    end if
    if (any([ubound(segment_angle, 1), ubound(segment_distance, 1), &
        ubound(segment_sign_u, 1), ubound(segment_sign_v, 1)] < &
        contour_last_index - 1)) then
      message = 'regular-row segment scratch arrays are too small'
      return
    end if
    if (.not. ieee_is_finite(source_contour_length) .or. &
        source_contour_length <= 0.0_real64 .or. &
        .not. ieee_is_finite(target_contour_length) .or. &
        target_contour_length <= 0.0_real64 .or. &
        .not. ieee_is_finite(correction_fraction)) then
      message = 'regular-row length-match values are invalid'
      return
    end if

    point_count = contour_last_index
    source%panel_index = panel_index
    source%contour_last_index = contour_last_index
    source%sewing_u = legacy_u(panel_index, 1:point_count, &
        sewing_slot)
    source%sewing_v = legacy_v(panel_index, 1:point_count, &
        sewing_slot)
    source%cut_u = legacy_u(panel_index, 1:point_count, &
        cut_slot)
    source%cut_v = legacy_v(panel_index, 1:point_count, &
        cut_slot)
    if (.not. source%is_valid()) then
      message = 'legacy regular-row source contains invalid geometry'
      return
    end if

    call build_regular_forward_candidate(source, reconstruction_start_index, &
        source_contour_length, target_contour_length, correction_fraction, &
        typed_candidate, typed_angle, typed_distance, typed_sign_u, &
        typed_sign_v, valid, message)
    if (.not. valid) return
    call build_legacy_forward_oracle(source, reconstruction_start_index, &
        source_contour_length, target_contour_length, correction_fraction, &
        legacy_oracle, oracle_angle, oracle_distance, oracle_sign_u, &
        oracle_sign_v, valid, message)
    if (.not. valid) return
    if (.not. edges_are_bit_exact(typed_candidate, legacy_oracle)) then
      valid = .false.
      message = 'typed and legacy regular-row reformats differ'
      return
    end if
    if (.not. scratch_is_bit_exact(typed_angle, oracle_angle, &
        reconstruction_start_index, point_count - 1) .or. &
        .not. scratch_is_bit_exact(typed_distance, oracle_distance, &
        reconstruction_start_index, point_count - 1) .or. &
        .not. scratch_is_bit_exact(typed_sign_u, oracle_sign_u, &
        reconstruction_start_index, point_count - 1) .or. &
        .not. scratch_is_bit_exact(typed_sign_v, oracle_sign_v, &
        reconstruction_start_index, point_count - 1)) then
      valid = .false.
      message = 'typed and legacy regular-row segment scratch differs'
      return
    end if

    legacy_u(panel_index, 1:point_count, &
        sewing_slot) = typed_candidate%sewing_u
    legacy_v(panel_index, 1:point_count, &
        sewing_slot) = typed_candidate%sewing_v
    legacy_u(panel_index, 1:point_count, &
        cut_slot) = typed_candidate%cut_u
    legacy_v(panel_index, 1:point_count, &
        cut_slot) = typed_candidate%cut_v
    segment_angle(reconstruction_start_index:point_count - 1) = &
        typed_angle(reconstruction_start_index:point_count - 1)
    segment_distance(reconstruction_start_index:point_count - 1) = &
        typed_distance(reconstruction_start_index:point_count - 1)
    segment_sign_u(reconstruction_start_index:point_count - 1) = &
        typed_sign_u(reconstruction_start_index:point_count - 1)
    segment_sign_v(reconstruction_start_index:point_count - 1) = &
        typed_sign_v(reconstruction_start_index:point_count - 1)
    valid = .true.
  end subroutine reformat_legacy_regular_extrados_row

  !> Apply one legacy-compatible leading-edge distortion pass to a real panel.
  !!
  !! The pass owns the lower extrados sewing contour in slot 9 for one real
  !! panel row.  Slot 11 is carried through unchanged.  `angle_start_index`
  !! names the segments whose angle/distance scratch is refreshed, while
  !! `reconstruction_start_index` names the first segment rebuilt.  Keeping
  !! those indices distinct is essential: historical inputs may intentionally
  !! consume scratch retained by the immediately preceding row/pass.
  !!
  !! A typed candidate and an independent fixed-form oracle are evaluated from
  !! the same immutable source and incoming scratch.  Coordinates and scratch
  !! are published transactionally only after bit-exact agreement.
  pure subroutine apply_legacy_regular_extrados_distortion_pass(panel_index, &
      contour_last_index, angle_start_index, reconstruction_start_index, &
      original_leading_edge_length, current_leading_edge_length, legacy_u, &
      legacy_v, segment_angle, segment_distance, segment_sign_u, &
      segment_sign_v, valid, message)
    integer, intent(in) :: panel_index, contour_last_index
    integer, intent(in) :: angle_start_index, reconstruction_start_index
    real(real64), intent(in) :: original_leading_edge_length
    real(real64), intent(in) :: current_leading_edge_length
    real(real64), intent(inout) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    real(real64), intent(inout) :: segment_angle(0:), segment_distance(0:)
    real(real64), intent(inout) :: segment_sign_u(0:), segment_sign_v(0:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(regular_extrados_edge_2d) :: source, typed_candidate, legacy_oracle
    real(real64), allocatable :: typed_angle(:), typed_distance(:)
    real(real64), allocatable :: typed_sign_u(:), typed_sign_v(:)
    real(real64), allocatable :: oracle_angle(:), oracle_distance(:)
    real(real64), allocatable :: oracle_sign_u(:), oracle_sign_v(:)
    integer :: point_count

    valid = .false.
    message = ''
    if (any(shape(legacy_u) /= shape(legacy_v))) then
      message = 'legacy U/V distortion-pass shapes differ'
      return
    end if
    if (panel_index < lbound(legacy_u, 1) .or. &
        panel_index > ubound(legacy_u, 1)) then
      message = 'distortion-pass panel index is outside legacy storage'
      return
    end if
    if (contour_last_index < 2 .or. &
        contour_last_index > ubound(legacy_u, 2)) then
      message = 'distortion-pass contour is outside legacy storage'
      return
    end if
    if (legacy_production_lower_sewing_slot < lbound(legacy_u, 3) .or. &
        legacy_production_lower_cut_slot > ubound(legacy_u, 3)) then
      message = 'legacy array has no lower distortion-pass production slots'
      return
    end if
    if (angle_start_index < 1 .or. &
        angle_start_index >= contour_last_index .or. &
        reconstruction_start_index < 1 .or. &
        reconstruction_start_index >= contour_last_index) then
      message = 'distortion-pass segment range is outside the contour'
      return
    end if
    if (any([ubound(segment_angle, 1), ubound(segment_distance, 1), &
        ubound(segment_sign_u, 1), ubound(segment_sign_v, 1)] < &
        contour_last_index - 1)) then
      message = 'distortion-pass segment scratch arrays are too small'
      return
    end if
    if (.not. ieee_is_finite(original_leading_edge_length) .or. &
        original_leading_edge_length < 0.0_real64 .or. &
        .not. ieee_is_finite(current_leading_edge_length) .or. &
        current_leading_edge_length < 0.0_real64) then
      message = 'distortion-pass leading-edge lengths are invalid'
      return
    end if

    point_count = contour_last_index
    source%panel_index = panel_index
    source%contour_last_index = contour_last_index
    source%sewing_u = legacy_u(panel_index, 1:point_count, &
        legacy_production_lower_sewing_slot)
    source%sewing_v = legacy_v(panel_index, 1:point_count, &
        legacy_production_lower_sewing_slot)
    source%cut_u = legacy_u(panel_index, 1:point_count, &
        legacy_production_lower_cut_slot)
    source%cut_v = legacy_v(panel_index, 1:point_count, &
        legacy_production_lower_cut_slot)
    if (.not. source%is_valid()) then
      message = 'legacy distortion-pass source contains invalid geometry'
      return
    end if

    call build_extrados_distortion_candidate(source, angle_start_index, &
        reconstruction_start_index, original_leading_edge_length, &
        current_leading_edge_length, segment_angle, segment_distance, &
        segment_sign_u, segment_sign_v, typed_candidate, typed_angle, &
        typed_distance, typed_sign_u, typed_sign_v, valid, message)
    if (.not. valid) return
    call build_legacy_extrados_distortion_oracle(source, angle_start_index, &
        reconstruction_start_index, original_leading_edge_length, &
        current_leading_edge_length, segment_angle, segment_distance, &
        segment_sign_u, segment_sign_v, legacy_oracle, oracle_angle, &
        oracle_distance, oracle_sign_u, oracle_sign_v, valid, message)
    if (.not. valid) return
    if (.not. edges_are_bit_exact(typed_candidate, legacy_oracle) .or. &
        .not. scratch_is_bit_exact(typed_angle, oracle_angle, &
        min(angle_start_index, reconstruction_start_index), &
        point_count - 1) .or. &
        .not. scratch_is_bit_exact(typed_distance, oracle_distance, &
        min(angle_start_index, reconstruction_start_index), &
        point_count - 1) .or. &
        .not. scratch_is_bit_exact(typed_sign_u, oracle_sign_u, &
        min(angle_start_index, reconstruction_start_index), &
        point_count - 1) .or. &
        .not. scratch_is_bit_exact(typed_sign_v, oracle_sign_v, &
        min(angle_start_index, reconstruction_start_index), &
        point_count - 1)) then
      valid = .false.
      message = 'typed and legacy extrados distortion passes differ'
      return
    end if

    legacy_u(panel_index, 1:point_count, &
        legacy_production_lower_sewing_slot) = typed_candidate%sewing_u
    legacy_v(panel_index, 1:point_count, &
        legacy_production_lower_sewing_slot) = typed_candidate%sewing_v
    legacy_u(panel_index, 1:point_count, &
        legacy_production_lower_cut_slot) = typed_candidate%cut_u
    legacy_v(panel_index, 1:point_count, &
        legacy_production_lower_cut_slot) = typed_candidate%cut_v
    segment_angle(angle_start_index:point_count - 1) = &
        typed_angle(angle_start_index:point_count - 1)
    segment_distance(angle_start_index:point_count - 1) = &
        typed_distance(angle_start_index:point_count - 1)
    segment_sign_u(angle_start_index:point_count - 1) = &
        typed_sign_u(angle_start_index:point_count - 1)
    segment_sign_v(angle_start_index:point_count - 1) = &
        typed_sign_v(angle_start_index:point_count - 1)
    valid = .true.
  end subroutine apply_legacy_regular_extrados_distortion_pass

  !> Construct one typed distortion-pass candidate from immutable inputs.
  pure subroutine build_extrados_distortion_candidate(source, &
      angle_start_index, reconstruction_start_index, &
      original_leading_edge_length, current_leading_edge_length, &
      incoming_angle, incoming_distance, incoming_sign_u, incoming_sign_v, &
      candidate, segment_angle, segment_distance, segment_sign_u, &
      segment_sign_v, valid, message)
    type(regular_extrados_edge_2d), intent(in) :: source
    integer, intent(in) :: angle_start_index, reconstruction_start_index
    real(real64), intent(in) :: original_leading_edge_length
    real(real64), intent(in) :: current_leading_edge_length
    real(real64), intent(in) :: incoming_angle(0:), incoming_distance(0:)
    real(real64), intent(in) :: incoming_sign_u(0:), incoming_sign_v(0:)
    type(regular_extrados_edge_2d), intent(inout) :: candidate
    real(real64), allocatable, intent(out) :: segment_angle(:)
    real(real64), allocatable, intent(out) :: segment_distance(:)
    real(real64), allocatable, intent(out) :: segment_sign_u(:)
    real(real64), allocatable, intent(out) :: segment_sign_v(:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    real(real32) :: legacy_epsilon, legacy_omega
    real(real64) :: straight_distance, delta_u, delta_v
    integer :: copy_first_index, point_count, point_index

    valid = .false.
    message = ''
    if (.not. source%is_valid()) then
      message = 'cannot distort an invalid regular extrados edge'
      return
    end if
    point_count = size(source%sewing_u)
    straight_distance = sqrt( &
        (source%sewing_u(angle_start_index) - &
        source%sewing_u(point_count))**legacy_square_exponent + &
        (source%sewing_v(angle_start_index) - &
        source%sewing_v(point_count))**legacy_square_exponent)
    legacy_epsilon = current_leading_edge_length - &
        original_leading_edge_length
    if (abs(legacy_epsilon) < real(0.01, real32)) then
      legacy_omega = 0.0_real32
    else
      if (.not. ieee_is_finite(straight_distance) .or. &
          straight_distance <= 0.0_real64 .or. &
          abs(real(legacy_epsilon, real64)/straight_distance) > &
          1.0_real64) then
        message = 'extrados distortion rotation is outside asin domain'
        return
      end if
      legacy_omega = real(1.0, real32)*asin( &
          real(legacy_epsilon, real64)/straight_distance)* &
          legacy_epsilon/abs(legacy_epsilon)
    end if

    allocate(segment_angle(point_count), segment_distance(point_count), &
        segment_sign_u(point_count), segment_sign_v(point_count))
    segment_angle = 0.0_real64
    segment_distance = 0.0_real64
    segment_sign_u = 0.0_real64
    segment_sign_v = 0.0_real64
    copy_first_index = min(angle_start_index, reconstruction_start_index)
    segment_angle(copy_first_index:point_count - 1) = &
        incoming_angle(copy_first_index:point_count - 1)
    segment_distance(copy_first_index:point_count - 1) = &
        incoming_distance(copy_first_index:point_count - 1)
    segment_sign_u(copy_first_index:point_count - 1) = &
        incoming_sign_u(copy_first_index:point_count - 1)
    segment_sign_v(copy_first_index:point_count - 1) = &
        incoming_sign_v(copy_first_index:point_count - 1)
    do point_index = angle_start_index, point_count - 1
      delta_v = source%sewing_v(point_index + 1) - &
          source%sewing_v(point_index)
      delta_u = source%sewing_u(point_index + 1) - &
          source%sewing_u(point_index)
      if (delta_u /= 0.0_real64) then
        segment_angle(point_index) = abs(atan(delta_v/delta_u)) + &
            legacy_omega
      else
        segment_angle(point_index) = half_pi + legacy_omega
      end if
      call legacy_axis_signs(delta_u, delta_v, &
          segment_sign_u(point_index), segment_sign_v(point_index))
      segment_distance(point_index) = sqrt( &
          (source%sewing_v(point_index) - &
          source%sewing_v(point_index + 1))**legacy_square_exponent + &
          (source%sewing_u(point_index) - &
          source%sewing_u(point_index + 1))**legacy_square_exponent)
    end do

    candidate = source
    do point_index = reconstruction_start_index, point_count - 1
      candidate%sewing_u(point_index + 1) = &
          candidate%sewing_u(point_index) + &
          segment_sign_u(point_index)*segment_distance(point_index)* &
          cos(segment_angle(point_index))
      candidate%sewing_v(point_index + 1) = &
          candidate%sewing_v(point_index) + &
          segment_sign_v(point_index)*segment_distance(point_index)* &
          sin(segment_angle(point_index))
    end do
    if (.not. candidate%is_valid()) then
      message = 'extrados distortion pass produced invalid geometry'
      return
    end if
    valid = .true.
  end subroutine build_extrados_distortion_candidate

  !> Independently reproduce one fixed-form distortion pass as an oracle.
  pure subroutine build_legacy_extrados_distortion_oracle(source, &
      angle_start_index, reconstruction_start_index, &
      original_leading_edge_length, current_leading_edge_length, &
      incoming_angle, incoming_distance, incoming_sign_u, incoming_sign_v, &
      oracle, anglee, distee, siu, siv, valid, message)
    type(regular_extrados_edge_2d), intent(in) :: source
    integer, intent(in) :: angle_start_index, reconstruction_start_index
    real(real64), intent(in) :: original_leading_edge_length
    real(real64), intent(in) :: current_leading_edge_length
    real(real64), intent(in) :: incoming_angle(0:), incoming_distance(0:)
    real(real64), intent(in) :: incoming_sign_u(0:), incoming_sign_v(0:)
    type(regular_extrados_edge_2d), intent(inout) :: oracle
    real(real64), allocatable, intent(out) :: anglee(:), distee(:)
    real(real64), allocatable, intent(out) :: siu(:), siv(:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    real(real32) :: epsilon, omega
    real(real64) :: dist, xdu, xdv
    integer :: copy_first_index, j, point_count

    valid = .false.
    message = ''
    point_count = size(source%sewing_u)
    dist = sqrt( &
        (source%sewing_u(angle_start_index) - &
        source%sewing_u(point_count))**legacy_square_exponent + &
        (source%sewing_v(angle_start_index) - &
        source%sewing_v(point_count))**legacy_square_exponent)
    epsilon = current_leading_edge_length - original_leading_edge_length
    if (abs(epsilon) < real(0.01, real32)) then
      omega = 0.0_real32
    else
      if (.not. ieee_is_finite(dist) .or. dist <= 0.0_real64 .or. &
          abs(real(epsilon, real64)/dist) > 1.0_real64) then
        message = 'legacy distortion rotation is outside asin domain'
        return
      end if
      omega = real(1.0, real32)*asin(real(epsilon, real64)/dist)* &
          epsilon/abs(epsilon)
    end if

    allocate(anglee(point_count), distee(point_count), siu(point_count), &
        siv(point_count))
    anglee = 0.0_real64
    distee = 0.0_real64
    siu = 0.0_real64
    siv = 0.0_real64
    copy_first_index = min(angle_start_index, reconstruction_start_index)
    anglee(copy_first_index:point_count - 1) = &
        incoming_angle(copy_first_index:point_count - 1)
    distee(copy_first_index:point_count - 1) = &
        incoming_distance(copy_first_index:point_count - 1)
    siu(copy_first_index:point_count - 1) = &
        incoming_sign_u(copy_first_index:point_count - 1)
    siv(copy_first_index:point_count - 1) = &
        incoming_sign_v(copy_first_index:point_count - 1)
    do j = angle_start_index, point_count - 1
      xdv = source%sewing_v(j + 1) - source%sewing_v(j)
      xdu = source%sewing_u(j + 1) - source%sewing_u(j)
      if (xdu /= 0.0_real64) then
        anglee(j) = abs(atan(xdv/xdu)) + omega
      else
        anglee(j) = half_pi + omega
      end if
      call legacy_axis_signs(xdu, xdv, siu(j), siv(j))
      distee(j) = sqrt( &
          (source%sewing_v(j) - source%sewing_v(j + 1))** &
          legacy_square_exponent + &
          (source%sewing_u(j) - source%sewing_u(j + 1))** &
          legacy_square_exponent)
    end do

    oracle = source
    do j = reconstruction_start_index, point_count - 1
      oracle%sewing_u(j + 1) = oracle%sewing_u(j) + &
          siu(j)*distee(j)*cos(anglee(j))
      oracle%sewing_v(j + 1) = oracle%sewing_v(j) + &
          siv(j)*distee(j)*sin(anglee(j))
    end do
    if (.not. oracle%is_valid()) then
      message = 'legacy extrados distortion pass produced invalid geometry'
      return
    end if
    valid = .true.
  end subroutine build_legacy_extrados_distortion_oracle

  !> Construct the typed forward reconstruction used as production authority.
  pure subroutine build_regular_forward_candidate(source, start_index, &
      source_contour_length, target_contour_length, correction_fraction, &
      candidate, segment_angle, segment_distance, segment_sign_u, &
      segment_sign_v, valid, message)
    type(regular_extrados_edge_2d), intent(in) :: source
    integer, intent(in) :: start_index
    real(real64), intent(in) :: source_contour_length, target_contour_length
    real(real64), intent(in) :: correction_fraction
    type(regular_extrados_edge_2d), intent(inout) :: candidate
    real(real64), allocatable, intent(out) :: segment_angle(:)
    real(real64), allocatable, intent(out) :: segment_distance(:)
    real(real64), allocatable, intent(out) :: segment_sign_u(:)
    real(real64), allocatable, intent(out) :: segment_sign_v(:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    real(real32) :: measured_length, corrected_length, length_scale
    real(real64) :: delta_u, delta_v
    integer :: point_index, point_count

    valid = .false.
    message = ''
    if (.not. source%is_valid()) then
      message = 'cannot reformat an invalid regular extrados edge'
      return
    end if
    point_count = size(source%sewing_u)
    if (start_index < 1 .or. start_index >= point_count) then
      message = 'regular extrados start index is outside its contour'
      return
    end if

    measured_length = 0.0_real32
    do point_index = start_index, point_count - 1
      measured_length = measured_length + sqrt( &
          (source%sewing_v(point_index) - &
          source%sewing_v(point_index + 1))**legacy_square_exponent + &
          (source%sewing_u(point_index) - &
          source%sewing_u(point_index + 1))**legacy_square_exponent)
    end do
    if (.not. ieee_is_finite(measured_length) .or. &
        measured_length <= 0.0_real32) then
      message = 'regular extrados has no measurable reformat run'
      return
    end if
    corrected_length = measured_length + correction_fraction*( &
        target_contour_length - source_contour_length)
    length_scale = corrected_length/measured_length
    if (.not. ieee_is_finite(length_scale) .or. &
        length_scale <= 0.0_real32) then
      message = 'regular extrados scale is not positive and finite'
      return
    end if

    allocate(segment_angle(point_count), segment_distance(point_count), &
        segment_sign_u(point_count), segment_sign_v(point_count))
    segment_angle = 0.0_real64
    segment_distance = 0.0_real64
    segment_sign_u = 0.0_real64
    segment_sign_v = 0.0_real64
    do point_index = start_index, point_count - 1
      delta_v = source%sewing_v(point_index + 1) - &
          source%sewing_v(point_index)
      delta_u = source%sewing_u(point_index + 1) - &
          source%sewing_u(point_index)
      if (delta_u /= 0.0_real64) then
        segment_angle(point_index) = abs(atan(delta_v/delta_u))
      else
        segment_angle(point_index) = half_pi
      end if
      call legacy_axis_signs(delta_u, delta_v, &
          segment_sign_u(point_index), segment_sign_v(point_index))
      segment_distance(point_index) = sqrt( &
          (source%sewing_v(point_index) - &
          source%sewing_v(point_index + 1))**legacy_square_exponent + &
          (source%sewing_u(point_index) - &
          source%sewing_u(point_index + 1))**legacy_square_exponent)
    end do

    candidate = source
    do point_index = start_index, point_count - 1
      candidate%sewing_u(point_index + 1) = &
          candidate%sewing_u(point_index) + &
          segment_sign_u(point_index)*length_scale* &
          segment_distance(point_index)*cos(segment_angle(point_index))
      candidate%sewing_v(point_index + 1) = &
          candidate%sewing_v(point_index) + &
          segment_sign_v(point_index)*length_scale* &
          segment_distance(point_index)*sin(segment_angle(point_index))
    end do
    if (.not. candidate%is_valid()) then
      message = 'regular extrados reformat produced invalid geometry'
      return
    end if
    valid = .true.
  end subroutine build_regular_forward_candidate

  !> Independently reproduce the fixed-form forward loop as a migration oracle.
  !!
  !! This intentional duplication is temporary.  Once the checked application
  !! fixtures establish the typed kernel as the sole compatibility authority,
  !! remove this oracle and its bit-exact gate rather than maintaining two
  !! production algorithms.
  pure subroutine build_legacy_forward_oracle(source, start_index, &
      source_contour_length, target_contour_length, correction_fraction, &
      oracle, anglee, distee, siu, siv, valid, message)
    type(regular_extrados_edge_2d), intent(in) :: source
    integer, intent(in) :: start_index
    real(real64), intent(in) :: source_contour_length, target_contour_length
    real(real64), intent(in) :: correction_fraction
    type(regular_extrados_edge_2d), intent(inout) :: oracle
    real(real64), allocatable, intent(out) :: anglee(:), distee(:)
    real(real64), allocatable, intent(out) :: siu(:), siv(:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    real(real32) :: dist2, dist3, distk
    real(real64) :: xdu, xdv
    integer :: j, point_count

    valid = .false.
    message = ''
    point_count = size(source%sewing_u)
    allocate(anglee(point_count), distee(point_count), siu(point_count), &
        siv(point_count))
    anglee = 0.0_real64
    distee = 0.0_real64
    siu = 0.0_real64
    siv = 0.0_real64

    dist2 = 0.0_real32
    do j = start_index, point_count - 1
      dist2 = dist2 + sqrt( &
          (source%sewing_v(j) - source%sewing_v(j + 1))** &
          legacy_square_exponent + &
          (source%sewing_u(j) - source%sewing_u(j + 1))** &
          legacy_square_exponent)
    end do
    if (.not. ieee_is_finite(dist2) .or. dist2 <= 0.0_real32) then
      message = 'legacy regular-row oracle has no measurable reformat run'
      return
    end if
    dist3 = dist2 + correction_fraction*( &
        target_contour_length - source_contour_length)
    distk = dist3/dist2
    if (.not. ieee_is_finite(distk) .or. distk <= 0.0_real32) then
      message = 'legacy regular-row oracle scale is not positive and finite'
      return
    end if

    do j = start_index, point_count - 1
      xdv = source%sewing_v(j + 1) - source%sewing_v(j)
      xdu = source%sewing_u(j + 1) - source%sewing_u(j)
      if (xdu /= 0.0_real64) then
        anglee(j) = abs(atan(xdv/xdu))
      else
        anglee(j) = half_pi
      end if
      call legacy_axis_signs(xdu, xdv, siu(j), siv(j))
      distee(j) = sqrt( &
          (source%sewing_v(j) - source%sewing_v(j + 1))** &
          legacy_square_exponent + &
          (source%sewing_u(j) - source%sewing_u(j + 1))** &
          legacy_square_exponent)
    end do

    oracle = source
    do j = start_index, point_count - 1
      oracle%sewing_u(j + 1) = oracle%sewing_u(j) + &
          siu(j)*distk*distee(j)*cos(anglee(j))
      oracle%sewing_v(j + 1) = oracle%sewing_v(j) + &
          siv(j)*distk*distee(j)*sin(anglee(j))
    end do
    if (.not. oracle%is_valid()) then
      message = 'legacy regular-row oracle produced invalid geometry'
      return
    end if
    valid = .true.
  end subroutine build_legacy_forward_oracle

  !> Preserve the fixed-form independent quadrant-condition ordering.
  pure subroutine legacy_axis_signs(delta_u, delta_v, sign_u, sign_v)
    real(real64), intent(in) :: delta_u, delta_v
    real(real64), intent(out) :: sign_u, sign_v

    sign_u = 0.0_real64
    sign_v = 0.0_real64
    if (delta_u >= 0.0_real64 .and. delta_v >= 0.0_real64) then
      sign_u = 1.0_real64
      sign_v = 1.0_real64
    end if
    if (delta_u <= 0.0_real64 .and. delta_v >= 0.0_real64) then
      sign_u = -1.0_real64
      sign_v = 1.0_real64
    end if
    if (delta_u >= 0.0_real64 .and. delta_v <= 0.0_real64) then
      sign_u = 1.0_real64
      sign_v = -1.0_real64
    end if
    if (delta_u <= 0.0_real64 .and. delta_v <= 0.0_real64) then
      sign_u = -1.0_real64
      sign_v = -1.0_real64
    end if
  end subroutine legacy_axis_signs

  !> Compare all owned coordinates by representation, including signed zero.
  pure logical function edges_are_bit_exact(first, second)
    type(regular_extrados_edge_2d), intent(in) :: first, second

    integer :: point_index

    edges_are_bit_exact = .false.
    if (.not. first%is_valid() .or. .not. second%is_valid()) return
    if (first%panel_index /= second%panel_index .or. &
        first%contour_first_index /= second%contour_first_index .or. &
        first%contour_last_index /= second%contour_last_index) return
    do point_index = 1, size(first%sewing_u)
      if (.not. same_real64_bits(first%sewing_u(point_index), &
          second%sewing_u(point_index)) .or. &
          .not. same_real64_bits(first%sewing_v(point_index), &
          second%sewing_v(point_index)) .or. &
          .not. same_real64_bits(first%cut_u(point_index), &
          second%cut_u(point_index)) .or. &
          .not. same_real64_bits(first%cut_v(point_index), &
          second%cut_v(point_index))) return
    end do
    edges_are_bit_exact = .true.
  end function edges_are_bit_exact

  !> Compare the populated part of one migration scratch array bit for bit.
  pure logical function scratch_is_bit_exact(first, second, first_index, &
      last_index)
    real(real64), intent(in) :: first(:), second(:)
    integer, intent(in) :: first_index, last_index

    integer :: point_index

    scratch_is_bit_exact = .false.
    if (lbound(first, 1) /= lbound(second, 1) .or. &
        ubound(first, 1) /= ubound(second, 1)) return
    if (first_index < lbound(first, 1) .or. &
        last_index > ubound(first, 1) .or. first_index > last_index) return
    do point_index = first_index, last_index
      if (.not. same_real64_bits(first(point_index), &
          second(point_index))) return
    end do
    scratch_is_bit_exact = .true.
  end function scratch_is_bit_exact

  pure logical function same_real64_bits(first, second)
    real(real64), intent(in) :: first, second
    integer(int64) :: first_bits, second_bits

    first_bits = transfer(first, first_bits)
    second_bits = transfer(second, second_bits)
    same_real64_bits = first_bits == second_bits
  end function same_real64_bits

  !> Validate the provenance and exact two-point relationship of join support.
  pure logical function preceding_join_support_is_valid(support)
    class(preceding_join_support_2d), intent(in) :: support

    real(real64) :: values(6)

    preceding_join_support_is_valid = .false.
    if (support%source_panel_index < 0) return
    if (support%boundary_rib_index /= support%source_panel_index + 1) return
    if (support%boundary_contour_first_index < 3) return
    if (support%support_point_index /= &
        support%boundary_contour_first_index - 1) return
    if (support%anchor_point_index /= support%support_point_index - 1) return
    values = [support%anchor_sewing_u, support%anchor_sewing_v, &
        support%sewing_u, support%sewing_v, support%cut_u, support%cut_v]
    if (.not. all(ieee_is_finite(values))) return
    preceding_join_support_is_valid = .true.
  end function preceding_join_support_is_valid

  !> Snapshot the terminal point immediately before the typed intrados range.
  !!
  !! This is a checked read adapter.  It never treats the support as part of
  !! the intrados boundary and leaves `support` unchanged on failure.
  pure subroutine copy_legacy_preceding_join_support(boundary, legacy_u, &
      legacy_v, support, valid, message)
    type(production_boundary_edge_2d), intent(in) :: boundary
    real(real64), intent(in) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    type(preceding_join_support_2d), intent(inout) :: support
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(preceding_join_support_2d) :: candidate

    valid = .false.
    message = ''
    if (.not. all(shape(legacy_u) == shape(legacy_v))) then
      message = 'legacy U/V join-support source shapes differ'
      return
    end if
    if (.not. boundary%is_valid() .or. &
        boundary%surface /= surface_intrados) then
      message = 'join support requires a valid terminal intrados boundary'
      return
    end if
    if (boundary%contour_first_index < 3) then
      message = 'terminal intrados range has no preceding join-support pair'
      return
    end if

    candidate%boundary_rib_index = boundary%boundary_rib_index
    candidate%source_panel_index = boundary%source_panel_index
    candidate%boundary_contour_first_index = boundary%contour_first_index
    candidate%support_point_index = boundary%contour_first_index - 1
    candidate%anchor_point_index = candidate%support_point_index - 1
    if (candidate%boundary_rib_index < lbound(legacy_u, 1) .or. &
        candidate%boundary_rib_index > ubound(legacy_u, 1) .or. &
        candidate%anchor_point_index < lbound(legacy_u, 2) .or. &
        candidate%support_point_index > ubound(legacy_u, 2) .or. &
        legacy_production_lower_sewing_slot < lbound(legacy_u, 3) .or. &
        legacy_production_lower_cut_slot > ubound(legacy_u, 3)) then
      message = 'terminal join support is outside legacy source storage'
      return
    end if

    candidate%anchor_sewing_u = legacy_u(candidate%boundary_rib_index, &
        candidate%anchor_point_index, legacy_production_lower_sewing_slot)
    candidate%anchor_sewing_v = legacy_v(candidate%boundary_rib_index, &
        candidate%anchor_point_index, legacy_production_lower_sewing_slot)
    candidate%sewing_u = legacy_u(candidate%boundary_rib_index, &
        candidate%support_point_index, legacy_production_lower_sewing_slot)
    candidate%sewing_v = legacy_v(candidate%boundary_rib_index, &
        candidate%support_point_index, legacy_production_lower_sewing_slot)
    candidate%cut_u = legacy_u(candidate%boundary_rib_index, &
        candidate%support_point_index, legacy_production_lower_cut_slot)
    candidate%cut_v = legacy_v(candidate%boundary_rib_index, &
        candidate%support_point_index, legacy_production_lower_cut_slot)
    if (.not. candidate%is_valid()) then
      message = 'legacy terminal join support is invalid'
      return
    end if

    support = candidate
    valid = .true.
  end subroutine copy_legacy_preceding_join_support

  !> Reproduce the legacy `anchor + anchor - support` extrapolation.
  !!
  !! The cut point receives exactly the sewing-point displacement so its
  !! established allowance vector is not stranded by the rewrite.
  pure subroutine reformat_preceding_join_support(source, reformatted, valid, &
      message)
    type(preceding_join_support_2d), intent(in) :: source
    type(preceding_join_support_2d), intent(inout) :: reformatted
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(preceding_join_support_2d) :: candidate
    real(real64) :: displacement_u, displacement_v

    valid = .false.
    message = ''
    if (.not. source%is_valid()) then
      message = 'cannot reformat invalid terminal join support'
      return
    end if
    candidate = source
    candidate%sewing_u = source%anchor_sewing_u + &
        source%anchor_sewing_u - source%sewing_u
    candidate%sewing_v = source%anchor_sewing_v + &
        source%anchor_sewing_v - source%sewing_v
    displacement_u = candidate%sewing_u - source%sewing_u
    displacement_v = candidate%sewing_v - source%sewing_v
    candidate%cut_u = source%cut_u + displacement_u
    candidate%cut_v = source%cut_v + displacement_v
    if (.not. candidate%is_valid()) then
      message = 'terminal join-support reformat produced invalid geometry'
      return
    end if
    reformatted = candidate
    valid = .true.
  end subroutine reformat_preceding_join_support

  !> Publish one terminal join-support sewing/cut pair transactionally.
  pure subroutine write_legacy_preceding_join_support(support, legacy_u, &
      legacy_v, valid, message)
    type(preceding_join_support_2d), intent(in) :: support
    real(real64), intent(inout) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    valid = .false.
    message = ''
    if (.not. all(shape(legacy_u) == shape(legacy_v))) then
      message = 'legacy U/V join-support destination shapes differ'
      return
    end if
    if (.not. support%is_valid()) then
      message = 'cannot publish invalid terminal join support'
      return
    end if
    if (support%boundary_rib_index < lbound(legacy_u, 1) .or. &
        support%boundary_rib_index > ubound(legacy_u, 1) .or. &
        support%support_point_index < lbound(legacy_u, 2) .or. &
        support%support_point_index > ubound(legacy_u, 2) .or. &
        legacy_production_lower_sewing_slot < lbound(legacy_u, 3) .or. &
        legacy_production_lower_cut_slot > ubound(legacy_u, 3)) then
      message = 'terminal join support is outside legacy destination storage'
      return
    end if

    legacy_u(support%boundary_rib_index, support%support_point_index, &
        legacy_production_lower_sewing_slot) = support%sewing_u
    legacy_v(support%boundary_rib_index, support%support_point_index, &
        legacy_production_lower_sewing_slot) = support%sewing_v
    legacy_u(support%boundary_rib_index, support%support_point_index, &
        legacy_production_lower_cut_slot) = support%cut_u
    legacy_v(support%boundary_rib_index, support%support_point_index, &
        legacy_production_lower_cut_slot) = support%cut_v
    valid = .true.
  end subroutine write_legacy_preceding_join_support

  !> Validate a length-match request against its exact terminal contour.
  pure logical function boundary_length_match_control_is_valid_for( &
      control, boundary)
    class(boundary_length_match_control), intent(in) :: control
    type(production_boundary_edge_2d), intent(in) :: boundary

    boundary_length_match_control_is_valid_for = .false.
    if (.not. boundary%is_valid()) return
    if (boundary%surface /= surface_intrados) return
    if (control%measurement_start_index <= &
        boundary%contour_first_index .or. &
        control%measurement_start_index > &
        boundary%contour_last_index) return
    if (control%reconstruction_start_index <= &
        boundary%contour_first_index .or. &
        control%reconstruction_start_index > &
        boundary%contour_last_index) return
    if (.not. ieee_is_finite(control%target_contour_length)) return
    if (control%target_contour_length <= 0.0_real64) return
    if (.not. ieee_is_finite(control%source_contour_length)) return
    if (control%source_contour_length <= 0.0_real64) return
    if (.not. ieee_is_finite(control%correction_fraction)) return
    boundary_length_match_control_is_valid_for = .true.
  end function boundary_length_match_control_is_valid_for

  !> Rebuild the selected terminal sewing run and translate its cut points.
  !!
  !! Segment lengths, quadrant signs, angle reconstruction, and operation
  !! ordering reproduce the right-intrados `ndif=1000` loop.  All angle and
  !! distance inputs are captured before reconstruction because the legacy
  !! loop also used scratch arrays populated from the original contour.
  !!
  !! Publication is transactional: `reformatted_boundary` changes only after
  !! every input and resulting coordinate has passed validation.
  pure subroutine reformat_terminal_intrados_boundary(boundary, control, &
      reformatted_boundary, valid, message)
    type(production_boundary_edge_2d), intent(in) :: boundary
    type(boundary_length_match_control), intent(in) :: control
    type(production_boundary_edge_2d), intent(inout) :: reformatted_boundary
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(production_boundary_edge_2d) :: candidate
    real(real64), allocatable :: segment_angle(:), segment_distance(:)
    real(real64), allocatable :: segment_sign_u(:), segment_sign_v(:)
    real(real32) :: legacy_measured_length, legacy_corrected_length
    real(real32) :: legacy_length_scale
    real(real64) :: correction, delta_u, delta_v
    integer :: point_count, point_index, measurement_index
    integer :: reconstruction_index

    valid = .false.
    message = ''
    if (.not. control%is_valid_for(boundary)) then
      message = 'invalid terminal boundary length-match request'
      return
    end if

    point_count = size(boundary%sewing_u)
    measurement_index = control%measurement_start_index - &
        boundary%contour_first_index + 1
    reconstruction_index = control%reconstruction_start_index - &
        boundary%contour_first_index + 1

    ! `dist2`, `dist3`, and `distk` have no declarations in the fixed-form
    ! main program.  Implicit typing therefore rounds the accumulator,
    ! corrected length, and scale to default REAL after each assignment.
    legacy_measured_length = 0.0_real32
    do point_index = 1, measurement_index - 1
      legacy_measured_length = legacy_measured_length + sqrt( &
          (boundary%sewing_v(point_index) - &
          boundary%sewing_v(point_index + 1))**legacy_square_exponent + &
          (boundary%sewing_u(point_index) - &
          boundary%sewing_u(point_index + 1))**legacy_square_exponent)
    end do
    if (.not. ieee_is_finite(legacy_measured_length) .or. &
        legacy_measured_length <= 0.0_real32) then
      message = 'terminal boundary has no measurable reformat run'
      return
    end if

    correction = control%correction_fraction*( &
        control%target_contour_length - control%source_contour_length)
    legacy_corrected_length = legacy_measured_length + correction
    legacy_length_scale = legacy_corrected_length/legacy_measured_length
    if (.not. ieee_is_finite(legacy_length_scale) .or. &
        legacy_length_scale <= 0.0_real32) then
      message = 'terminal boundary reformat scale is not positive and finite'
      return
    end if

    allocate(segment_angle(point_count), segment_distance(point_count), &
        segment_sign_u(point_count), segment_sign_v(point_count))
    segment_angle = 0.0_real64
    segment_distance = 0.0_real64
    segment_sign_u = 0.0_real64
    segment_sign_v = 0.0_real64

    do point_index = reconstruction_index, 2, -1
      delta_v = boundary%sewing_v(point_index - 1) - &
          boundary%sewing_v(point_index)
      delta_u = boundary%sewing_u(point_index - 1) - &
          boundary%sewing_u(point_index)
      if (delta_u /= 0.0_real64) then
        segment_angle(point_index) = abs(atan(delta_v/delta_u))
      else
        segment_angle(point_index) = half_pi
      end if

      call legacy_axis_signs(delta_u, delta_v, &
          segment_sign_u(point_index), segment_sign_v(point_index))
      segment_distance(point_index) = sqrt( &
          (boundary%sewing_v(point_index) - &
          boundary%sewing_v(point_index - 1))**legacy_square_exponent + &
          (boundary%sewing_u(point_index) - &
          boundary%sewing_u(point_index - 1))**legacy_square_exponent)
    end do

    candidate = boundary
    do point_index = reconstruction_index, 2, -1
      candidate%sewing_u(point_index - 1) = &
          candidate%sewing_u(point_index) + &
          segment_sign_u(point_index)*legacy_length_scale* &
          segment_distance(point_index)*cos(segment_angle(point_index))
      candidate%sewing_v(point_index - 1) = &
          candidate%sewing_v(point_index) + &
          segment_sign_v(point_index)*legacy_length_scale* &
          segment_distance(point_index)*sin(segment_angle(point_index))
    end do

    ! Preserve each established allowance vector while moving the sewing and
    ! cut points together.  The legacy path moved slot 9 but stranded slot 11.
    candidate%cut_u = boundary%cut_u + &
        (candidate%sewing_u - boundary%sewing_u)
    candidate%cut_v = boundary%cut_v + &
        (candidate%sewing_v - boundary%sewing_v)
    if (.not. candidate%is_valid()) then
      message = 'terminal boundary reformat produced invalid geometry'
      return
    end if

    reformatted_boundary = candidate
    valid = .true.
  end subroutine reformat_terminal_intrados_boundary

end module leparagliding_panel_reformat
