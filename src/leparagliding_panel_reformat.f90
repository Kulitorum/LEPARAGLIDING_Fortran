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
  use, intrinsic :: iso_fortran_env, only : real32, real64
  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
  use leparagliding_domain_model, only : production_boundary_edge_2d, &
      surface_intrados
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

  public :: reformat_terminal_intrados_boundary

contains

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

      ! These independent conditions preserve the legacy axis sign order.
      if (delta_u >= 0.0_real64 .and. delta_v >= 0.0_real64) then
        segment_sign_u(point_index) = 1.0_real64
        segment_sign_v(point_index) = 1.0_real64
      end if
      if (delta_u <= 0.0_real64 .and. delta_v >= 0.0_real64) then
        segment_sign_u(point_index) = -1.0_real64
        segment_sign_v(point_index) = 1.0_real64
      end if
      if (delta_u >= 0.0_real64 .and. delta_v <= 0.0_real64) then
        segment_sign_u(point_index) = 1.0_real64
        segment_sign_v(point_index) = -1.0_real64
      end if
      if (delta_u <= 0.0_real64 .and. delta_v <= 0.0_real64) then
        segment_sign_u(point_index) = -1.0_real64
        segment_sign_v(point_index) = -1.0_real64
      end if
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
