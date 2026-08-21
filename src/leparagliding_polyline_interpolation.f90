! SPDX-License-Identifier: GPL-3.0-or-later
!
!> Checked arc-length interpolation on a two-dimensional polyline.
!!
!! The legacy `interpseg` routine mixed length measurement, segment selection,
!! interpolation, and fixed-size work arrays.  This module gives the result
!! named ownership and validates the complete request before changing it.  A
!! compatibility wrapper retains the historical call shape while the four
!! fixed-form callers continue to use the procedure facade.
module leparagliding_polyline_interpolation
  use, intrinsic :: iso_fortran_env, only : error_unit, real64
  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
  implicit none
  private

  integer, parameter :: legacy_coordinate_capacity = 1000
  integer, parameter :: legacy_result_capacity = 100

  !> One successfully interpolated location and its source-segment provenance.
  type, public :: polyline_interpolation_result
    integer :: segment_start_index = 0
    real(real64) :: distance_into_segment = 0.0_real64
    real(real64) :: total_length = 0.0_real64
    real(real64) :: horizontal_coordinate = 0.0_real64
    real(real64) :: vertical_coordinate = 0.0_real64
  contains
    procedure :: is_valid => polyline_interpolation_result_is_valid
  end type polyline_interpolation_result

  public :: interpolate_polyline_at_distance
  public :: interpseg

contains

  !> Check the intrinsic invariants that do not require the source polyline.
  pure logical function polyline_interpolation_result_is_valid(result) &
      result(valid)
    class(polyline_interpolation_result), intent(in) :: result

    valid = result%segment_start_index >= 1 .and. &
        ieee_is_finite(result%distance_into_segment) .and. &
        result%distance_into_segment >= 0.0_real64 .and. &
        ieee_is_finite(result%total_length) .and. &
        result%total_length > 0.0_real64 .and. &
        ieee_is_finite(result%horizontal_coordinate) .and. &
        ieee_is_finite(result%vertical_coordinate)
  end function polyline_interpolation_result_is_valid

  !> Interpolate at a measured distance from `first_index` along a polyline.
  !!
  !! A request exactly on a shared vertex belongs to the following non-zero
  !! segment.  That detail deliberately preserves the old inclusive scan,
  !! which kept scanning and overwrote the preceding segment selection.
  !! Zero-length duplicate segments contribute no distance and are skipped,
  !! avoiding the legacy division by zero without changing a valid contour.
  !! The output is transactional: `result` is unchanged on any invalid input.
  pure subroutine interpolate_polyline_at_distance(horizontal_coordinates, &
      vertical_coordinates, first_index, last_index, requested_distance, &
      result, valid, message)
    real(real64), intent(in) :: horizontal_coordinates(:)
    real(real64), intent(in) :: vertical_coordinates(:)
    integer, intent(in) :: first_index, last_index
    real(real64), intent(in) :: requested_distance
    type(polyline_interpolation_result), intent(inout) :: result
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(polyline_interpolation_result) :: candidate
    real(real64) :: cumulative_length, next_cumulative_length
    real(real64) :: delta_horizontal, delta_vertical, fraction
    real(real64) :: segment_length
    integer :: point_index
    logical :: segment_found

    valid = .false.
    message = ''
    if (size(horizontal_coordinates) /= size(vertical_coordinates)) then
      message = 'polyline coordinate arrays have different sizes'
      return
    end if
    if (first_index < 1 .or. last_index > size(horizontal_coordinates) .or. &
        last_index <= first_index) then
      message = 'polyline interpolation index interval is invalid'
      return
    end if
    if (.not. all(ieee_is_finite( &
        horizontal_coordinates(first_index:last_index))) .or. &
        .not. all(ieee_is_finite( &
        vertical_coordinates(first_index:last_index)))) then
      message = 'polyline contains a non-finite coordinate'
      return
    end if
    if (.not. ieee_is_finite(requested_distance)) then
      message = 'requested polyline distance is not finite'
      return
    end if

    candidate%total_length = 0.0_real64
    do point_index = first_index, last_index - 1
      delta_horizontal = horizontal_coordinates(point_index + 1) - &
          horizontal_coordinates(point_index)
      delta_vertical = vertical_coordinates(point_index + 1) - &
          vertical_coordinates(point_index)
      segment_length = sqrt(delta_horizontal**2 + delta_vertical**2)
      if (.not. ieee_is_finite(segment_length) .or. &
          .not. ieee_is_finite(candidate%total_length + segment_length)) then
        message = 'polyline length exceeds the supported numeric range'
        return
      end if
      candidate%total_length = candidate%total_length + segment_length
    end do
    if (candidate%total_length <= 0.0_real64) then
      message = 'polyline has no non-zero segment'
      return
    end if
    if (requested_distance < 0.0_real64 .or. &
        requested_distance > candidate%total_length) then
      message = 'requested distance lies outside the polyline'
      return
    end if

    cumulative_length = 0.0_real64
    segment_found = .false.
    do point_index = first_index, last_index - 1
      delta_horizontal = horizontal_coordinates(point_index + 1) - &
          horizontal_coordinates(point_index)
      delta_vertical = vertical_coordinates(point_index + 1) - &
          vertical_coordinates(point_index)
      segment_length = sqrt(delta_horizontal**2 + delta_vertical**2)
      next_cumulative_length = cumulative_length + segment_length

      ! Keep scanning after a match.  At a shared endpoint the later segment
      ! therefore wins, exactly as in the original inclusive legacy loop.
      if (segment_length > 0.0_real64 .and. &
          requested_distance >= cumulative_length .and. &
          requested_distance <= next_cumulative_length) then
        candidate%segment_start_index = point_index
        candidate%distance_into_segment = &
            requested_distance - cumulative_length
        fraction = candidate%distance_into_segment / segment_length
        candidate%horizontal_coordinate = &
            horizontal_coordinates(point_index) + fraction * delta_horizontal
        candidate%vertical_coordinate = &
            vertical_coordinates(point_index) + fraction * delta_vertical
        segment_found = .true.
      end if
      cumulative_length = next_cumulative_length
    end do
    if (.not. segment_found .or. .not. candidate%is_valid()) then
      message = 'no valid source segment contains the requested distance'
      return
    end if

    result = candidate
    valid = .true.
  end subroutine interpolate_polyline_at_distance

  !> Preserve the historical fixed-array `interpseg` API for current callers.
  !!
  !! Only result element 1 is part of that API; the unused suffix is left
  !! untouched.  Invalid legacy requests previously consumed uninitialized
  !! segment state, so they now fail immediately with a useful diagnostic.
  subroutine interpseg(jini, jfin, jc, ldif, useg, vseg, ltotal, linter, &
      upoint, vpoint)
    integer, intent(in) :: jini, jfin
    integer, intent(out) :: jc
    real(real64), intent(out) :: ldif, ltotal
    real(real64), intent(in) :: useg(legacy_coordinate_capacity)
    real(real64), intent(in) :: vseg(legacy_coordinate_capacity)
    real(real64), intent(in) :: linter
    real(real64), intent(inout) :: upoint(legacy_result_capacity)
    real(real64), intent(inout) :: vpoint(legacy_result_capacity)

    type(polyline_interpolation_result) :: interpolation
    character(len=160) :: message
    logical :: valid

    call interpolate_polyline_at_distance(useg, vseg, jini, jfin, linter, &
        interpolation, valid, message)
    if (.not. valid) then
      write(error_unit, '(2A)') 'ERROR: interpseg: ', trim(message)
      error stop 1
    end if

    jc = interpolation%segment_start_index
    ldif = interpolation%distance_into_segment
    ltotal = interpolation%total_length
    upoint(1) = interpolation%horizontal_coordinate
    vpoint(1) = interpolation%vertical_coordinate
  end subroutine interpseg

end module leparagliding_polyline_interpolation
