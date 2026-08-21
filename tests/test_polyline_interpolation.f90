program test_polyline_interpolation
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_quiet_nan, ieee_value
  use leparagliding_polyline_interpolation, only : interpseg, &
      interpolate_polyline_at_distance, polyline_interpolation_result
  implicit none

  integer, parameter :: legacy_coordinate_capacity = 1000
  integer, parameter :: legacy_result_capacity = 100
  real(real64), parameter :: tolerance = 2.0e-12_real64
  real(real64) :: horizontal(3), vertical(3), nan_value
  real(real64) :: legacy_horizontal(legacy_coordinate_capacity)
  real(real64) :: legacy_vertical(legacy_coordinate_capacity)
  real(real64) :: legacy_result_x(legacy_result_capacity)
  real(real64) :: legacy_result_y(legacy_result_capacity)
  real(real64) :: distance_into_segment, total_length
  type(polyline_interpolation_result) :: result, saved_result
  character(len=160) :: message
  integer :: segment_start_index
  logical :: valid

  horizontal = [0.0_real64, 3.0_real64, 3.0_real64]
  vertical = [0.0_real64, 4.0_real64, 8.0_real64]

  call interpolate_polyline_at_distance(horizontal, vertical, 1, 3, &
      2.5_real64, result, valid, message)
  call require(valid, 'interior interpolation failed: '//trim(message))
  call require(result%segment_start_index == 1, &
      'interior interpolation selected the wrong segment')
  call require_close(result%distance_into_segment, 2.5_real64, &
      'interior distance into segment')
  call require_close(result%total_length, 9.0_real64, &
      'polyline total length')
  call require_close(result%horizontal_coordinate, 1.5_real64, &
      'interior horizontal coordinate')
  call require_close(result%vertical_coordinate, 2.0_real64, &
      'interior vertical coordinate')

  call interpolate_polyline_at_distance(horizontal, vertical, 1, 3, &
      5.0_real64, result, valid, message)
  call require(valid, 'shared-vertex interpolation failed: '//trim(message))
  call require(result%segment_start_index == 2, &
      'shared vertex did not retain legacy following-segment ownership')
  call require_close(result%distance_into_segment, 0.0_real64, &
      'shared-vertex distance into following segment')
  call require_close(result%horizontal_coordinate, 3.0_real64, &
      'shared-vertex horizontal coordinate')
  call require_close(result%vertical_coordinate, 4.0_real64, &
      'shared-vertex vertical coordinate')

  call interpolate_polyline_at_distance(horizontal, vertical, 1, 3, &
      7.0_real64, result, valid, message)
  call require(valid, 'vertical-segment interpolation failed: '//trim(message))
  call require(result%segment_start_index == 2, &
      'vertical interpolation selected the wrong segment')
  call require_close(result%horizontal_coordinate, 3.0_real64, &
      'vertical-segment horizontal coordinate')
  call require_close(result%vertical_coordinate, 6.0_real64, &
      'vertical-segment vertical coordinate')

  horizontal = [0.0_real64, 0.0_real64, 3.0_real64]
  vertical = [0.0_real64, 0.0_real64, 4.0_real64]
  call interpolate_polyline_at_distance(horizontal, vertical, 1, 3, &
      0.0_real64, result, valid, message)
  call require(valid, 'duplicate-point interpolation failed: '//trim(message))
  call require(result%segment_start_index == 2, &
      'zero-length segment was selected')
  call require_close(result%total_length, 5.0_real64, &
      'duplicate-point total length')
  call require_close(result%horizontal_coordinate, 0.0_real64, &
      'duplicate-point horizontal coordinate')
  call require_close(result%vertical_coordinate, 0.0_real64, &
      'duplicate-point vertical coordinate')

  result%segment_start_index = 77
  result%distance_into_segment = 11.0_real64
  result%total_length = 22.0_real64
  result%horizontal_coordinate = 33.0_real64
  result%vertical_coordinate = 44.0_real64
  saved_result = result
  call interpolate_polyline_at_distance( &
      [0.0_real64, 1.0_real64], [0.0_real64], 1, 2, 0.5_real64, &
      result, valid, message)
  call require(.not. valid, 'mismatched coordinate arrays were accepted')
  call require(len_trim(message) > 0, &
      'mismatched arrays did not return a diagnostic')
  call require_result_unchanged(result, saved_result, &
      'mismatched-array failure changed the result')

  call interpolate_polyline_at_distance( &
      [0.0_real64, 1.0_real64], [0.0_real64, 0.0_real64], &
      1, 2, 2.0_real64, result, valid, message)
  call require(.not. valid, 'out-of-range distance was accepted')
  call require_result_unchanged(result, saved_result, &
      'out-of-range failure changed the result')

  call interpolate_polyline_at_distance( &
      [0.0_real64, 0.0_real64], [1.0_real64, 1.0_real64], &
      1, 2, 0.0_real64, result, valid, message)
  call require(.not. valid, 'zero-length polyline was accepted')
  call require_result_unchanged(result, saved_result, &
      'zero-length failure changed the result')

  nan_value = ieee_value(0.0_real64, ieee_quiet_nan)
  call interpolate_polyline_at_distance( &
      [0.0_real64, nan_value], [0.0_real64, 1.0_real64], &
      1, 2, 0.5_real64, result, valid, message)
  call require(.not. valid, 'non-finite coordinate was accepted')
  call require_result_unchanged(result, saved_result, &
      'non-finite failure changed the result')

  legacy_horizontal = -999.0_real64
  legacy_vertical = -999.0_real64
  legacy_result_x = -777.0_real64
  legacy_result_y = 888.0_real64
  legacy_horizontal(7:9) = [0.0_real64, 3.0_real64, 3.0_real64]
  legacy_vertical(7:9) = [0.0_real64, 4.0_real64, 8.0_real64]
  call interpseg(7, 9, segment_start_index, distance_into_segment, &
      legacy_horizontal, legacy_vertical, total_length, 5.0_real64, &
      legacy_result_x, legacy_result_y)
  call require(segment_start_index == 8, &
      'compatibility wrapper changed shared-vertex segment ownership')
  call require_close(distance_into_segment, 0.0_real64, &
      'compatibility wrapper distance into segment')
  call require_close(total_length, 9.0_real64, &
      'compatibility wrapper total length')
  call require_close(legacy_result_x(1), 3.0_real64, &
      'compatibility wrapper horizontal coordinate')
  call require_close(legacy_result_y(1), 4.0_real64, &
      'compatibility wrapper vertical coordinate')
  call require_close(legacy_result_x(2), -777.0_real64, &
      'compatibility wrapper changed unused horizontal output')
  call require_close(legacy_result_y(2), 888.0_real64, &
      'compatibility wrapper changed unused vertical output')

  write (*, '(A)') 'PASS: typed polyline interpolation'

contains

  subroutine require_result_unchanged(actual, expected, diagnostic)
    type(polyline_interpolation_result), intent(in) :: actual, expected
    character(len=*), intent(in) :: diagnostic

    call require(actual%segment_start_index == expected%segment_start_index &
        .and. abs(actual%distance_into_segment - &
        expected%distance_into_segment) <= 0.0_real64 .and. &
        abs(actual%total_length - expected%total_length) <= 0.0_real64 .and. &
        abs(actual%horizontal_coordinate - &
        expected%horizontal_coordinate) <= 0.0_real64 .and. &
        abs(actual%vertical_coordinate - &
        expected%vertical_coordinate) <= 0.0_real64, diagnostic)
  end subroutine require_result_unchanged

  subroutine require_close(actual, expected, diagnostic)
    real(real64), intent(in) :: actual, expected
    character(len=*), intent(in) :: diagnostic

    call require(abs(actual - expected) <= &
        tolerance * (1.0_real64 + abs(expected)), diagnostic)
  end subroutine require_close

  subroutine require(condition, diagnostic)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: diagnostic

    if (.not. condition) then
      write (*, '(2A)') 'FAIL: ', trim(diagnostic)
      error stop 1
    end if
  end subroutine require

end program test_polyline_interpolation
