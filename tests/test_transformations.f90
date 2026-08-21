program test_transformations
  use, intrinsic :: iso_fortran_env, only : real64
  use leparagliding_transformations, only : loc2glo2d
  implicit none

  integer, parameter :: capacity = 3000
  real(real64), parameter :: tolerance = 2.0e-12_real64
  real(real64) :: local_x(0:capacity), local_y(0:capacity)
  real(real64) :: global_x(0:capacity), global_y(0:capacity)

  local_x = 0.0_real64
  local_y = 0.0_real64
  global_x = -777.0_real64
  global_y = 888.0_real64
  local_x(1:3) = [1.0_real64, 0.0_real64, -2.0_real64]
  local_y(1:3) = [0.0_real64, 2.0_real64, -1.0_real64]

  call loc2glo2d(3, local_x, local_y, global_x, global_y, &
      10.0_real64, -4.0_real64, 90.0_real64)
  call require_close(global_x(1), 10.0_real64, 'point 1 X')
  call require_close(global_y(1), -3.0_real64, 'point 1 Y')
  call require_close(global_x(2), 8.0_real64, 'point 2 X')
  call require_close(global_y(2), -4.0_real64, 'point 2 Y')
  call require_close(global_x(3), 11.0_real64, 'point 3 X')
  call require_close(global_y(3), -6.0_real64, 'point 3 Y')
  call require(abs(global_x(0) + 777.0_real64) <= 0.0_real64 .and. &
      abs(global_y(0) - 888.0_real64) <= 0.0_real64, &
      'index zero was modified')
  call require(abs(global_x(4) + 777.0_real64) <= 0.0_real64 .and. &
      abs(global_y(4) - 888.0_real64) <= 0.0_real64, &
      'unused suffix was modified')

  call loc2glo2d(0, local_x, local_y, global_x, global_y, &
      1.0_real64, 2.0_real64, 45.0_real64)
  call require(abs(global_x(0) + 777.0_real64) <= 0.0_real64 .and. &
      abs(global_y(0) - 888.0_real64) <= 0.0_real64, &
      'zero-point call modified output')

  write (*, '(A)') 'transformation tests passed'

contains

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

end program test_transformations
