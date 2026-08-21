program test_geometry_2d
  use, intrinsic :: iso_fortran_env, only : real32, real64
  use leparagliding_geometry_2d, only : vredis, xrxs, flatt, axisch, &
      angdis2, vrib_hole_ellipse
  implicit none

  real(real64), parameter :: tolerance = 2.0e-12_real64
  real(real64) :: x_source(5000), y_source(5000)
  real(real64) :: x_result(5000), y_result(5000)
  real(real64) :: line_r_x(2), line_r_y(2), line_s_x(2), line_s_y(2)
  real(real32) :: intersection_x, intersection_y
  real(real64), allocatable :: rx(:, :), ry(:, :), rz(:, :)
  real(real64), allocatable :: pl1x(:, :), pl1y(:, :), pl2x(:, :), pl2y(:, :)
  real(real64), allocatable :: pr1x(:, :), pr1y(:, :), pr2x(:, :), pr2y(:, :)
  real(real64) :: axis_x_in(500), axis_y_in(500)
  real(real64) :: axis_x_out(500), axis_y_out(500), angle
  real(real64) :: p3u, p3v, legacy_angle
  real(real64) :: center_x, center_y, semiaxis_a, semiaxis_b

  call test_redistribution()
  call test_intersection()
  call test_flattening()
  call test_axis_copy()
  call test_unfinished_helper()
  call test_hole_ellipse()

  write (*, '(A)') '2D geometry tests passed'

contains

  subroutine test_redistribution()
    integer :: point_index

    x_source = 0.0_real64
    y_source = 0.0_real64
    x_result = -777.0_real64
    y_result = 888.0_real64
    x_source(1:2) = [1.0_real64, 5.0_real64]
    y_source(1:2) = [-2.0_real64, 6.0_real64]

    call vredis(x_source, y_source, x_result, y_result, 2, 5)
    do point_index = 1, 5
      call require_close(x_result(point_index), real(point_index, real64), &
          'two-point redistribution X')
      call require_close(y_result(point_index), &
          -2.0_real64 + 2.0_real64 * real(point_index - 1, real64), &
          'two-point redistribution Y')
    end do
  end subroutine test_redistribution

  subroutine test_intersection()
    line_r_x = [0.0_real64, 2.0_real64]
    line_r_y = [0.0_real64, 2.0_real64]
    line_s_x = [0.0_real64, 2.0_real64]
    line_s_y = [2.0_real64, 0.0_real64]
    call xrxs(line_r_x, line_r_y, line_s_x, line_s_y, &
        intersection_x, intersection_y)
    call require_close(real(intersection_x, real64), 1.0_real64, &
        'line intersection X')
    call require_close(real(intersection_y, real64), 1.0_real64, &
        'line intersection Y')

    line_s_x = [1.5_real64, 1.5_real64]
    line_s_y = [-1.0_real64, 3.0_real64]
    call xrxs(line_r_x, line_r_y, line_s_x, line_s_y, &
        intersection_x, intersection_y)
    call require_close(real(intersection_x, real64), 1.5_real64, &
        'vertical-line intersection X')
    call require_close(real(intersection_y, real64), 1.5_real64, &
        'vertical-line intersection Y')
  end subroutine test_intersection

  subroutine test_flattening()
    allocate(rx(0:100, 500), ry(0:100, 500), rz(0:100, 500))
    allocate(pl1x(0:100, 500), pl1y(0:100, 500))
    allocate(pl2x(0:100, 500), pl2y(0:100, 500))
    allocate(pr1x(0:100, 500), pr1y(0:100, 500))
    allocate(pr2x(0:100, 500), pr2y(0:100, 500))

    rx = 0.0_real64
    ry = 0.0_real64
    rz = 0.0_real64
    pl1x = -777.0_real64
    pl1y = -777.0_real64
    pl2x = -777.0_real64
    pl2y = -777.0_real64
    pr1x = -777.0_real64
    pr1y = -777.0_real64
    pr2x = -777.0_real64
    pr2y = -777.0_real64

    rx(0, 1:2) = [0.0_real64, 0.0_real64]
    ry(0, 1:2) = [0.0_real64, 1.0_real64]
    rx(1, 1:2) = [2.0_real64, 2.0_real64]
    ry(1, 1:2) = [0.0_real64, 1.0_real64]

    call flatt(0, 1, rx, ry, rz, pl1x, pl1y, pl2x, pl2y, &
        pr1x, pr1y, pr2x, pr2y)
    call require_point(pl1x(0, 1), pl1y(0, 1), 0.0_real64, 0.0_real64, &
        'flattened first-left')
    call require_point(pr1x(0, 1), pr1y(0, 1), 2.0_real64, 0.0_real64, &
        'flattened first-right')
    call require_point(pl2x(0, 1), pl2y(0, 1), 0.0_real64, 1.0_real64, &
        'flattened second-left')
    call require_point(pr2x(0, 1), pr2y(0, 1), 2.0_real64, 1.0_real64, &
        'flattened second-right')
    call require_close(pl1x(1, 1), -777.0_real64, &
        'flattening modified an unrelated rib')

    deallocate(rx, ry, rz, pl1x, pl1y, pl2x, pl2y)
    deallocate(pr1x, pr1y, pr2x, pr2y)
  end subroutine test_flattening

  subroutine test_axis_copy()
    axis_x_in = 0.0_real64
    axis_y_in = 0.0_real64
    axis_x_out = -777.0_real64
    axis_y_out = 888.0_real64
    axis_x_in(1:3) = [1.0_real64, -2.0_real64, 4.0_real64]
    axis_y_in(1:3) = [-3.0_real64, 5.0_real64, 7.0_real64]
    angle = 45.0_real64

    call axisch(3, angle, axis_x_in, axis_y_in, axis_x_out, axis_y_out)
    call require_close(angle, 0.0_real64, 'axis-copy angle reset')
    call require(all(abs(axis_x_out(1:3) - axis_x_in(1:3)) <= &
        tolerance), 'axis-copy X coordinates')
    call require(all(abs(axis_y_out(1:3) - axis_y_in(1:3)) <= &
        tolerance), 'axis-copy Y coordinates')
    call require_close(axis_x_out(4), -777.0_real64, &
        'axis-copy modified unused X suffix')
    call require_close(axis_y_out(4), 888.0_real64, &
        'axis-copy modified unused Y suffix')
  end subroutine test_axis_copy

  subroutine test_unfinished_helper()
    p3u = 12.0_real64
    p3v = -8.0_real64
    legacy_angle = 0.75_real64
    call angdis2(0.0_real64, 0.0_real64, 2.0_real64, 3.0_real64, &
        p3u, p3v, legacy_angle, 5.0_real64)
    call require_close(p3u, 12.0_real64, 'unfinished helper P3 X')
    call require_close(p3v, -8.0_real64, 'unfinished helper P3 Y')
    call require_close(legacy_angle, 0.75_real64, &
        'unfinished helper angle')
  end subroutine test_unfinished_helper

  subroutine test_hole_ellipse()
    call vrib_hole_ellipse(0.0_real64, 0.0_real64, &
        2.0_real64, 0.0_real64, 0.0_real64, 2.0_real64, &
        4.0_real64, 2.0_real64, 0.5_real64, 40.0_real64, &
        20.0_real64, 0.25_real64, center_x, center_y, semiaxis_a, &
        semiaxis_b, angle)
    call require_point(center_x, center_y, 0.75_real64, 1.0_real64, &
        'H/V-rib hole center')
    call require_close(semiaxis_a, 0.6_real64, 'H/V-rib semiaxis A')
    call require_close(semiaxis_b, 0.3_real64, 'H/V-rib semiaxis B')
    call require_close(angle, 0.0_real64, 'H/V-rib hole angle')
  end subroutine test_hole_ellipse

  subroutine require_point(actual_x, actual_y, expected_x, expected_y, diagnostic)
    real(real64), intent(in) :: actual_x, actual_y, expected_x, expected_y
    character(len=*), intent(in) :: diagnostic

    call require_close(actual_x, expected_x, trim(diagnostic) // ' X')
    call require_close(actual_y, expected_y, trim(diagnostic) // ' Y')
  end subroutine require_point

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

end program test_geometry_2d
