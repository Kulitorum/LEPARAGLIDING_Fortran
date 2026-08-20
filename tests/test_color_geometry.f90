program test_color_geometry
  use, intrinsic :: iso_fortran_env, only : real64
  use leparagliding_color_geometry
  implicit none

  real(real64) :: profile_x(4), edge_u(4), edge_v(4)
  real(real64) :: boundary_u, boundary_v
  real(real64) :: plx, ply, prx, pry, mlx, mly, mrx, mry
  logical :: found, valid

  profile_x = [100.0_real64, 75.0_real64, 25.0_real64, 0.0_real64]
  edge_u = [0.0_real64, 1.0_real64, 3.0_real64, 4.0_real64]
  edge_v = [0.0_real64, 2.0_real64, 6.0_real64, 8.0_real64]

  call locate_color_boundary_on_edge(profile_x, edge_u, edge_v, 1, 4, &
      1, 50.0_real64, found, boundary_u, boundary_v)
  call require(found, 'boundary was not located')
  call require_close(boundary_u, 2.0_real64, 'boundary U')
  call require_close(boundary_v, 4.0_real64, 'boundary V')

  ! A repeated profile coordinate formerly caused a 0/0 slope path.
  profile_x = [100.0_real64, 50.0_real64, 50.0_real64, 0.0_real64]
  call locate_color_boundary_on_edge(profile_x, edge_u, edge_v, 1, 4, &
      1, 50.0_real64, found, boundary_u, boundary_v)
  call require(found, 'repeated-coordinate boundary was not located')
  call require_close(boundary_u, 1.0_real64, 'repeated boundary U')
  call require_close(boundary_v, 2.0_real64, 'repeated boundary V')

  call color_piece_match_points(0.0_real64, 0.0_real64, 10.0_real64, &
      0.0_real64, 1.0_real64, 0.11_real64, plx, ply, prx, pry, valid)
  call require(valid, 'paired matching-point inset was rejected')
  call require_close(plx, 5.0_real64, 'positive piece mark X')
  call require_close(prx, 5.0_real64, 'negative piece mark X')
  call require_close(ply, 0.89_real64, 'positive piece mark inset')
  call require_close(pry, -0.89_real64, 'negative piece mark inset')

  call offset_color_seam(0.0_real64, 0.0_real64, 10.0_real64, &
      0.0_real64, 1.0_real64, plx, ply, prx, pry, mlx, mly, mrx, mry, valid)
  call require(valid, 'parallel seam offset was rejected')
  call require_close(ply, 1.0_real64, 'positive allowance')
  call require_close(mly, -1.0_real64, 'negative allowance')

contains

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message
    if (.not. condition) then
      write (*, '(A)') 'FAIL: '//message
      error stop 1
    end if
  end subroutine require

  subroutine require_close(actual, expected, message)
    real(real64), intent(in) :: actual, expected
    character(len=*), intent(in) :: message
    call require(abs(actual - expected) <= 1.0e-10_real64, message)
  end subroutine require_close

end program test_color_geometry
