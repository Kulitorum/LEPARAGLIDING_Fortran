! SPDX-License-Identifier: GPL-3.0-or-later
!
! Geometry shared by the legacy section-15/16 color workflow.  Keeping these
! calculations in a small typed module makes the coordinate conventions
! explicit and prevents the upper/lower surface implementations from drifting.
module leparagliding_color_geometry
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
  implicit none
  private

  public :: locate_color_boundary_on_edge
  public :: offset_color_seam
  public :: color_piece_match_points

contains

  !> Locate a chordwise color boundary on one developed panel edge.
  !!
  !! `profile_x_percent` contains LEP's normalized airfoil X coordinates.  The
  !! section-15/16 convention measures the requested boundary in the opposite
  !! direction, so a profile point has color coordinate `100 - X`.  The same
  !! interpolation fraction is applied directly to the developed edge.  This
  !! avoids the slope divisions used by the historical code, including the
  !! vertical-segment 0/0 that produced NaNs in the Swoop2 project.
  !!
  !! @param[in] profile_x_percent Normalized airfoil X values, normally
  !!            `u(rib,1:n,2)`.
  !! @param[in] edge_u Developed-panel U coordinates for the selected edge.
  !! @param[in] edge_v Developed-panel V coordinates for the selected edge.
  !! @param[in] first_point First profile point to examine.
  !! @param[in] last_point Last profile point to examine, inclusive.
  !! @param[in] point_step Traversal direction, normally 1 or -1.
  !! @param[in] boundary_percent Section-15/16 chord percentage.
  !! @param[out] found True when a finite containing segment was found.
  !! @param[out] boundary_u Developed U coordinate of the intersection.
  !! @param[out] boundary_v Developed V coordinate of the intersection.
  subroutine locate_color_boundary_on_edge(profile_x_percent, edge_u, &
      edge_v, first_point, last_point, point_step, boundary_percent, &
      found, boundary_u, boundary_v)
    real(real64), intent(in) :: profile_x_percent(:)
    real(real64), intent(in) :: edge_u(:)
    real(real64), intent(in) :: edge_v(:)
    integer, intent(in) :: first_point, last_point, point_step
    real(real64), intent(in) :: boundary_percent
    logical, intent(out) :: found
    real(real64), intent(out) :: boundary_u, boundary_v

    real(real64), parameter :: tolerance = 1.0e-10_real64
    real(real64) :: coordinate_a, coordinate_b, denominator
    real(real64) :: interpolation_fraction
    integer :: point_index, next_point, available_points

    found = .false.
    boundary_u = 0.0_real64
    boundary_v = 0.0_real64

    available_points = min(size(profile_x_percent), size(edge_u), &
        size(edge_v))
    if (available_points < 2 .or. point_step == 0) return
    if (first_point < 1 .or. first_point > available_points) return
    if (last_point < 1 .or. last_point > available_points) return
    if (.not. ieee_is_finite(boundary_percent)) return

    point_index = first_point
    do while (point_index /= last_point)
      next_point = point_index + point_step
      if (next_point < 1 .or. next_point > available_points) return

      coordinate_a = 100.0_real64 - profile_x_percent(point_index)
      coordinate_b = 100.0_real64 - profile_x_percent(next_point)
      if (.not. ieee_is_finite(coordinate_a) .or. &
          .not. ieee_is_finite(coordinate_b)) then
        point_index = next_point
        cycle
      end if

      if (boundary_percent >= min(coordinate_a, coordinate_b) - tolerance &
          .and. boundary_percent <= max(coordinate_a, coordinate_b) + &
          tolerance) then
        denominator = coordinate_b - coordinate_a
        if (abs(denominator) <= tolerance) then
          if (abs(boundary_percent - coordinate_a) > tolerance) then
            point_index = next_point
            cycle
          end if
          interpolation_fraction = 0.0_real64
        else
          interpolation_fraction = (boundary_percent - coordinate_a) / &
              denominator
        end if

        interpolation_fraction = max(0.0_real64, &
            min(1.0_real64, interpolation_fraction))
        boundary_u = edge_u(point_index) + interpolation_fraction * &
            (edge_u(next_point) - edge_u(point_index))
        boundary_v = edge_v(point_index) + interpolation_fraction * &
            (edge_v(next_point) - edge_v(point_index))
        found = ieee_is_finite(boundary_u) .and. &
            ieee_is_finite(boundary_v)
        if (.not. found) then
          boundary_u = 0.0_real64
          boundary_v = 0.0_real64
        end if
        return
      end if

      point_index = next_point
    end do
  end subroutine locate_color_boundary_on_edge

  !> Construct the two parallel cut edges around an internal sewing line.
  !! @param[in] left_x,left_y First endpoint of the sewing line.
  !! @param[in] right_x,right_y Second endpoint of the sewing line.
  !! @param[in] allowance Distance from the sewing line in model units.
  !! @param[out] plus_left_x,... Endpoints on the positive normal side.
  !! @param[out] minus_left_x,... Endpoints on the negative normal side.
  !! @param[out] valid False for a zero-length or non-finite seam.
  subroutine offset_color_seam(left_x, left_y, right_x, right_y, &
      allowance, plus_left_x, plus_left_y, plus_right_x, plus_right_y, &
      minus_left_x, minus_left_y, minus_right_x, minus_right_y, valid)
    real(real64), intent(in) :: left_x, left_y, right_x, right_y
    real(real64), intent(in) :: allowance
    real(real64), intent(out) :: plus_left_x, plus_left_y
    real(real64), intent(out) :: plus_right_x, plus_right_y
    real(real64), intent(out) :: minus_left_x, minus_left_y
    real(real64), intent(out) :: minus_right_x, minus_right_y
    logical, intent(out) :: valid

    real(real64), parameter :: tolerance = 1.0e-12_real64
    real(real64) :: delta_x, delta_y, seam_length, normal_x, normal_y

    plus_left_x = 0.0_real64
    plus_left_y = 0.0_real64
    plus_right_x = 0.0_real64
    plus_right_y = 0.0_real64
    minus_left_x = 0.0_real64
    minus_left_y = 0.0_real64
    minus_right_x = 0.0_real64
    minus_right_y = 0.0_real64
    valid = .false.

    if (.not. all(ieee_is_finite([left_x, left_y, right_x, right_y, &
        allowance]))) return
    delta_x = right_x - left_x
    delta_y = right_y - left_y
    seam_length = hypot(delta_x, delta_y)
    if (seam_length <= tolerance) return

    normal_x = -delta_y / seam_length
    normal_y = delta_x / seam_length
    plus_left_x = left_x + allowance * normal_x
    plus_left_y = left_y + allowance * normal_y
    plus_right_x = right_x + allowance * normal_x
    plus_right_y = right_y + allowance * normal_y
    minus_left_x = left_x - allowance * normal_x
    minus_left_y = left_y - allowance * normal_y
    minus_right_x = right_x - allowance * normal_x
    minus_right_y = right_y - allowance * normal_y
    valid = .true.
  end subroutine offset_color_seam

  !> Place corresponding marks just inside both adjacent piece cut edges.
  !!
  !! The two points share the seam midpoint/longitudinal station.  Each begins
  !! on its piece's allowance cut edge and moves `inset` toward the sewing line,
  !! keeping the mark inside the allowance and therefore hidden after assembly.
  subroutine color_piece_match_points(left_x, left_y, right_x, right_y, &
      allowance, inset, plus_mark_x, plus_mark_y, minus_mark_x, &
      minus_mark_y, valid)
    real(real64), intent(in) :: left_x, left_y, right_x, right_y
    real(real64), intent(in) :: allowance, inset
    real(real64), intent(out) :: plus_mark_x, plus_mark_y
    real(real64), intent(out) :: minus_mark_x, minus_mark_y
    logical, intent(out) :: valid

    real(real64), parameter :: tolerance = 1.0e-12_real64
    real(real64) :: delta_x, delta_y, seam_length, normal_x, normal_y
    real(real64) :: midpoint_x, midpoint_y, mark_offset

    plus_mark_x = 0.0_real64
    plus_mark_y = 0.0_real64
    minus_mark_x = 0.0_real64
    minus_mark_y = 0.0_real64
    valid = .false.
    if (.not. all(ieee_is_finite([left_x, left_y, right_x, right_y, &
        allowance, inset]))) return

    delta_x = right_x - left_x
    delta_y = right_y - left_y
    seam_length = hypot(delta_x, delta_y)
    if (seam_length <= tolerance) return
    normal_x = -delta_y / seam_length
    normal_y = delta_x / seam_length
    midpoint_x = 0.5_real64 * (left_x + right_x)
    midpoint_y = 0.5_real64 * (left_y + right_y)
    mark_offset = max(0.0_real64, abs(allowance) - max(0.0_real64, inset))

    plus_mark_x = midpoint_x + mark_offset * normal_x
    plus_mark_y = midpoint_y + mark_offset * normal_y
    minus_mark_x = midpoint_x - mark_offset * normal_x
    minus_mark_y = midpoint_y - mark_offset * normal_y
    valid = .true.
  end subroutine color_piece_match_points

end module leparagliding_color_geometry
