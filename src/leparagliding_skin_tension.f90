! SPDX-License-Identifier: GPL-3.0-or-later
!
!> Model and evaluate the authored skin-tension law.
!!
!! Section 31 stores positions from the trailing edge towards the leading
!! edge, while panel development accumulates distance in the opposite
!! direction.  The legacy implementation reverses and rescales those values
!! into shared scratch arrays.  This module performs that conversion once,
!! gives every quantity a unit-bearing name, and validates the resulting law
!! before it is used by production geometry.
module leparagliding_skin_tension
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
  use leparagliding_domain_model, only : surface_extrados, surface_intrados
  implicit none
  private

  real(real64), parameter :: position_tolerance = 1.0e-12_real64

  !> Piecewise-linear overwidth law for one rib boundary and one surface.
  !!
  !! `developed_position_percent` is ordered from the first developed
  !! contour point to the last and lies in [0, 100]. `overwidth_percent` is
  !! multiplied by the width of the adjacent panel to obtain the offset.
  type, public :: skin_tension_law
    integer :: boundary_rib_index = -1
    integer :: surface = 0
    integer :: interpolation_type = 0
    real(real64) :: upper_bound_factor = 1.0_real64
    real(real64), allocatable :: developed_position_percent(:)
    real(real64), allocatable :: overwidth_percent(:)
  contains
    procedure :: is_valid => skin_tension_law_is_valid
  end type skin_tension_law

  public :: copy_legacy_new_skin_tension_law
  public :: evaluate_skin_tension_offset

contains

  !> Test the invariants required by the piecewise-linear evaluator.
  pure logical function skin_tension_law_is_valid(law) result(valid)
    class(skin_tension_law), intent(in) :: law
    integer :: point_index

    valid = .false.
    if (law%boundary_rib_index < 0) return
    if (law%surface /= surface_extrados .and. &
        law%surface /= surface_intrados) return
    if (law%interpolation_type /= 1) return
    if (.not. ieee_is_finite(law%upper_bound_factor)) return
    if (law%upper_bound_factor < 1.0_real64) return
    if (.not. allocated(law%developed_position_percent)) return
    if (.not. allocated(law%overwidth_percent)) return
    if (size(law%developed_position_percent) < 2) return
    if (size(law%overwidth_percent) /= &
        size(law%developed_position_percent)) return
    if (.not. all(ieee_is_finite(law%developed_position_percent))) return
    if (.not. all(ieee_is_finite(law%overwidth_percent))) return
    if (any(law%developed_position_percent < -position_tolerance)) return
    if (any(law%developed_position_percent > &
        100.0_real64 + position_tolerance)) return
    if (any(law%overwidth_percent < 0.0_real64)) return
    do point_index = 2, size(law%developed_position_percent)
      if (law%developed_position_percent(point_index) <= &
          law%developed_position_percent(point_index - 1)) return
    end do
    valid = .true.
  end function skin_tension_law_is_valid

  !> Convert one Section 31 rib law from legacy percentages.
  !!
  !! The adapter is transactional: `law` is changed only after all source
  !! values pass validation.  Columns 1/2 describe extrados position and
  !! overwidth; columns 3/4 describe intrados position and overwidth.
  pure subroutine copy_legacy_new_skin_tension_law(legacy_values, &
      point_count, boundary_rib_index, surface, interpolation_type, law, &
      valid, message)
    real(real64), intent(in) :: legacy_values(:, :)
    integer, intent(in) :: point_count
    integer, intent(in) :: boundary_rib_index
    integer, intent(in) :: surface
    integer, intent(in) :: interpolation_type
    type(skin_tension_law), intent(inout) :: law
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message
    type(skin_tension_law) :: candidate
    integer :: position_column
    integer :: overwidth_column
    integer :: source_index
    integer :: target_index

    valid = .false.
    message = ''
    if (point_count < 2) then
      message = 'skin-tension law needs at least two points'
      return
    end if
    if (point_count > size(legacy_values, 1)) then
      message = 'skin-tension point count exceeds source rows'
      return
    end if
    if (size(legacy_values, 2) < 4) then
      message = 'skin-tension source needs four columns'
      return
    end if
    if (boundary_rib_index < 0) then
      message = 'skin-tension boundary rib index is negative'
      return
    end if
    select case (surface)
    case (surface_extrados)
      position_column = 1
      overwidth_column = 2
    case (surface_intrados)
      position_column = 3
      overwidth_column = 4
    case default
      message = 'skin-tension surface is unsupported'
      return
    end select
    if (interpolation_type /= 1) then
      message = 'only linear skin-tension interpolation is supported'
      return
    end if
    if (.not. all(ieee_is_finite( &
        legacy_values(1:point_count, position_column)))) then
      message = 'skin-tension positions must be finite'
      return
    end if
    if (.not. all(ieee_is_finite( &
        legacy_values(1:point_count, overwidth_column)))) then
      message = 'skin-tension overwidths must be finite'
      return
    end if
    if (any(legacy_values(1:point_count, position_column) < &
        -100.0_real64 * position_tolerance) .or. &
        any(legacy_values(1:point_count, position_column) > &
        100.0_real64 * (1.0_real64 + position_tolerance))) then
      message = 'skin-tension positions must lie between 0 and 100 percent'
      return
    end if
    if (any(legacy_values(1:point_count, overwidth_column) < &
        0.0_real64)) then
      message = 'skin-tension overwidths cannot be negative'
      return
    end if
    do source_index = 2, point_count
      if (legacy_values(source_index, position_column) <= &
          legacy_values(source_index - 1, position_column)) then
        message = 'skin-tension positions must increase strictly'
        return
      end if
    end do

    candidate%boundary_rib_index = boundary_rib_index
    candidate%surface = surface
    candidate%interpolation_type = interpolation_type
    ! Preserve Section 31's historical inclusive upper-bound tolerance.
    candidate%upper_bound_factor = real(1.001, real64)
    allocate(candidate%developed_position_percent(point_count))
    allocate(candidate%overwidth_percent(point_count))
    do target_index = 1, point_count
      source_index = point_count + 1 - target_index
      candidate%developed_position_percent(target_index) = &
          100.0_real64 - legacy_values(source_index, position_column)
      candidate%overwidth_percent(target_index) = &
          legacy_values(source_index, overwidth_column)
    end do
    if (abs(candidate%developed_position_percent(1)) > &
        position_tolerance .or. &
        abs(candidate%developed_position_percent(point_count) - &
        100.0_real64) > position_tolerance) then
      message = 'skin-tension positions must cover 0 through 100 percent'
      return
    end if
    if (.not. candidate%is_valid()) then
      message = 'converted skin-tension law violates its invariants'
      return
    end if

    law = candidate
    valid = .true.
  end subroutine copy_legacy_new_skin_tension_law

  !> Evaluate the overwidth offset at one developed contour distance.
  !!
  !! The interval test and last-match-wins behavior intentionally reproduce
  !! Section 31.  This keeps results stable where the 1.001 upper-bound factor
  !! makes neighboring intervals overlap slightly.
  pure subroutine evaluate_skin_tension_offset(law, contour_length, &
      panel_width, developed_distance, offset, valid, message)
    type(skin_tension_law), intent(in) :: law
    real(real64), intent(in) :: contour_length
    real(real64), intent(in) :: panel_width
    real(real64), intent(in) :: developed_distance
    real(real64), intent(out) :: offset
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message
    integer :: interval_index
    real(real64) :: interval_first
    real(real64) :: interval_last
    real(real64) :: offset_first
    real(real64) :: offset_last
    real(real64) :: slope
    real(real64) :: intercept
    logical :: interval_found

    offset = 0.0_real64
    valid = .false.
    message = ''
    if (.not. law%is_valid()) then
      message = 'skin-tension law is invalid'
      return
    end if
    if (.not. ieee_is_finite(contour_length) .or. &
        contour_length <= 0.0_real64) then
      message = 'skin-tension contour length must be finite and positive'
      return
    end if
    if (.not. ieee_is_finite(panel_width) .or. &
        panel_width < 0.0_real64) then
      message = 'skin-tension panel width must be finite and nonnegative'
      return
    end if
    if (.not. ieee_is_finite(developed_distance)) then
      message = 'skin-tension developed distance must be finite'
      return
    end if

    interval_found = .false.
    do interval_index = 1, size(law%developed_position_percent) - 1
      interval_first = (law%developed_position_percent(interval_index) / &
          100.0_real64) * contour_length
      interval_last = (law%developed_position_percent(interval_index + 1) / &
          100.0_real64) * contour_length
      if (developed_distance >= interval_first .and. &
          developed_distance <= interval_last * law%upper_bound_factor) then
        offset_first = panel_width * &
            law%overwidth_percent(interval_index) / 100.0_real64
        offset_last = panel_width * &
            law%overwidth_percent(interval_index + 1) / 100.0_real64
        slope = (offset_last - offset_first) / &
            (interval_last - interval_first)
        intercept = offset_last - slope * interval_last
        offset = slope * developed_distance + intercept
        interval_found = .true.
      end if
    end do
    if (.not. interval_found) then
      message = 'developed distance lies outside the skin-tension law'
      return
    end if
    if (.not. ieee_is_finite(offset)) then
      message = 'skin-tension evaluation produced a nonfinite offset'
      offset = 0.0_real64
      return
    end if
    valid = .true.
  end subroutine evaluate_skin_tension_offset

end module leparagliding_skin_tension
