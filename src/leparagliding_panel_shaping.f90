! SPDX-License-Identifier: GPL-3.0-or-later
!
!> Typed panel-edge shaping and sewing-allowance geometry.
!!
!! This module isolates the numerical contract of the legacy `puntslat`
!! routine from its shared `latv` storage.  The contract contains two
!! historical details which are intentionally retained:
!!
!! * a horizontal first segment uses a vertical normal at its initial point
!!   but a horizontal normal at its endpoint; and
!! * every later contour point is based on the endpoint of its incoming
!!   segment, not on the following segment's start-biased point view.
!!
!! Both behaviours are manufacturing-file compatibility requirements.  A
!! future correction must therefore be introduced as a named policy rather
!! than silently changing this kernel.
module leparagliding_panel_shaping
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
  use leparagliding_domain_model, only : neutral_panel_2d, &
      surface_extrados, surface_intake, surface_intrados
  implicit none
  private

  !> Select the edge belonging to the panel's lower-index rib.
  integer, parameter, public :: panel_side_lower = 1
  !> Select the edge belonging to the panel's higher-index rib.
  integer, parameter, public :: panel_side_higher = 2

  ! Preserve the default-REAL representation in legacy `xupp*0.1` before its
  ! promotion to REAL(8).  A REAL(8) literal would change low-order bits.
  real(real64), parameter :: legacy_allowance_mm_to_model = &
      real(0.1, real64)
  real(real64), parameter :: half_pi = &
      2.0_real64*atan(1.0_real64)

  !> Sewing and cut contours for one side of one developed panel.
  !!
  !! Coordinates use the same model units as `neutral_panel_2d`.  Point one
  !! corresponds to `contour_first_index`; the final point corresponds to
  !! `contour_last_index`.  `side` names a lower- or higher-index rib and must
  !! not be interpreted as a geometric left/right direction.
  type, public :: shaped_panel_side_2d
    integer :: panel_index = -1
    integer :: surface = 0
    integer :: side = 0
    integer :: contour_first_index = 0
    integer :: contour_last_index = -1
    real(real64), allocatable :: sewing_u(:)
    real(real64), allocatable :: sewing_v(:)
    real(real64), allocatable :: cut_u(:)
    real(real64), allocatable :: cut_v(:)
  contains
    procedure :: is_valid => shaped_panel_side_is_valid
  end type shaped_panel_side_2d

  public :: shape_neutral_panel_side

contains

  !> Build one shaped sewing contour and its outward cut allowance.
  !!
  !! `shaping_offset` contains one offset per contour point in model units.
  !! `allowance_mm` is converted using the legacy default-REAL factor `0.1`.
  !! Negative finite values are accepted because the legacy routine also used
  !! signed offsets and allowances without imposing a policy constraint.
  !!
  !! Publication is transactional: `shaped_side` is assigned only after the
  !! input and every computed coordinate have passed validation.
  pure subroutine shape_neutral_panel_side(panel, side, shaping_offset, &
      allowance_mm, shaped_side, valid, message)
    type(neutral_panel_2d), intent(in) :: panel
    integer, intent(in) :: side
    real(real64), intent(in) :: shaping_offset(:)
    real(real64), intent(in) :: allowance_mm
    type(shaped_panel_side_2d), intent(inout) :: shaped_side
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(shaped_panel_side_2d) :: candidate
    integer :: point_count

    valid = .false.
    message = ''
    if (.not. panel%is_valid()) then
      message = 'cannot shape an invalid neutral panel'
      return
    end if
    if (side /= panel_side_lower .and. side /= panel_side_higher) then
      message = 'unknown panel side'
      return
    end if

    point_count = panel%contour_last_index - panel%contour_first_index + 1
    if (size(shaping_offset) /= point_count) then
      message = 'shaping-offset count differs from panel point count'
      return
    end if
    if (.not. all(ieee_is_finite(shaping_offset))) then
      message = 'shaping offsets must be finite'
      return
    end if
    if (.not. ieee_is_finite(allowance_mm)) then
      message = 'sewing allowance must be finite'
      return
    end if

    select case (side)
    case (panel_side_lower)
      call build_shaped_side(panel, side, shaping_offset, allowance_mm, &
          panel%lower_segment_start_u, panel%lower_segment_start_v, &
          panel%lower_segment_end_u, panel%lower_segment_end_v, candidate, &
          valid, message)
    case (panel_side_higher)
      call build_shaped_side(panel, side, shaping_offset, allowance_mm, &
          panel%higher_segment_start_u, panel%higher_segment_start_v, &
          panel%higher_segment_end_u, panel%higher_segment_end_v, candidate, &
          valid, message)
    end select
    if (.not. valid) return

    shaped_side = candidate
  end subroutine shape_neutral_panel_side

  !> Apply the legacy normal convention to an already selected exact edge.
  pure subroutine build_shaped_side(panel, side, shaping_offset, allowance_mm, &
      segment_start_u, segment_start_v, segment_end_u, segment_end_v, &
      candidate, valid, message)
    type(neutral_panel_2d), intent(in) :: panel
    integer, intent(in) :: side
    real(real64), intent(in) :: shaping_offset(:), allowance_mm
    real(real64), intent(in) :: segment_start_u(:), segment_start_v(:)
    real(real64), intent(in) :: segment_end_u(:), segment_end_v(:)
    type(shaped_panel_side_2d), intent(out) :: candidate
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    integer :: segment_index, point_count
    real(real64) :: delta_u, delta_v, normal_u, normal_v
    real(real64) :: allowance_model

    valid = .false.
    message = ''
    point_count = size(shaping_offset)

    ! Reject all degenerate segments before allocating or computing a partial
    ! candidate.  The original routine gave such a segment arbitrary axis
    ! signs; there is no meaningful manufacturing normal to preserve.
    do segment_index = 1, point_count - 1
      delta_u = segment_end_u(segment_index) - &
          segment_start_u(segment_index)
      delta_v = segment_end_v(segment_index) - &
          segment_start_v(segment_index)
      if (is_exact_zero(delta_u) .and. is_exact_zero(delta_v)) then
        message = 'neutral panel contains a degenerate edge segment'
        return
      end if
    end do

    candidate%panel_index = panel%panel_index
    candidate%surface = panel%surface
    candidate%side = side
    candidate%contour_first_index = panel%contour_first_index
    candidate%contour_last_index = panel%contour_last_index
    allocate(candidate%sewing_u(point_count), &
        candidate%sewing_v(point_count), candidate%cut_u(point_count), &
        candidate%cut_v(point_count))
    allowance_model = allowance_mm*legacy_allowance_mm_to_model

    ! `puntslat` computed its initial angle after the segment loop and omitted
    ! the horizontal-segment special case used for all segment endpoints.
    delta_u = segment_end_u(1) - segment_start_u(1)
    delta_v = segment_end_v(1) - segment_start_v(1)
    call legacy_normal(delta_u, delta_v, side, .true., normal_u, normal_v)
    candidate%sewing_u(1) = segment_start_u(1) + &
        shaping_offset(1)*normal_u
    candidate%sewing_v(1) = segment_start_v(1) + &
        shaping_offset(1)*normal_v
    candidate%cut_u(1) = candidate%sewing_u(1) + &
        allowance_model*normal_u
    candidate%cut_v(1) = candidate%sewing_v(1) + &
        allowance_model*normal_v

    do segment_index = 1, point_count - 1
      delta_u = segment_end_u(segment_index) - &
          segment_start_u(segment_index)
      delta_v = segment_end_v(segment_index) - &
          segment_start_v(segment_index)
      call legacy_normal(delta_u, delta_v, side, .false., normal_u, normal_v)

      ! Deliberately use the incoming segment endpoint.  At a triangulation
      ! join this can differ from the next segment start and is the exact
      ! selection made by the legacy loop's final write to a shared point.
      candidate%sewing_u(segment_index + 1) = &
          segment_end_u(segment_index) + &
          shaping_offset(segment_index + 1)*normal_u
      candidate%sewing_v(segment_index + 1) = &
          segment_end_v(segment_index) + &
          shaping_offset(segment_index + 1)*normal_v
      candidate%cut_u(segment_index + 1) = &
          candidate%sewing_u(segment_index + 1) + &
          allowance_model*normal_u
      candidate%cut_v(segment_index + 1) = &
          candidate%sewing_v(segment_index + 1) + &
          allowance_model*normal_v
    end do

    if (.not. candidate%is_valid()) then
      message = 'panel shaping produced non-finite geometry'
      return
    end if
    valid = .true.
  end subroutine build_shaped_side

  !> Return the unit normal encoded by the branch order in `puntslat`.
  pure subroutine legacy_normal(delta_u, delta_v, side, initial_point, &
      normal_u, normal_v)
    real(real64), intent(in) :: delta_u, delta_v
    integer, intent(in) :: side
    logical, intent(in) :: initial_point
    real(real64), intent(out) :: normal_u, normal_v

    real(real64) :: angle, sign_u, sign_v

    ! These are intentionally independent IF statements.  On an axis, more
    ! than one condition matches and the last match reproduces the original
    ! routine's sign choice.
    sign_u = 0.0_real64
    sign_v = 0.0_real64
    if (delta_u >= 0.0_real64 .and. delta_v >= 0.0_real64) then
      sign_u = -1.0_real64
      sign_v = 1.0_real64
    end if
    if (delta_u <= 0.0_real64 .and. delta_v >= 0.0_real64) then
      sign_u = -1.0_real64
      sign_v = -1.0_real64
    end if
    if (delta_u >= 0.0_real64 .and. delta_v <= 0.0_real64) then
      sign_u = 1.0_real64
      sign_v = 1.0_real64
    end if
    if (delta_u <= 0.0_real64 .and. delta_v <= 0.0_real64) then
      sign_u = 1.0_real64
      sign_v = -1.0_real64
    end if
    if (side == panel_side_higher) then
      sign_u = -sign_u
      sign_v = -sign_v
    end if

    if (initial_point) then
      ! Avoid an actual division by zero while preserving DATAN(+/-infinity)
      ! for vertical segments and DATAN(0) for horizontal segments.
      if (is_exact_zero(delta_u)) then
        angle = half_pi
      else
        angle = abs(atan(delta_v/delta_u))
      end if
    else if (is_exact_zero(delta_v)) then
      ! Historical endpoint special case: horizontal means PI/2, not zero.
      angle = half_pi
    else if (is_exact_zero(delta_u)) then
      angle = half_pi
    else
      angle = abs(atan(delta_v/delta_u))
    end if

    normal_u = sign_u*sin(angle)
    normal_v = sign_v*cos(angle)
  end subroutine legacy_normal

  !> Exact zero test used where zero selects a legacy compatibility branch.
  !!
  !! Both comparisons are intentional: unlike a tolerance test, this retains
  !! the old routine's distinction between an axis-aligned segment and a very
  !! small nonzero component, while avoiding compiler equality warnings.
  pure logical function is_exact_zero(value) result(zero)
    real(real64), intent(in) :: value

    zero = value <= 0.0_real64 .and. value >= 0.0_real64
  end function is_exact_zero

  !> Test the self-contained invariants of one shaped side.
  pure logical function shaped_panel_side_is_valid(shaped_side) result(valid)
    class(shaped_panel_side_2d), intent(in) :: shaped_side
    integer :: point_count

    valid = .false.
    if (shaped_side%panel_index < 0) return
    if (shaped_side%surface < surface_extrados .or. &
        shaped_side%surface > surface_intrados) return
    if (shaped_side%side /= panel_side_lower .and. &
        shaped_side%side /= panel_side_higher) return
    if (shaped_side%contour_first_index < 1) return
    if (shaped_side%contour_last_index < &
        shaped_side%contour_first_index) return
    point_count = shaped_side%contour_last_index - &
        shaped_side%contour_first_index + 1
    if (point_count < 2) return
    if (.not. allocated(shaped_side%sewing_u)) return
    if (.not. allocated(shaped_side%sewing_v)) return
    if (.not. allocated(shaped_side%cut_u)) return
    if (.not. allocated(shaped_side%cut_v)) return
    if (any([lbound(shaped_side%sewing_u, 1), &
        lbound(shaped_side%sewing_v, 1), lbound(shaped_side%cut_u, 1), &
        lbound(shaped_side%cut_v, 1)] /= 1)) return
    if (size(shaped_side%sewing_u) /= point_count) return
    if (size(shaped_side%sewing_v) /= point_count) return
    if (size(shaped_side%cut_u) /= point_count) return
    if (size(shaped_side%cut_v) /= point_count) return
    if (.not. all(ieee_is_finite(shaped_side%sewing_u))) return
    if (.not. all(ieee_is_finite(shaped_side%sewing_v))) return
    if (.not. all(ieee_is_finite(shaped_side%cut_u))) return
    if (.not. all(ieee_is_finite(shaped_side%cut_v))) return
    valid = .true.
  end function shaped_panel_side_is_valid

end module leparagliding_panel_shaping
