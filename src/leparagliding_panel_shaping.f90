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
  use leparagliding_domain_model, only : profile_topology, neutral_panel_2d, &
      neutral_boundary_edge_2d, production_boundary_edge_2d, &
      surface_extrados, surface_intake, surface_intrados, &
      legacy_production_lower_sewing_slot, &
      legacy_production_higher_sewing_slot, &
      legacy_production_lower_cut_slot, &
      legacy_production_higher_cut_slot
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
  public :: shape_neutral_boundary_edge
  public :: write_legacy_shaped_panel_side

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

    candidate%panel_index = panel%panel_index
    candidate%surface = panel%surface
    candidate%side = side
    candidate%contour_first_index = panel%contour_first_index
    candidate%contour_last_index = panel%contour_last_index
    select case (side)
    case (panel_side_lower)
      call build_shaped_coordinates(side, shaping_offset, allowance_mm, &
          panel%lower_segment_start_u, panel%lower_segment_start_v, &
          panel%lower_segment_end_u, panel%lower_segment_end_v, &
          candidate%sewing_u, candidate%sewing_v, candidate%cut_u, &
          candidate%cut_v, valid, message)
    case (panel_side_higher)
      call build_shaped_coordinates(side, shaping_offset, allowance_mm, &
          panel%higher_segment_start_u, panel%higher_segment_start_v, &
          panel%higher_segment_end_u, panel%higher_segment_end_v, &
          candidate%sewing_u, candidate%sewing_v, candidate%cut_u, &
          candidate%cut_v, valid, message)
    end select
    if (.not. valid) return
    if (.not. candidate%is_valid()) then
      valid = .false.
      message = 'panel shaping produced invalid geometry'
      return
    end if

    shaped_side = candidate
  end subroutine shape_neutral_panel_side

  !> Publish one shaped side through its exact legacy production slots.
  !!
  !! The shaped surface must cover exactly the corresponding topology range.
  !! The lower side owns slots 9/11 and the higher side owns slots 10/12.  All
  !! array, identity, range, and slot checks complete before the first write,
  !! so rejected calls leave both destinations unchanged.  `legacy_u` and
  !! `legacy_v` must be distinct actual arguments.
  pure subroutine write_legacy_shaped_panel_side(shaped_side, topology, &
      legacy_u, legacy_v, valid, message)
    type(shaped_panel_side_2d), intent(in) :: shaped_side
    type(profile_topology), intent(in) :: topology
    real(real64), intent(inout) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    integer :: expected_first, expected_last, sewing_slot, cut_slot

    valid = .false.
    message = ''
    if (any(shape(legacy_u) /= shape(legacy_v))) then
      message = 'legacy U/V shaped-side destination shapes differ'
      return
    end if
    if (.not. shaped_side%is_valid()) then
      message = 'cannot write an invalid shaped panel side'
      return
    end if
    if (.not. topology%is_valid()) then
      message = 'shaped-side write-back received invalid topology'
      return
    end if
    select case (shaped_side%surface)
    case (surface_extrados)
      expected_first = topology%extrados%first
      expected_last = topology%extrados%last
    case (surface_intake)
      expected_first = topology%intake%first
      expected_last = topology%intake%last
    case (surface_intrados)
      expected_first = topology%intrados%first
      expected_last = topology%intrados%last
    case default
      message = 'shaped-side write-back does not support this surface'
      return
    end select
    if (shaped_side%contour_first_index /= expected_first .or. &
        shaped_side%contour_last_index /= expected_last) then
      message = 'shaped panel side range differs from source topology'
      return
    end if
    select case (shaped_side%side)
    case (panel_side_lower)
      sewing_slot = legacy_production_lower_sewing_slot
      cut_slot = legacy_production_lower_cut_slot
    case (panel_side_higher)
      sewing_slot = legacy_production_higher_sewing_slot
      cut_slot = legacy_production_higher_cut_slot
    case default
      message = 'shaped-side write-back received an unknown panel side'
      return
    end select
    if (shaped_side%panel_index < lbound(legacy_u, 1) .or. &
        shaped_side%panel_index > ubound(legacy_u, 1)) then
      message = 'shaped panel index is outside legacy destinations'
      return
    end if
    if (shaped_side%contour_first_index < lbound(legacy_u, 2) .or. &
        shaped_side%contour_last_index > ubound(legacy_u, 2)) then
      message = 'shaped panel side exceeds legacy point capacity'
      return
    end if
    if (sewing_slot < lbound(legacy_u, 3) .or. &
        cut_slot > ubound(legacy_u, 3)) then
      message = 'legacy array lacks selected shaped-side production slots'
      return
    end if

    legacy_u(shaped_side%panel_index, expected_first:expected_last, &
        sewing_slot) = shaped_side%sewing_u
    legacy_v(shaped_side%panel_index, expected_first:expected_last, &
        sewing_slot) = shaped_side%sewing_v
    legacy_u(shaped_side%panel_index, expected_first:expected_last, &
        cut_slot) = shaped_side%cut_u
    legacy_v(shaped_side%panel_index, expected_first:expected_last, &
        cut_slot) = shaped_side%cut_v
    valid = .true.
  end subroutine write_legacy_shaped_panel_side

  !> Shape one physical terminal edge without inventing an outward panel.
  !!
  !! A terminal production boundary always uses the lower/outward normal and
  !! owns only sewing/cut geometry for legacy slots 9/11.  Its neutral source
  !! is the higher edge of the final real panel, retained in a distinct type.
  pure subroutine shape_neutral_boundary_edge(neutral_edge, shaping_offset, &
      allowance_mm, production_edge, valid, message)
    type(neutral_boundary_edge_2d), intent(in) :: neutral_edge
    real(real64), intent(in) :: shaping_offset(:)
    real(real64), intent(in) :: allowance_mm
    type(production_boundary_edge_2d), intent(inout) :: production_edge
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(production_boundary_edge_2d) :: candidate
    integer :: point_count

    valid = .false.
    message = ''
    if (.not. neutral_edge%is_valid()) then
      message = 'cannot shape an invalid neutral boundary edge'
      return
    end if
    if (neutral_edge%surface /= surface_extrados .and. &
        neutral_edge%surface /= surface_intrados) then
      message = 'terminal production shaping supports skin surfaces only'
      return
    end if
    point_count = neutral_edge%contour_last_index - &
        neutral_edge%contour_first_index + 1
    if (size(shaping_offset) /= point_count) then
      message = 'shaping-offset count differs from boundary point count'
      return
    end if
    if (.not. all(ieee_is_finite(shaping_offset))) then
      message = 'terminal shaping offsets must be finite'
      return
    end if
    if (.not. ieee_is_finite(allowance_mm)) then
      message = 'terminal sewing allowance must be finite'
      return
    end if

    candidate%boundary_rib_index = neutral_edge%boundary_rib_index
    candidate%source_panel_index = neutral_edge%source_panel_index
    candidate%surface = neutral_edge%surface
    candidate%contour_first_index = neutral_edge%contour_first_index
    candidate%contour_last_index = neutral_edge%contour_last_index
    call build_shaped_coordinates(panel_side_lower, shaping_offset, &
        allowance_mm, neutral_edge%segment_start_u, &
        neutral_edge%segment_start_v, neutral_edge%segment_end_u, &
        neutral_edge%segment_end_v, candidate%sewing_u, candidate%sewing_v, &
        candidate%cut_u, candidate%cut_v, valid, message)
    if (.not. valid) return
    if (.not. candidate%is_valid()) then
      valid = .false.
      message = 'terminal shaping produced invalid geometry'
      return
    end if

    production_edge = candidate
  end subroutine shape_neutral_boundary_edge

  !> Apply the legacy normal convention to an already selected exact edge.
  pure subroutine build_shaped_coordinates(side, shaping_offset, allowance_mm, &
      segment_start_u, segment_start_v, segment_end_u, segment_end_v, &
      sewing_u, sewing_v, cut_u, cut_v, valid, message)
    integer, intent(in) :: side
    real(real64), intent(in) :: shaping_offset(:), allowance_mm
    real(real64), intent(in) :: segment_start_u(:), segment_start_v(:)
    real(real64), intent(in) :: segment_end_u(:), segment_end_v(:)
    real(real64), allocatable, intent(out) :: sewing_u(:), sewing_v(:)
    real(real64), allocatable, intent(out) :: cut_u(:), cut_v(:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    integer :: segment_index, point_count
    real(real64) :: delta_u, delta_v, normal_u, normal_v
    real(real64) :: allowance_model

    valid = .false.
    message = ''
    point_count = size(shaping_offset)
    if (point_count < 2) then
      message = 'shaped edge needs at least two contour points'
      return
    end if
    if (size(segment_start_u) /= point_count - 1 .or. &
        size(segment_start_v) /= point_count - 1 .or. &
        size(segment_end_u) /= point_count - 1 .or. &
        size(segment_end_v) /= point_count - 1) then
      message = 'neutral edge segment count differs from point count'
      return
    end if

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

    allocate(sewing_u(point_count), sewing_v(point_count), &
        cut_u(point_count), cut_v(point_count))
    allowance_model = allowance_mm*legacy_allowance_mm_to_model

    ! `puntslat` computed its initial angle after the segment loop and omitted
    ! the horizontal-segment special case used for all segment endpoints.
    delta_u = segment_end_u(1) - segment_start_u(1)
    delta_v = segment_end_v(1) - segment_start_v(1)
    call legacy_normal(delta_u, delta_v, side, .true., normal_u, normal_v)
    sewing_u(1) = segment_start_u(1) + &
        shaping_offset(1)*normal_u
    sewing_v(1) = segment_start_v(1) + &
        shaping_offset(1)*normal_v
    cut_u(1) = sewing_u(1) + &
        allowance_model*normal_u
    cut_v(1) = sewing_v(1) + &
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
      sewing_u(segment_index + 1) = &
          segment_end_u(segment_index) + &
          shaping_offset(segment_index + 1)*normal_u
      sewing_v(segment_index + 1) = &
          segment_end_v(segment_index) + &
          shaping_offset(segment_index + 1)*normal_v
      cut_u(segment_index + 1) = &
          sewing_u(segment_index + 1) + &
          allowance_model*normal_u
      cut_v(segment_index + 1) = &
          sewing_v(segment_index + 1) + &
          allowance_model*normal_v
    end do

    if (.not. all(ieee_is_finite(sewing_u)) .or. &
        .not. all(ieee_is_finite(sewing_v)) .or. &
        .not. all(ieee_is_finite(cut_u)) .or. &
        .not. all(ieee_is_finite(cut_v))) then
      message = 'edge shaping produced non-finite geometry'
      return
    end if
    valid = .true.
  end subroutine build_shaped_coordinates

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
