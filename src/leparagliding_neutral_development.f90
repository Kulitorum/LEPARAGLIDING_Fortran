! SPDX-License-Identifier: GPL-3.0-or-later
!
!> Develop spatial wing-surface strips into exact neutral 2D segments.
!!
!! This module extracts the distance-preserving stage-7 quadrilateral
!! calculation without changing its numerical conventions. In particular, it
!! retains the one-argument `atan(dy/dx)` join update and does not force the
!! independently reconstructed higher-index segment joins to close.
module leparagliding_neutral_development
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
  use leparagliding_domain_model, only : neutral_panel_2d, profile_topology, &
      spatial_rib_geometry_3d, surface_extrados, surface_intake, &
      surface_intrados, &
      extrados_topologies_are_index_compatible
  implicit none
  private

  !> The six spatial distances measured from one profile quadrilateral.
  !!
  !! With lower/higher ribs `L/R` and consecutive samples `0/1`, these are
  !! `|R0-L0|`, `|R1-L0|`, `|R1-R0|`, `|L1-R0|`, `|L1-L0|`, and `|R1-L1|`.
  type, public :: quadrilateral_distances_3d
    real(real64) :: start_cross_edge = 0.0_real64
    real(real64) :: lower_start_to_higher_end = 0.0_real64
    real(real64) :: higher_contour_edge = 0.0_real64
    real(real64) :: higher_start_to_lower_end = 0.0_real64
    real(real64) :: lower_contour_edge = 0.0_real64
    real(real64) :: end_cross_edge = 0.0_real64
  contains
    procedure :: is_valid => quadrilateral_distances_are_valid
  end type quadrilateral_distances_3d

  type :: development_join_state
    real(real64) :: lower_u = 0.0_real64
    real(real64) :: lower_v = 0.0_real64
    real(real64) :: cross_edge_angle_rad = 0.0_real64
  end type development_join_state

  type :: developed_segment_2d
    real(real64) :: lower_start_u = 0.0_real64
    real(real64) :: lower_start_v = 0.0_real64
    real(real64) :: lower_end_u = 0.0_real64
    real(real64) :: lower_end_v = 0.0_real64
    real(real64) :: higher_start_u = 0.0_real64
    real(real64) :: higher_start_v = 0.0_real64
    real(real64) :: higher_end_u = 0.0_real64
    real(real64) :: higher_end_v = 0.0_real64
  end type developed_segment_2d

  public :: develop_extrados_panel
  public :: develop_intake_panel
  public :: develop_intrados_panel

contains

  !> Return true when all six source distances are finite and nonnegative.
  pure logical function quadrilateral_distances_are_valid(distances) &
      result(valid)
    class(quadrilateral_distances_3d), intent(in) :: distances

    valid = ieee_is_finite(distances%start_cross_edge) .and. &
        ieee_is_finite(distances%lower_start_to_higher_end) .and. &
        ieee_is_finite(distances%higher_contour_edge) .and. &
        ieee_is_finite(distances%higher_start_to_lower_end) .and. &
        ieee_is_finite(distances%lower_contour_edge) .and. &
        ieee_is_finite(distances%end_cross_edge) .and. &
        distances%start_cross_edge >= 0.0_real64 .and. &
        distances%lower_start_to_higher_end >= 0.0_real64 .and. &
        distances%higher_contour_edge >= 0.0_real64 .and. &
        distances%higher_start_to_lower_end >= 0.0_real64 .and. &
        distances%lower_contour_edge >= 0.0_real64 .and. &
        distances%end_cross_edge >= 0.0_real64
  end function quadrilateral_distances_are_valid

  !> Purely develop all extrados quadrilaterals between two adjacent ribs.
  !!
  !! The update is transactional: on failure, `panel` and `source_distances`
  !! retain their previous values. The exact segment arrays are authoritative;
  !! the point views intentionally remain start-biased at a nonclosing join.
  pure subroutine develop_extrados_panel(lower_rib, higher_rib, &
      lower_topology, higher_topology, panel, source_distances, valid, message)
    type(spatial_rib_geometry_3d), intent(in) :: lower_rib, higher_rib
    type(profile_topology), intent(in) :: lower_topology, higher_topology
    type(neutral_panel_2d), intent(inout) :: panel
    type(quadrilateral_distances_3d), allocatable, intent(inout) :: &
        source_distances(:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    valid = .false.
    message = ''
    if (.not. extrados_topologies_are_index_compatible(lower_topology, &
        higher_topology)) then
      message = 'extrados development requires matching extrados indices'
      return
    end if
    call develop_surface_panel(lower_rib, higher_rib, &
        lower_topology%extrados%first, lower_topology%extrados%last, &
        surface_extrados, .false., panel, source_distances, valid, message)
  end subroutine develop_extrados_panel

  !> Purely develop the intake contour and its post-intake support segment.
  !!
  !! The exact contour arrays cover `intake%first:intake%last-1`. The legacy
  !! stage-7 loop also develops the quadrilateral at `intake%last`; it is not an
  !! intake contour segment and is returned through the explicit support fields
  !! of `neutral_panel_2d`. `source_distances` contains the contour traces
  !! followed by that support trace as its final entry.
  !!
  !! The update is transactional: rejected geometry leaves both outputs
  !! unchanged. Adjacent intake ranges must match exactly, and both spatial
  !! ribs must contain the point after the shared intake/intrados endpoint.
  pure subroutine develop_intake_panel(lower_rib, higher_rib, &
      lower_topology, higher_topology, panel, source_distances, valid, message)
    type(spatial_rib_geometry_3d), intent(in) :: lower_rib, higher_rib
    type(profile_topology), intent(in) :: lower_topology, higher_topology
    type(neutral_panel_2d), intent(inout) :: panel
    type(quadrilateral_distances_3d), allocatable, intent(inout) :: &
        source_distances(:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    valid = .false.
    message = ''
    if (.not. lower_topology%is_valid() .or. &
        .not. higher_topology%is_valid() .or. &
        lower_topology%intake%first /= higher_topology%intake%first .or. &
        lower_topology%intake%last /= higher_topology%intake%last) then
      message = 'intake development requires matching intake indices'
      return
    end if
    if (lower_topology%intake%last + 1 > lower_topology%point_count .or. &
        higher_topology%intake%last + 1 > higher_topology%point_count) then
      message = 'profile topology does not cover the intake support point'
      return
    end if

    call develop_surface_panel(lower_rib, higher_rib, &
        lower_topology%intake%first, lower_topology%intake%last, &
        surface_intake, .true., panel, source_distances, valid, message)
  end subroutine develop_intake_panel

  !> Purely develop the exact intrados segments between two adjacent ribs.
  !!
  !! The developed range is `intrados%first:intrados%last-1`, corresponding to
  !! legacy indices `np(:,2)+np(:,3)-1:np(:,1)-1`. The initial join state is
  !! local to this surface, so its first segment is produced directly without
  !! reading or reserving legacy scratch index 499. Rejected calls leave both
  !! output objects unchanged.
  pure subroutine develop_intrados_panel(lower_rib, higher_rib, &
      lower_topology, higher_topology, panel, source_distances, valid, message)
    type(spatial_rib_geometry_3d), intent(in) :: lower_rib, higher_rib
    type(profile_topology), intent(in) :: lower_topology, higher_topology
    type(neutral_panel_2d), intent(inout) :: panel
    type(quadrilateral_distances_3d), allocatable, intent(inout) :: &
        source_distances(:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    valid = .false.
    message = ''
    if (.not. lower_topology%is_valid() .or. &
        .not. higher_topology%is_valid() .or. &
        lower_topology%intrados%first /= higher_topology%intrados%first .or. &
        lower_topology%intrados%last /= higher_topology%intrados%last) then
      message = 'intrados development requires matching intrados indices'
      return
    end if

    call develop_surface_panel(lower_rib, higher_rib, &
        lower_topology%intrados%first, lower_topology%intrados%last, &
        surface_intrados, .false., panel, source_distances, valid, message)
  end subroutine develop_intrados_panel

  !> Develop one matching surface range, optionally retaining its next segment.
  !!
  !! `source_distances` has one entry per exact contour segment. When
  !! `include_post_surface_support` is true, a final entry describes the
  !! separately stored support segment at `contour_last_index`.
  pure subroutine develop_surface_panel(lower_rib, higher_rib, &
      contour_first_index, contour_last_index, surface, &
      include_post_surface_support, panel, source_distances, valid, message)
    type(spatial_rib_geometry_3d), intent(in) :: lower_rib, higher_rib
    integer, intent(in) :: contour_first_index, contour_last_index, surface
    logical, intent(in) :: include_post_surface_support
    type(neutral_panel_2d), intent(inout) :: panel
    type(quadrilateral_distances_3d), allocatable, intent(inout) :: &
        source_distances(:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(neutral_panel_2d) :: candidate
    type(quadrilateral_distances_3d), allocatable :: candidate_distances(:)
    type(development_join_state) :: state, next_state
    type(developed_segment_2d) :: segment
    integer :: point_count, segment_count, trace_count
    integer :: source_index, local_index
    logical :: segment_valid

    valid = .false.
    message = ''
    if (.not. lower_rib%is_valid() .or. .not. higher_rib%is_valid()) then
      message = 'surface development received invalid spatial rib geometry'
      return
    end if
    if (lower_rib%rib_index < 0 .or. &
        higher_rib%rib_index /= lower_rib%rib_index + 1) then
      message = 'surface development requires adjacent rib identities'
      return
    end if
    if (contour_first_index < 1 .or. &
        contour_last_index <= contour_first_index) then
      message = 'surface development received an invalid contour range'
      return
    end if

    point_count = contour_last_index - contour_first_index + 1
    segment_count = point_count - 1
    trace_count = segment_count
    if (include_post_surface_support) trace_count = trace_count + 1
    if (size(lower_rib%x) < contour_last_index + &
        merge(1, 0, include_post_surface_support) .or. &
        size(higher_rib%x) < contour_last_index + &
        merge(1, 0, include_post_surface_support)) then
      message = 'spatial rib coordinates do not cover the surface range'
      return
    end if

    candidate%panel_index = lower_rib%rib_index
    candidate%lower_rib_index = lower_rib%rib_index
    candidate%higher_rib_index = higher_rib%rib_index
    candidate%surface = surface
    candidate%contour_first_index = contour_first_index
    candidate%contour_last_index = contour_last_index
    candidate%has_post_surface_support = include_post_surface_support

    allocate(candidate%lower_start_biased_u(point_count), &
        candidate%lower_start_biased_v(point_count), &
        candidate%higher_start_biased_u(point_count), &
        candidate%higher_start_biased_v(point_count))
    allocate(candidate%lower_segment_start_u(segment_count), &
        candidate%lower_segment_start_v(segment_count), &
        candidate%lower_segment_end_u(segment_count), &
        candidate%lower_segment_end_v(segment_count), &
        candidate%higher_segment_start_u(segment_count), &
        candidate%higher_segment_start_v(segment_count), &
        candidate%higher_segment_end_u(segment_count), &
        candidate%higher_segment_end_v(segment_count))
    allocate(candidate_distances(trace_count))

    state = development_join_state()
    do source_index = contour_first_index, contour_last_index - 1
      local_index = source_index - contour_first_index + 1
      call measure_spatial_quadrilateral(lower_rib, higher_rib, source_index, &
          candidate_distances(local_index))
      call develop_quadrilateral(candidate_distances(local_index), state, &
          segment, next_state, segment_valid)
      if (.not. segment_valid) then
        message = 'spatial quadrilateral cannot be developed safely'
        return
      end if
      call store_contour_segment(candidate, local_index, segment)
      state = next_state
    end do

    if (include_post_surface_support) then
      call measure_spatial_quadrilateral(lower_rib, higher_rib, &
          contour_last_index, candidate_distances(trace_count))
      call develop_quadrilateral(candidate_distances(trace_count), state, &
          segment, next_state, segment_valid)
      if (.not. segment_valid) then
        message = 'post-surface support cannot be developed safely'
        return
      end if
      candidate%support_lower_start_u = segment%lower_start_u
      candidate%support_lower_start_v = segment%lower_start_v
      candidate%support_lower_end_u = segment%lower_end_u
      candidate%support_lower_end_v = segment%lower_end_v
      candidate%support_higher_start_u = segment%higher_start_u
      candidate%support_higher_start_v = segment%higher_start_v
      candidate%support_higher_end_u = segment%higher_end_u
      candidate%support_higher_end_v = segment%higher_end_v
      candidate%support_lower_join_gap = sqrt( &
          (segment%lower_start_u - &
          candidate%lower_start_biased_u(point_count))**2 + &
          (segment%lower_start_v - &
          candidate%lower_start_biased_v(point_count))**2)
      candidate%support_higher_join_gap = sqrt( &
          (segment%higher_start_u - &
          candidate%higher_start_biased_u(point_count))**2 + &
          (segment%higher_start_v - &
          candidate%higher_start_biased_v(point_count))**2)
    end if

    if (.not. candidate%is_valid()) then
      message = 'developed surface failed neutral-panel validation'
      return
    end if
    panel = candidate
    source_distances = candidate_distances
    valid = .true.
  end subroutine develop_surface_panel

  !> Store one exact segment and update the point views and join diagnostics.
  pure subroutine store_contour_segment(panel, local_index, segment)
    type(neutral_panel_2d), intent(inout) :: panel
    integer, intent(in) :: local_index
    type(developed_segment_2d), intent(in) :: segment

    if (local_index > 1) then
      panel%maximum_lower_join_gap = max(panel%maximum_lower_join_gap, sqrt( &
          (segment%lower_start_u - &
          panel%lower_segment_end_u(local_index - 1))**2 + &
          (segment%lower_start_v - &
          panel%lower_segment_end_v(local_index - 1))**2))
      panel%maximum_higher_join_gap = max(panel%maximum_higher_join_gap, sqrt( &
          (segment%higher_start_u - &
          panel%higher_segment_end_u(local_index - 1))**2 + &
          (segment%higher_start_v - &
          panel%higher_segment_end_v(local_index - 1))**2))
    end if

    panel%lower_start_biased_u(local_index) = segment%lower_start_u
    panel%lower_start_biased_v(local_index) = segment%lower_start_v
    panel%higher_start_biased_u(local_index) = segment%higher_start_u
    panel%higher_start_biased_v(local_index) = segment%higher_start_v
    panel%lower_start_biased_u(local_index + 1) = segment%lower_end_u
    panel%lower_start_biased_v(local_index + 1) = segment%lower_end_v
    panel%higher_start_biased_u(local_index + 1) = segment%higher_end_u
    panel%higher_start_biased_v(local_index + 1) = segment%higher_end_v
    panel%lower_segment_start_u(local_index) = segment%lower_start_u
    panel%lower_segment_start_v(local_index) = segment%lower_start_v
    panel%lower_segment_end_u(local_index) = segment%lower_end_u
    panel%lower_segment_end_v(local_index) = segment%lower_end_v
    panel%higher_segment_start_u(local_index) = segment%higher_start_u
    panel%higher_segment_start_v(local_index) = segment%higher_start_v
    panel%higher_segment_end_u(local_index) = segment%higher_end_u
    panel%higher_segment_end_v(local_index) = segment%higher_end_v
  end subroutine store_contour_segment

  !> Measure the legacy six-distance schema for one spatial quadrilateral.
  pure subroutine measure_spatial_quadrilateral(lower_rib, higher_rib, &
      point_index, distances)
    type(spatial_rib_geometry_3d), intent(in) :: lower_rib, higher_rib
    integer, intent(in) :: point_index
    type(quadrilateral_distances_3d), intent(out) :: distances

    distances%start_cross_edge = distance_3d( &
        higher_rib%x(point_index), higher_rib%y(point_index), &
        higher_rib%z(point_index), lower_rib%x(point_index), &
        lower_rib%y(point_index), lower_rib%z(point_index))
    distances%lower_start_to_higher_end = distance_3d( &
        higher_rib%x(point_index + 1), higher_rib%y(point_index + 1), &
        higher_rib%z(point_index + 1), lower_rib%x(point_index), &
        lower_rib%y(point_index), lower_rib%z(point_index))
    distances%higher_contour_edge = distance_3d( &
        higher_rib%x(point_index + 1), higher_rib%y(point_index + 1), &
        higher_rib%z(point_index + 1), higher_rib%x(point_index), &
        higher_rib%y(point_index), higher_rib%z(point_index))
    distances%higher_start_to_lower_end = distance_3d( &
        higher_rib%x(point_index), higher_rib%y(point_index), &
        higher_rib%z(point_index), lower_rib%x(point_index + 1), &
        lower_rib%y(point_index + 1), lower_rib%z(point_index + 1))
    distances%lower_contour_edge = distance_3d( &
        lower_rib%x(point_index + 1), lower_rib%y(point_index + 1), &
        lower_rib%z(point_index + 1), lower_rib%x(point_index), &
        lower_rib%y(point_index), lower_rib%z(point_index))
    distances%end_cross_edge = distance_3d( &
        higher_rib%x(point_index + 1), higher_rib%y(point_index + 1), &
        higher_rib%z(point_index + 1), lower_rib%x(point_index + 1), &
        lower_rib%y(point_index + 1), lower_rib%z(point_index + 1))
  end subroutine measure_spatial_quadrilateral

  !> Reconstruct one 2D quadrilateral with the exact legacy expression order.
  pure subroutine develop_quadrilateral(distances, state, segment, next_state, &
      valid)
    type(quadrilateral_distances_3d), intent(in) :: distances
    type(development_join_state), intent(in) :: state
    type(developed_segment_2d), intent(out) :: segment
    type(development_join_state), intent(out) :: next_state
    logical, intent(out) :: valid

    real(real64) :: pa, pb, pc, pd, pe
    real(real64) :: pa2r, pa1r, phr, pa2l, pa1l, phl
    real(real64) :: right_height_squared, left_height_squared
    real(real64) :: cosine, sine, cross_u, cross_v

    valid = .false.
    segment = developed_segment_2d()
    next_state = development_join_state()
    if (.not. distances%is_valid()) return
    pa = distances%start_cross_edge
    pb = distances%lower_start_to_higher_end
    pc = distances%higher_contour_edge
    pd = distances%higher_start_to_lower_end
    pe = distances%lower_contour_edge
    if (pa <= 0.0_real64) return

    pa2r = (pa*pa - pb*pb + pc*pc)/(2.0*pa)
    pa1r = pa - pa2r
    right_height_squared = pc*pc - pa2r*pa2r
    if (right_height_squared < 0.0_real64) return
    phr = sqrt(right_height_squared)

    pa2l = (pa*pa - pe*pe + pd*pd)/(2.0*pa)
    pa1l = pa - pa2l
    left_height_squared = pd*pd - pa2l*pa2l
    if (left_height_squared < 0.0_real64) return
    phl = sqrt(left_height_squared)

    cosine = cos(state%cross_edge_angle_rad)
    sine = sin(state%cross_edge_angle_rad)
    segment%lower_start_u = state%lower_u
    segment%lower_start_v = state%lower_v
    segment%higher_start_u = pa*cosine + state%lower_u
    segment%higher_start_v = pa*sine + state%lower_v
    segment%lower_end_u = pa1l*cosine - phl*sine + state%lower_u
    segment%lower_end_v = pa1l*sine + phl*cosine + state%lower_v
    segment%higher_end_u = pa1r*cosine - phr*sine + state%lower_u
    segment%higher_end_v = pa1r*sine + phr*cosine + state%lower_v

    next_state%lower_u = segment%lower_end_u
    next_state%lower_v = segment%lower_end_v
    cross_u = segment%higher_end_u - segment%lower_end_u
    cross_v = segment%higher_end_v - segment%lower_end_v
    next_state%cross_edge_angle_rad = atan(cross_v/cross_u)
    if (.not. ieee_is_finite(next_state%cross_edge_angle_rad)) return
    valid = .true.
  end subroutine develop_quadrilateral

  !> Reproduce the legacy real-exponent Euclidean distance expression.
  pure real(real64) function distance_3d(first_x, first_y, first_z, &
      second_x, second_y, second_z) result(distance)
    real(real64), intent(in) :: first_x, first_y, first_z
    real(real64), intent(in) :: second_x, second_y, second_z

    distance = sqrt((first_x - second_x)**2.0 + &
        (first_y - second_y)**2.0 + (first_z - second_z)**2.0)
  end function distance_3d

end module leparagliding_neutral_development
