program test_neutral_development
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_quiet_nan, ieee_value
  use leparagliding_domain_model
  use leparagliding_neutral_development
  implicit none

  type(spatial_rib_geometry_3d) :: lower_rib, higher_rib
  type(profile_topology) :: topology, different_intrados_topology
  type(profile_topology) :: mismatched_topology
  type(neutral_panel_2d) :: panel
  type(quadrilateral_distances_3d), allocatable :: source_distances(:)
  character(len=160) :: message
  logical :: valid
  real(real64) :: saved_higher_gap, developed_end_width

  ! One planar 4-by-3 rectangle exercises all six spatial distances and the
  ! canonical zero-angle initial join state.
  topology%point_count = 4
  topology%extrados = index_range(1, 2)
  topology%intake = index_range(2, 3)
  topology%intrados = index_range(3, 4)
  topology%leading_edge_index = 2
  call require(topology%is_valid(), 'rectangle topology is invalid')

  lower_rib%rib_index = 0
  lower_rib%x = [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64]
  lower_rib%y = [0.0_real64, 3.0_real64, 0.0_real64, 0.0_real64]
  lower_rib%z = 0.0_real64*lower_rib%x
  higher_rib%rib_index = 1
  higher_rib%x = [4.0_real64, 4.0_real64, 4.0_real64, 4.0_real64]
  higher_rib%y = [0.0_real64, 3.0_real64, 0.0_real64, 0.0_real64]
  higher_rib%z = 0.0_real64*higher_rib%x

  call develop_extrados_panel(lower_rib, higher_rib, topology, topology, &
      panel, source_distances, valid, message)
  call require(valid, 'planar rectangle rejected: '//trim(message))
  call require(panel%is_valid(), 'planar result is not a valid neutral panel')
  call require(panel%panel_index == 0 .and. panel%lower_rib_index == 0 .and. &
      panel%higher_rib_index == 1, 'panel-zero identities were not retained')
  call require(size(source_distances) == 1, 'rectangle trace count')
  call require_close(source_distances(1)%start_cross_edge, 4.0_real64, &
      'rectangle start cross-edge')
  call require_close(source_distances(1)%lower_start_to_higher_end, &
      5.0_real64, 'rectangle first diagonal')
  call require_close(source_distances(1)%higher_contour_edge, 3.0_real64, &
      'rectangle higher contour edge')
  call require_close(source_distances(1)%higher_start_to_lower_end, &
      5.0_real64, 'rectangle second diagonal')
  call require_close(source_distances(1)%lower_contour_edge, 3.0_real64, &
      'rectangle lower contour edge')
  call require_close(source_distances(1)%end_cross_edge, 4.0_real64, &
      'rectangle end cross-edge')
  call require_close(panel%lower_segment_start_u(1), 0.0_real64, &
      'rectangle lower start U')
  call require_close(panel%lower_segment_end_v(1), 3.0_real64, &
      'rectangle lower end V')
  call require_close(panel%higher_segment_start_u(1), 4.0_real64, &
      'rectangle higher start U')
  call require_close(panel%higher_segment_end_v(1), 3.0_real64, &
      'rectangle higher end V')
  call require_close(panel%maximum_lower_join_gap, 0.0_real64, &
      'one-segment lower join gap')
  call require_close(panel%maximum_higher_join_gap, 0.0_real64, &
      'one-segment higher join gap')

  ! A non-planar two-segment strip proves that the exact higher endpoints and
  ! next-segment starts remain distinct instead of being silently closed.
  topology%point_count = 5
  topology%extrados = index_range(1, 3)
  topology%intake = index_range(3, 4)
  topology%intrados = index_range(4, 5)
  topology%leading_edge_index = 3
  lower_rib%x = [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64]
  lower_rib%y = [0.0_real64, 3.0_real64, 6.0_real64, 0.0_real64, 0.0_real64]
  lower_rib%z = 0.0_real64*lower_rib%x
  higher_rib%x = [4.0_real64, 4.0_real64, 4.0_real64, 4.0_real64, 4.0_real64]
  higher_rib%y = [0.0_real64, 3.0_real64, 6.0_real64, 0.0_real64, 0.0_real64]
  higher_rib%z = [0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 0.0_real64]

  call develop_extrados_panel(lower_rib, higher_rib, topology, topology, &
      panel, source_distances, valid, message)
  call require(valid, 'non-planar strip rejected: '//trim(message))
  call require(size(source_distances) == 2, 'non-planar trace count')
  call require_close(source_distances(1)%end_cross_edge, sqrt(17.0_real64), &
      'non-planar end cross-edge trace')
  call require_close(panel%maximum_lower_join_gap, 0.0_real64, &
      'lower edge did not remain exactly chained')
  call require(panel%maximum_higher_join_gap > 1.0e-8_real64, &
      'non-planar higher join gap was silently closed')
  call require_close(panel%higher_start_biased_u(2), &
      panel%higher_segment_start_u(2), 'start-biased higher point view')
  call require(abs(panel%higher_segment_start_u(2) - &
      panel%higher_segment_end_u(1)) > 1.0e-8_real64 .or. &
      abs(panel%higher_segment_start_v(2) - &
      panel%higher_segment_end_v(1)) > 1.0e-8_real64, &
      'exact non-planar higher endpoints were merged')
  call require_close(segment_length( &
      panel%lower_segment_start_u(1), panel%lower_segment_start_v(1), &
      panel%lower_segment_end_u(1), panel%lower_segment_end_v(1)), &
      source_distances(1)%lower_contour_edge, &
      'developed lower edge does not preserve its source distance')
  call require_close(segment_length( &
      panel%higher_segment_start_u(1), panel%higher_segment_start_v(1), &
      panel%higher_segment_end_u(1), panel%higher_segment_end_v(1)), &
      source_distances(1)%higher_contour_edge, &
      'developed higher edge does not preserve its source distance')
  developed_end_width = segment_length(panel%lower_segment_end_u(1), &
      panel%lower_segment_end_v(1), panel%higher_segment_end_u(1), &
      panel%higher_segment_end_v(1))
  call require(abs(developed_end_width - &
      source_distances(1)%end_cross_edge) > 1.0e-8_real64, &
      'test strip did not expose the legacy end-width residual')

  ! Adjacent source profiles may share extrados indices while using different
  ! intake/intrados discretizations.  The legacy extrados loop accepts this.
  different_intrados_topology%point_count = 6
  different_intrados_topology%extrados = index_range(1, 3)
  different_intrados_topology%intake = index_range(3, 4)
  different_intrados_topology%intrados = index_range(4, 6)
  different_intrados_topology%leading_edge_index = 3
  call develop_extrados_panel(lower_rib, higher_rib, topology, &
      different_intrados_topology, panel, source_distances, valid, message)
  call require(valid, &
      'matching extrados with different intrados was rejected: '//trim(message))

  ! Rejected calls do not leak partially developed geometry or trace arrays.
  saved_higher_gap = panel%maximum_higher_join_gap
  higher_rib%rib_index = 2
  call develop_extrados_panel(lower_rib, higher_rib, topology, topology, &
      panel, source_distances, valid, message)
  call require(.not. valid, 'non-adjacent ribs were accepted')
  call require_close(panel%maximum_higher_join_gap, saved_higher_gap, &
      'identity failure changed the previous panel')
  call require(size(source_distances) == 2, &
      'identity failure changed the previous trace')
  higher_rib%rib_index = 1

  mismatched_topology%point_count = 5
  mismatched_topology%extrados = index_range(1, 2)
  mismatched_topology%intake = index_range(2, 4)
  mismatched_topology%intrados = index_range(4, 5)
  mismatched_topology%leading_edge_index = 2
  call develop_extrados_panel(lower_rib, higher_rib, topology, &
      mismatched_topology, panel, source_distances, valid, message)
  call require(.not. valid, 'mismatched topology was accepted')
  call require_close(panel%maximum_higher_join_gap, saved_higher_gap, &
      'topology failure changed the previous panel')

  higher_rib%x(2) = ieee_value(0.0_real64, ieee_quiet_nan)
  call develop_extrados_panel(lower_rib, higher_rib, topology, topology, &
      panel, source_distances, valid, message)
  call require(.not. valid, 'non-finite spatial rib was accepted')
  call require_close(panel%maximum_higher_join_gap, saved_higher_gap, &
      'non-finite failure changed the previous panel')
  higher_rib%x(2) = 4.0_real64

  higher_rib%x(1) = lower_rib%x(1)
  higher_rib%y(1) = lower_rib%y(1)
  higher_rib%z(1) = lower_rib%z(1)
  call develop_extrados_panel(lower_rib, higher_rib, topology, topology, &
      panel, source_distances, valid, message)
  call require(.not. valid, 'zero start cross-edge was accepted')
  call require_close(panel%maximum_higher_join_gap, saved_higher_gap, &
      'degenerate failure changed the previous panel')

  write (*, '(A)') 'PASS: pure neutral extrados development'

contains

  pure real(real64) function segment_length(start_u, start_v, end_u, end_v) &
      result(length)
    real(real64), intent(in) :: start_u, start_v, end_u, end_v

    length = sqrt((end_u - start_u)**2 + (end_v - start_v)**2)
  end function segment_length

  subroutine require(condition, diagnostic)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: diagnostic

    if (.not. condition) then
      write (*, '(A)') 'FAIL: '//diagnostic
      error stop 1
    end if
  end subroutine require

  subroutine require_close(actual, expected, diagnostic)
    real(real64), intent(in) :: actual, expected
    character(len=*), intent(in) :: diagnostic

    call require(abs(actual - expected) <= 1.0e-10_real64 * &
        (1.0_real64 + max(abs(actual), abs(expected))), diagnostic)
  end subroutine require_close

end program test_neutral_development
