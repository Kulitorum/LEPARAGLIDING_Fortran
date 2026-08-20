program test_neutral_development
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_quiet_nan, ieee_value
  use leparagliding_domain_model
  use leparagliding_neutral_development
  implicit none

  type(spatial_rib_geometry_3d) :: lower_rib, higher_rib
  type(spatial_rib_geometry_3d) :: intake_lower_rib, intake_higher_rib
  type(spatial_rib_geometry_3d) :: short_higher_rib, bad_support_higher_rib
  type(spatial_rib_geometry_3d) :: intrados_lower_rib, intrados_higher_rib
  type(spatial_rib_geometry_3d) :: nonadjacent_intrados_rib
  type(profile_topology) :: topology, different_intrados_topology
  type(profile_topology) :: mismatched_topology, intake_topology
  type(profile_topology) :: mismatched_intake_topology
  type(profile_topology) :: intrados_topology, mismatched_intrados_topology
  type(profile_topology) :: oversized_intrados_topology
  type(neutral_panel_2d) :: panel, intrados_panel
  type(quadrilateral_distances_3d), allocatable :: source_distances(:)
  type(quadrilateral_distances_3d), allocatable :: intrados_distances(:)
  character(len=160) :: message
  logical :: valid
  real(real64) :: saved_higher_gap, developed_end_width
  real(real64) :: saved_support_end_v
  real(real64) :: saved_intrados_end_v

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

  ! Intake owns its contour segments and exposes the following legacy
  ! quadrilateral separately as tangent/support geometry.
  intake_topology%point_count = 6
  intake_topology%extrados = index_range(1, 3)
  intake_topology%intake = index_range(3, 5)
  intake_topology%intrados = index_range(5, 6)
  intake_topology%leading_edge_index = 3
  call require(intake_topology%is_valid(), 'intake topology is invalid')

  intake_lower_rib%rib_index = 0
  intake_lower_rib%x = [0.0_real64, 0.0_real64, 0.0_real64, &
      0.0_real64, 0.0_real64, 0.0_real64]
  intake_lower_rib%y = [99.0_real64, 98.0_real64, 0.0_real64, &
      2.0_real64, 5.0_real64, 9.0_real64]
  intake_lower_rib%z = 0.0_real64*intake_lower_rib%x
  intake_higher_rib%rib_index = 1
  intake_higher_rib%x = [4.0_real64, 4.0_real64, 4.0_real64, &
      4.0_real64, 4.0_real64, 4.0_real64]
  intake_higher_rib%y = intake_lower_rib%y
  intake_higher_rib%z = 0.0_real64*intake_higher_rib%x

  call develop_intake_panel(intake_lower_rib, intake_higher_rib, &
      intake_topology, intake_topology, panel, source_distances, valid, message)
  call require(valid, 'planar intake rejected: '//trim(message))
  call require(panel%is_valid(), 'typed intake is not a valid neutral panel')
  call require(panel%surface == surface_intake, 'typed intake surface identity')
  call require(panel%contour_first_index == 3 .and. &
      panel%contour_last_index == 5, 'typed intake contour range')
  call require(size(panel%lower_segment_start_u) == 2, &
      'support leaked into exact intake segments')
  call require(size(source_distances) == 3, &
      'intake traces do not include the final support entry')
  call require(panel%has_post_surface_support, &
      'typed intake omitted its post-surface support')
  call require_close(panel%lower_segment_start_v(1), 0.0_real64, &
      'intake did not reset its local development origin')
  call require_close(panel%lower_segment_end_v(1), 2.0_real64, &
      'first intake segment endpoint')
  call require_close(panel%lower_segment_end_v(2), 5.0_real64, &
      'last intake contour endpoint')
  call require_close(panel%support_lower_start_v, 5.0_real64, &
      'intake support start')
  call require_close(panel%support_lower_end_v, 9.0_real64, &
      'intake support end')
  call require_close(panel%support_higher_start_u, 4.0_real64, &
      'higher intake support start')
  call require_close(panel%support_lower_join_gap, 0.0_real64, &
      'lower intake support join gap')
  call require_close(panel%support_higher_join_gap, 0.0_real64, &
      'higher intake support join gap')
  call require_close(source_distances(1)%lower_contour_edge, 2.0_real64, &
      'first intake source distance')
  call require_close(source_distances(2)%lower_contour_edge, 3.0_real64, &
      'second intake source distance')
  call require_close(source_distances(3)%lower_contour_edge, 4.0_real64, &
      'intake support source distance')

  ! Intake range and support failures are transactional.
  saved_support_end_v = panel%support_lower_end_v
  mismatched_intake_topology%point_count = 7
  mismatched_intake_topology%extrados = index_range(1, 3)
  mismatched_intake_topology%intake = index_range(3, 6)
  mismatched_intake_topology%intrados = index_range(6, 7)
  mismatched_intake_topology%leading_edge_index = 3
  call develop_intake_panel(intake_lower_rib, intake_higher_rib, &
      intake_topology, mismatched_intake_topology, panel, source_distances, &
      valid, message)
  call require(.not. valid, 'mismatched intake indices were accepted')
  call require_close(panel%support_lower_end_v, saved_support_end_v, &
      'intake topology failure changed the previous panel')
  call require(size(source_distances) == 3, &
      'intake topology failure changed the previous traces')

  short_higher_rib = intake_higher_rib
  short_higher_rib%x = intake_higher_rib%x(1:5)
  short_higher_rib%y = intake_higher_rib%y(1:5)
  short_higher_rib%z = intake_higher_rib%z(1:5)
  call develop_intake_panel(intake_lower_rib, short_higher_rib, &
      intake_topology, intake_topology, panel, source_distances, valid, message)
  call require(.not. valid, 'missing intake support point was accepted')
  call require_close(panel%support_lower_end_v, saved_support_end_v, &
      'intake capacity failure changed the previous panel')

  bad_support_higher_rib = intake_higher_rib
  bad_support_higher_rib%x(5) = intake_lower_rib%x(5)
  bad_support_higher_rib%y(5) = intake_lower_rib%y(5)
  bad_support_higher_rib%z(5) = intake_lower_rib%z(5)
  call develop_intake_panel(intake_lower_rib, bad_support_higher_rib, &
      intake_topology, intake_topology, panel, source_distances, valid, message)
  call require(.not. valid, 'degenerate intake support was accepted')
  call require_close(panel%support_lower_end_v, saved_support_end_v, &
      'intake support failure changed the previous panel')
  call require(size(source_distances) == 3, &
      'intake support failure changed the previous traces')

  ! Intrados starts at the shared intake endpoint F=np2+np3-1=5 and owns
  ! exactly F:L-1. Its local development does not require any legacy scratch
  ! storage for the first segment.
  intrados_topology%point_count = 7
  intrados_topology%extrados = index_range(1, 3)
  intrados_topology%intake = index_range(3, 5)
  intrados_topology%intrados = index_range(5, 7)
  intrados_topology%leading_edge_index = 3
  call require(intrados_topology%is_valid(), 'intrados topology is invalid')

  intrados_lower_rib%rib_index = 0
  intrados_lower_rib%x = [0.0_real64, 0.0_real64, 0.0_real64, &
      0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64]
  intrados_lower_rib%y = [99.0_real64, 98.0_real64, 97.0_real64, &
      96.0_real64, 0.0_real64, 2.0_real64, 5.0_real64]
  intrados_lower_rib%z = 0.0_real64*intrados_lower_rib%x
  intrados_higher_rib%rib_index = 1
  intrados_higher_rib%x = [4.0_real64, 4.0_real64, 4.0_real64, &
      4.0_real64, 4.0_real64, 4.0_real64, 4.0_real64]
  intrados_higher_rib%y = intrados_lower_rib%y
  intrados_higher_rib%z = 0.0_real64*intrados_higher_rib%x

  call develop_intrados_panel(intrados_lower_rib, intrados_higher_rib, &
      intrados_topology, intrados_topology, intrados_panel, &
      intrados_distances, valid, message)
  call require(valid, 'planar intrados rejected: '//trim(message))
  call require(intrados_panel%is_valid(), &
      'typed intrados is not a valid neutral panel')
  call require(intrados_panel%surface == surface_intrados, &
      'typed intrados surface identity')
  call require(intrados_panel%contour_first_index == 5 .and. &
      intrados_panel%contour_last_index == 7, 'typed intrados contour range')
  call require(size(intrados_panel%lower_segment_start_u) == 2, &
      'typed intrados segment count')
  call require(size(intrados_distances) == 2, &
      'typed intrados trace count')
  call require(.not. intrados_panel%has_post_surface_support, &
      'intrados unexpectedly exposed a post-surface support segment')
  call require_close(intrados_panel%lower_segment_start_v(1), 0.0_real64, &
      'intrados did not reset its local development origin')
  call require_close(intrados_panel%lower_segment_end_v(1), 2.0_real64, &
      'first intrados segment endpoint')
  call require_close(intrados_panel%lower_segment_end_v(2), 5.0_real64, &
      'last intrados segment endpoint')
  call require_close(intrados_panel%higher_segment_start_u(1), 4.0_real64, &
      'first higher intrados segment start')
  call require_close(intrados_distances(1)%lower_contour_edge, &
      2.0_real64, 'first intrados source distance')
  call require_close(intrados_distances(2)%lower_contour_edge, &
      3.0_real64, 'second intrados source distance')

  ! Intrados topology, identity, capacity, and geometry failures are all
  ! transactional: neither the previous exact panel nor its traces may change.
  saved_intrados_end_v = intrados_panel%lower_segment_end_v(2)
  mismatched_intrados_topology%point_count = 7
  mismatched_intrados_topology%extrados = index_range(1, 2)
  mismatched_intrados_topology%intake = index_range(2, 4)
  mismatched_intrados_topology%intrados = index_range(4, 7)
  mismatched_intrados_topology%leading_edge_index = 2
  call develop_intrados_panel(intrados_lower_rib, intrados_higher_rib, &
      intrados_topology, mismatched_intrados_topology, intrados_panel, &
      intrados_distances, valid, message)
  call require(.not. valid, 'mismatched intrados indices were accepted')
  call require_close(intrados_panel%lower_segment_end_v(2), &
      saved_intrados_end_v, 'intrados topology failure changed the panel')
  call require(size(intrados_distances) == 2, &
      'intrados topology failure changed the traces')

  nonadjacent_intrados_rib = intrados_higher_rib
  nonadjacent_intrados_rib%rib_index = 2
  call develop_intrados_panel(intrados_lower_rib, nonadjacent_intrados_rib, &
      intrados_topology, intrados_topology, intrados_panel, &
      intrados_distances, valid, message)
  call require(.not. valid, 'non-adjacent intrados ribs were accepted')
  call require_close(intrados_panel%lower_segment_end_v(2), &
      saved_intrados_end_v, 'intrados identity failure changed the panel')

  oversized_intrados_topology%point_count = 8
  oversized_intrados_topology%extrados = index_range(1, 3)
  oversized_intrados_topology%intake = index_range(3, 6)
  oversized_intrados_topology%intrados = index_range(6, 8)
  oversized_intrados_topology%leading_edge_index = 3
  call develop_intrados_panel(intrados_lower_rib, intrados_higher_rib, &
      oversized_intrados_topology, oversized_intrados_topology, &
      intrados_panel, intrados_distances, valid, message)
  call require(.not. valid, 'uncovered intrados range was accepted')
  call require_close(intrados_panel%lower_segment_end_v(2), &
      saved_intrados_end_v, 'intrados capacity failure changed the panel')

  nonadjacent_intrados_rib = intrados_higher_rib
  nonadjacent_intrados_rib%x(5) = intrados_lower_rib%x(5)
  nonadjacent_intrados_rib%y(5) = intrados_lower_rib%y(5)
  nonadjacent_intrados_rib%z(5) = intrados_lower_rib%z(5)
  call develop_intrados_panel(intrados_lower_rib, nonadjacent_intrados_rib, &
      intrados_topology, intrados_topology, intrados_panel, &
      intrados_distances, valid, message)
  call require(.not. valid, 'degenerate first intrados segment was accepted')
  call require_close(intrados_panel%lower_segment_end_v(2), &
      saved_intrados_end_v, 'intrados geometry failure changed the panel')
  call require(size(intrados_distances) == 2, &
      'intrados geometry failure changed the traces')

  write (*, '(A)') 'PASS: pure neutral surface development'

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
