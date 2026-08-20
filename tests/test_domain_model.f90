program test_domain_model
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_quiet_nan, ieee_value
  use leparagliding_domain_model
  implicit none

  real(real64) :: legacy_u(0:2, 6, 12), legacy_v(0:2, 6, 12)
  integer :: legacy_np(0:2, 9)
  real(real64) :: legacy_x(0:2, 4), legacy_y(0:2, 4), legacy_z(0:2, 4)
  real(real64) :: pl1_u(0:2, 500), pl1_v(0:2, 500)
  real(real64) :: pl2_u(0:2, 500), pl2_v(0:2, 500)
  real(real64) :: pr1_u(0:2, 500), pr1_v(0:2, 500)
  real(real64) :: pr2_u(0:2, 500), pr2_v(0:2, 500)
  real(real64) :: planform_station(0:21), spatial_station(0:21)
  real(real64) :: spatial_height(0:21)
  type(normalized_profile_2d) :: profile
  type(profile_topology) :: topology, saved_topology, neutral_topology
  type(profile_topology) :: test_topology, mismatched_topology
  type(profile_topology) :: maximum_capacity_topology
  type(profile_topology) :: profile_topologies(0:2)
  type(spatial_rib_geometry_3d) :: spatial_rib, zero_based_spatial_rib
  type(production_panel_edges_2d) :: panel
  type(production_panel_2d) :: complete_panel
  type(neutral_panel_2d) :: neutral_panel, extrados_write_panel
  type(neutral_panel_2d) :: zero_based_neutral_panel
  type(neutral_panel_2d) :: disconnected_support_panel
  type(neutral_panel_2d) :: maximum_capacity_intake_panel
  type(neutral_panel_2d) :: maximum_capacity_intrados_panel
  type(neutral_boundary_edge_2d) :: boundary_edge, invalid_boundary_edge
  type(rib_identity), allocatable :: identities(:), saved_identities(:)
  type(color_division) :: division
  character(len=120) :: message
  logical :: valid, panel_zero_active, parity_consistent, gap_valid
  real(real64) :: gap
  integer :: i

  legacy_u = 0.0_real64
  legacy_v = 0.0_real64
  legacy_np = 0
  legacy_x = 0.0_real64
  legacy_y = 0.0_real64
  legacy_z = 0.0_real64
  pl1_u = 0.0_real64
  pl1_v = 0.0_real64
  pl2_u = 0.0_real64
  pl2_v = 0.0_real64
  pr1_u = 0.0_real64
  pr1_v = 0.0_real64
  pr2_u = 0.0_real64
  pr2_v = 0.0_real64

  legacy_np(0, 1:6) = [4, 2, 2, 2, 3, 2]
  legacy_np(1, 1:6) = [4, 2, 2, 2, 3, 2]
  legacy_np(2, 1:6) = [5, 3, 2, 2, 4, 3]

  call copy_legacy_profile_topology(legacy_np, 1, topology, valid, message)
  call require(valid, 'valid profile topology rejected: '//trim(message))
  call require(topology%is_valid(), 'copied profile topology is invalid')
  call require(topology%extrados%first == 1, 'extrados first point')
  call require(topology%extrados%last == 2, 'extrados last point')
  call require(topology%intake%last == 3, 'intake last point')
  call require(topology%intrados%last == 4, 'intrados last point')
  saved_topology = topology
  profile_topologies(1) = topology
  call copy_legacy_profile_topology(legacy_np, 0, profile_topologies(0), &
      valid, message)
  call require(valid, 'panel-zero topology rejected: '//trim(message))
  call copy_legacy_profile_topology(legacy_np, 2, profile_topologies(2), &
      valid, message)
  call require(valid, 'higher topology rejected: '//trim(message))
  legacy_np(1, 5) = 4
  call copy_legacy_profile_topology(legacy_np, 1, topology, valid, message)
  call require(.not. valid, 'inconsistent intake endpoint was accepted')
  call require(topology%intake%last == saved_topology%intake%last, &
      'failed topology copy changed the destination')
  legacy_np(1, 5) = 3
  legacy_np(1, 1) = 5
  call copy_legacy_profile_topology(legacy_np, 1, test_topology, valid, &
      message)
  call require(.not. valid, 'inconsistent total point count was accepted')
  legacy_np(1, 1) = 4
  legacy_np(1, 6) = 4
  call copy_legacy_profile_topology(legacy_np, 1, test_topology, valid, &
      message)
  call require(valid, 'global leading-edge index was over-constrained')
  legacy_np(1, 6) = 2
  legacy_np(1, 1:6) = [500, 250, 20, 232, 269, 250]
  call copy_legacy_profile_topology(legacy_np, 1, test_topology, valid, &
      message)
  call require(valid, '500-point topology rejected after scratch removal')
  legacy_np(1, 1:6) = [4, 2, 2, 2, 3, 2]

  legacy_u(1, 1:4, legacy_normalized_profile_slot) = &
      [100.0_real64, 50.0_real64, 0.0_real64, 100.0_real64]
  legacy_v(1, 1:4, legacy_normalized_profile_slot) = &
      [0.0_real64, 12.0_real64, 0.0_real64, -5.0_real64]

  call copy_legacy_normalized_profile(legacy_u, legacy_v, 1, topology, &
      profile, valid, message)
  call require(valid, 'valid normalized profile rejected: '//trim(message))
  call require(profile%is_valid(), 'copied normalized profile is invalid')
  call require(profile%rib_index == 1, 'normalized rib index')
  call require(size(profile%chord_percent) == 4, 'normalized point count')
  call require_close(profile%chord_percent(2), 50.0_real64, &
      'normalized chord coordinate')
  call require_close(profile%height_percent(2), 12.0_real64, &
      'normalized height coordinate')

  ! The typed profile owns its copy rather than aliasing mutable legacy state.
  legacy_u(1, 2, legacy_normalized_profile_slot) = 75.0_real64
  call require_close(profile%chord_percent(2), 50.0_real64, &
      'normalized profile must own copied data')

  legacy_u(1, 1:4, legacy_production_lower_sewing_slot) = &
      [0.0_real64, 1.0_real64, 2.0_real64, 3.0_real64]
  legacy_v(1, 1:4, legacy_production_lower_sewing_slot) = &
      [0.0_real64, 2.0_real64, 4.0_real64, 6.0_real64]
  legacy_u(1, 1:4, legacy_production_higher_sewing_slot) = &
      [10.0_real64, 12.0_real64, 14.0_real64, 16.0_real64]
  legacy_v(1, 1:4, legacy_production_higher_sewing_slot) = &
      [1.0_real64, 5.0_real64, 9.0_real64, 13.0_real64]
  legacy_u(1, 1:4, legacy_production_lower_cut_slot) = &
      [-0.1_real64, 0.9_real64, 1.9_real64, 2.9_real64]
  legacy_v(1, 1:4, legacy_production_lower_cut_slot) = &
      [0.0_real64, 2.0_real64, 4.0_real64, 6.0_real64]
  legacy_u(1, 1:4, legacy_production_higher_cut_slot) = &
      [10.1_real64, 12.1_real64, 14.1_real64, 16.1_real64]
  legacy_v(1, 1:4, legacy_production_higher_cut_slot) = &
      [1.0_real64, 5.0_real64, 9.0_real64, 13.0_real64]
  legacy_u(2, 1:5, legacy_normalized_profile_slot) = &
      [100.0_real64, 50.0_real64, 0.0_real64, 40.0_real64, 100.0_real64]
  legacy_v(2, 1:5, legacy_normalized_profile_slot) = &
      [0.0_real64, 8.0_real64, 0.0_real64, -8.0_real64, 0.0_real64]

  call copy_legacy_production_panel_edges(legacy_u, legacy_v, 1, 4, 3, &
      panel, valid, message)
  call require(valid, 'valid developed panel rejected: '//trim(message))
  call require(panel%is_valid(), 'copied developed panel is invalid')
  call require(panel%lower_rib_index == 1, 'lower panel rib index')
  call require(panel%higher_rib_index == 2, 'higher panel rib index')
  call require(size(panel%lower_sewing_u) == 4, 'lower edge point count')
  call require(size(panel%higher_sewing_u) == 3, 'higher edge point count')
  call require_close(panel%lower_sewing_v(3), 4.0_real64, &
      'lower sewing edge coordinate')
  call require_close(panel%higher_sewing_u(2), 12.0_real64, &
      'higher sewing edge coordinate')
  call require_close(panel%lower_cut_u(1), -0.1_real64, &
      'lower cut edge coordinate')

  ! The composite adapter exposes exactly one integration object per panel.
  legacy_u(1, 2, legacy_normalized_profile_slot) = 50.0_real64
  call copy_legacy_production_panel(legacy_u, legacy_v, profile_topologies, 1, &
      complete_panel, valid, message)
  call require(valid, 'complete developed panel rejected: '//trim(message))
  call require(complete_panel%is_valid(), 'complete panel is invalid')
  call require_close(complete_panel%lower_profile%chord_percent(2), &
      50.0_real64, 'complete panel lower profile')
  call require_close(complete_panel%edges%higher_sewing_v(2), 5.0_real64, &
      'complete panel higher developed edge')
  call require(size(complete_panel%higher_profile%chord_percent) == 5, &
      'higher profile point count')
  call require(size(complete_panel%edges%higher_sewing_u) == 4, &
      'independent higher edge point count')

  ! Panel zero is used for center/virtual-rib construction and is not invalid.
  call copy_legacy_production_panel(legacy_u, legacy_v, profile_topologies, 0, &
      complete_panel, valid, message)
  call require(valid, 'valid panel zero rejected: '//trim(message))
  call require(complete_panel%panel_index == 0, 'panel zero index')
  division%boundary_id = 1
  division%panel_index = 0
  division%lower_chord_percent = 50.0_real64
  division%higher_chord_percent = 50.0_real64
  call require(division%is_valid(), 'valid panel-zero color division rejected')

  legacy_x(2, 1:2) = [20.0_real64, 21.0_real64]
  legacy_y(2, 1:2) = [30.0_real64, 31.0_real64]
  legacy_z(2, 1:2) = [40.0_real64, 41.0_real64]
  call copy_legacy_spatial_rib(legacy_x, legacy_y, legacy_z, 2, 2, &
      spatial_rib, valid, message)
  call require(valid, 'valid spatial rib copy rejected: '//trim(message))
  call require(spatial_rib%is_valid(), 'copied spatial rib is invalid')
  call require_close(spatial_rib%z(2), 41.0_real64, &
      'copied spatial rib coordinate')
  legacy_z(2, 2) = ieee_value(0.0_real64, ieee_quiet_nan)
  call copy_legacy_spatial_rib(legacy_x, legacy_y, legacy_z, 2, 2, &
      spatial_rib, valid, message)
  call require(.not. valid, 'non-finite spatial rib accepted')
  call require(spatial_rib%is_valid(), 'failed copy damaged spatial rib')
  call require_close(spatial_rib%z(2), 41.0_real64, &
      'failed copy changed spatial rib')

  zero_based_spatial_rib%rib_index = 2
  allocate(zero_based_spatial_rib%x(0:3), &
      zero_based_spatial_rib%y(0:3), zero_based_spatial_rib%z(0:3))
  zero_based_spatial_rib%x = 0.0_real64
  zero_based_spatial_rib%y = 0.0_real64
  zero_based_spatial_rib%z = 0.0_real64
  call require(.not. zero_based_spatial_rib%is_valid(), &
      'zero-based spatial coordinates violated the topology-index contract')

  division%boundary_id = 7
  division%panel_index = 1
  division%lower_chord_percent = 48.0_real64
  division%higher_chord_percent = 54.46_real64
  call require(division%is_valid(), 'valid color division rejected')
  division%higher_chord_percent = 101.0_real64
  call require(.not. division%is_valid(), 'out-of-range color accepted')

  ! A failed adapter call is transactional: it preserves the previous object.
  legacy_u(1, 2, legacy_normalized_profile_slot) = &
      ieee_value(0.0_real64, ieee_quiet_nan)
  call copy_legacy_normalized_profile(legacy_u, legacy_v, 1, topology, &
      profile, valid, message)
  call require(.not. valid, 'non-finite normalized profile accepted')
  call require(len_trim(message) > 0, 'adapter failure has no diagnostic')
  call require(profile%is_valid(), 'failed copy damaged existing profile')
  call require_close(profile%chord_percent(2), 50.0_real64, &
      'failed copy changed existing profile')

  legacy_v(1, 2, legacy_production_higher_sewing_slot) = &
      ieee_value(0.0_real64, ieee_quiet_nan)
  call copy_legacy_production_panel_edges(legacy_u, legacy_v, 1, 4, 3, &
      panel, valid, message)
  call require(.not. valid, 'non-finite developed panel accepted')
  call require(panel%is_valid(), 'failed copy damaged existing panel')
  call require_close(panel%higher_sewing_u(2), 12.0_real64, &
      'failed copy changed existing panel')

  call copy_legacy_production_panel_edges(legacy_u, legacy_v, 2, 4, 3, &
      panel, valid, message)
  call require(.not. valid, 'panel without adjacent rib was accepted')

  ! Reconstruct canonical neutral edges from quadrilateral corner arrays.
  neutral_topology%point_count = 6
  neutral_topology%extrados = index_range(1, 3)
  neutral_topology%intake = index_range(3, 4)
  neutral_topology%intrados = index_range(4, 6)
  neutral_topology%leading_edge_index = 3
  call require(neutral_topology%is_valid(), 'neutral test topology is invalid')
  pl1_u(0, 1:2) = [0.0_real64, 3.0_real64]
  pl2_u(0, 1:2) = [3.0_real64, 6.0_real64]
  pr1_u(0, 1:2) = [0.0_real64, 3.0_real64]
  pr2_u(0, 1:2) = [3.0_real64, 6.0_real64]
  pr1_v(0, 1:2) = 4.0_real64
  pr2_v(0, 1:2) = 4.0_real64
  call copy_legacy_neutral_panel(pl1_u, pl1_v, pl2_u, pl2_v, &
      pr1_u, pr1_v, pr2_u, pr2_v, 0, neutral_topology, &
      neutral_topology, surface_extrados, neutral_panel, valid, message)
  call require(valid, 'valid neutral extrados rejected: '//trim(message))
  call require(neutral_panel%is_valid(), 'neutral extrados is invalid')
  zero_based_neutral_panel = neutral_panel
  deallocate(zero_based_neutral_panel%lower_start_biased_u)
  allocate(zero_based_neutral_panel%lower_start_biased_u(0:2))
  zero_based_neutral_panel%lower_start_biased_u = &
      neutral_panel%lower_start_biased_u
  call require(.not. zero_based_neutral_panel%is_valid(), &
      'zero-based neutral coordinates were accepted')
  call require(size(neutral_panel%lower_start_biased_u) == 3, &
      'neutral extrados point count')
  call require_close(polyline_length_2d( &
      neutral_panel%lower_start_biased_u, &
      neutral_panel%lower_start_biased_v), 6.0_real64, &
      'neutral start-biased point-view length')
  call require_close(neutral_panel_lower_edge_length(neutral_panel), &
      6.0_real64, 'exact neutral lower segment length')
  call require_close(neutral_panel_higher_edge_length(neutral_panel), &
      6.0_real64, 'exact neutral higher segment length')
  gap = neutral_panel_edge_gap(neutral_panel, 2, gap_valid)
  call require(gap_valid, 'valid neutral gap index rejected')
  call require_close(gap, 4.0_real64, 'neutral cross-panel gap')

  ! Preserve exact segments when independently flattened joins do not meet.
  pr1_u(0, 2) = 3.25_real64
  call copy_legacy_neutral_panel(pl1_u, pl1_v, pl2_u, pl2_v, &
      pr1_u, pr1_v, pr2_u, pr2_v, 0, neutral_topology, &
      neutral_topology, surface_extrados, neutral_panel, valid, message)
  call require(valid, 'gapped neutral extrados rejected: '//trim(message))
  call require_close(neutral_panel%maximum_higher_join_gap, 0.25_real64, &
      'higher neutral join gap')
  call require_close(neutral_panel_higher_edge_length(neutral_panel), &
      5.75_real64, 'exact gapped higher-edge length')
  call require_close(polyline_length_2d( &
      neutral_panel%higher_start_biased_u, &
      neutral_panel%higher_start_biased_v), 6.0_real64, &
      'documented start-biased point view')
  gap = neutral_panel_edge_gap(neutral_panel, 2, gap_valid)
  call require(gap_valid, 'gapped internal width index rejected')
  call require_close(gap, sqrt(16.0625_real64), &
      'internal width did not use the following segment starts')

  ! A terminal boundary is one physical edge, not a fabricated panel beyond
  ! the final rib.  It retains the final real panel's exact higher segments.
  call derive_neutral_boundary_edge(neutral_panel, boundary_edge, valid, &
      message)
  call require(valid, 'valid terminal boundary rejected: '//trim(message))
  call require(boundary_edge%is_valid(), 'terminal boundary is invalid')
  call require(boundary_edge%source_panel_index == 0, &
      'terminal boundary lost its source panel')
  call require(boundary_edge%boundary_rib_index == 1, &
      'terminal boundary has the wrong physical rib')
  call require(boundary_edge%surface == surface_extrados, &
      'terminal boundary has the wrong surface')
  call require_close(boundary_edge%segment_start_u(2), &
      neutral_panel%higher_segment_start_u(2), &
      'terminal boundary did not copy the higher segment start')
  call require_close(boundary_edge%segment_end_v(2), &
      neutral_panel%higher_segment_end_v(2), &
      'terminal boundary did not copy the higher segment end')
  call require_close(boundary_edge%maximum_join_gap, 0.25_real64, &
      'terminal boundary lost the higher-edge join diagnostic')
  call require_close(neutral_boundary_edge_length(boundary_edge), &
      5.75_real64, 'terminal boundary exact segment length')
  call require(.not. boundary_edge%has_post_surface_support, &
      'extrados boundary invented post-surface support')

  ! Invalid source geometry and invalid provenance are rejected without
  ! changing the previously derived destination.
  extrados_write_panel = neutral_panel
  extrados_write_panel%higher_rib_index = 2
  call derive_neutral_boundary_edge(extrados_write_panel, boundary_edge, &
      valid, message)
  call require(.not. valid, 'invalid source panel produced a boundary')
  call require_close(boundary_edge%maximum_join_gap, 0.25_real64, &
      'failed boundary derivation changed the destination')
  invalid_boundary_edge = boundary_edge
  invalid_boundary_edge%boundary_rib_index = 2
  call require(.not. invalid_boundary_edge%is_valid(), &
      'terminal boundary accepted inconsistent source provenance')
  call require_close(neutral_boundary_edge_length(invalid_boundary_edge), &
      0.0_real64, 'invalid terminal boundary length is not zero')

  ! Publishing a typed extrados panel writes exact segments only after every
  ! destination and panel invariant has passed.
  extrados_write_panel = neutral_panel
  pl1_u(0, 1:3) = -101.0_real64
  pl1_v(0, 1:3) = -102.0_real64
  pl2_u(0, 1:3) = -103.0_real64
  pl2_v(0, 1:3) = -104.0_real64
  pr1_u(0, 1:3) = -105.0_real64
  pr1_v(0, 1:3) = -106.0_real64
  pr2_u(0, 1:3) = -107.0_real64
  pr2_v(0, 1:3) = -108.0_real64
  pl1_u(0, 500) = 5001.0_real64
  pr2_v(1, 1) = 171.0_real64
  call write_legacy_extrados_panel(extrados_write_panel, neutral_topology, &
      neutral_topology, pl1_u, pl1_v, pl2_u, pl2_v, pr1_u, pr1_v, &
      pr2_u, pr2_v, valid, message)
  call require(valid, 'valid typed extrados write rejected: '//trim(message))
  do i = 1, 2
    call require_close(pl1_u(0, i), &
        extrados_write_panel%lower_segment_start_u(i), &
        'typed lower start u was not published')
    call require_close(pl1_v(0, i), &
        extrados_write_panel%lower_segment_start_v(i), &
        'typed lower start v was not published')
    call require_close(pl2_u(0, i), &
        extrados_write_panel%lower_segment_end_u(i), &
        'typed lower end u was not published')
    call require_close(pl2_v(0, i), &
        extrados_write_panel%lower_segment_end_v(i), &
        'typed lower end v was not published')
    call require_close(pr1_u(0, i), &
        extrados_write_panel%higher_segment_start_u(i), &
        'typed higher start u was not published')
    call require_close(pr1_v(0, i), &
        extrados_write_panel%higher_segment_start_v(i), &
        'typed higher start v was not published')
    call require_close(pr2_u(0, i), &
        extrados_write_panel%higher_segment_end_u(i), &
        'typed higher end u was not published')
    call require_close(pr2_v(0, i), &
        extrados_write_panel%higher_segment_end_v(i), &
        'typed higher end v was not published')
  end do
  call require_close(pl1_u(0, 3), -101.0_real64, &
      'write-back changed the post-extrados index')
  call require_close(pl1_u(0, 500), 5001.0_real64, &
      'write-back changed an unrelated high segment index')
  call require_close(pr2_v(1, 1), 171.0_real64, &
      'write-back changed another panel row')

  pl1_u(0, 1) = 213.0_real64
  call write_legacy_extrados_panel(extrados_write_panel, neutral_topology, &
      saved_topology, pl1_u, pl1_v, pl2_u, pl2_v, pr1_u, pr1_v, &
      pr2_u, pr2_v, valid, message)
  call require(.not. valid, 'incompatible write-back topology was accepted')
  call require_close(pl1_u(0, 1), 213.0_real64, &
      'topology failure changed its destination')

  pl1_u(0, 1) = 321.0_real64
  extrados_write_panel%panel_index = 3
  call write_legacy_extrados_panel(extrados_write_panel, neutral_topology, &
      neutral_topology, pl1_u, pl1_v, pl2_u, pl2_v, pr1_u, pr1_v, &
      pr2_u, pr2_v, valid, message)
  call require(.not. valid, 'invalid typed extrados write was accepted')
  call require_close(pl1_u(0, 1), 321.0_real64, &
      'failed write-back changed its destination')
  extrados_write_panel = neutral_panel
  call write_legacy_extrados_panel(extrados_write_panel, neutral_topology, &
      neutral_topology, pl1_u, pl1_v, pl2_u, pl2_v, pr1_u, pr1_v, &
      pr2_u, pr2_v, valid, message)
  call require(valid, 'typed extrados restore failed: '//trim(message))

  test_topology%point_count = 7
  test_topology%extrados = index_range(1, 3)
  test_topology%intake = index_range(3, 5)
  test_topology%intrados = index_range(5, 7)
  test_topology%leading_edge_index = 3
  call copy_legacy_neutral_panel(pl1_u, pl1_v, pl2_u, pl2_v, &
      pr1_u, pr1_v, pr2_u, pr2_v, 0, neutral_topology, &
      test_topology, surface_extrados, neutral_panel, valid, message)
  call require(valid, &
      'matching extrados with different intrados was rejected: '//trim(message))

  ! Neutral copies are transactional and reject non-finite source geometry.
  pr1_u(0, 2) = ieee_value(0.0_real64, ieee_quiet_nan)
  call copy_legacy_neutral_panel(pl1_u, pl1_v, pl2_u, pl2_v, &
      pr1_u, pr1_v, pr2_u, pr2_v, 0, neutral_topology, &
      neutral_topology, surface_extrados, neutral_panel, valid, message)
  call require(.not. valid, 'non-finite neutral segment was accepted')
  call require_close(neutral_panel%maximum_higher_join_gap, 0.25_real64, &
      'failed neutral copy changed the destination')
  pr1_u(0, 2) = 3.0_real64

  mismatched_topology%point_count = 6
  mismatched_topology%extrados = index_range(1, 2)
  mismatched_topology%intake = index_range(2, 4)
  mismatched_topology%intrados = index_range(4, 6)
  mismatched_topology%leading_edge_index = 2
  call require(mismatched_topology%is_valid(), &
      'mismatched test topology is invalid')
  call copy_legacy_neutral_panel(pl1_u, pl1_v, pl2_u, pl2_v, &
      pr1_u, pr1_v, pr2_u, pr2_v, 0, neutral_topology, &
      mismatched_topology, surface_extrados, neutral_panel, valid, message)
  call require(.not. valid, 'mismatched adjacent topology was accepted')
  call require_close(neutral_panel%maximum_higher_join_gap, 0.25_real64, &
      'topology failure changed the neutral destination')

  ! Intake's extra segment is support geometry, not a contour point.
  pl1_u(0, 3:4) = [0.0_real64, 1.0_real64]
  pl2_u(0, 3:4) = [1.0_real64, 2.0_real64]
  pr1_u(0, 3:4) = [0.0_real64, 1.0_real64]
  pr2_u(0, 3:4) = [1.0_real64, 2.0_real64]
  pr1_v(0, 3:4) = 2.0_real64
  pr2_v(0, 3:4) = 2.0_real64
  call copy_legacy_neutral_panel(pl1_u, pl1_v, pl2_u, pl2_v, &
      pr1_u, pr1_v, pr2_u, pr2_v, 0, neutral_topology, &
      neutral_topology, surface_intake, neutral_panel, valid, message)
  call require(valid, 'valid neutral intake rejected: '//trim(message))
  call require(size(neutral_panel%lower_start_biased_u) == 2, &
      'support segment leaked into intake points')
  call require(neutral_panel%has_post_surface_support, &
      'intake support segment was not exposed')
  call require_close(neutral_panel%support_lower_end_u, 2.0_real64, &
      'intake support endpoint')
  call derive_neutral_boundary_edge(neutral_panel, boundary_edge, valid, &
      message)
  call require(valid, 'intake terminal boundary rejected: '//trim(message))
  call require(boundary_edge%surface == surface_intake, &
      'intake boundary has the wrong surface')
  call require(boundary_edge%has_post_surface_support, &
      'intake boundary lost post-surface support')
  call require_close(boundary_edge%support_start_u, &
      neutral_panel%support_higher_start_u, &
      'intake boundary support start did not use the higher edge')
  call require_close(boundary_edge%support_end_v, &
      neutral_panel%support_higher_end_v, &
      'intake boundary support end did not use the higher edge')
  call require_close(boundary_edge%support_join_gap, &
      neutral_panel%support_higher_join_gap, &
      'intake boundary lost its support join diagnostic')
  call require_close(neutral_boundary_edge_length(boundary_edge), &
      1.0_real64, 'intake boundary length included its support segment')

  ! Intake publication owns the exact contour segment and the explicit support
  ! at the following index, while leaving unrelated indices and rows alone.
  pl1_u(0, 3:4) = -201.0_real64
  pl1_v(0, 3:4) = -202.0_real64
  pl2_u(0, 3:4) = -203.0_real64
  pl2_v(0, 3:4) = -204.0_real64
  pr1_u(0, 3:4) = -205.0_real64
  pr1_v(0, 3:4) = -206.0_real64
  pr2_u(0, 3:4) = -207.0_real64
  pr2_v(0, 3:4) = -208.0_real64
  pl1_u(0, 500) = 6500.0_real64
  call write_legacy_intake_panel(neutral_panel, neutral_topology, &
      neutral_topology, pl1_u, pl1_v, pl2_u, pl2_v, pr1_u, pr1_v, &
      pr2_u, pr2_v, valid, message)
  call require(valid, 'valid typed intake write rejected: '//trim(message))
  call require_close(pl1_u(0, 3), &
      neutral_panel%lower_segment_start_u(1), 'typed intake segment start')
  call require_close(pl2_v(0, 3), &
      neutral_panel%lower_segment_end_v(1), 'typed intake segment end')
  call require_close(pr1_u(0, 4), neutral_panel%support_higher_start_u, &
      'typed intake support start')
  call require_close(pr2_v(0, 4), neutral_panel%support_higher_end_v, &
      'typed intake support end')
  call require_close(pl1_u(0, 500), 6500.0_real64, &
      'typed intake write changed an unrelated high segment index')
  pr2_v(1, 4) = 814.0_real64
  call write_legacy_intake_panel(neutral_panel, neutral_topology, &
      neutral_topology, pl1_u, pl1_v, pl2_u, pl2_v, pr1_u, pr1_v, &
      pr2_u, pr2_v, valid, message)
  call require(valid, 'repeated typed intake write failed: '//trim(message))
  call require_close(pr2_v(1, 4), 814.0_real64, &
      'typed intake write changed another panel row')

  disconnected_support_panel = neutral_panel
  disconnected_support_panel%support_lower_start_u = &
      disconnected_support_panel%support_lower_start_u + 1.0_real64
  call require(.not. disconnected_support_panel%is_valid(), &
      'disconnected intake support was accepted without a matching gap')
  pl1_u(0, 3) = 772.0_real64
  call write_legacy_intake_panel(disconnected_support_panel, &
      neutral_topology, neutral_topology, pl1_u, pl1_v, pl2_u, pl2_v, &
      pr1_u, pr1_v, pr2_u, pr2_v, valid, message)
  call require(.not. valid, 'disconnected typed intake write was accepted')
  call require_close(pl1_u(0, 3), 772.0_real64, &
      'disconnected intake failure changed its destination')
  call write_legacy_intake_panel(neutral_panel, neutral_topology, &
      neutral_topology, pl1_u, pl1_v, pl2_u, pl2_v, pr1_u, pr1_v, &
      pr2_u, pr2_v, valid, message)
  call require(valid, 'typed intake rewrite failed: '//trim(message))

  maximum_capacity_topology%point_count = 500
  maximum_capacity_topology%extrados = index_range(1, 498)
  maximum_capacity_topology%intake = index_range(498, 499)
  maximum_capacity_topology%intrados = index_range(499, 500)
  maximum_capacity_topology%leading_edge_index = 498
  call require(maximum_capacity_topology%is_valid(), &
      'maximum-capacity topology is invalid')
  maximum_capacity_intake_panel = neutral_panel
  maximum_capacity_intake_panel%contour_first_index = 498
  maximum_capacity_intake_panel%contour_last_index = 499
  pl1_u(0, 498) = 4981.0_real64
  pl1_u(0, 499) = 4991.0_real64
  call write_legacy_intake_panel(maximum_capacity_intake_panel, &
      maximum_capacity_topology, maximum_capacity_topology, pl1_u, pl1_v, &
      pl2_u, pl2_v, pr1_u, pr1_v, pr2_u, pr2_v, valid, message)
  call require(valid, 'intake support could not use real segment 499: '// &
      trim(message))
  call require_close(pl1_u(0, 498), &
      maximum_capacity_intake_panel%lower_segment_start_u(1), &
      'maximum-capacity intake segment was not published')
  call require_close(pl1_u(0, 499), &
      maximum_capacity_intake_panel%support_lower_start_u, &
      'maximum-capacity intake support was not published at 499')

  ! Once support consumers are finished, typed intrados legitimately replaces
  ! the shared index 499 with the final real contour segment.
  maximum_capacity_intrados_panel = maximum_capacity_intake_panel
  maximum_capacity_intrados_panel%surface = surface_intrados
  maximum_capacity_intrados_panel%contour_first_index = 499
  maximum_capacity_intrados_panel%contour_last_index = 500
  maximum_capacity_intrados_panel%has_post_surface_support = .false.
  maximum_capacity_intrados_panel%support_lower_join_gap = 0.0_real64
  maximum_capacity_intrados_panel%support_higher_join_gap = 0.0_real64
  call require(maximum_capacity_intrados_panel%is_valid(), &
      'maximum-capacity intrados panel is invalid')
  call write_legacy_intrados_panel(maximum_capacity_intrados_panel, &
      maximum_capacity_topology, maximum_capacity_topology, pl1_u, pl1_v, &
      pl2_u, pl2_v, pr1_u, pr1_v, pr2_u, pr2_v, valid, message)
  call require(valid, 'maximum-capacity intrados write failed: '// &
      trim(message))
  call require_close(pl1_u(0, 499), &
      maximum_capacity_intrados_panel%lower_segment_start_u(1), &
      'intrados did not replace support at real segment 499')
  pl1_u(0, 499) = 7499.0_real64
  call write_legacy_intrados_panel(maximum_capacity_intake_panel, &
      maximum_capacity_topology, maximum_capacity_topology, pl1_u, pl1_v, &
      pl2_u, pl2_v, pr1_u, pr1_v, pr2_u, pr2_v, valid, message)
  call require(.not. valid, 'intake panel was accepted as typed intrados')
  call require_close(pl1_u(0, 499), 7499.0_real64, &
      'wrong-surface intrados write changed its destination')

  pl1_u(0, 3) = 771.0_real64
  call write_legacy_intake_panel(extrados_write_panel, neutral_topology, &
      neutral_topology, pl1_u, pl1_v, pl2_u, pl2_v, pr1_u, pr1_v, &
      pr2_u, pr2_v, valid, message)
  call require(.not. valid, 'extrados panel was accepted as typed intake')
  call require_close(pl1_u(0, 3), 771.0_real64, &
      'wrong-surface intake write changed its destination')
  call write_legacy_intake_panel(neutral_panel, neutral_topology, &
      neutral_topology, pl1_u, pl1_v, pl2_u, pl2_v, pr1_u, pr1_v, &
      pr2_u, pr2_v, valid, message)
  call require(valid, 'typed intake restore failed: '//trim(message))

  pr2_v(0, 4) = ieee_value(0.0_real64, ieee_quiet_nan)
  call copy_legacy_neutral_panel(pl1_u, pl1_v, pl2_u, pl2_v, &
      pr1_u, pr1_v, pr2_u, pr2_v, 0, neutral_topology, &
      neutral_topology, surface_intake, neutral_panel, valid, message)
  call require(.not. valid, 'non-finite intake support was accepted')
  call require_close(neutral_panel%support_lower_end_u, 2.0_real64, &
      'failed support copy changed the neutral destination')
  pr2_v(0, 4) = 2.0_real64

  ! Intrados reads and writes its exact contour range directly.
  pl1_u(0, 4) = 0.0_real64
  pl2_u(0, 4) = 2.0_real64
  pr1_u(0, 4) = 0.0_real64
  pr2_u(0, 4) = 2.0_real64
  pr1_v(0, 4) = 3.0_real64
  pr2_v(0, 4) = 3.0_real64
  pl1_u(0, 5) = 2.0_real64
  pl2_u(0, 5) = 4.0_real64
  pr1_u(0, 5) = 2.0_real64
  pr2_u(0, 5) = 4.0_real64
  pr1_v(0, 5) = 3.0_real64
  pr2_v(0, 5) = 3.0_real64
  call copy_legacy_neutral_panel(pl1_u, pl1_v, pl2_u, pl2_v, &
      pr1_u, pr1_v, pr2_u, pr2_v, 0, neutral_topology, &
      neutral_topology, surface_intrados, neutral_panel, valid, message)
  call require(valid, 'valid neutral intrados rejected: '//trim(message))
  call require_close(neutral_panel%lower_start_biased_u(2), 2.0_real64, &
      'intrados segment join was not reconstructed')
  call require_close(neutral_panel%lower_start_biased_u(3), 4.0_real64, &
      'intrados continuation endpoint')
  pl1_u(0, 4) = ieee_value(0.0_real64, ieee_quiet_nan)
  call copy_legacy_neutral_panel(pl1_u, pl1_v, pl2_u, pl2_v, &
      pr1_u, pr1_v, pr2_u, pr2_v, 0, neutral_topology, &
      neutral_topology, surface_intrados, neutral_panel, valid, message)
  call require(.not. valid, 'non-finite intrados segment was accepted')
  call require_close(neutral_panel%lower_start_biased_u(3), 4.0_real64, &
      'failed intrados copy changed the neutral destination')
  pl1_u(0, 4) = 0.0_real64

  pl1_u(0, 4:5) = -401.0_real64
  call write_legacy_intrados_panel(neutral_panel, neutral_topology, &
      neutral_topology, pl1_u, pl1_v, pl2_u, pl2_v, pr1_u, pr1_v, &
      pr2_u, pr2_v, valid, message)
  call require(valid, 'valid typed intrados write rejected: '//trim(message))
  call require_close(pl1_u(0, 4), neutral_panel%lower_segment_start_u(1), &
      'typed intrados first segment was not published')
  call require_close(pl1_u(0, 5), neutral_panel%lower_segment_start_u(2), &
      'typed intrados continuation was not published')

  ! Infer parity-sensitive row-zero roles and the nonphysical tip support.
  planform_station = 0.0_real64
  spatial_station = 0.0_real64
  spatial_height = 0.0_real64
  do i = 1, 20
    planform_station(i) = real(i, real64)
    spatial_station(i) = 2.0_real64 * real(i, real64)
    spatial_height(i) = 0.5_real64 * real(i, real64)
  end do
  planform_station(0) = -planform_station(1)
  spatial_station(0) = -spatial_station(1)
  spatial_station(21) = 2.0_real64 * spatial_station(20) - &
      spatial_station(19)
  spatial_height(21) = 2.0_real64 * spatial_height(20) - &
      spatial_height(19)
  allocate(identities(5:26))
  call infer_legacy_rib_identities(39, 20, planform_station(1), &
      planform_station, spatial_station, spatial_height, identities, &
      panel_zero_active, parity_consistent, valid, message)
  call require(valid, 'odd-cell rib roles rejected: '//trim(message))
  call require(panel_zero_active, 'odd-cell panel zero was not activated')
  call require(parity_consistent, 'odd-cell role parity diagnostic')
  call require(lbound(identities, 1) == 0, &
      'identity adapter did not replace a nonzero lower bound')
  call require(identities(0)%role == rib_role_symmetry_mirror_physical, &
      'odd-cell row-zero role')
  call require(identities(20)%role == rib_role_physical_wingtip, &
      'physical wingtip role')
  call require(identities(21)%is_tip_support(), 'tip support role')
  call require(identities(21)%participates_in_legacy_geometry(), &
      'tip support lost its structural-geometry role')
  call require(.not. identities(21)%is_output_half_rib(), &
      'tip support incorrectly marked for 3D output')

  planform_station = 0.0_real64
  spatial_station = 0.0_real64
  spatial_height = 0.0_real64
  do i = 2, 15
    planform_station(i) = real(i - 1, real64)
    spatial_station(i) = 2.0_real64 * real(i - 1, real64)
    spatial_height(i) = 0.5_real64 * real(i - 1, real64)
  end do
  spatial_station(16) = 2.0_real64 * spatial_station(15) - &
      spatial_station(14)
  spatial_height(16) = 2.0_real64 * spatial_height(15) - &
      spatial_height(14)
  call infer_legacy_rib_identities(28, 15, 0.0_real64, planform_station, &
      spatial_station, spatial_height, identities, panel_zero_active, &
      parity_consistent, valid, message)
  call require(valid, 'even-cell rib roles rejected: '//trim(message))
  call require(.not. panel_zero_active, &
      'even-cell centerline alias activated panel zero')
  call require(parity_consistent, 'even-cell role parity diagnostic')
  call require(identities(0)%role == rib_role_symmetry_centerline_alias, &
      'even-cell row-zero role')
  call require(identities(1)%role == rib_role_physical_centerline, &
      'even-cell first physical role')

  ! A legacy zero-thickness odd central cell remains valid but is diagnosed.
  call infer_legacy_rib_identities(29, 15, 0.005_real64, planform_station, &
      spatial_station, spatial_height, identities, panel_zero_active, &
      parity_consistent, valid, message)
  call require(valid, 'near-zero odd center was rejected: '//trim(message))
  call require(.not. panel_zero_active, &
      'near-zero center ignored the legacy 0.01 threshold')
  call require(.not. parity_consistent, &
      'near-zero odd center lost the parity diagnostic')
  call require(identities(0)%role == rib_role_symmetry_centerline_alias, &
      'near-zero row zero is not a centerline alias')
  call require(identities(0)%participates_in_legacy_geometry(), &
      'centerline alias lost its computational role')
  call require(.not. identities(0)%bounds_active_skin_panel(), &
      'centerline alias incorrectly bounds an active panel')

  ! Promotion to REAL(8) must retain the legacy default-REAL 0.01 boundary.
  call infer_legacy_rib_identities(29, 15, &
      real(real(0.01), real64), planform_station, spatial_station, &
      spatial_height, identities, panel_zero_active, parity_consistent, &
      valid, message)
  call require(valid, 'exact legacy center threshold was rejected')
  call require(panel_zero_active, &
      'REAL(4) center threshold changed during typed promotion')
  call require(parity_consistent, &
      'exact odd-cell threshold lost its parity diagnostic')

  ! Fatal inference checks are transactional: the accepted descriptor table
  ! must survive every rejected source-array or topology candidate unchanged.
  saved_identities = identities
  call infer_legacy_rib_identities(29, 14, 0.005_real64, planform_station, &
      spatial_station, spatial_height, identities, panel_zero_active, &
      parity_consistent, valid, message)
  call require(.not. valid, 'inconsistent physical rib count was accepted')
  call require_same_identities(identities, saved_identities, &
      'rib-count failure changed the identity table')

  planform_station(0) = 1.0_real64
  call infer_legacy_rib_identities(29, 15, 0.005_real64, planform_station, &
      spatial_station, spatial_height, identities, panel_zero_active, &
      parity_consistent, valid, message)
  call require(.not. valid, 'invalid row-zero reflection was accepted')
  call require_same_identities(identities, saved_identities, &
      'reflection failure changed the identity table')
  planform_station(0) = 0.0_real64

  spatial_height(0) = ieee_value(0.0_real64, ieee_quiet_nan)
  call infer_legacy_rib_identities(29, 15, 0.005_real64, planform_station, &
      spatial_station, spatial_height, identities, panel_zero_active, &
      parity_consistent, valid, message)
  call require(.not. valid, 'non-finite spatial height was accepted')
  call require_same_identities(identities, saved_identities, &
      'non-finite failure changed the identity table')
  spatial_height(0) = 0.0_real64

  spatial_station(16) = spatial_station(16) + 1.0_real64
  call infer_legacy_rib_identities(29, 15, 0.005_real64, planform_station, &
      spatial_station, spatial_height, identities, panel_zero_active, &
      parity_consistent, valid, message)
  call require(.not. valid, 'invalid tip extrapolation was accepted')
  call require_same_identities(identities, saved_identities, &
      'tip failure changed the identity table')
  spatial_station(16) = 2.0_real64 * spatial_station(15) - &
      spatial_station(14)

  write (*, '(A)') 'PASS: typed domain model and legacy adapters'

contains

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

    call require(abs(actual - expected) <= 1.0e-10_real64, diagnostic)
  end subroutine require_close

  subroutine require_same_identities(actual, expected, diagnostic)
    type(rib_identity), allocatable, intent(in) :: actual(:), expected(:)
    character(len=*), intent(in) :: diagnostic

    call require(allocated(actual) .and. allocated(expected), diagnostic)
    call require(lbound(actual, 1) == lbound(expected, 1) .and. &
        ubound(actual, 1) == ubound(expected, 1), diagnostic)
    call require(all(actual%legacy_index == expected%legacy_index) .and. &
        all(actual%role == expected%role) .and. &
        all(actual%profile_source_index == expected%profile_source_index) .and. &
        all(actual%placement_anchor_index == &
        expected%placement_anchor_index), diagnostic)
  end subroutine require_same_identities

end program test_domain_model
