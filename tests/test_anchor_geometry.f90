program test_anchor_geometry
  use, intrinsic :: iso_fortran_env, only : real64
  use leparagliding_domain_model, only : index_range, normalized_profile_2d, &
      rib_identity, rib_role_physical_center_adjacent, &
      rib_role_physical_interior, rib_role_symmetry_mirror_physical, &
      rib_role_tip_extrapolated_support
  use leparagliding_spatial_geometry, only : rib_definition
  use leparagliding_anchor_geometry
  implicit none

  integer :: failures

  failures = 0
  call test_definition_adapter(failures)
  call test_definition_adapter_is_transactional(failures)
  call test_generated_definition_provenance(failures)
  call test_legacy_snapshot_preserves_distinct_producers(failures)
  call test_resolved_geometry_and_legacy_comparison(failures)
  call test_spatial_placement_boundary(failures)
  call test_symmetry_resolution(failures)

  if (failures /= 0) then
    write (*, '(A,I0)') 'anchor geometry tests failed: ', failures
    error stop 1
  end if
  write (*, '(A)') 'anchor geometry tests passed'

contains

  subroutine test_definition_adapter(failures)
    integer, intent(inout) :: failures
    real(real64) :: legacy_rib(0:3, 200)
    type(rib_anchor_definition) :: definition
    type(rib_identity) :: identity
    logical :: valid, matches
    character(len=160) :: message

    legacy_rib = 0.0_real64
    identity = physical_identity(1)
    legacy_rib(1, 15) = 1.0_real64
    legacy_rib(1, 16) = 50.0_real64
    legacy_rib(1, 17) = 777.0_real64
    legacy_rib(1, 21) = 95.0_real64

    call copy_legacy_rib_anchor_definition(legacy_rib, identity, definition, &
        valid, message)
    call require(valid, 'valid Section-3 definition adapter: '//trim(message), &
        failures)
    if (.not. valid) return
    call require(definition%authored_anchor_count == 1, &
        'actual legacy anchor count retained', failures)
    call require(size(definition%anchors) == 1, &
        'inactive Section-3 percentages are not retained', failures)
    call require_close(definition%anchors(1)%chord_fraction, 0.5_real64, &
        'active anchor fraction', failures)
    call require_close(definition%brake_chord_fraction, 0.95_real64, &
        'independent brake fraction', failures)
    call rib_anchor_definition_matches_legacy(definition, legacy_rib, &
        matches, message)
    call require(matches, 'definition/legacy comparison: '//trim(message), &
        failures)
  end subroutine test_definition_adapter

  subroutine test_definition_adapter_is_transactional(failures)
    integer, intent(inout) :: failures
    real(real64) :: legacy_rib(0:3, 200)
    type(rib_anchor_definition) :: definition
    type(rib_identity) :: identity
    logical :: valid
    character(len=160) :: message

    legacy_rib = 0.0_real64
    identity = physical_identity(1)
    legacy_rib(1, 15) = 1.0_real64
    legacy_rib(1, 16) = 40.0_real64
    call copy_legacy_rib_anchor_definition(legacy_rib, identity, definition, &
        valid, message)
    call require(valid, 'transactional adapter setup', failures)
    if (.not. valid) return

    legacy_rib(1, 15) = 1.5_real64
    call copy_legacy_rib_anchor_definition(legacy_rib, identity, definition, &
        valid, message)
    call require(.not. valid, 'fractional anchor count rejected', failures)
    call require(index(message, 'integer') > 0, &
        'fractional count diagnostic', failures)
    call require(definition%authored_anchor_count == 1 .and. &
        size(definition%anchors) == 1, &
        'failed definition copy leaves destination unchanged', failures)
    call require_close(definition%anchors(1)%chord_fraction, 0.4_real64, &
        'transactional definition value', failures)
  end subroutine test_definition_adapter_is_transactional

  subroutine test_generated_definition_provenance(failures)
    integer, intent(inout) :: failures
    real(real64) :: legacy_rib(0:5, 200)
    type(rib_anchor_definition) :: source, generated
    type(rib_identity) :: identity
    logical :: valid
    character(len=160) :: message

    legacy_rib = 0.0_real64
    identity%legacy_index = 3
    identity%role = rib_role_physical_interior
    identity%profile_source_index = 3
    identity%placement_anchor_index = 3
    legacy_rib(3, 15) = 2.0_real64
    legacy_rib(3, 16:17) = [30.0_real64, 65.0_real64]
    legacy_rib(3, 21) = 90.0_real64
    call copy_legacy_rib_anchor_definition(legacy_rib, identity, source, &
        valid, message)
    call require(valid, 'tip-source anchor definition', failures)
    if (.not. valid) return

    identity%legacy_index = 5
    identity%role = rib_role_tip_extrapolated_support
    identity%profile_source_index = 3
    identity%placement_anchor_index = 4
    call build_generated_rib_anchor_definition(source, identity, generated, &
        valid, message)
    call require(valid, 'tip-support anchor provenance: '//trim(message), &
        failures)
    if (.not. valid) return
    call require(generated%identity%legacy_index == 5 .and. &
        generated%identity%profile_source_index == 3 .and. &
        generated%identity%placement_anchor_index == 4, &
        'generated identity is retained exactly', failures)
    call require(generated%authored_anchor_count == 2, &
        'generated definition retains actual source count', failures)
  end subroutine test_generated_definition_provenance

  subroutine test_legacy_snapshot_preserves_distinct_producers(failures)
    integer, intent(inout) :: failures
    real(real64) :: legacy_rib(0:3, 200)
    real(real64) :: legacy_u(0:3, 8, 20), legacy_v(0:3, 8, 20)
    real(real64) :: legacy_w(0:3, 8, 20)
    type(rib_anchor_definition) :: definition
    type(resolved_rib_anchors) :: resolved
    type(rib_identity) :: identity
    logical :: valid, matches
    character(len=160) :: message

    legacy_rib = 0.0_real64
    legacy_u = 0.0_real64
    legacy_v = 0.0_real64
    legacy_w = 0.0_real64
    identity = physical_identity(1)
    legacy_rib(1, 15) = 1.0_real64
    legacy_rib(1, 16) = 50.0_real64
    legacy_rib(1, 21) = 95.0_real64
    legacy_rib(1, 66) = 5.0_real64
    ! A terminal/collapsed contour may leave both interpolation producers at
    ! their initialized zero while the requested chord remains nonzero.
    legacy_u(1, 1, 19) = 2.0_real64
    legacy_v(1, 1, 19) = 3.0_real64
    legacy_w(1, 1, 19) = 4.0_real64
    call copy_legacy_rib_anchor_definition(legacy_rib, identity, definition, &
        valid, message)
    call require(valid, 'distinct-producer definition setup', failures)
    if (.not. valid) return
    call copy_legacy_resolved_rib_anchors(definition, legacy_rib, legacy_u, &
        legacy_v, legacy_w, resolved, valid, message)
    call require(valid, 'checked legacy resolved snapshot: '//trim(message), &
        failures)
    if (.not. valid) return
    call require_close(resolved%anchors(1)%requested_chordwise_cm, &
        5.0_real64, 'snapshot requested chord', failures)
    call require_close(resolved%anchors(1)%profile_point%chordwise_cm, &
        0.0_real64, 'snapshot unresolved Stage-9 local chord', failures)
    call require_close(resolved%anchors(1)%intrados_mark_point%chordwise_cm, &
        0.0_real64, 'snapshot unresolved Stage-6 mark chord', failures)
    call resolved_rib_anchors_match_legacy(resolved, definition, legacy_rib, &
        legacy_u, legacy_v, legacy_w, matches, message)
    call require(matches, 'distinct legacy producers compare independently', &
        failures)
  end subroutine test_legacy_snapshot_preserves_distinct_producers

  subroutine test_resolved_geometry_and_legacy_comparison(failures)
    integer, intent(inout) :: failures
    real(real64) :: legacy_rib(0:3, 200)
    real(real64) :: legacy_u(0:3, 8, 20), legacy_v(0:3, 8, 20)
    real(real64) :: legacy_w(0:3, 8, 20)
    type(normalized_profile_2d) :: profile
    type(rib_definition) :: placement
    type(rib_anchor_definition) :: definition
    type(resolved_rib_anchors) :: resolved
    type(rib_identity) :: identity
    real(real64) :: expected_distance
    logical :: valid, matches
    character(len=160) :: message

    identity = physical_identity(1)
    profile = sample_profile(1)
    placement = sample_placement(identity)
    legacy_rib = 0.0_real64
    legacy_u = 0.0_real64
    legacy_v = 0.0_real64
    legacy_w = 0.0_real64
    legacy_rib(1, 15) = 1.0_real64
    legacy_rib(1, 16) = 50.0_real64
    legacy_rib(1, 21) = 95.0_real64
    call copy_legacy_rib_anchor_definition(legacy_rib, identity, definition, &
        valid, message)
    call require(valid, 'resolved definition setup', failures)
    if (.not. valid) return

    call resolve_rib_anchors(profile, placement, definition, resolved, valid, &
        message)
    call require(valid, 'typed anchor resolution: '//trim(message), failures)
    if (.not. valid) return
    expected_distance = hypot(5.0_real64, 0.5_real64)
    call require_close(resolved%anchors(1)%profile_point%chordwise_cm, &
        5.0_real64, 'resolved local chord', failures)
    call require_close(resolved%anchors(1)%profile_point%height_cm, &
        0.5_real64, 'resolved local height', failures)
    call require_close( &
        resolved%anchors(1)%trailing_edge_intrados_distance_cm, &
        expected_distance, 'resolved TE distance', failures)
    call require_close(resolved%anchors(1)%spatial_point%x_cm, 2.0_real64, &
        'resolved spatial X', failures)
    call require_close(resolved%anchors(1)%spatial_point%y_cm, 8.0_real64, &
        'resolved spatial Y', failures)
    call require_close(resolved%anchors(1)%spatial_point%z_cm, 3.7_real64, &
        'resolved spatial Z with explicit displacement', failures)

    legacy_rib(1, 66) = 5.0_real64
    legacy_rib(1, 111) = 5.0_real64
    legacy_rib(1, 121) = 0.5_real64
    legacy_rib(1, 131) = expected_distance
    legacy_u(1, 1, 6) = 5.0_real64
    legacy_v(1, 1, 6) = 0.5_real64
    legacy_u(1, 1, 19) = 2.0_real64
    legacy_v(1, 1, 19) = 8.0_real64
    legacy_w(1, 1, 19) = 3.7_real64
    call resolved_rib_anchors_match_legacy(resolved, definition, legacy_rib, &
        legacy_u, legacy_v, legacy_w, matches, message)
    call require(matches, 'complete resolved legacy comparison: '// &
        trim(message), failures)

    legacy_rib(1, 131) = expected_distance + 1.0_real64
    call resolved_rib_anchors_match_legacy(resolved, definition, legacy_rib, &
        legacy_u, legacy_v, legacy_w, matches, message)
    call require(.not. matches .and. index(message, 'ordinal 1') > 0, &
        'TE-distance drift is rejected with anchor ordinal', failures)
  end subroutine test_resolved_geometry_and_legacy_comparison

  subroutine test_spatial_placement_boundary(failures)
    integer, intent(inout) :: failures
    real(real64) :: legacy_rib(0:3, 200)
    real(real64) :: legacy_u(0:3, 8, 20), legacy_v(0:3, 8, 20)
    real(real64) :: legacy_w(0:3, 8, 20)
    type(normalized_profile_2d) :: profile
    type(rib_definition) :: placement, rotated_placement, wrong_placement
    type(rib_anchor_definition) :: definition
    type(resolved_rib_anchors) :: source, placed, rotated
    type(rib_identity) :: identity
    real(real64) :: retained_x
    logical :: valid
    character(len=160) :: message

    identity = physical_identity(1)
    profile = sample_profile(1)
    placement = sample_placement(identity)
    legacy_rib = 0.0_real64
    legacy_rib(1, 15) = 1.0_real64
    legacy_rib(1, 16) = 50.0_real64
    legacy_rib(1, 21) = 95.0_real64
    call copy_legacy_rib_anchor_definition(legacy_rib, identity, definition, &
        valid, message)
    call require(valid, 'spatial-boundary definition setup', failures)
    if (.not. valid) return
    call resolve_rib_anchors(profile, placement, definition, source, valid, &
        message)
    call require(valid, 'spatial-boundary source setup', failures)
    if (.not. valid) return

    ! Prove placement does not trust the incoming legacy snapshot coordinates.
    source%anchors(1)%spatial_point%x_cm = 90.0_real64
    source%anchors(1)%spatial_point%y_cm = 91.0_real64
    source%anchors(1)%spatial_point%z_cm = 92.0_real64
    call place_resolved_rib_anchors(source, placement, placed, valid, message)
    call require(valid, 'typed Stage-12 placement: '//trim(message), failures)
    if (.not. valid) return
    call require_close(placed%anchors(1)%spatial_point%x_cm, 2.0_real64, &
        'typed Stage-12 X', failures)
    call require_close(placed%anchors(1)%spatial_point%y_cm, 8.0_real64, &
        'typed Stage-12 Y', failures)
    call require_close(placed%anchors(1)%spatial_point%z_cm, 3.7_real64, &
        'typed Stage-12 Z', failures)
    call require_close(source%anchors(1)%spatial_point%x_cm, 90.0_real64, &
        'typed placement does not mutate its source', failures)

    legacy_u = -7.0_real64
    legacy_v = -7.0_real64
    legacy_w = -7.0_real64
    call write_legacy_resolved_anchor_spatial_points(placed, legacy_u, &
        legacy_v, legacy_w, valid, message)
    call require(valid, 'narrow Stage-12 compatibility writer: '// &
        trim(message), failures)
    call require_close(legacy_u(1, 1, 19), 2.0_real64, &
        'writer publishes active X', failures)
    call require_close(legacy_v(1, 1, 19), 8.0_real64, &
        'writer publishes active Y', failures)
    call require_close(legacy_w(1, 1, 19), 3.7_real64, &
        'writer publishes active Z', failures)
    call require_close(legacy_u(1, 1, 18), -7.0_real64, &
        'writer preserves Stage-12 intermediate slots', failures)
    call require_close(legacy_u(1, 2, 19), -7.0_real64, &
        'writer preserves inactive anchor ordinals', failures)

    rotated_placement = placement
    rotated_placement%washin_angle_rad = 0.2_real64
    call place_resolved_rib_anchors(source, rotated_placement, rotated, &
        valid, message)
    call require(valid, 'nonzero-washin Stage-12 placement', failures)
    if (valid) then
      call require_close(rotated%anchors(1)%spatial_point%y_cm, &
          3.0_real64 + 5.0_real64 * cos(0.2_real64) + &
          0.5_real64 * sin(0.2_real64), &
          'Stage-12 displacement does not move rotated chord', failures)
      call require_close(rotated%anchors(1)%spatial_point%z_cm, &
          4.0_real64 + 5.0_real64 * sin(0.2_real64) - &
          0.5_real64 * cos(0.2_real64) + 0.2_real64, &
          'Stage-12 displacement follows wash-in rotation', failures)
    end if

    wrong_placement = placement
    wrong_placement%identity = physical_identity(2)
    wrong_placement%source_profile_number = 2
    retained_x = placed%anchors(1)%spatial_point%x_cm
    call place_resolved_rib_anchors(source, wrong_placement, placed, valid, &
        message)
    call require(.not. valid .and. index(message, 'provenance') > 0, &
        'placement rejects mismatched provenance', failures)
    call require_close(placed%anchors(1)%spatial_point%x_cm, retained_x, &
        'failed placement leaves destination unchanged', failures)
  end subroutine test_spatial_placement_boundary

  subroutine test_symmetry_resolution(failures)
    integer, intent(inout) :: failures
    real(real64) :: legacy_rib(0:3, 200)
    real(real64) :: legacy_u(0:3, 8, 20), legacy_v(0:3, 8, 20)
    real(real64) :: legacy_w(0:3, 8, 20)
    type(normalized_profile_2d) :: profile
    type(rib_definition) :: placement
    type(rib_anchor_definition) :: source_definition, target_definition
    type(resolved_rib_anchors) :: source, mirrored
    type(rib_identity) :: source_identity, target_identity
    real(real64) :: expected_distance
    logical :: valid, matches
    character(len=160) :: message

    source_identity = physical_identity(1)
    target_identity%legacy_index = 0
    target_identity%role = rib_role_symmetry_mirror_physical
    target_identity%profile_source_index = 1
    target_identity%placement_anchor_index = 1
    profile = sample_profile(1)
    placement = sample_placement(source_identity)
    legacy_rib = 0.0_real64
    legacy_u = 0.0_real64
    legacy_v = 0.0_real64
    legacy_w = 0.0_real64
    legacy_rib(1, 15) = 1.0_real64
    legacy_rib(1, 16) = 50.0_real64
    legacy_rib(1, 21) = 95.0_real64
    call copy_legacy_rib_anchor_definition(legacy_rib, source_identity, &
        source_definition, valid, message)
    call require(valid, 'symmetry source definition', failures)
    if (.not. valid) return
    call build_generated_rib_anchor_definition(source_definition, &
        target_identity, target_definition, valid, message)
    call require(valid, 'symmetry target definition', failures)
    if (.not. valid) return
    call resolve_rib_anchors(profile, placement, source_definition, source, &
        valid, message)
    call require(valid, 'symmetry source resolution', failures)
    if (.not. valid) return
    call build_symmetry_resolved_anchors(source, target_definition, mirrored, &
        valid, message)
    call require(valid, 'symmetry resolved constructor: '//trim(message), &
        failures)
    if (.not. valid) return
    call require_close(mirrored%anchors(1)%spatial_point%x_cm, &
        -source%anchors(1)%spatial_point%x_cm, 'mirrored spatial X', failures)
    call require_close(mirrored%anchors(1)%spatial_point%y_cm, &
        source%anchors(1)%spatial_point%y_cm, 'preserved spatial Y', failures)
    call require_close(mirrored%anchors(1)%profile_point%height_cm, &
        source%anchors(1)%profile_point%height_cm, &
        'preserved mirrored local point', failures)

    expected_distance = hypot(5.0_real64, 0.5_real64)
    legacy_rib(0, 15) = 1.0_real64
    legacy_rib(0, 16) = 50.0_real64
    legacy_rib(0, 21) = 95.0_real64
    legacy_rib(0, 66) = 5.0_real64
    legacy_rib(0, 111) = 5.0_real64
    legacy_rib(0, 121) = 0.5_real64
    legacy_rib(0, 131) = expected_distance
    legacy_u(0, 1, 6) = 5.0_real64
    legacy_v(0, 1, 6) = 0.5_real64
    legacy_u(0, 1, 19) = -2.0_real64
    legacy_v(0, 1, 19) = 8.0_real64
    legacy_w(0, 1, 19) = 3.7_real64
    call resolved_rib_anchors_match_legacy(mirrored, target_definition, &
        legacy_rib, legacy_u, legacy_v, legacy_w, matches, message)
    call require(matches, 'mirrored legacy comparison: '//trim(message), &
        failures)
  end subroutine test_symmetry_resolution

  function physical_identity(rib_index) result(identity)
    integer, intent(in) :: rib_index
    type(rib_identity) :: identity

    identity%legacy_index = rib_index
    identity%profile_source_index = rib_index
    identity%placement_anchor_index = rib_index
    if (rib_index == 1) then
      identity%role = rib_role_physical_center_adjacent
    else
      identity%role = rib_role_physical_interior
    end if
  end function physical_identity

  function sample_profile(rib_index) result(profile)
    integer, intent(in) :: rib_index
    type(normalized_profile_2d) :: profile

    profile%rib_index = rib_index
    profile%topology%point_count = 6
    profile%topology%extrados = index_range(1, 3)
    profile%topology%intake = index_range(3, 4)
    profile%topology%intrados = index_range(4, 6)
    profile%topology%leading_edge_index = 3
    profile%chord_fraction = [1.0_real64, 0.5_real64, 0.0_real64, &
        0.0_real64, 0.5_real64, 1.0_real64]
    profile%height_fraction = [0.0_real64, 0.05_real64, 0.1_real64, &
        0.1_real64, 0.05_real64, 0.0_real64]
  end function sample_profile

  function sample_placement(identity) result(definition)
    type(rib_identity), intent(in) :: identity
    type(rib_definition) :: definition

    definition%identity = identity
    definition%source_profile_number = identity%profile_source_index
    definition%leading_edge_position_cm = 3.0_real64
    definition%trailing_edge_position_cm = 13.0_real64
    definition%chord_length_cm = 10.0_real64
    definition%spatial_station_cm = 2.0_real64
    definition%spatial_height_cm = 4.0_real64
    definition%profile_vertical_displacement_cm = 0.2_real64
    definition%profile_height_scale = 1.0_real64
  end function sample_placement

  subroutine require(condition, label, failures)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: label
    integer, intent(inout) :: failures

    if (.not. condition) then
      failures = failures + 1
      write (*, '(2A)') 'FAIL: ', trim(label)
    end if
  end subroutine require

  subroutine require_close(actual, expected, label, failures)
    real(real64), intent(in) :: actual, expected
    character(len=*), intent(in) :: label
    integer, intent(inout) :: failures
    real(real64), parameter :: tolerance = 1.0e-11_real64

    call require(abs(actual - expected) <= tolerance, label, failures)
  end subroutine require_close

end program test_anchor_geometry
