program test_panel_shaping
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_quiet_nan, ieee_value
  use leparagliding_domain_model, only : profile_topology, neutral_panel_2d, &
      neutral_boundary_edge_2d, production_boundary_edge_2d, &
      derive_neutral_boundary_edge, surface_extrados, surface_intrados, &
      legacy_production_lower_sewing_slot, &
      legacy_production_higher_sewing_slot, &
      legacy_production_lower_cut_slot, legacy_production_higher_cut_slot
  use leparagliding_panel_shaping
  implicit none

  type(neutral_panel_2d) :: panel, bad_panel, empty_panel
  type(profile_topology) :: topology
  type(neutral_boundary_edge_2d) :: neutral_boundary
  type(shaped_panel_side_2d) :: shaped, saved
  type(shaped_panel_side_2d) :: extrados_lower, extrados_higher
  type(shaped_panel_side_2d) :: intrados_higher
  type(shaped_panel_side_2d) :: mismatched_extrados_side
  type(production_boundary_edge_2d) :: production_boundary
  type(production_boundary_edge_2d) :: saved_production_boundary
  character(len=160) :: message
  logical :: valid
  integer :: quadrant
  real(real64) :: nan_value, allowance_model
  real(real64) :: legacy_u(0:3, 6, 13), legacy_v(0:3, 6, 13)
  real(real64) :: expected_u(0:3, 6, 13), expected_v(0:3, 6, 13)
  real(real64), allocatable :: empty_offsets(:)
  real(real64), parameter :: delta_u(4) = &
      [3.0_real64, -3.0_real64, -3.0_real64, 3.0_real64]
  real(real64), parameter :: delta_v(4) = &
      [4.0_real64, 4.0_real64, -4.0_real64, -4.0_real64]
  real(real64), parameter :: lower_normal_u(4) = &
      [-0.8_real64, -0.8_real64, 0.8_real64, 0.8_real64]
  real(real64), parameter :: lower_normal_v(4) = &
      [0.6_real64, -0.6_real64, -0.6_real64, 0.6_real64]
  real(real64), parameter :: varied_offsets(2) = &
      [1.25_real64, -0.5_real64]

  nan_value = ieee_value(0.0_real64, ieee_quiet_nan)

  ! All four non-axis-aligned quadrants lock the legacy sign table.  The
  ! higher-index side must use the exact opposite normal, and point offsets
  ! are deliberately unequal to prove that shaping is point-local.
  do quadrant = 1, 4
    call initialize_one_segment_panel(panel, delta_u(quadrant), &
        delta_v(quadrant))
    call shape_neutral_panel_side(panel, panel_side_lower, varied_offsets, &
        0.0_real64, shaped, valid, message)
    call require(valid, 'lower quadrant rejected: '//trim(message))
    call require(shaped%is_valid(), 'lower quadrant result is invalid')
    call require(shaped%panel_index == panel%panel_index .and. &
        shaped%surface == panel%surface .and. &
        shaped%side == panel_side_lower, 'lower result identity')
    call require_close(shaped%sewing_u(1), &
        10.0_real64 + varied_offsets(1)*lower_normal_u(quadrant), &
        'lower quadrant initial U')
    call require_close(shaped%sewing_v(1), &
        20.0_real64 + varied_offsets(1)*lower_normal_v(quadrant), &
        'lower quadrant initial V')
    call require_close(shaped%sewing_u(2), &
        10.0_real64 + delta_u(quadrant) + &
        varied_offsets(2)*lower_normal_u(quadrant), &
        'lower quadrant endpoint U')
    call require_close(shaped%sewing_v(2), &
        20.0_real64 + delta_v(quadrant) + &
        varied_offsets(2)*lower_normal_v(quadrant), &
        'lower quadrant endpoint V')

    call shape_neutral_panel_side(panel, panel_side_higher, varied_offsets, &
        0.0_real64, shaped, valid, message)
    call require(valid, 'higher quadrant rejected: '//trim(message))
    call require(shaped%side == panel_side_higher, 'higher result identity')
    call require_close(shaped%sewing_u(1), &
        110.0_real64 - varied_offsets(1)*lower_normal_u(quadrant), &
        'higher quadrant initial U')
    call require_close(shaped%sewing_v(1), &
        120.0_real64 - varied_offsets(1)*lower_normal_v(quadrant), &
        'higher quadrant initial V')
    call require_close(shaped%sewing_u(2), &
        110.0_real64 + delta_u(quadrant) - &
        varied_offsets(2)*lower_normal_u(quadrant), &
        'higher quadrant endpoint U')
    call require_close(shaped%sewing_v(2), &
        120.0_real64 + delta_v(quadrant) - &
        varied_offsets(2)*lower_normal_v(quadrant), &
        'higher quadrant endpoint V')
  end do

  ! A positive vertical segment matches two legacy sign branches; the later
  ! branch wins.  Division by zero is avoided without changing the PI/2
  ! normal that the old floating-point expression produced.
  call initialize_one_segment_panel(panel, 0.0_real64, 5.0_real64)
  call shape_neutral_panel_side(panel, panel_side_lower, &
      [2.0_real64, 3.0_real64], 0.0_real64, shaped, valid, message)
  call require(valid, 'vertical lower edge rejected: '//trim(message))
  call require_close(shaped%sewing_u(1), 8.0_real64, &
      'vertical initial U')
  call require_close(shaped%sewing_v(1), 20.0_real64, &
      'vertical initial V')
  call require_close(shaped%sewing_u(2), 7.0_real64, &
      'vertical endpoint U')
  call require_close(shaped%sewing_v(2), 25.0_real64, &
      'vertical endpoint V')

  ! `puntslat` treated the first point of a horizontal segment differently
  ! from its endpoint: initial angle zero versus endpoint angle PI/2.
  call initialize_one_segment_panel(panel, 5.0_real64, 0.0_real64)
  call shape_neutral_panel_side(panel, panel_side_lower, &
      [2.0_real64, 3.0_real64], 0.0_real64, shaped, valid, message)
  call require(valid, 'horizontal lower edge rejected: '//trim(message))
  call require_close(shaped%sewing_u(1), 10.0_real64, &
      'horizontal legacy initial U')
  call require_close(shaped%sewing_v(1), 22.0_real64, &
      'horizontal legacy initial V')
  call require_close(shaped%sewing_u(2), 18.0_real64, &
      'horizontal legacy endpoint U')
  call require_close(shaped%sewing_v(2), 20.0_real64, &
      'horizontal legacy endpoint V')
  call shape_neutral_panel_side(panel, panel_side_higher, &
      [2.0_real64, 3.0_real64], 0.0_real64, shaped, valid, message)
  call require(valid, 'horizontal higher edge rejected: '//trim(message))
  call require_close(shaped%sewing_u(1), 110.0_real64, &
      'horizontal higher initial U')
  call require_close(shaped%sewing_v(1), 118.0_real64, &
      'horizontal higher initial V')
  call require_close(shaped%sewing_u(2), 112.0_real64, &
      'horizontal higher endpoint U')
  call require_close(shaped%sewing_v(2), 120.0_real64, &
      'horizontal higher endpoint V')

  ! Sewing allowance is specified in millimetres.  Expected values use the
  ! promoted default-REAL 0.1 representation retained from the old source.
  call initialize_one_segment_panel(panel, 3.0_real64, 4.0_real64)
  call shape_neutral_panel_side(panel, panel_side_lower, &
      [0.0_real64, 0.0_real64], 12.0_real64, shaped, valid, message)
  call require(valid, 'allowance case rejected: '//trim(message))
  allowance_model = 12.0_real64*real(0.1, real64)
  call require_close_strict(shaped%cut_u(1) - shaped%sewing_u(1), &
      -0.8_real64*allowance_model, 'legacy allowance conversion U')
  call require_close_strict(shaped%cut_v(1) - shaped%sewing_v(1), &
      0.6_real64*allowance_model, 'legacy allowance conversion V')
  call require_close_strict(shaped%cut_u(2) - shaped%sewing_u(2), &
      -0.8_real64*allowance_model, 'endpoint allowance conversion U')

  ! A complete extrados panel owns two independently shaped production sides.
  ! This locks the surface and side identities plus the opposite cut normals
  ! used by the Stage-8 slot-9/11 and slot-10/12 transactions.
  panel%surface = surface_extrados
  panel%contour_first_index = 1
  panel%contour_last_index = 2
  topology%point_count = 4
  topology%extrados%first = 1
  topology%extrados%last = 2
  topology%intake%first = 2
  topology%intake%last = 3
  topology%intrados%first = 3
  topology%intrados%last = 4
  topology%leading_edge_index = 2
  call require(topology%is_valid(), 'invalid shaped-side adapter topology')
  call shape_neutral_panel_side(panel, panel_side_lower, &
      [0.25_real64, 0.75_real64], 10.0_real64, extrados_lower, &
      valid, message)
  call require(valid, 'extrados lower side rejected: '//trim(message))
  call shape_neutral_panel_side(panel, panel_side_higher, &
      [0.25_real64, 0.75_real64], 10.0_real64, extrados_higher, &
      valid, message)
  call require(valid, 'extrados higher side rejected: '//trim(message))
  call require(extrados_lower%surface == surface_extrados .and. &
      extrados_lower%side == panel_side_lower, &
      'extrados lower identity')
  call require(extrados_higher%surface == surface_extrados .and. &
      extrados_higher%side == panel_side_higher, &
      'extrados higher identity')
  call require_close(extrados_lower%sewing_u(1), 9.8_real64, &
      'extrados lower sewing U')
  call require_close(extrados_higher%sewing_u(1), 110.2_real64, &
      'extrados higher sewing U')
  call require_close_strict(extrados_lower%cut_u(1)- &
      extrados_lower%sewing_u(1), -0.8_real64*real(0.1, real64)* &
      10.0_real64, 'extrados lower cut normal')
  call require_close_strict(extrados_higher%cut_u(1)- &
      extrados_higher%sewing_u(1), 0.8_real64*real(0.1, real64)* &
      10.0_real64, 'extrados higher cut normal')

  ! Sentinels prove that each adapter call owns exactly one panel row, the
  ! extrados range, and its side-selected sewing/cut slot pair.
  legacy_u = -777.0_real64
  legacy_v = 888.0_real64
  expected_u = legacy_u
  expected_v = legacy_v
  expected_u(2, 1:2, legacy_production_lower_sewing_slot) = &
      extrados_lower%sewing_u
  expected_v(2, 1:2, legacy_production_lower_sewing_slot) = &
      extrados_lower%sewing_v
  expected_u(2, 1:2, legacy_production_lower_cut_slot) = &
      extrados_lower%cut_u
  expected_v(2, 1:2, legacy_production_lower_cut_slot) = &
      extrados_lower%cut_v
  call write_legacy_shaped_panel_side(extrados_lower, topology, legacy_u, &
      legacy_v, valid, message)
  call require(valid, 'lower extrados write rejected: '//trim(message))
  call require_legacy_equal(legacy_u, legacy_v, expected_u, expected_v, &
      'lower extrados exact-range write')

  legacy_u = -777.0_real64
  legacy_v = 888.0_real64
  expected_u = legacy_u
  expected_v = legacy_v
  expected_u(2, 1:2, legacy_production_higher_sewing_slot) = &
      extrados_higher%sewing_u
  expected_v(2, 1:2, legacy_production_higher_sewing_slot) = &
      extrados_higher%sewing_v
  expected_u(2, 1:2, legacy_production_higher_cut_slot) = &
      extrados_higher%cut_u
  expected_v(2, 1:2, legacy_production_higher_cut_slot) = &
      extrados_higher%cut_v
  call write_legacy_shaped_panel_side(extrados_higher, topology, legacy_u, &
      legacy_v, valid, message)
  call require(valid, 'higher extrados write rejected: '//trim(message))
  call require_legacy_equal(legacy_u, legacy_v, expected_u, expected_v, &
      'higher extrados exact-range write')

  mismatched_extrados_side = extrados_higher
  mismatched_extrados_side%contour_first_index = 2
  mismatched_extrados_side%contour_last_index = 3
  legacy_u = -777.0_real64
  legacy_v = 888.0_real64
  expected_u = legacy_u
  expected_v = legacy_v
  call write_legacy_shaped_panel_side(mismatched_extrados_side, topology, &
      legacy_u, legacy_v, valid, message)
  call require(.not. valid, 'mismatched extrados range was written')
  call require_legacy_equal(legacy_u, legacy_v, expected_u, expected_v, &
      'rejected extrados write transaction')

  ! The same writer selects the topology's exact intrados range while retaining
  ! the higher-side slot pair used by the new regular-panel checkpoint.
  panel%surface = surface_intrados
  panel%contour_first_index = 3
  panel%contour_last_index = 4
  call shape_neutral_panel_side(panel, panel_side_higher, &
      [0.5_real64, 1.0_real64], 10.0_real64, intrados_higher, &
      valid, message)
  call require(valid, 'intrados higher side rejected: '//trim(message))
  legacy_u = -777.0_real64
  legacy_v = 888.0_real64
  expected_u = legacy_u
  expected_v = legacy_v
  expected_u(2, 3:4, legacy_production_higher_sewing_slot) = &
      intrados_higher%sewing_u
  expected_v(2, 3:4, legacy_production_higher_sewing_slot) = &
      intrados_higher%sewing_v
  expected_u(2, 3:4, legacy_production_higher_cut_slot) = &
      intrados_higher%cut_u
  expected_v(2, 3:4, legacy_production_higher_cut_slot) = &
      intrados_higher%cut_v
  call write_legacy_shaped_panel_side(intrados_higher, topology, legacy_u, &
      legacy_v, valid, message)
  call require(valid, 'higher intrados write rejected: '//trim(message))
  call require_legacy_equal(legacy_u, legacy_v, expected_u, expected_v, &
      'higher intrados exact-range write')

  ! At a neutral-development join, the next segment's start may not equal the
  ! incoming segment's end.  Point two must retain the incoming endpoint bias.
  call initialize_two_segment_gap_panel(panel)
  call shape_neutral_panel_side(panel, panel_side_lower, &
      [0.0_real64, 2.0_real64, 0.0_real64], 0.0_real64, shaped, valid, message)
  call require(valid, 'join-gap case rejected: '//trim(message))
  call require_close(shaped%sewing_u(2), 1.4_real64, &
      'incoming-segment endpoint bias U')
  call require_close(shaped%sewing_v(2), 5.2_real64, &
      'incoming-segment endpoint bias V')
  call require(abs(shaped%sewing_u(2) - &
      panel%lower_segment_start_u(2)) > 20.0_real64, &
      'join point incorrectly used the following segment start')

  ! A physical terminal boundary comes from the final panel's higher neutral
  ! edge but deliberately uses the lower/outward normal.  It is not a panel
  ! and owns no higher-side sewing or cut coordinates.
  call derive_neutral_boundary_edge(panel, neutral_boundary, valid, message)
  call require(valid, 'terminal neutral edge rejected: '//trim(message))
  call shape_neutral_boundary_edge(neutral_boundary, &
      [0.0_real64, 2.0_real64, 0.0_real64], 0.0_real64, &
      production_boundary, valid, message)
  call require(valid, 'terminal production edge rejected: '//trim(message))
  call require(production_boundary%is_valid(), &
      'terminal production result is invalid')
  call require(production_boundary%source_panel_index == panel%panel_index, &
      'terminal source-panel identity')
  call require(production_boundary%boundary_rib_index == &
      panel%higher_rib_index, 'terminal boundary-rib identity')
  call require_close(production_boundary%sewing_u(2), 101.4_real64, &
      'terminal incoming-segment endpoint bias U')
  call require_close(production_boundary%sewing_v(2), 105.2_real64, &
      'terminal incoming-segment endpoint bias V')
  saved_production_boundary = production_boundary
  call shape_neutral_boundary_edge(neutral_boundary, [0.0_real64], &
      0.0_real64, production_boundary, valid, message)
  call require(.not. valid, 'terminal offset-count mismatch was accepted')
  call require(all(abs(production_boundary%sewing_u - &
      saved_production_boundary%sewing_u) <= 0.0_real64) .and. &
      all(abs(production_boundary%sewing_v - &
      saved_production_boundary%sewing_v) <= 0.0_real64) .and. &
      all(abs(production_boundary%cut_u - &
      saved_production_boundary%cut_u) <= 0.0_real64) .and. &
      all(abs(production_boundary%cut_v - &
      saved_production_boundary%cut_v) <= 0.0_real64), &
      'failed terminal shaping changed its destination')

  ! Every failure below must retain this complete successful value.
  saved = shaped
  call shape_neutral_panel_side(panel, 99, &
      [0.0_real64, 0.0_real64, 0.0_real64], 0.0_real64, &
      shaped, valid, message)
  call require_rejected_unchanged(valid, message, shaped, saved, &
      'invalid side')

  call shape_neutral_panel_side(panel, panel_side_lower, [0.0_real64], &
      0.0_real64, shaped, valid, message)
  call require_rejected_unchanged(valid, message, shaped, saved, &
      'invalid offset shape')

  bad_panel = panel
  deallocate(bad_panel%lower_segment_end_v)
  call shape_neutral_panel_side(bad_panel, panel_side_lower, &
      [0.0_real64, 0.0_real64, 0.0_real64], 0.0_real64, &
      shaped, valid, message)
  call require_rejected_unchanged(valid, message, shaped, saved, &
      'invalid panel shape')

  call shape_neutral_panel_side(panel, panel_side_lower, &
      [0.0_real64, nan_value, 0.0_real64], 0.0_real64, &
      shaped, valid, message)
  call require_rejected_unchanged(valid, message, shaped, saved, &
      'non-finite offset')
  call shape_neutral_panel_side(panel, panel_side_lower, &
      [0.0_real64, 0.0_real64, 0.0_real64], nan_value, &
      shaped, valid, message)
  call require_rejected_unchanged(valid, message, shaped, saved, &
      'non-finite allowance')

  bad_panel = panel
  bad_panel%lower_segment_start_u(1) = nan_value
  bad_panel%lower_start_biased_u(1) = nan_value
  call shape_neutral_panel_side(bad_panel, panel_side_lower, &
      [0.0_real64, 0.0_real64, 0.0_real64], 0.0_real64, &
      shaped, valid, message)
  call require_rejected_unchanged(valid, message, shaped, saved, &
      'non-finite panel')

  call initialize_one_segment_panel(bad_panel, 0.0_real64, 0.0_real64)
  call require(bad_panel%is_valid(), &
      'degenerate fixture should pass neutral-panel structural validation')
  call shape_neutral_panel_side(bad_panel, panel_side_lower, &
      [0.0_real64, 0.0_real64], 0.0_real64, shaped, valid, message)
  call require_rejected_unchanged(valid, message, shaped, saved, &
      'degenerate segment')

  allocate(empty_offsets(0))
  call shape_neutral_panel_side(empty_panel, panel_side_lower, empty_offsets, &
      0.0_real64, shaped, valid, message)
  call require_rejected_unchanged(valid, message, shaped, saved, &
      'empty panel')

  write (*, '(A)') 'PASS: typed panel shaping compatibility kernel'

contains

  subroutine initialize_one_segment_panel(test_panel, edge_delta_u, &
      edge_delta_v)
    type(neutral_panel_2d), intent(out) :: test_panel
    real(real64), intent(in) :: edge_delta_u, edge_delta_v

    call initialize_panel(test_panel, [10.0_real64], [20.0_real64], &
        [10.0_real64 + edge_delta_u], &
        [20.0_real64 + edge_delta_v])
  end subroutine initialize_one_segment_panel

  subroutine initialize_two_segment_gap_panel(test_panel)
    type(neutral_panel_2d), intent(out) :: test_panel

    call initialize_panel(test_panel, [0.0_real64, 30.0_real64], &
        [0.0_real64, 40.0_real64], [3.0_real64, 33.0_real64], &
        [4.0_real64, 44.0_real64])
  end subroutine initialize_two_segment_gap_panel

  subroutine initialize_panel(test_panel, start_u, start_v, end_u, end_v)
    type(neutral_panel_2d), intent(out) :: test_panel
    real(real64), intent(in) :: start_u(:), start_v(:), end_u(:), end_v(:)

    integer :: segment_index, segment_count, point_count

    segment_count = size(start_u)
    point_count = segment_count + 1
    test_panel%panel_index = 2
    test_panel%lower_rib_index = 2
    test_panel%higher_rib_index = 3
    test_panel%surface = surface_intrados
    test_panel%contour_first_index = 7
    test_panel%contour_last_index = 7 + segment_count
    allocate(test_panel%lower_start_biased_u(point_count), &
        test_panel%lower_start_biased_v(point_count), &
        test_panel%higher_start_biased_u(point_count), &
        test_panel%higher_start_biased_v(point_count))
    allocate(test_panel%lower_segment_start_u(segment_count), &
        test_panel%lower_segment_start_v(segment_count), &
        test_panel%lower_segment_end_u(segment_count), &
        test_panel%lower_segment_end_v(segment_count), &
        test_panel%higher_segment_start_u(segment_count), &
        test_panel%higher_segment_start_v(segment_count), &
        test_panel%higher_segment_end_u(segment_count), &
        test_panel%higher_segment_end_v(segment_count))

    test_panel%lower_segment_start_u = start_u
    test_panel%lower_segment_start_v = start_v
    test_panel%lower_segment_end_u = end_u
    test_panel%lower_segment_end_v = end_v
    test_panel%higher_segment_start_u = start_u + 100.0_real64
    test_panel%higher_segment_start_v = start_v + 100.0_real64
    test_panel%higher_segment_end_u = end_u + 100.0_real64
    test_panel%higher_segment_end_v = end_v + 100.0_real64
    do segment_index = 1, segment_count
      test_panel%lower_start_biased_u(segment_index) = start_u(segment_index)
      test_panel%lower_start_biased_v(segment_index) = start_v(segment_index)
      test_panel%higher_start_biased_u(segment_index) = &
          start_u(segment_index) + 100.0_real64
      test_panel%higher_start_biased_v(segment_index) = &
          start_v(segment_index) + 100.0_real64
    end do
    test_panel%lower_start_biased_u(point_count) = end_u(segment_count)
    test_panel%lower_start_biased_v(point_count) = end_v(segment_count)
    test_panel%higher_start_biased_u(point_count) = &
        end_u(segment_count) + 100.0_real64
    test_panel%higher_start_biased_v(point_count) = &
        end_v(segment_count) + 100.0_real64

    test_panel%maximum_lower_join_gap = 0.0_real64
    test_panel%maximum_higher_join_gap = 0.0_real64
    do segment_index = 2, segment_count
      test_panel%maximum_lower_join_gap = max( &
          test_panel%maximum_lower_join_gap, hypot( &
          start_u(segment_index) - end_u(segment_index - 1), &
          start_v(segment_index) - end_v(segment_index - 1)))
    end do
    test_panel%maximum_higher_join_gap = &
        test_panel%maximum_lower_join_gap
    call require(test_panel%is_valid(), 'invalid shaping test fixture')
  end subroutine initialize_panel

  subroutine require_rejected_unchanged(call_valid, call_message, actual, &
      expected, label)
    logical, intent(in) :: call_valid
    character(len=*), intent(in) :: call_message, label
    type(shaped_panel_side_2d), intent(in) :: actual, expected

    call require(.not. call_valid, trim(label)//' was accepted')
    call require(len_trim(call_message) > 0, &
        trim(label)//' returned no diagnostic')
    call require(actual%panel_index == expected%panel_index .and. &
        actual%surface == expected%surface .and. &
        actual%side == expected%side .and. &
        actual%contour_first_index == expected%contour_first_index .and. &
        actual%contour_last_index == expected%contour_last_index, &
        trim(label)//' changed result metadata')
    call require(all(abs(actual%sewing_u - expected%sewing_u) <= &
        0.0_real64) .and. &
        all(abs(actual%sewing_v - expected%sewing_v) <= 0.0_real64) .and. &
        all(abs(actual%cut_u - expected%cut_u) <= 0.0_real64) .and. &
        all(abs(actual%cut_v - expected%cut_v) <= 0.0_real64), &
        trim(label)//' changed result coordinates')
  end subroutine require_rejected_unchanged

  subroutine require_close(actual, expected, label)
    real(real64), intent(in) :: actual, expected
    character(len=*), intent(in) :: label

    call require(abs(actual - expected) <= &
        2.0e-12_real64*max(1.0_real64, abs(expected)), &
        trim(label)//' mismatch')
  end subroutine require_close

  subroutine require_close_strict(actual, expected, label)
    real(real64), intent(in) :: actual, expected
    character(len=*), intent(in) :: label

    call require(abs(actual - expected) <= 2.0e-14_real64, &
        trim(label)//' mismatch')
  end subroutine require_close_strict

  subroutine require_legacy_equal(actual_u, actual_v, expected_u, expected_v, &
      label)
    real(real64), intent(in) :: actual_u(0:,:,:), actual_v(0:,:,:)
    real(real64), intent(in) :: expected_u(0:,:,:), expected_v(0:,:,:)
    character(len=*), intent(in) :: label

    call require(all(shape(actual_u) == shape(expected_u)) .and. &
        all(shape(actual_v) == shape(expected_v)), &
        trim(label)//' shape mismatch')
    call require(all(abs(actual_u-expected_u) <= 0.0_real64) .and. &
        all(abs(actual_v-expected_v) <= 0.0_real64), &
        trim(label)//' changed unowned legacy coordinates')
  end subroutine require_legacy_equal

  subroutine require(condition, message_text)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message_text

    if (.not. condition) then
      write (*, '(A)') 'FAIL: '//message_text
      error stop 1
    end if
  end subroutine require

end program test_panel_shaping
