program test_panel_reformat
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_quiet_nan, ieee_value
  use leparagliding_domain_model, only : production_boundary_edge_2d, &
      surface_intrados, legacy_production_lower_sewing_slot, &
      legacy_production_lower_cut_slot
  use leparagliding_panel_reformat
  implicit none

  type(production_boundary_edge_2d) :: boundary, reformatted, saved
  type(production_boundary_edge_2d) :: direction_boundary
  type(boundary_length_match_control) :: control
  type(preceding_join_support_2d) :: join_support, reformatted_join
  type(preceding_join_support_2d) :: saved_join
  real(real64) :: legacy_u(0:5, 10, 12)
  real(real64) :: legacy_v(0:5, 10, 12)
  real(real64) :: expected_u(0:5, 10, 12)
  real(real64) :: expected_v(0:5, 10, 12)
  character(len=160) :: message
  logical :: valid

  boundary%source_panel_index = 3
  boundary%boundary_rib_index = 4
  boundary%surface = surface_intrados
  boundary%contour_first_index = 5
  boundary%contour_last_index = 8
  boundary%sewing_u = [0.0_real64, 1.0_real64, 2.0_real64, 3.0_real64]
  boundary%sewing_v = [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64]
  boundary%cut_u = boundary%sewing_u
  boundary%cut_v = [1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64]
  call require(boundary%is_valid(), 'valid terminal fixture rejected')

  ! The terminal range starts at point 5, so point 4 is its separate join
  ! support and point 3 is the fixed anchor used by the legacy extrapolation.
  legacy_u = -101.0_real64
  legacy_v = -202.0_real64
  legacy_u(4, 3, legacy_production_lower_sewing_slot) = 10.0_real64
  legacy_v(4, 3, legacy_production_lower_sewing_slot) = 20.0_real64
  legacy_u(4, 4, legacy_production_lower_sewing_slot) = 13.0_real64
  legacy_v(4, 4, legacy_production_lower_sewing_slot) = 22.0_real64
  legacy_u(4, 4, legacy_production_lower_cut_slot) = 14.0_real64
  legacy_v(4, 4, legacy_production_lower_cut_slot) = 24.0_real64
  call copy_legacy_preceding_join_support(boundary, legacy_u, legacy_v, &
      join_support, valid, message)
  call require(valid, 'valid terminal join support rejected: '//trim(message))
  call require(join_support%is_valid(), 'copied terminal join is invalid')
  call require(join_support%anchor_point_index == 3 .and. &
      join_support%support_point_index == 4, &
      'terminal join support lost its contour indices')
  call require_close(join_support%anchor_sewing_u, 10.0_real64, &
      'terminal join anchor U')
  call require_close(join_support%sewing_u, 13.0_real64, &
      'terminal join sewing U')
  call require_close(join_support%cut_v, 24.0_real64, &
      'terminal join cut V')

  call reformat_preceding_join_support(join_support, reformatted_join, &
      valid, message)
  call require(valid, 'valid terminal join reformat rejected: '//trim(message))
  call require_close(reformatted_join%sewing_u, 7.0_real64, &
      'reformatted terminal join sewing U')
  call require_close(reformatted_join%sewing_v, 18.0_real64, &
      'reformatted terminal join sewing V')
  call require_close(reformatted_join%cut_u, 8.0_real64, &
      'reformatted terminal join cut U')
  call require_close(reformatted_join%cut_v, 20.0_real64, &
      'reformatted terminal join cut V')

  expected_u = legacy_u
  expected_v = legacy_v
  expected_u(4, 4, legacy_production_lower_sewing_slot) = 7.0_real64
  expected_v(4, 4, legacy_production_lower_sewing_slot) = 18.0_real64
  expected_u(4, 4, legacy_production_lower_cut_slot) = 8.0_real64
  expected_v(4, 4, legacy_production_lower_cut_slot) = 20.0_real64
  call write_legacy_preceding_join_support(reformatted_join, legacy_u, &
      legacy_v, valid, message)
  call require(valid, 'valid terminal join publication rejected: '// &
      trim(message))
  call require(all(legacy_u == expected_u) .and. all(legacy_v == expected_v), &
      'terminal join publication changed storage outside its exact point')

  ! Failed support operations leave both typed and compatibility storage alone.
  saved_join = reformatted_join
  boundary%surface = 0
  call copy_legacy_preceding_join_support(boundary, legacy_u, legacy_v, &
      reformatted_join, valid, message)
  call require(.not. valid, 'non-intrados terminal join source was accepted')
  call require_same_join_support(reformatted_join, saved_join, &
      'failed terminal join copy changed its destination')
  boundary%surface = surface_intrados

  expected_u = legacy_u
  expected_v = legacy_v
  reformatted_join%source_panel_index = -1
  call write_legacy_preceding_join_support(reformatted_join, legacy_u, &
      legacy_v, valid, message)
  call require(.not. valid, 'invalid terminal join publication was accepted')
  call require(all(legacy_u == expected_u) .and. all(legacy_v == expected_v), &
      'failed terminal join publication changed compatibility storage')

  control%measurement_start_index = 7
  control%reconstruction_start_index = 7
  control%source_contour_length = 3.0_real64
  control%target_contour_length = 4.0_real64
  control%correction_fraction = 1.0_real64
  call reformat_terminal_intrados_boundary(boundary, control, reformatted, &
      valid, message)
  call require(valid, 'valid terminal reformat rejected: '//trim(message))
  call require(reformatted%is_valid(), 'reformatted boundary is invalid')
  call require_close(reformatted%sewing_u(1), -1.0_real64, &
      'first reconstructed sewing U')
  call require_close(reformatted%sewing_u(2), 0.5_real64, &
      'second reconstructed sewing U')
  call require_close(reformatted%sewing_u(3), 2.0_real64, &
      'unchanged anchor sewing U')
  call require_close(reformatted%sewing_u(4), 3.0_real64, &
      'unchanged terminal sewing U')
  call require(all(abs(reformatted%sewing_v) <= 1.0e-12_real64), &
      'horizontal reformat changed sewing V')
  call require(all(abs((reformatted%cut_u - reformatted%sewing_u) - &
      (boundary%cut_u - boundary%sewing_u)) <= 1.0e-12_real64) .and. &
      all(abs((reformatted%cut_v - reformatted%sewing_v) - &
      (boundary%cut_v - boundary%sewing_v)) <= 1.0e-12_real64), &
      'reformat did not preserve the established cut vectors')
  call require(reformatted%source_panel_index == &
      boundary%source_panel_index .and. &
      reformatted%boundary_rib_index == boundary%boundary_rib_index, &
      'reformat changed terminal provenance')

  ! Measurement and reconstruction anchors are intentionally separate.
  control%reconstruction_start_index = 8
  call reformat_terminal_intrados_boundary(boundary, control, reformatted, &
      valid, message)
  call require(valid, 'split-index terminal reformat rejected: '//trim(message))
  call require_close(reformatted%sewing_u(1), -1.5_real64, &
      'split-index first sewing U')
  call require_close(reformatted%sewing_u(2), 0.0_real64, &
      'split-index second sewing U')
  call require_close(reformatted%sewing_u(3), 1.5_real64, &
      'split-index third sewing U')
  call require_close(reformatted%sewing_u(4), 3.0_real64, &
      'split-index anchor sewing U')

  ! A non-unit control fraction can shrink the selected run.
  control%reconstruction_start_index = 7
  control%source_contour_length = 3.0_real64
  control%target_contour_length = 2.0_real64
  control%correction_fraction = 0.5_real64
  call reformat_terminal_intrados_boundary(boundary, control, reformatted, &
      valid, message)
  call require(valid, 'fractional shrink reformat rejected: '//trim(message))
  call require_close(reformatted%sewing_u(1), 0.5_real64, &
      'fractional shrink first sewing U')
  call require_close(reformatted%sewing_u(2), 1.25_real64, &
      'fractional shrink second sewing U')

  ! The absolute-angle plus independent-sign reconstruction must retain every
  ! quadrant when the requested correction is zero.
  direction_boundary = boundary
  direction_boundary%contour_last_index = 9
  direction_boundary%sewing_u = [0.0_real64, 1.0_real64, 0.0_real64, &
      -1.0_real64, 0.0_real64]
  direction_boundary%sewing_v = [0.0_real64, 1.0_real64, 2.0_real64, &
      1.0_real64, 0.0_real64]
  direction_boundary%cut_u = direction_boundary%sewing_u
  direction_boundary%cut_v = direction_boundary%sewing_v + 1.0_real64
  control%measurement_start_index = 9
  control%reconstruction_start_index = 9
  control%source_contour_length = 4.0_real64*sqrt(2.0_real64)
  control%target_contour_length = control%source_contour_length
  control%correction_fraction = 1.0_real64
  call reformat_terminal_intrados_boundary(direction_boundary, control, &
      reformatted, valid, message)
  call require(valid, 'quadrant reformat rejected: '//trim(message))
  call require(all(abs(reformatted%sewing_u - &
      direction_boundary%sewing_u) <= 1.0e-12_real64) .and. &
      all(abs(reformatted%sewing_v - direction_boundary%sewing_v) <= &
      1.0e-12_real64), 'quadrant reconstruction changed a zero-delta path')

  ! Rejected requests are transactional.
  saved = reformatted
  control%measurement_start_index = boundary%contour_first_index
  call reformat_terminal_intrados_boundary(boundary, control, reformatted, &
      valid, message)
  call require(.not. valid, 'empty measurement run was accepted')
  call require_same_boundary(reformatted, saved, &
      'failed measurement request changed its destination')

  control%measurement_start_index = 7
  control%source_contour_length = 3.0_real64
  control%target_contour_length = -1.0_real64
  call reformat_terminal_intrados_boundary(boundary, control, reformatted, &
      valid, message)
  call require(.not. valid, 'negative target length was accepted')
  call require_same_boundary(reformatted, saved, &
      'failed target request changed its destination')

  control%source_contour_length = 0.0_real64
  control%target_contour_length = 4.0_real64
  call reformat_terminal_intrados_boundary(boundary, control, reformatted, &
      valid, message)
  call require(.not. valid, 'zero source length was accepted')
  call require_same_boundary(reformatted, saved, &
      'failed source request changed its destination')

  control%source_contour_length = 3.0_real64
  control%target_contour_length = 4.0_real64
  control%correction_fraction = &
      ieee_value(0.0_real64, ieee_quiet_nan)
  call reformat_terminal_intrados_boundary(boundary, control, reformatted, &
      valid, message)
  call require(.not. valid, 'non-finite correction fraction was accepted')
  call require_same_boundary(reformatted, saved, &
      'non-finite request changed its destination')

contains

  subroutine require(condition, description)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: description

    if (.not. condition) then
      write (*, '(2A)') 'FAIL: ', trim(description)
      error stop 1
    end if
  end subroutine require

  subroutine require_close(actual, expected, description)
    real(real64), intent(in) :: actual, expected
    character(len=*), intent(in) :: description

    call require(abs(actual - expected) <= 1.0e-12_real64, description)
  end subroutine require_close

  subroutine require_same_boundary(actual, expected, description)
    type(production_boundary_edge_2d), intent(in) :: actual, expected
    character(len=*), intent(in) :: description

    call require(actual%source_panel_index == expected%source_panel_index .and. &
        actual%boundary_rib_index == expected%boundary_rib_index .and. &
        actual%surface == expected%surface .and. &
        actual%contour_first_index == expected%contour_first_index .and. &
        actual%contour_last_index == expected%contour_last_index .and. &
        all(abs(actual%sewing_u - expected%sewing_u) <= 0.0_real64) .and. &
        all(abs(actual%sewing_v - expected%sewing_v) <= 0.0_real64) .and. &
        all(abs(actual%cut_u - expected%cut_u) <= 0.0_real64) .and. &
        all(abs(actual%cut_v - expected%cut_v) <= 0.0_real64), description)
  end subroutine require_same_boundary

  subroutine require_same_join_support(actual, expected, description)
    type(preceding_join_support_2d), intent(in) :: actual, expected
    character(len=*), intent(in) :: description

    call require(actual%boundary_rib_index == expected%boundary_rib_index .and. &
        actual%source_panel_index == expected%source_panel_index .and. &
        actual%boundary_contour_first_index == &
        expected%boundary_contour_first_index .and. &
        actual%anchor_point_index == expected%anchor_point_index .and. &
        actual%support_point_index == expected%support_point_index .and. &
        actual%anchor_sewing_u == expected%anchor_sewing_u .and. &
        actual%anchor_sewing_v == expected%anchor_sewing_v .and. &
        actual%sewing_u == expected%sewing_u .and. &
        actual%sewing_v == expected%sewing_v .and. &
        actual%cut_u == expected%cut_u .and. &
        actual%cut_v == expected%cut_v, description)
  end subroutine require_same_join_support

end program test_panel_reformat
