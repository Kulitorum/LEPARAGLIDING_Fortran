program test_domain_model
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_quiet_nan, ieee_value
  use leparagliding_domain_model
  implicit none

  real(real64) :: legacy_u(0:2, 4, 12), legacy_v(0:2, 4, 12)
  real(real64) :: legacy_x(0:2, 4), legacy_y(0:2, 4), legacy_z(0:2, 4)
  type(normalized_profile_2d) :: profile
  type(spatial_rib_geometry_3d) :: spatial_rib
  type(production_panel_edges_2d) :: panel
  type(production_panel_2d) :: complete_panel
  type(color_division) :: division
  character(len=120) :: message
  logical :: valid

  legacy_u = 0.0_real64
  legacy_v = 0.0_real64
  legacy_x = 0.0_real64
  legacy_y = 0.0_real64
  legacy_z = 0.0_real64

  legacy_u(1, 1:3, legacy_normalized_profile_slot) = &
      [100.0_real64, 50.0_real64, 0.0_real64]
  legacy_v(1, 1:3, legacy_normalized_profile_slot) = &
      [0.0_real64, 12.0_real64, 0.0_real64]

  call copy_legacy_normalized_profile(legacy_u, legacy_v, 1, 3, &
      profile, valid, message)
  call require(valid, 'valid normalized profile rejected: '//trim(message))
  call require(profile%is_valid(), 'copied normalized profile is invalid')
  call require(profile%rib_index == 1, 'normalized rib index')
  call require(size(profile%chord_percent) == 3, 'normalized point count')
  call require_close(profile%chord_percent(2), 50.0_real64, &
      'normalized chord coordinate')
  call require_close(profile%height_percent(2), 12.0_real64, &
      'normalized height coordinate')

  ! The typed profile owns its copy rather than aliasing mutable legacy state.
  legacy_u(1, 2, legacy_normalized_profile_slot) = 75.0_real64
  call require_close(profile%chord_percent(2), 50.0_real64, &
      'normalized profile must own copied data')

  legacy_u(1, 1:3, legacy_production_lower_sewing_slot) = &
      [0.0_real64, 1.0_real64, 2.0_real64]
  legacy_v(1, 1:3, legacy_production_lower_sewing_slot) = &
      [0.0_real64, 2.0_real64, 4.0_real64]
  legacy_u(1, 1:2, legacy_production_higher_sewing_slot) = &
      [10.0_real64, 12.0_real64]
  legacy_v(1, 1:2, legacy_production_higher_sewing_slot) = &
      [1.0_real64, 5.0_real64]
  legacy_u(1, 3, legacy_production_higher_sewing_slot) = 14.0_real64
  legacy_v(1, 3, legacy_production_higher_sewing_slot) = 9.0_real64
  legacy_u(1, 1:3, legacy_production_lower_cut_slot) = &
      [-0.1_real64, 0.9_real64, 1.9_real64]
  legacy_v(1, 1:3, legacy_production_lower_cut_slot) = &
      [0.0_real64, 2.0_real64, 4.0_real64]
  legacy_u(1, 1:3, legacy_production_higher_cut_slot) = &
      [10.1_real64, 12.1_real64, 14.1_real64]
  legacy_v(1, 1:3, legacy_production_higher_cut_slot) = &
      [1.0_real64, 5.0_real64, 9.0_real64]
  legacy_u(2, 1:2, legacy_normalized_profile_slot) = &
      [100.0_real64, 0.0_real64]
  legacy_v(2, 1:2, legacy_normalized_profile_slot) = &
      [0.0_real64, -8.0_real64]

  call copy_legacy_production_panel_edges(legacy_u, legacy_v, 1, 3, 2, &
      panel, valid, message)
  call require(valid, 'valid developed panel rejected: '//trim(message))
  call require(panel%is_valid(), 'copied developed panel is invalid')
  call require(panel%lower_rib_index == 1, 'lower panel rib index')
  call require(panel%higher_rib_index == 2, 'higher panel rib index')
  call require(size(panel%lower_sewing_u) == 3, 'lower edge point count')
  call require(size(panel%higher_sewing_u) == 2, 'higher edge point count')
  call require_close(panel%lower_sewing_v(3), 4.0_real64, &
      'lower sewing edge coordinate')
  call require_close(panel%higher_sewing_u(2), 12.0_real64, &
      'higher sewing edge coordinate')
  call require_close(panel%lower_cut_u(1), -0.1_real64, &
      'lower cut edge coordinate')

  ! The composite adapter exposes exactly one integration object per panel.
  legacy_u(1, 2, legacy_normalized_profile_slot) = 50.0_real64
  call copy_legacy_production_panel(legacy_u, legacy_v, 1, 3, 2, 3, 3, &
      complete_panel, valid, message)
  call require(valid, 'complete developed panel rejected: '//trim(message))
  call require(complete_panel%is_valid(), 'complete panel is invalid')
  call require_close(complete_panel%lower_profile%chord_percent(2), &
      50.0_real64, 'complete panel lower profile')
  call require_close(complete_panel%edges%higher_sewing_v(2), 5.0_real64, &
      'complete panel higher developed edge')
  call require(size(complete_panel%higher_profile%chord_percent) == 2, &
      'higher profile point count')
  call require(size(complete_panel%edges%higher_sewing_u) == 3, &
      'independent higher edge point count')

  ! Panel zero is used for center/virtual-rib construction and is not invalid.
  call copy_legacy_production_panel(legacy_u, legacy_v, 0, 2, 2, 2, 2, &
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
  call copy_legacy_normalized_profile(legacy_u, legacy_v, 1, 3, &
      profile, valid, message)
  call require(.not. valid, 'non-finite normalized profile accepted')
  call require(len_trim(message) > 0, 'adapter failure has no diagnostic')
  call require(profile%is_valid(), 'failed copy damaged existing profile')
  call require_close(profile%chord_percent(2), 50.0_real64, &
      'failed copy changed existing profile')

  legacy_v(1, 2, legacy_production_higher_sewing_slot) = &
      ieee_value(0.0_real64, ieee_quiet_nan)
  call copy_legacy_production_panel_edges(legacy_u, legacy_v, 1, 3, 2, &
      panel, valid, message)
  call require(.not. valid, 'non-finite developed panel accepted')
  call require(panel%is_valid(), 'failed copy damaged existing panel')
  call require_close(panel%higher_sewing_u(2), 12.0_real64, &
      'failed copy changed existing panel')

  call copy_legacy_production_panel_edges(legacy_u, legacy_v, 2, 3, 2, &
      panel, valid, message)
  call require(.not. valid, 'panel without adjacent rib was accepted')

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

end program test_domain_model
