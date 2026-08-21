program test_structural_geometry
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan
  use leparagliding_structural_geometry
  implicit none

  integer, parameter :: rib_count = 4, point_capacity = 5, slot_count = 60
  real(real64) :: legacy_u(0:rib_count, point_capacity, slot_count)
  real(real64) :: legacy_v(0:rib_count, point_capacity, slot_count)
  real(real64) :: legacy_w(0:rib_count, point_capacity, slot_count)
  real(real64) :: ballooning(0:rib_count, point_capacity)
  type(spatial_panel_surface) :: surface, saved_surface
  type(local_frame_3d) :: frame
  logical :: valid
  character(len=160) :: message
  integer :: rib_index, point_index, slot_index

  do rib_index = 0, rib_count
    do point_index = 1, point_capacity
      ballooning(rib_index, point_index) = &
          0.01_real64 * real(10 * rib_index + point_index, real64)
      do slot_index = 1, slot_count
        legacy_u(rib_index, point_index, slot_index) = &
            real(10000 * rib_index + 100 * point_index + slot_index, real64)
        legacy_v(rib_index, point_index, slot_index) = &
            -2.0_real64 * legacy_u(rib_index, point_index, slot_index)
        legacy_w(rib_index, point_index, slot_index) = &
            3.0_real64 * legacy_u(rib_index, point_index, slot_index)
      end do
    end do
  end do

  call copy_legacy_spatial_panel_surface(legacy_u, legacy_v, legacy_w, &
      ballooning, 1, 4, surface, valid, message)
  call require(valid, 'valid spatial panel rejected: '//trim(message))
  call require(surface%is_valid(), 'copied surface is invalid')
  call require(surface%panel_index == 1 .and. &
      surface%lower_rib_index == 1 .and. surface%higher_rib_index == 2, &
      'panel adjacency was not retained')
  call require_point(surface%lower_skin(3), legacy_u(1, 3, 47), &
      legacy_v(1, 3, 47), legacy_w(1, 3, 47), 'lower spatial skin')
  call require_point(surface%higher_skin(3), legacy_u(2, 3, 47), &
      legacy_v(2, 3, 47), legacy_w(2, 3, 47), 'higher spatial skin')
  call require_point(surface%neutral_median(3), legacy_u(2, 3, 48), &
      legacy_v(2, 3, 48), legacy_w(2, 3, 48), 'neutral median')
  call require_point(surface%inflated_median(3), legacy_u(2, 3, 49), &
      legacy_v(2, 3, 49), legacy_w(2, 3, 49), 'inflated median')
  call require_point(surface%local_neutral(3), legacy_u(2, 3, 53), &
      legacy_v(2, 3, 53), legacy_w(2, 3, 53), 'local neutral')
  call require_point(surface%local_inflated(3), legacy_u(2, 3, 54), &
      legacy_v(2, 3, 54), legacy_w(2, 3, 54), 'local inflated')
  call require_point(surface%plane_normal_support(3), legacy_u(2, 3, 55), &
      legacy_v(2, 3, 55), legacy_w(2, 3, 55), 'normal support')
  call require_close(surface%ballooning_height(3), ballooning(2, 3), &
      'ballooning height was not retained')

  frame%axis_1 = direction_3d(1.0_real64, 0.0_real64, 0.0_real64)
  frame%axis_2 = direction_3d(0.0_real64, 1.0_real64, 0.0_real64)
  frame%axis_3 = direction_3d(0.0_real64, 0.0_real64, 1.0_real64)
  call require(frame%is_valid(), 'canonical right-handed frame rejected')
  frame%axis_3%z = -1.0_real64
  call require(.not. frame%is_valid(), 'left-handed frame accepted')
  frame%axis_3 = direction_3d(0.0_real64, 0.0_real64, 1.0_real64)
  frame%axis_2%x = 0.1_real64
  call require(.not. frame%is_valid(), 'non-orthogonal frame accepted')

  saved_surface = surface
  legacy_u(2, 2, 49) = ieee_value(0.0_real64, ieee_quiet_nan)
  call copy_legacy_spatial_panel_surface(legacy_u, legacy_v, legacy_w, &
      ballooning, 1, 4, surface, valid, message)
  call require(.not. valid, 'non-finite inflated point accepted')
  call require(len_trim(message) > 0, 'failed copy returned no diagnostic')
  call require_same_surface(surface, saved_surface, &
      'failed copy mutated the destination')
  legacy_u(2, 2, 49) = saved_surface%inflated_median(2)%x_cm

  call copy_legacy_spatial_panel_surface(legacy_u, legacy_v, legacy_w, &
      ballooning, -1, 4, surface, valid, message)
  call require(.not. valid, 'negative panel accepted')
  call require_same_surface(surface, saved_surface, &
      'negative panel mutated the destination')
  call copy_legacy_spatial_panel_surface(legacy_u, legacy_v, legacy_w, &
      ballooning, rib_count, 4, surface, valid, message)
  call require(.not. valid, 'panel beyond higher-rib bounds accepted')
  call copy_legacy_spatial_panel_surface(legacy_u, legacy_v, legacy_w, &
      ballooning, 1, 1, surface, valid, message)
  call require(.not. valid, 'one-point surface accepted')
  call copy_legacy_spatial_panel_surface(legacy_u, legacy_v, legacy_w, &
      ballooning, 1, point_capacity + 1, surface, valid, message)
  call require(.not. valid, 'oversized point count accepted')

  write (*, '(A)') 'structural geometry tests passed'

contains

  subroutine require_point(actual, expected_x, expected_y, expected_z, label)
    use leparagliding_spatial_geometry, only : point_3d
    type(point_3d), intent(in) :: actual
    real(real64), intent(in) :: expected_x, expected_y, expected_z
    character(len=*), intent(in) :: label

    call require_close(actual%x_cm, expected_x, label)
    call require_close(actual%y_cm, expected_y, label)
    call require_close(actual%z_cm, expected_z, label)
  end subroutine require_point

  subroutine require_same_surface(actual, expected, label)
    type(spatial_panel_surface), intent(in) :: actual, expected
    character(len=*), intent(in) :: label

    call require(actual%panel_index == expected%panel_index .and. &
        actual%lower_rib_index == expected%lower_rib_index .and. &
        actual%higher_rib_index == expected%higher_rib_index, label)
    call require(all(abs(actual%lower_skin%x_cm - &
        expected%lower_skin%x_cm) <= 0.0_real64) .and. &
        all(abs(actual%inflated_median%y_cm - &
        expected%inflated_median%y_cm) <= 0.0_real64) .and. &
        all(abs(actual%ballooning_height - expected%ballooning_height) <= &
        0.0_real64), label)
  end subroutine require_same_surface

  subroutine require_close(actual, expected, diagnostic)
    real(real64), intent(in) :: actual, expected
    character(len=*), intent(in) :: diagnostic

    call require(abs(actual - expected) <= 0.0_real64, diagnostic)
  end subroutine require_close

  subroutine require(condition, diagnostic)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: diagnostic

    if (.not. condition) then
      write (*, '(2A)') 'FAIL: ', trim(diagnostic)
      error stop 1
    end if
  end subroutine require

end program test_structural_geometry
