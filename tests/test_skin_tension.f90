program test_skin_tension
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_quiet_nan, ieee_value
  use leparagliding_domain_model, only : surface_extrados, surface_intrados, &
      surface_intake
  use leparagliding_skin_tension
  implicit none

  real(real64) :: legacy_values(4, 4)
  real(real64) :: classic_values(6, 4)
  real(real64) :: offset
  real(real64) :: saved_position
  real(real64) :: saved_classic_position
  type(skin_tension_law) :: law
  type(classic_skin_tension_law) :: classic_law
  logical :: valid
  character(len=160) :: message

  ! The classic input is one six-point table for the whole wing.  Each surface
  ! has independent position/overwidth columns and reverses into developed
  ! leading-to-trailing contour order.
  classic_values(:, 1) = [0.0_real64, 10.0_real64, 20.0_real64, &
      80.0_real64, 90.0_real64, 100.0_real64]
  classic_values(:, 2) = [0.0_real64, 1.0_real64, 2.0_real64, &
      4.0_real64, 2.0_real64, 0.0_real64]
  classic_values(:, 3) = [0.0_real64, 15.0_real64, 30.0_real64, &
      70.0_real64, 85.0_real64, 100.0_real64]
  classic_values(:, 4) = [0.0_real64, 3.0_real64, 6.0_real64, &
      9.0_real64, 3.0_real64, 0.0_real64]

  call copy_legacy_classic_skin_tension_law(classic_values, &
      surface_extrados, classic_law, valid, message)
  call require(valid, 'valid classic extrados law rejected: '//trim(message))
  call require(classic_law%is_valid(), 'classic extrados law is invalid')
  call require(classic_law%surface == surface_extrados, &
      'classic extrados surface identity was lost')
  call require_close(classic_law%developed_position_percent(2), &
      10.0_real64, 'classic reversed position')
  call require_close(classic_law%overwidth_percent(2), 2.0_real64, &
      'classic reversed overwidth')

  call evaluate_classic_skin_tension_offset(classic_law, 100.0_real64, &
      10.0_real64, 15.0_real64, offset, valid, message)
  call require(valid, 'classic inner interval rejected: '//trim(message))
  call require_close(offset, 0.3_real64, 'classic inner interval offset')
  call evaluate_classic_skin_tension_offset(classic_law, 100.0_real64, &
      10.0_real64, -10.0_real64, offset, valid, message)
  call require(valid, 'classic leading extrapolation rejected: '//trim(message))
  call require_close(offset, -0.2_real64, 'classic leading extrapolation')
  call evaluate_classic_skin_tension_offset(classic_law, 100.0_real64, &
      10.0_real64, 110.0_real64, offset, valid, message)
  call require(valid, 'classic trailing extrapolation rejected: '//trim(message))
  call require_close(offset, -0.1_real64, 'classic trailing extrapolation')

  call copy_legacy_classic_skin_tension_law(classic_values, &
      surface_intrados, classic_law, valid, message)
  call require(valid, 'valid classic intrados law rejected: '//trim(message))
  call require(classic_law%surface == surface_intrados, &
      'classic intrados surface identity was lost')
  call require_close(classic_law%developed_position_percent(2), &
      15.0_real64, 'classic intrados position columns')
  call require_close(classic_law%overwidth_percent(2), 3.0_real64, &
      'classic intrados overwidth columns')

  ! Failed classic conversions retain the previous complete law.
  saved_classic_position = classic_law%developed_position_percent(2)
  classic_values(2, 3) = classic_values(1, 3)
  call copy_legacy_classic_skin_tension_law(classic_values, &
      surface_intrados, classic_law, valid, message)
  call require(.not. valid, 'duplicate classic positions were accepted')
  call require_close(classic_law%developed_position_percent(2), &
      saved_classic_position, 'classic conversion failure changed destination')
  classic_values(2, 3) = 15.0_real64
  classic_values(2, 4) = -1.0_real64
  call copy_legacy_classic_skin_tension_law(classic_values, &
      surface_intrados, classic_law, valid, message)
  call require(.not. valid, 'negative classic overwidth was accepted')
  classic_values(2, 4) = 3.0_real64
  classic_values(2, 4) = ieee_value(0.0_real64, ieee_quiet_nan)
  call copy_legacy_classic_skin_tension_law(classic_values, &
      surface_intrados, classic_law, valid, message)
  call require(.not. valid, 'nonfinite classic overwidth was accepted')
  classic_values(2, 4) = 3.0_real64
  call copy_legacy_classic_skin_tension_law(classic_values, surface_intake, &
      classic_law, valid, message)
  call require(.not. valid, 'classic intake law was accepted')

  call evaluate_classic_skin_tension_offset(classic_law, 0.0_real64, &
      10.0_real64, 0.0_real64, offset, valid, message)
  call require(.not. valid, 'zero classic contour length was accepted')

  ! Authored positions increase from 0 to 100.  The adapter reverses them
  ! into the direction in which the flattened contour distance accumulates.
  legacy_values(:, 1) = [0.0_real64, 20.0_real64, 60.0_real64, 100.0_real64]
  legacy_values(:, 2) = [0.0_real64, 1.0_real64, 3.0_real64, 0.0_real64]
  legacy_values(:, 3) = [0.0_real64, 25.0_real64, 50.0_real64, 100.0_real64]
  legacy_values(:, 4) = [0.0_real64, 4.0_real64, 10.0_real64, 0.0_real64]

  call copy_legacy_new_skin_tension_law(legacy_values, 4, 7, &
      surface_intrados, 1, law, valid, message)
  call require(valid, 'valid intrados law rejected: '//trim(message))
  call require(law%is_valid(), 'converted intrados law is invalid')
  call require(law%boundary_rib_index == 7, 'boundary identity was lost')
  call require(law%surface == surface_intrados, 'surface identity was lost')
  call require_close(law%developed_position_percent(1), 0.0_real64, &
      'first developed fraction')
  call require_close(law%developed_position_percent(2), 50.0_real64, &
      'reversed developed fraction')
  call require_close(law%developed_position_percent(3), 75.0_real64, &
      'second reversed developed fraction')
  call require_close(law%developed_position_percent(4), 100.0_real64, &
      'last developed fraction')
  call require_close(law%overwidth_percent(2), 10.0_real64, &
      'reversed overwidth fraction')

  call evaluate_skin_tension_offset(law, 100.0_real64, 20.0_real64, &
      25.0_real64, offset, valid, message)
  call require(valid, 'first interval rejected: '//trim(message))
  call require_close(offset, 1.0_real64, 'first interval offset')

  call evaluate_skin_tension_offset(law, 100.0_real64, 20.0_real64, &
      50.04_real64, offset, valid, message)
  call require(valid, 'overlap interval rejected: '//trim(message))
  ! Both intervals match because of the 1.001 upper factor.  Section 31 lets
  ! the later interval win, yielding the descending rather than extrapolated
  ! ascending value.
  call require_close(offset, 1.99808_real64, 'last matching interval wins')

  call evaluate_skin_tension_offset(law, 100.0_real64, 20.0_real64, &
      100.0_real64, offset, valid, message)
  call require(valid, 'law endpoint rejected: '//trim(message))
  call require_close(offset, 0.0_real64, 'law endpoint offset')

  call evaluate_skin_tension_offset(law, 100.0_real64, 20.0_real64, &
      101.0_real64, offset, valid, message)
  call require(.not. valid, 'out-of-range distance was accepted')
  call require_close(offset, 0.0_real64, 'failed evaluation output')

  ! A higher panel side selects the next boundary's independently identified
  ! law.  Its contour length and width scale that boundary's normalized data.
  call copy_legacy_new_skin_tension_law(legacy_values, 4, 8, &
      surface_intrados, 1, law, valid, message)
  call require(valid, 'higher-boundary intrados law rejected: '//trim(message))
  call require(law%boundary_rib_index == 8, &
      'higher-boundary intrados identity was lost')
  call evaluate_skin_tension_offset(law, 120.0_real64, 30.0_real64, &
      60.0_real64, offset, valid, message)
  call require(valid, 'higher-boundary intrados point rejected: '// &
      trim(message))
  call require_close(offset, 3.0_real64, &
      'higher-boundary intrados scaling')

  call copy_legacy_new_skin_tension_law(legacy_values, 4, 3, &
      surface_extrados, 1, law, valid, message)
  call require(valid, 'valid extrados law rejected: '//trim(message))
  call require(law%boundary_rib_index == 3, &
      'extrados boundary identity was lost')
  call require(law%surface == surface_extrados, &
      'extrados surface identity was lost')
  call require_close(law%developed_position_percent(2), 40.0_real64, &
      'extrados position columns')
  call require_close(law%overwidth_percent(2), 3.0_real64, &
      'extrados overwidth columns')

  call evaluate_skin_tension_offset(law, 200.0_real64, 10.0_real64, &
      80.0_real64, offset, valid, message)
  call require(valid, 'extrados law point rejected: '//trim(message))
  call require_close(offset, 0.3_real64, 'extrados law offset')
  call evaluate_skin_tension_offset(law, 200.0_real64, 10.0_real64, &
      200.0_real64, offset, valid, message)
  call require(valid, 'extrados law endpoint rejected: '//trim(message))
  call require_close(offset, 0.0_real64, 'extrados endpoint offset')

  law%developed_position_percent(1) = 1.0_real64
  call require(.not. law%is_valid(), 'law without zero endpoint was valid')
  law%developed_position_percent(1) = 0.0_real64
  law%developed_position_percent(4) = 99.0_real64
  call require(.not. law%is_valid(), 'law without full endpoint was valid')
  law%developed_position_percent(4) = 100.0_real64

  ! Rejected adapters are transactional and leave the previous law intact.
  saved_position = law%developed_position_percent(2)
  call copy_legacy_new_skin_tension_law(legacy_values, 1, 3, &
      surface_extrados, 1, law, valid, message)
  call require(.not. valid, 'one-point law was accepted')
  call require_close(law%developed_position_percent(2), saved_position, &
      'one-point failure changed destination')

  call copy_legacy_new_skin_tension_law(legacy_values, 4, 3, &
      surface_intake, 1, law, valid, message)
  call require(.not. valid, 'intake skin law was accepted')
  call copy_legacy_new_skin_tension_law(legacy_values, 4, 3, &
      surface_extrados, 2, law, valid, message)
  call require(.not. valid, 'unsupported interpolation was accepted')

  legacy_values(2, 1) = legacy_values(1, 1)
  call copy_legacy_new_skin_tension_law(legacy_values, 4, 3, &
      surface_extrados, 1, law, valid, message)
  call require(.not. valid, 'duplicate positions were accepted')
  legacy_values(2, 1) = 20.0_real64

  legacy_values(2, 2) = -1.0_real64
  call copy_legacy_new_skin_tension_law(legacy_values, 4, 3, &
      surface_extrados, 1, law, valid, message)
  call require(.not. valid, 'negative overwidth was accepted')
  legacy_values(2, 2) = 1.0_real64

  legacy_values(2, 2) = ieee_value(0.0_real64, ieee_quiet_nan)
  call copy_legacy_new_skin_tension_law(legacy_values, 4, 3, &
      surface_extrados, 1, law, valid, message)
  call require(.not. valid, 'nonfinite overwidth was accepted')
  legacy_values(2, 2) = 1.0_real64

  call evaluate_skin_tension_offset(law, 0.0_real64, 20.0_real64, &
      0.0_real64, offset, valid, message)
  call require(.not. valid, 'zero contour length was accepted')
  call evaluate_skin_tension_offset(law, 100.0_real64, -1.0_real64, &
      0.0_real64, offset, valid, message)
  call require(.not. valid, 'negative panel width was accepted')
  call evaluate_skin_tension_offset(law, 100.0_real64, 20.0_real64, &
      ieee_value(0.0_real64, ieee_quiet_nan), offset, valid, message)
  call require(.not. valid, 'nonfinite developed distance was accepted')

  write (*, '(A)') 'skin tension tests passed'

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
    real(real64), intent(in) :: actual
    real(real64), intent(in) :: expected
    character(len=*), intent(in) :: description

    call require(abs(actual - expected) <= 1.0e-10_real64, description)
  end subroutine require_close

end program test_skin_tension
