program test_profile_data
  use, intrinsic :: iso_fortran_env, only : real64
  use leparagliding_procedures, only : datair, xyzt
  implicit none

  integer, allocatable :: profile_counts(:,:)
  real(real64), allocatable :: rib(:,:), profile_u(:,:,:), profile_v(:,:,:)
  real(real64), allocatable :: profile_w(:,:,:)
  real(real64), allocatable :: auxiliary_u(:,:,:), auxiliary_v(:,:,:)
  real(real64), allocatable :: auxiliary_w(:,:,:)
  real(real64), parameter :: fixture_u(10) = [ &
      1.00_real64, 0.50_real64, 0.00_real64, 0.02_real64, &
      0.03_real64, 0.04_real64, 0.06_real64, 0.08_real64, &
      0.50_real64, 1.00_real64]
  real(real64), parameter :: fixture_v(10) = [ &
      0.00_real64, 0.10_real64, 0.00_real64, -0.02_real64, &
     -0.025_real64, -0.03_real64, -0.025_real64, -0.02_real64, &
     -0.01_real64, 0.00_real64]

  allocate(profile_counts(0:100, 9))
  allocate(rib(0:100, 500))
  allocate(profile_u(0:100, 500, 99), profile_v(0:100, 500, 99))
  profile_counts = 0
  rib = 0.0_real64
  profile_u = 0.0_real64
  profile_v = 0.0_real64

  open (24, status='scratch', action='readwrite')

  call run_case(3.0_real64, 6.0_real64, 10, 5, 7, 5, 7, &
      'boundaries already at source point j')
  call run_case(3.9_real64, 7.9_real64, 10, 6, 8, 6, 8, &
      'boundaries snapped to source point j+1')
  call run_case(3.1_real64, 6.1_real64, 10, 5, 7, 5, 7, &
      'boundaries snapped to source point j')
  call run_case(3.5_real64, 6.0_real64, 11, 6, 8, 0, 7, &
      'one interpolated inlet-boundary insertion')
  call run_case(3.5_real64, 5.0_real64, 12, 6, 8, 0, 0, &
      'interpolated inlet-boundary insertions')
  call run_case(3.4_real64, 3.6_real64, 12, 6, 7, 0, 0, &
      'two insertions within one source segment')
  call test_auxiliary_profile_transform()

  close (24)
  write (*, '(A)') 'PASS: .dat profile boundary rebuild'

contains

  subroutine run_case(start_percent, end_percent, expected_count, &
      expected_start_index, expected_end_index, moved_start_source, &
      moved_end_source, diagnostic)
    real(real64), intent(in) :: start_percent, end_percent
    integer, intent(in) :: expected_count, expected_start_index
    integer, intent(in) :: expected_end_index, moved_start_source
    integer, intent(in) :: moved_end_source
    character(len=*), intent(in) :: diagnostic
    integer :: start_index, end_index

    call write_profile_fixture()
    rib(1, 11) = start_percent
    rib(1, 12) = end_percent
    call datair(1, rib, profile_counts, profile_u, profile_v)

    start_index = profile_counts(1, 2)
    end_index = profile_counts(1, 5)
    call require(profile_counts(1, 1) == expected_count, &
        trim(diagnostic)//': total point count')
    call require(start_index == expected_start_index, &
        trim(diagnostic)//': exact start index')
    call require(end_index == expected_end_index, &
        trim(diagnostic)//': exact end index')
    call require_close(profile_u(1, start_index, 1), &
        start_percent / 100.0_real64, trim(diagnostic)//': start coordinate')
    call require_close(profile_u(1, end_index, 1), &
        end_percent / 100.0_real64, trim(diagnostic)//': end coordinate')
    call require_close(profile_v(1, start_index, 1), &
        lower_surface_height(start_percent / 100.0_real64), &
        trim(diagnostic)//': start height')
    call require_close(profile_v(1, end_index, 1), &
        lower_surface_height(end_percent / 100.0_real64), &
        trim(diagnostic)//': end height')
    call require(profile_counts(1, 5) == profile_counts(1, 2) + &
        profile_counts(1, 3) - 1, trim(diagnostic)//': intake identity')
    call require(profile_counts(1, 1) == profile_counts(1, 2) + &
        profile_counts(1, 3) + profile_counts(1, 4) - 2, &
        trim(diagnostic)//': total identity')
    call require_source_order(expected_count, moved_start_source, &
        moved_end_source, diagnostic)
  end subroutine run_case

  subroutine require_source_order(output_count, moved_start_source, &
      moved_end_source, diagnostic)
    integer, intent(in) :: output_count, moved_start_source, moved_end_source
    character(len=*), intent(in) :: diagnostic
    integer :: source_index, output_index
    logical :: found

    output_index = 1
    do source_index = 1, size(fixture_u)
      if (source_index == moved_start_source .or. &
          source_index == moved_end_source) cycle
      found = .false.
      do while (output_index <= output_count)
        if (abs(profile_u(1, output_index, 1) - &
            fixture_u(source_index)) <= 1.0e-12_real64 .and. &
            abs(profile_v(1, output_index, 1) - &
            fixture_v(source_index)) <= 1.0e-12_real64) then
          found = .true.
          output_index = output_index + 1
          exit
        end if
        output_index = output_index + 1
      end do
      call require(found, trim(diagnostic)// &
          ': unchanged source point missing or reordered')
    end do
  end subroutine require_source_order

  subroutine write_profile_fixture()
    integer :: point_index

    rewind (24)
    write (24, '(A)') 'synthetic DAT profile'
    do point_index = 1, size(fixture_u)
      write (24, '(2(ES24.16,1X))') fixture_u(point_index), &
          fixture_v(point_index)
    end do
    endfile (24)
    rewind (24)
  end subroutine write_profile_fixture

  subroutine test_auxiliary_profile_transform()
    allocate(profile_w(0:100, 500, 99))
    allocate(auxiliary_u(0:100, 500, 10))
    allocate(auxiliary_v(0:100, 500, 10))
    allocate(auxiliary_w(0:100, 500, 10))
    profile_w = 0.0_real64
    auxiliary_u = -777.0_real64
    auxiliary_v = -777.0_real64
    auxiliary_w = -777.0_real64

    rib(1, :) = 0.0_real64
    rib(1, 3) = 100.0_real64
    rib(1, 6) = 200.0_real64
    rib(1, 7) = 300.0_real64
    rib(1, 5) = 10.0_real64
    auxiliary_u(1, 2, 1) = 2.0_real64
    auxiliary_v(1, 2, 1) = 3.0_real64
    auxiliary_w(1, 2, 1) = 0.0_real64

    call xyzt(1, 2, profile_u, profile_v, profile_w, rib, profile_counts, &
        auxiliary_u, auxiliary_v, auxiliary_w)
    call require_close(auxiliary_u(1, 2, 2), 2.0_real64, &
        'auxiliary wash-in X')
    call require_close(auxiliary_v(1, 2, 2), 3.0_real64, &
        'auxiliary wash-in Y')
    call require_close(auxiliary_u(1, 2, 5), 102.0_real64, &
        'auxiliary absolute X')
    call require_close(auxiliary_v(1, 2, 5), 297.0_real64, &
        'auxiliary absolute Y')
    call require_close(auxiliary_w(1, 2, 5), 200.0_real64, &
        'auxiliary absolute Z')

    deallocate(profile_w, auxiliary_u, auxiliary_v, auxiliary_w)
  end subroutine test_auxiliary_profile_transform

  function lower_surface_height(chord_fraction) result(height)
    real(real64), intent(in) :: chord_fraction
    real(real64) :: height
    integer :: point_index

    height = huge(0.0_real64)
    do point_index = 3, size(fixture_u) - 1
      if (fixture_u(point_index) <= chord_fraction .and. &
          fixture_u(point_index + 1) >= chord_fraction .and. &
          fixture_v(point_index) <= 0.0_real64) then
        height = fixture_v(point_index) + &
            (chord_fraction - fixture_u(point_index)) * &
            (fixture_v(point_index + 1) - fixture_v(point_index)) / &
            (fixture_u(point_index + 1) - fixture_u(point_index))
        return
      end if
    end do
  end function lower_surface_height

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

    call require(abs(actual - expected) <= 1.0e-12_real64, diagnostic)
  end subroutine require_close

end program test_profile_data
