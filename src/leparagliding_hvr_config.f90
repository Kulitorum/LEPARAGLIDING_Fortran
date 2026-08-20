!> Parse and hold the optional LEparagliding 3.29 H/V-rib configuration.
module leparagliding_hvr_config
  use, intrinsic :: iso_fortran_env, only : iostat_end, real64
  implicit none
  private

  integer, parameter, public :: max_hvr_hole_specs = 100

  !> One enabled hole pattern from input section 38.
  type, public :: hvr_hole_spec
    integer :: source_line = 0
    integer :: type_id = 0
    integer :: hole_count = 0
    real(real64) :: parameter(5) = 0.0_real64
  end type hvr_hole_spec

  !> Collection of section-38 hole patterns.
  type, public :: hvr_hole_config
    integer :: count = 0
    type(hvr_hole_spec) :: spec(max_hvr_hole_specs)
  end type hvr_hole_config

  !> Horizontal and vertical drawing separation factors from section 39.
  type, public :: hvr_position_config
    real(real64) :: horizontal(6) = 1.0_real64
    real(real64) :: vertical(6) = 1.0_real64
  end type hvr_position_config

  public :: find_hvr_hole_spec, read_hvr_sections

contains

  !> Read optional sections 38 and 39, preserving compatibility at end-of-file.
  !!
  !! A 3.28 input ends immediately after section 37. That case is accepted and
  !! leaves both configurations at their documented 3.29 defaults. Once either
  !! new section begins, malformed or incomplete data is reported to the caller.
  subroutine read_hvr_sections(unit_number, holes, positions, sections_present, &
                               status, message)
    integer, intent(in) :: unit_number
    type(hvr_hole_config), intent(out) :: holes
    type(hvr_position_config), intent(out) :: positions
    logical, intent(out) :: sections_present
    integer, intent(out) :: status
    character(len=*), intent(out) :: message

    character(len=512) :: line, section_name
    character(len=50) :: position_name
    integer :: control, declared_count, i, io_status, source_line, type_id
    integer :: enabled, hole_count, position_type
    real(real64) :: parameter(5), horizontal, vertical

    holes = hvr_hole_config()
    positions = hvr_position_config()
    sections_present = .false.
    status = 0
    message = ''

    call read_nonblank_line(unit_number, line, io_status)
    if (io_status == iostat_end) return
    if (io_status /= 0) then
      call set_error(status, message, 'could not read section 38 header')
      return
    end if
    sections_present = .true.

    call read_required_line(unit_number, section_name, status, message, &
                            'section 38 title')
    if (status /= 0) return
    if (index(to_upper(section_name), '38.') == 0 .or. &
        index(to_upper(section_name), 'HVR HOLES') == 0) then
      call set_error(status, message, 'expected section 38 HVR HOLES')
      return
    end if
    call read_required_line(unit_number, line, status, message, &
                            'section 38 closing header')
    if (status /= 0) return
    call read_required_line(unit_number, line, status, message, &
                            'section 38 control value')
    if (status /= 0) return
    read(line, *, iostat=io_status) control
    if (io_status /= 0 .or. (control /= 0 .and. control /= 1)) then
      call set_error(status, message, 'section 38 control must be 0 or 1')
      return
    end if

    if (control == 1) then
      call read_required_line(unit_number, line, status, message, &
                              'section 38 row count')
      if (status /= 0) return
      read(line, *, iostat=io_status) declared_count
      if (io_status /= 0 .or. declared_count < 0 .or. &
          declared_count > max_hvr_hole_specs) then
        call set_error(status, message, &
                       'section 38 row count is outside 0..100')
        return
      end if

      do i = 1, declared_count
        call read_required_line(unit_number, line, status, message, &
                                'section 38 hole row')
        if (status /= 0) return
        read(line, *, iostat=io_status) source_line, type_id, enabled, &
                                         hole_count, parameter
        if (io_status /= 0) then
          call set_error(status, message, 'invalid section 38 hole row')
          return
        end if
        if (enabled /= 0 .and. enabled /= 1) then
          call set_error(status, message, &
                         'section 38 enabled flag must be 0 or 1')
          return
        end if
        if (enabled == 0) cycle

        type_id = canonical_hvr_type(type_id)
        if (type_id == 0) then
          call set_error(status, message, &
                         'section 38 supports HVR types 5/15, 6/16, and 9')
          return
        end if
        if (hole_count < 1 .or. hole_count > 100) then
          call set_error(status, message, &
                         'section 38 hole count is outside 1..100')
          return
        end if
        if (.not. valid_hole_parameters(type_id, parameter)) then
          call set_error(status, message, &
                         'section 38 hole parameters are outside their ranges')
          return
        end if

        holes%count = holes%count + 1
        holes%spec(holes%count)%source_line = source_line
        holes%spec(holes%count)%type_id = type_id
        holes%spec(holes%count)%hole_count = hole_count
        holes%spec(holes%count)%parameter = parameter
      end do
    end if

    call read_nonblank_line(unit_number, line, io_status)
    if (io_status /= 0) then
      call set_error(status, message, 'section 39 is missing after section 38')
      return
    end if
    call read_required_line(unit_number, section_name, status, message, &
                            'section 39 title')
    if (status /= 0) return
    if (index(to_upper(section_name), '39.') == 0 .or. &
        index(to_upper(section_name), 'HVR POSITION PARAMETERS') == 0) then
      call set_error(status, message, &
                     'expected section 39 HVR POSITION PARAMETERS')
      return
    end if
    call read_required_line(unit_number, line, status, message, &
                            'section 39 closing header')
    if (status /= 0) return
    call read_required_line(unit_number, line, status, message, &
                            'section 39 control value')
    if (status /= 0) return
    read(line, *, iostat=io_status) control
    if (io_status /= 0 .or. (control /= 0 .and. control /= 1)) then
      call set_error(status, message, 'section 39 control must be 0 or 1')
      return
    end if

    if (control == 1) then
      call read_required_line(unit_number, line, status, message, &
                              'section 39 row count')
      if (status /= 0) return
      read(line, *, iostat=io_status) declared_count
      if (io_status /= 0 .or. declared_count < 0 .or. declared_count > 6) then
        call set_error(status, message, &
                       'section 39 row count is outside 0..6')
        return
      end if

      do i = 1, declared_count
        call read_required_line(unit_number, line, status, message, &
                                'section 39 position row')
        if (status /= 0) return
        read(line, *, iostat=io_status) position_name, horizontal, vertical
        if (io_status /= 0) then
          call set_error(status, message, 'invalid section 39 position row')
          return
        end if
        position_type = position_type_from_name(position_name)
        if (position_type == 0) then
          call set_error(status, message, &
                         'unknown section 39 HVR position name')
          return
        end if
        if (horizontal < 0.0_real64 .or. vertical < 0.0_real64) then
          call set_error(status, message, &
                         'section 39 separation factors must be nonnegative')
          return
        end if
        positions%horizontal(position_type) = horizontal
        positions%vertical(position_type) = vertical
      end do
    end if
  end subroutine read_hvr_sections

  !> Return the last enabled specification for an HVR type.
  subroutine find_hvr_hole_spec(config, requested_type, spec, found)
    type(hvr_hole_config), intent(in) :: config
    integer, intent(in) :: requested_type
    type(hvr_hole_spec), intent(out) :: spec
    logical, intent(out) :: found

    integer :: i, wanted_type

    spec = hvr_hole_spec()
    found = .false.
    wanted_type = canonical_hvr_type(requested_type)
    if (wanted_type == 0) return

    do i = config%count, 1, -1
      if (config%spec(i)%type_id == wanted_type) then
        spec = config%spec(i)
        found = .true.
        return
      end if
    end do
  end subroutine find_hvr_hole_spec

  subroutine read_nonblank_line(unit_number, line, io_status)
    integer, intent(in) :: unit_number
    character(len=*), intent(out) :: line
    integer, intent(out) :: io_status

    do
      read(unit_number, '(A)', iostat=io_status) line
      if (io_status /= 0 .or. len_trim(line) > 0) return
    end do
  end subroutine read_nonblank_line

  subroutine read_required_line(unit_number, line, status, message, context)
    integer, intent(in) :: unit_number
    character(len=*), intent(out) :: line
    integer, intent(inout) :: status
    character(len=*), intent(inout) :: message
    character(len=*), intent(in) :: context

    integer :: io_status

    if (status /= 0) return
    read(unit_number, '(A)', iostat=io_status) line
    if (io_status /= 0) call set_error(status, message, &
                                      'could not read '//trim(context))
  end subroutine read_required_line

  subroutine set_error(status, message, text)
    integer, intent(out) :: status
    character(len=*), intent(out) :: message
    character(len=*), intent(in) :: text

    status = 1
    message = text
  end subroutine set_error

  integer function canonical_hvr_type(type_id) result(canonical_type)
    integer, intent(in) :: type_id

    select case (type_id)
    case (5, 15)
      canonical_type = 15
    case (6, 16)
      canonical_type = 16
    case (9)
      canonical_type = 9
    case default
      canonical_type = 0
    end select
  end function canonical_hvr_type

  logical function valid_hole_parameters(type_id, parameter) result(valid)
    integer, intent(in) :: type_id
    real(real64), intent(in) :: parameter(5)

    select case (type_id)
    case (15, 16)
      valid = parameter(1) > 0.0_real64 .and. parameter(1) <= 100.0_real64 .and. &
              parameter(2) > 0.0_real64 .and. parameter(2) <= 100.0_real64 .and. &
              parameter(3) >= 0.0_real64 .and. parameter(3) <= 1.0_real64
    case (9)
      ! Pere's example and implementation use parameter 2 as a/b,
      ! parameter 3 as vertical displacement, and parameter 4 as rotation.
      valid = parameter(1) > 0.0_real64 .and. parameter(1) <= 100.0_real64 .and. &
              parameter(2) > 0.0_real64 .and. parameter(2) <= 1.0_real64 .and. &
              parameter(3) >= -1.0_real64 .and. parameter(3) <= 1.0_real64
    case default
      valid = .false.
    end select
  end function valid_hole_parameters

  integer function position_type_from_name(name) result(type_id)
    character(len=*), intent(in) :: name

    select case (trim(to_upper(adjustl(name))))
    case ('VRIB_TYPE1')
      type_id = 1
    case ('VRIB_TYPE2')
      type_id = 2
    case ('VRIB_TYPE3')
      type_id = 3
    case ('VRIB_TYPE4')
      type_id = 4
    case ('VRIB_TYPE5')
      type_id = 5
    case ('VRIB_TYPE6')
      type_id = 6
    case default
      type_id = 0
    end select
  end function position_type_from_name

  pure function to_upper(text) result(upper)
    character(len=*), intent(in) :: text
    character(len=len(text)) :: upper
    integer :: i, code

    upper = text
    do i = 1, len(text)
      code = iachar(upper(i:i))
      if (code >= iachar('a') .and. code <= iachar('z')) &
        upper(i:i) = achar(code - iachar('a') + iachar('A'))
    end do
  end function to_upper

end module leparagliding_hvr_config
