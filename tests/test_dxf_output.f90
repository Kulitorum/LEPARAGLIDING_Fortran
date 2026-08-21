program test_dxf_output
  use, intrinsic :: iso_fortran_env, only : real64
  use leparagliding_mark_types, only : typm1, typm2, typm4, typm5
  use leparagliding_dxf_output, only : pointg, line_layer, ellipse, &
      set_ellipse_segments, txt, dxfinit, dxfend
  implicit none

  integer, parameter :: maximum_records = 400
  character(len=160) :: records(maximum_records)
  character(len=50) :: utf8_text
  real(real64) :: point_y, point_radius
  integer :: record_count

  typm1 = 0
  typm2 = 0.0_real64
  typm4 = 0
  typm5 = 0.0_real64

  call test_layered_line()
  call test_mark_state()
  call test_ellipse_configuration()
  call test_utf8_text()
  call test_document_framing()

  write (*, '(A)') 'DXF output tests passed'

contains

  subroutine test_layered_line()
    call open_output()
    call line_layer(1.25_real64, -2.5_real64, 3.75_real64, &
        4.5_real64, 'color_seams', 4)
    call read_output()

    call require(record_count == 18, 'layered line record count')
    call require_record(1, 'LINE', 'layered line entity')
    call require_record(2, '8', 'layered line layer group')
    call require_record(3, 'color_seams', 'layered line layer')
    call require_record(4, '6', 'layered line type group')
    call require_record(5, 'CONTINUOUS', 'layered line type')
    call require_record(6, '10', 'layered line first X group')
    call require_record(7, '1.2500', 'layered line first X')
    call require_record(8, '20', 'layered line first Y group')
    call require_record(9, '2.5000', 'layered line reflected first Y')
    call require_record(10, '11', 'layered line second X group')
    call require_record(11, '3.7500', 'layered line second X')
    call require_record(12, '21', 'layered line second Y group')
    call require_record(13, '-4.5000', 'layered line reflected second Y')
    call require_record(14, '39', 'layered line thickness group')
    call require_record(15, '0', 'layered line thickness')
    call require_record(16, '62', 'layered line color group')
    call require_record(17, '4', 'layered line color')
    call require_record(18, '0', 'layered line terminator')
  end subroutine test_layered_line

  subroutine test_mark_state()
    typm1(1) = 2
    typm2(1) = 2.0_real64
    point_y = 2.0_real64
    point_radius = -1.0_real64

    call open_output()
    call pointg(1.0_real64, point_y, point_radius, 7)
    call read_output()

    call require_close(point_y, 2.0_real64, 'circle mark changed input Y')
    ! `pointg` intentionally promotes its historical default-real factor.
    call require_close(point_radius, real(0.1, real64) * 2.0_real64, &
        'circle mark radius preserves default-real factor')
    call require(find_record('CIRCLE') > 0, 'circle mark entity')
    call require(find_record('mcircles') > 0, 'circle mark layer')
  end subroutine test_mark_state

  subroutine test_ellipse_configuration()
    call open_output()
    call set_ellipse_segments(4)
    call ellipse(0.0_real64, 0.0_real64, 1.0_real64, 0.5_real64, &
        0.0_real64, 3)
    call set_ellipse_segments(60)
    call read_output()

    call require(count_records('VERTEX') == 5, &
        'configured ellipse vertex count')
    call require(find_record('POLYLINE') == 1, 'ellipse polyline header')
    call require(find_record('SEQEND') > 0, 'ellipse polyline terminator')
  end subroutine test_ellipse_configuration

  subroutine test_utf8_text()
    utf8_text = 'Canig' // achar(195) // achar(179)

    call open_output()
    call txt(5.0_real64, 6.0_real64, 0.7_real64, 15.0_real64, &
        utf8_text, 7)
    call read_output()

    call require(find_record('TEXT') == 1, 'text entity header')
    call require(find_record('Canig\U+00F3') > 0, &
        'UTF-8 text was not escaped as DXF Unicode')
    call require_record(record_count - 2, '50', 'text angle group')
    call require_record(record_count - 1, '15.00', 'text angle')
    call require_record(record_count, '0', 'text terminator')
  end subroutine test_utf8_text

  subroutine test_document_framing()
    call open_output()
    call dxfinit(20)
    call dxfend(20)
    call read_output()

    call require_record(1, '0', 'document initial group')
    call require_record(2, 'SECTION', 'document section')
    call require_record(3, '2', 'document section-name group')
    call require_record(4, 'HEADER', 'document header name')
    call require(find_record('AC1009') > 0, 'document DXF version')
    call require(find_record('ANSI_1252') > 0, 'document code page')
    call require_record(record_count - 2, 'ENDSEC', 'document end section')
    call require_record(record_count - 1, '0', 'document EOF group')
    call require_record(record_count, 'EOF', 'document EOF value')
  end subroutine test_document_framing

  subroutine open_output()
    integer :: io_status

    close (20, iostat=io_status)
    open (20, status='scratch', action='readwrite', form='formatted')
  end subroutine open_output

  subroutine read_output()
    integer :: io_status

    records = ''
    record_count = 0
    rewind (20)
    do
      read (20, '(A)', iostat=io_status) records(record_count + 1)
      if (io_status < 0) exit
      call require(io_status == 0, 'failed to read scratch DXF output')
      record_count = record_count + 1
      call require(record_count < maximum_records, &
          'scratch DXF output exceeded test capacity')
    end do
    close (20)
  end subroutine read_output

  integer function find_record(expected) result(record_index)
    character(len=*), intent(in) :: expected
    integer :: candidate

    record_index = 0
    do candidate = 1, record_count
      if (trim(adjustl(records(candidate))) == expected) then
        record_index = candidate
        return
      end if
    end do
  end function find_record

  integer function count_records(expected) result(record_total)
    character(len=*), intent(in) :: expected
    integer :: candidate

    record_total = 0
    do candidate = 1, record_count
      if (trim(adjustl(records(candidate))) == expected) &
          record_total = record_total + 1
    end do
  end function count_records

  subroutine require_record(record_index, expected, diagnostic)
    integer, intent(in) :: record_index
    character(len=*), intent(in) :: expected, diagnostic

    call require(record_index >= 1 .and. record_index <= record_count, &
        trim(diagnostic) // ': record index outside output')
    call require(trim(adjustl(records(record_index))) == expected, &
        trim(diagnostic) // ': unexpected record value')
  end subroutine require_record

  subroutine require_close(actual, expected, diagnostic)
    real(real64), intent(in) :: actual, expected
    character(len=*), intent(in) :: diagnostic

    call require(abs(actual - expected) <= 1.0e-12_real64, diagnostic)
  end subroutine require_close

  subroutine require(condition, diagnostic)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: diagnostic

    if (.not. condition) then
      write (*, '(2A)') 'FAIL: ', trim(diagnostic)
      error stop 1
    end if
  end subroutine require

end program test_dxf_output
