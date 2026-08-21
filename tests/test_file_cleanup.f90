program test_file_cleanup
  use leparagliding_file_cleanup, only : fix_dxf_nan, nonan, &
      replace_in_string
  implicit none

  character(len=*), parameter :: nonan_path = 'test-file-cleanup-nonan.tmp'
  character(len=*), parameter :: missing_eof_path = &
      'test-file-cleanup-missing-eof.tmp'
  character(len=*), parameter :: existing_eof_path = &
      'test-file-cleanup-existing-eof.tmp'
  character(len=*), parameter :: missing_path = &
      'test-file-cleanup-does-not-exist.tmp'
  character(len=256) :: message, text, saved_text
  character(len=5) :: short_text, saved_short_text
  character(len=256) :: lines(16)
  integer :: line_count
  logical :: valid

  text = 'before NaN middle NaN after'
  call replace_in_string(text, 'NaN', '0.0', valid, message)
  call require(valid, 'equal-length token replacement failed: '//trim(message))
  call require(trim(text) == 'before 0.0 middle 0.0 after', &
      'equal-length token replacement result')

  text = 'alpha token omega token'
  call replace_in_string(text, 'token', 'X', valid, message)
  call require(valid, 'shorter token replacement failed: '//trim(message))
  call require(trim(text) == 'alpha X omega X', &
      'shorter token replacement result')

  text = 'A x B x'
  call replace_in_string(text, 'x', 'expanded', valid, message)
  call require(valid, 'longer token replacement failed: '//trim(message))
  call require(trim(text) == 'A expanded B expanded', &
      'longer token replacement result')

  text = 'unchanged text and spacing'
  saved_text = text
  call replace_in_string(text, 'absent', 'replacement', valid, message)
  call require(valid, 'no-match replacement failed')
  call require(text == saved_text, 'no-match replacement changed text')
  call replace_in_string(text, '', 'X', valid, message)
  call require(.not. valid, 'empty search token accepted')
  call require(len_trim(message) > 0, 'empty-token failure lacks diagnostic')
  call require(text == saved_text, 'empty-token failure changed text')

  short_text = 'aaaaa'
  saved_short_text = short_text
  call replace_in_string(short_text, 'a', &
      'this replacement cannot fit in the destination', valid, message)
  call require(.not. valid, 'overflowing replacement accepted')
  call require(short_text == saved_short_text, &
      'overflow failure changed text')

  call write_fixture(nonan_path, [character(len=40) :: &
      'HEADER unchanged', 'value NaN remains positioned', 'NaNNaN', &
      'banana and nan are case-sensitive', ''])
  call nonan(nonan_path, valid, message)
  call require(valid, 'NONAN failed: '//trim(message))
  call read_fixture(nonan_path, lines, line_count)
  call require(line_count == 5, 'NONAN changed record count')
  call require(trim(lines(1)) == 'HEADER unchanged', &
      'NONAN changed an unrelated record')
  call require(trim(lines(2)) == 'value 0.0 remains positioned', &
      'NONAN missed an embedded token')
  call require(trim(lines(3)) == '0.00.0', &
      'NONAN missed adjacent tokens')
  call require(trim(lines(4)) == 'banana and nan are case-sensitive', &
      'NONAN changed token case or unrelated text')
  call require(len_trim(lines(5)) == 0, 'NONAN removed an empty record')

  call write_fixture(missing_eof_path, [character(len=40) :: &
      '  0', 'SECTION', 'value NaN', '  0', 'ENDSEC'])
  call fix_dxf_nan(missing_eof_path, valid, message)
  call require(valid, 'missing-EOF DXF fix failed: '//trim(message))
  call read_fixture(missing_eof_path, lines, line_count)
  call require(line_count == 7, 'missing EOF did not add exactly two records')
  call require(trim(lines(3)) == 'value 0.0', &
      'DXF fix missed NaN replacement')
  call require(trim(adjustl(lines(6))) == '0' .and. &
      trim(lines(7)) == 'EOF', &
      'DXF fix appended an invalid EOF marker')

  call write_fixture(existing_eof_path, [character(len=40) :: &
      '  0', 'SECTION', 'unchanged payload', '  0', 'EOF'])
  call fix_dxf_nan(existing_eof_path, valid, message)
  call require(valid, 'existing-EOF DXF fix failed: '//trim(message))
  call read_fixture(existing_eof_path, lines, line_count)
  call require(line_count == 5, 'existing EOF was duplicated')
  call require(trim(lines(3)) == 'unchanged payload', &
      'DXF fix changed unrelated payload')
  call require(trim(adjustl(lines(4))) == '0' .and. &
      trim(lines(5)) == 'EOF', &
      'DXF fix changed the existing EOF marker')

  call delete_fixture_if_present(missing_path)
  call nonan(missing_path, valid, message)
  call require(.not. valid, 'missing input file accepted')
  call require(len_trim(message) > 0, 'missing-file failure lacks diagnostic')

  call delete_fixture_if_present(nonan_path)
  call delete_fixture_if_present(missing_eof_path)
  call delete_fixture_if_present(existing_eof_path)
  write (*, '(A)') 'PASS: typed file cleanup'

contains

  subroutine write_fixture(path, fixture_lines)
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: fixture_lines(:)
    integer :: fixture_unit, io_status, line_index

    open(newunit=fixture_unit, file=path, status='replace', action='write', &
        iostat=io_status)
    call require(io_status == 0, 'cannot create cleanup fixture')
    do line_index = 1, size(fixture_lines)
      write(fixture_unit, '(A)', iostat=io_status) trim(fixture_lines(line_index))
      call require(io_status == 0, 'cannot write cleanup fixture')
    end do
    close(fixture_unit, iostat=io_status)
    call require(io_status == 0, 'cannot close cleanup fixture')
  end subroutine write_fixture

  subroutine read_fixture(path, fixture_lines, fixture_line_count)
    character(len=*), intent(in) :: path
    character(len=*), intent(out) :: fixture_lines(:)
    integer, intent(out) :: fixture_line_count
    integer :: fixture_unit, io_status

    fixture_lines = ''
    fixture_line_count = 0
    open(newunit=fixture_unit, file=path, status='old', action='read', &
        iostat=io_status)
    call require(io_status == 0, 'cannot open cleanup result')
    do
      if (fixture_line_count == size(fixture_lines)) then
        call require(.false., 'cleanup result exceeds test capacity')
      end if
      read(fixture_unit, '(A)', iostat=io_status) &
          fixture_lines(fixture_line_count + 1)
      if (io_status < 0) exit
      call require(io_status == 0, 'cannot read cleanup result')
      fixture_line_count = fixture_line_count + 1
    end do
    close(fixture_unit, iostat=io_status)
    call require(io_status == 0, 'cannot close cleanup result')
  end subroutine read_fixture

  subroutine delete_fixture_if_present(path)
    character(len=*), intent(in) :: path
    integer :: fixture_unit, io_status
    logical :: exists

    inquire(file=path, exist=exists)
    if (.not. exists) return
    open(newunit=fixture_unit, file=path, status='old', action='read', &
        iostat=io_status)
    call require(io_status == 0, 'cannot open cleanup fixture for deletion')
    close(fixture_unit, status='delete', iostat=io_status)
    call require(io_status == 0, 'cannot delete cleanup fixture')
  end subroutine delete_fixture_if_present

  subroutine require(condition, diagnostic)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: diagnostic

    if (.not. condition) then
      write (*, '(2A)') 'FAIL: ', trim(diagnostic)
      error stop 1
    end if
  end subroutine require

end program test_file_cleanup
