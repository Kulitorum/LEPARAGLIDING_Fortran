! SPDX-License-Identifier: GPL-3.0-or-later
!
!> Safely post-process generated text and DXF files.
!!
!! This free-form module is the Phase-7 replacement for
!! `procedures/file_cleanup.inc`.  It retains literal, case-sensitive `NaN`
!! replacement and the public NONAN/FIX_DXF_NAN operation split while using
!! caller-visible status diagnostics, private new units, and checked I/O.
module leparagliding_file_cleanup
  use, intrinsic :: iso_fortran_env, only : iostat_end
  implicit none
  private

  integer, parameter :: maximum_text_record_length = 32768

  type :: text_record
    character(len=:), allocatable :: value
  end type text_record

  public :: fix_dxf_nan
  public :: nonan
  public :: replace_in_string

contains

  !> Replace every non-overlapping occurrence of one literal token.
  !!
  !! Matching is case-sensitive and trailing blanks in `old_token` and
  !! `new_token` are not part of either token.  The fixed-length destination
  !! is blank-padded on success.  Empty search tokens and expansions beyond
  !! the destination capacity fail without changing `text`.
  pure subroutine replace_in_string(text, old_token, new_token, valid, message)
    character(len=*), intent(inout) :: text
    character(len=*), intent(in) :: old_token, new_token
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    character(len=len(text)) :: candidate
    integer :: input_index, output_index, source_length
    integer :: old_length, new_length, occurrence_count, result_length

    valid = .false.
    message = ''
    old_length = len_trim(old_token)
    new_length = len_trim(new_token)
    source_length = len_trim(text)
    if (old_length == 0) then
      message = 'replacement search token is empty'
      return
    end if

    occurrence_count = 0
    input_index = 1
    do while (input_index <= source_length - old_length + 1)
      if (text(input_index:input_index + old_length - 1) == &
          old_token(1:old_length)) then
        occurrence_count = occurrence_count + 1
        input_index = input_index + old_length
      else
        input_index = input_index + 1
      end if
    end do
    result_length = source_length + &
        occurrence_count * (new_length - old_length)
    if (result_length > len(text)) then
      message = 'replacement result exceeds destination capacity'
      return
    end if

    candidate = ''
    input_index = 1
    output_index = 1
    do while (input_index <= source_length)
      if (input_index <= source_length - old_length + 1) then
        if (text(input_index:input_index + old_length - 1) == &
            old_token(1:old_length)) then
          if (new_length > 0) then
            candidate(output_index:output_index + new_length - 1) = &
                new_token(1:new_length)
          end if
          output_index = output_index + new_length
          input_index = input_index + old_length
        else
          candidate(output_index:output_index) = text(input_index:input_index)
          output_index = output_index + 1
          input_index = input_index + 1
        end if
      else
        candidate(output_index:output_index) = text(input_index:input_index)
        output_index = output_index + 1
        input_index = input_index + 1
      end if
    end do

    text = candidate
    valid = .true.
  end subroutine replace_in_string

  !> Replace every literal `NaN` occurrence in a text file with `0.0`.
  !!
  !! All input is read and transformed successfully before the original file
  !! is opened with replacement status.  Records other than the substituted
  !! token are retained in order; no DXF EOF record is added by NONAN.
  subroutine nonan(filename, valid, message)
    character(len=*), intent(in) :: filename
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(text_record), allocatable :: records(:)
    logical :: replacement_valid
    character(len=len(message)) :: replacement_message
    integer :: record_index

    valid = .false.
    message = ''
    call read_text_file(filename, records, valid, message)
    if (.not. valid) return
    do record_index = 1, size(records)
      call replace_in_string(records(record_index)%value, 'NaN', '0.0', &
          replacement_valid, replacement_message)
      if (.not. replacement_valid) then
        message = 'NaN replacement failed: '//trim(replacement_message)
        valid = .false.
        return
      end if
    end do
    call write_text_file(filename, records, valid, message)
  end subroutine nonan

  !> Replace literal DXF `NaN` tokens and ensure one standard EOF marker.
  !!
  !! A standard marker is the adjacent record pair `0`, `EOF` after leading
  !! and trailing blanks are ignored.  An existing marker is retained without
  !! duplication; a missing marker appends the legacy records `"  0"` and
  !! `"EOF"`.  Failure before publication leaves the source file unchanged.
  subroutine fix_dxf_nan(filename, valid, message)
    character(len=*), intent(in) :: filename
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(text_record), allocatable :: records(:), expanded_records(:)
    logical :: replacement_valid
    character(len=len(message)) :: replacement_message
    integer :: record_count, record_index

    valid = .false.
    message = ''
    call read_text_file(filename, records, valid, message)
    if (.not. valid) return
    do record_index = 1, size(records)
      call replace_in_string(records(record_index)%value, 'NaN', '0.0', &
          replacement_valid, replacement_message)
      if (.not. replacement_valid) then
        message = 'DXF NaN replacement failed: '//trim(replacement_message)
        valid = .false.
        return
      end if
    end do

    if (.not. contains_dxf_eof(records)) then
      record_count = size(records)
      allocate(expanded_records(record_count + 2))
      if (record_count > 0) expanded_records(1:record_count) = records
      expanded_records(record_count + 1)%value = '  0'
      expanded_records(record_count + 2)%value = 'EOF'
      call move_alloc(expanded_records, records)
    end if
    call write_text_file(filename, records, valid, message)
  end subroutine fix_dxf_nan

  !> Read a complete formatted text file into independently owned records.
  subroutine read_text_file(filename, records, valid, message)
    character(len=*), intent(in) :: filename
    type(text_record), allocatable, intent(out) :: records(:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    character(len=maximum_text_record_length) :: line
    character(len=512) :: io_message
    integer :: input_unit, io_status, record_count, record_index

    valid = .false.
    message = ''
    open(newunit=input_unit, file=trim(filename), status='old', action='read', &
        form='formatted', iostat=io_status, iomsg=io_message)
    if (io_status /= 0) then
      message = 'cannot open input file: '//trim(io_message)
      return
    end if

    record_count = 0
    do
      read(input_unit, '(A)', iostat=io_status, iomsg=io_message) line
      if (io_status == iostat_end) exit
      if (io_status /= 0) then
        message = 'cannot read input file: '//trim(io_message)
        close(input_unit)
        return
      end if
      if (len_trim(line) == maximum_text_record_length) then
        message = 'input record reaches the supported length limit'
        close(input_unit)
        return
      end if
      record_count = record_count + 1
    end do

    rewind(input_unit, iostat=io_status, iomsg=io_message)
    if (io_status /= 0) then
      message = 'cannot rewind input file: '//trim(io_message)
      close(input_unit)
      return
    end if
    allocate(records(record_count))
    do record_index = 1, record_count
      read(input_unit, '(A)', iostat=io_status, iomsg=io_message) line
      if (io_status /= 0) then
        message = 'cannot reread input file: '//trim(io_message)
        close(input_unit)
        return
      end if
      records(record_index)%value = trim(line)
    end do
    close(input_unit, iostat=io_status, iomsg=io_message)
    if (io_status /= 0) then
      message = 'cannot close input file: '//trim(io_message)
      return
    end if
    valid = .true.
  end subroutine read_text_file

  !> Replace a formatted text file from a fully prepared record collection.
  subroutine write_text_file(filename, records, valid, message)
    character(len=*), intent(in) :: filename
    type(text_record), intent(in) :: records(:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    character(len=512) :: io_message
    integer :: output_unit, io_status, record_index

    valid = .false.
    message = ''
    open(newunit=output_unit, file=trim(filename), status='replace', &
        action='write', form='formatted', iostat=io_status, iomsg=io_message)
    if (io_status /= 0) then
      message = 'cannot open output file: '//trim(io_message)
      return
    end if
    do record_index = 1, size(records)
      write(output_unit, '(A)', iostat=io_status, iomsg=io_message) &
          records(record_index)%value
      if (io_status /= 0) then
        message = 'cannot write output file: '//trim(io_message)
        close(output_unit)
        return
      end if
    end do
    close(output_unit, iostat=io_status, iomsg=io_message)
    if (io_status /= 0) then
      message = 'cannot close output file: '//trim(io_message)
      return
    end if
    valid = .true.
  end subroutine write_text_file

  !> Detect a standard two-record DXF EOF marker anywhere in the document.
  pure logical function contains_dxf_eof(records) result(has_eof)
    type(text_record), intent(in) :: records(:)
    integer :: record_index

    has_eof = .false.
    do record_index = 2, size(records)
      if (trim(adjustl(records(record_index - 1)%value)) == '0' .and. &
          trim(adjustl(records(record_index)%value)) == 'EOF') then
        has_eof = .true.
        return
      end if
    end do
  end function contains_dxf_eof

end module leparagliding_file_cleanup
