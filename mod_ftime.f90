!> Routines for timing programs.
module ftime
IMPLICIT NONE

private

integer, parameter :: ftime_max_name_len = 50 !< maximum length of a timer name

character(ftime_max_name_len), allocatable :: ftime_name(:)
integer, allocatable :: ftime_count(:)
real(8), allocatable :: ftime_elapsed(:)
integer :: ftime_len = 0
integer :: ftime_rate = -1

public :: ftime_init, ftime_start, ftime_stop, ftime_print, ftime_cleanup, ftime_time

contains

!===============================================================================
!> @note Calling this subroutine is optional.
!> The subroutine stores the clock counting rate but it will also be set later
!> if this subroutine is not called.
!>
!> @param[out] error return nonzero error code if an error is encountered
!===============================================================================
  subroutine ftime_init(error)
    integer, intent(out), optional :: error
    if (present(error)) error = 0
    call system_clock(count_rate=ftime_rate)
  endsubroutine

!===============================================================================
!> Stop all timers and print their values.
!>
!> This subroutine will stop all timers and then print the elapsed time of all
!> timers to the specified output unit. Output is in units of seconds.
!>
!> @param[in]  iounit Unit to which the output will be printed (e.g., \c stdout).
!> @param[out] error  return nonzero error code if an error is encountered
!===============================================================================
  subroutine ftime_print(iounit, error)
    integer, intent(in) :: iounit
    integer, intent(out), optional :: error

    integer :: i
    integer :: max_len
    real(8) :: max_time
    character(20) :: format_str

    if (present(error)) error = 0

    ! stop all timers
    do i = 1,ftime_len
      call ftime_stop(ftime_name(i))
    enddo

    write(iounit, '(a)') '=== TIMING RESULTS [s] ==='

    max_len = 0
    max_time = 0d0
    do i = 1,ftime_len
      max_len = max(len(trim(adjustl(ftime_name(i)))), max_len)
      max_time = max(max_time, ftime_elapsed(i))
    enddo

    write(format_str, '(a,i0)') '(a', max_len+1

    ! format the times based on the magnitude of the numbers
    if (max_time < 1d1) then
      format_str = trim(adjustl(format_str)) // ', f6.4)'
    elseif (max_time < 1d2) then
      format_str = trim(adjustl(format_str)) // ', f6.3)'
    elseif (max_time < 1d3) then
      format_str = trim(adjustl(format_str)) // ', f6.2)'
    else
      format_str = trim(adjustl(format_str)) // ', es10.3)'
    endif

    do i = 1,ftime_len
      write(iounit, format_str) ftime_name(i), ftime_elapsed(i)
    enddo

    return
  endsubroutine ftime_print

!===============================================================================
!> Start a timer.
!>
!> 1. If the timer exists, start it.
!> 2. If the timer exists and is already running, do nothing. Return \c error=1.
!> 3. If the timer does not exist, append a new timer and start it.
!>
!> @param[in]  timer_name string timer name to be started
!> @param[out] error      return nonzero error code if an error is encountered.
!===============================================================================
  subroutine ftime_start(timer_name, error)
    character(*), intent(in) :: timer_name
    integer, intent(out), optional :: error

    integer :: idx

    character(ftime_max_name_len), allocatable :: tmp_ftime_name(:)
    integer, allocatable :: tmp_ftime_count(:)
    real(8), allocatable :: tmp_ftime_elapsed(:)

    if (present(error)) error = 0

    idx = ftime_search(timer_name)

    if (idx > 0) then
      if (ftime_count(idx) > 0) then
        call system_clock(count=ftime_count(idx))
        return
      else
        ! timer is already running
        if (present(error)) error = 1
        return
      endif
    endif

    ! the timer was not found, need to append
    allocate(tmp_ftime_name(ftime_len+1))
    allocate(tmp_ftime_count(ftime_len+1))
    allocate(tmp_ftime_elapsed(ftime_len+1))

    if (ftime_len > 0) then
      tmp_ftime_name(1:ftime_len) = ftime_name
      tmp_ftime_count(1:ftime_len) = ftime_count
      tmp_ftime_elapsed(1:ftime_len) = ftime_elapsed
    endif

    tmp_ftime_name(ftime_len+1) = timer_name
    call system_clock(count=tmp_ftime_count(ftime_len+1))
    tmp_ftime_elapsed(ftime_len+1) = 0d0

    if (ftime_len > 0) then
      deallocate(ftime_name)
      deallocate(ftime_count)
      deallocate(ftime_elapsed)
    endif

    ftime_len = ftime_len + 1

    allocate(ftime_name(ftime_len))
    allocate(ftime_count(ftime_len))
    allocate(ftime_elapsed(ftime_len))

    ftime_name = tmp_ftime_name
    ftime_count = tmp_ftime_count
    ftime_elapsed = tmp_ftime_elapsed

    deallocate(tmp_ftime_name)
    deallocate(tmp_ftime_count)
    deallocate(tmp_ftime_elapsed)

    return
  endsubroutine ftime_start

!===============================================================================
!> Stop a timer.
!>
!> 1. If the timer exists and is running, stop it and accumulate the elapsed
!>    time.
!> 2. If the timer exists but is not running, do nothing. \c error=1
!> 3. If the timer does not exist, do nothing. \c error=2
!>
!> This will also store the clock count rate if it was not stored with \c
!> ftime_init.
!>
!> @param[in]  timer_name string timer name to be stopped
!> @param[out] error      return nonzero error code if an error is encountered.
!===============================================================================
  subroutine ftime_stop(timer_name, error)
    character(*), intent(in) :: timer_name
    integer, intent(out), optional :: error

    integer :: idx
    integer :: final_count
    real(8) :: final_time

    if (present(error)) error = 0

    ! if we don't know the clock rate yet, record it
    if (ftime_rate < 0) call system_clock(count_rate=ftime_rate)

    idx = ftime_search(timer_name)

    if (ftime_count(idx) > 0) then
      call system_clock(count=final_count)
      final_time = (final_count - ftime_count(idx)) / real(ftime_rate, 8)
      ftime_elapsed(idx) = ftime_elapsed(idx) + final_time
      ftime_count(idx) = -1 ! stop timer
      return
    else
      ! timer is not running
      if (present(error)) error = 1
      return
    endif

    ! has not been found...
    if (present(error)) error = 2
    return
  endsubroutine ftime_stop

  ! optional: reset the data and deallocate everything
!===============================================================================
!> Optionally cleanup all stored data.
!>
!> @param[out] error return nonzero error code if an error is encountered
!===============================================================================
  subroutine ftime_cleanup(error)
    integer, intent(out), optional :: error

    if (present(error)) error = 0

    if (ftime_len > 0) then
      deallocate(ftime_name)
      deallocate(ftime_count)
      deallocate(ftime_elapsed)
    endif

    ftime_len = 0

    return
  endsubroutine ftime_cleanup

!===============================================================================
!> Internal function to search for a timer in the list.
!>
!> Currently uses a linear search. A bisection search would be better but could
!> introduce additional overhead. If the linear search is affecting your timing,
!> you probably need a different timing library anyway.
!>
!> @param[in]  timer_name string timer name to be found
!> @return index of timer in list, returns \c -1 if not found
!===============================================================================
  integer function ftime_search(timer_name)
    character(*), intent(in) :: timer_name

    integer :: i

    do i = 1,ftime_len
      if (ftime_name(i) == timer_name) then
        ftime_search = i
        return
      endif
    enddo

    ftime_search = -1
    return
  endfunction ftime_search

!===============================================================================
!> Return the elapsed time for a given timer.
!>
!> If the timer is not found, \c error=1.
!> @note this routine does not stop a running timer. The user likely wants to
!> call \c ftime_stop before checking the elapsed time. Output is in units of
!> seconds.
!>
!> @param[in]  timer_name string timer name to be checked
!> @param[out] error      return nonzero error code if an error is encountered.
!> @return elapsed time of timer, return \c -1d0 if timer is not found
!===============================================================================
  real(8) function ftime_time(timer_name, error)
    character(*), intent(in) :: timer_name
    integer, intent(out), optional :: error

    integer :: idx

    if (present(error)) error = 0

    idx = ftime_search(timer_name)

    if (idx > 0) then
      ftime_time = ftime_elapsed(idx)
      return
    endif

    if (present(error)) error = 1
    ftime_time = -1d0
    return
  endfunction ftime_time

endmodule ftime
