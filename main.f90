program main
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
use ftime
IMPLICIT NONE

integer :: i
real(8) :: xi

call ftime_init()

call ftime_start('first')
do i = 1,1000000
  call random_number(xi)
  write(*,*) xi
enddo
call ftime_stop('first')

call ftime_start('second')
do i = 1,10000000
  call random_number(xi)
  write(*,*) xi
enddo
call ftime_stop('second')

call ftime_print(stdout)

!call ftime_cleanup()

endprogram main
