# FTIME

Basic timing routines for Fortran programs. As far as I can tell, all of the code is F2003 compliant.

## Typical Usage

An example is provided in `main.f90` in this repository but the basic outline follows.

```f90
program main
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
use ftime
IMPLICIT NONE

call ftime_init()

call ftime_start('my_timer')
! ... do something ...
call ftime_stop('my_timer')

call ftime_print(stdout)
call ftime_cleanup()

endprogram main
```

## Available Subroutines and Functions

The source code in `mod_ftime.f90` contains Doxygen style comments. Here is also a brief description. **NOTE** that all subroutines and function accept an optional `error` output parameter that will return a nonzero error code if an error is encountered.

- `ftime_init` Optional. Stores the clock count rate but this will also be evaluated later if necessary.
- `ftime_start` Start a timer. Accepts a string as a timer name.
- `ftime_stop` Stop a timer. Accepts a string as a timer name.
- `ftime_time` Function. Returns the elapsed time (in seconds) on a given timer. Note: does not stop a running timer.
- `ftime_print` Stops all timers and prints elapsed time (in seconds).
- `ftime_cleanup` Optional. Deallocates used memory.

## Examples

The code here is F2003 compliant and can should be usable with any standard-compliant compiler. Included in this repository is a `Makefile` and an example program in `main.f90`. The example can be compiled and ran with the following:
```
make && ./main.x
```
or in "debug-mode" with
```
METHOD=dbg make && ./main.x
```
