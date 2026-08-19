module leparagliding_mark_types
  use, intrinsic :: iso_fortran_env, only : real64
  implicit none
  private

  integer, parameter, public :: max_mark_types = 50
  integer, public :: typm1(max_mark_types)
  integer, public :: typm4(max_mark_types)
  real(real64), public :: typm2(max_mark_types)
  real(real64), public :: typm3(max_mark_types)
  real(real64), public :: typm5(max_mark_types)
  real(real64), public :: typm6(max_mark_types)

end module leparagliding_mark_types
