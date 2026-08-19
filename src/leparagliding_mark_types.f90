!> Own the drawing configuration for each supported pattern-mark category.
!!
!! These arrays replace the historical `/markstypes/` COMMON block. Their
!! indices correspond to the mark categories read from input section 20.
module leparagliding_mark_types
  use, intrinsic :: iso_fortran_env, only : real64
  implicit none
  private

  !> Maximum number of configurable mark categories.
  integer, parameter, public :: max_mark_types = 50
  !> Mark construction or display mode by category.
  integer, public :: typm1(max_mark_types)
  !> Mark shape or point mode by category.
  integer, public :: typm4(max_mark_types)
  !> Primary longitudinal size or position factor by category.
  real(real64), public :: typm2(max_mark_types)
  !> Primary transverse size or text-height factor by category.
  real(real64), public :: typm3(max_mark_types)
  !> Symbol size by category.
  real(real64), public :: typm5(max_mark_types)
  !> Symbol spacing by category.
  real(real64), public :: typm6(max_mark_types)

end module leparagliding_mark_types
