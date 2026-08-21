! SPDX-License-Identifier: GPL-3.0-or-later
!
!> Transform feature-local two-dimensional coordinates into drawing space.
!!
!! This free-form module is the first retired executable procedure include.
!! Its fixed-capacity interface intentionally matches the historical callers
!! while giving them an explicit, checked module procedure and canonical
!! `real64` arithmetic.  The assignment order remains byte-for-byte compatible
!! with the former contained procedure because several callers reuse the
!! fixed work arrays between construction steps.
module leparagliding_transformations
  use, intrinsic :: iso_fortran_env, only : real64
  use leparagliding_geometry, only : vector_2d, rotation_2d, &
      rotation_from_degrees, rotate_and_translate
  implicit none
  private

  integer, parameter :: legacy_coordinate_capacity = 3000

  public :: loc2glo2d

contains

  !> Rotate and translate a sequence of local points into drawing coordinates.
  !!
  !! Indices 1:`point_count` are written in point order; index zero and the
  !! unused suffix are retained.  Callers own the historical fixed-capacity
  !! precondition while their work arrays remain compatibility storage.
  subroutine loc2glo2d(point_count, local_x, local_y, global_x, global_y, &
      origin_x, origin_y, angle_degrees)
    integer, intent(in) :: point_count
    real(real64), intent(in) :: local_x(0:legacy_coordinate_capacity)
    real(real64), intent(in) :: local_y(0:legacy_coordinate_capacity)
    real(real64), intent(inout) :: global_x(0:legacy_coordinate_capacity)
    real(real64), intent(inout) :: global_y(0:legacy_coordinate_capacity)
    real(real64), intent(in) :: origin_x, origin_y, angle_degrees

    type(vector_2d) :: local, origin, global
    type(rotation_2d) :: rotation
    integer :: point_index

    origin = vector_2d(origin_x, origin_y)
    rotation = rotation_from_degrees(angle_degrees)
    do point_index = 1, point_count
      local = vector_2d(local_x(point_index), local_y(point_index))
      global = rotate_and_translate(local, origin, rotation)
      global_x(point_index) = global%x
      global_y(point_index) = global%y
    end do
  end subroutine loc2glo2d

end module leparagliding_transformations
