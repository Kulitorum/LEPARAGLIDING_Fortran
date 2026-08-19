!> Provide typed, side-effect-free 2D and 3D geometry primitives.
!!
!! This module is the preferred home for reusable geometry. It isolates vector,
!! plane, and rotation concepts from the legacy coordinate-array layout.
module leparagliding_geometry
  use, intrinsic :: iso_fortran_env, only : real64
  implicit none
  private

  !> Cartesian vector or point in two dimensions.
  type, public :: vector_2d
    !> X coordinate.
    real(real64) :: x = 0.0_real64
    !> Y coordinate.
    real(real64) :: y = 0.0_real64
  end type vector_2d

  !> Cartesian vector or point in three dimensions.
  type, public :: vector_3d
    !> X coordinate.
    real(real64) :: x = 0.0_real64
    !> Y coordinate.
    real(real64) :: y = 0.0_real64
    !> Z coordinate.
    real(real64) :: z = 0.0_real64
  end type vector_3d

  !> Plane represented by `normal . point + offset = 0`.
  type, public :: plane_3d
    !> Non-normalized plane normal.
    type(vector_3d) :: normal
    !> Constant term in the Cartesian plane equation.
    real(real64) :: offset = 0.0_real64
  end type plane_3d

  !> Row-major 2-by-2 rotation matrix.
  type, public :: rotation_2d
    !> Matrix entry in row X, column X.
    real(real64) :: xx = 1.0_real64
    !> Matrix entry in row X, column Y.
    real(real64) :: xy = 0.0_real64
    !> Matrix entry in row Y, column X.
    real(real64) :: yx = 0.0_real64
    !> Matrix entry in row Y, column Y.
    real(real64) :: yy = 1.0_real64
  end type rotation_2d

  public :: plane_through_points
  public :: normalize
  public :: point_normal_to_plane
  public :: point_along_line
  public :: rotation_from_degrees
  public :: rotate_and_translate

contains

  !> Construct a plane through three non-collinear points.
  !! @param[in] point1 First point on the plane.
  !! @param[in] point2 Second point on the plane.
  !! @param[in] point3 Third point on the plane.
  !! @return Plane with a cross-product normal and matching offset.
  pure function plane_through_points(point1, point2, point3) result(plane)
    type(vector_3d), intent(in) :: point1, point2, point3
    type(plane_3d) :: plane

    plane%normal%x = (point2%y-point1%y)*(point3%z-point1%z) &
                   - (point2%z-point1%z)*(point3%y-point1%y)
    plane%normal%y = (point2%z-point1%z)*(point3%x-point1%x) &
                   - (point2%x-point1%x)*(point3%z-point1%z)
    plane%normal%z = (point2%x-point1%x)*(point3%y-point1%y) &
                   - (point2%y-point1%y)*(point3%x-point1%x)
    plane%offset = -plane%normal%x*point1%x &
                   -plane%normal%y*point1%y &
                   -plane%normal%z*point1%z
  end function plane_through_points

  !> Scale a 3D vector to unit length.
  !! @param[in] vector Vector to normalize; it must have nonzero length.
  !! @return Unit vector pointing in the same direction.
  pure function normalize(vector) result(unit_vector)
    type(vector_3d), intent(in) :: vector
    type(vector_3d) :: unit_vector
    real(real64) :: scale

    scale = sqrt(1.0_real64/(vector%x*vector%x + vector%y*vector%y &
                            + vector%z*vector%z))
    unit_vector%x = scale*vector%x
    unit_vector%y = scale*vector%y
    unit_vector%z = scale*vector%z
  end function normalize

  !> Move a point along a plane normal by a signed distance.
  !! @param[in] point Starting point.
  !! @param[in] normal Nonzero plane-normal vector.
  !! @param[in] distance Signed displacement along `normal`.
  !! @return Displaced point.
  pure function point_normal_to_plane(point, normal, distance) result(output)
    type(vector_3d), intent(in) :: point, normal
    real(real64), intent(in) :: distance
    type(vector_3d) :: output
    real(real64) :: scale

    scale = distance/sqrt(normal%x*normal%x + normal%y*normal%y &
                          + normal%z*normal%z)
    output%x = point%x + normal%x*scale
    output%y = point%y + normal%y*scale
    output%z = point%z + normal%z*scale
  end function point_normal_to_plane

  !> Locate a point at a signed distance along the line from point 1 to point 2.
  !! @param[in] point1 Line origin.
  !! @param[in] point2 A distinct point defining the line direction.
  !! @param[in] distance Signed distance measured from `point1`.
  !! @return Interpolated or extrapolated point on the infinite line.
  pure function point_along_line(point1, point2, distance) result(output)
    type(vector_3d), intent(in) :: point1, point2
    real(real64), intent(in) :: distance
    type(vector_3d) :: output
    real(real64) :: total_distance, fraction

    total_distance = sqrt((point2%x-point1%x)*(point2%x-point1%x) &
                        + (point2%y-point1%y)*(point2%y-point1%y) &
                        + (point2%z-point1%z)*(point2%z-point1%z))
    fraction = distance/total_distance
    output%x = point1%x + fraction*(point2%x-point1%x)
    output%y = point1%y + fraction*(point2%y-point1%y)
    output%z = point1%z + fraction*(point2%z-point1%z)
  end function point_along_line

  !> Construct a counter-clockwise 2D rotation matrix.
  !! @param[in] angle Rotation angle in degrees.
  !! @return Rotation matrix for the requested angle.
  pure function rotation_from_degrees(angle) result(rotation)
    real(real64), intent(in) :: angle
    type(rotation_2d) :: rotation
    real(real64) :: radians, sine, cosine, pi

    pi = 4.0_real64*atan(1.0_real64)
    radians = angle*pi/180.0_real64
    sine = sin(radians)
    cosine = cos(radians)
    rotation%xx = cosine
    rotation%xy = -sine
    rotation%yx = sine
    rotation%yy = cosine
  end function rotation_from_degrees

  !> Rotate a local point and translate it into global coordinates.
  !! @param[in] point Point expressed in the local coordinate system.
  !! @param[in] origin Global origin of the local coordinate system.
  !! @param[in] rotation Local-to-global rotation matrix.
  !! @return Point expressed in global coordinates.
  pure function rotate_and_translate(point, origin, rotation) result(output)
    type(vector_2d), intent(in) :: point, origin
    type(rotation_2d), intent(in) :: rotation
    type(vector_2d) :: output

    output%x = rotation%xx*point%x + rotation%xy*point%y + origin%x
    output%y = rotation%yx*point%x + rotation%yy*point%y + origin%y
  end function rotate_and_translate

end module leparagliding_geometry
