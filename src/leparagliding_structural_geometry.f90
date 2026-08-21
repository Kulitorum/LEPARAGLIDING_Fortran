! SPDX-License-Identifier: GPL-3.0-or-later
!
!> Name the spatial panel geometry consumed by 3D shaping and internals.
!!
!! Stage 8 and `panels3d` historically exchange six unrelated meanings through
!! `u/v/w` slots 47:55.  This Phase-6 compatibility boundary gives those
!! meanings independent ownership while later feature migrations continue to
!! read the numbered arrays.  The adapter is deliberately transactional: a
!! malformed legacy slice cannot replace a previously valid surface.
module leparagliding_structural_geometry
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
  use leparagliding_spatial_geometry, only : point_3d
  implicit none
  private

  integer, parameter :: legacy_spatial_skin_slot = 47
  integer, parameter :: legacy_neutral_median_slot = 48
  integer, parameter :: legacy_inflated_median_slot = 49
  integer, parameter :: legacy_local_neutral_slot = 53
  integer, parameter :: legacy_local_inflated_slot = 54
  integer, parameter :: legacy_plane_normal_support_slot = 55
  integer, parameter :: required_legacy_spatial_slot_count = &
      legacy_plane_normal_support_slot
  real(real64), parameter :: frame_tolerance = 1.0e-10_real64

  !> Dimensionless direction in a spatial local frame.
  type, public :: direction_3d
    real(real64) :: x = 0.0_real64
    real(real64) :: y = 0.0_real64
    real(real64) :: z = 0.0_real64
  contains
    procedure :: is_finite => direction_is_finite
    procedure :: squared_length => direction_squared_length
  end type direction_3d

  !> Orthonormal frame attached to one shaped median profile.
  !!
  !! The origin is the median leading-edge point.  Axes retain neutral names
  !! until Pere confirms the aerodynamic axis vocabulary and positive signs.
  type, public :: local_frame_3d
    type(point_3d) :: origin
    type(direction_3d) :: axis_1
    type(direction_3d) :: axis_2
    type(direction_3d) :: axis_3
  contains
    procedure :: is_valid => local_frame_is_valid
  end type local_frame_3d

  !> Complete legacy `panels3d` result for one real panel.
  !!
  !! Lower/higher skin points are the adjacent spatial ribs copied through
  !! slot 47.  The remaining arrays are stored historically on the higher rib
  !! row.  Local coordinates deliberately remain three-dimensional because
  !! downstream code reads all of slots 53/54 even though most drawings use
  !! only their second and third components.
  type, public :: spatial_panel_surface
    integer :: panel_index = -1
    integer :: lower_rib_index = -1
    integer :: higher_rib_index = -1
    type(point_3d), allocatable :: lower_skin(:)
    type(point_3d), allocatable :: higher_skin(:)
    type(point_3d), allocatable :: neutral_median(:)
    type(point_3d), allocatable :: inflated_median(:)
    type(point_3d), allocatable :: local_neutral(:)
    type(point_3d), allocatable :: local_inflated(:)
    type(point_3d), allocatable :: plane_normal_support(:)
    real(real64), allocatable :: ballooning_height(:)
  contains
    procedure :: is_valid => spatial_panel_surface_is_valid
  end type spatial_panel_surface

  public :: copy_legacy_spatial_panel_surface
  public :: spatial_panel_surface_matches_legacy

contains

  pure logical function direction_is_finite(direction) result(valid)
    class(direction_3d), intent(in) :: direction

    valid = all(ieee_is_finite([direction%x, direction%y, direction%z]))
  end function direction_is_finite

  pure real(real64) function direction_squared_length(direction) result(value)
    class(direction_3d), intent(in) :: direction

    value = direction%x**2 + direction%y**2 + direction%z**2
  end function direction_squared_length

  !> Validate a finite, unit-length, mutually orthogonal right-handed frame.
  pure logical function local_frame_is_valid(frame) result(valid)
    class(local_frame_3d), intent(in) :: frame
    real(real64) :: handed_x, handed_y, handed_z

    valid = .false.
    if (.not. frame%origin%is_valid()) return
    if (.not. frame%axis_1%is_finite() .or. &
        .not. frame%axis_2%is_finite() .or. &
        .not. frame%axis_3%is_finite()) return
    if (abs(frame%axis_1%squared_length() - 1.0_real64) > &
        frame_tolerance) return
    if (abs(frame%axis_2%squared_length() - 1.0_real64) > &
        frame_tolerance) return
    if (abs(frame%axis_3%squared_length() - 1.0_real64) > &
        frame_tolerance) return
    if (abs(direction_dot(frame%axis_1, frame%axis_2)) > &
        frame_tolerance) return
    if (abs(direction_dot(frame%axis_1, frame%axis_3)) > &
        frame_tolerance) return
    if (abs(direction_dot(frame%axis_2, frame%axis_3)) > &
        frame_tolerance) return

    handed_x = frame%axis_1%y * frame%axis_2%z - &
        frame%axis_1%z * frame%axis_2%y
    handed_y = frame%axis_1%z * frame%axis_2%x - &
        frame%axis_1%x * frame%axis_2%z
    handed_z = frame%axis_1%x * frame%axis_2%y - &
        frame%axis_1%y * frame%axis_2%x
    if (abs(handed_x - frame%axis_3%x) > frame_tolerance .or. &
        abs(handed_y - frame%axis_3%y) > frame_tolerance .or. &
        abs(handed_z - frame%axis_3%z) > frame_tolerance) return
    valid = .true.
  end function local_frame_is_valid

  pure real(real64) function direction_dot(left, right) result(value)
    type(direction_3d), intent(in) :: left, right

    value = left%x * right%x + left%y * right%y + left%z * right%z
  end function direction_dot

  pure logical function spatial_panel_surface_is_valid(surface) result(valid)
    class(spatial_panel_surface), intent(in) :: surface
    integer :: point_count

    valid = .false.
    if (surface%panel_index < 0) return
    if (surface%lower_rib_index /= surface%panel_index) return
    if (surface%higher_rib_index /= surface%panel_index + 1) return
    if (.not. allocated(surface%lower_skin) .or. &
        .not. allocated(surface%higher_skin) .or. &
        .not. allocated(surface%neutral_median) .or. &
        .not. allocated(surface%inflated_median) .or. &
        .not. allocated(surface%local_neutral) .or. &
        .not. allocated(surface%local_inflated) .or. &
        .not. allocated(surface%plane_normal_support) .or. &
        .not. allocated(surface%ballooning_height)) return
    point_count = size(surface%lower_skin)
    if (point_count < 2) return
    if (size(surface%higher_skin) /= point_count .or. &
        size(surface%neutral_median) /= point_count .or. &
        size(surface%inflated_median) /= point_count .or. &
        size(surface%local_neutral) /= point_count .or. &
        size(surface%local_inflated) /= point_count .or. &
        size(surface%plane_normal_support) /= point_count .or. &
        size(surface%ballooning_height) /= point_count) return
    if (.not. all_points_are_valid(surface%lower_skin) .or. &
        .not. all_points_are_valid(surface%higher_skin) .or. &
        .not. all_points_are_valid(surface%neutral_median) .or. &
        .not. all_points_are_valid(surface%inflated_median) .or. &
        .not. all_points_are_valid(surface%local_neutral) .or. &
        .not. all_points_are_valid(surface%local_inflated) .or. &
        .not. all_points_are_valid(surface%plane_normal_support)) return
    if (.not. all(ieee_is_finite(surface%ballooning_height))) return
    valid = .true.
  end function spatial_panel_surface_is_valid

  pure logical function all_points_are_valid(points) result(valid)
    type(point_3d), intent(in) :: points(:)
    integer :: point_index

    valid = .true.
    do point_index = 1, size(points)
      if (.not. points(point_index)%is_valid()) then
        valid = .false.
        return
      end if
    end do
  end function all_points_are_valid

  !> Copy one completed legacy `panels3d` slice into named owned geometry.
  !!
  !! `panel_index` identifies the lower rib.  Legacy results are stored on row
  !! `panel_index+1`, matching the old `panels3d(i,...)` calling convention.
  !! Failure leaves `surface` unchanged.
  pure subroutine copy_legacy_spatial_panel_surface(legacy_u, legacy_v, &
      legacy_w, legacy_ballooning_height, panel_index, point_count, surface, &
      valid, message)
    real(real64), intent(in) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    real(real64), intent(in) :: legacy_w(0:,:,:)
    real(real64), intent(in) :: legacy_ballooning_height(0:,:)
    integer, intent(in) :: panel_index, point_count
    type(spatial_panel_surface), intent(inout) :: surface
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(spatial_panel_surface) :: candidate
    integer :: higher_rib_index, point_index

    valid = .false.
    message = ''
    higher_rib_index = panel_index + 1
    if (panel_index < 0 .or. higher_rib_index > ubound(legacy_u, 1) .or. &
        higher_rib_index > ubound(legacy_v, 1) .or. &
        higher_rib_index > ubound(legacy_w, 1) .or. &
        higher_rib_index > ubound(legacy_ballooning_height, 1)) then
      message = 'panel adjacency is outside legacy row bounds'
      return
    end if
    if (point_count < 2 .or. point_count > ubound(legacy_u, 2) .or. &
        point_count > ubound(legacy_v, 2) .or. &
        point_count > ubound(legacy_w, 2) .or. &
        point_count > ubound(legacy_ballooning_height, 2)) then
      message = 'point count is outside legacy bounds'
      return
    end if
    if (ubound(legacy_u, 3) < required_legacy_spatial_slot_count .or. &
        ubound(legacy_v, 3) < required_legacy_spatial_slot_count .or. &
        ubound(legacy_w, 3) < required_legacy_spatial_slot_count) then
      message = 'legacy geometry lacks required 3D-shaping slots'
      return
    end if

    candidate%panel_index = panel_index
    candidate%lower_rib_index = panel_index
    candidate%higher_rib_index = higher_rib_index
    allocate(candidate%lower_skin(point_count))
    allocate(candidate%higher_skin(point_count))
    allocate(candidate%neutral_median(point_count))
    allocate(candidate%inflated_median(point_count))
    allocate(candidate%local_neutral(point_count))
    allocate(candidate%local_inflated(point_count))
    allocate(candidate%plane_normal_support(point_count))
    allocate(candidate%ballooning_height(point_count))
    do point_index = 1, point_count
      call copy_point(legacy_u, legacy_v, legacy_w, panel_index, point_index, &
          legacy_spatial_skin_slot, candidate%lower_skin(point_index))
      call copy_point(legacy_u, legacy_v, legacy_w, higher_rib_index, &
          point_index, legacy_spatial_skin_slot, &
          candidate%higher_skin(point_index))
      call copy_point(legacy_u, legacy_v, legacy_w, higher_rib_index, &
          point_index, legacy_neutral_median_slot, &
          candidate%neutral_median(point_index))
      call copy_point(legacy_u, legacy_v, legacy_w, higher_rib_index, &
          point_index, legacy_inflated_median_slot, &
          candidate%inflated_median(point_index))
      call copy_point(legacy_u, legacy_v, legacy_w, higher_rib_index, &
          point_index, legacy_local_neutral_slot, &
          candidate%local_neutral(point_index))
      call copy_point(legacy_u, legacy_v, legacy_w, higher_rib_index, &
          point_index, legacy_local_inflated_slot, &
          candidate%local_inflated(point_index))
      call copy_point(legacy_u, legacy_v, legacy_w, higher_rib_index, &
          point_index, legacy_plane_normal_support_slot, &
          candidate%plane_normal_support(point_index))
      candidate%ballooning_height(point_index) = &
          legacy_ballooning_height(higher_rib_index, point_index)
    end do
    if (.not. candidate%is_valid()) then
      message = 'legacy 3D-shaping slice contains invalid geometry'
      return
    end if

    surface = candidate
    valid = .true.
  end subroutine copy_legacy_spatial_panel_surface

  !> Require exact agreement with every retained legacy panel value.
  !!
  !! This check is intended for narrow production-consumer migrations.  It
  !! rebuilds a transactional compatibility candidate, compares metadata,
  !! every named point array, and ballooning heights without a tolerance, and
  !! permits the caller to consume the owned surface only after all values
  !! agree.  Failure does not modify `surface` or the legacy arrays.
  pure subroutine spatial_panel_surface_matches_legacy(surface, legacy_u, &
      legacy_v, legacy_w, legacy_ballooning_height, matches, message)
    type(spatial_panel_surface), intent(in) :: surface
    real(real64), intent(in) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    real(real64), intent(in) :: legacy_w(0:,:,:)
    real(real64), intent(in) :: legacy_ballooning_height(0:,:)
    logical, intent(out) :: matches
    character(len=*), intent(out) :: message

    type(spatial_panel_surface) :: legacy_surface
    logical :: copied
    character(len=len(message)) :: copy_message

    matches = .false.
    message = ''
    if (.not. surface%is_valid()) then
      message = 'typed spatial panel surface is invalid'
      return
    end if
    call copy_legacy_spatial_panel_surface(legacy_u, legacy_v, legacy_w, &
        legacy_ballooning_height, surface%panel_index, &
        size(surface%neutral_median), legacy_surface, copied, copy_message)
    if (.not. copied) then
      message = 'legacy spatial panel comparison failed: '// &
          trim(copy_message)
      return
    end if
    if (surface%lower_rib_index /= legacy_surface%lower_rib_index .or. &
        surface%higher_rib_index /= legacy_surface%higher_rib_index) then
      message = 'typed and legacy panel adjacency differ'
      return
    end if
    if (.not. points_match_exactly(surface%lower_skin, &
        legacy_surface%lower_skin)) then
      message = 'typed and legacy lower spatial skins differ'
      return
    end if
    if (.not. points_match_exactly(surface%higher_skin, &
        legacy_surface%higher_skin)) then
      message = 'typed and legacy higher spatial skins differ'
      return
    end if
    if (.not. points_match_exactly(surface%neutral_median, &
        legacy_surface%neutral_median)) then
      message = 'typed and legacy neutral median surfaces differ'
      return
    end if
    if (.not. points_match_exactly(surface%inflated_median, &
        legacy_surface%inflated_median)) then
      message = 'typed and legacy inflated median surfaces differ'
      return
    end if
    if (.not. points_match_exactly(surface%local_neutral, &
        legacy_surface%local_neutral)) then
      message = 'typed and legacy local neutral surfaces differ'
      return
    end if
    if (.not. points_match_exactly(surface%local_inflated, &
        legacy_surface%local_inflated)) then
      message = 'typed and legacy local inflated surfaces differ'
      return
    end if
    if (.not. points_match_exactly(surface%plane_normal_support, &
        legacy_surface%plane_normal_support)) then
      message = 'typed and legacy plane-normal support points differ'
      return
    end if
    if (any(abs(surface%ballooning_height - &
        legacy_surface%ballooning_height) > 0.0_real64)) then
      message = 'typed and legacy ballooning heights differ'
      return
    end if
    matches = .true.
  end subroutine spatial_panel_surface_matches_legacy

  pure logical function points_match_exactly(left, right) result(matches)
    type(point_3d), intent(in) :: left(:), right(:)
    integer :: point_index

    matches = .false.
    if (size(left) /= size(right)) return
    do point_index = 1, size(left)
      if (abs(left(point_index)%x_cm - right(point_index)%x_cm) > &
          0.0_real64 .or. &
          abs(left(point_index)%y_cm - right(point_index)%y_cm) > &
          0.0_real64 .or. &
          abs(left(point_index)%z_cm - right(point_index)%z_cm) > &
          0.0_real64) return
    end do
    matches = .true.
  end function points_match_exactly

  pure subroutine copy_point(legacy_u, legacy_v, legacy_w, rib_index, &
      point_index, slot_index, point)
    real(real64), intent(in) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    real(real64), intent(in) :: legacy_w(0:,:,:)
    integer, intent(in) :: rib_index, point_index, slot_index
    type(point_3d), intent(out) :: point

    point%x_cm = legacy_u(rib_index, point_index, slot_index)
    point%y_cm = legacy_v(rib_index, point_index, slot_index)
    point%z_cm = legacy_w(rib_index, point_index, slot_index)
  end subroutine copy_point

end module leparagliding_structural_geometry
