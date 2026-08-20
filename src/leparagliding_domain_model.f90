! SPDX-License-Identifier: GPL-3.0-or-later
!
!> Define coordinate domains used while constructing a paraglider wing.
!!
!! The legacy solver stores several unrelated coordinate systems in numbered
!! slots of the global `u` and `v` arrays.  This module gives those systems
!! distinct types so that normalized airfoil data, spatial wing geometry, and
!! flattened manufacturing geometry cannot be passed to one another by
!! accident.  The adapters at the end of the module are the only intended
!! bridge from the legacy slot layout into these types.
module leparagliding_domain_model
  use, intrinsic :: iso_fortran_env, only : real64
  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
  implicit none
  private

  !> Legacy `u/v` slot holding airfoil coordinates as percentages of chord.
  integer, parameter, public :: legacy_normalized_profile_slot = 2
  !> Legacy `u/v` slot holding the lower-index production sewing edge.
  integer, parameter, public :: legacy_production_lower_sewing_slot = 9
  !> Legacy `u/v` slot holding the higher-index production sewing edge.
  integer, parameter, public :: legacy_production_higher_sewing_slot = 10
  !> Legacy `u/v` slot holding the lower-index production cut edge.
  integer, parameter, public :: legacy_production_lower_cut_slot = 11
  !> Legacy `u/v` slot holding the higher-index production cut edge.
  integer, parameter, public :: legacy_production_higher_cut_slot = 12

  real(real64), parameter :: percent_tolerance = 1.0e-10_real64

  !> Airfoil profile in the chord-normalized two-dimensional domain.
  !!
  !! Invariants: `rib_index >= 0`; both arrays are allocated with the same
  !! extent of at least two points; every coordinate is finite; and
  !! `chord_percent` lies in [0, 100].  `height_percent` is signed and is not
  !! range-limited because reflex and thick profiles are valid inputs.
  type, public :: normalized_profile_2d
    integer :: rib_index = -1
    real(real64), allocatable :: chord_percent(:)
    real(real64), allocatable :: height_percent(:)
  contains
    procedure :: is_valid => normalized_profile_is_valid
  end type normalized_profile_2d

  !> One rib/profile after placement in the fully spatial wing domain.
  !!
  !! Coordinates use LEP model units.  Invariants: `rib_index >= 0`; all
  !! coordinate arrays are allocated with the same extent of at least two
  !! points; and every coordinate is finite.
  type, public :: spatial_rib_geometry_3d
    integer :: rib_index = -1
    real(real64), allocatable :: x(:)
    real(real64), allocatable :: y(:)
    real(real64), allocatable :: z(:)
  contains
    procedure :: is_valid => spatial_rib_is_valid
  end type spatial_rib_geometry_3d

  !> Tensioned lower- and higher-rib edges of one production panel.
  !!
  !! Coordinates use LEP model units and have no drawing-sheet translation.
  !! Invariants: `panel_index >= 0`; the edge rib indices equal the panel
  !! index and its successor; each U/V pair is allocated with equal extents
  !! of at least two points; and all coordinates are finite.  The two edges
  !! may have different point counts because adjacent profiles can
  !! legitimately use different discretizations.
  type, public :: production_panel_edges_2d
    integer :: panel_index = -1
    integer :: lower_rib_index = -1
    integer :: higher_rib_index = -1
    real(real64), allocatable :: lower_sewing_u(:)
    real(real64), allocatable :: lower_sewing_v(:)
    real(real64), allocatable :: higher_sewing_u(:)
    real(real64), allocatable :: higher_sewing_v(:)
    real(real64), allocatable :: lower_cut_u(:)
    real(real64), allocatable :: lower_cut_v(:)
    real(real64), allocatable :: higher_cut_u(:)
    real(real64), allocatable :: higher_cut_v(:)
  contains
    procedure :: is_valid => production_panel_edges_is_valid
  end type production_panel_edges_2d

  !> Complete typed view of one flattened panel and its source profiles.
  !!
  !! This is the production-facing migration unit: color geometry can use the
  !! two normalized profiles to locate chordwise boundaries and the nested
  !! developed edges to map those boundaries into manufacturing coordinates.
  !! Invariants: all nested objects are valid and refer to the same panel/ribs.
  !! Profile and edge point counts are deliberately independent: legacy slot
  !! 10 is discretized by the panel/lower profile even when the adjacent rib's
  !! normalized profile contains a different number of points.  Geometry
  !! consumers must therefore use their common valid prefix when pairing them.
  type, public :: production_panel_2d
    integer :: panel_index = -1
    type(normalized_profile_2d) :: lower_profile
    type(normalized_profile_2d) :: higher_profile
    type(production_panel_edges_2d) :: edges
  contains
    procedure :: is_valid => production_panel_is_valid
  end type production_panel_2d

  !> One open color division crossing a flattened panel between adjacent ribs.
  !!
  !! This is the design-space definition, before mapping onto developed panel
  !! edges.  Invariants: identifiers are positive, `panel_index >= 0`, and
  !! both rib chord positions are finite percentages in [0, 100].  Panel zero
  !! is the valid central/virtual-rib panel used by the geometry pipeline.
  type, public :: color_division
    integer :: boundary_id = 0
    integer :: panel_index = -1
    real(real64) :: lower_chord_percent = 0.0_real64
    real(real64) :: higher_chord_percent = 0.0_real64
  contains
    procedure :: is_valid => color_division_is_valid
  end type color_division

  public :: copy_legacy_normalized_profile
  public :: copy_legacy_spatial_rib
  public :: copy_legacy_production_panel_edges
  public :: copy_legacy_production_panel

contains

  !> Test whether a normalized profile satisfies all documented invariants.
  pure logical function normalized_profile_is_valid(profile) result(valid)
    class(normalized_profile_2d), intent(in) :: profile

    valid = .false.
    if (profile%rib_index < 0) return
    if (.not. allocated(profile%chord_percent)) return
    if (.not. allocated(profile%height_percent)) return
    if (size(profile%chord_percent) < 2) return
    if (size(profile%height_percent) /= size(profile%chord_percent)) return
    if (.not. all(ieee_is_finite(profile%chord_percent))) return
    if (.not. all(ieee_is_finite(profile%height_percent))) return
    if (any(profile%chord_percent < -percent_tolerance)) return
    if (any(profile%chord_percent > 100.0_real64 + percent_tolerance)) return
    valid = .true.
  end function normalized_profile_is_valid

  !> Test whether spatial rib geometry satisfies all documented invariants.
  pure logical function spatial_rib_is_valid(geometry) result(valid)
    class(spatial_rib_geometry_3d), intent(in) :: geometry

    valid = .false.
    if (geometry%rib_index < 0) return
    if (.not. allocated(geometry%x)) return
    if (.not. allocated(geometry%y)) return
    if (.not. allocated(geometry%z)) return
    if (size(geometry%x) < 2) return
    if (size(geometry%y) /= size(geometry%x)) return
    if (size(geometry%z) /= size(geometry%x)) return
    if (.not. all(ieee_is_finite(geometry%x))) return
    if (.not. all(ieee_is_finite(geometry%y))) return
    if (.not. all(ieee_is_finite(geometry%z))) return
    valid = .true.
  end function spatial_rib_is_valid

  !> Test whether developed panel edges satisfy all documented invariants.
  pure logical function production_panel_edges_is_valid(panel) result(valid)
    class(production_panel_edges_2d), intent(in) :: panel

    valid = .false.
    if (panel%panel_index < 0) return
    if (panel%lower_rib_index /= panel%panel_index) return
    if (panel%higher_rib_index /= panel%panel_index + 1) return
    if (.not. allocated(panel%lower_sewing_u)) return
    if (.not. allocated(panel%lower_sewing_v)) return
    if (.not. allocated(panel%higher_sewing_u)) return
    if (.not. allocated(panel%higher_sewing_v)) return
    if (.not. allocated(panel%lower_cut_u)) return
    if (.not. allocated(panel%lower_cut_v)) return
    if (.not. allocated(panel%higher_cut_u)) return
    if (.not. allocated(panel%higher_cut_v)) return
    if (size(panel%lower_sewing_u) < 2 .or. &
        size(panel%higher_sewing_u) < 2) return
    if (size(panel%lower_sewing_v) /= size(panel%lower_sewing_u)) return
    if (size(panel%higher_sewing_v) /= size(panel%higher_sewing_u)) return
    if (size(panel%lower_cut_u) /= size(panel%lower_sewing_u)) return
    if (size(panel%lower_cut_v) /= size(panel%lower_sewing_u)) return
    if (size(panel%higher_cut_u) /= size(panel%higher_sewing_u)) return
    if (size(panel%higher_cut_v) /= size(panel%higher_sewing_u)) return
    if (.not. all(ieee_is_finite(panel%lower_sewing_u))) return
    if (.not. all(ieee_is_finite(panel%lower_sewing_v))) return
    if (.not. all(ieee_is_finite(panel%higher_sewing_u))) return
    if (.not. all(ieee_is_finite(panel%higher_sewing_v))) return
    if (.not. all(ieee_is_finite(panel%lower_cut_u))) return
    if (.not. all(ieee_is_finite(panel%lower_cut_v))) return
    if (.not. all(ieee_is_finite(panel%higher_cut_u))) return
    if (.not. all(ieee_is_finite(panel%higher_cut_v))) return
    valid = .true.
  end function production_panel_edges_is_valid

  !> Test whether a complete developed panel satisfies its nested invariants.
  pure logical function production_panel_is_valid(panel) result(valid)
    class(production_panel_2d), intent(in) :: panel

    valid = .false.
    if (panel%panel_index < 0) return
    if (.not. panel%lower_profile%is_valid()) return
    if (.not. panel%higher_profile%is_valid()) return
    if (.not. panel%edges%is_valid()) return
    if (panel%edges%panel_index /= panel%panel_index) return
    if (panel%lower_profile%rib_index /= panel%panel_index) return
    if (panel%higher_profile%rib_index /= panel%panel_index + 1) return
    valid = .true.
  end function production_panel_is_valid

  !> Test whether a color division satisfies all documented invariants.
  pure logical function color_division_is_valid(division) result(valid)
    class(color_division), intent(in) :: division

    valid = .false.
    if (division%boundary_id < 1) return
    if (division%panel_index < 0) return
    if (.not. ieee_is_finite(division%lower_chord_percent)) return
    if (.not. ieee_is_finite(division%higher_chord_percent)) return
    if (division%lower_chord_percent < -percent_tolerance) return
    if (division%lower_chord_percent > 100.0_real64 + &
        percent_tolerance) return
    if (division%higher_chord_percent < -percent_tolerance) return
    if (division%higher_chord_percent > 100.0_real64 + &
        percent_tolerance) return
    valid = .true.
  end function color_division_is_valid

  !> Copy one normalized rib profile out of the legacy `u/v` slot arrays.
  !!
  !! The first legacy dimension must use the historical zero-based rib bound.
  !! On success `profile` is replaced atomically with an independent copy.  On
  !! failure `profile` is unchanged, `valid` is false, and `message` explains
  !! the rejected invariant.
  !!
  !! @param[in] legacy_u,legacy_v Historical coordinate arrays.
  !! @param[in] rib_index Zero-based rib index to copy.
  !! @param[in] point_count Number of profile points to copy from index one.
  !! @param[inout] profile Typed destination; unchanged on failure.
  !! @param[out] valid True only when a complete valid profile was copied.
  !! @param[out] message Empty on success; diagnostic text on failure.
  subroutine copy_legacy_normalized_profile(legacy_u, legacy_v, rib_index, &
      point_count, profile, valid, message)
    real(real64), intent(in) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    integer, intent(in) :: rib_index, point_count
    type(normalized_profile_2d), intent(inout) :: profile
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(normalized_profile_2d) :: candidate

    valid = .false.
    message = ''
    if (.not. legacy_shapes_match(legacy_u, legacy_v)) then
      message = 'legacy U/V array shapes differ'
      return
    end if
    if (rib_index < 0 .or. rib_index > ubound(legacy_u, 1)) then
      message = 'rib index is outside the legacy array'
      return
    end if
    if (point_count < 2 .or. point_count > size(legacy_u, 2)) then
      message = 'profile point count is outside the legacy array'
      return
    end if
    if (legacy_normalized_profile_slot > size(legacy_u, 3)) then
      message = 'legacy array has no normalized-profile slot'
      return
    end if

    candidate%rib_index = rib_index
    candidate%chord_percent = legacy_u(rib_index, &
        1:point_count, legacy_normalized_profile_slot)
    candidate%height_percent = legacy_v(rib_index, &
        1:point_count, legacy_normalized_profile_slot)
    if (.not. candidate%is_valid()) then
      message = 'normalized profile contains invalid coordinates'
      return
    end if

    profile = candidate
    valid = .true.
  end subroutine copy_legacy_normalized_profile

  !> Copy one placed rib profile from the legacy spatial `x/y/z` arrays.
  !!
  !! The three coordinate arrays must have identical zero-based rib bounds
  !! and point capacities.  On success `geometry` owns an independent copy;
  !! validation failure leaves the previous object unchanged.
  !!
  !! @param[in] legacy_x,legacy_y,legacy_z Spatial skin coordinates.
  !! @param[in] rib_index Zero-based rib index to copy.
  !! @param[in] point_count Number of contour points to copy from index one.
  !! @param[inout] geometry Typed destination; unchanged on failure.
  !! @param[out] valid True only when a complete finite rib was copied.
  !! @param[out] message Empty on success; diagnostic text on failure.
  subroutine copy_legacy_spatial_rib(legacy_x, legacy_y, legacy_z, &
      rib_index, point_count, geometry, valid, message)
    real(real64), intent(in) :: legacy_x(0:,:), legacy_y(0:,:)
    real(real64), intent(in) :: legacy_z(0:,:)
    integer, intent(in) :: rib_index, point_count
    type(spatial_rib_geometry_3d), intent(inout) :: geometry
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(spatial_rib_geometry_3d) :: candidate

    valid = .false.
    message = ''
    if (.not. all(shape(legacy_x) == shape(legacy_y)) .or. &
        .not. all(shape(legacy_x) == shape(legacy_z))) then
      message = 'legacy X/Y/Z array shapes differ'
      return
    end if
    if (rib_index < 0 .or. rib_index > ubound(legacy_x, 1)) then
      message = 'rib index is outside the legacy spatial array'
      return
    end if
    if (point_count < 2 .or. point_count > size(legacy_x, 2)) then
      message = 'spatial point count is outside the legacy array'
      return
    end if

    candidate%rib_index = rib_index
    candidate%x = legacy_x(rib_index, 1:point_count)
    candidate%y = legacy_y(rib_index, 1:point_count)
    candidate%z = legacy_z(rib_index, 1:point_count)
    if (.not. candidate%is_valid()) then
      message = 'spatial rib contains invalid coordinates'
      return
    end if

    geometry = candidate
    valid = .true.
  end subroutine copy_legacy_spatial_rib

  !> Copy one flattened panel's production edges from legacy `u/v` slots.
  !!
  !! Slot 9 belongs to `panel_index`; slot 10 represents the adjacent rib.
  !! Each destination array owns its data.  On validation failure `panel` is
  !! unchanged, preventing a half-updated object from escaping the adapter.
  !!
  !! @param[in] legacy_u,legacy_v Historical coordinate arrays.
  !! @param[in] panel_index Zero-based panel/left-rib index.
  !! @param[in] lower_point_count Number of lower-index edge points to copy.
  !! @param[in] higher_point_count Number of higher-index edge points to copy.
  !! @param[inout] panel Typed destination; unchanged on failure.
  !! @param[out] valid True only when both complete edges were copied.
  !! @param[out] message Empty on success; diagnostic text on failure.
  subroutine copy_legacy_production_panel_edges(legacy_u, legacy_v, &
      panel_index, lower_point_count, higher_point_count, panel, valid, &
      message)
    real(real64), intent(in) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    integer, intent(in) :: panel_index, lower_point_count, higher_point_count
    type(production_panel_edges_2d), intent(inout) :: panel
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(production_panel_edges_2d) :: candidate

    valid = .false.
    message = ''
    if (.not. legacy_shapes_match(legacy_u, legacy_v)) then
      message = 'legacy U/V array shapes differ'
      return
    end if
    if (panel_index < 0 .or. panel_index + 1 > ubound(legacy_u, 1)) then
      message = 'panel or adjacent rib index is outside the legacy array'
      return
    end if
    if (lower_point_count < 2 .or. &
        lower_point_count > size(legacy_u, 2)) then
      message = 'lower-index edge point count is outside the legacy array'
      return
    end if
    if (higher_point_count < 2 .or. &
        higher_point_count > size(legacy_u, 2)) then
      message = 'higher-index edge point count is outside the legacy array'
      return
    end if
    if (legacy_production_higher_cut_slot > size(legacy_u, 3)) then
      message = 'legacy array has no complete production-panel edge slots'
      return
    end if

    candidate%panel_index = panel_index
    candidate%lower_rib_index = panel_index
    candidate%higher_rib_index = panel_index + 1
    candidate%lower_sewing_u = legacy_u(panel_index, 1:lower_point_count, &
        legacy_production_lower_sewing_slot)
    candidate%lower_sewing_v = legacy_v(panel_index, 1:lower_point_count, &
        legacy_production_lower_sewing_slot)
    candidate%higher_sewing_u = legacy_u(panel_index, 1:higher_point_count, &
        legacy_production_higher_sewing_slot)
    candidate%higher_sewing_v = legacy_v(panel_index, 1:higher_point_count, &
        legacy_production_higher_sewing_slot)
    candidate%lower_cut_u = legacy_u(panel_index, 1:lower_point_count, &
        legacy_production_lower_cut_slot)
    candidate%lower_cut_v = legacy_v(panel_index, 1:lower_point_count, &
        legacy_production_lower_cut_slot)
    candidate%higher_cut_u = legacy_u(panel_index, 1:higher_point_count, &
        legacy_production_higher_cut_slot)
    candidate%higher_cut_v = legacy_v(panel_index, 1:higher_point_count, &
        legacy_production_higher_cut_slot)
    if (.not. candidate%is_valid()) then
      message = 'developed panel contains invalid coordinates'
      return
    end if

    panel = candidate
    valid = .true.
  end subroutine copy_legacy_production_panel_edges

  !> Copy one complete flattened panel from the legacy coordinate store.
  !!
  !! This composes the focused profile and edge adapters into the object used
  !! by production geometry.  The update remains transactional: `panel` is
  !! replaced only after both source profiles and both developed edges pass
  !! validation.
  !!
  !! @param[in] legacy_u,legacy_v Historical coordinate arrays.
  !! @param[in] panel_index Zero-based panel/left-rib index.
  !! @param[in] lower_profile_point_count Lower-index profile point count.
  !! @param[in] higher_profile_point_count Higher-index profile point count.
  !! @param[in] lower_edge_point_count Point count for production slot 9.
  !! @param[in] higher_edge_point_count Point count for production slot 10.
  !! @param[inout] panel Typed destination; unchanged on failure.
  !! @param[out] valid True only when the entire panel was copied.
  !! @param[out] message Empty on success; diagnostic text on failure.
  subroutine copy_legacy_production_panel(legacy_u, legacy_v, panel_index, &
      lower_profile_point_count, higher_profile_point_count, &
      lower_edge_point_count, higher_edge_point_count, panel, valid, message)
    real(real64), intent(in) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    integer, intent(in) :: panel_index
    integer, intent(in) :: lower_profile_point_count
    integer, intent(in) :: higher_profile_point_count
    integer, intent(in) :: lower_edge_point_count, higher_edge_point_count
    type(production_panel_2d), intent(inout) :: panel
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(production_panel_2d) :: candidate

    valid = .false.
    message = ''
    candidate%panel_index = panel_index
    call copy_legacy_normalized_profile(legacy_u, legacy_v, panel_index, &
        lower_profile_point_count, candidate%lower_profile, valid, message)
    if (.not. valid) return
    call copy_legacy_normalized_profile(legacy_u, legacy_v, &
        panel_index + 1, higher_profile_point_count, &
        candidate%higher_profile, &
        valid, message)
    if (.not. valid) return
    call copy_legacy_production_panel_edges(legacy_u, legacy_v, panel_index, &
        lower_edge_point_count, higher_edge_point_count, candidate%edges, &
        valid, message)
    if (.not. valid) return
    if (.not. candidate%is_valid()) then
      valid = .false.
      message = 'complete developed panel has inconsistent source domains'
      return
    end if

    panel = candidate
    valid = .true.
  end subroutine copy_legacy_production_panel

  !> Return true when legacy coordinate arrays can be indexed in lockstep.
  pure logical function legacy_shapes_match(legacy_u, legacy_v) result(match)
    real(real64), intent(in) :: legacy_u(0:,:,:), legacy_v(0:,:,:)

    match = all(shape(legacy_u) == shape(legacy_v))
  end function legacy_shapes_match

end module leparagliding_domain_model
