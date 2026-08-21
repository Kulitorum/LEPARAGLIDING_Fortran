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
  !> Profile/neutral-panel surface identifiers.
  integer, parameter, public :: surface_extrados = 1
  integer, parameter, public :: surface_intake = 2
  integer, parameter, public :: surface_intrados = 3

  !> Explicit legacy-rib roles.  Index zero has two different meanings
  !! depending on whether the central panel is finite or collapsed, so role
  !! must never be inferred from index or declared parity alone.
  integer, parameter, public :: rib_role_unknown = 0
  integer, parameter, public :: rib_role_physical_centerline = 1
  integer, parameter, public :: rib_role_physical_center_adjacent = 2
  integer, parameter, public :: rib_role_physical_interior = 3
  integer, parameter, public :: rib_role_physical_wingtip = 4
  integer, parameter, public :: rib_role_symmetry_mirror_physical = 5
  integer, parameter, public :: rib_role_symmetry_centerline_alias = 6
  integer, parameter, public :: rib_role_tip_extrapolated_support = 7

  real(real64), parameter :: percent_tolerance = 1.0e-10_real64
  real(real64), parameter :: geometry_tolerance = 1.0e-9_real64
  ! Preserve the default-REAL representation used by legacy `cencell >= 0.01`
  ! before the implicit REAL(4) value is promoted into this REAL(8) module.
  real(real64), parameter :: legacy_central_panel_threshold = &
      real(0.01, real64)

  !> Inclusive range in the ordered contour-point array.
  type, public :: index_range
    integer :: first = 0
    integer :: last = -1
  contains
    procedure :: is_valid => index_range_is_valid
    procedure :: size => index_range_size
  end type index_range

  !> Named topology of one ordered airfoil contour.
  !!
  !! The three ranges share their boundary samples: the final extrados point
  !! is the first intake point, and the final intake point is the first
  !! intrados point.  This replaces legacy `np(:,1:6)` at typed boundaries.
  type, public :: profile_topology
    integer :: point_count = 0
    type(index_range) :: extrados
    type(index_range) :: intake
    type(index_range) :: intrados
    integer :: leading_edge_index = 0
  contains
    procedure :: is_valid => profile_topology_is_valid
  end type profile_topology

  !> Semantic identity of one row in the legacy rib-indexed arrays.
  !!
  !! `profile_source_index` identifies the authored profile copied into a
  !! generated row. `placement_anchor_index` identifies the physical rib used
  !! to place or extrapolate it. Both equal `legacy_index` for authored ribs.
  type, public :: rib_identity
    integer :: legacy_index = -1
    integer :: role = rib_role_unknown
    integer :: profile_source_index = -1
    integer :: placement_anchor_index = -1
  contains
    procedure :: is_valid => rib_identity_is_valid
    procedure :: is_authored_physical => rib_identity_is_authored_physical
    procedure :: participates_in_legacy_geometry => &
        rib_identity_participates_in_legacy_geometry
    procedure :: bounds_active_skin_panel => &
        rib_identity_bounds_active_skin_panel
    procedure :: is_output_half_rib => rib_identity_is_output_half_rib
    procedure :: is_tip_support => rib_identity_is_tip_support
  end type rib_identity

  !> Airfoil profile in the chord-normalized two-dimensional domain.
  !!
  !! Invariants: `rib_index >= 0`; both arrays are allocated with the same
  !! extent of at least two points; every coordinate is finite; and
  !! `chord_percent` lies in [0, 100].  `height_percent` is signed and is not
  !! range-limited because reflex and thick profiles are valid inputs.
  type, public :: normalized_profile_2d
    integer :: rib_index = -1
    type(profile_topology) :: topology
    real(real64), allocatable :: chord_percent(:)
    real(real64), allocatable :: height_percent(:)
  contains
    procedure :: is_valid => normalized_profile_is_valid
  end type normalized_profile_2d

  !> One rib/profile after placement in the fully spatial wing domain.
  !!
  !! Coordinates use LEP model units.  Invariants: `rib_index >= 0`; all
  !! coordinate arrays are one-based, allocated with the same extent of at
  !! least two points; and every coordinate is finite.  One-based storage keeps
  !! profile topology indices identical to their source contour indices.
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

  !> Tensioned production edge at one physical terminal rib.
  !!
  !! The boundary is derived from the higher side of the final real panel but
  !! is deliberately not itself a panel.  It owns only the sewing and cut
  !! contours published through the legacy lower-edge slots.  No higher-side
  !! geometry exists beyond the physical wingtip.
  type, public :: production_boundary_edge_2d
    integer :: boundary_rib_index = -1
    integer :: source_panel_index = -1
    integer :: surface = 0
    integer :: contour_first_index = 0
    integer :: contour_last_index = -1
    real(real64), allocatable :: sewing_u(:)
    real(real64), allocatable :: sewing_v(:)
    real(real64), allocatable :: cut_u(:)
    real(real64), allocatable :: cut_v(:)
  contains
    procedure :: is_valid => production_boundary_edge_is_valid
  end type production_boundary_edge_2d

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

  !> Untensioned development of one panel surface in its local 2D frame.
  !!
  !! `lower_*` belongs to `panel_index`; `higher_*` belongs to its successor.
  !! Arrays contain contour points rather than the legacy quadrilateral-corner
  !! pairs.  Intake's historical extra tangent segment is exposed separately.
  type, public :: neutral_panel_2d
    integer :: panel_index = -1
    integer :: lower_rib_index = -1
    integer :: higher_rib_index = -1
    integer :: surface = 0
    integer :: contour_first_index = 0
    integer :: contour_last_index = -1
    ! Start-biased point views: at a join, the following segment start wins.
    !! Exact calculations must use the segment arrays below because legacy
    !! quadrilateral reconstruction can leave a measurable join gap.
    real(real64), allocatable :: lower_start_biased_u(:)
    real(real64), allocatable :: lower_start_biased_v(:)
    real(real64), allocatable :: higher_start_biased_u(:)
    real(real64), allocatable :: higher_start_biased_v(:)
    real(real64), allocatable :: lower_segment_start_u(:)
    real(real64), allocatable :: lower_segment_start_v(:)
    real(real64), allocatable :: lower_segment_end_u(:)
    real(real64), allocatable :: lower_segment_end_v(:)
    real(real64), allocatable :: higher_segment_start_u(:)
    real(real64), allocatable :: higher_segment_start_v(:)
    real(real64), allocatable :: higher_segment_end_u(:)
    real(real64), allocatable :: higher_segment_end_v(:)
    real(real64) :: maximum_lower_join_gap = 0.0_real64
    real(real64) :: maximum_higher_join_gap = 0.0_real64
    logical :: has_post_surface_support = .false.
    real(real64) :: support_lower_start_u = 0.0_real64
    real(real64) :: support_lower_start_v = 0.0_real64
    real(real64) :: support_lower_end_u = 0.0_real64
    real(real64) :: support_lower_end_v = 0.0_real64
    real(real64) :: support_higher_start_u = 0.0_real64
    real(real64) :: support_higher_start_v = 0.0_real64
    real(real64) :: support_higher_end_u = 0.0_real64
    real(real64) :: support_higher_end_v = 0.0_real64
    real(real64) :: support_lower_join_gap = 0.0_real64
    real(real64) :: support_higher_join_gap = 0.0_real64
  contains
    procedure :: is_valid => neutral_panel_is_valid
  end type neutral_panel_2d

  !> One terminal neutral edge derived from the higher side of a real panel.
  !!
  !! This is boundary support for consumers that need the physical wingtip's
  !! outward comparison edge.  It is deliberately not a panel: no geometry is
  !! invented beyond the final physical rib, and `source_panel_index` records
  !! the real panel whose higher edge supplies every coordinate.  Intake keeps
  !! its historical following-segment support explicit.
  type, public :: neutral_boundary_edge_2d
    integer :: boundary_rib_index = -1
    integer :: source_panel_index = -1
    integer :: surface = 0
    integer :: contour_first_index = 0
    integer :: contour_last_index = -1
    real(real64), allocatable :: start_biased_u(:)
    real(real64), allocatable :: start_biased_v(:)
    real(real64), allocatable :: segment_start_u(:)
    real(real64), allocatable :: segment_start_v(:)
    real(real64), allocatable :: segment_end_u(:)
    real(real64), allocatable :: segment_end_v(:)
    real(real64) :: maximum_join_gap = 0.0_real64
    logical :: has_post_surface_support = .false.
    real(real64) :: support_start_u = 0.0_real64
    real(real64) :: support_start_v = 0.0_real64
    real(real64) :: support_end_u = 0.0_real64
    real(real64) :: support_end_v = 0.0_real64
    real(real64) :: support_join_gap = 0.0_real64
  contains
    procedure :: is_valid => neutral_boundary_edge_is_valid
  end type neutral_boundary_edge_2d

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
  public :: copy_legacy_profile_topology
  public :: copy_legacy_spatial_rib
  public :: copy_legacy_production_panel_edges
  public :: copy_legacy_production_panel
  public :: infer_legacy_rib_identities
  public :: topologies_are_index_compatible
  public :: extrados_topologies_are_index_compatible
  public :: copy_legacy_neutral_panel
  public :: copy_legacy_neutral_panel_from_counts
  public :: write_legacy_extrados_panel
  public :: write_legacy_intake_panel
  public :: write_legacy_intrados_panel
  public :: write_legacy_production_boundary
  public :: write_legacy_neutral_boundary
  public :: derive_neutral_boundary_edge
  public :: polyline_length_2d
  public :: neutral_panel_lower_edge_length
  public :: neutral_panel_higher_edge_length
  public :: neutral_boundary_edge_length
  public :: neutral_panel_edge_gap
  public :: geometry_values_are_close

contains

  !> Return true for a non-empty one-based inclusive point range.
  pure logical function index_range_is_valid(range) result(valid)
    class(index_range), intent(in) :: range

    valid = range%first >= 1 .and. range%last >= range%first
  end function index_range_is_valid

  !> Return the number of points in an inclusive range, or zero if invalid.
  pure integer function index_range_size(range) result(point_count)
    class(index_range), intent(in) :: range

    point_count = 0
    if (range%is_valid()) point_count = range%last - range%first + 1
  end function index_range_size

  !> Test the shared-endpoint topology of an ordered profile contour.
  pure logical function profile_topology_is_valid(topology) result(valid)
    class(profile_topology), intent(in) :: topology

    valid = .false.
    if (topology%point_count < 2) return
    if (.not. topology%extrados%is_valid()) return
    if (.not. topology%intake%is_valid()) return
    if (.not. topology%intrados%is_valid()) return
    if (topology%extrados%first /= 1) return
    if (topology%extrados%last /= topology%intake%first) return
    if (topology%intake%last /= topology%intrados%first) return
    if (topology%intrados%last /= topology%point_count) return
    if (topology%extrados%size() < 2) return
    if (topology%intake%size() < 2) return
    if (topology%intrados%size() < 2) return
    if (topology%leading_edge_index < 1) return
    if (topology%leading_edge_index > topology%point_count) return
    valid = .true.
  end function profile_topology_is_valid

  !> Test whether a rib descriptor uses one of the documented roles.
  pure logical function rib_identity_is_valid(identity) result(valid)
    class(rib_identity), intent(in) :: identity

    valid = .false.
    if (identity%legacy_index < 0) return
    if (identity%profile_source_index < 1) return
    if (identity%placement_anchor_index < 1) return
    select case (identity%role)
    case (rib_role_physical_centerline, rib_role_physical_center_adjacent)
      if (identity%legacy_index /= 1) return
      if (identity%profile_source_index /= identity%legacy_index) return
      if (identity%placement_anchor_index /= identity%legacy_index) return
    case (rib_role_physical_interior)
      if (identity%legacy_index < 2) return
      if (identity%profile_source_index /= identity%legacy_index) return
      if (identity%placement_anchor_index /= identity%legacy_index) return
    case (rib_role_physical_wingtip)
      if (identity%legacy_index < 2) return
      if (identity%profile_source_index /= identity%legacy_index) return
      if (identity%placement_anchor_index /= identity%legacy_index) return
    case (rib_role_symmetry_mirror_physical, &
          rib_role_symmetry_centerline_alias)
      if (identity%legacy_index /= 0) return
      if (identity%profile_source_index /= 1) return
      if (identity%placement_anchor_index /= 1) return
    case (rib_role_tip_extrapolated_support)
      if (identity%legacy_index <= 2) return
      if (identity%profile_source_index /= identity%legacy_index - 2) return
      if (identity%placement_anchor_index /= identity%legacy_index - 1) return
    case default
      return
    end select
    valid = .true.
  end function rib_identity_is_valid

  !> True only for rows read as physical ribs from the wing definition.
  pure logical function rib_identity_is_authored_physical(identity) &
      result(authored)
    class(rib_identity), intent(in) :: identity

    authored = identity%role >= rib_role_physical_centerline .and. &
        identity%role <= rib_role_physical_wingtip
  end function rib_identity_is_authored_physical

  !> True for rows consumed by any legacy geometry calculation.
  !! This includes the collapsed centerline alias and the extrapolated support
  !! used by structural-feature geometry near the wingtip.
  pure logical function rib_identity_participates_in_legacy_geometry(identity) &
      result(participates)
    class(rib_identity), intent(in) :: identity

    participates = identity%is_authored_physical() .or. &
        identity%role == rib_role_symmetry_mirror_physical .or. &
        identity%role == rib_role_symmetry_centerline_alias .or. &
        identity%role == rib_role_tip_extrapolated_support
  end function rib_identity_participates_in_legacy_geometry

  !> True when the row bounds a nondegenerate manufacturing skin panel.
  pure logical function rib_identity_bounds_active_skin_panel(identity) &
      result(boundary)
    class(rib_identity), intent(in) :: identity

    boundary = identity%is_authored_physical() .or. &
        identity%role == rib_role_symmetry_mirror_physical
  end function rib_identity_bounds_active_skin_panel

  !> True for authored semi-wing ribs emitted before 3D mirroring.
  pure logical function rib_identity_is_output_half_rib(identity) &
      result(output_rib)
    class(rib_identity), intent(in) :: identity

    output_rib = identity%is_authored_physical()
  end function rib_identity_is_output_half_rib

  !> True only for the generated extrapolation beyond the physical wingtip.
  pure logical function rib_identity_is_tip_support(identity) result(support)
    class(rib_identity), intent(in) :: identity

    support = identity%role == rib_role_tip_extrapolated_support
  end function rib_identity_is_tip_support

  !> Test whether a normalized profile satisfies all documented invariants.
  pure logical function normalized_profile_is_valid(profile) result(valid)
    class(normalized_profile_2d), intent(in) :: profile

    valid = .false.
    if (profile%rib_index < 0) return
    if (.not. profile%topology%is_valid()) return
    if (.not. allocated(profile%chord_percent)) return
    if (.not. allocated(profile%height_percent)) return
    if (size(profile%chord_percent) < 2) return
    if (size(profile%height_percent) /= size(profile%chord_percent)) return
    if (size(profile%chord_percent) /= profile%topology%point_count) return
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
    if (lbound(geometry%x, 1) /= 1) return
    if (lbound(geometry%y, 1) /= 1) return
    if (lbound(geometry%z, 1) /= 1) return
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

  !> Test whether one physical terminal production edge is self-consistent.
  pure logical function production_boundary_edge_is_valid(edge) result(valid)
    class(production_boundary_edge_2d), intent(in) :: edge
    integer :: point_count

    valid = .false.
    if (edge%source_panel_index < 0) return
    if (edge%boundary_rib_index /= edge%source_panel_index + 1) return
    if (edge%surface /= surface_extrados .and. &
        edge%surface /= surface_intrados) return
    if (edge%contour_first_index < 1) return
    if (edge%contour_last_index < edge%contour_first_index) return
    point_count = edge%contour_last_index - edge%contour_first_index + 1
    if (point_count < 2) return
    if (.not. allocated(edge%sewing_u)) return
    if (.not. allocated(edge%sewing_v)) return
    if (.not. allocated(edge%cut_u)) return
    if (.not. allocated(edge%cut_v)) return
    if (any([lbound(edge%sewing_u, 1), lbound(edge%sewing_v, 1), &
        lbound(edge%cut_u, 1), lbound(edge%cut_v, 1)] /= 1)) return
    if (size(edge%sewing_u) /= point_count) return
    if (size(edge%sewing_v) /= point_count) return
    if (size(edge%cut_u) /= point_count) return
    if (size(edge%cut_v) /= point_count) return
    if (.not. all(ieee_is_finite(edge%sewing_u))) return
    if (.not. all(ieee_is_finite(edge%sewing_v))) return
    if (.not. all(ieee_is_finite(edge%cut_u))) return
    if (.not. all(ieee_is_finite(edge%cut_v))) return
    valid = .true.
  end function production_boundary_edge_is_valid

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

  !> Test whether a neutral panel owns one coherent pair of developed edges.
  pure logical function neutral_panel_is_valid(panel) result(valid)
    class(neutral_panel_2d), intent(in) :: panel
    integer :: point_count, segment_index
    real(real64) :: expected_lower_join_gap, expected_higher_join_gap
    real(real64) :: expected_support_lower_gap, expected_support_higher_gap

    valid = .false.
    if (panel%panel_index < 0) return
    if (panel%lower_rib_index /= panel%panel_index) return
    if (panel%higher_rib_index /= panel%panel_index + 1) return
    if (panel%surface < surface_extrados .or. &
        panel%surface > surface_intrados) return
    if (panel%contour_first_index < 1) return
    if (panel%contour_last_index < panel%contour_first_index) return
    point_count = panel%contour_last_index - panel%contour_first_index + 1
    if (point_count < 2) return
    if (.not. allocated(panel%lower_start_biased_u)) return
    if (.not. allocated(panel%lower_start_biased_v)) return
    if (.not. allocated(panel%higher_start_biased_u)) return
    if (.not. allocated(panel%higher_start_biased_v)) return
    if (size(panel%lower_start_biased_u) /= point_count) return
    if (size(panel%lower_start_biased_v) /= point_count) return
    if (size(panel%higher_start_biased_u) /= point_count) return
    if (size(panel%higher_start_biased_v) /= point_count) return
    if (.not. allocated(panel%lower_segment_start_u)) return
    if (.not. allocated(panel%lower_segment_start_v)) return
    if (.not. allocated(panel%lower_segment_end_u)) return
    if (.not. allocated(panel%lower_segment_end_v)) return
    if (.not. allocated(panel%higher_segment_start_u)) return
    if (.not. allocated(panel%higher_segment_start_v)) return
    if (.not. allocated(panel%higher_segment_end_u)) return
    if (.not. allocated(panel%higher_segment_end_v)) return
    if (any([lbound(panel%lower_start_biased_u, 1), &
        lbound(panel%lower_start_biased_v, 1), &
        lbound(panel%higher_start_biased_u, 1), &
        lbound(panel%higher_start_biased_v, 1), &
        lbound(panel%lower_segment_start_u, 1), &
        lbound(panel%lower_segment_start_v, 1), &
        lbound(panel%lower_segment_end_u, 1), &
        lbound(panel%lower_segment_end_v, 1), &
        lbound(panel%higher_segment_start_u, 1), &
        lbound(panel%higher_segment_start_v, 1), &
        lbound(panel%higher_segment_end_u, 1), &
        lbound(panel%higher_segment_end_v, 1)] /= 1)) return
    if (size(panel%lower_segment_start_u) /= point_count - 1) return
    if (size(panel%lower_segment_start_v) /= point_count - 1) return
    if (size(panel%lower_segment_end_u) /= point_count - 1) return
    if (size(panel%lower_segment_end_v) /= point_count - 1) return
    if (size(panel%higher_segment_start_u) /= point_count - 1) return
    if (size(panel%higher_segment_start_v) /= point_count - 1) return
    if (size(panel%higher_segment_end_u) /= point_count - 1) return
    if (size(panel%higher_segment_end_v) /= point_count - 1) return
    if (.not. all(ieee_is_finite(panel%lower_start_biased_u))) return
    if (.not. all(ieee_is_finite(panel%lower_start_biased_v))) return
    if (.not. all(ieee_is_finite(panel%higher_start_biased_u))) return
    if (.not. all(ieee_is_finite(panel%higher_start_biased_v))) return
    if (.not. all(ieee_is_finite(panel%lower_segment_start_u))) return
    if (.not. all(ieee_is_finite(panel%lower_segment_start_v))) return
    if (.not. all(ieee_is_finite(panel%lower_segment_end_u))) return
    if (.not. all(ieee_is_finite(panel%lower_segment_end_v))) return
    if (.not. all(ieee_is_finite(panel%higher_segment_start_u))) return
    if (.not. all(ieee_is_finite(panel%higher_segment_start_v))) return
    if (.not. all(ieee_is_finite(panel%higher_segment_end_u))) return
    if (.not. all(ieee_is_finite(panel%higher_segment_end_v))) return
    if (.not. ieee_is_finite(panel%maximum_lower_join_gap)) return
    if (.not. ieee_is_finite(panel%maximum_higher_join_gap)) return
    if (panel%maximum_lower_join_gap < 0.0_real64) return
    if (panel%maximum_higher_join_gap < 0.0_real64) return
    do segment_index = 1, point_count - 1
      if (.not. points_match(panel%lower_start_biased_u(segment_index), &
          panel%lower_start_biased_v(segment_index), &
          panel%lower_segment_start_u(segment_index), &
          panel%lower_segment_start_v(segment_index))) return
      if (.not. points_match(panel%higher_start_biased_u(segment_index), &
          panel%higher_start_biased_v(segment_index), &
          panel%higher_segment_start_u(segment_index), &
          panel%higher_segment_start_v(segment_index))) return
    end do
    if (.not. points_match(panel%lower_start_biased_u(point_count), &
        panel%lower_start_biased_v(point_count), &
        panel%lower_segment_end_u(point_count - 1), &
        panel%lower_segment_end_v(point_count - 1))) return
    if (.not. points_match(panel%higher_start_biased_u(point_count), &
        panel%higher_start_biased_v(point_count), &
        panel%higher_segment_end_u(point_count - 1), &
        panel%higher_segment_end_v(point_count - 1))) return
    expected_lower_join_gap = 0.0_real64
    expected_higher_join_gap = 0.0_real64
    do segment_index = 2, point_count - 1
      expected_lower_join_gap = max(expected_lower_join_gap, hypot( &
          panel%lower_segment_start_u(segment_index) - &
          panel%lower_segment_end_u(segment_index - 1), &
          panel%lower_segment_start_v(segment_index) - &
          panel%lower_segment_end_v(segment_index - 1)))
      expected_higher_join_gap = max(expected_higher_join_gap, hypot( &
          panel%higher_segment_start_u(segment_index) - &
          panel%higher_segment_end_u(segment_index - 1), &
          panel%higher_segment_start_v(segment_index) - &
          panel%higher_segment_end_v(segment_index - 1)))
    end do
    if (.not. geometry_values_are_close(panel%maximum_lower_join_gap, &
        expected_lower_join_gap)) return
    if (.not. geometry_values_are_close(panel%maximum_higher_join_gap, &
        expected_higher_join_gap)) return
    if (panel%has_post_surface_support .neqv. &
        (panel%surface == surface_intake)) return
    if (panel%has_post_surface_support) then
      if (.not. ieee_is_finite(panel%support_lower_start_u)) return
      if (.not. ieee_is_finite(panel%support_lower_start_v)) return
      if (.not. ieee_is_finite(panel%support_lower_end_u)) return
      if (.not. ieee_is_finite(panel%support_lower_end_v)) return
      if (.not. ieee_is_finite(panel%support_higher_start_u)) return
      if (.not. ieee_is_finite(panel%support_higher_start_v)) return
      if (.not. ieee_is_finite(panel%support_higher_end_u)) return
      if (.not. ieee_is_finite(panel%support_higher_end_v)) return
      if (.not. ieee_is_finite(panel%support_lower_join_gap)) return
      if (.not. ieee_is_finite(panel%support_higher_join_gap)) return
      if (panel%support_lower_join_gap < 0.0_real64) return
      if (panel%support_higher_join_gap < 0.0_real64) return
      expected_support_lower_gap = hypot(panel%support_lower_start_u - &
          panel%lower_start_biased_u(point_count), &
          panel%support_lower_start_v - &
          panel%lower_start_biased_v(point_count))
      expected_support_higher_gap = hypot(panel%support_higher_start_u - &
          panel%higher_start_biased_u(point_count), &
          panel%support_higher_start_v - &
          panel%higher_start_biased_v(point_count))
      if (.not. geometry_values_are_close(panel%support_lower_join_gap, &
          expected_support_lower_gap)) return
      if (.not. geometry_values_are_close(panel%support_higher_join_gap, &
          expected_support_higher_gap)) return
    else
      if (.not. geometry_values_are_close(panel%support_lower_join_gap, &
          0.0_real64)) return
      if (.not. geometry_values_are_close(panel%support_higher_join_gap, &
          0.0_real64)) return
    end if
    valid = .true.
  end function neutral_panel_is_valid

  !> Test the provenance and exact-segment invariants of a terminal edge.
  pure logical function neutral_boundary_edge_is_valid(edge) result(valid)
    class(neutral_boundary_edge_2d), intent(in) :: edge
    integer :: point_count, segment_index
    real(real64) :: expected_join_gap, expected_support_gap

    valid = .false.
    if (edge%source_panel_index < 0) return
    if (edge%boundary_rib_index /= edge%source_panel_index + 1) return
    if (edge%surface < surface_extrados .or. &
        edge%surface > surface_intrados) return
    if (edge%contour_first_index < 1) return
    if (edge%contour_last_index < edge%contour_first_index) return
    point_count = edge%contour_last_index - edge%contour_first_index + 1
    if (point_count < 2) return
    if (.not. allocated(edge%start_biased_u)) return
    if (.not. allocated(edge%start_biased_v)) return
    if (.not. allocated(edge%segment_start_u)) return
    if (.not. allocated(edge%segment_start_v)) return
    if (.not. allocated(edge%segment_end_u)) return
    if (.not. allocated(edge%segment_end_v)) return
    if (any([lbound(edge%start_biased_u, 1), &
        lbound(edge%start_biased_v, 1), &
        lbound(edge%segment_start_u, 1), &
        lbound(edge%segment_start_v, 1), &
        lbound(edge%segment_end_u, 1), &
        lbound(edge%segment_end_v, 1)] /= 1)) return
    if (size(edge%start_biased_u) /= point_count) return
    if (size(edge%start_biased_v) /= point_count) return
    if (size(edge%segment_start_u) /= point_count - 1) return
    if (size(edge%segment_start_v) /= point_count - 1) return
    if (size(edge%segment_end_u) /= point_count - 1) return
    if (size(edge%segment_end_v) /= point_count - 1) return
    if (.not. all(ieee_is_finite(edge%start_biased_u))) return
    if (.not. all(ieee_is_finite(edge%start_biased_v))) return
    if (.not. all(ieee_is_finite(edge%segment_start_u))) return
    if (.not. all(ieee_is_finite(edge%segment_start_v))) return
    if (.not. all(ieee_is_finite(edge%segment_end_u))) return
    if (.not. all(ieee_is_finite(edge%segment_end_v))) return
    if (.not. ieee_is_finite(edge%maximum_join_gap)) return
    if (edge%maximum_join_gap < 0.0_real64) return
    do segment_index = 1, point_count - 1
      if (.not. points_match(edge%start_biased_u(segment_index), &
          edge%start_biased_v(segment_index), &
          edge%segment_start_u(segment_index), &
          edge%segment_start_v(segment_index))) return
    end do
    if (.not. points_match(edge%start_biased_u(point_count), &
        edge%start_biased_v(point_count), &
        edge%segment_end_u(point_count - 1), &
        edge%segment_end_v(point_count - 1))) return
    expected_join_gap = 0.0_real64
    do segment_index = 2, point_count - 1
      expected_join_gap = max(expected_join_gap, hypot( &
          edge%segment_start_u(segment_index) - &
          edge%segment_end_u(segment_index - 1), &
          edge%segment_start_v(segment_index) - &
          edge%segment_end_v(segment_index - 1)))
    end do
    if (.not. geometry_values_are_close(edge%maximum_join_gap, &
        expected_join_gap)) return
    if (edge%has_post_surface_support .neqv. &
        (edge%surface == surface_intake)) return
    if (edge%has_post_surface_support) then
      if (.not. ieee_is_finite(edge%support_start_u)) return
      if (.not. ieee_is_finite(edge%support_start_v)) return
      if (.not. ieee_is_finite(edge%support_end_u)) return
      if (.not. ieee_is_finite(edge%support_end_v)) return
      if (.not. ieee_is_finite(edge%support_join_gap)) return
      if (edge%support_join_gap < 0.0_real64) return
      expected_support_gap = hypot(edge%support_start_u - &
          edge%start_biased_u(point_count), edge%support_start_v - &
          edge%start_biased_v(point_count))
      if (.not. geometry_values_are_close(edge%support_join_gap, &
          expected_support_gap)) return
    else
      if (.not. geometry_values_are_close(edge%support_join_gap, &
          0.0_real64)) return
    end if
    valid = .true.
  end function neutral_boundary_edge_is_valid

  !> Derive a terminal boundary from a real panel's higher neutral edge.
  !!
  !! The operation is transactional.  The source panel remains unchanged and
  !! the destination is replaced only after its provenance, exact segments,
  !! join diagnostics, and optional intake support all validate.
  pure subroutine derive_neutral_boundary_edge(source_panel, edge, valid, &
      message)
    type(neutral_panel_2d), intent(in) :: source_panel
    type(neutral_boundary_edge_2d), intent(inout) :: edge
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(neutral_boundary_edge_2d) :: candidate
    integer :: point_count, segment_count

    valid = .false.
    message = ''
    if (.not. source_panel%is_valid()) then
      message = 'terminal boundary received an invalid source panel'
      return
    end if

    candidate%source_panel_index = source_panel%panel_index
    candidate%boundary_rib_index = source_panel%higher_rib_index
    candidate%surface = source_panel%surface
    candidate%contour_first_index = source_panel%contour_first_index
    candidate%contour_last_index = source_panel%contour_last_index
    candidate%maximum_join_gap = source_panel%maximum_higher_join_gap
    candidate%has_post_surface_support = &
        source_panel%has_post_surface_support
    point_count = candidate%contour_last_index - &
        candidate%contour_first_index + 1
    segment_count = point_count - 1
    allocate(candidate%start_biased_u(point_count), &
        candidate%start_biased_v(point_count), &
        candidate%segment_start_u(segment_count), &
        candidate%segment_start_v(segment_count), &
        candidate%segment_end_u(segment_count), &
        candidate%segment_end_v(segment_count))
    candidate%start_biased_u = source_panel%higher_start_biased_u
    candidate%start_biased_v = source_panel%higher_start_biased_v
    candidate%segment_start_u = source_panel%higher_segment_start_u
    candidate%segment_start_v = source_panel%higher_segment_start_v
    candidate%segment_end_u = source_panel%higher_segment_end_u
    candidate%segment_end_v = source_panel%higher_segment_end_v
    if (candidate%has_post_surface_support) then
      candidate%support_start_u = source_panel%support_higher_start_u
      candidate%support_start_v = source_panel%support_higher_start_v
      candidate%support_end_u = source_panel%support_higher_end_u
      candidate%support_end_v = source_panel%support_higher_end_v
      candidate%support_join_gap = source_panel%support_higher_join_gap
    end if
    if (.not. candidate%is_valid()) then
      message = 'derived terminal boundary failed validation'
      return
    end if

    edge = candidate
    valid = .true.
  end subroutine derive_neutral_boundary_edge

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

  !> Copy and validate one row of the legacy `np(:,1:6)` profile schema.
  !!
  !! Columns 2/3/4 are inclusive point counts for extrados, intake, and
  !! intrados. Columns 2 and 5 are shared endpoints. Redundant columns are
  !! checked rather than normalized silently so malformed producers remain
  !! visible. On failure the destination is unchanged.
  subroutine copy_legacy_profile_topology(legacy_np, rib_index, topology, &
      valid, message)
    integer, intent(in) :: legacy_np(0:,:)
    integer, intent(in) :: rib_index
    type(profile_topology), intent(inout) :: topology
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(profile_topology) :: candidate
    integer :: extrados_count, intake_count, intrados_count
    integer :: expected_intake_end, expected_point_count

    valid = .false.
    message = ''
    if (rib_index < 0 .or. rib_index > ubound(legacy_np, 1)) then
      message = 'rib index is outside the legacy topology array'
      return
    end if
    if (size(legacy_np, 2) < 6) then
      message = 'legacy topology array has fewer than six columns'
      return
    end if

    extrados_count = legacy_np(rib_index, 2)
    intake_count = legacy_np(rib_index, 3)
    intrados_count = legacy_np(rib_index, 4)
    expected_intake_end = extrados_count + intake_count - 1
    expected_point_count = extrados_count + intake_count + &
        intrados_count - 2
    if (legacy_np(rib_index, 5) /= expected_intake_end) then
      message = 'legacy intake endpoint disagrees with surface counts'
      return
    end if
    if (legacy_np(rib_index, 1) /= expected_point_count) then
      message = 'legacy total point count disagrees with surface counts'
      return
    end if
    candidate%point_count = legacy_np(rib_index, 1)
    candidate%extrados = index_range(1, extrados_count)
    candidate%intake = index_range(extrados_count, expected_intake_end)
    candidate%intrados = index_range(expected_intake_end, &
        candidate%point_count)
    candidate%leading_edge_index = legacy_np(rib_index, 6)
    if (.not. candidate%is_valid()) then
      message = 'legacy profile partitions or leading edge are invalid'
      return
    end if

    topology = candidate
    valid = .true.
  end subroutine copy_legacy_profile_topology

  !> Return true when two profiles use identical contour index partitions.
  !! Leading-edge samples may differ because they are geometric observations,
  !! not a requirement of stage-7 quadrilateral pairing.
  pure logical function topologies_are_index_compatible(first, second) &
      result(compatible)
    type(profile_topology), intent(in) :: first, second

    compatible = first%is_valid() .and. second%is_valid() .and. &
        first%point_count == second%point_count .and. &
        first%extrados%first == second%extrados%first .and. &
        first%extrados%last == second%extrados%last .and. &
        first%intake%first == second%intake%first .and. &
        first%intake%last == second%intake%last .and. &
        first%intrados%first == second%intrados%first .and. &
        first%intrados%last == second%intrados%last
  end function topologies_are_index_compatible

  !> Return true when two valid profiles can pair extrados samples by index.
  !!
  !! Stage 7 permits adjacent profiles to have different intake/intrados point
  !! counts.  Extrados development therefore requires only the extrados range
  !! itself to match; full-topology compatibility is intentionally stronger.
  pure logical function extrados_topologies_are_index_compatible(first, &
      second) result(compatible)
    type(profile_topology), intent(in) :: first, second

    compatible = first%is_valid() .and. second%is_valid() .and. &
        first%extrados%first == second%extrados%first .and. &
        first%extrados%last == second%extrados%last
  end function extrados_topologies_are_index_compatible

  !> Infer explicit roles for authored and generated legacy rib rows.
  !!
  !! The legacy central-cell width and its 0.01 threshold determine whether row
  !! zero is a finite-width mirrored boundary or a degenerate centerline alias.
  !! Declared cell parity is reported separately as a consistency diagnostic;
  !! legacy inputs with a zero-thickness odd central cell remain accepted.
  !! The returned allocatable array has bounds `0:physical_rib_count+1` and is
  !! replaced only after all role and geometry checks succeed.
  subroutine infer_legacy_rib_identities(cell_count, physical_rib_count, &
      central_cell_width, planform_station, spatial_station, spatial_height, &
      identities, panel_zero_active, parity_consistent, valid, message)
    integer, intent(in) :: cell_count, physical_rib_count
    real(real64), intent(in) :: central_cell_width
    real(real64), intent(in) :: planform_station(0:)
    real(real64), intent(in) :: spatial_station(0:), spatial_height(0:)
    type(rib_identity), allocatable, intent(inout) :: identities(:)
    logical, intent(out) :: panel_zero_active, parity_consistent, valid
    character(len=*), intent(out) :: message

    type(rib_identity), allocatable :: candidate(:)
    integer :: rib_index, expected_rib_count
    logical :: odd_cell_topology

    valid = .false.
    panel_zero_active = .false.
    parity_consistent = .false.
    message = ''
    if (cell_count < 2) then
      message = 'cell count must be at least two'
      return
    end if
    if (physical_rib_count < 2) then
      message = 'physical semi-wing must contain at least two ribs'
      return
    end if
    expected_rib_count = cell_count / 2 + 1
    if (physical_rib_count /= expected_rib_count) then
      message = 'physical rib count disagrees with cell topology'
      return
    end if
    if (ubound(planform_station, 1) < physical_rib_count + 1 .or. &
        ubound(spatial_station, 1) < physical_rib_count + 1 .or. &
        ubound(spatial_height, 1) < physical_rib_count + 1) then
      message = 'legacy rib arrays do not include both generated rows'
      return
    end if
    if (.not. all(ieee_is_finite( &
        planform_station(0:physical_rib_count + 1)))) then
      message = 'planform station contains a non-finite value'
      return
    end if
    if (.not. all(ieee_is_finite( &
        spatial_station(0:physical_rib_count + 1)))) then
      message = 'spatial station contains a non-finite value'
      return
    end if
    if (.not. all(ieee_is_finite( &
        spatial_height(0:physical_rib_count + 1)))) then
      message = 'spatial height contains a non-finite value'
      return
    end if
    if (.not. ieee_is_finite(central_cell_width)) then
      message = 'central-cell width is non-finite'
      return
    end if

    odd_cell_topology = mod(cell_count, 2) == 1
    panel_zero_active = central_cell_width >= legacy_central_panel_threshold
    parity_consistent = odd_cell_topology .eqv. panel_zero_active
    if (abs(planform_station(0) + planform_station(1)) > &
        geometry_tolerance * (1.0_real64 + abs(planform_station(1)))) then
      message = 'generated center row is not the planform mirror of rib one'
      return
    end if
    if (abs(spatial_station(0) + spatial_station(1)) > &
        geometry_tolerance * (1.0_real64 + abs(spatial_station(1)))) then
      message = 'generated center row is not the spatial mirror of rib one'
      return
    end if
    if (abs(spatial_station(physical_rib_count + 1) - &
        (2.0_real64 * spatial_station(physical_rib_count) - &
        spatial_station(physical_rib_count - 1))) > &
        geometry_tolerance * (1.0_real64 + &
        abs(spatial_station(physical_rib_count + 1)))) then
      message = 'tip support station is not extrapolated through the wingtip'
      return
    end if
    if (abs(spatial_height(physical_rib_count + 1) - &
        (2.0_real64 * spatial_height(physical_rib_count) - &
        spatial_height(physical_rib_count - 1))) > &
        geometry_tolerance * (1.0_real64 + &
        abs(spatial_height(physical_rib_count + 1)))) then
      message = 'tip support height is not extrapolated through the wingtip'
      return
    end if

    allocate(candidate(0:physical_rib_count + 1))
    candidate(0)%legacy_index = 0
    candidate(0)%profile_source_index = 1
    candidate(0)%placement_anchor_index = 1
    candidate(1)%legacy_index = 1
    candidate(1)%profile_source_index = 1
    candidate(1)%placement_anchor_index = 1
    if (panel_zero_active) then
      candidate(0)%role = rib_role_symmetry_mirror_physical
      candidate(1)%role = rib_role_physical_center_adjacent
    else
      candidate(0)%role = rib_role_symmetry_centerline_alias
      candidate(1)%role = rib_role_physical_centerline
    end if
    do rib_index = 2, physical_rib_count - 1
      candidate(rib_index)%legacy_index = rib_index
      candidate(rib_index)%role = rib_role_physical_interior
      candidate(rib_index)%profile_source_index = rib_index
      candidate(rib_index)%placement_anchor_index = rib_index
    end do
    rib_index = physical_rib_count
    candidate(rib_index)%legacy_index = rib_index
    candidate(rib_index)%role = rib_role_physical_wingtip
    candidate(rib_index)%profile_source_index = rib_index
    candidate(rib_index)%placement_anchor_index = rib_index
    rib_index = physical_rib_count + 1
    candidate(rib_index)%legacy_index = rib_index
    candidate(rib_index)%role = rib_role_tip_extrapolated_support
    candidate(rib_index)%profile_source_index = physical_rib_count - 1
    candidate(rib_index)%placement_anchor_index = physical_rib_count
    do rib_index = 0, physical_rib_count + 1
      if (.not. candidate(rib_index)%is_valid()) then
        panel_zero_active = .false.
        parity_consistent = .false.
        message = 'inferred rib identity is internally inconsistent'
        return
      end if
    end do

    call move_alloc(candidate, identities)
    valid = .true.
  end subroutine infer_legacy_rib_identities

  !> Copy one normalized rib profile out of the legacy `u/v` slot arrays.
  !!
  !! The first legacy dimension must use the historical zero-based rib bound.
  !! On success `profile` is replaced atomically with an independent copy.  On
  !! failure `profile` is unchanged, `valid` is false, and `message` explains
  !! the rejected invariant.
  !!
  !! @param[in] legacy_u,legacy_v Historical coordinate arrays.
  !! @param[in] rib_index Zero-based rib index to copy.
  !! @param[in] topology Valid named topology controlling the copied extent.
  !! @param[inout] profile Typed destination; unchanged on failure.
  !! @param[out] valid True only when a complete valid profile was copied.
  !! @param[out] message Empty on success; diagnostic text on failure.
  subroutine copy_legacy_normalized_profile(legacy_u, legacy_v, rib_index, &
      topology, profile, valid, message)
    real(real64), intent(in) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    integer, intent(in) :: rib_index
    type(profile_topology), intent(in) :: topology
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
    if (.not. topology%is_valid()) then
      message = 'normalized profile topology is invalid'
      return
    end if
    if (topology%point_count > size(legacy_u, 2)) then
      message = 'profile point count is outside the legacy array'
      return
    end if
    if (legacy_normalized_profile_slot > size(legacy_u, 3)) then
      message = 'legacy array has no normalized-profile slot'
      return
    end if

    candidate%rib_index = rib_index
    candidate%topology = topology
    candidate%chord_percent = legacy_u(rib_index, &
        1:topology%point_count, legacy_normalized_profile_slot)
    candidate%height_percent = legacy_v(rib_index, &
        1:topology%point_count, legacy_normalized_profile_slot)
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
  !! @param[in] profile_topologies Validated stage-6 profile topology table.
  !! @param[in] panel_index Zero-based panel/left-rib index.
  !! @param[inout] panel Typed destination; unchanged on failure.
  !! @param[out] valid True only when the entire panel was copied.
  !! @param[out] message Empty on success; diagnostic text on failure.
  subroutine copy_legacy_production_panel(legacy_u, legacy_v, &
      profile_topologies, panel_index, panel, valid, message)
    real(real64), intent(in) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    type(profile_topology), intent(in) :: profile_topologies(0:)
    integer, intent(in) :: panel_index
    type(production_panel_2d), intent(inout) :: panel
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(production_panel_2d) :: candidate
    type(profile_topology) :: lower_topology, higher_topology

    valid = .false.
    message = ''
    if (panel_index < 0 .or. &
        panel_index + 1 > ubound(profile_topologies, 1)) then
      message = 'panel index is outside the typed topology table'
      return
    end if
    candidate%panel_index = panel_index
    lower_topology = profile_topologies(panel_index)
    higher_topology = profile_topologies(panel_index + 1)
    if (.not. lower_topology%is_valid() .or. &
        .not. higher_topology%is_valid()) then
      message = 'production panel received invalid profile topology'
      return
    end if
    call copy_legacy_normalized_profile(legacy_u, legacy_v, panel_index, &
        lower_topology, candidate%lower_profile, valid, message)
    if (.not. valid) return
    call copy_legacy_normalized_profile(legacy_u, legacy_v, &
        panel_index + 1, higher_topology, candidate%higher_profile, &
        valid, message)
    if (.not. valid) return
    call copy_legacy_production_panel_edges(legacy_u, legacy_v, panel_index, &
        lower_topology%point_count, lower_topology%point_count, &
        candidate%edges, valid, &
        message)
    if (.not. valid) return
    if (.not. candidate%is_valid()) then
      valid = .false.
      message = 'complete developed panel has inconsistent source domains'
      return
    end if

    panel = candidate
    valid = .true.
  end subroutine copy_legacy_production_panel

  !> Publish one physical terminal production edge to legacy slots 9 and 11.
  !!
  !! The boundary row represents the outward side of the physical wingtip, not
  !! a panel beyond it.  Consequently this adapter writes only the legacy
  !! lower sewing/cut slots over the exact surface contour.  Slots 10 and 12,
  !! all other rows and points, and all neutral `pl*/pr*` arrays remain outside
  !! its ownership.  Every precondition is checked before the first assignment,
  !! so rejected calls leave both destination arrays unchanged.  `legacy_u` and
  !! `legacy_v` must be distinct actual arguments.
  pure subroutine write_legacy_production_boundary(boundary, topology, &
      legacy_u, legacy_v, valid, message)
    type(production_boundary_edge_2d), intent(in) :: boundary
    type(profile_topology), intent(in) :: topology
    real(real64), intent(inout) :: legacy_u(0:,:,:), legacy_v(0:,:,:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(index_range) :: contour_range

    valid = .false.
    message = ''
    if (.not. legacy_shapes_match(legacy_u, legacy_v)) then
      message = 'legacy U/V production-boundary destination shapes differ'
      return
    end if
    if (.not. boundary%is_valid()) then
      message = 'cannot write an invalid production boundary'
      return
    end if
    if (.not. topology%is_valid()) then
      message = 'production-boundary write-back received invalid topology'
      return
    end if
    select case (boundary%surface)
    case (surface_extrados)
      contour_range = topology%extrados
    case (surface_intrados)
      contour_range = topology%intrados
    case default
      message = 'production-boundary write-back does not support this surface'
      return
    end select
    if (boundary%contour_first_index /= contour_range%first .or. &
        boundary%contour_last_index /= contour_range%last) then
      message = 'production boundary range differs from source topology'
      return
    end if
    if (boundary%boundary_rib_index < lbound(legacy_u, 1) .or. &
        boundary%boundary_rib_index > ubound(legacy_u, 1)) then
      message = 'production boundary rib is outside legacy destinations'
      return
    end if
    if (boundary%contour_first_index < lbound(legacy_u, 2) .or. &
        boundary%contour_last_index > ubound(legacy_u, 2)) then
      message = 'production boundary exceeds legacy point capacity'
      return
    end if
    if (legacy_production_lower_sewing_slot < lbound(legacy_u, 3) .or. &
        legacy_production_lower_cut_slot > ubound(legacy_u, 3)) then
      message = 'legacy array has no terminal production-boundary slots'
      return
    end if

    legacy_u(boundary%boundary_rib_index, &
        boundary%contour_first_index:boundary%contour_last_index, &
        legacy_production_lower_sewing_slot) = boundary%sewing_u
    legacy_v(boundary%boundary_rib_index, &
        boundary%contour_first_index:boundary%contour_last_index, &
        legacy_production_lower_sewing_slot) = boundary%sewing_v
    legacy_u(boundary%boundary_rib_index, &
        boundary%contour_first_index:boundary%contour_last_index, &
        legacy_production_lower_cut_slot) = boundary%cut_u
    legacy_v(boundary%boundary_rib_index, &
        boundary%contour_first_index:boundary%contour_last_index, &
        legacy_production_lower_cut_slot) = boundary%cut_v
    valid = .true.
  end subroutine write_legacy_production_boundary

  !> Publish one exact terminal neutral edge to the legacy lower segment row.
  !!
  !! Stage 7 owns only real panels, so it intentionally does not construct a
  !! fictitious panel at `boundary_rib_index`. Legacy Stage 8 nevertheless
  !! expects the physical terminal edge in that row's `pl1/pl2` arrays. This
  !! adapter supplies only those exact segment endpoints and leaves every
  !! other row, point, and higher-side `pr*` array outside its ownership.
  pure subroutine write_legacy_neutral_boundary(edge, legacy_start_u, &
      legacy_start_v, legacy_end_u, legacy_end_v, valid, message)
    type(neutral_boundary_edge_2d), intent(in) :: edge
    real(real64), intent(inout) :: legacy_start_u(0:,:), legacy_start_v(0:,:)
    real(real64), intent(inout) :: legacy_end_u(0:,:), legacy_end_v(0:,:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message
    integer :: write_last_index

    valid = .false.
    message = ''
    if (.not. all(shape(legacy_start_u) == shape(legacy_start_v)) .or. &
        .not. all(shape(legacy_start_u) == shape(legacy_end_u)) .or. &
        .not. all(shape(legacy_start_u) == shape(legacy_end_v))) then
      message = 'terminal neutral-edge destination shapes differ'
      return
    end if
    if (.not. edge%is_valid()) then
      message = 'cannot write an invalid terminal neutral edge'
      return
    end if
    if (edge%surface /= surface_extrados .and. &
        edge%surface /= surface_intrados) then
      message = 'terminal neutral write-back supports skin surfaces only'
      return
    end if
    if (edge%boundary_rib_index < lbound(legacy_start_u, 1) .or. &
        edge%boundary_rib_index > ubound(legacy_start_u, 1)) then
      message = 'terminal neutral boundary rib is outside destination'
      return
    end if
    write_last_index = edge%contour_last_index - 1
    if (edge%contour_first_index < lbound(legacy_start_u, 2) .or. &
        write_last_index > ubound(legacy_start_u, 2)) then
      message = 'terminal neutral segments exceed destination capacity'
      return
    end if

    legacy_start_u(edge%boundary_rib_index, &
        edge%contour_first_index:write_last_index) = edge%segment_start_u
    legacy_start_v(edge%boundary_rib_index, &
        edge%contour_first_index:write_last_index) = edge%segment_start_v
    legacy_end_u(edge%boundary_rib_index, &
        edge%contour_first_index:write_last_index) = edge%segment_end_u
    legacy_end_v(edge%boundary_rib_index, &
        edge%contour_first_index:write_last_index) = edge%segment_end_v
    valid = .true.
  end subroutine write_legacy_neutral_boundary

  !> Copy one neutral developed surface from legacy quadrilateral corners.
  !!
  !! Each legacy row stores segment endpoints as `pl1/pl2` for the
  !! lower-index rib and `pr1/pr2` for the higher-index rib. This adapter
  !! reconstructs point views, preserves exact segment endpoints, records the
  !! legacy triangulation's join gaps, and keeps the intake-only support tangent
  !! explicit.
  subroutine copy_legacy_neutral_panel(legacy_pl1_u, legacy_pl1_v, &
      legacy_pl2_u, legacy_pl2_v, legacy_pr1_u, legacy_pr1_v, &
      legacy_pr2_u, legacy_pr2_v, panel_index, lower_topology, &
      higher_topology, surface, panel, valid, message)
    real(real64), intent(in) :: legacy_pl1_u(0:,:), legacy_pl1_v(0:,:)
    real(real64), intent(in) :: legacy_pl2_u(0:,:), legacy_pl2_v(0:,:)
    real(real64), intent(in) :: legacy_pr1_u(0:,:), legacy_pr1_v(0:,:)
    real(real64), intent(in) :: legacy_pr2_u(0:,:), legacy_pr2_v(0:,:)
    integer, intent(in) :: panel_index, surface
    type(profile_topology), intent(in) :: lower_topology, higher_topology
    type(neutral_panel_2d), intent(inout) :: panel
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(neutral_panel_2d) :: candidate
    type(index_range) :: contour_range, higher_contour_range
    integer :: segment_index, source_index, local_index, point_count
    real(real64) :: lower_start_u, lower_start_v
    real(real64) :: higher_start_u, higher_start_v

    valid = .false.
    message = ''
    if (.not. neutral_legacy_shapes_match(legacy_pl1_u, legacy_pl1_v, &
        legacy_pl2_u, legacy_pl2_v, legacy_pr1_u, legacy_pr1_v, &
        legacy_pr2_u, legacy_pr2_v)) then
      message = 'legacy neutral-panel array shapes differ'
      return
    end if
    if (panel_index < 0 .or. panel_index > ubound(legacy_pl1_u, 1)) then
      message = 'panel index is outside the legacy neutral-panel arrays'
      return
    end if
    if (.not. lower_topology%is_valid() .or. &
        .not. higher_topology%is_valid()) then
      message = 'neutral panel received invalid profile topology'
      return
    end if
    select case (surface)
    case (surface_extrados)
      contour_range = lower_topology%extrados
      higher_contour_range = higher_topology%extrados
    case (surface_intake)
      contour_range = lower_topology%intake
      higher_contour_range = higher_topology%intake
    case (surface_intrados)
      contour_range = lower_topology%intrados
      higher_contour_range = higher_topology%intrados
    case default
      message = 'unknown neutral-panel surface'
      return
    end select
    if (contour_range%first /= higher_contour_range%first .or. &
        contour_range%last /= higher_contour_range%last) then
      message = 'adjacent profiles have incompatible surface indices'
      return
    end if
    if (contour_range%last > size(legacy_pl1_u, 2)) then
      message = 'surface contour exceeds neutral-panel point capacity'
      return
    end if

    candidate%panel_index = panel_index
    candidate%lower_rib_index = panel_index
    candidate%higher_rib_index = panel_index + 1
    candidate%surface = surface
    candidate%contour_first_index = contour_range%first
    candidate%contour_last_index = contour_range%last
    point_count = contour_range%size()
    allocate(candidate%lower_start_biased_u(point_count), &
        candidate%lower_start_biased_v(point_count))
    allocate(candidate%higher_start_biased_u(point_count), &
        candidate%higher_start_biased_v(point_count))
    allocate(candidate%lower_segment_start_u(point_count - 1), &
        candidate%lower_segment_start_v(point_count - 1), &
        candidate%lower_segment_end_u(point_count - 1), &
        candidate%lower_segment_end_v(point_count - 1))
    allocate(candidate%higher_segment_start_u(point_count - 1), &
        candidate%higher_segment_start_v(point_count - 1), &
        candidate%higher_segment_end_u(point_count - 1), &
        candidate%higher_segment_end_v(point_count - 1))

    do segment_index = contour_range%first, contour_range%last - 1
      local_index = segment_index - contour_range%first + 1
      source_index = segment_index

      lower_start_u = legacy_pl1_u(panel_index, source_index)
      lower_start_v = legacy_pl1_v(panel_index, source_index)
      higher_start_u = legacy_pr1_u(panel_index, source_index)
      higher_start_v = legacy_pr1_v(panel_index, source_index)
      if (local_index > 1) then
        candidate%maximum_lower_join_gap = max( &
            candidate%maximum_lower_join_gap, hypot(lower_start_u - &
            candidate%lower_segment_end_u(local_index - 1), lower_start_v - &
            candidate%lower_segment_end_v(local_index - 1)))
        candidate%maximum_higher_join_gap = max( &
            candidate%maximum_higher_join_gap, hypot(higher_start_u - &
            candidate%higher_segment_end_u(local_index - 1), &
            higher_start_v - &
            candidate%higher_segment_end_v(local_index - 1)))
      end if
      candidate%lower_start_biased_u(local_index) = lower_start_u
      candidate%lower_start_biased_v(local_index) = lower_start_v
      candidate%higher_start_biased_u(local_index) = higher_start_u
      candidate%higher_start_biased_v(local_index) = higher_start_v
      candidate%lower_segment_start_u(local_index) = lower_start_u
      candidate%lower_segment_start_v(local_index) = lower_start_v
      candidate%higher_segment_start_u(local_index) = higher_start_u
      candidate%higher_segment_start_v(local_index) = higher_start_v
      candidate%lower_start_biased_u(local_index + 1) = &
          legacy_pl2_u(panel_index, source_index)
      candidate%lower_start_biased_v(local_index + 1) = &
          legacy_pl2_v(panel_index, source_index)
      candidate%higher_start_biased_u(local_index + 1) = &
          legacy_pr2_u(panel_index, source_index)
      candidate%higher_start_biased_v(local_index + 1) = &
          legacy_pr2_v(panel_index, source_index)
      candidate%lower_segment_end_u(local_index) = &
          legacy_pl2_u(panel_index, source_index)
      candidate%lower_segment_end_v(local_index) = &
          legacy_pl2_v(panel_index, source_index)
      candidate%higher_segment_end_u(local_index) = &
          legacy_pr2_u(panel_index, source_index)
      candidate%higher_segment_end_v(local_index) = &
          legacy_pr2_v(panel_index, source_index)
    end do

    if (surface == surface_intake) then
      candidate%has_post_surface_support = .true.
      source_index = contour_range%last
      candidate%support_lower_start_u = &
          legacy_pl1_u(panel_index, source_index)
      candidate%support_lower_start_v = &
          legacy_pl1_v(panel_index, source_index)
      candidate%support_lower_end_u = &
          legacy_pl2_u(panel_index, source_index)
      candidate%support_lower_end_v = &
          legacy_pl2_v(panel_index, source_index)
      candidate%support_higher_start_u = &
          legacy_pr1_u(panel_index, source_index)
      candidate%support_higher_start_v = &
          legacy_pr1_v(panel_index, source_index)
      candidate%support_higher_end_u = &
          legacy_pr2_u(panel_index, source_index)
      candidate%support_higher_end_v = &
          legacy_pr2_v(panel_index, source_index)
      candidate%support_lower_join_gap = hypot( &
          candidate%support_lower_start_u - &
          candidate%lower_start_biased_u(point_count), &
          candidate%support_lower_start_v - &
          candidate%lower_start_biased_v(point_count))
      candidate%support_higher_join_gap = hypot( &
          candidate%support_higher_start_u - &
          candidate%higher_start_biased_u(point_count), &
          candidate%support_higher_start_v - &
          candidate%higher_start_biased_v(point_count))
    end if
    if (.not. candidate%is_valid()) then
      message = 'neutral developed panel contains invalid coordinates'
      return
    end if

    panel = candidate
    valid = .true.
  end subroutine copy_legacy_neutral_panel

  !> Write one validated typed extrados panel to legacy corner arrays.
  !!
  !! This is the compatibility boundary for the typed stage-7 producer.  Every
  !! precondition is checked before the first assignment, so a rejected panel
  !! leaves all eight destination arrays unchanged.  Only the extrados segment
  !! indices `contour_first_index:contour_last_index-1` are written; intake,
  !! intrados, and support coordinates remain owned by their existing producers.
  !!
  !! The eight destination arrays must be distinct actual arguments.
  !!
  !! @param[in] panel Complete typed extrados development to publish.
  !! @param[in] lower_topology,higher_topology Adjacent source topologies.
  !! @param[inout] legacy_pl1_u,legacy_pl1_v Lower segment starts.
  !! @param[inout] legacy_pl2_u,legacy_pl2_v Lower segment ends.
  !! @param[inout] legacy_pr1_u,legacy_pr1_v Higher segment starts.
  !! @param[inout] legacy_pr2_u,legacy_pr2_v Higher segment ends.
  !! @param[out] valid True only after the complete panel was written.
  !! @param[out] message Empty on success; diagnostic text on failure.
  pure subroutine write_legacy_extrados_panel(panel, lower_topology, &
      higher_topology, legacy_pl1_u, legacy_pl1_v, legacy_pl2_u, &
      legacy_pl2_v, legacy_pr1_u, legacy_pr1_v, legacy_pr2_u, &
      legacy_pr2_v, valid, message)
    type(neutral_panel_2d), intent(in) :: panel
    type(profile_topology), intent(in) :: lower_topology, higher_topology
    real(real64), intent(inout) :: legacy_pl1_u(0:,:), legacy_pl1_v(0:,:)
    real(real64), intent(inout) :: legacy_pl2_u(0:,:), legacy_pl2_v(0:,:)
    real(real64), intent(inout) :: legacy_pr1_u(0:,:), legacy_pr1_v(0:,:)
    real(real64), intent(inout) :: legacy_pr2_u(0:,:), legacy_pr2_v(0:,:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    call write_legacy_surface_panel(panel, lower_topology, higher_topology, &
        surface_extrados, legacy_pl1_u, legacy_pl1_v, legacy_pl2_u, &
        legacy_pl2_v, legacy_pr1_u, legacy_pr1_v, legacy_pr2_u, &
        legacy_pr2_v, valid, message)
  end subroutine write_legacy_extrados_panel

  !> Write one validated typed intake panel and its explicit support segment.
  !!
  !! Exact intake segments are written at `first:last-1`; the separately owned
  !! post-intake support quadrilateral is written at `last`. Every other surface
  !! remains untouched. As with the extrados wrapper, the eight destination
  !! arrays must be distinct actuals.
  pure subroutine write_legacy_intake_panel(panel, lower_topology, &
      higher_topology, legacy_pl1_u, legacy_pl1_v, legacy_pl2_u, &
      legacy_pl2_v, legacy_pr1_u, legacy_pr1_v, legacy_pr2_u, &
      legacy_pr2_v, valid, message)
    type(neutral_panel_2d), intent(in) :: panel
    type(profile_topology), intent(in) :: lower_topology, higher_topology
    real(real64), intent(inout) :: legacy_pl1_u(0:,:), legacy_pl1_v(0:,:)
    real(real64), intent(inout) :: legacy_pl2_u(0:,:), legacy_pl2_v(0:,:)
    real(real64), intent(inout) :: legacy_pr1_u(0:,:), legacy_pr1_v(0:,:)
    real(real64), intent(inout) :: legacy_pr2_u(0:,:), legacy_pr2_v(0:,:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    call write_legacy_surface_panel(panel, lower_topology, higher_topology, &
        surface_intake, legacy_pl1_u, legacy_pl1_v, legacy_pl2_u, &
        legacy_pl2_v, legacy_pr1_u, legacy_pr1_v, legacy_pr2_u, &
        legacy_pr2_v, valid, message)
  end subroutine write_legacy_intake_panel

  !> Write one validated typed intrados panel to its exact legacy segment range.
  !!
  !! This wrapper is used only after vent consumers no longer need intake's
  !! post-surface support at the shared intake/intrados index. Publication is
  !! transactional and may use segment index 499 when a 500-point profile makes
  !! that index part of the real intrados contour.
  pure subroutine write_legacy_intrados_panel(panel, lower_topology, &
      higher_topology, legacy_pl1_u, legacy_pl1_v, legacy_pl2_u, &
      legacy_pl2_v, legacy_pr1_u, legacy_pr1_v, legacy_pr2_u, &
      legacy_pr2_v, valid, message)
    type(neutral_panel_2d), intent(in) :: panel
    type(profile_topology), intent(in) :: lower_topology, higher_topology
    real(real64), intent(inout) :: legacy_pl1_u(0:,:), legacy_pl1_v(0:,:)
    real(real64), intent(inout) :: legacy_pl2_u(0:,:), legacy_pl2_v(0:,:)
    real(real64), intent(inout) :: legacy_pr1_u(0:,:), legacy_pr1_v(0:,:)
    real(real64), intent(inout) :: legacy_pr2_u(0:,:), legacy_pr2_v(0:,:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    call write_legacy_surface_panel(panel, lower_topology, higher_topology, &
        surface_intrados, legacy_pl1_u, legacy_pl1_v, legacy_pl2_u, &
        legacy_pl2_v, legacy_pr1_u, legacy_pr1_v, legacy_pr2_u, &
        legacy_pr2_v, valid, message)
  end subroutine write_legacy_intrados_panel

  !> Shared transactional compatibility writer for regular neutral surfaces.
  pure subroutine write_legacy_surface_panel(panel, lower_topology, &
      higher_topology, expected_surface, legacy_pl1_u, legacy_pl1_v, &
      legacy_pl2_u, legacy_pl2_v, legacy_pr1_u, legacy_pr1_v, &
      legacy_pr2_u, legacy_pr2_v, valid, message)
    type(neutral_panel_2d), intent(in) :: panel
    type(profile_topology), intent(in) :: lower_topology, higher_topology
    integer, intent(in) :: expected_surface
    real(real64), intent(inout) :: legacy_pl1_u(0:,:), legacy_pl1_v(0:,:)
    real(real64), intent(inout) :: legacy_pl2_u(0:,:), legacy_pl2_v(0:,:)
    real(real64), intent(inout) :: legacy_pr1_u(0:,:), legacy_pr1_v(0:,:)
    real(real64), intent(inout) :: legacy_pr2_u(0:,:), legacy_pr2_v(0:,:)
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(index_range) :: lower_range, higher_range
    integer :: local_index, source_index, segment_count, write_last_index
    logical :: writes_post_surface_support

    valid = .false.
    message = ''
    if (.not. neutral_legacy_shapes_match(legacy_pl1_u, legacy_pl1_v, &
        legacy_pl2_u, legacy_pl2_v, legacy_pr1_u, legacy_pr1_v, &
        legacy_pr2_u, legacy_pr2_v)) then
      message = 'legacy neutral-panel destination shapes differ'
      return
    end if
    if (.not. panel%is_valid()) then
      message = 'cannot write an invalid neutral panel'
      return
    end if
    if (panel%surface /= expected_surface) then
      message = 'neutral write-back received the wrong surface'
      return
    end if
    if (.not. lower_topology%is_valid() .or. &
        .not. higher_topology%is_valid()) then
      message = 'neutral write-back received invalid topology'
      return
    end if
    writes_post_surface_support = .false.
    select case (expected_surface)
    case (surface_extrados)
      lower_range = lower_topology%extrados
      higher_range = higher_topology%extrados
    case (surface_intake)
      lower_range = lower_topology%intake
      higher_range = higher_topology%intake
      writes_post_surface_support = .true.
    case (surface_intrados)
      lower_range = lower_topology%intrados
      higher_range = higher_topology%intrados
    case default
      message = 'neutral write-back does not support this surface'
      return
    end select
    if (lower_range%first /= higher_range%first .or. &
        lower_range%last /= higher_range%last) then
      message = 'neutral write-back received incompatible surface indices'
      return
    end if
    if (panel%contour_first_index /= lower_range%first .or. &
        panel%contour_last_index /= lower_range%last) then
      message = 'typed panel range differs from source surface topology'
      return
    end if
    if (panel%panel_index > ubound(legacy_pl1_u, 1)) then
      message = 'panel index is outside neutral-panel destinations'
      return
    end if
    write_last_index = panel%contour_last_index - 1
    if (writes_post_surface_support) &
        write_last_index = panel%contour_last_index
    if (panel%contour_first_index < lbound(legacy_pl1_u, 2) .or. &
        write_last_index > ubound(legacy_pl1_u, 2)) then
      message = 'surface segments exceed neutral-panel point capacity'
      return
    end if
    segment_count = panel%contour_last_index - panel%contour_first_index
    do local_index = 1, segment_count
      source_index = panel%contour_first_index + local_index - 1
      legacy_pl1_u(panel%panel_index, source_index) = &
          panel%lower_segment_start_u(local_index)
      legacy_pl1_v(panel%panel_index, source_index) = &
          panel%lower_segment_start_v(local_index)
      legacy_pl2_u(panel%panel_index, source_index) = &
          panel%lower_segment_end_u(local_index)
      legacy_pl2_v(panel%panel_index, source_index) = &
          panel%lower_segment_end_v(local_index)
      legacy_pr1_u(panel%panel_index, source_index) = &
          panel%higher_segment_start_u(local_index)
      legacy_pr1_v(panel%panel_index, source_index) = &
          panel%higher_segment_start_v(local_index)
      legacy_pr2_u(panel%panel_index, source_index) = &
          panel%higher_segment_end_u(local_index)
      legacy_pr2_v(panel%panel_index, source_index) = &
          panel%higher_segment_end_v(local_index)
    end do
    if (writes_post_surface_support) then
      source_index = panel%contour_last_index
      legacy_pl1_u(panel%panel_index, source_index) = &
          panel%support_lower_start_u
      legacy_pl1_v(panel%panel_index, source_index) = &
          panel%support_lower_start_v
      legacy_pl2_u(panel%panel_index, source_index) = panel%support_lower_end_u
      legacy_pl2_v(panel%panel_index, source_index) = panel%support_lower_end_v
      legacy_pr1_u(panel%panel_index, source_index) = &
          panel%support_higher_start_u
      legacy_pr1_v(panel%panel_index, source_index) = &
          panel%support_higher_start_v
      legacy_pr2_u(panel%panel_index, source_index) = &
          panel%support_higher_end_u
      legacy_pr2_v(panel%panel_index, source_index) = &
          panel%support_higher_end_v
    end if
    valid = .true.
  end subroutine write_legacy_surface_panel

  !> Compose topology and neutral-panel adapters from the legacy arrays.
  subroutine copy_legacy_neutral_panel_from_counts(legacy_pl1_u, &
      legacy_pl1_v, legacy_pl2_u, legacy_pl2_v, legacy_pr1_u, &
      legacy_pr1_v, legacy_pr2_u, legacy_pr2_v, legacy_np, panel_index, &
      surface, panel, valid, message)
    real(real64), intent(in) :: legacy_pl1_u(0:,:), legacy_pl1_v(0:,:)
    real(real64), intent(in) :: legacy_pl2_u(0:,:), legacy_pl2_v(0:,:)
    real(real64), intent(in) :: legacy_pr1_u(0:,:), legacy_pr1_v(0:,:)
    real(real64), intent(in) :: legacy_pr2_u(0:,:), legacy_pr2_v(0:,:)
    integer, intent(in) :: legacy_np(0:,:)
    integer, intent(in) :: panel_index, surface
    type(neutral_panel_2d), intent(inout) :: panel
    logical, intent(out) :: valid
    character(len=*), intent(out) :: message

    type(profile_topology) :: lower_topology, higher_topology

    valid = .false.
    message = ''
    call copy_legacy_profile_topology(legacy_np, panel_index, &
        lower_topology, valid, message)
    if (.not. valid) return
    call copy_legacy_profile_topology(legacy_np, panel_index + 1, &
        higher_topology, valid, message)
    if (.not. valid) return
    call copy_legacy_neutral_panel(legacy_pl1_u, legacy_pl1_v, &
        legacy_pl2_u, legacy_pl2_v, legacy_pr1_u, legacy_pr1_v, &
        legacy_pr2_u, legacy_pr2_v, panel_index, lower_topology, &
        higher_topology, surface, panel, valid, message)
  end subroutine copy_legacy_neutral_panel_from_counts

  !> Return the Euclidean length of a finite 2D polyline.
  pure real(real64) function polyline_length_2d(u, v) result(length)
    real(real64), intent(in) :: u(:), v(:)
    integer :: point_index

    length = 0.0_real64
    if (size(u) /= size(v) .or. size(u) < 2) return
    do point_index = 1, size(u) - 1
      length = length + hypot(u(point_index + 1) - u(point_index), &
          v(point_index + 1) - v(point_index))
    end do
  end function polyline_length_2d

  !> Sum exact lower-index legacy segment lengths without closing join gaps.
  pure real(real64) function neutral_panel_lower_edge_length(panel) &
      result(length)
    type(neutral_panel_2d), intent(in) :: panel
    integer :: segment_index

    length = 0.0_real64
    if (.not. panel%is_valid()) return
    do segment_index = 1, size(panel%lower_segment_start_u)
      length = length + sqrt((panel%lower_segment_end_u(segment_index) - &
          panel%lower_segment_start_u(segment_index))**2 + &
          (panel%lower_segment_end_v(segment_index) - &
          panel%lower_segment_start_v(segment_index))**2)
    end do
  end function neutral_panel_lower_edge_length

  !> Sum exact higher-index legacy segment lengths without closing join gaps.
  pure real(real64) function neutral_panel_higher_edge_length(panel) &
      result(length)
    type(neutral_panel_2d), intent(in) :: panel
    integer :: segment_index

    length = 0.0_real64
    if (.not. panel%is_valid()) return
    do segment_index = 1, size(panel%higher_segment_start_u)
      length = length + sqrt((panel%higher_segment_end_u(segment_index) - &
          panel%higher_segment_start_u(segment_index))**2 + &
          (panel%higher_segment_end_v(segment_index) - &
          panel%higher_segment_start_v(segment_index))**2)
    end do
  end function neutral_panel_higher_edge_length

  !> Sum exact terminal-boundary segments without closing reconstruction gaps.
  !! Invalid boundaries return zero, matching the neutral-panel length helpers.
  pure real(real64) function neutral_boundary_edge_length(edge) result(length)
    type(neutral_boundary_edge_2d), intent(in) :: edge
    integer :: segment_index

    length = 0.0_real64
    if (.not. edge%is_valid()) return
    do segment_index = 1, size(edge%segment_start_u)
      length = length + sqrt((edge%segment_end_u(segment_index) - &
          edge%segment_start_u(segment_index))**2 + &
          (edge%segment_end_v(segment_index) - &
          edge%segment_start_v(segment_index))**2)
    end do
  end function neutral_boundary_edge_length

  !> Measure the cross-panel gap at one original contour index.
  real(real64) function neutral_panel_edge_gap(panel, contour_index, &
      valid) result(gap)
    type(neutral_panel_2d), intent(in) :: panel
    integer, intent(in) :: contour_index
    logical, intent(out) :: valid
    integer :: local_index

    gap = 0.0_real64
    valid = .false.
    if (.not. panel%is_valid()) return
    if (contour_index < panel%contour_first_index .or. &
        contour_index > panel%contour_last_index) return
    local_index = contour_index - panel%contour_first_index + 1
    if (local_index < size(panel%lower_start_biased_u)) then
      gap = sqrt((panel%higher_segment_start_u(local_index) - &
          panel%lower_segment_start_u(local_index))**2.0 + &
          (panel%higher_segment_start_v(local_index) - &
          panel%lower_segment_start_v(local_index))**2.0)
    else
      gap = sqrt((panel%higher_segment_end_u(local_index - 1) - &
          panel%lower_segment_end_u(local_index - 1))**2.0 + &
          (panel%higher_segment_end_v(local_index - 1) - &
          panel%lower_segment_end_v(local_index - 1))**2.0)
    end if
    valid = .true.
  end function neutral_panel_edge_gap

  !> Compare two model-space measurements with scale-aware tolerance.
  pure logical function geometry_values_are_close(first, second) &
      result(close)
    real(real64), intent(in) :: first, second

    close = ieee_is_finite(first) .and. ieee_is_finite(second) .and. &
        abs(first - second) <= geometry_tolerance * &
        (1.0_real64 + max(abs(first), abs(second)))
  end function geometry_values_are_close

  !> Compare every legacy neutral-panel array with the first array's shape.
  pure logical function neutral_legacy_shapes_match(pl1_u, pl1_v, pl2_u, &
      pl2_v, pr1_u, pr1_v, pr2_u, pr2_v) result(match)
    real(real64), intent(in) :: pl1_u(0:,:), pl1_v(0:,:)
    real(real64), intent(in) :: pl2_u(0:,:), pl2_v(0:,:)
    real(real64), intent(in) :: pr1_u(0:,:), pr1_v(0:,:)
    real(real64), intent(in) :: pr2_u(0:,:), pr2_v(0:,:)

    match = all(shape(pl1_u) == shape(pl1_v)) .and. &
        all(shape(pl1_u) == shape(pl2_u)) .and. &
        all(shape(pl1_u) == shape(pl2_v)) .and. &
        all(shape(pl1_u) == shape(pr1_u)) .and. &
        all(shape(pl1_u) == shape(pr1_v)) .and. &
        all(shape(pl1_u) == shape(pr2_u)) .and. &
        all(shape(pl1_u) == shape(pr2_v))
  end function neutral_legacy_shapes_match

  !> Compare two model-space points for cache/view coherence.
  pure logical function points_match(first_u, first_v, second_u, second_v) &
      result(match)
    real(real64), intent(in) :: first_u, first_v, second_u, second_v

    match = geometry_values_are_close(first_u, second_u) .and. &
        geometry_values_are_close(first_v, second_v)
  end function points_match

  !> Return true when legacy coordinate arrays can be indexed in lockstep.
  pure logical function legacy_shapes_match(legacy_u, legacy_v) result(match)
    real(real64), intent(in) :: legacy_u(0:,:,:), legacy_v(0:,:,:)

    match = all(shape(legacy_u) == shape(legacy_v))
  end function legacy_shapes_match

end module leparagliding_domain_model
