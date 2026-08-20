c***********************************************************************
c      LEparagliding legacy procedure facade
c      Procedures remain in fixed form; explicit module interfaces allow
c      the compiler to validate every call while migration continues.
c***********************************************************************

!> Expose the legacy calculation and output procedures through explicit APIs.
!!
!! Grouped include files retain their historical implementations while this
!! module gives callers compiler-checked interfaces. New procedures should use
!! `implicit none` and explicit argument intents whenever practical.
       module leparagliding_procedures
       use leparagliding_geometry
       use leparagliding_domain_model
       use leparagliding_color_geometry
       use leparagliding_mark_types

       private :: typm1, typm2, typm3, typm4, typm5, typm6
       private :: dxf_point_entity, dxf_circle_entity
       private :: dxf_line3d_entity, dxf_text_header, dxf_text_footer
       private :: utf8_to_dxf_text
       private :: joncs45, interpolate_surface_y

       integer, private, save :: configured_ellipse_segments = 60

       contains

       include 'procedures/dxf_output.inc'
       include 'procedures/color_construction.inc'
       include 'procedures/geometry_2d.inc'
       include 'procedures/panel_edges.inc'
       include 'procedures/junctions.inc'
       include 'procedures/offsets_reinforcements.inc'
       include 'procedures/geometry_3d.inc'
       include 'procedures/pattern_marks.inc'
       include 'procedures/profile_data.inc'
       include 'procedures/geometry_utilities.inc'
       include 'procedures/interpolation.inc'
       include 'procedures/file_cleanup.inc'
       include 'procedures/transformations.inc'

       end module leparagliding_procedures
