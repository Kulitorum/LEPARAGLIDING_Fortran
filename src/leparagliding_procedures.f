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
       use leparagliding_dxf_output, only : pointg,point,poinc,poinl,
     + circle,mtriangle,linevent,segment101,line,line_layer,line3d,
     + line3dn,poly2d,ellipse,set_ellipse_segments,romano,txt,itxt,
     + itxt2,dxfinit,dxfend
       use leparagliding_geometry_2d, only : vredis,xrxs,flatt,axisch,
     + angdis2,vrib_hole_ellipse
       use leparagliding_transformations, only : loc2glo2d
       use leparagliding_file_cleanup, only : nonan,fix_dxf_nan,
     + replace_in_string
       use leparagliding_polyline_interpolation, only : interpseg
       use leparagliding_profile_data, only : datair,remapcont,xyzt

       private :: typm1, typm2, typm3, typm4, typm5, typm6
       private :: joncs45, interpolate_surface_y

       contains

       include 'procedures/color_construction.inc'
       include 'procedures/panel_edges.inc'
       include 'procedures/junctions.inc'
       include 'procedures/offsets_reinforcements.inc'
       include 'procedures/geometry_3d.inc'
       include 'procedures/pattern_marks.inc'
       include 'procedures/geometry_utilities.inc'

       end module leparagliding_procedures
