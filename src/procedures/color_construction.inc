!> Map one surface's classic rib records onto every developed skin panel.
!!
!! Equal boundary identifiers on consecutive ribs are paired independently of
!! record order.  Each panel is first copied through a checked legacy-slot
!! adapter into a `production_panel_2d`; all subsequent names identify their
!! coordinate domain explicitly.  On-rib boundaries are then delegated to
!! `draw_color_panel_construction`.  The `surface_direction` flag selects the
!! profile traversal only: 1 walks the extrados toward the leading edge and
!! -1 walks the intrados in reverse.  The caller owns the single checked
!! conversion from legacy coordinate storage; this routine consumes only the
!! typed normalized profiles and developed edges.  Keeping this topology in
!! one subroutine removes the historical duplicated extrados/intrados
!! implementation.
!!
!! @param[in] record_count Number of rib records for this surface.
!! @param[in] record_rib Rib number owned by each record.
!! @param[in] record_boundary_count Number of boundaries in each record.
!! @param[in] record_boundary_id Stable identifier for pairing between ribs.
!! @param[in] boundary_percent Chord percentage for each boundary.
!! @param[in] off_rib_offset Historical fractional-rib offset; only zero is
!!            currently a manufacturable straight division.
!! @param[in] semiwing_rib_count Number of ribs in the computed semi-wing.
!! @param[in] production_panels Complete typed normalized profiles and
!!            developed edges, indexed by their lower rib/panel number.
!! @param[in] panel_available True for each panel that passed the checked
!!            legacy adapter at the stage boundary.
!! @param[in] panel_separation Horizontal developed-panel placement by index.
!! @param[in] surface_layout Surface row placement and orientation.
!! @param[in] allowance_mm Internal allowance applied to both color pieces.
!! @param[in] mark_inset_mm Inward matching-mark offset from both cut edges.
!! @param[in] cad_color AutoCAD color used for this surface overlay.
!! @param[in] surface_direction 1 for extrados; -1 for intrados.
       subroutine draw_classic_color_surface(record_count,record_rib,
     + record_boundary_count,record_boundary_id,boundary_percent,
     + off_rib_offset,semiwing_rib_count,production_panels,
     + panel_available,panel_separation,surface_layout,
     + allowance_mm,mark_inset_mm,cad_color,surface_direction)
       implicit none

       integer, intent(in) :: record_count,semiwing_rib_count
       integer, intent(in) :: record_rib(100),record_boundary_count(100)
       integer, intent(in) :: record_boundary_id(100,100)
       type(production_panel_2d), intent(in) :: production_panels(:)
       logical, intent(in) :: panel_available(:)
       integer, intent(in) :: cad_color,surface_direction
       real*8, intent(in) :: boundary_percent(100,100)
       real*8, intent(in) :: off_rib_offset(100,100)
       real*8, intent(in) :: panel_separation(0:100)
       real*8, intent(in) :: allowance_mm,mark_inset_mm
       type(layout_transform_2d), intent(in) :: surface_layout

       integer :: panel_index,left_record,left_boundary
       integer :: right_record,right_boundary,candidate_record
       integer :: candidate_boundary,boundary_identifier
       integer :: left_first_point,left_last_point
       integer :: right_first_point,right_last_point,point_step
       real*8 :: left_u,left_v,right_u,right_v
       logical :: left_found,right_found
       type(layout_transform_2d) :: panel_layout

       if (record_count.lt.1) return
       if (surface_direction.ne.1.and.surface_direction.ne.-1) return
       if (.not.surface_layout%is_valid()) return
       if (size(production_panels).lt.semiwing_rib_count-1) return
       if (size(panel_available).lt.semiwing_rib_count-1) return

       do panel_index=1,semiwing_rib_count-1
       if (.not.panel_available(panel_index)) cycle
       if (.not.production_panels(panel_index)%is_valid()) cycle
       if (production_panels(panel_index)%panel_index.ne.panel_index) cycle

       do left_record=1,record_count
       if (record_rib(left_record).eq.panel_index) then

       do left_boundary=1,record_boundary_count(left_record)
       if (dabs(off_rib_offset(left_record,left_boundary)).le.
     + 1.0d-10) then

       boundary_identifier=record_boundary_id(left_record,left_boundary)
       right_record=0
       right_boundary=0

       do candidate_record=1,record_count
       if (record_rib(candidate_record).eq.panel_index+1) then
       do candidate_boundary=1,record_boundary_count(candidate_record)
       if (record_boundary_id(candidate_record,candidate_boundary).eq.
     + boundary_identifier.and.
     + dabs(off_rib_offset(candidate_record,candidate_boundary)).le.
     + 1.0d-10) then
       right_record=candidate_record
       right_boundary=candidate_boundary
       exit
       end if
       end do
       end if
       if (right_record.ne.0) exit
       end do

       if (right_record.ne.0) then
       if (surface_direction.eq.1) then
       left_first_point=production_panels(panel_index)%lower_profile%
     + topology%extrados%first
       left_last_point=production_panels(panel_index)%lower_profile%
     + topology%extrados%last
       right_first_point=production_panels(panel_index)%higher_profile%
     + topology%extrados%first
       right_last_point=min(production_panels(panel_index)%higher_profile%
     + topology%extrados%last,production_panels(panel_index)%lower_profile%
     + topology%extrados%last)
       point_step=1
       else
       left_first_point=production_panels(panel_index)%lower_profile%
     + topology%intrados%last
       left_last_point=production_panels(panel_index)%lower_profile%
     + topology%intrados%first
       right_first_point=min(production_panels(panel_index)%higher_profile%
     + topology%intrados%last,production_panels(panel_index)%lower_profile%
     + topology%intrados%last)
       right_last_point=max(production_panels(panel_index)%higher_profile%
     + topology%intrados%first,production_panels(panel_index)%lower_profile%
     + topology%intrados%first)
       point_step=-1
       end if

       call locate_color_boundary_on_edge(
     + production_panels(panel_index)%lower_profile%chord_fraction,
     + production_panels(panel_index)%edges%lower_sewing_u,
     + production_panels(panel_index)%edges%lower_sewing_v,
     + left_first_point,left_last_point,point_step,
     + boundary_percent(left_record,left_boundary),left_found,
     + left_u,left_v)

       call locate_color_boundary_on_edge(
     + production_panels(panel_index)%higher_profile%chord_fraction,
     + production_panels(panel_index)%edges%higher_sewing_u,
     + production_panels(panel_index)%edges%higher_sewing_v,
     + right_first_point,right_last_point,point_step,
     + boundary_percent(right_record,right_boundary),right_found,
     + right_u,right_v)

       if (left_found.and.right_found) then
       panel_layout=surface_layout
       panel_layout%translation_u=panel_layout%translation_u+
     + panel_separation(panel_index)
       call draw_color_panel_construction(left_u,left_v,right_u,right_v,
     + panel_layout,allowance_mm,mark_inset_mm,
     + cad_color)
       end if
       end if

       end if
       end do
       end if
       end do
       end do

       return
       end

!> Draw one straight internal color seam on a developed skin panel.
!!
!! The original panel geometry remains untouched.  New construction entities
!! are emitted on dedicated layers: `color_seams` for the sewing line,
!! `color_allowance` for the two adjacent piece cut edges, and `color_marks`
!! for registration marks.  This separation preserves every legacy layer and
!! AutoCAD color while making the artwork overlay independently selectable.
!!
!! LEP model coordinates are centimetres in the panel layout; sewing values
!! remain millimetres in the input file and are converted here with factor 0.1.
!!
!! @param[in] left_u,left_v Boundary point on the panel's left developed edge.
!! @param[in] right_u,right_v Boundary point on the right developed edge.
!! @param[in] panel_layout Typed placement in the two-dimensional sheet.
!! @param[in] allowance_mm Seam allowance for both adjacent color pieces.
!! @param[in] mark_inset_mm Inward offset from each internal cut edge.
!! @param[in] cad_color AutoCAD color index identifying the surface.
       subroutine draw_color_panel_construction(left_u,left_v,right_u,
     + right_v,panel_layout,allowance_mm,
     + mark_inset_mm,cad_color)
       implicit none

       real*8, intent(in) :: left_u,left_v,right_u,right_v
       real*8, intent(in) :: allowance_mm,mark_inset_mm
       integer, intent(in) :: cad_color
       type(layout_transform_2d), intent(in) :: panel_layout

       real*8 :: seam_left_x,seam_left_y,seam_right_x,seam_right_y
       real*8 :: plus_left_x,plus_left_y,plus_right_x,plus_right_y
       real*8 :: minus_left_x,minus_left_y,minus_right_x,minus_right_y
       real*8 :: mark_left_x,mark_left_y,mark_right_x,mark_right_y
       real*8 :: allowance_model,mark_inset_model,mark_half_size
       logical :: valid_offsets,valid_marks,valid_left_layout
       logical :: valid_right_layout

       call panel_layout%map_point(left_u,left_v,seam_left_x,seam_left_y,
     + valid_left_layout)
       call panel_layout%map_point(right_u,right_v,seam_right_x,
     + seam_right_y,valid_right_layout)
       if (.not.valid_left_layout.or..not.valid_right_layout) return
       allowance_model=0.1d0*max(0.0d0,allowance_mm)
       mark_inset_model=0.1d0*max(0.0d0,mark_inset_mm)
       mark_half_size=0.15d0

       call offset_color_seam(seam_left_x,seam_left_y,seam_right_x,
     + seam_right_y,allowance_model,plus_left_x,plus_left_y,
     + plus_right_x,plus_right_y,minus_left_x,minus_left_y,
     + minus_right_x,minus_right_y,valid_offsets)
       if (.not.valid_offsets) return

       call line_layer(seam_left_x,seam_left_y,seam_right_x,
     + seam_right_y,'color_seams',cad_color)
       call line_layer(plus_left_x,plus_left_y,plus_right_x,
     + plus_right_y,'color_allowance',cad_color)
       call line_layer(minus_left_x,minus_left_y,minus_right_x,
     + minus_right_y,'color_allowance',cad_color)

       call color_piece_match_points(seam_left_x,seam_left_y,
     + seam_right_x,seam_right_y,allowance_model,mark_inset_model,
     + mark_left_x,mark_left_y,
     + mark_right_x,mark_right_y,valid_marks)
       if (.not.valid_marks) return

       call color_registration_cross(mark_left_x,mark_left_y,
     + mark_half_size,cad_color)
       call color_registration_cross(mark_right_x,mark_right_y,
     + mark_half_size,cad_color)

       return
       end

!> Draw a compact cross on the dedicated color-registration layer.
!! @param[in] mark_x,mark_y Cross center in final 2D drawing coordinates.
!! @param[in] half_size Half the cross arm length in model centimetres.
!! @param[in] cad_color AutoCAD color index retained from the color surface.
       subroutine color_registration_cross(mark_x,mark_y,half_size,
     + cad_color)
       implicit none
       real*8, intent(in) :: mark_x,mark_y,half_size
       integer, intent(in) :: cad_color

       call line_layer(mark_x-half_size,mark_y,mark_x+half_size,mark_y,
     + 'color_marks',cad_color)
       call line_layer(mark_x,mark_y-half_size,mark_x,mark_y+half_size,
     + 'color_marks',cad_color)
       return
       end
