c***********************************************************************
c     15. INTERNAL COLOR DIVISIONS ON DEVELOPED PANELS
c***********************************************************************
c
c     Purpose
c     -------
c     Sections 15 (extrados) and 16 (intrados) describe open artwork
c     division lines by their chord percentage at each rib.  Matching
c     boundary identifiers on consecutive ribs define a straight cut across
c     the intervening developed panel.  This include converts those records
c     into:
c
c       color_seams      the sewing line shared by both color pieces,
c       color_allowance  one parallel cut edge for each adjacent piece,
c       color_marks      paired marks inset 1.1 mm from both cut edges.
c
c     Existing panel entities, layer names, and CAD colors are not changed.
c     Extrados overlay entities retain ACI 4; intrados entities retain ACI 30.
c
c     Typed coordinate flow
c     ---------------
c       color_production_panels(panel)%lower_profile / %higher_profile
c                              normalized chord/profile coordinates
c       color_production_panels(panel)%edges
c                              developed left/right sewing edges
c       seppix(panel)   horizontal placement of the developed panel
c       xyshift         extrados row; xyshift+890.95 is the intrados row
c
c     This stage is the sole compatibility boundary for the color consumer:
c     it checks and copies legacy u/v storage into the complete panel objects
c     once.  The drawing procedure has no knowledge of numbered slots.
c
c     Only classic on-rib records (npce/npci >= 1 and xpc2*=0) create panel
c     construction.  The historical -2 panel-input mode remains visible in
c     the planform view but is not silently reinterpreted here.
c
c     Both surfaces call the same documented topology routine.  Its typed
c     interpolation handles repeated/vertical coordinates without dividing
c     slopes, fixing the Swoop2 section-15/16 NaN failure.
c***********************************************************************

c     Build the typed panel collection once for both color surfaces.
       color_panels_required=npce.ge.1.or.
     + (atp.ne.'ss'.and.npci.ge.1)
       if (allocated(color_production_panels))
     + deallocate(color_production_panels)
       if (allocated(color_production_panel_available))
     + deallocate(color_production_panel_available)
       if (color_panels_required) then
       allocate(color_production_panels(1:max(0,nribss-1)))
       allocate(color_production_panel_available(1:max(0,nribss-1)))
       color_production_panel_available=.false.
       do color_panel_index=1,nribss-1
       call copy_legacy_production_panel(u,v,wing_profile_topologies,
     + color_panel_index,color_production_panels(color_panel_index),
     + color_panel_valid,color_panel_message)
       color_production_panel_available(color_panel_index)=color_panel_valid
       if (.not.color_panel_valid) then
       write (*,'(A,I0,2A)') 'Warning: skipped color panel ',
     + color_panel_index,': ',trim(color_panel_message)
       end if
       end do
       end if

c     Extrados: profile points run from trailing edge toward leading edge.
       if (npce.ge.1) then
       call draw_classic_color_surface(npce,npc1e,npc2e,npc3e,xpc1e,
     + xpc2e,nribss,color_production_panels,
     + color_production_panel_available,seppix,
     + layout_transform_2d(translation_u=1970.0d0*xkf,
     + translation_v=xyshift*xkf,rotation_rad=0.0d0,
     + u_direction=1,v_direction=-1),xupp,
     + color_mark_inset_mm,4,1)
       end if

c     Intrados: skip single-surface `ss` profiles and traverse in reverse.
       if (atp.ne.'ss'.and.npci.ge.1) then
       call draw_classic_color_surface(npci,npc1i,npc2i,npc3i,xpc1i,
     + xpc2i,nribss,color_production_panels,
     + color_production_panel_available,seppix,
     + layout_transform_2d(translation_u=1970.0d0*xkf,
     + translation_v=(890.95d0+xyshift)*xkf,
     + rotation_rad=0.0d0,u_direction=1,v_direction=-1),xlow,
     + color_mark_inset_mm,30,-1)
       end if

c***********************************************************************
c     End section 15/16 color construction
c***********************************************************************
