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
c     Coordinate flow
c     ---------------
c       u/v(rib,:,2)    normalized profile used to locate a chord percentage
c       u/v(panel,:,9)  developed left panel edge (rib panel)
c       u/v(panel,:,10) developed right panel edge (rib panel+1)
c       seppix(panel)   horizontal placement of the developed panel
c       xyshift         extrados row; xyshift+890.95 is the intrados row
c
c     Only classic on-rib records (npce/npci >= 1 and xpc2*=0) create panel
c     construction.  The historical -2 panel-input mode remains visible in
c     the planform view but is not silently reinterpreted here.
c
c     Both surfaces call the same documented topology routine.  Its typed
c     interpolation handles repeated/vertical coordinates without dividing
c     slopes, fixing the Swoop2 section-15/16 NaN failure.
c***********************************************************************

c     Extrados: profile points run from trailing edge toward leading edge.
       if (npce.ge.1) then
       call draw_classic_color_surface(npce,npc1e,npc2e,npc3e,xpc1e,
     + xpc2e,nribss,wing_profile_topologies,u,v,seppix,
     + layout_transform_2d(translation_u=1970.0d0*xkf,
     + translation_v=xyshift*xkf,rotation_rad=0.0d0,
     + u_direction=1,v_direction=-1),xupp,
     + color_mark_inset_mm,4,1)
       end if

c     Intrados: skip single-surface `ss` profiles and traverse in reverse.
       if (atp.ne.'ss'.and.npci.ge.1) then
       call draw_classic_color_surface(npci,npc1i,npc2i,npc3i,xpc1i,
     + xpc2i,nribss,wing_profile_topologies,u,v,seppix,
     + layout_transform_2d(translation_u=1970.0d0*xkf,
     + translation_v=(890.95d0+xyshift)*xkf,
     + rotation_rad=0.0d0,u_direction=1,v_direction=-1),xlow,
     + color_mark_inset_mm,30,-1)
       end if

c***********************************************************************
c     End section 15/16 color construction
c***********************************************************************
