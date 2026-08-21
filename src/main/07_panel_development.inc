!> Flatten upper, lower, and vent skin strips into per-panel 2D coordinates.
!!
!! Each 3D quadrilateral between consecutive profile samples is reconstructed
!! from its six edge/diagonal distances, then chained into `pl*/pr*` developed
!! coordinates.  Stage 8 turns these neutral developed strips into tensioned
!! panel edges, sewing borders, cutting variants, and production layouts.
c      7. PANEL DEVELOPMENT
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      7.1 Panel 1'-1 extrados
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=1

       px0=0.
       py0=0.
       ptheta=0.

       do j=1,np(1,2)-1 ! extrados panels point j

c      Distances between points rib 0 and 1. Row 0 is the explicit mirrored
c      spatial rib; the former xx/yy/zz arrays duplicated these coordinates.
       pa=dsqrt((x(i,j)-x(0,j))**2.+(y(i,j)-y(0,j))**2.+
     + (z(i,j)-z(0,j))**2.)
       pb=dsqrt((x(i,j+1)-x(0,j))**2.+(y(i,j+1)-y(0,j))**2.+
     + (z(i,j+1)-z(0,j))**2.)
       pc=dsqrt((x(i,j)-x(i,j+1))**2.+(y(i,j)-y(i,j+1))**2.+
     + (z(i,j)-z(i,j+1))**2.)
       pd=dsqrt((x(i,j)-x(0,j+1))**2.+(y(i,j)-y(0,j+1))**2.+
     + (z(i,j)-z(0,j+1))**2.)
       pe=dsqrt((x(0,j+1)-x(0,j))**2.+(y(0,j+1)-y(0,j))**2.+
     + (z(0,j+1)-z(0,j))**2.)
       pf=dsqrt((x(i,j+1)-x(0,j+1))**2.+
     + (y(i,j+1)-y(0,j+1))**2.+(z(i,j+1)-z(0,j+1))**2.)
       
       pa2r=(pa*pa-pb*pb+pc*pc)/(2.*pa)
       pa1r=pa-pa2r
       phr=dsqrt(pc*pc-pa2r*pa2r)

       pa2l=(pa*pa-pe*pe+pd*pd)/(2.*pa)
       pa1l=pa-pa2l
       phl=dsqrt(pd*pd-pa2l*pa2l)

       pb2t=(pb*pb-pe*pe+pf*pf)/(2.*pb)
       pb1t=pb-pb2t
       pht=dsqrt(pf*pf-pb2t*pb2t)
       
       pw1=datan(phr/pa1r)
       phu=pb1t*dtan(pw1)

c      Quadrilater coordinates
       pl1x(i,j)=px0
       pl1y(i,j)=py0

       pr1x(i,j)=pa*dcos(ptheta)+px0
       pr1y(i,j)=pa*dsin(ptheta)+py0

       pl2x(i,j)=pa1l*dcos(ptheta)-phl*dsin(ptheta)+px0
       pl2y(i,j)=pa1l*dsin(ptheta)+phl*dcos(ptheta)+py0
       
       pr2x(i,j)=pa1r*dcos(ptheta)-phr*dsin(ptheta)+px0
       pr2y(i,j)=pa1r*dsin(ptheta)+phr*dcos(ptheta)+py0

c      Iteration
       px0=pl2x(i,j)
       py0=pl2y(i,j)
       ptheta=datan((pr2y(i,j)-pl2y(i,j))/(pr2x(i,j)-pl2x(i,j)))
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      REVISAR!!!!!
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       pl1x(0,j)=pl1x(i,j)
       pl1y(0,j)=pl1y(i,j)

       pr1x(0,j)=pr1x(i,j)
       pr1y(0,j)=pr1y(i,j)

       pl2x(0,j)=pl2x(i,j)
       pl2y(0,j)=pl2y(i,j)

       pr2x(0,j)=pr2x(i,j)
       pr2y(0,j)=pr2y(i,j)

c       write (*,*) "1: ", pl1y(0,j), pr1y(0,j)
c       write (*,*) "2: ", pl2y(0,j), pr2y(0,j)
c      Result OK y iguals!!!

       end do
       
c      Extrados

c      Box (1,3)

       i=1
       
       psep=1970.*xkf+seppix(i)*float(i-1)  ! WARNING
       psey=400.*xkf

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      7.2 Panel 1'-1 intrados
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      7.2.1 Intrados

c      Box (2,3)

       i=1

       px0=0.
       py0=0.
       ptheta=0.

       do j=np(1,2)+np(1,3)-1,np(1,1)-1 ! extrados panels, point j

c      Distances between the explicit mirrored rib 0 and physical rib 1.
       pa=dsqrt((x(i,j)-x(0,j))**2.+(y(i,j)-y(0,j))**2.+
     + (z(i,j)-z(0,j))**2.)
       pb=dsqrt((x(i,j+1)-x(0,j))**2.+(y(i,j+1)-y(0,j))**2.+
     + (z(i,j+1)-z(0,j))**2.)
       pc=dsqrt((x(i,j)-x(i,j+1))**2.+(y(i,j)-y(i,j+1))**2.+
     + (z(i,j)-z(i,j+1))**2.)
       pd=dsqrt((x(i,j)-x(0,j+1))**2.+(y(i,j)-y(0,j+1))**2.+
     + (z(i,j)-z(0,j+1))**2.)
       pe=dsqrt((x(0,j+1)-x(0,j))**2.+(y(0,j+1)-y(0,j))**2.+
     + (z(0,j+1)-z(0,j))**2.)
       pf=dsqrt((x(i,j+1)-x(0,j+1))**2.+
     + (y(i,j+1)-y(0,j+1))**2.+(z(i,j+1)-z(0,j+1))**2.)
       
c       write (*,*) i,pa,pb,pc,pd,pe,pf

       pa2r=(pa*pa-pb*pb+pc*pc)/(2.*pa)
       pa1r=pa-pa2r
       phr=dsqrt(pc*pc-pa2r*pa2r)

       pa2l=(pa*pa-pe*pe+pd*pd)/(2.*pa)
       pa1l=pa-pa2l
       phl=dsqrt(pd*pd-pa2l*pa2l)

c      Quadrilater coordinates
       pl1x(i,j)=px0
       pl1y(i,j)=py0

       pr1x(i,j)=pa*dcos(ptheta)+px0
       pr1y(i,j)=pa*dsin(ptheta)+py0

       pl2x(i,j)=pa1l*dcos(ptheta)-phl*dsin(ptheta)+px0
       pl2y(i,j)=pa1l*dsin(ptheta)+phl*dcos(ptheta)+py0

       pr2x(i,j)=pa1r*dcos(ptheta)-phr*dsin(ptheta)+px0
       pr2y(i,j)=pa1r*dsin(ptheta)+phr*dcos(ptheta)+py0


       pl1x(0,j)=pl1x(i,j)
       pl1y(0,j)=pl1y(i,j)

       pr1x(0,j)=pr1x(i,j)
       pr1y(0,j)=pr1y(i,j)

       pl2x(0,j)=pl2x(i,j)
       pl2y(0,j)=pl2y(i,j)

       pr2x(0,j)=pr2x(i,j)
       pr2y(0,j)=pr2y(i,j)

       px0=pl2x(i,j)
       py0=pl2y(i,j)
       ptheta=datan((pr2y(i,j)-pl2y(i,j))/(pr2x(i,j)-pl2x(i,j)))
       
       end do
       
c      Intrados

       i=1
       
       psep=1970.*xkf+seppix(i)*float(i-1) ! WARNING
       psey=1291.*xkf
       ncontrol=1

c      Control if cell is closed
       if(int(rib(i,14)).eq.0.and.int(rib(i+1,14)).eq.0) then
       psey=400.*xkf
       ncontrol=0
       end if
       
c      7.2.2 Air intakes (vents) panels

       i=1

       px0=0.
       py0=0.
       ptheta=0.

       do j=np(1,2), np(1,2)+np(1,3)-2 ! vent panels, point j

c      Distances between the explicit mirrored rib 0 and physical rib 1.
       
       pa=dsqrt((x(i,j)-x(0,j))**2.+(y(i,j)-y(0,j))**2.+
     + (z(i,j)-z(0,j))**2.)
       pb=dsqrt((x(i,j+1)-x(0,j))**2.+(y(i,j+1)-y(0,j))**2.+
     + (z(i,j+1)-z(0,j))**2.)
       pc=dsqrt((x(i,j)-x(i,j+1))**2.+(y(i,j)-y(i,j+1))**2.+
     + (z(i,j)-z(i,j+1))**2.)
       pd=dsqrt((x(i,j)-x(0,j+1))**2.+(y(i,j)-y(0,j+1))**2.+
     + (z(i,j)-z(0,j+1))**2.)
       pe=dsqrt((x(0,j+1)-x(0,j))**2.+(y(0,j+1)-y(0,j))**2.+
     + (z(0,j+1)-z(0,j))**2.)
       pf=dsqrt((x(i,j+1)-x(0,j+1))**2.+
     + (y(i,j+1)-y(0,j+1))**2.+(z(i,j+1)-z(0,j+1))**2.)
       
       pa2r=(pa*pa-pb*pb+pc*pc)/(2.*pa)
       pa1r=pa-pa2r
       phr=dsqrt(pc*pc-pa2r*pa2r)

       pa2l=(pa*pa-pe*pe+pd*pd)/(2.*pa)
       pa1l=pa-pa2l
       phl=dsqrt(pd*pd-pa2l*pa2l)

c      Quadrilater coordinates
       pl1x(i,j)=px0
       pl1y(i,j)=py0

       pr1x(i,j)=pa*dcos(ptheta)+px0
       pr1y(i,j)=pa*dsin(ptheta)+py0

       pl2x(i,j)=pa1l*dcos(ptheta)-phl*dsin(ptheta)+px0
       pl2y(i,j)=pa1l*dsin(ptheta)+phl*dcos(ptheta)+py0

       pr2x(i,j)=pa1r*dcos(ptheta)-phr*dsin(ptheta)+px0
       pr2y(i,j)=pa1r*dsin(ptheta)+phr*dcos(ptheta)+py0

       px0=pl2x(i,j)
       py0=pl2y(i,j)
       ptheta=datan((pr2y(i,j)-pl2y(i,j))/(pr2x(i,j)-pl2x(i,j)))

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Assigna panell 0 zona vents
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       pl1x(0,j)=pl1x(i,j)
       pl1y(0,j)=pl1y(i,j)

       pr1x(0,j)=pr1x(i,j)
       pr1y(0,j)=pr1y(i,j)

       pl2x(0,j)=pl2x(i,j)
       pl2y(0,j)=pl2y(i,j)

       pr2x(0,j)=pr2x(i,j)
       pr2y(0,j)=pr2y(i,j)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       end do

c      Vents drawing

       if (n1draw.eq.1) then ! Draw central vent
       if (k26d.eq.0) then

c      Verify central cell width  
       if (cencell.ge.0.01)  then  

       i=1
       
       psep=1970.*xkf+seppix(i)*float(i-1) ! WARNING
       psey=1371.*xkf
       ncontrol=0

c      Control if cell is closed
       if(int(rib(1,14)).eq.0.and.int(rib(1+1,14)).eq.0) then
       ncontrol=1
       end if
c      Dibuixa boques
       ncontrol=1

       do j=np(1,2),np(1,2)+ncontrol*(np(1,3)-2)+(ncontrol-1)

       call line(psep+pl1x(i,j),psey-pl1y(i,j),psep+pr1x(i,j),
     + psey-pr1y(i,j),6)
       call line(psep+pl1x(i,j),psey-pl1y(i,j),psep+pr2x(i,j),
     + psey-pr2y(i,j),5)
       call line(psep+pl1x(i,j),psey-pl1y(i,j),psep+pl2x(i,j),
     + psey-pl2y(i,j),4)
       call line(psep+pr1x(i,j),psey-pr1y(i,j),psep+pr2x(i,j),
     + psey-pr2y(i,j),3)
       call line(psep+pr1x(i,j),psey-pr1y(i,j),psep+pl2x(i,j),
     + psey-pl2y(i,j),2)
       call line(psep+pl2x(i,j),psey-pl2y(i,j),psep+pr2x(i,j),
     + psey-pr2y(i,j),3)

       end do

       end if

       end if ! k26d=0
       end if ! n1draw

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      7.3 Panels 1 to nribss-1 extrados
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=1,nribss-1 ! panel i

       px0=0.
       py0=0.
       ptheta=0.

c      Extrados

       do j=1,np(i,2)-1,1 ! extrados panels point j

c      Distances between points
       pa=dsqrt((x(i+1,j)-x(i,j))**2.+(y(i+1,j)-y(i,j))**2.+
     + (z(i+1,j)-z(i,j))**2.)
       pb=dsqrt((x(i+1,j+1)-x(i,j))**2.+(y(i+1,j+1)-y(i,j))**2.+
     + (z(i+1,j+1)-z(i,j))**2.)
       pc=dsqrt((x(i+1,j+1)-x(i+1,j))**2.+(y(i+1,j+1)-y(i+1,j))**2.+
     + (z(i+1,j+1)-z(i+1,j))**2.)
       pd=dsqrt((x(i+1,j)-x(i,j+1))**2.+(y(i+1,j)-y(i,j+1))**2.+
     + (z(i+1,j)-z(i,j+1))**2.)
       pe=dsqrt((x(i,j+1)-x(i,j))**2.+(y(i,j+1)-y(i,j))**2.+
     + (z(i,j+1)-z(i,j))**2.)
       pf=dsqrt((x(i+1,j+1)-x(i,j+1))**2.+(y(i+1,j+1)-y(i,j+1))**2.+
     + (z(i+1,j+1)-z(i,j+1))**2.)
       
       pa2r=(pa*pa-pb*pb+pc*pc)/(2.*pa)
       pa1r=pa-pa2r
       phr=dsqrt(pc*pc-pa2r*pa2r)

       pa2l=(pa*pa-pe*pe+pd*pd)/(2.*pa)
       pa1l=pa-pa2l
       phl=dsqrt(pd*pd-pa2l*pa2l)

       pb2t=(pb*pb-pe*pe+pf*pf)/(2.*pb)
       pb1t=pb-pb2t
       pht=dsqrt(pf*pf-pb2t*pb2t)
       
       pw1=datan(phr/pa1r)
       phu=pb1t*dtan(pw1)

c      Quadrilater coordinates
       pl1x(i,j)=px0
       pl1y(i,j)=py0

       pr1x(i,j)=pa*dcos(ptheta)+px0
       pr1y(i,j)=pa*dsin(ptheta)+py0

       pl2x(i,j)=pa1l*dcos(ptheta)-phl*dsin(ptheta)+px0
       pl2y(i,j)=pa1l*dsin(ptheta)+phl*dcos(ptheta)+py0
       
       pr2x(i,j)=pa1r*dcos(ptheta)-phr*dsin(ptheta)+px0
       pr2y(i,j)=pa1r*dsin(ptheta)+phr*dcos(ptheta)+py0

c      Iteration
       px0=pl2x(i,j)
       py0=pl2y(i,j)
       ptheta=datan((pr2y(i,j)-pl2y(i,j))/(pr2x(i,j)-pl2x(i,j)))
       
       end do
       
       end do

c      Dual-run every final extrados panel through the pure typed developer.
c      Compare all exact 2D endpoints and the six independent source distances
c      before intrados reuses the legacy work variables. After agreement,
c      publish the typed exact segments back through the checked compatibility
c      adapter.  The typed producer then owns regular panel rows 0:nribss-1 at
c      the exact extrados segment indices first:last-1.
       do i=0,nribss-1
       call develop_extrados_panel(wing_spatial_ribs(i),
     + wing_spatial_ribs(i+1),wing_profile_topologies(i),
     + wing_profile_topologies(i+1),neutral_extrados_panel,
     + typed_extrados_source_distances,domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)') 'ERROR: typed extrados panel ',i,': ',
     + trim(domain_model_message)
       stop 1
       end if

       do j=wing_profile_topologies(i)%extrados%first,
     + wing_profile_topologies(i)%extrados%last-1
       k=j-wing_profile_topologies(i)%extrados%first+1

       if (.not.geometry_values_are_close(
     + neutral_extrados_panel%lower_segment_start_u(k),pl1x(i,j)).or.
     + .not.geometry_values_are_close(
     + neutral_extrados_panel%lower_segment_start_v(k),pl1y(i,j)).or.
     + .not.geometry_values_are_close(
     + neutral_extrados_panel%lower_segment_end_u(k),pl2x(i,j)).or.
     + .not.geometry_values_are_close(
     + neutral_extrados_panel%lower_segment_end_v(k),pl2y(i,j)).or.
     + .not.geometry_values_are_close(
     + neutral_extrados_panel%higher_segment_start_u(k),pr1x(i,j)).or.
     + .not.geometry_values_are_close(
     + neutral_extrados_panel%higher_segment_start_v(k),pr1y(i,j)).or.
     + .not.geometry_values_are_close(
     + neutral_extrados_panel%higher_segment_end_u(k),pr2x(i,j)).or.
     + .not.geometry_values_are_close(
     + neutral_extrados_panel%higher_segment_end_v(k),pr2y(i,j))) then
       write (*,'(A,2(I0,A))') 'ERROR: typed/legacy extrados endpoint at ',
     + i,', point ',j,''
       stop 1
       end if

c      Re-evaluate the legacy distance expressions here so the comparison is
c      independent of the pure module's distance helper. Panel 0 uses the
c      explicit mirrored legacy row 0, which is also the typed snapshot source.
       if(i.eq.0) then
       pa=dsqrt((x(1,j)-x(0,j))**2.+(y(1,j)-y(0,j))**2.+
     + (z(1,j)-z(0,j))**2.)
       pb=dsqrt((x(1,j+1)-x(0,j))**2.+
     + (y(1,j+1)-y(0,j))**2.+(z(1,j+1)-z(0,j))**2.)
       pc=dsqrt((x(1,j+1)-x(1,j))**2.+
     + (y(1,j+1)-y(1,j))**2.+(z(1,j+1)-z(1,j))**2.)
       pd=dsqrt((x(1,j)-x(0,j+1))**2.+
     + (y(1,j)-y(0,j+1))**2.+(z(1,j)-z(0,j+1))**2.)
       pe=dsqrt((x(0,j+1)-x(0,j))**2.+
     + (y(0,j+1)-y(0,j))**2.+(z(0,j+1)-z(0,j))**2.)
       pf=dsqrt((x(1,j+1)-x(0,j+1))**2.+
     + (y(1,j+1)-y(0,j+1))**2.+
     + (z(1,j+1)-z(0,j+1))**2.)
       else
       pa=dsqrt((x(i+1,j)-x(i,j))**2.+(y(i+1,j)-y(i,j))**2.+
     + (z(i+1,j)-z(i,j))**2.)
       pb=dsqrt((x(i+1,j+1)-x(i,j))**2.+
     + (y(i+1,j+1)-y(i,j))**2.+(z(i+1,j+1)-z(i,j))**2.)
       pc=dsqrt((x(i+1,j+1)-x(i+1,j))**2.+
     + (y(i+1,j+1)-y(i+1,j))**2.+
     + (z(i+1,j+1)-z(i+1,j))**2.)
       pd=dsqrt((x(i+1,j)-x(i,j+1))**2.+
     + (y(i+1,j)-y(i,j+1))**2.+(z(i+1,j)-z(i,j+1))**2.)
       pe=dsqrt((x(i,j+1)-x(i,j))**2.+(y(i,j+1)-y(i,j))**2.+
     + (z(i,j+1)-z(i,j))**2.)
       pf=dsqrt((x(i+1,j+1)-x(i,j+1))**2.+
     + (y(i+1,j+1)-y(i,j+1))**2.+
     + (z(i+1,j+1)-z(i,j+1))**2.)
       end if

       if (.not.geometry_values_are_close(
     + typed_extrados_source_distances(k)%start_cross_edge,pa).or.
     + .not.geometry_values_are_close(typed_extrados_source_distances(k)%
     + lower_start_to_higher_end,pb).or.
     + .not.geometry_values_are_close(typed_extrados_source_distances(k)%
     + higher_contour_edge,pc).or.
     + .not.geometry_values_are_close(typed_extrados_source_distances(k)%
     + higher_start_to_lower_end,pd).or.
     + .not.geometry_values_are_close(typed_extrados_source_distances(k)%
     + lower_contour_edge,pe).or.
     + .not.geometry_values_are_close(typed_extrados_source_distances(k)%
     + end_cross_edge,pf)) then
       write (*,'(A,2(I0,A))') 'ERROR: typed/legacy source distance at ',
     + i,', point ',j,''
       stop 1
       end if
       end do

       call write_legacy_extrados_panel(neutral_extrados_panel,
     + wing_profile_topologies(i),wing_profile_topologies(i+1),
     + pl1x,pl1y,pl2x,pl2y,pr1x,pr1y,pr2x,pr2y,
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)') 'ERROR: typed extrados write-back ',i,': ',
     + trim(domain_model_message)
       stop 1
       end if
       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      7.4 Panels 1 to nribss-1 Intrados
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      7.4.1 Intrados

       do i=1,nribss-1 ! panel i

       px0=0.
       py0=0.
       ptheta=0.

       do j=np(i,2)+np(i,3)-1,np(i,1)-1 ! extrados panels, point j

c      Distances between points
       pa=dsqrt((x(i+1,j)-x(i,j))**2.+(y(i+1,j)-y(i,j))**2.+
     + (z(i+1,j)-z(i,j))**2.)
       pb=dsqrt((x(i+1,j+1)-x(i,j))**2.+(y(i+1,j+1)-y(i,j))**2.+
     + (z(i+1,j+1)-z(i,j))**2.)
       pc=dsqrt((x(i+1,j+1)-x(i+1,j))**2.+(y(i+1,j+1)-y(i+1,j))**2.+
     + (z(i+1,j+1)-z(i+1,j))**2.)
       pd=dsqrt((x(i+1,j)-x(i,j+1))**2.+(y(i+1,j)-y(i,j+1))**2.+
     + (z(i+1,j)-z(i,j+1))**2.)
       pe=dsqrt((x(i,j+1)-x(i,j))**2.+(y(i,j+1)-y(i,j))**2.+
     + (z(i,j+1)-z(i,j))**2.)
       pf=dsqrt((x(i+1,j+1)-x(i,j+1))**2.+(y(i+1,j+1)-y(i,j+1))**2.+
     + (z(i+1,j+1)-z(i,j+1))**2.)
       
       pa2r=(pa*pa-pb*pb+pc*pc)/(2.*pa)
       pa1r=pa-pa2r
       phr=dsqrt(pc*pc-pa2r*pa2r)

       pa2l=(pa*pa-pe*pe+pd*pd)/(2.*pa)
       pa1l=pa-pa2l
       phl=dsqrt(pd*pd-pa2l*pa2l)

c      Quadrilater coordinates
       pl1x(i,j)=px0
       pl1y(i,j)=py0

       pr1x(i,j)=pa*dcos(ptheta)+px0
       pr1y(i,j)=pa*dsin(ptheta)+py0

       pl2x(i,j)=pa1l*dcos(ptheta)-phl*dsin(ptheta)+px0
       pl2y(i,j)=pa1l*dsin(ptheta)+phl*dcos(ptheta)+py0

       pr2x(i,j)=pa1r*dcos(ptheta)-phr*dsin(ptheta)+px0
       pr2y(i,j)=pa1r*dsin(ptheta)+phr*dcos(ptheta)+py0

       px0=pl2x(i,j)
       py0=pl2y(i,j)
       ptheta=datan((pr2y(i,j)-pl2y(i,j))/(pr2x(i,j)-pl2x(i,j)))
       
       end do
       
       end do

c      Retain every real intrados panel before intake's explicit support
c      temporarily occupies the shared first-intrados segment.  The exact
c      endpoint and source-distance comparisons make the typed panels
c      authoritative without publishing them until vent processing is done.
       allocate(neutral_intrados_panels(0:nribss-1))
       do i=0,nribss-1
       call develop_intrados_panel(wing_spatial_ribs(i),
     + wing_spatial_ribs(i+1),wing_profile_topologies(i),
     + wing_profile_topologies(i+1),neutral_intrados_panels(i),
     + typed_intrados_source_distances,domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)') 'ERROR: typed intrados panel ',i,': ',
     + trim(domain_model_message)
       stop 1
       end if

       do j=wing_profile_topologies(i)%intrados%first,
     + wing_profile_topologies(i)%intrados%last-1
       k=j-wing_profile_topologies(i)%intrados%first+1
       typed_coordinates_match=geometry_values_are_close(
     + neutral_intrados_panels(i)%lower_segment_start_u(k),pl1x(i,j)).and.
     + geometry_values_are_close(
     + neutral_intrados_panels(i)%lower_segment_start_v(k),pl1y(i,j)).and.
     + geometry_values_are_close(
     + neutral_intrados_panels(i)%lower_segment_end_u(k),pl2x(i,j)).and.
     + geometry_values_are_close(
     + neutral_intrados_panels(i)%lower_segment_end_v(k),pl2y(i,j)).and.
     + geometry_values_are_close(
     + neutral_intrados_panels(i)%higher_segment_start_u(k),pr1x(i,j)).and.
     + geometry_values_are_close(
     + neutral_intrados_panels(i)%higher_segment_start_v(k),pr1y(i,j)).and.
     + geometry_values_are_close(
     + neutral_intrados_panels(i)%higher_segment_end_u(k),pr2x(i,j)).and.
     + geometry_values_are_close(
     + neutral_intrados_panels(i)%higher_segment_end_v(k),pr2y(i,j))
       if (.not.typed_coordinates_match) then
       write (*,'(A,2(I0,A))') 'ERROR: typed/legacy intrados endpoint at ',
     + i,', point ',j,''
       stop 1
       end if

c      Keep the distance oracle independent of the pure developer.
       pa=dsqrt((x(i+1,j)-x(i,j))**2.+(y(i+1,j)-y(i,j))**2.+
     + (z(i+1,j)-z(i,j))**2.)
       pb=dsqrt((x(i+1,j+1)-x(i,j))**2.+
     + (y(i+1,j+1)-y(i,j))**2.+(z(i+1,j+1)-z(i,j))**2.)
       pc=dsqrt((x(i+1,j+1)-x(i+1,j))**2.+
     + (y(i+1,j+1)-y(i+1,j))**2.+
     + (z(i+1,j+1)-z(i+1,j))**2.)
       pd=dsqrt((x(i+1,j)-x(i,j+1))**2.+
     + (y(i+1,j)-y(i,j+1))**2.+(z(i+1,j)-z(i,j+1))**2.)
       pe=dsqrt((x(i,j+1)-x(i,j))**2.+(y(i,j+1)-y(i,j))**2.+
     + (z(i,j+1)-z(i,j))**2.)
       pf=dsqrt((x(i+1,j+1)-x(i,j+1))**2.+
     + (y(i+1,j+1)-y(i,j+1))**2.+
     + (z(i+1,j+1)-z(i,j+1))**2.)

       if (.not.geometry_values_are_close(
     + typed_intrados_source_distances(k)%start_cross_edge,pa).or.
     + .not.geometry_values_are_close(typed_intrados_source_distances(k)%
     + lower_start_to_higher_end,pb).or.
     + .not.geometry_values_are_close(typed_intrados_source_distances(k)%
     + higher_contour_edge,pc).or.
     + .not.geometry_values_are_close(typed_intrados_source_distances(k)%
     + higher_start_to_lower_end,pd).or.
     + .not.geometry_values_are_close(typed_intrados_source_distances(k)%
     + lower_contour_edge,pe).or.
     + .not.geometry_values_are_close(typed_intrados_source_distances(k)%
     + end_cross_edge,pf)) then
       write (*,'(A,2(I0,A))') 'ERROR: typed/legacy intrados distance at ',
     + i,', point ',j,''
       stop 1
       end if
       end do
       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
c     7.4.2 Air intakes (vents) panels calculus and drawing
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
       do i=0,nribss-1 ! panel i

       px0=0.
       py0=0.
       ptheta=0.

       do j=np(i,2), np(i,2)+np(i,3)-1 ! vent panels, point j

c      Distances between points
       pa=dsqrt((x(i+1,j)-x(i,j))**2.+(y(i+1,j)-y(i,j))**2.+
     + (z(i+1,j)-z(i,j))**2.)
       pb=dsqrt((x(i+1,j+1)-x(i,j))**2.+(y(i+1,j+1)-y(i,j))**2.+
     + (z(i+1,j+1)-z(i,j))**2.)
       pc=dsqrt((x(i+1,j+1)-x(i+1,j))**2.+(y(i+1,j+1)-y(i+1,j))**2.+
     + (z(i+1,j+1)-z(i+1,j))**2.)
       pd=dsqrt((x(i+1,j)-x(i,j+1))**2.+(y(i+1,j)-y(i,j+1))**2.+
     + (z(i+1,j)-z(i,j+1))**2.)
       pe=dsqrt((x(i,j+1)-x(i,j))**2.+(y(i,j+1)-y(i,j))**2.+
     + (z(i,j+1)-z(i,j))**2.)
       pf=dsqrt((x(i+1,j+1)-x(i,j+1))**2.+(y(i+1,j+1)-y(i,j+1))**2.+
     + (z(i+1,j+1)-z(i,j+1))**2.)
       
c       write (*,*) i,pa,pb,pc,pd,pe,pf

       pa2r=(pa*pa-pb*pb+pc*pc)/(2.*pa)
       pa1r=pa-pa2r
       phr=dsqrt(pc*pc-pa2r*pa2r)

       pa2l=(pa*pa-pe*pe+pd*pd)/(2.*pa)
       pa1l=pa-pa2l
       phl=dsqrt(pd*pd-pa2l*pa2l)

c      Quadrilater coordinates
       pl1x(i,j)=px0
       pl1y(i,j)=py0

       pr1x(i,j)=pa*dcos(ptheta)+px0
       pr1y(i,j)=pa*dsin(ptheta)+py0

       pl2x(i,j)=pa1l*dcos(ptheta)-phl*dsin(ptheta)+px0
       pl2y(i,j)=pa1l*dsin(ptheta)+phl*dcos(ptheta)+py0

       pr2x(i,j)=pa1r*dcos(ptheta)-phr*dsin(ptheta)+px0
       pr2y(i,j)=pa1r*dsin(ptheta)+phr*dcos(ptheta)+py0

       px0=pl2x(i,j)
       py0=pl2y(i,j)
       ptheta=datan((pr2y(i,j)-pl2y(i,j))/(pr2x(i,j)-pl2x(i,j)))

       end do
       
       end do

c      Dual-run the final regular intake rows.  The typed panel owns the exact
c      intake segments first:last-1 and the separate post-intake support at
c      last. Row nribss is not a regular panel and remains untouched.
       do i=0,nribss-1
       call develop_intake_panel(wing_spatial_ribs(i),
     + wing_spatial_ribs(i+1),wing_profile_topologies(i),
     + wing_profile_topologies(i+1),neutral_intake_panel,
     + typed_intake_source_distances,domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)') 'ERROR: typed intake panel ',i,': ',
     + trim(domain_model_message)
       stop 1
       end if

       do j=wing_profile_topologies(i)%intake%first,
     + wing_profile_topologies(i)%intake%last
       k=j-wing_profile_topologies(i)%intake%first+1
       typed_coordinates_match=.false.
       if (j.lt.wing_profile_topologies(i)%intake%last) then
       typed_coordinates_match=geometry_values_are_close(
     + neutral_intake_panel%lower_segment_start_u(k),pl1x(i,j)).and.
     + geometry_values_are_close(
     + neutral_intake_panel%lower_segment_start_v(k),pl1y(i,j)).and.
     + geometry_values_are_close(
     + neutral_intake_panel%lower_segment_end_u(k),pl2x(i,j)).and.
     + geometry_values_are_close(
     + neutral_intake_panel%lower_segment_end_v(k),pl2y(i,j)).and.
     + geometry_values_are_close(
     + neutral_intake_panel%higher_segment_start_u(k),pr1x(i,j)).and.
     + geometry_values_are_close(
     + neutral_intake_panel%higher_segment_start_v(k),pr1y(i,j)).and.
     + geometry_values_are_close(
     + neutral_intake_panel%higher_segment_end_u(k),pr2x(i,j)).and.
     + geometry_values_are_close(
     + neutral_intake_panel%higher_segment_end_v(k),pr2y(i,j))
       else
       typed_coordinates_match=geometry_values_are_close(
     + neutral_intake_panel%support_lower_start_u,pl1x(i,j)).and.
     + geometry_values_are_close(
     + neutral_intake_panel%support_lower_start_v,pl1y(i,j)).and.
     + geometry_values_are_close(
     + neutral_intake_panel%support_lower_end_u,pl2x(i,j)).and.
     + geometry_values_are_close(
     + neutral_intake_panel%support_lower_end_v,pl2y(i,j)).and.
     + geometry_values_are_close(
     + neutral_intake_panel%support_higher_start_u,pr1x(i,j)).and.
     + geometry_values_are_close(
     + neutral_intake_panel%support_higher_start_v,pr1y(i,j)).and.
     + geometry_values_are_close(
     + neutral_intake_panel%support_higher_end_u,pr2x(i,j)).and.
     + geometry_values_are_close(
     + neutral_intake_panel%support_higher_end_v,pr2y(i,j))
       end if
       if (.not.typed_coordinates_match) then
       write (*,'(A,2(I0,A))') 'ERROR: typed/legacy intake endpoint at ',
     + i,', point ',j,''
       stop 1
       end if

c      Keep this oracle independent of the typed distance helper.
       pa=dsqrt((x(i+1,j)-x(i,j))**2.+(y(i+1,j)-y(i,j))**2.+
     + (z(i+1,j)-z(i,j))**2.)
       pb=dsqrt((x(i+1,j+1)-x(i,j))**2.+
     + (y(i+1,j+1)-y(i,j))**2.+(z(i+1,j+1)-z(i,j))**2.)
       pc=dsqrt((x(i+1,j+1)-x(i+1,j))**2.+
     + (y(i+1,j+1)-y(i+1,j))**2.+
     + (z(i+1,j+1)-z(i+1,j))**2.)
       pd=dsqrt((x(i+1,j)-x(i,j+1))**2.+
     + (y(i+1,j)-y(i,j+1))**2.+(z(i+1,j)-z(i,j+1))**2.)
       pe=dsqrt((x(i,j+1)-x(i,j))**2.+(y(i,j+1)-y(i,j))**2.+
     + (z(i,j+1)-z(i,j))**2.)
       pf=dsqrt((x(i+1,j+1)-x(i,j+1))**2.+
     + (y(i+1,j+1)-y(i,j+1))**2.+
     + (z(i+1,j+1)-z(i,j+1))**2.)

       if (.not.geometry_values_are_close(
     + typed_intake_source_distances(k)%start_cross_edge,pa).or.
     + .not.geometry_values_are_close(typed_intake_source_distances(k)%
     + lower_start_to_higher_end,pb).or.
     + .not.geometry_values_are_close(typed_intake_source_distances(k)%
     + higher_contour_edge,pc).or.
     + .not.geometry_values_are_close(typed_intake_source_distances(k)%
     + higher_start_to_lower_end,pd).or.
     + .not.geometry_values_are_close(typed_intake_source_distances(k)%
     + lower_contour_edge,pe).or.
     + .not.geometry_values_are_close(typed_intake_source_distances(k)%
     + end_cross_edge,pf)) then
       write (*,'(A,2(I0,A))') 'ERROR: typed/legacy intake distance at ',
     + i,', point ',j,''
       stop 1
       end if
       end do

       call write_legacy_intake_panel(neutral_intake_panel,
     + wing_profile_topologies(i),wing_profile_topologies(i+1),
     + pl1x,pl1y,pl2x,pl2y,pr1x,pr1y,pr2x,pr2y,
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)') 'ERROR: typed intake write-back ',i,': ',
     + trim(domain_model_message)
       stop 1
       end if
       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      7.4.3 Vents drawing
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (n1draw.eq.1) then ! Override classic vents

       if (k26d.eq.0) then ! Draw classic vents

c      Saltar cella 0 si gruix nul
       iini=0
       if (cencell.lt.0.01) then
       iini=1
       end if

       do i=iini,nribss-1
       
       psep=1970.*xkf+seppix(i)*1.0d0
       psey=1371.*xkf
       ncontrol=0

c      Control if cell is closed
       if(int(rib(i,14)).eq.0.and.int(rib(i+1,14)).eq.0) then
       ncontrol=1
       end if

       ncontrol=1       

       do j=np(i,2),np(i,2)+ncontrol*(np(i,3)-2)+(ncontrol-1)

       call line(psep+pl1x(i,j),psey-pl1y(i,j),psep+pr1x(i,j),
     + psey-pr1y(i,j),6)
       call line(psep+pl1x(i,j),psey-pl1y(i,j),psep+pr2x(i,j),
     + psey-pr2y(i,j),5)
       call line(psep+pl1x(i,j),psey-pl1y(i,j),psep+pl2x(i,j),
     + psey-pl2y(i,j),4)
       call line(psep+pr1x(i,j),psey-pr1y(i,j),psep+pr2x(i,j),
     + psey-pr2y(i,j),3)
       call line(psep+pr1x(i,j),psey-pr1y(i,j),psep+pl2x(i,j),
     + psey-pl2y(i,j),2)
       call line(psep+pl2x(i,j),psey-pl2y(i,j),psep+pr2x(i,j),
     + psey-pr2y(i,j),3)

       end do

       end do

       end if ! Vents classical

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      7.4.4 Vents drawing Adre
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (k26d.eq.0) then ! Draw classic vents

c      Saltar cella 0 si gruix nul
       iini=0
       if (cencell.lt.0.01) then
       iini=1
       end if

c      Count in ribs
       do i=iini,nribss-1
       
       psep=1970.*xkf+2520.*xkf+seppix(i)*1.0d0
       psey=1371.*xkf
       ncontrol=0

c      Control if cell is closed
       if(int(rib(i,14)).eq.0.and.int(rib(i+1,14)).eq.0) then
       ncontrol=1
       end if

       ncontrol=1       

c      Verify central cell width  
   
       do j=np(i,2),np(i,2)+ncontrol*(np(i,3)-2)+(ncontrol-1)

       call line(psep+pl1x(i,j),psey-pl1y(i,j),psep+pr1x(i,j),
     + psey-pr1y(i,j),6)
c       call line(psep+pl1x(i,j),psey-pl1y(i,j),psep+pr2x(i,j),
c     + psey-pr2y(i,j),5)
       call line(psep+pl1x(i,j),psey-pl1y(i,j),psep+pl2x(i,j),
     + psey-pl2y(i,j),4)
       call line(psep+pr1x(i,j),psey-pr1y(i,j),psep+pr2x(i,j),
     + psey-pr2y(i,j),3)
c       call line(psep+pr1x(i,j),psey-pr1y(i,j),psep+pl2x(i,j),
c     + psey-pl2y(i,j),2)
       call line(psep+pl2x(i,j),psey-pl2y(i,j),psep+pr2x(i,j),
     + psey-pr2y(i,j),3)

       end do

       end do

       end if ! Vents classical

       end if ! n1draw


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
