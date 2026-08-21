!> Apply skin-tension shaping and generate production panel geometry.
!!
!! Converts the neutral development from stage 7 into left/right panel edges
!! (`u/v` slots 9/10), sewing borders (11/12), end extensions, vents, shaping
!! cuts, printed/laser variants, and their marks.  Stage 15 relies on slots
!! 9/10 being complete before adding internal color divisions.
c     8. SKIN TENSION
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.1 Calculs previs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Vores de costura

       xcos=xupp/10. ! extrados

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Longituds i amples de celles
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      8.1.1 Extrados longitud cella extrema       
       i=nribss
       rib(nribss,23)=0.
       do j=1,np(nribss,2)-1
       rib(nribss,23)=rib(i,23)+sqrt((pr2x(i-1,j)-pr1x(i-1,j))**2+
     + (pr2y(i-1,j)-pr1y(i-1,j))**2)
       end do

c      Dual-run the first neutral-panel consumer through named geometry.  The
c      typed value becomes authoritative only after the legacy oracle agrees.
       call copy_legacy_neutral_panel(pl1x,pl1y,pl2x,pl2y,
     + pr1x,pr1y,pr2x,pr2y,i-1,wing_profile_topologies(i-1),
     + wing_profile_topologies(i),surface_extrados,
     + neutral_extrados_panel,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: invalid neutral extrados: ',
     + trim(domain_model_message)
       stop 1
       end if
       call derive_neutral_boundary_edge(neutral_extrados_panel,
     + terminal_extrados_edge,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: invalid terminal extrados edge: ',
     + trim(domain_model_message)
       stop 1
       end if
       typed_geometry_metric=neutral_boundary_edge_length(
     + terminal_extrados_edge)
       if (.not.geometry_values_are_close(typed_geometry_metric,
     + rib(nribss,23))) then
       write (*,'(A,I0,2(1X,ES24.16))')
     + 'ERROR: neutral/legacy extrados length differs at rib ',nribss,
     + typed_geometry_metric,rib(nribss,23)
       stop 1
       end if
c      Typed exact segments are authoritative after the legacy oracle agrees.
       rib(nribss,23)=typed_geometry_metric

c      8.1.2 Intrados longitud cella extrema       
       i=nribss
       rib(nribss,25)=0.
       do j=np(i,2)+np(i,3)-1,np(i,1)-1
       rib(i,25)=rib(i,25)+sqrt((pr2x(i-1,j)-pr1x(i-1,j))**2+
     + (pr2y(i-1,j)-pr1y(i-1,j))**2)
       end do

       call derive_neutral_boundary_edge(
     + neutral_intrados_panels(nribss-1),terminal_intrados_edge,
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: invalid terminal intrados edge: ',
     + trim(domain_model_message)
       stop 1
       end if
       typed_geometry_metric=neutral_boundary_edge_length(
     + terminal_intrados_edge)
       if (.not.geometry_values_are_close(typed_geometry_metric,
     + rib(nribss,25))) then
       write (*,'(A,I0,2(1X,ES24.16))')
     + 'ERROR: neutral/legacy intrados length differs at rib ',nribss,
     + typed_geometry_metric,rib(nribss,25)
       stop 1
       end if
       rib(nribss,25)=typed_geometry_metric

c      8.1.2+ Inlet longitud cella extrema       
       i=nribss
       rib(nribss,26)=0.
       do j=np(i,2),np(i,2)+np(i,3)-2
       rib(i,26)=rib(i,26)+sqrt((pr2x(i-1,j)-pr1x(i-1,j))**2+
     + (pr2y(i-1,j)-pr1y(i-1,j))**2)
       end do

       call copy_legacy_neutral_panel(pl1x,pl1y,pl2x,pl2y,
     + pr1x,pr1y,pr2x,pr2y,i-1,wing_profile_topologies(i-1),
     + wing_profile_topologies(i),surface_intake,
     + neutral_intake_panel,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: invalid tip neutral intake: ',
     + trim(domain_model_message)
       stop 1
       end if
       call derive_neutral_boundary_edge(neutral_intake_panel,
     + terminal_intake_edge,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: invalid terminal intake edge: ',
     + trim(domain_model_message)
       stop 1
       end if
       typed_geometry_metric=neutral_boundary_edge_length(
     + terminal_intake_edge)
       if (.not.geometry_values_are_close(typed_geometry_metric,
     + rib(nribss,26))) then
       write (*,'(A,I0,2(1X,ES24.16))')
     + 'ERROR: neutral/legacy intake length differs at rib ',nribss,
     + typed_geometry_metric,rib(nribss,26)
       stop 1
       end if
       rib(nribss,26)=typed_geometry_metric

c      Resta de celles
       do i=0,nribss-1

       rib(i,23)=0.
       rib(i,25)=0.
       rib(i,26)=0.

c      8.1.3 Longitud extrados
       do j=1,np(i,2)-1
       rib(i,23)=rib(i,23)+sqrt((pl2x(i,j)-pl1x(i,j))**2+
     + (pl2y(i,j)-pl1y(i,j))**2)
       end do

       call copy_legacy_neutral_panel(pl1x,pl1y,pl2x,pl2y,
     + pr1x,pr1y,pr2x,pr2y,i,wing_profile_topologies(i),
     + wing_profile_topologies(i+1),surface_extrados,
     + neutral_extrados_panel,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)') 'ERROR: invalid neutral panel ',i,': ',
     + trim(domain_model_message)
       stop 1
       end if
       typed_geometry_metric=neutral_panel_lower_edge_length(
     + neutral_extrados_panel)
       if (.not.geometry_values_are_close(typed_geometry_metric,rib(i,23)))
     + then
       write (*,'(A,I0,2(1X,ES24.16))')
     + 'ERROR: neutral/legacy extrados length differs at rib ',i,
     + typed_geometry_metric,rib(i,23)
       stop 1
       end if
       rib(i,23)=typed_geometry_metric

c      8.1.4 Longitud intrados
       do j=np(i,2)+np(i,3)-1,np(i,1)-1
       rib(i,25)=rib(i,25)+sqrt((pl2x(i,j)-pl1x(i,j))**2+
     + (pl2y(i,j)-pl1y(i,j))**2)
       end do

       typed_geometry_metric=neutral_panel_lower_edge_length(
     + neutral_intrados_panels(i))
       if (.not.geometry_values_are_close(typed_geometry_metric,rib(i,25)))
     + then
       write (*,'(A,I0,2(1X,ES24.16))')
     + 'ERROR: neutral/legacy intrados length differs at rib ',i,
     + typed_geometry_metric,rib(i,25)
       stop 1
       end if
       rib(i,25)=typed_geometry_metric

c      8.1.5 Longitud inlet
       do j=np(i,2),np(i,2)+np(i,3)-2
       rib(i,26)=rib(i,26)+sqrt((pl2x(i,j)-pl1x(i,j))**2+
     + (pl2y(i,j)-pl1y(i,j))**2)
       end do

       call copy_legacy_neutral_panel(pl1x,pl1y,pl2x,pl2y,
     + pr1x,pr1y,pr2x,pr2y,i,wing_profile_topologies(i),
     + wing_profile_topologies(i+1),surface_intake,
     + neutral_intake_panel,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)') 'ERROR: invalid neutral intake ',i,': ',
     + trim(domain_model_message)
       stop 1
       end if
       typed_geometry_metric=neutral_panel_lower_edge_length(
     + neutral_intake_panel)
       if (.not.geometry_values_are_close(typed_geometry_metric,rib(i,26)))
     + then
       write (*,'(A,I0,2(1X,ES24.16))')
     + 'ERROR: neutral/legacy intake length differs at rib ',i,
     + typed_geometry_metric,rib(i,26)
       stop 1
       end if
       rib(i,26)=typed_geometry_metric

c      8.1.6 Punts de calcul amples de celles

c      Calcula punts je ji per definir ample celles, calcula ample

       je=int(np(i,2)/2)
       rib(i,22)=dsqrt((pr1x(i,je)-pl1x(i,je))**2.+(pr1y(i,je)
     + -pl1y(i,je))**2.)

       typed_geometry_metric=neutral_panel_edge_gap(
     + neutral_extrados_panel,je,neutral_gap_valid)
       if (.not.neutral_gap_valid.or.
     + .not.geometry_values_are_close(typed_geometry_metric,rib(i,22)))
     + then
       write (*,'(A,I0,2(1X,ES24.16))')
     + 'ERROR: neutral/legacy extrados width differs at panel ',i,
     + typed_geometry_metric,rib(i,22)
       stop 1
       end if
       rib(i,22)=typed_geometry_metric

       ji=int((np(i,2)+np(i,3)-1+np(i,1))/2)
       rib(i,24)=dsqrt((pr1x(i,ji)-pl1x(i,ji))**2.+(pr1y(i,ji)
     + -pl1y(i,ji))**2.)

       typed_geometry_metric=neutral_panel_edge_gap(
     + neutral_intrados_panels(i),ji,neutral_gap_valid)
       if (.not.neutral_gap_valid.or.
     + .not.geometry_values_are_close(typed_geometry_metric,rib(i,24)))
     + then
       write (*,'(A,I0,2(1X,ES24.16))')
     + 'ERROR: neutral/legacy intrados width differs at panel ',i,
     + typed_geometry_metric,rib(i,24)
       stop 1
       end if
       rib(i,24)=typed_geometry_metric

c      Print linia esquerra
c       do j=1,np(i,2)-1
c       call line(pl1x(i,j),-pl1y(i,j),pl2x(i,j),-pl2y(i,j),1)
c       end do

       end do

c      Ample celles extremes       
       i=nribss
       je=int(np(i,2)/2)
       rib(i,22)=dsqrt((pr1x(i-1,je)-pl1x(i-1,je))**2.+(pr1y(i-1,je)
     + -pl1y(i-1,je))**2.)

       call copy_legacy_neutral_panel(pl1x,pl1y,pl2x,pl2y,
     + pr1x,pr1y,pr2x,pr2y,i-1,wing_profile_topologies(i-1),
     + wing_profile_topologies(i),surface_extrados,
     + neutral_extrados_panel,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: invalid tip neutral extrados: ',
     + trim(domain_model_message)
       stop 1
       end if
       typed_geometry_metric=neutral_panel_edge_gap(
     + neutral_extrados_panel,je,neutral_gap_valid)
       if (.not.neutral_gap_valid.or.
     + .not.geometry_values_are_close(typed_geometry_metric,rib(i,22)))
     + then
       write (*,'(A,I0,2(1X,ES24.16))')
     + 'ERROR: neutral/legacy extrados width differs at rib ',i,
     + typed_geometry_metric,rib(i,22)
       stop 1
       end if
       rib(i,22)=typed_geometry_metric

       ji=int((np(i,2)+np(i,3)-1+np(i,1))/2)
       rib(i,24)=dsqrt((pr1x(i-1,ji)-pl1x(i-1,ji))**2.+(pr1y(i-1,ji)
     + -pl1y(i-1,ji))**2.)

       typed_geometry_metric=neutral_panel_edge_gap(
     + neutral_intrados_panels(i-1),ji,neutral_gap_valid)
       if (.not.neutral_gap_valid.or.
     + .not.geometry_values_are_close(typed_geometry_metric,rib(i,24)))
     + then
       write (*,'(A,I0,2(1X,ES24.16))')
     + 'ERROR: neutral/legacy intrados width differs at rib ',i,
     + typed_geometry_metric,rib(i,24)
       stop 1
       end if
       rib(i,24)=typed_geometry_metric

c      WARNING 20190624: Ample inlet not defined here!

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.2 SOBREAMPLES EXTRADOS
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c        write (*,*) "pi 8.2. =",pi

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case classic skin tension k31d=0
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (k31d.eq.0) then

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.2.1 Sobreamples esquerra extrados
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=0,nribss

c      Initialize sob points from TE to LE

       do k=1,6
       xsob(k)=((100.-(skin(7-k,1)))/100.)*rib(i,23)
       ysob(k)=rib(i,22)*skin(7-k,2)/100.
       end do

c      Longituds u

       u(i,1,7)=0.
       
       do j=1,np(i,2)-1
       u(i,j+1,7)=u(i,j,7)+sqrt((pl2x(i,j)-pl1x(i,j))**2+
     + (pl2y(i,j)-pl1y(i,j))**2)
       end do

c      Sobreamples v      

       v(i,1,7)=rib(i,22)*skin(6,2)/100.

       do j=1,np(i,2)-1

c      LE zone

       if(u(i,j+1,7).le.xsob(2)) then
       xm=(ysob(2)-ysob(1))/(xsob(2)-xsob(1))
       xn=ysob(2)-xm*xsob(2)
       v(i,j+1,7)=xm*(u(i,j+1,7))+xn
       xm1=xm
       xn1=xn
       end if
       
       if(u(i,j+1,7).gt.xsob(2).and.u(i,j+1,7).le.xsob(3)) then
       xm=(ysob(3)-ysob(2))/(xsob(3)-xsob(2))
       xn=ysob(2)-xm*xsob(2)
       v(i,j+1,7)=xm*(u(i,j+1,7))+xn
       xm2=xm
       xn2=xn
       end if

c      Central panel

       if(u(i,j+1,7).gt.xsob(3).and.u(i,j+1,7).le.xsob(4)) then
       xm=(ysob(4)-ysob(3))/(xsob(4)-xsob(3))
       xn=ysob(3)-xm*xsob(3)
       v(i,j+1,7)=xm*(u(i,j+1,7))+xn
       xm2=xm
       xn2=xn
       end if

c      TE zone

       if(u(i,j+1,7).gt.xsob(4).and.u(i,j+1,7).le.xsob(5)) then
       xm=(ysob(5)-ysob(4))/(xsob(5)-xsob(4))
       xn=ysob(4)-xm*xsob(4)
       v(i,j+1,7)=xm*(u(i,j+1,7))+xn
       xm3=xm
       xn3=xn

       end if

       if(u(i,j+1,7).gt.xsob(5)) then
       xm=(ysob(6)-ysob(5))/(xsob(6)-xsob(5))
       xn=ysob(5)-xm*xsob(5)
       v(i,j+1,7)=xm*(u(i,j+1,7))+xn
       xm3=xm
       xn3=xn
       end if

c      Calcula punts esquerra

       xdv=(pl2y(i,j)-pl1y(i,j))
       xdu=(pl2x(i,j)-pl1x(i,j))

       if (xdv.ne.0.) then
       alpl=abs(datan((pl2y(i,j)-pl1y(i,j))/(pl2x(i,j)-pl1x(i,j))))
       else
       alpl=2.*datan(1.0d0)
       end if

       if (xdu.ge.0.and.xdv.ge.0) then ! case 2-I
       siu(j)=-1.
       siv(j)=1.
       end if
       if (xdu.le.0.and.xdv.ge.0) then ! case 2-II
       siu(j)=-1.
       siv(j)=-1.
       end if
       if (xdu.ge.0.and.xdv.le.0) then ! case 2-III
       siu(j)=1.
       siv(j)=1.
       end if
       if (xdu.le.0.and.xdv.le.0) then ! case 2-IV
       siu(j)=1.
       siv(j)=-1.
       end if

c      WARNING!!!!!!!!!!! pl2x,pl2y or pl1x,pl1y?????????
c      Amb "2" fa un salt la vora esquerra...

       u(i,j+1,9)=pl2x(i,j)+siu(j)*v(i,j+1,7)*dsin(alpl)
       v(i,j+1,9)=pl2y(i,j)+siv(j)*v(i,j+1,7)*dcos(alpl)

       u(i,j+1,11)=u(i,j+1,9)+siu(j)*xupp*0.1*dsin(alpl)
       v(i,j+1,11)=v(i,j+1,9)+siv(j)*xupp*0.1*dcos(alpl)

c      Impresió de control
c       if (j.eq.10) then
c       write (*,*) "OOO ",i,u(i,j+1,9),v(i,j+1,9)  
c       end if
       
       end do ! j=1,np(i,2)-1

c      Punt inicial j=1
       
       alpl=abs(datan((pl2y(i,1)-pl1y(i,1))/(pl2x(i,1)-pl1x(i,1))))

c      Potser hauria de ser pl1x i pl1y???? Yes
       u(i,1,9)=pl1x(i,j)+siu(1)*v(i,1,7)*dsin(alpl)
       v(i,1,9)=pl1y(i,j)+siv(1)*v(i,1,7)*dcos(alpl)

       u(i,1,11)=u(i,1,9)+siu(1)*xupp*0.1*dsin(alpl)
       v(i,1,11)=v(i,1,9)+siv(1)*xupp*0.1*dcos(alpl)

c      Dual-run every regular lower boundary through the named classic law.
c      The retained scalar arithmetic remains the compatibility oracle; only a
c      complete agreeing offset contour is allowed to feed typed shaping.
       if (i.lt.nribss) then
       do j=1,np(i,2)
       call evaluate_classic_skin_tension_offset(
     + classic_extrados_tension_law,rib(i,23),rib(i,22),u(i,j,7),
     + typed_skin_tension_offset,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,A,I0,2A)')
     + 'ERROR: classic lower extrados tension failed at panel ',i,
     + ', point ',j,': ',trim(domain_model_message)
       stop 1
       end if
       if (.not.geometry_values_are_close(typed_skin_tension_offset,
     + v(i,j,7))) then
       write (*,'(A,I0,A,I0,2(1X,ES24.16))')
     + 'ERROR: classic lower extrados tension differs at panel ',i,
     + ', point ',j,typed_skin_tension_offset,v(i,j,7)
       stop 1
       end if
       v(i,j,7)=typed_skin_tension_offset
       end do
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Opció usar subrutina (sobreescriu punts anteriors)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call puntslat(i,pl1x,pl1y,pl2x,pl2y,1,np(i,2)-1,u,v,xupp,-1)

c      Build the same regular production side through the pure kernel, verify
c      its complete contour, and publish slots 9/11 with the checked writer.
       if (i.lt.nribss) then
       call copy_legacy_neutral_panel(pl1x,pl1y,pl2x,pl2y,
     + pr1x,pr1y,pr2x,pr2y,i,wing_profile_topologies(i),
     + wing_profile_topologies(i+1),surface_extrados,
     + neutral_extrados_panel,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: invalid classic neutral extrados at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       call shape_neutral_panel_side(neutral_extrados_panel,
     + panel_side_lower,v(i,1:np(i,2),7),xupp,
     + typed_extrados_lower_side,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: classic lower extrados shaping failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       do j=1,np(i,2)
       typed_coordinates_match=
     + geometry_values_are_close(typed_extrados_lower_side%sewing_u(j),
     + u(i,j,9)).and.
     + geometry_values_are_close(typed_extrados_lower_side%sewing_v(j),
     + v(i,j,9)).and.
     + geometry_values_are_close(typed_extrados_lower_side%cut_u(j),
     + u(i,j,11)).and.
     + geometry_values_are_close(typed_extrados_lower_side%cut_v(j),
     + v(i,j,11))
       if (.not.typed_coordinates_match) then
       write (*,'(A,I0,A,I0)')
     + 'ERROR: classic lower extrados shaping differs at panel ',i,
     + ', point ',j
       stop 1
       end if
       end do
       call write_legacy_shaped_panel_side(typed_extrados_lower_side,
     + wing_profile_topologies(i),u,v,domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: classic lower extrados publication failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       end if

       end do  ! i

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.2.2 Sobreamples dreta extrados
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Sobreamples dreta

       do i=0,nribss

c      Initialize sob points

       do k=1,6

       xsob(k)=((100.-(skin(7-k,1)))/100.)*rib(i+1,23)
       ysob(k)=rib(i,22)*skin(7-k,2)/100.

       end do

c      Longitud u

       u(i,1,8)=0.
       
       do j=1,np(i,2)-1

       u(i,j+1,8)=u(i,j,8)+sqrt((pr2x(i,j)-pr1x(i,j))**2+
     + (pr2y(i,j)-pr1y(i,j))**2)

       end do

c      Sobreamples v      

       v(i,1,8)=rib(i,22)*skin(6,2)/100.

       do j=1,np(i,2)-1

c      LE zone

       if(u(i,j+1,8).le.xsob(2)) then
       xm=(ysob(2)-ysob(1))/(xsob(2)-xsob(1))
       xn=ysob(2)-xm*xsob(2)
       v(i,j+1,8)=xm*(u(i,j+1,8))+xn
       xm1=xm
       xn1=xn
       end if

       if(u(i,j+1,8).gt.xsob(2).and.u(i,j+1,8).le.xsob(3)) then
       xm=(ysob(3)-ysob(2))/(xsob(3)-xsob(2))
       xn=ysob(2)-xm*xsob(2)
       v(i,j+1,8)=xm*(u(i,j+1,8))+xn
       xm2=xm
       xn2=xn
       end if

c      Central panel

       if(u(i,j+1,8).gt.xsob(3).and.u(i,j+1,8).le.xsob(4)) then
       xm=(ysob(4)-ysob(3))/(xsob(4)-xsob(3))
       xn=ysob(3)-xm*xsob(3)
       v(i,j+1,8)=xm*(u(i,j+1,8))+xn
       xm2=xm
       xn2=xn
       end if

c      TE zone

       if(u(i,j+1,8).gt.xsob(4).and.u(i,j+1,8).le.xsob(5)) then
       xm=(ysob(5)-ysob(4))/(xsob(5)-xsob(4))
       xn=ysob(4)-xm*xsob(4)
       v(i,j+1,8)=xm*(u(i,j+1,8))+xn
       xm3=xm
       xn3=xn
       end if

       if(u(i,j+1,8).gt.xsob(5)) then
       xm=(ysob(6)-ysob(5))/(xsob(6)-xsob(5))
       xn=ysob(5)-xm*xsob(5)
       v(i,j+1,8)=xm*(u(i,j+1,8))+xn
       xm3=xm
       xn3=xn
       end if

c      Calcula punts dreta

       xdv=(pr2y(i,j)-pr1y(i,j))
       xdu=(pr2x(i,j)-pr1x(i,j))

       if (xdv.ne.0.) then
       alpr=abs(datan((pr2y(i,j)-pr1y(i,j))/(pr2x(i,j)-pr1x(i,j))))
       else
       alpr=2.*datan(1.0d0)
       end if

       if (xdu.ge.0.and.xdv.ge.0) then ! case 3-I
       siu(j)=1.
       siv(j)=-1.
       end if
       if (xdu.le.0.and.xdv.ge.0) then ! case 3-II
       siu(j)=1.
       siv(j)=1.
       end if
       if (xdu.ge.0.and.xdv.le.0) then ! case 3-III
       siu(j)=-1.
       siv(j)=-1.
       end if
       if (xdu.le.0.and.xdv.le.0) then ! case 3-IV
       siu(j)=-1.
       siv(j)=1.
       end if

       u(i,j+1,10)=pr2x(i,j)+siu(j)*v(i,j+1,8)*dsin(alpr)
       v(i,j+1,10)=pr2y(i,j)+siv(j)*v(i,j+1,8)*dcos(alpr)

       u(i,j+1,12)=u(i,j+1,10)+siu(j)*xupp*0.1*dsin(alpr)
       v(i,j+1,12)=v(i,j+1,10)+siv(j)*xupp*0.1*dcos(alpr)

       end do  ! j=1,np(i,2)-1

c      Initial point not defined in previous bucle

       alpr=abs(datan((pr2y(i,1)-pr1y(i,1))/(pr2x(i,1)-pr1x(i,1))))

       u(i,1,10)=pr1x(i,1)+siu(1)*v(i,1,8)*dsin(alpr)
       v(i,1,10)=pr1y(i,1)+siv(1)*v(i,1,8)*dcos(alpr)
       
       u(i,1,12)=u(i,1,10)+siu(1)*xupp*0.1*dsin(alpr)
       v(i,1,12)=v(i,1,10)+siv(1)*xupp*0.1*dcos(alpr)

c      The classic higher edge changes boundary contour length but retains the
c      current panel width.  Preserve that asymmetric scaling exactly.
       if (i.lt.nribss) then
       do j=1,np(i,2)
       call evaluate_classic_skin_tension_offset(
     + classic_extrados_tension_law,rib(i+1,23),rib(i,22),u(i,j,8),
     + typed_skin_tension_offset,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,A,I0,2A)')
     + 'ERROR: classic higher extrados tension failed at panel ',i,
     + ', point ',j,': ',trim(domain_model_message)
       stop 1
       end if
       if (.not.geometry_values_are_close(typed_skin_tension_offset,
     + v(i,j,8))) then
       write (*,'(A,I0,A,I0,2(1X,ES24.16))')
     + 'ERROR: classic higher extrados tension differs at panel ',i,
     + ', point ',j,typed_skin_tension_offset,v(i,j,8)
       stop 1
       end if
       v(i,j,8)=typed_skin_tension_offset
       end do
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Opció usar subrutina (sobreescriu punts anteriors)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call puntslat(i,pr1x,pr1y,pr2x,pr2y,1,np(i,2)-1,u,v,xupp,1)

c      Publish a higher classic side only for a real panel; the terminal row's
c      historical scratch writes remain untouched and are not modeled as a
c      fabricated outward panel.
       if (i.lt.nribss) then
       call copy_legacy_neutral_panel(pl1x,pl1y,pl2x,pl2y,
     + pr1x,pr1y,pr2x,pr2y,i,wing_profile_topologies(i),
     + wing_profile_topologies(i+1),surface_extrados,
     + neutral_extrados_panel,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: invalid classic neutral extrados at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       call shape_neutral_panel_side(neutral_extrados_panel,
     + panel_side_higher,v(i,1:np(i,2),8),xupp,
     + typed_extrados_higher_side,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: classic higher extrados shaping failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       do j=1,np(i,2)
       typed_coordinates_match=
     + geometry_values_are_close(typed_extrados_higher_side%sewing_u(j),
     + u(i,j,10)).and.
     + geometry_values_are_close(typed_extrados_higher_side%sewing_v(j),
     + v(i,j,10)).and.
     + geometry_values_are_close(typed_extrados_higher_side%cut_u(j),
     + u(i,j,12)).and.
     + geometry_values_are_close(typed_extrados_higher_side%cut_v(j),
     + v(i,j,12))
       if (.not.typed_coordinates_match) then
       write (*,'(A,I0,A,I0)')
     + 'ERROR: classic higher extrados shaping differs at panel ',i,
     + ', point ',j
       stop 1
       end if
       end do
       call write_legacy_shaped_panel_side(typed_extrados_higher_side,
     + wing_profile_topologies(i),u,v,domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: classic higher extrados publication failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       end if

c      Leading edge segment unformated rib(i,96) extra

       if (i.lt.nribss) then
       j=np(i,2)
       rib(i,96)=dsqrt((u(i,j,9)-u(i,j,10))**2.+
     + ((v(i,j,9)-v(i,j,10))**2.))
       end if

       end do  ! i

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       end if  ! Case classic skin tension k31d=0
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case new skin tension k31d=1 Linear interpolation EXTRADOS
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (k31d.eq.1) then

c       write (*,*) "NOTE: NEW skin tension"

c      Case 1: Linear interpolation and cell width is rib(i,22) in extrados
c      and rib(i,24) in intrados
c      If (ntypei31(i).eq.1) then

c      Initialize sob points
       do i=0,nribss
       do k=1,skinpoints(i)
       xsobnew(i,k)=((100.-(skinnew(i,skinpoints(i)+1-k,1)))/100.)
     + *rib(i,23)
       ysobnew(i,k)=rib(i,22)*skinnew(i,skinpoints(i)+1-k,2)/100.
       end do ! k
       end do ! i (initialize all sob points)

       do i=0,nribss

c      Assign lengths and sob distances
       u(i,1,7)=0.
       u(i,1,8)=0.

       typed_geometry_metric=0.d0
       do j=1,np(i,2)
       do k=1,skinpoints(i)-1

c      Left side
       if (u(i,j,7).ge.xsobnew(i,k).and.u(i,j,7).le.xsobnew(i,k+1)
     + *1.001) ! NOTE: Comparar reals és perillós, afegim multiplicador
     + then
       xm=(ysobnew(i,k+1)-ysobnew(i,k))/(xsobnew(i,k+1)-xsobnew(i,k))
       xn=ysobnew(i,k+1)-xm*xsobnew(i,k+1)
       v(i,j,7)=xm*(u(i,j,7))+xn
       end if

       u(i,j+1,7)=u(i,j,7)+sqrt((pl2x(i,j)-pl1x(i,j))**2+
     + (pl2y(i,j)-pl1y(i,j))**2)

       end do ! k

c      The retained interpolation is the compatibility oracle.  For real
c      panels, independently accumulate the exact neutral-contour distance,
c      compare the typed law result, and publish the agreeing offset.  The
c      terminal row is a boundary rather than a regular panel.
       if (i.lt.nribss) then
       if (.not.geometry_values_are_close(typed_geometry_metric,
     + u(i,j,7))) then
       write (*,'(A,I0,A,I0,2(1X,ES24.16))')
     + 'ERROR: extrados lower distance mismatch at panel ',i,
     + ', point ',j,typed_geometry_metric,u(i,j,7)
       stop 1
       end if
       call evaluate_skin_tension_offset(
     + typed_extrados_tension_laws(i),rib(i,23),rib(i,22),
     + typed_geometry_metric,typed_skin_tension_offset,
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,A,I0,2A)')
     + 'ERROR: invalid extrados lower tension at panel ',i,
     + ', point ',j,': ',trim(domain_model_message)
       stop 1
       end if
       if (.not.geometry_values_are_close(typed_skin_tension_offset,
     + v(i,j,7))) then
       write (*,'(A,I0,A,I0,2(1X,ES24.16))')
     + 'ERROR: extrados lower offset mismatch at panel ',i,
     + ', point ',j,typed_skin_tension_offset,v(i,j,7)
       stop 1
       end if
       v(i,j,7)=typed_skin_tension_offset
       if (j.lt.np(i,2)) then
       typed_geometry_metric=typed_geometry_metric+sqrt(
     + (pl2x(i,j)-pl1x(i,j))**2+
     + (pl2y(i,j)-pl1y(i,j))**2)
       end if
       end if

       end do ! j

c      Compute points left side
       call puntslat(i,pl1x,pl1y,pl2x,pl2y,1,np(i,2),u,v,xupp,-1)

c      Shape each real lower extrados boundary from the exact neutral panel.
c      Compare the complete typed result before publishing any slot so a late
c      mismatch cannot leave a partially replaced production side.
       if (i.lt.nribss) then
       call copy_legacy_neutral_panel(pl1x,pl1y,pl2x,pl2y,
     + pr1x,pr1y,pr2x,pr2y,i,wing_profile_topologies(i),
     + wing_profile_topologies(i+1),surface_extrados,
     + neutral_extrados_panel,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: invalid neutral extrados before lower shaping at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       call shape_neutral_panel_side(neutral_extrados_panel,
     + panel_side_lower,v(i,1:np(i,2),7),xupp,
     + typed_extrados_lower_side,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: typed lower extrados shaping failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       do j=1,np(i,2)
       typed_coordinates_match=
     + geometry_values_are_close(typed_extrados_lower_side%sewing_u(j),
     + u(i,j,9)).and.
     + geometry_values_are_close(typed_extrados_lower_side%sewing_v(j),
     + v(i,j,9)).and.
     + geometry_values_are_close(typed_extrados_lower_side%cut_u(j),
     + u(i,j,11)).and.
     + geometry_values_are_close(typed_extrados_lower_side%cut_v(j),
     + v(i,j,11))
       if (.not.typed_coordinates_match) then
       write (*,'(A,I0,A,I0)')
     + 'ERROR: typed/legacy lower extrados shaping differs at panel ',i,
     + ', point ',j
       stop 1
       end if
       end do
       call write_legacy_shaped_panel_side(typed_extrados_lower_side,
     + wing_profile_topologies(i),u,v,domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: lower extrados publication failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       end if

       end do  ! i

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Right side: Use length of rib(i+1,23)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Initialize again sob points
       do i=0,nribss
       do k=1,skinpoints(i)
       xsobnew(i,k)=((100.-(skinnew(i+1,skinpoints(i+1)+1-k,1)))/100.)
     + *rib(i+1,23)
       ysobnew(i,k)=rib(i,22)*skinnew(i+1,skinpoints(i+1)+1-k,2)/100.
       end do ! k
       end do ! i (initialize all sob points)

       do i=0,nribss

c      Assign lengths and sob distances
       u(i,1,7)=0.
       u(i,1,8)=0.

       typed_geometry_metric=0.d0
       do j=1,np(i,2)
       do k=1,skinpoints(i)

c      Right side
       if (i.lt.nribss) then

       if (u(i,j,8).ge.xsobnew(i+0,k).and.u(i,j,8).le.xsobnew(i+0,k+1)
     + *1.001) ! NOTE: Comparar reals és perillós, afegim multiplicador
     + then
       xm=(ysobnew(i+0,k+1)-ysobnew(i+0,k))/
     +    (xsobnew(i+0,k+1)-xsobnew(i+0,k))
       xn=ysobnew(i+0,k+1)-xm*xsobnew(i+0,k+1)
       v(i,j,8)=xm*(u(i,j,8))+xn
       end if

       u(i,j+1,8)=u(i,j,8)+sqrt((pr2x(i,j)-pr1x(i,j))**2+
     + (pr2y(i,j)-pr1y(i,j))**2)

c      VERIFICAR QUE:
c      u(i,np(i,2),8)=xsobnew(i+1,kmax) !!!!!!!!!!!!!!!!!!
c      I coherencia amb pr* !!!!

       end if

       end do ! k

c      The higher edge uses boundary i+1's extrados law but the current
c      panel width, matching the Section-31 legacy convention.
       if (i.lt.nribss) then
       if (.not.geometry_values_are_close(typed_geometry_metric,
     + u(i,j,8))) then
       write (*,'(A,I0,A,I0,2(1X,ES24.16))')
     + 'ERROR: extrados higher distance mismatch at panel ',i,
     + ', point ',j,typed_geometry_metric,u(i,j,8)
       stop 1
       end if
       call evaluate_skin_tension_offset(
     + typed_extrados_tension_laws(i+1),rib(i+1,23),rib(i,22),
     + typed_geometry_metric,typed_skin_tension_offset,
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,A,I0,2A)')
     + 'ERROR: invalid extrados higher tension at panel ',i,
     + ', point ',j,': ',trim(domain_model_message)
       stop 1
       end if
       if (.not.geometry_values_are_close(typed_skin_tension_offset,
     + v(i,j,8))) then
       write (*,'(A,I0,A,I0,2(1X,ES24.16))')
     + 'ERROR: extrados higher offset mismatch at panel ',i,
     + ', point ',j,typed_skin_tension_offset,v(i,j,8)
       stop 1
       end if
       v(i,j,8)=typed_skin_tension_offset
       if (j.lt.np(i,2)) then
       typed_geometry_metric=typed_geometry_metric+sqrt(
     + (pr2x(i,j)-pr1x(i,j))**2+
     + (pr2y(i,j)-pr1y(i,j))**2)
       end if
       end if

       end do ! j

c       write (*,*) i,u(i,np(i,2),7),xsobnew(i-1,skinpoints(i)),
c     + u(i,np(i,2),8),xsobnew(i,skinpoints(i))

c      Compute points right side
       call puntslat(i,pr1x,pr1y,pr2x,pr2y,1,np(i,2),u,v,xupp,1)

c      Repeat the exact-panel shaping contract for the higher-index edge.
c      Slots 10/12 are published only after every sewing and cut coordinate
c      agrees with the retained puntslat result.
       if (i.lt.nribss) then
       call copy_legacy_neutral_panel(pl1x,pl1y,pl2x,pl2y,
     + pr1x,pr1y,pr2x,pr2y,i,wing_profile_topologies(i),
     + wing_profile_topologies(i+1),surface_extrados,
     + neutral_extrados_panel,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: invalid neutral extrados before higher shaping at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       call shape_neutral_panel_side(neutral_extrados_panel,
     + panel_side_higher,v(i,1:np(i,2),8),xupp,
     + typed_extrados_higher_side,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: typed higher extrados shaping failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       do j=1,np(i,2)
       typed_coordinates_match=
     + geometry_values_are_close(typed_extrados_higher_side%sewing_u(j),
     + u(i,j,10)).and.
     + geometry_values_are_close(typed_extrados_higher_side%sewing_v(j),
     + v(i,j,10)).and.
     + geometry_values_are_close(typed_extrados_higher_side%cut_u(j),
     + u(i,j,12)).and.
     + geometry_values_are_close(typed_extrados_higher_side%cut_v(j),
     + v(i,j,12))
       if (.not.typed_coordinates_match) then
       write (*,'(A,I0,A,I0)')
     + 'ERROR: typed/legacy higher extrados shaping differs at panel ',i,
     + ', point ',j
       stop 1
       end if
       end do
       call write_legacy_shaped_panel_side(typed_extrados_higher_side,
     + wing_profile_topologies(i),u,v,domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: higher extrados publication failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       end if

c      Leading edge segment unformated rib(i,96) extra
       if (i.lt.nribss) then
       j=np(i,2)
       rib(i,96)=dsqrt((u(i,j,9)-u(i,j,10))**2.+
     + ((v(i,j,9)-v(i,j,10))**2.))
       end if

       end do  ! i

c       end if ! end case 1.1

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       end if  ! k31d=1 Linear interpolation EXTRADOS
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.2.3 Reformat right side of extrados panels (optional)
c            "antiprecission"
c      Method not recommended
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Jump to the end if ndif=1000 (no effect)
       if (ndif.ne.1000) then 
      
c      Avoid xndif=0
       if (xndif.eq.0.) then
       xndif=0.01
       end if

c      Calcula diferencies de longitud costat exterior - interior

c       ndif=15
c       xndif=0.9

       do i=1,nribss-1
       rib(i,80)=0.
       rib(i,79)=0.
       end do

       do i=1,nribss-1

       do j=np(i,2)-ndif,np(i,2)-1

       rib(i,80)=rib(i,80)+sqrt((u(i,j,11)-u(i,j+1,11))**2.+
     + (v(i,j,11)-v(i,j+1,11))**2.)

       rib(i,79)=rib(i,79)+sqrt((u(i-1,j,12)-u(i-1,j+1,12))**2.+
     + (v(i-1,j,12)-v(i-1,j+1,12))**2.)

       end do

       rib(i,81)=(rib(i,80)-rib(i,79))*xndif

c       write (*,*) "rib(i,81) ", i, rib(i,81)

       end do

c      Redefinir punts 10 i 12 del morro dels panells

       do i=1,nribss-1

       xxa=dsqrt((u(i-1,np(i-1,2),9)-u(i-1,np(i-1,2),10))**2.+
     + (v(i-1,np(i-1,2),9)-v(i-1,np(i-1,2),10))**2.)

       tetha3=dacos(rib(i,81)/(2.*xxa))

       tetha1=datan((v(i-1,np(i-1,2),9)-v(i-1,np(i-1,2),10))/
     + (u(i-1,np(i-1,2),10)-u(i-1,np(i-1,2),9)))

       tetha2=pi-tetha3-tetha1

       u(i-1,np(i,2),28)=u(i-1,np(i,2),10)+rib(i,81)*dcos(tetha2)
       v(i-1,np(i,2),28)=v(i-1,np(i,2),10)+rib(i,81)*dsin(tetha2)
      
       end do

c      Alineació del tram modificat      

       do i=1,nribss-1

       tetha4=datan((v(i-1,np(i,2),28)-v(i-1,np(i,2),10))/
     + (u(i-1,np(i,2),28)-u(i-1,np(i,2),10)))

       rib(i,82)=dsqrt((u(i-1,np(i-1,2)-ndif,10)-u(i-1,np(i-1,2),10))
     + **2.+(v(i-1,np(i-1,2)-ndif,10)-v(i-1,np(i-1,2),10))**2.)

       do j=np(i-1,2)-ndif, np(i-1,2)

       xdis=dsqrt((u(i-1,j,10)-u(i-1,np(i-1,2)-ndif,10))**2.+
     + (v(i-1,j,10)-v(i-1,np(i-1,2)-ndif,10))**2.)

       u(i-1,j,10)=u(i-1,j,10)+(xdis**3./(rib(i,82)**3.))*
     + rib(i,81)*dcos(tetha4)
       v(i-1,j,10)=v(i-1,j,10)+(xdis**3./(rib(i,82)**3.))*
     + rib(i,81)*dsin(tetha4)

       end do

       end do

c      Actualitza les vores i punts especials

       do i=0,nribss-2

       do j=np(i,2)-ndif, np(i,2)

       alpr=abs(datan((v(i,j,10)-v(i,j-1,10))/
     + (u(i,j,10)-u(i,j-1,10))))
       u(i,j,12)=u(i,j,10)+xcos*dsin(alpr)
       v(i,j,12)=v(i,j,10)-xcos*dcos(alpr)

       end do

       alple=abs(datan((v(i,np(i,2),10)-v(i,np(i,2),9))/
     + (u(i,np(i,2),10)-u(i,np(i,2),9))))

       u(i,np(i,2),15)=u(i,np(i,2),10)+xupple*0.1*dsin(alple)
       v(i,np(i,2),15)=v(i,np(i,2),10)+xupple*0.1*dcos(alple)

       u(i,np(i,2),25)=u(i,np(i,2),15)+xupp*0.1*dcos(alple)
       v(i,np(i,2),25)=v(i,np(i,2),15)-xupp*0.1*dsin(alple)


       amle=(v(i,np(i,2),10)-v(i,np(i,2),9))/
     + (u(i,np(i,2),10)-u(i,np(i,2),9))

ccccccc Warning pr2y...       
       amler=((pr2y(i,np(i,2)-1)-pr1y(i,np(i,2)-1))/
     + (pr2x(i,np(i,2)-1)-pr1x(i,np(i,2)-1)))

       b1=v(i,np(i,2),15)-amle*u(i,np(i,2),15)
       b2=v(i,np(i,2),12)-amler*u(i,np(i,2),12)

       u(i,np(i,2),27)=(b2-b1)/(amle-amler)
       v(i,np(i,2),27)=amle*u(i,np(i,2),27)+b1

       end do


       end if ! Jump section if ne=1000
       
c14     continue

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.2.4 REFORMAT PANELS FOR PERFECT MATCHING
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Detect if ndif parameter is set for reformat
       if (ndif.eq.1000) then

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Compute rib and panels lengths (also done in chapter 11. (!!!!))
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=1,nribss

       rib(i,30)=0. ! extra panel left
       rib(i,31)=0. ! extra rib
       rib(i,32)=0. ! extra panel right
       rib(i,33)=0. ! intra panel left
       rib(i,34)=0. ! intra rib
       rib(i,35)=0. ! intra panel right

c      At left
       do j=1,np(i,2)-1

       rib(i,30)=rib(i,30)+sqrt((u(i-1,j,10)-u(i-1,j+1,10))**2.+
     + ((v(i-1,j,10)-v(i-1,j+1,10))**2.))

       rib(i,31)=rib(i,31)+sqrt((u(i,j,3)-u(i,j+1,3))**2.+((v(i,j,3)
     + -v(i,j+1,3))**2.))

       rib(i,32)=rib(i,32)+sqrt((u(i,j,9)-u(i,j+1,9))**2.+((v(i,j,9)
     + -v(i,j+1,9))**2.))

       end do

       do j=np(i,2)+np(i,3),np(i,1)-1

       rib(i,33)=rib(i,33)+sqrt((u(i-1,j,10)-u(i-1,j+1,10))**2.+
     + ((v(i-1,j,10)-v(i-1,j+1,10))**2.))

       rib(i,34)=rib(i,34)+sqrt((u(i,j,3)-u(i,j+1,3))**2.+((v(i,j,3)
     + -v(i,j+1,3))**2.))

       rib(i,35)=rib(i,35)+sqrt((u(i,j,9)-u(i,j+1,9))**2.+((v(i,j,9)
     + -v(i,j+1,9))**2.))

       end do

c      Amplification cofficients
       rib(i,36)=rib(i,30)/rib(i,31)
       rib(i,37)=rib(i,32)/rib(i,31)
       rib(i,38)=rib(i,33)/rib(i,34)
       rib(i,39)=rib(i,35)/rib(i,34)

       rib(0,36)=rib(1,36)
       rib(0,37)=rib(1,36)
       rib(0,38)=rib(1,38)
       rib(0,39)=rib(1,38)

c      Differences with control coefficient xndif

       rib(i,90)=xndif*(rib(i,31)-rib(i,30))
       rib(i,92)=xndif*(rib(i,31)-rib(i,32))

       end do

c      Define rib 0
       rib(0,30)=rib(1,32)
       rib(0,31)=rib(1,31)
       rib(0,32)=rib(1,30)

       rib(0,33)=rib(1,35)
       rib(0,34)=rib(1,34)
       rib(0,35)=rib(1,33)

       rib(0,90)=xndif*(rib(0,31)-rib(0,30))
       rib(0,92)=xndif*(rib(0,31)-rib(0,32))


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Identify point jirl-r where initialize reformating skin(3,1) in %
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=1,nribss

c      Left side

       xirl(i)=rib(i,30)*skin(3,1)/100.

       jirl(i)=1
       rib(i,40)=0.
       do j=1,np(i,2)-1
       rib(i,40)=rib(i,40)+sqrt((u(i-1,j,10)-u(i-1,j+1,10))**2.+
     + (v(i-1,j,10)-v(i-1,j+1,10))**2.)
       if (rib(i,40).lt.rib(i,30)-xirl(i)) then
       x40=rib(i,40)
       jirl(i)=j
       end if
       end do

c      Right side

       xirr(i)=rib(i,32)*skin(3,1)/100.

       jirr(i)=1
       rib(i,42)=0.
       do j=1,np(i,2)-1
       rib(i,42)=rib(i,42)+sqrt((u(i,j,9)-u(i,j+1,9))**2.+
     + (v(i,j,9)-v(i,j+1,9))**2.)
       if (rib(i,42).lt.rib(i,32)-xirr(i)) then
       x42=rib(i,42)
       jirr(i)=j
       end if
       end do

       end do

c      Assign in rib 0
       jirl(0)=jirl(1)
       jirr(0)=jirr(1)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat the last segments
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat left side - extrados
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=1,nribss

c      Row i-1 is the higher edge of real panel i-1, including the physical
c      tip edge when i=nribss.  The typed routine owns exactly points 1:np;
c      its internal fixed-form oracle must agree bit for bit before write-back.
       call reformat_legacy_regular_higher_extrados_row(i-1,np(i,2),
     + int(jirl(i)),rib(i,30),rib(i,31),xndif,u,v,
     + anglee,distee,siu,siv,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: regular higher extrados reformat failed at panel ',i-1,
     + ': ',trim(domain_model_message)
       stop 1
       end if

c      The following point is intake support, not part of the extrados edge.
       j=np(i,2)+1
       u(i-1,j,10)=u(i-1,j-1,10)+u(i-1,j-1,10)-u(i-1,j-2,10)
       v(i-1,j,10)=v(i-1,j-1,10)+v(i-1,j-1,10)-v(i-1,j-2,10)

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat right side - extrados
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=0,nribss

c      Regular panel rows now cross a typed boundary which independently
c      checks the historical forward loop bit for bit.  The physical terminal
c      row remains below as the legacy oracle until terminal extrados ownership
c      is represented explicitly.  Neither path owns the intake support point
c      extrapolated after this branch.
       if (i.lt.nribss) then
       call reformat_legacy_regular_lower_extrados_row(i,np(i,2),
     + int(jirr(i)),
     + rib(i,32),rib(i,31),xndif,u,v,anglee,distee,siu,siv,
     + domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: regular lower extrados reformat failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if

       else

c      Calcule length of the reformating polyline for terminal legacy row

       dist1=0.
       do j=1,jirr(i)-1
       dist1=dist1+sqrt((v(i,j,9)-v(i,j+1,9))**2.+
     + (u(i,j,9)-u(i,j+1,9))**2.)
       end do
       
       dist2=0.
       do j=jirr(i),np(i,2)-1
       dist2=dist2+sqrt((v(i,j,9)-v(i,j+1,9))**2.+
     + (u(i,j,9)-u(i,j+1,9))**2.)
       end do

       dist3=dist2+rib(i,92)

       distk=dist3/dist2  ! amplification coefficient

c      Reformat
      
       j=max(1,int(jirr(i))-1)
       u(i,j,32)=u(i,j,9)
       v(i,j,32)=v(i,j,9)

       do j=jirr(i),np(i,2)-1

c      Atention whit angle definition using abs tangents
c      Make subroutines
c      Angles and distances, left side

       xdv=(v(i,j+1,9)-v(i,j,9))
       xdu=(u(i,j+1,9)-u(i,j,9))
       if (xdu.ne.0.) then
       anglee(j)=abs(datan(xdv/xdu))
       else
       anglee(j)=2.*datan(1.0d0)
       end if

       if (xdu.ge.0.and.xdv.ge.0) then ! case 1-I
       siu(j)=1.
       siv(j)=1.
       end if
       if (xdu.le.0.and.xdv.ge.0) then ! case 1-II
       siu(j)=-1.
       siv(j)=1.
       end if
       if (xdu.ge.0.and.xdv.le.0) then ! case 1-III
       siu(j)=1.
       siv(j)=-1.
       end if
       if (xdu.le.0.and.xdv.le.0) then ! case 1-IV
       siu(j)=-1.
       siv(j)=-1.
       end if

       distee(j)=dsqrt((v(i,j,9)-v(i,j+1,9))**2.+
     + (u(i,j,9)-u(i,j+1,9))**2.)

       end do

c      Define

       do j=jirr(i),np(i,2)-1
       u(i,j+1,32)=u(i,j,9)+siu(j)*distk*distee(j)*dcos(anglee(j))
       v(i,j+1,32)=v(i,j,9)+siv(j)*distk*distee(j)*dsin(anglee(j))
       u(i,j+1,9)=u(i,j+1,32)
       v(i,j+1,9)=v(i,j+1,32)
       end do

       end if

       j=np(i,2)+1
       u(i,j,9)=u(i,j-1,9)+u(i,j-1,9)-u(i,j-2,9)
       v(i,j,9)=v(i,j-1,9)+v(i,j-1,9)-v(i,j-2,9)

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Final verification left and right side
c      Can be erased
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
       do i=1,nribss

       rib(i,40)=0.
       do j=1,np(i,2)-1
       rib(i,40)=rib(i,40)+sqrt((u(i-1,j,10)-u(i-1,j+1,10))**2.+
     + (v(i-1,j,10)-v(i-1,j+1,10))**2.)
       end do
      
       rib(i,42)=0.
       do j=1,np(i,2)-1
       rib(i,42)=rib(i,42)+sqrt((u(i,j,9)-u(i,j+1,9))**2.+
     + (v(i,j,9)-v(i,j+1,9))**2.)
       end do

c       write (*,*) "IGUALS ",i,rib(i,40),rib(i,31),rib(i,42)

       end do

       end if ! end reformat 

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Distorsion calculus
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Leading edge segment formated rib(i,97) extra

       do i=0,nribss-1

       if (i.lt.nribss) then
       j=np(i,2)
       rib(i,97)=dsqrt((u(i,j,9)-u(i,j,10))**2.+
     + ((v(i,j,9)-v(i,j,10))**2.))
c       write (*,*) "rib(i,97) ",i,rib(i,97)
       end if

       end do

       do i=0,nribss-1

c       write (*,*) "DISTORSION 1 LE (mm)",i,(rib(i,97)-rib(i,96))*10
      
       end do

c      Iterations
       do itera=1,5

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Distorsion correction
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Correction only for case ndif=1000

       if (ndif.eq.1000) then

       do i=1,nribss-1

       k=jirl(i)

       dist=dsqrt(((u(i,k,9)-u(i,np(i,2),9))**2.)+
     + ((v(i,k,9)-v(i,np(i,2),9))**2.))
      
       epsilon=(rib(i,97)-rib(i,96))

c      Angle of rotation
       if (abs(epsilon).lt.0.01) then
       omega=0.
       else
c      Experimental correction
       omega=1.0*dasin(epsilon/dist)*epsilon/(abs(epsilon))
       end if

c      Rotate left side (all correction)

       do j=jirl(i),np(i,2)-1

c      Atention whit angle definition using abs tangents
c      Make subroutines
c      Angles and distances, left side

       xdv=(v(i,j+1,9)-v(i,j,9))
       xdu=(u(i,j+1,9)-u(i,j,9))
       if (xdu.ne.0.) then
       anglee(j)=abs(datan(xdv/xdu))+omega
       else
       anglee(j)=2.*datan(1.0d0)+omega
       end if

       if (xdu.ge.0.and.xdv.ge.0) then ! case 1-I
       siu(j)=1.
       siv(j)=1.
       end if
       if (xdu.le.0.and.xdv.ge.0) then ! case 1-II
       siu(j)=-1.
       siv(j)=1.
       end if
       if (xdu.ge.0.and.xdv.le.0) then ! case 1-III
       siu(j)=1.
       siv(j)=-1.
       end if
       if (xdu.le.0.and.xdv.le.0) then ! case 1-IV
       siu(j)=-1.
       siv(j)=-1.
       end if

       distee(j)=dsqrt((v(i,j,9)-v(i,j+1,9))**2.+
     + (u(i,j,9)-u(i,j+1,9))**2.)

       end do  ! end j

c      Define

       do j=jirr(i),np(i,2)-1
       u(i,j+1,32)=u(i,j,9)+siu(j)*distee(j)*dcos(anglee(j))
       v(i,j+1,32)=v(i,j,9)+siv(j)*distee(j)*dsin(anglee(j))
       u(i,j+1,9)=u(i,j+1,32)
       v(i,j+1,9)=v(i,j+1,32)
       end do

       end do  ! end i

       end if  ! if ndif=1000


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Distorsion 2 calculus
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Leading edge segment formated rib(i,97) extra

       do i=0,nribss-1

       if (i.lt.nribss) then
       j=np(i,2)
       rib(i,97)=dsqrt((u(i,j,9)-u(i,j,10))**2.+
     + ((v(i,j,9)-v(i,j,10))**2.))
       end if

       end do ! i

       end do ! itera


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     8.2.5 Calcule external points
c     Seam borders left (11) and right (12)
c     Points LE 11,12,14,15,24,25
c     Points TE 11,12,14,15,24,25
c     IMPROVED by subroutine extpoints in 2018-12-26
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      icasec=1 tangent
c      icasec=2 orthogonal

       icasecle=1
       icasecte=1

c      Load vectors

c      Saltar cella 0 si gruix nul
       iini=0
       if (cencell.lt.0.01) then
       iini=1
       end if

       do i=iini,nribss

       npi=1
       npf=np(i,2)
       npo=npf-npi+1

c      Copy data in a free vector
       do j=1,npo

       uf(i,j,9)=u(i,npi+j-1,9)
       uf(i,j,10)=u(i,npi+j-1,10)
       uf(i,j,11)=u(i,npi+j-1,11)
       uf(i,j,12)=u(i,npi+j-1,12)
       vf(i,j,9)=v(i,npi+j-1,9)
       vf(i,j,10)=v(i,npi+j-1,10)
       vf(i,j,11)=v(i,npi+j-1,11)
       vf(i,j,12)=v(i,npi+j-1,12)

       ufe(i,j,9)=u(i,npi+j-1,9)
       ufe(i,j,10)=u(i,npi+j-1,10)
       ufe(i,j,11)=u(i,npi+j-1,11)
       ufe(i,j,12)=u(i,npi+j-1,12)
       vfe(i,j,9)=v(i,npi+j-1,9)
       vfe(i,j,10)=v(i,npi+j-1,10)
       vfe(i,j,11)=v(i,npi+j-1,11)
       vfe(i,j,12)=v(i,npi+j-1,12)

       end do

c      Call external points subroutine
c       call extpoints(i,uf,vf,npo,xupp,xupple,xuppte,1)

       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.2.6 Draw sobreamples extrados panels
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
       if (n1draw.eq.1) then ! draw

c      Box (1,3)

c      Avoid central panel if thickness is 0
       iini=0  ! panel 0 (central)
       if (cencell.lt.0.01) then
       iini=1
       end if

       do i=iini,nribss-1
       
       psep=1970.*xkf+seppix(i)*1.0d0
       psey=400.*xkf

cccccccccccccccccccccccccccccccccccccccccccccccccccccc    
c      Draw minirib extrados
cccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (i.ge.0.and.rib(i+1,56).gt.1.and.rib(i+1,56).ne.100
     + .and.atp.ne."ss") then

       xpo1=(u(i,1,9)+u(i,1,10))/2.
       ypo1=(v(i,1,9)+v(i,1,10))/2.
       j=jcve(i+1)
       xpo2=(u(i,j,9)+u(i,j,10))/2.
       ypo2=(v(i,j,9)+v(i,j,10))/2.

c      Avoid division by zero!!!
       if (xpo2-xpo1.ne.0.) then
       alpha=datan((ypo2-ypo1)/(xpo2-xpo1))
       end if
       if (abs(xpo2-xpo1).lt.0.00001) then
       alpha=pi/2.
       end if

       xpo3=xpo1+rib(i+1,60)*dcos(alpha)
       ypo3=ypo1+rib(i+1,60)*dsin(alpha)
       xdesx=xdes*dsin(alpha)
       xdesy=xdes*dcos(alpha)

c      Draw reference points for miniribs in extrados
       call line(psep+xpo1,psey-ypo1,psep+xpo3,psey-ypo3,5)
       call pointg(psep+xpo1-xdesx,psey-ypo1-xdesy,xcir,1)
       call pointg(psep+xpo3-xdesx,psey-ypo3-xdesy,xcir,1)

c      Laser cuting
       xadd=2520.*xkf
       call point(psep+xpo1+xadd,psey-ypo1,1)
       call point(psep+xpo3+xadd,psey-ypo3,1)

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccc

       npo=np(i,2)

c      Call draw panel extrados (complete)
c       call dpanelc(i,uf,vf,npo,psep,psey)

       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.2.7 Dibuixa sobreamples panells extrados Adre
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Box (1,5)

c      Avoid central panel if thickness is 0
       iini=0  ! panel 0 (central)
       if (cencell.lt.0.01) then
       iini=1
       end if

       do i=iini,nribss-1

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Dibuixa marques romanes AD
       call romano(i,psep+5.,psey+0.5,0.0d0,typm6(8)*0.1,7)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
       psep=1970.*xkf+2520.*xkf+seppix(i)*1.0d0
       psey=400.*xkf

c      Draw romano in last panel
       if (i.eq.nribss-1) then
       call romano(i+1,psep+5.,psey+0.5,0.0d0,typm6(8)*0.1,7)
       end if

c      Call draw panel extrados (complete)
c       call dpanelb(i,uf,vf,npo,psep,psey)

       end do

       end if ! n1draw

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.3 SOBREAMPLES VENTS
c      REVISAR, NO FINALITZAT
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      FUNCIONA (amb artificis!) es pot millorar

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.3.1 Sobreamples vents
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       xcos=xlow/10. ! vent sewing allowance

c      Save xlow
       xlowsaved=xlow

c      Avoid central panel if thickness is 0
       iini=0  ! panel 0 (central)
       if (cencell.lt.0.01) then
       iini=1
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Artifici 1:
c      Salva punts 9,10,11,12 a un vector de seguretat
c      per evitar interferencies en calcul de punts extrados a secció 11.4
c      i recupera després de calcul sobreamples de vents
c      que dónen problemes per algun motiu
c      Eliminar aquesta secció i la seva restitució més a baix
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=iini,nribss

       
       do j=1,np(i,1) ! salva tots els punts

       usalvat(i,j,9)=u(i,j,9)
       vsalvat(i,j,9)=v(i,j,9)
       usalvat(i,j,10)=u(i,j,10)
       vsalvat(i,j,10)=v(i,j,10)

       usalvat(i,j,11)=u(i,j,11)
       vsalvat(i,j,11)=v(i,j,11)
       usalvat(i,j,12)=u(i,j,12)
       vsalvat(i,j,12)=v(i,j,12)

       end do
       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.3.1 Sobreamples esquerra vents
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=iini,nribss

c      Set sewing border in vents according vent type
       if (k31d.eq.1.and.rib(i+1,165).ge.1) then
       xlow=xupp 
       else
       xlow=xlowsaved
       end if

c      Initialize sob points

c      CASE 0
       if (k31d.eq.0) then
       xsob(1)=0.
       ysob(1)=rib(i,22)*skin(1,2)/100.
       xsob(2)=rib(i,26)
       ysob(2)=rib(i,24)*skin(6,4)/100.
       end if

c      CASE 1.1
       if (k31d.eq.1) then
       xsob(1)=0.
       ysob(1)=rib(i,22)*skinnew(i,1,2)/100.
       xsob(2)=rib(i,26)
       ysob(2)=rib(i,24)*skinnew(i,skinpoints(i),4)/100.
       end if

c      Longituds u

       u(i,np(i,2),7)=0.
       
       do j=np(i,2),np(i,2)+np(i,3)-1
       u(i,j+1,7)=u(i,j,7)+sqrt((pl2x(i,j)-pl1x(i,j))**2+
     + (pl2y(i,j)-pl1y(i,j))**2)
       end do

c      Sobreample v      

       if (k31d.eq.0) then
       v(i,np(i,2),7)=rib(i,22)*skin(1,2)/100.
       end if
       if (k31d.eq.1) then
       v(i,np(i,2),7)=rib(i,22)*skinnew(i,1,2)/100.
       end if

       do j=np(i,2),np(i,2)+np(i,3)-1

       xm=(ysob(2)-ysob(1))/(xsob(2)-xsob(1))
       xn=ysob(2)-xm*xsob(2)

       v(i,j+1,7)=xm*(u(i,j+1,7))+xn

c      Solve some numeric problems!!!?
       if (v(i,j+1,7).le.0.001) then
       v(i,j+1,7)=0.0d0
       end if

       xm1=xm
       xn1=xn  
      
c      Calcula punts esquerra

       xdv=(pl2y(i,j)-pl1y(i,j))
       xdu=(pl2x(i,j)-pl1x(i,j))

       if (xdv.ne.0.) then
       alpl=abs(datan((pl2y(i,j)-pl1y(i,j))/(pl2x(i,j)-pl1x(i,j))))
       else
       alpl=2.*datan(1.0d0)
       end if

       if (xdu.ge.0.and.xdv.ge.0) then ! case 2-I
       siu(j)=-1.
       siv(j)=1.
       end if
       if (xdu.le.0.and.xdv.ge.0) then ! case 2-II
       siu(j)=-1.
       siv(j)=-1.
       end if
       if (xdu.ge.0.and.xdv.le.0) then ! case 2-III
       siu(j)=1.
       siv(j)=1.
       end if
       if (xdu.le.0.and.xdv.le.0) then ! case 2-IV
       siu(j)=1.
       siv(j)=-1.
       end if

       u(i,j,9)=pl1x(i,j)+siu(j)*v(i,j,7)*dsin(alpl)
       v(i,j,9)=pl1y(i,j)+siv(j)*v(i,j,7)*dcos(alpl)

       u(i,j,11)=u(i,j,9)+siu(j)*xlow*0.1*dsin(alpl)
       v(i,j,11)=v(i,j,9)+siv(j)*xlow*0.1*dcos(alpl)

c      PROBLEM with pl2y(i,j) value when j=np(i,2)
c      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
       end do

       npx=np(i,2)

       alpl=abs(datan((pl2y(i,npx)-pl1y(i,npx))/
     + (pl2x(i,npx)-pl1x(i,npx))))
       alpl2=alpl

c      pl2x(i,npx-1) no correcte
       u(i,npx,9)=pl1x(i,npx)-v(i,npx,7)*dsin(alpl)
       v(i,npx,9)=pl1y(i,npx)+v(i,npx,7)*dcos(alpl)

       u(i,npx,11)=u(i,npx,9)-xlow*0.1*dsin(alpl)
       v(i,npx,11)=v(i,npx,9)+xlow*0.1*dcos(alpl)

       npx=np(i,2)+np(i,3)-1

       alpl=abs(datan((pl2y(i,npx)-pl1y(i,npx))/
     + (pl2x(i,npx)-pl1x(i,npx))))
       alpl2=alpl

c      pl2x(i,npx-1) no correcte
       u(i,npx,9)=pl1x(i,npx)-v(i,npx,7)*dsin(alpl)
       v(i,npx,9)=pl1y(i,npx)+v(i,npx,7)*dcos(alpl)

       u(i,npx,11)=u(i,npx,9)-xlow*0.1*dsin(alpl)
       v(i,npx,11)=v(i,npx,9)+xlow*0.1*dcos(alpl)
     
       end do ! i

       i=2
       if (i.eq.2) then
       do j=np(i,2),np(i,2)+np(i,3)-1
c       write (*,*) "vent 11 ",i,j,u(i,j,11),v(i,j,11)
       end do
       end if



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.3.2 Sobreamples dreta vents
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=iini,nribss-1

c      Set sewing border in vents according vent type
       if (k31d.eq.1.and.rib(i+1,165).ge.1) then
       xlow=xupp 
       else
       xlow=xlowsaved
       end if

c      Initialize sob points

c      Case k31d=0
       if (k31d.eq.0) then
       xsob(1)=0.
       ysob(1)=rib(i,22)*skin(1,2)/100.
       xsob(2)=rib(i,26)
       ysob(2)=rib(i,24)*skin(6,4)/100.
       end if

c      Case k31d=1
       if (k31d.eq.1) then
       xsob(1)=0.
       ysob(1)=rib(i,22)*skinnew(i+1,1,2)/100.
       xsob(2)=rib(i+1,26)
       ysob(2)=rib(i,24)*skinnew(i+1,skinpoints(i+1),4)/100.
       end if

c      Longituds u

       u(i,np(i,2),8)=0.
       
       do j=np(i,2),np(i,2)+np(i,3)-2
       u(i,j+1,8)=u(i,j,8)+sqrt((pr2x(i,j)-pr1x(i,j))**2+
     + (pr2y(i,j)-pr1y(i,j))**2)
       end do

c      Sobreample v      

       if (k31d.eq.0) then
       v(i,np(i,2),8)=rib(i,22)*skin(1,2)/100.
       end if
       if (k31d.eq.1) then
       v(i,np(i,2),8)=rib(i,22)*skinnew(i+1,1,2)/100.
       end if


       do j=np(i,2),np(i,2)+np(i,3)-2

       xm=(ysob(2)-ysob(1))/(xsob(2)-xsob(1))
       xn=ysob(2)-xm*xsob(2)

       v(i,j+1,8)=xm*(u(i,j+1,8))+xn

       xm1=xm
       xn1=xn  
      
c      Calcula punts dreta

       alpr=abs(datan((pr2y(i,j)-pr1y(i,j))/(pr2x(i,j)-pr1x(i,j))))

       u(i,j+1,10)=pr2x(i,j)+v(i,j+1,8)*dsin(alpr)
       v(i,j+1,10)=pr2y(i,j)-v(i,j+1,8)*dcos(alpr)

       u(i,j+1,12)=u(i,j+1,10)+xlow*0.1*dsin(alpr)
       v(i,j+1,12)=v(i,j+1,10)-xlow*0.1*dcos(alpr)

       end do

       npx=np(i,2)

       alpr=abs(datan((pr2y(i,npx)-pr1y(i,npx))/
     + (pr2x(i,npx)-pr1x(i,npx))))
       alpr2=alpr

       u(i,npx,10)=pr1x(i,npx)+v(i,npx,8)*dsin(alpr)
       v(i,npx,10)=pr1y(i,npx)-v(i,npx,8)*dcos(alpr)
       
       u(i,npx,12)=u(i,npx,10)+xlow*0.1*dsin(alpr)
       v(i,npx,12)=v(i,npx,10)-xlow*0.1*dcos(alpr)

       end do ! i

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.3.3 Calcula cantonades vents
c      Versio millorada segons 8.2.5
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Saltar cella 0 si gruix nul
       iini=0
       if (cencell.lt.0.01) then
       iini=1
       end if

       do i=iini,nribss

       npi=np(i,2)
       npf=np(i,2)+np(i,3)-1
       npo=npf-npi+1

c      Copy data in a free vector
       do j=1,npo

       uf(i,j,9)=u(i,npi+j-1,9)
       uf(i,j,10)=u(i,npi+j-1,10)
       uf(i,j,11)=u(i,npi+j-1,11)
       uf(i,j,12)=u(i,npi+j-1,12)
       vf(i,j,9)=v(i,npi+j-1,9)
       vf(i,j,10)=v(i,npi+j-1,10)
       vf(i,j,11)=v(i,npi+j-1,11)
       vf(i,j,12)=v(i,npi+j-1,12)

       ufv(i,j,9)=u(i,npi+j-1,9)
       ufv(i,j,10)=u(i,npi+j-1,10)
       ufv(i,j,11)=u(i,npi+j-1,11)
       ufv(i,j,12)=u(i,npi+j-1,12)
       vfv(i,j,9)=v(i,npi+j-1,9)
       vfv(i,j,10)=v(i,npi+j-1,10)
       vfv(i,j,11)=v(i,npi+j-1,11)
       vfv(i,j,12)=v(i,npi+j-1,12)

       end do

c      Call external points subroutine
c       call extpoints(i,uf,vf,npo,xupp,xupp,xupp,1)

       end do

c      Retore xlow
       xlow=xlowsaved


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.3.4 Vents drawing > send to 8.5.2
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c       if (k31d.eq.1.and.k26d.ne.0) then ! Draw classic vents

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

c      Call draw panel extrados (complete)
c      call dpanelc(i,uf,vf,npo,psep,psey)

       end do

c       end if ! Vents case 1


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Artifici 1:
c      Recupera punts 9,10,11,12 del vector de seguretat
c      per evitar interferencies en calcul de punts extrados a secció 11.4
c      i recupera després de calcul sobreamples de vents
c      que dónen problemes per algun motiu
c      Eliminar aquesta secció i la seva restitució més amunt
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=iini,nribss
       do j=1,np(i,1) ! salva tots els punts

       u(i,j,9)=usalvat(i,j,9)
       v(i,j,9)=vsalvat(i,j,9)
       u(i,j,10)=usalvat(i,j,10)
       v(i,j,10)=vsalvat(i,j,10)

       u(i,j,11)=usalvat(i,j,11)
       v(i,j,11)=vsalvat(i,j,11)
       u(i,j,12)=usalvat(i,j,12)
       v(i,j,12)=vsalvat(i,j,12)

       end do
       end do

       i=2
       if (i.eq.2) then
       do j=1,np(i,2)
c      write (*,*) "salvat-11 ",i,u(i,j,11),v(i,j,11)
       end do
       end if



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.4 SOBREAMPLES INTRADOS
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Vent consumers are finished. Publish the retained exact intrados panels
c      over the temporary intake support at their shared boundary. No numbered
c      scratch point is involved, and the non-panel row nribss is untouched.
       do i=0,nribss-1
       call write_legacy_intrados_panel(neutral_intrados_panels(i),
     + wing_profile_topologies(i),wing_profile_topologies(i+1),
     + pl1x,pl1y,pl2x,pl2y,pr1x,pr1y,pr2x,pr2y,
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)') 'ERROR: typed intrados write-back ',i,': ',
     + trim(domain_model_message)
       stop 1
       end if
       end do
       
       xcos=xlow/10. ! intrados sewing allowance

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case classic skin tension k31d=0
c      Includes 8.4.1 and 8.4.2
c      NOTE: The first lower intrados point intentionally uses the preceding
c      intake segment (npx-1), while the higher side uses its own first
c      intrados segment.  Both are typed below through an explicit classic
c      normal policy rather than silently changing the general pure kernel.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (k31d.eq.0) then

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.4.1 Sobreamples esquerra intrados
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=0,nribss

c      Initialize sob points

       do k=1,6

       xsob(k)=((100.-(skin(7-k,3)))/100.)*rib(i,25)
       ysob(k)=rib(i,24)*skin(7-k,4)/100.

       end do

c      Longituds u

       u(i,np(i,2)+np(i,3)-1,7)=0.
       
       do j=np(i,2)+np(i,3)-1,np(i,1)-1

       u(i,j+1,7)=u(i,j,7)+sqrt((pl2x(i,j)-pl1x(i,j))**2+
     + (pl2y(i,j)-pl1y(i,j))**2)

       end do

c      Sobreample v      

       v(i,np(i,2)+np(i,3)-1,7)=rib(i,24)*skin(6,4)/100.

       do j=np(i,2)+np(i,3)-1,np(i,1)-1

c      LE zone

       if(u(i,j+1,7).le.xsob(2)) then

       xm=(ysob(2)-ysob(1))/(xsob(2)-xsob(1))
       xn=ysob(2)-xm*xsob(2)

       v(i,j+1,7)=xm*(u(i,j+1,7))+xn

       xm1=xm
       xn1=xn

       end if
       
       if(u(i,j+1,7).gt.xsob(2).and.u(i,j+1,7).le.xsob(3)) then

       xm=(ysob(3)-ysob(2))/(xsob(3)-xsob(2))
       xn=ysob(2)-xm*xsob(2)

       v(i,j+1,7)=xm*(u(i,j+1,7))+xn

       xm2=xm
       xn2=xn

       end if

c      Central panel

       if(u(i,j+1,7).gt.xsob(3).and.u(i,j+1,7).le.xsob(4)) then

       xm=(ysob(4)-ysob(3))/(xsob(4)-xsob(3))
       xn=ysob(3)-xm*xsob(3)

       v(i,j+1,7)=xm*(u(i,j+1,7))+xn

       xm2=xm
       xn2=xn

       end if

c      TE zone

       if(u(i,j+1,7).gt.xsob(4).and.u(i,j+1,7).le.xsob(5)) then

       xm=(ysob(5)-ysob(4))/(xsob(5)-xsob(4))
       xn=ysob(4)-xm*xsob(4)

       v(i,j+1,7)=xm*(u(i,j+1,7))+xn

       xm3=xm
       xn3=xn

       end if

       if(u(i,j+1,7).gt.xsob(5)) then

       xm=(ysob(6)-ysob(5))/(xsob(6)-xsob(5))
       xn=ysob(5)-xm*xsob(5)

       v(i,j+1,7)=xm*(u(i,j+1,7))+xn

       xm3=xm
       xn3=xn

       end if

c      Calcula punts esquerra

       alpl=abs(datan((pl2y(i,j)-pl1y(i,j))/(pl2x(i,j)-pl1x(i,j))))

       u(i,j+1,9)=pl2x(i,j)-v(i,j+1,7)*dsin(alpl)
       v(i,j+1,9)=pl2y(i,j)+v(i,j+1,7)*dcos(alpl)

       u(i,j+1,11)=u(i,j+1,9)-xlow*0.1*dsin(alpl)
       v(i,j+1,11)=v(i,j+1,9)+xlow*0.1*dcos(alpl)
       
       end do

c       u(i,np(i,1),14)=u(i,np(i,1),9)+xlowte*0.1*dcos(alpl)
c       v(i,np(i,1),14)=v(i,np(i,1),9)+xlowte*0.1*dsin(alpl)
       
       npx=np(i,2)+np(i,3)-1

       alpl=abs(datan((pl2y(i,npx-1)-pl1y(i,npx-1))/
     + (pl2x(i,npx-1)-pl1x(i,npx-1))))
       alpl2=alpl

c      pl2x(i,npx-1) no correcte
       u(i,npx,9)=pl1x(i,npx)-v(i,npx,7)*dsin(alpl)
       v(i,npx,9)=pl1y(i,npx)+v(i,npx,7)*dcos(alpl)

       u(i,npx,11)=u(i,npx,9)-xlow*0.1*dsin(alpl)
       v(i,npx,11)=v(i,npx,9)+xlow*0.1*dcos(alpl)

c      Dual-run the complete lower offset contour through the named classic
c      intrados law.  This side retains panel i's own contour length and width.
       if (i.lt.nribss) then
       do j=npx,np(i,1)
       call evaluate_classic_skin_tension_offset(
     + classic_intrados_tension_law,rib(i,25),rib(i,24),u(i,j,7),
     + typed_skin_tension_offset,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,A,I0,2A)')
     + 'ERROR: classic lower intrados tension failed at panel ',i,
     + ', point ',j,': ',trim(domain_model_message)
       stop 1
       end if
       if (.not.geometry_values_are_close(typed_skin_tension_offset,
     + v(i,j,7))) then
       write (*,'(A,I0,A,I0,2(1X,ES24.16))')
     + 'ERROR: classic lower intrados tension differs at panel ',i,
     + ', point ',j,typed_skin_tension_offset,v(i,j,7)
       stop 1
       end if
       v(i,j,7)=typed_skin_tension_offset
       end do

c      The classic lower initial normal comes from the preceding intake
c      segment even though its anchor is the first intrados point.  Pass that
c      exact vector to the explicit classic policy, compare every coordinate,
c      and publish only lower production slots 9/11 after agreement.
       call shape_classic_intrados_panel_side(neutral_intrados_panels(i),
     + panel_side_lower,v(i,npx:np(i,1),7),xlow,
     + pl2x(i,npx-1)-pl1x(i,npx-1),
     + pl2y(i,npx-1)-pl1y(i,npx-1),typed_intrados_lower_side,
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: classic lower intrados shaping failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       do j=npx,np(i,1)
       k=j-npx+1
       typed_coordinates_match=
     + geometry_values_are_close(typed_intrados_lower_side%sewing_u(k),
     + u(i,j,9)).and.
     + geometry_values_are_close(typed_intrados_lower_side%sewing_v(k),
     + v(i,j,9)).and.
     + geometry_values_are_close(typed_intrados_lower_side%cut_u(k),
     + u(i,j,11)).and.
     + geometry_values_are_close(typed_intrados_lower_side%cut_v(k),
     + v(i,j,11))
       if (.not.typed_coordinates_match) then
       write (*,'(A,I0,A,I0)')
     + 'ERROR: classic lower intrados shaping differs at panel ',i,
     + ', point ',j
       stop 1
       end if
       end do
       call write_legacy_shaped_panel_side(typed_intrados_lower_side,
     + wing_profile_topologies(i),u,v,domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: classic lower intrados publication failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       end if

c       u(i,npx,14)=u(i,npx,9)-xlowle*0.1*dcos(alpl)
c       v(i,npx,14)=v(i,npx,9)-xlowle*0.1*dsin(alpl)

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.4.2 Sobreamples dreta intrados   
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Sobreamples dreta

       do i=0,nribss

c      Initialize sob points

       do k=1,6

       xsob(k)=((100.-(skin(7-k,3)))/100.)*rib(i+1,25)
       ysob(k)=rib(i,24)*skin(7-k,4)/100.

       end do

c      Longitud u

       u(i,np(i,2)+np(i,3)-1,8)=0.

       do j=np(i,2)+np(i,3)-1,np(i,1)-1

       u(i,j+1,8)=u(i,j,8)+sqrt((pr2x(i,j)-pr1x(i,j))**2+
     + (pr2y(i,j)-pr1y(i,j))**2)

       end do

c      Sobreample v      

       v(i,np(i,2)+np(i,3)-1,8)=rib(i,24)*skin(6,4)/100.

       do j=np(i,2)+np(i,3)-2,np(i,1)-1

c      LE zone

       if(u(i,j+1,8).le.xsob(2)) then

       xm=(ysob(2)-ysob(1))/(xsob(2)-xsob(1))
       xn=ysob(2)-xm*xsob(2)

       v(i,j+1,8)=xm*(u(i,j+1,8))+xn

       xm1=xm
       xn1=xn

       end if

       if(u(i,j+1,8).gt.xsob(2).and.u(i,j+1,8).le.xsob(3)) then

       xm=(ysob(3)-ysob(2))/(xsob(3)-xsob(2))
       xn=ysob(2)-xm*xsob(2)

       v(i,j+1,8)=xm*(u(i,j+1,8))+xn

       xm2=xm
       xn2=xn

       end if

c      Central panel

       if(u(i,j+1,8).gt.xsob(3).and.u(i,j+1,8).le.xsob(4)) then

       xm=(ysob(4)-ysob(3))/(xsob(4)-xsob(3))
       xn=ysob(3)-xm*xsob(3)

       v(i,j+1,8)=xm*(u(i,j+1,8))+xn

       xm2=xm
       xn2=xn

       end if

c      TE zone

       if(u(i,j+1,8).gt.xsob(4).and.u(i,j+1,8).le.xsob(5)) then

       xm=(ysob(5)-ysob(4))/(xsob(5)-xsob(4))
       xn=ysob(4)-xm*xsob(4)

       v(i,j+1,8)=xm*(u(i,j+1,8))+xn

       xm3=xm
       xn3=xn

       end if

       if(u(i,j+1,8).gt.xsob(5)) then

       xm=(ysob(6)-ysob(5))/(xsob(6)-xsob(5))
       xn=ysob(5)-xm*xsob(5)

       v(i,j+1,8)=xm*(u(i,j+1,8))+xn

       xm3=xm
       xn3=xn

       end if

c      Calcula punts dreta

       alpr=abs(datan((pr2y(i,j)-pr1y(i,j))/(pr2x(i,j)-pr1x(i,j))))

c      OPTION add corrections in sign (see 8.2.2)
c      But probably in normal intrados panels is not necessary

       u(i,j+1,10)=pr2x(i,j)+v(i,j+1,8)*dsin(alpr)
       v(i,j+1,10)=pr2y(i,j)-v(i,j+1,8)*dcos(alpr)

       u(i,j+1,12)=u(i,j+1,10)+xlow*0.1*dsin(alpr)
       v(i,j+1,12)=v(i,j+1,10)-xlow*0.1*dcos(alpr)

       end do  ! j=np(i,2)+np(i,3)-1,np(i,1)-1

c      Initial point not definined in previous bucle
       
       npx=np(i,2)+np(i,3)-1

       alpr=abs(datan((pr2y(i,npx)-pr1y(i,npx))/
     + (pr2x(i,npx)-pr1x(i,npx))))
       alpr2=alpr

       u(i,npx,10)=pr1x(i,npx)+v(i,npx,8)*dsin(alpr)
       v(i,npx,10)=pr1y(i,npx)-v(i,npx,8)*dcos(alpr)
       
       u(i,npx,12)=u(i,npx,10)+xlow*0.1*dsin(alpr)
       v(i,npx,12)=v(i,npx,10)-xlow*0.1*dcos(alpr)

c      The classic higher boundary changes to rib i+1's contour length while
c      retaining panel i's measured width.  Dual-run its complete offset range
c      through the named law before any typed production geometry consumes it.
       if (i.lt.nribss) then
       do j=npx,np(i,1)
       call evaluate_classic_skin_tension_offset(
     + classic_intrados_tension_law,rib(i+1,25),rib(i,24),u(i,j,8),
     + typed_skin_tension_offset,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,A,I0,2A)')
     + 'ERROR: classic higher intrados tension failed at panel ',i,
     + ', point ',j,': ',trim(domain_model_message)
       stop 1
       end if
       if (.not.geometry_values_are_close(typed_skin_tension_offset,
     + v(i,j,8))) then
       write (*,'(A,I0,A,I0,2(1X,ES24.16))')
     + 'ERROR: classic higher intrados tension differs at panel ',i,
     + ', point ',j,typed_skin_tension_offset,v(i,j,8)
       stop 1
       end if
       v(i,j,8)=typed_skin_tension_offset
       end do

c      Pass its first intrados segment to the explicit classic normal policy.
c      Compare the complete legacy sewing/cut oracle, then let the checked
c      writer publish only higher slots 10/12.
       call shape_classic_intrados_panel_side(neutral_intrados_panels(i),
     + panel_side_higher,v(i,npx:np(i,1),8),xlow,
     + neutral_intrados_panels(i)%higher_segment_end_u(1)-
     + neutral_intrados_panels(i)%higher_segment_start_u(1),
     + neutral_intrados_panels(i)%higher_segment_end_v(1)-
     + neutral_intrados_panels(i)%higher_segment_start_v(1),
     + typed_intrados_higher_side,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: classic higher intrados shaping failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       do j=npx,np(i,1)
       k=j-npx+1
       typed_coordinates_match=
     + geometry_values_are_close(typed_intrados_higher_side%sewing_u(k),
     + u(i,j,10)).and.
     + geometry_values_are_close(typed_intrados_higher_side%sewing_v(k),
     + v(i,j,10)).and.
     + geometry_values_are_close(typed_intrados_higher_side%cut_u(k),
     + u(i,j,12)).and.
     + geometry_values_are_close(typed_intrados_higher_side%cut_v(k),
     + v(i,j,12))
       if (.not.typed_coordinates_match) then
       write (*,'(A,I0,A,I0)')
     + 'ERROR: classic higher intrados shaping differs at panel ',i,
     + ', point ',j
       stop 1
       end if
       end do
       call write_legacy_shaped_panel_side(typed_intrados_higher_side,
     + wing_profile_topologies(i),u,v,domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: classic higher intrados publication failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       end if

c      Leading edge segment unformated rib(i,98) intra

       if (i.lt.nribss) then
       j=np(i,2)+np(i,3)-1
       rib(i,98)=dsqrt((u(i,j,9)-u(i,j,10))**2.+
     + ((v(i,j,9)-v(i,j,10))**2.))
       end if

       end do  ! i

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       end if  ! Case classic skin tension k31d=0
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case new skin tension k31d=1 Linear interpolation INTRADOS
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (k31d.eq.1) then

c       write (*,*) "NOTE: NEW skin tension in intrados"

c      Case 1: Linear interpolation and cell width is rib(i,24)
c       if (ntypei31(i).eq.1) then

c      Initialize sob points
       do i=0,nribss
       do k=1,skinpoints(i)
       xsobnew(i,k)=((100.-(skinnew(i,skinpoints(i)+1-k,3)))/100.)
     + *rib(i,25)
       ysobnew(i,k)=rib(i,24)*skinnew(i,skinpoints(i)+1-k,4)/100.
c       write (*,*) i,k,xsobnew(i,k),ysobnew(i,k)
       end do ! k
       end do ! i (initialize all sob points)

c      Stage 7 creates only real panels.  Seed the legacy terminal lower row
c      from the exact higher edge of the final real intrados panel so the old
c      distance and puntslat calculations have a physical, defined oracle.
       call write_legacy_neutral_boundary(terminal_intrados_edge,
     + pl1x,pl1y,pl2x,pl2y,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: terminal intrados neutral write-back: ',
     + trim(domain_model_message)
       stop 1
       end if

       do i=0,nribss

c      Assign lengths and sob distances
       u(i,np(i,2)+np(i,3)-1,7)=0.
       u(i,np(i,2)+np(i,3)-1,8)=0.

       typed_geometry_metric=0.d0
       do j=np(i,2)+np(i,3)-1,np(i,1)-1
       do k=1,skinpoints(i)-1

c      Left side
       if (u(i,j,7).ge.xsobnew(i,k).and.u(i,j,7).le.xsobnew(i,k+1)
     + *1.001) ! NOTE: Comparar reals és perillós, afegim multiplicador
     + then
       xm=(ysobnew(i,k+1)-ysobnew(i,k))/(xsobnew(i,k+1)-xsobnew(i,k))
       xn=ysobnew(i,k+1)-xm*xsobnew(i,k+1)
       v(i,j,7)=xm*(u(i,j,7))+xn

       end if

       u(i,j+1,7)=u(i,j,7)+sqrt((pl2x(i,j)-pl1x(i,j))**2+
     + (pl2y(i,j)-pl1y(i,j))**2)

c      Right side
       if (i.lt.nribss) then

       if (u(i,j,8).ge.xsobnew(i+1,k).and.u(i,j,8).le.
     + xsobnew(i+1,k+1)
     + *1.001) ! NOTE: Comparar reals és perillós, afegim multiplicador
     + then
       xm=(ysobnew(i+1,k+1)-ysobnew(i+1,k))/
     +    (xsobnew(i+1,k+1)-xsobnew(i+1,k))
       xn=ysobnew(i+1,k+1)-xm*xsobnew(i+1,k+1)
       v(i,j,8)=xm*(u(i,j,8))+xn

       end if

       u(i,j+1,8)=u(i,j,8)+sqrt((pr2x(i,j)-pr1x(i,j))**2+
     + (pr2y(i,j)-pr1y(i,j))**2)

       end if

       end do ! k

c      Regular lower intrados boundaries are the first authoritative typed
c      skin-tension slice.  Keep the legacy calculation as an exact oracle,
c      then publish the independently evaluated named law only after it
c      agrees.  The terminal row remains a physical boundary, not a panel.
       if (i.lt.nribss) then
       call evaluate_skin_tension_offset(
     + typed_intrados_tension_laws(i),rib(i,25),rib(i,24),u(i,j,7),
     + typed_skin_tension_offset,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,A,I0,2A)')
     + 'ERROR: typed intrados tension failed at panel ',i,', point ',j,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       if (.not.geometry_values_are_close(typed_skin_tension_offset,
     + v(i,j,7))) then
       write (*,'(A,I0,A,I0,2(1X,ES24.16))')
     + 'ERROR: typed/legacy intrados tension differs at panel ',i,
     + ', point ',j,typed_skin_tension_offset,v(i,j,7)
       stop 1
       end if
       v(i,j,7)=typed_skin_tension_offset
       end if

c      The regular higher side uses boundary i+1's intrados law.  Accumulate
c      its distance independently from the exact retained higher neutral edge,
c      compare both the legacy distance and offset, then publish the agreeing
c      typed offset.  No higher side exists for terminal row nribss.
       if (i.lt.nribss) then
       if (.not.geometry_values_are_close(typed_geometry_metric,
     + u(i,j,8))) then
       write (*,'(A,I0,A,I0,2(1X,ES24.16))')
     + 'ERROR: higher intrados distance differs at panel ',i,
     + ', point ',j,typed_geometry_metric,u(i,j,8)
       stop 1
       end if
       call evaluate_skin_tension_offset(
     + typed_intrados_tension_laws(i+1),rib(i+1,25),rib(i+1,24),
     + typed_geometry_metric,typed_skin_tension_offset,
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,A,I0,2A)')
     + 'ERROR: typed higher intrados tension failed at panel ',i,
     + ', point ',j,': ',trim(domain_model_message)
       stop 1
       end if
       if (.not.geometry_values_are_close(typed_skin_tension_offset,
     + v(i,j,8))) then
       write (*,'(A,I0,A,I0,2(1X,ES24.16))')
     + 'ERROR: typed/legacy higher intrados tension differs at panel ',i,
     + ', point ',j,typed_skin_tension_offset,v(i,j,8)
       stop 1
       end if
       v(i,j,8)=typed_skin_tension_offset
       k=j-(np(i,2)+np(i,3)-1)+1
       typed_geometry_metric=typed_geometry_metric+sqrt(
     + (neutral_intrados_panels(i)%higher_segment_end_u(k)-
     + neutral_intrados_panels(i)%higher_segment_start_u(k))**2+
     + (neutral_intrados_panels(i)%higher_segment_end_v(k)-
     + neutral_intrados_panels(i)%higher_segment_start_v(k))**2)
       end if

       end do ! j

c      The final contour point has a tension value but no following segment.
c      Evaluate its offset separately so segment accumulation stops at L-1
c      without leaving the endpoint consumed by puntslat undefined.
       j=np(i,1)
       do k=1,skinpoints(i)-1
       if (u(i,j,7).ge.xsobnew(i,k).and.u(i,j,7).le.xsobnew(i,k+1)
     + *1.001) then
       xm=(ysobnew(i,k+1)-ysobnew(i,k))/(xsobnew(i,k+1)-xsobnew(i,k))
       xn=ysobnew(i,k+1)-xm*xsobnew(i,k+1)
       v(i,j,7)=xm*u(i,j,7)+xn
       end if
       if (i.lt.nribss) then
       if (u(i,j,8).ge.xsobnew(i+1,k).and.u(i,j,8).le.
     + xsobnew(i+1,k+1)*1.001) then
       xm=(ysobnew(i+1,k+1)-ysobnew(i+1,k))/
     +    (xsobnew(i+1,k+1)-xsobnew(i+1,k))
       xn=ysobnew(i+1,k+1)-xm*xsobnew(i+1,k+1)
       v(i,j,8)=xm*u(i,j,8)+xn
       end if
       end if
       end do

c      The endpoint has no following neutral segment, but it still receives
c      a skin-tension offset and is consumed as the final shaped point.
       if (i.lt.nribss) then
       call evaluate_skin_tension_offset(
     + typed_intrados_tension_laws(i),rib(i,25),rib(i,24),u(i,j,7),
     + typed_skin_tension_offset,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: typed intrados endpoint tension failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       if (.not.geometry_values_are_close(typed_skin_tension_offset,
     + v(i,j,7))) then
       write (*,'(A,I0,2(1X,ES24.16))')
     + 'ERROR: typed/legacy intrados endpoint tension differs at panel ',i,
     + typed_skin_tension_offset,v(i,j,7)
       stop 1
       end if
       v(i,j,7)=typed_skin_tension_offset
       end if

c      The final higher contour point has an offset but no outgoing segment.
c      The exact accumulator therefore already contains its developed distance.
       if (i.lt.nribss) then
       if (.not.geometry_values_are_close(typed_geometry_metric,
     + u(i,j,8))) then
       write (*,'(A,I0,2(1X,ES24.16))')
     + 'ERROR: higher intrados endpoint distance differs at panel ',i,
     + typed_geometry_metric,u(i,j,8)
       stop 1
       end if
       call evaluate_skin_tension_offset(
     + typed_intrados_tension_laws(i+1),rib(i+1,25),rib(i+1,24),
     + typed_geometry_metric,typed_skin_tension_offset,
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: typed higher intrados endpoint failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       if (.not.geometry_values_are_close(typed_skin_tension_offset,
     + v(i,j,8))) then
       write (*,'(A,I0,2(1X,ES24.16))')
     + 'ERROR: typed/legacy higher intrados endpoint differs at panel ',i,
     + typed_skin_tension_offset,v(i,j,8)
       stop 1
       end if
       v(i,j,8)=typed_skin_tension_offset
       end if

c      The physical terminal rib is not a panel.  Independently traverse its
c      exact retained edge, validate the legacy cumulative distances and
c      offsets, and retain one offset per terminal contour point for shaping.
       if (i.eq.nribss) then
       if (allocated(typed_terminal_intrados_offsets))
     + deallocate(typed_terminal_intrados_offsets)
       allocate(typed_terminal_intrados_offsets(
     + terminal_intrados_edge%contour_last_index-
     + terminal_intrados_edge%contour_first_index+1))
       typed_geometry_metric=0.d0
       do j=terminal_intrados_edge%contour_first_index,
     + terminal_intrados_edge%contour_last_index
       k=j-terminal_intrados_edge%contour_first_index+1
       if (.not.geometry_values_are_close(typed_geometry_metric,
     + u(i,j,7))) then
       write (*,'(A,I0,2(1X,ES24.16))')
     + 'ERROR: typed/legacy terminal intrados distance differs at point ',j,
     + typed_geometry_metric,u(i,j,7)
       stop 1
       end if
       call evaluate_skin_tension_offset(
     + typed_intrados_tension_laws(i),rib(i,25),rib(i,24),
     + typed_geometry_metric,typed_skin_tension_offset,
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: terminal intrados tension failed at point ',j,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       if (.not.geometry_values_are_close(typed_skin_tension_offset,
     + v(i,j,7))) then
       write (*,'(A,I0,2(1X,ES24.16))')
     + 'ERROR: typed/legacy terminal intrados tension differs at point ',j,
     + typed_skin_tension_offset,v(i,j,7)
       stop 1
       end if
       typed_terminal_intrados_offsets(k)=typed_skin_tension_offset
       v(i,j,7)=typed_skin_tension_offset
       if (j.lt.terminal_intrados_edge%contour_last_index) then
       typed_geometry_metric=typed_geometry_metric+sqrt(
     + (terminal_intrados_edge%segment_end_u(k)-
     + terminal_intrados_edge%segment_start_u(k))**2+
     + (terminal_intrados_edge%segment_end_v(k)-
     + terminal_intrados_edge%segment_start_v(k))**2)
       end if
       end do
       if (.not.geometry_values_are_close(typed_geometry_metric,
     + rib(i,25))) then
       write (*,'(A,2(1X,ES24.16))')
     + 'ERROR: terminal intrados typed length differs:',
     + typed_geometry_metric,rib(i,25)
       stop 1
       end if
       end if

c      Compute points left side
       call puntslat(i,pl1x,pl1y,pl2x,pl2y,np(i,1)-np(i,4)+1,
     + np(i,1)-1,u,v,xlow,-1)

c      Shape the same real-panel boundary from the exact typed neutral edge.
c      All sewing and cut coordinates must agree with puntslat before the
c      typed result is allowed to replace numbered slots 9 and 11.
       if (i.lt.nribss) then
       call shape_neutral_panel_side(neutral_intrados_panels(i),
     + panel_side_lower,
     + v(i,np(i,1)-np(i,4)+1:np(i,1),7),xlow,
     + typed_intrados_lower_side,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: typed lower intrados shaping failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       do j=np(i,1)-np(i,4)+1,np(i,1)
       k=j-(np(i,1)-np(i,4)+1)+1
       typed_coordinates_match=
     + geometry_values_are_close(typed_intrados_lower_side%sewing_u(k),
     + u(i,j,9)).and.
     + geometry_values_are_close(typed_intrados_lower_side%sewing_v(k),
     + v(i,j,9)).and.
     + geometry_values_are_close(typed_intrados_lower_side%cut_u(k),
     + u(i,j,11)).and.
     + geometry_values_are_close(typed_intrados_lower_side%cut_v(k),
     + v(i,j,11))
       if (.not.typed_coordinates_match) then
       write (*,'(A,I0,A,I0)')
     + 'ERROR: typed/legacy lower intrados shaping differs at panel ',i,
     + ', point ',j
       stop 1
       end if
       u(i,j,9)=typed_intrados_lower_side%sewing_u(k)
       v(i,j,9)=typed_intrados_lower_side%sewing_v(k)
       u(i,j,11)=typed_intrados_lower_side%cut_u(k)
       v(i,j,11)=typed_intrados_lower_side%cut_v(k)
       end do
       end if

c      Publish the separately modeled physical terminal production boundary.
c      It uses the lower/outward normal but the final real panel's higher
c      neutral source; no panel or higher-side slots exist beyond the wingtip.
       if (i.eq.nribss) then
       call shape_neutral_boundary_edge(terminal_intrados_edge,
     + typed_terminal_intrados_offsets,xlow,
     + typed_terminal_intrados_production_edge,domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: terminal intrados shaping failed: ',
     + trim(domain_model_message)
       stop 1
       end if
       do j=terminal_intrados_edge%contour_first_index,
     + terminal_intrados_edge%contour_last_index
       k=j-terminal_intrados_edge%contour_first_index+1
       typed_coordinates_match=
     + geometry_values_are_close(
     + typed_terminal_intrados_production_edge%sewing_u(k),u(i,j,9)).and.
     + geometry_values_are_close(
     + typed_terminal_intrados_production_edge%sewing_v(k),v(i,j,9)).and.
     + geometry_values_are_close(
     + typed_terminal_intrados_production_edge%cut_u(k),u(i,j,11)).and.
     + geometry_values_are_close(
     + typed_terminal_intrados_production_edge%cut_v(k),v(i,j,11))
       if (.not.typed_coordinates_match) then
       write (*,'(A,I0)')
     + 'ERROR: typed/legacy terminal intrados shaping differs at point ',j
       stop 1
       end if
       end do
       call write_legacy_production_boundary(
     + typed_terminal_intrados_production_edge,
     + wing_profile_topologies(i),u,v,domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: terminal intrados production write-back: ',
     + trim(domain_model_message)
       stop 1
       end if
       end if

c      Compute points right side
       if (i.lt.nribss) then
       call puntslat(i,pr1x,pr1y,pr2x,pr2y,np(i,1)-np(i,4)+1,
     + np(i,1)-1,u,v,xlow,1)

c      Shape and publish the real higher intrados side from its retained exact
c      neutral panel.  Compare the complete sewing/cut contour before the
c      checked adapter writes only slots 10/12 over the intrados range.
       call shape_neutral_panel_side(neutral_intrados_panels(i),
     + panel_side_higher,
     + v(i,np(i,1)-np(i,4)+1:np(i,1),8),xlow,
     + typed_intrados_higher_side,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: typed higher intrados shaping failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       do j=np(i,1)-np(i,4)+1,np(i,1)
       k=j-(np(i,1)-np(i,4)+1)+1
       typed_coordinates_match=
     + geometry_values_are_close(typed_intrados_higher_side%sewing_u(k),
     + u(i,j,10)).and.
     + geometry_values_are_close(typed_intrados_higher_side%sewing_v(k),
     + v(i,j,10)).and.
     + geometry_values_are_close(typed_intrados_higher_side%cut_u(k),
     + u(i,j,12)).and.
     + geometry_values_are_close(typed_intrados_higher_side%cut_v(k),
     + v(i,j,12))
       if (.not.typed_coordinates_match) then
       write (*,'(A,I0,A,I0)')
     + 'ERROR: typed/legacy higher intrados shaping differs at panel ',i,
     + ', point ',j
       stop 1
       end if
       end do
       call write_legacy_shaped_panel_side(typed_intrados_higher_side,
     + wing_profile_topologies(i),u,v,domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: higher intrados publication failed at panel ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       end if

c      Leading edge segment unformated rib(i,98) intra
       if (i.lt.nribss) then
       j=np(i,2)+np(i,3)-1
       rib(i,98)=dsqrt((u(i,j,9)-u(i,j,10))**2.+
     + ((v(i,j,9)-v(i,j,10))**2.))
       end if

       end do  ! i

c       end if ! end case 1.1

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       end if  ! k31d=1 Linear interpolation INTRADOS
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.4.3 Not necessary?
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.4.4 REFORMAT PANELS FOR PERFECT MATCHING
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Detect if ndif parameter is set for reformat
       if (ndif.eq.1000) then

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Compute rib and panels lengths (also done in chapter 11. (!))
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=1,nribss

       rib(i,33)=0. ! intra panel left
       rib(i,34)=0. ! intra rib
       rib(i,35)=0. ! intra panel right

       do j=np(i,2)+np(i,3)-1,np(i,1)-1

       rib(i,33)=rib(i,33)+sqrt((u(i-1,j,10)-u(i-1,j+1,10))**2.+
     + ((v(i-1,j,10)-v(i-1,j+1,10))**2.))

       rib(i,34)=rib(i,34)+sqrt((u(i,j,3)-u(i,j+1,3))**2.+((v(i,j,3)
     + -v(i,j+1,3))**2.))

       rib(i,35)=rib(i,35)+sqrt((u(i,j,9)-u(i,j+1,9))**2.+((v(i,j,9)
     + -v(i,j+1,9))**2.))

       end do

c      Amplification cofficients
       rib(i,38)=rib(i,33)/rib(i,34)
       rib(i,39)=rib(i,35)/rib(i,34)

       rib(0,38)=rib(1,38)
       rib(0,39)=rib(1,38)

c      Differences with control coefficient xndif

       rib(i,93)=xndif*(rib(i,34)-rib(i,33))
       rib(i,95)=xndif*(rib(i,34)-rib(i,35))
       if (i.eq.nribss.and.k31d.eq.1) then
       typed_terminal_intrados_reformat_control%source_contour_length=
     + rib(i,35)
       typed_terminal_intrados_reformat_control%target_contour_length=
     + rib(i,34)
       typed_terminal_intrados_reformat_control%correction_fraction=xndif
       end if

       end do

c      Define rib 0
     
       rib(0,33)=rib(1,35)
       rib(0,34)=rib(1,34)
       rib(0,35)=rib(1,33)

       rib(0,93)=xndif*(rib(0,34)-rib(0,33))
       rib(0,95)=xndif*(rib(0,34)-rib(0,35))

c      Define special rib (probably is not necessary)

       rib(nribss,35)=rib(nribss,34)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Identify point jirl-r where initialize reformating skin(3,3) in %
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=1,nribss

c      Left side

       xirl(i)=rib(i,33)*skin(3,3)/100.

       jirl(i)=np(i,2)+np(i,3)-1
       rib(i,43)=0.
       do j=np(i,2)+np(i,3)-1,np(i,1)-1
       rib(i,43)=rib(i,43)+sqrt((u(i-1,j,10)-u(i-1,j+1,10))**2.+
     + (v(i-1,j,10)-v(i-1,j+1,10))**2.)
       if (rib(i,43).lt.xirl(i)) then
       x43=rib(i,43)
       jirl(i)=j
       end if
       end do

c      Right side

       xirr(i)=rib(i,35)*skin(3,3)/100.

       jirr(i)=np(i,2)+np(i,3)-1
       rib(i,45)=0.
       do j=np(i,2)+np(i,3)-1,np(i,1)-1
       rib(i,45)=rib(i,45)+sqrt((u(i,j,9)-u(i,j+1,9))**2.+
     + (v(i,j,9)-v(i,j+1,9))**2.)
       if (rib(i,45).lt.xirr(i)) then
       x45=rib(i,45)
       jirr(i)=j
       end if
       end do

       end do

c      Assign in rib 0
       jirl(0)=jirl(1)
       jirr(0)=jirr(1)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat the last segments
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat left side - intrados
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=1,nribss

c      Calcule length of the reformating polyline

       dist1=0.0d0
       do j=jirl(i),np(i,1)-1
       dist1=dist1+dsqrt((v(i-1,j,10)-v(i-1,j+1,10))**2.+
     + (u(i-1,j,10)-u(i-1,j+1,10))**2.)
       end do
       
       dist2=0.0d0
       do j=np(i,2)+np(i,3)-1,jirl(i)-1
       dist2=dist2+dsqrt((v(i-1,j,10)-v(i-1,j+1,10))**2.+
     + (u(i-1,j,10)-u(i-1,j+1,10))**2.)
       end do

c       write (*,*) "rib(i,33)=",i,dist1+dist2,rib(i,33),
c     + dist1+dist2-rib(i,33)


       dist3=dist2+rib(i,93)

       distk=dist3/dist2  ! amplification coefficient

c      Reformat
      
       j=jirl(i-1)+1
       u(i-1,j,33)=u(i-1,j,10)
       v(i-1,j,33)=v(i-1,j,10)

c      Eliminar dis22!!!!!!!!!!!!!!!!!!!!!
       dis22=0.

       do j=jirl(i),np(i,2)+np(i,3),-1

c      Atention whit angle definition using abs tangents
c      Make subroutines
c      Angles and distances, left side

       xdv=(v(i-1,j-1,10)-v(i-1,j,10))
       xdu=(u(i-1,j-1,10)-u(i-1,j,10))
       if (xdu.ne.0.) then
       anglee(j)=abs(datan(xdv/xdu))
       else
       anglee(j)=2.*datan(1.0d0)
       end if

       if (xdu.ge.0.and.xdv.ge.0) then ! case 1-I
       siu(j)=1.
       siv(j)=1.
       end if
       if (xdu.le.0.and.xdv.ge.0) then ! case 1-II
       siu(j)=-1.
       siv(j)=1.
       end if
       if (xdu.ge.0.and.xdv.le.0) then ! case 1-III
       siu(j)=1.
       siv(j)=-1.
       end if
       if (xdu.le.0.and.xdv.le.0) then ! case 1-IV
       siu(j)=-1.
       siv(j)=-1.
       end if

       distee(j)=dsqrt((v(i-1,j,10)-v(i-1,j-1,10))**2.+
     + (u(i-1,j,10)-u(i-1,j-1,10))**2.)

       end do

c      Define

       do j=jirl(i),np(i,2)+np(i,3),-1
       u(i-1,j-1,30)=u(i-1,j,10)+siu(j)*distk*distee(j)*dcos(anglee(j))
       v(i-1,j-1,30)=v(i-1,j,10)+siv(j)*distk*distee(j)*dsin(anglee(j))
       u(i-1,j-1,10)=u(i-1,j-1,30)
       v(i-1,j-1,10)=v(i-1,j-1,30)
       end do

c      AFEGIR calcul punts extrems!!!!!!!!!!!

c       j=np(i,2)+np(i,3)-1
c       u(i-1,j,10)=u(i-1,j-1,10)+u(i-1,j-1,10)-u(i-1,j-2,10)
c       v(i-1,j,10)=v(i-1,j-1,10)+v(i-1,j-1,10)-v(i-1,j-2,10)

c      VERIFICATION

       dist=0.0d0
       do j=np(i,2)+np(i,3)-1,np(i,1)-1
       dist=dist+sqrt((v(i-1,j,10)-v(i-1,j+1,10))**2.+
     + (u(i-1,j,10)-u(i-1,j+1,10))**2.)
       end do
c       write (*,*) "VERFICACIO L ",i,dist,rib(i,33),rib(i,34),rib(i,35)

       end do ! i

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat right side - intrados
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=0,nribss

c      Calcule length of the reformating polyline

c      Snapshot the one intake-side point which the historical terminal
c      reformatter changes outside the exact intrados contour.
       if (i.eq.nribss.and.k31d.eq.1) then
       call copy_legacy_preceding_join_support(
     + typed_terminal_intrados_production_edge,u,v,
     + typed_terminal_intrados_join_support,domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: terminal intrados join-support copy: ',
     + trim(domain_model_message)
       stop 1
       end if
       end if

       dist1=0.0d0
       do j=jirr(i),np(i,1)-1
       dist1=dist1+sqrt((v(i,j,9)-v(i,j+1,9))**2.+
     + (u(i,j,9)-u(i,j+1,9))**2.)
       end do
       
       dist2=0.0d0
       do j=np(i,2)+np(i,3)-1,jirr(i)-1
       dist2=dist2+sqrt((v(i,j,9)-v(i,j+1,9))**2.+
     + (u(i,j,9)-u(i,j+1,9))**2.)
       end do

c       write (*,*) "rib(i,35)=",i,dist1+dist2,rib(i,35),
c     + dist1+dist2-rib(i,35)


       dist3=dist2+rib(i,95)

       distk=dist3/dist2  ! amplification coefficient

c      Reformat
      
       j=min(np(i,1),int(jirr(i))+1)
       u(i,j,35)=u(i,j,9)
       v(i,j,35)=v(i,j,9)

       do j=jirl(i),np(i,2)+np(i,3),-1

c      Atention whit angle definition using abs tangents
c      Make subroutines
c      Angles and distances, left side

       xdv=(v(i,j-1,9)-v(i,j,9))
       xdu=(u(i,j-1,9)-u(i,j,9))
       if (xdu.ne.0.) then
       anglee(j)=abs(datan(xdv/xdu))
       else
       anglee(j)=2.*datan(1.0d0)
       end if

       if (xdu.ge.0.and.xdv.ge.0) then ! case 1-I
       siu(j)=1.
       siv(j)=1.
       end if
       if (xdu.le.0.and.xdv.ge.0) then ! case 1-II
       siu(j)=-1.
       siv(j)=1.
       end if
       if (xdu.ge.0.and.xdv.le.0) then ! case 1-III
       siu(j)=1.
       siv(j)=-1.
       end if
       if (xdu.le.0.and.xdv.le.0) then ! case 1-IV
       siu(j)=-1.
       siv(j)=-1.
       end if

       distee(j)=dsqrt((v(i,j,9)-v(i,j-1,9))**2.+
     + (u(i,j,9)-u(i,j-1,9))**2.)

       end do

c      Define

       do j=jirl(i),np(i,2)+np(i,3),-1
       u(i,j-1,32)=u(i,j,9)+siu(j)*distk*distee(j)*dcos(anglee(j))
       v(i,j-1,32)=v(i,j,9)+siv(j)*distk*distee(j)*dsin(anglee(j))
       u(i,j-1,9)=u(i,j-1,32)
       v(i,j-1,9)=v(i,j-1,32)
       end do

c      AFEGIR calcul punts extrems!!!!!!!!!!!

       j=np(i,2)+np(i,3)-2
       u(i,j,9)=u(i,j-1,9)+u(i,j-1,9)-u(i,j,9)
       v(i,j,9)=v(i,j-1,9)+v(i,j-1,9)-v(i,j,9)

c      The final row is a physical boundary rather than another panel.  The
c      legacy reconstruction above remains the comparison oracle, including
c      its distinct jirr measurement and jirl reconstruction indices.  Once
c      every exact intrados sewing point agrees, publish the typed result and
c      translate its cut contour by the same displacement.  The historical
c      extrapolation at intrados_first-1 is outside this boundary's ownership.
       if (i.eq.nribss.and.k31d.eq.1) then
       typed_terminal_intrados_reformat_control%
     + measurement_start_index=jirr(i)
       typed_terminal_intrados_reformat_control%
     + reconstruction_start_index=jirl(i)
       call reformat_terminal_intrados_boundary(
     + typed_terminal_intrados_production_edge,
     + typed_terminal_intrados_reformat_control,
     + typed_terminal_intrados_reformatted_edge,domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: terminal intrados reformat failed: ',
     + trim(domain_model_message)
       stop 1
       end if
       do j=typed_terminal_intrados_reformatted_edge%contour_first_index,
     + typed_terminal_intrados_reformatted_edge%contour_last_index
       k=j-typed_terminal_intrados_reformatted_edge%contour_first_index+1
       typed_coordinates_match=geometry_values_are_close(
     + typed_terminal_intrados_reformatted_edge%sewing_u(k),u(i,j,9)).and.
     + geometry_values_are_close(
     + typed_terminal_intrados_reformatted_edge%sewing_v(k),v(i,j,9))
       if (.not.typed_coordinates_match) then
       write (*,'(A,I0,4(1X,ES24.16))')
     + 'ERROR: typed/legacy terminal intrados reformat differs at point ',j,
     + typed_terminal_intrados_reformatted_edge%sewing_u(k),u(i,j,9),
     + typed_terminal_intrados_reformatted_edge%sewing_v(k),v(i,j,9)
       stop 1
       end if
       end do
       call write_legacy_production_boundary(
     + typed_terminal_intrados_reformatted_edge,
     + wing_profile_topologies(i),u,v,domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: terminal intrados reformat write-back: ',
     + trim(domain_model_message)
       stop 1
       end if
       typed_terminal_intrados_production_edge=
     + typed_terminal_intrados_reformatted_edge

c      The old extrapolation immediately above is the join-support oracle.
c      Compare it independently because it is not an intrados contour point,
c      then publish the typed sewing/cut pair at that one exact point.
       call reformat_preceding_join_support(
     + typed_terminal_intrados_join_support,
     + typed_terminal_intrados_reformatted_join_support,
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: terminal join-support reformat failed: ',
     + trim(domain_model_message)
       stop 1
       end if
       j=typed_terminal_intrados_reformatted_join_support%
     + support_point_index
       typed_coordinates_match=geometry_values_are_close(
     + typed_terminal_intrados_reformatted_join_support%sewing_u,
     + u(i,j,9)).and.geometry_values_are_close(
     + typed_terminal_intrados_reformatted_join_support%sewing_v,
     + v(i,j,9))
       if (.not.typed_coordinates_match) then
       write (*,'(A,I0)')
     + 'ERROR: typed/legacy terminal join support differs at point ',j
       stop 1
       end if
       call write_legacy_preceding_join_support(
     + typed_terminal_intrados_reformatted_join_support,u,v,
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: terminal join-support write-back: ',
     + trim(domain_model_message)
       stop 1
       end if
       end if

c      VERIFICATION

       dist=0.0d0
       do j=np(i,2)+np(i,3)-1,np(i,1)-1
       dist=dist+sqrt((v(i,j,9)-v(i,j+1,9))**2.+
     + (u(i,j,9)-u(i,j+1,9))**2.)
       end do
c       write (*,*) "VERFICACIO R ",i,dist,rib(i,33),rib(i,34),rib(i,35)


       end do ! i

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Final verification left and right side
c      Can be erased
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
       do i=1,nribss

       rib(i,40)=0.
       do j=1,np(i,2)-1
       rib(i,40)=rib(i,40)+sqrt((u(i-1,j,10)-u(i-1,j+1,10))**2.+
     + (v(i-1,j,10)-v(i-1,j+1,10))**2.)
       end do
      
       rib(i,42)=0.
       do j=1,np(i,2)-1
       rib(i,42)=rib(i,42)+sqrt((u(i,j,9)-u(i,j+1,9))**2.+
     + (v(i,j,9)-v(i,j+1,9))**2.)
       end do

c       write (*,*) "IGUALS ",i,rib(i,40),rib(i,31),rib(i,42)

       end do

       end if ! end reformat 

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Distorsion calculus
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Leading edge segment formated rib(i,99) intra

       do i=0,nribss-1

       if (i.lt.nribss) then
       j=np(i,2)+np(i,3)-1
       rib(i,99)=dsqrt((u(i,j,9)-u(i,j,10))**2.+
     + ((v(i,j,9)-v(i,j,10))**2.))
       end if

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Distorsion correction
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Correction only for case ndif=1000

c      NOT DISTORSION CORRECTION 1001 !!!!!!!!!!!!!!!!!!!!!!

c      Iterations

       do itera=1,5

       if (ndif.eq.1000) then

c      Not reformating rib 1 to preserve simmetry

       do i=2,nribss-1

       k1=jirl(i)
       k2=np(i,2)+np(i,3)-1

c      Straight distance in reformat area
       dist=dsqrt(((u(i,k1,9)-u(i,k2,9))**2.)+
     + ((v(i,k1,9)-v(i,k2,9))**2.))
      
       epsilon=(rib(i,99)-rib(i,98))

c      Angle of rotation
       if (abs(epsilon).lt.0.01) then
       omega=0.
       else
c      Experimental correction
       omega=1.0*dasin(epsilon/dist)*epsilon/(abs(epsilon))
       end if

c      Rotate left side (all correction)

       do j=jirl(i),np(i,2)+np(i,3),-1

c      Atention whit angle definition using abs tangents
c      Make subroutines
c      Angles and distances, left side

       xdv=(v(i,j-1,9)-v(i,j,9))
       xdu=(u(i,j-1,9)-u(i,j,9))
       if (xdu.ne.0.) then
       anglee(j)=abs(datan(xdv/xdu))-omega
       else
       anglee(j)=2.*datan(1.0d0)-omega
       end if

       if (xdu.ge.0.and.xdv.ge.0) then ! case 1-I
       siu(j)=1.
       siv(j)=1.
       end if
       if (xdu.le.0.and.xdv.ge.0) then ! case 1-II
       siu(j)=-1.
       siv(j)=1.
       end if
       if (xdu.ge.0.and.xdv.le.0) then ! case 1-III
       siu(j)=1.
       siv(j)=-1.
       end if
       if (xdu.le.0.and.xdv.le.0) then ! case 1-IV
       siu(j)=-1.
       siv(j)=-1.
       end if

       distee(j)=dsqrt((v(i,j,9)-v(i,j-1,9))**2.+
     + (u(i,j,9)-u(i,j-1,9))**2.)

       end do  ! end j

c      Define

       do j=jirl(i),np(i,2)+np(i,3),-1
       u(i,j-1,32)=u(i,j,9)+siu(j)*distee(j)*dcos(anglee(j))
       v(i,j-1,32)=v(i,j,9)+siv(j)*distee(j)*dsin(anglee(j))
       u(i,j-1,9)=u(i,j-1,32)
       v(i,j-1,9)=v(i,j-1,32)
       end do

c      AFEGIR calcul punts extrems!!!!!!!!!!! ?
c      com reformat normal

       end do  ! end i

       end if  ! if ndif=1000

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Distorsion 2 calculus
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Leading edge segment formated rib(i,99) extra

       do i=0,nribss-1

       if (i.lt.nribss) then
       j=np(i,2)+np(i,3)-1
       rib(i,99)=dsqrt((u(i,j,9)-u(i,j,10))**2.+
     + ((v(i,j,9)-v(i,j,10))**2.))
       end if

       end do ! i

       end do ! itera


c      DISTORSIONS
       do i=0,nribss-1
c       write (*,*) "99-98 ",rib(i,99),rib(i,98),rib(i,99)-rib(i,98)
       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.4.5 Calcula cantonades panells intrados
c      Versio millorada segons 8.2.5
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
     
c      Saltar cella 0 si gruix nul
       iini=0
       if (cencell.lt.0.01) then
       iini=1
       end if

       do i=iini,nribss

       npi=np(i,2)+np(i,3)-1
       npf=np(i,1)
       npo=npf-npi+1

c      Copy data in a free vector
       do j=1,npo

       uf(i,j,9)=u(i,npi+j-1,9)
       uf(i,j,10)=u(i,npi+j-1,10)
       uf(i,j,11)=u(i,npi+j-1,11)
       uf(i,j,12)=u(i,npi+j-1,12)
       vf(i,j,9)=v(i,npi+j-1,9)
       vf(i,j,10)=v(i,npi+j-1,10)
       vf(i,j,11)=v(i,npi+j-1,11)
       vf(i,j,12)=v(i,npi+j-1,12)

       ufi(i,j,9)=u(i,npi+j-1,9)
       ufi(i,j,10)=u(i,npi+j-1,10)
       ufi(i,j,11)=u(i,npi+j-1,11)
       ufi(i,j,12)=u(i,npi+j-1,12)
       vfi(i,j,9)=v(i,npi+j-1,9)
       vfi(i,j,10)=v(i,npi+j-1,10)
       vfi(i,j,11)=v(i,npi+j-1,11)
       vfi(i,j,12)=v(i,npi+j-1,12)

       end do

c      Call external points subroutine
c       call extpoints(i,uf,vf,npo,xupp,xuppte,xupple,1)

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.4.6 Dibuixa sobreamples panells intrados i vores de costura
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (n1draw.eq.1) then ! draw

c      Control if type is not "ss"
       if (atp.ne."ss") then

c      Box

c      Saltar cella 0 si gruix nul
       iini=0
       if (cencell.lt.0.01) then
       iini=1
       end if

       do i=iini,nribss-1
   
       psep=1970.*xkf+seppix(i)*1.0d0
       psey=1291.*xkf

cccccccccccccccccccccccccccccccccccccccccccccccccccccc    
c      Draw minirib intrados
cccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (i.ge.0.and.rib(i+1,56).gt.1.and.rib(i+1,56).ne.100.and
     + .atp.ne."ss") then

       xpo1=(u(i,np(i,1),9)+u(i,np(i,1),10))/2.
       ypo1=(v(i,np(i,1),9)+v(i,np(i,1),10))/2.
       j=jcvi(i+1)
       xpo2=(u(i,j,9)+u(i,j,10))/2.
       ypo2=(v(i,j,9)+v(i,j,10))/2.

c      Evita divisions per zero!!!
       if (xpo2-xpo1.ne.0.) then
       alpha=datan((ypo2-ypo1)/(xpo2-xpo1))
       end if
       if (abs(xpo2-xpo1).lt.0.00001) then
       alpha=pi/2.
       end if

       xpo3=xpo1-rib(i+1,61)*dcos(alpha)
       ypo3=ypo1-rib(i+1,61)*dsin(alpha)
       xdesx=xdes*dsin(alpha)
       xdesy=xdes*dcos(alpha)

c      Draw reference points for miniribs in intrados
       call line(psep+xpo1,psey-ypo1,psep+xpo3,psey-ypo3,5)
       call pointg(psep+xpo1-xdesx,psey-ypo1-xdesy,xcir,1)
       call pointg(psep+xpo3-xdesx,psey-ypo3-xdesy,xcir,1)

c      Laser cuting
       xadd=2520.*xkf
       call point(psep+xpo1+xadd,psey-ypo1,1)
       call point(psep+xpo3+xadd,psey-ypo3,1)

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Call draw panel intrados (complete)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      To 8.5 section

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.4.7 Dibuixa sobreamples panells intrados i vores de costura
c      Adre
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      To 8.5 section


c      End if control if not "ss"
       end if

       end if ! n1draw



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.5 Draw all panels
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.5.0.1 LAUNCH 3D-shaping calculus
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (k29d.eq.1) then

c      Set vector 47 for 3D airfoil coordinates       
       do i=0,nribss
       do j=1,np(i,1)
       u(i,j,47)=x(i,j)
       v(i,j,47)=y(i,j)
       w(i,j,47)=z(i,j)
       end do
       end do

c      Load "free" vectors in a new vector...
c      "69" points at left  side panel between rib i and i+1
c      "70" points at right side panel between rib i and i+1

       do i=0,nribss-1

c      Extrados
       do j=1,np(i,2)
       u(i,j,69)=ufe(i,j,9)
       v(i,j,69)=vfe(i,j,9)
       u(i,j,70)=ufe(i,j,10)
       v(i,j,70)=vfe(i,j,10)
       end do

c      Vents
       do j=np(i,2),np(i,2)+np(i,3)-1
       u(i,j,69)=ufv(i,j-np(i,2)+1,9)
       v(i,j,69)=vfv(i,j-np(i,2)+1,9)
       u(i,j,70)=ufv(i,j-np(i,2)+1,10)
       v(i,j,70)=vfv(i,j-np(i,2)+1,10)
       end do

c      Intrados
       do j=np(i,2)+np(i,3)-1,np(i,1)
       u(i,j,69)=ufi(i,j-(np(i,2)+np(i,3)-2),9)
       v(i,j,69)=vfi(i,j-(np(i,2)+np(i,3)-2),9)
       u(i,j,70)=ufi(i,j-(np(i,2)+np(i,3)-2),10)
       v(i,j,70)=vfi(i,j-(np(i,2)+np(i,3)-2),10)
       end do

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
c      Call main 3d subroutine 
c      Computes median airfoil (48)
c      Computes hautok(i,j)
c      Computes ovalized median airfoil (49)
c      Computes median local 2D airfoil (53)
c      Computes ovalized median local 2D airfoil (54)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Call panels 3D
       do i=1,nribss
       call panels3d(i,rib,np,u,v,w,
     + uppcuts,iupp,kiupp,lowcuts,ilow,kilow,hautok)
       end do

c      WARNING hautok=0.0 for i=0 and i=1 (?)
c      Provisional fix:
       i=1
       do j=1,np(i,1)
       hautok(i,j)=hautok(2,j)
       hautok(0,j)=hautok(i,j)
       end do
       


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     8.5.0.1+ 6.12.0 Miniribs
c     Now we have hautok vector
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Note: In section 8.5.0.0 we draw ovalized 2D airfoils
              
c      Box (1,2)
       
       sepxx=700.*xkf
       sepyy=100.*xkf
c      sepyy=100

       kx=0
       ky=0
       kyy=0

       do i=1,nribss

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*1.0*float(ky)

       sepxjo=sepxx+sepjox*float(kx)
       sepyjo=sepyy+sepjoy*1.0*float(ky)

c       if (i.eq.3) then
c       wsepxx,sepyy,sepjox,sepjoy
c       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.12.0 Miniribs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Upper surface minirib
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (rib(i,56).gt.1.and.rib(i,56).ne.100.and.atp.ne."ss") 
     + then

       rib(i,60)=0. ! Extrados minirib length

c      Detect point extrados
       jcontrole=1
       xequise=u(i-1,1,3)
       yequise=v(i-1,1,3)
       xminirib=rib(i-1,5)*rib(i,56)/100.
       do j=1,np(i,2)-1
       if (u(i-1,1,3)-u(i-1,j,3).lt.xminirib.and.u(i-1,1,3)-u(i-1,j+1,3)
     + .ge.xminirib) then
       jcontrole=j
       jcve(i)=j ! control vector extrados

c      Final point interpolation
       rib(i,107)=xminirib-(u(i-1,1,3)-u(i-1,j,3))
       xequise=u(i-1,j,3)-rib(i,107)
       yequise=v(i-1,j,3)-rib(i,107)*(v(i-1,j,3)-v(i-1,j+1,3))/
     + (u(i-1,j,3)-u(i-1,j+1,3))
       end if
       end do

c      Modification parameters
       rib(i-1,57)=c3001(3) !mod length
       rib(i-1,58)=c3001(4)*hautok(i-1,jcontrole)
       xminimod=rib(i-1,5)*rib(i-1,57)/100.
       xminisup=rib(i-1,58)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Set no modification i3001(2)=0
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (i3001(2).eq.0) then
c      Set vector 71 (airfoil) and 72 (contour)
       do j=1,jcontrole
       u(i-1,j,71)=u(i-1,j,3)
       v(i-1,j,71)=v(i-1,j,3)
       u(i-1,j+1,71)=u(i-1,j+1,3)
       v(i-1,j+1,71)=v(i-1,j+1,3)
       alpha=datan(-(v(i-1,j,71)-v(i-1,j+1,71))/
     + (u(i-1,j,71)-u(i-1,j+1,71)))
       u(i-1,j,72)=u(i-1,j,71)+xrib*0.1*dsin(alpha)
       v(i-1,j,72)=v(i-1,j,71)+xrib*0.1*dcos(alpha)
       end do
c      Last point
       j=jcontrole+1
       alpha=datan(-(v(i-1,j-1,71)-v(i-1,j,71))/
     + (u(i-1,j-1,71)-u(i-1,j,71)))
       u(i-1,j,71)=xequise
       v(i-1,j,71)=yequise
       u(i-1,j,72)=xequise+xrib*0.1*dsin(alpha)
       v(i-1,j,72)=yequise+xrib*0.1*dcos(alpha)
c      Length
       do j=1,jcontrole
       rib(i,60)=rib(i,60)+sqrt((u(i-1,j,71)-u(i-1,j+1,71))**2.+
     + (v(i-1,j,71)-v(i-1,j+1,71))**2.)
       end do
       end if ! mod 0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Set linear modification i3001(2)=1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (i3001(2).eq.1) then
c      Set vector 71 (airfoil) and 72 (contour)
       do j=1,jcontrole

c      Set modification
       yminiloc=0.0d0
       if (u(i-1,j,3).ge.u(i-1,1,3)-xminirib.and.
     + u(i-1,j,3).le.u(i-1,1,3)-xminirib+xminimod) then
       xminiloc=u(i-1,j,3)-xequise
       yminiloc=xminisup*(1.0d0-xminiloc/xminimod)
       end if

       u(i-1,j,71)=u(i-1,j,3)
       v(i-1,j,71)=v(i-1,j,3)+yminiloc
       u(i-1,j+1,71)=u(i-1,j+1,3)
       v(i-1,j+1,71)=v(i-1,j+1,3)+yminiloc
       alpha=datan(-(v(i-1,j,71)-v(i-1,j+1,71))/
     + (u(i-1,j,71)-u(i-1,j+1,71)))
       u(i-1,j,72)=u(i-1,j,71)+xrib*0.1*dsin(alpha)
       v(i-1,j,72)=v(i-1,j,71)+xrib*0.1*dcos(alpha)
       end do
c      Last point
       j=jcontrole+1
       alpha=datan(-(v(i-1,j-1,71)-v(i-1,j,71))/
     + (u(i-1,j-1,71)-u(i-1,j,71)))
       u(i-1,j,71)=xequise
       v(i-1,j,71)=yequise+xminisup
       u(i-1,j,72)=u(i-1,j,71)+xrib*0.1*dsin(alpha)
       v(i-1,j,72)=v(i-1,j,71)+xrib*0.1*dcos(alpha)
c      Length
       do j=1,jcontrole
       rib(i,60)=rib(i,60)+sqrt((u(i-1,j,71)-u(i-1,j+1,71))**2.+
     + (v(i-1,j,71)-v(i-1,j+1,71))**2.)
       end do
       end if ! mod 1

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Set cosinuidal modification
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (i3001(2).eq.3) then

c      Set vector 71 (airfoil) and 72 (contour)
       do j=1,jcontrole

c      Set modification
       yminiloc=0.0d0
       if (u(i-1,j,3).ge.u(i-1,1,3)-xminirib.and.
     + u(i-1,j,3).le.u(i-1,1,3)-xminirib+xminimod) then
       xminiloc=u(i-1,j,3)-xequise
       yminiloc=(xminisup/2.0d0)*(1.0d0+dcos(pi*xminiloc/xminimod))
       end if

       u(i-1,j,71)=u(i-1,j,3)
       v(i-1,j,71)=v(i-1,j,3)+yminiloc
       u(i-1,j+1,71)=u(i-1,j+1,3)
       v(i-1,j+1,71)=v(i-1,j+1,3)+yminiloc
       alpha=datan(-(v(i-1,j,71)-v(i-1,j+1,71))/
     + (u(i-1,j,71)-u(i-1,j+1,71)))
       u(i-1,j,72)=u(i-1,j,71)+xrib*0.1*dsin(alpha)
       v(i-1,j,72)=v(i-1,j,71)+xrib*0.1*dcos(alpha)
       end do
c      Last point
       j=jcontrole+1
       alpha=datan(-(v(i-1,j-1,71)-v(i-1,j,71))/
     + (u(i-1,j-1,71)-u(i-1,j,71)))
       u(i-1,j,71)=xequise
       v(i-1,j,71)=yequise+xminisup
       u(i-1,j,72)=u(i-1,j,71)+xrib*0.1*dsin(alpha)
       v(i-1,j,72)=v(i-1,j,71)+xrib*0.1*dcos(alpha)
c      Length
       do j=1,jcontrole
       rib(i,60)=rib(i,60)+sqrt((u(i-1,j,71)-u(i-1,j+1,71))**2.+
     + (v(i-1,j,71)-v(i-1,j+1,71))**2.)
       end do

       end if ! cosinuidal modification

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      DRAW section
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (evenodd2.eq.0.or.i.gt.1) then
c      Draw extrados minirib, Print and MC
       do j=1,jcontrole
c      Aifoil contour
       call line(sepx+u(i-1,j,71),-v(i-1,j,71)+sepy-sepriy*0.5,
     + sepx+u(i-1,j+1,71),-v(i-1,j+1,71)+sepy-sepriy*0.5,1)
c      Exterior contour
       call line(sepx+u(i-1,j,72),-v(i-1,j,72)+sepy-sepriy*0.5,
     + sepx+u(i-1,j+1,72),-v(i-1,j+1,72)+sepy-sepriy*0.5,3)
c      Exterior contour MC
       call line(2530.*xkf+sepx+u(i-1,j,72),-v(i-1,j,72)+sepy-sepriy*0.5
     + ,2530.*xkf+sepx+u(i-1,j+1,72),-v(i-1,j+1,72)+sepy-sepriy*0.5,1)
c      Perpendicular segments
       call line(sepx+u(i-1,j,72),-v(i-1,j,72)+sepy-sepriy*0.5,
     + sepx+u(i-1,j,71),-v(i-1,j,71)+sepy-sepriy*0.5,5)
       end do
       j=jcontrole+1
       call line(sepx+u(i-1,j,72),-v(i-1,j,72)+sepy-sepriy*0.5,
     + sepx+u(i-1,j,71),-v(i-1,j,71)+sepy-sepriy*0.5,5)
c      Initial perpendicular segment
       j=1
       call line(2530.*xkf+sepx+u(i-1,j,72),-v(i-1,j,72)+sepy-sepriy*0.5
     + ,2530.*xkf+sepx+u(i-1,j,71),-v(i-1,j,71)+sepy-sepriy*0.5,1)
       j=jcontrole+1
       call line(2530.*xkf+sepx+u(i-1,j,72),-v(i-1,j,72)+sepy-sepriy*0.5
     + ,2530.*xkf+sepx+u(i-1,j,71),-v(i-1,j,71)+sepy-sepriy*0.5,1)

       endif
       end if ! upper surface

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Bottom surface minirib
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (rib(i,56).gt.1.and.rib(i,56).ne.100.and.atp.ne."ss") 
     + then

       rib(i,61)=0. ! Intrados minirib length

c      Detect point intrados
       jcontroli=np(i,1)
       xequisi=u(i-1,np(i,1),3)
       yequisi=v(i-1,np(i,1),3)
       xminirib=rib(i-1,5)*rib(i,56)/100.
       do j=np(i,1),np(i,2),-1
       if (u(i-1,1,3)-u(i-1,j,3).lt.xminirib.and.u(i-1,1,3)-u(i-1,j-1,3)
     + .ge.xminirib) then
       jcontroli=j
       jcvi(i)=j ! control vector intrados

c      Final point interpolation
       rib(i,107)=xminirib-(u(i-1,1,3)-u(i-1,j,3))
       xequisi=u(i-1,j,3)-rib(i,107)
       yequisi=v(i-1,j,3)-rib(i,107)*(v(i-1,j,3)-v(i-1,j-1,3))/
     + (u(i-1,j,3)-u(i-1,j-1,3))
       end if
       end do

c      Modification parameters
       rib(i-1,57)=c3001(3) !mod length
       rib(i-1,58)=c3001(4)*hautok(i-1,jcontrole)
       rib(i-1,59)=c3001(4)*hautok(i-1,jcontroli)
       xminimod=rib(i-1,5)*rib(i-1,57)/100.
       xminisup=rib(i-1,58)
       xminiinf=rib(i-1,59)

c       if (i.eq.2) then
c       write (*,*) i,xminimod,xminisup,xminiinf
c       write (*,*) hautok(i-1,jcontrole),hautok(i-1,jcontroli)
c       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Set no modification i3001(2)=0
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (i3001(2).eq.0) then
c      Set vector 71 (airfoil) and 72 (contour)
       do j=np(i,1),jcontroli,-1
       u(i-1,j,71)=u(i-1,j,3)
       v(i-1,j,71)=v(i-1,j,3)
       u(i-1,j-1,71)=u(i-1,j-1,3)
       v(i-1,j-1,71)=v(i-1,j-1,3)
       alpha=datan((v(i-1,j,71)-v(i-1,j-1,71))/
     + (u(i-1,j,71)-u(i-1,j-1,71)))
       u(i-1,j,72)=u(i-1,j,71)+xrib*0.1*dsin(alpha)
       v(i-1,j,72)=v(i-1,j,71)-xrib*0.1*dcos(alpha)
       end do
c      Last point
       j=jcontroli-1
       alpha=datan((v(i-1,j,71)-v(i-1,j+1,71))/
     + (u(i-1,j,71)-u(i-1,j+1,71)))
       u(i-1,j,71)=xequisi
       v(i-1,j,71)=yequisi
       u(i-1,j,72)=xequisi+xrib*0.1*dsin(alpha)
       v(i-1,j,72)=yequisi-xrib*0.1*dcos(alpha)
c      Length
       do j=np(i,1),jcontroli,-1
       rib(i,61)=rib(i,61)+sqrt((u(i-1,j,71)-u(i-1,j-1,71))**2.+
     + (v(i-1,j,71)-v(i-1,j-1,71))**2.)
       end do
       end if ! mod 0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Set linear modification i3001(2)=1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (i3001(2).eq.1) then
c      Set vector 71 (airfoil) and 72 (contour)
       do j=np(i,1),jcontroli,-1

c      Set modification
       yminiloc=0.0d0
       if (u(i-1,j,3).ge.u(i-1,1,3)-xminirib.and.
     + u(i-1,j,3).le.u(i-1,1,3)-xminirib+xminimod) then
       xminiloc=u(i-1,j,3)-xequisi
       yminiloc=xminiinf*(1.0d0-xminiloc/xminimod)
       end if

       u(i-1,j,71)=u(i-1,j,3)
       v(i-1,j,71)=v(i-1,j,3)-yminiloc
       u(i-1,j-1,71)=u(i-1,j-1,3)
       v(i-1,j-1,71)=v(i-1,j-1,3)-yminiloc
       alpha=datan((v(i-1,j,71)-v(i-1,j-1,71))/
     + (u(i-1,j,71)-u(i-1,j-1,71)))
       u(i-1,j,72)=u(i-1,j,71)+xrib*0.1*dsin(alpha)
       v(i-1,j,72)=v(i-1,j,71)-xrib*0.1*dcos(alpha)
       end do
c      Last point
       j=jcontroli-1
       alpha=datan((v(i-1,j,71)-v(i-1,j+1,71))/
     + (u(i-1,j,71)-u(i-1,j+1,71)))
       u(i-1,j,71)=xequisi
       v(i-1,j,71)=yequisi-xminiinf
       u(i-1,j,72)=u(i-1,j,71)+xrib*0.1*dsin(alpha)
       v(i-1,j,72)=v(i-1,j,71)-xrib*0.1*dcos(alpha)
c      Length
       do j=np(i,1),jcontroli,-1
       rib(i,61)=rib(i,61)+sqrt((u(i-1,j,71)-u(i-1,j-1,71))**2.+
     + (v(i-1,j,71)-v(i-1,j-1,71))**2.)
       end do
       end if ! mod 1

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Set cosinuidal modification i3001(2)=3
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (i3001(2).eq.3) then
c      Set vector 71 (airfoil) and 72 (contour)
       do j=np(i,1),jcontroli,-1

c      Set modification
       yminiloc=0.0d0
       if (u(i-1,j,3).ge.u(i-1,1,3)-xminirib.and.
     + u(i-1,j,3).le.u(i-1,1,3)-xminirib+xminimod) then
       xminiloc=u(i-1,j,3)-xequisi
       yminiloc=(xminiinf/2.0d0)*(1.0d0+dcos(pi*xminiloc/xminimod))
       end if

       u(i-1,j,71)=u(i-1,j,3)
       v(i-1,j,71)=v(i-1,j,3)-yminiloc
       u(i-1,j-1,71)=u(i-1,j-1,3)
       v(i-1,j-1,71)=v(i-1,j-1,3)-yminiloc
       alpha=datan((v(i-1,j,71)-v(i-1,j-1,71))/
     + (u(i-1,j,71)-u(i-1,j-1,71)))
       u(i-1,j,72)=u(i-1,j,71)+xrib*0.1*dsin(alpha)
       v(i-1,j,72)=v(i-1,j,71)-xrib*0.1*dcos(alpha)
       end do
c      Last point
       j=jcontroli-1
       alpha=datan((v(i-1,j,71)-v(i-1,j+1,71))/
     + (u(i-1,j,71)-u(i-1,j+1,71)))
       u(i-1,j,71)=xequisi
       v(i-1,j,71)=yequisi-xminiinf
       u(i-1,j,72)=u(i-1,j,71)+xrib*0.1*dsin(alpha)
       v(i-1,j,72)=v(i-1,j,71)-xrib*0.1*dcos(alpha)
c      Length
       do j=np(i,1),jcontroli,-1
       rib(i,61)=rib(i,61)+sqrt((u(i-1,j,71)-u(i-1,j-1,71))**2.+
     + (v(i-1,j,71)-v(i-1,j-1,71))**2.)
       end do
       end if ! mod 1

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      DRAW section
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (evenodd2.eq.0.or.i.gt.1) then
c      Draw intrados minirib, Print and MC
       do j=np(i,1),jcontroli,-1
c      Airfoil contour
       call line(sepx+u(i-1,j,71),-v(i-1,j,71)+sepy-sepriy*0.5,
     + sepx+u(i-1,j-1,71),-v(i-1,j-1,71)+sepy-sepriy*0.5,1)
c      Exterior contour
       call line(sepx+u(i-1,j,72),-v(i-1,j,72)+sepy-sepriy*0.5,
     + sepx+u(i-1,j-1,72),-v(i-1,j-1,72)+sepy-sepriy*0.5,3) 
c      Exterior contour MC
       call line(2530.*xkf+sepx+u(i-1,j,72),-v(i-1,j,72)+sepy-sepriy*0.5
     + ,2530.*xkf+sepx+u(i-1,j-1,72),-v(i-1,j-1,72)+sepy-sepriy*0.5,1) 
c      Perpendicular segments
       call line(sepx+u(i-1,j,72),-v(i-1,j,72)+sepy-sepriy*0.5,
     + sepx+u(i-1,j,71),-v(i-1,j,71)+sepy-sepriy*0.5,5)
       end do
       j=jcontroli-1
       call line(sepx+u(i-1,j,72),-v(i-1,j,72)+sepy-sepriy*0.5,
     + sepx+u(i-1,j,71),-v(i-1,j,71)+sepy-sepriy*0.5,5)
c      Initial perpendicular segment
       j=np(i,1)
       call line(2530.*xkf+sepx+u(i-1,j,72),-v(i-1,j,72)+sepy-sepriy*0.5
     + ,2530.*xkf+sepx+u(i-1,j,71),-v(i-1,j,71)+sepy-sepriy*0.5,1)
       j=jcontroli-1
       call line(2530.*xkf+sepx+u(i-1,j,71),-v(i-1,j,71)+sepy-sepriy*0.5
     + ,2530.*xkf+sepx+u(i-1,j,72),-v(i-1,j,72)+sepy-sepriy*0.5,1)

c      Draw control romano point
       j=jcontroli
       x1=2530.*xkf+sepx+0.5*(xequisi+xequise)
       y1=sepy-sepriy*0.5-0.5*(yequisi+yequise)
       call romano(i,x1+1.,y1+2.,0.0d0,typm6(9)*0.1,7)

c      Draw init segments print and MC
       j1=jcontrole+1
       j2=jcontroli-1
       call line(sepx+u(i-1,j1,71),-v(i-1,j1,71)+sepy-sepriy*0.5,
     + sepx+u(i-1,j2,71),-v(i-1,j2,71)+sepy-sepriy*0.5,1)
       call line(2530.*xkf+sepx+u(i-1,j1,71),
     + -v(i-1,j1,71)+sepy-sepriy*0.5,
     + 2530.*xkf+sepx+u(i-1,j2,71),-v(i-1,j2,71)+sepy-sepriy*0.5,1)

c      Draw minirib number
       j=jcontrole+1
       x1=sepx+u(i-1,j1,71)
       y1=-0.5*(v(i-1,j1,71)+v(i-1,j2,71))+sepy-sepriy*0.5
       call itxt(x1-rib(i,60)*0.35-30.,y1,3.0d0,0.0d0,i,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Section 38: elliptic holes in miniribs (type 9)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       call find_hvr_hole_spec(hvr_holes,9,hvr_hole,
     + hvr_hole_found)
       if (hvr_hole_found) then
       hvr_hmax=abs(yequise-yequisi)
       do hvr_hole_index=1,hvr_hole%hole_count
       hvr_span_fraction=dfloat(hvr_hole%hole_count+2-
     + hvr_hole_index)/dfloat(hvr_hole%hole_count+2)
       x3=hvr_span_fraction*xminirib+0.05d0*xminirib
       y3=hvr_span_fraction*hvr_hmax*hvr_hole%parameter(3)
       hvr_hole_height=hvr_span_fraction*hvr_hmax*
     + hvr_hole%parameter(1)/100.0d0
       b=0.5d0*abs(hvr_hole_height)
       a=abs(hvr_hole%parameter(2))*b
       angle=hvr_hole%parameter(4)*pi/180.0d0
       x0=sepx+u(i-1,1,3)-x3
       y0=sepy-sepriy*0.5+v(i-1,1,3)-y3
       call ellipse(x0,y0,a,b,angle,3)
       call ellipse(2530.*xkf+x0,y0,a,b,angle,3)
       end do
       endif

       endif
       end if ! bottom surface

       kx=int((float(i)/6.))
       ky=i-kx*6
       kyy=kyy+1

       end do ! i airfoil

















cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Call compute zones of influence
c      zinf(i,6,1) first cut extrados
c      zinf(i,6,2) second cut extrados
c      zinf(i,6,4) first cut intrados
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
       do i=1,nribss
       call czinf(i,rib,np,u,v,w,
     + uppcuts,iupp,kiupp,lowcuts,ilow,kilow,hautok,zinf)
       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.5.0.2 Call tessellation calculus
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Call 3D tessellation
c      nsegments=12 ! increment for more detail

c      Tessellation active cases DXF, DXF ext, STL
       if (pp29(3,1).eq.1.or.pp29(4,1).eq.1.or.pp29(5,1).eq.1) then

       do i=1,nribss
       if (rib(i,2).ge.0.01) then
       call tessella(i,rib,np,u,v,w,hautok,nsegments,tesse3d)
       end if
       end do

       end if ! tessellation


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.5.0.3 Draw ovalized airfoils in 2D
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Box (1,8)
       
       sepxx=700.*xkf
       sepyy=100.*xkf
       kx=0
       ky=0
       kyy=0

       do i=1,nribss

       sepx=6.0*1260.0*xkf+sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*1.0*float(ky)

c      Draw vents (copied from 9.4)

c      ini
       j=np(i,2)
       alpha1=pi+datan((w(i,j,53)-w(i,j-1,53))/(v(i,j,53)-v(i,j-1,53)))
       call line(sepx+v(i,j,53),-w(i,j,53)+sepy,sepx+v(i,j,53)-
     + 4.*dsin(alpha1),-w(i,j,53)-4.*dcos(alpha1)+sepy,1)

       x1=2530.*xkf+sepx+v(i,j,53)+0.1*typm6(4)*dsin(alpha1)
       y1=-w(i,j,53)+sepy+0.1*typm6(4)*dcos(alpha1)
       x2=2530.*xkf+sepx+v(i,j,53)-0.1*(typm5(4)-typm6(4))*dsin(alpha1)
       y2=-0.1*(typm5(4)-typm6(4))*dcos(alpha1)-w(i,j,53)+sepy

       if (typevent.eq.1) then
c      Double point
c       call point(x1,y1,3)
c       call point(x2,y2,3)
       end if

       if (typevent.eq.2) then
c      Segment
c       call linevent(x1,y1,x2,y2,3)
       end if

       if (typevent.eq.3) then
c      Segment 101
c       call segment101(x1,y1,x2,y2,3)
       end if

c      fi
       j=np(i,2)+np(i,3)-1
       alpha1=pi+datan((w(i,j+1,53)-w(i,j-1,53))/(v(i,j+1,53)-
     + v(i,j-1,53)))
       call line(sepx+v(i,j,53),-w(i,j,53)+sepy,sepx+v(i,j,53)-
     + 4.*dsin(alpha1),-w(i,j,53)-4.*dcos(alpha1)+sepy,1)

       x1=2530.*xkf+sepx+v(i,j,53)+0.1*typm6(4)*dsin(alpha1)
       y1=-w(i,j,53)+sepy+0.1*typm6(4)*dcos(alpha1)
       x2=2530.*xkf+sepx+v(i,j,53)-0.1*(typm5(4)-typm6(4))*dsin(alpha1)
       y2=-0.1*(typm5(4)-typm6(4))*dcos(alpha1)-w(i,j,53)+sepy

       if (typevent.eq.1) then
c      Double point
c       call point(x1,y1,3)
c       call point(x2,y2,3)
       end if

       if (typevent.eq.2) then
c      Segment
c       call linevent(x1,y1,x2,y2,3)
       end if

       if (typevent.eq.3) then
c      Segment 101
c       call segment101(x1,y1,x2,y2,3)
       end if

c      Numbering
       call romano(i,sepx+0.89d0*rib(i,5),sepy-1.0d0,0.0d0,
     + typm6(9)*0.1,4)
       call itxt(sepx-16.-84.*(typm3(9)/7.),sepy,typm3(9),0.0d0,i,7)

c      Draw middle 2D airfoil (index 53) and ovalized (index 54)
c      Nota: millorar dibuix SK

       if (atp.ne."ss") then
       do j=1,np(i,1)-1
       call line(sepx+v(i,j,53),sepy-w(i,j,53),sepx+v(i,j+1,53),
     + sepy-w(i,j+1,53),7)
       call line(sepx+v(i,j,54),sepy-w(i,j,54),sepx+v(i,j+1,54),
     + sepy-w(i,j+1,54),4)
       end do ! j
       end if

       if (atp.eq."ss") then
       do j=1,np(i,2)-1
       call line(sepx+v(i,j,53),sepy-w(i,j,53),sepx+v(i,j+1,53),
     + sepy-w(i,j+1,53),7)
       call line(sepx+v(i,j,54),sepy-w(i,j,54),sepx+v(i,j+1,54),
     + sepy-w(i,j+1,54),4)
       end do ! j
       end if

       if (atp.eq."ss".and.rib(i,165).eq.1) then
       do j=np(i,2)-1,np(i,2)+np(i,3)-2
       call line(sepx+v(i,j,53),sepy-w(i,j,53),sepx+v(i,j+1,53),
     + sepy-w(i,j+1,53),7)
       call line(sepx+v(i,j,54),sepy-w(i,j,54),sepx+v(i,j+1,54),
     + sepy-w(i,j+1,54),4)
       end do ! j
       end if

       kx=int((float(i)/6.))
       ky=i-kx*6
       kyy=kyy+1

c      Draw points cuts 3D-shaping
       if (k29d.eq.1) then

       ng=rib(i,169)

c      Extrados points
       if (uppcuts(ng).eq.0) then
       end if

       if (uppcuts(ng).eq.1) then
       j=iupp(1,2,ng)
       call line(sepx+v(i,j,53),sepy-w(i,j,53),
     + sepx+v(i,j,54),sepy-w(i,j,54),3)
       j=iupp(1,3,ng)
       call line(sepx+v(i,j,53),sepy-w(i,j,53),
     + sepx+v(i,j,54),sepy-w(i,j,54),6)
       end if

       if (uppcuts(ng).eq.2) then
       j=iupp(1,2,ng)
       call line(sepx+v(i,j,53),sepy-w(i,j,53),
     + sepx+v(i,j,54),sepy-w(i,j,54),3)
       j=iupp(1,3,ng)
       call line(sepx+v(i,j,53),sepy-w(i,j,53),
     + sepx+v(i,j,54),sepy-w(i,j,54),6)
       j=iupp(2,3,ng)
       call line(sepx+v(i,j,53),sepy-w(i,j,53),
     + sepx+v(i,j,54),sepy-w(i,j,54),2)
       end if

c      Intrados points
       if (atp.ne."ss") then
       if (lowcuts(ng).eq.0) then
       end if
       if (lowcuts(ng).eq.1) then
       j=ilow(1,2,ng)
       call line(sepx+v(i,j,53),sepy-w(i,j,53),
     + sepx+v(i,j,54),sepy-w(i,j,54),32)
       j=ilow(1,3,ng)
       call line(sepx+v(i,j,53),sepy-w(i,j,53),
     + sepx+v(i,j,54),sepy-w(i,j,54),5)
       end if
       end if

       end if ! k29d=1

       end do ! i


       end if ! k29d=1


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (n1draw.eq.1) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Plotter panels
c      Box (1,3)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Avoid central panel if thickness is 0
       iini=0  ! panel 0 (central)
       if (cencell.lt.0.01) then
       iini=1
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.5.1 Extrados without vents
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Plotter BOX(1,3)

       do i=iini,nribss-1
       
       psep=1970.*xkf+seppix(i)*1.0d0
       psey=400.*xkf

c      Print panel numbers
       xlen=rib(2,2)-rib(1,2)
c      Boxes (0,3),(-1,3)
       if (k29d.eq.1) then
       call itxt2(psep+1.1*xlen,-psey-890.95*xkf*1.0+30.,
     + typm3(9),0.0d0,i+1,7)
       if (atp.ne."ss") then
       call itxt2(psep+1.1*xlen,-psey-890.95*xkf*0.0+30.,
     + typm3(9),0.0d0,i+1,7)
       end if
       end if
c      Boxes (1,3) (2,3)
       call itxt2(psep+1.1*xlen,-psey+890.95*xkf*1.0-64.,
     + typm3(9),0.0d0,i+1,7)
       if (atp.ne."ss") then
       call itxt2(psep+1.1*xlen,-psey+890.95*xkf*2.0-64.,
     + typm3(9),0.0d0,i+1,7)
       end if

c      load data extrados from a free vector

       npi=1
       npf=np(i,2)
       npo=npf-npi+1

       do j=1,npo

       uf(i,j,9)=ufe(i,j,9)
       uf(i,j,10)=ufe(i,j,10)
       uf(i,j,11)=ufe(i,j,11)
       uf(i,j,12)=ufe(i,j,12)
       vf(i,j,9)=vfe(i,j,9)
       vf(i,j,10)=vfe(i,j,10)
       vf(i,j,11)=vfe(i,j,11)
       vf(i,j,12)=vfe(i,j,12)

       end do

c      Print panels extrados without vents
       if (k26d.eq.0.or.rib(i+1,165).ne.1) then
       call extpoints(i,uf,vf,npo,xupp,xupple,xuppte,1)
       call dpanelc(i,uf,vf,npo,psep,psey)
       end if

       end do

c      Laser BOX(1,5)

       do i=iini,nribss-1
       
       psep=1970.*xkf+2520.*xkf+seppix(i)*1.0d0
       psey=400.*xkf

c      Print panels extrados without vents
       if (k26d.eq.0.or.rib(i+1,165).ne.1) then
       call extpoints(i,uf,vf,npo,xupp,xupple,xuppte,1)
       call dpanelb(i,uf,vf,npo,psep,psey)
       end if

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.5.2 Extrados with vents
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Plotter BOX(1,3)

       do i=iini,nribss-1
       
       psep=1970.*xkf+seppix(i)*1.0d0
       psey=400.*xkf

c      load data extrados from a free vector

c      Piece 1
       npi=1
       npf=np(i,2)
       npo1=npf-npi+1

       do j=1,npo1
       uf(i,j,9)=ufe(i,j,9)
       uf(i,j,10)=ufe(i,j,10)
       uf(i,j,11)=ufe(i,j,11)
       uf(i,j,12)=ufe(i,j,12)
       vf(i,j,9)=vfe(i,j,9)
       vf(i,j,10)=vfe(i,j,10)
       vf(i,j,11)=vfe(i,j,11)
       vf(i,j,12)=vfe(i,j,12)
       end do

       npi=np(i,2)
       npf=np(i,2)+np(i,3)-1
       npo2=npf-npi+1

c      Move piece 2 to 1 by translation and rotation
       cs1x=(uf(i,npo1,9)+uf(i,npo1,10))*0.5
       cs1y=(vf(i,npo1,9)+vf(i,npo1,10))*0.5
       cs2x=(ufv(i,1,9)+ufv(i,1,10))*0.5
       cs2y=(vfv(i,1,9)+vfv(i,1,10))*0.5

       alpha=(datan((v(i,npo1,10)-v(i,npo1,9))/
     + (u(i,npo1,10)-u(i,npo1,9))))

c      1) Traslació -cs2x
       do j=1,npo2
       ufb(i,j,9)=ufv(i,j,9)-cs2x
       ufb(i,j,10)=ufv(i,j,10)-cs2x
       ufb(i,j,11)=ufv(i,j,11)-cs2x
       ufb(i,j,12)=ufv(i,j,12)-cs2x
       end do

c      2) Rotació alpha
       do j=1,npo2
       ufa(i,j,9)=ufb(i,j,9)*dcos(alpha)-vfv(i,j,9)*dsin(alpha)
       vfa(i,j,9)=ufb(i,j,9)*dsin(alpha)+vfv(i,j,9)*dcos(alpha)
       ufa(i,j,10)=ufb(i,j,10)*dcos(alpha)-vfv(i,j,10)*dsin(alpha)
       vfa(i,j,10)=ufb(i,j,10)*dsin(alpha)+vfv(i,j,10)*dcos(alpha)
       ufa(i,j,11)=ufb(i,j,11)*dcos(alpha)-vfv(i,j,11)*dsin(alpha)
       vfa(i,j,11)=ufb(i,j,11)*dsin(alpha)+vfv(i,j,11)*dcos(alpha)
       ufa(i,j,12)=ufb(i,j,12)*dcos(alpha)-vfv(i,j,12)*dsin(alpha)
       vfa(i,j,12)=ufb(i,j,12)*dsin(alpha)+vfv(i,j,12)*dcos(alpha)
       end do

c      3) Traslació (cs1x,cs1y)
       do j=1,npo2
       ufb(i,j,9)=ufa(i,j,9)+cs1x
       ufb(i,j,10)=ufa(i,j,10)+cs1x
       ufb(i,j,11)=ufa(i,j,11)+cs1x
       ufb(i,j,12)=ufa(i,j,12)+cs1x
       vfb(i,j,9)=vfa(i,j,9)+cs1y
       vfb(i,j,10)=vfa(i,j,10)+cs1y
       vfb(i,j,11)=vfa(i,j,11)+cs1y
       vfb(i,j,12)=vfa(i,j,12)+cs1y
       end do

c      Load Piece 2
       do j=2,npo2  ! 2 to avoid double definition

       uf(i,j+npo1-1,9)=ufb(i,j,9)
       uf(i,j+npo1-1,10)=ufb(i,j,10)
       uf(i,j+npo1-1,11)=ufb(i,j,11)
       uf(i,j+npo1-1,12)=ufb(i,j,12)
       vf(i,j+npo1-1,9)=vfb(i,j,9)
       vf(i,j+npo1-1,10)=vfb(i,j,10)
       vf(i,j+npo1-1,11)=vfb(i,j,11)
       vf(i,j+npo1-1,12)=vfb(i,j,12)

       end do

       npo=npf

c      Print panels extrados with vents
       
       if (k26d.eq.1.and.rib(i+1,165).eq.1) then
c      WARNING!!! Amples laterals xupp no fan efecte a la subrutina
c      Cal definir abans als vectors u(i,j,11) v u(i,j,12) v
       call extpoints(i,uf,vf,npo,xupp,xupple,xuppte,1)
       call dpanelc(i,uf,vf,npo,psep,psey)
       call line(psep+uf(i,npo1,9),psey-vf(i,npo1,9),
     + psep+uf(i,npo1,10),psey-vf(i,npo1,10),1)


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Print points vents (left)

       xu=uf(i,npo,9)
       xv=vf(i,npo,9)

c      Despla a vores punts de control de costures
       alp=abs(datan((vf(i,npo,9)-vf(i,npo-1,9))/
     + (uf(i,npo,9)-uf(i,npo-1,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,3)
c      Point laser
       call point(psep+xu+2520.*xkf,-xv+psey,7)

c      Print points vents (right)

       xu=uf(i,npo,10)
       xv=vf(i,npo,10)

c      Despla a vores punts de control de costures
       alp=abs(datan((vf(i,npo,10)-vf(i,npo-1,10))/
     + (uf(i,npo,10)-uf(i,npo-1,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,3)
c      Point laser
       call point(psep+xu+2520.*xkf,-xv+psey,7)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       end if

       end do

c      Laser BOX(1,5)

       do i=iini,nribss-1

       psep=1970.*xkf+2520.*xkf+seppix(i)*1.0d0
       psey=400.*xkf

c      Print panels extrados with vents
       if (k26d.eq.1.and.rib(i+1,165).eq.1) then
       call extpoints(i,uf,vf,npo,xupp,xuppte,xupple,1)
       call dpanelb(i,uf,vf,npo,psep,psey)

       end if

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.5.3 Vents amb vores
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=iini,nribss-1
       
       psep=1970.*xkf+seppix(i)*1.0d0
       psey=1371.*xkf

c      load data vents from a free vector

       npi=np(i,2)
       npf=np(i,2)+np(i,3)-1
       npo=npf-npi+1

       do j=1,npo

       uf(i,j,9)=ufv(i,j,9)
       uf(i,j,10)=ufv(i,j,10)
       uf(i,j,11)=ufv(i,j,11)
       uf(i,j,12)=ufv(i,j,12)
       vf(i,j,9)=vfv(i,j,9)
       vf(i,j,10)=vfv(i,j,10)
       vf(i,j,11)=vfv(i,j,11)
       vf(i,j,12)=vfv(i,j,12)

       end do

c      Print panels vents
       if (k26d.eq.1.and.rib(i+1,165).eq.0) then

c       write (*,*) i,xlow,xlowle,xlowte

c      WARNING!!! xlow NOT USED in subroutine (defided previosly in 8.3.1 and 8.3.2)
       call extpoints(i,uf,vf,npo,xlow,xlowle,xupple,1)
       call dpanelc(i,uf,vf,npo,psep,psey)
       end if

c      Vents case -2 (joint to intrados)
c      NO CAL
       if (k26d.eq.1.and.rib(i+1,165).eq.-2) then
c       call extpoints(i,uf,vf,npo,xlow,xlowle,xupple,1)
c       call dpanelc(i,uf,vf,npo,psep,psey)
c       call line(psep+uf(i,npo,9),psey-vf(i,npo,9),psep+uf(i,1,10),
c     + psey-vf(i,1,10),1)
       end if

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.5.4 Intrados without vents
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Control if type is not "ss"
       if (atp.ne."ss") then

       do i=iini,nribss-1
       
       psep=1970.*xkf+seppix(i)*1.0d0
       psey=1291.*xkf

c      load data vents from a free vector

       npi=np(i,2)+np(i,3)-1
       npf=np(i,1)
       npo=npf-npi+1

       do j=1,npo

       uf(i,j,9)=ufi(i,j,9)
       uf(i,j,10)=ufi(i,j,10)
       uf(i,j,11)=ufi(i,j,11)
       uf(i,j,12)=ufi(i,j,12)
       vf(i,j,9)=vfi(i,j,9)
       vf(i,j,10)=vfi(i,j,10)
       vf(i,j,11)=vfi(i,j,11)
       vf(i,j,12)=vfi(i,j,12)

       end do

c      Print panels intrados
       if (k26d.eq.0.or.rib(i+1,165).ge.0) then
       call extpoints(i,uf,vf,npo,xupp,xlowte,xlowle,1)
       call dpanelc(i,uf,vf,npo,psep,psey)
       end if

       end do

c      Laser BOX(1,5)

       do i=iini,nribss-1
       
       psep=1970.*xkf+2520.*xkf+seppix(i)*1.0d0
       psey=1291.*xkf

c      Print panels intrados
       if (k26d.eq.0.or.rib(i+1,165).ge.0) then
       call extpoints(i,uf,vf,npo,xupp,xlowte,xlowle,1)
       call dpanelb(i,uf,vf,npo,psep,psey)
       end if

       end do

       end if  ! control intrados "ss"


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.5.5 Intrados with vents
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Control if type is not "ss"
       if (atp.ne."ss") then

       do i=iini,nribss-1
       
       psep=1970.*xkf+seppix(i)*1.0d0
       psey=1291.*xkf

c      load data intrados from a free vector

       npi=np(i,2)+np(i,3)-1
       npf=np(i,1)
       npo1=npf-npi+1

       do j=1,npo1
       ufa(i,j,9)=ufi(i,j,9)
       ufa(i,j,10)=ufi(i,j,10)
       ufa(i,j,11)=ufi(i,j,11)
       ufa(i,j,12)=ufi(i,j,12)
       vfa(i,j,9)=vfi(i,j,9)
       vfa(i,j,10)=vfi(i,j,10)
       vfa(i,j,11)=vfi(i,j,11)
       vfa(i,j,12)=vfi(i,j,12)
       end do

       cs1x=(ufa(i,1,9)+ufa(i,1,10))*0.5 ! Point 1
       cs1y=(vfa(i,1,9)+vfa(i,1,10))*0.5 ! Point 1
       
       alpha1=(datan((vfa(i,1,10)-vfa(i,1,9))/
     + (ufa(i,1,10)-ufa(i,1,9))))

c      1) Print panels intrados without vents and without LE
       if (k26d.eq.1.and.rib(i+1,165).le.-1) then
       call extpoints(i,ufa,vfa,npo1,xupp,xlowte,xlowle,1)
       call dpanelc1(i,ufa,vfa,npo,psep,psey)
       end if

c      2) Load vents 

       npi=np(i,2)
       npf=np(i,2)+np(i,3)-1
       npo2=npf-npi+1      
             
       do j=1,npo2
       ufb(i,j,9)=ufv(i,j,9)
       ufb(i,j,10)=ufv(i,j,10)
       ufb(i,j,11)=ufv(i,j,11)
       ufb(i,j,12)=ufv(i,j,12)
       vfb(i,j,9)=vfv(i,j,9)
       vfb(i,j,10)=vfv(i,j,10)
       vfb(i,j,11)=vfv(i,j,11)
       vfb(i,j,12)=vfv(i,j,12)
       end do

c      3) Translate vents from point 2 to (0,0)
      
       cs2x=(ufb(i,npo2,9)+ufb(i,npo2,10))*0.5
       cs2y=(vfb(i,npo2,9)+vfb(i,npo2,10))*0.5
       
       do j=1,npo2
       ufb(i,j,9)=ufv(i,j,9)-cs2x
       ufb(i,j,10)=ufv(i,j,10)-cs2x
       ufb(i,j,11)=ufv(i,j,11)-cs2x
       ufb(i,j,12)=ufv(i,j,12)-cs2x
       vfb(i,j,9)=vfv(i,j,9)-cs2y
       vfb(i,j,10)=vfv(i,j,10)-cs2y
       vfb(i,j,11)=vfv(i,j,11)-cs2y
       vfb(i,j,12)=vfv(i,j,12)-cs2y
       end do

c      4) Rotation angle alpha

       alpha2=(datan((vfb(i,npo2,10)-vfb(i,npo2,9))/
     + (ufb(i,npo2,10)-ufb(i,npo2,9))))
       alpha=-(alpha2-alpha1)
      
       do j=1,npo2
       ufc(i,j,9)=ufb(i,j,9)*dcos(alpha)-vfb(i,j,9)*dsin(alpha)
       vfc(i,j,9)=ufb(i,j,9)*dsin(alpha)+vfb(i,j,9)*dcos(alpha)
       ufc(i,j,10)=ufb(i,j,10)*dcos(alpha)-vfb(i,j,10)*dsin(alpha)
       vfc(i,j,10)=ufb(i,j,10)*dsin(alpha)+vfb(i,j,10)*dcos(alpha)
       ufc(i,j,11)=ufb(i,j,11)*dcos(alpha)-vfb(i,j,11)*dsin(alpha)
       vfc(i,j,11)=ufb(i,j,11)*dsin(alpha)+vfb(i,j,11)*dcos(alpha)
       ufc(i,j,12)=ufb(i,j,12)*dcos(alpha)-vfb(i,j,12)*dsin(alpha)
       vfc(i,j,12)=ufb(i,j,12)*dsin(alpha)+vfb(i,j,12)*dcos(alpha)
       end do

c      5) Translate vents from point (0,0) to 1
       
       do j=1,npo2
       uf(i,j,9)=ufc(i,j,9)+cs1x
       uf(i,j,10)=ufc(i,j,10)+cs1x
       uf(i,j,11)=ufc(i,j,11)+cs1x
       uf(i,j,12)=ufc(i,j,12)+cs1x
       vf(i,j,9)=vfc(i,j,9)+cs1y
       vf(i,j,10)=vfc(i,j,10)+cs1y
       vf(i,j,11)=vfc(i,j,11)+cs1y
       vf(i,j,12)=vfc(i,j,12)+cs1y
       end do

c      6) Force last point of vent as first point of intrados (?)

       uf(i,npo2,9)=ufa(i,1,9)
       uf(i,npo2,10)=ufa(i,1,10)
       uf(i,npo2,11)=ufa(i,1,11)
       uf(i,npo2,12)=ufa(i,1,12)
       vf(i,npo2,9)=vfa(i,1,9)
       vf(i,npo2,10)=vfa(i,1,10)
       vf(i,npo2,11)=vfa(i,1,11)
       vf(i,npo2,12)=vfa(i,1,12)
       
c      7) Print vents without TE case -1
       if (k26d.eq.1.and.rib(i+1,165).eq.-1.) then
       call extpoints(i,uf,vf,npo2,xupp,xlowte,xlowle,1)
       call dpanelc2(i,uf,vf,npo2,psep,psey)
      
c      Print points vents (left)

       xu=uf(i,1,9)
       xv=vf(i,1,9)

c      Despla a vores punts de control de costures
       alp=abs(datan((vf(i,1,9)-vf(i,2,9))/
     + (uf(i,1,9)-uf(i,2,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,3)
c      Point laser
       call point(psep+xu+2520.*xkf,-xv+psey,7)

c      Print points vents (right)

       xu=uf(i,1,10)
       xv=vf(i,1,10)

c      Despla a vores punts de control de costures
       alp=abs(datan((vf(i,1,10)-vf(i,2,10))/
     + (uf(i,1,10)-uf(i,2,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,3)
c      Point laser
       call point(psep+xu+2520.*xkf,-xv+psey,7)
       end if


c      8) Print vents without TE case -2
       if (k26d.eq.1.and.rib(i+1,165).eq.-2.) then
       call extpoints(i,uf,vf,npo2,xupp,xlowte,xlowle,1)
       call dpanelcm2(i,uf,vf,npo2,psep,psey)
       do j=1,npo2-1 ! LASER
       call line(psep+2520.*xkf+uf(i,j,12),psey-vf(i,j,12),
     + psep+2520.*xkf+uf(i,j+1,12),psey-vf(i,j+1,12),3)
       end do
       call line(psep+uf(i,npo2,9),psey-vf(i,npo2,9),psep+uf(i,1,10),
     + psey-vf(i,1,10),1)

c      Linia de vora diagonal
       lvalp=datan((vf(i,npo2,9)-vf(i,1,10))/
     + ((uf(i,1,10)-uf(i,npo2,9))))
       lv1u=uf(i,npo2,9)-xlowle*0.1*dsin(lvalp)
       lv1v=vf(i,npo2,9)-xlowle*0.1*dcos(lvalp)
       lv2u=uf(i,1,10)-xlowle*0.1*dsin(lvalp)
       lv2v=vf(i,1,10)-xlowle*0.1*dcos(lvalp)
      
c      Intersection of segments with diagonal
c      Point left
       xru(1)=uf(i,npo2,11)
       xru(2)=uf(i,npo2-1,11)
       xrv(1)=vf(i,npo2,11)
       xrv(2)=vf(i,npo2-1,11)
       xsu(1)=lv1u
       xsu(2)=lv2u
       xsv(1)=lv1v
       xsv(2)=lv2v
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)
       lv3u=xtu
       lv3v=xtv
c      Point right
       xru(1)=uf(i,2,12)
       xru(2)=uf(i,1,12)
       xrv(1)=vf(i,2,12)
       xrv(2)=vf(i,1,12)
       xsu(1)=lv1u
       xsu(2)=lv2u
       xsv(1)=lv1v
       xsv(2)=lv2v
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)
       lv4u=xtu
       lv4v=xtv

c      Print versions
       call line(psep+lv3u,psey-lv3v,psep+lv4u,psey-lv4v,3)
       call line(psep+lv3u,psey-lv3v,psep+uf(i,npo2,11),
     + psey-vf(i,npo2,11),3)
       call line(psep+lv4u,psey-lv4v,psep+uf(i,1,12),psey-vf(i,1,12),3)
       call line(psep+uf(i,npo2,9),psey-vf(i,npo2,9),
     + psep+uf(i,npo2,9)-xlowle*0.1*dsin(lvalp),
     + psey-vf(i,npo2,9)+xlowle*0.1*dcos(lvalp),3)
       call line(psep+uf(i,1,10),psey-vf(i,1,10),
     + psep+uf(i,1,10)-xlowle*0.1*dsin(lvalp),
     + psey-vf(i,1,10)+xlowle*0.1*dcos(lvalp),3)
       call line(psep+uf(i,npo2,9),psey-vf(i,npo2,9),
     + psep+uf(i,npo2,11),psey-vf(i,npo2,11),3)
 
c      Laser version
       call line(psep+2520.*xkf+lv3u,psey-lv3v,psep+
     + 2520.*xkf+lv4u,psey-lv4v,3)
       call line(psep+2520.*xkf+lv3u,psey-lv3v,psep+
     + 2520.*xkf+uf(i,npo2,11),
     + psey-vf(i,npo2,11),3)
       call line(psep+2520.*xkf+lv4u,psey-lv4v,psep+
     + 2520.*xkf+uf(i,1,12),psey-vf(i,1,12),3)

c      Print points vents (left)

       xu=uf(i,1,9)
       xv=vf(i,1,9)

c      Despla a vores punts de control de costures
       alp=abs(datan((vf(i,1,9)-vf(i,2,9))/
     + (uf(i,1,9)-uf(i,2,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)

c      Point imp
c       call pointg(psep+xu,psey-xv,xcir,3)
c      Point laser
c       call point(psep+xu+2520.*xkf,-xv+psey,7)

c      Print points vents (right)

       xu=uf(i,1,10)
       xv=vf(i,1,10)

c      Despla a vores punts de control de costures
       alp=abs(datan((vf(i,1,10)-vf(i,2,10))/
     + (uf(i,1,10)-uf(i,2,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,3)
c      Point laser
       call point(psep+xu+2520.*xkf,-xv+psey,7)
       end if ! case vent -2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c      9) Print vents without TE case -3
       if (k26d.eq.1.and.rib(i+1,165).eq.-3.) then
       call extpoints(i,uf,vf,npo2,xupp,xlowte,xlowle,1)
       call dpanelcm3(i,uf,vf,npo2,psep,psey)
       do j=1,npo2-1 ! LASER
       call line(psep+2520.*xkf+uf(i,j,11),psey-vf(i,j,11),
     + psep+2520.*xkf+uf(i,j+1,11),psey-vf(i,j+1,11),3)
       end do
       call line(psep+uf(i,1,9),psey-vf(i,1,9),psep+uf(i,npo2,10),
     + psey-vf(i,npo2,10),1)

c      Linia de vora diagonal
       lvalp=datan((vf(i,npo2,10)-vf(i,1,9))/
     + ((uf(i,npo2,10)-uf(i,1,9))))
       lv1u=uf(i,1,9)+xlowle*0.1*dsin(lvalp)
       lv1v=vf(i,1,9)-xlowle*0.1*dcos(lvalp)
       lv2u=uf(i,npo2,10)+xlowle*0.1*dsin(lvalp)
       lv2v=vf(i,npo2,10)-xlowle*0.1*dcos(lvalp)
     
c      Intersection of segments with diagonal
c      Point left
       xru(1)=uf(i,2,11)
       xru(2)=uf(i,1,11)
       xrv(1)=vf(i,2,11)
       xrv(2)=vf(i,1,11)
       xsu(1)=lv1u
       xsu(2)=lv2u
       xsv(1)=lv1v
       xsv(2)=lv2v
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)
       lv3u=xtu
       lv3v=xtv
c      Point right
       xru(1)=uf(i,npo2,12)
       xru(2)=uf(i,npo2-1,12)
       xrv(1)=vf(i,npo2,12)
       xrv(2)=vf(i,npo2-1,12)
       xsu(1)=lv1u
       xsu(2)=lv2u
       xsv(1)=lv1v
       xsv(2)=lv2v
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)
       lv4u=xtu
       lv4v=xtv

c      Print versions
       call line(psep+lv3u,psey-lv3v,psep+lv4u,psey-lv4v,3)
       call line(psep+lv3u,psey-lv3v,psep+uf(i,1,11),
     + psey-vf(i,1,11),3)
       call line(psep+lv4u,psey-lv4v,psep+uf(i,npo2,12),
     + psey-vf(i,npo2,12),3)
       call line(psep+uf(i,1,9),psey-vf(i,1,9),
     + psep+uf(i,1,9)+xlowle*0.1*dsin(lvalp),
     + psey-vf(i,1,9)+xlowle*0.1*dcos(lvalp),3)
       call line(psep+uf(i,npo2,10),psey-vf(i,npo2,10),
     + psep+uf(i,npo2,10)+xlowle*0.1*dsin(lvalp),
     + psey-vf(i,npo2,10)+xlowle*0.1*dcos(lvalp),3)
       call line(psep+uf(i,npo2,10),psey-vf(i,npo2,10),
     + psep+uf(i,npo2,12),psey-vf(i,npo2,12),3)

c      Laser version
       call line(psep+2520.*xkf+lv3u,psey-lv3v,psep+
     + 2520.*xkf+lv4u,psey-lv4v,3)
       call line(psep+2520.*xkf+lv3u,psey-lv3v,psep+
     + 2520.*xkf+uf(i,1,11),psey-vf(i,1,11),3)
       call line(psep+2520.*xkf+lv4u,psey-lv4v,psep+
     + 2520.*xkf+uf(i,npo2,12),psey-vf(i,npo2,12),3)

c      Print points vents (left)

       xu=uf(i,1,9)
       xv=vf(i,1,9)

c      Despla a vores punts de control de costures
       alp=abs(datan((vf(i,1,9)-vf(i,2,9))/
     + (uf(i,1,9)-uf(i,2,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,3)
c      Point laser
       call point(psep+xu+2520.*xkf,-xv+psey,7)

c      Print points vents (right)

       xu=uf(i,1,10)
       xv=vf(i,1,10)

c      Despla a vores punts de control de costures
       alp=abs(datan((vf(i,1,10)-vf(i,2,10))/
     + (uf(i,1,10)-uf(i,2,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)

c      Point imp
c       call pointg(psep+xu,psey-xv,xcir,3)
c      Point laser
c       call point(psep+xu+2520.*xkf,-xv+psey,7)
       end if ! case vent -3
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc




       end do

c      Laser BOX(1,5)

       do i=iini,nribss-1
       
       psep=1970.*xkf+2520.*xkf+seppix(i)*1.0d0
       psey=1291.*xkf
    
c      Print panels intrados without vents
       if (k26d.eq.1.and.rib(i+1,165).le.-1) then
       call extpoints(i,ufa,vfa,npo1,xupp,xlowte,xlowle,1)
       call dpanelb1(i,ufa,vfa,npo1,psep,psey)
       end if

c      Print vents
       if (k26d.eq.1.and.rib(i+1,165).eq.-1) then
       call extpoints(i,uf,vf,npo2,xupp,xupple,xlowle,1)
       call dpanelb2(i,uf,vf,npo2,psep,psey)
       end if

       end do

       end if  ! control intrados "ss"


       end if ! n1draw section 8.5

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.6 Draw all panels (with 3D cuts)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (k29d.eq.1) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Plotter panels
c      BOX (0,3)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Avoid central panel if thickness is 0
       iini=0  ! panel 0 (central)
       if (cencell.lt.0.01) then
       iini=1
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Sign adjustement
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       do i=iini,nribss-1
       is1=1
       is2=1
       is4=1
       if (zinf(i,6,1).lt.0.) then
c       is1=-1
       end if
       if (zinf(i,6,2).lt.0.) then
c       is2=-1
       end if
       if (zinf(i,6,4).lt.0.) then
c       is4=-1
       end if
c       write (*,*) "Signe  ", i,is1,is2,is4
c       write (*,*) "Signe- ", i,is1*-1,-1*is2,-1*is4

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.6.1 Extrados
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Plotter BOX(-1,3)

c      FORCED RECURSE
c      Please REVIEW all xcirc,xdes denitions!!!!
c      Used in subroutine drwvent to call prinfpv
       do i=0,nribss
       csi(i,50)=xcir
       csi(i,51)=xdes
       end do
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      REVISAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       zinf(0,6,2)=zinf(1,6,2)
       zinf(0,6,1)=zinf(1,6,1)
       zinf(0,6,4)=zinf(1,6,4)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=iini,nribss-1

       ic2=dint(rib(i+1,165)) ! vent type
       
       psep=1970.*xkf+seppix(i)*1.0d0
       psey=xyshift*xkf-890.95*xyextra*xkf ! remove 890.95 to set BOX (1,3)

c      REDEFINITION (!) for proper printing
       ng=rib(i+1,169)

       if (i.eq.0) then
       ng=rib(1,169)
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Previ BRUTE FORCE, use for subroutine drwvent
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       npi=1
       npf=np(i,2)
       npo=npf-npi+1

       do j=1,npo
       ufa(i,j,9)=ufe(i,j,9)
       ufa(i,j,10)=ufe(i,j,10)
       ufa(i,j,11)=ufe(i,j,11)
       ufa(i,j,12)=ufe(i,j,12)
       vfa(i,j,9)=vfe(i,j,9)
       vfa(i,j,10)=vfe(i,j,10)
       vfa(i,j,11)=vfe(i,j,11)
       vfa(i,j,12)=vfe(i,j,12)
       end do

c       cs1x=(ufa(i,1,9)+ufa(i,1,10))*0.5 ! Point 1
c       cs1y=(vfa(i,1,9)+vfa(i,1,10))*0.5 ! Point 1

c      IN DOUBT... USE BRUTE FORCE!!!!!!!!!!!!!!!!!!!!!
       csi(i,21)=ufa(i,npo,9)
       csi(i,22)=vfa(i,npo,9)
       csi(i,23)=ufa(i,npo,10)
       csi(i,24)=vfa(i,npo,10)
       csi(i,25)=ufa(i,npo,11)
       csi(i,26)=vfa(i,npo,11)
       csi(i,27)=ufa(i,npo,12)
       csi(i,28)=vfa(i,npo,12)
       
c       alpha1=(datan((vfa(i,1,10)-vfa(i,1,9))/
c     + (ufa(i,1,10)-ufa(i,1,9))))

c       csi(i,29)=alpha1
c       csi(i,30)=xlow
c       csi(i,31)=xlowte
c       csi(i,32)=xlowle
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      End BRUTE FORCE
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccccccccccccccccccc    
c      Draw minirib extrados (copy code from 8.2.6)
cccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (i.ge.0.and.rib(i+1,56).gt.1.and.rib(i+1,56).ne.100
     + .and.atp.ne."ss") then

       xpo1=(u(i,1,9)+u(i,1,10))/2.
       ypo1=(v(i,1,9)+v(i,1,10))/2.
       j=jcve(i+1)
       xpo2=(u(i,j,9)+u(i,j,10))/2.
       ypo2=(v(i,j,9)+v(i,j,10))/2.

c      Avoid division by zero!!!
       if (xpo2-xpo1.ne.0.) then
       alpha=datan((ypo2-ypo1)/(xpo2-xpo1))
       end if
       if (abs(xpo2-xpo1).lt.0.00001) then
       alpha=pi/2.
       end if

       xpo3=xpo1+rib(i+1,60)*dcos(alpha)
       ypo3=ypo1+rib(i+1,60)*dsin(alpha)
       xdesx=xdes*dsin(alpha)
       xdesy=xdes*dcos(alpha)

c      Draw reference points for miniribs in extrados
       call line(psep+xpo1,psey-ypo1,psep+xpo3,psey-ypo3,5)
       call pointg(psep+xpo1-xdesx,psey-ypo1-xdesy,xcir,1)
       call pointg(psep+xpo3-xdesx,psey-ypo3-xdesy,xcir,1)

c      Laser cuting
       xadd=2520.*xkf
       if (i37_2005.eq.1) then
       call line(psep+xpo1+xadd,psey-ypo1,
     + psep+xpo3+xadd,psey-ypo3,5)
       endif
       call point(psep+xpo1+xadd,psey-ypo1,1)
       call point(psep+xpo3+xadd,psey-ypo3,1)

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case extrados 0 cuts
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (uppcuts(ng).eq.0) then

c      Extrados panel from TE to LE

       npi=1
       npf=np(i,2)
       npo=npf-npi+1

       do j=1,npo
       uf(i,j,9)=ufe(i,j,9)
       uf(i,j,10)=ufe(i,j,10)
       uf(i,j,11)=ufe(i,j,11)
       uf(i,j,12)=ufe(i,j,12)
       vf(i,j,9)=vfe(i,j,9)
       vf(i,j,10)=vfe(i,j,10)
       vf(i,j,11)=vfe(i,j,11)
       vf(i,j,12)=vfe(i,j,12)
       end do

c      Set panel side lengths part 1
       call llarlr(i,1,1,npo,uf,vf,llarl,llarr)

c      Roman marks
       x1=uf(i,1,9)
       y1=vf(i,1,9)
       x2=uf(i,1,10)
       y2=vf(i,1,10)
       y3=1.0d0
       call romanop(i,1,x1,y1,x2,y2,y3,psep,psey,xkf)

c      Print panels extrados without vents
       if (k26d.eq.0.or.rib(i+1,165).le.0) then
       call extpoints(i,uf,vf,npo,xupp,xupple,xuppte,1)
       call dpanelcc(i,uf,vf,npo,psep,psey,3)
       call dpanelcc(i,uf,vf,npo,psep+2520.*xkf,psey,7)
       end if

c      Print equidistant points
       xinil=0.0d0
       xinir=0.0d0
       call xmarksi(i,1,1,npo,uf,vf,rib,xinil,xinir,
     + xfinl,xfinr,xmark,llarl,llarr,psep,psey,xcir,xdes,xkf)

c      Print panels extrados with vents
       if (k26d.eq.1.and.rib(i+1,165).ge.1) then
       call extpoints(i,uf,vf,npo,xupp,xupple,xuppte,1)
       call dpanelcc(i,uf,vf,npo,psep,psey,2)
       call dpanelcc(i,uf,vf,npo,psep+2520.*xkf,psey,6)

c      Set panel side lengths part 4
       call llarlr(i,1,4,np(i,3),ufv,vfv,llarl,llarr)

c      Print vents
       call drwvent(i,np,uf,vf,ufv,vfv,psep,psey,xupp,xupple,
     + xuppte,xkf,1,ic2,csi)   ! case print
       call drwvent(i,np,uf,vf,ufv,vfv,psep+2520.*xkf,psey,xupp,xupple,
     + xuppte,xkf,2,ic2,csi)   ! case laser
       end if
   
       end if ! Extrados 0 cuts

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case extrados 1 cut
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (uppcuts(ng).eq.1) then
c      Extrados panel from TE to first cut
       npo=iupp(1,3,ng)
       do j=1,npo
       uf(i,j,9)=ufe(i,j,9)
       uf(i,j,10)=ufe(i,j,10)
       uf(i,j,11)=ufe(i,j,11)
       uf(i,j,12)=ufe(i,j,12)
       vf(i,j,9)=vfe(i,j,9)
       vf(i,j,10)=vfe(i,j,10)
       vf(i,j,11)=vfe(i,j,11)
       vf(i,j,12)=vfe(i,j,12)
       end do

c      Roman marks
       x1=uf(i,1,9)
       y1=vf(i,1,9)
       x2=uf(i,1,10)
       y2=vf(i,1,10)
       y3=1.0d0
       call romanop(i,1,x1,y1,x2,y2,y3,psep,psey,xkf)

c      Set panel side lengths part 1
       call llarlr(i,1,1,npo,uf,vf,llarl,llarr)

c      Print equidistant points
       xinil=0.0d0
       xinir=0.0d0
       call xmarksi(i,1,1,npo,uf,vf,rib,xinil,xinir,
     + xfinl,xfinr,xmark,llarl,llarr,psep,psey,xcir,xdes,xkf)

       call extpoints(i,uf,vf,npo,xupp,xupp,xuppte,1)
       call dpanelcc(i,uf,vf,npo,psep,psey,2)
       call dpanelcc(i,uf,vf,npo,psep+2520.*xkf,psey,6)
       call arc3p(i,npo,uf,vf,zinf(i+1,6,1),1*is1,psep,psey,1,xupp)
       call arc3p(i,npo,uf,vf,zinf(i+1,6,1),1*is1,psep+2520.*xkf,psey,
     + 2,xupp)

c      Extrados panel from cut to LE
       ysaut=-ysautt

       npi=iupp(1,3,ng)
       npf=np(i,2)
       npo=npf-npi+1

       do j=1,npo
       uf(i,j,9)=ufe(i,j+npi-1,9)
       uf(i,j,10)=ufe(i,j+npi-1,10)
       uf(i,j,11)=ufe(i,j+npi-1,11)
       uf(i,j,12)=ufe(i,j+npi-1,12)
       vf(i,j,9)=vfe(i,j+npi-1,9)
       vf(i,j,10)=vfe(i,j+npi-1,10)
       vf(i,j,11)=vfe(i,j+npi-1,11)
       vf(i,j,12)=vfe(i,j+npi-1,12)
       end do

c      Roman marks
       x1=uf(i,1,9)
       y1=vf(i,1,9)
       x2=uf(i,1,10)
       y2=vf(i,1,10)
       y3=1.0d0
c       call romanop(i,x1,-y1,x2,-y2,y3,psep,psey+ysaut,xkf)

c      Set panel side lengths part 2
       call llarlr(i,1,2,npo,uf,vf,llarl,llarr)

c      Print equidistant points
       xinil=xfinl(i,1,1)
       xinir=xfinr(i,1,1)
       call xmarksi(i,1,2,npo,uf,vf,rib,xinil,xinir,
     + xfinl,xfinr,xmark,llarl,llarr,psep,psey+ysaut,xcir,xdes,xkf)

c      Print panels without vents       
       if (k26d.eq.0.or.rib(i+1,165).le.0) then
       call extpoints(i,uf,vf,npo,xupp,xupple,xupp,1)
       call dpanelcc(i,uf,vf,npo,psep,psey+ysaut,4)
       call dpanelcc(i,uf,vf,npo,psep+2520.*xkf,psey+ysaut,8)
       call arc3p(i,1,uf,vf,zinf(i+1,6,1),-1*is1,psep,psey+ysaut,1,xupp)
       call arc3p(i,1,uf,vf,zinf(i+1,6,1),-1*is1,psep+2520.*xkf,
     + psey+ysaut,2,xupp)



c      CALL ROMANO ARC EXPERIMENTAL
       call romanoparc(i,1,1,uf,vf,zinf(i+1,6,1),-1,psep,psey+ysaut,
     + 1,xkf)



       end if

c      Print panels with vents       
       if (k26d.eq.0.or.rib(i+1,165).ge.1) then
       call extpoints(i,uf,vf,npo,xupp,xupple,xupp,1)
       call dpanelcc(i,uf,vf,npo,psep,psey+ysaut,1)
       call dpanelcc(i,uf,vf,npo,psep+2520.*xkf,psey+ysaut,5)
       call arc3p(i,1,uf,vf,zinf(i+1,6,1),-1*is1,psep,psey+ysaut,1,xupp)
       call arc3p(i,1,uf,vf,zinf(i+1,6,1),-1*is1,psep+2520.*xkf,
     + psey+ysaut,2,xupp)

c      Set panel side lengths part 4
       call llarlr(i,1,4,np(i,3),ufv,vfv,llarl,llarr)



c      CALL ROMANO ARC EXPERIMENTAL
       call romanoparc(i,1,1,uf,vf,zinf(i+1,6,1),-1,psep,psey+ysaut,
     + 1,xkf)



c      Print vents
       call drwvent(i,np,uf,vf,ufv,vfv,psep,psey+ysaut,xupp,xupple,
     + xuppte,xkf,1,ic2,csi)   ! case print
       call drwvent(i,np,uf,vf,ufv,vfv,psep+2520.*xkf,psey+ysaut,xupp,
     + xupple,xuppte,xkf,2,ic2,csi)   ! case laser
       end if

       end if ! Extrados 1 cut

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case extrados 2 cuts
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (uppcuts(ng).eq.2) then

c      Extrados panel from TE to first cut
       npo=iupp(1,3,ng)
       do j=1,npo
       uf(i,j,9)=ufe(i,j,9)
       uf(i,j,10)=ufe(i,j,10)
       uf(i,j,11)=ufe(i,j,11)
       uf(i,j,12)=ufe(i,j,12)
       vf(i,j,9)=vfe(i,j,9)
       vf(i,j,10)=vfe(i,j,10)
       vf(i,j,11)=vfe(i,j,11)
       vf(i,j,12)=vfe(i,j,12)
       end do

c      Roman marks
       x1=uf(i,1,9)
       y1=vf(i,1,9)
       x2=uf(i,1,10)
       y2=vf(i,1,10)
       y3=1.0d0
       call romanop(i,1,x1,y1,x2,y2,y3,psep,psey,xkf)

c      Set panel side lengths part 1
       call llarlr(i,1,1,npo,uf,vf,llarl,llarr)

c      Print equidistant points
       xinil=0.0d0
       xinir=0.0d0
       call xmarksi(i,1,1,npo,uf,vf,rib,xinil,xinir,
     + xfinl,xfinr,xmark,llarl,llarr,psep,psey,xcir,xdes,xkf)

       call extpoints(i,uf,vf,npo,xupp,xupp,xuppte,1)
       call dpanelcc(i,uf,vf,npo,psep,psey,2)
       call dpanelcc(i,uf,vf,npo,psep+2520.*xkf,psey,6)
       call arc3p(i,npo,uf,vf,zinf(i+1,6,1),1*is1,psep,psey,1,xupp)
       call arc3p(i,npo,uf,vf,zinf(i+1,6,1),1*is1,psep+2520.*xkf,psey,2,
     + xupp)

c      Extrados panel from first to second cut
       ysaut=-ysautt
      
       npi=iupp(1,3,ng)
       npf=iupp(2,3,ng)
       npo=npf-npi+1

       do j=1,npo
       uf(i,j,9)=ufe(i,j+npi-1,9)
       uf(i,j,10)=ufe(i,j+npi-1,10)
       uf(i,j,11)=ufe(i,j+npi-1,11)
       uf(i,j,12)=ufe(i,j+npi-1,12)
       vf(i,j,9)=vfe(i,j+npi-1,9)
       vf(i,j,10)=vfe(i,j+npi-1,10)
       vf(i,j,11)=vfe(i,j+npi-1,11)
       vf(i,j,12)=vfe(i,j+npi-1,12)
       end do

c      Set panel side lengths part 2
       call llarlr(i,1,2,npo,uf,vf,llarl,llarr)

c      Print equidistant points
       xinil=xfinl(i,1,1)
       xinir=xfinr(i,1,1)
c       write (*,*) "X ",i,xinil,xinir
       call xmarksi(i,1,1,npo,uf,vf,rib,xinil,xinir,
     + xfinl,xfinr,xmark,llarl,llarr,psep,psey+ysaut,xcir,xdes,xkf)
       call extpoints(i,uf,vf,npo,xupp,xupp,xupp,1)
       call dpanelcc(i,uf,vf,npo,psep,psey+ysaut,1)
       call dpanelcc(i,uf,vf,npo,psep+2520.*xkf,psey+ysaut,5)

       call arc3p(i,1,uf,vf,zinf(i+1,6,1),-1*is1,psep,psey+ysaut,1,xupp)
       call arc3p(i,1,uf,vf,zinf(i+1,6,1),-1*is1,psep+2520.*xkf,psey+
     + ysaut,2,xupp)
       call arc3p(i,npo,uf,vf,zinf(i+1,6,2),1*is2,psep,psey+ysaut,
     + 1,xupp)
       call arc3p(i,npo,uf,vf,zinf(i+1,6,2),1*is2,psep+2520.*xkf,psey+
     + ysaut,2,xupp)


c      CALL ROMANO ARC EXPERIMENTAL
       call romanoparc(i,1,1,uf,vf,zinf(i+1,6,1),-1,psep,psey+ysaut,
     + 1,xkf)

c      Extrados panel from second cut to LE
       ysaut=-ysautt*2.0d0
      
       npi=iupp(2,3,ng)
       npf=np(i,2)
       npo=npf-npi+1

       do j=1,npo
       uf(i,j,9)=ufe(i,j+npi-1,9)
       uf(i,j,10)=ufe(i,j+npi-1,10)
       uf(i,j,11)=ufe(i,j+npi-1,11)
       uf(i,j,12)=ufe(i,j+npi-1,12)
       vf(i,j,9)=vfe(i,j+npi-1,9)
       vf(i,j,10)=vfe(i,j+npi-1,10)
       vf(i,j,11)=vfe(i,j+npi-1,11)
       vf(i,j,12)=vfe(i,j+npi-1,12)
       end do

c      Set panel side lengths part 3
       call llarlr(i,1,3,npo,uf,vf,llarl,llarr)

c      Print equidistant points
       xinil=xfinl(i,1,2)
       xinir=xfinr(i,1,2)
       call xmarksi(i,1,1,npo,uf,vf,rib,xinil,xinir,
     + xfinl,xfinr,xmark,llarl,llarr,psep,psey+ysaut,xcir,xdes,xkf)

c      Print panels without vents       
       if (k26d.eq.0.or.rib(i+1,165).le.0) then
       call extpoints(i,uf,vf,npo,xupp,xupple,xupp,1)
       call dpanelcc(i,uf,vf,npo,psep,psey+ysaut,4)
       call dpanelcc(i,uf,vf,npo,psep+2520.*xkf,psey+ysaut,8)



       call arc3p(i,1,uf,vf,zinf(i+1,6,2),-1*is2,psep,psey+ysaut,1,xupp)
       call arc3p(i,1,uf,vf,zinf(i+1,6,2),-1*is2,psep+2520.*xkf,psey+
     + ysaut,2,xupp)




       end if

c      CALL ROMANO ARC EXPERIMENTAL
       call romanoparc(i,1,1,uf,vf,zinf(i+1,6,2),-1,psep,psey+ysaut,
     + 2,xkf)


c      Print panels with vents       
       if (k26d.eq.0.or.rib(i+1,165).ge.1) then
       call extpoints(i,uf,vf,npo,xupp,xupple,xupp,1)
       call dpanelcc(i,uf,vf,npo,psep,psey+ysaut,1)
       call dpanelcc(i,uf,vf,npo,psep+2520.*xkf,psey+ysaut,5)
       call arc3p(i,1,uf,vf,zinf(i+1,6,2),-1*is2,psep,psey+ysaut,1,xupp)
       call arc3p(i,1,uf,vf,zinf(i+1,6,2),-1*is2,psep+2520.*xkf,psey+
     + ysaut,2,xupp)



c      CALL ROMANO ARC EXPERIMENTAL
       call romanoparc(i,1,1,uf,vf,zinf(i+1,6,2),-1,psep,psey+ysaut,
     + 2,xkf)



c      Set panel side lengths part 4
       call llarlr(i,1,4,np(i,3),ufv,vfv,llarl,llarr)

c      Print vents
       call drwvent(i,np,uf,vf,ufv,vfv,psep,psey+ysaut,xupp,xupple,
     + xuppte,xkf,1,ic2,csi)   ! case print
       call drwvent(i,np,uf,vf,ufv,vfv,psep+2520.*xkf,psey+ysaut,xupp,
     + xupple,xuppte,xkf,2,ic2,csi)   ! case laser
       end if

       ysaut=-ysautt

       end if ! 2 extrados cuts
 
       end do ! rib i

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      8.6.2 Intrados
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Plotter BOX(0,3) and BOX (0,5)

       if (atp.ne."ss") then

c      Avoid central panel if thickness is 0
       iini=0  ! panel 0 (central)
       if (cencell.lt.0.01) then
       iini=1
       end if

       do i=iini,nribss-1

       ic2=dint(rib(i+1,165)) ! vent type

       psep=1970.*xkf+seppix(i)*1.0d0
       psey=xyshift*xkf-890.95*xyintra*xkf ! remove 890.95 to set BOX (1,3)

c      REDEFINITION (!) for proper printing
       ng=rib(i+1,169)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Previ BRUTE FORCE, use for subroutine drwvent
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       npi=np(i,2)+np(i,3)-1
       npf=np(i,1)
       npo1=npf-npi+1

       do j=1,npo1
       ufa(i,j,9)=ufi(i,j,9)
       ufa(i,j,10)=ufi(i,j,10)
       ufa(i,j,11)=ufi(i,j,11)
       ufa(i,j,12)=ufi(i,j,12)
       vfa(i,j,9)=vfi(i,j,9)
       vfa(i,j,10)=vfi(i,j,10)
       vfa(i,j,11)=vfi(i,j,11)
       vfa(i,j,12)=vfi(i,j,12)
       end do

       cs1x=(ufa(i,1,9)+ufa(i,1,10))*0.5 ! Point 1
       cs1y=(vfa(i,1,9)+vfa(i,1,10))*0.5 ! Point 1

c      IN DOUBT... USE BRUTE FORCE!!!!!!!!!!!!!!!!!!!!!
       csi(i,1)=ufa(i,1,9)
       csi(i,2)=vfa(i,1,9)
       csi(i,3)=ufa(i,1,10)
       csi(i,4)=vfa(i,1,10)
       csi(i,5)=ufa(i,1,11)
       csi(i,6)=vfa(i,1,11)
       csi(i,7)=ufa(i,1,12)
       csi(i,8)=vfa(i,1,12)
       
       alpha1=(datan((vfa(i,1,10)-vfa(i,1,9))/
     + (ufa(i,1,10)-ufa(i,1,9))))

       csi(i,9)=alpha1
       csi(i,10)=xlow
       csi(i,11)=xlowte
       csi(i,12)=xlowle
       csi(i,13)=xupple

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      End BRUTE FORCE
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccccccccccccccccccc    
c      Draw minirib intrados (copied from 8.4.6)
c      and adding "ysaut"
cccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (lowcuts(ng).eq.1) then
       ysaut=-ysautt
       else
       ysaut=0.0d0
       end if

       if (i.ge.0.and.rib(i+1,56).gt.1.and.rib(i+1,56).ne.100.and
     + .atp.ne."ss") then

       xpo1=(u(i,np(i,1),9)+u(i,np(i,1),10))/2.
       ypo1=(v(i,np(i,1),9)+v(i,np(i,1),10))/2.
       j=jcvi(i+1)
       xpo2=(u(i,j,9)+u(i,j,10))/2.
       ypo2=(v(i,j,9)+v(i,j,10))/2.

c      Evita divisions per zero!!!
       if (xpo2-xpo1.ne.0.) then
       alpha=datan((ypo2-ypo1)/(xpo2-xpo1))
       end if
       if (abs(xpo2-xpo1).lt.0.00001) then
       alpha=pi/2.
       end if

       xpo3=xpo1-rib(i+1,61)*dcos(alpha)
       ypo3=ypo1-rib(i+1,61)*dsin(alpha)
       xdesx=xdes*dsin(alpha)
       xdesy=xdes*dcos(alpha)

c      Draw reference points for miniribs in intrados
       call line(psep+xpo1,psey-ypo1+ysaut,psep+xpo3,psey-ypo3+ysaut,5)
       call pointg(psep+xpo1-xdesx,psey-ypo1-xdesy+ysaut,xcir,1)
       call pointg(psep+xpo3-xdesx,psey-ypo3-xdesy+ysaut,xcir,1)

c      Laser cuting
       xadd=2520.*xkf
       if (i37_2005.eq.1) then
       call line(psep+xpo1+xadd,psey-ypo1+ysaut,
     + psep+xpo3+xadd,psey-ypo3+ysaut,5)
       endif
       call point(psep+xpo1+xadd,psey-ypo1+ysaut,1)
       call point(psep+xpo3+xadd,psey-ypo3+ysaut,1)

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case intrados 0 cuts
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (lowcuts(ng).eq.0) then

c      Intrados panel from LE to TE

       npi=np(i,2)+np(i,3)-1
       npf=np(i,1)
       npo=npf-npi+1

       do j=1,npo
       uf(i,j,9)=ufi(i,j,9)
       uf(i,j,10)=ufi(i,j,10)
       uf(i,j,11)=ufi(i,j,11)
       uf(i,j,12)=ufi(i,j,12)
       vf(i,j,9)=vfi(i,j,9)
       vf(i,j,10)=vfi(i,j,10)
       vf(i,j,11)=vfi(i,j,11)
       vf(i,j,12)=vfi(i,j,12)
       end do

c      Set panel side lengths part 1
       call llarlr(i,2,1,npo,uf,vf,llarl,llarr)

c      Roman marks
       x1=uf(i,npo,10)
       y1=vf(i,npo,10)
       x2=uf(i,npo,9)
       y2=vf(i,npo,9)
       y3=1.0d0
       call romanop(i,-1,x1,-y1,x2,-y2,y3,psep,psey,xkf)

c      Print equidistant points
       xinil=0.0d0
       xinir=0.0d0
       call xmarksi(i,2,1,npo,uf,vf,rib,xinil,xinir,
     + xfinl,xfinr,xmark,llarl,llarr,psep,psey,xcir,xdes,xkf)

c      Print intrados marks (experimental)
       xinil=0.0d0
       xinir=0.0d0
       call iam(i,2,1,np,npo,uf,vf,rib,xinil,xinir,
     + xfinl,xfinr,xmark,llarl,llarr,psep,psey,xcir,xdes,xkf,
     + typm1,typm2,typm3,typm4,typm5,typm6,xrib)


c      Print intrados without vents
       if (k26d.eq.0.or.rib(i+1,165).ge.0) then
       call extpoints(i,uf,vf,npo,xlow,xlowte,xlowle,1)
       call dpanelcc(i,uf,vf,npo,psep,psey,3)
       call dpanelcc(i,uf,vf,npo,psep+2520.*xkf,psey,7)
       end if

c      Print intrados with vents
       if (k26d.eq.1.and.rib(i+1,165).lt.0) then
       call extpoints(i,uf,vf,npo,xlow,xlowte,xlowle,1)
       call dpanelcc(i,uf,vf,npo,psep,psey,4)
       call dpanelcc(i,uf,vf,npo,psep+2520.*xkf,psey,8)

c      Print vents

c      Print intrados marks (experimental)
       xinil=llarl(i,2,1)
       xinir=llarr(i,2,1)
       call iam(i,2,4,np,np(i,3),ufv,vfv,rib,xinil,xinir,
     + xfinl,xfinr,xmark,llarl,llarr,psep,psey,xcir,xdes,xkf,
     + typm1,typm2,typm3,typm4,typm5,typm6,xrib)

c      Set panel side lengths part 4
       call llarlr(i,2,4,np(i,3),ufv,vfv,llarl,llarr)

       call drwvent(i,np,uf,vf,ufv,vfv,psep,psey,xlow,xlowle,
     + xlowte,xkf,1,ic2,csi)   ! case print
       call drwvent(i,np,uf,vf,ufv,vfv,psep+2520.*xkf,psey,xlow,xlowle,
     + xlowte,xkf,2,ic2,csi)   ! case laser

       end if

c      Think why???????????????????
c      Si no els punts queden desplaçats a l'extrados!!!
       ysaut=-ysautt

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case intrados 1 cut
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (lowcuts(ng).eq.1) then


c      Intrados panel from first cut to TE
       ysaut=-ysautt

       npi=ilow(1,3,ng)-(np(i,2)+np(i,3)-2)
       npf=np(i,1)-(np(i,2)+np(i,3)-2)
       npo=npf-npi+1

       do j=1,npo
       uf(i,j,9)=ufi(i,j+npi-1,9)
       uf(i,j,10)=ufi(i,j+npi-1,10)
       uf(i,j,11)=ufi(i,j+npi-1,11)
       uf(i,j,12)=ufi(i,j+npi-1,12)
       vf(i,j,9)=vfi(i,j+npi-1,9)
       vf(i,j,10)=vfi(i,j+npi-1,10)
       vf(i,j,11)=vfi(i,j+npi-1,11)
       vf(i,j,12)=vfi(i,j+npi-1,12)
       end do

c      Set panel side lengths part 1
       call llarlr(i,2,1,npo,uf,vf,llarl,llarr)

c      Roman marks
       x1=uf(i,npo,10)
       y1=vf(i,npo,10)
       x2=uf(i,npo,9)
       y2=vf(i,npo,9)
       y3=1.0d0
       call romanop(i,-1,x1,-y1,x2,-y2,y3,psep,psey+ysaut,xkf)

c      Print equidistant points
       xinil=0.0d0
       xinir=0.0d0
       call xmarksi(i,2,1,npo,uf,vf,rib,xinil,xinir,
     + xfinl,xfinr,xmark,llarl,llarr,psep,psey+ysaut,xcir,xdes,xkf)



c      Print intrados marks (experimental)
       xinil=0.0d0
       xinir=0.0d0
       call iam(i,2,1,np,npo,uf,vf,rib,xinil,xinir,
     + xfinl,xfinr,xmark,llarl,llarr,psep,psey+ysaut,xcir,xdes,xkf,
     + typm1,typm2,typm3,typm4,typm5,typm6,xrib)



c       if (k26d.eq.0.or.rib(i+1,165).ge.0) then
       call extpoints(i,uf,vf,npo,xlow,xlowte,xlow,1)
       call dpanelcc(i,uf,vf,npo,psep,psey+ysaut,4)
       call dpanelcc(i,uf,vf,npo,psep+2520.*xkf,psey+ysaut,8)
       call arc3p(i,1,uf,vf,zinf(i+1,6,4),-1*is4,psep,psey+ysaut,1,xlow)
       call arc3p(i,1,uf,vf,zinf(i+1,6,4),-1*is4,psep+2520.*xkf,psey+
     + ysaut,2,xlow)
c       end if

c      Extrados panel from LE to first cut
       npi=np(i,2)+np(i,3)-1
       npf=ilow(1,3,ng)
       npo=npf-npi+1

       do j=1,npo
       uf(i,j,9)=ufi(i,j,9)
       uf(i,j,10)=ufi(i,j,10)
       uf(i,j,11)=ufi(i,j,11)
       uf(i,j,12)=ufi(i,j,12)
       vf(i,j,9)=vfi(i,j,9)
       vf(i,j,10)=vfi(i,j,10)
       vf(i,j,11)=vfi(i,j,11)
       vf(i,j,12)=vfi(i,j,12)
       end do

c      Set panel side lengths part 2
       call llarlr(i,2,2,npo,uf,vf,llarl,llarr)

c      Print equidistant points
       xinil=xfinl(i,2,1)
       xinir=xfinr(i,2,1)
       call xmarksi(i,2,2,npo,uf,vf,rib,xinil,xinir,
     + xfinl,xfinr,xmark,llarl,llarr,psep,psey,xcir,xdes,xkf)

c      Print intrados marks (experimental)
       xinil=llarl(i,2,1)
       xinir=llarr(i,2,1)
       call iam(i,2,2,np,npo,uf,vf,rib,xinil,xinir,
     + xfinl,xfinr,xmark,llarl,llarr,psep,psey,xcir,xdes,xkf,
     + typm1,typm2,typm3,typm4,typm5,typm6,xrib)


c      Print without vents
       if (k26d.eq.0.or.rib(i+1,165).ge.0) then
       call extpoints(i,uf,vf,npo,xlow,xlow,xlowle,1)
       call dpanelcc(i,uf,vf,npo,psep,psey,2)
       call dpanelcc(i,uf,vf,npo,psep+2520.*xkf,psey,6)
       call arc3p(i,npo,uf,vf,zinf(i+1,6,4),1*is4,psep,psey,1,xlow)
       call arc3p(i,npo,uf,vf,zinf(i+1,6,4),1*is4,psep+2520.*xkf,psey,2,
     + xlow)
       end if

c      CALL ROMANO ARC EXPERIMENTAL
       call romanoparc(i,-1,npo,uf,vf,zinf(i+1,6,4),1,psep,psey,
     + 1,xkf)

c      Print with vents
       if (k26d.eq.1.and.rib(i+1,165).lt.0) then
       call extpoints(i,uf,vf,npo,xlow,xlow,xlowle,1)
       call dpanelcc(i,uf,vf,npo,psep,psey,1)
       call dpanelcc(i,uf,vf,npo,psep+2520.*xkf,psey,5)
       call arc3p(i,npo,uf,vf,zinf(i+1,6,4),1*is4,psep,psey,1,xlow)
       call arc3p(i,npo,uf,vf,zinf(i+1,6,4),1*is4,psep+2520.*xkf,psey,2,
     + xlow)

c      Set panel side lengths part 4
       call llarlr(i,2,4,np(i,3),ufv,vfv,llarl,llarr)

c      Print vents
       call drwvent(i,np,uf,vf,ufv,vfv,psep,psey,xlow,xlowle,
     + xlowte,xkf,1,ic2,csi)   ! case print
       call drwvent(i,np,uf,vf,ufv,vfv,psep+2520.*xkf,psey,xlow,xlowle,
     + xlowte,xkf,2,ic2,csi)   ! case laser

c      Print intrados marks (experimental)
       xinil=llarl(i,2,1)+llarl(i,2,2)
       xinir=llarr(i,2,1)+llarr(i,2,2)
       call iam(i,2,4,np,np(i,3),ufv,vfv,rib,xinil,xinir,
     + xfinl,xfinr,xmark,llarl,llarr,psep,psey,xcir,xdes,xkf,
     + typm1,typm2,typm3,typm4,typm5,typm6,xrib)

       end if

       end if ! lowcuts=1

       end do ! i

       end if ! "ss"

       end if ! k29d=1

c      Print llarl and llarl. Use in section 11.4 for print equidistant marks
c       write (*,*) "EXTRADOS"
c       do i=1,nribss
c       write (*,*) "part 1 ",i,llarl(i,1,1),llarr(i,1,1)
c       write (*,*) "part 2 ",i,llarl(i,1,2),llarr(i,1,2)
c       write (*,*) "part 3 ",i,llarl(i,1,3),llarr(i,1,3)
c       end do
c       write (*,*) "INTRADOS"
c       do i=1,nribss
c       write (*,*) "part 1 ",i,llarl(i,2,1),llarr(i,2,1)
c       write (*,*) "part 2 ",i,llarl(i,2,2),llarr(i,2,2)
c       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
