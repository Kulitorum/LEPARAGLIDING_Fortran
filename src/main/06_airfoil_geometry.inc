!> Load airfoils and construct the wing's profile and spatial coordinates.
!!
!! Populates normalized/scaled `u/v` slots and absolute `x/y/z` skin points,
!! applies wash-in and rib rotations, and prepares junction, rod, and internal
!! geometry consumed by panel development, suspension, and 3D output stages.
c      6. AIRFOILS coordinates calculus
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read airfoils
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=1,nribss

c      rewind 23
       open (24, file=nomair(i))
       rewind (24)

       k=len_trim(nomair(i))
       nomairext(i)=nomair(i)(k-2:k)

c       write (*,*) i,nomair(i),k,nomairext(i),"OK"

c      Case .txt airfoil

       if (nomairext(i).eq."txt") then

c      6.1 Read global points
       read (24,*) 
       read (24,*) np(i,1) ! total points
       read (24,*) np(i,2) ! extrados points
       read (24,*) np(i,3) ! intake points
       read (24,*) np(i,4) ! intrados points

c      Reject oversized legacy .txt profiles before their coordinates can
c      index the fixed-capacity arrays. The .dat reader enforces the same bound.
       if (np(i,1).lt.2.or.np(i,1).gt.ubound(u,2)) then
       write (*,*) "ERROR: profile has",np(i,1),"points; capacity is",
     + ubound(u,2)
       write (50,*) "ERROR: profile has",np(i,1),"points; capacity is",
     + ubound(u,2)
       stop 1
       end if

c      6.2 Read airfoil coordinates
       do j=1,np(i,1)
       read (24,*) u(i,j,1),v(i,j,1)
       v(i,j,1)=v(i,j,1)*rib(i,160)
       end do

       end if ! case .txt

c      Case .dat airfoil

       if (nomairext(i).eq."dat") then
c      Go to reformat airfoil (u,v) and np
       call datair(i,rib,np,u,v)

       end if

       end do ! i

c      Complete topology shared by .txt and .dat profiles.  datair now derives
c      all five values from its rebuilt contour; repeating the shared-endpoint
c      identity here keeps both input paths on one authoritative rule.
c      Column 5 is the shared intake/intrados endpoint.
c
c      Locate the leading-edge sample for both .txt and .dat profiles.
c      Real profile files do not always contain an exact (0,0) point.
       do i=1,nribss
       np(i,5)=np(i,2)+np(i,3)-1
       np(i,6)=1
       nose_distance_sq=huge(0.0d0)
       do j=1,np(i,1)
       if (u(i,j,1)*u(i,j,1)+v(i,j,1)*v(i,j,1).lt.
     + nose_distance_sq) then
       np(i,6)=j
       nose_distance_sq=u(i,j,1)*u(i,j,1)+v(i,j,1)*v(i,j,1)
       endif
       end do
       end do

c      Virtual profiles are assigned only after every source profile is read.
c      This keeps mixed .txt/.dat inputs from copying uninitialized counts.
       do j=1,6
       np(0,j)=np(1,j)
       np(nribss+1,j)=np(nribss-1,j)
       end do
       do j=1,np(nribss+1,1)
       u(nribss+1,j,1)=u(nribss-1,j,1)
       v(nribss+1,j,1)=v(nribss-1,j,1)
       end do

c      Freeze the authoritative stage-6 topology as named snapshots.  This
c      validates every physical and generated profile before stage 7 uses its
c      named surface ranges; both input paths have already enforced capacity.
       allocate(wing_profile_topologies(0:nribss+1))
       do i=0,nribss+1
       call copy_legacy_profile_topology(np,i,wing_profile_topologies(i),
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)') 'ERROR: invalid profile topology at rib ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       end do

c      Complete the two generated definitions from named authored sources.
c      Their Stage-4 legacy rows are deliberately not adapter inputs: row 0
c      and nribss+1 never received every late-scaled column.  The symmetry
c      constructor mirrors the complete row-1 definition with explicit
c      provenance.  The tip constructor takes profile fields from nribss-1
c      and extrapolates station/height through physical wingtip nribss.
       if (.not.allocated(wing_rib_definitions).or.
     + .not.allocated(wing_rib_definition_available)) then
       write (*,'(A)')
     + 'ERROR: Stage-6 rib-definition collection is unavailable'
       stop 1
       end if
       call build_symmetry_rib_definition(wing_rib_definitions(1),
     + wing_rib_identities(0),wing_rib_definitions(0),
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: symmetry definition failed: ',
     + trim(domain_model_message)
       stop 1
       end if
       wing_rib_definition_available(0)=.true.
       call build_tip_support_rib_definition(
     + wing_rib_definitions(nribss-1),wing_rib_definitions(nribss),
     + wing_rib_identities(nribss+1),wing_rib_definitions(nribss+1),
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: tip-support definition failed: ',
     + trim(domain_model_message)
       stop 1
       end if
       wing_rib_definition_available(nribss+1)=.true.
       if (any(.not.wing_rib_definition_available)) then
       write (*,'(A)')
     + 'ERROR: Stage-6 rib-definition collection remains incomplete'
       stop 1
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Detect cut points if defined by points (case 1)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Do nothing

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Detect cut points if defined by % along contour from TE (case 2)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (k29dd.eq.2) then

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Detect cut points if defined by % (case 3)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c       if (k29dd.eq.3) then

       do i=1,nribss
       ng=int(rib(i,169))
c      Preserve the historical mapping for real groups without allowing an
c      incomplete group assignment to index below iupp's one-based dimension.
       if (ng.ge.lbound(iupp,3).and.ng.le.ubound(iupp,3)) then
       do j=1,np(i,1)-1

c       write (*,*) xiupp(1,2,ng),u(i,j,1)
c       if (u(i,j,1).lt.xiupp(1,2,ng).and.u(i,j+1,1).ge.xiupp(1,2,ng)) then
       iupp(1,2,ng)=j
c       end if
c       if (u(i,j,1).gt.xiupp(1,3,ng).and.u(i,j+1,1).ge.xiupp(1,3,ng)) then
c       iupp(1,3,ng)=j
c       end if
c       if (u(i,j,1).gt.xiupp(2,2,ng).and.u(i,j+1,1).ge.xiupp(2,2,ng)) then
c       iupp(2,2,ng)=j
c       end if
c       if (u(i,j,1).gt.xiupp(2,3,ng).and.u(i,j+1,1).ge.xiupp(2,3,ng)) then
c       iupp(2,3,ng)=j
c       end if
c      Think intrados/extrdos cases

c       if (u(i,j,1).lt.xilow(1,2,ng).and.u(i,j+1,1).ge.xilow(1,2,ng)) then
c       ilow(1,2,ng)=j
c       end if
c       if (u(i,j,1).lt.xilow(1,3,ng).and.u(i,j+1,1).ge.xilow(1,3,ng)) then
c       ilow(1,3,ng)=j
c       end if

       end do
       end if
       end do

c       end if ! k29dd=3


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Airfoil coordinates modifications
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=1,nribss+1

       tetha=rib(i,8)*pi/180.0d0
       rot_z=rib(i,250)*pi/180.0d0
       pos=rib(i,5)*rib(i,251)/100.0d0

       do j=1,np(i,1)

c      6.3 Airfoil coordinates *100
       u(i,j,2)=100.*u(i,j,1)
       v(i,j,2)=100.*v(i,j,1)

c      6.4 Airfoil escaled and displaced coordinates
       u(i,j,3)=rib(i,5)*u(i,j,1)
       v(i,j,3)=rib(i,5)*v(i,j,1)-rib(i,50)

c      6.5 Airfoil washin coordinates, rotation in X-axis
       u(i,j,4)=(u(i,j,3)-(rib(i,10)/100.)*rib(i,5))*dcos(tetha)+
     + (v(i,j,3))*dsin(tetha)+(rib(i,10)/100.)*rib(i,5)
       v(i,j,4)=(-u(i,j,3)+(rib(i,10)/100.)*rib(i,5))*dsin(tetha)+
     + (v(i,j,3))*dcos(tetha)

c      6.5+ Airfoil rotation in Z by pos

       wnew(j)=-u(i,j,4)*dsin(rot_z)+pos*dsin(rot_z)
       unew(j)=u(i,j,4)*dcos(rot_z)+pos*(1-dcos(rot_z))
       vnew(j)=v(i,j,4)

       u(i,j,4)=unew(j)
       v(i,j,4)=vnew(j)
       w(i,j,4)=wnew(j)

c      6.6 Airfoil (u,v,w) espace coordinates
c       u(i,j,5)=u(i,j,4)
c       v(i,j,5)=v(i,j,4)*dcos(rib(i,9)*pi/180.)
c       w(i,j,5)=-v(i,j,4)*dsin(rib(i,9)*pi/180.)

c      6.6 Airfoils rotation in Y-axis
       w(i,j,5)=-w(i,j,4)*dcos(rib(i,9)*pi/180.)-
     + v(i,j,4)*dsin(rib(i,9)*pi/180.)
       u(i,j,5)=u(i,j,4)
       v(i,j,5)=-w(i,j,4)*dsin(rib(i,9)*pi/180.)+
     + v(i,j,4)*dcos(rib(i,9)*pi/180.)

c      6.7 Airfoil (x,y,z) absolute coordinates
       x(i,j)=rib(i,6)-w(i,j,5)
       y(i,j)=rib(i,3)+u(i,j,5)
       z(i,j)=rib(i,7)-v(i,j,5)

c      6.8 Tornar a posar la costella al seu lloc
       v(i,j,3)=v(i,j,3)+rib(i,50)

c      6.5,6.6,6.7 Call to subroutine xyzt
c       u_aux(i,j,1)=u(i,j,3)
c       v_aux(i,j,1)=v(i,j,3)-rib(i,50)
c       w_aux(i,j,1)=0.0d0
c       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
c       x(i,j)=w_aux(i,j,5)
c       y(i,j)=u_aux(i,j,5)
c       z(i,j)=v_aux(i,j,5)

c      6.8 Tornar a posar la costella al seu lloc
c      WARNING! Això no ho veig clar
c      Fer experiment per a verificar. És necessari?
c       v(i,j,3)=v(i,j,3)+rib(i,50)

       end do ! j

c      Additional point in airfoil plane (use in 10.1+)
       j=1
       u_aux(i,j,1)=(u(i,1,3)+u(i,np(i,6),3))*0.5d0
       v_aux(i,j,1)=50.0d0
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       x_apap(i)=w_aux(i,j,5)
       y_apap(i)=u_aux(i,j,5)
       z_apap(i)=v_aux(i,j,5)

       end do ! i

       np(0,6)=np(1,6)

c      6.9 Airfoil 0 = 1' (symetrical to airfoil 1) BUT NON DISPLACED COORD

       do j=1,np(1,1)

       u(0,j,1)=u(1,j,1)
c      v(0,j,1)=u(1,j,1) ERROR
       v(0,j,1)=v(1,j,1)

c      Airfoil coordinates *100
       u(0,j,2)=u(1,j,2)
       v(0,j,2)=v(1,j,2)

c      Airfoil escaled coordinates
       u(0,j,3)=u(1,j,3)
       v(0,j,3)=v(1,j,3)

c      Airfoil washin coordinates
       u(0,j,4)=u(1,j,4)
       v(0,j,4)=v(1,j,4)

c      Airfoil (u,v,w) espace coordinates
       u(0,j,5)=-u(1,j,5)
       v(0,j,5)=v(1,j,5)
       w(0,j,5)=w(1,j,5)

c      Airfoil (x,y,z) absolute coordinates
       x(0,j)=-x(1,j)
       y(0,j)=y(1,j)
       z(0,j)=z(1,j)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Tornar a posar la costella al seu lloc
c      WARNING WARNIG
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c       v(0,j,3)=u(1,j,3)+rib(1,50)  ERROR
       v(0,j,3)=v(1,j,3)

       end do

c      Dual-run complete-profile construction after all physical rows, the
c      extrapolated tip support, and explicit mirrored row 0 exist in legacy
c      X/Y/Z.  Normalized input is selected by each definition's named source
c      provenance, never by assuming target row equals profile row.
c
c      Physical and tip rows use the exact Stage-6 transform kernel.  Row 0 is
c      constructed last by mirroring the final typed row-1 geometry, matching
c      the historical post-transform operation rather than transforming its
c      incomplete compatibility row.  Every coordinate must agree before the
c      checked adapter may publish typed X/Y/Z authority only when the entire
c      wing is bit-exact.  A merely tolerance-close value is not published:
c      downstream legacy geometry contains branch-sensitive comparisons that
c      can amplify a last-bit change.  Slots u/v/w(:,3:5) remain untouched for
c      their current downstream consumers.
       allocate(typed_spatial_rib_candidates(0:nribss+1))
       typed_spatial_geometry_exact=.true.
       do i=1,nribss+1
       typed_profile_source_index=
     + wing_rib_definitions(i)%source_profile_number
       call copy_legacy_normalized_profile(u,v,typed_profile_source_index,
     + wing_profile_topologies(typed_profile_source_index),
     + typed_normalized_profile,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,A,I0,2A)')
     + 'ERROR: normalized profile source ',typed_profile_source_index,
     + ' for spatial rib ',i,': ',trim(domain_model_message)
       stop 1
       end if
       if (i.eq.nribss+1) then
       call build_tip_support_spatial_rib(typed_normalized_profile,
     + wing_rib_definitions(i),typed_spatial_rib_candidates(i),
     + domain_model_valid,
     + domain_model_message)
       else
       call build_spatial_rib_geometry(typed_normalized_profile,
     + wing_rib_definitions(i),typed_spatial_rib_candidates(i),
     + domain_model_valid,domain_model_message)
       end if
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)') 'ERROR: typed spatial rib ',i,': ',
     + trim(domain_model_message)
       stop 1
       end if
       do j=1,wing_profile_topologies(i)%point_count
       typed_coordinates_match=
     + geometry_values_are_close(typed_spatial_rib_candidates(i)%x(j),
     + x(i,j)).and.
     + geometry_values_are_close(typed_spatial_rib_candidates(i)%y(j),
     + y(i,j)).and.
     + geometry_values_are_close(typed_spatial_rib_candidates(i)%z(j),
     + z(i,j))
       if (.not.typed_coordinates_match) then
       write (*,'(A,I0,A,I0,A,6(1X,ES24.16))')
     + 'ERROR: typed/legacy spatial mismatch at rib ',i,', point ',j,
     + ':',typed_spatial_rib_candidates(i)%x(j),x(i,j),
     + typed_spatial_rib_candidates(i)%y(j),y(i,j),
     + typed_spatial_rib_candidates(i)%z(j),z(i,j)
       stop 1
       end if
       if (any(abs([typed_spatial_rib_candidates(i)%x(j)-x(i,j),
     + typed_spatial_rib_candidates(i)%y(j)-y(i,j),
     + typed_spatial_rib_candidates(i)%z(j)-z(i,j)])
     + .gt.0.0d0)) typed_spatial_geometry_exact=.false.
       end do
       end do

       call build_symmetry_spatial_rib(typed_spatial_rib_candidates(1),
     + wing_rib_definitions(0),typed_spatial_rib_candidates(0),
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: typed symmetry geometry failed: ',
     + trim(domain_model_message)
       stop 1
       end if
       do j=1,wing_profile_topologies(0)%point_count
       typed_coordinates_match=
     + geometry_values_are_close(typed_spatial_rib_candidates(0)%x(j),
     + x(0,j)).and.
     + geometry_values_are_close(typed_spatial_rib_candidates(0)%y(j),
     + y(0,j)).and.
     + geometry_values_are_close(typed_spatial_rib_candidates(0)%z(j),
     + z(0,j))
       if (.not.typed_coordinates_match) then
       write (*,'(A,I0,A,6(1X,ES24.16))')
     + 'ERROR: typed/legacy symmetry mismatch at point ',j,':',
     + typed_spatial_rib_candidates(0)%x(j),x(0,j),
     + typed_spatial_rib_candidates(0)%y(j),y(0,j),
     + typed_spatial_rib_candidates(0)%z(j),z(0,j)
       stop 1
       end if
       if (any(abs([typed_spatial_rib_candidates(0)%x(j)-x(0,j),
     + typed_spatial_rib_candidates(0)%y(j)-y(0,j),
     + typed_spatial_rib_candidates(0)%z(j)-z(0,j)])
     + .gt.0.0d0)) typed_spatial_geometry_exact=.false.
       end do

c      Publish all rows or none: partial typed authority would leave adjacent
c      panels in mixed numerical domains.  Slot-1 profile fractions preserve
c      the legacy multiplication path; the retained fallback still prevents a
c      non-exact candidate from becoming authoritative on an unproved input.
       allocate(wing_spatial_ribs(0:nribss+1))
       if (typed_spatial_geometry_exact) then
       wing_spatial_ribs=typed_spatial_rib_candidates
       do i=0,nribss+1
       call write_legacy_spatial_rib_geometry(wing_spatial_ribs(i),x,y,z,
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)') 'ERROR: spatial publication failed at rib ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       end do
       else
c      Stage 7 already consumes wing_spatial_ribs.  Preserve its exact legacy
c      authority whenever the independently constructed candidate differs by
c      even one bit; tolerance-close candidates must never leak downstream.
       do i=0,nribss+1
       call copy_legacy_spatial_rib(x,y,z,i,
     + wing_profile_topologies(i)%point_count,wing_spatial_ribs(i),
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: legacy spatial authority copy failed at rib ',i,': ',
     + trim(domain_model_message)
       stop 1
       end if
       end do
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.10+ Compute TE-anchor lengths along airfoil contour
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Column-group ownership at this compatibility producer is offset by the
c      active A--E ordinal klz=1:rib(i,15):
c        65+klz  -> 66:70  chordwise position in the restored rib-local plane
c       110+klz  -> 111:115 interpolated intrados chord coordinate
c       120+klz  -> 121:125 interpolated intrados height coordinate
c       130+klz  -> 131:135 distance from trailing edge along the intrados
c      Columns 110, 116:120, and 126:130 are not members of these arrays.  F
c      (brake, Section-3 column 21) is intentionally outside this A--E contour
c      distance producer.  The typed anchor module independently reconstructs
c      and compares every active value after Stage 12 resolves spatial points.

       do i=0,nribss

       do klz=1,rib(i,15) ! Iterate in A,B,C,D,E

       do j=np(i,1),2,-1

c      Detect segment j-1,j where anchor A,B,C,D,E is and interpolate
c      point u,v = rib(i,110+klz) rib(i,120+klz)

       if (u(i,j-1,3).le.rib(i,65+klz).and.u(i,j,3).gt.rib(i,65+klz).
     + and.u(i,j,3).ge.0) then

c      Interpolation v=xm * u + xb
       rib(i,110+klz)=rib(i,65+klz)
       xm=(v(i,j,3)-v(i,j-1,3))/(u(i,j,3)-u(i,j-1,3))
       xb=v(i,j-1,3)-xm*u(i,j-1,3)
       rib(i,120+klz)=xm*rib(i,110+klz)+xb

c      Compute distances along bottom surface

       rib(i,130+klz)=0.
       do jj=np(i,1),j+1,-1
       rib(i,130+klz)=rib(i,130+klz)+sqrt((u(i,jj,3)-u(i,jj-1,3))**2+
     + (v(i,jj,3)-v(i,jj-1,3))**2)
       end do
       rib(i,130+klz)=rib(i,130+klz)+sqrt((u(i,j,3)-rib(i,110+klz))**2+
     + (v(i,j,3)-rib(i,120+klz))**2)

c      Verificar amb CAD!!!

       end if

       end do ! j

c       write (*,*) i,klz,rib(i,131)

       end do ! klz

       end do ! rib i

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.11 Compute external cutt edges in airfoils (i,j,16)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      REVISAR LA PREOGRAMACIO. SIMPLIFICAR

       xcos=xrib/10. ! rib sewing allowance mm to cm

       do i=0,nribss

c      Also used for the two endpoints when there are no interior points
       xcosk=1.0d0
      
       do j=2,np(i,1)-1

c      Amplification factor
       xcosk=1.0

c      Fer mitja entre j-1 i j+1
       alpha1=(datan((v(i,j+1,3)-v(i,j,3))/((u(i,j+1,3)-u(i,j,3)))))
       alpha2=(datan((v(i,j,3)-v(i,j-1,3))/((u(i,j,3)-u(i,j-1,3)))))

       alpha=0.5*(alpha1+alpha2)

c      Alpha correction in sawtooht mono-surface airfoils
c      Dóna la volta a la vora superior

       if (alpha1.lt.0.and.alpha2.gt.0.and.j.ge.np(i,2)) then
c       alpha=alpha+pi
c       write (*,*) "correccio"
       end if     

       if (i.eq.1) then
c       write (*,*) "AQUI ",i,j,alpha1,alpha2,alpha
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Correcció xcosk a vores dent de serra
       if (atp.eq."ss".and.j.ge.np(i,2)+np(i,3)-1) then
       xcosk=1./(dsin(0.5*(pi+alpha1-alpha2))) 
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       u(i,j,16)=u(i,j,3)-xcos*xcosk*dsin(alpha)

       if(v(i,j,3).ge.0.) then
       v(i,j,16)=v(i,j,3)+xcos*xcosk*dcos(alpha)
       end if

       if(v(i,j,3).ge.0.and.j.ge.np(i,2)) then
       u(i,j,16)=u(i,j,3)+xcos*xcosk*dsin(alpha)
       v(i,j,16)=v(i,j,3)-xcos*xcosk*dcos(alpha)
       end if

       if(v(i,j,3).lt.0.) then
       u(i,j,16)=u(i,j,3)+xcos*xcosk*dsin(alpha)
       v(i,j,16)=v(i,j,3)-xcos*xcosk*dcos(alpha)
       end if

       if(u(i,j,3).eq.0) then
       u(i,j,16)=u(i,j,3)-xcos*xcosk
       v(i,j,16)=v(i,j,3)
       end if

       end do

       j=1

       alpha=(datan((v(i,j+1,3)-v(i,j,3))/((u(i,j+1,3)-u(i,j,3)))))

       u(i,j,16)=u(i,j,3)-xcos*xcosk*dsin(alpha)

       if(v(i,j,3).ge.0.) then
       v(i,j,16)=v(i,j,3)+xcos*xcosk*dcos(alpha)
       end if

       if(v(i,j,3).lt.0.) then
       u(i,j,16)=u(i,j,3)+xcos*xcosk*dsin(alpha)
       v(i,j,16)=v(i,j,3)-xcos*xcosk*dcos(alpha)

       end if

       j=np(i,1)

       alpha=(datan((v(i,j,3)-v(i,j-1,3))/((u(i,j,3)-u(i,j-1,3)))))

       u(i,j,16)=u(i,j,3)-xcos*xcosk*dsin(alpha)

       if(v(i,j,3).ge.0.) then
       v(i,j,16)=v(i,j,3)+xcos*xcosk*dcos(alpha)
       end if

       if(v(i,j,3).le.0.) then
       u(i,j,16)=u(i,j,3)+xcos*xcosk*dsin(alpha)
       v(i,j,16)=v(i,j,3)-xcos*xcosk*dcos(alpha)
       end if

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.11+ Airfoils thickness
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Aproximate calculus (not inperpolated)
c      Special airfoils not considered
c      Please improve

       do i=0,nribss

c      A degenerate profile has zero thickness unless extrema are found
       xth1=0.0d0
       xth2=0.0d0

c      Extrados thickness
       do j=1,np(i,1)-1
       if (v(i,j+1,3).ge.v(i,j,3).and.u(i,j+1,3).lt.u(i,j,3)) then
       xth1=v(i,j+1,3)
       end if
c      Intrados thickness
       if (v(i,j+1,3).le.v(i,j,3).and.u(i,j+1,3).gt.u(i,j,3)) then
       xth2=v(i,j+1,3)
       end if
       end do
c      Airfoil thickness
       rib(i,148)=xth1-xth2
       rib(i,149)=100.*rib(i,148)/rib(i,5)

c      write (*,*) "Thicknees= ", i, rib(i,148), rib(i,149)

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.12 Airfoils drawing (complete)
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

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Numering ribs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call itxt(sepx+19.-84.*(typm3(9)/7.),sepy,typm3(9),0.0d0,i,7)
       call itxt(sepx+19.-84.*(typm3(9)/7.),sepy+890.95*xkf,typm3(9),
     + 0.0d0,i,7)
       call itxt(sepx-16.-84.*(typm3(9)/7.)+2530.*xkf,sepy,typm3(9),
     + 0.0d0,i,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     6.12.0 Miniribs 
c     >>> moved to 8.5.0 section (!) using hautok vector 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.12.1 Airfoil extrados
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        do j=1,np(i,2)-1

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Dibuixa airfoils basic contour
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call line(sepx+u(i,j,3),-v(i,j,3)+sepy,sepx+u(i,j+1,3),
     + -v(i,j+1,3)+sepy,1)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Dibuixa airfoils washin basic contour al seu lloc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call line(sepx+u(i,j,4),-v(i,j,4)+sepy+xkf*890.95-rib(i,50),
     + sepx+u(i,j+1,4),-v(i,j+1,4)+sepy+xkf*890.95-rib(i,50),3)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Airfoils borders
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call line(sepx+u(i,j,16),-v(i,j,16)+sepy,sepx+u(i,j+1,16),
     + -v(i,j+1,16)+sepy,3)

       call line(sepx+u(i,j,16),-v(i,j,16)+sepy,sepx+u(i,j,3),
     + -v(i,j,3)+sepy,5)

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.12.2 Air inlets
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case "pc"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (atp.eq."pc") then

       j=np(i,2)
       k=np(i,2)+np(i,3)-1

c      Basic contour using straight line
       call line(sepx+u(i,j,3),-v(i,j,3)+sepy,sepx+u(i,k,3),
     + -v(i,k,3)+sepy,1)

c      Washin basic contour using straight line
       call line(sepx+u(i,j,4),-v(i,j,4)+sepy+xkf*890.85-rib(i,50),
     + sepx+u(i,k,4),-v(i,k,4)+sepy+xkf*890.85-rib(i,50),3)
 
c      Airfoil borders
       
       xdv=v(i,j,3)-v(i,k,3)
       xdu=u(i,k,3)-u(i,j,3)
       alpha=datan(xdv/xdu)
       if (xdv.eq.0) then
       alpha=0.
       end if

       u(i,j+2,16)=u(i,j,3)-xrib*0.1*dsin(alpha)
       v(i,j+2,16)=v(i,j,3)-xrib*0.1*dcos(alpha)
       u(i,k-2,16)=u(i,k,3)-xrib*0.1*dsin(alpha)
       v(i,k-2,16)=v(i,k,3)-xrib*0.1*dcos(alpha)

       if (rib(i,149).ne.0.) then
       call line(sepx+u(i,j+2,16),-v(i,j+2,16)+sepy,sepx+u(i,k-2,16),
     + -v(i,k-2,16)+sepy,3)
       end if

c      Two lines intersection

c      up nose
       
       xru(1)=u(i,j,16)
       xru(2)=u(i,j-1,16)
       xrv(1)=v(i,j,16)
       xrv(2)=v(i,j-1,16)
       xsu(1)=u(i,j+2,16)
       xsu(2)=u(i,k-2,16)
       xsv(1)=v(i,j+2,16)
       xsv(2)=v(i,k-2,16)

       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)

       u(i,j+1,16)=xtu
       v(i,j+1,16)=xtv

       if (rib(i,149).ne.0.) then
       call line(sepx+u(i,j+1,16),-v(i,j+1,16)+sepy,sepx+u(i,j,16),
     + -v(i,j,16)+sepy,3)
       call line(sepx+u(i,j+1,16),-v(i,j+1,16)+sepy,sepx+u(i,j+2,16),
     + -v(i,j+2,16)+sepy,3)
       end if

c      bottom nose
       
       xru(1)=u(i,k,16)
       xru(2)=u(i,k+1,16)
       xrv(1)=v(i,k,16)
       xrv(2)=v(i,k+1,16)
       xsu(1)=u(i,j+2,16)
       xsu(2)=u(i,k-2,16)
       xsv(1)=v(i,j+2,16)
       xsv(2)=v(i,k-2,16)
      
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)

       u(i,k-1,16)=xtu
       v(i,k-1,16)=xtv

c      Print segments if not zero thickness
       if (rib(i,149).ne.0.) then
       call line(sepx+u(i,k-1,16),-v(i,k-1,16)+sepy,sepx+u(i,k-2,16),
     + -v(i,k-2,16)+sepy,3)
       call line(sepx+u(i,k-1,16),-v(i,k-1,16)+sepy,sepx+u(i,k,16),
     + -v(i,k,16)+sepy,3)
       end if

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C      Case "ds" or "ss"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (atp.eq."ds".or.atp.eq."ss") then

       do j=np(i,2),np(i,2)+np(i,3)-2

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Dibuixa airfoils basic contour
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call line(sepx+u(i,j,3),-v(i,j,3)+sepy,sepx+u(i,j+1,3),
     + -v(i,j+1,3)+sepy,1)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Dibuixa airfoils washin basic contour al seu lloc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call line(sepx+u(i,j,4),-v(i,j,4)+sepy+xkf*890.95-rib(i,50),
     + sepx+u(i,j+1,4),-v(i,j+1,4)+sepy+xkf*890.95-rib(i,50),3)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Airfoils borders
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call line(sepx+u(i,j,16),-v(i,j,16)+sepy,sepx+u(i,j+1,16),
     + -v(i,j+1,16)+sepy,3)

       
       call line(sepx+u(i,j,16),-v(i,j,16)+sepy,sepx+u(i,j,3),
     + -v(i,j,3)+sepy,5)

       end do

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.12.3 Airfoil intrados
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c       do j=1,np(i,1)-1
        do j=np(i,2)+np(i,3)-1,np(i,1)-1

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Dibuixa airfoils basic contour
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call line(sepx+u(i,j,3),-v(i,j,3)+sepy,sepx+u(i,j+1,3),
     + -v(i,j+1,3)+sepy,1)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Dibuixa airfoils washin basic contour al seu lloc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call line(sepx+u(i,j,4),-v(i,j,4)+sepy+xkf*890.85-rib(i,50),
     + sepx+u(i,j+1,4),-v(i,j+1,4)+sepy+xkf*890.85-rib(i,50),3)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Airfoils borders
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call line(sepx+u(i,j,16),-v(i,j,16)+sepy,sepx+u(i,j+1,16),
     + -v(i,j+1,16)+sepy,3)

       
       call line(sepx+u(i,j,16),-v(i,j,16)+sepy,sepx+u(i,j,3),
     + -v(i,j,3)+sepy,5)

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.12.4 Dibuixa punts-segments singulars vores airfoils
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
       j=np(i,1)

       call line(sepx+u(i,j,16),-v(i,j,16)+sepy,sepx+u(i,j,3),
     + -v(i,j,3)+sepy,5)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.12.5 Draw holes
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do l=1,hol(i,1,1)

c      Dibuixa forat tipus 1 (alleugerament elliptics)
       if (hol(i,l,9).eq.1) then

       xx0=(hol(i,l,2))*rib(i,5)/100.0d0+sepx
       yy0=(-(hol(i,l,3))*rib(i,5)/100.0d0+sepy)
       xxa=(hol(i,l,4))*rib(i,5)/100.0d0
       yyb=((hol(i,l,5))*rib(i,5)/100.0d0)

       call ellipse(xx0,yy0,xxa,yyb,(hol(i,l,6)),3)
       end if ! Type 1

c      Dibuixa forat tipus 3 (triangles)

       if (hol(i,l,9).eq.3) then

       alptri=hol(i,l,6)*pi/180.
       atri=hol(i,l,4)*rib(i,5)/100.
       btri=hol(i,l,5)*rib(i,5)/100.
       rtri=hol(i,l,7)*rib(i,5)/100.
       
c      Marcador de costat negatiu
       satri=atri/sqrt(atri*atri)

       atri=abs(atri)

       cgor=0.5*pi-alptri
       ctri=dsqrt(atri*atri+btri*btri-2.*atri*btri*dcos(cgor))
       agor=dacos((ctri*ctri+btri*btri-atri*atri)/(2.*btri*ctri))
       bgor=dacos((ctri*ctri+atri*atri-btri*btri)/(2.*atri*ctri))
       aggor=pi-agor
       bggor=pi-bgor
       cggor=pi-cgor
       h1tri=rtri/dsin(0.5*cgor)
       h2tri=rtri/dsin(0.5*bgor)
       h3tri=rtri/dsin(0.5*agor)

       x1=hol(i,l,2)*rib(i,5)/100.
       y1=hol(i,l,3)*rib(i,5)/100.

       x2=x1+satri*atri*dcos(alptri)
       y2=y1+atri*dsin(alptri)

       x3=x1
       y3=y1+btri

       o1x=x1+satri*(h1tri*dcos(alptri+0.5*cgor))
       o1y=y1+h1tri*dsin(alptri+0.5*cgor)

       o2x=x2-satri*(h2tri*dcos(-alptri+0.5*bgor))
       o2y=y2+h2tri*dsin(-alptri+0.5*bgor)

       o3x=x3+satri*(h3tri*dsin(0.5*agor))
       o3y=y3-h3tri*dcos(0.5*agor)

c      Inicialitza comptatge punts triangle
       k=1

       step1=cggor/6.
       do tetha=0.,cggor+0.01,step1
       xtri(k)=o1x-satri*rtri*dcos(tetha)
       ytri(k)=o1y-rtri*dsin(tetha)
       k=k+1
       end do

       step2=bggor/6.
       do tetha=alptri,alptri+bggor+0.01,step2
       xtri(k)=o2x+satri*rtri*dsin(tetha)
       ytri(k)=o2y-rtri*dcos(tetha)
       k=k+1
       end do

       step3=aggor/6.
       do tetha=agor,pi+0.01,step3
       xtri(k)=o3x+satri*rtri*dcos(tetha)
       ytri(k)=o3y+rtri*dsin(tetha)
       k=k+1
       end do

       xtri(k)=xtri(1)
       ytri(k)=ytri(1)

c      Dibuixa triangles

       if (satri.eq.1.) then
       do k=1,21
       call line(sepx+xtri(k),-ytri(k)+sepy,
     + sepx+xtri(k+1),-ytri(k+1)+sepy,3)
       end do
       end if

       if (satri.eq.-1.) then
       do k=1,21
       call line(sepx+xtri(k),-ytri(k)+sepy,
     + sepx+xtri(k+1),-ytri(k+1)+sepy,3)
       end do
       end if

       end if ! Type 3


c      Draw holes type 4 (rectangles)
c      Request from Pawel Lipski
c      Added 2023-12-07

       if (hol(i,l,9).eq.4) then

       alptri=hol(i,l,6)*pi/180.
       atri=hol(i,l,4)*rib(i,5)/100.
       btri=hol(i,l,5)*rib(i,5)/100.
       rtri=hol(i,l,7)*rib(i,5)/100.
       
c      Flag negative value. -1 if atri negative, 1 atri if potitive
       satri=atri/sqrt(atri*atri)
       atri=abs(atri) ! makes positive by default

c      Rectangle corners
       x1=hol(i,l,2)*rib(i,5)/100.
       y1=hol(i,l,3)*rib(i,5)/100.

       x2=x1+satri*atri*dcos(alptri)
       y2=y1+atri*dsin(alptri)

       x3=x2-satri*btri*dsin(alptri)
       y3=y2+btri*dcos(alptri)

       x4=x1-satri*btri*dsin(alptri)
       y4=y1+btri*dcos(alptri)


c      Circle centers of rounded corners
       o1x=x1+satri*rtri*dcos(alptri)-satri*rtri*dsin(alptri)
       o1y=y1+rtri*dsin(alptri)+rtri*dcos(alptri)

       o2x=x2-satri*rtri*dcos(alptri)-satri*rtri*dsin(alptri)
       o2y=y2-rtri*dsin(alptri)+rtri*dcos(alptri)

       o3x=x3-satri*rtri*dcos(alptri)+satri*rtri*dsin(alptri)
       o3y=y3-rtri*dsin(alptri)-rtri*dcos(alptri)

       o4x=x4+satri*rtri*dcos(alptri)+satri*rtri*dsin(alptri)
       o4y=y4+rtri*dsin(alptri)-rtri*dcos(alptri)


c      Draw segments
       x12=x1+satri*rtri*dcos(alptri)
       y12=y1+rtri*dsin(alptri)
       x21=x2-satri*rtri*dcos(alptri)
       y21=y2-rtri*dsin(alptri)

       x23=x2-satri*rtri*dsin(alptri)
       y23=y2+rtri*dcos(alptri)
       x32=x3+satri*rtri*dsin(alptri)
       y32=y3-rtri*dcos(alptri)

       x34=x3-satri*rtri*dcos(alptri)
       y34=y3-rtri*dsin(alptri)
       x43=x4+satri*rtri*dcos(alptri)
       y43=y4+rtri*dsin(alptri)

       x41=x4+satri*rtri*dsin(alptri)
       y41=y4-rtri*dcos(alptri)
       x14=x1-satri*rtri*dsin(alptri)
       y14=y1+rtri*dcos(alptri)


c      Case satri=1
       if (satri.eq.1) then

        k=1
c       First arc
        do tetha=0.0,pi/2.,pi/12.
        xtri(k)=o1x+rtri*dcos(tetha+pi+alptri)
        ytri(k)=o1y+rtri*dsin(tetha+pi+alptri)
        k=k+1
        end do

c       Second arc
        do tetha=0.0,pi/2.,pi/12.
        xtri(k)=o2x+rtri*dcos(tetha+1.5*pi+alptri)
        ytri(k)=o2y+rtri*dsin(tetha+1.5*pi+alptri)
        k=k+1
        end do

c       Third arc
        do tetha=0.0,pi/2.,pi/12.
        xtri(k)=o3x+rtri*dcos(tetha+alptri)
        ytri(k)=o3y+rtri*dsin(tetha+alptri)
        k=k+1
        end do

c       Fourth arc
        do tetha=0.0,pi/2.,pi/12.
        xtri(k)=o4x+rtri*dcos(tetha+0.5*pi+alptri)
        ytri(k)=o4y+rtri*dsin(tetha+0.5*pi+alptri)
        k=k+1
        end do 
        xtri(k)=xtri(1)
        ytri(k)=ytri(1)

       end if ! satri=1


c      Case satri=-1
       if (satri.eq.-1.) then

        k=1
c       First arc
        do tetha=0.0,pi/2.,pi/12.
        xtri(k)=o1x+rtri*dcos(-tetha-alptri)
        ytri(k)=o1y+rtri*dsin(-tetha-alptri)
        k=k+1
        end do

c       Second arc
        do tetha=0.0,pi/2.,pi/12.
        xtri(k)=o2x+rtri*dcos(-tetha-0.5*pi-alptri)
        ytri(k)=o2y+rtri*dsin(-tetha-0.5*pi-alptri)
        k=k+1
        end do

c       Third arc
        do tetha=0.0,pi/2.,pi/12.
        xtri(k)=o3x+rtri*dcos(-tetha+pi-alptri)
        ytri(k)=o3y+rtri*dsin(-tetha+pi-alptri)
        k=k+1
        end do

c       Fourth arc
        do tetha=0.0,pi/2.,pi/12.
        xtri(k)=o4x+rtri*dcos(-tetha+0.5*pi-alptri)
        ytri(k)=o4y+rtri*dsin(-tetha+0.5*pi-alptri)
        k=k+1
        end do 
        xtri(k)=xtri(1)
        ytri(k)=ytri(1)

       end if ! satri=-1


c      Dibuixa arcs

       if (satri.eq.1.) then
       do k=1,28
       call line(sepx+xtri(k),-ytri(k)+sepy,
     + sepx+xtri(k+1),-ytri(k+1)+sepy,3)
       end do
       end if

       if (satri.eq.-1.) then
       do k=1,28
       call line(sepx+xtri(k),-ytri(k)+sepy,
     + sepx+xtri(k+1),-ytri(k+1)+sepy,3)
       end do
       end if

       end if ! Type 4




       end do  ! holes

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.12.6 Print joncs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      SOLVE INTERFERENCE BETWEEN BLOCS!!!!

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.12.6.1 First) Iterate in blocs type 1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      For each bloc do
       do m=1,k21blocs
c      For each group do
       do ng=1,k21blocf(m,3)

c      Detect rib belonging to group
       if (i.ge.ngoo(m,ng,2).and.i.le.ngoo(m,ng,3)) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case bloc type 1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (k21blocf(m,2).eq.1) then

c       write (*,*) "t1 ",k21blocf(m,1),k21blocf(m,2),i


c       if (rib(i,166).ne.0) then

       do jjk=1,4
       xextra(ng,jjk)=xextraa(m,ng,jjk)
       xintra(ng,jjk)=xintraa(m,ng,jjk)
       sjo(ng,jjk)=sjoo(m,ng,jjk)
       end do

       rib(i,166)=float(ng)

       call joncs(i,u,v,rib,xintra,xextra,xjonc,npo,atp,np)
       call pjoncs_b12(i,xjonc,npo,sjo,sepx,sepy,rib,xkf)
       call pjoncs_b17(i,xjonc,npo,sjo,sepxjo,sepyjo,rib,xkf)

       joncf(i,m,ng,2)=rib(i,167) ! Jonc lenght


c       end if

       end if ! bloc type 1

       end if ! detect rib

       end do ! group n

       end do ! bloc m

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.12.6.2 Second) Iterate in blocs type 2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      For each bloc do
       do m=1,k21blocs
c      For each group do
       do ng=1,k21blocf(m,3)

c      Detect rib belonging to group
       if (i.ge.ngoo(m,ng,2).and.i.le.ngoo(m,ng,3)) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case bloc rods type 2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (k21blocf(m,2).eq.2) then

c       write (*,*) "t2 ",k21blocf(m,1),k21blocf(m,2),i

       do jjk=1,4
       sjo2(ng,jjk)=sjoo(m,ng,jjk)
       end do

       rib(i,166)=float(ng)
       nparc=41
       
c      WARNING! Not afectet by sepxjo, sepyjo parameters !!!!
c      Divide between (1,2) and (1,7) boxes
       call joncs2(i,u,v,rib,x21,xjonc2,nparc,atp,np,m,ng)
       call pjoncs2(i,xjonc2,nparc,sjo2,sepx,sepy,rib,xkf)   

       joncf(i,m,ng,2)=rib(i,167) ! Jonc lenght

c      Print romano points BOX(1,7) -exp-
c       call romanop(i,1,sepx+xjonc2(i,1,1),sepy-xjonc2(i,1,2), ! BOX(1,7)
c     + sepx+xjonc2(i,nparc,1),sepy-xjonc2(i,nparc,2),0,0,0,xkf)

       end if


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case bloc rods type 3  (shark nose)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (k21blocf(m,2).eq.3) then

       do jjk=1,8
       xextra(ng,jjk)=xextraa(m,ng,jjk)
       xintra(ng,jjk)=xintraa(m,ng,jjk)
       sjo(ng,jjk)=sjoo(m,ng,jjk)
       end do

       rib(i,166)=float(ng)

c      Upper rod
       call joncst3s(i,u,v,rib,xintra,xextra,xjonc,npo,atp,np)
       call pjoncs_b12(i,xjonc,npo,sjo,sepx,sepy,rib,xkf)
       call pjoncs_b17(i,xjonc,npo,sjo,sepxjo,sepyjo,rib,xkf)
       joncf(i,m,ng,2)=rib(i,167) ! Jonc lenght

c      Lower rod
       call joncst3i(i,u,v,rib,xintra,xextra,xjonc,npo,atp,np)
c      Draw lower jonc in box (1,2)
       call pjoncs_b12(i,xjonc,npo,sjo,sepx,sepy,rib,xkf)
c      Draw lower jonc in box (1,7)
       sepyjo=sepyjo+50.0d0*xwf*jonc_i_coe
       call pjoncs_b17(i,xjonc,npo,sjo,sepxjo,sepyjo,rib,xkf)
       joncf(i,m,ng,3)=rib(i,167) ! Jonc lenght
       sepyjo=sepyjo-50.0d0*xwf*jonc_i_coe

       end if ! bloc type 3

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case long rods on the extrados (type 4)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (k21blocf(m,2).eq.4) then
       do jjk=1,4
       xextra(ng,jjk)=xextraa(m,ng,jjk)
       xintra(ng,jjk)=xintraa(m,ng,jjk)
       sjo(ng,jjk)=sjoo(m,ng,jjk)
       end do
       rib(i,166)=float(ng)
       call joncs4(i,u,v,rib,xintra,xextra,xjonc,npo,atp,np)
       call pjoncs_b12(i,xjonc,npo,sjo,sepx,sepy,rib,xkf)
       call pjoncs_b17(i,xjonc,npo,sjo,sepxjo,sepyjo,rib,xkf)
       joncf(i,m,ng,4)=rib(i,167)
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case long rods on the intrados (type 5)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (k21blocf(m,2).eq.5) then
       do jjk=1,4
       xextra(ng,jjk)=xextraa(m,ng,jjk)
       xintra(ng,jjk)=xintraa(m,ng,jjk)
       sjo(ng,jjk)=sjoo(m,ng,jjk)
       end do
       rib(i,166)=float(ng)
       call joncs5(i,u,v,rib,xintra,xextra,xjonc,npo,atp,np)
       call pjoncs_b12(i,xjonc,npo,sjo,sepx,sepy,rib,xkf)
       call pjoncs_b17(i,xjonc,npo,sjo,sepxjo,sepyjo,rib,xkf)
       joncf(i,m,ng,5)=rib(i,167)
       end if


       end if ! detect rib
   
       end do ! group n

       end do ! bloc m

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.12.7 Print mylars
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (rib(i,168).ne.0) then
       call mylars(i,u,v,sepx,sepy,rib,xmy,np,xkf,atp)
       end if

c      Change airfoil loction

       kx=int((float(i)/6.))
       ky=i-kx*6
       kyy=kyy+1

       end do  ! i airfoil

      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.13 Airfoils drawing (para mesa de corte)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
              
c      Box (1,4)

       sepxx=700.*xkf
       sepyy=100.*xkf

       kx=0
       ky=0
       kyy=0

       do i=1,nribss

       sepx=2530.*xkf+sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky)

       call romano(i,sepx+0.89d0*rib(i,5),sepy-1.0d0,0.0d0,
     + typm6(9)*0.1,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.13.1 Airfoils borders
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Case "ds" or "ss"

       if (atp.eq."ds".or.atp.eq."ss") then

       do j=1,np(i,1)-1
       call line(sepx+u(i,j,16),-v(i,j,16)+sepy,sepx+u(i,j+1,16),
     + -v(i,j+1,16)+sepy,1)
       end do

       end if

c      Case "pc"

       jvi=np(i,2)           ! vent in
       jvo=np(i,2)+np(i,3)-1 ! vent out

       if (atp.eq."pc") then

c      Extrados

       do j=1,np(i,2)-1
       call line(sepx+u(i,j,16),-v(i,j,16)+sepy,sepx+u(i,j+1,16),
     + -v(i,j+1,16)+sepy,1)

       end do

c      Nose

c      Print segments if not zero thickness
       if (rib(i,149).ne.0.) then

       call line(sepx+u(i,jvi,16),-v(i,jvi,16)+sepy,sepx+u(i,jvo,16),
     + -v(i,jvo,16)+sepy,2)

c      Erase old code below:
c       call line(sepx+u(i,j+2,16),-v(i,j+2,16)+sepy,sepx+u(i,k-2,16),
c     + -v(i,k-2,16)+sepy,3)

c       call line(sepx+u(i,j+1,16),-v(i,j+1,16)+sepy,sepx+u(i,j,16),
c     + -v(i,j,16)+sepy,3)
c       call line(sepx+u(i,j+1,16),-v(i,j+1,16)+sepy,sepx+u(i,j+2,16),
c     + -v(i,j+2,16)+sepy,3)

c       call line(sepx+u(i,k-1,16),-v(i,k-1,16)+sepy,sepx+u(i,k-2,16),
c     + -v(i,k-2,16)+sepy,3)
c       call line(sepx+u(i,k-1,16),-v(i,k-1,16)+sepy,sepx+u(i,k,16),
c     + -v(i,k,16)+sepy,3)
       end if

c      Intrados

       do j=np(i,2)+np(i,3)-1,np(i,1)-1
       call line(sepx+u(i,j,16),-v(i,j,16)+sepy,sepx+u(i,j+1,16),
     + -v(i,j+1,16)+sepy,1)

       end do

       end if

     

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.13.2 Draw singular points-segments in airfoils border
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
       j=np(i,1)
       call line(sepx+u(i,j,16),-v(i,j,16)+sepy,sepx+u(i,j,3),
     + -v(i,j,3)+sepy,1)

       j=1
       call line(sepx+u(i,j,16),-v(i,j,16)+sepy,sepx+u(i,j,3),
     + -v(i,j,3)+sepy,1)

c See also: 9.3 Draw anchor points in airfoils

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      6.13.3 Draw elliptical holes (MC)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do l=1,hol(i,1,1)

c      Holes type 1 (ellipses)
       if (hol(i,l,9).eq.1) then

       xx0=hol(i,l,2)*rib(i,5)/100.+sepx
       yy0=(-hol(i,l,3)*rib(i,5)/100.+sepy)
       xxa=hol(i,l,4)*rib(i,5)/100.
       yyb=(hol(i,l,5)*rib(i,5)/100.)

       call ellipse(xx0,yy0,xxa,yyb,hol(i,l,6),1)

       end if ! Type 1

c      Dibuixa forat tipus 3 (triangles)

       if (hol(i,l,9).eq.3) then

       alptri=hol(i,l,6)*pi/180.
       atri=hol(i,l,4)*rib(i,5)/100.
       btri=hol(i,l,5)*rib(i,5)/100.
       rtri=hol(i,l,7)*rib(i,5)/100.
       
c      Marcador de costat negatiu
       satri=atri/sqrt(atri*atri)

       atri=abs(atri)

       cgor=0.5*pi-alptri
       ctri=dsqrt(atri*atri+btri*btri-2.*atri*btri*dcos(cgor))
       agor=dacos((ctri*ctri+btri*btri-atri*atri)/(2.*btri*ctri))
       bgor=dacos((ctri*ctri+atri*atri-btri*btri)/(2.*atri*ctri))
       aggor=pi-agor
       bggor=pi-bgor
       cggor=pi-cgor
       h1tri=rtri/dsin(0.5*cgor)
       h2tri=rtri/dsin(0.5*bgor)
       h3tri=rtri/dsin(0.5*agor)

       x1=hol(i,l,2)*rib(i,5)/100.
       y1=hol(i,l,3)*rib(i,5)/100.

       x2=x1+satri*atri*dcos(alptri)
       y2=y1+atri*dsin(alptri)

       x3=x1
       y3=y1+btri

       o1x=x1+satri*(h1tri*dcos(alptri+0.5*cgor))
       o1y=y1+h1tri*dsin(alptri+0.5*cgor)

       o2x=x2-satri*(h2tri*dcos(-alptri+0.5*bgor))
       o2y=y2+h2tri*dsin(-alptri+0.5*bgor)

       o3x=x3+satri*(h3tri*dsin(0.5*agor))
       o3y=y3-h3tri*dcos(0.5*agor)

c       write (*,*) atri,btri,ctri,agor,bgor,cgor

c      Inicialitza comptatge punts triangle
       k=1

       step1=cggor/6.
       do tetha=0.,cggor+0.01,step1
       xtri(k)=o1x-satri*rtri*dcos(tetha)
       ytri(k)=o1y-rtri*dsin(tetha)
       k=k+1
       end do

       step2=bggor/6.
       do tetha=alptri,alptri+bggor+0.01,step2
       xtri(k)=o2x+satri*rtri*dsin(tetha)
       ytri(k)=o2y-rtri*dcos(tetha)
       k=k+1
       end do

       step3=aggor/6.
       do tetha=agor,pi+0.01,step3
       xtri(k)=o3x+satri*rtri*dcos(tetha)
       ytri(k)=o3y+rtri*dsin(tetha)
       k=k+1
       end do

       xtri(k)=xtri(1)
       ytri(k)=ytri(1)

c      Dibuixa triangles

       if (satri.eq.1.) then
       do k=1,21
       call line(sepx+xtri(k),-ytri(k)+sepy,
     + sepx+xtri(k+1),-ytri(k+1)+sepy,1)
       end do
       end if

       if (satri.eq.-1.) then
       do k=1,21
       call line(sepx+xtri(k),-ytri(k)+sepy,
     + sepx+xtri(k+1),-ytri(k+1)+sepy,1)
       end do
       end if

       end if ! Type 3


c      Draw holes type 4 (rectangles)
c      Request from Pawel Lipski
c      Added 2023-12-07

       if (hol(i,l,9).eq.4) then

       alptri=hol(i,l,6)*pi/180.
       atri=hol(i,l,4)*rib(i,5)/100.
       btri=hol(i,l,5)*rib(i,5)/100.
       rtri=hol(i,l,7)*rib(i,5)/100.
       
c      Flag negative value. -1 if atri negative, 1 atri if potitive
       satri=atri/sqrt(atri*atri)
       atri=abs(atri) ! makes positive by default

c      Rectangle corners
       x1=hol(i,l,2)*rib(i,5)/100.
       y1=hol(i,l,3)*rib(i,5)/100.

       x2=x1+satri*atri*dcos(alptri)
       y2=y1+atri*dsin(alptri)

       x3=x2-satri*btri*dsin(alptri)
       y3=y2+btri*dcos(alptri)

       x4=x1-satri*btri*dsin(alptri)
       y4=y1+btri*dcos(alptri)


c      Circle centers of rounded corners
       o1x=x1+satri*rtri*dcos(alptri)-satri*rtri*dsin(alptri)
       o1y=y1+rtri*dsin(alptri)+rtri*dcos(alptri)

       o2x=x2-satri*rtri*dcos(alptri)-satri*rtri*dsin(alptri)
       o2y=y2-rtri*dsin(alptri)+rtri*dcos(alptri)

       o3x=x3-satri*rtri*dcos(alptri)+satri*rtri*dsin(alptri)
       o3y=y3-rtri*dsin(alptri)-rtri*dcos(alptri)

       o4x=x4+satri*rtri*dcos(alptri)+satri*rtri*dsin(alptri)
       o4y=y4+rtri*dsin(alptri)-rtri*dcos(alptri)


c      Draw segments
       x12=x1+satri*rtri*dcos(alptri)
       y12=y1+rtri*dsin(alptri)
       x21=x2-satri*rtri*dcos(alptri)
       y21=y2-rtri*dsin(alptri)

       x23=x2-satri*rtri*dsin(alptri)
       y23=y2+rtri*dcos(alptri)
       x32=x3+satri*rtri*dsin(alptri)
       y32=y3-rtri*dcos(alptri)

       x34=x3-satri*rtri*dcos(alptri)
       y34=y3-rtri*dsin(alptri)
       x43=x4+satri*rtri*dcos(alptri)
       y43=y4+rtri*dsin(alptri)

       x41=x4+satri*rtri*dsin(alptri)
       y41=y4-rtri*dcos(alptri)
       x14=x1-satri*rtri*dsin(alptri)
       y14=y1+rtri*dcos(alptri)


c      Case satri=1
       if (satri.eq.1) then

        k=1
c       First arc
        do tetha=0.0,pi/2.,pi/12.
        xtri(k)=o1x+rtri*dcos(tetha+pi+alptri)
        ytri(k)=o1y+rtri*dsin(tetha+pi+alptri)
        k=k+1
        end do

c       Second arc
        do tetha=0.0,pi/2.,pi/12.
        xtri(k)=o2x+rtri*dcos(tetha+1.5*pi+alptri)
        ytri(k)=o2y+rtri*dsin(tetha+1.5*pi+alptri)
        k=k+1
        end do

c       Third arc
        do tetha=0.0,pi/2.,pi/12.
        xtri(k)=o3x+rtri*dcos(tetha+alptri)
        ytri(k)=o3y+rtri*dsin(tetha+alptri)
        k=k+1
        end do

c       Fourth arc
        do tetha=0.0,pi/2.,pi/12.
        xtri(k)=o4x+rtri*dcos(tetha+0.5*pi+alptri)
        ytri(k)=o4y+rtri*dsin(tetha+0.5*pi+alptri)
        k=k+1
        end do 
        xtri(k)=xtri(1)
        ytri(k)=ytri(1)

       end if ! satri=1


c      Case satri=-1
       if (satri.eq.-1.) then

        k=1
c       First arc
        do tetha=0.0,pi/2.,pi/12.
        xtri(k)=o1x+rtri*dcos(-tetha-alptri)
        ytri(k)=o1y+rtri*dsin(-tetha-alptri)
        k=k+1
        end do

c       Second arc
        do tetha=0.0,pi/2.,pi/12.
        xtri(k)=o2x+rtri*dcos(-tetha-0.5*pi-alptri)
        ytri(k)=o2y+rtri*dsin(-tetha-0.5*pi-alptri)
        k=k+1
        end do

c       Third arc
        do tetha=0.0,pi/2.,pi/12.
        xtri(k)=o3x+rtri*dcos(-tetha+pi-alptri)
        ytri(k)=o3y+rtri*dsin(-tetha+pi-alptri)
        k=k+1
        end do

c       Fourth arc
        do tetha=0.0,pi/2.,pi/12.
        xtri(k)=o4x+rtri*dcos(-tetha+0.5*pi-alptri)
        ytri(k)=o4y+rtri*dsin(-tetha+0.5*pi-alptri)
        k=k+1
        end do 
        xtri(k)=xtri(1)
        ytri(k)=ytri(1)

       end if ! satri=-1


c      Dibuixa arcs

       if (satri.eq.1.) then
       do k=1,28
       call line(sepx+xtri(k),-ytri(k)+sepy,
     + sepx+xtri(k+1),-ytri(k+1)+sepy,1)
       end do
       end if

       if (satri.eq.-1.) then
       do k=1,28
       call line(sepx+xtri(k),-ytri(k)+sepy,
     + sepx+xtri(k+1),-ytri(k+1)+sepy,1)
       end do
       end if

       end if ! Type 4



       end do  ! holes

       kx=int((float(i)/6.))
       ky=i-kx*6
       kyy=kyy+1

       end do  ! in ribs
      

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      5+ Drawing planform in 2D view x-y
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      5+.1 Planform

c      Box (3,1)

       x0=0.
       y0=2000.*xkf
       
c      Ribs

       do i=1,nribss

       do j=1,np(i,1)-1

       x1=x(i,j)+x0
       y1=y(i,j)+y0
       x2=x(i,j+1)+x0
       y2=y(i,j+1)+y0

       call line(x1,y1,x2,y2,1)
       call line(-x1,y1,-x2,y2,1)

       end do

       end do

c      Trailing edge

       do i=1,nribss-1

       call line(x(i,1)+x0,y(i,1)+y0,x(i+1,1)+x0,y(i+1,1)+y0,1)
       call line(-x(i,1)+x0,y(i,1)+y0,-x(i+1,1)+x0,y(i+1,1)+y0,1)

       end do

       call line(-x(1,1)+x0,y(1,1)+y0,x(1,1)+x0,y(1,1)+y0,1)

c      Leading edge

c      Use the leading-edge index selected from the nearest profile point
       j=np(1,6)

       do i=1,nribss-1

       call line(x(i,j)+x0,y(i,j)+y0,x(i+1,j)+x0,y(i+1,j)+y0,1)
       call line(-x(i,j)+x0,y(i,j)+y0,-x(i+1,j)+x0,y(i+1,j)+y0,1)

       end do

       call line(-x(1,j)+x0,y(1,j)+y0,x(1,j)+x0,y(1,j)+y0,1)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Vents - Air intakes
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
c      Classic vents
       if (k31d.eq.0) then

c      Vent in
       do i=1,nribss

       j=np(i,2)

c      Control if cell is closed

       if(int(rib(i,14)).eq.1) then
       call line(x(i-1,j)+x0,y(i-1,j)+y0,x(i,j)+x0,y(i,j)+y0,3)
       call line(-x(i-1,j)+x0,y(i-1,j)+y0,-x(i,j)+x0,y(i,j)+y0,3)
       end if
       if(int(rib(i,14)).eq.0) then
       call line(x(i-1,j)+x0,y(i-1,j)+y0,x(i,j)+x0,y(i,j)+y0,9)
       call line(-x(i-1,j)+x0,y(i-1,j)+y0,-x(i,j)+x0,y(i,j)+y0,9)
       end if

       end do

c      Vent out
       do i=1,nribss

       j=np(i,2)+np(i,3)-1

c      Control if cell is closed

       if(int(rib(i,14)).eq.1) then
       call line(x(i-1,j)+x0,y(i-1,j)+y0,x(i,j)+x0,y(i,j)+y0,3)
       call line(-x(i-1,j)+x0,y(i-1,j)+y0,-x(i,j)+x0,y(i,j)+y0,3)
       end if

       end do
       end if ! k31d=0

c      Case new vents
c      Estructura similar al cas 3D secció 21.2
       if (k31d.eq.1) then

       do i=1,nribss

       p1x=x(i-1,np(i,2))+x0
       p1y=y(i-1,np(i,2))+y0
       p2x=x(i,np(i,2))+x0
       p2y=y(i,np(i,2))+y0

       j=np(i,2)+np(i,3)-1

       p3x=x(i-1,j)+x0
       p3y=y(i-1,j)+y0
       p4x=x(i,j)+x0
       p4y=y(i,j)+y0

       if (rib(i,165).eq.0) then
       call line(p1x,p1y,p2x,p2y,3)
       call line(-p1x,p1y,-p2x,p2y,3)
       call line(p3x,p3y,p4x,p4y,3)
       call line(-p3x,p3y,-p4x,p4y,3)
       end if

       if (rib(i,165).eq.1) then
       call line(p3x,p3y,p4x,p4y,3)
       call line(-p3x,p3y,-p4x,p4y,3)
       end if

       if (rib(i,165).eq.-1) then
       call line(p1x,p1y,p2x,p2y,3)
       call line(-p1x,p1y,-p2x,p2y,3)
       end if

       if (rib(i,165).eq.-2) then
       call line(p1x,p1y,p2x,p2y,3)
       call line(-p1x,p1y,-p2x,p2y,3)
       call line(p3x,p3y,p2x,p2y,3)
       call line(-p3x,p3y,-p2x,p2y,3)
       end if

       if (rib(i,165).eq.-3) then
       call line(p1x,p1y,p2x,p2y,3)
       call line(-p1x,p1y,-p2x,p2y,3)
       call line(p1x,p1y,p4x,p4y,3)
       call line(-p1x,p1y,-p4x,p4y,3)
       end if

       end do

       end if ! k31d=1

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      5+.2 Drawing real*8 canopy in 2D view x-z
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Box (4,1)

       x0=0.
       y0=3000.*xkf

       do i=1,nribss

       do j=1,np(i,1)-1

       x1=x(i,j)+0.
       y1=z(i,j)+3000.*xkf
       x2=x(i,j+1)+0.
       y2=z(i,j+1)+3000.*xkf

       call line(x1,y1,x2,y2,7)
       call line(-x1,y1,-x2,y2,7)

       end do

       end do

c      Vent in

       do i=1,nribss

       j=np(i,2)

c      Control if cell is closed

       if(int(rib(i,14)).eq.1) then
       call line(x(i-1,j)+x0,z(i-1,j)+y0,x(i,j)+x0,z(i,j)+y0,3)
       call line(-x(i-1,j)+x0,z(i-1,j)+y0,-x(i,j)+x0,z(i,j)+y0,3)
       end if
       if(int(rib(i,14)).eq.0) then
       call line(x(i-1,j)+x0,z(i-1,j)+y0,x(i,j)+x0,z(i,j)+y0,9)
       call line(-x(i-1,j)+x0,z(i-1,j)+y0,-x(i,j)+x0,z(i,j)+y0,9)
       end if

       end do

c      Vent out
       do i=1,nribss

       j=np(i,2)+np(i,3)-1

c      Control if cell is closed

       if(int(rib(i,14)).eq.1) then
       call line(x(i-1,j)+x0,z(i-1,j)+y0,x(i,j)+x0,z(i,j)+y0,3)
       call line(-x(i-1,j)+x0,z(i-1,j)+y0,-x(i,j)+x0,z(i,j)+y0,3)
       end if

       end do

c      Drawing leading edge
c      Use the leading-edge index selected from the nearest profile point
       j=np(1,6)

       do i=1,nribss-1

       call line(x(i,j)+x0,z(i,j)+y0,x(i+1,j)+x0,z(i+1,j)+y0,5)
       call line(-x(i,j)+x0,z(i,j)+y0,-x(i+1,j)+x0,z(i+1,j)+y0,5)

       end do

       call line(-x(1,j)+x0,z(1,j)+y0,x(1,j)+x0,z(1,j)+y0,5)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      5+.3 Drawing real*8 canopy in 2D view y-z
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Box (4,2)

       x0=1260.*xkf
       y0=3000.*xkf

c      Airfoils y-z
       do i=1,nribss

       do j=1,np(i,1)-1
       call line(-y(i,j)+x0,z(i,j)+y0,-y(i,j+1)+x0,z(i,j+1)+y0,3)
       end do

c      Trailing edge
       j=np(i,1)
       call line(-y(i-1,j)+x0,z(i-1,j)+y0,-y(i,j)+x0,z(i,j)+y0,2)
c      Inlet in       
       j=np(i,2)
       if(rib(i,14).eq.1) then
       call line(-y(i-1,j)+x0,z(i-1,j)+y0,-y(i,j)+x0,z(i,j)+y0,1)
       end if
       if(rib(i,14).eq.0) then
       call line(-y(i-1,j)+x0,z(i-1,j)+y0,-y(i,j)+x0,z(i,j)+y0,9)
       end if

c      Inlet out     
c       if(rib(i-1,14).eq.1.and.rib(i,14).eq.1) then
       if(rib(i,14).eq.1) then
       j=np(i,2)+np(i,3)-1
       call line(-y(i-1,j)+x0,z(i-1,j)+y0,-y(i,j)+x0,z(i,j)+y0,1)
       end if

c      Long lines
       do j=2, np(i,2)-1,5
       call line(-y(i-1,j)+x0,z(i-1,j)+y0,-y(i,j)+x0,z(i,j)+y0,4)
       end do
       do j=np(i,2)+np(i,3)+1,np(i,1)-1,5
       call line(-y(i-1,j)+x0,z(i-1,j)+y0,-y(i,j)+x0,z(i,j)+y0,8)
       end do

       end do

c      Ultim perfil
c       i=nribss
c       do j=1,np(i,1)-1
c       call line(-y(i,j)+x0,z(i,j)+y0,-y(i,j+1)+x0,z(i,j+1)+y0,4)
c       end do



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
