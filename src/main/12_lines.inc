!> Build the suspension-line topology, spatial endpoints, and drawing tree.
!!
!! Expands section-9 ramification paths into `corda` records, resolves line
!! endpoints from singular rib points and risers, calculates lengths/angles,
!! and draws line plans using the configured construction colors.
c      12. LINES
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c        write (*,*) "pi 12. =",pi
c        pi=4.0d0*datan(1.)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      12.1 Write lines matrix
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do ii=1,slp

       do j=1,cam(ii)

c       write (*,*) ii,j, mc(ii,j,2), mc(ii,j,3), mc(ii,j,4), mc(ii,j,5),
c     + mc(ii,j,6), mc(ii,j,7)," - "  ,mc(ii,j,14), mc(ii,j,15)


       end do
       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      12.2 Identifica les cordes a calcular
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       ic1=0
       ic2=1

       do ii=1,slp      ! Itera en numero de plans

       do k=2,10,2      ! Explore all five possible levels
       
       do j=1,cam(ii)-1 ! Itera en camins de cada pla

c      A level not present in this path represents no line
       a=0.0d0
       b=0.0d0
       aa=0.0d0
       bb=0.0d0

       if (mc(ii,j,1).le.4.and.k.le.8) then ! Detecta fins 4 nivells
       a=dfloat(mc(ii,j,k))
       b=dfloat(mc(ii,j,k+1))
       aa=dfloat(mc(ii,j+1,k))
       bb=dfloat(mc(ii,j+1,k+1))
       endif

       if (mc(ii,j,1).eq.5.and.k.eq.10) then ! Llegeix nivell 5
       a=dfloat(mc(ii,j,k))
       b=dfloat(mc(ii,j,k+1))
       aa=dfloat(mc(ii,j+1,k))
       bb=dfloat(mc(ii,j+1,k+1))
       endif

       if (a.ne.0.0d0.and.b.ne.0.0d0) then ! count pair line

c      While pair is equal, increase counter
       if (a.eq.aa.and.b.eq.bb) then

       ic2=ic2+1 ! comptabilitza cordes iguals

c      Si arribem a final del cami comptabilitzar la corda
       if (j.eq.cam(ii)-1) then

       ic1=ic1+1

       corda(ic1,1)=ii                    !planol
       corda(ic1,2)=mc(ii,j+1,k)          !nivell
       corda(ic1,3)=mc(ii,j+1,k+1)        !ordre
       corda(ic1,4)=ic2                   !punts d'acció
       corda(ic1,5)=mc(ii,j+1,1)          !ramificacions del camÃ­
       corda(ic1,6)=mc(ii,j+1,14)         !final row
       corda(ic1,7)=mc(ii,j+1,15)         !final rib

c      Assign types
       corda(ic1,30+mc(ii,j+1,k))=mc(ii,j+1,30+mc(ii,j+1,k))

       ic2=1

       end if

       end if

c      Si canvia la corda al mateix nivell
       if (a.ne.aa.or.b.ne.bb) then

c       write (*,*) "Ep ",b,bb

       ic1=ic1+1

       corda(ic1,1)=ii                  !planol
       corda(ic1,2)=mc(ii,j,k)          !nivell
       corda(ic1,3)=mc(ii,j,k+1)        !ordre
       corda(ic1,4)=ic2                 !punts d'acciÃ³
       corda(ic1,5)=mc(ii,j,1)          !ramificacions del camÃ­
       corda(ic1,6)=mc(ii,j,14)         !final row
       corda(ic1,7)=mc(ii,j,15)         !final rib

c      Assign types
       corda(ic1,30+mc(ii,j,k))=mc(ii,j,30+mc(ii,j,k))

       ic2=1

       end if

c      Si arribem a l'ultima linia i no es zero
       if (j.eq.cam(ii)-1) then

       a=dfloat(mc(ii,j,k))
       b=dfloat(mc(ii,j,k+1))
       aa=dfloat(mc(ii,j+1,k))
       bb=dfloat(mc(ii,j+1,k+1))

c      Last level
       if ((a.ne.aa.or.b.ne.bb).and.(aa.ne.0.0d0.and.bb.ne.0.0d0)) then

       ic1=ic1+1

       corda(ic1,1)=ii                    !planol
       corda(ic1,2)=mc(ii,j+1,k)          !nivell
       corda(ic1,3)=mc(ii,j+1,k+1)        !ordre
       corda(ic1,4)=ic2                   !punts d'accio
       corda(ic1,5)=mc(ii,j+1,1)          !ramificacions del camÃ­
       corda(ic1,6)=mc(ii,j+1,14)         !final row
       corda(ic1,7)=mc(ii,j+1,15)         !final rib

c      Assign types
       corda(ic1,30+mc(ii,j+1,k))=mc(ii,j+1,30+mc(ii,j+1,k))

       ic2=1

       end if ! last level

       end if ! last line not zero

       end if ! count pair line

       end do  ! j path
       end do  ! k level 2 4 6 8 10 
       end do  ! ii plan

       cordam=ic1 ! maxim nombre de cordes

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      12.3 Compute anchor points in 3D space
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Need simplification as uses same code as section 6 (!!!!!)

       do i=1,nribss+1

       tetha=rib(i,8)*pi/180.
       rot_z=rib(i,250)*pi/180.0
       pos=rib(i,5)*rib(i,251)/100.0

       do j=1,rib(i,15) ! anchor number


c      Call to subroutine xyzt
c       u_aux(i,j,1)=u(i,j,6)
c       v_aux(i,j,1)=v(i,j,6)
c       w_aux(i,j,1)=0.0d0
c       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
c       w(i,j,19)=w_aux(i,j,5)
c       u(i,j,19)=u_aux(i,j,5)
c       v(i,j,19)=v_aux(i,j,5)


c       goto 2


c      Airfoil anchor washin coordinates
c      Note the parameter -rib(i,50) and review section 6.8 (!)
       u(i,j,17)=(u(i,j,6)-(rib(i,10)/100.)*rib(i,5))*dcos(tetha)+
     + v(i,j,6)*dsin(tetha)+(rib(i,10)/100.)*rib(i,5)
       v(i,j,17)=(-u(i,j,6)+(rib(i,10)/100.)*rib(i,5))*dsin(tetha)+
     + v(i,j,6)*dcos(tetha)-rib(i,50)

c      Airfoil rotation in Z-axis. ( View section 6, MUST be the same )

       wnew(j)=-u(i,j,17)*dsin(rot_z)+pos*dsin(rot_z)
       unew(j)=u(i,j,17)*dcos(rot_z)+pos*(1-dcos(rot_z))
       vnew(j)=v(i,j,17)
       u(i,j,4)=unew(j)
       v(i,j,4)=vnew(j)
       w(i,j,4)=wnew(j)

c      Airfoil anchor (u,v,w) espace coordinates
c       u(i,j,18)=u(i,j,17)
c       v(i,j,18)=v(i,j,17)*dcos(rib(i,9)*pi/180.)
c       w(i,j,18)=-v(i,j,17)*dsin(rib(i,9)*pi/180.)

c      Airfoils rotation in Y-axis
       w(i,j,18)=-w(i,j,4)*dcos(rib(i,9)*pi/180.)-
     + v(i,j,4)*dsin(rib(i,9)*pi/180.)
       u(i,j,18)=u(i,j,4)
       v(i,j,18)=-w(i,j,4)*dsin(rib(i,9)*pi/180.)+
     + v(i,j,4)*dcos(rib(i,9)*pi/180.)

c      Airfoil anchor (x,y,z) absolute coordinates

       u(i,j,19)=rib(i,6)-w(i,j,18)
       v(i,j,19)=rib(i,3)+u(i,j,18)
       w(i,j,19)=rib(i,7)-v(i,j,18)

c 2      continue
    
       end do
       end do

       i=0
       do j=1,rib(1,15)
       u(i,j,19)=-u(1,j,19)
       v(i,j,19)=v(1,j,19)
       w(i,j,19)=w(1,j,19)
       end do

c      Snapshot the unmodified Stage-12.3 A--E anchor boundary before brake
c      displacement (12.4) or single-surface virtual-anchor replacement
c      (12.4+) can change slot 19.  The checked transactional adapter retains
c      each distinct legacy producer: requested chord, Stage-6 intrados mark
c      point/TE distance, Stage-9 slot-6 profile point, and Stage-12.3 absolute
c      X/Y/Z.  It does not force them equal: the even-cell terminal physical
c      profile demonstrates that the old intrados search can have no segment.
c
c      Row 0 keeps its explicit mirrored identity.  Tip support nribss+1 stays
c      available only in wing_anchor_definitions: Stage 9 never produces its
c      slot-6 anchors even though old Stage 12 loops over that row.  Save and
c      restore shared loop indices so collection cannot perturb downstream
c      drawing or mark placement.
       if (.not.allocated(wing_anchor_definitions).or.
     +     .not.allocated(wing_rib_definitions)) then
       write (*,'(A)')
     + 'ERROR: typed anchor dependencies unavailable at Stage 12.3'
       stop 1
       end if
       if (allocated(wing_base_resolved_anchors))
     + deallocate(wing_base_resolved_anchors)
       if (allocated(wing_base_resolved_anchor_available))
     + deallocate(wing_base_resolved_anchor_available)
       allocate(wing_base_resolved_anchors(0:nribss+1))
       allocate(wing_base_resolved_anchor_available(0:nribss+1))
       wing_base_resolved_anchor_available=.false.
       anchor_saved_i=i
       anchor_saved_j=j
       do i=0,nribss
       call copy_legacy_resolved_rib_anchors(wing_anchor_definitions(i),
     + rib,u,v,w,
     + wing_base_resolved_anchors(i),domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)') 'ERROR: base anchor snapshot at rib ',i,': ',
     + trim(domain_model_message)
       stop 1
       end if
       call resolved_rib_anchors_match_legacy(
     + wing_base_resolved_anchors(i),wing_anchor_definitions(i),rib,u,v,w,
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: typed/legacy base anchor mismatch at rib ',i,': ',
     + trim(domain_model_message)
       stop 1
       end if
       wing_base_resolved_anchor_available(i)=.true.
       end do
       if (any(.not.wing_base_resolved_anchor_available(0:nribss))) then
       write (*,'(A)') 'ERROR: incomplete base anchor collection'
       stop 1
       end if

c      Independently replace only the Stage-12.3 spatial portion from the
c      retained Stage-9 rib-local points and named rib placement definitions.
c      The separate Stage-6 mark points/distances remain untouched.  Physical
c      rows are transformed directly; row 0 mirrors the completed row-1
c      candidate, exactly matching the historical post-transform policy.
c
c      Publication is whole-wing and bit-exact.  Tolerance agreement catches
c      a real algorithmic mismatch, but even a last-bit delta keeps the legacy
c      slot-19 producer authoritative because line topology is branch-sensitive.
c      Tip support is intentionally excluded until Stage 9 gives it a defined
c      local point.  Later brake displacement and single-surface replacement
c      still act on the published compatibility slot as before.
       if (allocated(typed_base_resolved_anchor_candidates))
     + deallocate(typed_base_resolved_anchor_candidates)
       allocate(typed_base_resolved_anchor_candidates(0:nribss))
       typed_base_anchor_spatial_exact=.true.
       do i=1,nribss
       call place_resolved_rib_anchors(wing_base_resolved_anchors(i),
     + wing_rib_definitions(i),typed_base_resolved_anchor_candidates(i),
     + domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)') 'ERROR: typed base anchor placement at rib ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       call resolved_rib_anchors_match_legacy(
     + typed_base_resolved_anchor_candidates(i),wing_anchor_definitions(i),
     + rib,u,v,w,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: typed/legacy placed anchor mismatch at rib ',i,': ',
     + trim(domain_model_message)
       stop 1
       end if
       do j=1,size(typed_base_resolved_anchor_candidates(i)%anchors)
       if (any(abs([
     + typed_base_resolved_anchor_candidates(i)%anchors(j)%spatial_point%x_cm-
     + u(i,j,19),
     + typed_base_resolved_anchor_candidates(i)%anchors(j)%spatial_point%y_cm-
     + v(i,j,19),
     + typed_base_resolved_anchor_candidates(i)%anchors(j)%spatial_point%z_cm-
     + w(i,j,19)]) .gt.0.0d0))
     + typed_base_anchor_spatial_exact=.false.
       end do
       end do

       call build_symmetry_resolved_anchors(
     + typed_base_resolved_anchor_candidates(1),wing_anchor_definitions(0),
     + typed_base_resolved_anchor_candidates(0),domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)') 'ERROR: typed center anchor placement failed: ',
     + trim(domain_model_message)
       stop 1
       end if
       call resolved_rib_anchors_match_legacy(
     + typed_base_resolved_anchor_candidates(0),wing_anchor_definitions(0),
     + rib,u,v,w,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(2A)')
     + 'ERROR: typed/legacy center anchor mismatch: ',
     + trim(domain_model_message)
       stop 1
       end if
       do j=1,size(typed_base_resolved_anchor_candidates(0)%anchors)
       if (any(abs([
     + typed_base_resolved_anchor_candidates(0)%anchors(j)%spatial_point%x_cm-
     + u(0,j,19),
     + typed_base_resolved_anchor_candidates(0)%anchors(j)%spatial_point%y_cm-
     + v(0,j,19),
     + typed_base_resolved_anchor_candidates(0)%anchors(j)%spatial_point%z_cm-
     + w(0,j,19)]) .gt.0.0d0))
     + typed_base_anchor_spatial_exact=.false.
       end do

       if (typed_base_anchor_spatial_exact) then
       wing_base_resolved_anchors(0:nribss)=
     + typed_base_resolved_anchor_candidates
       do i=0,nribss
       call write_legacy_resolved_anchor_spatial_points(
     + wing_base_resolved_anchors(i),u,v,w,domain_model_valid,
     + domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)') 'ERROR: anchor spatial publication at rib ',i,
     + ': ',trim(domain_model_message)
       stop 1
       end if
       end do
       end if
       i=anchor_saved_i
       j=anchor_saved_j

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     12.4 Compute singular rib points in 3D space
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=0,nribss+1

       tetha=rib(i,8)*pi/180.0d0
       rot_z=rib(i,250)*pi/180.0
       pos=rib(i,5)*rib(i,251)/100.0

       do j=6,8 ! singular point number, brakes, inlet in, inlet out

c      Airfoil anchor washin coordinates
       u(i,j,17)=(u(i,j,6)-(rib(i,10)/100.)*rib(i,5))*dcos(tetha)+
     + v(i,j,6)*dsin(tetha)+(rib(i,10)/100.)*rib(i,5)
       v(i,j,17)=(-u(i,j,6)+(rib(i,10)/100.)*rib(i,5))*dsin(tetha)+
     + v(i,j,6)*dcos(tetha)-rib(i,50)

c      Airfoil rotation in Z-axis. ( View section 6, MUST be the same )

       wnew(j)=-u(i,j,17)*dsin(rot_z)+pos*dsin(rot_z)
       unew(j)=u(i,j,17)*dcos(rot_z)+pos*(1-dcos(rot_z))
       vnew(j)=v(i,j,17)
       u(i,j,4)=unew(j)
       v(i,j,4)=vnew(j)
       w(i,j,4)=wnew(j)

c      Airfoil anchor (u,v,w) espace coordinates
c       u(i,j,18)=u(i,j,17)
c       v(i,j,18)=v(i,j,17)*dcos(rib(i,9)*pi/180.0d0)
c       w(i,j,18)=-v(i,j,17)*dsin(rib(i,9)*pi/180.0d0)

c      Airfoils rotation in Y-axis
       w(i,j,18)=-w(i,j,4)*dcos(rib(i,9)*pi/180.)-
     + v(i,j,4)*dsin(rib(i,9)*pi/180.)
       u(i,j,18)=u(i,j,4)
       v(i,j,18)=-w(i,j,4)*dsin(rib(i,9)*pi/180.)+
     + v(i,j,4)*dcos(rib(i,9)*pi/180.)

c      Airfoil anchor (x,y,z) absolute coordinates

       u(i,j,19)=rib(i,6)-w(i,j,18)
       v(i,j,19)=rib(i,3)+u(i,j,18)
       w(i,j,19)=rib(i,7)-v(i,j,18)

c      Brake distribution

       if (j.eq.6) then

       xprib=(rib(i,2)/rib(nribss,2))*100.0d0
       xlx=0.0d0
       xly=0.0d0
       
       if (xprib.ge.bd(1,1).and.xprib.lt.bd(2,1)) then
       xm=(bd(2,2)-bd(1,2))/(bd(2,1)-bd(1,1))
       xb=bd(1,2)-xm*bd(1,1)
       xxl=xm*xprib+xb
       xlx=xxl*dsin(rib(i,9)*pi/180.)
       xly=xxl*dcos(rib(i,9)*pi/180.)
       end if

       if (xprib.ge.bd(2,1).and.xprib.lt.bd(3,1)) then
       xm=(bd(3,2)-bd(2,2))/(bd(3,1)-bd(2,1))
       xb=bd(2,2)-xm*bd(2,1)
       xxl=xm*xprib+xb
       xlx=xxl*dsin(rib(i,9)*pi/180.)
       xly=xxl*dcos(rib(i,9)*pi/180.)
       end if

       if (xprib.ge.bd(3,1).and.xprib.lt.bd(4,1)) then
       xm=(bd(4,2)-bd(3,2))/(bd(4,1)-bd(3,1))
       xb=bd(3,2)-xm*bd(3,1)
       xxl=xm*xprib+xb
       xlx=xxl*dsin(rib(i,9)*pi/180.)
       xly=xxl*dcos(rib(i,9)*pi/180.)
       end if

       if (xprib.ge.bd(4,1).and.xprib.le.bd(5,1)) then
       xm=(bd(5,2)-bd(4,2))/(bd(5,1)-bd(4,1))
       xb=bd(4,2)-xm*bd(4,1)
       xxl=xm*xprib+xb
       xlx=xxl*dsin(rib(i,9)*pi/180.)
       xly=xxl*dcos(rib(i,9)*pi/180.)
       end if

       u(i,j,19)=u(i,j,19)+xlx
       w(I,j,19)=w(i,j,19)-xly

       end if
    
       end do
       end do


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      12.4+ Redefineix punts ancoratge per a parapents tipus ss
c      Atencio als parametres atp i kaaa
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Punts tipus 19 definits a extrados
       if (atp.eq."ss") then

       do i=0,nribss
       do j=1,rib(i,15)

c      Desa ancoratges originals a vector 20       
       u(i,j,20)=u(i,j,19)
       v(i,j,20)=v(i,j,19)
       w(i,j,20)=w(i,j,19)

c      Defineix ancoratges virtuals extrados       
       jp=anccont(i,j)

       u(i,j,19)=(x(i,jp)+x(i,jp-1))/2.
       v(i,j,19)=(y(i,jp)+y(i,jp-1))/2.
       w(i,j,19)=(z(i,jp)+z(i,jp-1))/2.

c      Activar kaaa=1 nomes al fer suspentes banda A deixar morro on es
    
       if (kaaa.eq.1) then
       u(i,1,19)=u(i,1,20)
       v(i,1,19)=v(i,1,20)
       w(i,1,19)=w(i,1,20)
       end if

c       write (*,*) "19 ss ",i,j,u(i,j,19),v(i,j,19),w(i,j,19)
c       write (*,*) "20 ds ",i,j,u(i,j,20),v(i,j,20),w(i,j,20)


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      ESPECIAL BHL-PAMPA
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      if rib(i,56) eq 0 not rotate triangle

       if (rib(i,56).eq.0.) then
       u(i,j,19)=u(i,j,20)
       v(i,j,19)=v(i,j,20)
       w(i,j,19)=w(i,j,20)
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      ESPECIAL BHL-PAMPA
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       end do
       end do
       
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     12.4++ Calcula carregues a cada ancoratge
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Calcula suma de ribs i suma de pesos relatius

       rib1t=0.
       rib2t=0.
       rib3t=0.
       xloadtot=0.

       do i=1,nribss

       if (rib(i,55).ne.0) then
       rib1t=rib1t+rib(i,5)
       rib2t=rib2t+rib(i,55)
       rib3t=rib3t+rib(i,5)*rib(i,55)
       end if

       end do

c       write (*,*) "rib1t, rib2t ", rib1t, rib2t

c      Assigna carregues a cada ancoratge       
       do i=1,nribss

       if (rib(i,55).ne.0) then

       do j=1,rib(i,15)
       aload(i,j)=(csusl/2.)*(cdis(int(rib(i,15)),j)/100.)*
     + (rib(i,55)*rib(i,5)/rib3t)

c       write (*,*) "A ", i, j, aload(i,j)
       xloadtot=xloadtot+aload(i,j)

       end do

       end if

       end do

c       write (*,*) "LOAD TOTAL: ",2.*xloadtot


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     12.5 Linies d'accio de cada corda
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Important
c      zcontrol=0 geometric action points, 
c      zcontrol=1 pondered ap
c      zcontrol=2 pondered ap 
c      comprovar les divergències

c      zcontrol=0

c      Defineix punt final de la linia d'accio de cada corda

       do i=1,cordam ! For all lines

       xcorda(i,3)=0.
       ycorda(i,3)=0.
       zcorda(i,3)=0.
       corda(i,8)=0.
       xload(i)=0.

       do ii=1,slp    !For all plans

       do k=2,8,2     ! For all levels (max=4)

       do j=1,cam(ii) ! For all paths

       if (ii.eq.corda(i,1)) then

       if (corda(i,2).eq.mc(ii,j,k).and.mc(ii,j,k+1).eq.corda(i,3)) then

       if (zcontrol.eq.0) then

c      0. Suma de les coordenades dels ancoratges finals
       xcorda(i,3)=xcorda(i,3)+u(mc(ii,j,15),mc(ii,j,14),19)
       ycorda(i,3)=ycorda(i,3)+v(mc(ii,j,15),mc(ii,j,14),19)
       zcorda(i,3)=zcorda(i,3)+w(mc(ii,j,15),mc(ii,j,14),19)

c      Carrega total a linia i
       xload(i)=xload(i)+aload(mc(ii,j,15),mc(ii,j,14))

       end if

       if(zcontrol.eq.1) then

c      1. Suma coordanades ancoratges ponderades per la long de rib
       xcorda(i,3)=xcorda(i,3)+u(mc(ii,j,15),mc(ii,j,14),19)*
     + rib(corda(i,7),5)
       ycorda(i,3)=ycorda(i,3)+v(mc(ii,j,15),mc(ii,j,14),19)*
     + rib(corda(i,7),5)
       zcorda(i,3)=zcorda(i,3)+w(mc(ii,j,15),mc(ii,j,14),19)*
     + rib(corda(i,7),5)

c      Suma longitud de ribs associats a i
       corda(i,8)=corda(i,8)+rib(corda(i,7),5)

       xload(i)=xload(i)+aload(mc(ii,j,15),mc(ii,j,14))

       end if

       if(zcontrol.eq.2) then

c      2. Coordenades ponderades per rib i pes relatiu
       xcorda(i,3)=xcorda(i,3)+u(mc(ii,j,15),mc(ii,j,14),19)*
     + rib(corda(i,7),5)*rib(corda(i,7),55)
       ycorda(i,3)=ycorda(i,3)+v(mc(ii,j,15),mc(ii,j,14),19)*
     + rib(corda(i,7),5)*rib(corda(i,7),55)
       zcorda(i,3)=zcorda(i,3)+w(mc(ii,j,15),mc(ii,j,14),19)*
     + rib(corda(i,7),5)*rib(corda(i,7),55)

       corda(i,8)=corda(i,8)+rib(corda(i,7),5)*rib(corda(i,7),55)

       xload(i)=xload(i)+aload(mc(ii,j,15),mc(ii,j,14))

       end if


       if(zcontrol.eq.3) then

c      3. Coordenades ponderades pel pes
       xcorda(i,3)=xcorda(i,3)+u(mc(ii,j,15),mc(ii,j,14),19)*
     + aload(mc(ii,j,15),mc(ii,j,14))
       ycorda(i,3)=ycorda(i,3)+v(mc(ii,j,15),mc(ii,j,14),19)*
     + aload(mc(ii,j,15),mc(ii,j,14))
       zcorda(i,3)=zcorda(i,3)+w(mc(ii,j,15),mc(ii,j,14),19)*
     + aload(mc(ii,j,15),mc(ii,j,14))

       corda(i,8)=corda(i,8)+aload(mc(ii,j,15),mc(ii,j,14))

       xload(i)=xload(i)+aload(mc(ii,j,15),mc(ii,j,14))

       end if


       end if

       end if

       end do

       end do

       end do

c      Center of gravity line i
       
       if(zcontrol.eq.0) then
       xcorda(i,3)=xcorda(i,3)/float(corda(i,4))
       ycorda(i,3)=ycorda(i,3)/float(corda(i,4))
       zcorda(i,3)=zcorda(i,3)/float(corda(i,4))
       end if

       if(zcontrol.eq.1) then
       xcorda(i,3)=xcorda(i,3)/corda(i,8)
       ycorda(i,3)=ycorda(i,3)/corda(i,8)
       zcorda(i,3)=zcorda(i,3)/corda(i,8)
       end if

       if(zcontrol.eq.2) then
       xcorda(i,3)=xcorda(i,3)/corda(i,8)
       ycorda(i,3)=ycorda(i,3)/corda(i,8)
       zcorda(i,3)=zcorda(i,3)/corda(i,8)
       end if

       if(zcontrol.eq.3) then
       xcorda(i,3)=xcorda(i,3)/xload(i)
       ycorda(i,3)=ycorda(i,3)/xload(i)
       zcorda(i,3)=zcorda(i,3)/xload(i)
       end if

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      12.6 Punts inicial i final de cada corda
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      12.6.1 LEVEL 1 (risers)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=1,cordam

       if (corda(i,2).eq.1) then

c      Cordes 1 tenen l'inici a mosquetons principals
       xcorda(i,1)=xkar
       ycorda(i,1)=ykar
       zcorda(i,1)=zkar

c      Equacio parametrica de la recta que passa per P1-P3

       dist=dsqrt((xcorda(i,3)-xcorda(i,1))**2+(ycorda(i,3)-
     + ycorda(i,1))**2+(zcorda(i,3)-zcorda(i,1))**2)

       cdl=(xcorda(i,3)-xcorda(i,1))/dist
       cdm=(ycorda(i,3)-ycorda(i,1))/dist
       cdn=(zcorda(i,3)-zcorda(i,1))/dist

c      Parametre necesari a la distancia objectiu
c      Cases risers 0 or 1
       x33(10)=x33(corda(i,1)) ! clengr
       t=x33(10)/(sqrt(cdl*cdl+cdm*cdm+cdn*cdn))

c      Punt P2 amb equacio parametrica
       xcorda(i,2)=xcorda(i,1)+cdl*t
       ycorda(i,2)=ycorda(i,1)+cdm*t
       zcorda(i,2)=zcorda(i,1)+cdn*t

       ii=corda(i,1)

       x1line(ii,1,corda(i,3))=xcorda(i,1)
       y1line(ii,1,corda(i,3))=ycorda(i,1)
       z1line(ii,1,corda(i,3))=zcorda(i,1)
       x2line(ii,1,corda(i,3))=xcorda(i,2)
       y2line(ii,1,corda(i,3))=ycorda(i,2)
       z2line(ii,1,corda(i,3))=zcorda(i,2)

c      comprobacio

       disto=dsqrt((xcorda(i,2)-xcorda(i,1))**2+(ycorda(i,2)-
     + ycorda(i,1))**2+(zcorda(i,2)-zcorda(i,1))**2)

       end if

       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      12.6.2 LEVEL 2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=1,cordam

       if (corda(i,2).eq.2) then

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      LEVEL 2 Si només dos nivells
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (corda(i,5).eq.2) then !directe a l'ancoratge

       xcorda(i,1)=x2line(corda(i,1),1,1)
       ycorda(i,1)=y2line(corda(i,1),1,1)
       zcorda(i,1)=z2line(corda(i,1),1,1)
      
       xcorda(i,2)=u(corda(i,7),corda(i,6),19)
       ycorda(i,2)=v(corda(i,7),corda(i,6),19)
       zcorda(i,2)=w(corda(i,7),corda(i,6),19)

       ii=corda(i,1)

       x1line(ii,2,corda(i,3))=xcorda(i,1)
       y1line(ii,2,corda(i,3))=ycorda(i,1)
       z1line(ii,2,corda(i,3))=zcorda(i,1)
       x2line(ii,2,corda(i,3))=xcorda(i,2)
       y2line(ii,2,corda(i,3))=ycorda(i,2)
       z2line(ii,2,corda(i,3))=zcorda(i,2)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Correccions necessaries a parapents ss (a nivell 2)
c      Calcula angles de gir phi0=phi1-phi2 a aplicar a triangles ss
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Per que es perd el valor de pi?
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       pi=4.0d0*datan(1.)

       if (atp.eq."ss") then

       phi1(ii,2,corda(i,3))=(180./pi)*datan((x2line(ii,2,corda(i,3))-
     + x1line(ii,2,corda(i,3)))/(z1line(ii,2,corda(i,3))-
     + z2line(ii,2,corda(i,3))))

       if (kaaa.eq.1.and.corda(i,6).eq.1) then
       phi2(ii,2,corda(i,3))=0.
       else
       phi2(ii,2,corda(i,3))=(180./pi)*datan((u(corda(i,7),corda(i,6),19
     + )-u(corda(i,7),corda(i,6),20))/(w(corda(i,7),corda(i,6),20
     + )-w(corda(i,7),corda(i,6),19)))
       end if

       phi2(ii,2,corda(i,3))=rib(corda(i,7),9)

       phi0(ii,2,corda(i,3))=phi1(ii,2,corda(i,3))-phi2(ii,2,corda(i,3))

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      LEVEL 2 Si tres nivells
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (corda(i,5).eq.3) then ! tres nivells

       xcorda(i,1)=x2line(corda(i,1),1,1)
       ycorda(i,1)=y2line(corda(i,1),1,1)
       zcorda(i,1)=z2line(corda(i,1),1,1)
                    
       dist=dsqrt((xcorda(i,3)-xcorda(i,1))**2+(ycorda(i,3)-
     + ycorda(i,1))**2+(zcorda(i,3)-zcorda(i,1))**2)

       cdl=(xcorda(i,3)-xcorda(i,1))/dist
       cdm=(ycorda(i,3)-ycorda(i,1))/dist
       cdn=(zcorda(i,3)-zcorda(i,1))/dist

c      Parametre necesari a la distancia objectiu

       d13=dsqrt((xcorda(i,3)-xcorda(i,1))**2+(ycorda(i,3)-ycorda(i,1))
     + **2+(zcorda(i,3)-zcorda(i,1))**2)
       
       t=(d13-raml(3,3))/(sqrt(cdl*cdl+cdm*cdm+cdn*cdn))

c      Punt P2 amb equacio parametrica
       xcorda(i,2)=xcorda(i,1)+cdl*t
       ycorda(i,2)=ycorda(i,1)+cdm*t
       zcorda(i,2)=zcorda(i,1)+cdn*t

       ii=corda(i,1)

       x1line(ii,2,corda(i,3))=xcorda(i,1)
       y1line(ii,2,corda(i,3))=ycorda(i,1)
       z1line(ii,2,corda(i,3))=zcorda(i,1)
       x2line(ii,2,corda(i,3))=xcorda(i,2)
       y2line(ii,2,corda(i,3))=ycorda(i,2)
       z2line(ii,2,corda(i,3))=zcorda(i,2)
       
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      LEVEL 2 Si quatre nivells
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (corda(i,5).eq.4) then ! Quatre nivells

       xcorda(i,1)=x2line(corda(i,1),1,1)
       ycorda(i,1)=y2line(corda(i,1),1,1)
       zcorda(i,1)=z2line(corda(i,1),1,1)
                    
       dist=dsqrt((xcorda(i,3)-xcorda(i,1))**2+(ycorda(i,3)-
     + ycorda(i,1))**2+(zcorda(i,3)-zcorda(i,1))**2)

       cdl=(xcorda(i,3)-xcorda(i,1))/dist
       cdm=(ycorda(i,3)-ycorda(i,1))/dist
       cdn=(zcorda(i,3)-zcorda(i,1))/dist

c      Parametre necesari a la distancia objectiu

       d13=dsqrt((xcorda(i,3)-xcorda(i,1))**2+(ycorda(i,3)-ycorda(i,1))
     + **2+(zcorda(i,3)-zcorda(i,1))**2)
       
       t=(d13-raml(4,3))/(sqrt(cdl*cdl+cdm*cdm+cdn*cdn))

c      Punt P2 amb equacio parametrica
       xcorda(i,2)=xcorda(i,1)+cdl*t
       ycorda(i,2)=ycorda(i,1)+cdm*t
       zcorda(i,2)=zcorda(i,1)+cdn*t

       ii=corda(i,1)

       x1line(ii,2,corda(i,3))=xcorda(i,1)
       y1line(ii,2,corda(i,3))=ycorda(i,1)
       z1line(ii,2,corda(i,3))=zcorda(i,1)
       x2line(ii,2,corda(i,3))=xcorda(i,2)
       y2line(ii,2,corda(i,3))=ycorda(i,2)
       z2line(ii,2,corda(i,3))=zcorda(i,2)

       end if

       end if

       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      12.6.3 LEVEL 3
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Inici al final de les cordes 2

       do i=1,cordam

       if (corda(i,2).eq.3) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Si només 3 nivells
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (corda(i,5).eq.3) then !directe a l'ancoratge

c      Explora mc i troba quin origen emprar
       do kk=1,slp
       do k=1,cam(kk)

       if (corda(i,2).eq.mc(kk,k,6).and.mc(kk,k,7).eq.corda(i,3)) then

       comp1(kk)=mc(kk,k,4)
       comp2(kk)=mc(kk,k,5)

       end if

       end do
       
       end do

c      Origen
       xcorda(i,1)=x2line(corda(i,1),2,int(comp2(corda(i,1))))
       ycorda(i,1)=y2line(corda(i,1),2,int(comp2(corda(i,1))))
       zcorda(i,1)=z2line(corda(i,1),2,int(comp2(corda(i,1))))

c      Final
       xcorda(i,2)=u(corda(i,7),corda(i,6),19)
       ycorda(i,2)=v(corda(i,7),corda(i,6),19)
       zcorda(i,2)=w(corda(i,7),corda(i,6),19)

       ii=corda(i,1)

       x1line(ii,3,corda(i,3))=xcorda(i,1)
       y1line(ii,3,corda(i,3))=ycorda(i,1)
       z1line(ii,3,corda(i,3))=zcorda(i,1)
       x2line(ii,3,corda(i,3))=xcorda(i,2)
       y2line(ii,3,corda(i,3))=ycorda(i,2)
       z2line(ii,3,corda(i,3))=zcorda(i,2)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Correccions necessaries a parapents ss (a nivell 3)
c      Calcula angles de gir phi0=phi1-phi2 a aplicar a triangles ss
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Per que es perd el valor de pi?
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       pi=4.0d0*datan(1.)

       if (atp.eq."ss") then

       phi1(ii,3,corda(i,3))=(180./pi)*datan((x2line(ii,3,corda(i,3))-
     + x1line(ii,3,corda(i,3)))/(z1line(ii,3,corda(i,3))-
     + z2line(ii,3,corda(i,3))))

       if (kaaa.eq.1.and.corda(i,6).eq.1) then
       phi2(ii,3,corda(i,3))=0.
       else
       phi2(ii,3,corda(i,3))=(180./pi)*datan((u(corda(i,7),corda(i,6),19
     + )-u(corda(i,7),corda(i,6),20))/(w(corda(i,7),corda(i,6),20
     + )-w(corda(i,7),corda(i,6),19)))
       end if

       phi2(ii,3,corda(i,3))=rib(corda(i,7),9)

       phi0(ii,3,corda(i,3))=phi1(ii,3,corda(i,3))-phi2(ii,3,corda(i,3))

c       write (*,*) ii, corda(i,2), corda(i,3), "phi0= ", 
c     + phi0(ii,3,corda(i,3))

c       write (*,*) "girs ",i,corda(i,1),corda(i,3),phi1(ii,4,corda(i,3))
c     + ,phi2(ii,4,corda(i,3)),phi0(ii,4,corda(i,3))

c      Fi angles de gir

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
       end if

c      Si hi ha quatre nivells
       if (corda(i,5).eq.4) then

c      Explora mc i troba quin origen emprar
       do kk=1,slp
       do k=1,cam(kk)

       if (corda(i,2).eq.mc(kk,k,6).and.mc(kk,k,7).eq.corda(i,3)) then

       comp1(kk)=mc(kk,k,4)
       comp2(kk)=mc(kk,k,5)

       end if

       end do
       
       end do

c      Origen
       xcorda(i,1)=x2line(corda(i,1),2,int(comp2(corda(i,1))))
       ycorda(i,1)=y2line(corda(i,1),2,int(comp2(corda(i,1))))
       zcorda(i,1)=z2line(corda(i,1),2,int(comp2(corda(i,1))))

       dist=dsqrt((xcorda(i,3)-xcorda(i,1))**2+(ycorda(i,3)-
     + ycorda(i,1))**2+(zcorda(i,3)-zcorda(i,1))**2)

       cdl=(xcorda(i,3)-xcorda(i,1))/dist
       cdm=(ycorda(i,3)-ycorda(i,1))/dist
       cdn=(zcorda(i,3)-zcorda(i,1))/dist

c      Parametre necesari a la distancia objectiu

       d13=dsqrt((xcorda(i,3)-xcorda(i,1))**2+(ycorda(i,3)-ycorda(i,1))
     + **2+(zcorda(i,3)-zcorda(i,1))**2)
       
c       t=(raml(4,4)-dl3)/(sqrt(cdl*cdl+cdm*cdm+cdn*cdn))
        
        t=(raml(4,3)-raml(4,4))/(sqrt(cdl*cdl+cdm*cdm+cdn*cdn))

c      Punt P2 amb equacio parametrica
       xcorda(i,2)=xcorda(i,1)+cdl*t
       ycorda(i,2)=ycorda(i,1)+cdm*t
       zcorda(i,2)=zcorda(i,1)+cdn*t

       ii=corda(i,1)

       x1line(ii,3,corda(i,3))=xcorda(i,1)
       y1line(ii,3,corda(i,3))=ycorda(i,1)
       z1line(ii,3,corda(i,3))=zcorda(i,1)
       x2line(ii,3,corda(i,3))=xcorda(i,2)
       y2line(ii,3,corda(i,3))=ycorda(i,2)
       z2line(ii,3,corda(i,3))=zcorda(i,2)
       
       end if

       end if

       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     12.6.4 LEVEL 4
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Inici al final de les cordes 3

       do i=1,cordam

       if (corda(i,2).eq.4) then

       if (corda(i,5).eq.4) then !directe a l'ancoratge

c      Explora mc i troba quin origen emprar
       do kk=1,slp
       do k=1,cam(kk)

       if (corda(i,2).eq.mc(kk,k,8).and.mc(kk,k,9).eq.corda(i,3)) then

       comp1(kk)=mc(kk,k,6)
       comp2(kk)=mc(kk,k,7)

       end if

       end do
       
       end do

c      Origen
       xcorda(i,1)=x2line(corda(i,1),3,int(comp2(corda(i,1))))
       ycorda(i,1)=y2line(corda(i,1),3,int(comp2(corda(i,1))))
       zcorda(i,1)=z2line(corda(i,1),3,int(comp2(corda(i,1))))

c      Final als ancoratges (parapents ds)
       xcorda(i,2)=u(corda(i,7),corda(i,6),19)
       ycorda(i,2)=v(corda(i,7),corda(i,6),19)
       zcorda(i,2)=w(corda(i,7),corda(i,6),19)

       ii=corda(i,1)

       x1line(ii,4,corda(i,3))=xcorda(i,1)
       y1line(ii,4,corda(i,3))=ycorda(i,1)
       z1line(ii,4,corda(i,3))=zcorda(i,1)
       x2line(ii,4,corda(i,3))=xcorda(i,2)
       y2line(ii,4,corda(i,3))=ycorda(i,2)
       z2line(ii,4,corda(i,3))=zcorda(i,2)


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Correccions necessaries a parapents ss (a nivell 4)
c      Calcula angles de gir phi0=phi1-phi2 a aplicar a triangles ss
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Per que es perd el valor de pi?
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       pi=4.0d0*datan(1.)

       if (atp.eq."ss") then

       phi1(ii,4,corda(i,3))=(180./pi)*datan((x2line(ii,4,corda(i,3))-
     + x1line(ii,4,corda(i,3)))/(z1line(ii,4,corda(i,3))-
     + z2line(ii,4,corda(i,3))))

       if (kaaa.eq.1.and.corda(i,6).eq.1) then
       phi2(ii,4,corda(i,3))=0.
       else
       phi2(ii,4,corda(i,3))=(180./pi)*datan((u(corda(i,7),corda(i,6),19
     + )-u(corda(i,7),corda(i,6),20))/(w(corda(i,7),corda(i,6),20
     + )-w(corda(i,7),corda(i,6),19)))
       end if

       phi2(ii,4,corda(i,3))=rib(corda(i,7),9)

       phi0(ii,4,corda(i,3))=phi1(ii,4,corda(i,3))-phi2(ii,4,corda(i,3))

c       write (*,*) "girs ",i,corda(i,1),corda(i,3),phi1(ii,4,corda(i,3))
c     + ,phi2(ii,4,corda(i,3)),phi0(ii,4,corda(i,3))

c      Fi angles de gir

       end if
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
       end if

       end if

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     12.7 Gir dels triangles parapents ss
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      calque = "triangles" (Note Ludovic G. 2018-05-12)

       if (atp.eq."ss") then

c      Verify thickness of last rib (escrit ja a 11.3)
       
       xsum=0.
       ic=0
       i=nribss
       do j=1,np(i,1)
       xsum=xsum+abs(v(i,j,3))
       end do
       if (xsum.ne.0.0) then
       ic=1
       end if

c      Calcula punt 4 situat al triangle 1-2-3

c      Recorre punts de l'extrados

       do i=1,cordam

c      No estabilo
       if (corda(i,7).ne.nribss*float(ic-1)*float(ic-1)) then

       ii=corda(i,1)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      12.7.1 Gir cordes en quart nivell
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Gir en cordes de tercer nivell
       if (corda(i,2).eq.4) then

c      write (*,*) "corda(i,2)=", corda(i,2), corda(i,5)

       if (corda(i,5).eq.4) then !directe a l'ancoratge

       jp=anccont(corda(i,7),corda(i,6))

c      MODIF Ludovic G. 2018-05-12
c      Note: do the same at "gir cordes tercer nivell"
c      New parameter idelta is a function of extradps points np(i,2)

c      idelta=np(i,2)*6/100  ! Ludovic
c      Warnig!!! BHL2-16 fails using previous!
       idelta=3 ! Previous definition by Pere

       xpt1=x(corda(i,7),jp-idelta)
       ypt1=y(corda(i,7),jp-idelta)
       zpt1=z(corda(i,7),jp-idelta)

       xpt2=x(corda(i,7),jp+idelta)
       ypt2=y(corda(i,7),jp+idelta)
       zpt2=z(corda(i,7),jp+idelta)

       xpt3=u(corda(i,7),corda(i,6),20)
       ypt3=v(corda(i,7),corda(i,6),20)
       zpt3=w(corda(i,7),corda(i,6),20)

c      Dibuixa els triangles a girar

       call line3d(xpt1,ypt1,zpt1,xpt2,ypt2,zpt2,1)
       call line3d(xpt1,ypt1,zpt1,xpt3,ypt3,zpt3,1)
       call line3d(xpt2,ypt2,zpt2,xpt3,ypt3,zpt3,1)

c      Calcula punt 4

c      Cosinus directors rectes 2-1 i 3-1

       xd21=dsqrt((xpt1-xpt2)**2.+(ypt1-ypt2)**2.+(zpt1-zpt2)**2.)
       xd31=dsqrt((xpt1-xpt3)**2.+(ypt1-ypt3)**2.+(zpt1-zpt3)**2.)
       xd32=dsqrt((xpt2-xpt3)**2.+(ypt2-ypt3)**2.+(zpt2-zpt3)**2.)

       cl21=(xpt1-xpt2)/xd21
       cm21=(ypt1-ypt2)/xd21
       cn21=(zpt1-zpt2)/xd21

       cl31=(xpt1-xpt3)/xd31
       cm31=(ypt1-ypt3)/xd31
       cn31=(zpt1-zpt3)/xd31

c      Punt 4 a recta que passa per 1-2 parametrica xt

       xt=xd31*(cl21*cl21+cm21*cm21+cn21*cn21)*
     + (cl21*cl31+cm21*cm31+cn21*cn31)

       xpt4=xpt1-cl21*xt
       ypt4=ypt1-cm21*xt
       zpt4=zpt1-cn21*xt

       call line3d(xpt4,ypt4,zpt4,xpt3,ypt3,zpt3,7)

       xd41=dsqrt((xpt1-xpt4)**2.+(ypt1-ypt4)**2.+(zpt1-zpt4)**2.)
      
       cl41=(xpt1-xpt4)/xd41
       cm41=(ypt1-ypt4)/xd41
       cn41=(zpt1-zpt4)/xd41

       xd43=dsqrt((xpt3-xpt4)**2.+(ypt3-ypt4)**2.+(zpt3-zpt4)**2.)

       cl43=(xpt3-xpt4)/xd43
       cm43=(ypt3-ypt4)/xd43
       cn43=(zpt3-zpt4)/xd43

c      Planol 1-2-3 (determinant)

       A1=(ypt2-ypt1)*(zpt3-zpt1)-(zpt2-zpt1)*(ypt3-ypt1)
       B1=(zpt2-zpt1)*(xpt3-xpt1)-(xpt2-xpt1)*(zpt3-zpt1)
       C1=(xpt2-xpt1)*(ypt3-ypt1)-(ypt2-ypt1)*(xpt3-xpt1)
       D1=-A1*xpt1-B1*ypt1-C1*zpt1

c      Punt 5 situat a la recta normal a 1-2-3 per 4

       xpt5=xpt4+A1*0.1
       ypt5=ypt4+B1*0.1
       zpt5=zpt4+C1*0.1

c      Eix normal al pla 1-2-3
c       call line3d(xpt4,ypt4,zpt4,xpt5,ypt5,zpt5,1)

       xd45=dsqrt((xpt5-xpt4)**2.+(ypt5-ypt4)**2.+(zpt5-zpt4)**2.)

       cl45=(xpt5-xpt4)/xd45
       cm45=(ypt5-ypt4)/xd45
       cn45=(zpt5-zpt4)/xd45

c       phi0(ii,corda(i,2),corda(i,3))=0.

       xptp6=xd43*dsin((pi/180.)*phi0(ii,4,corda(i,3)))
       yptp6=0.
       zptp6=-xd43*dcos((pi/180.)*phi0(ii,4,corda(i,3)))

c      "-" sign before cn41 in ypt6 be aware, review!!! however works!

       xpt6=cl45*xptp6+cm45*yptp6-cn45*zptp6+xpt4
       ypt6=cl41*xptp6+cm41*yptp6+cn41*zptp6+ypt4
       zpt6=cl43*xptp6+cm43*yptp6-cn43*zptp6+zpt4

c      Truc per ajustar posicio Y !!!!!!!!!!!!!!!!!
c      Funciona força be
c      REVISAR !!!!!!!!!!!!!!!!!!!!!!!!!!!!
c      ypt6=ypt3

c       write (*,*) "ypt6 ypt4 phi0 ", ypt6, ypt4, 
c     + phi0(ii,4,corda(i,3))

c      Segona definició, per recta perpendicular al pla 1-2-3
 
       xt=xd43*dcos((pi/180.)*phi0(ii,4,corda(i,3)))
       xpt7=xpt4+cl43*xt
       ypt7=ypt4+cm43*xt
       zpt7=zpt4+cn43*xt
c       call line3d(0.,0.,0.,xpt7,ypt7,zpt7,5)

       xt=(((xd43*xd43*(dsin((pi/180.)*phi0(ii,4,corda(i,3)))))/
     + (sqrt(A1*A1+B1*B1+C1*C1))))

c       write (*,*) "xt ", xt

c       xpt6=xpt7+A1*xt
c       ypt6=ypt7+B1*xt
c       zpt6=zpt7+C1*xt

***************************************************************
*      Codi introduit per Ludovic G. en data 20180419
*      Jo he de revisar
*      Merci!
***************************************************************

       iv=corda(i,7)
       if (rib(iv,56).eq.0.) then
       xpt6=u(corda(i,7),corda(i,6),19)
       ypt6=v(corda(i,7),corda(i,6),19)
       zpt6=w(corda(i,7),corda(i,6),19)
       end if

***************************************************************

c      Dibuixa triangles girats

       if (kaaa.eq.0.or.corda(i,6).ne.1) then
       call line3d(xpt3,ypt3,zpt3,xpt6,ypt6,zpt6,1)
       call line3d(xpt1,ypt1,zpt1,xpt6,ypt6,zpt6,2)
       call line3d(xpt2,ypt2,zpt2,xpt6,ypt6,zpt6,2)
       end if

       u(corda(i,7),corda(i,6),19)=xpt6
       v(corda(i,7),corda(i,6),19)=ypt6
       w(corda(i,7),corda(i,6),19)=zpt6

c      No gira b.a. si kaaa=1

       if (kaaa.eq.1.and.corda(i,6).eq.1) then
       u(corda(i,7),corda(i,6),19)=u(corda(i,7),corda(i,6),20)
       v(corda(i,7),corda(i,6),19)=v(corda(i,7),corda(i,6),20)
       w(corda(i,7),corda(i,6),19)=w(corda(i,7),corda(i,6),20)
       end if


       end if

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      12.7.2 Gir cordes en tercer nivell
c      opcionalment es podria fer servir rutina anterior amb corda(i,2)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Gir en cordes de tercer nivell
       if (corda(i,2).eq.3) then

c      write (*,*) "corda(i,2)=", corda(i,2), corda(i,5)

       if (corda(i,5).eq.3) then !directe a l'ancoratge

       jp=anccont(corda(i,7),corda(i,6))

       xpt1=x(corda(i,7),jp-3)
       ypt1=y(corda(i,7),jp-3)
       zpt1=z(corda(i,7),jp-3)

       xpt2=x(corda(i,7),jp+3)
       ypt2=y(corda(i,7),jp+3)
       zpt2=z(corda(i,7),jp+3)

       xpt3=u(corda(i,7),corda(i,6),20)
       ypt3=v(corda(i,7),corda(i,6),20)
       zpt3=w(corda(i,7),corda(i,6),20)

c      Dibuixa els triangles a girar

       call line3d(xpt1,ypt1,zpt1,xpt2,ypt2,zpt2,1)
       call line3d(xpt1,ypt1,zpt1,xpt3,ypt3,zpt3,1)
       call line3d(xpt2,ypt2,zpt2,xpt3,ypt3,zpt3,1)

c      Calcula punt 4

c      Cosinus directors rectes 2-1 i 3-1

       xd21=dsqrt((xpt1-xpt2)**2.+(ypt1-ypt2)**2.+(zpt1-zpt2)**2.)
       xd31=dsqrt((xpt1-xpt3)**2.+(ypt1-ypt3)**2.+(zpt1-zpt3)**2.)
       xd32=dsqrt((xpt2-xpt3)**2.+(ypt2-ypt3)**2.+(zpt2-zpt3)**2.)

       cl21=(xpt1-xpt2)/xd21
       cm21=(ypt1-ypt2)/xd21
       cn21=(zpt1-zpt2)/xd21

       cl31=(xpt1-xpt3)/xd31
       cm31=(ypt1-ypt3)/xd31
       cn31=(zpt1-zpt3)/xd31

c      Punt 4 a recta que passa per 1-2 parametrica xt

       xt=xd31*(cl21*cl21+cm21*cm21+cn21*cn21)*
     + (cl21*cl31+cm21*cm31+cn21*cn31)

       xpt4=xpt1-cl21*xt
       ypt4=ypt1-cm21*xt
       zpt4=zpt1-cn21*xt

       call line3d(xpt4,ypt4,zpt4,xpt3,ypt3,zpt3,7)

       xd41=dsqrt((xpt1-xpt4)**2.+(ypt1-ypt4)**2.+(zpt1-zpt4)**2.)
       xd43=dsqrt((xpt3-xpt4)**2.+(ypt3-ypt4)**2.+(zpt3-zpt4)**2.)

       cl43=(xpt3-xpt4)/xd43
       cm43=(ypt3-ypt4)/xd43
       cn43=(zpt3-zpt4)/xd43
      
c      Planol 1-2-3 (determinant)

       A1=(ypt2-ypt1)*(zpt3-zpt1)-(zpt2-zpt1)*(ypt3-ypt1)
       B1=(zpt2-zpt1)*(xpt3-xpt1)-(xpt2-xpt1)*(zpt3-zpt1)
       C1=(xpt2-xpt1)*(ypt3-ypt1)-(ypt2-ypt1)*(xpt3-xpt1)
       D1=-A1*xpt1-B1*ypt1-C1*zpt1

c      Punt 5 situat a la recta normal a 1-2-3 per 4

       xpt5=xpt4+A1*1.
       ypt5=ypt4+B1*1.
       zpt5=zpt4+C1*1.

c       call line3d(xpt4,ypt4,zpt4,xpt5,ypt5,zpt5,4)

       xd45=dsqrt((xpt5-xpt4)**2.+(ypt5-ypt4)**2.+(zpt5-zpt4)**2.)

       cl45=(xpt5-xpt4)/xd45
       cm45=(ypt5-ypt4)/xd45
       cn45=(zpt5-zpt4)/xd45

c      Transformacio de coordenades per traslacio i rotacio

       xptp6=xd43*dsin((pi/180.)*phi0(ii,3,corda(i,3)))
       yptp6=0.
       zptp6=xd43*dcos((pi/180.)*phi0(ii,3,corda(i,3)))

       xpt6=cl45*xptp6+cm45*yptp6+cn45*zptp6+xpt4
       ypt6=cl21*xptp6+cm21*yptp6-cn21*zptp6+ypt4
       zpt6=cl43*xptp6+cm43*yptp6+cn43*zptp6+zpt4


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      ESPECIAL BHL-PAMPA
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c       iv=corda(i,7)
c       if (iv.eq.0.or.iv.eq.1.or.iv.eq.4.or.iv.eq.5.or.iv.eq.8.or.
c     + iv.eq.9) then
c       xpt6=u(corda(i,7),corda(i,6),19)
c       ypt6=v(corda(i,7),corda(i,6),19)
c       zpt6=w(corda(i,7),corda(i,6),19)
c
c       end if

       iv=corda(i,7)
       if (rib(iv,56).eq.0.) then
       xpt6=u(corda(i,7),corda(i,6),19)
       ypt6=v(corda(i,7),corda(i,6),19)
       zpt6=w(corda(i,7),corda(i,6),19)
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      ESPECIAL BHL-PAMPA
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c      Dibuixa triangles girats

       if (kaaa.eq.0.or.corda(i,6).ne.1) then
       call line3d(xpt3,ypt3,zpt3,xpt6,ypt6,zpt6,1)
       call line3d(xpt1,ypt1,zpt1,xpt6,ypt6,zpt6,2)
       call line3d(xpt2,ypt2,zpt2,xpt6,ypt6,zpt6,2)
       end if

       u(corda(i,7),corda(i,6),19)=xpt6
       v(corda(i,7),corda(i,6),19)=ypt6
       w(corda(i,7),corda(i,6),19)=zpt6

c      No gira b.a. si kaaa=1

       if (kaaa.eq.1.and.corda(i,6).eq.1) then
       u(corda(i,7),corda(i,6),19)=u(corda(i,7),corda(i,6),20)
       v(corda(i,7),corda(i,6),19)=v(corda(i,7),corda(i,6),20)
       w(corda(i,7),corda(i,6),19)=w(corda(i,7),corda(i,6),20)
       end if


       end if

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       end if

       end do

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     12.8 Longituds de les cordes
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=1,cordam

       xx2=x2line(corda(i,1),corda(i,2),corda(i,3))
       xx1=x1line(corda(i,1),corda(i,2),corda(i,3))
       yy2=y2line(corda(i,1),corda(i,2),corda(i,3))
       yy1=y1line(corda(i,1),corda(i,2),corda(i,3))
       zz2=z2line(corda(i,1),corda(i,2),corda(i,3))
       zz1=z1line(corda(i,1),corda(i,2),corda(i,3))

c      Si parapent ss actualitza posicio ancoratges
       if (atp.eq."ss") then

c      Actualitza nivell 4
       if (corda(i,2).eq.4.and.corda(i,5).eq.4) then

       xx2=u(corda(i,7),corda(i,6),19)
       yy2=v(corda(i,7),corda(i,6),19)
       zz2=w(corda(i,7),corda(i,6),19)

       x2line(corda(i,1),corda(i,2),corda(i,3))=xx2
       y2line(corda(i,1),corda(i,2),corda(i,3))=yy2
       z2line(corda(i,1),corda(i,2),corda(i,3))=zz2

       end if     

c      Actualitza nivell 3
       if (corda(i,2).eq.3.and.corda(i,5).eq.3) then

       xx2=u(corda(i,7),corda(i,6),19)
       yy2=v(corda(i,7),corda(i,6),19)
       zz2=w(corda(i,7),corda(i,6),19)

       x2line(corda(i,1),corda(i,2),corda(i,3))=xx2
       y2line(corda(i,1),corda(i,2),corda(i,3))=yy2
       z2line(corda(i,1),corda(i,2),corda(i,3))=zz2

       end if     

c      Actualitza nivell 2
       if (corda(i,2).eq.2.and.corda(i,5).eq.2) then

       xx2=u(corda(i,7),corda(i,6),19)
       yy2=v(corda(i,7),corda(i,6),19)
       zz2=w(corda(i,7),corda(i,6),19)

       x2line(corda(i,1),corda(i,2),corda(i,3))=xx2
       y2line(corda(i,1),corda(i,2),corda(i,3))=yy2
       z2line(corda(i,1),corda(i,2),corda(i,3))=zz2

       end if

       end if

       xline(i)=dsqrt((xx2-xx1)**2+(yy2-yy1)**2+(zz2-zz1)**2)
       xline2(i)=dsqrt((xcorda(i,2)-xcorda(i,1))**2+
     + (ycorda(i,2)-ycorda(i,1))**2+(zcorda(i,2)-zcorda(i,1))**2)

c      write (*,*) "xline, xline2 ",i,xline(i),xline2(i)

c      write (*,*) "loads ",i, xline(i), xload(i)

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     12.9 Correccions elastiques
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c       write(*,*) "i, xline(i), corda(i,1), corda(i,2), csus(corda(i,1),
c     + corda(i,2)), xload(i), xlide(i)"

       cttt2=0.
       cttt3=0.
       cttt4=0.

       do i=1,cordam

       xlide(i)=xline(i)*(xload(i)/10.)*csus(corda(i,1),corda(i,2))/100.

       if (corda(i,2).eq.1) then
       xlide(i)=0.
       end if

       xlifi(i)=xline(i)-xlide(i)

       if (corda(i,2).eq.2) then
       cttt2=cttt2+xload(i)
       end if

       if (corda(i,2).eq.3) then
       cttt3=cttt3+xload(i)
       end if

       if (corda(i,2).eq.4) then
       cttt4=cttt4+xload(i)
       end if

       end do

c       write (*,*) "Carrega total: ", cttt2*2., cttt3*2., cttt4*2.

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     12.10 Dibuixar cordes 2D
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       corda1=cordam

       do i=1,corda1

       x0=(1260.-160.)*xkf
       y0=1800.*xkf

       x00=1260.*xkf*float(corda(i,1)-1)

c      Colors definition (not used since 2.70 version!)
c      Use iccolor(i) vector
       if (corda(i,1).eq.1) then
       icc=1 ! line color
       end if
       if (corda(i,1).eq.2) then
       icc=30 ! line color
       end if
       if (corda(i,1).eq.3) then
       icc=3 ! line color
       end if
       if (corda(i,1).eq.4) then
       icc=4 ! line color
       end if
       if (corda(i,1).eq.5) then
       icc=5 ! line color
       end if
       if (corda(i,1).eq.6) then
       icc=6 ! line color
       end if

       if (i37c.eq.0) then ! Use colors by risers
       call line(x1line(corda(i,1),corda(i,2),corda(i,3))+x0+x00,
     + z1line(corda(i,1),corda(i,2),corda(i,3))+y0,
     + x2line(corda(i,1),corda(i,2),corda(i,3))+x0+x00,
     + z2line(corda(i,1),corda(i,2),corda(i,3))+y0,iccolor(corda(i,1)))
       end if

       if (i37c.eq.1) then ! Use colors according table 34
       corda(i,40)=corda(i,30+corda(i,2)) ! Type on line i
       iccolor34(i)=int(tyline(corda(i,40),10))
       call line(x1line(corda(i,1),corda(i,2),corda(i,3))+x0+x00,
     + z1line(corda(i,1),corda(i,2),corda(i,3))+y0,
     + x2line(corda(i,1),corda(i,2),corda(i,3))+x0+x00,
     + z2line(corda(i,1),corda(i,2),corda(i,3))+y0,iccolor34(i))
       end if

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
