!> Resolve named/singular rib locations used by lines and reinforcements.
!!
!! Interpolates anchors, intake limits, trailing-edge points, and related
!! construction locations on the prepared profiles.  The resulting absolute
!! and developed coordinates feed suspension, rib marks, and later reports.
c      9. SINGULAR RIB POINTS
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       
c        write (*,*) "pi 9. =",pi

c      9.1 Compute anchor points

c      Slot 6 is the restored rib-local profile plane.  A--E activity is
c      governed by rib(:,15), while this interpolation also prepares F at
c      ordinal 6 from the independent brake percentage rib(:,21).  Stage 6's
c      columns 111:115/121:125 cover only the active A--E prefix; Stage 12.3
c      later places that same prefix in absolute spatial slot 19.

       do i=0,nribss         ! Itera in ribs

       do k=1,6 ! Itera in A,B,C,D,E,F

       do j=1,np(i,1)-1

c       if(u(i,j,3).le.rib(i,5)*rib(i,15+k)/100.and.u(i,j+1,3)
c     + .ge.rib(i,5)*rib(i,15+k)/100.and.j.ge.np(i,2)) then

c      Please, don't compare directly two double precision numbers !!!

       if(real(u(i,j,3)).le.real(rib(i,5)*rib(i,15+k)/100).and.
     + real(u(i,j+1,3)).ge.real(rib(i,5)*rib(i,15+k)/100).and.
     + j.ge.np(i,2)) then

       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xn=v(i,j,3)-xm*u(i,j,3)
       u(i,k,6)=rib(i,5)*rib(i,15+k)/100.0d0
       v(i,k,6)=xm*u(i,k,6)+xn

c       anccount(i,k)=j  ! anchor "j" detection in i rib

c       write (*,*) "Anchor control i,k,j = ",i,k,anccont(i,k)


       end if

       end do

       end do

       end do

c      9.1+ Detecta punts j a extrados propers a ancoratges

       
c        write (*,*) "pi 9.1+ =",pi

       do i=0,nribss         ! Itera in ribs

       do k=1,6 ! Itera in A,B,C,D,E,F

       do j=np(i,2),2,-1

       if(u(i,j,3).le.rib(i,5)*rib(i,15+k)/100..and.u(i,j-1,3)
     + .ge.rib(i,5)*rib(i,15+k)/100..and.j.le.np(i,2)) then

       anccont(i,k)=j

       end if

       end do

       end do

       end do


c      9.1++ Detecta punts j a intrados propers a ancoratges
c      Atenció casos ds i ss (ESTUDIAR)


       do i=0,nribss         ! Itera in ribs

       do k=1,6 ! Itera in A,B,C,D,E,F

       do j=1,np(i,1)-1

       if(u(i,j,3).le.rib(i,5)*rib(i,15+k)/100..and.u(i,j+1,3)
     + .ge.rib(i,5)*rib(i,15+k)/100..and.u(i,j,3).le.u(i,j+1,3)) then

       ancconti(i,k)=j

       end if

       end do

       end do

       end do
       

c      9.2 Compute inlets points

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      ENTRE 9.1+ i 9.2 es perd el valor de pi INEXPLICABLE
       pi=4.0d0*datan(1.0d0)   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c        write (*,*) "pi 9.2. =",pi

       do i=1,nribss         ! Itera in ribs

       do k=11,12 ! air in out

       do j=1,np(i,1)

       if(u(i,j,3).le.rib(i,5)*rib(i,k)/100.0d0.and.u(i,j+1,3)
     + .ge.rib(i,5)*rib(i,k)/100.0d0.and.j.ge.np(i,2)-1) then

       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xn=v(i,j,3)-xm*u(i,j,3)
       u(i,k-4,6)=rib(i,5)*rib(i,k)/100.0d0
       v(i,k-4,6)=xm*u(i,k,6)+xn

       end if

       end do

       end do

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      9.3 Draw anchor points in airfoils
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       
c        write (*,*) "pi 9.3 =",pi

c      Box (1,2)

       sepxx=700.*xkf
       sepyy=100.*xkf

       kx=0
       ky=0
       kyy=0

       do i=1,nribss

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky)


c      Normal points A,B,C,D,E,F
       do j=1,6

c      Added case points brakes F j=6 (request from Eric Fontaine)
       if (j.le.rib(i,15).or.(j.eq.6.and.rib(i,21).lt.100.and.
     + rib(i,21).gt.0.)) then

c      9.3.1 Anchor points
       call line(sepx+u(i,j,6),-v(i,j,6)+sepy,sepx+u(i,j,6),
     + -0.+sepy,7)

c      9.3.2 Anchor points in mesa de corte

c      Case 1: classic 3 orange points
       if (typetab.eq.1) then
       call point(2530.*xkf+sepx+u(i,j,6),-v(i,j,6)+
     + sepy+typm6(5),30)
       call point(2530.*xkf+sepx+u(i,j,6),-v(i,j,6)+
     + sepy-1.*typm5(5)+typm6(5),30)
       call point(2530.*xkf+sepx+u(i,j,6),-v(i,j,6)+
     + sepy-2.*typm5(5)+typm6(5),30)
       end if

c      Case 2: rotated 3 orange points
       if (typetab.eq.2) then
c      Define "ji" of the anchor in the other "j" count 1,2,3,4,5
       ji=ancconti(i,j)  ! Compte, aqui "j" és "k"
       alpha1=datan((v(i,ji-1,3)-v(i,ji+1,3))/(u(i,ji-1,3)-u(i,ji+1,3)))
c      Amb desplaçament typm6(5) xdes com a intra
       xpeq=typm6(5)*dsin(alpha1)
       ypeq=typm6(5)*dcos(alpha1)
       call point(2530.*xkf+sepx+u(i,j,6)+xpeq,-v(i,j,6)+
     + sepy+ypeq,30)
       call point(2530.*xkf+sepx+u(i,j,6)+
     + (typm6(5)-1.*typm5(5))*dsin(alpha1),-v(i,j,6)+
     + sepy+(typm6(5)-1.*typm5(5))*dcos(alpha1),30)
       call point(2530.*xkf+sepx+u(i,j,6)+
     + (typm6(5)-2.*typm5(5))*dsin(alpha1),-v(i,j,6)+
     + sepy+(typm6(5)-2.*typm5(5))*dcos(alpha1),30)
       end if

c      Case 3: triangle 2 mm
c      REVISAR CAS ss !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       if (typetab.eq.3) then
c      Define "ji" of the anchor in the other "j" count 1,2,3,4,5
       ji=ancconti(i,j)  ! Compte, aqui "j" és "k"
       alpha1=datan((v(i,ji-1,3)-v(i,ji+1,3))/(u(i,ji-1,3)-u(i,ji+1,3)))
c      Amb desplaçament typm6(5) xdes com a intra
       xpeq=typm6(5)*dsin(alpha1)
       ypeq=typm6(5)*dcos(alpha1)
       call mtriangle(2530.*xkf+sepx+u(i,j,6)+xpeq,-v(i,j,6)+sepy+ypeq,
     + typm5(5),alpha1,30)
       end if

       end if ! Cases control
       end do ! Points A,B,C,D,E,F


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      9.4 Marca entrada aire a les costelles
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      9.4.1 Cases 0,-1,-2,-3,-4,-6 draw normal inlet LE
       if (rib(i,165).eq.0.or.rib(i,165).eq.-1.or.rib(i,165).eq.-2.or.
     + rib(i,165).eq.-3.or.rib(i,165).eq.-4.or.rib(i,165).eq.-6) then

       j=np(i,2)
       alpha1=pi+datan((v(i,j,3)-v(i,j-1,3))/(u(i,j,3)-u(i,j-1,3)))

       call line(sepx+u(i,j,3),-v(i,j,3)+sepy,sepx+u(i,j,3)-
     + 4.*dsin(alpha1),-v(i,j,3)-4.*dcos(alpha1)+sepy,1)

       x1=2530.*xkf+sepx+u(i,j,3)+0.1*typm6(4)*dsin(alpha1)
       y1=-v(i,j,3)+sepy+0.1*typm6(4)*dcos(alpha1)
       x2=2530.*xkf+sepx+u(i,j,3)-0.1*(typm5(4)-typm6(4))*dsin(alpha1)
       y2=-0.1*(typm5(4)-typm6(4))*dcos(alpha1)-v(i,j,3)+sepy

c      Laser points
       if (typevent.eq.1) then
c      Double point
       call point(x1,y1,3)
       call point(x2,y2,3)
       end if

       if (typevent.eq.2) then
c      Segment
       call linevent(x1,y1,x2,y2,3)
       end if

       if (typevent.eq.3) then
c      Segment 101
       call segment101(x1,y1,x2,y2,3)
       end if

       end if  ! case not 4

c      9.4.2 Case 4,5 draw special displaced LE inlet
       if (rib(i,165).eq.4.or.rib(i,165).eq.5) then

c      Load inlet polyline
       jini=np(i,2)
       jfin=np(i,2)+np(i,3)-1
       do j=jini,jfin
       useg(j)=u(i,j,3)
       vseg(j)=v(i,j,3)
       end do
c      First run to compute only polyline length (vent)
       linter=0.0d0
       call interpseg(jini,jfin,jc,ldif,
     + useg,vseg,ltotal,linter,upoint,vpoint)
c      Second run to interpolation
       linter=ltotal*(1.-csi(i,20)*0.01)
       call interpseg(jini,jfin,jc,ldif,
     + useg,vseg,ltotal,linter,upoint,vpoint)

c      Draw segment
       j=jc+1
       alpha1=pi+datan((v(i,j,3)-v(i,j-1,3))/(u(i,j,3)-u(i,j-1,3)))

       call line(sepx+upoint(1),-vpoint(1)+sepy,sepx+upoint(1)-
     + 4.*dsin(alpha1),-vpoint(1)-4.*dcos(alpha1)+sepy,30)

       x1=2530.*xkf+sepx+upoint(1)+0.1*typm6(4)*dsin(alpha1)
       y1=-vpoint(1)+sepy+0.1*typm6(4)*dcos(alpha1)
       x2=2530.*xkf+sepx+upoint(1)-0.1*(typm5(4)-typm6(4))*dsin(alpha1)
       y2=-0.1*(typm5(4)-typm6(4))*dcos(alpha1)-vpoint(1)+sepy

c      Laser points
       if (typevent.eq.1) then
c      Double point
       call point(x1,y1,3)
       call point(x2,y2,3)
       end if

       if (typevent.eq.2) then
c      Segment
       call linevent(x1,y1,x2,y2,3)
       end if

       if (typevent.eq.3) then
c      Segment 101
       call segment101(x1,y1,x2,y2,3)
       end if

       end if ! Inlet case 4


c      fi inlet (point intrados)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      9.4.3 Cases 0,1,-3,4,6 draw normal TE inlet
       if (rib(i,165).eq.0.or.rib(i,165).eq.1.or.rib(i,165).eq.-3.or.
     + rib(i,165).eq.4.or.rib(i,165).eq.6) then

       j=np(i,2)+np(i,3)-1
       alpha1=pi+datan((v(i,j+1,3)-v(i,j-1,3))/(u(i,j+1,3)-u(i,j-1,3)))

       call line(sepx+u(i,j,3),-v(i,j,3)+sepy,sepx+u(i,j,3)-
     + 4.*dsin(alpha1),-v(i,j,3)-4.*dcos(alpha1)+sepy,1)

       x1=2530.*xkf+sepx+u(i,j,3)+0.1*typm6(4)*dsin(alpha1)
       y1=-v(i,j,3)+sepy+0.1*typm6(4)*dcos(alpha1)
       x2=2530.*xkf+sepx+u(i,j,3)-0.1*(typm5(4)-typm6(4))*dsin(alpha1)
       y2=-0.1*(typm5(4)-typm6(4))*dcos(alpha1)-v(i,j,3)+sepy

c      Laser points
       if (typevent.eq.1) then
c      Double point
       call point(x1,y1,3)
       call point(x2,y2,3)
       end if

       if (typevent.eq.2) then
c      Segment
       call linevent(x1,y1,x2,y2,3)
       end if

       if (typevent.eq.3) then
c      Segment 101
       call segment101(x1,y1,x2,y2,3)
       end if

       end if ! case not -4

c      9.4.4 Case -4,-5 draw special displaced inlet
       if (rib(i,165).eq.-4.or.rib(i,165).eq.-5) then

c      Load inlet polyline
       jini=np(i,2)
       jfin=np(i,2)+np(i,3)-1
       do j=jini,jfin
       useg(j)=u(i,j,3)
       vseg(j)=v(i,j,3)
       end do
c      First run to compute only polyline length (vent)
       linter=0.0d0
       call interpseg(jini,jfin,jc,ldif,
     + useg,vseg,ltotal,linter,upoint,vpoint)
c      Second run to interpolation
       linter=ltotal*(1.-csi(i,20)*0.01)
       call interpseg(jini,jfin,jc,ldif,
     + useg,vseg,ltotal,linter,upoint,vpoint)

c      Draw segment
       j=jc+1
       alpha1=pi+datan((v(i,j,3)-v(i,j-1,3))/(u(i,j,3)-u(i,j-1,3)))
       call line(sepx+upoint(1),-vpoint(1)+sepy,sepx+upoint(1)-
     + 4.*dsin(alpha1),-vpoint(1)-4.*dcos(alpha1)+sepy,30)

       x1=2530.*xkf+sepx+upoint(1)+0.1*typm6(4)*dsin(alpha1)
       y1=-vpoint(1)+sepy+0.1*typm6(4)*dcos(alpha1)
       x2=2530.*xkf+sepx+upoint(1)-0.1*(typm5(4)-typm6(4))*dsin(alpha1)
       y2=-0.1*(typm5(4)-typm6(4))*dcos(alpha1)-vpoint(1)+sepy

c      Laser points
       if (typevent.eq.1) then
c      Double point
       call point(x1,y1,3)
       call point(x2,y2,3)
       end if

       if (typevent.eq.2) then
c      Segment
       call linevent(x1,y1,x2,y2,3)
       end if

       if (typevent.eq.3) then
c      Segment 101
       call segment101(x1,y1,x2,y2,3)
       end if

       end if  ! case -4


c     9.5 Aditional points (junquillos)

       if (narp.ne.0) then ! Only if narp.eq.0 k17d
       do l=1,narp
       call point(2530.*xkf+sepx+xarp(l)*rib(i,5)/100.,
     + -yarp(l)*rib(i,5)/100.+sepy,5)
c       call point(2530.*xkf+sepx+10.8*rib(i,5)/100.,
c     + +5.53*rib(i,5)/100.+sepy,5)
       end do
       end if

       kx=int((float(i)/6.))
       ky=i-kx*6
       kyy=kyy+1

       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      9.9 MIDDLE PANEL UNLOADED RIBS
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Box (1,6)
c      1260.*4.*xkf
       
       sepxx=1260.*4.*xkf+700.*xkf
       sepyy=100.*xkf
c      sepyy=100

       kx=0
       ky=0
       kyy=0

       do i=1,nribss

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*1.0*float(ky)

c      Detect complete unloaded rib
       if (rib(i,56).eq.100.and.atp.ne."ss") then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      9.9.1 Define intermediate airfoil beetwen i-1 and i
c      Interpole chord and thickness
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do j=1,np(i,1)
       u(i,j,43)=u(i-1,j,2)*0.5*(rib(i,5)+rib(i-1,5))/100.
       v(i,j,43)=v(i-1,j,2)*(0.5*(rib(i,149)+rib(i-1,149))/
     + rib(i-1,149))*0.5*(rib(i,5)+rib(i-1,5))/100.
       end do

c      Calculate midline in panel

       do j=1,np(i,1)
       u(i,j,44)=(u(i-1,j,9)+u(i-1,j,10))/2.
       v(i,j,44)=(v(i-1,j,9)+v(i-1,j,10))/2.

c       u(i,j,44)=u(i-1,j,9)
c       v(i,j,44)=v(i-1,j,9)

       end do

c      Compare lengths extra in rib (150) and panels (153)
       
       rib(i,150)=0.
       rib(i,153)=0.
       do j=1,np(i,2)-1    
       rib(i,150)=rib(i,150)+dsqrt((u(i,j,43)-u(i,j+1,43))**2.+
     + (v(i,j,43)-v(i,j+1,43))**2.)
       rib(i,153)=rib(i,153)+dsqrt((u(i,j,44)-u(i,j+1,44))**2.+
     + (v(i,j,44)-v(i,j+1,44))**2.)
       end do
       rib(i,156)=rib(i,150)-rib(i,153)

c      Compare lengths vent in rib (151) and panels (154)
c      9-10 Vent panel still not defined (!)
       
       rib(i,151)=0.
       rib(i,154)=0.
       do j=np(i,2),np(i,2)+np(i,3)-2
       rib(i,151)=rib(i,151)+dsqrt((u(i,j,43)-u(i,j+1,43))**2.+
     + (v(i,j,43)-v(i,j+1,43))**2.)
       rib(i,154)=rib(i,154)+dsqrt((u(i,j,44)-u(i,j+1,44))**2.+
     + (v(i,j,44)-v(i,j+1,44))**2.)
       end do
       rib(i,157)=rib(i,151)-rib(i,154)
       j1=np(i,2)
       j2=np(i,2)+np(i,3)-1
c       rib(i,151)=dsqrt((u(i,j1,44)-u(i,j2,44))**2.+
c     + (v(i,j1,44)-v(i,j2,44))**2.)

c      Compare lengths intra in rib (152) and panels (155)
       
       rib(i,152)=0.
       rib(i,155)=0.
       do j=np(i,2)+np(i,3)-1,np(i,1)-1
       rib(i,152)=rib(i,152)+dsqrt((u(i,j,43)-u(i,j+1,43))**2.+
     + (v(i,j,43)-v(i,j+1,43))**2.)
       rib(i,155)=rib(i,155)+dsqrt((u(i,j,44)-u(i,j+1,44))**2.+
     + (v(i,j,44)-v(i,j+1,44))**2.)
       end do
       rib(i,158)=rib(i,152)-rib(i,155)

c       write (*,*) i,rib(i,150),rib(i,153),rib(i,156)
c       write (*,*) i,rib(i,151),rib(i,154),rib(i,157)
c       write (*,*) i,rib(i,152),rib(i,155),rib(i,158)

c      Reformat rib extrados

       do j=1,np(i,2)-1  ! all extrados

       xdv=(v(i,j+1,43)-v(i,j,43))
       xdu=(u(i,j+1,43)-u(i,j,43))
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

       distee(j)=dsqrt((v(i,j,43)-v(i,j+1,43))**2.+
     + (u(i,j,43)-u(i,j+1,43))**2.)

       end do

c      Define reformated airfoil extrados

       distk=rib(i,153)/rib(i,150)
       do j=1,np(i,2)-1
       u(i,j+1,45)=u(i,j,43)+siu(j)*distk*distee(j)*dcos(anglee(j))
       v(i,j+1,45)=v(i,j,43)+siv(j)*distk*distee(j)*dsin(anglee(j))
       u(i,j+1,43)=u(i,j+1,45)
       v(i,j+1,43)=v(i,j+1,45)
       end do

c      Verify reformated airfoil extrados

       rib(i,150)=0.
       do j=1,np(i,2)-1
       rib(i,150)=rib(i,150)+dsqrt((u(i,j,43)-u(i,j+1,43))**2.+
     + (v(i,j,43)-v(i,j+1,43))**2.)
       end do
c       write (*,*) "RE ",i,rib(i,150),rib(i,153),rib(i,150)-rib(i,153)


c      Reformat rib intrados

       do j=np(i,1),np(i,2)+np(i,3),-1  ! all intrados

       xdv=(v(i,j,43)-v(i,j-1,43))
       xdu=(u(i,j,43)-u(i,j-1,43))
       if (xdu.ne.0.) then
       anglee(j)=abs(datan(xdv/xdu))
       else
       anglee(j)=2.*datan(1.0d0)
       end if

c       write (*,*) "ANGLE ",i,j,anglee(j)

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

       distee(j)=dsqrt((v(i,j,43)-v(i,j-1,43))**2.+
     + (u(i,j,43)-u(i,j-1,43))**2.)

       end do

c      Define reformated airfoil intrados

       distk=rib(i,155)/rib(i,152)
       do j=np(i,1),np(i,2)+np(i,3),-1        
       u(i,j-1,45)=u(i,j,43)-siu(j)*distk*distee(j)*dcos(anglee(j))
       v(i,j-1,45)=v(i,j,43)-siv(j)*distk*distee(j)*dsin(anglee(j))
       u(i,j-1,43)=u(i,j-1,45)
       v(i,j-1,43)=v(i,j-1,45)
       end do

c      Verify reformated airfoil intrados

       rib(i,152)=0.
       do j=np(i,1),np(i,2)+np(i,3),-1         
       rib(i,152)=rib(i,152)+dsqrt((u(i,j,43)-u(i,j-1,43))**2.+
     + (v(i,j,43)-v(i,j-1,43))**2.)
       end do
c       write (*,*) "RI ",i,rib(i,152),rib(i,155),rib(i,152)-rib(i,155)

c      Reformat vent ds (simplified method)

       if (atp.eq."ds") then

       if (np(i,3).eq.3) then ! interpolate only 1 point
       j=np(i,2)+1
       u(i,j,43)=(u(i,j-1,43)+u(i,j+1,43))*0.5
       v(i,j,43)=(v(i,j-1,43)+v(i,j+1,43))*0.5
       end if

       if (np(i,3).ge.4) then ! interpolate only 2 point
       j=np(i,2)+1
       u(i,j,43)=(2.*u(i,j-1,43)+1.*u(i,j+2,43))/3.
       v(i,j,43)=(2.*v(i,j-1,43)+1.*v(i,j+2,43))/3.
       j=np(i,2)+2
       u(i,j,43)=(1.*u(i,j-2,43)+2.*u(i,j+1,43))/3.
       v(i,j,43)=(1.*v(i,j-2,43)+2.*v(i,j+1,43))/3.
       end if

       end if

c      Reformat vent pc (lineal vent)

       if (atp.eq."pc") then

       xdu=(u(i,np(i,2)+np(i,3)-1,43)-u(i,np(i,2),43))/(np(i,3)-1)
       xdv=(v(i,np(i,2)+np(i,3)-1,43)-v(i,np(i,2),43))/(np(i,3)-1)
       do j=np(i,2),np(i,2)+np(i,3)-2
       u(i,j+1,43)=u(i,j,43)+xdu
       v(i,j+1,43)=v(i,j,43)+xdv
       end do

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      9.9.2 Compute external cut edges in airfoils (i,j,46)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       xcos=xrib/10. ! rib sewing allowance mm to cm

c      Also used for the two endpoints when there are no interior points
       xcosk=1.0d0
      
       do j=2,np(i,1)-1

c      Amplification factor
       xcosk=1.0

c      Fer mitja entre j-1 i j+1
       alpha1=(datan((v(i,j+1,43)-v(i,j,43))/((u(i,j+1,43)-u(i,j,43)))))
       alpha2=(datan((v(i,j,43)-v(i,j-1,43))/((u(i,j,43)-u(i,j-1,43)))))

       alpha=0.5*(alpha1+alpha2)

c      Alpha correction in sawtooht mono-surface airfoils
c      Dóna la volta a la vora superior

       if (alpha1.lt.0.and.alpha2.gt.0.and.j.ge.np(i,2)) then
       alpha=alpha+pi
       end if

       u(i,j,46)=u(i,j,43)-xcos*xcosk*dsin(alpha)

       if(v(i,j,43).ge.0.) then
       v(i,j,46)=v(i,j,43)+xcos*xcosk*dcos(alpha)
       end if

       if(v(i,j,43).lt.0.) then
       u(i,j,46)=u(i,j,43)+xcos*xcosk*dsin(alpha)
       v(i,j,46)=v(i,j,43)-xcos*xcosk*dcos(alpha)
       end if

       if(u(i,j,3).eq.0) then
       u(i,j,46)=u(i,j,43)-xcos*xcosk
       v(i,j,46)=v(i,j,43)
       end if

       end do

       j=1

       alpha=(datan((v(i,j+1,43)-v(i,j,43))/((u(i,j+1,43)-u(i,j,43)))))

       u(i,j,46)=u(i,j,43)-xcos*xcosk*dsin(alpha)

       if(v(i,j,43).ge.0.) then
       v(i,j,46)=v(i,j,43)+xcos*xcosk*dcos(alpha)
       end if

       if(v(i,j,43).lt.0.) then
       u(i,j,46)=u(i,j,43)+xcos*xcosk*dsin(alpha)
       v(i,j,46)=v(i,j,43)-xcos*xcosk*dcos(alpha)

       end if

       j=np(i,1)

       alpha=(datan((v(i,j,43)-v(i,j-1,43))/((u(i,j,43)-u(i,j-1,43)))))

       u(i,j,46)=u(i,j,43)-xcos*xcosk*dsin(alpha)

       if(v(i,j,43).ge.0.) then
       v(i,j,46)=v(i,j,43)+xcos*xcosk*dcos(alpha)
       end if

       if(v(i,j,43).le.0.) then
       u(i,j,46)=u(i,j,43)+xcos*xcosk*dsin(alpha)
       v(i,j,46)=v(i,j,43)-xcos*xcosk*dcos(alpha)
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      9.9.3 Print unloaded ribs, internal line (i,j,43)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c       Now not used

c       do j=1,np(i,2)-1
c       call line(sepx+u(i,j,43),-v(i,j,43)+sepy-sepriy*0.5,
c     + sepx+u(i,j+1,43),-v(i,j+1,43)+sepy-sepriy*0.5,1)
c       end do
c       do j=np(i,2),np(i,2)+np(i,3)-2
c       call line(sepx+u(i,j,43),-v(i,j,43)+sepy-sepriy*0.5,
c     + sepx+u(i,j+1,43),-v(i,j+1,43)+sepy-sepriy*0.5,2)
c       end do
c       do j=np(i,2)+np(i,3)-1,np(i,1)-1
c       call line(sepx+u(i,j,43),-v(i,j,43)+sepy-sepriy*0.5,
c     + sepx+u(i,j+1,43),-v(i,j+1,43)+sepy-sepriy*0.5,3)
c       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      9.9.4 Print unloaded ribs, external line (i,j,46)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do j=1,np(i,2)-1
       call line(sepx+u(i,j,46),-v(i,j,46)+sepy-sepriy*0.5,
     + sepx+u(i,j+1,46),-v(i,j+1,46)+sepy-sepriy*0.5,1)
       end do
       do j=np(i,2),np(i,2)+np(i,3)-2
       call line(sepx+u(i,j,46),-v(i,j,46)+sepy-sepriy*0.5,
     + sepx+u(i,j+1,46),-v(i,j+1,46)+sepy-sepriy*0.5,2)
       end do
       do j=np(i,2)+np(i,3)-1,np(i,1)-1
       call line(sepx+u(i,j,46),-v(i,j,46)+sepy-sepriy*0.5,
     + sepx+u(i,j+1,46),-v(i,j+1,46)+sepy-sepriy*0.5,1)
       end do

       j=1
       call line(sepx+u(i,j,46),-v(i,j,46)+sepy-sepriy*0.5,
     + sepx+u(i,j,43),-v(i,j,43)+sepy-sepriy*0.5,1)
       j=np(i,1)
       call line(sepx+u(i,j,46),-v(i,j,46)+sepy-sepriy*0.5,
     + sepx+u(i,j,43),-v(i,j,43)+sepy-sepriy*0.5,1)
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      9.9.5 Draw romano and itxt mark in rib
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc     
       j=1
       x1=sepx+u(i,j,43)
       y1=-v(i,j,43)+sepy-sepriy*0.5
       call romano(i,x1-20.,y1,0.0d0,typm6(9)*0.1,7)
       call itxt(x1,y1,5.0d0,0.0d0,i,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      9.9.6 Draw vents
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      ini
       j=np(i,2)

       alpha1=datan((v(i,j,43)-v(i,j-1,43))/(u(i,j,43)-u(i,j-1,43)))
      
       call point(sepx+u(i,j,43)+xdes*dsin(alpha1),
     + -v(i,j,43)+sepy-sepriy*0.5+xdes*dcos(alpha1),3)
       call point(sepx+u(i,j,43)-1.8*dsin(alpha1),
     + -1.8*dcos(alpha1)-v(i,j,43)+sepy-sepriy*0.5,3)

c      fi
       j=np(i,2)+np(i,3)-1

       alpha1=datan((v(i,j+1,43)-v(i,j-1,43))/(u(i,j+1,43)-u(i,j-1,43)))
       
       call point(sepx+u(i,j,43)+xdes*dsin(alpha1),
     + -v(i,j,43)+sepy-sepriy*0.5+xdes*dcos(alpha1),3)
       call point(sepx+u(i,j,43)-2.*dsin(alpha1),
     + -2*dcos(alpha1)-v(i,j,43)+sepy-sepriy*0.5,3)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      9.9.7 Marks extrados
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do k=1,60  ! Up to 60 marks

c      Partial lengths init

       rib(i,40)=0.
       rib(i,41)=0.
       rib(i,42)=0.
       rib(i,43)=0.
       rib(i,44)=0.
       rib(i,45)=0.

       xmk=xmark*float(k)

       do j=1,np(i,2)-1

       xprev=rib(i,41)

       rib(i,41)=rib(i,41)+sqrt((u(i,j,43)-u(i,j+1,43))**2.+((v(i,j,43)
     + -v(i,j+1,43))**2.))

       xpost=rib(i,41)

       if(xmk.lt.xpost.and.xmk.ge.xprev) then

c      dibuixa marca

       dist=dsqrt((u(i,j,43)-u(i,j+1,43))**2.+((v(i,j,43)
     + -v(i,j+1,43))**2.))

       dist1=xmk-xprev

       xu=u(i,j,43)+(u(i,j+1,43)-u(i,j,43))*(dist1/dist)
       xv=v(i,j,43)+(v(i,j+1,43)-v(i,j,43))*(dist1/dist)

c      Despla a vores punts de control de costures
       alp=(datan((v(i,j+1,43)-v(i,j,43))/(u(i,j+1,43)-u(i,j,43))))
       if (xv.lt.0.) then
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)
       end if
       if (xv.ge.0.) then
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)
       end if

c      Dibuixa punt a les costelles

       call point(sepx+xu,sepy-sepriy*0.5-xv,7)
       
       end if 

       end do ! j extrados

       end do ! mark K

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      9.9.8 Marks intrados
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do k=1,60

c      Partial lengths init

       rib(i,40)=0.
       rib(i,41)=0.
       rib(i,42)=0.
       rib(i,43)=0.
       rib(i,44)=0.
       rib(i,45)=0.

       xmk=xmark*float(k)

       do j=np(i,1),np(i,2)+np(i,3),-1

       xprev=rib(i,44)

       rib(i,44)=rib(i,44)+sqrt((u(i,j,43)-u(i,j-1,43))**2.+((v(i,j,43)
     + -v(i,j-1,43))**2.))

       xpost=rib(i,44)

       if(xmk.lt.xpost.and.xmk.ge.xprev) then

c      dibuixa marca

       dist=dsqrt((u(i,j,43)-u(i,j-1,43))**2.+((v(i,j,43)
     + -v(i,j-1,43))**2.))

       dist1=xmk-xprev

       xu=u(i,j,43)+(u(i,j-1,43)-u(i,j,43))*(dist1/dist)
       xv=v(i,j,43)+(v(i,j-1,43)-v(i,j,43))*(dist1/dist)

c      Despla a vores punts de control de costures
       alp=(datan((v(i,j,43)-v(i,j-1,43))/(u(i,j,43)-u(i,j-1,43))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)

c      Dibuixa punt a les costelles
       call point(sepx+xu,sepy-sepriy*0.5-xv,7)

       end if

       end do ! j intrados

       end do ! k


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      9.9.9 Draw holes in unloaded ribs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
       do l=1,nhols11

c       write (*,*) i,nhols11,ii11

c      Dibuixa forat tipus 11 (alleugerament elliptics)
     
       xx0=(hol(ii11,l,2))*0.5*(rib(i-1,5)+rib(i,5))/100.0d0+sepx
       yy0=(-(hol(ii11,l,3))*0.5*(rib(i-1,5)+rib(i,5))
     + /100.0d0+sepy-sepriy*0.5)
       xxa=(hol(ii11,l,4))*0.5*(rib(i-1,5)+rib(i,5))/100.0d0
       yyb=((hol(ii11,l,5))*0.5*(rib(i-1,5)+rib(i,5))/100.0d0)

       call ellipse(xx0,yy0,xxa,yyb,(hol(ii11,l,6)),1)

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Change airfoil location
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       end if  ! Calcule and draw rib i

       kx=int((float(i)/6.))
       ky=i-kx*6
       kyy=kyy+1

       end do ! rib i


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      9.9.10 Draw equidistant points in panels
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      9.9.10.1 Extrados
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=0,nribss

       psep=1970.*xkf+2520.*xkf+seppix(i)*1.0d0
       psey=400.*xkf

c      Detect complete unloaded rib
       if (rib(i,56).eq.100.and.atp.ne."ss") then

c      Initial point

       j=1
       xu=u(i,j,44)
       xv=v(i,j,44)

       alp=abs(datan((v(i,j+1,44)-v(i,j,44))/(u(i,j+1,44)-u(i,j,44))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)

       call point(psep+xu-seppix(i),psey-xv,4)
       call point(psep+xu-seppix(i)-2520.*xkf,psey-xv,4)

c      Final point

       j=np(i,2)
       xu=u(i,j,44)
       xv=v(i,j,44)

       alp=abs(datan((v(i,j-1,44)-v(i,j,44))/(u(i,j-1,44)-u(i,j,44))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)

       call point(psep+xu-seppix(i),psey-xv,4)
       call point(psep+xu-seppix(i)-2520.*xkf,psey-xv,4)

c      Internal equidistant points

       do k=1,60  ! Up to 60 marks

c      Partial lengths init

       rib(i,40)=0.
       rib(i,41)=0.
       rib(i,42)=0.
       rib(i,43)=0.
       rib(i,44)=0.
       rib(i,45)=0.

       xmk=xmark*float(k)

       do j=1,np(i,2)-1

       xprev=rib(i,41)

       rib(i,41)=rib(i,41)+sqrt((u(i,j,44)-u(i,j+1,44))**2.+
     + ((v(i,j,44)-v(i,j+1,44))**2.))

       xpost=rib(i,41)

       if(xmk.lt.xpost.and.xmk.ge.xprev) then

c      dibuixa marca

       dist=dsqrt((u(i,j,44)-u(i,j+1,44))**2.+((v(i,j,44)
     + -v(i,j+1,44))**2.))

       dist1=xmk-xprev

       xu=u(i,j,44)+(u(i,j+1,44)-u(i,j,44))*(dist1/dist)
       xv=v(i,j,44)+(v(i,j+1,44)-v(i,j,44))*(dist1/dist)

c      Despla a vores punts de control de costures
c       alp=(datan((v(i,j+1,44)-v(i,j,44))/(u(i,j+1,44)-u(i,j,44))))
c       if (xv.lt.0.) then
c       xu=xu+xdes*dsin(alp)
c       xv=xv-xdes*dcos(alp)
c       end if
c       if (xv.ge.0.) then
c       xu=xu-xdes*dsin(alp)
c       xv=xv+xdes*dcos(alp)
c       end if

c      Dibuixa punt a les costelles

c       call point(psep+xu-seppix(i),psey-xv,1)

c      Despla a vores punts de control de costures
       alp=(datan((v(i,j+1,44)-v(i,j,44))/(u(i,j+1,44)-u(i,j,44))))
       if (xv.lt.0.) then
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)
       end if
       if (xv.ge.0.) then
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)
       end if

       call point(psep+xu-seppix(i),psey-xv,1)
       call point(psep+xu-seppix(i)-2520.*xkf,psey-xv,1)

       end if ! xmk

       end do ! j extrados

       end do ! mark K

       end if ! unloaded rib

       end do ! rib i


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      9.9.10.2 Intrados
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=0,nribss

       psep=1970.*xkf+2520.*xkf+seppix(i)*1.0d0
       psey=1291.*xkf

c      Detect complete unloaded rib
       if (rib(i,56).eq.100.and.atp.ne."ss") then

c      Initial point

       j=np(i,1)
       xu=u(i,j,44)
       xv=v(i,j,44)

       alp=abs(datan((v(i,j-1,44)-v(i,j,44))/(u(i,j-1,44)-u(i,j,44))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)

       call point(psep+xu-seppix(i),psey-xv,4)
       call point(psep+xu-seppix(i)-2520.*xkf,psey-xv,4)

c      Final point

       j=np(i,2)+np(i,3)-1
       xu=u(i,j,44)
       xv=v(i,j,44)

       alp=abs(datan((v(i,j-1,44)-v(i,j,44))/(u(i,j-1,44)-u(i,j,44))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)

       call point(psep+xu-seppix(i),psey-xv,4)
       call point(psep+xu-seppix(i)-2520.*xkf,psey-xv,4)

c      Internal equidistant points

       do k=1,60  ! Up to 60 marks

c      Partial lengths init

       rib(i,40)=0.
       rib(i,41)=0.
       rib(i,42)=0.
       rib(i,43)=0.
       rib(i,44)=0.
       rib(i,45)=0.

       xmk=xmark*float(k)

       do j=np(i,1),np(i,2)+np(i,3),-1

       xprev=rib(i,43)

       rib(i,43)=rib(i,43)+sqrt((u(i,j,44)-u(i,j-1,44))**2.+
     + ((v(i,j,44)-v(i,j-1,44))**2.))

       xpost=rib(i,43)

       if(xmk.lt.xpost.and.xmk.ge.xprev) then

c      dibuixa marca

       dist=dsqrt((u(i,j,44)-u(i,j-1,44))**2.+((v(i,j,44)
     + -v(i,j-1,44))**2.))

       dist1=xmk-xprev

       xu=u(i,j,44)+(u(i,j-1,44)-u(i,j,44))*(dist1/dist)
       xv=v(i,j,44)+(v(i,j-1,44)-v(i,j,44))*(dist1/dist)

c      Despla a vores punts de control de costures
c       alp=(datan((v(i,j+1,44)-v(i,j,44))/(u(i,j+1,44)-u(i,j,44))))
c       if (xv.lt.0.) then
c       xu=xu+xdes*dsin(alp)
c       xv=xv-xdes*dcos(alp)
c       end if
c       if (xv.ge.0.) then
c       xu=xu-xdes*dsin(alp)
c       xv=xv+xdes*dcos(alp)
c       end if

c      Dibuixa punt a les costelles

c       call point(psep+xu-seppix(i),psey-xv,1)

c      Despla a vores punts de control de costures
       alp=(datan((v(i,j-1,44)-v(i,j,44))/(u(i,j-1,44)-u(i,j,44))))
c       if (xv.lt.0.) then
c       xu=xu+xdes*dsin(alp)
c       xv=xv-xdes*dcos(alp)
c       end if
c       if (xv.ge.0.) then
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)
c       end if

       call point(psep+xu-seppix(i),psey-xv,1)
       call point(psep+xu-seppix(i)-2520.*xkf,psey-xv,1)

       end if ! xmk

       end do ! j extrados

       end do ! mark K

       end if ! unloaded rib

       end do ! rib i


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
