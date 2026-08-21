!> Emit the calculated wing, internals, lines, and controls to the 3D DXFs.
!!
!! Writes `lep-3d.dxf` and the optional surface stream using absolute `x/y/z`
!! coordinates prepared in stage 6 and later shaping stages.  Surface colors
!! and element visibility follow section-25 settings; no 2D panel coordinates
!! are reused as spatial geometry.
c      21. 3D model DXF drawing
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c       call dxfinit(25)

       do i=1,nribss

c      21.1 Extrados airfoil blue

       do j=1,np(i,2)-1

       p1x=x(i,j)
       p1y=y(i,j)
       p1z=z(i,j)
       p2x=x(i,j+1)
       p2y=y(i,j+1)
       p2z=z(i,j+1)

       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,ele3dc(11))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,ele3dc(11))

       end do

c      Draw extrados, straight line

       if (i.le.nribss.and.ele3d(11).eq.1) then

       do j=1,np(i,2)-1
       p1x=x(i-1,j)
       p1y=y(i-1,j)
       p1z=z(i-1,j)
       p2x=x(i,j)
       p2y=y(i,j)
       p2z=z(i,j)
       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,ele3dc(11))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,ele3dc(11))
       end do

       end if

c      21.2 Vents

c      Case "ds" or "ss"
       if (atp.eq."ds".or.atp.eq."ss") then

       do j=np(i,2),np(i,2)+np(i,3)-2

       p1x=x(i,j)
       p1y=y(i,j)
       p1z=z(i,j)
       p2x=x(i,j+1)
       p2y=y(i,j+1)
       p2z=z(i,j+1)

       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,ele3dc(12))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,ele3dc(12))

       end do

c      Draw vents, straight line

       if (i.le.nribss.and.ele3d(12).eq.1) then

       do j=np(i,2),np(i,2)+np(i,3)-2
       p1x=x(i-1,j)
       p1y=y(i-1,j)
       p1z=z(i-1,j)
       p2x=x(i,j)
       p2y=y(i,j)
       p2z=z(i,j)
       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,ele3dc(12))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,ele3dc(12))
       end do

       end if

       end if ! ds or ss

c      Case "pc"
       if (atp.eq."pc") then

       j1=np(i,2)
       j2=np(i,2)+np(i,3)-1

       p1x=x(i,j1)
       p1y=y(i,j1)
       p1z=z(i,j1)
       p2x=x(i,j2)
       p2y=y(i,j2)
       p2z=z(i,j2)

       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,ele3dc(12))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,ele3d(12))

       end if

c      21.3 Intrados 

       do j=np(i,2)+np(i,3)-1,np(i,1)-1

       p1x=x(i,j)
       p1y=y(i,j)
       p1z=z(i,j)
       p2x=x(i,j+1)
       p2y=y(i,j+1)
       p2z=z(i,j+1)

       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,ele3dc(13))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,ele3dc(13))

       end do

c      Draw intrados, straight line

       if (i.le.nribss.and.ele3d(13).eq.1) then

       do j=np(i,2)+np(i,3)-1,np(i,1)-1
       p1x=x(i-1,j)
       p1y=y(i-1,j)
       p1z=z(i-1,j)
       p2x=x(i,j)
       p2y=y(i,j)
       p2z=z(i,j)
       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,ele3dc(13))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,ele3dc(13))
       end do

       end if


       end do

c      21.4 Trailing edge

       do i=1,nribss-1

       p1x=x(i,1)
       p1y=y(i,1)
       p1z=z(i,1)
       p2x=x(i+1,1)
       p2y=y(i+1,1)
       p2z=z(i+1,1)

       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,ele3dc(11))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,ele3dc(11))

       end do

       p1x=x(1,1)
       p1y=y(1,1)
       p1z=z(1,1)
       p2x=-x(1,1)
       p2y=y(1,1)
       p2z=z(1,1)

       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,ele3dc(11))

c      21.5 Leading edge

c      Case classic
       if (k26d.eq.0) then

c      Vent in

       do i=1,nribss

       p1x=x(i-1,np(i,2))
       p1y=y(i-1,np(i,2))
       p1z=z(i-1,np(i,2))
       p2x=x(i,np(i,2))
       p2y=y(i,np(i,2))
       p2z=z(i,np(i,2))

       if (rib(i,14).eq.1) then
       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,ele3dc(12))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,ele3dc(12))
       end if
       if (rib(i,14).eq.0) then
       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,9)
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,9)
       end if


       end do

c       p1x=x(1,np(i,2))
c       p1y=y(1,np(i,2))
c       p1z=z(1,np(i,2))
c       p2x=-x(1,np(i,2))
c       p2y=y(1,np(i,2))
c       p2z=z(1,np(i,2))

c       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,1)

c      Vent out

       do i=1,nribss

       j=np(i,2)+np(i,3)-1

       p1x=x(i-1,j)
       p1y=y(i-1,j)
       p1z=z(i-1,j)
       p2x=x(i,j)
       p2y=y(i,j)
       p2z=z(i,j)

       if (rib(i,14).eq.1) then
       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,ele3dc(12))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,ele3dc(12))
       end if
       if (rib(i,14).eq.4) then
       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,9)
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,9)
       end if

       end do

       end if ! k26d=0


c      Case new vents
       if (k26d.eq.1) then

c      Vent in

       do i=1,nribss

       p1x=x(i-1,np(i,2))
       p1y=y(i-1,np(i,2))
       p1z=z(i-1,np(i,2))
       p2x=x(i,np(i,2))
       p2y=y(i,np(i,2))
       p2z=z(i,np(i,2))

       j=np(i,2)+np(i,3)-1

       p3x=x(i-1,j)
       p3y=y(i-1,j)
       p3z=z(i-1,j)
       p4x=x(i,j)
       p4y=y(i,j)
       p4z=z(i,j)

       if (rib(i,165).eq.0) then
       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,ele3dc(12))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,ele3dc(12))
       call line3d(p3x,p3y,p3z,p4x,p4y,p4z,ele3dc(12))
       call line3d(-p3x,p3y,p3z,-p4x,p4y,p4z,ele3dc(12))
       end if

       if (rib(i,165).eq.1.or.rib(i,165).eq.6) then
       call line3d(p3x,p3y,p3z,p4x,p4y,p4z,ele3dc(12))
       call line3d(-p3x,p3y,p3z,-p4x,p4y,p4z,ele3dc(12))
       end if

       if (rib(i,165).eq.-1.or.rib(i,165).eq.-6) then
       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,ele3dc(12))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,ele3dc(12))
       end if

       if (rib(i,165).eq.-2) then
       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,ele3dc(12))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,ele3dc(12))
       call line3d(p3x,p3y,p3z,p2x,p2y,p2z,ele3dc(12))
       call line3d(-p3x,p3y,p3z,-p2x,p2y,p2z,ele3dc(12))
       end if

       if (rib(i,165).eq.-3) then
       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,ele3dc(12))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,ele3dc(12))
       call line3d(p1x,p1y,p1z,p4x,p4y,p4z,ele3dc(12))
       call line3d(-p1x,p1y,p1z,-p4x,p4y,p4z,ele3dc(12))
       end if


c      Vents type 4,-4,-5
       if (rib(i,165).eq.4.or.rib(i,165).eq.-4.or.rib(i,165).eq.-5) then

c      Interpolate new vent init

c      Load vent polyline
c      Left side
       do j=np(i,2),np(i,2)+np(i,3)-1
       xpoly(j)=x(i-1,j)
       ypoly(j)=y(i-1,j)
       zpoly(j)=z(i-1,j)
       end do
       npo=np(i,3)

c      Length vent left
       xlenl=0.0d0
       do j=np(i,2),np(i,2)+np(i,3)-2
       xlenl=xlenl+dsqrt((xpoly(j+1)-xpoly(j))*(xpoly(j+1)-xpoly(j))+
     + (ypoly(j+1)-ypoly(j))*(ypoly(j+1)-ypoly(j))+
     + (zpoly(j+1)-zpoly(j))*(zpoly(j+1)-zpoly(j)))  
       end do
       xlenlr=xlenl*(1.-csi(i,19)/100.)

c      Interpole point
       call interpoly3d(xpoly,ypoly,zpoly,x_poly,y_poly,z_poly,
     + xlenlr,np(i,2),np(i,2)+np(i,3)-1,npolyl,distrel)
       x_poly1=x_poly
       y_poly1=y_poly
       z_poly1=z_poly
       distrel1=distrel

c      Right side
       do j=np(i,2),np(i,2)+np(i,3)-1
       xpoly(j)=x(i,j)
       ypoly(j)=y(i,j)
       zpoly(j)=z(i,j)
       end do
       npo=np(i,3)

c      Length vent right
       xlenr=0.0d0
       do j=np(i,2),np(i,2)+np(i,3)-2
       xlenr=xlenr+dsqrt((xpoly(j+1)-xpoly(j))*(xpoly(j+1)-xpoly(j))+
     + (ypoly(j+1)-ypoly(j))*(ypoly(j+1)-ypoly(j))+
     + (zpoly(j+1)-zpoly(j))*(zpoly(j+1)-zpoly(j)))  
       end do
       xlenrr=xlenr*(1.-csi(i,20)/100.)

c      Interpole point
       call interpoly3d(xpoly,ypoly,zpoly,x_poly,y_poly,z_poly,
     + xlenrr,np(i,2),np(i,2)+np(i,3)-1,npolyl,distrel)
       x_poly2=x_poly
       y_poly2=y_poly
       z_poly2=z_poly
       distrel2=distrel

c      Case 4,-4,-5
c      Draw line vent
       call line3d(x_poly1,y_poly1,z_poly1,x_poly2,y_poly2,z_poly2,
     + ele3dc(12))
       call line3d(-x_poly1,y_poly1,z_poly1,-x_poly2,y_poly2,z_poly2,
     + ele3dc(12))

c      Case -5 still not drawn

c      Draw line extrados
       if (rib(i,165).eq.-4.or.rib(i,165).eq.-5) then
       j=np(i,2)
       call line3d(x(i-1,j),y(i-1,j),z(i-1,j),x(i,j),y(i,j),z(i,j),
     + ele3dc(12))
       call line3d(-x(i-1,j),y(i-1,j),z(i-1,j),-x(i,j),y(i,j),z(i,j),
     + ele3dc(12))
       end if

c      Draw line intrados
       if (rib(i,165).eq.4.or.rib(i,165).eq.5) then
       j=np(i,2)+np(i,3)-1
       call line3d(x(i-1,j),y(i-1,j),z(i-1,j),x(i,j),y(i,j),z(i,j),
     + ele3dc(12))
       call line3d(-x(i-1,j),y(i-1,j),z(i-1,j),-x(i,j),y(i,j),z(i,j),
     + ele3dc(12))
       end if

       end if ! vents 4,-4,-5

       end do ! i


       end if ! k26d=1


c      21.6 lines 3D

c      Lines A,B,C,D,...

       do i=1,cordam

       p2x=x2line(corda(i,1),corda(i,2),corda(i,3))
       p1x=x1line(corda(i,1),corda(i,2),corda(i,3))
       p2y=y2line(corda(i,1),corda(i,2),corda(i,3))
       p1y=y1line(corda(i,1),corda(i,2),corda(i,3))
       p2z=z2line(corda(i,1),corda(i,2),corda(i,3))
       p1z=z1line(corda(i,1),corda(i,2),corda(i,3))

       if (i37c.eq.0) then ! colors by risers
       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,iccolor(10+corda(i,1)))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,iccolor(10+corda(i,1)))
       end if

       if (i37c.eq.1) then ! colors by table 34
       iccolor34(i)=int(tyline(corda(i,40),10))
       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,iccolor34(i))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,iccolor34(i))
       end if

       end do

c      21.7 Brakes

       do i=cordam+1,cordat

       p2x=x2line(corda(i,1),corda(i,2),corda(i,3))
       p1x=x1line(corda(i,1),corda(i,2),corda(i,3))
       p2y=y2line(corda(i,1),corda(i,2),corda(i,3))
       p1y=y1line(corda(i,1),corda(i,2),corda(i,3))
       p2z=z2line(corda(i,1),corda(i,2),corda(i,3))
       p1z=z1line(corda(i,1),corda(i,2),corda(i,3))

       if (i37c.eq.0) then ! colors by risers
       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,iccolor(16))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,iccolor(16))
       end if

       if (i37c.eq.1) then ! colors by table 34
       iccolor34(i)=int(tyline(corda(i,40),10))
       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,iccolor34(i))
       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,iccolor34(i))
       end if

       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      21.8 H-V-ribs 3D drawing
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do k=1,nhvr

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      21.8.1 H-ribs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (hvr(k,2).eq.1) then
       i=hvr(k,3)
c      warning
       ii=hvr(k,4)

       do j=1,21

       p1x=hx3(i,j,ii)
       p1y=hy3(i,j,ii)
       p1z=hz3(i,j,ii)
       p2x=hx2(i,j,ii)
       p2y=hy2(i,j,ii)
       p2z=hz2(i,j,ii)

       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,2)

       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,2)

       if (j.lt.21) then

       call line3d(-hx2(i,j,ii),hy2(i,j,ii),hz2(i,j,ii),
     + -hx2(i,j+1,ii),hy2(i,j+1,ii),hz2(i,j+1,ii),2)
       call line3d(-hx3(i,j,ii),hy3(i,j,ii),hz3(i,j,ii),
     + -hx3(i,j+1,ii),hy3(i,j,ii),hz3(i,j+1,ii),2)

       call line3d(hx2(i,j,ii),hy2(i,j,ii),hz2(i,j,ii),
     + hx2(i,j+1,ii),hy2(i,j+1,ii),hz2(i,j+1,ii),2)
       call line3d(hx3(i,j,ii),hy3(i,j,ii),hz3(i,j,ii),
     + hx3(i,j+1,ii),hy3(i,j,ii),hz3(i,j+1,ii),2)

       end if

       end do

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      21.8.2 V-ribs partial
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Obsolete, erased

       if (hvr(k,2).eq.2) then
       i=hvr(k,3)
       ii=hvr(k,4)

       do j=1,21

       p1x=rx1(i,j,ii)
       p1y=ry1(i,j,ii)
       p1z=rz1(i,j,ii)
       p2x=rx2(i,j,ii)
       p2y=ry2(i,j,ii)
       p2z=rz2(i,j,ii)

c      Diagonal i-1 to i
       if (hvr(k,5).eq.1) then
c       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,5)
c       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,5)
       end if

       if (j.lt.21) then
       if (hvr(k,5).eq.1) then
c       call line3d(rx1(i,j,ii),ry1(i,j,ii),rz1(i,j,ii),
c     + rx1(i,j+1,ii),ry1(i,j+1,ii),rz1(i,j+1,ii),5)
c       call line3d(-rx1(i,j,ii),ry1(i,j,ii),rz1(i,j,ii),
c     + -rx1(i,j+1,ii),ry1(i,j+1,ii),rz1(i,j+1,ii),5)
       end if
       end if

       p1x=rx3(i,j,ii)
       p1y=ry3(i,j,ii)
       p1z=rz3(i,j,ii)
       p2x=rx2(i,j,ii)
       p2y=ry2(i,j,ii)
       p2z=rz2(i,j,ii)

c      Diagonal i to i+1
       if (hvr(k,6).eq.1) then
c       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,1)
c       call line3d(-p1x,p1y,p1z,-p2x,p2y,p2z,1)
       end if

       if (j.lt.21) then
       if (hvr(k,6).eq.1) then
c       call line3d(rx3(i,j,ii),ry3(i,j,ii),rz3(i,j,ii),
c     + rx3(i,j+1,ii),ry3(i,j+1,ii),rz3(i,j+1,ii),1)
c       call line3d(-rx3(i,j,ii),ry3(i,j,ii),rz3(i,j,ii),
c     + -rx3(i,j+1,ii),ry3(i,j+1,ii),rz3(i,j+1,ii),1)
       end if
       end if

       end do

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      21.8.4 VH-ribs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      OBSOLETE, draw before. ERASE

       if (hvr(k,2).eq.4) then
       i=hvr(k,3)-1
       ii=hvr(k,4)

       do j=1,21

c      Diagonal i-1 to i

       p1x=sx1(i,j,ii)
       p1y=sy1(i,j,ii)
       p1z=sz1(i,j,ii)
       p2x=sx2(i,j,ii)
       p2y=sy2(i,j,ii)
       p2z=sz2(i,j,ii)

       if (hvr(k,5).eq.1) then
c       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,4)
       end if

       if (j.lt.21) then
       if (hvr(k,5).eq.1) then
c       call line3d(rx1(i,j,ii),ry1(i,j,ii),rz1(i,j,ii),
c     + rx1(i,j+1,ii),ry1(i,j+1,ii),rz1(i,j+1,ii),4)
       end if
       end if

c      Horizontal i to i+1

       p1x=sx1(i,j,ii)
       p1y=sy1(i,j,ii)
       p1z=sz1(i,j,ii)
       p2x=sx3(i,j,ii)
       p2y=sy3(i,j,ii)
       p2z=sz3(i,j,ii)

       if (hvr(k,5).eq.1.or.hvr(k,6).eq.1) then
c       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,4)
       end if

       if (j.lt.21) then
       if (hvr(k,5).eq.1) then
c       call line3d(rx1(i,j,ii),ry1(i,j,ii),rz1(i,j,ii),
c     + rx1(i,j+1,ii),ry1(i,j+1,ii),rz1(i,j+1,ii),4)
       end if
       end if

c      Diagonal i+1 to i+2

       p1x=sx3(i,j,ii)
       p1y=sy3(i,j,ii)
       p1z=sz3(i,j,ii)
       p2x=sx4(i,j,ii)
       p2y=sy4(i,j,ii)
       p2z=sz4(i,j,ii)

       if (hvr(k,6).eq.1) then
c       call line3d(p1x,p1y,p1z,p2x,p2y,p2z,4)
       end if

       if (j.lt.21) then
       if (hvr(k,6).eq.1) then
c       call line3d(rx3(i,j,ii),ry3(i,j,ii),rz3(i,j,ii),
c     + rx3(i,j+1,ii),ry3(i,j+1,ii),rz3(i,j+1,ii),4)
       end if
       end if

       end do

       end if

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      21.9 Draw the intermediate and ovalized airfoil in 3D
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=1,nribss

c      All airfoil points
       if (atp.ne."ss") then
       jfin=np(i,1)-1
       end if

c      Limite the number of points in single skin
       if (atp.eq."ss") then
       jfin=np(i,2)+np(i,3)-2
       end if

c      Section 21.9 is the first production consumer of the owned structural
c      surface.  Construct the panel i-1/i view only when one of its median
c      profiles is requested, require exact agreement with all retained
c      panels3d outputs, then draw from named neutral/inflated point arrays.
c      The checked snapshot keeps the current panels3d producer authoritative
c      while removing numbered slots 48/49 from this DXF consumer.
       if ((pp29(1,1).eq.1.and.i.ge.pp29(1,2).and.
     + i.le.pp29(1,3)).or.(pp29(2,1).eq.1.and.
     + i.ge.pp29(2,2).and.i.le.pp29(2,3))) then
       call copy_legacy_spatial_panel_surface(u,v,w,hautok,i-1,np(i,1),
     + typed_dxf_panel_surface,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)') 'ERROR: 3D DXF panel surface ',i-1,': ',
     + trim(domain_model_message)
       stop 1
       end if
       call spatial_panel_surface_matches_legacy(typed_dxf_panel_surface,
     + u,v,w,hautok,domain_model_valid,domain_model_message)
       if (.not.domain_model_valid) then
       write (*,'(A,I0,2A)')
     + 'ERROR: typed/legacy 3D DXF surface mismatch at panel ',i-1,': ',
     + trim(domain_model_message)
       stop 1
       end if
       end if

c      Intermediate airfoil (48)
       if (pp29(1,1).eq.1) then
       if (i.ge.pp29(1,2).and.i.le.pp29(1,3)) then
       do j=1,jfin
       call line3d(typed_dxf_panel_surface%neutral_median(j)%x_cm,
     + typed_dxf_panel_surface%neutral_median(j)%y_cm,
     + typed_dxf_panel_surface%neutral_median(j)%z_cm,
     + typed_dxf_panel_surface%neutral_median(j+1)%x_cm,
     + typed_dxf_panel_surface%neutral_median(j+1)%y_cm,
     + typed_dxf_panel_surface%neutral_median(j+1)%z_cm,7)
       if (pp29(1,4).eq.0) then
       call line3d(-typed_dxf_panel_surface%neutral_median(j)%x_cm,
     + typed_dxf_panel_surface%neutral_median(j)%y_cm,
     + typed_dxf_panel_surface%neutral_median(j)%z_cm,
     + -typed_dxf_panel_surface%neutral_median(j+1)%x_cm,
     + typed_dxf_panel_surface%neutral_median(j+1)%y_cm,
     + typed_dxf_panel_surface%neutral_median(j+1)%z_cm,7)
       end if
       end do
       end if
       end if

c      Ovalized airfoil (49)
       if (pp29(2,1).eq.1) then
       if (i.ge.pp29(2,2).and.i.le.pp29(2,3)) then
       do j=1,jfin
       call line3d(typed_dxf_panel_surface%inflated_median(j)%x_cm,
     + typed_dxf_panel_surface%inflated_median(j)%y_cm,
     + typed_dxf_panel_surface%inflated_median(j)%z_cm,
     + typed_dxf_panel_surface%inflated_median(j+1)%x_cm,
     + typed_dxf_panel_surface%inflated_median(j+1)%y_cm,
     + typed_dxf_panel_surface%inflated_median(j+1)%z_cm,4)
       if (pp29(2,4).eq.0) then
       call line3d(-typed_dxf_panel_surface%inflated_median(j)%x_cm,
     + typed_dxf_panel_surface%inflated_median(j)%y_cm,
     + typed_dxf_panel_surface%inflated_median(j)%z_cm,
     + -typed_dxf_panel_surface%inflated_median(j+1)%x_cm,
     + typed_dxf_panel_surface%inflated_median(j+1)%y_cm,
     + typed_dxf_panel_surface%inflated_median(j+1)%z_cm,4)
       end if
       end do
       end if
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      21.10 Draw tessellation in 3D model
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
       if (pp29(3,1).eq.1) then
       if (i.ge.pp29(3,2).and.i.le.pp29(3,3)) then

c      21.10.1 Extrados
c      Extrados transverse line
       do k=1,nsegments
       do j=1,np(i,2)
       call line3d(tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + tesse3d(1,i,j,k+1),tesse3d(2,i,j,k+1),tesse3d(3,i,j,k+1),
     + ele3dc(11))
       if (pp29(3,4).eq.1) then ! draw symetric part
       call line3d(-tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + -tesse3d(1,i,j,k+1),tesse3d(2,i,j,k+1),tesse3d(3,i,j,k+1),
     + ele3dc(11))
       end if
       end do ! j
       end do ! k

c      Extrados longitudinal lines
       do k=1,nsegments+1
       do j=1,np(i,2)-1       
       call line3d(tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + tesse3d(1,i,j+1,k),tesse3d(2,i,j+1,k),tesse3d(3,i,j+1,k),
     + ele3dc(11))
       if (pp29(3,4).eq.1) then ! draw symetric part
       call line3d(-tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + -tesse3d(1,i,j+1,k),tesse3d(2,i,j+1,k),tesse3d(3,i,j+1,k),
     + ele3dc(11))
       end if
       end do ! j
       end do ! k

c      21.10.2 Vents
c      Draw vents only if closed
c      Still not draw special cases
       if (rib(i,165).eq.1.or.rib(i,165).eq.-1.or.
     + rib(i,14).eq.0) then
c      Vents transverse line
       do k=1,nsegments
       do j=np(i,2),np(i,2)+np(i,3)-1
       call line3d(tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + tesse3d(1,i,j,k+1),tesse3d(2,i,j,k+1),tesse3d(3,i,j,k+1),
     + ele3dc(12))
       if (pp29(3,4).eq.1) then ! draw symetric part
       call line3d(-tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + -tesse3d(1,i,j,k+1),tesse3d(2,i,j,k+1),tesse3d(3,i,j,k+1),
     + ele3dc(12))
       end if
       end do ! j
       end do ! k

c      Vents longitudinal lines
       do k=1,nsegments+1
       do j=np(i,2),np(i,2)+np(i,3)-1   
       call line3d(tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + tesse3d(1,i,j+1,k),tesse3d(2,i,j+1,k),tesse3d(3,i,j+1,k),
     + ele3dc(12))
       if (pp29(3,4).eq.1) then ! draw symetric part
       call line3d(-tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + -tesse3d(1,i,j+1,k),tesse3d(2,i,j+1,k),tesse3d(3,i,j+1,k),
     + ele3dc(12))
       end if
       end do ! j
       end do ! k
       end if ! Vents only if closed rib


c      21.10.3 Intrados
       if (atp.ne."ss") then
c      Intrados transverse line
       do k=1,nsegments
       do j=np(i,2)+np(i,3)-1,np(i,1)
       call line3d(tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + tesse3d(1,i,j,k+1),tesse3d(2,i,j,k+1),tesse3d(3,i,j,k+1),
     + ele3dc(13))
       if (pp29(3,4).eq.1) then ! draw symetric part
       call line3d(-tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + -tesse3d(1,i,j,k+1),tesse3d(2,i,j,k+1),tesse3d(3,i,j,k+1),
     + ele3dc(13))
       end if
       end do ! j
       end do ! k

c      Intrados longitudinal lines
       do k=1,nsegments+1
       do j=np(i,2)+np(i,3)-1,np(i,1)-1   
       call line3d(tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + tesse3d(1,i,j+1,k),tesse3d(2,i,j+1,k),tesse3d(3,i,j+1,k),
     + ele3dc(13))
       if (pp29(3,4).eq.1) then ! draw symetric part
       call line3d(-tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + -tesse3d(1,i,j+1,k),tesse3d(2,i,j+1,k),tesse3d(3,i,j+1,k),
     + ele3dc(13))
       end if
       end do ! j
       end do ! k
       end if ! wing type is not "ss"


       end if ! i included
       end if ! case DXF in model


       end do ! i main loop

       call dxfend(25)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      21.11 Draw external 3D DXF tessellation
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (pp29(4,1).eq.1) then ! Draw external DXF

       do i=1,nribss

       if (pp29(4,1).eq.1) then
       if (i.ge.pp29(4,2).and.i.le.pp29(4,3)) then

c      Write new DXF file (file 26)

c      Extrados transverse line
       do k=1,nsegments
       do j=1,np(i,2)
       call line3dn(tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + tesse3d(1,i,j,k+1),tesse3d(2,i,j,k+1),tesse3d(3,i,j,k+1),
     + ele3dc(11))
       if (pp29(4,4).eq.1) then ! draw symetric part
       call line3dn(-tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + -tesse3d(1,i,j,k+1),tesse3d(2,i,j,k+1),tesse3d(3,i,j,k+1),
     + ele3dc(11))
       end if
       end do ! j
       end do ! k

c      Extrados longitudinal lines
       do k=1,nsegments+1
       do j=1,np(i,2)-1       
       call line3dn(tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + tesse3d(1,i,j+1,k),tesse3d(2,i,j+1,k),tesse3d(3,i,j+1,k),
     + ele3dc(11))
       if (pp29(4,4).eq.1) then ! draw symetric part
       call line3dn(-tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + -tesse3d(1,i,j+1,k),tesse3d(2,i,j+1,k),tesse3d(3,i,j+1,k),
     + ele3dc(11))
       end if
       end do ! j
       end do ! k

c      Draw vents only if closed
c      Still not draw special cases
       if (rib(i,165).eq.1.or.rib(i,165).eq.-1.or.
     + rib(i,14).eq.0) then
c      Vents transverse line
       do k=1,nsegments
       do j=np(i,2),np(i,2)+np(i,3)-1
       call line3dn(tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + tesse3d(1,i,j,k+1),tesse3d(2,i,j,k+1),tesse3d(3,i,j,k+1),
     + ele3dc(12))
       if (pp29(4,4).eq.1) then ! draw symetric part
       call line3dn(-tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + -tesse3d(1,i,j,k+1),tesse3d(2,i,j,k+1),tesse3d(3,i,j,k+1),
     + ele3dc(12))
       end if
       end do ! j
       end do ! k

c      Vents longitudinal lines
       do k=1,nsegments+1
       do j=np(i,2),np(i,2)+np(i,3)-1   
       call line3dn(tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + tesse3d(1,i,j+1,k),tesse3d(2,i,j+1,k),tesse3d(3,i,j+1,k),
     + ele3dc(12))
       if (pp29(4,4).eq.1) then ! draw symetric part
       call line3dn(-tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + -tesse3d(1,i,j+1,k),tesse3d(2,i,j+1,k),tesse3d(3,i,j+1,k),
     + ele3dc(12))
       end if
       end do ! j
       end do ! k
       end if ! Vents only if closed rib

       if (atp.ne."ss") then
c      Intrados transverse line
       do k=1,nsegments
       do j=np(i,2)+np(i,3)-1,np(i,1)
       call line3dn(tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + tesse3d(1,i,j,k+1),tesse3d(2,i,j,k+1),tesse3d(3,i,j,k+1),
     + ele3dc(13))
       if (pp29(4,4).eq.1) then ! draw symetric part
       call line3dn(-tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + -tesse3d(1,i,j,k+1),tesse3d(2,i,j,k+1),tesse3d(3,i,j,k+1),
     + ele3dc(13))
       end if
       end do ! j
       end do ! k

c      Intrados longitudinal lines
       do k=1,nsegments+1
       do j=np(i,2)+np(i,3)-1,np(i,1)-1   
       call line3dn(tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + tesse3d(1,i,j+1,k),tesse3d(2,i,j+1,k),tesse3d(3,i,j+1,k),
     + ele3dc(13))
       if (pp29(4,4).eq.1) then ! draw symetric part
       call line3dn(-tesse3d(1,i,j,k),tesse3d(2,i,j,k),tesse3d(3,i,j,k),
     + -tesse3d(1,i,j+1,k),tesse3d(2,i,j+1,k),tesse3d(3,i,j+1,k),
     + ele3dc(13))
       end if
       end do ! j
       end do ! k
       end if ! wing type is not "ss"

       end if
       end if

       end do

       call dxfend(26)

       end if ! Close external dxf

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      21.12 Draw tessellation in 3D model (OpenSCAD view)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (pp29(5,1).eq.1) then ! Case SCAD and STL

c      Constructing OpenSCAD file
       write(27,*) "// http://www.laboratoridenvol.com"
       write(27,*) "// OpenSCAD file generated by LEparagliding version"
     + ," ",lepv
       write(27,*)

c      Avoid zero width centrall cell
       if (pp29(5,2).eq.1.and.rib(1,2).lt.0.10d0) then
       pp29(5,2)=2
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      22.12.1 Extrados surface
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Define points
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       write(27,*) "PolPoints_Ext = [ "

c      Keep the closing metadata valid when the selected panel range is empty
       npoints=0
       nlpanels=max(0,pp29(5,3)-pp29(5,2)+1)
       
       do i=pp29(5,2),pp29(5,3) ! Iterate in panels
       npoints=np(i,2)
       nlpanels=pp29(5,3)-pp29(5,2)+1 ! number of local panels

       do k=nsegments+1,1,-1 ! Iterate in segments

       write (27,*)
       write (27,*) "// Cell ",i," Line ",k,"from ",nsegments+1
       write (27,*)

       do j=1,np(i,2) 

       write (27,'(A3,1x,F12.3,A3,F12.3,A3,F12.3,A4,A9,1x,I6)') "[ ",
     + tesse3d(1,i,j,k)," , ",tesse3d(2,i,j,k)," , ",-tesse3d(3,i,j,k),
     + " ], ","// Point ",j+npoints*(nsegments+1-k)+
     + npoints*(nsegments+1)*(i-pp29(5,2))-1

       end do ! j
       end do ! k
       end do ! i

c      Add one point only to close the list, not used
       write (27,'(A3,1x,F12.3,A3,F12.3,A3,F12.3,A5,A9,1x,I6)') "[ ",
     + 0.0d0," , ",0.0d0," , ",0.0d0,
     + " ]]; ","// End ",(nsegments+1)*npoints*nlpanels

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Define faces
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       write(27,*)
       write(27,*) "PolFaces_Ext = [ "
       write(27,*)

       iini=1 ! control
       k0=0
       
       do k=1,(nsegments+1)*nlpanels-1

       k0=k0+1

c      Count cells
       if (k0.eq.nsegments+1) then
       iini=iini+1
       k0=0
       end if

       write (27,*)
       write (27,'(A9,1x,I2,A9,1x,I2,1x,A6,1x,I2)') 
     + "// Panel ",(iini-1)+pp29(5,2),
     + " Segment ",k0," from ",+ nsegments
       write (27,*)

c      if (nflag.ne.1) then ! Don't draw zero face

       do j=1,npoints-1

       write (27,'(A3,1x,I6,A3,I6,A3,I6,A3,I6,A19,I3,A12,I2,A7,I2)') 
     + "[ ",
     + j-1+(npoints)*(k-1)," , ",
     + j+(npoints)*(k-1)," , ",
     + j+(npoints)*(k)," , ",
     + j-1+(npoints)*(k),
     + " ], // Face number ",j," in segment ",k0,
     + " panel ",(iini-1)+pp29(5,2)
    
       end do ! j

       end do ! k
 
       write(27,*) " [ 0,0,0,0 ]]; // Close faces"

       write(27,*)
       write(27,*) "// Draw extrados surfaces"
       write(27,*) "color ([1,0,0])"
       write(27,*) "polyhedron ( PolPoints_Ext, PolFaces_Ext );"

       if (pp29(5,4).eq.1) then ! mirror
       write(27,*) "color ([1,0,0])"
       write(27,*) "mirror ([1,0,0])"
       write(27,*) "polyhedron ( PolPoints_Ext, PolFaces_Ext );"
       end if



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      22.12.1 Vents surface
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Define points
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       write(27,*) "PolPoints_Ven = [ "

c      Keep the closing metadata valid when the selected panel range is empty
       npoints=0
       nlpanels=max(0,pp29(5,3)-pp29(5,2)+1)
       
       do i=pp29(5,2),pp29(5,3) ! Iterate in panels
       npoints=np(i,3)
       nlpanels=pp29(5,3)-pp29(5,2)+1 ! number of local panels

       do k=nsegments+1,1,-1 ! Iterate in segments

       write (27,*)
       write (27,*) "// Cell ",i," Line ",k,"from ",nsegments+1
       write (27,*)

       do j=np(i,2),np(i,2)+np(i,3)-1

       write (27,'(A3,1x,F12.3,A3,F12.3,A3,F12.3,A4,A9,1x,I6)') "[ ",
     + tesse3d(1,i,j,k)," , ",tesse3d(2,i,j,k)," , ",-tesse3d(3,i,j,k),
     + " ], ","// Point ",j+npoints*(nsegments+1-k)+
     + npoints*(nsegments+1)*(i-pp29(5,2))-1-np(i,2)

       end do ! j
       end do ! k
       end do ! i

c      Add one point only to close the list, not used
       write (27,'(A3,1x,F12.3,A3,F12.3,A3,F12.3,A5,A9,1x,I6)') "[ ",
     + 0.0d0," , ",0.0d0," , ",0.0d0,
     + " ]]; ","// End ",(nsegments+1)*npoints*nlpanels

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Define faces
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       write(27,*)
       write(27,*) "PolFaces_Ven = [ "
       write(27,*)

       iini=1 ! control
       k0=0
       
       do k=1,(nsegments+1)*nlpanels-1

       k0=k0+1

c      Count cells
       if (k0.eq.nsegments+1) then
       iini=iini+1
       k0=0
       end if

       write (27,*)
       write (27,'(A9,1x,I2,A9,1x,I2,1x,A6,1x,I2)') 
     + "// Panel ",(iini-1)+pp29(5,2),
     + " Segment ",k0," from ",+ nsegments
       write (27,*)

c      if (nflag.ne.1) then ! Don't draw zero face

       if (rib(iini-1+pp29(5,2),14).eq.0.or.atp.eq."ss") then

       do j=1,npoints-1

       write (27,'(A3,1x,I6,A3,I6,A3,I6,A3,I6,A19,I3,A12,I2,A7,I2)') 
     + "[ ",
     + j-1+(npoints)*(k-1)," , ",
     + j+(npoints)*(k-1)," , ",
     + j+(npoints)*(k)," , ",
     + j-1+(npoints)*(k),
     + " ], // Face number ",j," in segment ",k0,
     + " panel ",(iini-1)+pp29(5,2)
    
       end do ! j

       end if

       end do ! k
 
       write(27,*) " [ 0,0,0,0 ]]; // Close faces"

       write(27,*)
       write(27,*) "// Draw extrados surfaces"
       write(27,*) "color ([0,0,1])"
       write(27,*) "polyhedron ( PolPoints_Ven, PolFaces_Ven );"

       if (pp29(5,4).eq.1) then ! mirror
       write(27,*) "color ([0,0,1])"
       write(27,*) "mirror ([1,0,0])"
       write(27,*) "polyhedron ( PolPoints_Ven, PolFaces_Ven );"
       end if


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      22.12.3 Intrados surface
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Define points
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       write(27,*) "PolPoints_Int = [ "

c      Keep the closing metadata valid when the selected panel range is empty
       npoints=0
       nlpanels=max(0,pp29(5,3)-pp29(5,2)+1)
       
       do i=pp29(5,2),pp29(5,3) ! Iterate in panels
       npoints=np(i,4)
       nlpanels=pp29(5,3)-pp29(5,2)+1 ! number of local panels

       do k=nsegments+1,1,-1 ! Iterate in segments

       write (27,*)
       write (27,*) "// Cell ",i," Line ",k,"from ",nsegments+1
       write (27,*)

       do j=np(i,2)+np(i,3)-1,np(i,1)

       write (27,'(A3,1x,F12.3,A3,F12.3,A3,F12.3,A4,A9,1x,I6)') "[ ",
     + tesse3d(1,i,j,k)," , ",tesse3d(2,i,j,k)," , ",-tesse3d(3,i,j,k),
     + " ], ","// Point ",j+npoints*(nsegments+1-k)+
     + npoints*(nsegments+1)*(i-pp29(5,2))-1-(np(i,2)+np(i,3)-1)

       end do ! j
       end do ! k
       end do ! i

c      Add one point only to close the list, not used
       write (27,'(A3,1x,F12.3,A3,F12.3,A3,F12.3,A5,A9,1x,I6)') "[ ",
     + 0.0d0," , ",0.0d0," , ",0.0d0,
     + " ]]; ","// End ",(nsegments+1)*npoints*nlpanels

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Define faces
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       write(27,*)
       write(27,*) "PolFaces_Int = [ "
       write(27,*)

       iini=1 ! control
       k0=0
       
       do k=1,(nsegments+1)*nlpanels-1

       k0=k0+1

c      Count cells
       if (k0.eq.nsegments+1) then
       iini=iini+1
       k0=0
       end if

       write (27,*)
       write (27,'(A9,1x,I2,A9,1x,I2,1x,A6,1x,I2)') 
     + "// Panel ",(iini-1)+pp29(5,2),
     + " Segment ",k0," from ",+ nsegments
       write (27,*)

c      if (nflag.ne.1) then ! Don't draw zero face

       do j=1,npoints-1

       write (27,'(A3,1x,I6,A3,I6,A3,I6,A3,I6,A19,I3,A12,I2,A7,I2)') 
     + "[ ",
     + j-1+(npoints)*(k-1)," , ",
     + j+(npoints)*(k-1)," , ",
     + j+(npoints)*(k)," , ",
     + j-1+(npoints)*(k),
     + " ], // Face number ",j," in segment ",k0,
     + " panel ",(iini-1)+pp29(5,2)
    
       end do ! j

       end do ! k
 
       write(27,*) " [ 0,0,0,0 ]]; // Close faces"

       write(27,*)
       write(27,*) "// Draw extrados surfaces"
       write(27,*) "color ([0,1,0])"
       write(27,*) "polyhedron ( PolPoints_Int, PolFaces_Int );"

       if (pp29(5,4).eq.1) then ! mirror
       write(27,*) "color ([0,1,0])"
       write(27,*) "mirror ([1,0,0])"
       write(27,*) "polyhedron ( PolPoints_Int, PolFaces_Int );"
       end if

c      End case OpenSCAD
       end if ! Case SCAD


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      21.13 Draw external STL file (CFD analysis)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (pp29(5,1).eq.1) then ! Draw external STL

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      21.13.1 Extrados surfaces
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       write (28,'(A14)') "solid extrados"
       write (28,*)

       write (29,'(A14)') "solid extrados"
       write (29,*)


       do i=pp29(5,2),pp29(5,3) ! Iterate in panels

       npoints=np(i,2)

       nlpanels=pp29(5,3)-pp29(5,2)+1 ! number of local panels

       do k=1,nsegments ! Iterate in segments

       do j=1,np(i,2)-1

c      Define points
       punt1(1)=tesse3d(1,i,j,k)
       punt1(2)=tesse3d(2,i,j,k)
       punt1(3)=-tesse3d(3,i,j,k)
       punt2(1)=tesse3d(1,i,j+1,k)
       punt2(2)=tesse3d(2,i,j+1,k)
       punt2(3)=-tesse3d(3,i,j+1,k)
       punt3(1)=tesse3d(1,i,j+1,k+1)
       punt3(2)=tesse3d(2,i,j+1,k+1)
       punt3(3)=-tesse3d(3,i,j+1,k+1)
       punt4(1)=tesse3d(1,i,j,k+1)
       punt4(2)=tesse3d(2,i,j,k+1)
       punt4(3)=-tesse3d(3,i,j,k+1)

       call planeby123(punt1,punt2,punt3,aplane,bplane,
     + cplane,dplane)
       call unitaryto123(unitv,aplane,bplane,cplane)

       write (28,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (28,*) "    outer loop"
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt1(1),punt1(2),punt1(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt2(1),punt2(2),punt2(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt3(1),punt3(2),punt3(3)
       write (28,*) "    endloop"
       write (28,'(A8)') "endfacet"
       write (28,*)

       write (29,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (29,*) "    outer loop"
       write (29,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt1(1),punt1(2),punt1(3)
       write (29,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt2(1),punt2(2),punt2(3)
       write (29,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt3(1),punt3(2),punt3(3)
       write (29,*) "    endloop"
       write (29,'(A8)') "endfacet"
       write (29,*)


c      Symmetry
       if (pp29(5,4).eq.1.and.i.ne.1) then
       write (28,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (28,*) "    outer loop"
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt1(1),punt1(2),punt1(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt2(1),punt2(2),punt2(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt3(1),punt3(2),punt3(3)
       write (28,*) "    endloop"
       write (28,'(A8)') "endfacet"
       write (28,*)
       end if

c      Symmetry
       if (pp29(5,4).eq.1.and.i.ne.1) then
       write (29,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (29,*) "    outer loop"
       write (29,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt1(1),punt1(2),punt1(3)
       write (29,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt2(1),punt2(2),punt2(3)
       write (29,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt3(1),punt3(2),punt3(3)
       write (29,*) "    endloop"
       write (29,'(A8)') "endfacet"
       write (29,*)
       end if


       call planeby123(punt3,punt4,punt1,aplane,bplane,
     + cplane,dplane)
       call unitaryto123(unitv,aplane,bplane,cplane)

       write (28,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (28,*) "    outer loop"
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt3(1),punt3(2),punt3(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt4(1),punt4(2),punt4(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt1(1),punt1(2),punt1(3)
       write (28,*) "    endloop"
       write (28,*) "endfacet"
       write (28,*)


       write (29,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (29,*) "    outer loop"
       write (29,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt3(1),punt3(2),punt3(3)
       write (29,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt4(1),punt4(2),punt4(3)
       write (29,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt1(1),punt1(2),punt1(3)
       write (29,*) "    endloop"
       write (29,*) "endfacet"
       write (29,*)


c      Symmetry
       if (pp29(5,4).eq.1.and.i.ne.1) then
       write (28,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (28,*) "    outer loop"
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt3(1),punt3(2),punt3(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt4(1),punt4(2),punt4(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt1(1),punt1(2),punt1(3)
       write (28,*) "    endloop"
       write (28,*) "endfacet"
       write (28,*)
       end if

c      Symmetry
       if (pp29(5,4).eq.1.and.i.ne.1) then
       write (29,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (29,*) "    outer loop"
       write (29,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt3(1),punt3(2),punt3(3)
       write (29,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt4(1),punt4(2),punt4(3)
       write (29,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt1(1),punt1(2),punt1(3)
       write (29,*) "    endloop"
       write (29,*) "endfacet"
       write (29,*)
       end if


       end do ! j
       end do ! k
       end do ! i

       write (28,'(A17)') "endsolid extrados"
       write (28,*)

       write (29,'(A17)') "endsolid extrados"
       write (29,*)


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      21.13.2 Vents surfaces
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


       write (28,'(A14)') "solid vents   "
       write (28,*)

       write (32,'(A14)') "solid vents   "
       write (32,*)


       do i=pp29(5,2),pp29(5,3) ! Iterate in panels

       if (rib(i,14).eq.0.or.atp.eq."ss".or.rib(i,14).eq.1) then ! Vents cases

       npoints=np(i,3)

       nlpanels=pp29(5,3)-pp29(5,2)+1 ! number of local panels

       do k=1,nsegments ! Iterate in segments

       do j=np(i,2),np(i,2)+np(i,3)-2

c      Define points
       punt1(1)=tesse3d(1,i,j,k)
       punt1(2)=tesse3d(2,i,j,k)
       punt1(3)=-tesse3d(3,i,j,k)
       punt2(1)=tesse3d(1,i,j+1,k)
       punt2(2)=tesse3d(2,i,j+1,k)
       punt2(3)=-tesse3d(3,i,j+1,k)
       punt3(1)=tesse3d(1,i,j+1,k+1)
       punt3(2)=tesse3d(2,i,j+1,k+1)
       punt3(3)=-tesse3d(3,i,j+1,k+1)
       punt4(1)=tesse3d(1,i,j,k+1)
       punt4(2)=tesse3d(2,i,j,k+1)
       punt4(3)=-tesse3d(3,i,j,k+1)

       call planeby123(punt1,punt2,punt3,aplane,bplane,
     + cplane,dplane)
       call unitaryto123(unitv,aplane,bplane,cplane)

       write (28,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (28,*) "    outer loop"
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt1(1),punt1(2),punt1(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt2(1),punt2(2),punt2(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt3(1),punt3(2),punt3(3)
       write (28,*) "    endloop"
       write (28,'(A8)') "endfacet"
       write (28,*)

       write (32,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (32,*) "    outer loop"
       write (32,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt1(1),punt1(2),punt1(3)
       write (32,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt2(1),punt2(2),punt2(3)
       write (32,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt3(1),punt3(2),punt3(3)
       write (32,*) "    endloop"
       write (32,'(A8)') "endfacet"
       write (32,*)


c      Symmetry
       if (pp29(5,4).eq.1.and.i.ne.1) then
       write (28,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (28,*) "    outer loop"
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt1(1),punt1(2),punt1(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt2(1),punt2(2),punt2(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt3(1),punt3(2),punt3(3)
       write (28,*) "    endloop"
       write (28,'(A8)') "endfacet"
       write (28,*)
       end if

c      Symmetry
       if (pp29(5,4).eq.1.and.i.ne.1) then
       write (32,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (32,*) "    outer loop"
       write (32,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt1(1),punt1(2),punt1(3)
       write (32,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt2(1),punt2(2),punt2(3)
       write (32,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt3(1),punt3(2),punt3(3)
       write (32,*) "    endloop"
       write (32,'(A8)') "endfacet"
       write (32,*)
       end if


       call planeby123(punt3,punt4,punt1,aplane,bplane,
     + cplane,dplane)
       call unitaryto123(unitv,aplane,bplane,cplane)

       write (28,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (28,*) "    outer loop"
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt3(1),punt3(2),punt3(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt4(1),punt4(2),punt4(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt1(1),punt1(2),punt1(3)
       write (28,*) "    endloop"
       write (28,*) "endfacet"
       write (28,*)

       write (32,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (32,*) "    outer loop"
       write (32,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt3(1),punt3(2),punt3(3)
       write (32,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt4(1),punt4(2),punt4(3)
       write (32,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt1(1),punt1(2),punt1(3)
       write (32,*) "    endloop"
       write (32,*) "endfacet"
       write (32,*)


c      Symmetry
       if (pp29(5,4).eq.1.and.i.ne.1) then
       write (28,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (28,*) "    outer loop"
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt3(1),punt3(2),punt3(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt4(1),punt4(2),punt4(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt1(1),punt1(2),punt1(3)
       write (28,*) "    endloop"
       write (28,*) "endfacet"
       write (28,*)
       end if

c      Symmetry
       if (pp29(5,4).eq.1.and.i.ne.1) then
       write (32,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (32,*) "    outer loop"
       write (32,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt3(1),punt3(2),punt3(3)
       write (32,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt4(1),punt4(2),punt4(3)
       write (32,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt1(1),punt1(2),punt1(3)
       write (32,*) "    endloop"
       write (32,*) "endfacet"
       write (32,*)
       end if


       end do ! j
       end do ! k
       end if ! Vents case
       end do ! i

       write (28,'(A17)') "endsolid vents   "
       write (28,*)

       write (32,'(A17)') "endsolid vents   "
       write (32,*)


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      21.13.1 Intrados surfaces
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (atp.ne."ss") then ! Not single skin

       write (28,'(A14)') "solid intrados"
       write (28,*)

       write (31,'(A14)') "solid intrados"
       write (31,*)


       do i=pp29(5,2),pp29(5,3) ! Iterate in panels

       npoints=np(i,4)

       nlpanels=pp29(5,3)-pp29(5,2)+1 ! number of local panels

       do k=1,nsegments ! Iterate in segments

       do j=np(i,2)+np(i,3)-1,np(i,1)-1

c      Define points
       punt1(1)=tesse3d(1,i,j,k)
       punt1(2)=tesse3d(2,i,j,k)
       punt1(3)=-tesse3d(3,i,j,k)
       punt2(1)=tesse3d(1,i,j+1,k)
       punt2(2)=tesse3d(2,i,j+1,k)
       punt2(3)=-tesse3d(3,i,j+1,k)
       punt3(1)=tesse3d(1,i,j+1,k+1)
       punt3(2)=tesse3d(2,i,j+1,k+1)
       punt3(3)=-tesse3d(3,i,j+1,k+1)
       punt4(1)=tesse3d(1,i,j,k+1)
       punt4(2)=tesse3d(2,i,j,k+1)
       punt4(3)=-tesse3d(3,i,j,k+1)

       call planeby123(punt1,punt2,punt3,aplane,bplane,
     + cplane,dplane)
       call unitaryto123(unitv,aplane,bplane,cplane)

       write (28,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (28,*) "    outer loop"
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt1(1),punt1(2),punt1(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt2(1),punt2(2),punt2(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt3(1),punt3(2),punt3(3)
       write (28,*) "    endloop"
       write (28,'(A8)') "endfacet"
       write (28,*)

       write (31,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (31,*) "    outer loop"
       write (31,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt1(1),punt1(2),punt1(3)
       write (31,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt2(1),punt2(2),punt2(3)
       write (31,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt3(1),punt3(2),punt3(3)
       write (31,*) "    endloop"
       write (31,'(A8)') "endfacet"
       write (31,*)


c      Symmetry
       if (pp29(5,4).eq.1.and.i.ne.1) then
       write (28,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (28,*) "    outer loop"
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt1(1),punt1(2),punt1(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt2(1),punt2(2),punt2(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt3(1),punt3(2),punt3(3)
       write (28,*) "    endloop"
       write (28,'(A8)') "endfacet"
       write (28,*)
       end if


c      Symmetry
       if (pp29(5,4).eq.1.and.i.ne.1) then
       write (31,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (31,*) "    outer loop"
       write (31,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt1(1),punt1(2),punt1(3)
       write (31,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt2(1),punt2(2),punt2(3)
       write (31,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt3(1),punt3(2),punt3(3)
       write (31,*) "    endloop"
       write (31,'(A8)') "endfacet"
       write (31,*)
       end if


       call planeby123(punt3,punt4,punt1,aplane,bplane,
     + cplane,dplane)
       call unitaryto123(unitv,aplane,bplane,cplane)

       write (28,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (28,*) "    outer loop"
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt3(1),punt3(2),punt3(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt4(1),punt4(2),punt4(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt1(1),punt1(2),punt1(3)
       write (28,*) "    endloop"
       write (28,*) "endfacet"
       write (28,*)

       write (31,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (31,*) "    outer loop"
       write (31,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt3(1),punt3(2),punt3(3)
       write (31,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt4(1),punt4(2),punt4(3)
       write (31,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",punt1(1),punt1(2),punt1(3)
       write (31,*) "    endloop"
       write (31,*) "endfacet"
       write (31,*)


c      Symmetry
       if (pp29(5,4).eq.1.and.i.ne.1) then
       write (28,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (28,*) "    outer loop"
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt3(1),punt3(2),punt3(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt4(1),punt4(2),punt4(3)
       write (28,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt1(1),punt1(2),punt1(3)
       write (28,*) "    endloop"
       write (28,*) "endfacet"
       write (28,*)
       end if

c      Symmetry
       if (pp29(5,4).eq.1.and.i.ne.1) then
       write (31,'(A13,x,F12.6,x,F12.6,x,F12.6)') 
     + "facet normal ",unitv(1),unitv(2),unitv(3)
       write (31,*) "    outer loop"
       write (31,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt3(1),punt3(2),punt3(3)
       write (31,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt4(1),punt4(2),punt4(3)
       write (31,'(A15,x,F12.3,x,F12.3,x,F12.3)') 
     + "        vertex ",-punt1(1),punt1(2),punt1(3)
       write (31,*) "    endloop"
       write (31,*) "endfacet"
       write (31,*)
       end if


       end do ! j
       end do ! k
       end do ! i

       write (28,'(A17)') "endsolid intrados"
       write (28,*)

       write (31,'(A17)') "endsolid intrados"
       write (31,*)


       end if ! atp not "ss"

       end if ! case STL


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
