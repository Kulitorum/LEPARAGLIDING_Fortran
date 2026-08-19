!> Construct and draw an ellipse fitted inside a four-point quadrilateral.
!! @param[in,out] pgx Horizontal point coordinates; points 1--4 are input and
!!                    helper points 5, 6, and 9 are written.
!! @param[in,out] pgy Vertical point coordinates; points 1--4 are input and
!!                    helper points 5, 6, and 9 are written.
!! @param[in] param1 Longitudinal ellipse size as a percentage of the center span.
!! @param[in] param2 Transverse size as a percentage of the two side distances.
!! @note Draws the resulting 50-segment ellipse through `line` on DXF unit 20.
       SUBROUTINE elliquad(pgx,pgy,param1,param2)

       real*8 pgx(100),pgy(100) ! generic point
       real*8 param1,param2
       real*8 pi,tetha,angle1,angleinc
       real*8 xp1,yp1,x1,y1,xp2,yp2,x2,y2
       real*8 a,b,distance,distance1,distance2

       pi=4.0d0*datan(1.0d0)

c      Auxiliar points in the quadrilater P1-P2-P3-P4
       pgx(5)=0.5*(pgx(1)+pgx(4))
       pgy(5)=0.5*(pgy(1)+pgy(4))
       pgx(6)=0.5*(pgx(2)+pgx(3))
       pgy(6)=0.5*(pgy(2)+pgy(3))
       pgx(9)=0.5*(pgx(5)+pgx(6))
       pgy(9)=0.5*(pgy(5)+pgy(6))

c      Ellipse angle
       tetha=datan((pgy(6)-pgy(5))/(pgx(6)-pgx(5)))

c      Distance P9-P8
       call distpr(pgx(1),pgy(1),pgx(2),pgy(2),pgx(9),pgy(9),distance)
       distance1=distance
c      Distance P9-P7
       call distpr(pgx(4),pgy(4),pgx(3),pgy(3),pgx(9),pgy(9),distance)
       distance2=distance

c      Definition of ellipse semiaxis
       a=(param1/200.)*dsqrt((pgx(5)-pgx(6))*(pgx(5)-pgx(6))+
     + (pgy(5)-pgy(6))*(pgy(5)-pgy(6)))
       b=(param2/200.)*(distance1+distance2)

c      Draw ellipses in 50 points
       angleinc=2.0d0*pi/dfloat(50)
       do i=1,50
       angle1=angleinc*dfloat(i-1)
       xp1=a*dcos(angle1)
       yp1=b*dsin(angle1)
       x1=xp1*dcos(tetha)-yp1*dsin(tetha)
       y1=xp1*dsin(tetha)+yp1*dcos(tetha)
       xp2=a*dcos(angle1+angleinc)
       yp2=b*dsin(angle1+angleinc)
       x2=xp2*dcos(tetha)-yp2*dsin(tetha)
       y2=xp2*dsin(tetha)+yp2*dcos(tetha)
       call line(pgx(9)+x1,pgy(9)+y1,pgx(9)+x2,pgy(9)+y2,3)
       end do

       return
       end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE distpr
c      Distance between point P3 and line r (P1-P2)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Calculate the perpendicular distance from a point to an infinite 2D line.
!! @param[in] p1x Horizontal coordinate of the first line point.
!! @param[in] p1y Vertical coordinate of the first line point.
!! @param[in] p2x Horizontal coordinate of the second line point.
!! @param[in] p2y Vertical coordinate of the second line point.
!! @param[in] p3x Horizontal coordinate of the query point.
!! @param[in] p3y Vertical coordinate of the query point.
!! @param[out] distance Absolute perpendicular distance.
!! @note Nearly vertical lines use the absolute X-coordinate difference.
       SUBROUTINE distpr(p1x,p1y,p2x,p2y,p3x,p3y,distance)

       real*8 p1x,p1y,p2x,p2y,p3x,p3y,distance
       real*8 A,B,C

c      Line Ax+By+C=0 by points P1,P2
       A=(p2y-p1y)/(p2x-p1x)
       B=-1.0d0
       C=(p2x*p1y-p1x*p2y)/(p2x-p1x)

c      Manage case A*A+B*B=0 !!!!!!!

c      Distance point P3 to line P1-P2
       distance=dabs((A*p3x+B*p3y+C)/dsqrt(A*A+B*B))

c      Manage special cases
c      Case p2x=p1x
       if (dabs(p2x-p1x).lt.0.001d0) then
       distance=dabs(p3x-p1x)
       end if

       return
       end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE arcfle
c      Draw arc known the segment 1-2 and the arrow flet
c      Arc between points P1-P2, arrow flet, sign, and color
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Draw a circular arc between two endpoints for a specified sagitta.
!! @param[in] p1x Horizontal coordinate of the first arc endpoint.
!! @param[in] p1y Vertical coordinate of the first arc endpoint.
!! @param[in] p2x Horizontal coordinate of the second arc endpoint.
!! @param[in] p2y Vertical coordinate of the second arc endpoint.
!! @param[in,out] flet Sagitta magnitude; values at or below 0.01 are raised to 0.01.
!! @param[in] iflet Arc-side sign, normally `-1` or `1`.
!! @param[in] isvet AutoCAD color index passed to the line writer.
!! @note The arc is approximated with 20 line segments on DXF unit 20.
       SUBROUTINE arcfle(p1x,p1y,p2x,p2y,flet,iflet,isvet)

       real*8 p1x,p1y,p2x,p2y,p3x,p3y,p4x,p4y,p5x,p5y,flet
       real*8 p6x,p6y,p7x,p7y
       real*8 R,h,s,tethaa
       real*8 tetha,omega,angle,angle0,incangle,pi,sgn
       integer iflet,isvet

       pi=4.0d0*datan(1.0d0)

c      Anticipate the case fle=0.0
       if (flet.le.0.01d0) then
       flet=0.01d0
       end if

c      Solve the circle x^2+y^2=R^2
       s=dsqrt((p2y-p1y)*(p2y-p1y)+(p2x-p1x)*(p2x-p1x))
       h=((s*s/4.)-flet*flet)/(2.*flet)
       R=flet+h

c      Draw arc
       sgn=dfloat(iflet)
       tethaa=datan((p2y-p1y)/(p2x-p1x))
       p3x=0.5*(p1x+p2x)
       p3y=0.5*(p1y+p2y)
       p4x=p3x+sgn*flet*dsin(tethaa)
       p4y=p3y-sgn*flet*dcos(tethaa)
       p5x=p3x-sgn*h*dsin(tethaa)
       p5y=p3y+sgn*h*dcos(tethaa)
       omega=datan(0.5d0*s/h)

       incangle=2.*omega/20.0d0
       angle0=0.5*pi-(tethaa+omega)

c       call line(p1x,p1y,p2x,p2y,3)
c       call line(p3x,p3y,p4x,p4y,2)
c       call line(p3x,p3y,p5x,p5y,1)

       do k=1,20
       angle=angle0+incangle*dfloat(k-1)
       p6x=p5x+sgn*R*dcos(angle)
       p6y=p5y-sgn*R*dsin(angle)
       p7x=p5x+sgn*R*dcos(angle+incangle)
       p7y=p5y-sgn*R*dsin(angle+incangle)
       call line(p6x,p6y,p7x,p7y,isvet)
       end do

       return
       end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE interpoly2D
c      Interpolate a point in a 2D polyline
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Locate a point at a cumulative distance along a 2D polyline interval.
!! @param[in] xpoly Polyline horizontal coordinates.
!! @param[in] ypoly Polyline vertical coordinates.
!! @param[out] x_poly Interpolated horizontal coordinate.
!! @param[out] y_poly Interpolated vertical coordinate.
!! @param[in] xpolylen Requested cumulative distance from `npoly1`.
!! @param[in] npoly1 First source point index.
!! @param[in] npoly2 Last source point index.
!! @param[out] npoly3 Index of the segment containing the result.
!! @param[out] distrel Fractional distance within that segment.
       SUBROUTINE interpoly2d(xpoly,ypoly,x_poly,y_poly
     + ,xpolylen,npoly1,npoly2,npoly3,distrel)

       real*8 xpoly(500),ypoly(500),x_poly,y_poly,xpolylen
       real*8 xacum1,xacum2,angle,pi,dist,distrel
       integer npoly1,npoly2,npoly3

       pi=4.0d0*datan(1.0d0)

       xacum1=0.0d0
       xacum2=0.0d0

       do j=npoly1,npoly2-1

       xacum2=xacum2+dsqrt((xpoly(j+1)-xpoly(j))*(xpoly(j+1)-xpoly(j))+
     + (ypoly(j+1)-ypoly(j))*(ypoly(j+1)-ypoly(j)))

c      Detect segment and interpolate easy (without angles!)
       if (xpolylen.ge.xacum1.and.xpolylen.le.xacum2) then

       dist=xpolylen-xacum1
       distrel=dist/(xacum2-xacum1) ! Relative distance in interpolation segment
       x_poly=xpoly(j)+distrel*(xpoly(j+1)-xpoly(j))
       y_poly=ypoly(j)+distrel*(ypoly(j+1)-ypoly(j))

       npoly3=j

       end if

       xacum1=xacum2
  
       end do

       return
       end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE interpoly3D
c      Interpolate a point in a 2D polyline
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Locate a point at a cumulative distance along a 3D polyline interval.
!! @param[in] xpoly Polyline X coordinates.
!! @param[in] ypoly Polyline Y coordinates.
!! @param[in] zpoly Polyline Z coordinates.
!! @param[out] x_poly Interpolated X coordinate.
!! @param[out] y_poly Interpolated Y coordinate.
!! @param[out] z_poly Interpolated Z coordinate.
!! @param[in] xpolylen Requested cumulative distance from `npoly1`.
!! @param[in] npoly1 First source point index.
!! @param[in] npoly2 Last source point index.
!! @param[out] npoly3 Index of the segment containing the result.
!! @param[out] distrel Fractional distance within that segment.
       SUBROUTINE interpoly3d(xpoly,ypoly,zpoly,x_poly,y_poly,z_poly,
     + xpolylen,npoly1,npoly2,npoly3,distrel)

       real*8 xpoly(500),ypoly(500),zpoly(500),x_poly,y_poly,z_poly
       real*8 xpolylen,xacum1,xacum2,angle,pi,dist,distrel
       integer npoly1,npoly2,npoly3

       pi=4.0d0*datan(1.0d0)

       xacum1=0.0d0
       xacum2=0.0d0

       do j=npoly1,npoly2-1

       xacum2=xacum2+dsqrt((xpoly(j+1)-xpoly(j))*(xpoly(j+1)-xpoly(j))+
     + (ypoly(j+1)-ypoly(j))*(ypoly(j+1)-ypoly(j))+
     + (zpoly(j+1)-zpoly(j))*(zpoly(j+1)-zpoly(j)))

c      Detect segment and interpolate easy (without angles!)
       if (xpolylen.ge.xacum1.and.xpolylen.le.xacum2) then

       dist=xpolylen-xacum1
       distrel=dist/(xacum2-xacum1) ! Relative distance in interpolation segment
       x_poly=xpoly(j)+distrel*(xpoly(j+1)-xpoly(j))
       y_poly=ypoly(j)+distrel*(ypoly(j+1)-ypoly(j))
       z_poly=zpoly(j)+distrel*(zpoly(j+1)-zpoly(j))

       npoly3=j

       end if

       xacum1=xacum2

       end do

       return
       end



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     SUBROUTINE eo (even odd detection)
c     evenodd = 1 ---> odd (parell 28)
c     evenodd = 0 ---> even (senar 29)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Classify a cell count as even or odd using the legacy convention.
!! @param[in] ncells Total number of wing cells.
!! @param[out] evenodd `0` for an even cell count and `1` for an odd count.
       SUBROUTINE eo(ncells,evenodd)

       integer evenodd,nribss,ncells

       control=((float(ncells))/2.)-float(int(float(ncells)/2.))
       
       if (control.eq.0.) then  ! even-parell
       nribss=int((float(ncells)/2.)+1.)
c      nribss=(ncells/2)+1
       evenodd=0
       end if

       if (control.ne.0) then  ! odd-senar
       nribss=int(((float(ncells)+1.)/2.))
c      nribss=(ncells+1)/2
       evenodd=1
       end if

       return
       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     SUBROUTINE distanceabc(x1,y1,z1,x2,y2,z2,x3,y3,z3,distance)
c     Calcules point 3 at a distance from 1, in segment between points 1 and 2
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Locate a point a signed distance from point 1 along the line toward point 2.
!! @param[in] x1 X coordinate of the line origin.
!! @param[in] y1 Y coordinate of the line origin.
!! @param[in] z1 Z coordinate of the line origin.
!! @param[in] x2 X coordinate of a point defining the line direction.
!! @param[in] y2 Y coordinate of a point defining the line direction.
!! @param[in] z2 Z coordinate of a point defining the line direction.
!! @param[out] x3 Result X coordinate.
!! @param[out] y3 Result Y coordinate.
!! @param[out] z3 Result Z coordinate.
!! @param[in] distance Signed distance measured from point 1.
       SUBROUTINE distance123(x1,y1,z1,x2,y2,z2,x3,y3,z3,distance)

       implicit none
       real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,distance
       type(vector_3d) point1,point2,output

       point1=vector_3d(x1,y1,z1)
       point2=vector_3d(x2,y2,z2)
       output=point_along_line(point1,point2,distance)

       x3=output%x
       y3=output%y
       z3=output%z

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     SUBROUTINE tessella(x1,y1,z1,x2,y2,z2,x3,y3,z3,distance)
c     
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Generate transverse tessellation points across a ballooned panel.
!! @param[in] i Right-hand rib index; rib `i-1` supplies the opposite panel side.
!! @param[in] rib Rib matrix retained for interface compatibility.
!! @param[in] np Profile point counts.
!! @param[in] u Prepared global X coordinates in slot 47.
!! @param[in] v Prepared global Y coordinates in slot 48.
!! @param[in] w Prepared global Z coordinates in slot 49.
!! @param[in] hautok Ballooning height at each profile point.
!! @param[in] nsegments Number of transverse subdivisions; must be positive.
!! @param[out] tesse3d Generated coordinates indexed by component, rib, point,
!!                     and transverse sample.
       SUBROUTINE tessella(i,rib,np,u,v,w,hautok,nsegments,tesse3d)

       real*8 x0,y0,z0,x1,y1,z1,x2,y2,z2,x3,y3,z3,distance,distot,distu
       real*8 x4,y4,z4,x5,y5,z5,x6,y6,z6,x7,y7,z7
       integer np(0:100,9),nsegments
       real*8 u(0:100,500,99),v(0:100,500,99),w(0:100,500,99)
       real*8 rib(0:100,500)
       real*8 hautok(0:100,500),d,q,R,theta,omega,pi,alpha
       real*8 tesse3d(3,0:100,500,99)

       pi=4.0d0*datan(1.0d0)
       
c      Start loop for each airfoil point
       do j=1,np(i,1)

c      1. Calcule arc local points

       d=dsqrt((u(i-1,j,47)-u(i,j,47))**2+(v(i-1,j,47)-v(i,j,47))**2+
     + (w(i-1,j,47)-w(i,j,47))**2)

       q=(d*d/(8.0*hautok(i,j)))-0.5*hautok(i,j)
       R=q+hautok(i,j)

       theta=datan(q/(0.50d0*d))
       omega=pi-(2.0d0*theta)

c      Start tessellation points

c      Local tessellation
c      Case hautok < 0.01
       if (hautok(i,j).ge.0.01d0) then
       do k=1,nsegments+1
       alpha=theta+(omega/dfloat(nsegments))*dfloat(k-1)
       x0=0.5*d-R*dcos(alpha)
       z0=R*dsin(alpha)-q

c      Interpolate points in space
       x1=u(i,j,47)
       y1=v(i,j,47)
       z1=w(i,j,47)
       x2=u(i-1,j,47)
       y2=v(i-1,j,47)
       z2=w(i-1,j,47)
       distance=x0
       call distance123(x1,y1,z1,x2,y2,z2,x3,y3,z3,distance)
       x5=x3
       y5=y3
       z5=z3

       x1=u(i,j,48)
       y1=v(i,j,48)
       z1=w(i,j,48)
       x2=u(i,j,49)
       y2=v(i,j,49)
       z2=w(i,j,49)
       distance=z0
       call distance123(x1,y1,z1,x2,y2,z2,x3,y3,z3,distance)
       x7=x3
       y7=y3
       z7=z3

       x4=x1
       y4=y1
       z4=z1

       x6=x5+(x7-x4)
       y6=y5+(y7-y4)
       z6=z5+(z7-z4)

c      Define tessellation points
       tesse3d(1,i,j,k)=x6
       tesse3d(2,i,j,k)=y6
       tesse3d(3,i,j,k)=z6

       end do !k segments
       end if ! hautok

c      Case hautok >= 0.01
       if (hautok(i,j).lt.0.01d0) then
       do k=1,nsegments+1
       x0=dfloat(k-1)*d/dfloat(nsegments)
       z0=0.0d0

c      Interpolate points in space
       x1=u(i,j,47)
       y1=v(i,j,47)
       z1=w(i,j,47)
       x2=u(i-1,j,47)
       y2=v(i-1,j,47)
       z2=w(i-1,j,47)
       distance=x0
       call distance123(x1,y1,z1,x2,y2,z2,x3,y3,z3,distance)
       x5=x3
       y5=y3
       z5=z3

c      Define tessellation points
       tesse3d(1,i,j,k)=x5
       tesse3d(2,i,j,k)=y5
       tesse3d(3,i,j,k)=z5

       end do ! k
       end if ! hautok

       end do !j

       return
       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     SUBROUTINE        interpseg(jini,jfin,jc,ldif,
c                       useg,vseg,linter,upoint,vpoint)
c     
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

