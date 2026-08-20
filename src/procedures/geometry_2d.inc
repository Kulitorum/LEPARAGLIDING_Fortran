!> Redistribute a 2D polyline to a requested number of approximately spaced points.
!! @param[in] xlin1 Horizontal coordinates of the source polyline.
!! @param[in] ylin1 Vertical coordinates of the source polyline.
!! @param[out] xlin3 Horizontal coordinates of the redistributed polyline.
!! @param[out] ylin3 Vertical coordinates of the redistributed polyline.
!! @param[in] n1vr Number of valid source points; must be at least two.
!! @param[in] n2vr Requested number of output points; must be at least two.
!! @note The legacy algorithm first samples every source segment in tenths; it
!!       is therefore approximate rather than an arc-length resampler.
       subroutine vredis(xlin1,ylin1,xlin3,ylin3,n1vr,n2vr)  

       real*8 xlin1(5000),ylin1(5000)
       real*8 xlin2(50000),ylin2(50000)
       real*8 xlin3(5000),ylin3(5000)
   

c      Case n1vr > 3

c      j2vr counter in x10 multiplied vector
       j2vr=0

c      Define local vector
       do j1vr=1,n1vr-1

       xj=xlin1(j1vr)
       yj=ylin1(j1vr)
       xjm1=xlin1(j1vr+1)
       yjm1=ylin1(j1vr+1)

       do kvr=0,10
       stvr=float(kvr)/10.
       j2vr=j2vr+1
c      Parametric equation in each segment
       xlin2(j2vr)=xj+stvr*(xjm1-xj)
       ylin2(j2vr)=yj+stvr*(yjm1-yj)
       end do
       j2vr=j2vr-1

       end do

       j2max=j2vr+1
      
       icount=int(float((10*n1vr-1)/(n2vr-1)))
       iespai=icount*(n2vr-1)
       isobra=10*(n1vr-1)-iespai

       itotes=10*(n1vr-1)

c      Assign vector 3

c      j3 counter in final vector
       j3vr=1
       j2vr=1
       do j22vr=1,j2max

       xlin3(j3vr)=xlin2(j2vr)
       ylin3(j3vr)=ylin2(j2vr)

c      Ajust exactly the final point
       if (j3vr.eq.n2vr) then
       xlin3(j3vr)=xlin1(n1vr)
       ylin3(j3vr)=ylin1(n1vr)
       end if

c      Assign excess of spaces
       iplus=0
       if (j3vr.le.isobra) then
       iplus=1
       end if

c      Count
       do ijkvr=1,icount+iplus
       j2vr=j2vr+1
       end do
      
       j3vr=j3vr+1

       end do

c      Special case n1vr=2
       if (n1vr.eq.2) then 
             
       disx=(xlin1(2)-xlin1(1))/float(n2vr-1)
       disy=(ylin1(2)-ylin1(1))/float(n2vr-1)

       do j=1,n2vr
       xlin3(j)=xlin1(1)+disx*float(j-1)
       ylin3(j)=ylin1(1)+disy*float(j-1)
       end do

       end if

       return

       end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     SUBROUTINE r and s lines 2D intersection
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Find the intersection of two infinite 2D lines, r and s.
!! @param[in] xru Horizontal coordinates of two points defining line r.
!! @param[in] xrv Vertical coordinates of two points defining line r.
!! @param[in] xsu Horizontal coordinates of two points defining line s.
!! @param[in] xsv Vertical coordinates of two points defining line s.
!! @param[out] xtu Horizontal coordinate of the line intersection.
!! @param[out] xtv Vertical coordinate of the line intersection.
!! @note Vertical lines receive special handling; parallel coincident lines do
!!       not have a defined result in this legacy implementation.
      SUBROUTINE xrxs(xru,xrv,xsu,xsv,xtu,xtv)

      real*8 xru(2),xrv(2),xsu(2),xsv(2)

      xmr=(xrv(2)-xrv(1))/(xru(2)-xru(1))
      xbr=xrv(1)-xmr*xru(1)
c      if (dabs((xru(2)-xru(1))).le.0.0001) then
c      xmr=(xrv(2)-xrv(1))/0.000001
c      end if

      xms=(xsv(2)-xsv(1))/(xsu(2)-xsu(1))
      xbs=xsv(1)-xms*xsu(1)
c      if (dabs((xsu(2)-xsu(1))).le.0.0001) then
c      xms=(xsv(2)-xsv(1))/0.000001
c      end if

      xtu=(xbs-xbr)/(xmr-xms)
      xtv=xmr*xtu+xbr

c     Case xsu(1)=xsu(2)
      if (dabs((xsu(2)-xsu(1))).le.0.0001d0) then
      xtu=xsu(1)
      xtv=xmr*xsu(1)+xbr
      end if

c     Case xru(1)=xru(2)
      if (dabs((xru(2)-xru(1))).le.0.0001d0) then
      xtu=xru(1)
      xtv=xms*xru(1)+xbs
      end if

c      write (*,*) "Z ", xru(1),xrv(1),xmr,xms,xtu,xtv

      return

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     SUBROUTINE  flattening
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Develop the quadrilateral strip between ribs `ni` and `ni+1` into 2D.
!! @param[in] ni Index of the first rib in the strip.
!! @param[in] npunts Number of spanwise/profile quadrilaterals to develop.
!! @param[in] rx Source X coordinates of the rib grid.
!! @param[in] ry Source Y coordinates of the rib grid.
!! @param[in] rz Source Z coordinates of the rib grid.
!! @param[out] pl1x Developed first-left X coordinate per quadrilateral.
!! @param[out] pl1y Developed first-left Y coordinate per quadrilateral.
!! @param[out] pl2x Developed second-left X coordinate per quadrilateral.
!! @param[out] pl2y Developed second-left Y coordinate per quadrilateral.
!! @param[out] pr1x Developed first-right X coordinate per quadrilateral.
!! @param[out] pr1y Developed first-right Y coordinate per quadrilateral.
!! @param[out] pr2x Developed second-right X coordinate per quadrilateral.
!! @param[out] pr2y Developed second-right Y coordinate per quadrilateral.
!! @note Distances are preserved by triangulating each source quadrilateral.
       SUBROUTINE flatt(ni,npunts,rx,ry,rz,
     + pl1x,pl1y,pl2x,pl2y,pr1x,pr1y,pr2x,pr2y)

       real*8 rx(0:100,500),ry(0:100,500),rz(0:100,500)

       real*8 pl1x(0:100,500),pl1y(0:100,500),pl2x(0:100,500),
     + pl2y(0:100,500)
       real*8 pr1x(0:100,500),pr1y(0:100,500),pr2x(0:100,500),
     + pr2y(0:100,500)

       real*8 phr,pa1r,pa2r,phl,pa1l,pa2l,px0,py0,ptheta,pw1
       real*8 pa,pb,pc,pd,pe,pf

       i=ni

       px0=0.0d0
       py0=0.0d0
       ptheta=0.0d0

       do j=1,npunts

c       write (*,*) "rx ry rx ", j, rx(i,j),ry(i,j),rz(i,j)

c      Distances between points
       pa=dsqrt((rx(i+1,j)-rx(i,j))**2.0d0+(ry(i+1,j)-ry(i,j))**2.0d0+
     + (rz(i+1,j)-rz(i,j))**2.0d0)
       pb=dsqrt((rx(i+1,j+1)-rx(i,j))**2.0d0+(ry(i+1,j+1)-ry(i,j))**
     + 2.0d0+(rz(i+1,j+1)-rz(i,j))**2.0d0)
       pc=dsqrt((rx(i+1,j+1)-rx(i+1,j))**2.0d0+(ry(i+1,j+1)-ry(i+1,j))
     + **2.+(rz(i+1,j+1)-rz(i+1,j))**2.0d0)
       pd=dsqrt((rx(i+1,j)-rx(i,j+1))**2.0d0+(ry(i+1,j)-ry(i,j+1))**
     + 2.0d0+(rz(i+1,j)-rz(i,j+1))**2.0d0)
       pe=dsqrt((rx(i,j+1)-rx(i,j))**2.0d0+(ry(i,j+1)-ry(i,j))**2.0d0+
     + (rz(i,j+1)-rz(i,j))**2.0d0)
       pf=dsqrt((rx(i+1,j+1)-rx(i,j+1))**2.0d0+(ry(i+1,j+1)-ry(i,j+1))
     + **2.0d0+(rz(i+1,j+1)-rz(i,j+1))**2.0d0)
       
       pa2r=(pa*pa-pb*pb+pc*pc)/(2.0d0*pa)
       pa1r=pa-pa2r
       phr=dsqrt(pc*pc-pa2r*pa2r)

       pa2l=(pa*pa-pe*pe+pd*pd)/(2.0d0*pa)
       pa1l=pa-pa2l
       phl=dsqrt(pd*pd-pa2l*pa2l)

       pb2t=(pb*pb-pe*pe+pf*pf)/(2.0d0*pb)
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

       return

       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     SUBROUTINE  axisch
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Copy a 2D point sequence through the legacy axis-change rotation.
!! @param[in] npunts Number of valid points.
!! @param[out] angle Applied angle in degrees; currently forced to zero.
!! @param[in] px9i Input horizontal coordinates.
!! @param[in] py9i Input vertical coordinates.
!! @param[out] px9o Rotated horizontal coordinates.
!! @param[out] py9o Rotated vertical coordinates.
!! @note Because the routine resets `angle`, current behavior is an identity copy.
       SUBROUTINE axisch(npunts,angle,px9i,py9i,px9o,py9o)

       real*8 px9i(500),py9i(500)
       real*8 px9o(500),py9o(500)

       real*8 angle,xc,xs,pi

       pi=4.0d0*datan(1.0d0)
       angle=0.0d0

       do j=1,npunts

       xc=dcos(angle*pi/180.0d0)
       xs=dsin(angle*pi/180.0d0)

       px9o(j)=xc*px9i(j)-xs*py9i(j)
       py9o(j)=xs*px9i(j)+xc*py9i(j)

       end do

       return

       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE angdis1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE angdis2
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      ES FA SERVIR??????????????
c      REVISAR el double precision:
c
c      gfortran-8 -g -fbacktrace -fdefault-real-8 
c      -falign-commons -fno-automatic -finit-local-zero 
c      -mcmodel=medium leparagliding.f 
c 

!> Preserve the unfinished legacy angle/distance helper for compatibility.
!! @param[in] p1u Horizontal coordinate of the first direction point.
!! @param[in] p1v Vertical coordinate of the first direction point.
!! @param[in] p2u Horizontal coordinate of the second direction point.
!! @param[in] p2v Vertical coordinate of the second direction point.
!! @param[in,out] p3u Intended offset horizontal coordinate; currently unchanged.
!! @param[in,out] p3v Intended offset vertical coordinate; currently unchanged.
!! @param[in,out] angl Intended line angle; currently left unchanged.
!! @param[in] dist Intended offset distance.
!! @warning All calculations that would set `angl` or point 3 are commented out;
!!          callers must not rely on this routine producing geometry.
       SUBROUTINE angdis2(p1u,p1v,p2u,p2v,p3u,p3v,angl,dist)

       real*8 du,dv,angl

       real*8 p1u,p1v,p2u,p2v,p3u,p3v,dist

       du=p2u-p1u
       dv=p2v-p1v

c       write (*,*) "sub ",p1u,p1v,p2u,p2v
      
       if (du.ne.0.) then 
c       angl=dabs(datan(dv/du))
       end if
ccccccccccccccccc       if (du.eq.0.) then angl=2.0d0*datan(1.0d0)
c       write (*,*) "sub dv du",dv,du,angl

c      Case 1
       if (du.ge.0.and.dv.ge.0) then
c       p3u=p1u-dist*dsin(angl)
c       p3v=p1v+dist*dcos(angl)
       end if

c      Case 2
       if (du.lt.0.and.dv.ge.0) then
c       p3u=p1u-dist*dsin(angl)
c       p3v=p1v-dist*dcos(angl)
       end if

c      Case 3
       if (du.ge.0.and.dv.le.0) then
c       p3u=p1u+dist*dsin(angl)
c       p3v=p1v+dist*dcos(angl)
       end if

c      Case 4
       if (du.lt.0.and.dv.le.0) then
c       p3u=p1u+dist*dsin(angl)
c       p3v=p1v-dist*dcos(angl)
       end if

c       write(*,*) "4444 ",angl,p3u,p3v

      
       return

       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE angdis3
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE d3p
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE EXTERNAL POINTS IN A PANEL
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Computes external points 11-24-14 and 12-25-15
c      in a panel from 1 to npo
c      but draws nothing
c
c      WARNING IN REDEFINITION VARIABLES u, v !!!
c      ISOLATE THEM!

!> Fit an ellipse inside a four-corner H/V-rib strip at a span fraction.
!! @param[in] lx1,ly1 Left edge at the first longitudinal station.
!! @param[in] rx1,ry1 Right edge at the first longitudinal station.
!! @param[in] lx2,ly2 Left edge at the second longitudinal station.
!! @param[in] rx2,ry2 Right edge at the second longitudinal station.
!! @param[in] span_fraction Position between the two stations, in `[0,1]`.
!! @param[in] height_percent Semiaxis along the local strip height, percent.
!! @param[in] width_percent Perpendicular semiaxis, percent of local height.
!! @param[in] center_fraction Center position from the local left to right edge.
!! @param[out] x0,y0 Ellipse center.
!! @param[out] a,b Ellipse semiaxes.
!! @param[out] angle Rotation of semiaxis `a` in radians.
       SUBROUTINE vrib_hole_ellipse(lx1,ly1,rx1,ry1,lx2,ly2,rx2,ry2,
     + span_fraction,height_percent,width_percent,center_fraction,
     + x0,y0,a,b,angle)
       implicit none
       real*8 lx1,ly1,rx1,ry1,lx2,ly2,rx2,ry2
       real*8 span_fraction,height_percent,width_percent,center_fraction
       real*8 x0,y0,a,b,angle,lx,ly,rx,ry,height

       lx=lx1+span_fraction*(lx2-lx1)
       ly=ly1+span_fraction*(ly2-ly1)
       rx=rx1+span_fraction*(rx2-rx1)
       ry=ry1+span_fraction*(ry2-ry1)
       height=dsqrt((rx-lx)**2+(ry-ly)**2)
       angle=datan2(ry-ly,rx-lx)
       x0=lx+center_fraction*(rx-lx)
       y0=ly+center_fraction*(ry-ly)
       a=0.005d0*height*height_percent
       b=0.005d0*height*width_percent
       end

