      SUBROUTINE datair(i,rib,np,u,v)

      integer np(0:100,9),kini(0:100),kfin(0:100),npini,npfin
      integer jkini,jkfin
      real*8 u(0:100,500,99),v(0:100,500,99)
      real*8 rib(0:100,500),xini,xfin,yini,yfin,d0,d1,d2,d3,dm
      real*8 ucont(500),vcont(500)

c     Read and count airfoil points

      rewind (24)

      read (24,*)

c      Read up to 1000 points and exit if end of file

       k=1

       do 100 j=1,1000

       read (24,*,IOSTAT=io) u(i,j,1),v(i,j,1)

       write (*,*) i,k,u(i,j,1),v(i,j,1)

       k=k+1

c      Exit if end of file
       if (io.lt.0) goto 200

 100   continue

 200   continue

       np(i,1)=k-2
      
       write (*,*) "Num punts initial",np(i,1)

c      Set inlet points

       xini=rib(i,11)/100.
       xfin=rib(i,12)/100.
       kini(i)=0
       kfin(i)=0

       do j=1,np(i,1)

c      Detect xini and reasign if necesary

       if (u(i,j,1).le.xini.and.u(i,j+1,1).gt.xini.and.v(i,j,1).
     + le.0.0d0.and.rib(i,11).ge.0.0d0) then
       jkini=j
       write (*,*) "jkini= ",j

       d0=dsqrt((u(i,j,1)-u(i,j-1,1))**2.+(v(i,j,1)-v(i,j-1,1))**2.)
       xm=(v(i,j+1,1)-v(i,j,1))/(u(i,j+1,1)-u(i,j,1))
       xb=v(i,j,1)-xm*u(i,j,1)
       yini=xm*xini+xb
       d1=dsqrt((xini-u(i,j,1))**2.+(yini-v(i,j,1))**2.)
       d2=dsqrt((xini-u(i,j+1,1))**2.+(yini-v(i,j+1,1))**2.)
       d3=dsqrt((u(i,j+2,1)-u(i,j+1,1))**2.+(v(i,j+2,1)-v(i,j+1,1))**2.)
       dm=(d0+d1+d2+d3)/3.0d0

       write (*,*) "Ep ini"
       write (*,*) u(i,j,1),xini
       write (*,*) v(i,j,1),yini
       write (*,*) d0,d1,d2,d3,dm

       if (d1.lt.(dm/5.0)) then ! move point j to xini
       u(i,j,1)=xini
       v(i,j,1)=yini
       endif

       if (d2.lt.(dm/5.0)) then ! move point j+1 to xini
       u(i,j+1,1)=xini
       v(i,j+1,1)=yini
       endif

       if (d1.ge.(dm/5.0).and.d2.ge.(dm/5)) then ! add point
       kini=1
       endif

       end if ! xini

c      A verificar gràficament (!!!!) cas inici extrados
c      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       if (u(i,j,1).ge.xini.and.u(i,j+1,1).lt.xini.and.v(i,j,1).
     + gt.0.0d0.and.rib(i,11).lt.0.0d0) then
       jkini=j
       write (*,*) "jkini= ",j
       end if

c      Detect xfin and reasign if necesary

       if (u(i,j,1).le.xfin.and.u(i,j+1,1).gt.xfin.and.v(i,j,1).
     + le.0.0d0) then
       jkfin=j
       write (*,*) "jkfin= ",j

       d0=dsqrt((u(i,j,1)-u(i,j-1,1))**2.+(v(i,j,1)-v(i,j-1,1))**2.)
       xm=(v(i,j+1,1)-v(i,j,1))/(u(i,j+1,1)-u(i,j,1))
       xb=v(i,j,1)-xm*u(i,j,1)
       yfin=xm*xfin+xb
       d1=dsqrt((xfin-u(i,j,1))**2.+(yfin-v(i,j,1))**2.)
       d2=dsqrt((xfin-u(i,j+1,1))**2.+(yfin-v(i,j+1,1))**2.)
       d3=dsqrt((u(i,j+2,1)-u(i,j+1,1))**2.+(v(i,j+2,1)-v(i,j+1,1))**2.)
       dm=(d0+d1+d2+d3)/3.0d0

       write (*,*) "Ep fin"
       write (*,*) u(i,j,1),xfin
       write (*,*) v(i,j,1),yfin
       write (*,*) d0,d1,d2,d3,dm

       if (d1.lt.(dm/5.0)) then ! move point j to xini
       u(i,j,1)=xfin
       v(i,j,1)=yfin
       endif

       if (d2.lt.(dm/5.0)) then ! move point j+1 to xini
       u(i,j+1,1)=xfin
       v(i,j+1,1)=yfin
       endif

       if (d1.ge.(dm/5.0).and.d2.ge.(dm/5)) then ! add point
       kfin(i)=1
       endif

       end if ! xfin

       end do ! j

c      Points counting
       np(i,1)=np(i,1)+kini(i)+kfin(i)
       np(i,2)=jkini+kini(i)
       np(i,3)=jkfin-jkini+1
       np(i,4)=np(i,1)-jkfin+1+kfin(i)

       write (*,*) "np(i,1)= ",np(i,1)
       write (*,*) "np(i,2)= ",np(i,2)
       write (*,*) "np(i,3)= ",np(i,3)
       write (*,*) "np(i,4)= ",np(i,4)

c      Reasign airfoil points

    
c      Remap airfoils using predefined points

       npini=1
       npfin=np(i,2)
       npobj=66

       do j=npini,npfin
       ucont(j)=u(i,j,1)
       vcont(j)=v(i,j,1)
       end do

       call remapcont(npini,npfin,npobj,ucont,vcont)

       return
       end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE REMAP CONTOUR
c      call subroutine remapcont
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       SUBROUTINE remapcont (npini,npfin,npobj,ucont,vcont)

       integer npini,npfin,npobj
       real*8 ucont(500),vcont(500),dx(500),dy(500),da(0:500),ds(500)
       real*8 dxn(0:500),dyn(0:500),dsn(0:500),dan(0:500)
       real*8 xl,dm,dmn,dyy,xln


c      Remap airfoils using predefined points

c      Law of points distribution

       xl=0.0d0
       da(npini-1)=0.0d0

c      Contour length
       do j=npini,npfin-1
       xl=xl+dsqrt((ucont(j)-ucont(j+1))**2.0+(vcont(j)
     + -vcont(j+1))**2.0)
       end do

       dm=xl/dfloat(npfin-npini)

       write (*,*) "Ep reformat"
       write (*,*) npini,npfin

c      Law of points distribution
       do j=npini,npfin-1
       dy(j)=dsqrt((ucont(j)-ucont(j+1))**2.+(vcont(j)-vcont(j+1))**2.)
     + /dm
       ds(j)=dsqrt((ucont(j)-ucont(j+1))**2.0+(vcont(j)
     + -vcont(j+1))**2.0)
       da(j)=da(j-1)+ds(j)
       dx(j)=da(j-1)/xl
       write (*,*) j,ds(j)*100.,da(j)*100.,dx(j)
       end do
       dx(npfin)=1.0d0
       dy(npfin)=dy(npfin-1)
       write (*,*) xl*100.

       write (*,*) "Law of points distribution"
       do j=npini,npfin
       write (*,*) j,dx(j),dy(j)
       end do

c      Reformat unitari segment

       dmn=1.0d0/dfloat(npobj-1)
       dan(0)=0.0d0
       dsn(0)=0.0d0
       xln=0.0d0

       do k=1,npobj-1

       dsn(k)=dmn*dyy

       dxn(k)=dmn*dfloat(k-1)

c       write (*,*) k,dxn(k)

c      Detect point
       do j=npini,npfin
       if (dxn(k).ge.dx(j).and.dxn(k).lt.dx(j+1)) then
       dyn(k)=dy(j)
       end if
       end do 

       dsn(k)=dmn*dyn(k)
       dan(k)=dan(k-1)+dsn(k-1)

       xln=xln+dsn(k-1)

       write (*,*) k,dsn(k)*100.,dan(k)*100.

       

       end do ! k

       write (*,*) "XLN= ",xln*100.

c      Scale contour

c      Old contour (xl)
c      New contor  (xln)
c      Scale factor xl/xln

c      Reformat contour

       do k=1,npobj-1

c      Detect point
       do j=npini,npfin-1
       if (dan(k).ge.da(j).and.dan(k).lt.da(j+1)) then
       dyn(k)=dy(j)
       end if

      
       end do


       end do ! k


       return
       end



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE xyzt
c      Rotates points in airfoils in tree axes and moves
c      to absolute ccordinates
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       SUBROUTINE xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)

       real*8 u(0:100,500,99),v(0:100,500,99),w(0:100,500,99) ! airfoil 3D
       real*8 rib(0:100,500)
       real*8 u_aux(0:100,500,10),v_aux(0:100,500,10),
     + w_aux(0:100,500,10)
       real*8 pi,tetha,rot_z,pos
       integer np(0:100,9)

       pi=4.0d0*datan(1.0d0)
       tetha=rib(i,8)*pi/180.0d0
       rot_z=rib(i,250)*pi/180.0d0
       pos=rib(i,5)*rib(i,251)/100.0d0

c      Starts with scaled and Z DISPLACED coordinates (i,j,3)

c      Washin rotation around X-axis
       u_aux(i,j,2)=(u_aux(i,j,1)-(rib(i,10)/100.)*rib(i,5))*
     + dcos(tetha)+(v_aux(i,j,1))*dsin(tetha)+(rib(i,10)/100.)*rib(i,5)
       v_aux(i,j,2)=(-u_aux(i,j,1)+(rib(i,10)/100.)*rib(i,5))*
     + dsin(tetha)+(v_aux(i,j,1))*dcos(tetha)
       w_aux(i,j,2)=0.0d0

c      Rotation around Z-axis
       w_aux(i,j,3)=-u_aux(i,j,2)*dsin(rot_z)+pos*dsin(rot_z)
       u_aux(i,j,3)=u_aux(i,j,2)*dcos(rot_z)+pos*(1-dcos(rot_z))
       v_aux(i,j,3)=v_aux(i,j,2)

c      Rotation around Y-axis
       w_aux(i,j,4)=-w_aux(i,j,3)*dcos(rib(i,9)*pi/180.)-
     + v_aux(i,j,3)*dsin(rib(i,9)*pi/180.)
       u_aux(i,j,4)=u_aux(i,j,3)
       v_aux(i,j,4)=-w_aux(i,j,3)*dsin(rib(i,9)*pi/180.)+
     + v_aux(i,j,3)*dcos(rib(i,9)*pi/180.)

c      Move to absolute coordinates
       w_aux(i,j,5)=rib(i,6)-w_aux(i,j,4)
       u_aux(i,j,5)=rib(i,3)+u_aux(i,j,4)
       v_aux(i,j,5)=rib(i,7)-v_aux(i,j,4)

       return
       end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE elliquad
c      Draw ellipses inside quadrilaters
c      Use in vents type 6 and -6
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

