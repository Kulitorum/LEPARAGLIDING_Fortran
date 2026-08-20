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

c      Distances between points rib 0 and 1
       pa=dsqrt((x(i,j)-xx(i,j))**2.+(y(i,j)-yy(i,j))**2.+
     + (z(i,j)-zz(i,j))**2.)
       pb=dsqrt((x(i,j+1)-xx(i,j))**2.+(y(i,j+1)-yy(i,j))**2.+
     + (z(i,j+1)-zz(i,j))**2.)
       pc=dsqrt((x(i,j)-x(i,j+1))**2.+(y(i,j)-y(i,j+1))**2.+
     + (z(i,j)-z(i,j+1))**2.)
       pd=dsqrt((x(i,j)-xx(i,j+1))**2.+(y(i,j)-yy(i,j+1))**2.+
     + (z(i,j)-zz(i,j+1))**2.)
       pe=dsqrt((xx(i,j+1)-xx(i,j))**2.+(yy(i,j+1)-yy(i,j))**2.+
     + (zz(i,j+1)-zz(i,j))**2.)
       pf=dsqrt((x(i,j+1)-xx(i,j+1))**2.+(y(i,j+1)-yy(i,j+1))**2.+
     + (z(i,j+1)-zz(i,j+1))**2.)
       
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

c      Distances between points
       pa=dsqrt((x(i,j)-xx(i,j))**2.+(y(i,j)-yy(i,j))**2.+
     + (z(i,j)-zz(i,j))**2.)
       pb=dsqrt((x(i,j+1)-xx(i,j))**2.+(y(i,j+1)-yy(i,j))**2.+
     + (z(i,j+1)-zz(i,j))**2.)
       pc=dsqrt((x(i,j)-x(i,j+1))**2.+(y(i,j)-y(i,j+1))**2.+
     + (z(i,j)-z(i,j+1))**2.)
       pd=dsqrt((x(i,j)-xx(i,j+1))**2.+(y(i,j)-yy(i,j+1))**2.+
     + (z(i,j)-zz(i,j+1))**2.)
       pe=dsqrt((xx(i,j+1)-xx(i,j))**2.+(yy(i,j+1)-yy(i,j))**2.+
     + (zz(i,j+1)-zz(i,j))**2.)
       pf=dsqrt((x(i,j+1)-xx(i,j+1))**2.+(y(i,j+1)-yy(i,j+1))**2.+
     + (z(i,j+1)-zz(i,j+1))**2.)
       
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

c      Distances between points
       
       pa=dsqrt((x(i,j)-xx(i,j))**2.+(y(i,j)-yy(i,j))**2.+
     + (z(i,j)-zz(i,j))**2.)
       pb=dsqrt((x(i,j+1)-xx(i,j))**2.+(y(i,j+1)-yy(i,j))**2.+
     + (z(i,j+1)-zz(i,j))**2.)
       pc=dsqrt((x(i,j)-x(i,j+1))**2.+(y(i,j)-y(i,j+1))**2.+
     + (z(i,j)-z(i,j+1))**2.)
       pd=dsqrt((x(i,j)-xx(i,j+1))**2.+(y(i,j)-yy(i,j+1))**2.+
     + (z(i,j)-zz(i,j+1))**2.)
       pe=dsqrt((xx(i,j+1)-xx(i,j))**2.+(yy(i,j+1)-yy(i,j))**2.+
     + (zz(i,j+1)-zz(i,j))**2.)
       pf=dsqrt((x(i,j+1)-xx(i,j+1))**2.+(y(i,j+1)-yy(i,j+1))**2.+
     + (z(i,j+1)-zz(i,j+1))**2.)
       
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

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
c     7.4.2 Air intakes (vents) panels calculus and drawing
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
c      Salva un vector per evitar interferencia vents-intrados
       do i=0,nribss
       npx=np(i,2)+np(i,3)-1
       pl1x(i,499)=pl1x(i,npx)
       pl1y(i,499)=pl1y(i,npx)
       pl2x(i,499)=pl2x(i,npx)
       pl2y(i,499)=pl2y(i,npx)
       pr1x(i,499)=pr1x(i,npx)
       pr1y(i,499)=pr1y(i,npx)
       pr2x(i,499)=pr2x(i,npx)
       pr2y(i,499)=pr2y(i,npx)
       end do


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
