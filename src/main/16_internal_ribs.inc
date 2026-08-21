!> Generate H, V, VH, minirib, junction, rod, and reinforcement parts.
!!
!! Consumes the section-12/21/23/38/39 definitions and prepared profile slots
!! to build internal contours, holes, sewing offsets, nylon-rod channels, and
!! production marks.  Most geometry is delegated to documented procedures in
!! `junctions.inc`, `leparagliding_geometry_2d`, and
!! `offsets_reinforcements.inc`.
c      16. H V and HV ribs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       xvrib=xvrib/10.

c       write (*,*) "xvrib=",xvrib
       
c       do i=0,nribss

c      Impressions de control
c       ii=1

c       write (*,*) "u(i,ii,6) ",i,ii,u(i,ii,6)
c       write (*,*) "np(i,2) ",i, np(i,2),np(i,1)
c       write (*,*) "jcon(i,ii,2) ",jcon(i,ii,2)

c       end do

       do k=1,nhvr

c      Defaults prevent optional or degenerate segments reusing stale values
       xm=0.0d0
       xb=0.0d0
       xsegment=0.0d0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.1 H straps Type 1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


       if (hvr(k,2).eq.1) then

c      Control central cell width
       if (hvr(k,3).gt.0.or.cencell.gt.0.01) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.1.1 Line i (2)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)
       ii=hvr(k,4)

       ucnt(i,ii,3)=u(i,ii,6)
       ucnt(i,ii,2)=ucnt(i,ii,3)-hvr(k,7)
       ucnt(i,ii,4)=ucnt(i,ii,3)+hvr(k,7)

c      Points 2,3,4 interpolation in rib i
       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,2).and.u(i,j+1,3).ge.ucnt(i,ii,2)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,2)=xm*ucnt(i,ii,2)+xb
       jcon(i,ii,2)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       jcon(i,ii,3)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,4).and.u(i,j+1,3).ge.ucnt(i,ii,4)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,4)=xm*ucnt(i,ii,4)+xb
       jcon(i,ii,4)=j
       end if

       end do
         

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat line 2-3-4 in n regular spaces   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       uinc=0.
       vinc=0.

       do j=1,21
       ucnt2(i,ii,j)=ucnt(i,ii,2)+uinc
       uinc=uinc+(ucnt(i,ii,4)-ucnt(i,ii,2))/20.

c      warning u=v
c       write (*,*) "u-v ", i,j,u(i,j,3),v(i,j,3)

c      Between 2 and jcon(i,ii,2)+1
       if (ucnt2(i,ii,j).le.u(i,jcon(i,ii,2)+1,3)) then
       xm=(v(i,jcon(i,ii,2)+1,3)-vcnt(i,ii,2))/(u(i,jcon(i,ii,2)+1,3)-
     + ucnt(i,ii,2))
       xb=vcnt(i,ii,2)-xm*ucnt(i,ii,2)
       vcnt2(i,ii,j)=xm*ucnt2(i,ii,j)+xb
       end if

c      Between jcon(i,ii,2)+1 and jcon(i,ii,4)

       if (ucnt2(i,ii,j).ge.u(i,jcon(i,ii,2)+1,3).and.ucnt2(i,ii,j)
     + .le.u(i,jcon(i,ii,4),3)) then
c      
       do l=jcon(i,ii,2)+1,jcon(i,ii,4)-1

c      Seleccionar tram d'interpolació

       if (ucnt2(i,ii,j).ge.u(i,l,3).and.ucnt2(i,ii,j).le.u(i,l+1,3)) 
     + then
       xm=(v(i,l+1,3)-v(i,l,3))/(u(i,l+1,3)-u(i,l,3))
       xb=v(i,l,3)-xm*u(i,l,3)
       end if
       end do
       vcnt2(i,ii,j)=xm*ucnt2(i,ii,j)+xb
c       write (*,*) "xm, xb ", xm,xb
       end if

c      Between jcon(i,ii,4) and 4       
       if (ucnt2(i,ii,j).gt.u(i,jcon(i,ii,4),3)) then
       xm=(vcnt(i,ii,4)-v(i,jcon(i,ii,4),3))/(ucnt(i,ii,4)-
     + u(i,jcon(i,ii,4),3))
       xb=vcnt(i,ii,4)-xm*ucnt(i,ii,4)
       vcnt2(i,ii,j)=xm*ucnt2(i,ii,j)+xb
       end if

c       write (*,*) "Line 2 ",i,j,ucnt2(i,ii,j),vcnt2(i,ii,j)

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.1.2 Line i+1 (3)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,5)
       ii=hvr(k,6)

       ucnt(i,ii,3)=u(i,ii,6)
       ucnt(i,ii,2)=ucnt(i,ii,3)-(hvr(k,7)+hvr(k,20))
       ucnt(i,ii,4)=ucnt(i,ii,3)+(hvr(k,7)+hvr(k,20))

c       write (*,*) "> ",i,ii,hvr(k,20)

c      Points 2,3,4 interpolation in rib i
       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,2).and.u(i,j+1,3).ge.ucnt(i,ii,2)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,2)=xm*ucnt(i,ii,2)+xb
       jcon(i,ii,2)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       jcon(i,ii,3)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,4).and.u(i,j+1,3).ge.ucnt(i,ii,4)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,4)=xm*ucnt(i,ii,4)+xb
       jcon(i,ii,4)=j
       end if

       end do
         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat line 2-3-4 in n regular spaces   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       uinc=0.
       vinc=0.

       do j=1,21
       ucnt3(i,ii,j)=ucnt(i,ii,2)+uinc
       uinc=uinc+(ucnt(i,ii,4)-ucnt(i,ii,2))/20.

c      Between 2 and jcon(i,ii,2)+1
       if (ucnt3(i,ii,j).le.u(i,jcon(i,ii,2)+1,3)) then
       xm=(v(i,jcon(i,ii,2)+1,3)-vcnt(i,ii,2))/(u(i,jcon(i,ii,2)+1,3)-
     + ucnt(i,ii,2))
       xb=vcnt(i,ii,2)-xm*ucnt(i,ii,2)
       vcnt3(i,ii,j)=xm*ucnt3(i,ii,j)+xb
       end if

c      Between jcon(i,ii,2)+1 and jcon(i,ii,4)

       if (ucnt3(i,ii,j).ge.u(i,jcon(i,ii,2)+1,3).and.ucnt3(i,ii,j)
     + .le.u(i,jcon(i,ii,4),3)) then
c      
       do l=jcon(i,ii,2)+1,jcon(i,ii,4)-1

c      Seleccionar tram d'interpolació

       if (ucnt3(i,ii,j).ge.u(i,l,3).and.ucnt3(i,ii,j).le.u(i,l+1,3)) 
     + then
       xm=(v(i,l+1,3)-v(i,l,3))/(u(i,l+1,3)-u(i,l,3))
       xb=v(i,l,3)-xm*u(i,l,3)
       end if
       end do
       vcnt3(i,ii,j)=xm*ucnt3(i,ii,j)+xb
       end if

c      Between jcon(i,ii,4) and 4       
       if (ucnt3(i,ii,j).gt.u(i,jcon(i,ii,4),3)) then
       xm=(vcnt(i,ii,4)-v(i,jcon(i,ii,4),3))/(ucnt(i,ii,4)-
     + u(i,jcon(i,ii,4),3))
       xb=vcnt(i,ii,4)-xm*ucnt(i,ii,4)
       vcnt3(i,ii,j)=xm*ucnt3(i,ii,j)+xb
       end if

       end do ! j

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.1.3 Lines 2 and 3 transportation on the space
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Rib i (Line 2)

       i=hvr(k,3)
       ii=hvr(k,4)

       tetha=rib(i,8)*pi/180.
       rot_z=rib(i,250)*pi/180.0
       pos=rib(i,5)*rib(i,251)/100.0

  
       do j=1,21
       ru(i,j,3)=ucnt2(i,ii,j)
       rv(i,j,3)=vcnt2(i,ii,j)-rib(i,50)
       end do

       do j=1,21

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       hx2(i,j,ii)=rx(i,j)
       hy2(i,j,ii)=ry(i,j)
       hz2(i,j,ii)=rz(i,j)

       end do


c       write(*,*) "Ep: ",hx2(0,1,1)

c      Rib i+1 (Line 3)

       i=hvr(k,5)
       ii=hvr(k,6)

       tetha=rib(i,8)*pi/180.

       do j=1,21
       ru(i,j,3)=ucnt3(i,ii,j)
       rv(i,j,3)=vcnt3(i,ii,j)-rib(i,50)
c      COMPTE AMB el rib(i,50) A ESTUDIAR       
       end do

       do j=1,21

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       hx3(i-1,j,ii)=rx(i,j)
       hy3(i-1,j,ii)=ry(i,j)
       hz3(i-1,j,ii)=rz(i,j)

       end do


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.1.4 H-rib 2-3 in 2D model
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)

       px0=0.
       py0=0.
       ptheta=0.

       do j=1,21

c      Distances between points
       pa=dsqrt((rx(i+1,j)-rx(i,j))**2.+(ry(i+1,j)-ry(i,j))**2.+
     + (rz(i+1,j)-rz(i,j))**2.)
       pb=dsqrt((rx(i+1,j+1)-rx(i,j))**2.+(ry(i+1,j+1)-ry(i,j))**2.+
     + (rz(i+1,j+1)-rz(i,j))**2.)
       pc=dsqrt((rx(i+1,j+1)-rx(i+1,j))**2.+(ry(i+1,j+1)-ry(i+1,j))**2.+
     + (rz(i+1,j+1)-rz(i+1,j))**2.)
       pd=dsqrt((rx(i+1,j)-rx(i,j+1))**2.+(ry(i+1,j)-ry(i,j+1))**2.+
     + (rz(i+1,j)-rz(i,j+1))**2.)
       pe=dsqrt((rx(i,j+1)-rx(i,j))**2.+(ry(i,j+1)-ry(i,j))**2.+
     + (rz(i,j+1)-rz(i,j))**2.)
       pf=dsqrt((rx(i+1,j+1)-rx(i,j+1))**2.+(ry(i+1,j+1)-ry(i,j+1))**2.+
     + (rz(i+1,j+1)-rz(i,j+1))**2.)
       
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
       phu=pb1t*tan(pw1)

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

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Tensa cintes. Calcula i imprimeix distancies...
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       j=10


       hdist(i)=dsqrt((pl1y(i,j)-pr1y(i,j))**2.+
     + (pl1x(i,j)-pr1x(i,J))**2.)
       hangle(i)=datan((pr1y(i,j)-pl1y(i,j))/(pr1x(i,j)-pl1x(i,j)))

c      Ajusta tensió de la cinta

c      Note: if use variable htens not work, I renamed htensi

       htens=htensi

       do j=1,21

       pr1x(i,j)=pr1x(i,j)-htensi*hdist(i)*dcos(hangle(i))
       pr1y(i,j)=pr1y(i,j)-htensi*hdist(i)*dsin(hangle(i))
       pr2x(i,j)=pr2x(i,j)-htensi*hdist(i)*dcos(hangle(i))
       pr2y(i,j)=pr2y(i,j)-htensi*hdist(i)*dsin(hangle(i))

       end do

c      Cintes tensades
       j=10

       hdist(i)=dsqrt((pl1y(i,j)-pr1y(i,j))**2.+
     + (pl1x(i,j)-pr1x(i,J))**2.)
       hangle(i)=datan((pr1y(i,j)-pl1y(i,j))/(pr1x(i,j)-pl1x(i,j)))

c       write (*,*) "dist tensades ", i, "-", i+1, hdist(i), hangle(i)

c      Drawing in 2D model
       
       psep=3300.*xkf+xrsep*hvr_positions%horizontal(1)*float(i)
       psey=800.*xkf+hvr_positions%vertical(1)*yrsep*float(ii)+sepri1_y

c       write (*,*) i,ii,psep,psey

       j=1

       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),30)

       j=21

       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),30)

       do j=1,21-1

c      Dibuixa limit esquerre
c       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pl2x(i,j),
c     + psey+pl2y(i,j),1)

c      Dibuixa limit dret
c       call line(psep+pr1x(i,j),psey+pr1y(i,j),psep+pr2x(i,j),
c     + psey+pr2y(i,j),1)


c       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
c     + psey+pr1y(i,j),4)


c      Vores de costura esquerra
       alpl=-(datan((pl1y(i,j)-pl2y(i,j))/(pl1x(i,j)-pl2x(i,j))))
       if (alpl.lt.0.) then
       alpl=alpl+pi
       end if

       lvcx(i,j)=psep+pl1x(i,j)-xvrib*dsin(alpl)
       lvcy(i,j)=psey+pl1y(i,j)-xvrib*dcos(alpl)

c      Vores de costura dreta
       alpr=-(datan((pr1y(i,j)-pr2y(i,j))/(pr1x(i,j)-pr2x(i,j))))
       if (alpr.lt.0.) then
       alpr=alpr+pi
       end if

       rvcx(i,j)=psep+pr1x(i,j)+xvrib*dsin(alpr)
       rvcy(i,j)=psey+pr1y(i,j)+xvrib*dcos(alpr)

c      Tancament lateral inici
       if (j.eq.1) then
       call line(psep+pl1x(i,j)-xvrib*dsin(alpl),psey+pl1y(i,j)
     + -xvrib*dcos(alpl),psep+pl1x(i,j),psey+pl1y(i,j),30)
       call line(psep+pr1x(i,j)+xvrib*dsin(alpr),psey+pr1y(i,j)
     + +xvrib*dcos(alpr),psep+pr1x(i,j),psey+pr1y(i,j),30)
       end if

c      Tancament lateral fi
       if (j.eq.20) then
       call line(psep+pl2x(i,j)-xvrib*dsin(alpl),psey+pl2y(i,j)
     + -xvrib*dcos(alpl),psep+pl2x(i,j),psey+pl2y(i,j),30)
       call line(psep+pr2x(i,j)+xvrib*dsin(alpr),psey+pr2y(i,j)
     + +xvrib*dcos(alpr),psep+pr2x(i,j),psey+pr2y(i,j),30)

       lvcx(i,j+1)=psep+pl2x(i,j)-xvrib*dsin(alpl)
       lvcy(i,j+1)=psey+pl2y(i,j)-xvrib*dcos(alpl)
       
       rvcx(i,j+1)=psep+pr2x(i,j)+xvrib*dsin(alpr)
       rvcy(i,j+1)=psey+pr2y(i,j)+xvrib*dcos(alpr)

       end if

       end do

c      Dibuixa punt central de control de costura AD

       alpl=-(datan((pl1y(i,1)-pl2y(i,20))/(pl1x(i,1)-pl2x(i,20))))
       if (alpl.lt.0.) then
       alpl=alpl+pi
       end if

       xpx=(pl1x(i,1)+pl2x(i,20))/2.-xdes*dsin(alpl)
       xpy=(pl1y(i,1)+pl2y(i,20))/2.-xdes*dcos(alpl)

       call point(psep+xpx,psey+xpy,3)

       alpr=-(datan((pr1y(i,1)-pr2y(i,20))/(pr1x(i,1)-pr2x(i,20))))
       if (alpr.lt.0.) then
       alpr=alpr+pi
       end if

       xpx=(pr1x(i,1)+pr2x(i,20))/2.+xdes*dsin(alpr)
       xpy=(pr1y(i,1)+pr2y(i,20))/2.+xdes*dcos(alpr)

       call point(psep+xpx,psey+xpy,1)


c      Etiqueta cintes en romans AD

       pi=4.0d0*datan(1.0d0)

       xpx=(pl1x(i,1)+pl2x(i,20))/2.-xdes*dsin(alpl)
       xpy=(pl1y(i,1)+pl2y(i,20))/2.-xdes*dcos(alpl)

c       alpl=(datan((pl2y(i,20)-pl1y(i,1))/(pl2x(i,20)-pl1x(i,1))))

c      write (*,*) "romano", hvr(k,3), hvr(k,4), xvrib, alpl

       xpx2=psep+xpx+0.4*hvr(k,7)*dcos(alpl)-0.6*xvrib*dsin(alpl)
       xpy2=psey+xpy-0.4*hvr(k,7)*dsin(alpl)-0.6*xvrib*dcos(alpl)
       
       call romano(int(hvr(k,3)),xpx2,xpy2,alpl,typm6(10)*0.1,7)

       xpx2=psep+xpx-0.6*hvr(k,7)*dcos(alpl)-0.6*xvrib*dsin(alpl)
       xpy2=psey+xpy+0.6*hvr(k,7)*dsin(alpl)-0.6*xvrib*dcos(alpl)
       
       call romano(int(hvr(k,4)),xpx2,xpy2,alpl,typm6(10)*0.1,7)

c       call line(psep+xpx,psey+xpy,psep+xpx+4.*dcos(alpl),
c     + psey+xpy+4.*dsin(alpl),1)

c      H-rib length
       hvr(k,15)=dsqrt((lvcx(i,1)-rvcx(i,1))**2.+
     + (lvcy(i,1)-rvcy(i,1))**2.)
     
c      Numera cintes H en decimals (VH type 1)
       call itxt(psep-xrsep+83.*xkf-120.*(typm3(10)/10.),psey-10,
     + typm3(10),0.0d0,i,7)
       call itxt(psep+hvr(k,15)-xrsep+83.*xkf-120.*(typm3(10)/10.),
     + psey-10,typm3(10),0.0d0,i+1,7)

       pi=4.0d0*datan(1.0d0)

c      Dibuixa vores amb segments completament enllaçats       

       do j=1,20

       call line(lvcx(i,j),lvcy(i,j),lvcx(i,j+1),lvcy(i,j+1),30)
       call line(rvcx(i,j),rvcy(i,j),rvcx(i,j+1),rvcy(i,j+1),30)

       end do

       end if

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.2 V ribs partial (Type 2)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (hvr(k,2).eq.2) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.2.1 Rib i
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Define main points

       i=hvr(k,3)    ! rib i
       ii=hvr(k,4)   ! row ii

       ucnt(i,ii,3)=u(i,ii,6)
       ucnt(i,ii,1)=ucnt(i,ii,3)-hvr(k,8)
       ucnt(i,ii,2)=ucnt(i,ii,3)-hvr(k,7)
       ucnt(i,ii,4)=ucnt(i,ii,3)+hvr(k,7)
       ucnt(i,ii,5)=ucnt(i,ii,3)+hvr(k,8)
       ucnt(i,ii,6)=ucnt(i,ii,1)
       ucnt(i,ii,7)=ucnt(i,ii,3)
       ucnt(i,ii,8)=ucnt(i,ii,5)
       ucnt(i,ii,9)=ucnt(i,ii,1)
       ucnt(i,ii,10)=ucnt(i,ii,3)
       ucnt(i,ii,11)=ucnt(i,ii,5)


c      Points 2,3,4 interpolation in rib i
       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,2).and.u(i,j+1,3).ge.ucnt(i,ii,2)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,2)=xm*ucnt(i,ii,2)+xb
       jcon(i,ii,2)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       jcon(i,ii,3)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,4).and.u(i,j+1,3).ge.ucnt(i,ii,4)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,4)=xm*ucnt(i,ii,4)+xb
       jcon(i,ii,4)=j
       end if

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat line 2-3-4 in n regular spaces   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       uinc=0.
       vinc=0.

       do j=1,21
       ucnt2(i,ii,j)=ucnt(i,ii,2)+uinc
       uinc=uinc+(ucnt(i,ii,4)-ucnt(i,ii,2))/20.

c      Between 2 and jcon(i,ii,2)+1
       if (ucnt2(i,ii,j).le.u(i,jcon(i,ii,2)+1,3)) then
       xm=(v(i,jcon(i,ii,2)+1,3)-vcnt(i,ii,2))/(u(i,jcon(i,ii,2)+1,3)-
     + ucnt(i,ii,2))
       xb=vcnt(i,ii,2)-xm*ucnt(i,ii,2)
       vcnt2(i,ii,j)=xm*ucnt2(i,ii,j)+xb
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Between jcon(i,ii,2)+1 and jcon(i,ii,4)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (ucnt2(i,ii,j).ge.u(i,jcon(i,ii,2)+1,3).and.ucnt2(i,ii,j)
     + .le.u(i,jcon(i,ii,4),3)) then
c      
       do l=jcon(i,ii,2)+1,jcon(i,ii,4)-1

c      Seleccionar tram d'interpolació

       if (ucnt2(i,ii,j).ge.u(i,l,3).and.ucnt2(i,ii,j).le.u(i,l+1,3)) 
     + then
       xm=(v(i,l+1,3)-v(i,l,3))/(u(i,l+1,3)-u(i,l,3))
       xb=v(i,l,3)-xm*u(i,l,3)
       end if
       end do
c       xm=(v(i,jcon(i,ii,2)+j,3)-v(i,jcon(i,ii,2)+j-1,3))/
c     + (u(i,jcon(i,ii,2)+j,3)-u(i,jcon(i,ii,2)+j-1,3))
c       xb=v(i,jcon(i,ii,2)+j-1,3)-xm*u(i,jcon(i,ii,2)+j-1,3)
       vcnt2(i,ii,j)=xm*ucnt2(i,ii,j)+xb
       end if

c      Between jcon(i,ii,4) and 4       
       if (ucnt2(i,ii,j).gt.u(i,jcon(i,ii,4),3)) then
       xm=(vcnt(i,ii,4)-v(i,jcon(i,ii,4),3))/(ucnt(i,ii,4)-
     + u(i,jcon(i,ii,4),3))
       xb=vcnt(i,ii,4)-xm*ucnt(i,ii,4)
       vcnt2(i,ii,j)=xm*ucnt2(i,ii,j)+xb
       end if

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.2.2 Rib i-1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)-1
       ii=hvr(k,4)

       hvr(k,19)=hvr(k,8)+hvr(k,15)

       ucnt(i,ii,3)=u(i,ii,6)
       ucnt(i,ii,1)=ucnt(i,ii,3)-hvr(k,19)
       ucnt(i,ii,2)=ucnt(i,ii,3)-hvr(k,7)
       ucnt(i,ii,4)=ucnt(i,ii,3)+hvr(k,7)
       ucnt(i,ii,5)=ucnt(i,ii,3)+hvr(k,19)
       ucnt(i,ii,6)=ucnt(i,ii,1)
       ucnt(i,ii,7)=ucnt(i,ii,3)
       ucnt(i,ii,8)=ucnt(i,ii,5)
       ucnt(i,ii,9)=ucnt(i,ii,1)
       ucnt(i,ii,10)=ucnt(i,ii,3)
       ucnt(i,ii,11)=ucnt(i,ii,5)

c      Points 1,3,5 interpolation in rib i-1
       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,1).and.u(i,j+1,3).ge.ucnt(i,ii,1)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,1)=xm*ucnt(i,ii,1)+xb
       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       end if

       if (u(i,j,3).le.ucnt(i,ii,5).and.u(i,j+1,3).ge.ucnt(i,ii,5)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,5)=xm*ucnt(i,ii,5)+xb
       end if

       end do

c      Points 9,10,11 interpolation in rib i-1
       do j=1,np(i,2)

       if (u(i,j,3).ge.ucnt(i,ii,9).and.u(i,j+1,3).le.ucnt(i,ii,9)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,9)=xm*ucnt(i,ii,9)+xb
       end if

       if (u(i,j,3).ge.ucnt(i,ii,10).and.u(i,j+1,3).le.ucnt(i,ii,10)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,10)=xm*ucnt(i,ii,10)+xb
       end if

       if (u(i,j,3).ge.ucnt(i,ii,11).and.u(i,j+1,3).le.ucnt(i,ii,11)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,11)=xm*ucnt(i,ii,11)+xb
       end if

       end do

c      Calculus of 6,7,8 points in rib i-1
       vcnt(i,ii,6)=(vcnt(i,ii,9)-vcnt(i,ii,1))*(hvr(k,9)/100.)+
     + vcnt(i,ii,1)
       vcnt(i,ii,7)=(vcnt(i,ii,10)-vcnt(i,ii,3))*(hvr(k,9)/100.)+
     + vcnt(i,ii,3)
       vcnt(i,ii,8)=(vcnt(i,ii,11)-vcnt(i,ii,5))*(hvr(k,9)/100.)+
     + vcnt(i,ii,5)

c      Redefinition of points 6,8 if angle is not 90 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      AQUÍ ÉS EL PROBLEMA!!!!!!!!!!!!!!
c      no és hvr(k,8) ans una valor afectat per hvr(k,17).... o similar!!!!
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

    
       if (hvr(k,10).ne.90.) then
       ucnt(i,ii,6)=ucnt(i,ii,7)-hvr(k,19)*dcos((pi/180.)*hvr(k,10))
       ucnt(i,ii,8)=ucnt(i,ii,7)+hvr(k,19)*dcos((pi/180.)*hvr(k,10))
       vcnt(i,ii,6)=vcnt(i,ii,7)-hvr(k,19)*dsin((pi/180.)*hvr(k,10))
       vcnt(i,ii,8)=vcnt(i,ii,7)+hvr(k,19)*dsin((pi/180.)*hvr(k,10))
       end if

c      Divide line 6-8 in n segments

       uinc=0.
       vinc=0.
       do j=1,21
       ucnt1(i,ii,j)=ucnt(i,ii,6)+uinc
       uinc=uinc+(ucnt(i,ii,8)-ucnt(i,ii,6))/20.
       vcnt1(i,ii,j)=vcnt(i,ii,6)+vinc
       vinc=vinc+(vcnt(i,ii,8)-vcnt(i,ii,6))/20.
       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.2.3 Rib i+1
cccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)+1
       ii=hvr(k,4)

       hvr(k,19)=hvr(k,8)+hvr(k,17)

       ucnt(i,ii,3)=u(i,ii,6)
       ucnt(i,ii,1)=ucnt(i,ii,3)-hvr(k,19)
       ucnt(i,ii,2)=ucnt(i,ii,3)-hvr(k,7)
       ucnt(i,ii,4)=ucnt(i,ii,3)+hvr(k,7)
       ucnt(i,ii,5)=ucnt(i,ii,3)+hvr(k,19)
       ucnt(i,ii,6)=ucnt(i,ii,1)
       ucnt(i,ii,7)=ucnt(i,ii,3)
       ucnt(i,ii,8)=ucnt(i,ii,5)
       ucnt(i,ii,9)=ucnt(i,ii,1)
       ucnt(i,ii,10)=ucnt(i,ii,3)
       ucnt(i,ii,11)=ucnt(i,ii,5)

c      NOTE: uso hvr(k,17) car hvr(k,16) no donava be
c       write (*,*) "2>",hvr(k,1),hvr(k,15),hvr(k,8),hvr(k,17)


c      Points 1,3,5 interpolation in rib i+1
       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,1).and.u(i,j+1,3).ge.ucnt(i,ii,1)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,1)=xm*ucnt(i,ii,1)+xb
       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       end if

       if (u(i,j,3).le.ucnt(i,ii,5).and.u(i,j+1,3).ge.ucnt(i,ii,5)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,5)=xm*ucnt(i,ii,5)+xb
       end if

       end do

c      Points 9,10,11 interpolation in rib i+1
       do j=1,np(i,2)

       if (u(i,j,3).ge.ucnt(i,ii,9).and.u(i,j+1,3).le.ucnt(i,ii,9)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,9)=xm*ucnt(i,ii,9)+xb
       end if

       if (u(i,j,3).ge.ucnt(i,ii,10).and.u(i,j+1,3).le.ucnt(i,ii,10)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,10)=xm*ucnt(i,ii,10)+xb
       end if

       if (u(i,j,3).ge.ucnt(i,ii,11).and.u(i,j+1,3).le.ucnt(i,ii,11)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,11)=xm*ucnt(i,ii,11)+xb
       end if

       end do

c      Calculus of 6,7,8 points in rib i+1
       vcnt(i,ii,6)=(vcnt(i,ii,9)-vcnt(i,ii,1))*(hvr(k,9)/100.)+
     + vcnt(i,ii,1)
       vcnt(i,ii,7)=(vcnt(i,ii,10)-vcnt(i,ii,3))*(hvr(k,9)/100.)+
     + vcnt(i,ii,3)
       vcnt(i,ii,8)=(vcnt(i,ii,11)-vcnt(i,ii,5))*(hvr(k,9)/100.)+
     + vcnt(i,ii,5)

c      Redefinition of points 6,8 if angle is not 90     
       if (hvr(k,10).ne.90.) then
       ucnt(i,ii,6)=ucnt(i,ii,7)-hvr(k,19)*dcos((pi/180.)*hvr(k,10))
       ucnt(i,ii,8)=ucnt(i,ii,7)+hvr(k,19)*dcos((pi/180.)*hvr(k,10))
       vcnt(i,ii,6)=vcnt(i,ii,7)-hvr(k,19)*dsin((pi/180.)*hvr(k,10))
       vcnt(i,ii,8)=vcnt(i,ii,7)+hvr(k,19)*dsin((pi/180.)*hvr(k,10))
       end if

c      Divide line 6-8 in n segments

       uinc=0.
       vinc=0.
       do j=1,21
       ucnt3(i,ii,j)=ucnt(i,ii,6)+uinc
       uinc=uinc+(ucnt(i,ii,8)-ucnt(i,ii,6))/20.
       vcnt3(i,ii,j)=vcnt(i,ii,6)+vinc
       vinc=vinc+(vcnt(i,ii,8)-vcnt(i,ii,6))/20.
       end do

c      Rib localisation
       i=hvr(k,3)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.2.4 V-ribs lines 1 2 3 transportation to 3D espace
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Rib i-1 (Line 1)

       i=hvr(k,3)-1

       tetha=rib(i,8)*pi/180.

       do j=1,21
       ru(i,j,3)=ucnt1(i,ii,j)
       rv(i,j,3)=vcnt1(i,ii,j)-rib(i,50)
       end do

       do j=1,21

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       rx1(i+1,j,ii)=rx(i,j)
       ry1(i+1,j,ii)=ry(i,j)
       rz1(i+1,j,ii)=rz(i,j)

       end do

c      Rib i (Line 2)

       i=hvr(k,3)

       tetha=rib(i,8)*pi/180.
       
       do j=1,21
       ru(i,j,3)=ucnt2(i,ii,j)
       rv(i,j,3)=vcnt2(i,ii,j)-rib(i,50)
       end do

       do j=1,21

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       rx2(i,j,ii)=rx(i,j)
       ry2(i,j,ii)=ry(i,j)
       rz2(i,j,ii)=rz(i,j)

       end do

c      Rib i+1 (Line 3)

       i=hvr(k,3)+1

       tetha=rib(i,8)*pi/180.

       do j=1,21
       ru(i,j,3)=ucnt3(i,ii,j)
       rv(i,j,3)=vcnt3(i,ii,j)-rib(i,50)
c      COMPTE AMB el rib(i,50) A ESTUDIAR       
       end do

       do j=1,21

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       rx3(i-1,j,ii)=rx(i,j)
       ry3(i-1,j,ii)=ry(i,j)
       rz3(i-1,j,ii)=rz(i,j)

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.2.5.X V-ribs 1-2 and 2-3 in 3D model
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Return to rib i
       i=hvr(k,3)

c      Rib 1-2 (blue)

       if (hvr(k,5).eq.1) then
       do j=1,21
       call line3d(rx1(i,j,ii),ry1(i,j,ii),rz1(i,j,ii),
     + rx2(i,j,ii),ry2(i,j,ii),rz2(i,j,ii),5)
       call line3d(-rx1(i,j,ii),ry1(i,j,ii),rz1(i,j,ii),
     + -rx2(i,j,ii),ry2(i,j,ii),rz2(i,j,ii),5)
       end do
       end if

c      Rib 2-3 (red)
       if (hvr(k,6).eq.1) then
       do j=1,21
       call line3d(rx3(i,j,ii),ry3(i,j,ii),rz3(i,j,ii),
     + rx2(i,j,ii),ry2(i,j,ii),rz2(i,j,ii),1)
       call line3d(-rx3(i,j,ii),ry3(i,j,ii),rz3(i,j,ii),
     + -rx2(i,j,ii),ry2(i,j,ii),rz2(i,j,ii),1)
       end do
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.2.5 V-ribs calculus and drawing in 3D and 2D
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.2.5.1 V-rib 1-2 in 2D model (blue)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)-1

       px0=0.
       py0=0.
       ptheta=0.

       do j=1,21

c      Distances between points
       pa=dsqrt((rx(i+1,j)-rx(i,j))**2.+(ry(i+1,j)-ry(i,j))**2.+
     + (rz(i+1,j)-rz(i,j))**2.)
       pb=dsqrt((rx(i+1,j+1)-rx(i,j))**2.+(ry(i+1,j+1)-ry(i,j))**2.+
     + (rz(i+1,j+1)-rz(i,j))**2.)
       pc=dsqrt((rx(i+1,j+1)-rx(i+1,j))**2.+(ry(i+1,j+1)-ry(i+1,j))**2.+
     + (rz(i+1,j+1)-rz(i+1,j))**2.)
       pd=dsqrt((rx(i+1,j)-rx(i,j+1))**2.+(ry(i+1,j)-ry(i,j+1))**2.+
     + (rz(i+1,j)-rz(i,j+1))**2.)
       pe=dsqrt((rx(i,j+1)-rx(i,j))**2.+(ry(i,j+1)-ry(i,j))**2.+
     + (rz(i,j+1)-rz(i,j))**2.)
       pf=dsqrt((rx(i+1,j+1)-rx(i,j+1))**2.+(ry(i+1,j+1)-ry(i,j+1))**2.+
     + (rz(i+1,j+1)-rz(i,j+1))**2.)
       
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

c      Drawing in 2D model
       
       psep=3300.*xkf+xrsep*float(i)
       psey=800.*xkf+yrsep*float(ii)

       if (hvr(k,5).eq.1) then

       j=1

c      Costat vora atac
       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),5)

       j=21
c      Costat fuga
       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),5)

c      Marca punts MC a l'esquerra

       alpha=-(datan((pl1y(i,1)-pl2y(i,20))/(pl1x(i,1)-pl2x(i,20))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xp6=pl1x(i,1)-xdes*dsin(alpha)-2.*xdes*dcos(alpha)
       yp6=pl1y(i,1)-xdes*dcos(alpha)+2.*xdes*dsin(alpha)
       xp8=pl1x(i,21)-xdes*dsin(alpha)+2.*xdes*dcos(alpha)
       yp8=pl1y(i,21)-xdes*dcos(alpha)-2.*xdes*dsin(alpha)
       xp7=0.5*(xp6+xp8)
       yp7=0.5*(yp6+yp8)

       call point(psep+xp6,psey+yp6,1)
       call point(psep+xp7,psey+yp7,1)
       call point(psep+xp8,psey+yp8,1)

c     Romano costat esquerra

      sl=1.
       
      xpx=(pl1x(i,1)+pl2x(i,20))/2.-xdes*dsin(alpha)
      xpy=(pl1y(i,1)+pl2y(i,20))/2.-xdes*dcos(alpha)

      xpx2=psep+xpx+0.5*hvr(k,8)*dcos(alpha)-0.3*xvrib*dsin(alpha)
      xpy2=psey+xpy-0.5*hvr(k,8)*dsin(alpha)-0.3*xvrib*dcos(alpha) 

      call romano(i,xpx2,xpy2,alpha,typm6(10)*0.1,7)

      xpx2=psep+xpx-0.5*hvr(k,8)*dcos(alpha)-0.3*xvrib*dsin(alpha)
      xpy2=psey+xpy+0.5*hvr(k,8)*dsin(alpha)-0.3*xvrib*dcos(alpha) 

      call romano(int(hvr(k,4)),xpx2,xpy2,alpha,typm6(10)*0.1,7)


c      Marca punts MC a la dreta

       alpha=-(datan((pr1y(i,1)-pr2y(i,20))/(pr1x(i,1)-pr2x(i,20))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xp7=0.5*(pr1x(i,1)+pr2x(i,20))+xdes*dsin(alpha)
       yp7=0.5*(pr1y(i,1)+pr2y(i,20))+xdes*dcos(alpha)

       call point(psep+xp7,psey+yp7,3)


c     Romano costat dret

      sr=1.
       
      xpx=(pr1x(i,1)+pr2x(i,20))/2.+xdes*dsin(alpha)
      xpy=(pr1y(i,1)+pr2y(i,20))/2.+xdes*dcos(alpha)

      xpx2=psep+xpx+0.3*hvr(k,7)*dcos(alpha)+0.3*xvrib*dsin(alpha)
      xpy2=psey+xpy-0.3*hvr(k,7)*dsin(alpha)+0.3*xvrib*dcos(alpha) 

      call romano(i+1,xpx2,xpy2,alpha,typm6(10)*0.1,7)
       
       do j=1,21-1

c      Vores de costura esquerra
       alpl=-(datan((pl1y(i,j)-pl2y(i,j))/(pl1x(i,j)-pl2x(i,j))))
       if (alpl.lt.0.) then
       alpl=alpl+pi
       end if

       lvcx(i,j)=psep+pl1x(i,j)-xvrib*dsin(alpl)
       lvcy(i,j)=psey+pl1y(i,j)-xvrib*dcos(alpl)

c      Vores de costura dreta
       alpr=-(datan((pr1y(i,j)-pr2y(i,j))/(pr1x(i,j)-pr2x(i,j))))
       if (alpr.lt.0.) then
       alpr=alpr+pi
       end if

       rvcx(i,j)=psep+pr1x(i,j)+xvrib*dsin(alpr)
       rvcy(i,j)=psey+pr1y(i,j)+xvrib*dcos(alpr)

c      Tancament lateral inici
       if (j.eq.1) then
       call line(psep+pl1x(i,j)-xvrib*dsin(alpl),psey+pl1y(i,j)
     + -xvrib*dcos(alpl),psep+pl1x(i,j),psey+pl1y(i,j),5)
       call line(psep+pr1x(i,j)+xvrib*dsin(alpr),psey+pr1y(i,j)
     + +xvrib*dcos(alpr),psep+pr1x(i,j),psey+pr1y(i,j),5)
       end if

c      Tancament lateral fi
       if (j.eq.20) then
       call line(psep+pl2x(i,j)-xvrib*dsin(alpl),psey+pl2y(i,j)
     + -xvrib*dcos(alpl),psep+pl2x(i,j),psey+pl2y(i,j),5)
       call line(psep+pr2x(i,j)+xvrib*dsin(alpr),psey+pr2y(i,j)
     + +xvrib*dcos(alpr),psep+pr2x(i,j),psey+pr2y(i,j),5)

       lvcx(i,j+1)=psep+pl2x(i,j)-xvrib*dsin(alpl)
       lvcy(i,j+1)=psey+pl2y(i,j)-xvrib*dcos(alpl)

       rvcx(i,j+1)=psep+pr2x(i,j)+xvrib*dsin(alpr)
       rvcy(i,j+1)=psey+pr2y(i,j)+xvrib*dcos(alpr)

       end if

c      V-rib length
       hvr(k,15)=dsqrt((lvcx(i,1)-rvcx(i,1))**2.+
     + (lvcy(i,1)-rvcy(i,1))**2.)

c      Numera cintes V (Type 2 left side, blue)
       call itxt(psep-xrsep+83.*xkf-120.*(typm3(10)/10.),psey-10,
     + typm3(10),0.0d0,i,7)
       call itxt(psep+hvr(k,15)-xrsep+83.*xkf-120.*(typm3(10)/10.),
     + psey-10,typm3(10),0.0d0,i+1,7)
   
       end do

c      Dibuixa vores amb segments completament enllaçats       
       do j=1,20

       call line(lvcx(i,j),lvcy(i,j),lvcx(i,j+1),lvcy(i,j+1),5)
       call line(rvcx(i,j),rvcy(i,j),rvcx(i,j+1),rvcy(i,j+1),5)

       end do

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.2.5.2 V-rib 2-3 in 2D model (red)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)

       px0=0.
       py0=0.
       ptheta=0.

       do j=1,21

c      Distances between points
       pa=dsqrt((rx(i+1,j)-rx(i,j))**2.+(ry(i+1,j)-ry(i,j))**2.+
     + (rz(i+1,j)-rz(i,j))**2.)
       pb=dsqrt((rx(i+1,j+1)-rx(i,j))**2.+(ry(i+1,j+1)-ry(i,j))**2.+
     + (rz(i+1,j+1)-rz(i,j))**2.)
       pc=dsqrt((rx(i+1,j+1)-rx(i+1,j))**2.+(ry(i+1,j+1)-ry(i+1,j))**2.+
     + (rz(i+1,j+1)-rz(i+1,j))**2.)
       pd=dsqrt((rx(i+1,j)-rx(i,j+1))**2.+(ry(i+1,j)-ry(i,j+1))**2.+
     + (rz(i+1,j)-rz(i,j+1))**2.)
       pe=dsqrt((rx(i,j+1)-rx(i,j))**2.+(ry(i,j+1)-ry(i,j))**2.+
     + (rz(i,j+1)-rz(i,j))**2.)
       pf=dsqrt((rx(i+1,j+1)-rx(i,j+1))**2.+(ry(i+1,j+1)-ry(i,j+1))**2.+
     + (rz(i+1,j+1)-rz(i,j+1))**2.)
       
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

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Drawing in 2D model
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
       psep=3300.*xkf+xrsep*float(i)
       psey=800.*xkf+yrsep*float(ii)

       if (hvr(k,6).eq.1) then

       j=1

c      Costat vora d'atac
       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),1)

       j=21
c      Costat fuga
       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),1)


c      Marca punts MC a l'esquerra

       alpha=-(datan((pl1y(i,1)-pl2y(i,20))/(pl1x(i,1)-pl2x(i,20))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xp7=0.5*(pl1x(i,1)+pl1x(i,21))-xdes*dsin(alpha)
       yp7=0.5*(pl1y(i,1)+pl1y(i,21))-xdes*dcos(alpha)

       call point(psep+xp7,psey+yp7,1)

c      Romano costat esquerra

       sl=1.
       
       xpx=(pl1x(i,1)+pl2x(i,20))/2.-sl*xdes*dsin(alpha)
       xpy=(pl1y(i,1)+pl2y(i,20))/2.-sl*xdes*dcos(alpha)

       xpx2=psep+xpx+0.3*hvr(k,7)*dcos(alpha)-0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy-0.3*hvr(k,7)*dsin(alpha)-0.3*xvrib*dcos(alpha) 

       call romano(i,xpx2,xpy2,alpha,typm6(10)*0.1,7)

c      Marca punts MC a la dreta

       alpha=-(datan((pr1y(i,1)-pr2y(i,20))/(pr1x(i,1)
     + -pr2x(i,20))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xp6=pr1x(i,1)+xdes*dsin(alpha)-2.*xdes*dcos(alpha)
       yp6=pr1y(i,1)+xdes*dcos(alpha)+2.*xdes*dsin(alpha)
       xp8=pr1x(i,21)+xdes*dsin(alpha)+2.*xdes*dcos(alpha)
       yp8=pr1y(i,21)+xdes*dcos(alpha)-2.*xdes*dsin(alpha)
       xp7=0.5*(xp6+xp8)
       yp7=0.5*(yp6+yp8)

       call point(psep+xp6,psey+yp6,1)
       call point(psep+xp7,psey+yp7,1)
       call point(psep+xp8,psey+yp8,1)

c      Romano costat dret

       sr=1.
       
       xpx=(pr1x(i,1)+pr2x(i,20))/2.+xdes*dsin(alpha)
       xpy=(pr1y(i,1)+pr2y(i,20))/2.+xdes*dcos(alpha)

       xpx2=psep+xpx+0.5*hvr(k,8)*dcos(alpha)+0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy-0.5*hvr(k,8)*dsin(alpha)+0.3*xvrib*dcos(alpha) 

       call romano(i+1,xpx2,xpy2,alpha,typm6(10)*0.1,7)

       xpx2=psep+xpx-0.5*hvr(k,8)*dcos(alpha)+0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy+0.5*hvr(k,8)*dsin(alpha)+0.3*xvrib*dcos(alpha) 

       call romano(int(hvr(k,4)),xpx2,xpy2,alpha,typm6(10)*0.1,7)

c      Vores de costura

       do j=1,21-1

c      Vores de costura esquerra
       alpl=-(datan((pl1y(i,j)-pl2y(i,j))/(pl1x(i,j)-pl2x(i,j))))
       if (alpl.lt.0.) then
       alpl=alpl+pi
       end if

       lvcx(i,j)=psep+pl1x(i,j)-xvrib*dsin(alpl)
       lvcy(i,j)=psey+pl1y(i,j)-xvrib*dcos(alpl)

c      Vores de costura dreta
       alpr=-(datan((pr1y(i,j)-pr2y(i,j))/(pr1x(i,j)-pr2x(i,j))))
       if (alpr.lt.0.) then
       alpr=alpr+pi
       end if

       rvcx(i,j)=psep+pr1x(i,j)+xvrib*dsin(alpr)
       rvcy(i,j)=psey+pr1y(i,j)+xvrib*dcos(alpr)

c      Tancament lateral inici
       if (j.eq.1) then
       call line(psep+pl1x(i,j)-xvrib*dsin(alpl),psey+pl1y(i,j)
     + -xvrib*dcos(alpl),psep+pl1x(i,j),psey+pl1y(i,j),1)
       call line(psep+pr1x(i,j)+xvrib*dsin(alpr),psey+pr1y(i,j)
     + +xvrib*dcos(alpr),psep+pr1x(i,j),psey+pr1y(i,j),1)
       end if

c      Tancament lateral fi
       if (j.eq.20) then
       call line(psep+pl2x(i,j)-xvrib*dsin(alpl),psey+pl2y(i,j)
     + -xvrib*dcos(alpl),psep+pl2x(i,j),psey+pl2y(i,j),1)
       call line(psep+pr2x(i,j)+xvrib*dsin(alpr),psey+pr2y(i,j)
     + +xvrib*dcos(alpr),psep+pr2x(i,j),psey+pr2y(i,j),1)

       lvcx(i,j+1)=psep+pl2x(i,j)-xvrib*dsin(alpl)
       lvcy(i,j+1)=psey+pl2y(i,j)-xvrib*dcos(alpl)

       rvcx(i,j+1)=psep+pr2x(i,j)+xvrib*dsin(alpr)
       rvcy(i,j+1)=psey+pr2y(i,j)+xvrib*dcos(alpr)

       end if

c      Numera cintes V
c       call itxt(psep-20-xrsep,psey-10,10.0d0,0.0d0,i,7)
c       call itxt(psep+20-xrsep,psey-10,10.0d0,0.0d0,i+1,7)

c      V-rib length
       hvr(k,15)=dsqrt((lvcx(i,1)-rvcx(i,1))**2.+
     + (lvcy(i,1)-rvcy(i,1))**2.)

c      Numera cintes V (Type 2 right side, red)
       call itxt(psep-xrsep+83.*xkf-120.*(typm3(10)/10.),
     + psey-10,typm3(10),0.0d0,i,7)
       call itxt(psep+hvr(k,15)-xrsep+83.*xkf-120.*(typm3(10)/10.),
     + psey-10,typm3(10),0.0d0,i+1,7)

       end do

c      Dibuixa vores amb segments completament enllaçats       
       do j=1,20

       call line(lvcx(i,j),lvcy(i,j),lvcx(i,j+1),lvcy(i,j+1),1)
       call line(rvcx(i,j),rvcy(i,j),rvcx(i,j+1),rvcy(i,j+1),1)

       end do

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.2.5.3 Drawing V-ribs in 2D ribs, Print and MC
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Box (1,2)

       sepxx=700.*xkf
       sepyy=100.*xkf

c      Rib i-1
       kx=int((float(i-2)/6.))
       ky=i-1-kx*6

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)

       if (hvr(k,5).eq.1) then

c      Segment
       call line(sepx+ucnt(i-1,ii,6),-vcnt(i-1,ii,6)+sepy,
     + sepx+ucnt(i-1,ii,8),-vcnt(i-1,ii,8)+sepy,1)
       call line(sepx+2530.*xkf+ucnt(i-1,ii,6),-vcnt(i-1,ii,6)+sepy,
     + sepx+2530.*xkf+ucnt(i-1,ii,8),-vcnt(i-1,ii,8)+sepy,1)

c      Punts marcatge V-rib
       alpha=datan((vcnt(i-1,ii,8)-vcnt(i-1,ii,6))/
     + (ucnt(i-1,ii,8)-ucnt(i-1,ii,6)))
       xp6=ucnt(i-1,ii,6)-xdes*dsin(alpha)+2.*xdes*dcos(alpha)
       yp6=vcnt(i-1,ii,6)+xdes*dcos(alpha)+2.*xdes*dsin(alpha)
       xp8=ucnt(i-1,ii,8)-xdes*dsin(alpha)-2.*xdes*dcos(alpha)
       yp8=vcnt(i-1,ii,8)+xdes*dcos(alpha)-2.*xdes*dsin(alpha)
       xp7=0.5*(xp6+xp8)
       yp7=0.5*(yp6+yp8)
       call point(sepx+xp6,sepy-yp6,1)
       call point(sepx+xp7,sepy-yp7,1)
       call point(sepx+xp8,sepy-yp8,1)
       call point(sepx+2530.*xkf+xp6,sepy-yp6,1)
       call point(sepx+2530.*xkf+xp7,sepy-yp7,1)
       call point(sepx+2530.*xkf+xp8,sepy-yp8,1)

       end if

c      Rib i (center)

       kx=int((float(i-1)/6.))
       ky=i-kx*6

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)

       call line(sepx+ucnt(i,ii,2),-vcnt(i,ii,2)+sepy,
     + sepx+ucnt(i,ii,4),-vcnt(i,ii,4)+sepy,4)

c      Rib i+1
       kx=int((float(i)/6.))
       ky=i+1-kx*6

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)

       if (hvr(k,6).eq.1) then

c      Segment
       call line(sepx+ucnt(i+1,ii,6),-vcnt(i+1,ii,6)+sepy,
     + sepx+ucnt(i+1,ii,8),-vcnt(i+1,ii,8)+sepy,5)
       call line(sepx+2530.*xkf+ucnt(i+1,ii,6),-vcnt(i+1,ii,6)+sepy,
     + sepx+2530.*xkf+ucnt(i+1,ii,8),-vcnt(i+1,ii,8)+sepy,5)

c      Punts marcatge V-rib
       alpha=datan((vcnt(i+1,ii,8)-vcnt(i+1,ii,6))/
     + (ucnt(i+1,ii,8)-ucnt(i+1,ii,6)))
       xp6=ucnt(i+1,ii,6)-xdes*dsin(alpha)
       yp6=vcnt(i+1,ii,6)+xdes*dcos(alpha)
       xp8=ucnt(i+1,ii,8)-xdes*dsin(alpha)
       yp8=vcnt(i+1,ii,8)+xdes*dcos(alpha)
       xp7=0.5*(xp6+xp8)
       yp7=0.5*(yp6+yp8)
       call point(sepx+xp6,sepy-yp6,1)
       call point(sepx+xp7,sepy-yp7,1)
       call point(sepx+xp8,sepy-yp8,1)
       call point(sepx+2530.*xkf+xp6,sepy-yp6,1)
       call point(sepx+2530.*xkf+xp7,sepy-yp7,1)
       call point(sepx+2530.*xkf+xp8,sepy-yp8,1)

       end if

       end if ! Type 2



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3 V ribs full Type 3
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3.1 V-ribs full but independent strips
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3.1.1 Rib i
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (hvr(k,2).eq.3) then

c      Define main points 2,3,4,9,10,11

       i=hvr(k,3)    ! rib i
       ii=hvr(k,4)   ! row ii

       ucnt(i,ii,3)=u(i,ii,6)
       ucnt(i,ii,1)=ucnt(i,ii,3)-hvr(k,8)
       ucnt(i,ii,2)=ucnt(i,ii,3)-hvr(k,7)
       ucnt(i,ii,4)=ucnt(i,ii,3)+hvr(k,7)
       ucnt(i,ii,5)=ucnt(i,ii,3)+hvr(k,8)
       ucnt(i,ii,6)=ucnt(i,ii,1)
       ucnt(i,ii,7)=ucnt(i,ii,3)
       ucnt(i,ii,8)=ucnt(i,ii,5)
       ucnt(i,ii,9)=ucnt(i,ii,1)
       ucnt(i,ii,10)=ucnt(i,ii,3)
       ucnt(i,ii,11)=ucnt(i,ii,5)

c      Points 2,3,4 interpolation in rib i
       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,2).and.u(i,j+1,3).ge.ucnt(i,ii,2)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,2)=xm*ucnt(i,ii,2)+xb
       jcon(i,ii,2)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       jcon(i,ii,3)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,4).and.u(i,j+1,3).ge.ucnt(i,ii,4)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,4)=xm*ucnt(i,ii,4)+xb
       jcon(i,ii,4)=j
       end if

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat line 2-3-4 in n regular spaces   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       uinc=0.
       vinc=0.

       do j=1,21
       ucnt2(i,ii,j)=ucnt(i,ii,2)+uinc
       uinc=uinc+(ucnt(i,ii,4)-ucnt(i,ii,2))/20.

c      Between 2 and jcon(i,ii,2)+1
       if (ucnt2(i,ii,j).le.u(i,jcon(i,ii,2)+1,3)) then
       xm=(v(i,jcon(i,ii,2)+1,3)-vcnt(i,ii,2))/(u(i,jcon(i,ii,2)+1,3)-
     + ucnt(i,ii,2))
       xb=vcnt(i,ii,2)-xm*ucnt(i,ii,2)
       vcnt2(i,ii,j)=xm*ucnt2(i,ii,j)+xb
       end if

c      Between jcon(i,ii,2)+1 and jcon(i,ii,4)

       if (ucnt2(i,ii,j).ge.u(i,jcon(i,ii,2)+1,3).and.ucnt2(i,ii,j)
     + .le.u(i,jcon(i,ii,4),3)) then
c      
       do l=jcon(i,ii,2)+1,jcon(i,ii,4)-1

c      Seleccionar tram d'interpolació

       if (ucnt2(i,ii,j).ge.u(i,l,3).and.ucnt2(i,ii,j).le.u(i,l+1,3)) 
     + then
       xm=(v(i,l+1,3)-v(i,l,3))/(u(i,l+1,3)-u(i,l,3))
       xb=v(i,l,3)-xm*u(i,l,3)
       end if
       end do
c       xm=(v(i,jcon(i,ii,2)+j,3)-v(i,jcon(i,ii,2)+j-1,3))/
c     + (u(i,jcon(i,ii,2)+j,3)-u(i,jcon(i,ii,2)+j-1,3))
c       xb=v(i,jcon(i,ii,2)+j-1,3)-xm*u(i,jcon(i,ii,2)+j-1,3)
c      !!!!!!!!!!!!!Revisar
       vcnt2(i,ii,j)=xm*ucnt2(i,ii,j)+xb
       end if

c      Between jcon(i,ii,4) and 4       
       if (ucnt2(i,ii,j).gt.u(i,jcon(i,ii,4),3)) then
       xm=(vcnt(i,ii,4)-v(i,jcon(i,ii,4),3))/(ucnt(i,ii,4)-
     + u(i,jcon(i,ii,4),3))
       xb=vcnt(i,ii,4)-xm*ucnt(i,ii,4)
       vcnt2(i,ii,j)=xm*ucnt2(i,ii,j)+xb
       end if

       end do



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3.1.2 Rib i-1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)-1
       ii=hvr(k,4)

       ucnt(i,ii,3)=u(i,ii,6)
       ucnt(i,ii,1)=ucnt(i,ii,3)-(hvr(k,8)+hvr(k,15))
       ucnt(i,ii,2)=ucnt(i,ii,3)-hvr(k,7)
       ucnt(i,ii,4)=ucnt(i,ii,3)+hvr(k,7)
       ucnt(i,ii,5)=ucnt(i,ii,3)+(hvr(k,8)+hvr(k,15))
       ucnt(i,ii,6)=ucnt(i,ii,1)
       ucnt(i,ii,7)=ucnt(i,ii,3)
       ucnt(i,ii,8)=ucnt(i,ii,5)
       ucnt(i,ii,9)=ucnt(i,ii,1)
       ucnt(i,ii,10)=ucnt(i,ii,3)
       ucnt(i,ii,11)=ucnt(i,ii,5)

c      Points 1,3,5 interpolation in rib i-1
       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,1).and.u(i,j+1,3).ge.ucnt(i,ii,1)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,1)=xm*ucnt(i,ii,1)+xb
       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       end if

       if (u(i,j,3).le.ucnt(i,ii,5).and.u(i,j+1,3).ge.ucnt(i,ii,5)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,5)=xm*ucnt(i,ii,5)+xb
       end if

       end do

c      Points 9,10,11 interpolation in rib i-1
       do j=1,np(i,2)

       if (u(i,j,3).gt.ucnt(i,ii,9).and.u(i,j+1,3).le.ucnt(i,ii,9)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,9)=xm*ucnt(i,ii,9)+xb
       jcon(i,ii,9)=j+1
       end if

       if (u(i,j,3).gt.ucnt(i,ii,10).and.u(i,j+1,3).le.ucnt(i,ii,10)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,10)=xm*ucnt(i,ii,10)+xb
       jcon(i,ii,10)=j+1
       end if

       if (u(i,j,3).gt.ucnt(i,ii,11).and.u(i,j+1,3).le.ucnt(i,ii,11)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,11)=xm*ucnt(i,ii,11)+xb
       jcon(i,ii,11)=j+1
       end if

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat 9-10-11 in n spaces (rib i-1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
c      Reformat in 20 spaces

       n1vr=jcon(i,ii,9)-jcon(i,ii,11)+1    
       n2vr=20+1

c      Load data polyline
       xlin1(1)=ucnt(i,ii,9)
       ylin1(1)=vcnt(i,ii,9)
       do j=2,n1vr-1
       xlin1(j)=u(i,jcon(i,ii,9)-j+1,3)
       ylin1(j)=v(i,jcon(i,ii,9)-j+1,3)
c      MIRAR SI CAL +-1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       end do
       xlin1(n1vr)=ucnt(i,ii,11)
       ylin1(n1vr)=vcnt(i,ii,11)

c      Call subroutine vector redistribution

       call vredis(xlin1,ylin1,xlin3,ylin3,n1vr,n2vr)

c      Load result polyline

       do j=1,n2vr
       ucnt1(i,ii,j)=xlin3(j)
       vcnt1(i,ii,j)=ylin3(j)
       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3.1.3 Rib i+1
cccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)+1
       ii=hvr(k,4)

       ucnt(i,ii,3)=u(i,ii,6)
       ucnt(i,ii,1)=ucnt(i,ii,3)-(hvr(k,8)+hvr(k,16))
       ucnt(i,ii,2)=ucnt(i,ii,3)-hvr(k,7)
       ucnt(i,ii,4)=ucnt(i,ii,3)+hvr(k,7)
       ucnt(i,ii,5)=ucnt(i,ii,3)+(hvr(k,8)+hvr(k,16))
       ucnt(i,ii,6)=ucnt(i,ii,1)
       ucnt(i,ii,7)=ucnt(i,ii,3)
       ucnt(i,ii,8)=ucnt(i,ii,5)
       ucnt(i,ii,9)=ucnt(i,ii,1)
       ucnt(i,ii,10)=ucnt(i,ii,3)
       ucnt(i,ii,11)=ucnt(i,ii,5)

c      Points 1,3,5 interpolation in rib i+1
       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,1).and.u(i,j+1,3).ge.ucnt(i,ii,1)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,1)=xm*ucnt(i,ii,1)+xb
       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       end if

       if (u(i,j,3).le.ucnt(i,ii,5).and.u(i,j+1,3).ge.ucnt(i,ii,5)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,5)=xm*ucnt(i,ii,5)+xb
       end if

       end do

c      Points 9,10,11 interpolation in rib i+1
       do j=1,np(i,2)

       if (u(i,j,3).gt.ucnt(i,ii,9).and.u(i,j+1,3).le.ucnt(i,ii,9)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,9)=xm*ucnt(i,ii,9)+xb
       jcon(i,ii,9)=j+1
       end if

       if (u(i,j,3).gt.ucnt(i,ii,10).and.u(i,j+1,3).le.ucnt(i,ii,10)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,10)=xm*ucnt(i,ii,10)+xb
       jcon(i,ii,10)=j+1
       end if

       if (u(i,j,3).gt.ucnt(i,ii,11).and.u(i,j+1,3).le.ucnt(i,ii,11)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,11)=xm*ucnt(i,ii,11)+xb
       jcon(i,ii,11)=j+1
       end if

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat 9-10-11 in n spaces (rib i+1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Experimental version     
c      Reformat in 20 spaces

       n1vr=jcon(i,ii,9)-jcon(i,ii,11)+1
       n2vr=20+1

c      Load data polyline
       xlin1(1)=ucnt(i,ii,9)
       ylin1(1)=vcnt(i,ii,9)
       do j=2,n1vr-1
       xlin1(j)=u(i,jcon(i,ii,9)-j+1,3)
       ylin1(j)=v(i,jcon(i,ii,9)-j+1,3)
c      MIRAR SI CAL +-1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       end do
       xlin1(n1vr)=ucnt(i,ii,11)
       ylin1(n1vr)=vcnt(i,ii,11)

c      Call subroutine vector redistribution

       call vredis(xlin1,ylin1,xlin3,ylin3,n1vr,n2vr)

c      Load result polyline

       do j=1,n2vr
       ucnt3(i,ii,j)=xlin3(j)
       vcnt3(i,ii,j)=ylin3(j)
       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Rib localisation
       i=hvr(k,3)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3.1.4 V-ribs lines 1 2 3 transportation to 3D espace
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Rib i-1 (Line 1)

       i=hvr(k,3)-1

       tetha=rib(i,8)*pi/180.

       do j=1,21
       ru(i,j,3)=ucnt1(i,ii,j)
       rv(i,j,3)=vcnt1(i,ii,j)-rib(i,50)
       end do

       do j=1,21

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       rx1(i+1,j,ii)=rx(i,j)
       ry1(i+1,j,ii)=ry(i,j)
       rz1(i+1,j,ii)=rz(i,j)

       end do

c      Rib i (Line 2)

       i=hvr(k,3)

       tetha=rib(i,8)*pi/180.
       
       do j=1,21
       ru(i,j,3)=ucnt2(i,ii,j)
       rv(i,j,3)=vcnt2(i,ii,j)-rib(i,50)
       end do

       do j=1,21

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       rx2(i,j,ii)=rx(i,j)
       ry2(i,j,ii)=ry(i,j)
       rz2(i,j,ii)=rz(i,j)

       end do

c      Rib i+1 (Line 3)

       i=hvr(k,3)+1

       tetha=rib(i,8)*pi/180.

       do j=1,21
       ru(i,j,3)=ucnt3(i,ii,j)
       rv(i,j,3)=vcnt3(i,ii,j)-rib(i,50)
c      COMPTE AMB el rib(i,50) A ESTUDIAR       
       end do

       do j=1,21

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       rx3(i-1,j,ii)=rx(i,j)
       ry3(i-1,j,ii)=ry(i,j)
       rz3(i-1,j,ii)=rz(i,j)

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3.1.5 V-ribs calculus and drawing in 3D and 2D
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3.1.5.1 V-rib 1-2 in 2D model (blue)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)-1

       px0=0.
       py0=0.
       ptheta=0.

       do j=1,21

c      Distances between points
       pa=dsqrt((rx(i+1,j)-rx(i,j))**2.+(ry(i+1,j)-ry(i,j))**2.+
     + (rz(i+1,j)-rz(i,j))**2.)
       pb=dsqrt((rx(i+1,j+1)-rx(i,j))**2.+(ry(i+1,j+1)-ry(i,j))**2.+
     + (rz(i+1,j+1)-rz(i,j))**2.)
       pc=dsqrt((rx(i+1,j+1)-rx(i+1,j))**2.+(ry(i+1,j+1)-ry(i+1,j))**2.+
     + (rz(i+1,j+1)-rz(i+1,j))**2.)
       pd=dsqrt((rx(i+1,j)-rx(i,j+1))**2.+(ry(i+1,j)-ry(i,j+1))**2.+
     + (rz(i+1,j)-rz(i,j+1))**2.)
       pe=dsqrt((rx(i,j+1)-rx(i,j))**2.+(ry(i,j+1)-ry(i,j))**2.+
     + (rz(i,j+1)-rz(i,j))**2.)
       pf=dsqrt((rx(i+1,j+1)-rx(i,j+1))**2.+(ry(i+1,j+1)-ry(i,j+1))**2.+
     + (rz(i+1,j+1)-rz(i,j+1))**2.)
       
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

c      Drawing in 2D model
       
       psep=3300.*xkf+xrsep*float(i)
       psey=800.*xkf+yrsep*float(ii)

       if (hvr(k,5).eq.1) then

       j=1
c      Costat vora atac
       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),5)

       j=21
c      Costat fuga
       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),5)

c      Marca punts MC a l'esquerra

       alpha=-(datan((pl1y(i,1)-pl2y(i,20))/(pl1x(i,1)-pl2x(i,20))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xp6=pl1x(i,1)-xdes*dsin(alpha)-2.*xdes*dcos(alpha)
       yp6=pl1y(i,1)-xdes*dcos(alpha)+2.*xdes*dsin(alpha)
       xp8=pl1x(i,21)-xdes*dsin(alpha)+2.*xdes*dcos(alpha)
       yp8=pl1y(i,21)-xdes*dcos(alpha)-2.*xdes*dsin(alpha)
       xp7=0.5*(xp6+xp8)
       yp7=0.5*(yp6+yp8)

       call point(psep+xp6,psey+yp6,1)
c       call point(psep+xp7,psey+yp7,1)
       call point(psep+xp8,psey+yp8,1)

c     Romano costat esquerra

      sl=1.
       
      xpx=(pl1x(i,1)+pl2x(i,20))/2.-xdes*dsin(alpha)
      xpy=(pl1y(i,1)+pl2y(i,20))/2.-xdes*dcos(alpha)

      xpx2=psep+xpx+0.5*hvr(k,8)*dcos(alpha)-0.3*xvrib*dsin(alpha)
      xpy2=psey+xpy-0.5*hvr(k,8)*dsin(alpha)-0.3*xvrib*dcos(alpha) 

      call romano(i,xpx2,xpy2,alpha,typm6(10)*0.1,7)

      xpx2=psep+xpx-0.5*hvr(k,8)*dcos(alpha)-0.3*xvrib*dsin(alpha)
      xpy2=psey+xpy+0.5*hvr(k,8)*dsin(alpha)-0.3*xvrib*dcos(alpha) 

      call romano(int(hvr(k,4)),xpx2,xpy2,alpha,typm6(10)*0.1,7)


c      Marca punts MC a la dreta

       alpha=-(datan((pr1y(i,1)-pr2y(i,20))/(pr1x(i,1)-pr2x(i,20))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xp7=0.5*(pr1x(i,1)+pr2x(i,20))+xdes*dsin(alpha)
       yp7=0.5*(pr1y(i,1)+pr2y(i,20))+xdes*dcos(alpha)

       call point(psep+xp7,psey+yp7,3)


c     Romano costat dret

      sr=1.
       
      xpx=(pr1x(i,1)+pr2x(i,20))/2.+xdes*dsin(alpha)
      xpy=(pr1y(i,1)+pr2y(i,20))/2.+xdes*dcos(alpha)

      xpx2=psep+xpx+0.3*hvr(k,7)*dcos(alpha)+0.3*xvrib*dsin(alpha)
      xpy2=psey+xpy-0.3*hvr(k,7)*dsin(alpha)+0.3*xvrib*dcos(alpha) 

      call romano(i+1,xpx2,xpy2,alpha,typm6(10)*0.1,7)
       
       do j=1,21-1

c      Vores de costura esquerra
       alpl=-(datan((pl1y(i,j)-pl2y(i,j))/(pl1x(i,j)-pl2x(i,j))))
       if (alpl.lt.0.) then
       alpl=alpl+pi
       end if

       lvcx(i,j)=psep+pl1x(i,j)-xvrib*dsin(alpl)
       lvcy(i,j)=psey+pl1y(i,j)-xvrib*dcos(alpl)

c      Vores de costura dreta
       alpr=-(datan((pr1y(i,j)-pr2y(i,j))/(pr1x(i,j)-pr2x(i,j))))
       if (alpr.lt.0.) then
       alpr=alpr+pi
       end if

       rvcx(i,j)=psep+pr1x(i,j)+xvrib*dsin(alpr)
       rvcy(I,j)=psey+pr1y(i,j)+xvrib*dcos(alpr)

c      Tancament lateral inici
       if (j.eq.1) then
       call line(psep+pl1x(i,j)-xvrib*dsin(alpl),psey+pl1y(i,j)
     + -xvrib*dcos(alpl),psep+pl1x(i,j),psey+pl1y(i,j),5)
       call line(psep+pr1x(i,j)+xvrib*dsin(alpr),psey+pr1y(i,j)
     + +xvrib*dcos(alpr),psep+pr1x(i,j),psey+pr1y(i,j),5)
       end if

c      Tancament lateral fi
       if (j.eq.20) then
       call line(psep+pl2x(i,j)-xvrib*dsin(alpl),psey+pl2y(i,j)
     + -xvrib*dcos(alpl),psep+pl2x(i,j),psey+pl2y(i,j),5)
       call line(psep+pr2x(i,j)+xvrib*dsin(alpr),psey+pr2y(i,j)
     + +xvrib*dcos(alpr),psep+pr2x(i,j),psey+pr2y(i,j),5)

       lvcx(i,j+1)=psep+pl2x(i,j)-xvrib*dsin(alpl)
       lvcy(i,j+1)=psey+pl2y(i,j)-xvrib*dcos(alpl)

       rvcx(i,j+1)=psep+pr2x(i,j)+xvrib*dsin(alpr)
       rvcy(i,j+1)=psey+pr2y(i,j)+xvrib*dcos(alpr)

       end if

c      V-rib length
       hvr(k,15)=dsqrt((lvcx(i,1)-rvcx(i,1))**2.+
     + (lvcy(i,1)-rvcy(i,1))**2.)

c      Numera cintes V Type 3
       call itxt(psep-xrsep+83.*xkf-120.*(typm3(10)/10.),
     + psey-10,typm3(10),0.0d0,i,7)
       call itxt(psep+hvr(k,15)-xrsep+83.*xkf-120.*(typm3(10)/10.),
     + psey-10,typm3(10),0.0d0,i+1,7)
       
       end do

c      Dibuixa vores amb segments completament enllaçats       
       do j=1,20

       call line(lvcx(i,j),lvcy(i,j),lvcx(i,j+1),lvcy(i,j+1),5)
       call line(rvcx(i,j),rvcy(i,j),rvcx(i,j+1),rvcy(i,j+1),5)

       end do

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3.1.5.2 V-rib 2-3 in 2D model (red)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)

       px0=0.
       py0=0.
       ptheta=0.

       do j=1,21

c      Distances between points
       pa=dsqrt((rx(i+1,j)-rx(i,j))**2.+(ry(i+1,j)-ry(i,j))**2.+
     + (rz(i+1,j)-rz(i,j))**2.)
       pb=dsqrt((rx(i+1,j+1)-rx(i,j))**2.+(ry(i+1,j+1)-ry(i,j))**2.+
     + (rz(i+1,j+1)-rz(i,j))**2.)
       pc=dsqrt((rx(i+1,j+1)-rx(i+1,j))**2.+(ry(i+1,j+1)-ry(i+1,j))**2.+
     + (rz(i+1,j+1)-rz(i+1,j))**2.)
       pd=dsqrt((rx(i+1,j)-rx(i,j+1))**2.+(ry(i+1,j)-ry(i,j+1))**2.+
     + (rz(i+1,j)-rz(i,j+1))**2.)
       pe=dsqrt((rx(i,j+1)-rx(i,j))**2.+(ry(i,j+1)-ry(i,j))**2.+
     + (rz(i,j+1)-rz(i,j))**2.)
       pf=dsqrt((rx(i+1,j+1)-rx(i,j+1))**2.+(ry(i+1,j+1)-ry(i,j+1))**2.+
     + (rz(i+1,j+1)-rz(i,j+1))**2.)
       
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

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Drawing in 2D model
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
       psep=3300.*xkf+xrsep*float(i)
       psey=800.*xkf+yrsep*float(ii)

       if (hvr(k,6).eq.1) then

       j=1

c      Costat vora d'atac
       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),1)

       j=21
c      Costat fuga
       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),1)


c      Marca punts MC a l'esquerra

       alpha=-(datan((pl1y(i,1)-pl2y(i,20))/(pl1x(i,1)-pl2x(i,20))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xp7=0.5*(pl1x(i,1)+pl1x(i,21))-xdes*dsin(alpha)
       yp7=0.5*(pl1y(i,1)+pl1y(i,21))-xdes*dcos(alpha)

       call point(psep+xp7,psey+yp7,1)

c      Romano costat esquerra

       sl=1.
       
       xpx=(pl1x(i,1)+pl2x(i,20))/2.-sl*xdes*dsin(alpha)
       xpy=(pl1y(i,1)+pl2y(i,20))/2.-sl*xdes*dcos(alpha)

       xpx2=psep+xpx+0.3*hvr(k,7)*dcos(alpha)-0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy-0.3*hvr(k,7)*dsin(alpha)-0.3*xvrib*dcos(alpha) 

       call romano(i,xpx2,xpy2,alpha,typm6(10)*0.1,7)

c      Marca punts MC a la dreta

       alpha=-(datan((pr1y(i,1)-pr2y(i,20))/(pr1x(i,1)
     + -pr2x(i,20))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xp6=pr1x(i,1)+xdes*dsin(alpha)-2.*xdes*dcos(alpha)
       yp6=pr1y(i,1)+xdes*dcos(alpha)+2.*xdes*dsin(alpha)
       xp8=pr1x(i,21)+xdes*dsin(alpha)+2.*xdes*dcos(alpha)
       yp8=pr1y(i,21)+xdes*dcos(alpha)-2.*xdes*dsin(alpha)
       xp7=0.5*(xp6+xp8)
       yp7=0.5*(yp6+yp8)

       call point(psep+xp6,psey+yp6,1)
c       call point(psep+xp7,psey+yp7,1)
       call point(psep+xp8,psey+yp8,1)

c      Romano costat dret

       sr=1.
       
       xpx=(pr1x(i,1)+pr2x(i,20))/2.+xdes*dsin(alpha)
       xpy=(pr1y(i,1)+pr2y(i,20))/2.+xdes*dcos(alpha)

       xpx2=psep+xpx+0.5*hvr(k,8)*dcos(alpha)+0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy-0.5*hvr(k,8)*dsin(alpha)+0.3*xvrib*dcos(alpha) 

       call romano(i+1,xpx2,xpy2,alpha,typm6(10)*0.1,7)

       xpx2=psep+xpx-0.5*hvr(k,8)*dcos(alpha)+0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy+0.5*hvr(k,8)*dsin(alpha)+0.3*xvrib*dcos(alpha) 

       call romano(int(hvr(k,4)),xpx2,xpy2,alpha,typm6(10)*0.1,7)

c      Vores de costura

       do j=1,21-1

c      Vores de costura esquerra
       alpl=-(datan((pl1y(i,j)-pl2y(i,j))/(pl1x(i,j)-pl2x(i,j))))
       if (alpl.lt.0.) then
       alpl=alpl+pi
       end if

       lvcx(i,j)=psep+pl1x(i,j)-xvrib*dsin(alpl)
       lvcy(i,j)=psey+pl1y(i,j)-xvrib*dcos(alpl)

c      Vores de costura dreta
       alpr=-(datan((pr1y(i,j)-pr2y(i,j))/(pr1x(i,j)-pr2x(i,j))))
       if (alpr.lt.0.) then
       alpr=alpr+pi
       end if

       rvcx(i,j)=psep+pr1x(i,j)+xvrib*dsin(alpr)
       rvcy(i,j)=psey+pr1y(i,j)+xvrib*dcos(alpr)

c      Tancament lateral inici
       if (j.eq.1) then
       call line(psep+pl1x(i,j)-xvrib*dsin(alpl),psey+pl1y(i,j)
     + -xvrib*dcos(alpl),psep+pl1x(i,j),psey+pl1y(i,j),1)
       call line(psep+pr1x(i,j)+xvrib*dsin(alpr),psey+pr1y(i,j)
     + +xvrib*dcos(alpr),psep+pr1x(i,j),psey+pr1y(i,j),1)
       end if

c      Tancament lateral fi
       if (j.eq.20) then
       call line(psep+pl2x(i,j)-xvrib*dsin(alpl),psey+pl2y(i,j)
     + -xvrib*dcos(alpl),psep+pl2x(i,j),psey+pl2y(i,j),1)
       call line(psep+pr2x(i,j)+xvrib*dsin(alpr),psey+pr2y(i,j)
     + +xvrib*dcos(alpr),psep+pr2x(i,j),psey+pr2y(i,j),1)

       lvcx(i,j+1)=psep+pl2x(i,j)-xvrib*dsin(alpl)
       lvcy(i,j+1)=psey+pl2y(i,j)-xvrib*dcos(alpl)

       rvcx(i,j+1)=psep+pr2x(i,j)+xvrib*dsin(alpr)
       rvcy(i,j+1)=psey+pr2y(i,j)+xvrib*dcos(alpr)

       end if

c      V-rib length
       hvr(k,15)=dsqrt((lvcx(i,1)-rvcx(i,1))**2.+
     + (lvcy(i,1)-rvcy(i,1))**2.)

c      Numera cintes V Type 3
       call itxt(psep-xrsep+83.*xkf-120.*(typm3(10)/10.),
     + psey-10,typm3(10),0.0d0,i,7)
       call itxt(psep+hvr(k,15)-xrsep+83.*xkf-120.*(typm3(10)/10.),
     + psey-10,typm3(10),0.0d0,i+1,7)

       end do

c      Dibuixa vores amb segments completament enllaçats       
       do j=1,20

       call line(lvcx(i,j),lvcy(i,j),lvcx(i,j+1),lvcy(i,j+1),1)
       call line(rvcx(i,j),rvcy(i,j),rvcx(i,j+1),rvcy(i,j+1),1)

       end do

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3.1.5.3 Drawing V-ribs marks in 2D ribs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Drawing in 2D ribs printing
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Box (1,2)

       sepxx=700.*xkf
       sepyy=100.*xkf

c      Rib i-1
       kx=int((float(i-2)/6.))
       ky=i-1-kx*6

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)

       if (hvr(k,5).eq.1) then
       call line(sepx+ucnt(i-1,ii,9),-vcnt(i-1,ii,9)+sepy,
     + sepx+ucnt(i-1,ii,11),-vcnt(i-1,ii,11)+sepy,4)

c      Draw 3 point in 9 and 11

       alpha=(datan((v(i-1,jcon(i-1,ii,9)-1,3)-v(i-1,jcon(i-1,ii,9)+1,
     + 3))/(u(i-1,jcon(i-1,ii,9)-1,3)-u(i-1,jcon(i-1,ii,9)+1,3))))
       if (alpha.lt.0.) then
c       alpha=alpha+pi
       end if

       xpeq=ucnt(i-1,ii,9)-1.*xdes*dsin(alpha)
       ypeq=vcnt(i-1,ii,9)+1.*xdes*dcos(alpha)

       call point(sepx+xpeq,sepy-ypeq,92)
       call point(sepx+xpeq+1*dsin(alpha),sepy-ypeq+1*dcos(alpha),92)
       call point(sepx+xpeq+2*dsin(alpha),sepy-ypeq+2*dcos(alpha),92)

       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,2)
       call point(2530.*xkf+sepx+xpeq+1*dsin(alpha),sepy-ypeq+
     + 1*dcos(alpha),92)
       call point(2530.*xkf+sepx+xpeq+2*dsin(alpha),sepy-ypeq+
     + 2*dcos(alpha),92)


       alpha=(datan((v(i-1,jcon(i-1,ii,11)-1,3)-v(i-1,jcon(i-1,ii,11)+1,
     + 3))/(u(i-1,jcon(i-1,ii,11)-1,3)-u(i-1,jcon(i-1,ii,11)+1,3))))
       if (alpha.lt.0.) then
c       alpha=alpha+pi
       end if

       xpeq=ucnt(i-1,ii,11)-1.*xdes*dsin(alpha)
       ypeq=vcnt(i-1,ii,11)+1.*xdes*dcos(alpha)

       call point(sepx+xpeq,sepy-ypeq,92)
       call point(sepx+xpeq+1*dsin(alpha),sepy-ypeq+1*dcos(alpha),92)
       call point(sepx+xpeq+2*dsin(alpha),sepy-ypeq+2*dcos(alpha),92)

       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,92)
       call point(2530.*xkf+sepx+xpeq+1*dsin(alpha),sepy-ypeq+
     + 1*dcos(alpha),92)
       call point(2530.*xkf+sepx+xpeq+2*dsin(alpha),sepy-ypeq+
     + 2*dcos(alpha),92)

c      Marks in V-rib
       alpha=datan((vcnt(i-1,ii,11)-vcnt(i-1,ii,9))/
     + (ucnt(i-1,ii,11)-ucnt(i-1,ii,9)))

       xp9=ucnt(i-1,ii,9)-xdes*dsin(alpha)+2.*xdes*dcos(alpha)
       yp9=vcnt(i-1,ii,9)+xdes*dcos(alpha)+2.*xdes*dsin(alpha)

       xp11=ucnt(i-1,ii,11)-xdes*dsin(alpha)-2.*xdes*dcos(alpha)
       yp11=vcnt(i-1,ii,11)+xdes*dcos(alpha)-2.*xdes*dsin(alpha)

       xu=sepx+xp9
       xv=-sepy+yp9
c       call pointg(xu,xv,xcir,1)
       xu=sepx+xp11
       xv=-sepy+yp11
c       call pointg(xu,xv,xcir,1)

       end if

c      Rib i (center)
       kx=int((float(i-1)/6.))
       ky=i-kx*6

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)

       call line(sepx+ucnt(i,ii,2),-vcnt(i,ii,2)+sepy,
     + sepx+ucnt(i,ii,4),-vcnt(i,ii,4)+sepy,4)

c      Rib i+1
       kx=int((float(i)/6.))
       ky=i+1-kx*6

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)

       if (hvr(k,6).eq.1) then
       call line(sepx+ucnt(i+1,ii,9),-vcnt(i+1,ii,9)+sepy,
     + sepx+ucnt(i+1,ii,11),-vcnt(i+1,ii,11)+sepy,3)

c      Punts marcatge V-rib
       alpha=datan((vcnt(i+1,ii,11)-vcnt(i+1,ii,9))/
     + (ucnt(i+1,ii,11)-ucnt(i+1,ii,9)))

       xp9=ucnt(i+1,ii,9)-xdes*dsin(alpha)
       yp9=vcnt(i+1,ii,9)+xdes*dcos(alpha)

       xp11=ucnt(i+1,ii,11)-xdes*dsin(alpha)
       yp11=vcnt(i+1,ii,11)+xdes*dcos(alpha)

       xu=sepx+xp9
       xv=-sepy+yp9
c       call pointg(xu,xv,xcir,1)
       xu=sepx+xp11
       xv=-sepy+yp11
c       call pointg(xu,xv,xcir,1)

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Drawing V-ribs in 2D ribs mesa corte
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Box (1,2)

       sepxx=700.*xkf
       sepyy=100.*xkf

c      Rib i-1
       kx=int((float(i-2)/6.))
       ky=i-1-kx*6

       sepx=2530.*xkf+sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)

       if (hvr(k,5).eq.1) then
c      Segment               
       call line(sepx+ucnt(i-1,ii,9),-vcnt(i-1,ii,9)+sepy,
     + sepx+ucnt(i-1,ii,11),-vcnt(i-1,ii,11)+sepy,4)

c      Punts marcatge V-rib
       alpha=datan((vcnt(i-1,ii,11)-vcnt(i-1,ii,9))/
     + (ucnt(i-1,ii,11)-ucnt(i-1,ii,9)))

       xp9=ucnt(i-1,ii,9)-xdes*dsin(alpha)+2.*xdes*dcos(alpha)
       yp9=vcnt(i-1,ii,9)+xdes*dcos(alpha)+2.*xdes*dsin(alpha)

       xp11=ucnt(i-1,ii,11)-xdes*dsin(alpha)-2.*xdes*dcos(alpha)
       yp11=vcnt(i-1,ii,11)+xdes*dcos(alpha)-2.*xdes*dsin(alpha)

       call point(sepx+xp9,sepy-yp9,3)
       call point(sepx+xp11,sepy-yp11,3)

       end if


c      Rib i+1
       kx=int((float(i)/6.))
       ky=i+1-kx*6

       sepx=2530.*xkf+sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)

       if (hvr(k,6).eq.1) then
c      Segment  
       call line(sepx+ucnt(i+1,ii,9),-vcnt(i+1,ii,9)+sepy,
     + sepx+ucnt(i+1,ii,11),-vcnt(i+1,ii,11)+sepy,3)


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Draw 3 point in 9 and 11

       alpha=(datan((v(i+1,jcon(i+1,ii,9)-1,3)-v(i+1,jcon(i+1,ii,9)+1,
     + 3))/(u(i+1,jcon(i+1,ii,9)-1,3)-u(i+1,jcon(i+1,ii,9)+1,3))))
       if (alpha.lt.0.) then
c       alpha=alpha+pi
       end if

       xpeq=ucnt(i+1,ii,9)-1.*xdes*dsin(alpha)
       ypeq=vcnt(i+1,ii,9)+1.*xdes*dcos(alpha)

       call point(sepx+xpeq,sepy-ypeq,92)
       call point(sepx+xpeq+1*dsin(alpha),sepy-ypeq+1*dcos(alpha),92)
       call point(sepx+xpeq+2*dsin(alpha),sepy-ypeq+2*dcos(alpha),92)

       call point(-2530.*xkf+sepx+xpeq,sepy-ypeq,92)
       call point(-2530.*xkf+sepx+xpeq+1*dsin(alpha),sepy-ypeq+
     + 1*dcos(alpha),92)
       call point(-2530.*xkf+sepx+xpeq+2*dsin(alpha),sepy-ypeq+
     + 2*dcos(alpha),92)


       alpha=(datan((v(i+1,jcon(i+1,ii,11)-1,3)-v(i+1,jcon(i+1,ii,11)+1,
     + 3))/(u(i+1,jcon(i+1,ii,11)-1,3)-u(i+1,jcon(i+1,ii,11)+1,3))))
       if (alpha.lt.0.) then
c       alpha=alpha+pi
       end if

       xpeq=ucnt(i+1,ii,11)-1.*xdes*dsin(alpha)
       ypeq=vcnt(i+1,ii,11)+1.*xdes*dcos(alpha)

       call point(sepx+xpeq,sepy-ypeq,92)
       call point(sepx+xpeq+1*dsin(alpha),sepy-ypeq+1*dcos(alpha),92)
       call point(sepx+xpeq+2*dsin(alpha),sepy-ypeq+2*dcos(alpha),92)

       call point(-2530.*xkf+sepx+xpeq,sepy-ypeq,5)
       call point(-2530.*xkf+sepx+xpeq+1*dsin(alpha),sepy-ypeq+
     + 1*dcos(alpha),92)
       call point(-2530.*xkf+sepx+xpeq+2*dsin(alpha),sepy-ypeq+
     + 2*dcos(alpha),92)



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

           
c      Punts marcatge V-rib
       alpha=datan((vcnt(i+1,ii,11)-vcnt(i+1,ii,9))/
     + (ucnt(i+1,ii,11)-ucnt(i+1,ii,9)))

       xp9=ucnt(i+1,ii,9)-xdes*dsin(alpha)
       yp9=vcnt(i+1,ii,9)+xdes*dcos(alpha)

       xp11=ucnt(i+1,ii,11)-xdes*dsin(alpha)
       yp11=vcnt(i+1,ii,11)+xdes*dcos(alpha)

       call point(sepx+xp9,sepy-yp9,3)
       call point(sepx+xp11,sepy-yp11,3)

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Draw V-rib type 3 in 3D model
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Left rib
       if (hvr(k,5).eq.1) then

       do j=1,21
       call line3d(rx2(i,j,ii),ry2(i,j,ii),rz2(i,j,ii),
     + rx1(i,j,ii),ry1(i,j,ii),rz1(i,j,ii),4)
       call line3d(-rx2(i,j,ii),ry2(i,j,ii),rz2(i,j,ii),
     + -rx1(i,j,ii),ry1(i,j,ii),rz1(i,j,ii),4)
       end do

       end if


c      Right rib
       if (hvr(k,6).eq.1) then

       do j=1,21
       call line3d(rx2(i,j,ii),ry2(i,j,ii),rz2(i,j,ii),
     + rx3(i,j,ii),ry3(i,j,ii),rz3(i,j,ii),3)
       call line3d(-rx2(i,j,ii),ry2(i,j,ii),rz2(i,j,ii),
     + -rx3(i,j,ii),ry3(i,j,ii),rz3(i,j,ii),3)
       end do

       end if


c      end if V-rib type 3

       end if




ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3.2 Continuous full V-rib type-5
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (hvr(k,2).eq.5) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3.2.1 Rib i points 2,3,4
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Define main points 2,3,4,9,10,11
c      See schema for points interpretation

       i=hvr(k,3)    ! rib i
c      PLEASE make max hvr(k,4)=rib(i,15)
       ii=hvr(k,4)   ! row ii
      
       ucnt(i,ii,3)=u(i,ii,6)
       ucnt(i,ii,2)=ucnt(i,ii,3)-hvr(k,10)
       ucnt(i,ii,4)=ucnt(i,ii,3)+hvr(k,10)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Points 2,3,4 interpolation in rib i
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,2).and.u(i,j+1,3).ge.ucnt(i,ii,2)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,2)=xm*ucnt(i,ii,2)+xb
       jcon(i,ii,2)=j

c      First 2
       if (ii.eq.1) then
       xtu2(i)=ucnt(i,ii,2)
       xtv2(i)=vcnt(i,ii,2)
       jcon2(i,ii,2)=jcon(i,ii,2)

c      Calcule te-2, variable rib(i,102)
       rib(i,102)=0.
       do jj=np(i,1),jcon2(i,ii,2)+2,-1
       rib(i,102)=rib(i,102)+sqrt((u(i,jj-1,3)-u(i,jj,3))**2+
     + (v(i,jj-1,3)-v(i,jj,3))**2)
       end do
       rib(i,102)=rib(i,102)+sqrt((u(i,jcon2(i,ii,2)+1,3)-ucnt(i,ii,2))
     + **2+(v(i,jcon2(i,ii,2)+1,3)-vcnt(i,ii,2))**2)

       end if

       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       jcon(i,ii,3)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,4).and.u(i,j+1,3).ge.ucnt(i,ii,4)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,4)=xm*ucnt(i,ii,4)+xb
       jcon(i,ii,4)=j

c      Last 4
       if (ii.eq.rib(i,15)) then
       xtu4(i)=ucnt(i,ii,4)
       xtv4(i)=vcnt(i,ii,4)
       jcon4(i,ii,4)=jcon(i,ii,4)

c      Calcule te-4
       rib(i,104)=0.
       do jj=np(i,1),jcon4(i,ii,4)+2,-1
       rib(i,104)=rib(i,104)+sqrt((u(i,jj-1,3)-u(i,jj,3))**2+
     + (v(i,jj-1,3)-v(i,jj,3))**2)
       end do
       rib(i,104)=rib(i,104)+sqrt((u(i,jcon4(i,ii,4)+1,3)-ucnt(i,ii,4))
     + **2+(v(i,jcon4(i,ii,4)+1,3)-vcnt(i,ii,4))**2)
c      Verificar graficament que OK

       end if

       end if

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Continue calculus c ii max
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (hvr(k,4).eq.rib(i,15)) then

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat line "2" in n spaces
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Reformat in 120 spaces
       n1vr=jcon4(i,int(rib(i,15)),4)-jcon2(i,1,2)+1
       n2vr=121

c      Load data polyline
       xlin1(1)=xtu2(i)
       ylin1(1)=xtv2(i)
       do j=2,n1vr-1
       xlin1(j)=u(i,jcon2(i,1,2)+j-1,3)
       ylin1(j)=v(i,jcon2(i,1,2)+j-1,3)
c      MIRAR SI CAL +-1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       end do
       xlin1(n1vr)=xtu4(i)
       ylin1(n1vr)=xtv4(i)
      
c      Call subroutine vector redistribution

       call vredis(xlin1,ylin1,xlin3,ylin3,n1vr,n2vr)

c      Load result polyline

       do j=1,n2vr
       ucnt2(i,ii,j)=xlin3(j)
       vcnt2(i,ii,j)=ylin3(j)
c       write (*,*) "Line 2f: ", j, xlin3(j),ylin3(j)
       end do
       
       end if ! Enf if in ii max
   

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3.2.2 Points 9-10-11 in Rib i-1 and reformat
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      We only need first "9" and last "11"

       i=hvr(k,3)-1
       ii=hvr(k,4)

       ucnt(i,ii,3)=u(i,ii,6)
       ucnt(i,ii,1)=ucnt(i,ii,3)-hvr(k,10)
       ucnt(i,ii,2)=ucnt(i,ii,3)-hvr(k,10)
       ucnt(i,ii,4)=ucnt(i,ii,3)+hvr(k,10)
       ucnt(i,ii,5)=ucnt(i,ii,3)+hvr(k,10)
       ucnt(i,ii,6)=ucnt(i,ii,1)
       ucnt(i,ii,7)=ucnt(i,ii,3)
       ucnt(i,ii,8)=ucnt(i,ii,5)
       ucnt(i,ii,9)=ucnt(i,ii,1)
       ucnt(i,ii,10)=ucnt(i,ii,3)
       ucnt(i,ii,11)=ucnt(i,ii,5)
       jcon(i,ii,9)=1
       jcon(i,ii,10)=1
       jcon(i,ii,11)=1

c      Points 2,3,4 interpolation in rib i-1
       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,2).and.u(i,j+1,3).ge.ucnt(i,ii,2)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,2)=xm*ucnt(i,ii,2)+xb
       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       end if

       if (u(i,j,3).le.ucnt(i,ii,4).and.u(i,j+1,3).ge.ucnt(i,ii,4)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,4)=xm*ucnt(i,ii,4)+xb
       end if

       end do

c      Points 9,10,11 interpolation in rib i-1 (not used)
       do j=1,np(i,2)

       if (u(i,j,3).gt.ucnt(i,ii,9).and.u(i,j+1,3).le.ucnt(i,ii,9)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,9)=xm*ucnt(i,ii,9)+xb
       jcon(i,ii,9)=j+1
       end if

       if (u(i,j,3).gt.ucnt(i,ii,10).and.u(i,j+1,3).le.ucnt(i,ii,10)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,10)=xm*ucnt(i,ii,10)+xb
       jcon(i,ii,10)=j+1
       end if

       if (u(i,j,3).gt.ucnt(i,ii,11).and.u(i,j+1,3).le.ucnt(i,ii,11)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,11)=xm*ucnt(i,ii,11)+xb
       jcon(i,ii,11)=j+1
       end if

c      Use the interpolated intrados points when no upper crossing exists.
       if (jcon(i,ii,9).eq.1) vcnt(i,ii,9)=vcnt(i,ii,2)
       if (jcon(i,ii,10).eq.1) vcnt(i,ii,10)=vcnt(i,ii,3)
       if (jcon(i,ii,11).eq.1) vcnt(i,ii,11)=vcnt(i,ii,4)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Detect first 9 and last 11 (rib i-1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      First 9
       if (ii.eq.1) then

c      Line "r"
       xru(1)=ucnt(i,ii,2)
       xrv(1)=vcnt(i,ii,2)
       xru(2)=ucnt(i,ii,2)-1.
       xrv(2)=vcnt(i,ii,2)+dtan(hvr(k,7)*pi/180.)

c      Look at segments near "A"
       do ki=jcon(i,ii,10),np(i,2)

c      Line "s"
       xsu(1)=u(i,ki,3)
       xsv(1)=v(i,ki,3)
       xsu(2)=u(i,ki+1,3)
       xsv(2)=v(i,ki+1,3)

       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)

       if (xsu(1).ge.xtu.and.xsu(2).lt.xtu.and.xsv(2).ge.0.) then
       xtu9(i)=xtu
       xtv9(i)=xtv
       jcon9(i,ii,2)=ki+1
       end if
     
       end do

c      Dibuix provisional, but in vertical rib!    
c       call line(xru(1),-300-xrv(1),xtu9(i),-300-xtv9(i),7)

       end if   ! First 9

       
c      Last 11
       if (ii.eq.rib(i,15)) then

c      Line "r"
       xru(1)=ucnt(i,ii,4)
       xrv(1)=vcnt(i,ii,4)
       xru(2)=ucnt(i,ii,4)+1.
       xrv(2)=vcnt(i,ii,4)+dtan(hvr(k,8)*pi/180.)

c      Look at segments near "A"
       do ki=1,np(i,2)

c      Line "s"
       xsu(1)=u(i,ki,3)
       xsv(1)=v(i,ki,3)
       xsu(2)=u(i,ki+1,3)
       xsv(2)=v(i,ki+1,3)

       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)

       if (xsu(1).ge.xtu.and.xsu(2).lt.xtu.and.xsv(2).ge.0.) then
       xtu11(i)=xtu
       xtv11(i)=xtv
       jcon11(i,ii,2)=ki+1

c      Calcule te-11
       rib(i,105)=0.
       do jj=1,jcon11(i,ii,2)-2

       rib(i,105)=rib(i,105)+sqrt((u(i,jj+1,3)-u(i,jj,3))**2+
     + (v(i,jj+1,3)-v(i,jj,3))**2)
       end do
ccccccc Ep! rib(1,105) NO.........!ccccccccccccccccccccccccccccccccccc
c       Error corregit 2018-05-12
c       rib(i,105)=rib(1,105)+sqrt((u(i,jcon11(i,ii,2)-1,3)-xtu11(i))
c     + **2+(v(i,jcon11(i,ii,2)-1,3)-xtv11(i))**2)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       rib(i,105)=rib(i,105)+sqrt((u(i,jcon11(i,ii,2)-1,3)-xtu11(i))
     + **2+(v(i,jcon11(i,ii,2)-1,3)-xtv11(i))**2)


       end if
     
       end do

c      Dibuix provisional, but in vertical rib!
c       call line(xru(1),-300-xrv(1),xtu11(i),-300-xtv11(i),7)

       end if   ! Last 11

       end do   ! in points upper surface

c      Keep later tangent lookups inside the profile when no crossing exists.
       if (jcon(i,ii,9).eq.1) jcon(i,ii,9)=2
       if (jcon(i,ii,10).eq.1) jcon(i,ii,10)=2
       if (jcon(i,ii,11).eq.1) jcon(i,ii,11)=2

c      Calculus of partial lengths in rib i-1

c      Calculus of te-11 length
       if (ii.eq.rib(i,15)) then
       xlte11(i)=0.
       do kl=1,jcon11(i,ii,2)-2
       xlte11(i)=xlte11(i)+sqrt((u(i,kl+1,3)-u(i,kl,3))**2+
     + (v(i,kl+1,3)-v(i,kl,3))**2)
       end do 
       kl=jcon11(i,ii,2)-1
       xlte11(i)=xlte11(i)+sqrt((u(i,kl,3)-xtu11(i))**2+
     + (v(i,kl,3)-xtv11(i))**2)
       end if

c      Calculus of le-9 length
       if (ii.eq.1) then
c       write (*,*) "Epppp   ", jcon9(i,ii,2),np(i,2)
       xlle9(i)=0.
       do kl=jcon9(i,ii,2),np(i,2)-1
       xlle9(i)=xlle9(i)+sqrt((u(i,kl+1,3)-u(i,kl,3))**2+
     + (v(i,kl+1,3)-v(i,kl,3))**2)
       end do 
       kl=jcon9(i,ii,2)
       xlle9(i)=xlle9(i)+sqrt((u(i,kl,3)-xtu9(i))**2+
     + (v(i,kl,3)-xtv9(i))**2)
       end if


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat line 1 (9-11) in n spaces (rib i-1)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c       if (ii.eq.rib(i+1,15)) then ! why i+1?
       if (ii.eq.rib(i,15)) then


c      Reformat in 120 spaces

       n1vr=jcon9(i,1,2)-jcon11(i,int(rib(i,15)),2)+1    
       n2vr=121

c      Load data polyline
       xlin1(1)=xtu9(i)
       ylin1(1)=xtv9(i)
       do j=2,n1vr-1
       xlin1(j)=u(i,jcon9(i,1,2)-j+1,3)
       ylin1(j)=v(i,jcon9(i,1,2)-j+1,3)
c      MIRAR SI CAL +-1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       end do
       xlin1(n1vr)=xtu11(i)
       ylin1(n1vr)=xtv11(i)

c      Esborrar?       
       do j=1,n1vr-1
c       call line(xlin1(j),-300-ylin1(j),xlin1(j+1),-300-ylin1(j+1),3)
       end do

c      Call subroutine vector redistribution

       call vredis(xlin1,ylin1,xlin3,ylin3,n1vr,n2vr)

c      Load result polyline

       do j=1,n2vr
       ucnt1(i,ii,j)=xlin3(j)
       vcnt1(i,ii,j)=ylin3(j)
       end do

       end if ! ii=rib(i+1,15)


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3.2.3 Points 9-10-11 in Rib i+1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      We only need first "9" and last "11"

       i=hvr(k,3)+1
       ii=hvr(k,4)

       ucnt(i,ii,3)=u(i,ii,6)
       ucnt(i,ii,1)=ucnt(i,ii,3)-hvr(k,10)
       ucnt(i,ii,2)=ucnt(i,ii,3)-hvr(k,10)
       ucnt(i,ii,4)=ucnt(i,ii,3)+hvr(k,10)
       ucnt(i,ii,5)=ucnt(i,ii,3)+hvr(k,10)
       ucnt(i,ii,6)=ucnt(i,ii,1)
       ucnt(i,ii,7)=ucnt(i,ii,3)
       ucnt(i,ii,8)=ucnt(i,ii,5)
       ucnt(i,ii,9)=ucnt(i,ii,1)
       ucnt(i,ii,10)=ucnt(i,ii,3)
       ucnt(i,ii,11)=ucnt(i,ii,5)
       jcon(i,ii,9)=1
       jcon(i,ii,10)=1
       jcon(i,ii,11)=1

c      Points 2,3,4 interpolation in rib i+1
       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,2).and.u(i,j+1,3).ge.ucnt(i,ii,2)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,2)=xm*ucnt(i,ii,2)+xb
       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       end if

       if (u(i,j,3).le.ucnt(i,ii,4).and.u(i,j+1,3).ge.ucnt(i,ii,4)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,4)=xm*ucnt(i,ii,4)+xb
       end if

       end do

c      Points 9,10,11 interpolation in rib i+1 (not used)
       do j=1,np(i,2)

       if (u(i,j,3).gt.ucnt(i,ii,9).and.u(i,j+1,3).le.ucnt(i,ii,9)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,9)=xm*ucnt(i,ii,9)+xb
       jcon(i,ii,9)=j+1
       end if

       if (u(i,j,3).gt.ucnt(i,ii,10).and.u(i,j+1,3).le.ucnt(i,ii,10)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,10)=xm*ucnt(i,ii,10)+xb
       jcon(i,ii,10)=j+1
       end if

       if (u(i,j,3).gt.ucnt(i,ii,11).and.u(i,j+1,3).le.ucnt(i,ii,11)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,11)=xm*ucnt(i,ii,11)+xb
       jcon(i,ii,11)=j+1
       end if

c      Use the interpolated intrados points when no upper crossing exists.
       if (jcon(i,ii,9).eq.1) vcnt(i,ii,9)=vcnt(i,ii,2)
       if (jcon(i,ii,10).eq.1) vcnt(i,ii,10)=vcnt(i,ii,3)
       if (jcon(i,ii,11).eq.1) vcnt(i,ii,11)=vcnt(i,ii,4)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Detect first 9 and last 11 (rib i+1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      First 9
       if (ii.eq.1) then

c      Line "r"
       xru(1)=ucnt(i,ii,2)
       xrv(1)=vcnt(i,ii,2)
       xru(2)=ucnt(i,ii,2)-1.
       xrv(2)=vcnt(i,ii,2)+dtan(hvr(k,7)*pi/180.)

c      Look at segments near "A"
       do ki=jcon(i,ii,10),np(i,2)

c      Line "s"
       xsu(1)=u(i,ki,3)
       xsv(1)=v(i,ki,3)
       xsu(2)=u(i,ki+1,3)
       xsv(2)=v(i,ki+1,3)

       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)

       if (xsu(1).ge.xtu.and.xsu(2).lt.xtu.and.xsv(2).ge.0.) then
       xtu9(i)=xtu
       xtv9(i)=xtv
       jcon9(i,ii,2)=ki+1
       end if
     
       end do

       end if   ! First 9

c      Last 11
       if (ii.eq.rib(i,15)) then

c      Line "r"
       xru(1)=ucnt(i,ii,4)
       xrv(1)=vcnt(i,ii,4)
       xru(2)=ucnt(i,ii,4)+1.
       xrv(2)=vcnt(i,ii,4)+dtan(hvr(k,8)*pi/180.)

c      Look at segments near "A"
       do ki=1,np(i,2)

c      Line "s"
       xsu(1)=u(i,ki,3)
       xsv(1)=v(i,ki,3)
       xsu(2)=u(i,ki+1,3)
       xsv(2)=v(i,ki+1,3)

       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)

       if (xsu(1).ge.xtu.and.xsu(2).lt.xtu.and.xsv(2).ge.0.) then
       xtu11(i)=xtu
       xtv11(i)=xtv
       jcon11(i,ii,2)=ki+1

c      Calcule te-11
       rib(i,105)=0.
       do jj=1,jcon11(i,ii,2)-2
       rib(i,105)=rib(i,105)+sqrt((u(i,jj+1,3)-u(i,jj,3))**2+
     + (v(i,jj+1,3)-v(i,jj,3))**2)
       end do
       rib(i,105)=rib(i,105)+sqrt((u(i,jcon11(i,ii,2)-1,3)-xtu11(i))
     + **2+(v(i,jcon11(i,ii,2)-1,3)-xtv11(i))**2)

       end if
     
       end do

       end if   ! Last 11

       end do

c      Keep later tangent lookups inside the profile when no crossing exists.
       if (jcon(i,ii,9).eq.1) jcon(i,ii,9)=2
       if (jcon(i,ii,10).eq.1) jcon(i,ii,10)=2
       if (jcon(i,ii,11).eq.1) jcon(i,ii,11)=2

c      Calculus of partial lengths in rib i+1

c      Calculus of te-11 length
       if (ii.eq.rib(i,15)) then
       xrte11(i)=0.
       do kl=1,jcon11(i,ii,2)-2
       xrte11(i)=xrte11(i)+sqrt((u(i,kl+1,3)-u(i,kl,3))**2+
     + (v(i,kl+1,3)-v(i,kl,3))**2)
       end do 
       kl=jcon11(i,ii,2)-1
       xrte11(i)=xrte11(i)+sqrt((u(i,kl,3)-xtu11(i))**2+
     + (v(i,kl,3)-xtv11(i))**2)
       end if

c      Calculus of le-9 length
       if (ii.eq.1) then
c       write (*,*) "Epppp   ", jcon9(i,ii,2),np(i,2)
       xrle9(i)=0.
       do kl=jcon9(i,ii,2),np(i,2)-1
       xrle9(i)=xrle9(i)+sqrt((u(i,kl+1,3)-u(i,kl,3))**2+
     + (v(i,kl+1,3)-v(i,kl,3))**2)
       end do 
       kl=jcon9(i,ii,2)
       xrle9(i)=xrle9(i)+sqrt((u(i,kl,3)-xtu9(i))**2+
     + (v(i,kl,3)-xtv9(i))**2)
c       write (*,*) "xrle9 ok ",i,xrle9(i)
       end if

c      Calculus of partial lengths in rib i (intrados)

c      Calculus of te-4 length
       if (ii.eq.rib(i,15)) then

             end if

c      Calculus of le-2 length
       if (ii.eq.1) then

             end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat line 3 9-11 in n spaces (rib i+1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      if (ii.eq.rib(i-1,15)) then ! why i-1?
       if (ii.eq.rib(i,15)) then

c      Reformat in 120 spaces

       n1vr=jcon9(i,1,2)-jcon11(i,int(rib(i,15)),2)+1    
       n2vr=121

c      Load data polyline
       xlin1(1)=xtu9(i)
       ylin1(1)=xtv9(i)
       do j=2,n1vr-1
       xlin1(j)=u(i,jcon9(i,1,2)-j+1,3)
       ylin1(j)=v(i,jcon9(i,1,2)-j+1,3)
c      MIRAR SI CAL +-1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       end do
       xlin1(n1vr)=xtu11(i)
       ylin1(n1vr)=xtv11(i)

       do j=1,n1vr-1
c       call line(xlin1(j),-312-ylin1(j),xlin1(j+1),-312-ylin1(j+1),6)
       end do

c      Call subroutine vector redistribution

       call vredis(xlin1,ylin1,xlin3,ylin3,n1vr,n2vr)

       do j=1,n2vr-1
c       call line(xlin3(j),-310-ylin3(j),xlin3(j+1),-310-ylin3(j+1),4)
       end do

c      Load result polyline

       do j=1,n2vr
       ucnt3(i,ii,j)=xlin3(j)
       vcnt3(i,ii,j)=ylin3(j)
       end do

       end if ! ii=rib(i-1,15)


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Rib localisation
       i=hvr(k,3)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (ii.eq.rib(i,15)) then
       i=hvr(k,3)-1
       do j=1,121
c       write (*,*) "C2: ",ucnt1(i,ii,j),vcnt1(i,ii,j)
       end do
c       write (*,*) "control ", i,ii,j,n2vr
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3.2.4 Lines 1 2 3 transportation to 3D espace
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (ii.eq.rib(i,15)) then

c      Rib i-1 (Line 1)

       i=hvr(k,3)-1

       tetha=rib(i,8)*pi/180.

       do j=1,121
       ru(i,j,3)=ucnt1(i,ii,j)
       rv(i,j,3)=vcnt1(i,ii,j)-rib(i,50)
c       write (*,*) "C3: ",ucnt1(i,ii,j),vcnt1(i,ii,j)
       end do
c       write (*,*) "control ", i,ii,j,n2vr

c      ESBORRAR
c       do j=1,n2vr-1
c       call line(300+ru(i,j,3),-300-rv(i,j,3),
c     + 300+ru(i,j+1,3),-300-rv(i,j+1,3),1)
c       end do


       do j=1,121

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       rx1(i+1,j,ii)=rx(i,j)
       ry1(i+1,j,ii)=ry(i,j)
       rz1(i+1,j,ii)=rz(i,j)

c       write (*,*) j, rx(i,j),ry(i,j),rz(i,j)


       end do

c      Rib i (Line 2)

       i=hvr(k,3)

       tetha=rib(i,8)*pi/180.
       
       do j=1,121
       ru(i,j,3)=ucnt2(i,ii,j)
       rv(i,j,3)=vcnt2(i,ii,j)-rib(i,50)
       end do

       do j=1,121

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       rx2(i,j,ii)=rx(i,j)
       ry2(i,j,ii)=ry(i,j)
       rz2(i,j,ii)=rz(i,j)

       end do

c      Rib i+1 (Line 3)

       i=hvr(k,3)+1

       tetha=rib(i,8)*pi/180.

       do j=1,121
       ru(i,j,3)=ucnt3(i,ii,j)
       rv(i,j,3)=vcnt3(i,ii,j)-rib(i,50)
c      COMPTE AMB el rib(i,50) A ESTUDIAR       
       end do

       do j=1,121

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       rx3(i-1,j,ii)=rx(i,j)
       ry3(i-1,j,ii)=ry(i,j)
       rz3(i-1,j,ii)=rz(i,j)

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3.2.5 V-ribs full Type 5 calculus and drawing in 3D and 2D
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
c      16.3.2.5.1 Left rib Type 5 (blue) 1-2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      AQUEST IF es tanca al final del rib 15??????????????????
       if (ii.eq.rib(i,15)) then

       i=hvr(k,3)-1

c      Call flattening subroutine

       call flatt(i,n2vr,rx,ry,rz,
     + pl1x,pl1y,pl2x,pl2y,pr1x,pr1y,pr2x,pr2y)

c      Rotate ribs in 2D
c      Align using right side
       angle=datan((pr1x(i,121)-pr1x(i,1))/(pr1y(i,121)-pr1y(i,1)))
       angle=angle*180./pi
       angle2=angle

       xc=dcos(angle*pi/180.0d0)
       xs=dsin(angle*pi/180.0d0)
      
       do j=1,n2vr
       px9o(j)=xc*pl1x(i,j)-xs*pl1y(i,j)
       py9o(j)=xs*pl1x(i,j)+xc*pl1y(i,j)
       end do
       do j=1,n2vr
       pl1x(i,j)=px9o(j)
       pl1y(i,j)=py9o(j)
       end do

       do j=1,n2vr
       px9o(j)=xc*pl2x(i,j)-xs*pl2y(i,j)
       py9o(j)=xs*pl2x(i,j)+xc*pl2y(i,j)
       end do
       do j=1,n2vr
       pl2x(i,j)=px9o(j)
       pl2y(i,j)=py9o(j)
       end do

       do j=1,n2vr
       px9o(j)=xc*pr1x(i,j)-xs*pr1y(i,j)
       py9o(j)=xs*pr1x(i,j)+xc*pr1y(i,j)
       end do
       do j=1,n2vr
       pr1x(i,j)=px9o(j)
       pr1y(i,j)=py9o(j)
       end do

       do j=1,n2vr
       px9o(j)=xc*pr2x(i,j)-xs*pr2y(i,j)
       py9o(j)=xs*pr2x(i,j)+xc*pr2y(i,j)
       end do
       do j=1,n2vr
       pr2x(i,j)=px9o(j)
       pr2y(i,j)=py9o(j)
       end do
      
c      Drawing in 2D model (BOX 2,7)
       
       psep=5820.*xkf+xrsep*1.2*hvr_positions%horizontal(5)*
     + float(i)-150
       psey=800.*xkf+yrsep*hvr_positions%vertical(5)*float(ii)-500.

       if (hvr(k,5).eq.1) then

c      Draw basic contour, left and right
       do j=1,120
       call line(psep+pl1x(i,j),psey+pl1y(i,j),
     + psep+pl1x(i,j+1),psey+pl1y(i,j+1),5)
       call line(psep+pr1x(i,j),psey+pr1y(i,j),
     + psep+pr1x(i,j+1),psey+pr1y(i,j+1),5)
       end do

       j=1
       call line(psep+pl1x(i,j),psey+pl1y(i,j),
     + psep+pr1x(i,j),psey+pr1y(i,j),5)

c      xsegment definition
       xsegment=dsqrt((pl1x(i,j)-pr1x(i,j))**2+
     + (pl1y(i,j)-pr1y(i,j))**2)
c      xrvlen
       xrvlen=dsqrt((pr1x(i,1)-pr1x(i,121))**2+
     + (pr1y(i,1)-pr1y(i,121))**2)

       j=121
       call line(psep+pl1x(i,j),psey+pl1y(i,j),
     + psep+pr1x(i,j),psey+pr1y(i,j),5)


c      External edges calculus and drawing

       do j=1,121

c      Edges left
       alpl=-(datan((pl1y(i,j)-pl2y(i,j))/(pl1x(i,j)-pl2x(i,j))))
       if (alpl.lt.0.) then
       alpl=alpl+pi
       end if
c      Correction in alpl
       if (j.eq.120) then
       alpl120=alpl
       end if
       if (j.eq.121) then
       alpl=alpl120
       end if

       lvcx(i,j)=psep+pl1x(i,j)-xrib*0.1*dsin(alpl)
       lvcy(i,j)=psey+pl1y(i,j)-xrib*0.1*dcos(alpl)

c      Edges right
       alpr=-datan2(pr1y(i,j)-pr2y(i,j),
     + pr1x(i,j)-pr2x(i,j))
c      Correction in alpr
       if (j.eq.120) then
       alpr120=alpr
       end if
       if (j.eq.121) then
       alpr=alpr120
       end if
       alprr(j)=alpr

       rvcx(i,j)=psep+pr1x(i,j)+xrib*0.1*dsin(alpr)
       rvcy(i,j)=psey+pr1y(i,j)+xrib*0.1*dcos(alpr)

       end do

c      Edges drawing       
       do j=1,120
       call line(lvcx(i,j),lvcy(i,j),lvcx(i,j+1),lvcy(i,j+1),5)
       end do

c      Segments drawing

       j=1
       call line(lvcx(i,j),lvcy(i,j),psep+pl1x(i,j),psey+pl1y(i,j),5)
       call line(rvcx(i,j),rvcy(i,j),psep+pr1x(i,j),psey+pr1y(i,j),5)
       j=121
       call line(lvcx(i,j),lvcy(i,j),psep+pl1x(i,j),psey+pl1y(i,j),5)
       call line(rvcx(i,j),rvcy(i,j),psep+pr1x(i,j),psey+pr1y(i,j),5)

c      Calculus of 9-11 length in planar rib i-1
        
       xl911(i)=0.
       do j=1,120
       xl911(i)=xl911(i)+sqrt((pl1x(i,j)-pl1x(i,j+1))**2+
     + (pl1y(i,j)-pl1y(i,j+1))**2)
       end do 

c      Calculus of 2-4 length in planar rib i-1 (rib i)
        
       xc24(i+1)=0.
       do j=1,120
       xc24(i+1)=xc24(i+1)+sqrt((pr1x(i,j)-pr1x(i,j+1))**2+
     + (pr1y(i,j)-pr1y(i,j+1))**2)
       end do 


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      VERIFICATION left     
c       write (*,*) "Rib i-1 ",i,xlle9(i),xl911(i),xlte11(i),rib(i,31),
c     + xlle9(i)+xl911(i)+xlte11(i) !OK
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Naming and marks in V-rib full continous (left-blue)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      V-rib length
       hvr(k,15)=dsqrt((lvcx(i,1)-rvcx(i,1))**2.+
     + (lvcy(i,1)-rvcy(i,1))**2.)

c      Naming ribs with number

       call itxt(psep-xrsep+83.*xkf-120.*(typm3(10)/10.),
     + psey-10.,typm3(10),0.0d0,i,7)
       call itxt(psep-xrsep+hvr(k,15)+83.*xkf-120.*(typm3(10)/10.),
     + psey-10.,typm3(10),0.0d0,i+1,7)


c      Mark rib at left (roman number) 

       alpha=-(datan((pl1y(i,1)-pr1y(i,1))/(pl1x(i,1)-pr1x(i,1))))

       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       sl=1.
       
       xpx=pl1x(i,1)*0.8+pr1x(i,1)*0.2-0.2*xrib*dsin(alpha) !double xrib
       xpy=pl1y(i,1)*0.8+pr1y(i,1)*0.2-0.2*xrib*dcos(alpha)
       xpx2=psep+xpx
       xpy2=psey+xpy

       call romano(i,xpx2,xpy2,alpha,typm6(10)*0.1,7)

c      Mark rib at right (roman number) 

       alpha=-(datan((pl1y(i,1)-pr1y(i,1))/(pl1x(i,1)-pr1x(i,1))))

       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       sr=1.
       
       xpx=pl1x(i,1)*0.2+pr1x(i,1)*0.8-0.2*xrib*dsin(alpha)
       xpy=pl1y(i,1)*0.2+pr1y(i,1)*0.8-0.2*xrib*dcos(alpha) 
       xpx2=psep+xpx
       xpy2=psey+xpy

       call romano(i+1,xpx2,xpy2,alpha,typm6(10)*0.1,7)


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Mark equidistant points in left rib (upper surface)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       jcontrol=1

       do jm=1,60 ! Till 60 marks

       xmarkjm=xmark*float(jm)

c      Draw only in rib
       if (xmarkjm.ge.rib(i,105).and.xmarkjm.le.rib(i,105)+xl911(i)) 
     + then

c      Define rib(i,106) only for first mark
       if (jcontrol.eq.1) then
       rib(i,106)=xmarkjm-rib(i,105)
       end if
       jcontrol=0

       xlen=0.
       xlenp=dsqrt((pl1x(i,121)-pl1x(i,120))**2+
     + (pl1y(i,121)-pl1y(i,120))**2)  ! Acumulated length

       do j=121,3,-1
       
c      Detect and draw equidistant point
       if (xmarkjm-rib(i,105).ge.xlen.and.xmarkjm-rib(i,105).lt.xlenp) 
     + then

       rib(i,107)=xmarkjm-xlen-rib(i,105)
       rib(i,108)=dsqrt((pl1x(i,j-1)-pl1x(i,j))**2+
     + (pl1y(i,j-1)-pl1y(i,j))**2)

c      Interpolate
       xequis=pl1x(i,j)-(rib(i,107)*(pl1x(i,j)-pl1x(i,j-1)))/rib(i,108)
       yequis=pl1y(i,j)-(rib(i,107)*(pl1y(i,j)-pl1y(i,j-1)))/rib(i,108)

c      Draw
       alpha=-(datan((pl1y(i,j)-pl1y(i,j-1))/(pl1x(i,j)-pl1x(i,j-1))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xpeq=xequis-1.*xdes*dsin(alpha)
       ypeq=yequis+1.*xdes*dcos(alpha)

       call point (psep+xpeq,psey+ypeq,3)

       end if ! detect point

c      Set xlen, xlenp for next segment
       xlen=xlen+sqrt((pl1x(i,j)-pl1x(i,j-1))**2+
     + (pl1y(i,j)-pl1y(i,j-1))**2)
       xlenp=xlen+sqrt((pl1x(i,j-1)-pl1x(i,j-2))**2+
     + (pl1y(i,j-1)-pl1y(i,j-2))**2)

       end do ! contour V-rib

       end if ! marks zone

       end do ! marks jm

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Mark equidistant points in bottom surface (rib i+1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Move to rib i+1

       jcontrol=1

       do jm=1,60 ! Till 50 marks

       xmarkjm=xmark*float(jm)

c      Draw only in rib
       if (xmarkjm.ge.rib(i+1,104).and.xmarkjm.le.rib(i+1,104)+
     + xc24(i+1)) then

c      Define rib(i,106) only for first mark
       if (jcontrol.eq.1) then
       rib(i+1,106)=xmarkjm-rib(i+1,104)
       end if
       jcontrol=0

       xlen=0.
       xlenp=dsqrt((pr1x(i,121)-pr1x(i,120))**2+
     + (pr1y(i,121)-pr1y(i,120))**2)

       do j=121,3,-1
       
c      Detect and draw equidistant point
       if (xmarkjm-rib(i+1,104).ge.xlen.and.xmarkjm-rib(i+1,104).lt.
     + xlenp) then

       rib(i+1,107)=xmarkjm-xlen-rib(i+1,104)
       rib(i+1,108)=dsqrt((pr1x(i,j-1)-pr1x(i,j))**2+
     + (pr1y(i,j-1)-pr1y(i,j))**2)

c      Interpolate
       xequis=pr1x(i,j)-(rib(i+1,107)*(pr1x(i,j)-pr1x(i,j-1)))/
     + rib(i+1,108)
       yequis=pr1y(i,j)-(rib(i+1,107)*(pr1y(i,j)-pr1y(i,j-1)))/
     + rib(i+1,108)

c      Draw
       alpha=-(datan((pr1y(i,j)-pr1y(i,j-1))/(pr1x(i,j)-pr1x(i,j-1))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xpeq=xequis+1.*xdes*dsin(alpha)
       ypeq=yequis-1.*xdes*dcos(alpha)

       call point (psep+xpeq,psey+ypeq,3)

       end if ! detect point

       xlen=xlen+sqrt((pr1x(i,j)-pr1x(i,j-1))**2+
     + (pr1y(i,j)-pr1y(i,j-1))**2)
       xlenp=xlen+sqrt((pr1x(i,j-1)-pr1x(i,j-2))**2+
     + (pr1y(i,j-1)-pr1y(i,j-2))**2)

       end do ! contour V-rib

       end if ! marks zone

       end do ! marks jm

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Mark anchor points in left rib
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call find_hvr_hole_spec(hvr_holes,15,hvr_hole,
     + hvr_hole_found)
       do klz=1,rib(i+1,15)

       xlen=0.
       xlenp=dsqrt((pr1x(i,121)-pr1x(i,120))**2+
     + (pr1y(i,121)-pr1y(i,120))**2)

       do j=121,3,-1

c      Detect and draw anchor point
       if (rib(i+1,130+klz)-rib(i+1,104).ge.xlen.and.
     + rib(i+1,130+klz)-rib(i+1,104).lt.
     + xlenp) then

    
       rib(i+1,107)=rib(i+1,130+klz)-xlen-rib(i+1,104)
       rib(i+1,108)=dsqrt((pr1x(i,j-1)-pr1x(i,j))**2+
     + (pr1y(i,j-1)-pr1y(i,j))**2)

c      Interpolate
       xequis=pr1x(i,j)-(rib(i+1,107)*(pr1x(i,j)-pr1x(i,j-1)))/
     + rib(i+1,108)
       yequis=pr1y(i,j)-(rib(i+1,107)*(pr1y(i,j)-pr1y(i,j-1)))/
     + rib(i+1,108)

c      Define anchor points in planar V-rib
       xanchor(i+1,klz)=xequis
       yanchor(i+1,klz)=yequis

c      Draw
       alpha=-(datan((pr1y(i,j)-pr1y(i,j-1))/(pr1x(i,j)-pr1x(i,j-1))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xpeq=xequis+1.*xdes*dsin(alpha)
       ypeq=yequis-1.*xdes*dcos(alpha)

c      Girar angle
       call point (psep+xpeq,psey+ypeq,30)
       call point (psep+xpeq-1.,psey+ypeq,30)
       call point (psep+xpeq-2.,psey+ypeq,30)

c      Section 38: one type-15 ellipse at each requested anchor
       if (hvr_hole_found.and.klz.le.hvr_hole%hole_count) then
       hvr_hmax=xequis-pl1x(i,j)
       x0=psep+xequis-hvr_hmax*hvr_hole%parameter(3)
       y0=psey+yequis
       a=abs(hvr_hole%parameter(1)*hvr_hmax*0.005d0)
       b=abs(hvr_hole%parameter(2)*hvr_hmax*0.005d0)
       call ellipse(x0,y0,a,b,0.0d0,3)
       endif

       end if ! detect point

       xlen=xlen+sqrt((pr1x(i,j)-pr1x(i,j-1))**2+
     + (pr1y(i,j)-pr1y(i,j-1))**2)
       xlenp=xlen+sqrt((pr1x(i,j-1)-pr1x(i,j-2))**2+
     + (pr1y(i,j-1)-pr1y(i,j-2))**2)

       end do ! in contour

       end do ! in anchors

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Draw parabolic holes in left rib
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


       do klz=1,rib(i+1,15)-1

       xprb(i+1,klz,0)=(xanchor(i+1,klz)+xanchor(i+1,klz+1))/2.
       yprb(i+1,klz,0)=(yanchor(i+1,klz)+yanchor(i+1,klz+1))/2.
       jconi(klz)=1
       jconf(klz)=120

       do j=1,120

c      Point 5
       if (pl1y(i,j).le.yprb(i+1,klz,0).and.pl1y(i,j+1).gt.
     + yprb(i+1,klz,0)) then

       xm=(pl1y(i,j+1)-pl1y(i,j))/(pl1x(i,j+1)-pl1x(i,j))
       xb=pl1y(i,j)-xm*pl1x(i,j)

       yprb(i+1,klz,5)=yprb(i+1,klz,0)
       xprb(i+1,klz,5)=(yprb(i+1,klz,5)-xb)/xm
       
c      Point 6
       dist05=xprb(i+1,klz,0)-xprb(i+1,klz,5)
       dist06=dist05*hvr(k,9)/100.
       yprb(i+1,klz,6)=yprb(i+1,klz,0)
       xprb(i+1,klz,6)=xprb(i+1,klz,0)-dist06
       end if

c      Point 1

       if (pr1y(i,j).le.yanchor(i+1,klz)+hvr(k,10).and.pr1y(i,j+1).gt.
     + yanchor(i+1,klz)+hvr(k,10)) then

       xm=(pr1y(i,j+1)-pr1y(i,j))/(pr1x(i,j+1)-pr1x(i,j))
       xb=pr1y(i,j)-xm*pr1x(i,j)

       yprb(i+1,klz,1)=yanchor(i+1,klz)+hvr(k,10)
       xprb(i+1,klz,1)=(yprb(i+1,klz,1)-xb)/xm
       end if

       end do ! end j

       end do ! end klz


       do klz=1,rib(i+1,15)-1

       do j=1,120

c      Point 2 Intersection right side parabola with airfoil contour

       if (pr1y(i,j).ge.yprb(i+1,klz,0).and.pr1y(i,j).lt.
     + yanchor(i+1,klz+1)) then

c      Line "r" (parabola)

       xkprb(klz)=(xprb(i+1,klz,1)-xprb(i+1,klz,6))/
     + ((yprb(i+1,klz,1)-yprb(i+1,klz,6))**2.)

       xru(1)=xkprb(klz)*((pr1y(i,j)-yprb(i+1,klz,6))**2.)+
     + xprb(i+1,klz,6)
       xrv(1)=pr1y(i,j)
       xru(2)=xkprb(klz)*((pr1y(i,j+1)-yprb(i+1,klz,6))**2.)+
     + xprb(i+1,klz,6)
       xrv(2)=pr1y(i,j+1)

c      Line "s" (bottom surface)

       xsu(1)=pr1x(i,j)
       xsv(1)=pr1y(i,j)
       xsu(2)=pr1x(i,j+1)
       xsv(2)=pr1y(i,j+1)

       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)


       if (xsv(1).le.xtv.and.xsv(2).gt.xtv) then  !!REVISAR
       yprb(i+1,klz,2)=xtv
       xprb(i+1,klz,2)=xtu
       end if  


       end if   

       end do ! in j

      
c       call line(psep+xprb(i+1,klz,0),psey+yprb(i+1,klz,0),
c     + psep+xprb(i+1,klz,6),psey+yprb(i+1,klz,6),2)

c       call line(psep+xprb(i+1,klz,1),psey+yprb(i+1,klz,1),
c     + psep+xprb(i+1,klz,6),psey+yprb(i+1,klz,6),3)

c       call line(psep+xprb(i+1,klz,2),psey+yprb(i+1,klz,2),
c     + psep+xprb(i+1,klz,6),psey+yprb(i+1,klz,6),1)

c      Draw parabolas


c      Detect extremal points
       do j=1,120

       if (pr1y(i,j).le.yprb(i+1,klz,1).and.pr1y(i,j+1).gt.
     + yprb(i+1,klz,1)) then
       jconi(klz)=j+1
       end if
       if (pr1y(i,j).le.yprb(i+1,klz,2).and.pr1y(i,j+1).gt.
     + yprb(i+1,klz,2)) then
       jconf(klz)=j
       end if

       end do

c      Draw parabolas

       if (hvr(k,9).le.100.) then

       j=jconi(klz)
       xpa1=xkprb(klz)*((pr1y(i,j)-yprb(i+1,klz,6))**2.)+
     + xprb(i+1,klz,6)
       call line(psep+xprb(i+1,klz,1),psey+yprb(i+1,klz,1),
     + psep+xpa1,psey+pr1y(i,j),5)
       do j=jconi(klz),jconf(klz)-1
       xpa1=xkprb(klz)*((pr1y(i,j)-yprb(i+1,klz,6))**2.)+
     + xprb(i+1,klz,6)
       xpa2=xkprb(klz)*((pr1y(i,j+1)-yprb(i+1,klz,6))**2.)+
     + xprb(i+1,klz,6)
       call line(psep+xpa1,psey+pr1y(i,j),
     + psep+xpa2,psey+pr1y(i,j+1),5)
       end do
       j=jconf(klz)
       xpa1=xkprb(klz)*((pr1y(i,j)-yprb(i+1,klz,6))**2.)+
     + xprb(i+1,klz,6)
       call line(psep+xprb(i+1,klz,2),psey+yprb(i+1,klz,2),
     + psep+xpa1,psey+pr1y(i,j),5)

       end if

       
c      Draw ellipses

       if (hvr(k,9).gt.100) then

       xprb(i+1,klz,7)=(xprb(i+1,klz,0)+xprb(i+1,klz,5))/2.
       yprb(i+1,klz,7)=yprb(i+1,klz,0)
       
       xa=0.5d0*((xprb(i+1,klz,0)-xprb(i+1,klz,5))*
     + (1.0d0-(2.0d0*(hvr(k,9)-100.0d0)/100.0d0)))
       xb=0.5d0*(yprb(i+1,klz,2)-yprb(i+1,klz,1)-2.*hvr(k,10))

       xgir=0.0d0

       do j=1,100-2.*pi/100.

       xsu(1)=xprb(i+1,klz,7)+xa*dcos(xgir)
       xsv(1)=yprb(i+1,klz,7)+xb*dsin(xgir)
       xsu(2)=xprb(i+1,klz,7)+xa*dcos(xgir+2.*pi/100.)
       xsv(2)=yprb(i+1,klz,7)+xb*dsin(xgir+2.*pi/100.)

       call line(psep+xsu(1),psey+xsv(1),psep+xsu(2),psey+xsv(2),5)

       xgir=xgir+2.0d0*pi/100.0d0

       end do

       end if

       end do ! in klz

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Complete the right cutting edge around parabolic openings
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       hvr_openings=max(0,int(rib(i+1,15))-1)
       hvr_edge_start=1
       do klz=1,hvr_openings
       hvr_gap_start=max(hvr_edge_start,
     + max(1,min(120,jconi(klz)-1)))
       hvr_gap_end=max(hvr_gap_start,
     + min(121,jconf(klz)+1))

       do j=hvr_edge_start,hvr_gap_start-1
       call line(rvcx(i,j),rvcy(i,j),
     + rvcx(i,j+1),rvcy(i,j+1),5)
       end do

       x1=psep+xprb(i+1,klz,1)
       y1=psey+yprb(i+1,klz,1)
       x2=x1+xrib*0.1d0*dsin(alprr(hvr_gap_start))
       y2=y1+xrib*0.1d0*dcos(alprr(hvr_gap_start))
       call line(x1,y1,x2,y2,5)
       call line(x2,y2,rvcx(i,hvr_gap_start),
     + rvcy(i,hvr_gap_start),5)

       x1=psep+xprb(i+1,klz,2)
       y1=psey+yprb(i+1,klz,2)
       x2=x1+xrib*0.1d0*dsin(alprr(hvr_gap_end))
       y2=y1+xrib*0.1d0*dcos(alprr(hvr_gap_end))
       call line(x1,y1,x2,y2,5)
       call line(x2,y2,rvcx(i,hvr_gap_end),
     + rvcy(i,hvr_gap_end),5)
       hvr_edge_start=hvr_gap_end
       end do

       do j=hvr_edge_start,120
       call line(rvcx(i,j),rvcy(i,j),
     + rvcx(i,j+1),rvcy(i,j+1),5)
       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Draw left diagonal in 3D space
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (ii.eq.rib(i,15)) then
       
       do j=1,121
       call line3d(rx2(i+1,j,ii),ry2(i+1,j,ii),rz2(i+1,j,ii),
     + rx1(i+1,j,ii),ry1(i+1,j,ii),rz1(i+1,j,ii),5)
       call line3d(-rx2(i+1,j,ii),ry2(i+1,j,ii),rz2(i+1,j,ii),
     + -rx1(i+1,j,ii),ry1(i+1,j,ii),rz1(i+1,j,ii),5)
       end do
       end if

       end if ! Draw left diagonal


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
c      16.3.2.5.2 Right rib (red) 2-3
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      i=hvr(k,3)+1 ? NO
       i=hvr(k,3)

c      Call flattening subroutine

       call flatt(i,n2vr,rx,ry,rz,
     + pl1x,pl1y,pl2x,pl2y,pr1x,pr1y,pr2x,pr2y)

c      Rotate ribs in 2D
c      Align using left side
       angle=datan((pr1x(i,121)-pr1x(i,1))/(pr1y(i,121)-pr1y(i,1)))
       angle=angle*180./pi

       xc=dcos(angle*pi/180.)
       xs=dsin(angle*pi/180.)

c      And do displacement using xsegment and angle2 defined in left rib
       xdx=xsegment*dcos(angle2*pi/180)
       xdy=xsegment*dsin(angle2*pi/180)

       do j=1,n2vr
       px9o(j)=xc*pl1x(i,j)-xs*pl1y(i,j)
       py9o(j)=xs*pl1x(i,j)+xc*pl1y(i,j)+xdy
       end do
       do j=1,n2vr
       pl1x(i,j)=px9o(j)
       pl1y(i,j)=py9o(j)
       end do

       do j=1,n2vr
       px9o(j)=xc*pl2x(i,j)-xs*pl2y(i,j)
       py9o(j)=xs*pl2x(i,j)+xc*pl2y(i,j)+xdy
       end do
       do j=1,n2vr
       pl2x(i,j)=px9o(j)
       pl2y(i,j)=py9o(j)
       end do

       do j=1,n2vr
       px9o(j)=xc*pr1x(i,j)-xs*pr1y(i,j)
       py9o(j)=xs*pr1x(i,j)+xc*pr1y(i,j)+xdy
       end do
       do j=1,n2vr
       pr1x(i,j)=px9o(j)
       pr1y(i,j)=py9o(j)
       end do

       do j=1,n2vr
       px9o(j)=xc*pr2x(i,j)-xs*pr2y(i,j)
       py9o(j)=xs*pr2x(i,j)+xc*pr2y(i,j)+xdy
       end do
       do j=1,n2vr
       pr2x(i,j)=px9o(j)
       pr2y(i,j)=py9o(j)
       end do


c      Drawing in 2D model
       
       psep=5820.*xkf+xrsep*1.2*hvr_positions%horizontal(5)*
     + float(i-1)+1.2*hvr_positions%horizontal(5)*xsegment-150
       psey=800.*xkf+yrsep*hvr_positions%vertical(5)*float(ii)-500.

       if (hvr(k,6).eq.1) then

c      Draw basic contour
       do j=1,120
       call line(psep+pl1x(i,j),psey+pl1y(i,j),
     + psep+pl1x(i,j+1),psey+pl1y(i,j+1),1)
       call line(psep+pr1x(i,j),psey+pr1y(i,j),
     + psep+pr1x(i,j+1),psey+pr1y(i,j+1),1)
       end do

       j=1
       call line(psep+pl1x(i,j),psey+pl1y(i,j),
     + psep+pr1x(i,j),psey+pr1y(i,j),1)
       j=121
       call line(psep+pl1x(i,j),psey+pl1y(i,j),
     + psep+pr1x(i,j),psey+pr1y(i,j),1)


c      External edges calculus and drawing

       do j=1,121

c      Edges left
       alpl=-datan2(pl1y(i,j)-pl2y(i,j),
     + pl1x(i,j)-pl2x(i,j))

c      Correction in alpl
       if (j.eq.120) then
       alpl120=alpl
       end if
       if (j.eq.121) then
       alpl=alpl120
       end if
       alpll(j)=alpl

       lvcx(i,j)=psep+pl1x(i,j)-xrib*0.1*dsin(alpl)
       lvcy(i,j)=psey+pl1y(i,j)-xrib*0.1*dcos(alpl)

c      Edges right
       alpr=-(datan((pr1y(i,j)-pr2y(i,j))/(pr1x(i,j)-pr2x(i,j))))
       if (alpr.lt.0.) then
       alpr=alpr+pi
       end if
c      Correction in alpr
       if (j.eq.120) then
       alpr120=alpr
       end if
       if (j.eq.121) then
       alpr=alpr120
       end if

       rvcx(i,j)=psep+pr1x(i,j)+xrib*0.1*dsin(alpr)
       rvcy(i,j)=psey+pr1y(i,j)+xrib*0.1*dcos(alpr)

       end do

c      Edges drawing       
       do j=1,120
       call line(rvcx(i,j),rvcy(i,j),rvcx(i,j+1),rvcy(i,j+1),1)
       end do

c      Segments drawing
       j=1
       call line(lvcx(i,j),lvcy(i,j),psep+pl1x(i,j),psey+pl1y(i,j),1)
       call line(rvcx(i,j),rvcy(i,j),psep+pr1x(i,j),psey+pr1y(i,j),1)
       j=121
       call line(lvcx(i,j),lvcy(i,j),psep+pl1x(i,j),psey+pl1y(i,j),1)
       call line(rvcx(i,j),rvcy(i,j),psep+pr1x(i,j),psey+pr1y(i,j),1)


c      Calculus of 9-11 length in planar rib i+1
        
       xr911(i+1)=0.
       do j=1,120
       xr911(i+1)=xr911(i+1)+sqrt((pr1x(i,j)-pr1x(i,j+1))**2+
     + (pr1y(i,j)-pr1y(i,j+1))**2)
       end do 

c      Calculus of 2-4 length in planar rib i
        
       xc24i=xc24(i) ! Left side
       xc24(i)=0.
       do j=1,120
       xc24(i)=xc24(i)+sqrt((pl1x(i,j)-pl1x(i,j+1))**2+
     + (pl1y(i,j)-pl1y(i,j+1))**2)
       end do 

c       write (*,*) "xc24(i) ",i,xc24i,xc24(i)
c      Petites diferencies 1 mm a corregir


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      VERIFICATION right     
c       write (*,*) "Rib i+1 ",i+1,xrle9(i+1),xr911(i+1),xrte11(i+1),
c     + rib(i+1,31),xrle9(i+1)+xr911(i+1)+xrte11(i+1) !
c      Hi ha petit error de 1 mm, a revisar (potser calcul r influ)
c      menys exacte r que l? a rutina planar
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Naming and marks in V-rib (right-red)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      V-rib length
       hvr(k,15)=dsqrt((lvcx(i,1)-rvcx(i,1))**2.+
     + (lvcy(i,1)-rvcy(i,1))**2.)

c      Naming ribs with number

       call itxt(psep-xrsep+83.*xkf-120.*(typm3(10)/10.),
     + psey-10,typm3(10),0.0d0,i,7)
       call itxt(psep-xrsep+hvr(k,15)+83.*xkf-120.*(typm3(10)/10.),
     + psey-10,typm3(10),0.0d0,i+1,7)

       
c      Mark rib at left (roman number) 

       alpha=-(datan((pl1y(i,1)-pr1y(i,1))/(pl1x(i,1)-pr1x(i,1))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       sl=1.
       
       xpx=pl1x(i,1)*0.8+pr1x(i,1)*0.2+0.2*xrib*dsin(alpha) !double xrib
       xpy=pl1y(i,1)*0.8+pr1y(i,1)*0.2+0.2*xrib*dcos(alpha) 
       xpx2=psep+xpx
       xpy2=psey+xpy

       call romano(i,xpx2,xpy2,alpha,typm6(10)*0.1,7)

c      Mark rib at right (roman number) 

       alpha=-(datan((pl1y(i,1)-pr1y(i,1))/(pl1x(i,1)-pr1x(i,1))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       sr=1.
       
       xpx=pl1x(i,1)*0.2+pr1x(i,1)*0.8+0.2*xrib*dsin(alpha)
       xpy=pl1y(i,1)*0.2+pr1y(i,1)*0.8+0.2*xrib*dcos(alpha)
       xpx2=psep+xpx
       xpy2=psey+xpy

       call romano(i+1,xpx2,xpy2,alpha,typm6(10)*0.1,7)


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Mark equidistant points in right rib (upper surface) rib i+1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       jcontrol=1

       do jm=1,60 ! Till 50 marks

       xmarkjm=xmark*float(jm)

c      Draw only in rib
       if (xmarkjm.ge.rib(i+1,105).and.xmarkjm.le.rib(i+1,105)+
     + xr911(i+1)) then

c      Define rib(i+1,106) only for first mark
       if (jcontrol.eq.1) then
       rib(i+1,106)=xmarkjm-rib(i+1,105)
       end if
       jcontrol=0

       xlen=0.
       xlenp=dsqrt((pr1x(i,121)-pr1x(i,120))**2+
     + (pr1y(i,121)-pr1y(i,120))**2)

       do j=121,3,-1  !!!!! REVISAR !!!!!!!!!!!!!!
c       do j=120,3,-1
       
c      Detect and draw equidistant point
       if (xmarkjm-rib(i+1,105).ge.xlen.and.xmarkjm-rib(i+1,105).lt.
     + xlenp) then

       rib(i+1,107)=xmarkjm-xlen-rib(i+1,105)
       rib(i+1,108)=dsqrt((pr1x(i,j-1)-pr1x(i,j))**2+
     + (pr1y(i,j-1)-pr1y(i,j))**2)

c      Interpolate
       xequis=pr1x(i,j)-(rib(i+1,107)*(pr1x(i,j)-pr1x(i,j-1)))/
     + rib(i+1,108)
       yequis=pr1y(i,j)-(rib(i+1,107)*(pr1y(i,j)-pr1y(i,j-1)))/
     + rib(i+1,108)

c      Draw
       alpha=-(datan((pr1y(i,j)-pr1y(i,j-1))/(pr1x(i,j)-pr1x(i,j-1))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xpeq=xequis+1.*xdes*dsin(alpha)
       ypeq=yequis-1.*xdes*dcos(alpha)

       call point (psep+xpeq,psey+ypeq,3)

       end if ! detect point

       xlen=xlen+sqrt((pr1x(i,j)-pr1x(i,j-1))**2+
     + (pr1y(i,j)-pr1y(i,j-1))**2)
       xlenp=xlen+sqrt((pr1x(i,j-1)-pr1x(i,j-2))**2+
     + (pr1y(i,j-1)-pr1y(i,j-2))**2)

       end do ! contour V-rib

       end if ! marks zone

       end do ! marks jm

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Mark equidistant points in bottom surface (rib i)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Move to rib i

       jcontrol=1

       do jm=1,60 ! Till 50 marks

       xmarkjm=xmark*float(jm)

c      Draw only in rib
       if (xmarkjm.ge.rib(i,104).and.xmarkjm.le.rib(i,104)+
     + xc24(i)) then

c      Define rib(i,106) only for first mark
       if (jcontrol.eq.1) then
       rib(i,106)=xmarkjm-rib(i,104)
       end if
       jcontrol=0

       xlen=0.
       xlenp=dsqrt((pl1x(i,121)-pl1x(i,120))**2+
     + (pl1y(i,121)-pl1y(i,120))**2)

       do j=121,3,-1
       
c      Detect and draw equidistant point
       if (xmarkjm-rib(i,104).ge.xlen.and.xmarkjm-rib(i,104).lt.
     + xlenp) then

       rib(i,107)=xmarkjm-xlen-rib(i,104)
       rib(i,108)=dsqrt((pl1x(i,j-1)-pl1x(i,j))**2+
     + (pl1y(i,j-1)-pl1y(i,j))**2)

c      Interpolate
       xequis=pl1x(i,j)-(rib(i,107)*(pl1x(i,j)-pl1x(i,j-1)))/
     + rib(i,108)
       yequis=pl1y(i,j)-(rib(i,107)*(pl1y(i,j)-pl1y(i,j-1)))/
     + rib(i,108)

c      Draw
       alpha=-(datan((pl1y(i,j)-pl1y(i,j-1))/(pl1x(i,j)-pl1x(i,j-1))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xpeq=xequis-1.*xdes*dsin(alpha)
       ypeq=yequis+1.*xdes*dcos(alpha)

       call point (psep+xpeq,psey+ypeq,3)

       end if ! detect point

       xlen=xlen+sqrt((pl1x(i,j)-pl1x(i,j-1))**2+
     + (pl1y(i,j)-pl1y(i,j-1))**2)
       xlenp=xlen+sqrt((pl1x(i,j-1)-pl1x(i,j-2))**2+
     + (pl1y(i,j-1)-pl1y(i,j-2))**2)

       end do ! contour V-rib

       end if ! marks zone

       end do ! marks jm


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Mark anchor points in right rib
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call find_hvr_hole_spec(hvr_holes,15,hvr_hole,
     + hvr_hole_found)
       do klz=1,rib(i,15)

       xlen=0.
       xlenp=dsqrt((pl1x(i,121)-pl1x(i,120))**2+
     + (pl1y(i,121)-pl1y(i,120))**2)

       do j=121,3,-1

c      Detect and draw anchor point
       if (rib(i,130+klz)-rib(i,104).ge.xlen.and.
     + rib(i,130+klz)-rib(i,104).lt.
     + xlenp) then

    
       rib(i,107)=rib(i,130+klz)-xlen-rib(i,104)
       rib(i,108)=dsqrt((pl1x(i,j-1)-pl1x(i,j))**2+
     + (pl1y(i,j-1)-pl1y(i,j))**2)

c      Interpolate
       xequis=pl1x(i,j)-(rib(i,107)*(pl1x(i,j)-pl1x(i,j-1)))/
     + rib(i,108)
       yequis=pl1y(i,j)-(rib(i,107)*(pl1y(i,j)-pl1y(i,j-1)))/
     + rib(i,108)

c      Define anchor points in planar V-rib
       xanchor(i,klz)=xequis
       yanchor(i,klz)=yequis

c      Draw
       alpha=-(datan((pl1y(i,j)-pl1y(i,j-1))/(pl1x(i,j)-pl1x(i,j-1))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xpeq=xequis-1.*xdes*dsin(alpha)
       ypeq=yequis+1.*xdes*dcos(alpha)

c      Girar angle
       call point (psep+xpeq,psey+ypeq,30)
       call point (psep+xpeq+1.,psey+ypeq,30)
       call point (psep+xpeq+2.,psey+ypeq,30)

c      Section 38: one type-15 ellipse at each requested anchor
       if (hvr_hole_found.and.klz.le.hvr_hole%hole_count) then
       hvr_hmax=-(xequis-pr1x(i,j))
       x0=psep+xequis+hvr_hmax*hvr_hole%parameter(3)
       y0=psey+yequis
       a=abs(hvr_hole%parameter(1)*hvr_hmax*0.005d0)
       b=abs(hvr_hole%parameter(2)*hvr_hmax*0.005d0)
       call ellipse(x0,y0,a,b,0.0d0,3)
       endif

       end if ! detect point

       xlen=xlen+sqrt((pl1x(i,j)-pl1x(i,j-1))**2+
     + (pl1y(i,j)-pl1y(i,j-1))**2)
       xlenp=xlen+sqrt((pl1x(i,j-1)-pl1x(i,j-2))**2+
     + (pl1y(i,j-1)-pl1y(i,j-2))**2)


       end do ! in contour


       end do ! in anchors



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Draw parabolic holes in right rib
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


       do klz=1,rib(i,15)-1

       xprb(i,klz,0)=(xanchor(i,klz)+xanchor(i,klz+1))/2.
       yprb(i,klz,0)=(yanchor(i,klz)+yanchor(i,klz+1))/2.
       jconi(klz)=1
       jconf(klz)=120

       do j=1,120

c      Point 5
       if (pr1y(i,j).le.yprb(i,klz,0).and.pr1y(i,j+1).gt.
     + yprb(i,klz,0)) then

       xm=(pr1y(i,j+1)-pr1y(i,j))/(pr1x(i,j+1)-pr1x(i,j))
       xb=pr1y(i,j)-xm*pr1x(i,j)

       yprb(i,klz,5)=yprb(i,klz,0)
       xprb(i,klz,5)=(yprb(i,klz,5)-xb)/xm

c       call line(psep+xprb(i,klz,5),psey+yprb(i,klz,5),
c     + psep+xprb(i,klz,5)+30,psey+yprb(i,klz,5),2)

       
c      Point 6
       dist05=-xprb(i,klz,0)+xprb(i,klz,5)
       dist06=dist05*hvr(k,9)/100.
       yprb(i,klz,6)=yprb(i,klz,0)
       xprb(i,klz,6)=xprb(i,klz,0)+dist06
       end if

c      Point 1

       if (pl1y(i,j).le.yanchor(i,klz)+hvr(k,10).and.pl1y(i,j+1).gt.
     + yanchor(i,klz)+hvr(k,10)) then

       xm=(pl1y(i,j+1)-pl1y(i,j))/(pl1x(i,j+1)-pl1x(i,j))
       xb=pl1y(i,j)-xm*pl1x(i,j)

       yprb(i,klz,1)=yanchor(i,klz)+hvr(k,10)
       xprb(i,klz,1)=(yprb(i,klz,1)-xb)/xm
       end if

       end do ! end j

       end do ! end klz


       do klz=1,rib(i+1,15)-1

       do j=1,120

c      Point 2 Intersection right side parabola with airfoil contour

       if (pl1y(i,j).ge.yprb(i,klz,0).and.pl1y(i,j).lt.
     + yanchor(i,klz+1)) then

c      Line "r" (parabola)

       xkprb(klz)=(xprb(i,klz,1)-xprb(i,klz,6))/
     + ((yprb(i,klz,1)-yprb(i,klz,6))**2.)

       xru(1)=xkprb(klz)*((pl1y(i,j)-yprb(i,klz,6))**2.)+
     + xprb(i,klz,6)
       xrv(1)=pl1y(i,j)
       xru(2)=xkprb(klz)*((pl1y(i,j+1)-yprb(i,klz,6))**2.)+
     + xprb(i,klz,6)
       xrv(2)=pl1y(i,j+1)

c      Line "s" (bottom surface)

       xsu(1)=pl1x(i,j)
       xsv(1)=pl1y(i,j)
       xsu(2)=pl1x(i,j+1)
       xsv(2)=pl1y(i,j+1)

       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)


       if (xsv(1).le.xtv.and.xsv(2).gt.xtv) then  !!REVISAR
       yprb(i,klz,2)=xtv
       xprb(i,klz,2)=xtu
       end if  


       end if   

       end do ! in j

      
c       call line(psep+xprb(i,klz,0),psey+yprb(i,klz,0),
c     + psep+xprb(i,klz,6),psey+yprb(i,klz,6),2)

c       call line(psep+xprb(i,klz,1),psey+yprb(i,klz,1),
c     + psep+xprb(i,klz,6),psey+yprb(i,klz,6),3)

c       call line(psep+xprb(i,klz,2),psey+yprb(i,klz,2),
c     + psep+xprb(i,klz,6),psey+yprb(i,klz,6),1)

c      Draw parabolas


c      Detect extremal points
       do j=1,120

       if (pl1y(i,j).le.yprb(i,klz,1).and.pl1y(i,j+1).gt.
     + yprb(i,klz,1)) then
       jconi(klz)=j+1
       end if
       if (pl1y(i,j).le.yprb(i,klz,2).and.pl1y(i,j+1).gt.
     + yprb(i,klz,2)) then
       jconf(klz)=j
       end if

       end do

c      Draw parabolas

       if (hvr(k,9).le.100.) then

       j=jconi(klz)
       xpa1=xkprb(klz)*((pl1y(i,j)-yprb(i,klz,6))**2.)+
     + xprb(i,klz,6)
       call line(psep+xprb(i,klz,1),psey+yprb(i,klz,1),
     + psep+xpa1,psey+pl1y(i,j),1)
       do j=jconi(klz),jconf(klz)-1
       xpa1=xkprb(klz)*((pl1y(i,j)-yprb(i,klz,6))**2.)+
     + xprb(i,klz,6)
       xpa2=xkprb(klz)*((pl1y(i,j+1)-yprb(i,klz,6))**2.)+
     + xprb(i,klz,6)
       call line(psep+xpa1,psey+pl1y(i,j),
     + psep+xpa2,psey+pl1y(i,j+1),1)
       end do
       j=jconf(klz)
       xpa1=xkprb(klz)*((pl1y(i,j)-yprb(i,klz,6))**2.)+
     + xprb(i,klz,6)
       call line(psep+xprb(i,klz,2),psey+yprb(i,klz,2),
     + psep+xpa1,psey+pl1y(i,j),1)

       end if


c      Draw ellipses

       if (hvr(k,9).gt.100) then

       xprb(i,klz,7)=(xprb(i,klz,5)+xprb(i,klz,0))/2.
       yprb(i,klz,7)=yprb(i,klz,0)
       
       xa=0.5*((xprb(i,klz,5)-xprb(i,klz,0))*
     + (1.-(2.*(hvr(k,9)-100.)/100.)))
       xb=0.5*(yprb(i,klz,2)-yprb(i,klz,1)-2.*hvr(k,10))

       xgir=0.

       do j=1,100-2.*pi/100.

       xsu(1)=xprb(i,klz,7)+xa*dcos(xgir)
       xsv(1)=yprb(i,klz,7)+xb*dsin(xgir)
       xsu(2)=xprb(i,klz,7)+xa*dcos(xgir+2.*pi/100.)
       xsv(2)=yprb(i,klz,7)+xb*dsin(xgir+2.*pi/100.)

       call line(psep+xsu(1),psey+xsv(1),psep+xsu(2),psey+xsv(2),1)

       xgir=xgir+2.*pi/100.

       end do

       end if

       end do ! in klz

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Complete the left cutting edge around parabolic openings
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       hvr_openings=max(0,int(rib(i,15))-1)
       hvr_edge_start=1
       do klz=1,hvr_openings
       hvr_gap_start=max(hvr_edge_start,
     + max(1,min(120,jconi(klz)-1)))
       hvr_gap_end=max(hvr_gap_start,
     + min(121,jconf(klz)+1))

       do j=hvr_edge_start,hvr_gap_start-1
       call line(lvcx(i,j),lvcy(i,j),
     + lvcx(i,j+1),lvcy(i,j+1),1)
       end do

       x1=psep+xprb(i,klz,1)
       y1=psey+yprb(i,klz,1)
       x2=x1-xrib*0.1d0*dsin(alpll(hvr_gap_start))
       y2=y1-xrib*0.1d0*dcos(alpll(hvr_gap_start))
       call line(x1,y1,x2,y2,1)
       call line(x2,y2,lvcx(i,hvr_gap_start),
     + lvcy(i,hvr_gap_start),1)

       x1=psep+xprb(i,klz,2)
       y1=psey+yprb(i,klz,2)
       x2=x1-xrib*0.1d0*dsin(alpll(hvr_gap_end))
       y2=y1-xrib*0.1d0*dcos(alpll(hvr_gap_end))
       call line(x1,y1,x2,y2,1)
       call line(x2,y2,lvcx(i,hvr_gap_end),
     + lvcy(i,hvr_gap_end),1)
       hvr_edge_start=hvr_gap_end
       end do

       do j=hvr_edge_start,120
       call line(lvcx(i,j),lvcy(i,j),
     + lvcx(i,j+1),lvcy(i,j+1),1)
       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Draw right diagonal in 3D space
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (ii.eq.rib(i,15)) then
       do j=1,121
       call line3d(rx2(i,j,ii),ry2(i,j,ii),rz2(i,j,ii),
     + rx3(i,j,ii),ry3(i,j,ii),rz3(i,j,ii),1)
       call line3d(-rx2(i,j,ii),ry2(i,j,ii),rz2(i,j,ii),
     + -rx3(i,j,ii),ry3(i,j,ii),rz3(i,j,ii),1)
       end do
       end if

       end if ! Draw right diagonal

       end if  ! rigth


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.3.2.5.3
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Marks in main ribs (rib i, bottom surface) plott and MC
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       sepxx=700.*xkf
       sepyy=100.*xkf

c      Rib i (center)
       kx=int((float(i-1)/6.))
       ky=i-kx*6

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)

       do klz=1,3
      
       alpha=-datan((xtv9(i-1)-vcnt(i-1,1,2))/(xtu9(i-1)-ucnt(i-1,1,2)))
c       if (rib(i,149).le.0.1d0) then ! Verify if airfoil thicknees = 0
c       alpha=pi/2.0d0
c       end if
       xp22=xtu2(i)+(1-klz+xdes)*dcos(alpha)
       yp22=xtv2(i)-(1-klz+xdes)*dsin(alpha)
       xu=sepx+xp22
       xv=-sepy+yp22
       call pointg(xu,-xv,xcir,6) ! Box (1,2)
       call point(xu+2530.*xkf,-xv,6) ! Box (1,4)

       alpha=datan((xtv11(i-1)-vcnt(i-1,ii,4))/
     + (xtu11(i-1)-ucnt(i-1,ii,4)))
c       if (rib(i,149).le.0.1d0) then ! Verify if airfoil thicknees = 0
c       alpha=pi/2.0d0
c       end if
       xp44=xtu4(i)-(1-klz+xdes)*dcos(alpha)
       yp44=xtv4(i)-(1-klz+xdes)*dsin(alpha)
       xu=sepx+xp44
       xv=-sepy+yp44
       call pointg(xu,-xv,xcir,6) ! Box (1,2)
       call point(xu+2530.*xkf,-xv,6) ! Box (1,4)

       end do

       if (ii.eq.rib(i,15)) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Full V-ribs marks in ribs plotting and MC
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Box (1,2)

       sepxx=700.*xkf
       sepyy=100.*xkf

c      Rib i-1 Left side
       kx=int((float(i-2)/6.))
       ky=i-1-kx*6

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)       

       if (hvr(k,5).eq.1) then
       call line(sepx+xtu9(i-1),-xtv9(i-1)+sepy,
     + sepx+ucnt(i-1,1,2),-vcnt(i-1,1,2)+sepy,5)
       call line(sepx+xtu11(i-1),-xtv11(i-1)+sepy,
     + sepx+ucnt(i-1,ii,4),-vcnt(i-1,ii,4)+sepy,5)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Marks 9-10 in V-rib left
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do klz=1,3
      
       alpha=-datan((xtv9(i-1)-vcnt(i-1,1,2))/(xtu9(i-1)-ucnt(i-1,1,2)))
       xp9=xtu9(i-1)-(1-klz+xdes)*dcos(alpha)
       yp9=xtv9(i-1)+(1-klz+xdes)*dsin(alpha)
       xu=sepx+xp9
       xv=-sepy+yp9
       call pointg(xu,-xv,xcir,6) ! Box (1,2)
       call point(xu+2530.*xkf,-xv,6) ! Box (1,4)

       alpha=datan((xtv11(i-1)-vcnt(i-1,ii,4))/
     + (xtu11(i-1)-ucnt(i-1,ii,4)))
       xp11=xtu11(i-1)+(1-klz+xdes)*dcos(alpha)
       yp11=xtv11(i-1)+(1-klz+xdes)*dsin(alpha)
       xu=sepx+xp11
       xv=-sepy+yp11
       call pointg(xu,-xv,xcir,6) ! Box (1,2)
       call point(xu+2530.*xkf,-xv,6) ! Box (1,4)

       end do

       end if  ! Left side


c      Rib i+1 Right side

       kx=int((float(i)/6.))
       ky=i+1-kx*6

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)

       if (hvr(k,6).eq.1) then
       call line(sepx+xtu9(i+1),-xtv9(i+1)+sepy,
     + sepx+ucnt(i+1,1,2),-vcnt(i+1,1,2)+sepy,1)
       call line(sepx+xtu11(i+1),-xtv11(i+1)+sepy,
     + sepx+ucnt(i+1,ii,4),-vcnt(i+1,ii,4)+sepy,1)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Marks 9-11 in V-rib right
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do klz=1,3 ! Tree points
      
       alpha=-datan((xtv9(i+1)-vcnt(i+1,1,2))/(xtu9(i+1)-ucnt(i+1,1,2)))
       xp9=xtu9(i+1)-(1-klz+xdes)*dcos(alpha)
       yp9=xtv9(i+1)+(1-klz+xdes)*dsin(alpha)
       xu=sepx+xp9
       xv=-sepy+yp9
       call pointg(xu,-xv,xcir,6)
       call point(xu+2530.*xkf,-xv,6) ! Box (1,4)

       alpha=datan((xtv11(i+1)-vcnt(i+1,ii,4))/
     + (xtu11(i+1)-ucnt(i+1,ii,4)))
       xp11=xtu11(i+1)+(1-klz+xdes)*dcos(alpha)
       yp11=xtv11(i+1)+(1-klz+xdes)*dsin(alpha)
       xu=sepx+xp11
       xv=-sepy+yp11
       call pointg(xu,-xv,xcir,6)
       call point(xu+2530.*xkf,-xv,6) ! Box (1,4)

       end do

       end if  ! Right side

       end if  ! rib(i,15)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       end if ! ii=rib(i,15)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       end if ! end type 5

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

 



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.4 HV ribs Type 4
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (hvr(k,2).eq.4) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.4.1 Rib i-1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)-1
       ii=hvr(k,4) ! row

       ucnt(i,ii,3)=u(i,ii,6)
       ucnt(i,ii,1)=ucnt(i,ii,3)-hvr(k,8)
       ucnt(i,ii,2)=ucnt(i,ii,3)-(hvr(k,7)+hvr(k,15))
       ucnt(i,ii,4)=ucnt(i,ii,3)+(hvr(k,7)+hvr(k,15))
       ucnt(i,ii,5)=ucnt(i,ii,3)+hvr(k,8)
       ucnt(i,ii,6)=ucnt(i,ii,1)
       ucnt(i,ii,7)=ucnt(i,ii,3)
       ucnt(i,ii,8)=ucnt(i,ii,5)
       ucnt(i,ii,9)=ucnt(i,ii,1)
       ucnt(i,ii,10)=ucnt(i,ii,3)
       ucnt(i,ii,11)=ucnt(i,ii,5)


c      Points 2,3,4 interpolation in rib i-1
       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,2).and.u(i,j+1,3).ge.ucnt(i,ii,2)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,2)=xm*ucnt(i,ii,2)+xb
       jcon(i,ii,2)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       jcon(i,ii,3)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,4).and.u(i,j+1,3).ge.ucnt(i,ii,4)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,4)=xm*ucnt(i,ii,4)+xb
       jcon(i,ii,4)=j
       end if

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat line 2-3-4 in n regular spaces   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       uinc=0.
       vinc=0.

       do j=1,21
       ucnt2(i,ii,j)=ucnt(i,ii,2)+uinc
       uinc=uinc+(ucnt(i,ii,4)-ucnt(i,ii,2))/20.

c      Between 2 and jcon(i,ii,2)+1
       if (ucnt2(i,ii,j).le.u(i,jcon(i,ii,2)+1,3)) then
       xm=(v(i,jcon(i,ii,2)+1,3)-vcnt(i,ii,2))/(u(i,jcon(i,ii,2)+1,3)-
     + ucnt(i,ii,2))
       xb=vcnt(i,ii,2)-xm*ucnt(i,ii,2)
       vcnt2(i,ii,j)=xm*ucnt2(i,ii,j)+xb
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Between jcon(i,ii,2)+1 and jcon(i,ii,4)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (ucnt2(i,ii,j).ge.u(i,jcon(i,ii,2)+1,3).and.ucnt2(i,ii,j)
     + .le.u(i,jcon(i,ii,4),3)) then
c      
       do l=jcon(i,ii,2)+1,jcon(i,ii,4)-1

c      Seleccionar tram d'interpolació

       if (ucnt2(i,ii,j).ge.u(i,l,3).and.ucnt2(i,ii,j).le.u(i,l+1,3)) 
     + then
       xm=(v(i,l+1,3)-v(i,l,3))/(u(i,l+1,3)-u(i,l,3))
       xb=v(i,l,3)-xm*u(i,l,3)
       end if
       end do
c       xm=(v(i,jcon(i,ii,2)+j,3)-v(i,jcon(i,ii,2)+j-1,3))/
c     + (u(i,jcon(i,ii,2)+j,3)-u(i,jcon(i,ii,2)+j-1,3))
c       xb=v(i,jcon(i,ii,2)+j-1,3)-xm*u(i,jcon(i,ii,2)+j-1,3)
       vcnt2(i,ii,j)=xm*ucnt2(i,ii,j)+xb
       end if

c      Between jcon(i,ii,4) and 4       
       if (ucnt2(i,ii,j).gt.u(i,jcon(i,ii,4),3)) then
       xm=(vcnt(i,ii,4)-v(i,jcon(i,ii,4),3))/(ucnt(i,ii,4)-
     + u(i,jcon(i,ii,4),3))
       xb=vcnt(i,ii,4)-xm*ucnt(i,ii,4)
       vcnt2(i,ii,j)=xm*ucnt2(i,ii,j)+xb
       end if

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.4.2 Rib i
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)
       ii=hvr(k,4)

       ucnt(i,ii,3)=u(i,ii,6)
       ucnt(i,ii,1)=ucnt(i,ii,3)-hvr(k,8)
       ucnt(i,ii,2)=ucnt(i,ii,3)-hvr(k,7)
       ucnt(i,ii,4)=ucnt(i,ii,3)+hvr(k,7)
       ucnt(i,ii,5)=ucnt(i,ii,3)+hvr(k,8)
       ucnt(i,ii,6)=ucnt(i,ii,1)
       ucnt(i,ii,7)=ucnt(i,ii,3)
       ucnt(i,ii,8)=ucnt(i,ii,5)
       ucnt(i,ii,9)=ucnt(i,ii,1)
       ucnt(i,ii,10)=ucnt(i,ii,3)
       ucnt(i,ii,11)=ucnt(i,ii,5)

c      Points 1,3,5 interpolation in rib i
       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,1).and.u(i,j+1,3).ge.ucnt(i,ii,1)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,1)=xm*ucnt(i,ii,1)+xb
       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       end if

       if (u(i,j,3).le.ucnt(i,ii,5).and.u(i,j+1,3).ge.ucnt(i,ii,5)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,5)=xm*ucnt(i,ii,5)+xb
       end if

       end do

c      Points 9,10,11 interpolation in rib i
       do j=1,np(i,2)

       if (u(i,j,3).ge.ucnt(i,ii,9).and.u(i,j+1,3).le.ucnt(i,ii,9)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,9)=xm*ucnt(i,ii,9)+xb
       end if

       if (u(i,j,3).ge.ucnt(i,ii,10).and.u(i,j+1,3).le.ucnt(i,ii,10)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,10)=xm*ucnt(i,ii,10)+xb
       end if

       if (u(i,j,3).ge.ucnt(i,ii,11).and.u(i,j+1,3).le.ucnt(i,ii,11)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,11)=xm*ucnt(i,ii,11)+xb
       end if

       end do

c      Calculus of 6,7,8 points in rib i
       vcnt(i,ii,6)=(vcnt(i,ii,9)-vcnt(i,ii,1))*(hvr(k,9)/100.)+
     + vcnt(i,ii,1)
       vcnt(i,ii,7)=(vcnt(i,ii,10)-vcnt(i,ii,3))*(hvr(k,9)/100.)+
     + vcnt(i,ii,3)
       vcnt(i,ii,8)=(vcnt(i,ii,11)-vcnt(i,ii,5))*(hvr(k,9)/100.)+
     + vcnt(i,ii,5)

c      Redefinition of points 6,8 if angle is not 90     
       if (hvr(k,10).ne.90.) then
       ucnt(i,ii,6)=ucnt(i,ii,7)-hvr(k,8)*dcos((pi/180.)*hvr(k,10))
       ucnt(i,ii,8)=ucnt(i,ii,7)+hvr(k,8)*dcos((pi/180.)*hvr(k,10))
       vcnt(i,ii,6)=vcnt(i,ii,7)-hvr(k,8)*dsin((pi/180.)*hvr(k,10))
       vcnt(i,ii,8)=vcnt(i,ii,7)+hvr(k,8)*dsin((pi/180.)*hvr(k,10))
       end if

c      Divide line 6-8 in n segments

       uinc=0.
       vinc=0.
       do j=1,21
       ucnt1(i,ii,j)=ucnt(i,ii,6)+uinc
       uinc=uinc+(ucnt(i,ii,8)-ucnt(i,ii,6))/20.
       vcnt1(i,ii,j)=vcnt(i,ii,6)+vinc
       vinc=vinc+(vcnt(i,ii,8)-vcnt(i,ii,6))/20.
       end do


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.4.3 Rib i+1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)+1
       ii=hvr(k,4)

       ucnt(i,ii,3)=u(i,ii,6)
       ucnt(i,ii,1)=ucnt(i,ii,3)-hvr(k,8)
       ucnt(i,ii,2)=ucnt(i,ii,3)-hvr(k,7)
       ucnt(i,ii,4)=ucnt(i,ii,3)+hvr(k,7)
       ucnt(i,ii,5)=ucnt(i,ii,3)+hvr(k,8)
       ucnt(i,ii,6)=ucnt(i,ii,1)
       ucnt(i,ii,7)=ucnt(i,ii,3)
       ucnt(i,ii,8)=ucnt(i,ii,5)
       ucnt(i,ii,9)=ucnt(i,ii,1)
       ucnt(i,ii,10)=ucnt(i,ii,3)
       ucnt(i,ii,11)=ucnt(i,ii,5)

c      Points 1,3,5 interpolation in rib i+1
       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,1).and.u(i,j+1,3).ge.ucnt(i,ii,1)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,1)=xm*ucnt(i,ii,1)+xb
       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       end if

       if (u(i,j,3).le.ucnt(i,ii,5).and.u(i,j+1,3).ge.ucnt(i,ii,5)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,5)=xm*ucnt(i,ii,5)+xb
       end if

       end do

c      Points 9,10,11 interpolation in rib i+1
       do j=1,np(i,2)

       if (u(i,j,3).ge.ucnt(i,ii,9).and.u(i,j+1,3).le.ucnt(i,ii,9)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,9)=xm*ucnt(i,ii,9)+xb
       end if

       if (u(i,j,3).ge.ucnt(i,ii,10).and.u(i,j+1,3).le.ucnt(i,ii,10)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,10)=xm*ucnt(i,ii,10)+xb
       end if

       if (u(i,j,3).ge.ucnt(i,ii,11).and.u(i,j+1,3).le.ucnt(i,ii,11)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,11)=xm*ucnt(i,ii,11)+xb
       end if

       end do

c      Calculus of 6,7,8 points in rib i+1
       vcnt(i,ii,6)=(vcnt(i,ii,9)-vcnt(i,ii,1))*(hvr(k,9)/100.)+
     + vcnt(i,ii,1)
       vcnt(i,ii,7)=(vcnt(i,ii,10)-vcnt(i,ii,3))*(hvr(k,9)/100.)+
     + vcnt(i,ii,3)
       vcnt(i,ii,8)=(vcnt(i,ii,11)-vcnt(i,ii,5))*(hvr(k,9)/100.)+
     + vcnt(i,ii,5)

c      Redefinition of points 7,8 if angle is not 90     
       if (hvr(k,10).ne.90.) then
       ucnt(i,ii,6)=ucnt(i,ii,7)-hvr(k,8)*dcos((pi/180.)*hvr(k,10))
       ucnt(i,ii,8)=ucnt(i,ii,7)+hvr(k,8)*dcos((pi/180.)*hvr(k,10))
       vcnt(i,ii,6)=vcnt(i,ii,7)-hvr(k,8)*dsin((pi/180.)*hvr(k,10))
       vcnt(i,ii,8)=vcnt(i,ii,7)+hvr(k,8)*dsin((pi/180.)*hvr(k,10))
       end if

c      Divide line 6-8 in n segments

       uinc=0.
       vinc=0.
       do j=1,21
       ucnt3(i,ii,j)=ucnt(i,ii,6)+uinc
       uinc=uinc+(ucnt(i,ii,8)-ucnt(i,ii,6))/20.
       vcnt3(i,ii,j)=vcnt(i,ii,6)+vinc
       vinc=vinc+(vcnt(i,ii,8)-vcnt(i,ii,6))/20.
       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.4.4 Rib i+2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)+2
       ii=hvr(k,4)

       ucnt(i,ii,3)=u(i,ii,6)
       ucnt(i,ii,1)=ucnt(i,ii,3)-hvr(k,8)
       ucnt(i,ii,2)=ucnt(i,ii,3)-(hvr(k,7)+hvr(k,16))
       ucnt(i,ii,4)=ucnt(i,ii,3)+(hvr(k,7)+hvr(k,16))
       ucnt(i,ii,5)=ucnt(i,ii,3)+hvr(k,8)
       ucnt(i,ii,6)=ucnt(i,ii,1)
       ucnt(i,ii,7)=ucnt(i,ii,3)
       ucnt(i,ii,8)=ucnt(i,ii,5)
       ucnt(i,ii,9)=ucnt(i,ii,1)
       ucnt(i,ii,10)=ucnt(i,ii,3)
       ucnt(i,ii,11)=ucnt(i,ii,5)


c      Points 2,3,4 interpolation in rib i+2
       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,2).and.u(i,j+1,3).ge.ucnt(i,ii,2)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,2)=xm*ucnt(i,ii,2)+xb
       jcon(i,ii,2)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       jcon(i,ii,3)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,4).and.u(i,j+1,3).ge.ucnt(i,ii,4)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,4)=xm*ucnt(i,ii,4)+xb
       jcon(i,ii,4)=j
       end if

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat line 2-3-4 in n regular spaces   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       uinc=0.
       vinc=0.

       do j=1,21
       ucnt4(i,ii,j)=ucnt(i,ii,2)+uinc
       uinc=uinc+(ucnt(i,ii,4)-ucnt(i,ii,2))/20.

c      Between 2 and jcon(i,ii,2)+1
       if (ucnt4(i,ii,j).le.u(i,jcon(i,ii,2)+1,3)) then
       xm=(v(i,jcon(i,ii,2)+1,3)-vcnt(i,ii,2))/(u(i,jcon(i,ii,2)+1,3)-
     + ucnt(i,ii,2))
       xb=vcnt(i,ii,2)-xm*ucnt(i,ii,2)
       vcnt4(i,ii,j)=xm*ucnt4(i,ii,j)+xb
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Between jcon(i,ii,2)+1 and jcon(i,ii,4)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (ucnt4(i,ii,j).ge.u(i,jcon(i,ii,2)+1,3).and.ucnt4(i,ii,j)
     + .le.u(i,jcon(i,ii,4),3)) then
c      
       do l=jcon(i,ii,2)+1,jcon(i,ii,4)-1

c      Seleccionar tram d'interpolació

       if (ucnt4(i,ii,j).ge.u(i,l,3).and.ucnt4(i,ii,j).le.u(i,l+1,3)) 
     + then
       xm=(v(i,l+1,3)-v(i,l,3))/(u(i,l+1,3)-u(i,l,3))
       xb=v(i,l,3)-xm*u(i,l,3)
       end if
       end do
c       xm=(v(i,jcon(i,ii,2)+j,3)-v(i,jcon(i,ii,2)+j-1,3))/
c     + (u(i,jcon(i,ii,2)+j,3)-u(i,jcon(i,ii,2)+j-1,3))
c       xb=v(i,jcon(i,ii,2)+j-1,3)-xm*u(i,jcon(i,ii,2)+j-1,3)
       vcnt4(i,ii,j)=xm*ucnt4(i,ii,j)+xb
       end if

c      Between jcon(i,ii,4) and 4       
       if (ucnt4(i,ii,j).gt.u(i,jcon(i,ii,4),3)) then
       xm=(vcnt(i,ii,4)-v(i,jcon(i,ii,4),3))/(ucnt(i,ii,4)-
     + u(i,jcon(i,ii,4),3))
       xb=vcnt(i,ii,4)-xm*ucnt(i,ii,4)
       vcnt4(i,ii,j)=xm*ucnt4(i,ii,j)+xb
       end if

       end do


c      Rib localisation
       i=hvr(k,3)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.4.4 VH-ribs lines 1 2 3 4 transportation to 3D espace
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Rib i (Line 1)

       i=hvr(k,3)

       tetha=rib(i,8)*pi/180.

       do j=1,21
       ru(i,j,3)=ucnt1(i,ii,j)
       rv(i,j,3)=vcnt1(i,ii,j)-rib(i,50)
       end do

       do j=1,21

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       sx1(i,j,ii)=rx(i,j)
       sy1(i,j,ii)=ry(i,j)
       sz1(i,j,ii)=rz(i,j)

       end do

c      Rib i-1 (Line 2)

       i=hvr(k,3)-1

       tetha=rib(i,8)*pi/180.
       
       do j=1,21
       ru(i,j,3)=ucnt2(i,ii,j)
       rv(i,j,3)=vcnt2(i,ii,j)-rib(i,50)
       end do

       do j=1,21

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       sx2(i,j,ii)=rx(i,j)
       sy2(i,j,ii)=ry(i,j)
       sz2(i,j,ii)=rz(i,j)

       end do

c      Rib i+1 (Line 3)

       i=hvr(k,3)+1

       tetha=rib(i,8)*pi/180.

       do j=1,21
       ru(i,j,3)=ucnt3(i,ii,j)
       rv(i,j,3)=vcnt3(i,ii,j)-rib(i,50)
c      COMPTE AMB el rib(i,50) A ESTUDIAR       
       end do

       do j=1,21

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       sx3(i,j,ii)=rx(i,j)
       sy3(i,j,ii)=ry(i,j)
       sz3(i,j,ii)=rz(i,j)

       end do

c      Rib i+2 (Line 4)

       i=hvr(k,3)+2

       tetha=rib(i,8)*pi/180.
       
       do j=1,21
       ru(i,j,3)=ucnt4(i,ii,j)
       rv(i,j,3)=vcnt4(i,ii,j)-rib(i,50)
       end do

       do j=1,21

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       sx4(i,j,ii)=rx(i,j)
       sy4(i,j,ii)=ry(i,j)
       sz4(i,j,ii)=rz(i,j)

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Drawing VH Type 4 in 3D
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


       do j=1,21

c      Rib i-1 to i+2

       i=hvr(k,3)

       if (hvr(k,5).eq.1) then
       call line3d(sx2(i-1,j,ii),sy2(i-1,j,ii),sz2(i-1,j,ii),
     + sx1(i,j,ii),sy1(i,j,ii),sz1(i,j,ii),3)
       call line3d(-sx2(i-1,j,ii),sy2(i-1,j,ii),sz2(i-1,j,ii),
     + -sx1(i,j,ii),sy1(i,j,ii),sz1(i,j,ii),3)
       end if

       call line3d(sx1(i,j,ii),sy1(i,j,ii),sz1(i,j,ii),
     + sx3(i+1,j,ii),sy3(i+1,j,ii),sz3(i+1,j,ii),2)
       call line3d(-sx1(i,j,ii),sy1(i,j,ii),sz1(i,j,ii),
     + -sx3(i+1,j,ii),sy3(i+1,j,ii),sz3(i+1,j,ii),2)

       if (hvr(k,6).eq.1) then
       call line3d(sx3(i+1,j,ii),sy3(i+1,j,ii),sz3(i+1,j,ii),
     + sx4(i+2,j,ii),sy4(i+2,j,ii),sz4(i+2,j,ii),1)
       call line3d(-sx3(i+1,j,ii),sy3(i+1,j,ii),sz3(i+1,j,ii),
     + -sx4(i+2,j,ii),sy4(i+2,j,ii),sz4(i+2,j,ii),1)
       end if

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.4.5 VH-ribs calculus and drawing in 2D and 3D
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      VH-rib 1-2 in 2D model
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)-1 ! color green

       px0=0.
       py0=0.
       ptheta=0.

       do j=1,21

c      Distances between points
       pa=dsqrt((rx(i+1,j)-rx(i,j))**2.+(ry(i+1,j)-ry(i,j))**2.+
     + (rz(i+1,j)-rz(i,j))**2.)
       pb=dsqrt((rx(i+1,j+1)-rx(i,j))**2.+(ry(i+1,j+1)-ry(i,j))**2.+
     + (rz(i+1,j+1)-rz(i,j))**2.)
       pc=dsqrt((rx(i+1,j+1)-rx(i+1,j))**2.+(ry(i+1,j+1)-ry(i+1,j))**2.+
     + (rz(i+1,j+1)-rz(i+1,j))**2.)
       pd=dsqrt((rx(i+1,j)-rx(i,j+1))**2.+(ry(i+1,j)-ry(i,j+1))**2.+
     + (rz(i+1,j)-rz(i,j+1))**2.)
       pe=dsqrt((rx(i,j+1)-rx(i,j))**2.+(ry(i,j+1)-ry(i,j))**2.+
     + (rz(i,j+1)-rz(i,j))**2.)
       pf=dsqrt((rx(i+1,j+1)-rx(i,j+1))**2.+(ry(i+1,j+1)-ry(i,j+1))**2.+
     + (rz(i+1,j+1)-rz(i,j+1))**2.)
       
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

c      Drawing in 2D model
       
       psep=3300.*xkf+xrsep*float(i)
       psey=800.*xkf+yrsep*float(ii)

       if (hvr(k,5).eq.1) then

       j=1

       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),3)

       j=21

       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),3)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Marca punts MC a l'esquerra
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       alpha=-(datan((pl1y(i,1)-pl2y(i,20))/(pl1x(i,1)-pl2x(i,20))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xp6=pl1x(i,1)-xdes*dsin(alpha)-2.*xdes*dcos(alpha)
       yp6=pl1y(i,1)-xdes*dcos(alpha)+2.*xdes*dsin(alpha)
       xp8=pl1x(i,21)-xdes*dsin(alpha)+2.*xdes*dcos(alpha)
       yp8=pl1y(i,21)-xdes*dcos(alpha)-2.*xdes*dsin(alpha)
c       xp7=0.5*(pl1x(i,1)+pl1x(i,21))-xdes*dsin(alpha)
c       yp7=0.5*(pl1y(i,1)+pl1y(i,21))-xdes*dcos(alpha)

       call point(psep+xp6,psey+yp6,1)
       call point(psep+xp8,psey+yp8,1)

c      Romano costat esquerra
       sl=1.
       
       xpx=(pl1x(i,1)+pl2x(i,20))/2.-sl*xdes*dsin(alpha)
       xpy=(pl1y(i,1)+pl2y(i,20))/2.-sl*xdes*dcos(alpha)

       xpx2=psep+xpx+0.3*hvr(k,7)*dcos(alpha)-0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy-0.3*hvr(k,7)*dsin(alpha)-0.3*xvrib*dcos(alpha) 

       call romano(i,xpx2,xpy2,alpha,typm6(10)*0.1,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Marca punts MC a la dreta
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       alpha=-(datan((pr1y(i,1)-pr2y(i,20))/(pr1x(i,1)
     + -pr2x(i,20))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xp6=pr1x(i,1)+xdes*dsin(alpha)-2.*xdes*dcos(alpha)
       yp6=pr1y(i,1)+xdes*dcos(alpha)+2.*xdes*dsin(alpha)
       xp8=pr1x(i,21)+xdes*dsin(alpha)+2.*xdes*dcos(alpha)
       yp8=pr1y(i,21)+xdes*dcos(alpha)-2.*xdes*dsin(alpha)
       xp7=0.5*(xp6+xp8)
       yp7=0.5*(yp6+yp8)

       call point(psep+xp6,psey+yp6,1)
       call point(psep+xp8,psey+yp8,1)

c      Romano costat dret
       sr=1.
       
       xpx=(pr1x(i,1)+pr2x(i,20))/2.+xdes*dsin(alpha)
       xpy=(pr1y(i,1)+pr2y(i,20))/2.+xdes*dcos(alpha)

       xpx2=psep+xpx+0.5*hvr(k,8)*dcos(alpha)+0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy-0.5*hvr(k,8)*dsin(alpha)+0.3*xvrib*dcos(alpha) 

       call romano(i+1,xpx2,xpy2,alpha,typm6(10)*0.1,7)

       xpx2=psep+xpx-0.5*hvr(k,8)*dcos(alpha)+0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy+0.5*hvr(k,8)*dsin(alpha)+0.3*xvrib*dcos(alpha) 

       call romano(int(hvr(k,4)),xpx2,xpy2,alpha,typm6(10)*0.1,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Vores de costura
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do j=1,21-1

c       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pl2x(i,j),
c     + psey+pl2y(i,j),30)

c       call line(psep+pr1x(i,j),psey+pr1y(i,j),psep+pr2x(i,j),
c     + psey+pr2y(i,j),2)

c      Vores de costura esquerra
       alpl=abs(datan((pl2y(i,j)-pl1y(i,j))/(pl2x(i,j)-pl1x(i,j))))

       call line(psep+pl1x(i,j)-xvrib*dsin(alpl),psey+pl1y(i,j)
     + +xvrib*dcos(alpl),psep+pl2x(i,j)-xvrib*dsin(alpl),
     + psey+pl2y(i,j)+xvrib*dcos(alpl),3)

c      Vores de costura dreta
       alpr=abs(datan((pr2y(i,j)-pr1y(i,j))/(pr2x(i,j)-pr1x(i,j))))
c       alpr=-(datan((pr1y(i,j)-pr2y(i,j))/(pr1x(i,j)-pr2x(i,j))))
       if (alpr.lt.0.) then ! Revisar cases atès que hi ha qualque no OK!!!!!!!!!!!!!
       alpr=alpr+pi
       end if

       call line(psep+pr1x(i,j)+xvrib*dsin(alpr),psey+pr1y(i,j)
     + -xvrib*dcos(alpr),psep+pr2x(i,j)+xvrib*dsin(alpr),
     + psey+pr2y(i,j)-xvrib*dcos(alpr),3)

c      Tancament lateral inici
       if (j.eq.1) then
       call line(psep+pl1x(i,j)-xvrib*dsin(alpl),psey+pl1y(i,j)
     + +xvrib*dcos(alpl),psep+pl1x(i,j),psey+pl1y(i,j),3)
       call line(psep+pr1x(i,j)+xvrib*dsin(alpr),psey+pr1y(i,j)
     + -xvrib*dcos(alpr),psep+pr1x(i,j),psey+pr1y(i,j),3)
       end if

c      Tancament lateral fi
       if (j.eq.20) then
       call line(psep+pl2x(i,j)-xvrib*dsin(alpl),psey+pl2y(i,j)
     + +xvrib*dcos(alpl),psep+pl2x(i,j),psey+pl2y(i,j),3)
       call line(psep+pr2x(i,j)+xvrib*dsin(alpr),psey+pr2y(i,j)
     + -xvrib*dcos(alpr),psep+pr2x(i,j),psey+pr2y(i,j),3)

       lvcx(i,j+1)=psep+pl2x(i,j)-xvrib*dsin(alpl)
       lvcy(i,j+1)=psey+pl2y(i,j)-xvrib*dcos(alpl)

       rvcx(i,j+1)=psep+pr2x(i,j)+xvrib*dsin(alpr)
       rvcy(i,j+1)=psey+pr2y(i,j)+xvrib*dcos(alpr)

       end if

       
       end do ! J

c      V-rib length
       hvr(k,15)=dsqrt((lvcx(i,21)-rvcx(i,21))**2.+
     + (lvcy(i,21)-rvcy(i,21))**2.)

c      Numera cintes Type 4 V (i to i-1)
       call itxt(psep-0.-xrsep+83.*xkf-120.*(typm3(10)/10.),
     + psey-10,typm3(10),0.0d0,i,7)
       call itxt(psep+hvr(k,15)-xrsep+83.*xkf-120.*(typm3(10)/10.),
     + psey-10,typm3(10),0.0d0,i+1,7)


       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      VH-rib 3-4 in 2D model
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)+1 ! color red

       px0=0.0d0
       py0=0.0d0
       ptheta=0.0d0

       do j=1,21

c      Distances between points
       pa=dsqrt((rx(i+1,j)-rx(i,j))**2.+(ry(i+1,j)-ry(i,j))**2.+
     + (rz(i+1,j)-rz(i,j))**2.)
       pb=dsqrt((rx(i+1,j+1)-rx(i,j))**2.+(ry(i+1,j+1)-ry(i,j))**2.+
     + (rz(i+1,j+1)-rz(i,j))**2.)
       pc=dsqrt((rx(i+1,j+1)-rx(i+1,j))**2.+(ry(i+1,j+1)-ry(i+1,j))**2.+
     + (rz(i+1,j+1)-rz(i+1,j))**2.)
       pd=dsqrt((rx(i+1,j)-rx(i,j+1))**2.+(ry(i+1,j)-ry(i,j+1))**2.+
     + (rz(i+1,j)-rz(i,j+1))**2.)
       pe=dsqrt((rx(i,j+1)-rx(i,j))**2.+(ry(i,j+1)-ry(i,j))**2.+
     + (rz(i,j+1)-rz(i,j))**2.)
       pf=dsqrt((rx(i+1,j+1)-rx(i,j+1))**2.+(ry(i+1,j+1)-ry(i,j+1))**2.+
     + (rz(i+1,j+1)-rz(i,j+1))**2.)
       
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

c      Drawing in 2D model
       
       psep=3300.*xkf+xrsep*float(i)
       psey=800.*xkf+yrsep*float(ii)

       if (hvr(k,6).eq.1) then

       j=1

       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),1)

       j=21

       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),1)


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Marca punts MC a l'esquerra
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       alpha=-(datan((pl1y(i,1)-pl2y(i,20))/(pl1x(i,1)-pl2x(i,20))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xp6=pl1x(i,1)-xdes*dsin(alpha)-2.*xdes*dcos(alpha)
       yp6=pl1y(i,1)-xdes*dcos(alpha)+2.*xdes*dsin(alpha)
       xp8=pl1x(i,21)-xdes*dsin(alpha)+2.*xdes*dcos(alpha)
       yp8=pl1y(i,21)-xdes*dcos(alpha)-2.*xdes*dsin(alpha)
c       xp7=0.5*(pl1x(i,1)+pl1x(i,21))-xdes*dsin(alpha)
c       yp7=0.5*(pl1y(i,1)+pl1y(i,21))-xdes*dcos(alpha)

       call point(psep+xp6,psey+yp6,1)
       call point(psep+xp8,psey+yp8,1)

c      Romano costat esquerra
       sl=1.
       
       xpx=(pl1x(i,1)+pl2x(i,20))/2.-sl*xdes*dsin(alpha)
       xpy=(pl1y(i,1)+pl2y(i,20))/2.-sl*xdes*dcos(alpha)

       xpx2=psep+xpx+0.3*hvr(k,7)*dcos(alpha)-0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy-0.3*hvr(k,7)*dsin(alpha)-0.3*xvrib*dcos(alpha) 

       call romano(i,xpx2,xpy2,alpha,typm6(10)*0.1,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Marca punts MC a la dreta
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       alpha=-(datan((pr1y(i,1)-pr2y(i,20))/(pr1x(i,1)
     + -pr2x(i,20))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xp6=pr1x(i,1)+xdes*dsin(alpha)-2.*xdes*dcos(alpha)
       yp6=pr1y(i,1)+xdes*dcos(alpha)+2.*xdes*dsin(alpha)
       xp8=pr1x(i,21)+xdes*dsin(alpha)+2.*xdes*dcos(alpha)
       yp8=pr1y(i,21)+xdes*dcos(alpha)-2.*xdes*dsin(alpha)
       xp7=0.5*(xp6+xp8)
       yp7=0.5*(yp6+yp8)

       call point(psep+xp6,psey+yp6,1)
       call point(psep+xp8,psey+yp8,1)

c      Romano costat dret
       sr=1.
       
       xpx=(pr1x(i,1)+pr2x(i,20))/2.+xdes*dsin(alpha)
       xpy=(pr1y(i,1)+pr2y(i,20))/2.+xdes*dcos(alpha)

       xpx2=psep+xpx+0.2*hvr(k,8)*dcos(alpha)+0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy-0.2*hvr(k,8)*dsin(alpha)+0.3*xvrib*dcos(alpha) 

       call romano(i+1,xpx2,xpy2,alpha,typm6(10)*0.1,7)

       xpx2=psep+xpx-0.2*hvr(k,8)*dcos(alpha)+0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy+0.2*hvr(k,8)*dsin(alpha)+0.3*xvrib*dcos(alpha) 

       call romano(int(hvr(k,4)),xpx2,xpy2,alpha,typm6(10)*0.1,7)


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Vores de costura
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do j=1,21-1

c       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pl2x(i,j),
c     + psey+pl2y(i,j),1)

c       call line(psep+pr1x(i,j),psey+pr1y(i,j),psep+pr2x(i,j),
c     + psey+pr2y(i,j),1)

c      Vores de costura esquerra
       alpl=abs(datan((pl2y(i,j)-pl1y(i,j))/(pl2x(i,j)-pl1x(i,j))))

       call line(psep+pl1x(i,j)-xvrib*dsin(alpl),psey+pl1y(i,j)
     + +xvrib*dcos(alpl),psep+pl2x(i,j)-xvrib*dsin(alpl),
     + psey+pl2y(i,j)+xvrib*dcos(alpl),1)

c      Vores de costura dreta
       alpr=abs(datan((pr2y(i,j)-pr1y(i,j))/(pr2x(i,j)-pr1x(i,j))))

       call line(psep+pr1x(i,j)+xvrib*dsin(alpr),psey+pr1y(i,j)
     + -xvrib*dcos(alpr),psep+pr2x(i,j)+xvrib*dsin(alpr),
     + psey+pr2y(i,j)-xvrib*dcos(alpr),1)

c      Tancament lateral inici
       if (j.eq.1) then
       call line(psep+pl1x(i,j)-xvrib*dsin(alpl),psey+pl1y(i,j)
     + +xvrib*dcos(alpl),psep+pl1x(i,j),psey+pl1y(i,j),1)
       call line(psep+pr1x(i,j)+xvrib*dsin(alpr),psey+pr1y(i,j)
     + -xvrib*dcos(alpr),psep+pr1x(i,j),psey+pr1y(i,j),1)
       end if

c      Tancament lateral fi
       if (j.eq.20) then
       call line(psep+pl2x(i,j)-xvrib*dsin(alpl),psey+pl2y(i,j)
     + +xvrib*dcos(alpl),psep+pl2x(i,j),psey+pl2y(i,j),1)
       call line(psep+pr2x(i,j)+xvrib*dsin(alpr),psey+pr2y(i,j)
     + -xvrib*dcos(alpr),psep+pr2x(i,j),psey+pr2y(i,j),1)

       lvcx(i,j+1)=psep+pl2x(i,j)-xvrib*dsin(alpl)
       lvcy(i,j+1)=psey+pl2y(i,j)-xvrib*dcos(alpl)

       rvcx(i,j+1)=psep+pr2x(i,j)+xvrib*dsin(alpr)
       rvcy(i,j+1)=psey+pr2y(i,j)+xvrib*dcos(alpr)

       end if


       end do ! j

c      V-rib length
       hvr(k,15)=dsqrt((lvcx(i,21)-rvcx(i,21))**2.+
     + (lvcy(i,21)-rvcy(i,21))**2.)

c      REVISAR PRESENTACIO

c      Numera cintes V TYpe 4 i+1 to i+2
       call itxt(psep-0.-xrsep+83.*xkf-120.*(typm3(10)/10.),
     + psey-10,typm3(10),0.0d0,i,7)
       call itxt(psep+hvr(k,15)-xrsep+83.*xkf-120.*(typm3(10)/10.),
     + psey-10,typm3(10),0.0d0,i+1,7)

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      VH-rib 1-3 in 2D model
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3) ! color yellow

       px0=0.
       py0=0.
       ptheta=0.

       do j=1,21

c      Distances between points
       pa=dsqrt((rx(i+1,j)-rx(i,j))**2.+(ry(i+1,j)-ry(i,j))**2.+
     + (rz(i+1,j)-rz(i,j))**2.)
       pb=dsqrt((rx(i+1,j+1)-rx(i,j))**2.+(ry(i+1,j+1)-ry(i,j))**2.+
     + (rz(i+1,j+1)-rz(i,j))**2.)
       pc=dsqrt((rx(i+1,j+1)-rx(i+1,j))**2.+(ry(i+1,j+1)-ry(i+1,j))**2.+
     + (rz(i+1,j+1)-rz(i+1,j))**2.)
       pd=dsqrt((rx(i+1,j)-rx(i,j+1))**2.+(ry(i+1,j)-ry(i,j+1))**2.+
     + (rz(i+1,j)-rz(i,j+1))**2.)
       pe=dsqrt((rx(i,j+1)-rx(i,j))**2.+(ry(i,j+1)-ry(i,j))**2.+
     + (rz(i,j+1)-rz(i,j))**2.)
       pf=dsqrt((rx(i+1,j+1)-rx(i,j+1))**2.+(ry(i+1,j+1)-ry(i,j+1))**2.+
     + (rz(i+1,j+1)-rz(i,j+1))**2.0d0)
       
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

c      Drawing in 2D model
       
       psep=3300.*xkf+xrsep*float(i)
       psey=800.*xkf+yrsep*float(ii)

       j=1

       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),30)

       j=21

       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),30)


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Marca punts MC a l'esquerra
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       alpha=-(datan((pl1y(i,1)-pl2y(i,20))/(pl1x(i,1)-pl2x(i,20))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xp6=pl1x(i,1)-xdes*dsin(alpha)-2.*xdes*dcos(alpha)
       yp6=pl1y(i,1)-xdes*dcos(alpha)+2.*xdes*dsin(alpha)
       xp8=pl1x(i,21)-xdes*dsin(alpha)+2.*xdes*dcos(alpha)
       yp8=pl1y(i,21)-xdes*dcos(alpha)-2.*xdes*dsin(alpha)
c       xp7=0.5*(pl1x(i,1)+pl1x(i,21))-xdes*dsin(alpha)
c       yp7=0.5*(pl1y(i,1)+pl1y(i,21))-xdes*dcos(alpha)

       call point(psep+xp6,psey+yp6,1)
       call point(psep+xp8,psey+yp8,1)

c      Romano costat esquerra
       sl=1.
       
       xpx=(pl1x(i,1)+pl2x(i,20))/2.-sl*xdes*dsin(alpha)
       xpy=(pl1y(i,1)+pl2y(i,20))/2.-sl*xdes*dcos(alpha)

       xpx2=psep+xpx+0.3*hvr(k,7)*dcos(alpha)-0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy-0.3*hvr(k,7)*dsin(alpha)-0.3*xvrib*dcos(alpha) 

       call romano(i,xpx2,xpy2,alpha,typm6(10)*0.1,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Marca punts MC a la dreta
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       alpha=-(datan((pr1y(i,1)-pr2y(i,20))/(pr1x(i,1)
     + -pr2x(i,20))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xp6=pr1x(i,1)+xdes*dsin(alpha)-2.*xdes*dcos(alpha)
       yp6=pr1y(i,1)+xdes*dcos(alpha)+2.*xdes*dsin(alpha)
       xp8=pr1x(i,21)+xdes*dsin(alpha)+2.*xdes*dcos(alpha)
       yp8=pr1y(i,21)+xdes*dcos(alpha)-2.*xdes*dsin(alpha)
       xp7=0.5*(xp6+xp8)
       yp7=0.5*(yp6+yp8)

       call point(psep+xp6,psey+yp6,1)
       call point(psep+xp8,psey+yp8,1)

c      Romano costat dret
       sr=1.
       
       xpx=(pr1x(i,1)+pr2x(i,20))/2.+xdes*dsin(alpha)
       xpy=(pr1y(i,1)+pr2y(i,20))/2.+xdes*dcos(alpha)

       xpx2=psep+xpx+0.5*hvr(k,8)*dcos(alpha)+0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy-0.5*hvr(k,8)*dsin(alpha)+0.3*xvrib*dcos(alpha) 

       call romano(i+1,xpx2,xpy2,alpha,typm6(10)*0.1,7)

       xpx2=psep+xpx-0.5*hvr(k,8)*dcos(alpha)+0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy+0.5*hvr(k,8)*dsin(alpha)+0.3*xvrib*dcos(alpha) 

       call romano(int(hvr(k,4)),xpx2,xpy2,alpha,typm6(10)*0.1,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Vores de costura
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do j=1,21-1

c       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pl2x(i,j),
c     + psey+pl2y(i,j),1)

c       call line(psep+pr1x(i,j),psey+pr1y(i,j),psep+pr2x(i,j),
c     + psey+pr2y(i,j),1)


c      Vores de costura esquerra
       alpl=abs(datan((pl2y(i,j)-pl1y(i,j))/(pl2x(i,j)-pl1x(i,j))))

       call line(psep+pl1x(i,j)-xvrib*dsin(alpl),psey+pl1y(i,j)
     + +xvrib*dcos(alpl),psep+pl2x(i,j)-xvrib*dsin(alpl),
     + psey+pl2y(i,j)+xvrib*dcos(alpl),30)

c      Vores de costura dreta
       alpr=abs(datan((pr2y(i,j)-pr1y(i,j))/(pr2x(i,j)-pr1x(i,j))))

       call line(psep+pr1x(i,j)+xvrib*dsin(alpr),psey+pr1y(i,j)
     + -xvrib*dcos(alpr),psep+pr2x(i,j)+xvrib*dsin(alpr),
     + psey+pr2y(i,j)-xvrib*dcos(alpr),30)

c      Tancament lateral inici
       if (j.eq.1) then
       call line(psep+pl1x(i,j)-xvrib*dsin(alpl),psey+pl1y(i,j)
     + +xvrib*dcos(alpl),psep+pl1x(i,j),psey+pl1y(i,j),30)
       call line(psep+pr1x(i,j)+xvrib*dsin(alpr),psey+pr1y(i,j)
     + -xvrib*dcos(alpr),psep+pr1x(i,j),psey+pr1y(i,j),30)
       end if

c      Tancament lateral fi
       if (j.eq.20) then
       call line(psep+pl2x(i,j)-xvrib*dsin(alpl),psey+pl2y(i,j)
     + +xvrib*dcos(alpl),psep+pl2x(i,j),psey+pl2y(i,j),30)
       call line(psep+pr2x(i,j)+xvrib*dsin(alpr),psey+pr2y(i,j)
     + -xvrib*dcos(alpr),psep+pr2x(i,j),psey+pr2y(i,j),30)

       lvcx(i,j+1)=psep+pl2x(i,j)-xvrib*dsin(alpl)
       lvcy(i,j+1)=psey+pl2y(i,j)-xvrib*dcos(alpl)

       rvcx(i,j+1)=psep+pr2x(i,j)+xvrib*dsin(alpr)
       rvcy(i,j+1)=psey+pr2y(i,j)+xvrib*dcos(alpr)

       end if

       end do

c      V-rib length
       hvr(k,15)=dsqrt((lvcx(i,21)-rvcx(i,21))**2.+
     + (lvcy(i,21)-rvcy(i,21))**2.)

c      Numera cintes H Type 4 i to i+1
       call itxt(psep-0.-xrsep+83.*xkf-120.*(typm3(10)/10.),psey-10,
     + typm3(10),0.0d0,i,7)
       call itxt(psep+hvr(k,15)-xrsep+83.*xkf-120.*(typm3(10)/10.),
     + psey-10,typm3(10),0.0d0,i+1,7)


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Drawing VH-ribs in 2D ribs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Box (1,2)

       sepxx=700.*xkf
       sepyy=100.*xkf

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Rib i-1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       kx=int((float(i-2)/6.))
       ky=i-1-kx*6

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)

c       write (*,*) "i,kx,ky ",i,kx,ky,sepx,sepy

       if (hvr(k,5).eq.1) then

c      Segment
       call line(sepx+ucnt(i-1,ii,2),-vcnt(i-1,ii,2)+sepy,
     + sepx+ucnt(i-1,ii,4),-vcnt(i-1,ii,4)+sepy,3)
       call line(sepx+2530.*xkf+ucnt(i-1,ii,2),-vcnt(i-1,ii,2)+sepy,
     + sepx+2530.*xkf+ucnt(i-1,ii,4),-vcnt(i-1,ii,4)+sepy,3)

c      Points in 2
       alpha=(datan((v(i-1,jcon(i-1,ii,2)-1,3)-v(i-1,jcon(i-1,ii,2)+1,
     + 3))/(u(i-1,jcon(i-1,ii,2)-1,3)-u(i-1,jcon(i-1,ii,2)+1,3))))
       xpeq=ucnt(i-1,ii,2)+1.*xdes*dsin(alpha)
       ypeq=vcnt(i-1,ii,2)-1.*xdes*dcos(alpha)
       call point(sepx+xpeq,sepy-ypeq,3)
       call point(sepx+xpeq-1*dsin(alpha),sepy-ypeq-1*dcos(alpha),1)
       call point(sepx+xpeq-2*dsin(alpha),sepy-ypeq-2*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,3)
       call point(2530.*xkf+sepx+xpeq-1*dsin(alpha),sepy-ypeq-
     + 1*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq-2*dsin(alpha),sepy-ypeq-
     + 2*dcos(alpha),1)

c      Points in 4
       alpha=(datan((v(i-1,jcon(i-1,ii,4)-1,3)-v(i-1,jcon(i-1,ii,4)+1,
     + 3))/(u(i-1,jcon(i-1,ii,4)-1,3)-u(i-1,jcon(i-1,ii,4)+1,3))))
       xpeq=ucnt(i-1,ii,4)+1.*xdes*dsin(alpha)
       ypeq=vcnt(i-1,ii,4)-1.*xdes*dcos(alpha)
       call point(sepx+xpeq,sepy-ypeq,3)
       call point(sepx+xpeq-1*dsin(alpha),sepy-ypeq-1*dcos(alpha),1)
       call point(sepx+xpeq-2*dsin(alpha),sepy-ypeq-2*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,3)
       call point(2530.*xkf+sepx+xpeq-1*dsin(alpha),sepy-ypeq-
     + 1*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq-2*dsin(alpha),sepy-ypeq-
     + 2*dcos(alpha),1)

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Rib i (center)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       kx=int((float(i-1)/6.))
       ky=i-kx*6

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)

c      Segment
       call line(sepx+ucnt(i,ii,6),-vcnt(i,ii,6)+sepy,
     + sepx+ucnt(i,ii,8),-vcnt(i,ii,8)+sepy,30)
       call line(sepx+2530.*xkf+ucnt(i,ii,6),-vcnt(i,ii,6)+sepy,
     + sepx+2530.*xkf+ucnt(i,ii,8),-vcnt(i,ii,8)+sepy,30)

c      Points in 6
       alpha=(datan((v(i,jcon(i,ii,6)-1,3)-v(i,jcon(i,ii,6)+1,
     + 3))/(u(i,jcon(i,ii,6)-1,3)-u(i,jcon(i,ii,6)+1,3))))
       xpeq=ucnt(i,ii,6)-1.*xdes*dsin(alpha)
       ypeq=vcnt(i,ii,6)+1.*xdes*dcos(alpha)
       call point(sepx+xpeq,sepy-ypeq,1)
c       call point(sepx+xpeq+1*dsin(alpha),sepy-ypeq+1*dcos(alpha),1)
c       call point(sepx+xpeq+2*dsin(alpha),sepy-ypeq+2*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,1)
c       call point(2530.*xkf+sepx+xpeq+1*dsin(alpha),sepy-ypeq+
c     + 1*dcos(alpha),1)
c       call point(2530.*xkf+sepx+xpeq+2*dsin(alpha),sepy-ypeq+
c     + 2*dcos(alpha),1)

c      Points in 8
       alpha=(datan((v(i,jcon(i,ii,8)-1,3)-v(i,jcon(i,ii,8)+1,
     + 3))/(u(i,jcon(i,ii,8)-1,3)-u(i,jcon(i,ii,8)+1,3))))
       xpeq=ucnt(i,ii,8)-1.*xdes*dsin(alpha)
       ypeq=vcnt(i,ii,8)+1.*xdes*dcos(alpha)
       call point(sepx+xpeq,sepy-ypeq,30)
c       call point(sepx+xpeq+1*dsin(alpha),sepy-ypeq+1*dcos(alpha),1)
c       call point(sepx+xpeq+2*dsin(alpha),sepy-ypeq+2*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,30)
c       call point(2530.*xkf+sepx+xpeq+1*dsin(alpha),sepy-ypeq+
c     + 1*dcos(alpha),1)
c       call point(2530.*xkf+sepx+xpeq+2*dsin(alpha),sepy-ypeq+
c     + 2*dcos(alpha),1)


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Rib i+1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       kx=int((float(i)/6.))
       ky=i+1-kx*6

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)

       if (hvr(k,6).eq.1) then
       
c      Segment
       call line(sepx+ucnt(i+1,ii,6),-vcnt(i+1,ii,6)+sepy,
     + sepx+ucnt(i+1,ii,8),-vcnt(i+1,ii,8)+sepy,30)
       call line(sepx+2530.*xkf+ucnt(i+1,ii,6),-vcnt(i+1,ii,6)+sepy,
     + sepx+2530.*xkf+ucnt(i+1,ii,8),-vcnt(i+1,ii,8)+sepy,30)

c      Points in 6
       alpha=(datan((v(i+1,jcon(i+1,ii,6)-1,3)-v(i+1,jcon(i+1,ii,6)+1,
     + 3))/(u(i+1,jcon(i+1,ii,6)-1,3)-u(i+1,jcon(i+1,ii,6)+1,3))))
       xpeq=ucnt(i+1,ii,6)-1.*xdes*dsin(alpha)
       ypeq=vcnt(i+1,ii,6)+1.*xdes*dcos(alpha)
       call point(sepx+xpeq,sepy-ypeq,30)
c       call point(sepx+xpeq+1*dsin(alpha),sepy-ypeq+1*dcos(alpha),1)
c       call point(sepx+xpeq+2*dsin(alpha),sepy-ypeq+2*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,30)
c       call point(2530.*xkf+sepx+xpeq+1*dsin(alpha),sepy-ypeq+
c     + 1*dcos(alpha),1)
c       call point(2530.*xkf+sepx+xpeq+2*dsin(alpha),sepy-ypeq+
c     + 2*dcos(alpha),1)

c      Points in 8
       alpha=(datan((v(i+1,jcon(i+1,ii,8)-1,3)-v(i+1,jcon(i+1,ii,8)+1,
     + 3))/(u(i+1,jcon(i+1,ii,8)-1,3)-u(i+1,jcon(i+1,ii,8)+1,3))))
       xpeq=ucnt(i+1,ii,8)-1.*xdes*dsin(alpha)
       ypeq=vcnt(i+1,ii,8)+1.*xdes*dcos(alpha)
       call point(sepx+xpeq,sepy-ypeq,1)
c       call point(sepx+xpeq+1*dsin(alpha),sepy-ypeq+1*dcos(alpha),1)
c       call point(sepx+xpeq+2*dsin(alpha),sepy-ypeq+2*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,1)
c       call point(2530.*xkf+sepx+xpeq+1*dsin(alpha),sepy-ypeq+
c     + 1*dcos(alpha),1)
c       call point(2530.*xkf+sepx+xpeq+2*dsin(alpha),sepy-ypeq+
c     + 2*dcos(alpha),1)

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Rib i+2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       kx=int((float(i+1)/6.))
       ky=i+2-kx*6

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)

       if (hvr(k,6).eq.1) then

c      Segment
       call line(sepx+ucnt(i+2,ii,2),-vcnt(i+2,ii,2)+sepy,
     + sepx+ucnt(i+2,ii,4),-vcnt(i+2,ii,4)+sepy,1)
       call line(sepx+2530.*xkf+ucnt(i+2,ii,2),-vcnt(i+2,ii,2)+sepy,
     + sepx+2530.*xkf+ucnt(i+2,ii,4),-vcnt(i+2,ii,4)+sepy,1)

c      Points in 2
       alpha=(datan((v(i+2,jcon(i+2,ii,2)-1,3)-v(i+2,jcon(i+2,ii,2)+1,
     + 3))/(u(i+2,jcon(i+2,ii,2)-1,3)-u(i+2,jcon(i+2,ii,2)+1,3))))
       xpeq=ucnt(i+2,ii,2)+1.*xdes*dsin(alpha)
       ypeq=vcnt(i+2,ii,2)-1.*xdes*dcos(alpha)
       call point(sepx+xpeq,sepy-ypeq,3)
       call point(sepx+xpeq-1*dsin(alpha),sepy-ypeq-1*dcos(alpha),1)
       call point(sepx+xpeq-2*dsin(alpha),sepy-ypeq-2*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,3)
       call point(2530.*xkf+sepx+xpeq-1*dsin(alpha),sepy-ypeq-
     + 1*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq-2*dsin(alpha),sepy-ypeq-
     + 2*dcos(alpha),1)

c      Points in 4
       alpha=(datan((v(i+2,jcon(i+2,ii,4)-1,3)-v(i+2,jcon(i+2,ii,4)+1,
     + 3))/(u(i+2,jcon(i+2,ii,4)-1,3)-u(i+2,jcon(i+2,ii,4)+1,3))))
       xpeq=ucnt(i+2,ii,4)+1.*xdes*dsin(alpha)
       ypeq=vcnt(i+2,ii,4)-1.*xdes*dcos(alpha)
       call point(sepx+xpeq,sepy-ypeq,3)
       call point(sepx+xpeq-1*dsin(alpha),sepy-ypeq-1*dcos(alpha),1)
       call point(sepx+xpeq-2*dsin(alpha),sepy-ypeq-2*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,3)
       call point(2530.*xkf+sepx+xpeq-1*dsin(alpha),sepy-ypeq-
     + 1*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq-2*dsin(alpha),sepy-ypeq-
     + 2*dcos(alpha),1)

       end if

       end if   ! end type VH



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.6 V-rib Type 6 GENERAL TYPE
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.6.1 Rib i
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Now ii is virtual row, assigned automatically for each rib
       iii=1

       if (hvr(k,2).eq.6) then

c      Define types 6 along rib
       if (k.ge.2.and.hvr(k-1,2).eq.6.and.hvr(k-1,3).eq.hvr(k,3)) then
       iii=iii+1
       end if

c      Define main points 2,3,4,9,10,11

       i=hvr(k,3)    ! rib i
       ii=iii       ! virtual row ii, in rib i

       ucnt(i,ii,3)=rib(i,5)*hvr(k,4)/100.0d0
       ucnt(i,ii,2)=ucnt(i,ii,3)-hvr(k,6)
       ucnt(i,ii,4)=ucnt(i,ii,3)+hvr(k,7)
       ucnt(i,ii,6)=ucnt(i,ii,2)
       ucnt(i,ii,7)=ucnt(i,ii,3)
       ucnt(i,ii,8)=ucnt(i,ii,4)
       ucnt(i,ii,9)=ucnt(i,ii,2)
       ucnt(i,ii,10)=ucnt(i,ii,3)
       ucnt(i,ii,11)=ucnt(i,ii,4)

c      Points 2,3,4 interpolation in rib i

       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,2).and.u(i,j+1,3).ge.ucnt(i,ii,2)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,2)=xm*ucnt(i,ii,2)+xb
       jcon(i,ii,2)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       jcon(i,ii,3)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,4).and.u(i,j+1,3).ge.ucnt(i,ii,4)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,4)=xm*ucnt(i,ii,4)+xb
       jcon(i,ii,4)=j
       end if

       end do

c      Points 9,10,11 interpolation in rib i

       do j=1,np(i,2)

       if (u(i,j,3).gt.ucnt(i,ii,9).and.u(i,j+1,3).le.ucnt(i,ii,9)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,9)=xm*ucnt(i,ii,9)+xb
       jcon(i,ii,9)=j+1
       end if

       if (u(i,j,3).gt.ucnt(i,ii,10).and.u(i,j+1,3).le.ucnt(i,ii,10)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,10)=xm*ucnt(i,ii,10)+xb
       jcon(i,ii,10)=j+1
       end if

       if (u(i,j,3).gt.ucnt(i,ii,11).and.u(i,j+1,3).le.ucnt(i,ii,11)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,11)=xm*ucnt(i,ii,11)+xb
       jcon(i,ii,11)=j+1
       end if

       end do

c      Points 6,8 interpolation in rib i

       vcnt(i,ii,6)=(vcnt(i,ii,9)-vcnt(i,ii,2))*(hvr(k,5)/100.)+
     + vcnt(i,ii,2)
       vcnt(i,ii,8)=(vcnt(i,ii,11)-vcnt(i,ii,4))*(hvr(k,5)/100.)+
     + vcnt(i,ii,4)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case h1=0.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (hvr(k,5).eq.0.) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat line 2-3-4 in n regular spaces   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Experimental version     
c      Reformat in 20 spaces

       n1vr=jcon(i,ii,4)-jcon(i,ii,2)+1
       n2vr=20+1

c      Load data polyline
       xlin1(1)=ucnt(i,ii,2)
       ylin1(1)=vcnt(i,ii,2)
       do j=2,n1vr-1
       xlin1(j)=u(i,jcon(i,ii,2)+j-1,3)
       ylin1(j)=v(i,jcon(i,ii,2)+j-1,3)
c      MIRAR SI CAL +-1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       end do
       xlin1(n1vr)=ucnt(i,ii,4)
       ylin1(n1vr)=vcnt(i,ii,4)

c      Call subroutine vector redistribution

       call vredis(xlin1,ylin1,xlin3,ylin3,n1vr,n2vr)

c      Load result polyline

       do j=1,n2vr
       ucnt2(i,ii,j)=xlin3(j)
       vcnt2(i,ii,j)=ylin3(j)
       end do

       end if ! case h1=0.

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case 0. < h1 < 100.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Redefine line 2: ucnt2(i,ii,j) - vcnt2(i,ii,j)
c      Divide line 6-8 in n segments

       uinc=0.
       vinc=0.
       do j=1,21
       ucnt2(i,ii,j)=ucnt(i,ii,6)+uinc
       uinc=uinc+(ucnt(i,ii,8)-ucnt(i,ii,6))/20.
       vcnt2(i,ii,j)=vcnt(i,ii,6)+vinc
       vinc=vinc+(vcnt(i,ii,8)-vcnt(i,ii,6))/20.
       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case h1=100.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (hvr(k,5).eq.100.) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat 9-10-11 in n spaces (rib i)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
c      Reformat in 20 spaces

       n1vr=jcon(i,ii,9)-jcon(i,ii,11)+1    
       n2vr=20+1

c      Load data polyline
       xlin1(1)=ucnt(i,ii,9)
       ylin1(1)=vcnt(i,ii,9)
       do j=2,n1vr-1
       xlin1(j)=u(i,jcon(i,ii,9)-j+1,3)
       ylin1(j)=v(i,jcon(i,ii,9)-j+1,3)
c      MIRAR SI CAL +-1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       end do
       xlin1(n1vr)=ucnt(i,ii,11)
       ylin1(n1vr)=vcnt(i,ii,11)

c      Call subroutine vector redistribution

       call vredis(xlin1,ylin1,xlin3,ylin3,n1vr,n2vr)

c      Load result polyline

       do j=1,n2vr
       ucnt2(i,ii,j)=xlin3(j)
       vcnt2(i,ii,j)=ylin3(j)
       end do
 
       end if ! Case h1=100.


cccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.6.2 Rib i+1
cccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,8)   ! rib i+1
c      ii use virtial row 

       ucnt(i,ii,3)=rib(i,5)*hvr(k,9)/100.0d0

c       ucnt(i,ii,1)=ucnt(i,ii,3)-hvr(k,8)

       ucnt(i,ii,2)=ucnt(i,ii,3)-hvr(k,11)
       ucnt(i,ii,4)=ucnt(i,ii,3)+hvr(k,12)
       ucnt(i,ii,6)=ucnt(i,ii,2)
       ucnt(i,ii,8)=ucnt(i,ii,4)
       ucnt(i,ii,9)=ucnt(i,ii,2)
       ucnt(i,ii,11)=ucnt(i,ii,4)

c      Points 2,3,4 interpolation in rib i+1

       do j=np(i,2),np(i,1)

       if (u(i,j,3).le.ucnt(i,ii,2).and.u(i,j+1,3).ge.ucnt(i,ii,2)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,2)=xm*ucnt(i,ii,2)+xb
       jcon(i,ii,2)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,3).and.u(i,j+1,3).ge.ucnt(i,ii,3)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,3)=xm*ucnt(i,ii,3)+xb
       jcon(i,ii,3)=j
       end if

       if (u(i,j,3).le.ucnt(i,ii,4).and.u(i,j+1,3).ge.ucnt(i,ii,4)) then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,4)=xm*ucnt(i,ii,4)+xb
       jcon(i,ii,4)=j
       end if

       end do

c      Points 9,10,11 interpolation in rib i+1

       do j=1,np(i,2)

       if (u(i,j,3).gt.ucnt(i,ii,9).and.u(i,j+1,3).le.ucnt(i,ii,9)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,9)=xm*ucnt(i,ii,9)+xb
       jcon(i,ii,9)=j+1
       end if

       if (u(i,j,3).gt.ucnt(i,ii,10).and.u(i,j+1,3).le.ucnt(i,ii,10)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,10)=xm*ucnt(i,ii,10)+xb
       jcon(i,ii,10)=j+1
       end if

       if (u(i,j,3).gt.ucnt(i,ii,11).and.u(i,j+1,3).le.ucnt(i,ii,11)) 
     + then
       xm=(v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))
       xb=v(i,j,3)-xm*u(i,j,3)
       vcnt(i,ii,11)=xm*ucnt(i,ii,11)+xb
       jcon(i,ii,11)=j+1
       end if

       end do

c      Points 6,8 interpolation in rib i+1

       vcnt(i,ii,6)=(vcnt(i,ii,9)-vcnt(i,ii,2))*(hvr(k,10)/100.)+
     + vcnt(i,ii,2)
       vcnt(i,ii,8)=(vcnt(i,ii,11)-vcnt(i,ii,4))*(hvr(k,10)/100.)+
     + vcnt(i,ii,4)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case h2=0.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (hvr(k,10).eq.0.) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat line 2-3-4 in n regular spaces   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Experimental version     
c      Reformat in 20 spaces

       n1vr=jcon(i,ii,4)-jcon(i,ii,2)+1
       n2vr=20+1

c      Load data polyline
       xlin1(1)=ucnt(i,ii,2)
       ylin1(1)=vcnt(i,ii,2)
       do j=2,n1vr-1
       xlin1(j)=u(i,jcon(i,ii,2)+j-1,3)
       ylin1(j)=v(i,jcon(i,ii,2)+j-1,3)
c      MIRAR SI CAL +-1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       end do
       xlin1(n1vr)=ucnt(i,ii,4)
       ylin1(n1vr)=vcnt(i,ii,4)

c      Call subroutine vector redistribution

       call vredis(xlin1,ylin1,xlin3,ylin3,n1vr,n2vr)

c      Load result polyline

       do j=1,n2vr
       ucnt3(i,ii,j)=xlin3(j)
       vcnt3(i,ii,j)=ylin3(j)
       end do

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case 0. < h2 < 100.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Redefine line 2: ucnt3(i,ii,j) - vcnt3(i,ii,j)
c      Divide line 6-8 in n segments

       uinc=0.
       vinc=0.
       do j=1,21
       ucnt3(i,ii,j)=ucnt(i,ii,6)+uinc
       uinc=uinc+(ucnt(i,ii,8)-ucnt(i,ii,6))/20.
       vcnt3(i,ii,j)=vcnt(i,ii,6)+vinc
       vinc=vinc+(vcnt(i,ii,8)-vcnt(i,ii,6))/20.
       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case h2=100.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (hvr(k,10).eq.100.) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat 9-10-11 in n spaces (rib i+1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Experimental version     
c      Reformat in 20 spaces

       n1vr=jcon(i,ii,9)-jcon(i,ii,11)+1
       n2vr=20+1

c      Load data polyline
       xlin1(1)=ucnt(i,ii,9)
       ylin1(1)=vcnt(i,ii,9)
       do j=2,n1vr-1
       xlin1(j)=u(i,jcon(i,ii,9)-j+1,3)
       ylin1(j)=v(i,jcon(i,ii,9)-j+1,3)
c      MIRAR SI CAL +-1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       end do
       xlin1(n1vr)=ucnt(i,ii,11)
       ylin1(n1vr)=vcnt(i,ii,11)

c      Call subroutine vector redistribution

       call vredis(xlin1,ylin1,xlin3,ylin3,n1vr,n2vr)

c      Load result polyline

       do j=1,n2vr
       ucnt3(i,ii,j)=xlin3(j)
       vcnt3(i,ii,j)=ylin3(j)
       end do

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Rib localisation
       i=hvr(k,3)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.6.3 V-ribs lines 2 3 transportation to 3D espace
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Rib i (Line 2)

       i=hvr(k,3)

       tetha=rib(i,8)*pi/180.
       
       do j=1,21
       ru(i,j,3)=ucnt2(i,ii,j)
       rv(i,j,3)=vcnt2(i,ii,j)-rib(i,50)
       end do

       do j=1,21

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       rx2(i,j,ii)=rx(i,j)
       ry2(i,j,ii)=ry(i,j)
       rz2(i,j,ii)=rz(i,j)

       end do

c      Rib i+1 (Line 3)

       i=hvr(k,8)

       tetha=rib(i,8)*pi/180.

       do j=1,21
       ru(i,j,3)=ucnt3(i,ii,j)
       rv(i,j,3)=vcnt3(i,ii,j)-rib(i,50)      
       end do

       do j=1,21

       u_aux(i,j,1)=ru(i,j,3)
       v_aux(i,j,1)=rv(i,j,3)
       w_aux(i,j,1)=0.0d0
       call xyzt(i,j,u,v,w,rib,np,u_aux,v_aux,w_aux)
       rx(i,j)=w_aux(i,j,5)
       ry(i,j)=u_aux(i,j,5)
       rz(i,j)=v_aux(i,j,5)

       rx3(i-1,j,ii)=rx(i,j)
       ry3(i-1,j,ii)=ry(i,j)
       rz3(i-1,j,ii)=rz(i,j)

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.6.4 V-ribs calculus and drawing in 3D and 2D
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.6.4 V-rib 2-3 in 2D model (red)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=hvr(k,3)

       px0=0.
       py0=0.
       ptheta=0.

       do j=1,21

c      Distances between points
       pa=dsqrt((rx(i+1,j)-rx(i,j))**2.+(ry(i+1,j)-ry(i,j))**2.+
     + (rz(i+1,j)-rz(i,j))**2.)
       pb=dsqrt((rx(i+1,j+1)-rx(i,j))**2.+(ry(i+1,j+1)-ry(i,j))**2.+
     + (rz(i+1,j+1)-rz(i,j))**2.)
       pc=dsqrt((rx(i+1,j+1)-rx(i+1,j))**2.+(ry(i+1,j+1)-ry(i+1,j))**2.+
     + (rz(i+1,j+1)-rz(i+1,j))**2.)
       pd=dsqrt((rx(i+1,j)-rx(i,j+1))**2.+(ry(i+1,j)-ry(i,j+1))**2.+
     + (rz(i+1,j)-rz(i,j+1))**2.)
       pe=dsqrt((rx(i,j+1)-rx(i,j))**2.+(ry(i,j+1)-ry(i,j))**2.+
     + (rz(i,j+1)-rz(i,j))**2.)
       pf=dsqrt((rx(i+1,j+1)-rx(i,j+1))**2.+(ry(i+1,j+1)-ry(i,j+1))**2.+
     + (rz(i+1,j+1)-rz(i,j+1))**2.)
       
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

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Drawing Type-6 in 2D model
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       

c      Draw in BOX(2,6)

       psep=(3300.+1260.+1260.)*xkf+
     + xrsep*hvr_positions%horizontal(6)*float(i)
       psey=(800.+890.85+100.)*xkf+hvr_positions%vertical(6)*
     + (hvr(k,4)+hvr(k,9))*0.5*rib(int(hvr(k,3)),5)*3.0/100.

c       write (*,*) "i,ii ",i,ii

       j=1

c      Costat vora d'atac
       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),1)

       j=21
c      Costat fuga
       call line(psep+pl1x(i,j),psey+pl1y(i,j),psep+pr1x(i,j),
     + psey+pr1y(i,j),1)

c      Optional section-38 holes for generic V-ribs (types 6/16).
       call find_hvr_hole_spec(hvr_holes,16,hvr_hole,hvr_hole_found)
       if (hvr_hole_found) then
       do iell=1,hvr_hole%hole_count
       hvr_span_fraction=dfloat(iell)/dfloat(hvr_hole%hole_count+1)
       call vrib_hole_ellipse(pl1x(i,1),pl1y(i,1),pr1x(i,1),pr1y(i,1),
     + pl1x(i,21),pl1y(i,21),pr1x(i,21),pr1y(i,21),hvr_span_fraction,
     + hvr_hole%parameter(1),hvr_hole%parameter(2),
     + hvr_hole%parameter(3),x0,y0,a,b,angle)
       call ellipse(psep+x0,psey+y0,a,b,angle,3)
       end do
       end if


c      Marca punts MC a l'esquerra

       alpha=-(datan((pl1y(i,1)-pl2y(i,20))/(pl1x(i,1)-pl2x(i,20))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xp6=pl1x(i,1)-xdes*dsin(alpha)-2.*xdes*dcos(alpha)
       yp6=pl1y(i,1)-xdes*dcos(alpha)+2.*xdes*dsin(alpha)
       xp8=pl1x(i,21)-xdes*dsin(alpha)+2.*xdes*dcos(alpha)
       yp8=pl1y(i,21)-xdes*dcos(alpha)-2.*xdes*dsin(alpha)
c       xp7=0.5*(pl1x(i,1)+pl1x(i,21))-xdes*dsin(alpha)
c       yp7=0.5*(pl1y(i,1)+pl1y(i,21))-xdes*dcos(alpha)

c       call point(psep+xp7,psey+yp7,2)

       call point(psep+xp6,psey+yp6,1)
c       call point(psep+xp7,psey+yp7,1)
       call point(psep+xp8,psey+yp8,1)


c      Romano costat esquerra

       sl=1.
       
       xpx=(pl1x(i,1)+pl2x(i,20))/2.-sl*xdes*dsin(alpha)
       xpy=(pl1y(i,1)+pl2y(i,20))/2.-sl*xdes*dcos(alpha)

       xpx2=psep+xpx+0.3*hvr(k,7)*dcos(alpha)-0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy-0.3*hvr(k,7)*dsin(alpha)-0.3*xvrib*dcos(alpha) 

       call romano(i,xpx2,xpy2,alpha,typm6(10)*0.1,7)

c      Marca punts MC a la dreta

       alpha=-(datan((pr1y(i,1)-pr2y(i,20))/(pr1x(i,1)
     + -pr2x(i,20))))
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

       xp6=pr1x(i,1)+xdes*dsin(alpha)-2.*xdes*dcos(alpha)
       yp6=pr1y(i,1)+xdes*dcos(alpha)+2.*xdes*dsin(alpha)
       xp8=pr1x(i,21)+xdes*dsin(alpha)+2.*xdes*dcos(alpha)
       yp8=pr1y(i,21)+xdes*dcos(alpha)-2.*xdes*dsin(alpha)
       xp7=0.5*(xp6+xp8)
       yp7=0.5*(yp6+yp8)

       call point(psep+xp6,psey+yp6,1)
c       call point(psep+xp7,psey+yp7,1)
       call point(psep+xp8,psey+yp8,1)

c      Romano costat dret

       sr=1.
       
       xpx=(pr1x(i,1)+pr2x(i,20))/2.+xdes*dsin(alpha)
       xpy=(pr1y(i,1)+pr2y(i,20))/2.+xdes*dcos(alpha)

       xpx2=psep+xpx+0.2*hvr(k,8)*dcos(alpha)+0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy-0.2*hvr(k,8)*dsin(alpha)+0.3*xvrib*dcos(alpha) 

       call romano(i+1,xpx2,xpy2,alpha,typm6(10)*0.1,7)

       xpx2=psep+xpx-0.2*hvr(k,8)*dcos(alpha)+0.3*xvrib*dsin(alpha)
       xpy2=psey+xpy+0.2*hvr(k,8)*dsin(alpha)+0.3*xvrib*dcos(alpha) 

c      Numbering of Type 6 pieces now fron 0 t 10 according the %
       call romano(int(hvr(k,4)*0.1),xpx2,xpy2,alpha,typm6(10)*0.1,7)

c      Vores de costura

       do j=1,21-1

c      Vores de costura esquerra
       alpl=-(datan((pl1y(i,j)-pl2y(i,j))/(pl1x(i,j)-pl2x(i,j))))
       if (alpl.lt.0.) then
       alpl=alpl+pi
       end if

       lvcx(i,j)=psep+pl1x(i,j)-xvrib*dsin(alpl)
       lvcy(i,j)=psey+pl1y(i,j)-xvrib*dcos(alpl)

c      Vores de costura dreta
       alpr=-(datan((pr1y(i,j)-pr2y(i,j))/(pr1x(i,j)-pr2x(i,j))))
       if (alpr.lt.0.) then
       alpr=alpr+pi
       end if

       rvcx(i,j)=psep+pr1x(i,j)+xvrib*dsin(alpr)
       rvcy(i,j)=psey+pr1y(i,j)+xvrib*dcos(alpr)

c      Tancament lateral inici
       if (j.eq.1) then
       call line(psep+pl1x(i,j)-xvrib*dsin(alpl),psey+pl1y(i,j)
     + -xvrib*dcos(alpl),psep+pl1x(i,j),psey+pl1y(i,j),1)
       call line(psep+pr1x(i,j)+xvrib*dsin(alpr),psey+pr1y(i,j)
     + +xvrib*dcos(alpr),psep+pr1x(i,j),psey+pr1y(i,j),1)
       end if

c      Tancament lateral fi
       if (j.eq.20) then
       call line(psep+pl2x(i,j)-xvrib*dsin(alpl),psey+pl2y(i,j)
     + -xvrib*dcos(alpl),psep+pl2x(i,j),psey+pl2y(i,j),1)
       call line(psep+pr2x(i,j)+xvrib*dsin(alpr),psey+pr2y(i,j)
     + +xvrib*dcos(alpr),psep+pr2x(i,j),psey+pr2y(i,j),1)

       lvcx(i,j+1)=psep+pl2x(i,j)-xvrib*dsin(alpl)
       lvcy(i,j+1)=psey+pl2y(i,j)-xvrib*dcos(alpl)

       rvcx(i,j+1)=psep+pr2x(i,j)+xvrib*dsin(alpr)
       rvcy(i,j+1)=psey+pr2y(i,j)+xvrib*dcos(alpr)

       end if

       end do ! j

c      V-rib length
       hvr(k,15)=dsqrt((lvcx(i,1)-rvcx(i,1))**2.0d0+
     + (lvcy(i,1)-rvcy(i,1))**2.0d0)

c      Numera cintes V Type 6
       call itxt(psep-xrsep+83.*xkf-120.*(typm3(10)/10.),psey-10,
     + typm3(10),0.0d0,i,7)
       call itxt(psep+hvr(k,15)-xrsep+83.*xkf-120.*(typm3(10)/10.),
     + psey-10,typm3(10),0.0d0,i+1,7)

c      Dibuixa vores amb segments completament enllaçats       
       do j=1,20

       call line(lvcx(i,j),lvcy(i,j),lvcx(i,j+1),lvcy(i,j+1),1)
       call line(rvcx(i,j),rvcy(i,j),rvcx(i,j+1),rvcy(i,j+1),1)

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.6.5 Drawing V-ribs marks in 2D ribs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Drawing in 2D ribs printing and MC (+2530.xkf)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Box (1,2)

       sepxx=700.*xkf
       sepyy=100.*xkf

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Rib i (center)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       kx=int((float(i-1)/6.))
       ky=i-kx*6

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case h1=0.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (hvr(k,5).eq.0.) then

c      Segment
       call line(sepx+ucnt(i,ii,2),-vcnt(i,ii,2)+sepy,
     + sepx+ucnt(i,ii,4),-vcnt(i,ii,4)+sepy,1)
       if (i37_2006.eq.1) then
       call line(sepx+2530.*xkf+ucnt(i,ii,2),-vcnt(i,ii,2)+sepy,
     + sepx+2530.*xkf+ucnt(i,ii,4),-vcnt(i,ii,4)+sepy,1)
       endif

c      Points in 2 and 4 (Experimental)

c      Points in 2
       alpha=(datan((v(i,jcon(i,ii,2)-1,3)-v(i,jcon(i,ii,2)+1,
     + 3))/(u(i,jcon(i,ii,2)-1,3)-u(i,jcon(i,ii,2)+1,3))))
       xpeq=ucnt(i,ii,2)+1.*xdes*dsin(alpha)
       ypeq=vcnt(i,ii,2)-1.*xdes*dcos(alpha)
       call point(sepx+xpeq,sepy-ypeq,1)
       call point(sepx+xpeq-1*dsin(alpha),sepy-ypeq-1*dcos(alpha),1)
       call point(sepx+xpeq-2*dsin(alpha),sepy-ypeq-2*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,1)
       call point(2530.*xkf+sepx+xpeq-1*dsin(alpha),sepy-ypeq-
     + 1*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq-2*dsin(alpha),sepy-ypeq-
     + 2*dcos(alpha),1)

c      Points in 4
       alpha=(datan((v(i,jcon(i,ii,4)-1,3)-v(i,jcon(i,ii,4)+1,
     + 3))/(u(i,jcon(i,ii,4)-1,3)-u(i,jcon(i,ii,4)+1,3))))
       xpeq=ucnt(i,ii,4)+1.*xdes*dsin(alpha)
       ypeq=vcnt(i,ii,4)-1.*xdes*dcos(alpha)
       call point(sepx+xpeq,sepy-ypeq,1)
       call point(sepx+xpeq-1*dsin(alpha),sepy-ypeq-1*dcos(alpha),1)
       call point(sepx+xpeq-2*dsin(alpha),sepy-ypeq-2*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,1)
       call point(2530.*xkf+sepx+xpeq-1*dsin(alpha),sepy-ypeq-
     + 1*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq-2*dsin(alpha),sepy-ypeq-
     + 2*dcos(alpha),1)

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case 0. < h1 < 100.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (hvr(k,5).gt.0.and.hvr(k,5).lt.100.) then

c      Segment
       call line(sepx+ucnt(i,ii,6),-vcnt(i,ii,6)+sepy,
     + sepx+ucnt(i,ii,8),-vcnt(i,ii,8)+sepy,1)
       if (i37_2006.eq.1) then
       call line(sepx+2530.*xkf+ucnt(i,ii,6),-vcnt(i,ii,6)+sepy,
     + sepx+2530.*xkf+ucnt(i,ii,8),-vcnt(i,ii,8)+sepy,1)
       endif

c      Punts marcatge V-rib
       alpha=datan((vcnt(i,ii,8)-vcnt(i,ii,6))/
     + (ucnt(i,ii,8)-ucnt(i,ii,6)))
       xp6=ucnt(i,ii,6)-xdes*dsin(alpha)
       yp6=vcnt(i,ii,6)+xdes*dcos(alpha)
       xp8=ucnt(i,ii,8)-xdes*dsin(alpha)
       yp8=vcnt(i,ii,8)+xdes*dcos(alpha)
       xp7=0.5*(xp6+xp8)
       yp7=0.5*(yp6+yp8)
       call point(sepx+xp6,sepy-yp6,1)
       call point(sepx+xp7,sepy-yp7,1)
       call point(sepx+xp8,sepy-yp8,1)
       call point(sepx+2530.*xkf+xp6,sepy-yp6,1)
       call point(sepx+2530.*xkf+xp7,sepy-yp7,1)
       call point(sepx+2530.*xkf+xp8,sepy-yp8,1)

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case h1=100.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (hvr(k,5).eq.100.) then

c      Segment
       call line(sepx+ucnt(i,ii,9),-vcnt(i,ii,9)+sepy,
     + sepx+ucnt(i,ii,11),-vcnt(i,ii,11)+sepy,1)
       if (i37_2006.eq.1) then
       call line(sepx+2530.*xkf+ucnt(i,ii,9),-vcnt(i,ii,9)+sepy,
     + sepx+2530.*xkf+ucnt(i,ii,11),-vcnt(i,ii,11)+sepy,1)
       endif

c      Points in 9
       alpha=(datan((v(i,jcon(i,ii,9)-1,3)-v(i,jcon(i,ii,9)+1,
     + 3))/(u(i,jcon(i,ii,9)-1,3)-u(i,jcon(i,ii,9)+1,3))))
       xpeq=ucnt(i,ii,9)-1.*xdes*dsin(alpha)
       ypeq=vcnt(i,ii,9)+1.*xdes*dcos(alpha)
       call point(sepx+xpeq,sepy-ypeq,1)
       call point(sepx+xpeq+1*dsin(alpha),sepy-ypeq+1*dcos(alpha),1)
       call point(sepx+xpeq+2*dsin(alpha),sepy-ypeq+2*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,1)
       call point(2530.*xkf+sepx+xpeq+1*dsin(alpha),sepy-ypeq+
     + 1*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq+2*dsin(alpha),sepy-ypeq+
     + 2*dcos(alpha),1)

c      Points in 11
       alpha=(datan((v(i,jcon(i,ii,11)-1,3)-v(i,jcon(i,ii,11)+1,
     + 3))/(u(i,jcon(i,ii,11)-1,3)-u(i,jcon(i,ii,11)+1,3))))
       xpeq=ucnt(i,ii,11)-1.*xdes*dsin(alpha)
       ypeq=vcnt(i,ii,11)+1.*xdes*dcos(alpha)
       call point(sepx+xpeq,sepy-ypeq,1)
       call point(sepx+xpeq+1*dsin(alpha),sepy-ypeq+1*dcos(alpha),1)
       call point(sepx+xpeq+2*dsin(alpha),sepy-ypeq+2*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,1)
       call point(2530.*xkf+sepx+xpeq+1*dsin(alpha),sepy-ypeq+
     + 1*dcos(alpha),1)
       call point(2530.*xkf+sepx+xpeq+2*dsin(alpha),sepy-ypeq+
     + 2*dcos(alpha),1)

       end if


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Rib i+1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       kx=int((float(i)/6.))
       ky=i+1-kx*6

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky-1)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case h2=0.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (hvr(k,10).eq.0.) then

c      Segments
       call line(sepx+ucnt(i+1,ii,2),-vcnt(i+1,ii,2)+sepy,
     + sepx+ucnt(i+1,ii,4),-vcnt(i+1,ii,4)+sepy,4)
       if (i37_2006.eq.1) then
       call line(sepx+2530.*xkf+ucnt(i+1,ii,2),-vcnt(i+1,ii,2)+sepy,
     + sepx+2530.*xkf+ucnt(i+1,ii,4),-vcnt(i+1,ii,4)+sepy,4)
       endif

c      Draw 3 point in 2 and 4

c      Points in 2 and 4 (Experimental)

c      Points in 2
       alpha=(datan((v(i+1,jcon(i+1,ii,2)-1,3)-v(i+1,jcon(i+1,ii,2)+1,
     + 3))/(u(i+1,jcon(i+1,ii,2)-1,3)-u(i+1,jcon(i+1,ii,2)+1,3))))
       xpeq=ucnt(i+1,ii,2)+1.*xdes*dsin(alpha)
       ypeq=vcnt(i+1,ii,2)-1.*xdes*dcos(alpha)
       call point(sepx+xpeq,sepy-ypeq,4)
       call point(sepx+xpeq-1*dsin(alpha),sepy-ypeq-1*dcos(alpha),4)
       call point(sepx+xpeq-2*dsin(alpha),sepy-ypeq-2*dcos(alpha),4)
       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,4)
       call point(2530.*xkf+sepx+xpeq-1*dsin(alpha),sepy-ypeq-
     + 1*dcos(alpha),4)
       call point(2530.*xkf+sepx+xpeq-2*dsin(alpha),sepy-ypeq-
     + 2*dcos(alpha),4)

c      Points in 4
       alpha=(datan((v(i+1,jcon(i+1,ii,4)-1,3)-v(i+1,jcon(i+1,ii,4)+1,
     + 3))/(u(i+1,jcon(i+1,ii,4)-1,3)-u(i+1,jcon(i+1,ii,4)+1,3))))
       xpeq=ucnt(i+1,ii,4)+1.*xdes*dsin(alpha)
       ypeq=vcnt(i+1,ii,4)-1.*xdes*dcos(alpha)
       call point(sepx+xpeq,sepy-ypeq,4)
       call point(sepx+xpeq-1*dsin(alpha),sepy-ypeq-1*dcos(alpha),4)
       call point(sepx+xpeq-2*dsin(alpha),sepy-ypeq-2*dcos(alpha),4)
       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,4)
       call point(2530.*xkf+sepx+xpeq-1*dsin(alpha),sepy-ypeq-
     + 1*dcos(alpha),4)
       call point(2530.*xkf+sepx+xpeq-2*dsin(alpha),sepy-ypeq-
     + 2*dcos(alpha),4)

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case 0. < h2 < 100.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (hvr(k,10).gt.0.and.hvr(k,10).lt.100.) then

c      Segments
       call line(sepx+ucnt(i+1,ii,6),-vcnt(i+1,ii,6)+sepy,
     + sepx+ucnt(i+1,ii,8),-vcnt(i+1,ii,8)+sepy,4)
       if (i37_2006.eq.1) then
       call line(sepx+2530.*xkf+ucnt(i+1,ii,6),-vcnt(i+1,ii,6)+sepy,
     + sepx+2530.*xkf+ucnt(i+1,ii,8),-vcnt(i+1,ii,8)+sepy,4)
       endif

c      Punts marcatge V-rib
       alpha=datan((vcnt(i+1,ii,8)-vcnt(i+1,ii,6))/
     + (ucnt(i+1,ii,8)-ucnt(i+1,ii,6)))
       xp6=ucnt(i+1,ii,6)-xdes*dsin(alpha)
       yp6=vcnt(i+1,ii,6)+xdes*dcos(alpha)
       xp8=ucnt(i+1,ii,8)-xdes*dsin(alpha)
       yp8=vcnt(i+1,ii,8)+xdes*dcos(alpha)
       xp7=0.5*(xp6+xp8)
       yp7=0.5*(yp6+yp8)
       call point(sepx+xp6,sepy-yp6,4)
       call point(sepx+xp7,sepy-yp7,4)
       call point(sepx+xp8,sepy-yp8,4)
       call point(sepx+2530.*xkf+xp6,sepy-yp6,4)
       call point(sepx+2530.*xkf+xp7,sepy-yp7,4)
       call point(sepx+2530.*xkf+xp8,sepy-yp8,4)

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case h2=100.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (hvr(k,10).eq.100.) then

c      Segements
       call line(sepx+ucnt(i+1,ii,9),-vcnt(i+1,ii,9)+sepy,
     + sepx+ucnt(i+1,ii,11),-vcnt(i+1,ii,11)+sepy,4)
       if (i37_2006.eq.1) then
       call line(sepx+2530.*xkf+ucnt(i+1,ii,9),-vcnt(i+1,ii,9)+sepy,
     + sepx+2530.*xkf+ucnt(i+1,ii,11),-vcnt(i+1,ii,11)+sepy,4)
       endif

c      Draw 3 point in 9 and 11

c      Points in 9
       alpha=(datan((v(i+1,jcon(i+1,ii,9)-1,3)-v(i+1,jcon(i+1,ii,9)+1,
     + 3))/(u(i+1,jcon(i+1,ii,9)-1,3)-u(i+1,jcon(i+1,ii,9)+1,3))))
       xpeq=ucnt(i+1,ii,9)-1.*xdes*dsin(alpha)
       ypeq=vcnt(i+1,ii,9)+1.*xdes*dcos(alpha)
       call point(sepx+xpeq,sepy-ypeq,4)
       call point(sepx+xpeq+1*dsin(alpha),sepy-ypeq+1*dcos(alpha),4)
       call point(sepx+xpeq+2*dsin(alpha),sepy-ypeq+2*dcos(alpha),4)
       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,4)
       call point(2530.*xkf+sepx+xpeq+1*dsin(alpha),sepy-ypeq+
     + 1*dcos(alpha),4)
       call point(2530.*xkf+sepx+xpeq+2*dsin(alpha),sepy-ypeq+
     + 2*dcos(alpha),4)

c      Points in 11
       alpha=(datan((v(i+1,jcon(i+1,ii,11)-1,3)-v(i+1,jcon(i+1,ii,11)+1,
     + 3))/(u(i+1,jcon(i+1,ii,11)-1,3)-u(i+1,jcon(i+1,ii,11)+1,3))))
       xpeq=ucnt(i+1,ii,11)-1.*xdes*dsin(alpha)
       ypeq=vcnt(i+1,ii,11)+1.*xdes*dcos(alpha)
       call point(sepx+xpeq,sepy-ypeq,4)
       call point(sepx+xpeq+1*dsin(alpha),sepy-ypeq+1*dcos(alpha),4)
       call point(sepx+xpeq+2*dsin(alpha),sepy-ypeq+2*dcos(alpha),4)
       call point(2530.*xkf+sepx+xpeq,sepy-ypeq,4)
       call point(2530.*xkf+sepx+xpeq+1*dsin(alpha),sepy-ypeq+
     + 1*dcos(alpha),4)
       call point(2530.*xkf+sepx+xpeq+2*dsin(alpha),sepy-ypeq+
     + 2*dcos(alpha),4)

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      16.6.6 Draw V-rib type 6 in 3D model
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Colors definition if even or odd
       control=((hvr(k,3))/2.)-float(int((hvr(k,3))/2.))
       if (control.ne.0) then
       icolor=4
       else
       icolor=30
       end if

c      Draw in 3D model
       do j=1,21
       call line3d(rx2(i,j,ii),ry2(i,j,ii),rz2(i,j,ii),
     + rx3(i,j,ii),ry3(i,j,ii),rz3(i,j,ii),icolor)
       call line3d(-rx2(i,j,ii),ry2(i,j,ii),rz2(i,j,ii),
     + -rx3(i,j,ii),ry3(i,j,ii),rz3(i,j,ii),icolor)
       end do

c      end if V-rib type 6
       end if


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   End type 6
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c      Seguent dispositiu

       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
