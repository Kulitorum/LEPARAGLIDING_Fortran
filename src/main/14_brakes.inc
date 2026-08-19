c      14. BRAKE CALCULUS
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      14.2 Identifica les cordes a calcular
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       ic1=cordam
       ic2=1

       ii=slp+1 ! Pla de frens

       do k=2,8,2 ! Explora fins 4 nivells
       
       do j=1,cam(ii)-1 ! Itera en camins de cada pla

       a=dfloat(mc(ii,j,k))
       b=dfloat(mc(ii,j,k+1))
       aa=dfloat(mc(ii,j+1,k))
       bb=dfloat(mc(ii,j+1,k+1))

       if (a.ne.0.0d0.and.b.ne.0.0d0) then ! saltar cordes 0 0

c      Mentre la corda sigui igual augmentar comptador
       if (a.eq.aa.and.b.eq.bb) then

       ic2=ic2+1 ! comptabilitza cordes iguals

c      Si arribem a final del camÃ­ comptabilitzar la corda
       if (j.eq.cam(ii)-1) then

       ic1=ic1+1

c       write (*,*) "corda ", ic1, ic2, " = ", mc(ii,j,k),mc(ii,j,k+1)

       corda(ic1,1)=ii                    !planol
       corda(ic1,2)=mc(ii,j+1,k)          !nivell
       corda(ic1,3)=mc(ii,j+1,k+1)        !ordre
       corda(ic1,4)=ic2                   !punts d'acciÃ³
       corda(ic1,5)=mc(ii,j+1,1)          !ramificacions del camÃ­
       corda(ic1,6)=mc(ii,j+1,14)         !final row
       corda(ic1,7)=mc(ii,j+1,15)         !final rib

c      Assign types
       corda(ic1,30+mc(ii,j+1,k))=mc(ii,j+1,30+mc(ii,j+1,k))

       ic2=1

       end if ! Last line

       end if ! while same level

c      Si canvia la corda al mateix nivell
       if (a.ne.aa.or.b.ne.bb) then

       ic1=ic1+1

c       write (*,*) "corda ", ic1, ic2, " = ", mc(ii,j,k),mc(ii,j,k+1)

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

       if ((a.ne.aa.or.b.ne.bb).and.(aa.ne.0.0d0.and.bb.ne.0.0d0)) then

       ic1=ic1+1

       corda(ic1,1)=ii                    !planol
       corda(ic1,2)=mc(ii,j+1,k)          !nivell
       corda(ic1,3)=mc(ii,j+1,k+1)        !ordre
       corda(ic1,4)=ic2                   !punts d'acciÃ³
       corda(ic1,5)=mc(ii,j+1,1)          !ramificacions del camÃ­
       corda(ic1,6)=mc(ii,j+1,14)         !final row
       corda(ic1,7)=mc(ii,j+1,15)         !final rib

c      Assign types
       corda(ic1,30+mc(ii,j+1,k))=mc(ii,j+1,30+mc(ii,j+1,k))
       
       ic2=1

       end if

       end if

       end if

       end do
       end do

       cordat=ic1 ! maxim nombre de cordes inclos les de fre

c      Correction for brakes, use type 6 instead of 1 for main line
       if (icase_brakes.eq.0) then ! set automatic brake description
       corda(cordam+1,31)=6
       end if


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      14.3 Compute anchor points in 3D space
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Need simplification as uses same code as section 6 (!!!!!)

       do i=1,nribss

       tetha=rib(i,8)*pi/180.
       rot_z=rib(i,250)*pi/180.0
       pos=rib(i,5)*rib(i,251)/100.0

       do j=1,rib(i,15) ! anchor number

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
c       v(i,j,18)=v(i,j,17)*dcos(rib(i,9)*pi/180.)
c       w(i,j,18)=-v(i,j,17)*dsin(rib(i,9)*pi/180.)

c      Airfoils rotation in Y-axis
       w(i,j,18)=-w(i,j,4)*dcos(rib(i,9)*pi/180.)-
     + v(i,j,4)*dsin(rib(i,9)*pi/180.)
       u(i,j,18)=u(i,j,4)
       v(i,j,18)=-w(i,j,4)*dsin(rib(i,9)*pi/180.)+
     + v(i,j,4)*dcos(rib(i,9)*pi/180.)

c      Airfoil anchor (x,y,z) absolute coordinates

c       Comment ???????????????????????????? !!!!!
c       u(i,j,19)=rib(i,6)-w(i,j,18)
c       v(i,j,19)=rib(i,3)+u(i,j,18)
c       w(i,j,19)=rib(i,7)-v(i,j,18) 
     

       end do
       end do

       do i=1,nribss
       do j=1,rib(i,15)

       end do

       end do
       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     14.4 Linies d'accio de cada corda
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Recompute anchors of the brakes, for fractional option

       ii=slp+1
       do j=1,cam(ii)
       if (mc(ii,j,15).lt.nribss) then
       i=mc(ii,j,15)
       u(i,6,19)=(1-brake(j,2))*u(i,6,19)+brake(j,2)*u(i+1,6,19)
       v(i,6,19)=(1-brake(j,2))*v(i,6,19)+brake(j,2)*v(i+1,6,19)
       w(i,6,19)=(1-brake(j,2))*w(i,6,19)+brake(j,2)*w(i+1,6,19)
       end if
       end do

c      Linies d'acció

       do i=cordam+1,cordat ! For all lines

       xcorda(i,3)=0.
       ycorda(i,3)=0.
       zcorda(i,3)=0.

       ii=slp+1 !For brake plans

       do k=2,8,2 ! For all levels

       do j=1,cam(ii) ! For all paths

       if (ii.eq.corda(i,1)) then

       if (corda(i,2).eq.mc(ii,j,k).and.mc(ii,j,k+1).eq.corda(i,3)) then

c      Adaptat a punts d'ancoratge de frens (6)
       xcorda(i,3)=xcorda(i,3)+u(mc(ii,j,15),mc(ii,j,14),19)
       ycorda(i,3)=ycorda(i,3)+v(mc(ii,j,15),mc(ii,j,14),19)
       zcorda(i,3)=zcorda(i,3)+w(mc(ii,j,15),mc(ii,j,14),19)

       end if

       end if

       end do

       end do

c      Center of gravity line i
       xcorda(i,3)=xcorda(i,3)/dfloat(corda(i,4))
       ycorda(i,3)=ycorda(i,3)/dfloat(corda(i,4))
       zcorda(i,3)=zcorda(i,3)/dfloat(corda(i,4))

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      14.5 Punts inicial i final de cada corda
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Level 1 (main brake line)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=cordam+1,cordat

       if (corda(i,2).eq.1) then

c      Frens iniciats sota maillons bandes darrera
       xcorda(i,1)=x2line(slp,1,1)
       ycorda(i,1)=y2line(slp,1,1)
       zcorda(i,1)=z2line(slp,1,1)

c      Equacio parametrica de la recta que passa per P1-P3

       dist=dsqrt((xcorda(i,3)-xcorda(i,1))**2.0d0+(ycorda(i,3)-
     + ycorda(i,1))**2.0d0+(zcorda(i,3)-zcorda(i,1))**2.0d0)

       cdl=(xcorda(i,3)-xcorda(i,1))/dist
       cdm=(ycorda(i,3)-ycorda(i,1))/dist
       cdn=(zcorda(i,3)-zcorda(i,1))/dist

c      Parametre necessari a la distancia objectiu

       t=clengb/(dsqrt(cdl*cdl+cdm*cdm+cdn*cdn))

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

c      comprovacio

       disto=dsqrt((xcorda(i,2)-xcorda(i,1))**2+(ycorda(i,2)-
     + ycorda(i,1))**2+(zcorda(i,2)-zcorda(i,1))**2)

       end if

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Level 2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=cordam+1,cordat

       if (corda(i,2).eq.2) then

       if (corda(i,5).eq.2) then ! un nivell adicional


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      REVISAT 2013-05-17 !!!!!!!!!!!!!!!!!!

c      Explora mc i troba quin origen emprar
       kk=slp+1
       do k=1,cam(kk)

       if (corda(i,2).eq.mc(kk,k,4).and.mc(kk,k,5).eq.corda(i,3)) then

       comp1(kk)=mc(kk,k,2)
       comp2(kk)=mc(kk,k,3)

       end if

       end do

c      Origen
       xcorda(i,1)=x2line(corda(i,1),1,int(comp2(corda(i,1))))
       ycorda(i,1)=y2line(corda(i,1),1,int(comp2(corda(i,1))))
       zcorda(i,1)=z2line(corda(i,1),1,int(comp2(corda(i,1))))

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc




c       xcorda(i,1)=x2line(corda(i,1),corda(i,2),corda(i,3))
c       ycorda(i,1)=y2line(corda(i,1),corda(i,2),corda(i,3))
c       zcorda(i,1)=z2line(corda(i,1),corda(i,2),corda(i,3))      

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
       
       end if

       if (corda(i,5).eq.3) then ! dos nivells adicionals

c       write (*,*) ">>> ", i, corda(i,1), corda(i,2), corda(i,3) 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      REVISAT 2013-05-03 !!!!!!!!!!!!!!!!!!

c      Explora mc i troba quin origen emprar
       kk=slp+1
       do k=1,cam(kk)

       if (corda(i,2).eq.mc(kk,k,4).and.mc(kk,k,5).eq.corda(i,3)) then

       comp1(kk)=mc(kk,k,2)
       comp2(kk)=mc(kk,k,3)

       end if

       end do

c      Origen
       xcorda(i,1)=x2line(corda(i,1),1,int(comp2(corda(i,1))))
       ycorda(i,1)=y2line(corda(i,1),1,int(comp2(corda(i,1))))
       zcorda(i,1)=z2line(corda(i,1),1,int(comp2(corda(i,1))))

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c       write (*,*) ">>> ", xcorda(i,1), ycorda(i,1),zcorda(i,1)
                    
       dist=dsqrt((xcorda(i,3)-xcorda(i,1))**2.0d0+(ycorda(i,3)-
     + ycorda(i,1))**2.0d0+(zcorda(i,3)-zcorda(i,1))**2.0d0)

       cdl=(xcorda(i,3)-xcorda(i,1))/dist
       cdm=(ycorda(i,3)-ycorda(i,1))/dist
       cdn=(zcorda(i,3)-zcorda(i,1))/dist

c      Parametre necesari a la distancia objectiu

       d13=dsqrt((xcorda(i,3)-xcorda(i,1))**2.0d0+(ycorda(i,3)-
     + ycorda(i,1))**2.0d0+(zcorda(i,3)-zcorda(i,1))**2.0d0)
       
       t=(d13-raml(5,3))/(dsqrt(cdl*cdl+cdm*cdm+cdn*cdn))

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

       if (corda(i,5).eq.4) then ! Tres nivells adicionals

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      REVISAT 2013-05-03 !!!!!!!!!!!!!!!!!!

c      Explora mc i troba quin origen emprar
       kk=slp+1
       do k=1,cam(kk)

       if (corda(i,2).eq.mc(kk,k,4).and.mc(kk,k,5).eq.corda(i,3)) then

       comp1(kk)=mc(kk,k,2)
       comp2(kk)=mc(kk,k,3)

       end if

       end do

c      Origen
       xcorda(i,1)=x2line(corda(i,1),1,int(comp2(corda(i,1))))
       ycorda(i,1)=y2line(corda(i,1),1,int(comp2(corda(i,1))))
       zcorda(i,1)=z2line(corda(i,1),1,int(comp2(corda(i,1))))


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c       xcorda(i,1)=x2line(corda(i,1),corda(i,2),corda(i,3))
c       ycorda(i,1)=y2line(corda(i,1),corda(i,2),corda(i,3))
c       zcorda(i,1)=z2line(corda(i,1),corda(i,2),corda(i,3))
                    
       dist=dsqrt((xcorda(i,3)-xcorda(i,1))**2+(ycorda(i,3)-
     + ycorda(i,1))**2+(zcorda(i,3)-zcorda(i,1))**2)

       cdl=(xcorda(i,3)-xcorda(i,1))/dist
       cdm=(ycorda(i,3)-ycorda(i,1))/dist
       cdn=(zcorda(i,3)-zcorda(i,1))/dist

c      Parametre necesari a la distancia objectiu

       d13=dsqrt((xcorda(i,3)-xcorda(i,1))**2+(ycorda(i,3)-ycorda(i,1))
     + **2+(zcorda(i,3)-zcorda(i,1))**2)
       
       t=(d13-raml(6,3))/(dsqrt(cdl*cdl+cdm*cdm+cdn*cdn))

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
c      Level 3
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Inici al final de les cordes 2

       do i=cordam+1,cordat

       if (corda(i,2).eq.3) then

       if (corda(i,5).eq.3) then !directe a l'ancoratge

c      Explora mc i troba quin origen emprar
       kk=slp+1
       do k=1,cam(kk)

       if (corda(i,2).eq.mc(kk,k,6).and.mc(kk,k,7).eq.corda(i,3)) then

       comp1(kk)=mc(kk,k,4)
       comp2(kk)=mc(kk,k,5)

       end if

       end do

c      Origen
       xcorda(i,1)=x2line(corda(i,1),2,int(comp2(corda(i,1))))
       ycorda(i,1)=y2line(corda(i,1),2,int(comp2(corda(i,1))))
       zcorda(i,1)=z2line(corda(i,1),2,int(comp2(corda(i,1))))

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
       
       end if

c      Si hi ha quatre nivells
       if (corda(i,5).eq.4) then

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      LEVEL 2
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      LEVEL 3
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Explora mc i troba quin origen emprar
       kk=slp+1
       do k=1,cam(kk)

       if (corda(i,2).eq.mc(kk,k,6).and.mc(kk,k,7).eq.corda(i,3)) then

       comp1(kk)=mc(kk,k,4)
       comp2(kk)=mc(kk,k,5)

       end if
       
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

       d13=dsqrt((xcorda(i,3)-xcorda(i,1))**2.0d0+(ycorda(i,3)-
     + ycorda(i,1))**2.0d0+(zcorda(i,3)-zcorda(i,1))**2.0d0)
       
c       t=(raml(6,4)-dl3)/(sqrt(cdl*cdl+cdm*cdm+cdn*cdn))
       t=(raml(6,3)-raml(6,4))/(dsqrt(cdl*cdl+cdm*cdm+cdn*cdn))

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
c      Level 4
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Inici al final de les cordes 3

       do i=cordam+1,cordat

       if (corda(i,2).eq.4) then

       if (corda(i,5).eq.4) then !directe a l'ancoratge

c      Explora mc i troba quin origen emprar
       kk=slp+1
       do k=1,cam(kk)

       if (corda(i,2).eq.mc(kk,k,8).and.mc(kk,k,9).eq.corda(i,3)) then

       comp1(kk)=mc(kk,k,6)
       comp2(kk)=mc(kk,k,7)

       end if
       
       end do

c      Origen
       xcorda(i,1)=x2line(corda(i,1),3,int(comp2(corda(i,1))))
       ycorda(i,1)=y2line(corda(i,1),3,int(comp2(corda(i,1))))
       zcorda(i,1)=z2line(corda(i,1),3,int(comp2(corda(i,1))))

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
       
       end if

       end if

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     14.6 Longituds de les cordes
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      do i=cordam+1,cordat

      xx2=x2line(corda(i,1),corda(i,2),corda(i,3))
      xx1=x1line(corda(i,1),corda(i,2),corda(i,3))
      yy2=y2line(corda(i,1),corda(i,2),corda(i,3))
      yy1=y1line(corda(i,1),corda(i,2),corda(i,3))
      zz2=z2line(corda(i,1),corda(i,2),corda(i,3))
      zz1=z1line(corda(i,1),corda(i,2),corda(i,3))

      xline(i)=dsqrt((xx2-xx1)**2+(yy2-yy1)**2+(zz2-zz1)**2)

      xlide(i)=0.0d0
      xlifi(i)=xline(i)

      end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     14.7 Dibuixar brakes 2D
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do i=cordam+1,cordat

       x0=(1260.-160.)*xkf
       y0=(1800.+890.95)*xkf

       x00=1260.*xkf*float(corda(i,1)-1)

       if (i37c.eq.0) then ! Use colors by risers
       call line(x1line(corda(i,1),corda(i,2),corda(i,3))+x0+x00,
     + z1line(corda(i,1),corda(i,2),corda(i,3))+y0,
     + x2line(corda(i,1),corda(i,2),corda(i,3))+x0+x00,
     + z2line(corda(i,1),corda(i,2),corda(i,3))+y0,iccolor(6))
c     + z2line(corda(i,1),corda(i,2),corda(i,3))+y0,corda(i,1))
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

c      Dibuixa distribucio de frenat

       x0=(1260.+890.)*xkf
       y0=(1800.+1000.)*xkf

       xf=rib(nribss,2)/100.

       string1="BRAKE_DISTRIBUTION"
       string2="CENTER"
       string3="WING_TIP"

       call txt(x0,y0-100.*xkf,10.0d0,0.0d0,string1,7)
       call txt(x0,y0-80.*xkf,10.0d0,0.0d0,string2,7)
       call txt(x0+100.*xf,y0-80.*xkf,10.0d0,0.0d0,string3,7)


       call line(x0,y0,x0+rib(nribss,2),y0,4)

       do k=1,5
       call line(x0+bd(k,1)*xf,y0,x0+bd(k,1)*xf,y0-bd(k,2),1)

       write (xstring,'(F6.2)') bd(k,1)
       call txt(x0+bd(k,1)*xf-20.,y0+30.*xkf,10.0d0,0.0d0,xstring,7)

       write (xstring,'(F6.2)') bd(k,2)
       call txt(x0+bd(k,1)*xf-20.,y0-50.*xkf,10.0d0,0.0d0,xstring,7)

       end do

       do k=1,4
       call line(x0+bd(k,1)*xf,y0-bd(k,2),x0+bd(k+1,1)*xf,
     + y0-bd(k+1,2),iccolor(6))
       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      14.8 Set more lines properties by types
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Compute lengths of each type
       do k=1,k34n  ! in all types
       lentype(k)=0.0d0 ! initial lenght of type k
       lentypewl(k)=0.0d0 ! initial lenght of type k without loops

c      Line lengths by types
       do i=1,cordat
       corda(i,40)=corda(i,30+corda(i,2)) ! Type on line i
       if (corda(i,40).eq.jltype(k)) then
       lentype(k)=lentype(k)+xlifi(i)+2.*tyline(k,9)
       lentypewl(k)=lentypewl(k)+xlifi(i)
       end if
c       write (*,*) i, corda(i,40),jltype(k),xlifi(i),lentype(k),
c     + lentypewl(k)
       end do

       end do ! k type

c      Total lengths
       xlength1=0.0d0 ! with loops
       xlength2=0.0d0 ! witout loops

c      Exclude risers
       do k=2,k34n  
       xlength1=xlength1+lentype(k)
       xlength2=xlength2+lentypewl(k)
       end do   
       xlength1=xlength1/50. ! x 2 and meters with loops
       xlength2=xlength2/50. ! x 2 and meters without loops

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      14.9 More lines properties
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Define some lines properties

c      Initialize values
       do i=1,nribss
       do k=0,50
       xlimo(i,k)=0.0d0
       end do
       end do

       do i=1,cordat
       xlimo(i,0)=xlifi(i)  ! Lenght without loops cm
       xlimo(i,1)=xlifi(i)+2.*tyline(corda(i,40),9) ! with loops cm
       xlimo(i,2)=(xlimo(i,1)/100.)*tyline(corda(i,40),7) ! weight g
       xlimo(i,3)=xlimo(i,1)*10.*tyline(corda(i,40),2) ! area mm2
       xlimo(i,30)=0.0d0 ! area reduction
       if (corda(i,2).eq.1.and.tylinea(corda(i,40),1).eq."r") then ! case r don't count loops
       xlimo(i,3)=xlimo(i,0)*10.*tyline(corda(i,40),2) ! area mm2
       end if
c      Case spliced loops
c      According experiences by E. Fontaine, the equivalent diameter on spliced part is D*sqrt(2.)
c      Then reduce total line area in xlimo(i,30)=2*D*lloop*(sqrt(2.)-2)
       if (tylinea(corda(i,40),8).eq."p".and.
     + tylinea(corda(i,40),1).ne."r") then ! reduce line frontal area
       xlimo(i,30)=2.*tyline(corda(i,40),2)*
     + 10.*tyline(corda(i,40),9)*(dsqrt(2.0d0)-2.0d0)
       xlimo(i,3)=xlimo(i,3)+xlimo(i,30) ! Spliced reduction
       end if
       xlimo(i,10)=(xcorda(i,1)+xcorda(i,2))/2. ! cdg x cm
       xlimo(i,11)=(ycorda(i,1)+ycorda(i,2))/2. ! cdg y cm
       xlimo(i,12)=(zcorda(i,1)+zcorda(i,2))/2. ! cdg z cm
       end do

c      Risers weight kg
       xlimog(14)=0.0d0 ! risers weight
       do i=1,cordam
       if (corda(i,2).eq.1) then
       xlimog(14)=xlimog(14)+(xlimo(i,1)/100.)*tyline(corda(i,40),7)
       end if
       end do
       xlimog(14)=xlimog(14)/500. ! kg both sides

c      Risers frontal surface
       xlimog(15)=0.0d0 ! risers surface
       do i=1,cordam
       if (corda(i,2).eq.1) then
       if (tylinea(corda(i,40),1).eq."c") then ! cylindrical risers count loops
       xlimog(15)=xlimog(15)+(xlimo(i,1)*10.)*tyline(corda(i,40),2)
       end if
       if (tylinea(corda(i,40),1).eq."r") then ! rectangular risers not count loops
       xlimog(15)=xlimog(15)+(xlimo(i,0)*10.)*tyline(corda(i,40),2)
c       write (*,*) i,xlimo(i,0),tyline(corda(i,40),2)
       end if
       end if
       end do
       xlimog(15)=2.*xlimog(15)/1000000. ! m2
       
c      Compute derived properties
       xlimog(2)=0.0d0 ! Lines weight (g)
       xlimog(3)=0.0d0 ! Lines area (m2)
       xlimog(10)=0.0d0 ! Lines x mass center
       xlimog(11)=0.0d0 ! Lines y mass center
       xlimog(12)=0.0d0 ! Lines z mass center

       do i=1,cordat
       xlimog(2)=xlimog(2)+2.*xlimo(i,2) ! weight g
       xlimog(3)=xlimog(3)+2.*xlimo(i,3) ! area mm2
       xlimog(10)=xlimog(10)+xlimo(i,10)*xlimo(i,2) ! x semiwing
       xlimog(11)=xlimog(11)+xlimo(i,11)*xlimo(i,2) ! y semiwing
       xlimog(12)=xlimog(12)+xlimo(i,12)*xlimo(i,2) ! z semiwing
       end do
       xlimog(10)=0.0d0                   ! cm
       xlimog(11)=2.*xlimog(11)/xlimog(2) ! cm
       xlimog(12)=2.*xlimog(12)/xlimog(2) ! cm
       xlimog(2)=xlimog(2)/1000.    ! total lines weight kg inc risers
       xlimog(3)=xlimog(3)/1000000. ! total lines area m2 inc risers

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Lines and risers drag coefficients
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      x35ro = air mass density 1.225 kg/m3
c      x35v  = speed m/s
c      x35mu = air dynamic viscosity 18.46 x 1e-6 Pascals·s

c      Reynolds numbers by types tyline(k,20)
       do k=1,k34n
       tyline(k,20)=(x35ro*x35v*tyline(k,2)*1e-3)/(x35mu*1e-6)
       end do

c      Reynolds number operating in each line xlimo(1,20)
       do i=1,cordat
       xlimo(i,20)=tyline(corda(i,40),20)
       end do

c      Drag coefficient in each line xlimo(1,21)
c      Weisener and its simple equation for a cylinder
c      Source: De Villiers, HEGALA v6 report
       do i=1,cordat
       xlimo(i,21)=(10.*xlimo(i,20)**(-2./3.))+1.0d0
c      Correction for rectangular risers, increment Cd
       if (corda(i,2).eq.1.and.tylinea(corda(i,40),1).eq."r") then
       xlimo(i,21)=1.98
       end if
       end do

c      Global line drag coeffcient
       x35dlcoef=0.0d0
       do i=1,cordat
       x35dlcoef=x35dlcoef+(xlimo(i,3)*1e-6)*xlimo(i,21)
       end do
       x35dlcoef=x35dlcoef/(0.5*xlimog(3)) ! use half line area
       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
