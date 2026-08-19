c     11. CALCULA LONGITUDS EXTRA INTRA EN PANELLS I PERFILS. MARKS
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     WARNING: Additional vent marks in section 8.5 (!)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
c        write (*,*) "pi 11. =",pi

c      11.1 Extrados

       do i=1,nribss

       rib(i,30)=0. ! extra panel left
       rib(i,31)=0. ! extra rib
       rib(i,32)=0. ! extra panel right
       rib(i,33)=0. ! intra panel left
       rib(i,34)=0. ! intra rib
       rib(i,35)=0. ! intra panel right

c      Compute lengths extrados
       do j=1,np(i,2)-1

c      WARNING longituds a dreta i esquerra de la costella extrados!
c      Arreglat amb el vector 29 definit mes enrera

c       rib(i,30)=rib(i,30)+sqrt((u(i-1,j,29)-u(i-1,j+1,29))**2.+
c     + ((v(i-1,j,29)-v(i-1,j+1,29))**2.))

c      ATENCIO, veure si compatible amb cas reformat especial

       rib(i,30)=rib(i,30)+sqrt((u(i-1,j,10)-u(i-1,j+1,10))**2.+
     + ((v(i-1,j,10)-v(i-1,j+1,10))**2.))

       rib(i,31)=rib(i,31)+sqrt((u(i,j,3)-u(i,j+1,3))**2.+((v(i,j,3)
     + -v(i,j+1,3))**2.))

       rib(i,32)=rib(i,32)+sqrt((u(i,j,9)-u(i,j+1,9))**2.+((v(i,j,9)
     + -v(i,j+1,9))**2.))

       end do

c      Compute lengths intrados
       do j=np(i,2)+np(i,3),np(i,1)-1

       rib(i,33)=rib(i,33)+sqrt((u(i-1,j,10)-u(i-1,j+1,10))**2.+
     + ((v(i-1,j,10)-v(i-1,j+1,10))**2.))

       rib(i,34)=rib(i,34)+sqrt((u(i,j,3)-u(i,j+1,3))**2.+((v(i,j,3)
     + -v(i,j+1,3))**2.))

       rib(i,35)=rib(i,35)+sqrt((u(i,j,9)-u(i,j+1,9))**2.+((v(i,j,9)
     + -v(i,j+1,9))**2.))

       end do

c      Amplification cofficients
       rib(i,36)=rib(i,30)/rib(i,31)
       rib(i,37)=rib(i,32)/rib(i,31)
       rib(i,38)=rib(i,33)/rib(i,34)
       rib(i,39)=rib(i,35)/rib(i,34)

       rib(0,36)=rib(1,36)
       rib(0,37)=rib(1,36)
       rib(0,38)=rib(1,38)
       rib(0,39)=rib(1,38)

c       write (*,*) "rib(i,36) ",i, rib(i,36), rib(i,30)

       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     AND COMPUTE AGAIN!!! "AMPLICATION" COEFFICIENTS
c     in new vectors rib(i,190) to rib(i,201)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Initilize vectors
       do i=0,nribss-1
       rib(i,190)=0. ! rib extra i
       rib(i,191)=0. ! panel extra left i
       rib(i,192)=0. ! rib extra i+1
       rib(i,193)=0. ! panel extra right i
       rib(i,194)=0. ! k 191/190
       rib(i,195)=0. ! k 193/192
       rib(i,196)=0. ! rib intra i
       rib(i,197)=0. ! panel intra left i
       rib(i,198)=0. ! rib intra i+1
       rib(i,199)=0. ! panel intra right i
       rib(i,200)=0. ! k 197/196
       rib(i,201)=0. ! k 199/198
       end do

       do i=0,nribss-1 ! for all ribs at left

c      A. Compute lengths extrados
       do j=1,np(i,2)-1
c      WARNING longituds a dreta i esquerra de la costella extrados!
c      Arreglat amb el vector 29 definit mes enrera
c       rib(i,30)=rib(i,30)+sqrt((u(i-1,j,29)-u(i-1,j+1,29))**2.+
c     + ((v(i-1,j,29)-v(i-1,j+1,29))**2.))
c      ATENCIO, veure si compatible amb cas reformat especial
       rib(i,190)=rib(i,190)+sqrt((u(i,j,3)-u(i,j+1,3))**2.+((v(i,j,3)
     + -v(i,j+1,3))**2.))
       rib(i,191)=rib(i,191)+sqrt((u(i,j,9)-u(i,j+1,9))**2.+((v(i,j,9)
     + -v(i,j+1,9))**2.))
       rib(i,192)=rib(i,192)+sqrt((u(i+1,j,3)-u(i+1,j+1,3))**2.+
     + ((v(i+1,j,3)-v(i+1,j+1,3))**2.))
       rib(i,193)=rib(i,193)+sqrt((u(i,j,10)-u(i,j+1,10))**2.+
     + ((v(i,j,10)-v(i,j+1,10))**2.))
       end do
       rib(i,194)=rib(i,191)/rib(i,190) ! Extrados left
       rib(i,195)=rib(i,193)/rib(i,192) ! Extrados right

c       write (*,*) i,"extra ",rib(i,194),rib(i,195)

c      B. Compute lengths intrados
       do j=np(i,2)+np(i,3)-1,np(i,1)-1
       rib(i,196)=rib(i,196)+sqrt((u(i,j,3)-u(i,j+1,3))**2.+((v(i,j,3)
     + -v(i,j+1,3))**2.))
       rib(i,197)=rib(i,197)+sqrt((u(i,j,9)-u(i,j+1,9))**2.+((v(i,j,9)
     + -v(i,j+1,9))**2.))
       rib(i,198)=rib(i,198)+sqrt((u(i+1,j,3)-u(i+1,j+1,3))**2.+
     + ((v(i+1,j,3)-v(i+1,j+1,3))**2.))
       rib(i,199)=rib(i,199)+sqrt((u(i,j,10)-u(i,j+1,10))**2.+
     + ((v(i,j,10)-v(i,j+1,10))**2.))
       end do ! j
       rib(i,200)=rib(i,197)/rib(i,196) ! Intrados left
       rib(i,201)=rib(i,199)/rib(i,198) ! Intrados right

c       write (*,*) i,"intra ",rib(i,200),rib(i,201)

       end do ! i ribs


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      11.2 Comprovació de longituds
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c       do i=1,nribss

c       write (*,*) "Extrados ", i, rib(i,30), rib(i,31), rib(i,32) 

c       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
c      11.3 Dibuixa marques a costelles
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       sepxx=700.*xkf
       sepyy=100.*xkf

       kx=0
       ky=0
       kyy=0

c      Verify thickness of last rib
       
       xsum=0.
       ic=0
       i=nribss
       do j=1,np(i,1)
       xsum=xsum+abs(v(i,j,3))
       end do
       if (xsum.ne.0.0) then
       ic=1
       end if

ccccccccccccccccccccccccccccccccccccccccccccccc
c     11.3.1 Extrados
ccccccccccccccccccccccccccccccccccccccccccccccc

c      Iteration in ribs    
       do i=1,nribss-1+ic

       sepx=sepxx+seprix*float(kx)
       sepy=sepyy+sepriy*float(ky)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Punt TE
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       j=1

       xu=u(i,j,3)
       xv=v(i,j,3)

c      Despla a vores punts de control de costures
       alp=pi/2.
       if (xv.lt.0.) then
c       xu=xu+xdes*dsin(alp)
c       xv=xv-xdes*dcos(alp)
       end if
       if (xv.ge.0.) then
c       xu=xu+xdes*dsin(alp)
c       xv=xv-xdes*dcos(alp)
       end if

       call pointg(sepx+xu,sepy-xv,xcir,4)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Punts interiors
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Equidistant points in ribs extrados (print)

       do k=1,60

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

       rib(i,41)=rib(i,41)+sqrt((u(i,j,3)-u(i,j+1,3))**2.+((v(i,j,3)
     + -v(i,j+1,3))**2.))

       xpost=rib(i,41)

       if(xmk.lt.xpost.and.xmk.ge.xprev) then

c      dibuixa marca

       dist=dsqrt((u(i,j,3)-u(i,j+1,3))**2.+((v(i,j,3)
     + -v(i,j+1,3))**2.))

       dist1=xmk-xprev

       xu=u(i,j,3)+(u(i,j+1,3)-u(i,j,3))*(dist1/dist)
       xv=v(i,j,3)+(v(i,j+1,3)-v(i,j,3))*(dist1/dist)

c      Despla a vores punts de control de costures
       alp=(datan((v(i,j+1,3)-v(i,j,3))/(u(i,j+1,3)-u(i,j,3))))
       if (xv.lt.0.) then
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)
       end if
       if (xv.ge.0.) then
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)
       end if

       call pointg(sepx+xu,sepy-xv,xcir,3)

c      Dibuixa punt a les costelles de la taula de tall
c      2530 per ajustar a BOX(1,4)
       call point(2530.*xkf+sepx+xu,sepy-xv,7)
       
       end if

       end do ! j extrados

c      new
       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      11.3.2 Intrados
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Punts interiors

       do k=1,60

c      Partial lengths init

       rib(i,40)=0.
       rib(i,41)=0.
       rib(i,42)=0.
       rib(i,43)=0.
       rib(i,44)=0.
       rib(i,45)=0.

       xmk=xmark*float(k)

c      Comprova si el parapent es tipus "ds" or "pc"
c      If type "ss" not needed

       if (atp.eq."ds".or.atp.eq."pc") then

       do j=np(i,1),np(i,2)+np(i,3),-1

       xprev=rib(i,44)

       rib(i,44)=rib(i,44)+sqrt((u(i,j,3)-u(i,j-1,3))**2.+((v(i,j,3)
     + -v(i,j-1,3))**2.))

       xpost=rib(i,44)

       if(xmk.lt.xpost.and.xmk.ge.xprev) then

c      dibuixa marca

       dist=dsqrt((u(i,j,3)-u(i,j-1,3))**2.+((v(i,j,3)
     + -v(i,j-1,3))**2.))

       dist1=xmk-xprev

       xu=u(i,j,3)+(u(i,j-1,3)-u(i,j,3))*(dist1/dist)
       xv=v(i,j,3)+(v(i,j-1,3)-v(i,j,3))*(dist1/dist)

c      Despla a vores punts de control de costures
       alp=(datan((v(i,j,3)-v(i,j-1,3))/(u(i,j,3)-u(i,j-1,3))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)

       call pointg(sepx+xu,sepy-xv,xcir,3)

c      Dibuixa punt a les costelles de la taula de tall
c      2530 per ajustar a BOX(1,4)
       call point(2530.*xkf+sepx+xu,sepy-xv,7)
     
       end if

       end do ! j intrados

c      Final verificació ds
       end if

       end do ! k

       kx=int((float(i)/6.))
       ky=i-kx*6
       kyy=kyy+1
       
       end do  ! i

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      11.4 Panels marks
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      11.4.1 Extrados panels mark 
c      Case classic
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (n1draw.eq.1) then ! Draw marks classic

       do i=0,nribss-1
       
       psep=1970.*xkf+seppix(i)*1.0d0
       psey=400.*xkf

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      11.4.1.2 Marks extrados left  
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Initial and final points

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Initial point
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       j=1

       xu=u(i,j,9)
       xv=v(i,j,9)

c      Despla a vores punts de control de costures
       alp=abs(datan((v(i,j+1,9)-v(i,j,9))/(u(i,j+1,9)-u(i,j,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,4)

c      Point laser       
       call point(psep+xu+2520.*xkf,-xv+psey,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Final point
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       j=np(i,2)

       xu=u(i,j,9)
       xv=v(i,j,9)

c      Despla a vores punts de control de costures
       alp=abs(datan((v(i,j-1,9)-v(i,j,9))/(u(i,j-1,9)-u(i,j,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,4)

c      Point laser      
       call point(psep+xu+2520.*xkf,-xv+psey,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Interior points
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do k=1,60

c      Partial lengths init

       rib(i,40)=0.
       rib(i,41)=0.
       rib(i,42)=0.
       rib(i,43)=0.
       rib(i,44)=0.
       rib(i,45)=0.

       xmk=xmark*float(k)

       xmk=xmk*rib(i,36) ! amplificacio de segment
      
c      Dibuixa a extrados left     

       do j=1,np(i,2)-1

       xprev=rib(i,40)

       rib(i,40)=rib(i,40)+sqrt((u(i,j,9)-u(i,j+1,9))**2.+((v(i,j,9)
     + -v(i,j+1,9))**2.))

       xpost=rib(i,40)

       if(xmk.le.xpost.and.xmk.ge.xprev) then

c      dibuixa marca

       dist=dsqrt((u(i,j,9)-u(i,j+1,9))**2.+((v(i,j,9)
     + -v(i,j+1,9))**2.))

       dist1=xmk-xprev

       xu=u(i,j,9)+(u(i,j+1,9)-u(i,j,9))*(dist1/dist)
       xv=v(i,j,9)+(v(i,j+1,9)-v(i,j,9))*(dist1/dist)

c      Despla a vores punts de control de costures
       alp=abs(datan((v(i,j+1,9)-v(i,j,9))/(u(i,j+1,9)-u(i,j,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,3)

c      Point laser
       call point(psep+xu+2520.*xkf,-xv+psey,7)
       
       end if

       end do ! j extrados left

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      11.4.1.3 Marks Panel extrados right
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Interiors
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       xmk=xmark*float(k)

       xmk=xmk*rib(i,37) ! amplificacio de segment

c      Dibuixa a extrados right     

       do j=1,np(i,2)-1

       xprev=rib(i,42)

       rib(i,42)=rib(i,42)+sqrt((u(i,j,10)-u(i,j+1,10))**2.+((v(i,j,10)
     + -v(i,j+1,10))**2.))

       xpost=rib(i,42)

       if(xmk.le.xpost.and.xmk.ge.xprev) then

c      dibuixa marca

       dist=dsqrt((u(i,j,10)-u(i,j+1,10))**2.+((v(i,j,10)
     + -v(i,j+1,10))**2.))

       dist1=xmk-xprev

       xu=u(i,j,10)+(u(i,j+1,10)-u(i,j,10))*(dist1/dist)
       xv=v(i,j,10)+(v(i,j+1,10)-v(i,j,10))*(dist1/dist)

c      Despla a vores punts de control de costures
       alp=abs(datan((v(i,j+1,10)-v(i,j,10))/(u(i,j+1,10)-u(i,j,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,3)

c      Point laser     
       call point(psep+xu+2520.*xkf,-xv+psey,7)
       
       end if

       end do ! j extrados right

       end do ! k

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Initial and final mark points outside the k-60-loop
c      Note by Pawel 20190507
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Initial point
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       j=1

       xu=u(i,j,10)
       xv=v(i,j,10)

c      Despla a vores punts de control de costures
       alp=abs(datan((v(i,j+1,10)-v(i,j,10))/(u(i,j+1,10)-u(i,j,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,4)

c      Point laser       
       call point(psep+xu+2520.*xkf,-xv+psey,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Final point
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       j=np(i,2)

       xu=u(i,j,10)
       xv=v(i,j,10)

c      Despla a vores punts de control de costures
       alp=abs(datan((v(i,j-1,10)-v(i,j,10))/(u(i,j-1,10)-u(i,j,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)

       call pointg(psep+xu,psey-xv,xcir,4)

c      Point laser       
       call point(psep+xu+2520.*xkf,-xv+psey,7)

       end do ! i extrados points

       end if ! n1draw points extrados right (and left?) 








ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Marks in case k29d = 1
       if (k29d.eq.1) then

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      11.4.1-3D Extrados panel marks
c      Case 3D
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
           
       do i=iini,nribss-1

       ic2=dint(rib(i+1,165)) ! vent type
       
       psep=1970.*xkf+seppix(i)*1.0d0
       psey=xyshift*xkf-890.95*xyextra*xkf ! remove 890.95 to set BOX (1,3)

c      REDEFINITION (!) for proper printing
       ng=rib(i+1,169)

       if (i.eq.0) then
       ng=rib(1,169)
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      11.4.1.2-3D Marks
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Extrados points, case 3D, initial and final left and right
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Falta els punts dels vents!

       do j=1,npo
c       write (*,*) i,j,j9,j10,xcir,xdes
       end do
     
c      Case 0 cuts
       if (uppcuts(ng).eq.0) then
       npi=1
       npf=np(i,2)
       call prinifp(i,npi,npf,u,v,psep,psey,xcir,xdes,xkf)
       end if

c      Case 1 cut
       if (uppcuts(ng).eq.1) then
       npi=1
       npf=iupp(1,3,ng)
       call prinifp(i,npi,npf,u,v,psep,psey,xcir,xdes,xkf)
       npi=iupp(1,3,ng)
       npf=np(i,2)
       call prinifp(i,npi,npf,u,v,psep,psey+ysaut,xcir,xdes,xkf)
       end if

c      Case 2 cuts
       if (uppcuts(ng).eq.2) then
       npi=1
       npf=iupp(1,3,ng)
       call prinifp(i,npi,npf,u,v,psep,psey,xcir,xdes,xkf)
       npi=iupp(1,3,ng)
       npf=iupp(2,3,ng)
       call prinifp(i,npi,npf,u,v,psep,psey+ysaut,xcir,xdes,xkf)
       npi=iupp(2,3,ng)
       npf=np(i,2)
       call prinifp(i,npi,npf,u,v,psep,psey+ysaut*2.,xcir,xdes,xkf)
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Intrados points, case 3D, initial and final left and right
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc






       end do ! i ribs


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      11.4.2-3D Intrados panel marks
c      Case 3D
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c      Plotter BOX(0,3) and BOX (0,5)

       if (atp.ne."ss") then

c      Avoid central panel if thickness is 0
       iini=0  ! panel 0 (central)
       if (cencell.lt.0.01) then
       iini=1
       end if

       do i=iini,nribss-1

       ic2=dint(rib(i+1,165)) ! vent type

       psep=1970.*xkf+seppix(i)*1.0d0
       psey=xyshift*xkf-890.95*xyintra*xkf ! remove 890.95 to set BOX (1,3)

c      REDEFINITION (!) for proper printing
       ng=rib(i+1,169)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Intrados points, case 3D, initial and final left and right
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Falta els punts dels vents!
     
c      Case 0 cuts
       if (lowcuts(ng).eq.0) then
       npi=np(i,2)+np(i,3)-1
       npf=np(i,1)
       call prinifp(i,npi,npf,u,v,psep,psey,xcir,xdes,xkf)
       end if

c      Case 1 cut
       if (lowcuts(ng).eq.1) then
       npi=np(i,2)+np(i,3)-1
       npf=ilow(1,3,ng)
       call prinifp(i,npi,npf,u,v,psep,psey,xcir,xdes,xkf)
       npi=ilow(1,3,ng)
       npf=np(i,1)
       call prinifp(i,npi,npf,u,v,psep,psey+ysaut,xcir,xdes,xkf)
       end if

       end do ! i ribs

       end if ! "ss"


       end if ! k29d = 1













ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      11.4.2 Intrados panel marks
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (n1draw.eq.1) then ! draw

c      Control if type is not "ss"
       if (atp.ne."ss") then

       do i=0,nribss-1
       
       psep=1970.*xkf+seppix(i)*1.0d0
       psey=1291.*xkf

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      11.4.2.1 Intrados panel marks Left
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Initial
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       j=np(i,2)+np(i,3)-1

       xu=u(i,j,9)
       xv=v(i,j,9)

c      Despla a vores punts de control de costures
       alp=abs(datan((v(i,j,9)-v(i,j+1,9))/(u(i,j,9)-u(i,j+1,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,4)

c      Point laser       
       call point(psep+xu+2520.*xkf,-xv+psey,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Final
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       j=np(i,1)

       xu=u(i,j,9)
       xv=v(i,j,9)

c      Despla a vores punts de control de costures
       alp=abs(datan((v(i,j-1,9)-v(i,j,9))/(u(i,j-1,9)-u(i,j,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,4)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Point laser       
       call point(psep+xu+2520.*xkf,-xv+psey,7)
c      Per calcular angle vora de fuga:
       xlll=psep+xu
       ylll=-xv+psey
c      See lep-2.73 to undestand use of thesse variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Interior
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do k=1,60

c      Partial lengths init

       rib(i,40)=0.
       rib(i,41)=0.
       rib(i,42)=0.
       rib(i,43)=0.
       rib(i,44)=0.
       rib(i,45)=0.

       xmk=xmark*float(k)

       xmk=xmk*rib(i,38) ! amplificacio de segment
      
c      Dibuixa a intrados left     

       do j=np(i,1),np(i,2)+np(i,3),-1

       xprev=rib(i,43)

       rib(i,43)=rib(i,43)+sqrt((u(i,j,9)-u(i,j-1,9))**2.+((v(i,j,9)
     + -v(i,j-1,9))**2.))

       xpost=rib(i,43)

       if(xmk.le.xpost.and.xmk.ge.xprev) then

c      dibuixa marca

       dist=dsqrt((u(i,j,9)-u(i,j-1,9))**2.+((v(i,j,9)
     + -v(i,j-1,9))**2.))

       dist1=xmk-xprev

       xu=u(i,j,9)+(u(i,j-1,9)-u(i,j,9))*(dist1/dist)
       xv=v(i,j,9)+(v(i,j-1,9)-v(i,j,9))*(dist1/dist)

c      Despla a vores punts de control de costures
       alp=abs(datan((v(i,j-1,9)-v(i,j,9))/(u(i,j-1,9)-u(i,j,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,3)

c      Point laser      
       call point(psep+xu+2520.*xkf,-xv+psey,7)
       
       end if

       end do ! j intrados left

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      11.4.2.2 Marks Panel intrados right
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Interior
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       xmk=xmark*float(k)

       xmk=xmk*rib(i,39) ! amplificacio de segment

c      Dibuixa a intrados right     

       do j=np(i,1),np(i,2)+np(i,3),-1

       xprev=rib(i,45)

       rib(i,45)=rib(i,45)+sqrt((u(i,j,10)-u(i,j-1,10))**2.+((v(i,j,10)
     + -v(i,j-1,10))**2.))

       xpost=rib(i,45)

       if(xmk.le.xpost.and.xmk.ge.xprev) then

c      dibuixa marca

       dist=dsqrt((u(i,j,10)-u(i,j-1,10))**2.+((v(i,j,10)
     + -v(i,j-1,10))**2.))

       dist1=xmk-xprev

       xu=u(i,j,10)+(u(i,j-1,10)-u(i,j,10))*(dist1/dist)
       xv=v(i,j,10)+(v(i,j-1,10)-v(i,j,10))*(dist1/dist)

c      Despla a vores punts de control de costures
       alp=abs(datan((v(i,j-1,10)-v(i,j,10))/(u(i,j-1,10)-u(i,j,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,3)

c      Point laser       
       call point(psep+xu+2520.*xkf,-xv+psey,7)
       
       end if

       end do ! j intrados right

       end do ! k

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Initial and final mark points outside the k-60-loop
c      Note by Pawel 20190507
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Initial
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       j=np(i,2)+np(i,3)-1

       xu=u(i,j,10)
       xv=v(i,j,10)

c      Despla a vores punts de control de costures
       alp=abs(datan((v(i,j,10)-v(i,j+1,10))/(u(i,j,10)-u(i,j+1,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,4)

c      Point laser       
       call point(psep+xu+2520.*xkf,-xv+psey,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Final
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       j=np(i,1)

       xu=u(i,j,10)
       xv=v(i,j,10)

c      Despla a vores punts de control de costures
       alp=abs(datan((v(i,j-1,10)-v(i,j,10))/(u(i,j-1,10)-u(i,j,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)

c      Point imp
       call pointg(psep+xu,psey-xv,xcir,4)
       call point(psep+xu+2520.*xkf,-xv+psey,7)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccccccccccccccccc
c      11.4.2.4 Roman numbers in intrados
c      Dibuixa marques romanes AD
cccccccccccccccccccccccccccccccccccccccccccccccccccc

c      xlll, ylll computed above       

       xrrr=psep+xu
       yrrr=-xv+psey
       alprom=abs(atan((yrrr-ylll)/(xrrr-xlll)))

       call romano(i+1,psep-5.*dcos(alprom)+xu+2520.*xkf, 
     + -xv+psey-0.7*dcos(alprom)-5.*dsin(alprom),pi-alprom,
     + typm6(8)*0.1,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      11.4.3 Anchor points mark in intrados (Experimental)
c      2015-09-06 Request by Scott
c      2018-01-20 Request by Nicolas
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Draw intrados marks
       icontrolmi=1

       if (icontrolmi.eq.1) then

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Left side
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do klz=1,rib(i,15) ! rib(i,15)=anchors number

       xlen=0.
       j=np(i,1)
       xlenp=dsqrt((v(i,j,9)-v(i,j-1,9))**2.+(u(i,j,9)-u(i,j-1,9))**2.)

       do j=np(i,1),np(i,2)+1,-1

c      Detect and draw anchor point
       if (rib(i,130+klz).ge.xlen.and.rib(i,130+klz).lt.xlenp) then

c       write (*,*) "Anchors ",i,j,klz,xlen,rib(i,130+klz),xlenp

       rib(i,107)=rib(i,130+klz)-xlen
       rib(i,108)=dsqrt((v(i,j,9)-v(i,j-1,9))**2.+(u(i,j,9)-u(i,j-1,9))
     + **2.)

c      Interpolate
       xequis=u(i,j,9)-(rib(i,107)*(u(i,j,9)-u(i,j-1,9)))/
     + rib(i,108)
       yequis=v(i,j,9)-(rib(i,107)*(v(i,j,9)-v(i,j-1,9)))/
     + rib(i,108)

c      Define anchor points in planar panel
       xanchoril(i,klz)=xequis
       yanchoril(i,klz)=yequis

c      Draw
       xdu=u(i,j,9)-u(i,j-1,9)
       xdv=v(i,j,9)-v(i,j-1,9)

       if (xdu.ne.0) then
       alpha=-(datan(xdv/xdu))
       end if
       if (xdu.eq.0.) then
       alpha=pi/2.
       end if
       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case imp
c      Line 4*xrib in plotting panels
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Case all
       call line(psep+xequis,psey-yequis,psep+xequis-0.4*xrib*
     + dsin(-alpha),psey-yequis-0.4*xrib*dcos(-alpha),30)
       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case laser
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       xpeq=xequis+1.*xdes*dsin(-alpha)
       ypeq=yequis-1.*xdes*dcos(-alpha)

       xdesp=1.0*(0.5*(xrib-20.*xdes))/10.
c       xdesp=typm6(5)
c      REVISAR

       xdesp1x=xdesp*dsin(-alpha)
       xdesp1y=-xdesp*dcos(-alpha)
       xdesp2x=2.*xdesp*dsin(-alpha)
       xdesp2y=-2.*xdesp*dcos(-alpha)

c      Case 1: classic 3 orange points
       if (typm4(5).eq.1) then
       call point (psep+xpeq+2520*xkf,psey-ypeq,30)
       call point (psep+xpeq+xdesp1x+2520*xkf,psey-ypeq-xdesp1y,30)
       call point (psep+xpeq+xdesp2x+2520*xkf,psey-ypeq-xdesp2y,30)
       end if

c      Case 2: Controled 3 orange points
       if (typm4(5).eq.2) then
       xpeq=xequis+1.*typm6(5)*dsin(-alpha)
       ypeq=yequis-1.*typm6(5)*dcos(-alpha)
       xdesp1x=typm5(5)*dsin(-alpha)
       xdesp1y=-typm5(5)*dcos(-alpha)
       xdesp2x=2.*typm5(5)*dsin(-alpha)
       xdesp2y=-2.*typm5(5)*dcos(-alpha)
       call point (psep+xpeq+2520*xkf,psey-ypeq,30)
       call point (psep+xpeq+xdesp1x+2520*xkf,psey-ypeq-xdesp1y,30)
       call point (psep+xpeq+xdesp2x+2520*xkf,psey-ypeq-xdesp2y,30)
       end if

c      Case 3: triangle h mm
       if (typm4(5).eq.3) then
       xpeq=xequis-typm6(5)*dsin(alpha)
       ypeq=yequis+typm6(5)*dcos(alpha)
       call mtriangle(psep+xpeq+2520*xkf,psey-ypeq,typm5(5),-alpha,1)
       end if

       end if

       xlen=xlen+sqrt((v(i,j,9)-v(i,j-1,9))**2.+(u(i,j,9)-u(i,j-1,9))
     + **2.)
       xlenp=xlen+sqrt((v(i,j-1,9)-v(i,j-2,9))**2.+
     + (u(i,j-1,9)-u(i,j-2,9))**2.)

       end do ! j

       end do ! klz


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Right side
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do klz=1,rib(i+1,15)

       xlen=0.
       j=np(i,1)
       xlenp=dsqrt((v(i,j,10)-v(i,j-1,10))**2.+(u(i,j,10)-u(i,j-1,10))
     + **2.)

       do j=np(i,1),np(i,2)+1,-1

c      Detect and draw anchor point
       if (rib(i+1,130+klz).ge.xlen.and.rib(i+1,130+klz).lt.xlenp) then

c       write (*,*) "Anchors ",i,j,klz,xlen,rib(i,130+klz),xlenp

       rib(i+1,107)=rib(i+1,130+klz)-xlen
       rib(i+1,108)=dsqrt((v(i,j,10)-v(i,j-1,10))**2.+(u(i,j,10)-
     + u(i,j-1,10))**2.)

c      Interpolate
       xequis=u(i,j,10)-(rib(i+1,107)*(u(i,j,10)-u(i,j-1,10)))/
     + rib(i+1,108)
       yequis=v(i,j,10)-(rib(i+1,107)*(v(i,j,10)-v(i,j-1,10)))/
     + rib(i+1,108)

c      Define anchor points in planar panel
       xanchorir(i,klz)=xequis
       yanchorir(i,klz)=yequis

c      Draw
       alpha=-(datan((v(i,j,10)-v(i,j-1,10))/(u(i,j,10)-u(i,j-1,10))))

       if (alpha.lt.0.) then
       alpha=alpha+pi
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case imp
c      Line 4*xrib in plotting panels
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call line(psep+xequis,psey-yequis,psep+xequis+0.4*xrib*
     + dsin(-alpha),psey-yequis+0.4*xrib*dcos(-alpha),30)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case laser
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       xpeq=xequis-1.*xdes*dsin(-alpha)
       ypeq=yequis+1.*xdes*dcos(-alpha)

       xdesp=1.0*(0.5*(xrib-20.*xdes))/10.
c      REVISAR
c       xdesp=typm6(5)

       xdesp1x=xdesp*dsin(-alpha)
       xdesp1y=-xdesp*dcos(-alpha)
       xdesp2x=2.*xdesp*dsin(-alpha)
       xdesp2y=-2.*xdesp*dcos(-alpha)

c      Case 1: classic 3 orange points
       if (typm4(5).eq.1) then
       call point (psep+xpeq+2520*xkf,psey-ypeq,30)
       call point (psep+xpeq-xdesp1x+2520*xkf,psey-ypeq+xdesp1y,30)
       call point (psep+xpeq-xdesp2x+2520*xkf,psey-ypeq+xdesp2y,30)
       end if

c      Case 2: controled 3 orange points
       if (typm4(5).eq.2) then
       xpeq=xequis+1.*typm6(5)*dsin(-alpha)
       ypeq=yequis-1.*typm6(5)*dcos(-alpha)
       xdesp1x=typm5(5)*dsin(-alpha)
       xdesp1y=-typm5(5)*dcos(-alpha)
       xdesp2x=2.*typm5(5)*dsin(-alpha)
       xdesp2y=-2.*typm5(5)*dcos(-alpha)
       call point (psep+xpeq+2520*xkf,psey-ypeq,30)
       call point (psep+xpeq-xdesp1x+2520*xkf,psey-ypeq+xdesp1y,30)
       call point (psep+xpeq-xdesp2x+2520*xkf,psey-ypeq+xdesp2y,30)
       end if

c      Case 3: triangle 2 mm
       if (typm4(5).eq.3) then
       xpeq=xequis-typm6(5)*dsin(-alpha)
       ypeq=yequis+typm6(5)*dcos(-alpha)
       call mtriangle(psep+xpeq+2520*xkf,psey-ypeq,typm5(5),-alpha+pi,1)
       end if

       end if

       xlen=xlen+dsqrt((v(i,j,10)-v(i,j-1,10))**2.+(u(i,j,10)-
     + u(i,j-1,10))**2.)
       xlenp=xlen+dsqrt((v(i,j-1,10)-v(i,j-2,10))**2.+
     + (u(i,j-1,10)-u(i,j-2,10))**2.)

       end do ! j

       end do ! klz

       end if ! icontrol-mi

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       end do ! i

c      End if control is not "ss"
       end if

       end if ! n1draw




cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
