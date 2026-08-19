!> Offset a closed airfoil-like 2D contour using the original side-selection rules.
!! @param[in,out] uo Horizontal coordinates; column 1 is the source and column 2
!!                   receives the offset contour.
!! @param[in,out] vo Vertical coordinates; column 1 is the source and column 2
!!                   receives the offset contour.
!! @param[in] npo Number of valid source points.
!! @param[in] xo Offset magnitude, scaled internally by 0.1.
!! @note Offset direction depends on the sign of V and local X progression.
       SUBROUTINE loffset(uo,vo,npo,xo)

       real*8 uo(0:500,2),vo(0:500,2)
       real*8 xo,alpha1,alpha2,alpha

       do j=1,npo

c      Angle alpha
       if (j.eq.1) then
       alpha=(datan((vo(j+1,1)-vo(j,1))/
     + (uo(j+1,1)-uo(j,1))))
       end if
       if (j.ge.2.and.j.lt.npo) then
       alpha1=(datan((vo(j+1,1)-vo(j,1))/
     + (uo(j+1,1)-uo(j,1))))
       alpha2=(datan((vo(j,1)-vo(j-1,1))/
     + (uo(j,1)-uo(j-1,1))))
       alpha=0.5*(alpha1+alpha2)
       end if
       if (j.eq.npo) then
       alpha=(datan((vo(j,1)-vo(j-2,1))/
     + (uo(j,1)-uo(j-2,1))))
       end if

c      OFFSET OTION 1 IS OK
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Offset points

       if(vo(j,1).ge.0.0d0) then
       uo(j,2)=uo(j,1)-xo*0.1d0*dsin(alpha)
       vo(j,2)=vo(j,1)+xo*0.1d0*dcos(alpha)
       end if

       if(vo(j,1).ge.0.0d0.and.uo(j,1).gt.uo(j-1,1).and.j.gt.1) then
       uo(j,2)=uo(j,1)+xo*0.1d0*dsin(alpha)
       vo(j,2)=vo(j,1)-xo*0.1d0*dcos(alpha)
       end if

       if(vo(j,1).lt.0.0d0) then
       uo(j,2)=uo(j,1)+xo*0.1d0*dsin(alpha)
       vo(j,2)=vo(j,1)-xo*0.1d0*dcos(alpha)
       end if

       if(uo(j,1).eq.0.0d0) then
       uo(j,2)=uo(j,1)-xo*0.1d0
       vo(j,2)=vo(j,1)
       end if
c       OPTION 1

c      OFFSET  OTION 2 IS IN TEST
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       if(uo(j,1).gt.uo(j+1,1).or.uo(j,1).eq.0.0d0) then
c       uo(j,2)=uo(j,1)+xo*0.1d0*dsin(alpha)
c       vo(j,2)=vo(j,1)-xo*0.1d0*dcos(alpha)
c       end if

c       if(uo(j,1).lt.uo(j+1,1)) then
c       uo(j,2)=uo(j,1)-xo*0.1d0*dsin(alpha)
c       vo(j,2)=vo(j,1)+xo*0.1d0*dcos(alpha)
c       end if

c       if(uo(j,npo).lt.uo(npo-1,1)) then
c       uo(npo,2)=uo(npo,1)-xo*0.1d0*dsin(alpha)
c       vo(npo,2)=vo(npo,1)+xo*0.1d0*dcos(alpha)
c       end if
c      OPTION 2 TEST

c       if (j.eq.24) then
c       write (*,*) "SUP", j,uo(j,2),uo(j,1),vo(j,2),vo(j,1),alpha
c       end if

c       write (*,*) "Ep ",j,xo,alpha

       end do ! j

       return

       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE LINE OFFSET2
c      Another version
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Offset a 2D contour using tangent direction and a fixed outward-side sign.
!! @param[in,out] uo Horizontal coordinates; column 1 is input and column 2 output.
!! @param[in,out] vo Vertical coordinates; column 1 is input and column 2 output.
!! @param[in] npo Number of valid contour points.
!! @param[in] xo Offset magnitude, scaled internally by 0.1.
!! @note This is the later alternative to `loffset`; it corrects tangent angles
!!       when the contour's X coordinate is increasing.
       SUBROUTINE loffset2(uo,vo,npo,xo)

       real*8 uo(0:500,2),vo(0:500,2)
       real*8 xo,alpha1,alpha2,alpha,alphaa,xsign
      
       xsign=-1.0d0
       pi=4.0d0*datan(1.0d0)

       do j=1,npo

c      Angle alpha definition and corrections
       if (j.eq.1) then
       alpha=(datan((vo(j+1,1)-vo(j,1))/
     + (uo(j+1,1)-uo(j,1))))
       end if
       if (j.ge.2.and.j.lt.npo) then
       alpha=(datan((vo(j+1,1)-vo(j,1))/
     + (uo(j+1,1)-uo(j,1))))
       end if
       if (j.eq.npo) then
       alpha=(datan((vo(j,1)-vo(j-1,1))/
     + (uo(j,1)-uo(j-1,1))))
       end if
c      Correction when x increasing       
       if (uo(j+1,1).ge.uo(j,1).and.j.lt.npo) then
       alpha=alpha-pi
       end if
       if (uo(j,1).ge.uo(j-1,1).and.j.eq.npo) then
       alpha=alpha-pi
       end if

c      Definition of displaced points
       uo(j,2)=uo(j,1)+xo*xsign*0.1d0*dsin(alpha)
       vo(j,2)=vo(j,1)-xo*xsign*0.1d0*dcos(alpha)

c       write (*,*) "Ep ",j,xo,xsign,alpha
       
c       write (*,*) j," fi ",alpha,(uo(j+1,1)-uo(j,1))*1000.,
c     + uo(j,2),vo(j,2)

c       write (*,*) "Ep ",j,uo(j,1),vo(j,1),uo(j,2),vo(j,2)


       end do

       return

       end




ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE PUNTS LATERALS
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Calculate offset lateral endpoints for the left or right side of a pattern.
!! @param[in] i Rib/panel index.
!! @param[in] pulat1x Horizontal coordinates of the first segment endpoints.
!! @param[in] pulat1y Vertical coordinates of the first segment endpoints.
!! @param[in] pulat2x Horizontal coordinates of the second segment endpoints.
!! @param[in] pulat2y Vertical coordinates of the second segment endpoints.
!! @param[in] npulati First segment index to process.
!! @param[in] npulatf Last segment index to process.
!! @param[in,out] latu Pattern horizontal coordinates; slots 7/8 contain offset
!!                     distances and slots 9--12 receive lateral points.
!! @param[in,out] latv Pattern vertical coordinates; slots 7/8 contain offset
!!                     distances and slots 9--12 receive lateral points.
!! @param[in] xupp Additional allowance magnitude, scaled internally by 0.1.
!! @param[in] latsgn Side selector: `-1` for left and `1` for right.
       SUBROUTINE puntslat(i,pulat1x,pulat1y,pulat2x,pulat2y,npulati,
     + npulatf,latu,latv,xupp,latsgn)

       real*8 pulat1x(0:100,500),pulat1y(0:100,500),pulat2x(0:100,500),
     + pulat2y(0:100,500)
       real*8 latu(0:100,500,99),latv(0:100,500,99),xdu,xdv
       real*8 siu(0:500),siv(0:500),alplat,xupp
       integer npulati,npulatf,npulatflatsgn

c      Calcula punts esquerra (-1)

       if (latsgn.eq.-1) then

       do j=npulati,npulatf

       xdv=(pulat2y(i,j)-pulat1y(i,j))
       xdu=(pulat2x(i,j)-pulat1x(i,j))

       if (xdv.ne.0.) then
       alplat=abs(datan((pulat2y(i,j)-pulat1y(i,j))/(pulat2x(i,j)-
     + pulat1x(i,j))))
       else
       alplat=2.*datan(1.0d0)
       end if

       if (xdu.ge.0.and.xdv.ge.0) then ! case 2-I
       siu(j)=-1.
       siv(j)=1.
       end if
       if (xdu.le.0.and.xdv.ge.0) then ! case 2-II
       siu(j)=-1.
       siv(j)=-1.
       end if
       if (xdu.ge.0.and.xdv.le.0) then ! case 2-III
       siu(j)=1.
       siv(j)=1.
       end if
       if (xdu.le.0.and.xdv.le.0) then ! case 2-IV
       siu(j)=1.
       siv(j)=-1.
       end if

       latu(i,j+1,9)=pulat2x(i,j)+siu(j)*latv(i,j+1,7)*dsin(alplat)
       latv(i,j+1,9)=pulat2y(i,j)+siv(j)*latv(i,j+1,7)*dcos(alplat)

       latu(i,j+1,11)=latu(i,j+1,9)+siu(j)*xupp*0.1*dsin(alplat)
       latv(i,j+1,11)=latv(i,j+1,9)+siv(j)*xupp*0.1*dcos(alplat)

c      Set central panel
       if (i.eq.1) then
      


       end if

c      Impresió de control
c       if (j.eq.10) then
c       write (*,*) "BBB ",i,latu(i,j+1,9),latv(i,j+1,9)
c       end if

       end do ! j

c      Punt inicial

       j=npulati
       
       alplat=abs(datan((pulat2y(i,j)-pulat1y(i,j))/
     + (pulat2x(i,j)-pulat1x(i,j))))

c      Potser hauria de ser pl1x i pl1y???? Yes
       latu(i,j,9)=pulat1x(i,j)+siu(j)*latv(i,j,7)*dsin(alplat)
       latv(i,j,9)=pulat1y(i,j)+siv(j)*latv(i,j,7)*dcos(alplat)

       latu(i,j,11)=latu(i,j,9)+siu(j)*xupp*0.1*dsin(alplat)
       latv(i,j,11)=latv(i,j,9)+siv(j)*xupp*0.1*dcos(alplat)

       end if ! Punts esquerra (-1)


c      Calcula punts dreta (1)

       if (latsgn.eq.1) then

       do j=npulati,npulatf

       xdv=(pulat2y(i,j)-pulat1y(i,j))
       xdu=(pulat2x(i,j)-pulat1x(i,j))

       if (xdv.ne.0.) then
       alplat=abs(datan((pulat2y(i,j)-pulat1y(i,j))/(pulat2x(i,j)-
     + pulat1x(i,j))))
       else
       alplat=2.*datan(1.0d0)
       end if

       if (xdu.ge.0.and.xdv.ge.0) then ! case 3-I
       siu(j)=1.
       siv(j)=-1.
       end if
       if (xdu.le.0.and.xdv.ge.0) then ! case 3-II
       siu(j)=1.
       siv(j)=1.
       end if
       if (xdu.ge.0.and.xdv.le.0) then ! case 3-III
       siu(j)=-1.
       siv(j)=-1.
       end if
       if (xdu.le.0.and.xdv.le.0) then ! case 3-IV
       siu(j)=-1.
       siv(j)=1.
       end if

       latu(i,j+1,10)=pulat2x(i,j)+siu(j)*latv(i,j+1,8)*dsin(alplat)
       latv(i,j+1,10)=pulat2y(i,j)+siv(j)*latv(i,j+1,8)*dcos(alplat)

       latu(i,j+1,12)=latu(i,j+1,10)+siu(j)*xupp*0.1*dsin(alplat)
       latv(i,j+1,12)=latv(i,j+1,10)+siv(j)*xupp*0.1*dcos(alplat)

c      Impresió de control
c       if (j.eq.10) then
c       write (*,*) "BBB ",i,latu(i,j+1,9),latv(i,j+1,9)
c       end if

       end do ! j

c      Punt inicial

       j=npulati
       
       alplat=abs(datan((pulat2y(i,j)-pulat1y(i,j))/(pulat2x(i,j)-
     + pulat1x(i,j))))

       latu(i,j,10)=pulat1x(i,j)+siu(j)*latv(i,j,8)*dsin(alplat)
       latv(i,j,10)=pulat1y(i,j)+siv(j)*latv(i,j,8)*dcos(alplat)

       latu(i,j,12)=latu(i,j,10)+siu(j)*xupp*0.1*dsin(alplat)
       latv(i,j,12)=latv(i,j,10)+siv(j)*xupp*0.1*dcos(alplat)

       end if ! Punts dreta (1)


       return

       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE MYLARS
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Construct and draw the nose-mylar reinforcement for one rib.
!! @param[in] i Rib index.
!! @param[in] u Prepared horizontal profile coordinates.
!! @param[in] v Prepared vertical profile coordinates.
!! @param[in] sepx Horizontal drawing translation for the rib pattern.
!! @param[in] sepy Vertical drawing translation for the rib pattern.
!! @param[in] rib Rib geometry and selected mylar-group indices.
!! @param[in] xmy Mylar shape parameters by group.
!! @param[in] np Profile point counts and vent indices.
!! @param[in] xkf Drawing scale factor.
!! @param[in] atp Airfoil-type code; `pc` enables the vent gap in the contour.
!! @note Writes reinforcement outlines, matching marks, and Romano labels to DXF.
       SUBROUTINE mylars(i,u,v,sepx,sepy,rib,xmy,np,xkf,atp)

       real*8 u(0:100,500,99),v(0:100,500,99),sepx,sepy
       real*8 rib(0:100,500),xmy(0:100,10)
       real*8 mx(20),my(20)
       real*8 x1,x2,x3,y1,y2,y3,xad4,xad7,xkf,alpha,xd
       integer np(0:100,9)
       character*2 atp

       xad7=(1260.*5.+80.)*xkf
       xad4=2530.*xkf
       ng=int(rib(i,168))
       jvi=np(i,2)           ! vent in
       jvo=np(i,2)+np(i,3)-1 ! vent out

       if (ng.ne.0) then ! Only if mylar is defined

c      Read airfoil and detect initial and final points

c      Initial i final points x-ccordinate
       mx(1)=xmy(ng,1)*rib(i,5)/100.
       mx(8)=xmy(ng,4)*rib(i,5)/100.

       do j=1,np(i,1)-1! Explore airfoil

c      Initial point

       if (u(i,j,3).ge.mx(1).and.u(i,j+1,3).lt.mx(1).and.v(i,j,3).ge.0) 
     + then 

       jini=j

       x1=u(i,j+1,3)
       y1=v(i,j+1,3)
       x2=u(i,j,3)
       y2=v(i,j,3)
       x3=mx(1)
       call interpola(x1,y1,x2,y2,x3,y3)
       my(1)=y3
       end if

c      Final point

       if (u(i,j,3).le.mx(8).and.u(i,j+1,3).gt.mx(8).and.j.ge.np(i,2)) 
     + then 

       jfin=j

       x1=u(i,j+1,3)
       y1=v(i,j+1,3)
       x2=u(i,j,3)
       y2=v(i,j,3)
       x3=mx(8)
       call interpola(x1,y1,x2,y2,x3,y3)
       my(8)=y3
       end if

       end do

c      Compute others points

       mx(3)=mx(1)+xmy(ng,2)*rib(i,5)/100.0d0
       my(3)=my(1)
       mx(6)=mx(8)
       my(6)=my(8)+xmy(ng,5)*rib(i,5)/100.0d0

c      Airfoil contour

c      Case not "pc"
       if (atp.ne."pc") then
       call line(sepx+mx(1)+xad7,sepy-my(1),
     + sepx+u(i,jini+1,3)+xad7,sepy-v(i,jini+1,3),10)
       do j=jini+1,jfin-1
       call line(sepx+u(i,j,3)+xad7,sepy-v(i,j,3),
     + sepx+u(i,j+1,3)+xad7,sepy-v(i,j+1,3),10)
       end do  
       call line(sepx+mx(8)+xad7,sepy-my(8),
     + sepx+u(i,jfin,3)+xad7,sepy-v(i,jfin,3),10)
       end if 

c      Case "pc"

       if (atp.eq."pc") then
       call line(sepx+mx(1)+xad7,sepy-my(1),
     + sepx+u(i,jini+1,3)+xad7,sepy-v(i,jini+1,3),10)
       do j=jini+1,jvi-1
       call line(sepx+u(i,j,3)+xad7,sepy-v(i,j,3),
     + sepx+u(i,j+1,3)+xad7,sepy-v(i,j+1,3),10)
       end do  
       call line(sepx+u(i,jvi,3)+xad7,sepy-v(i,jvi,3),
     + sepx+u(i,jvo,3)+xad7,sepy-v(i,jvo,3),3)  
       do j=jvo,jfin-1
       call line(sepx+u(i,j,3)+xad7,sepy-v(i,j,3),
     + sepx+u(i,j+1,3)+xad7,sepy-v(i,j+1,3),10)
       end do 
       call line(sepx+mx(8)+xad7,sepy-my(8),
     + sepx+u(i,jfin,3)+xad7,sepy-v(i,jfin,3),10)
       end if ! "pc"

c      Mylar roman number

       x1=0.5*(mx(3)+mx(6))
       y1=0.5*(my(3)+my(6))

       alpha=datan((my(6)-my(3))/(mx(6)-mx(3)))

       x2=x1+1.0*dsin(alpha)  ! Shift 1 cm
       y2=y1-1.0*dcos(alpha)

       call romano(i,sepx+x2+xad7,sepy-y2,alpha,3.0d0,7)

c      Compute additional points 2-4
c      First use one unit distance xd=1.0

       xd=1.0d0
c      Point 2
       x1=mx(3)
       y1=my(3)
       x2=mx(1)
       y2=my(1)
       call interpd(x1,y1,x2,y2,x3,y3,xd)
       mx(2)=x3
       my(2)=y3
c      Point 4
       x1=mx(3)
       y1=my(3)
       x2=mx(6)
       y2=my(6)
       call interpd(x1,y1,x2,y2,x3,y3,xd)
       mx(4)=x3
       my(4)=y3

c      Second make proportional triangles

       xdi=dsqrt((mx(2)-mx(4))**2.+(my(2)-my(4))**2.)
       if (xdi.ne.0.) then
       xd=xmy(ng,3)*rib(i,5)/(100.*xdi)
c      Point 2
       x1=mx(3)
       y1=my(3)
       x2=mx(1)
       y2=my(1)
       call interpd(x1,y1,x2,y2,x3,y3,xd)
       mx(2)=x3
       my(2)=y3
c      Point 4
       x1=mx(3)
       y1=my(3)
       x2=mx(6)
       y2=my(6)
       call interpd(x1,y1,x2,y2,x3,y3,xd)
       mx(4)=x3
       my(4)=y3
       else
       end if

c      Compute additional points 5-6
c      First use one unit distance xd=1.0

       xd=1.0d0
c      Point 5
       x1=mx(6)
       y1=my(6)
       x2=mx(3)
       y2=my(3)
       call interpd(x1,y1,x2,y2,x3,y3,xd)
       mx(5)=x3
       my(5)=y3
c      Point 7
       x1=mx(6)
       y1=my(6)
       x2=mx(8)
       y2=my(8)
       call interpd(x1,y1,x2,y2,x3,y3,xd)
       mx(7)=x3
       my(7)=y3

c      Second make proportional triangles

       xdi=dsqrt((mx(5)-mx(7))**2.+(my(5)-my(7))**2.)
       if (xdi.ne.0.) then
       xd=xmy(ng,6)*rib(i,5)/(100.*xdi)
c      Point 5
       x1=mx(6)
       y1=my(6)
       x2=mx(3)
       y2=my(3)
       call interpd(x1,y1,x2,y2,x3,y3,xd)
       mx(5)=x3
       my(5)=y3
c      Point 7
       x1=mx(6)
       y1=my(6)
       x2=mx(8)
       y2=my(8)
       call interpd(x1,y1,x2,y2,x3,y3,xd)
       mx(7)=x3
       my(7)=y3
       else
       end if

c      Draw basic mylar contour 1-8 in ribs
      
       call line(sepx+mx(1),sepy-my(1),sepx+mx(2),sepy-my(2),10)
       call line(sepx+mx(2),sepy-my(2),sepx+mx(4),sepy-my(4),10)
       call line(sepx+mx(4),sepy-my(4),sepx+mx(5),sepy-my(5),10)
       call line(sepx+mx(5),sepy-my(5),sepx+mx(7),sepy-my(7),10)
       call line(sepx+mx(7),sepy-my(7),sepx+mx(8),sepy-my(8),10)
     
c      Draw basic mylar contour 1-8 in BOX(1,7)

       call line(sepx+mx(1)+xad7,sepy-my(1),
     + sepx+mx(2)+xad7,sepy-my(2),10)
       call line(sepx+mx(2)+xad7,sepy-my(2),
     + sepx+mx(4)+xad7,sepy-my(4),10)
       call line(sepx+mx(4)+xad7,sepy-my(4),
     + sepx+mx(5)+xad7,sepy-my(5),10)
       call line(sepx+mx(5)+xad7,sepy-my(5),
     + sepx+mx(7)+xad7,sepy-my(7),10)
       call line(sepx+mx(7)+xad7,sepy-my(7),
     + sepx+mx(8)+xad7,sepy-my(8),10)

c      Draw marks in BOX(1,4)

       x1=mx(1)
       y1=my(1)
       x2=mx(1)+0.7
       y2=my(1)
       call segment101(sepx+x1+xad4,sepy-y1,sepx+x2+xad4,sepy-y2,2)
       x1=mx(8)
       y1=my(8)
       x2=mx(8)
       y2=my(8)+0.7
       call segment101(sepx+x1+xad4,sepy-y1,sepx+x2+xad4,sepy-y2,2)

       end if

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     SUBROUTINE interpolation of polyline - linear
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Linearly interpolate a scalar value on a piecewise-linear function.
!! @param[in] n Number of tabulated points.
!! @param[in] xpoint Tabulated, nondecreasing X coordinates.
!! @param[in] ypoint Corresponding tabulated Y coordinates.
!! @param[in] xvalue X coordinate to evaluate.
!! @param[out] yvalue Interpolated Y value, or zero when X is outside all intervals.
      SUBROUTINE interpolyg(n,xpoint,ypoint,xvalue,yvalue)

      real*8 xpoint(50),ypoint(50),xvalue,yvalue,xm,xb
      real*8 funsup(50),funinf(50),funval(50)

      yvalue=0.0d0

      do i=1,n-1

      if (xvalue.ge.xpoint(i).and.xvalue.le.xpoint(i+1)) then
      xm=(ypoint(i+1)-ypoint(i))/(xpoint(i+1)-xpoint(i))
      xb=ypoint(i)-xm*xpoint(i)
      yvalue=xm*xvalue+xb
      end if

      end do

      return

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     SUBROUTINE interpolation of polyline - lagrange
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


!> Evaluate the global Lagrange interpolating polynomial through tabulated points.
!! @param[in] n Number of interpolation points.
!! @param[in] xpoint Distinct tabulated X coordinates.
!! @param[in] ypoint Corresponding tabulated Y coordinates.
!! @param[in] xvalue X coordinate to evaluate.
!! @param[out] yvalue Interpolated polynomial value.
      SUBROUTINE lagrangeip(n,xpoint,ypoint,xvalue,yvalue)

      real*8 xpoint(50),ypoint(50),xvalue,yvalue
      real*8 funsup(50),funinf(50),funval(50)

      yvalue=0.0d0

      do i=1,n

      funsup(i)=1.
      do j=1,n
      if (j.ne.i) then
      funsup(i)=funsup(i)*(xvalue-xpoint(j))
      end if
      end do

      funinf(i)=1.
      do j=1,n
      if (j.ne.i) then
      funinf(i)=funinf(i)*(xpoint(i)-xpoint(j))
      end if
      end do
      
      funval(i)=funsup(i)/funinf(i)

      yvalue=yvalue+funval(i)*ypoint(i)

      end do

      return

      end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE FLETXA (arrow or haut)
c      Numerical calculus arrow "haut" of circular segment
c      s = segment of the arc
c      sm =length of the arc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Solve the sagitta of a circular segment from chord and arc lengths.
!! @param[in] s Chord length.
!! @param[in] sm Target arc length; normally between `s` and `pi*s/2`.
!! @param[out] haut Solved sagitta (arrow height).
!! @note Uses a one-percent stepping search followed by linear interpolation and
!!       writes convergence diagnostics to standard output.
       SUBROUTINE fletxa(s,sm,haut)

       real*8 s,g,sm,haut,llarg,radi,pi
       real*8 beta,alpha,alp20
       pi=4.0d0*datan(1.0d0)

       g=0.0d0
       i=1

       llarg=pi*s/2.

       do while (llarg.ge.sm)

       write (*,*) i,g,llarg,pi*s/2.

       x1=llarg
       y1=g

       beta=datan(g/(0.5d0*s))
       alpha=pi-2.*beta
       alp20=alpha/20.0d0        ! Discretize arc in 20 segments
       radi=dsqrt(g*g+0.25d0*s*s)
       llarg=40.*radi*dsin(0.5d0*alp20)
      
       g=g+(s/100.)  ! increment a cent of s
       i=i+1
       
       end do

       x2=llarg
       y2=g

       haut=radi-g-s/100.
       write (*,*) i,g,llarg,pi*s/2.
       write (*,*) "haut=",haut

       write (*,*) x1,y1
       write (*,*) x2,y2

c      Interpolation

       xm=(y2-y1)/(x2-x1)
       xb=y1-xm*x1
       g=xm*sm+xb

       radi=dsqrt(g*g+0.25d0*s*s)
       haut=radi-g

       write (*,*) sm,g,haut

       return

       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     SUBROUTINE panels3d
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

