c      5. GRAPHIC DESIGN
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      5.1 Planform
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Box (1,1)

c      5.1.1 Ribs
       do i=1,nribss
       call line(rib(i,2),rib(i,3),rib(i,2),rib(i,4),5)
       call line(-rib(i,2),rib(i,3),-rib(i,2),rib(i,4),5)

       call itxt(rib(i,2)-110.*(typm3(9)/10.),rib(i,3)-15,
     + typm3(9),0.0d0,i,7)
       call itxt(-rib(i,2)-110.*(typm3(9)/10.),rib(i,3)-15,
     + typm3(9),0.0d0,i,7)

       end do

c      5.1.2 Draw rotated ribs ("Z" versions")
       do i=1,nribss
c      Using auxiliar points 1,2,3
       x3=rib(i,2)
       y3=rib(i,3)+(rib(i,4)-rib(i,3))*rib(i,251)/100.0
       x1=x3+(y3-rib(i,3))*dsin(rib(i,250)*pi/180.0)
       y1=y3-(y3-rib(i,3))*dcos(rib(i,250)*pi/180.0)
       x2=x3-(rib(i,4)-y3)*dsin(rib(i,250)*pi/180.0)
       y2=y3+(rib(i,4)-y3)*dcos(rib(i,250)*pi/180.0)

       if (dabs(rib(i,250)).gt.0.01d0) then ! 0.000 in IF
       call line(x1,y1,x2,y2,2)
       call line(-x1,y1,-x2,y2,2)
       end if

       end do

c      5.1.3 Leading edge        
       call line(-rib(1,2),rib(1,3),rib(1,2),rib(1,3),1)
       do i=1,nribss-1
       call line(rib(i,2),rib(i,3),rib(i+1,2),rib(i+1,3),1)
       call line(-rib(i,2),rib(i,3),-rib(i+1,2),rib(i+1,3),1)
       end do
 
c      5.1.4 Trailing edge  
       call line(-rib(1,2),rib(1,4),rib(1,2),rib(1,4),3)
       do i=1,nribss-1
       call line(rib(i,2),rib(i,4),rib(i+1,2),rib(i+1,4),3)
       call line(-rib(i,2),rib(i,4),-rib(i+1,2),rib(i+1,4),3)
       end do

c      Vents

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      DRAW vents in CASES in 1-1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (k26d.eq.1) then

       do i=1,nribss

c      Vents: Line 1-2 in, Line 3-4 out
       pgx(1)=rib(i-1,2)
       pgy(1)=rib(i-1,3)+rib(i-1,5)*rib(i,11)/100.
       pgx(2)=rib(i,2)
       pgy(2)=rib(i,3)+rib(i,5)*rib(i,11)/100.
       pgx(3)=rib(i,2)
       pgy(3)=rib(i,3)+rib(i,5)*rib(i,12)/100.
       pgx(4)=rib(i-1,2)
       pgy(4)=rib(i-1,3)+rib(i-1,5)*rib(i,12)/100.

c      Draw especial vents CASE 0
       if (rib(i,165).eq.0.) then
       call line(pgx(3),pgy(3),pgx(4),pgy(4),6)
       call line(-pgx(3),pgy(3),-pgx(4),pgy(4),6)
       call line(pgx(1),pgy(1),pgx(2),pgy(2),6)
       call line(-pgx(1),pgy(1),-pgx(2),pgy(2),6)
       end if

c      Draw especial vents CASE 1
       if (rib(i,165).eq.1) then
       call line(pgx(3),pgy(3),pgx(4),pgy(4),6)
       call line(-pgx(3),pgy(3),-pgx(4),pgy(4),6)
       end if

c      Draw especial vents CASE 4
       if (rib(i,165).eq.4.) then
c      Line 5-6
       param1=csi(i,19)
       param2=csi(i,20)
       pgy(5)=pgy(1)+((pgy(4)-pgy(1)))*(1.-(param1/100.))
       pgy(6)=pgy(2)+((pgy(3)-pgy(2)))*(1.-(param2/100.))
       call line(pgx(1),pgy(5),pgx(2),pgy(6),6)
       call line(-pgx(1),pgy(5),-pgx(2),pgy(6),6)
c      Line 3-4
       call line(pgx(3),pgy(3),pgx(4),pgy(4),6)
       call line(-pgx(3),pgy(3),-pgx(4),pgy(4),6)
       end if

c      Draw especial vents CASE 5
       if (rib(i,165).eq.5.) then
c      Arc 5-6
       param1=csi(i,19)
       param2=csi(i,20)
c      Fletxa as % of vent
       param3=csi(i,18)*(pgy(4)-pgy(1)+pgy(3)-pgy(2))/200.
       pgy(5)=pgy(1)+((pgy(4)-pgy(1)))*(1.-(param1/100.))
       pgy(6)=pgy(2)+((pgy(3)-pgy(2)))*(1.-(param2/100.))
c      Arc 5-6
       call arcfle(pgx(1),pgy(5),pgx(2),pgy(6),param3,1,6)
       call arcfle(-pgx(1),pgy(5),-pgx(2),pgy(6),param3,1,6)
c      Line 3-4
       call line(pgx(3),pgy(3),pgx(4),pgy(4),6)
       call line(-pgx(3),pgy(3),-pgx(4),pgy(4),6)
       end if

c      Draw especial vents CASE 6
       if (rib(i,165).eq.6) then
       call line(pgx(3),pgy(3),pgx(4),pgy(4),6)
       call line(-pgx(3),pgy(3),-pgx(4),pgy(4),6)
       param1=csi(i,19)
       param2=csi(i,20)
       call elliquad(pgx,pgy,param1,param2)
       call elliquad(-pgx,pgy,param1,param2)
       end if

c      Draw especial vents CASE -1
       if (rib(i,165).eq.-1.) then
       call line(pgx(1),pgy(1),pgx(2),pgy(2),6)
       call line(-pgx(1),pgy(1),-pgx(2),pgy(2),6)
       end if

c      Draw especial vents CASE -2
       if (rib(i,165).eq.-2.) then
       call line(pgx(1),pgy(1),pgx(2),pgy(2),6)
       call line(-pgx(1),pgy(1),-pgx(2),pgy(2),6)
       call line(pgx(4),pgy(4),pgx(2),pgy(2),6)
       call line(-pgx(4),pgy(4),-pgx(2),pgy(2),6)
       end if

c      Draw especial vents CASE -3
       if (rib(i,165).eq.-3.) then
       call line(pgx(1),pgy(1),pgx(2),pgy(2),6)
       call line(-pgx(1),pgy(1),-pgx(2),pgy(2),6)
       call line(pgx(1),pgy(1),pgx(3),pgy(3),6)
       call line(-pgx(1),pgy(1),-pgx(3),pgy(3),6)
       end if

c      Draw especial vents CASE -4
       if (rib(i,165).eq.-4.) then
c      Line 5-6
       param1=csi(i,19)
       param2=csi(i,20)
       pgy(5)=pgy(1)+((pgy(4)-pgy(1)))*(1.-(param1/100.))
       pgy(6)=pgy(2)+((pgy(3)-pgy(2)))*(1.-(param2/100.))
       call line(pgx(1),pgy(5),pgx(2),pgy(6),6)
       call line(-pgx(1),pgy(5),-pgx(2),pgy(6),6)
c      Line 1-2
       call line(pgx(1),pgy(1),pgx(2),pgy(2),6)
       call line(-pgx(1),pgy(1),-pgx(2),pgy(2),6)
       end if

c      Draw especial vents CASE -5
       if (rib(i,165).eq.-5.) then
c      Arc 5-6
       param1=csi(i,19)
       param2=csi(i,20)
c      Fletxa as % of vent
       param3=csi(i,18)*(pgy(4)-pgy(1)+pgy(3)-pgy(2))/200.
       pgy(5)=pgy(1)+((pgy(4)-pgy(1)))*(1.-(param1/100.))
       pgy(6)=pgy(2)+((pgy(3)-pgy(2)))*(1.-(param2/100.))
c      Arc 5-6
       call arcfle(pgx(1),pgy(5),pgx(2),pgy(6),param3,-1,6)
       call arcfle(-pgx(1),pgy(5),-pgx(2),pgy(6),param3,-1,6)
c      Line 1-2
       call line(pgx(1),pgy(1),pgx(2),pgy(2),6)
       call line(-pgx(1),pgy(1),-pgx(2),pgy(2),6)
       end if

c      Draw especial vents CASE -6
       if (rib(i,165).eq.-6) then
       call line(pgx(1),pgy(1),pgx(2),pgy(2),6)
       call line(-pgx(1),pgy(1),-pgx(2),pgy(2),6)
       param1=csi(i,19)
       param2=csi(i,20)
       call elliquad(pgx,pgy,param1,param2)
       call elliquad(-pgx,pgy,param1,param2)
       end if

        end do

        end if ! k26d=1

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      5.1.6 Draw vents classic in 1-1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (k26d.eq.0) then

c      Intake in

       if (cencell.ge.0.01.and.rib(1,14).eq.1)  then  
       call line(-rib(1,2),rib(1,3)+rib(1,5)*rib(1,11)/100.,rib(1,2),
     + rib(1,3)+rib(1,5)*rib(1,11)/100.,6)
       end if

       do i=2,nribss
       if(rib(i,14).eq.1) then
       call line(rib(i-1,2),rib(i-1,3)+rib(i-1,5)*rib(1,11)/100.,
     + rib(i,2),rib(i,3)+rib(i,5)*rib(1,11)/100.,6)
       call line(-rib(i-1,2),rib(i-1,3)+rib(i-1,5)*rib(1,11)/100.,
     + -rib(i,2),rib(i,3)+rib(i,5)*rib(1,11)/100.,6)
       end if

       if(rib(i,14).eq.0) then
       call line(rib(i-1,2),rib(i-1,3)+rib(i-1,5)*rib(1,11)/100.,
     + rib(i,2),rib(i,3)+rib(i,5)*rib(1,11)/100.,9)
       call line(-rib(i-1,2),rib(i-1,3)+rib(i-1,5)*rib(1,11)/100.,
     + -rib(i,2),rib(i,3)+rib(i,5)*rib(1,11)/100.,9)
       end if          

       end do

c      Central cell
       i=1
       if(rib(i,14).eq.0) then
       call line(-rib(i,2),rib(i,3)+rib(i,5)*rib(1,11)/100.,
     + rib(i,2),rib(i,3)+rib(i,5)*rib(1,11)/100.,9)
       end if      
        
c      Intake out

       if (cencell.ge.0.01.and.rib(1,14).eq.1)  then  
       call line(-rib(1,2),rib(1,3)+rib(1,5)*rib(1,12)/100.,rib(1,2),
     + rib(1,3)+rib(1,5)*rib(1,12)/100.,6)
       end if

       do i=2,nribss
       if(rib(i,14).eq.1) then
       call line(rib(i-1,2),rib(i-1,3)+rib(i-1,5)*rib(1,12)/100.,
     + rib(i,2),rib(i,3)+rib(i,5)*rib(1,12)/100.,6)
       call line(-rib(i-1,2),rib(i-1,3)+rib(i-1,5)*rib(1,12)/100.,
     + -rib(i,2),rib(i,3)+rib(i,5)*rib(1,12)/100.,6)
       end if
       end do

       end if ! k26d=0


c      5.1.7 Anchor points

       xpoi=3. ! segment

       do i=1,nribss

       do k=1,int(rib(i,15))

       call line(rib(i,2)-xpoi,rib(i,3)+rib(i,5)*rib(i,15+k)/100.,
     + rib(i,2)+xpoi,rib(i,3)+rib(i,5)*rib(i,15+k)/100.,1)

       call line(xpoi-rib(i,2),rib(i,3)+rib(i,5)*rib(i,15+k)/100.,
     + -rib(i,2)-xpoi,rib(i,3)+rib(i,5)*rib(i,15+k)/100.,1)
       
       end do
       end do

c      5.1.8 Brakes in 2D included fractionary points

       do k=1,cam(ii)     ! cam in brake lines

       i=int(brake(k,3))  ! rib

       if (rib(i,21).ne.0.) then

       xr=rib(i,2)
       xs=rib(i,3)+rib(i,5)*rib(i,21)/100.

       brake(k,1)=float(int(brake(k,3)))
       brake(k,2)=brake(k,3)-brake(k,1)

       if (i.lt.nribss.and.brake(k,2).ne.0.) then
       xrm=rib(i+1,2)
       xsm=rib(i+1,3)+rib(i+1,5)*rib(i+0,21)/100.0d0 ! i+0 ok
       xr=xr+brake(k,2)*(xrm-xr)
       xs=xs+brake(k,2)*(xsm-xs)
       end if

       call ellipse(xr,xs,1.5d0*xcir,1.5d0*xcir,0.0d0,1)
       call ellipse(-xr,xs,1.5d0*xcir,1.5d0*xcir,0.0d0,1)

       end if

       end do

c      5.1.8+ Restitute virtual anchor for 3D if rib i+1 not defined
c      Not strictly necessary
       do k=1,cam(ii)
       i=int(brake(k,3)) 
       if (i.lt.nribss.and.rib(i+1,21).eq.0.and.brake(k,2).ne.0) then
       rib(i+1,21)=100.  ! only 100% case, or specify value
       end if   
       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      5.1.9 Extrados colors
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Case 1
       if (npce.ge.1) then
       xpoi=2. ! segment
       do k=1,npce
       i=npc1e(k)
       do l=1,npc2e(k)
       ydist=100.-xpc1e(k,l)
       call line(rib(i,2)-xpoi,rib(i,3)+rib(i,5)*ydist/100.,
     + rib(i,2)+xpoi,rib(i,3)+rib(i,5)*ydist/100.,4)
       end do
       end do
       end if

c      Case -2
       if (npce.eq.-2) then
       do k=1,npace ! for each panel
       il=npac1e(k)-1
       ir=npac1e(k)
       do l=1,npac2e(k) ! for each cut
       ydistl=100.-xpac1e(k,l)
       ydistr=100.-xpac2e(k,l)
       if (il.ne.0) then
       call line(rib(il,2),rib(il,3)+rib(il,5)*ydistl/100.,
     + rib(ir,2),rib(ir,3)+rib(ir,5)*ydistr/100.,4)
       end if
       if (il.eq.0) then ! solve central panel
       call line(rib(il,2)*0.0,rib(il,3)+rib(il,5)*ydistl/100.,
     + rib(ir,2),rib(ir,3)+rib(ir,5)*ydistr/100.,4)
       end if
       end do
       end do
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      5.1.10 Intrados colors
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (atp.ne."ss") then

c      Case 1
       if (npci.ge.1) then
       xpoi=2. ! segment
       do k=1,npci
       i=npc1i(k)
       do l=1,npc2i(k)
       ydist=100.-xpc1i(k,l)
       call line(-rib(i,2)-xpoi,rib(i,3)+rib(i,5)*ydist/100.,
     + -rib(i,2)+xpoi,rib(i,3)+rib(i,5)*ydist/100.,30)
       end do
       end do
       end if

c      Case -2
       if (npci.eq.-2) then
       do k=1,npaci ! for each panel
       il=npac1i(k)-1
       ir=npac1i(k)
       do l=1,npac2i(k) ! for each cut
       ydistl=100.-xpac1i(k,l)
       ydistr=100.-xpac2i(k,l)
       if (il.ne.0) then
       call line(-rib(il,2),rib(il,3)+rib(il,5)*ydistl/100.,
     + -rib(ir,2),rib(ir,3)+rib(ir,5)*ydistr/100.,30)
       end if
       if (il.eq.0) then ! solve central panel
       call line(-rib(il,2)*0.0,rib(il,3)+rib(il,5)*ydistl/100.,
     + -rib(ir,2),rib(ir,3)+rib(ir,5)*ydistr/100.,30)
       end if
       end do
       end do
       end if

       end if

c      5.1.11 Miniribs and unloaded middle ribs

       rib(0,2)=-rib(1,2)
       rib(0,3)=rib(1,3)
       rib(0,4)=rib(1,4)

       do i=1,nribss

c      Draw only if minirib present
       if (rib(i,56).gt.1) then

       xru(1)=0.5*(rib(i,2)+rib(i-1,2))
       xrv(1)=0.5*(rib(i,3)+rib(i-1,3))
       xru(2)=0.5*(rib(i,2)+rib(i-1,2))
       xrv(2)=0.5*(rib(i,4)+rib(i-1,4))
       xrv(1)=xrv(2)-0.5*(rib(i,5)+rib(i-1,5))*rib(i,56)/100.

       call line(xru(1),xrv(1),xru(2),xrv(2),8)
       call line(-xru(1),xrv(1),-xru(2),xrv(2),8)
       
       end if

       end do

c      5.1.12 Draw special wingtips 

c      Draw wintip type 2 (parabolic)
c      Generic parabola y=xk*x**xn
       if (k27d.eq.2) then
       x0=rib(nribss,5)*sectx(27,3)*0.01
       dist=x0
       y0=0.0d0
       x1=x0+rib(nribss,3)-rib(nribss-1,3)
       y1=rib(nribss,2)-rib(nribss-1,2)
       if (sectx(27,4).eq.0.0) then
       xn=y1*x1/((x1-x0)*y1)
       else
       xn=x1*dtan(sectx(27,4))/y1
       end if
       xk=y1/(x1**xn)
c      Compute parabola in 60 points
       do k=1,60
       xinc=x1/dfloat(59)
       xpunt(k)=xinc*dfloat(k-1)
       ypunt(k)=xk*xpunt(k)**xn
       end do
c      Set parabola in place and draw
       n=60
       x0=rib(nribss,2)
       y0=-(dist+rib(nribss,3))
       angle=90.
       do k=1,60
       xloc(k)=xpunt(k)
       yloc(k)=ypunt(k)
       end do
       call loc2glo2D(n,xloc,yloc,xglo,yglo,x0,y0,angle)
       do k=1,59
       call line(xglo(k),-yglo(k),xglo(k+1),-yglo(k+1),30)
       call line(-xglo(k),-yglo(k),-xglo(k+1),-yglo(k+1),30)
       end do
c      Define complete base line
       do k=1,60
       
       end do
       end if


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      5.2 Canopy design - vault
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Box (1,1)

       x0=0.
       y0=380.*xkf

       call line(-rib(1,6),y0+rib(1,7),rib(1,6),y0+rib(1,7),5)
       do i=1,nribss-1
       call line(rib(i,6),y0+rib(i,7),rib(i+1,6),y0+rib(i+1,7),5)
       call line(-rib(i,6),y0+rib(i,7),-rib(i+1,6),y0+rib(i+1,7),5)
       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      5.3 Boxes
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Matrix 6x4

       do asep=0.,7.,1.
       do bsep=0.,3.,1.

       x1=-630.*xkf+asep*1260.*xkf
       x2=630.*xkf+asep*1260.*xkf
       y1=-50.*xkf+bsep*890.95*xkf
       y2=840.95*xkf+bsep*890.95*xkf

       call line(x1,y1,x2,y1,9)
       call line(x1,y1,x1,y2,9)
       call line(x2,y1,x2,y2,9)
       call line(x1,y2,x2,y2,9)
       
       end do
       end do

c      Additional boxes for 3D-shaping

       if (n1draw.eq.1) then

       if (k29d.ne.0) then

       do asep=2.,4.,1.
       do bsep=-1.,-2.,-1.

       x1=-630.*xkf+asep*1260.*xkf
       x2=630.*xkf+asep*1260.*xkf
       y1=-50.*xkf+bsep*890.95*xkf
       y2=840.95*xkf+bsep*890.95*xkf

       if (asep.ne.3.) then
       call line(x1,y1,x2,y1,9)
       call line(x1,y1,x1,y2,9)
       call line(x2,y1,x2,y2,9)
       call line(x1,y2,x2,y2,9)
       end if
       
       end do
       end do

       end if
       end if ! n1draw

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
