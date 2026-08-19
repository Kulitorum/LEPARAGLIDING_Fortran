!> Calculate extended leading- and trailing-edge corners for a panel.
!! The routine stores construction-line endpoints in coordinate slots 14/15
!! and the resulting side intersections or orthogonal corners in slots 24/25.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in,out] uf Developed-panel horizontal coordinates.
!! @param[in,out] vf Developed-panel vertical coordinates.
!! @param[in] npo Number of profile points in the panel.
!! @param[in] xpal General side allowance, used by the orthogonal method.
!! @param[in] xpale Leading-edge extension allowance.
!! @param[in] xpate Trailing-edge extension allowance.
!! @param[in] icase Extension method: 1 follows the side tangents; 2 uses
!!                  corners orthogonal to the end construction line.
       SUBROUTINE extpoints(i,uf,vf,npo,xpal,xpale,xpate,icase)

       real*8 uf(0:100,500,50),vf(0:100,500,50)
       real*8 xupp,xupple,xpale,xpate,xpal,xuppte,alple,alpte
       real*8 xru(2),xrv(2),xsu(2),xsv(2)
c       integer icase

       xupple=xpale
       xuppte=xpate
       xupp=xpal
c      WARNING!!! xupp NOT USED

c       icase=1 ! Tangent
c       icase=2 ! Orthogonal

c       icase=1

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case tangent
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Points LE 14,15,24,25

       if (icase.eq.1) then

       alple=-(datan((vf(i,npo,10)-vf(i,npo,9))/
     + (uf(i,npo,10)-uf(i,npo,9))))

       uf(i,npo,14)=uf(i,npo,9)+xupple*0.1*dsin(alple)
       vf(i,npo,14)=vf(i,npo,9)+xupple*0.1*dcos(alple)

       uf(i,npo,15)=uf(i,npo,10)+xupple*0.1*dsin(alple)
       vf(i,npo,15)=vf(i,npo,10)+xupple*0.1*dcos(alple)

c      recta r (14-15)

       xru(1)=uf(i,npo,14)
       xrv(1)=vf(i,npo,14)
       xru(2)=uf(i,npo,15)
       xrv(2)=vf(i,npo,15)

c      recta s left (11)

       xsu(1)=uf(i,npo-1,11)
       xsv(1)=vf(i,npo-1,11)
       xsu(2)=uf(i,npo,11)
       xsv(2)=vf(i,npo,11)

c      Intersection 24
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)

       uf(i,npo,24)=xtu
       vf(i,npo,24)=xtv

c      recta s right (11)

       xsu(1)=uf(i,npo-1,12)
       xsv(1)=vf(i,npo-1,12)
       xsu(2)=uf(i,npo,12)
       xsv(2)=vf(i,npo,12)

c      Intersection 24
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)

       uf(i,npo,25)=xtu
       vf(i,npo,25)=xtv

c      Points TE 14,15,24,25

       alpte=-(datan((vf(i,1,10)-vf(i,1,9))/
     + (uf(i,1,10)-uf(i,1,9))))

       uf(i,1,14)=uf(i,1,9)-xuppte*0.1*dsin(alpte)
       vf(i,1,14)=vf(i,1,9)-xuppte*0.1*dcos(alpte)

       uf(i,1,15)=uf(i,1,10)-xuppte*0.1*dsin(alpte)
       vf(i,1,15)=vf(i,1,10)-xuppte*0.1*dcos(alpte)

c      recta r (14-15)

       xru(1)=uf(i,1,14)
       xrv(1)=vf(i,1,14)
       xru(2)=uf(i,1,15)
       xrv(2)=vf(i,1,15)

c      recta s left (11)

       xsu(1)=uf(i,1,11)
       xsv(1)=vf(i,1,11)
       xsu(2)=uf(i,2,11)
       xsv(2)=vf(i,2,11)

c      Intersection 24
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)

       uf(i,1,24)=xtu
       vf(i,1,24)=xtv

c      recta s right (11)

       xsu(1)=uf(i,1,12)
       xsv(1)=vf(i,1,12)
       xsu(2)=uf(i,2,12)
       xsv(2)=vf(i,2,12)

c      Intersection 25
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)

       uf(i,1,25)=xtu
       vf(i,1,25)=xtv

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case orthogonal
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (icase.eq.2) then

       alple=-(datan((vf(i,npo,10)-vf(i,npo,9))/
     + (uf(i,npo,10)-uf(i,npo,9))))

       uf(i,npo,14)=uf(i,npo,9)+xupple*0.1*dsin(alple)
       vf(i,npo,14)=vf(i,npo,9)+xupple*0.1*dcos(alple)

       uf(i,npo,15)=uf(i,npo,10)+xupple*0.1*dsin(alple)
       vf(i,npo,15)=vf(i,npo,10)+xupple*0.1*dcos(alple)

       uf(i,npo,24)=uf(i,npo,14)-xupp*0.1*dcos(alple)
       vf(i,npo,24)=vf(i,npo,14)+xupp*0.1*dsin(alple)

       uf(i,npo,25)=uf(i,npo,15)+xupp*0.1*dcos(alple)
       vf(i,npo,25)=vf(i,npo,15)-xupp*0.1*dsin(alple)

c      Points TE 14,15,24,25,26,27
c      TE case orthogonal

       alpte=-(datan((vf(i,1,10)-vf(i,1,9))/(uf(i,1,10)-uf(i,1,9))))

       uf(i,1,14)=uf(i,1,9)-xuppte*0.1*dsin(alpte)
       vf(i,1,14)=vf(i,1,9)-xuppte*0.1*dcos(alpte)

       uf(i,1,15)=uf(i,1,10)-xuppte*0.1*dsin(alpte)
       vf(i,1,15)=vf(i,1,10)-xuppte*0.1*dcos(alpte)

       uf(i,1,24)=uf(i,1,14)-xupp*0.1*dcos(alpte)
       vf(i,1,24)=vf(i,1,14)+xupp*0.1*dsin(alpte)
 
       uf(i,1,25)=uf(i,1,15)+xupp*0.1*dcos(alpte)
       vf(i,1,25)=vf(i,1,15)-xupp*0.1*dsin(alpte)

       end if

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE DRAW PANEL COMPLETE
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Draw a complete developed panel, including allowances and seam borders.
!! Geometry is read from the conventional coordinate slots in `uf` and `vf`
!! and emitted as DXF entities through `line`.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in] uf Developed-panel horizontal coordinates.
!! @param[in] vf Developed-panel vertical coordinates.
!! @param[in] npo Number of profile points in the panel.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
       SUBROUTINE dpanelc(i,uf,vf,npo,psep,psey)

       real*8 uf(0:100,500,50),vf(0:100,500,50)
       real*8 psep,psey

       do j=1,npo-1
c      Sobreamples esquerra

       call line(psep+uf(i,j,9),psey-vf(i,j,9),psep+uf(i,j+1,9),
     + psey-vf(i,j+1,9),1)

c      Sobreamples dreta

       call line(psep+uf(i,j,10),psey-vf(i,j,10),psep+uf(i,j+1,10),
     + psey-vf(i,j+1,10),1)

c      Vores de costura esquerra

       call line(psep+uf(i,j,11),psey-vf(i,j,11),psep+uf(i,j+1,11),
     + psey-vf(i,j+1,11),3)

c      Vores de costura dreta

       call line(psep+uf(i,j,12),psey-vf(i,j,12),psep+uf(i,j+1,12),
     + psey-vf(i,j+1,12),3)

       end do

c      Four horizontal segments 11-9, 10-12

       call line(psep+uf(i,npo,11),psey-vf(i,npo,11),
     + psep+uf(i,npo,9),psey-vf(i,npo,9),3)

       call line(psep+uf(i,npo,10),psey-vf(i,npo,10),
     + psep+uf(i,npo,12),psey-vf(i,npo,12),3)

       call line(psep+uf(i,1,11),psey-vf(i,1,11),
     + psep+uf(i,1,9),psey-vf(i,1,9),3)

       call line(psep+uf(i,1,10),psey-vf(i,1,10),
     + psep+uf(i,1,12),psey-vf(i,1,12),3)

c      Four vertical segments 10-15, 9-14

       call line(psep+uf(i,1,10),psey-vf(i,1,10),
     + psep+uf(i,1,15),psey-vf(i,1,15),3)

       call line(psep+uf(i,npo,10),psey-vf(i,npo,10),
     + psep+uf(i,npo,15),psey-vf(i,npo,15),3)
       
       call line(psep+uf(i,1,9),psey-vf(i,1,9),
     + psep+uf(i,1,14),psey-vf(i,1,14),3)

       call line(psep+uf(i,npo,9),psey-vf(i,npo,9),
     + psep+uf(i,npo,14),psey-vf(i,npo,14),3)

c      Two horizontal lines 14-15

       call line(psep+uf(i,1,14),psey-vf(i,1,14),
     + psep+uf(i,1,15),psey-vf(i,1,15),3)

       call line(psep+uf(i,npo,14),psey-vf(i,npo,14),
     + psep+uf(i,npo,15),psey-vf(i,npo,15),3)

c      Draw eight corner segments

       call line(psep+uf(i,1,14),psey-vf(i,1,14),
     + psep+uf(i,1,24),psey-vf(i,1,24),3)

       call line(psep+uf(i,1,11),psey-vf(i,1,11),
     + psep+uf(i,1,24),psey-vf(i,1,24),3)

       call line(psep+uf(i,1,15),psey-vf(i,1,15),
     + psep+uf(i,1,25),psey-vf(i,1,25),3)
     
       call line(psep+uf(i,1,12),psey-vf(i,1,12),
     + psep+uf(i,1,25),psey-vf(i,1,25),3)

       call line(psep+uf(i,npo,14),psey-vf(i,npo,14),
     + psep+uf(i,npo,24),psey-vf(i,npo,24),3)

       call line(psep+uf(i,npo,11),psey-vf(i,npo,11),
     + psep+uf(i,npo,24),psey-vf(i,npo,24),3)

       call line(psep+uf(i,npo,15),psey-vf(i,npo,15),
     + psep+uf(i,npo,25),psey-vf(i,npo,25),3)
     
       call line(psep+uf(i,npo,12),psey-vf(i,npo,12),
     + psep+uf(i,npo,25),psey-vf(i,npo,25),3)

c      Trailing edge extrados
       call line(psep+uf(i,1,9),psey-vf(i,1,9),
     + psep+uf(i,1,10),psey-vf(i,1,10),1)
       
c      Init extrados
       call line(psep+uf(i,npo,9),psey-vf(i,npo,9),
     + psep+uf(i,npo,10),psey-vf(i,npo,10),1)
            
       return
       end



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE DRAW PANEL COMPLETE WITH CONTROL (ic)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Draw a selected-border variant of a complete developed panel.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in] uf Developed-panel horizontal coordinates.
!! @param[in] vf Developed-panel vertical coordinates.
!! @param[in] npo Number of profile points in the panel.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
!! @param[in] ic Legacy border-selection mode controlling which end and side
!!               construction lines are emitted.
       SUBROUTINE dpanelcc(i,uf,vf,npo,psep,psey,ic)

       real*8 uf(0:100,500,50),vf(0:100,500,50)
       real*8 psep,psey
       integer icont(10),ic

       do k=1,8
       icont(k)=0
       end do

c      case ic=1
       if (ic.eq.1) then ! Vores laterals exteriors
       icont(1)=1
       icont(4)=1
       end if

c      case ic=2
       if (ic.eq.2) then ! Vores laterals exteriors sense LE
       icont(1)=1
       icont(2)=1
       icont(4)=1
       icont(5)=1
       icont(7)=1
       end if

c      case ic=3
       if (ic.eq.3) then ! Vores laterals exteriors tot
       icont(1)=1
       icont(2)=1
       icont(3)=1
       icont(4)=1
       icont(5)=1
       icont(6)=1
       icont(7)=1
       icont(8)=1
       end if

c      case ic=4
       if (ic.eq.4) then ! Vores laterals exteriors sense TE
       icont(1)=1
       icont(3)=1
       icont(4)=1
       icont(6)=1
       icont(8)=1
       end if

c      case ic=5
       if (ic.eq.5) then ! Vores laterals exteriors
       icont(1)=1
       end if

c      case ic=6
       if (ic.eq.6) then ! Vores laterals exteriors sense TE
       icont(1)=1
       icont(2)=1
       end if

c      case ic=7
       if (ic.eq.7) then ! Vores laterals exteriors sense TE
       icont(1)=1
       icont(2)=1
       icont(3)=1
       end if

c      case ic=8
       if (ic.eq.8) then ! Vores laterals exteriors sense TE
       icont(1)=1
       icont(3)=1
       end if

       do j=1,npo-1

       if (icont(4).eq.1) then
c      Sobreamples esquerra
       call line(psep+uf(i,j,9),psey-vf(i,j,9),psep+uf(i,j+1,9),
     + psey-vf(i,j+1,9),1)
c      Sobreamples dreta
       call line(psep+uf(i,j,10),psey-vf(i,j,10),psep+uf(i,j+1,10),
     + psey-vf(i,j+1,10),1)
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      MULTIBAT
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (icont(1).eq.1) then
c      Vores de costura esquerra
       call line(psep+uf(i,j,11),psey-vf(i,j,11),psep+uf(i,j+1,11),
     + psey-vf(i,j+1,11),3)
c      Vores de costura dreta
       call line(psep+uf(i,j,12),psey-vf(i,j,12),psep+uf(i,j+1,12),
     + psey-vf(i,j+1,12),3)
       end if

       end do

       if (icont(8).eq.1) then
c      Corners fin 11-9-14 and 12-10-15
       call line(psep+uf(i,npo,11),psey-vf(i,npo,11),
     + psep+uf(i,npo,9),psey-vf(i,npo,9),3)
       call line(psep+uf(i,npo,9),psey-vf(i,npo,9),
     + psep+uf(i,npo,14),psey-vf(i,npo,14),3)
       call line(psep+uf(i,npo,10),psey-vf(i,npo,10),
     + psep+uf(i,npo,12),psey-vf(i,npo,12),3)
       call line(psep+uf(i,npo,10),psey-vf(i,npo,10),
     + psep+uf(i,npo,15),psey-vf(i,npo,15),3)
       end if

       if (icont(7).eq.1) then
c      Corners init 11-9-14 and 12-10-15
       call line(psep+uf(i,1,11),psey-vf(i,1,11),
     + psep+uf(i,1,9),psey-vf(i,1,9),3)
       call line(psep+uf(i,1,10),psey-vf(i,1,10),
     + psep+uf(i,1,12),psey-vf(i,1,12),3)
       call line(psep+uf(i,1,10),psey-vf(i,1,10),
     + psep+uf(i,1,15),psey-vf(i,1,15),3)     
       call line(psep+uf(i,1,9),psey-vf(i,1,9),
     + psep+uf(i,1,14),psey-vf(i,1,14),3)
       end if

       if (icont(2).eq.1) then
c      Init bracket
c      Lines 14-15
       call line(psep+uf(i,1,14),psey-vf(i,1,14),
     + psep+uf(i,1,15),psey-vf(i,1,15),3)
c      Segments 14-24-11 and 15-25-12
       call line(psep+uf(i,1,14),psey-vf(i,1,14),
     + psep+uf(i,1,24),psey-vf(i,1,24),3)
       call line(psep+uf(i,1,11),psey-vf(i,1,11),
     + psep+uf(i,1,24),psey-vf(i,1,24),3)
       call line(psep+uf(i,1,15),psey-vf(i,1,15),
     + psep+uf(i,1,25),psey-vf(i,1,25),3)
       call line(psep+uf(i,1,12),psey-vf(i,1,12),
     + psep+uf(i,1,25),psey-vf(i,1,25),3)
       end if

       if (icont(3).eq.1) then
c      Final bracket
c      Lines 14-15
       call line(psep+uf(i,npo,14),psey-vf(i,npo,14),
     + psep+uf(i,npo,15),psey-vf(i,npo,15),3)
c      Segments 14-24-11 and 15-25-12
       call line(psep+uf(i,npo,14),psey-vf(i,npo,14),
     + psep+uf(i,npo,24),psey-vf(i,npo,24),3)
       call line(psep+uf(i,npo,11),psey-vf(i,npo,11),
     + psep+uf(i,npo,24),psey-vf(i,npo,24),3)
       call line(psep+uf(i,npo,15),psey-vf(i,npo,15),
     + psep+uf(i,npo,25),psey-vf(i,npo,25),3)
       call line(psep+uf(i,npo,12),psey-vf(i,npo,12),
     + psep+uf(i,npo,25),psey-vf(i,npo,25),3)
       end if

       if (icont(5).eq.1) then
c      Initial segment
       call line(psep+uf(i,1,9),psey-vf(i,1,9),
     + psep+uf(i,1,10),psey-vf(i,1,10),1)
       end if

       if (icont(6).eq.1) then
c      Final segment
       call line(psep+uf(i,npo,9),psey-vf(i,npo,9),
     + psep+uf(i,npo,10),psey-vf(i,npo,10),1)
       end if
            
       return

       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE DRAW ARC BY 3 POINTS WITH CONTROL (ic)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Construct and draw a circular panel edge from two endpoints and a sagitta.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in] npunt Profile-point index at which the curved edge is built.
!! @param[in] uf Developed-panel horizontal coordinates.
!! @param[in] vf Developed-panel vertical coordinates.
!! @param[in,out] dfle Requested sagitta; raised to the routine's minimum
!!                      usable value when necessary.
!! @param[in,out] is Arc-side selector; a negative sagitta reverses its sign.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
!! @param[in] ic Output mode: 1 draws all construction borders; 2 draws only
!!               the external border.
!! @param[in] xupp Panel allowance used to offset the companion arc.
       SUBROUTINE arc3p(i,npunt,uf,vf,dfle,is,psep,psey,ic,xupp)

       real*8 uf(0:100,500,50),vf(0:100,500,50)
       real*8 psep,psey,dfle,sfle,tetha,omega,puntu(0:10),puntv(0:10)
       real*8 a,b,c,d,e,f,ep,eps,ep1,ep3,radi,epinc,bv,cv,xupp
       real*8 g,mu,xi,parcu(0:21),parcv(0:21),parcul(0:21),parcvl(0:21)
       real*8 parcve(0:21),parcue(0:21)
       real*8 xru(2),xrv(2),xsu(2),xsv(2)
       integer npunt,ic,is

c      is = convexitat (1) o concavitat (-1) de l'arc

       j=npunt

c      Evita radi infinit
       if (dabs(dfle).lt.0.000001d0) then
       dfle=0.00001d0 ! :)
       end if

c      Sign control (use negative 3D shaping)
       ii=1
       if (dfle.lt.0.) then
       ii=-1
       end if
       is=is*ii

c      ic=1 draw complete borders
c      ic=2 draw only external borders

c      Set basic points 1-2-3-4

       puntu(1)=uf(i,j,9)
       puntv(1)=vf(i,j,9)
       puntu(3)=uf(i,j,10)
       puntv(3)=vf(i,j,10)
       puntu(4)=0.5d0*(puntu(1)+puntu(3))
       puntv(4)=0.5d0*(puntv(1)+puntv(3))
       puntu(9)=puntu(3)
       puntv(9)=puntv(3)

       sfle=0.5d0*dsqrt((puntu(1)-puntu(3))**2.+(puntv(1)-puntv(3))**2.)
       tetha=datan(dfle/sfle)
       omega=datan((puntv(1)-puntv(3))/(puntu(3)-puntu(1)))
       if (is.eq.1) then
       puntu(2)=puntu(4)+dfle*dsin(omega)
       puntv(2)=puntv(4)+dfle*dcos(omega)
       end if
       if (is.eq.-1) then
       puntu(2)=puntu(4)-dfle*dsin(omega)
       puntv(2)=puntv(4)-dfle*dcos(omega)
       end if

c      Draw two segments
c       call line(psep+puntu(1),psey-puntv(1),
c     + psep+puntu(2),psey-puntv(2),1)
c       call line(psep+puntu(3),psey-puntv(3),
c     + psep+puntu(2),psey-puntv(2),1)

c      Circle by 1-2-3 analytical solution

       a=2.0d0*(puntu(2)-puntu(1))
       b=2.0d0*(puntv(2)-puntv(1))
       c=puntu(1)*puntu(1)-puntu(2)*puntu(2)+
     + puntv(1)*puntv(1)-puntv(2)*puntv(2)
       d=2.0d0*(puntu(3)-puntu(2))
       e=2.0d0*(puntv(3)-puntv(2))
       f=puntu(2)*puntu(2)-puntu(3)*puntu(3)+
     + puntv(2)*puntv(2)-puntv(3)*puntv(3)
       puntv(0)=((c*d/a)-f)/(e-(b*d/a))
       puntu(0)=-(puntv(0)*b+c)/a
       radi=dsqrt((puntu(1)-puntu(0))**2.+(puntv(1)-puntv(0))**2.)

c      Draw basic radius
c       call line(psep+puntu(1),psey-puntv(1),
c     + psep+puntu(0),psey-puntv(0),7)
c       call line(psep+puntu(3),psey-puntv(3),
c     + psep+puntu(0),psey-puntv(0),7)

c      Consider points in an horizontal segment 1-3

       if (dabs(puntu(3)-puntu(1)).ge.0.01d0) then
       xi=datan((puntv(1)-puntv(3))/(puntu(1)-puntu(3)))
       end if

       g=dsqrt(radi*radi-sfle*sfle)
       mu=datan(sfle/g)

       puntu(3)=puntu(1)+2.0d0*sfle
       puntv(3)=puntv(1)
       puntu(0)=puntu(1)+sfle
       puntv(0)=puntv(1)-g

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Arc internal
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Arc global coordinates (u,v) using 21 points
       do k=1,21
       ep=mu-(2.0d0*mu/20.0d0)*dfloat(k-1)
       parcu(k-1)=puntu(0)-radi*dsin(ep)
       parcv(k-1)=puntv(0)+radi*dcos(ep)
       end do

c      Arc local coordinates (u',v')
       do k=0,20
       parcul(k)=parcu(k)-puntu(1)
       parcvl(k)=dfloat(is)*(parcv(k)-puntv(1))
       end do

c      Rotate local coordinates around punt 1
       do k=0,20
       parcu(k)=parcul(k)*dcos(xi)-parcvl(k)*dsin(xi)+puntu(1)
       parcv(k)=parcul(k)*dsin(xi)+parcvl(k)*dcos(xi)+puntv(1)
       end do

c      Draw rotated arc
       if (ic.ne.2) then
       do k=1,20
       call line(psep+parcu(k-1),psey-parcv(k-1),
     + psep+parcu(k),psey-parcv(k),1)
       end do
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Arc external
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Arc global coordinates (u,v) using 21 points
       do k=1,21
       ep=mu-(2.0d0*mu/20.0d0)*dfloat(k-1)
       parcu(k-1)=puntu(0)-(radi+dfloat(ii)*xupp*0.1d0)*dsin(ep)
       parcv(k-1)=puntv(0)+(radi+dfloat(ii)*xupp*0.1d0)*dcos(ep)
       end do

c      Arc local coordinates (u',v')
       do k=0,20
       parcul(k)=parcu(k)-puntu(1)
       parcvl(k)=dfloat(is)*(parcv(k)-puntv(1))
       end do

c      Rotate local coocrdinates around punt 1
       do k=0,20
       parcue(k)=parcul(k)*dcos(xi)-parcvl(k)*dsin(xi)+puntu(1)
       parcve(k)=parcul(k)*dsin(xi)+parcvl(k)*dcos(xi)+puntv(1)
       end do

c      Draw rotated arc
       do k=1,20
       call line(psep+parcue(k-1),psey-parcve(k-1),
     + psep+parcue(k),psey-parcve(k),3)
       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Intersection points
c      Points of intersection of arcs with external borders
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Intersection left
       xru(1)=parcue(0)
       xrv(1)=parcve(0)
       xru(2)=parcue(1)
       xrv(2)=parcve(1)

       if (is*ii.eq.1) then
       xsu(1)=uf(i,j,11)
       xsv(1)=vf(i,j,11)
       xsu(2)=uf(i,j-1,11)
       xsv(2)=vf(i,j-1,11)
       end if

       if (is*ii.eq.-1) then 
       xsu(1)=uf(i,j,11)
       xsv(1)=vf(i,j,11)
       xsu(2)=uf(i,j+1,11)
       xsv(2)=vf(i,j+1,11)
       end if

       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)

       call line(psep+xsu(1),psey-xsv(1),psep+xtu,psey-xtv,3)
       call line(psep+xru(1),psey-xrv(1),psep+xtu,psey-xtv,3)
       if (ic.ne.2) then
       call line(psep+puntu(1),psey-puntv(1),psep+xtu,psey-xtv,3)
       call line(psep+puntu(1),psey-puntv(1),psep+xsu(1),psey-xsv(1),3)
       call line(psep+puntu(1),psey-puntv(1),psep+xru(1),psey-xrv(1),3)
       end if

c      Intersection right
       xru(1)=parcue(20)
       xrv(1)=parcve(20)
       xru(2)=parcue(19)
       xrv(2)=parcve(19)

       if (is*ii.eq.1) then
       xsu(1)=uf(i,j,12)
       xsv(1)=vf(i,j,12)
       xsu(2)=uf(i,j-1,12)
       xsv(2)=vf(i,j-1,12)
       end if

       if (is*ii.eq.-1) then
       xsu(1)=uf(i,j,12)
       xsv(1)=vf(i,j,12)
       xsu(2)=uf(i,j+1,12)
       xsv(2)=vf(i,j+1,12)
       end if

       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)

       call line(psep+xsu(1),psey-xsv(1),psep+xtu,psey-xtv,3)
       call line(psep+xru(1),psey-xrv(1),psep+xtu,psey-xtv,3)
       if (ic.ne.2) then
       call line(psep+puntu(9),psey-puntv(9),psep+xtu,psey-xtv,3)
       call line(psep+puntu(9),psey-puntv(9),psep+xsu(1),psey-xsv(1),3)
       call line(psep+puntu(9),psey-puntv(9),psep+xru(1),psey-xrv(1),3)
       end if

       return
       end



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE DRAW ARC BY 3 POINTS WITH CONTROL (ic) 
c      SPECIAL!!!!!
c      USED ONLY FOR OBTAINIG SOME POINTS FOR ROMAN PRINTING....
c      INTEGRAR DINS romanoparc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Build the circular-edge construction used by Romano arc marks.
!! This specialized companion to `arc3p` retains the historical construction
!! sequence expected by the mark-printing code.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in] npunt Profile-point index at which the arc is constructed.
!! @param[in] uf Developed-panel horizontal coordinates.
!! @param[in] vf Developed-panel vertical coordinates.
!! @param[in,out] dfle Requested sagitta, normalized when too small or signed.
!! @param[in,out] is Arc-side selector, updated for a negative sagitta.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
!! @param[in] ic Legacy construction-output mode.
!! @param[in] xupp Panel allowance applied to the companion arc.
       SUBROUTINE arc3parc(i,npunt,uf,vf,dfle,is,psep,psey,ic,xupp)

       real*8 uf(0:100,500,50),vf(0:100,500,50)
       real*8 psep,psey,dfle,sfle,tetha,omega,puntu(0:10),puntv(0:10)
       real*8 a,b,c,d,e,f,ep,eps,ep1,ep3,radi,epinc,bv,cv,xupp
       real*8 g,mu,xi,parcu(0:21),parcv(0:21),parcul(0:21),parcvl(0:21)
       real*8 parcve(0:21),parcue(0:21)
       real*8 xru(2),xrv(2),xsu(2),xsv(2)
       real*8 uu1,vv1,uu2,vv2,xlen1,xlen2,xlenco
       integer npunt,ic,is,kini,kfin,ii

       j=npunt
       xlenco=typm2(8)

c      Normalize signed sagittas in the same way as arc3p.
       ii=1
       if (dfle.lt.0.0d0) then
       ii=-1
       end if
       is=is*ii

c      Evita radi infinit
       if (dfle.lt.0.000001d0) then
       dfle=0.00001d0 ! :)
       end if

c      ic=1 draw complete borders
c      ic=2 draw only external borders

c      Set basic points 1-2-3-4

       puntu(1)=uf(i,j,9)
       puntv(1)=vf(i,j,9)
       puntu(3)=uf(i,j,10)
       puntv(3)=vf(i,j,10)
       puntu(4)=0.5d0*(puntu(1)+puntu(3))
       puntv(4)=0.5d0*(puntv(1)+puntv(3))
       puntu(9)=puntu(3)
       puntv(9)=puntv(3)

       sfle=0.5d0*dsqrt((puntu(1)-puntu(3))**2.+(puntv(1)-puntv(3))**2.)
       tetha=datan(dfle/sfle)
       omega=datan((puntv(1)-puntv(3))/(puntu(3)-puntu(1)))
       if (is.eq.1) then
       puntu(2)=puntu(4)+dfle*dsin(omega)
       puntv(2)=puntv(4)+dfle*dcos(omega)
       end if
       if (is.eq.-1) then
       puntu(2)=puntu(4)-dfle*dsin(omega)
       puntv(2)=puntv(4)-dfle*dcos(omega)
       end if

c      Circle by 1-2-3 analytical solution
       a=2.0d0*(puntu(2)-puntu(1))
       b=2.0d0*(puntv(2)-puntv(1))
       c=puntu(1)*puntu(1)-puntu(2)*puntu(2)+
     + puntv(1)*puntv(1)-puntv(2)*puntv(2)
       d=2.0d0*(puntu(3)-puntu(2))
       e=2.0d0*(puntv(3)-puntv(2))
       f=puntu(2)*puntu(2)-puntu(3)*puntu(3)+
     + puntv(2)*puntv(2)-puntv(3)*puntv(3)
       puntv(0)=((c*d/a)-f)/(e-(b*d/a))
       puntu(0)=-(puntv(0)*b+c)/a
       radi=dsqrt((puntu(1)-puntu(0))**2.+(puntv(1)-puntv(0))**2.)

c      Consider points in an horizontal segment 1-3
       if (dabs(puntu(3)-puntu(1)).ge.0.01d0) then
       xi=datan((puntv(1)-puntv(3))/(puntu(1)-puntu(3)))
       end if

       g=dsqrt(radi*radi-sfle*sfle)
       mu=datan(sfle/g)

       puntu(3)=puntu(1)+2.0d0*sfle
       puntv(3)=puntv(1)
       puntu(0)=puntu(1)+sfle
       puntv(0)=puntv(1)-g

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Arc internal
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Case extrados
c      Arc global coordinates (u,v) using 21 points
       do k=1,21
       ep=mu-(2.0d0*mu/20.0d0)*dfloat(k-1)
       parcu(k-1)=puntu(0)-radi*dsin(ep)
       parcv(k-1)=puntv(0)+radi*dcos(ep)
       end do

c      Arc local coordinates (u',v')
       do k=0,20
       parcul(k)=parcu(k)-puntu(1)
       parcvl(k)=dfloat(is)*(parcv(k)-puntv(1))
       end do

c      Rotate local coordinates around punt 1
       do k=0,20
       parcu(k)=parcul(k)*dcos(xi)-parcvl(k)*dsin(xi)+puntu(1)
       parcv(k)=parcul(k)*dsin(xi)+parcvl(k)*dcos(xi)+puntv(1)
       end do

c      Draw rotated arc
       if (ic.ne.2) then
       do k=1,20
c       call line(psep+parcu(k-1),psey-parcv(k-1),
c     + psep+parcu(k),psey-parcv(k),1)
       end do
       end if

c      Arc length
       xlen1=0.0d0
       do k=0,20-1
       xlen1=xlen1+dsqrt((parcu(k+1)-parcu(k))**2.+
     + (parcv(k+1)-parcv(k))**2)
       end do

c      Detect kini and kfin
       xlen2=0.0d0
       kini=0
       kfin=2
       do k=0,20-1
       xlen2=xlen2+dsqrt((parcu(k+1)-parcu(k))**2.+
     + (parcv(k+1)-parcv(k))**2)
       if (xlen2.le.xlen1*xlenco) then
       kini=k
       kfin=k+2
       end if
       end do
c      Some arrangements...
       if (kini.ge.19) then
       kfin=20
       end if
       if (kfin.eq.20) then
       kini=18
       end if

c      Define especial points in the arc
       uu1=parcu(kini)
       vv1=parcv(kini)
       uu2=parcu(kfin)
       vv2=parcv(kfin)

       return
       end    ! ARC3P special


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE DRAW VENT WITH CONTROLS
c      Draw vent panels types
c      ic1=1 > print; ic1=2 > laser
c      ic2= vent type 1,0,-1,-2,-3
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc     
!> Draw a developed vent panel and its cutting/sewing construction geometry.
!! The routine supports the historical vent variants encoded by `ic2` and
!! writes the chosen representation as DXF line, curve, point, and mark
!! entities. Coordinate slot meanings follow the panel development pipeline.
!! @param[in] i Panel or rib index.
!! @param[in] np Number of points defining the panel/profile geometry.
!! @param[in] uf Developed-panel horizontal coordinates.
!! @param[in] vf Developed-panel vertical coordinates.
!! @param[in] ufv Vent-specific horizontal coordinates.
!! @param[in] vfv Vent-specific vertical coordinates.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
!! @param[in] xupp General seam allowance.
!! @param[in] xupple Leading-edge allowance.
!! @param[in] xuppte Trailing-edge allowance.
!! @param[in] xkf Global drawing scale factor.
!! @param[in] ic1 Output process selector used by the legacy print/laser paths.
!! @param[in] ic2 Vent construction variant selector.
!! @param[in,out] csi Per-rib vent control and construction parameters.
       SUBROUTINE drwvent(i,np,uf,vf,ufv,vfv,psep,psey,xupp,xupple,
     + xuppte,xkf,ic1,ic2,csi)

       real*8 uf(0:100,500,50),vf(0:100,500,50)
       real*8 ufv(0:100,500,50),vfv(0:100,500,50)
       real*8 ufa(0:100,500,50),vfa(0:100,500,50) !auxiliar
       real*8 ufb(0:100,500,50),vfb(0:100,500,50) !auxiliar
       integer np(0:100,9)
       real*8 psep,psey,xupp,xupple,xuppte,xkf,xcsi,ycsi
       real*8 cs2x,cs2y,cs1x,cs1y,alpha,alpha1,alpha2,csi(0:100,60)
       integer npi,npf,np0,npo1,i,ic1,ic2
       real*8 xru(2),xrv(2),xsu(2),xsv(2)
       real*8 xlenl,xlenlr,xlenr,xlenrr
       real*8 xcir,xdes
       real*8 pgx(100),pgy(100),param1,param2,param3
       real*8 distrel,distrel1,distrel2
       integer npol,npr,npoly,npolyl,npolyr

       real*8 xpoly(500),ypoly(500)
       real*8 x_poly,y_poly,x_poly1,y_poly1,x_poly2,y_poly2
       real*8 x_poly11,y_poly11,x_poly12,y_poly12
       real*8 xu,xv,alp
       real*8 xupplenew

c      Vent points
       npi=np(i,2)
       npf=np(i,2)+np(i,3)-1
       npo=npf-npi+1

c      Parameters
       xdes=csi(i,51)
       xcir=csi(i,50)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Parameters for translation and rotation ic2 ge 1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (ic2.ge.1) then
       npo1=np(i,2)
       cs1x=(uf(i,npo1,9)+uf(i,npo1,10))*0.5
       cs1y=(vf(i,npo1,9)+vf(i,npo1,10))*0.5
       cs2x=(ufv(i,1,9)+ufv(i,1,10))*0.5
       cs2y=(vfv(i,1,9)+vfv(i,1,10))*0.5
       alpha=(datan((vf(i,npo1,10)-vf(i,npo1,9))/
     + (uf(i,npo1,10)-uf(i,npo1,9))))

c      Translation -cs2x
       do j=1,npo
       ufb(i,j,9)=ufv(i,j,9)-cs2x
       ufb(i,j,10)=ufv(i,j,10)-cs2x
       ufb(i,j,11)=ufv(i,j,11)-cs2x
       ufb(i,j,12)=ufv(i,j,12)-cs2x
       vfb(i,j,9)=vfv(i,j,9)
       vfb(i,j,10)=vfv(i,j,10)
       vfb(i,j,11)=vfv(i,j,11)
       vfb(i,j,12)=vfv(i,j,12)
       end do

c      Rotation alpha and traslation csx1,cs1y
       do j=1,npo
       ufa(i,j,9)=ufb(i,j,9)*dcos(alpha)-vfv(i,j,9)*dsin(alpha)+
     + cs1x
       ufa(i,j,10)=ufb(i,j,10)*dcos(alpha)-vfv(i,j,10)*dsin(alpha)+
     + cs1x
       ufa(i,j,11)=ufb(i,j,11)*dcos(alpha)-vfv(i,j,11)*dsin(alpha)+
     + cs1x
       ufa(i,j,12)=ufb(i,j,12)*dcos(alpha)-vfv(i,j,12)*dsin(alpha)+
     + cs1x
       vfa(i,j,9)=ufb(i,j,9)*dsin(alpha)+vfv(i,j,9)*dcos(alpha)+
     + cs1y
       vfa(i,j,10)=ufb(i,j,10)*dsin(alpha)+vfv(i,j,10)*dcos(alpha)+
     + cs1y
       vfa(i,j,11)=ufb(i,j,11)*dsin(alpha)+vfv(i,j,11)*dcos(alpha)+
     + cs1y
       vfa(i,j,12)=ufb(i,j,12)*dsin(alpha)+vfv(i,j,12)*dcos(alpha)+
     + cs1y
       end do

c      BRUTE FORCE redefine
       ufa(i,1,9)=csi(i,21)
       vfa(i,1,9)=csi(i,22)
       ufa(i,1,10)=csi(i,23)
       vfa(i,1,10)=csi(i,24)
       ufa(i,1,11)=csi(i,25)
       vfa(i,1,11)=csi(i,26)
       ufa(i,1,12)=csi(i,27)
       vfa(i,1,12)=csi(i,28)

       call extpoints(i,ufa,vfa,npo,xupp,xupple,xuppte,1)

       end if ! ic2 ge 1

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Parameters for translation and rotation ic2 le -1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       If (ic2.le.-1) then

       do j=1,npo
       ufb(i,j,9)=ufv(i,j,9)
       ufb(i,j,10)=ufv(i,j,10)
       ufb(i,j,11)=ufv(i,j,11)
       ufb(i,j,12)=ufv(i,j,12)
       vfb(i,j,9)=vfv(i,j,9)
       vfb(i,j,10)=vfv(i,j,10)
       vfb(i,j,11)=vfv(i,j,11)
       vfb(i,j,12)=vfv(i,j,12)
       end do

       cs1x=(ufv(i,npo,9)+ufv(i,npo,10))*0.5
       cs1y=(vfv(i,npo,9)+vfv(i,npo,10))*0.5
       cs2x=(ufv(i,1,9)+ufv(i,1,10))*0.5
       cs2y=(vfv(i,1,9)+vfv(i,1,10))*0.5

       alpha1=(datan((vfv(i,npo,10)-vfv(i,npo,9))/
     + (ufv(i,npo,10)-ufv(i,npo,9))))
       alpha2=(datan((vfb(i,1,10)-vfb(i,1,9))/
     + (ufb(i,npo,10)-ufb(i,1,9))))

c      Translate vents from point 2 to (0,0)
             
       do j=1,npo
       ufb(i,j,9)=ufv(i,j,9)-cs1x
       ufb(i,j,10)=ufv(i,j,10)-cs1x
       ufb(i,j,11)=ufv(i,j,11)-cs1x
       ufb(i,j,12)=ufv(i,j,12)-cs1x
       vfb(i,j,9)=vfv(i,j,9)-cs1y
       vfb(i,j,10)=vfv(i,j,10)-cs1y
       vfb(i,j,11)=vfv(i,j,11)-cs1y
       vfb(i,j,12)=vfv(i,j,12)-cs1y
       end do

c      Rotation angle alpha
c      Translation using brute force (section 8.5.5) variable csi
c      TRY ANOTHER METHOD!!!!!!!!!!!!!!
c      Force last point

       alpha=-alpha1+csi(i,9)
       xcsi=(csi(i,1)+csi(i,3))*0.5
       ycsi=(csi(i,2)+csi(i,4))*0.5

       do j=1,npo
       ufa(i,j,9)=ufb(i,j,9)*dcos(alpha)-vfb(i,j,9)*dsin(alpha)+xcsi
       ufa(i,j,10)=ufb(i,j,10)*dcos(alpha)-vfb(i,j,10)*dsin(alpha)+
     + xcsi
       ufa(i,j,11)=ufb(i,j,11)*dcos(alpha)-vfb(i,j,11)*dsin(alpha)+
     + xcsi
       ufa(i,j,12)=ufb(i,j,12)*dcos(alpha)-vfb(i,j,12)*dsin(alpha)+
     + xcsi
       vfa(i,j,9)=ufb(i,j,9)*dsin(alpha)+vfb(i,j,9)*dcos(alpha)+ycsi
       vfa(i,j,10)=ufb(i,j,10)*dsin(alpha)+vfb(i,j,10)*dcos(alpha)+
     + ycsi
       vfa(i,j,11)=ufb(i,j,11)*dsin(alpha)+vfb(i,j,11)*dcos(alpha)+
     + ycsi
       vfa(i,j,12)=ufb(i,j,12)*dsin(alpha)+vfb(i,j,12)*dcos(alpha)+
     + ycsi
       end do

c      BRUTE FORCE redefine
       ufa(i,npo,9)=csi(i,1)
       vfa(i,npo,9)=csi(i,2)
       ufa(i,npo,10)=csi(i,3)
       vfa(i,npo,10)=csi(i,4)
       ufa(i,npo,11)=csi(i,5)
       vfa(i,npo,11)=csi(i,6)
       ufa(i,npo,12)=csi(i,7)
       vfa(i,npo,12)=csi(i,8)

c      USE LOW!!!
       call extpoints(i,ufa,vfa,npo,csi(i,10),csi(i,11),csi(i,12),1)

       end if ! ic1=-1

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Draw complete vent case ic2=1,6
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (ic2.eq.1.or.ic2.eq.6) then

c      Subcase ic1=1 (Print)
c      Points vent
       if (ic1.eq.1) then
       j9=npo
       j10=npo
       call prinfpv(i,j9,j10,ufa,vfa,psep,psey,csi(i,50),csi(i,51),xkf,
     + ic1,ic2)
c      Segments
       call line(psep+ufa(i,1,9),psey-vfa(i,1,9),psep+ufa(i,1,10),
     + psey-vfa(i,1,10),1)
       call line(psep+ufa(i,npo,9),psey-vfa(i,npo,9),psep+ufa(i,npo,10),
     + psey-vfa(i,npo,10),1)
       call line(psep+ufa(i,npo,24),psey-vfa(i,npo,24),
     + psep+ufa(i,npo,25),psey-vfa(i,npo,25),3)
c      Lateral unions       
       call line(psep+ufa(i,npo,24),psey-vfa(i,npo,24),
     + psep+ufa(i,npo,11),psey-vfa(i,npo,11),3)
       call line(psep+ufa(i,npo,25),psey-vfa(i,npo,25),
     + psep+ufa(i,npo,12),psey-vfa(i,npo,12),3)
c      Corners
       call line(psep+ufa(i,npo,9),psey-vfa(i,npo,9),
     + psep+ufa(i,npo,11),psey-vfa(i,npo,11),3)
       call line(psep+ufa(i,npo,10),psey-vfa(i,npo,10),
     + psep+ufa(i,npo,12),psey-vfa(i,npo,12),3)
       call line(psep+ufa(i,npo,14),psey-vfa(i,npo,14),
     + psep+ufa(i,npo,9),psey-vfa(i,npo,9),3)
       call line(psep+ufa(i,npo,15),psey-vfa(i,npo,15),
     + psep+ufa(i,npo,10),psey-vfa(i,npo,10),3)
c      Laterals
       do j=1,npo-1
       call line(psep+ufa(i,j,9),psey-vfa(i,j,9),psep+ufa(i,j+1,9),
     + psey-vfa(i,j+1,9),1)
       call line(psep+ufa(i,j,10),psey-vfa(i,j,10),psep+ufa(i,j+1,10),
     + psey-vfa(i,j+1,10),1)
       call line(psep+ufa(i,j,11),psey-vfa(i,j,11),psep+ufa(i,j+1,11),
     + psey-vfa(i,j+1,11),3)
       call line(psep+ufa(i,j,12),psey-vfa(i,j,12),psep+ufa(i,j+1,12),
     + psey-vfa(i,j+1,12),3)
       end do
c      Case ic2=6 draw elliptical inlets
       if (ic2.eq.6) then
       pgx(1)=psep+ufa(i,1,9)
       pgy(1)=psey-vfa(i,1,9)
       pgx(2)=psep+ufa(i,1,10)
       pgy(2)=psey-vfa(i,1,10)
       pgx(4)=psep+ufa(i,npo,9)
       pgy(4)=psey-vfa(i,npo,9)
       pgx(3)=psep+ufa(i,npo,10)
       pgy(3)=psey-vfa(i,npo,10)
       param1=csi(i+1,19)
       param2=csi(i+1,20)
       call elliquad(pgx,pgy,param1,param2)
       end if

       end if ! ic1=1

c      Subcase ic1=2 (Laser)
       if (ic1.eq.2) then
c      Punts vent
c      Revisar
c      xcir,xdes aquí ja està destrossat!!!!!!!!!
c       write (*,*) "HERE IS 1 ",i,csi(i,50),csi(i,51)
c       write (*,*) "HERE IS 2 ",i,xcir,xdes
c      Note csi(i,50)=xcir
c      Note csi(i,51)=xdes
       j9=npo
       j10=npo
       call prinfpv(i,j9,j10,ufa,vfa,psep,psey,csi(i,50),csi(i,51),xkf,
     + ic1,ic2)
c      Segments
       call line(psep+ufa(i,npo,24),psey-vfa(i,npo,24),
     + psep+ufa(i,npo,25),psey-vfa(i,npo,25),3)
c      Lateral unions       
       call line(psep+ufa(i,npo,24),psey-vfa(i,npo,24),
     + psep+ufa(i,npo,11),psey-vfa(i,npo,11),3)
       call line(psep+ufa(i,npo,25),psey-vfa(i,npo,25),
     + psep+ufa(i,npo,12),psey-vfa(i,npo,12),3)
c      Laterals
       do j=1,npo-1
       call line(psep+ufa(i,j,11),psey-vfa(i,j,11),psep+ufa(i,j+1,11),
     + psey-vfa(i,j+1,11),3)
       call line(psep+ufa(i,j,12),psey-vfa(i,j,12),psep+ufa(i,j+1,12),
     + psey-vfa(i,j+1,12),3)
       end do
c      Case ic2=6 draw elliptical inlets
       if (ic2.eq.6) then
       pgx(1)=psep+ufa(i,1,9)
       pgy(1)=psey-vfa(i,1,9)
       pgx(2)=psep+ufa(i,1,10)
       pgy(2)=psey-vfa(i,1,10)
       pgx(4)=psep+ufa(i,npo,9)
       pgy(4)=psey-vfa(i,npo,9)
       pgx(3)=psep+ufa(i,npo,10)
       pgy(3)=psey-vfa(i,npo,10)
       param1=csi(i+1,19)
       param2=csi(i+1,20)
       call elliquad(pgx,pgy,param1,param2)
       end if

       end if ! ic1=2

       end if ! ic2=1,6

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Draw complete vent case ic2=-1,-6 (intrados)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (ic2.eq.-1.or.ic2.eq.-6) then

c      Subcase ic1=1 (Print)
       if (ic1.eq.1) then
c      Points vent
       j9=1
       j10=1
       call prinfpv(i,j9,j10,ufa,vfa,psep,psey,csi(i,50),csi(i,51),xkf,
     + ic1,ic2)
c      Segments
       call line(psep+ufa(i,1,9),psey-vfa(i,1,9),psep+ufa(i,1,10),
     + psey-vfa(i,1,10),1)
       call line(psep+ufa(i,npo,9),psey-vfa(i,npo,9),
     + psep+ufa(i,npo,10),psey-vfa(i,npo,10),1)
       call line(psep+ufa(i,1,24),psey-vfa(i,1,24),
     + psep+ufa(i,1,25),psey-vfa(i,1,25),3)
c      Lateral unions       
       call line(psep+ufa(i,1,24),psey-vfa(i,1,24),
     + psep+ufa(i,1,11),psey-vfa(i,1,11),3)
       call line(psep+ufa(i,1,25),psey-vfa(i,1,25),
     + psep+ufa(i,1,12),psey-vfa(i,1,12),3)
c      Corners
       call line(psep+ufa(i,1,9),psey-vfa(i,1,9),
     + psep+ufa(i,1,11),psey-vfa(i,1,11),3)
       call line(psep+ufa(i,1,10),psey-vfa(i,1,10),
     + psep+ufa(i,1,12),psey-vfa(i,1,12),3)
       call line(psep+ufa(i,1,14),psey-vfa(i,1,14),
     + psep+ufa(i,1,9),psey-vfa(i,1,9),3)
       if (ufa(i,1,25).ge.ufa(i,1,15)) then ! segment correction
       call line(psep+ufa(i,1,15),psey-vfa(i,1,15),
     + psep+ufa(i,1,10),psey-vfa(i,1,10),3)
       else
       call line(psep+ufa(i,1,25),psey-vfa(i,1,25),
     + psep+ufa(i,1,10),psey-vfa(i,1,10),3)
       end if
c      Laterals
       do j=1,npo-1
       call line(psep+ufa(i,j,9),psey-vfa(i,j,9),psep+ufa(i,j+1,9),
     + psey-vfa(i,j+1,9),1)
       call line(psep+ufa(i,j,10),psey-vfa(i,j,10),psep+ufa(i,j+1,10),
     + psey-vfa(i,j+1,10),1)
       call line(psep+ufa(i,j,11),psey-vfa(i,j,11),psep+ufa(i,j+1,11),
     + psey-vfa(i,j+1,11),3)
       call line(psep+ufa(i,j,12),psey-vfa(i,j,12),psep+ufa(i,j+1,12),
     + psey-vfa(i,j+1,12),3)
       end do
c      Case ic2=-6 draw elliptical inlets
       if (ic2.eq.-6) then
       pgx(1)=psep+ufa(i,1,9)
       pgy(1)=psey-vfa(i,1,9)
       pgx(2)=psep+ufa(i,1,10)
       pgy(2)=psey-vfa(i,1,10)
       pgx(4)=psep+ufa(i,npo,9)
       pgy(4)=psey-vfa(i,npo,9)
       pgx(3)=psep+ufa(i,npo,10)
       pgy(3)=psey-vfa(i,npo,10)
       param1=csi(i+1,19)
       param2=csi(i+1,20)
       call elliquad(pgx,pgy,param1,param2)
       end if

       end if ! ic1=1

c      Subcase ic1=2 (Laser)
       if (ic1.eq.2) then
c      Points vent
       j9=1
       j10=1
       call prinfpv(i,j9,j10,ufa,vfa,psep,psey,csi(i,50),csi(i,51),xkf,
     + ic1,ic2)
c      Segments
       call line(psep+ufa(i,1,24),psey-vfa(i,1,24),
     + psep+ufa(i,1,25),psey-vfa(i,1,25),3)
c      Lateral unions       
       call line(psep+ufa(i,1,24),psey-vfa(i,1,24),
     + psep+ufa(i,1,11),psey-vfa(i,1,11),3)
       call line(psep+ufa(i,1,25),psey-vfa(i,1,25),
     + psep+ufa(i,1,12),psey-vfa(i,1,12),3)
c      Laterals
       do j=1,npo-1
       call line(psep+ufa(i,j,11),psey-vfa(i,j,11),psep+ufa(i,j+1,11),
     + psey-vfa(i,j+1,11),3)
       call line(psep+ufa(i,j,12),psey-vfa(i,j,12),psep+ufa(i,j+1,12),
     + psey-vfa(i,j+1,12),3)
       end do
c      Case ic2=-6 draw elliptical inlets
       if (ic2.eq.-6) then
       pgx(1)=psep+ufa(i,1,9)
       pgy(1)=psey-vfa(i,1,9)
       pgx(2)=psep+ufa(i,1,10)
       pgy(2)=psey-vfa(i,1,10)
       pgx(4)=psep+ufa(i,npo,9)
       pgy(4)=psey-vfa(i,npo,9)
       pgx(3)=psep+ufa(i,npo,10)
       pgy(3)=psey-vfa(i,npo,10)
       param1=csi(i+1,19)
       param2=csi(i+1,20)
       call elliquad(pgx,pgy,param1,param2)
       end if

       end if ! ic1=2

       end if ! ic2=-1,-6

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Draw complete vent case ic2=-2 (intrados)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (ic2.eq.-2) then

c      Redefine 24,14,15,25
       alpha=datan((vfa(i,npo,9)-vfa(i,1,10))/
     + (ufa(i,1,10)-ufa(i,npo,9)))
       ufa(i,1,14)=ufa(i,npo,9)-csi(i,12)*0.1*dsin(alpha)
       vfa(i,1,14)=vfa(i,npo,9)-csi(i,12)*0.1*dcos(alpha)
       ufa(i,1,15)=ufa(i,1,10)-csi(i,12)*0.1*dsin(alpha)
       vfa(i,1,15)=vfa(i,1,10)-csi(i,12)*0.1*dcos(alpha)

c      Point 24
       xru(1)=ufa(i,npo,11)
       xrv(1)=vfa(i,npo,11)
       xru(2)=ufa(i,npo-1,11)
       xrv(2)=vfa(i,npo-1,11)
       xsu(1)=ufa(i,1,14)
       xsu(2)=ufa(i,1,15)
       xsv(1)=vfa(i,1,14)
       xsv(2)=vfa(i,1,15)
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)
       ufa(i,1,24)=xtu
       vfa(i,1,24)=xtv

c      Point 25
       xru(1)=ufa(i,1,12)
       xru(2)=ufa(i,2,12)
       xrv(1)=vfa(i,1,12)
       xrv(2)=vfa(i,2,12)
       xsu(1)=ufa(i,1,14)
       xsv(1)=vfa(i,1,14)
       xsu(2)=ufa(i,1,15)
       xsv(2)=vfa(i,1,15)
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)
       ufa(i,1,25)=xtu
       vfa(i,1,25)=xtv

c      Subcase ic1=1 (Print)
       if (ic1.eq.1) then
c      Points vent
       j9=npo
       j10=1
       call prinfpv(i,j9,j10,ufa,vfa,psep,psey,csi(i,50),csi(i,51),xkf,
     + ic1,ic2)
c      Segments
       call line(psep+ufa(i,npo,9),psey-vfa(i,npo,9),psep+ufa(i,npo,10),
     + psey-vfa(i,npo,10),1)
       call line(psep+ufa(i,npo,9),psey-vfa(i,npo,9),psep+ufa(i,1,10),
     + psey-vfa(i,1,10),1)
       call line(psep+ufa(i,1,24),psey-vfa(i,1,24),
     + psep+ufa(i,1,25),psey-vfa(i,1,25),3)
c      Lateral unions       
       call line(psep+ufa(i,1,24),psey-vfa(i,1,24),
     + psep+ufa(i,npo,11),psey-vfa(i,npo,11),3)
       call line(psep+ufa(i,1,25),psey-vfa(i,1,25),
     + psep+ufa(i,1,12),psey-vfa(i,1,12),3)
c      Corners
       call line(psep+ufa(i,npo,9),psey-vfa(i,npo,9),
     + psep+ufa(i,npo,11),psey-vfa(i,npo,11),3)
       call line(psep+ufa(i,1,10),psey-vfa(i,1,10),
     + psep+ufa(i,1,12),psey-vfa(i,1,12),3)
       call line(psep+ufa(i,1,14),psey-vfa(i,1,14),
     + psep+ufa(i,npo,9),psey-vfa(i,npo,9),3)
       if (ufa(i,1,25).ge.ufa(i,1,15)) then ! segment correction
       call line(psep+ufa(i,1,15),psey-vfa(i,1,15),
     + psep+ufa(i,1,10),psey-vfa(i,1,10),3)
       else
       call line(psep+ufa(i,1,25),psey-vfa(i,1,25),
     + psep+ufa(i,1,10),psey-vfa(i,1,10),3)
       end if
c      Laterals
       do j=1,npo-1
       call line(psep+ufa(i,j,10),psey-vfa(i,j,10),psep+ufa(i,j+1,10),
     + psey-vfa(i,j+1,10),1)
       call line(psep+ufa(i,j,12),psey-vfa(i,j,12),psep+ufa(i,j+1,12),
     + psey-vfa(i,j+1,12),3)
       end do
       end if ! ic1=1

c      Subcase ic1=2 (Laser)
       if (ic1.eq.2) then
c      Points vent
       j9=npo
       j10=1
       call prinfpv(i,j9,j10,ufa,vfa,psep,psey,csi(i,50),csi(i,51),xkf,
     + ic1,ic2)
c      Segments
       call line(psep+ufa(i,1,24),psey-vfa(i,1,24),
     + psep+ufa(i,1,25),psey-vfa(i,1,25),3)
c      Lateral unions       
       call line(psep+ufa(i,1,24),psey-vfa(i,1,24),
     + psep+ufa(i,npo,11),psey-vfa(i,npo,11),3)
       call line(psep+ufa(i,1,25),psey-vfa(i,1,25),
     + psep+ufa(i,1,12),psey-vfa(i,1,12),3)
c      Laterals
       do j=1,npo-1
       call line(psep+ufa(i,j,12),psey-vfa(i,j,12),psep+ufa(i,j+1,12),
     + psey-vfa(i,j+1,12),3)
       end do
       end if ! ic1=2

       end if ! ic2=-2

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Draw complete vent case ic2=-3 (intrados)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (ic2.eq.-3) then

c      !!!!!!!!!!!!!!!! RECALCULAR !!!!!!!!!!!!!!!!!!!!!!!!

c      Redefine 24,14,15,25
       alpha=datan((vfa(i,1,9)-vfa(i,npo,10))/
     + (ufa(i,npo,10)-ufa(i,1,9)))
       ufa(i,1,14)=ufa(i,1,9)-csi(i,12)*0.1*dsin(alpha)
       vfa(i,1,14)=vfa(i,1,9)-csi(i,12)*0.1*dcos(alpha)
       ufa(i,1,15)=ufa(i,npo,10)-csi(i,12)*0.1*dsin(alpha)
       vfa(i,1,15)=vfa(i,npo,10)-csi(i,12)*0.1*dcos(alpha)

c      Point 24
       xru(1)=ufa(i,1,11)
       xrv(1)=vfa(i,1,11)
       xru(2)=ufa(i,2,11)
       xrv(2)=vfa(i,2,11)
       xsu(1)=ufa(i,1,14)
       xsu(2)=ufa(i,1,15)
       xsv(1)=vfa(i,1,14)
       xsv(2)=vfa(i,1,15)
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)
       ufa(i,1,24)=xtu
       vfa(i,1,24)=xtv

c      Point 25
       xru(1)=ufa(i,npo,12)
       xru(2)=ufa(i,npo-1,12)
       xrv(1)=vfa(i,npo,12)
       xrv(2)=vfa(i,npo-1,12)
       xsu(1)=ufa(i,1,14)
       xsv(1)=vfa(i,1,14)
       xsu(2)=ufa(i,1,15)
       xsv(2)=vfa(i,1,15)
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)
       ufa(i,1,25)=xtu
       vfa(i,1,25)=xtv

c      Subcase ic1=1 (Print)
       if (ic1.eq.1) then
c      Points vent
       j9=1
       j10=npo
       call prinfpv(i,j9,j10,ufa,vfa,psep,psey,csi(i,50),csi(i,51),xkf,
     + ic1,ic2)
c      Segments
       call line(psep+ufa(i,npo,9),psey-vfa(i,npo,9),psep+ufa(i,npo,10),
     + psey-vfa(i,npo,10),1)
       call line(psep+ufa(i,1,9),psey-vfa(i,1,9),psep+ufa(i,npo,10),
     + psey-vfa(i,npo,10),1)
       call line(psep+ufa(i,1,24),psey-vfa(i,1,24),
     + psep+ufa(i,1,25),psey-vfa(i,1,25),3)
c      Lateral unions       
       call line(psep+ufa(i,1,24),psey-vfa(i,1,24),
     + psep+ufa(i,1,11),psey-vfa(i,1,11),3)
       call line(psep+ufa(i,1,25),psey-vfa(i,1,25),
     + psep+ufa(i,npo,12),psey-vfa(i,npo,12),3)
c      Corners
       call line(psep+ufa(i,1,9),psey-vfa(i,1,9),
     + psep+ufa(i,1,11),psey-vfa(i,1,11),3)
       call line(psep+ufa(i,npo,10),psey-vfa(i,npo,10),
     + psep+ufa(i,npo,12),psey-vfa(i,npo,12),3)
       call line(psep+ufa(i,1,14),psey-vfa(i,1,14),
     + psep+ufa(i,1,9),psey-vfa(i,1,9),3)
       if (ufa(i,1,15).ge.ufa(i,1,25)) then ! segment correction
       call line(psep+ufa(i,1,25),psey-vfa(i,1,25),
     + psep+ufa(i,npo,10),psey-vfa(i,npo,10),3)
       else
       call line(psep+ufa(i,1,15),psey-vfa(i,1,15),
     + psep+ufa(i,npo,10),psey-vfa(i,npo,10),3)
       end if
c      Laterals
       do j=1,npo-1
       call line(psep+ufa(i,j,9),psey-vfa(i,j,9),psep+ufa(i,j+1,9),
     + psey-vfa(i,j+1,9),1)
       call line(psep+ufa(i,j,11),psey-vfa(i,j,11),psep+ufa(i,j+1,11),
     + psey-vfa(i,j+1,11),3)
       end do
       end if ! ic1=1

c      Subcase ic1=2 (Laser)
       if (ic1.eq.2) then
c      Points vent
       j9=1
       j10=npo
       call prinfpv(i,j9,j10,ufa,vfa,psep,psey,csi(i,50),csi(i,51),xkf,
     + ic1,ic2)
c      Segments
       call line(psep+ufa(i,1,24),psey-vfa(i,1,24),
     + psep+ufa(i,1,25),psey-vfa(i,1,25),3)
c      Lateral unions       
       call line(psep+ufa(i,1,24),psey-vfa(i,1,24),
     + psep+ufa(i,1,11),psey-vfa(i,1,11),3)
       call line(psep+ufa(i,1,25),psey-vfa(i,1,25),
     + psep+ufa(i,npo,12),psey-vfa(i,npo,12),3)
c      Laterals
       do j=1,npo-1
       call line(psep+ufa(i,j,11),psey-vfa(i,j,11),psep+ufa(i,j+1,11),
     + psey-vfa(i,j+1,11),3)
       end do
       end if ! ic1=2

       end if ! ic2=-3

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Draw complete vent case ic2=-4,-5 (intrados)
c      Advanced generic vent...
c      Use vector csi(i,k) for transport additional interpolation data
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (ic2.eq.-4.or.ic2.eq.-5) then

       if (ic2.eq.-4) then
c      Length vent left
       xlenl=0.0d0
       do j=1,npo-1
       xlenl=xlenl+dsqrt((ufa(i,j,9)-ufa(i,j+1,9))**2.+
     + (vfa(i,j,9)-vfa(i,j+1,9))**2.)
       end do
       xlenlr=xlenl*(1.-csi(i+1,19)/100.)

c      Length vent rigth
       xlenr=0.0d0
       do j=1,npo-1
       xlenr=xlenr+dsqrt((ufa(i,j,10)-ufa(i,j+1,10))**2.+
     + (vfa(i,j,10)-vfa(i,j+1,10))**2.)
       end do
       xlenrr=xlenr*(1.-csi(i+1,20)/100.)
       end if

       if (ic2.eq.-5) then
c      Length vent left
       xlenl=0.0d0
       do j=1,npo-1
       xlenl=xlenl+dsqrt((ufa(i,j,9)-ufa(i,j+1,9))**2.+
     + (vfa(i,j,9)-vfa(i,j+1,9))**2.)
       end do
       xlenlr=xlenl*(1.-csi(i+1,19)/100.)

c      Length vent rigth
       xlenr=0.0d0
       do j=1,npo-1
       xlenr=xlenr+dsqrt((ufa(i,j,10)-ufa(i,j+1,10))**2.+
     + (vfa(i,j,10)-vfa(i,j+1,10))**2.)
       end do
       xlenrr=xlenr*(1.-csi(i+1,20)/100.)

c      Deflection parameter (vent type -5)
       param3=(csi(i+1,18)/100.)*0.5*(xlenl+xlenr)
       end if

c      Interpolate new vent init
c      Left side
       do j=1,npo
       xpoly(j)=ufa(i,j,9)
       ypoly(j)=vfa(i,j,9)
       end do
       call interpoly2d(xpoly,ypoly,x_poly,y_poly,xlenlr,1,npo,npolyl,
     + distrel)
       x_poly1=x_poly
       y_poly1=y_poly
       distrel1=distrel
c      Right side
       do j=1,npo
       xpoly(j)=ufa(i,j,10)
       ypoly(j)=vfa(i,j,10)
       end do
       call interpoly2d(xpoly,ypoly,x_poly,y_poly,xlenrr,1,npo,npolyr,
     + distrel)
       x_poly2=x_poly
       y_poly2=y_poly
       distrel2=distrel

c      Points 14-15
       alpha=dabs(datan((y_poly2-y_poly1)/(x_poly2-x_poly1)))
       ufa(i,1,14)=x_poly1-csi(i,12)*0.1*dsin(alpha)
       vfa(i,1,14)=y_poly1-csi(i,12)*0.1*dcos(alpha)
       ufa(i,1,15)=x_poly2-csi(i,12)*0.1*dsin(alpha)
       vfa(i,1,15)=y_poly2-csi(i,12)*0.1*dcos(alpha)

       npoly=npolyl
       if (npolyl.eq.1) then
       npoly=2
       end if       

c      Point 24'
       xru(1)=ufa(i,npolyl+1,11)
       xrv(1)=vfa(i,npolyl+1,11)
       xru(2)=ufa(i,npolyl,11)
       xrv(2)=vfa(i,npolyl,11)
       xsu(1)=ufa(i,1,14)
       xsu(2)=ufa(i,1,15)
       xsv(1)=vfa(i,1,14)
       xsv(2)=vfa(i,1,15)
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)
       ufa(i,1,24)=xtu
       vfa(i,1,24)=xtv

c      Point 11'
       x_poly11=ufa(i,npolyl,11)+(ufa(i,npolyl+1,11)-ufa(i,npolyl,11))*
     + distrel1
       y_poly11=vfa(i,npolyl,11)+(vfa(i,npolyl+1,11)-vfa(i,npolyl,11))*
     + distrel1

c      Point 12'
       x_poly12=ufa(i,npolyr,12)+(ufa(i,npolyr+1,12)-ufa(i,npolyr,12))*
     + distrel2
       y_poly12=vfa(i,npolyr,12)+(vfa(i,npolyr+1,12)-vfa(i,npolyr,12))*
     + distrel2

       npoly=npolyr
       if (npolyr.eq.1) then
       npoly=2
       end if  

c      Point 25'
       xru(1)=ufa(i,npolyr+1,12)
       xrv(1)=vfa(i,npolyr+1,12)
       xru(2)=ufa(i,npolyr,12)
       xrv(2)=vfa(i,npolyr,12)
       xsu(1)=ufa(i,1,14)
       xsu(2)=ufa(i,1,15)
       xsv(1)=vfa(i,1,14)
       xsv(2)=vfa(i,1,15)
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)
       ufa(i,1,25)=xtu
       vfa(i,1,25)=xtv

c      Subcase ic1=1 (Print)
       if (ic1.eq.1) then

c      Points vent
c      Point left
       xu=x_poly1
       xv=y_poly1
       alp=abs(datan((vfa(i,npolyl+1,9)-vfa(i,npolyl,9)/
     + (ufa(i,npolyl+1,9)-ufa(i,npolyl,9)))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)   
       call pointg(psep+xu,-xv+psey,xcir,4)
c      Point right
       xu=x_poly2
       xv=y_poly2
       alp=abs(datan((vfa(i,npolyr+1,10)-vfa(i,npolyr,10)/
     + (ufa(i,npolyr+1,10)-ufa(i,npolyr,10)))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)   
       call pointg(psep+xu,-xv+psey,xcir,4)

c      Segments
c      Line 9-10
       call line(psep+ufa(i,npo,9),psey-vfa(i,npo,9),psep+ufa(i,npo,10),
     + psey-vfa(i,npo,10),1)

       if (ic2.eq.-4) then
c      Line 9'-10'
       call line(psep+x_poly1,psey-y_poly1,psep+x_poly2,psey-y_poly2,1)
c      Line 14'-15'
       call line(psep+ufa(i,1,14),psey-vfa(i,1,14),
     + psep+ufa(i,1,15),psey-vfa(i,1,15),3)
       end if

       if (ic2.eq.-5) then
c      Line 9'-10'
       call arcfle(psep+x_poly1,psey-y_poly1,psep+x_poly2,psey-y_poly2,
     + param3,1,1)
c      Line 14'-15'
       call arcfle(psep+ufa(i,1,14),psey-vfa(i,1,14),
     + psep+ufa(i,1,15),psey-vfa(i,1,15),param3,1,3)
       end if
     
c      Corners
c      Line 14'-24' and 15-25'
       call line(psep+ufa(i,1,14),psey-vfa(i,1,14),
     + psep+ufa(i,1,24),psey-vfa(i,1,24),3)
       call line(psep+ufa(i,1,15),psey-vfa(i,1,15),
     + psep+ufa(i,1,25),psey-vfa(i,1,25),3)
c      Line 24'-11' and 25'-12'      
       call line(psep+ufa(i,1,24),psey-vfa(i,1,24),
     + psep+x_poly11,psey-y_poly11,3)
       call line(psep+ufa(i,1,25),psey-vfa(i,1,25),
     + psep+x_poly12,psey-y_poly12,3)
c      Line 9'-11'
       call line(psep+x_poly1,psey-y_poly1,psep+x_poly11,
     + psey-y_poly11,3)
c      Line 10'-12'
       call line(psep+x_poly2,psey-y_poly2,psep+x_poly12,
     + psey-y_poly12,3)
c      Lines 9'-14' and 10'-15'
       call line(psep+x_poly1,psey-y_poly1,psep+ufa(i,1,14),
     + psey-vfa(i,1,14),3)
       call line(psep+x_poly2,psey-y_poly2,psep+ufa(i,1,15),
     + psey-vfa(i,1,15),3)

c      Laterals right
       call line(psep+ufa(i,npolyr+1,10),psey-vfa(i,npolyr+1,10),
     + psep+x_poly2,psey-y_poly2,1)
       do j=npolyr+1,npo-1
       call line(psep+ufa(i,j,10),psey-vfa(i,j,10),psep+ufa(i,j+1,10),
     + psey-vfa(i,j+1,10),1)
       end do
       call line(psep+ufa(i,npolyr+1,12),psey-vfa(i,npolyr+1,12),
     + psep+x_poly12,psey-y_poly12,3)
       do j=npolyr+1,npo-1
       call line(psep+ufa(i,j,12),psey-vfa(i,j,12),psep+ufa(i,j+1,12),
     + psey-vfa(i,j+1,12),3)
       end do
c      Laterals left
       call line(psep+ufa(i,npolyl+1,9),psey-vfa(i,npolyl+1,9),
     + psep+x_poly1,psey-y_poly1,1)
       do j=npolyl+1,npo-1
       call line(psep+ufa(i,j,9),psey-vfa(i,j,9),psep+ufa(i,j+1,9),
     + psey-vfa(i,j+1,9),1)
       end do
       call line(psep+ufa(i,npolyl+1,11),psey-vfa(i,npolyl+1,11),
     + psep+x_poly11,psey-y_poly11,3)
       do j=npolyl+1,npo-1
       call line(psep+ufa(i,j,11),psey-vfa(i,j,11),psep+ufa(i,j+1,11),
     + psey-vfa(i,j+1,11),3)
       end do

       end if ! ic1=1

c      Subcase ic1=2 (Laser)
       if (ic1.eq.2) then

c      Points vent
c      Point left
       xu=x_poly1
       xv=y_poly1
       alp=abs(datan((vfa(i,npolyl+1,9)-vfa(i,npolyl,9)/
     + (ufa(i,npolyl+1,9)-ufa(i,npolyl,9)))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)   
       call point(psep+xu,-xv+psey,7)
c      Point right
       xu=x_poly2
       xv=y_poly2
       alp=abs(datan((vfa(i,npolyr+1,10)-vfa(i,npolyr,10)/
     + (ufa(i,npolyr+1,10)-ufa(i,npolyr,10)))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)   
       call point(psep+xu,-xv+psey,7)

c      Segments

       if (ic2.eq.-4) then
c      Line 14'-15'
       call line(psep+ufa(i,1,14),psey-vfa(i,1,14),
     + psep+ufa(i,1,15),psey-vfa(i,1,15),3)
       end if

       if (ic2.eq.-5) then
c      Line 14'-15'
       call arcfle(psep+ufa(i,1,14),psey-vfa(i,1,14),
     + psep+ufa(i,1,15),psey-vfa(i,1,15),param3,1,3)
       end if

c      Corners
c      Line 14'-24' and 15-25'
       call line(psep+ufa(i,1,14),psey-vfa(i,1,14),
     + psep+ufa(i,1,24),psey-vfa(i,1,24),3)
       call line(psep+ufa(i,1,15),psey-vfa(i,1,15),
     + psep+ufa(i,1,25),psey-vfa(i,1,25),3)
c      Line 24'-11' and 25'-12'      
       call line(psep+ufa(i,1,24),psey-vfa(i,1,24),
     + psep+x_poly11,psey-y_poly11,3)
       call line(psep+ufa(i,1,25),psey-vfa(i,1,25),
     + psep+x_poly12,psey-y_poly12,3)

c      Laterals right
       call line(psep+ufa(i,npolyr+1,12),psey-vfa(i,npolyr+1,12),
     + psep+x_poly12,psey-y_poly12,3)
       do j=npolyr+1,npo-1
       call line(psep+ufa(i,j,12),psey-vfa(i,j,12),psep+ufa(i,j+1,12),
     + psey-vfa(i,j+1,12),3)
       end do
c      Laterals left
       call line(psep+ufa(i,npolyl+1,11),psey-vfa(i,npolyl+1,11),
     + psep+x_poly11,psey-y_poly11,3)
       do j=npolyl+1,npo-1
       call line(psep+ufa(i,j,11),psey-vfa(i,j,11),psep+ufa(i,j+1,11),
     + psey-vfa(i,j+1,11),3)
       end do


       end if ! ic1=2


       end if ! ic2=-4,-5

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Draw complete vent case ic2=4,5 (extrados)
c      Advanced generic vent...
c      Use vector csi(i,k) for transport additional interpolation data
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (ic2.eq.4.or.ic2.eq.5) then

c       write(*,*) "CCCCCCCC ",xupplenew


       if (ic2.eq.4) then
c      Length vent left
       xlenl=0.0d0
       do j=1,npo-1
       xlenl=xlenl+dsqrt((ufa(i,j,9)-ufa(i,j+1,9))**2.+
     + (vfa(i,j,9)-vfa(i,j+1,9))**2.)
       end do
       xlenlr=xlenl*(1.-csi(i+1,19)/100.)

c      Length vent rigth
       xlenr=0.0d0
       do j=1,npo-1
       xlenr=xlenr+dsqrt((ufa(i,j,10)-ufa(i,j+1,10))**2.+
     + (vfa(i,j,10)-vfa(i,j+1,10))**2.)
       end do
       xlenrr=xlenr*(1.-csi(i+1,20)/100.)
       end if

       if (ic2.eq.5) then
c      Length vent left
       xlenl=0.0d0
       do j=1,npo-1
       xlenl=xlenl+dsqrt((ufa(i,j,9)-ufa(i,j+1,9))**2.+
     + (vfa(i,j,9)-vfa(i,j+1,9))**2.)
       end do
       xlenlr=xlenl*(1.-csi(i+1,19)/100.)

c      Length vent rigth
       xlenr=0.0d0
       do j=1,npo-1
       xlenr=xlenr+dsqrt((ufa(i,j,10)-ufa(i,j+1,10))**2.+
     + (vfa(i,j,10)-vfa(i,j+1,10))**2.)
       end do
       xlenrr=xlenr*(1.-csi(i+1,20)/100.)

c      Deflection parameter (vent type 5)
       param3=(csi(i+1,18)/100.)*0.5*(xlenl+xlenr)
       end if

c      Interpolate new vent init
c      Left side
       do j=1,npo
       xpoly(j)=ufa(i,j,9)
       ypoly(j)=vfa(i,j,9)
       end do
       call interpoly2d(xpoly,ypoly,x_poly,y_poly,xlenlr,1,npo,npolyl,
     + distrel)
       x_poly1=x_poly
       y_poly1=y_poly
       distrel1=distrel
c      Right side
       do j=1,npo
       xpoly(j)=ufa(i,j,10)
       ypoly(j)=vfa(i,j,10)
       end do
       call interpoly2d(xpoly,ypoly,x_poly,y_poly,xlenrr,1,npo,npolyr,
     + distrel)
       x_poly2=x_poly
       y_poly2=y_poly
       distrel2=distrel

c      USE BRUTE FORCE!!!!!!!!!!!!!!
c       csi(i,13)=xupple

c      Points 14-15
       alpha=dabs(datan((y_poly2-y_poly1)/(x_poly2-x_poly1)))
       ufa(i,1,14)=x_poly1+xupple*0.1*dsin(alpha)
       vfa(i,1,14)=y_poly1+xupple*0.1*dcos(alpha)
       ufa(i,1,15)=x_poly2+xupple*0.1*dsin(alpha)
       vfa(i,1,15)=y_poly2+xupple*0.1*dcos(alpha)

       npoly=npolyl
       if (npolyl.eq.1) then
       npoly=2
       end if       

c      Point 24'
       xru(1)=ufa(i,npolyl+1,11)
       xrv(1)=vfa(i,npolyl+1,11)
       xru(2)=ufa(i,npolyl,11)
       xrv(2)=vfa(i,npolyl,11)
       xsu(1)=ufa(i,1,14)
       xsu(2)=ufa(i,1,15)
       xsv(1)=vfa(i,1,14)
       xsv(2)=vfa(i,1,15)
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)
       ufa(i,1,24)=xtu
       vfa(i,1,24)=xtv

c      Point 11'
       x_poly11=ufa(i,npolyl,11)+(ufa(i,npolyl+1,11)-ufa(i,npolyl,11))*
     + distrel1
       y_poly11=vfa(i,npolyl,11)+(vfa(i,npolyl+1,11)-vfa(i,npolyl,11))*
     + distrel1

c      Point 12'
       x_poly12=ufa(i,npolyr,12)+(ufa(i,npolyr+1,12)-ufa(i,npolyr,12))*
     + distrel2
       y_poly12=vfa(i,npolyr,12)+(vfa(i,npolyr+1,12)-vfa(i,npolyr,12))*
     + distrel2

       npoly=npolyr
       if (npolyr.eq.1) then
       npoly=2
       end if  

c      Point 25'
       xru(1)=ufa(i,npolyr+1,12)
       xrv(1)=vfa(i,npolyr+1,12)
       xru(2)=ufa(i,npolyr,12)
       xrv(2)=vfa(i,npolyr,12)
       xsu(1)=ufa(i,1,14)
       xsu(2)=ufa(i,1,15)
       xsv(1)=vfa(i,1,14)
       xsv(2)=vfa(i,1,15)
       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)
       ufa(i,1,25)=xtu
       vfa(i,1,25)=xtv

c      Subcase ic1=1 (Print)
       if (ic1.eq.1) then

c      Points vent
c      Point left
       xu=x_poly1
       xv=y_poly1
       alp=abs(datan((vfa(i,npolyl+1,9)-vfa(i,npolyl,9)/
     + (ufa(i,npolyl+1,9)-ufa(i,npolyl,9)))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)   
       call pointg(psep+xu,-xv+psey,xcir,4)
c      Point right
       xu=x_poly2
       xv=y_poly2
       alp=abs(datan((vfa(i,npolyr+1,10)-vfa(i,npolyr,10)/
     + (ufa(i,npolyr+1,10)-ufa(i,npolyr,10)))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)   
       call pointg(psep+xu,-xv+psey,xcir,4)

c      Segments
c      Line 9-10
       call line(psep+ufa(i,1,9),psey-vfa(i,1,9),psep+ufa(i,1,10),
     + psey-vfa(i,1,10),1)

       if (ic2.eq.4) then
c      Line 9'-10'
       call line(psep+x_poly1,psey-y_poly1,psep+x_poly2,psey-y_poly2,1)
c      Line 14'-15'
       call line(psep+ufa(i,1,14),psey-vfa(i,1,14),
     + psep+ufa(i,1,15),psey-vfa(i,1,15),3)
       end if

       if (ic2.eq.5) then
c      Line 9'-10'
       call arcfle(psep+x_poly1,psey-y_poly1,psep+x_poly2,psey-y_poly2,
     + param3,-1,1)
c      Line 14'-15'
       call arcfle(psep+ufa(i,1,14),psey-vfa(i,1,14),
     + psep+ufa(i,1,15),psey-vfa(i,1,15),param3,-1,3)
       end if
     
c      Corners
c      Line 14'-24' and 15-25'
       call line(psep+ufa(i,1,14),psey-vfa(i,1,14),
     + psep+ufa(i,1,24),psey-vfa(i,1,24),3)
       call line(psep+ufa(i,1,15),psey-vfa(i,1,15),
     + psep+ufa(i,1,25),psey-vfa(i,1,25),3)
c      Line 24'-11' and 25'-12'      
       call line(psep+ufa(i,1,24),psey-vfa(i,1,24),
     + psep+x_poly11,psey-y_poly11,3)
       call line(psep+ufa(i,1,25),psey-vfa(i,1,25),
     + psep+x_poly12,psey-y_poly12,3)
c      Line 9'-11'
       call line(psep+x_poly1,psey-y_poly1,psep+x_poly11,
     + psey-y_poly11,3)
c      Line 10'-12'
       call line(psep+x_poly2,psey-y_poly2,psep+x_poly12,
     + psey-y_poly12,3)
c      Lines 9'-14' and 10'-15'
       call line(psep+x_poly1,psey-y_poly1,psep+ufa(i,1,14),
     + psey-vfa(i,1,14),3)
       call line(psep+x_poly2,psey-y_poly2,psep+ufa(i,1,15),
     + psey-vfa(i,1,15),3)

c      Laterals right
       call line(psep+ufa(i,npolyr,10),psey-vfa(i,npolyr,10),
     + psep+x_poly2,psey-y_poly2,2)
       do j=1,npolyr-1
       call line(psep+ufa(i,j,10),psey-vfa(i,j,10),psep+ufa(i,j+1,10),
     + psey-vfa(i,j+1,10),1)
       end do
       call line(psep+ufa(i,npolyr,12),psey-vfa(i,npolyr,12),
     + psep+x_poly12,psey-y_poly12,2)
       do j=1,npolyr-1
       call line(psep+ufa(i,j,12),psey-vfa(i,j,12),psep+ufa(i,j+1,12),
     + psey-vfa(i,j+1,12),3)
       end do
c      Laterals left
       call line(psep+ufa(i,npolyl,9),psey-vfa(i,npolyl,9),
     + psep+x_poly1,psey-y_poly1,2)
       do j=1,npolyl-1
       call line(psep+ufa(i,j,9),psey-vfa(i,j,9),psep+ufa(i,j+1,9),
     + psey-vfa(i,j+1,9),1)
       end do
       call line(psep+ufa(i,npolyl,11),psey-vfa(i,npolyl,11),
     + psep+x_poly11,psey-y_poly11,2)
       do j=1,npolyl-1
       call line(psep+ufa(i,j,11),psey-vfa(i,j,11),psep+ufa(i,j+1,11),
     + psey-vfa(i,j+1,11),3)
       end do

       end if ! ic1=1

c      Subcase ic1=2 (Laser)
       if (ic1.eq.2) then

c      Points vent
c      Point left
       xu=x_poly1
       xv=y_poly1
       alp=abs(datan((vfa(i,npolyl+1,9)-vfa(i,npolyl,9)/
     + (ufa(i,npolyl+1,9)-ufa(i,npolyl,9)))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)   
       call point(psep+xu,-xv+psey,7)
c      Point right
       xu=x_poly2
       xv=y_poly2
       alp=abs(datan((vfa(i,npolyr+1,10)-vfa(i,npolyr,10)/
     + (ufa(i,npolyr+1,10)-ufa(i,npolyr,10)))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)   
       call point(psep+xu,-xv+psey,7)

c      Segments
       if (ic2.eq.4) then
c      Line 14'-15'
       call line(psep+ufa(i,1,14),psey-vfa(i,1,14),
     + psep+ufa(i,1,15),psey-vfa(i,1,15),3)
       end if
       if (ic2.eq.5) then
c      Line 14'-15'
       call arcfle(psep+ufa(i,1,14),psey-vfa(i,1,14),
     + psep+ufa(i,1,15),psey-vfa(i,1,15),param3,-1,3)
       end if
     
c      Corners
c      Line 14'-24' and 15-25'
       call line(psep+ufa(i,1,14),psey-vfa(i,1,14),
     + psep+ufa(i,1,24),psey-vfa(i,1,24),3)
       call line(psep+ufa(i,1,15),psey-vfa(i,1,15),
     + psep+ufa(i,1,25),psey-vfa(i,1,25),3)
c      Line 24'-11' and 25'-12'      
       call line(psep+ufa(i,1,24),psey-vfa(i,1,24),
     + psep+x_poly11,psey-y_poly11,3)
       call line(psep+ufa(i,1,25),psey-vfa(i,1,25),
     + psep+x_poly12,psey-y_poly12,3)

c      Laterals right
       call line(psep+ufa(i,npolyr,12),psey-vfa(i,npolyr,12),
     + psep+x_poly12,psey-y_poly12,3)
       do j=1,npolyr-1
       call line(psep+ufa(i,j,12),psey-vfa(i,j,12),psep+ufa(i,j+1,12),
     + psey-vfa(i,j+1,12),3)
       end do
c      Laterals left
       call line(psep+ufa(i,npolyl,11),psey-vfa(i,npolyl,11),
     + psep+x_poly11,psey-y_poly11,3)
       do j=1,npolyl-1
       call line(psep+ufa(i,j,11),psey-vfa(i,j,11),psep+ufa(i,j+1,11),
     + psey-vfa(i,j+1,11),3)
       end do

       end if ! ic1=2

       end if ! ic2=4,5

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE DRAW PANEL WITHOUT FIRST LIMIT
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Draw a complete developed panel while omitting its first end border.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in] uf Developed-panel horizontal coordinates.
!! @param[in] vf Developed-panel vertical coordinates.
!! @param[in] npo Number of profile points in the panel.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
       SUBROUTINE dpanelc1(i,uf,vf,npo,psep,psey)

       real*8 uf(0:100,500,50),vf(0:100,500,50)
       real*8 psep,psey

       do j=1,npo-1
c      Sobreamples esquerra

       call line(psep+uf(i,j,9),psey-vf(i,j,9),psep+uf(i,j+1,9),
     + psey-vf(i,j+1,9),1)

c      Sobreamples dreta

       call line(psep+uf(i,j,10),psey-vf(i,j,10),psep+uf(i,j+1,10),
     + psey-vf(i,j+1,10),1)

c      Vores de costura esquerra

       call line(psep+uf(i,j,11),psey-vf(i,j,11),psep+uf(i,j+1,11),
     + psey-vf(i,j+1,11),3)

c      Vores de costura dreta

       call line(psep+uf(i,j,12),psey-vf(i,j,12),psep+uf(i,j+1,12),
     + psey-vf(i,j+1,12),3)

       end do

c      Two horizontal segments 11-9, 10-12

       call line(psep+uf(i,npo,11),psey-vf(i,npo,11),
     + psep+uf(i,npo,9),psey-vf(i,npo,9),3)

       call line(psep+uf(i,npo,10),psey-vf(i,npo,10),
     + psep+uf(i,npo,12),psey-vf(i,npo,12),3)

c      Two vertical segments 10-15, 9-14

       call line(psep+uf(i,npo,10),psey-vf(i,npo,10),
     + psep+uf(i,npo,15),psey-vf(i,npo,15),3)
       
       call line(psep+uf(i,npo,9),psey-vf(i,npo,9),
     + psep+uf(i,npo,14),psey-vf(i,npo,14),3)

c      Two horizontal lines 14-15

       call line(psep+uf(i,npo,14),psey-vf(i,npo,14),
     + psep+uf(i,npo,15),psey-vf(i,npo,15),3)

c      Draw four corner segments

       call line(psep+uf(i,npo,14),psey-vf(i,npo,14),
     + psep+uf(i,npo,24),psey-vf(i,npo,24),3)

       call line(psep+uf(i,npo,11),psey-vf(i,npo,11),
     + psep+uf(i,npo,24),psey-vf(i,npo,24),3)

       call line(psep+uf(i,npo,15),psey-vf(i,npo,15),
     + psep+uf(i,npo,25),psey-vf(i,npo,25),3)
     
       call line(psep+uf(i,npo,12),psey-vf(i,npo,12),
     + psep+uf(i,npo,25),psey-vf(i,npo,25),3)
       
c      Init extrados
       call line(psep+uf(i,1,9),psey-vf(i,1,9),
     + psep+uf(i,1,10),psey-vf(i,1,10),1)

       call line(psep+uf(i,npo,9),psey-vf(i,npo,9),
     + psep+uf(i,npo,10),psey-vf(i,npo,10),1)
      
       return
       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE DRAW PANEL WITHOUT LAST BORDER
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Draw a complete developed panel while omitting its last end border.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in] uf Developed-panel horizontal coordinates.
!! @param[in] vf Developed-panel vertical coordinates.
!! @param[in] npo Number of profile points in the panel.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
       SUBROUTINE dpanelc2(i,uf,vf,npo,psep,psey)

       real*8 uf(0:100,500,50),vf(0:100,500,50)
       real*8 psep,psey

       do j=1,npo-1
c      Sobreamples esquerra

       call line(psep+uf(i,j,9),psey-vf(i,j,9),psep+uf(i,j+1,9),
     + psey-vf(i,j+1,9),1)

c      Sobreamples dreta

       call line(psep+uf(i,j,10),psey-vf(i,j,10),psep+uf(i,j+1,10),
     + psey-vf(i,j+1,10),1)

c      Vores de costura esquerra

       call line(psep+uf(i,j,11),psey-vf(i,j,11),psep+uf(i,j+1,11),
     + psey-vf(i,j+1,11),3)

c      Vores de costura dreta

       call line(psep+uf(i,j,12),psey-vf(i,j,12),psep+uf(i,j+1,12),
     + psey-vf(i,j+1,12),3)

       end do

c      Two horizontal segments 11-9, 10-12

       call line(psep+uf(i,1,11),psey-vf(i,1,11),
     + psep+uf(i,1,9),psey-vf(i,1,9),3)

       call line(psep+uf(i,1,10),psey-vf(i,1,10),
     + psep+uf(i,1,12),psey-vf(i,1,12),3)

c      Two vertical segments 10-15, 9-14

       call line(psep+uf(i,1,10),psey-vf(i,1,10),
     + psep+uf(i,1,15),psey-vf(i,1,15),3)
       
       call line(psep+uf(i,1,9),psey-vf(i,1,9),
     + psep+uf(i,1,14),psey-vf(i,1,14),3)

c      One horizontal lines 14-15

       call line(psep+uf(i,1,14),psey-vf(i,1,14),
     + psep+uf(i,1,15),psey-vf(i,1,15),3)

c      Draw four corner segments

       call line(psep+uf(i,1,14),psey-vf(i,1,14),
     + psep+uf(i,1,24),psey-vf(i,1,24),3)

       call line(psep+uf(i,1,11),psey-vf(i,1,11),
     + psep+uf(i,1,24),psey-vf(i,1,24),3)

       call line(psep+uf(i,1,15),psey-vf(i,1,15),
     + psep+uf(i,1,25),psey-vf(i,1,25),3)
     
       call line(psep+uf(i,1,12),psey-vf(i,1,12),
     + psep+uf(i,1,25),psey-vf(i,1,25),3)

c      Trailing edge extrados
       call line(psep+uf(i,1,9),psey-vf(i,1,9),
     + psep+uf(i,1,10),psey-vf(i,1,10),1)
                   
       return
       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE DRAW PANEL TYPE -2  WITHOUT LAST BORDER
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Draw the legacy type -2 panel variant without its last end border.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in] uf Developed-panel horizontal coordinates.
!! @param[in] vf Developed-panel vertical coordinates.
!! @param[in] npo Number of profile points in the panel.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
       SUBROUTINE dpanelcm2(i,uf,vf,npo,psep,psey)

       real*8 uf(0:100,500,50),vf(0:100,500,50)
       real*8 psep,psey

       do j=1,npo-1
c      Sobreamples esquerra

c       call line(psep+uf(i,j,9),psey-vf(i,j,9),psep+uf(i,j+1,9),
c     + psey-vf(i,j+1,9),1)

c      Sobreamples dreta

       call line(psep+uf(i,j,10),psey-vf(i,j,10),psep+uf(i,j+1,10),
     + psey-vf(i,j+1,10),1)

c      Vores de costura esquerra

c       call line(psep+uf(i,j,11),psey-vf(i,j,11),psep+uf(i,j+1,11),
c     + psey-vf(i,j+1,11),3)

c      Vores de costura dreta

       call line(psep+uf(i,j,12),psey-vf(i,j,12),psep+uf(i,j+1,12),
     + psey-vf(i,j+1,12),3)

       end do

c      Two horizontal segments 11-9, 10-12

c       call line(psep+uf(i,1,11),psey-vf(i,1,11),
c     + psep+uf(i,1,9),psey-vf(i,1,9),3)

       call line(psep+uf(i,1,10),psey-vf(i,1,10),
     + psep+uf(i,1,12),psey-vf(i,1,12),3)

c      Two vertical segments 10-15, 9-14

c       call line(psep+uf(i,1,10),psey-vf(i,1,10),
c     + psep+uf(i,1,15),psey-vf(i,1,15),3)
       
c       call line(psep+uf(i,1,9),psey-vf(i,1,9),
c     + psep+uf(i,1,14),psey-vf(i,1,14),3)

c      One horizontal lines 14-15

c       call line(psep+uf(i,1,14),psey-vf(i,1,14),
c     + psep+uf(i,1,15),psey-vf(i,1,15),3)

c      Draw four corner segments

c       call line(psep+uf(i,1,14),psey-vf(i,1,14),
c     + psep+uf(i,1,24),psey-vf(i,1,24),3)

c       call line(psep+uf(i,1,11),psey-vf(i,1,11),
c     + psep+uf(i,1,24),psey-vf(i,1,24),3)

c       call line(psep+uf(i,1,15),psey-vf(i,1,15),
c     + psep+uf(i,1,25),psey-vf(i,1,25),3)
     
c       call line(psep+uf(i,1,12),psey-vf(i,1,12),
c     + psep+uf(i,1,25),psey-vf(i,1,25),3)

c      Trailing edge extrados
c       call line(psep+uf(i,1,9),psey-vf(i,1,9),
c     + psep+uf(i,1,10),psey-vf(i,1,10),1)
                   
       return
       end





ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE DRAW PANEL TYPE -3  WITHOUT LAST BORDER
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Draw the legacy type -3 panel variant without its last end border.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in] uf Developed-panel horizontal coordinates.
!! @param[in] vf Developed-panel vertical coordinates.
!! @param[in] npo Number of profile points in the panel.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
       SUBROUTINE dpanelcm3(i,uf,vf,npo,psep,psey)

       real*8 uf(0:100,500,50),vf(0:100,500,50)
       real*8 psep,psey

       do j=1,npo-1
c      Sobreamples esquerra

       call line(psep+uf(i,j,9),psey-vf(i,j,9),psep+uf(i,j+1,9),
     + psey-vf(i,j+1,9),1)

c      Sobreamples dreta

c       call line(psep+uf(i,j,10),psey-vf(i,j,10),psep+uf(i,j+1,10),
c     + psey-vf(i,j+1,10),1)

c      Vores de costura esquerra

       call line(psep+uf(i,j,11),psey-vf(i,j,11),psep+uf(i,j+1,11),
     + psey-vf(i,j+1,11),3)

c      Vores de costura dreta

c       call line(psep+uf(i,j,12),psey-vf(i,j,12),psep+uf(i,j+1,12),
c     + psey-vf(i,j+1,12),3)

       end do

c      Two horizontal segments 11-9, 10-12

       call line(psep+uf(i,1,11),psey-vf(i,1,11),
     + psep+uf(i,1,9),psey-vf(i,1,9),3)

c       call line(psep+uf(i,1,10),psey-vf(i,1,10),
c     + psep+uf(i,1,12),psey-vf(i,1,12),3)

c      Two vertical segments 10-15, 9-14

c       call line(psep+uf(i,1,10),psey-vf(i,1,10),
c     + psep+uf(i,1,15),psey-vf(i,1,15),3)
       
c       call line(psep+uf(i,1,9),psey-vf(i,1,9),
c     + psep+uf(i,1,14),psey-vf(i,1,14),3)

c      One horizontal lines 14-15

c       call line(psep+uf(i,1,14),psey-vf(i,1,14),
c     + psep+uf(i,1,15),psey-vf(i,1,15),3)

c      Draw four corner segments

c       call line(psep+uf(i,1,14),psey-vf(i,1,14),
c     + psep+uf(i,1,24),psey-vf(i,1,24),3)

c       call line(psep+uf(i,1,11),psey-vf(i,1,11),
c     + psep+uf(i,1,24),psey-vf(i,1,24),3)

c       call line(psep+uf(i,1,15),psey-vf(i,1,15),
c     + psep+uf(i,1,25),psey-vf(i,1,25),3)
     
c       call line(psep+uf(i,1,12),psey-vf(i,1,12),
c     + psep+uf(i,1,25),psey-vf(i,1,25),3)

c      Trailing edge extrados
c       call line(psep+uf(i,1,9),psey-vf(i,1,9),
c     + psep+uf(i,1,10),psey-vf(i,1,10),1)
                   
       return
       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE DRAW PANEL ONLY BORDERS
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Draw only the seam-border geometry of a developed panel.
!! This reduced representation is used by cutting/laser output paths.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in] uf Developed-panel horizontal coordinates.
!! @param[in] vf Developed-panel vertical coordinates.
!! @param[in] npo Number of profile points in the panel.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
       SUBROUTINE dpanelb(i,uf,vf,npo,psep,psey)

       real*8 uf(0:100,500,50),vf(0:100,500,50)
       real*8 psep,psey

       do j=1,npo-1

c      Vores de costura esquerra

       call line(psep+uf(i,j,11),psey-vf(i,j,11),psep+uf(i,j+1,11),
     + psey-vf(i,j+1,11),3)

c      Vores de costura dreta

       call line(psep+uf(i,j,12),psey-vf(i,j,12),psep+uf(i,j+1,12),
     + psey-vf(i,j+1,12),3)

       end do

c      Draw eight corner segments

       call line(psep+uf(i,1,14),psey-vf(i,1,14),
     + psep+uf(i,1,24),psey-vf(i,1,24),3)

       call line(psep+uf(i,1,11),psey-vf(i,1,11),
     + psep+uf(i,1,24),psey-vf(i,1,24),3)

       call line(psep+uf(i,1,15),psey-vf(i,1,15),
     + psep+uf(i,1,25),psey-vf(i,1,25),3)
     
       call line(psep+uf(i,1,12),psey-vf(i,1,12),
     + psep+uf(i,1,25),psey-vf(i,1,25),3)

       call line(psep+uf(i,npo,14),psey-vf(i,npo,14),
     + psep+uf(i,npo,24),psey-vf(i,npo,24),3)

       call line(psep+uf(i,npo,11),psey-vf(i,npo,11),
     + psep+uf(i,npo,24),psey-vf(i,npo,24),3)

       call line(psep+uf(i,npo,15),psey-vf(i,npo,15),
     + psep+uf(i,npo,25),psey-vf(i,npo,25),3)
     
       call line(psep+uf(i,npo,12),psey-vf(i,npo,12),
     + psep+uf(i,npo,25),psey-vf(i,npo,25),3)

c      Trailing edge extrados
       call line(psep+uf(i,1,14),psey-vf(i,1,14),
     + psep+uf(i,1,15),psey-vf(i,1,15),3)
       
c      Init extrados
       call line(psep+uf(i,npo,14),psey-vf(i,npo,14),
     + psep+uf(i,npo,15),psey-vf(i,npo,15),3)
    
       return
       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE DRAW PANEL WITHOUT FIRST LIMIT (LASER)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Draw seam borders while omitting the panel's first end border.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in] uf Developed-panel horizontal coordinates.
!! @param[in] vf Developed-panel vertical coordinates.
!! @param[in] npo Number of profile points in the panel.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
       SUBROUTINE dpanelb1(i,uf,vf,npo,psep,psey)

       real*8 uf(0:100,500,50),vf(0:100,500,50)
       real*8 psep,psey

       do j=1,npo-1
  
c      Vores de costura esquerra

       call line(psep+uf(i,j,11),psey-vf(i,j,11),psep+uf(i,j+1,11),
     + psey-vf(i,j+1,11),3)

c      Vores de costura dreta

       call line(psep+uf(i,j,12),psey-vf(i,j,12),psep+uf(i,j+1,12),
     + psey-vf(i,j+1,12),3)

       end do

c      Two horizontal lines 14-15

       call line(psep+uf(i,npo,14),psey-vf(i,npo,14),
     + psep+uf(i,npo,15),psey-vf(i,npo,15),3)

c      Draw four corner segments

       call line(psep+uf(i,npo,14),psey-vf(i,npo,14),
     + psep+uf(i,npo,24),psey-vf(i,npo,24),3)

       call line(psep+uf(i,npo,11),psey-vf(i,npo,11),
     + psep+uf(i,npo,24),psey-vf(i,npo,24),3)

       call line(psep+uf(i,npo,15),psey-vf(i,npo,15),
     + psep+uf(i,npo,25),psey-vf(i,npo,25),3)
     
       call line(psep+uf(i,npo,12),psey-vf(i,npo,12),
     + psep+uf(i,npo,25),psey-vf(i,npo,25),3)
             
       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE DRAW PANEL WITHOUT LAST BORDER (LASER)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Draw seam borders while omitting the panel's last end border.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in] uf Developed-panel horizontal coordinates.
!! @param[in] vf Developed-panel vertical coordinates.
!! @param[in] npo Number of profile points in the panel.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
       SUBROUTINE dpanelb2(i,uf,vf,npo,psep,psey)

       real*8 uf(0:100,500,50),vf(0:100,500,50)
       real*8 psep,psey

       do j=1,npo-1

c      Vores de costura esquerra

       call line(psep+uf(i,j,11),psey-vf(i,j,11),psep+uf(i,j+1,11),
     + psey-vf(i,j+1,11),3)

c      Vores de costura dreta

       call line(psep+uf(i,j,12),psey-vf(i,j,12),psep+uf(i,j+1,12),
     + psey-vf(i,j+1,12),3)

       end do

c      One horizontal lines 14-15

       call line(psep+uf(i,1,14),psey-vf(i,1,14),
     + psep+uf(i,1,15),psey-vf(i,1,15),3)

c      Draw four corner segments

       call line(psep+uf(i,1,14),psey-vf(i,1,14),
     + psep+uf(i,1,24),psey-vf(i,1,24),3)

       call line(psep+uf(i,1,11),psey-vf(i,1,11),
     + psep+uf(i,1,24),psey-vf(i,1,24),3)

       call line(psep+uf(i,1,15),psey-vf(i,1,15),
     + psep+uf(i,1,25),psey-vf(i,1,25),3)
     
       call line(psep+uf(i,1,12),psey-vf(i,1,12),
     + psep+uf(i,1,25),psey-vf(i,1,25),3)
                   
       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE JONCS
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

