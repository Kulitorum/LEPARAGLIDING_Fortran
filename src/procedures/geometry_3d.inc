       SUBROUTINE panels3d(i,rib,np,u,v,w,
     + uppcuts,iupp,kiupp,lowcuts,ilow,kilow,hautok)

       integer k29d,ncuts(200),jcut(0:10),np(0:100,9)
       real*8 u(0:100,500,99),v(0:100,500,99),w(0:100,500,99)
       real*8 rib(0:100,500)
       real*8 cutamp(10),cut29(0:10,200)
       real*8 llargc,llarg(500)
       real*8 s,sm,haut,hautm,tetha,radi,scontrol
       real*8 hautmmax,hautmax,q
       real*8 hautok(0:100,500),anglok(0:100,500)

       integer iupp(10,10,200),ilow(10,10,200)
       real*8 kiupp(10,200),kilow(10,200)
       integer uppcuts(200),lowcuts(200),iflag

       real*8 punt0(3),punt1(3),punt2(3),punt3(3),punt4(3)
       real*8 aplane,bplane,cplane,dplane,xt,dp0
       real*8 aplanei(0:100),bplanei(0:100),cplanei(0:100),
     + dplanei(0:100)

       real*8 lcosd(3),mcosd(3),ncosd(3),xdis

       ng=rib(i,169)

c      1. First step: Set median airfoil between i and i-1

       do j=1,np(i,1)
       u(i,j,48)=0.5d0*(u(i-1,j,47)+u(i,j,47))
       v(i,j,48)=0.5d0*(v(i-1,j,47)+v(i,j,47))
       w(i,j,48)=0.5d0*(w(i-1,j,47)+w(i,j,47))
       end do

c      2. Second steep: Compute hautok(i,j)

       do j=1,np(i,1)

       s=dsqrt((u(i-1,j,47)-u(i,j,47))**2+(v(i-1,j,47)-v(i,j,47))**2+
     + (w(i-1,j,47)-w(i,j,47))**2)
       sm=dsqrt((u(i-1,j,69)-u(i-1,j,70))**2+(v(i-1,j,69)-v(i-1,j,70))
     + **2)

c      Numerical root finding
c      Millorar algorisme per a tensions altes, assegurar que funciona

c      (Avoid sqrt of negative numbers)
       hautmax=0.5d0*dsqrt((dabs(sm*sm-s*s)))
       q=sm*s/((8.0d0*hautmax)+0.00001d0)
       hautmmax=(q-hautmax)

       iflag=0

c       if (i.eq.1.and.j.eq.40) then
c       write (*,*) "haummax= ",hautmmax, s, sm
c       end if

c      Numerical solution by increments in hautm
       do l=0,300
       hautm=((dfloat(l)+0.00001d0)/300.0d0)*hautmmax*4.0d0  ! value 4.0 experimental (!)
       tetha=datan(0.5d0*s/hautm)
       radi=sm/(2.0d0*tetha)
       haut=radi-hautm
       scontrol=2.0d0*radi*sin(tetha)
       if (scontrol.ge.s.and.iflag.eq.0) then
       hautok(i,j)=haut
       iflag=1
c      Works these if?
       if (dabs(sm-s).le.0.01) then
       hautok(i,j)=0.0d0
       end if
       end if ! scontrol

c      CONTROL
c       if (i.eq.1.and.j.eq.40) then
c       write (*,*) l,s,sm,hautm,scontrol,hautok(i,j)
c       end if

       end do ! l

c      CONTROL
c       if (i.eq.1.and.j.eq.40) then
c       write (*,*) l,s,sm,hautm,scontrol,hautok(i,j)
c      end if

       end do ! j

c       WARNING hautok=0.0 for i=0 and i=1 (?)
c       write (*,*) "Ep ",i,hautok(i,30)
       

c      3. Compute ovalized airfoil

c      Set the plane of the airfoil
 
       punt1(1)=u(i,1,48)
       punt1(2)=v(i,1,48)
       punt1(3)=w(i,1,48)

       jjj=dint(dfloat((1+np(i,2)))/2.0d0)
       punt2(1)=u(i,jjj,48)
       punt2(2)=v(i,jjj,48)
       punt2(3)=w(i,jjj,48)
       
       punt3(1)=u(i,np(i,2),48)
       punt3(2)=v(i,np(i,2),48)
       punt3(3)=w(i,np(i,2),48)

c      Plane Ax+By+Cz+D=0
       call planeby123(punt1,punt2,punt3,aplane,bplane,cplane,
     + dplane)

       aplanei(i)=aplane
       bplanei(i)=bplane
       cplanei(i)=cplane
       dplanei(i)=dplane

c      Points 55 perpendicular to the airfoil plane
       do j=1,np(i,1)
       dp0=10.0d0
       punt0(1)=u(i,j,48)
       punt0(2)=v(i,j,48)
       punt0(3)=w(i,j,48)
       call pointp3d(punt0,aplane,bplane,cplane,dp0,punt4)
       u(i,j,55)=punt4(1)
       v(i,j,55)=punt4(2)
       w(i,j,55)=punt4(3)
       end do ! j

c      Points 49 of the ovalized airfoil
       do j=1,np(i,1)

       punt1(1)=u(i,j-1,48)
       punt1(2)=v(i,j-1,48)
       punt1(3)=w(i,j-1,48)
       punt2(1)=u(i,j+1,48)
       punt2(2)=v(i,j+1,48)
       punt2(3)=w(i,j+1,48)
       punt3(1)=u(i,j,55)
       punt3(2)=v(i,j,55)
       punt3(3)=w(i,j,55)

       if (j.eq.1) then
       punt1(1)=u(i,j,48)
       punt1(2)=v(i,j,48)
       punt1(3)=w(i,j,48)
       punt2(1)=u(i,j+1,48)
       punt2(2)=v(i,j+1,48)
       punt2(3)=w(i,j+1,48)
       punt3(1)=u(i,j,55)
       punt3(2)=v(i,j,55)
       punt3(3)=w(i,j,55)
       end if

       if (j.eq.np(i,1)) then
       punt1(1)=u(i,j-1,48)
       punt1(2)=v(i,j-1,48)
       punt1(3)=w(i,j-1,48)
       punt2(1)=u(i,j,48)
       punt2(2)=v(i,j,48)
       punt2(3)=w(i,j,48)
       punt3(1)=u(i,j-1,55)
       punt3(2)=v(i,j-1,55)
       punt3(3)=w(i,j-1,55)
       end if

c      Plane 1-2-3 perpendicular to the airfoil in point j
       call planeby123(punt1,punt2,punt3,aplane,bplane,cplane,
     + dplane)

c      Point perpendicular to the plane 1-2-3 in line passing by punt0 at dp0
       dp0=hautok(i,j)
       punt0(1)=u(i,j,48)
       punt0(2)=v(i,j,48)
       punt0(3)=w(i,j,48)
       call pointp3d(punt0,aplane,bplane,cplane,dp0,punt4)

       u(i,j,49)=punt4(1)
       v(i,j,49)=punt4(2)
       w(i,j,49)=punt4(3)

       end do ! j

c      4. Compute median airfoil in local coordinates

c      Local axes in airfoil i: 0-1,0-2,0-3

c      punt0 = nose point coordinates median airfoil
       punt0(1)=u(i,np(i,6),48)
       punt0(2)=v(i,np(i,6),48)
       punt0(3)=w(i,np(i,6),48)

c      Point1 in line perpendicular to plane 48, by point 0
       call  pointp3d(punt0,aplanei(i),bplanei(i),cplanei(i),50.0d0,
     + punt1)

c       call line3d(punt0(1),punt0(2),punt0(3),
c     + punt1(1),punt1(2),punt1(3),4)

c      Point 2 in the leading edge of median airfoil (48)
       punt2(1)=u(i,np(i,1),48)
       punt2(2)=v(i,np(i,1),48)
       punt2(3)=w(i,np(i,1),48)

c      Define plane 0-1-2
       call planeby123(punt0,punt1,punt2,aplane,bplane,cplane,
     + dplane)
       
c      Point3 in line perpendicular to plane 0-1-2, by point 0
       call pointp3d(punt0,aplane,bplane,cplane,50.0d0,punt3)

c      Now we have tree the local axys in airfoil (48) 0-1,0-2,0-3

c       call line3d(punt0(1),punt0(2),punt0(3),
c     + punt3(1),punt3(2),punt3(3),1)

c      Director cosine 0-1,0-2,0-3

       lcosd(1)=(punt1(1)-punt0(1))/50.0d0
       mcosd(1)=(punt1(2)-punt0(2))/50.0d0
       ncosd(1)=(punt1(3)-punt0(3))/50.0d0

       xdis=dsqrt((punt2(1)-punt0(1))**2+(punt2(2)-punt0(2))**2+
     + (punt2(3)-punt0(3))**2)
       lcosd(2)=(punt2(1)-punt0(1))/xdis
       mcosd(2)=(punt2(2)-punt0(2))/xdis
       ncosd(2)=(punt2(3)-punt0(3))/xdis

       lcosd(3)=(punt3(1)-punt0(1))/50.0d0
       mcosd(3)=(punt3(2)-punt0(2))/50.0d0
       ncosd(3)=(punt3(3)-punt0(3))/50.0d0

c      Compute local coordinates of the median airfoil
       call glo2loc(i,punt0,lcosd,mcosd,ncosd,u,v,w,np)
c      Return vectors u(i,j,53) > median local
c      Return vectors u(,j,54) > median ovalized


c      Define cut points

       return

       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     SUBROUTINE czinf compute zones of influence of cutts
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      REMOVE write (*,*)
c      REMOVE not used variables
c      print in lep-out.txt

       SUBROUTINE czinf(i,rib,np,u,v,w,
     + uppcuts,iupp,kiupp,lowcuts,ilow,kilow,hautok,zinf)

       integer k29d,ncuts(200),jcut(0:10),np(0:100,9)
       real*8 u(0:100,500,99),v(0:100,500,99),w(0:100,500,99)
       real*8 rib(0:100,500)
       real*8 cutamp(10),cut29(0:10,200)
       real*8 llargc,llarg(500)
       real*8 s,sm,haut,hautm,tetha,radi,scontrol
       real*8 hautmmax,hautmax,q
       real*8 hautok(0:100,500),anglok(0:100,500)

       integer iupp(10,10,200),ilow(10,10,200)
       real*8 kiupp(10,200),kilow(10,200)
       integer uppcuts(200),lowcuts(200),iflag

       real*8 punt0(3),punt1(3),punt2(3),punt3(3),punt4(3)
       real*8 aplane,bplane,cplane,dplane,xt,dp0
       real*8 aplanei(0:100),bplanei(0:100),cplanei(0:100),
     + dplanei(0:100)

       real*8 lcosd(3),mcosd(3),ncosd(3),xdis

       real*8 zinf(0:100,10,10)

       ng=rib(i,169)

c      Cases
       if (uppcuts(ng).eq.0) then
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case 1 cut extrados
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (uppcuts(ng).eq.1) then

c      Zone 1
       d1=0.0d0
       d2=0.0d0
       do j=iupp(1,2,ng),iupp(1,3,ng)-1
       d1=d1+sqrt((v(i,j,53)-v(i,j+1,53))**2.0d0+
     + (w(i,j,53)-w(i,j+1,53))**2.0d0)
       end do
       do j=iupp(1,2,ng),iupp(1,3,ng)-1
       d2=d2+sqrt((v(i,j,54)-v(i,j+1,54))**2.0d0+
     + (w(i,j,54)-w(i,j+1,54))**2.0d0)
       end do
       zinf(i,1,1)=d1
       zinf(i,2,1)=d2
       zinf(i,3,1)=0.0d0
       zinf(i,4,1)=d2-d1
       zinf(i,5,1)=zinf(i,4,1)

c      Zone 2 (not considered, beacuse j2=j3)
       zinf(i,1,2)=0.0d0
       zinf(i,2,2)=0.0d0
       zinf(i,3,2)=0.0d0
       zinf(i,4,2)=0.0d0

c      Zone 3
       d1=0.0d0
       d2=0.0d0
       do j=iupp(1,3,ng),np(i,2)-1
       d1=d1+sqrt((v(i,j,53)-v(i,j+1,53))**2.0d0+
     + (w(i,j,53)-w(i,j+1,53))**2.0d0)
       end do
       do j=iupp(1,3,ng),np(i,2)-1
       d2=d2+sqrt((v(i,j,54)-v(i,j+1,54))**2.0d0+
     + (w(i,j,54)-w(i,j+1,54))**2.0d0)
       end do
       zinf(i,1,3)=d1
       zinf(i,2,3)=d2
       zinf(i,3,3)=d2-d1
       zinf(i,4,3)=0.0d0
       zinf(i,5,3)=zinf(i,3,3)

c      Compatibility vaule in first cut
       zinf(i,6,1)=0.5d0*(zinf(i,5,3)+zinf(i,5,1))*kiupp(1,ng)

       end if ! Case 1 cut extrados

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case 2 cuts extrados
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (uppcuts(ng).eq.2) then

c      Zone 1
       d1=0.0d0
       d2=0.0d0
       do j=iupp(1,2,ng),iupp(1,3,ng)-1
       d1=d1+sqrt((v(i,j,53)-v(i,j+1,53))**2.0d0+
     + (w(i,j,53)-w(i,j+1,53))**2.0d0)
       end do
       do j=iupp(1,2,ng),iupp(1,3,ng)-1
       d2=d2+sqrt((v(i,j,54)-v(i,j+1,54))**2.0d0+
     + (w(i,j,54)-w(i,j+1,54))**2.0d0)
       end do
       zinf(i,1,1)=d1
       zinf(i,2,1)=d2
       zinf(i,3,1)=0.0d0
       zinf(i,4,1)=d2-d1
       zinf(i,5,1)=zinf(i,4,1)
       
c      Zone 2
       d1=0.0d0
       d2=0.0d0
       do j=iupp(2,2,ng),iupp(2,3,ng)-1
       d1=d1+dsqrt((v(i,j,53)-v(i,j+1,53))**2.0d0+
     + (w(i,j,53)-w(i,j+1,53))**2.0d0)
       end do
       do j=iupp(2,2,ng),iupp(2,3,ng)-1
       d2=d2+dsqrt((v(i,j,54)-v(i,j+1,54))**2.0d0+
     + (w(i,j,54)-w(i,j+1,54))**2.0d0)
       end do
       zinf(i,1,2)=d1
       zinf(i,2,2)=d2
       zinf(i,3,2)=0.5d0*(d2-d1)
       zinf(i,4,2)=0.5d0*(d2-d1)
       zinf(i,5,2)=zinf(i,3,2)*2.0d0

c      Compatibility value first cut
       zinf(i,6,1)=0.5d0*(zinf(i,5,2)*0.5d0+zinf(i,5,1))*kiupp(1,ng)

c      Zone 3
       d1=0.0d0
       d2=0.0d0
       do j=iupp(2,3,ng),np(i,2)-1
       d1=d1+sqrt((v(i,j,53)-v(i,j+1,53))**2.0d0+
     + (w(i,j,53)-w(i,j+1,53))**2.0d0)
       end do
       do j=iupp(2,3,ng),np(i,2)-1
       d2=d2+sqrt((v(i,j,54)-v(i,j+1,54))**2.0d0+
     + (w(i,j,54)-w(i,j+1,54))**2.0d0)
       end do
       zinf(i,1,3)=d1
       zinf(i,2,3)=d2
       zinf(i,3,3)=d2-d1
       zinf(i,4,3)=0.0d0
       zinf(i,5,3)=zinf(i,3,3)

c      Compatibility value second cut
       zinf(i,6,2)=0.5d0*(zinf(i,5,3)+zinf(i,5,2)*0.5d0)*kiupp(2,ng)

       end if ! Case 2 cut extrados

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case vents
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Zone 4
       d1=0.0d0
       d2=0.0d0
       do j=np(i,2),np(i,2)+np(i,3)-2
       d1=d1+sqrt((v(i,j,53)-v(i,j+1,53))**2.0d0+
     + (w(i,j,53)-w(i,j+1,53))**2.0d0)
       end do
       do j=np(i,2),np(i,2)+np(i,3)-2
       d2=d2+sqrt((v(i,j,54)-v(i,j+1,54))**2.0d0+
     + (w(i,j,54)-w(i,j+1,54))**2.0d0)
       end do
       zinf(i,1,4)=d1
       zinf(i,2,4)=d2
       zinf(i,3,4)=0.5d0*(d2-d1)
       zinf(i,4,4)=0.5d0*(d2-d1)
       zinf(i,5,4)=2.0d0*zinf(i,4,4)
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case 1 cut intrados
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (lowcuts(ng).eq.1) then

c      Zone 5
       d1=0.0d0
       d2=0.0d0
       do j=np(i,2)+np(i,3)-1,ilow(1,2,ng)-1
       d1=d1+sqrt((v(i,j,53)-v(i,j+1,53))**2.0d0+
     + (w(i,j,53)-w(i,j+1,53))**2.0d0)
       end do
       do j=np(i,2)+np(i,3)-1,ilow(1,2,ng)-1
       d2=d2+sqrt((v(i,j,54)-v(i,j+1,54))**2.0d0+
     + (w(i,j,54)-w(i,j+1,54))**2.0d0)
       end do
       zinf(i,1,5)=d1
       zinf(i,2,5)=d2
       zinf(i,3,5)=0.0d0
       zinf(i,4,5)=d2-d1
       zinf(i,5,5)=zinf(i,4,5)
 
c      Zone 6
       d1=0.0d0
       d2=0.0d0
       do j=ilow(1,2,ng),ilow(1,3,ng)-1
       d1=d1+sqrt((v(i,j,53)-v(i,j+1,53))**2.0d0+
     + (w(i,j,53)-w(i,j+1,53))**2.0d0)
       end do
       do j=ilow(1,2,ng),ilow(1,3,ng)-1
       d2=d2+sqrt((v(i,j,54)-v(i,j+1,54))**2.0d0+
     + (w(i,j,54)-w(i,j+1,54))**2.0d0)
       end do
       zinf(i,1,6)=d1
       zinf(i,2,6)=d2
       zinf(i,3,6)=d2-d1
       zinf(i,4,6)=0.0d0
       zinf(i,5,6)=zinf(i,3,6)

c      Compatibility value first cut intrados
       zinf(i,6,4)=0.5d0*(zinf(i,5,6)+zinf(i,5,5))*kilow(1,ng)

       end if ! Case 1 cut intrados

       return

       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     SUBROUTINE planeby123
c     Plane Ax+By+Cz+D=0 defined by points 1,2,3
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       SUBROUTINE planeby123(punt1,punt2,punt3,aplane,bplane,cplane,
     + dplane)

       implicit none
       real*8 punt1(3),punt2(3),punt3(3)
       real*8 aplane,bplane,cplane,dplane
       type(vector_3d) point1,point2,point3
       type(plane_3d) plane

       point1=vector_3d(punt1(1),punt1(2),punt1(3))
       point2=vector_3d(punt2(1),punt2(2),punt2(3))
       point3=vector_3d(punt3(1),punt3(2),punt3(3))
       plane=plane_through_points(point1,point2,point3)

       aplane=plane%normal%x
       bplane=plane%normal%y
       cplane=plane%normal%z
       dplane=plane%offset

       return

       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Unitary vector and normal to plane by 123
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       SUBROUTINE unitaryto123(unitv,
     + aplane,bplane,cplane)

       implicit none
       real*8 unitv(3)
       real*8 aplane,bplane,cplane
       type(vector_3d) normal,unit_normal

       normal=vector_3d(aplane,bplane,cplane)
       unit_normal=normalize(normal)
       unitv(1)=unit_normal%x
       unitv(2)=unit_normal%y
       unitv(3)=unit_normal%z

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     SUBROUTINE pointp3d
c     Point by 0 perpendicular to plane Ax+By+Cz+D=0 at distance d
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


       SUBROUTINE pointp3d(punt0,aplane,bplane,cplane,dp0,punt4)

       implicit none
       real*8 punt0(3),punt4(3)
       real*8 aplane,bplane,cplane,dp0
       type(vector_3d) point,normal,output

       point=vector_3d(punt0(1),punt0(2),punt0(3))
       normal=vector_3d(aplane,bplane,cplane)
       output=point_normal_to_plane(point,normal,dp0)

       punt4(1)=output%x
       punt4(2)=output%y
       punt4(3)=output%z

       return

       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     SUBROUTINE glo2loc
c     Transform global 3d coordinates to local coordinates
c     Specifically applied to middle airfoil index 53 and 54
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      (x0,y0,z0) center of local axes
c      Index 48 = 3D median airfoil
c      Index 49 = 3D ovalized median airfoil
c      Index 53 = 2D median airfoil
c      Index 54 = 2D ovalized median airfoil

       SUBROUTINE glo2loc(i,punt0,lcosd,mcosd,ncosd,u,v,w,np)

       real*8 lcosd(3),mcosd(3),ncosd(3)
       real*8 punt0(3)
       real*8 u(0:100,500,99),v(0:100,500,99),w(0:100,500,99)
       integer np(0:100,9)

       x0=punt0(1)
       y0=punt0(2)
       z0=punt0(3)

       do j=1,np(i,1)
       u(i,j,53)=lcosd(1)*(u(i,j,48)-punt0(1))+
     + mcosd(1)*(v(i,j,48)-punt0(2))+ncosd(1)*(w(i,j,48)-punt0(3))
       v(i,j,53)=lcosd(2)*(u(i,j,48)-punt0(1))+
     + mcosd(2)*(v(i,j,48)-punt0(2))+ncosd(2)*(w(i,j,48)-punt0(3))
       w(i,j,53)=lcosd(3)*(u(i,j,48)-punt0(1))+
     + mcosd(3)*(v(i,j,48)-punt0(2))+ncosd(3)*(w(i,j,48)-punt0(3))

       u(i,j,54)=lcosd(1)*(u(i,j,49)-punt0(1))+
     + mcosd(1)*(v(i,j,49)-punt0(2))+ncosd(1)*(w(i,j,49)-punt0(3))
       v(i,j,54)=lcosd(2)*(u(i,j,49)-punt0(1))+
     + mcosd(2)*(v(i,j,49)-punt0(2))+ncosd(2)*(w(i,j,49)-punt0(3))
       w(i,j,54)=lcosd(3)*(u(i,j,49)-punt0(1))+
     + mcosd(3)*(v(i,j,49)-punt0(2))+ncosd(3)*(w(i,j,49)-punt0(3))
       end do
  
       return

       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     SUBROUTINE print01
c     Print TE extrados
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

