       SUBROUTINE joncs(i,u,v,rib,xintra,xextra,xjonc,npo,atp,np)

       real*8 u(0:100,500,99),v(0:100,500,99)
       real*8 xextra(0:100,10),xintra(0:100,10)
       real*8 xjonc(0:100,0:500,10),k1,k2
       real*8 x1i,x1f,x2i,x2f
       real*8 x1pi,x1pf,x2pi,x2pf
       real*8 x1,x2,x3,x4,y1,y2,y3,y4
       real*8 rib(0:100,500)
       integer np(0:100,9)
       character*2 atp

       ng=int(rib(i,166))

       if (ng.ne.0) then ! Only if rod is defined

c      Read airfoil i and detect transition segments

c      Set provisional virtual point 3 "ss"
       if (atp.eq."ss".and.xintra(ng,2).gt.rib(i,12)) then 
       
       j=np(i,2)+np(i,3)-1

       x4=u(i,j+1,3) ! safe vector
       y4=v(i,j+1,3) ! safe vector

       x1=u(i,j-1,3)
       y1=v(i,j-1,3)
       x2=u(i,j,3)
       y2=v(i,j,3)
       x3=xintra(ng,2)*rib(i,5)/100.0d0
      
       call interpola(x1,y1,x2,y2,x3,y3)
  
       u(i,j+1,3)=x3 ! modification
       v(i,j+1,3)=y3 ! modification      
       end if

c      Set initial and final points
       x1f=xextra(ng,2)*rib(i,5)/100.0d0
       x1i=xextra(ng,1)*rib(i,5)/100.0d0
       x2f=xintra(ng,2)*rib(i,5)/100.0d0
       x2i=xintra(ng,1)*rib(i,5)/100.0d0

c      Set points to explore
       if (atp.eq."ss") then 
       npunts=np(i,2)+np(i,3)-1
       end if
       if (atp.ne."ss") then 
       npunts=np(i,1)-1
       end if

       do j=1,npunts! Explore airfoil

c       do j=1,np(i,2)+np(i,3)-1

c      Final point extrados
       if (u(i,j,3).ge.x1f.and.u(i,j+1,3).lt.x1f.and.v(i,j,3).ge.0) 
     + then 
       j1f=j
       x1=u(i,j+1,3)
       y1=v(i,j+1,3)
       x2=u(i,j,3)
       y2=v(i,j,3)
       x3=x1f
       call interpola(x1,y1,x2,y2,x3,y3)
       x1pf=x3
       y1pf=y3
       end if

c      Initial point extrados
       if (u(i,j,3).ge.x1i.and.u(i,j+1,3).lt.x1i.and.v(i,j,3).ge.0) 
     + then 
       j1i=j
       x1=u(i,j+1,3)
       y1=v(i,j+1,3)
       x2=u(i,j,3)
       y2=v(i,j,3)
       x3=x1i
       call interpola(x1,y1,x2,y2,x3,y3)
       x1pi=x3
       y1pi=y3
       end if

       np1=j1i-j1f+2   !!! WARNING +2 or +1?

c      Initial point intrados
       if (u(i,j,3).le.x2i.and.u(i,j+1,3).gt.x2i.and.v(i,j,3).lt.0) 
     + then 
       j2i=j
       x1=u(i,j,3)
       y1=v(i,j,3)
       x2=u(i,j+1,3)
       y2=v(i,j+1,3)
       x3=x2i
       call interpola(x1,y1,x2,y2,x3,y3)
       x2pi=x3
       y2pi=y3
       end if

c      Final point intrados
c      Warning amb el ge.x2f 
       if (u(i,j,3).le.x2f.and.u(i,j+1,3).ge.x2f.and.v(i,j,3).lt.0) 
     + then 
       j2f=j
       x1=u(i,j,3)
       y1=v(i,j,3)
       x2=u(i,j+1,3)
       y2=v(i,j+1,3)
       x3=x2f
       call interpola(x1,y1,x2,y2,x3,y3)
       x2pf=x3
       y2pf=y3
       end if

       np2=j1f-j1i+2   !!! WARNING +2 or +1?

       end do   ! j in airfoil  

c      Define jonc line 0 (without deflection)
       
c      Extrados segment
       xjonc(i,j1f,1)=x1pf
       xjonc(i,j1f,2)=y1pf
       do j=j1f+1,j1i
       xjonc(i,j,1)=u(i,j,3)
       xjonc(i,j,2)=v(i,j,3)
       end do

c      Nose segment
       xjonc(i,j1i+1,1)=x1pi
       xjonc(i,j1i+1,2)=y1pi
       do j=j1i+1,j2i
       xjonc(i,j+1,1)=u(i,j,3)
       xjonc(i,j+1,2)=v(i,j,3)
       end do

c      Intrados segment
       xjonc(i,j2i+2,1)=x2pi
       xjonc(i,j2i+2,2)=y2pi
       do j=j2i+1,j2f
       xjonc(i,j+2,1)=u(i,j,3)
       xjonc(i,j+2,2)=v(i,j,3)
       end do
       xjonc(i,j2f+3,1)=x2pf
       xjonc(i,j2f+3,2)=y2pf

       npo=j2f-j1f+4  !!! WARNING +4 or +2?

c      Add jonc deflections
       k1=(xextra(ng,3)*rib(i,5)/100.0d0)/((x1f-x1i)**xextra(ng,4))
       k2=(xintra(ng,3)*rib(i,5)/100.0d0)/((x2f-x2i)**xintra(ng,4))

       do j=j1i+1,j1f,-1
       xjonc(i,j,2)=xjonc(i,j,2)-k1*((xjonc(i,j,1))-x1i)**xextra(ng,4)
       end do

       do j=j2i+2,j2f+3
       xjonc(i,j,2)=xjonc(i,j,2)+k2*((xjonc(i,j,1))-x2i)**xintra(ng,4)
       end do

c      Reformat jonc
       do j=1,npo
       xjonc(i,j,3)=xjonc(i,j+j1f-1,1)
       xjonc(i,j,4)=xjonc(i,j+j1f-1,2)
       end do

c      Reformat jonc again in local index (change name)
       do j=1,npo
       xjonc(i,j,1)=xjonc(i,j,3)
       xjonc(i,j,2)=xjonc(i,j,4)
       end do

c      Restitution of virtual point 3
       if (atp.eq."ss".and.xintra(ng,2).gt.rib(i,12)) then 
       j=np(i,2)+np(i,3)-1
       u(i,j+1,3)=x4
       v(i,j+1,3)=y4
       end if
  
       end if ! ng.ne.0

       return
       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE JONCS TYPE 3 SUP
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       SUBROUTINE joncst3s(i,u,v,rib,xintra,xextra,xjonc,npo,atp,np)

       real*8 u(0:100,500,99),v(0:100,500,99)
       real*8 xextra(0:100,10),xintra(0:100,10)
       real*8 xjonc(0:100,0:500,10),k1,k2
       real*8 x1i,x1f,x2i,x2f,x3i,x3f,x4i,x4f
       real*8 x1pi,x1pf,x2pi,x2pf
       real*8 y1pi,y1pf,y2pi,y2pf
       real*8 x3pi,x3pf,x4pi,x4pf
       real*8 y3pi,y3pf,y4pi,y4pf
       real*8 ae,be,nse,ai,bi,nsi

       real*8 x1,x2,x3,x4,y1,y2,y3,y4
       real*8 rib(0:100,500)
       integer np(0:100,9)
       character*2 atp

      real*8 xloc(0:3000),yloc(0:3000)
      real*8 xglo(0:3000),yglo(0:3000)
      real*8 xglo3(0:50),yglo3(0:50),xglo4(0:50),yglo4(0:50)
      real*8 angle,pi

      pi=4.0d0*datan(1.0d0)


       ng=int(rib(i,166))

       if (ng.ne.0) then ! Only if rod is defined

c      Read airfoil i and detect transition segments

c      Set provisional virtual point 3 "ss"
       if (atp.eq."ss".and.xintra(ng,2).gt.rib(i,12)) then 
       
       j=np(i,2)+np(i,3)-1 ! vent out

       x4=u(i,j+1,3) ! safe vector
       y4=v(i,j+1,3) ! safe vector

       x1=u(i,j-1,3)
       y1=v(i,j-1,3)
       x2=u(i,j,3)
       y2=v(i,j,3)
       x3=xintra(ng,2)*rib(i,5)/100.0d0
      
       call interpola(x1,y1,x2,y2,x3,y3)
  
       u(i,j+1,3)=x3 ! modification
       v(i,j+1,3)=y3 ! modification      
       end if

c      Set initial and final points
       x1f=xextra(ng,2)*rib(i,5)/100.0d0
       x1i=xextra(ng,1)*rib(i,5)/100.0d0
       x2f=xintra(ng,2)*rib(i,5)/100.0d0
       x2i=xintra(ng,1)*rib(i,5)/100.0d0
       x3i=xextra(ng,5)*rib(i,5)/100.0d0
       x4i=xintra(ng,5)*rib(i,5)/100.0d0

c      Set points to explore
       if (atp.eq."ss") then ! Explore extrados and inlet
       npunts=np(i,2)+np(i,3)-1
       end if
       if (atp.ne."ss") then ! Explore all airfoil
       npunts=np(i,1)-1
       end if

       do j=1,npunts! Explore airfoil

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      1 Extrados modificaction
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Final point extrados x1pf
       if (u(i,j,3).ge.x1f.and.u(i,j+1,3).lt.x1f.and.v(i,j,3).ge.0) 
     + then 
       j1f=j
       x1=u(i,j+1,3)
       y1=v(i,j+1,3)
       x2=u(i,j,3)
       y2=v(i,j,3)
       x3=x1f
       call interpola(x1,y1,x2,y2,x3,y3)
       x1pf=x3
       y1pf=y3
       end if
c      Initial point extrados x1pi
       if (u(i,j,3).ge.x1i.and.u(i,j+1,3).lt.x1i.and.v(i,j,3).ge.0) 
     + then 
       j1i=j
       x1=u(i,j+1,3)
       y1=v(i,j+1,3)
       x2=u(i,j,3)
       y2=v(i,j,3)
       x3=x1i
       call interpola(x1,y1,x2,y2,x3,y3)
       x1pi=x3
       y1pi=y3
       end if
       np1=j1i-j1f+1 ! Points in extra modificacion

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Point xsne (3)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (u(i,j,3).le.x3i.and.u(i,j+1,3).gt.x3i.and.v(i,j,3).lt.0) 
     + then 
       j3i=j
       x1=u(i,j,3)
       y1=v(i,j,3)
       x2=u(i,j+1,3)
       y2=v(i,j+1,3)
       x3=x3i
       call interpola(x1,y1,x2,y2,x3,y3)
       x3pi=x3
       y3pi=y3
       end if

       end do   ! j in airfoil  

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Define jonc line 0 (without deflection)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
c      Extrados segment (1)
       xjonc(i,j1f,1)=x1pf
       xjonc(i,j1f,2)=y1pf
       do j=j1f+1,j1i
       xjonc(i,j,1)=u(i,j,3)
       xjonc(i,j,2)=v(i,j,3)
       end do

c      Nose segment (1-3)
       xjonc(i,j1i+1,1)=x1pi
       xjonc(i,j1i+1,2)=y1pi
       do j=j1i+1,j3i
       xjonc(i,j,1)=u(i,j,3)
       xjonc(i,j,2)=v(i,j,3)
       end do
       np13=j3i-j1i+1

c      Parabola 3 (intrados transition upper rod) 11 points
       ae=xextra(ng,6)*rib(i,5)/100.0d0
       be=xextra(ng,7)*rib(i,5)/100.0d0
       nse=xextra(ng,8)*rib(i,5)/100.0d0
       do k=1,11
       xloc(k)=ae*dfloat(k-1)/10.0d0
       yloc(k)=(be/(ae**nse))*xloc(k)**nse
       end do
       angle=(180./pi)*datan((v(i,j3i-2,3)-y3pi)/(u(i,j3i-2,3)-x3pi))
       call loc2glo2D(11,xloc,yloc,xglo,yglo,x3pi,y3pi,angle)
       do k=1,11
       xglo3(k)=xglo(k)
       yglo3(k)=yglo(k)
       xjonc(i,j3i+k-1,1)=xglo3(k)
       xjonc(i,j3i+k-1,2)=yglo3(k)
       end do
       np3=11

c      Add jonc deflections
       k1=(xextra(ng,3)*rib(i,5)/100.0d0)/((x1f-x1i)**xextra(ng,4))
       k2=(xintra(ng,3)*rib(i,5)/100.0d0)/((x2f-x2i)**xintra(ng,4))

       do j=j1i+1,j1f,-1
       xjonc(i,j,2)=xjonc(i,j,2)-k1*((xjonc(i,j,1))-x1i)**xextra(ng,4)
       end do

c      FISRT ROD (!) 1+nose13+3

       npo=np1+np13+np3-2
c       write (*,*) "np1 np13 np3 npo",np1,np13,np3,npo

c      Reformat jonc
       do j=1,npo
       xjonc(i,j,3)=xjonc(i,j+j1f-1,1)
       xjonc(i,j,4)=xjonc(i,j+j1f-1,2)
       end do

c      Reformat jonc again (change name)
       do j=1,npo
       xjonc(i,j,1)=xjonc(i,j,3)
       xjonc(i,j,2)=xjonc(i,j,4)
       end do

c      Restitution of virtual point 3
       if (atp.eq."ss".and.xintra(ng,2).gt.rib(i,12)) then 
       j=np(i,2)+np(i,3)-1
       u(i,j+1,3)=x4
       v(i,j+1,3)=y4
       end if
  
       end if ! ng.ne.0

       return
       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE JONCS TYPE 3 INF
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       SUBROUTINE joncst3i(i,u,v,rib,xintra,xextra,xjonc,npo,atp,np)

       real*8 u(0:100,500,99),v(0:100,500,99)
       real*8 xextra(0:100,10),xintra(0:100,10)
       real*8 xjonc(0:100,0:500,10),k1,k2
       real*8 x1i,x1f,x2i,x2f,x3i,x3f,x4i,x4f
       real*8 x1pi,x1pf,x2pi,x2pf
       real*8 y1pi,y1pf,y2pi,y2pf
       real*8 x3pi,x3pf,x4pi,x4pf
       real*8 y3pi,y3pf,y4pi,y4pf
       real*8 ae,be,nse,ai,bi,nsi

       real*8 x1,x2,x3,x4,y1,y2,y3,y4
       real*8 rib(0:100,500)
       integer np(0:100,9)
       character*2 atp

      real*8 xloc(0:3000),yloc(0:3000)
      real*8 xglo(0:3000),yglo(0:3000)
      real*8 xglo3(0:50),yglo3(0:50),xglo4(0:50),yglo4(0:50)
      real*8 angle,pi

      pi=4.0d0*datan(1.0d0)


       ng=int(rib(i,166))

       if (ng.ne.0) then ! Only if rod is defined

c      Read airfoil i and detect transition segments

c      Set provisional virtual point 3 "ss"
       if (atp.eq."ss".and.xintra(ng,2).gt.rib(i,12)) then 
       
       j=np(i,2)+np(i,3)-1 ! vent out

       x4=u(i,j+1,3) ! safe vector
       y4=v(i,j+1,3) ! safe vector

       x1=u(i,j-1,3)
       y1=v(i,j-1,3)
       x2=u(i,j,3)
       y2=v(i,j,3)
       x3=xintra(ng,2)*rib(i,5)/100.0d0
      
       call interpola(x1,y1,x2,y2,x3,y3)
  
       u(i,j+1,3)=x3 ! modification
       v(i,j+1,3)=y3 ! modification      
       end if

c      Set initial and final points
       x1f=xextra(ng,2)*rib(i,5)/100.0d0
       x1i=xextra(ng,1)*rib(i,5)/100.0d0
       x2f=xintra(ng,2)*rib(i,5)/100.0d0
       x2i=xintra(ng,1)*rib(i,5)/100.0d0
       x3i=xextra(ng,5)*rib(i,5)/100.0d0
       x4i=xintra(ng,5)*rib(i,5)/100.0d0

c       write (*,*) "intra ",xintra(ng,5)

c      Set points to explore
       if (atp.eq."ss") then ! Explore extrados and inlet
       npunts=np(i,2)+np(i,3)-1
       end if
       if (atp.ne."ss") then ! Explore all airfoil
       npunts=np(i,1)-1
       end if

       do j=1,npunts! Explore airfoil

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
c      Point xsni (4)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (u(i,j,3).le.x4i.and.u(i,j+1,3).gt.x4i.and.v(i,j,3).lt.0) 
     + then 
       j4i=j
       x1=u(i,j,3)
       y1=v(i,j,3)
       x2=u(i,j+1,3)
       y2=v(i,j+1,3)
       x3=x4i
       call interpola(x1,y1,x2,y2,x3,y3)
       x4pi=x3
       y4pi=y3
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      2 Intrados modificaction
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Initial point intrados
       if (u(i,j,3).le.x2i.and.u(i,j+1,3).gt.x2i.and.v(i,j,3).lt.0) 
     + then 
       j2i=j
       x1=u(i,j,3)
       y1=v(i,j,3)
       x2=u(i,j+1,3)
       y2=v(i,j+1,3)
       x3=x2i
       call interpola(x1,y1,x2,y2,x3,y3)
       x2pi=x3
       y2pi=y3
       end if
c      Final point intrados
c      Warning amb el ge.x2f 
       if (u(i,j,3).le.x2f.and.u(i,j+1,3).ge.x2f.and.v(i,j,3).lt.0) 
     + then 
       j2f=j
       x1=u(i,j,3)
       y1=v(i,j,3)
       x2=u(i,j+1,3)
       y2=v(i,j+1,3)
       x3=x2f
       call interpola(x1,y1,x2,y2,x3,y3)
       x2pf=x3
       y2pf=y3
       end if
       np2=j2f-j2i+1 ! Points in intr modification
c      WARNING!!!! +1 or +2 ?????????????
c      Review joncst3s !!!!!

       end do   ! j in airfoil  

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Define jonc line 0 (without deflection)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
c      Nose segment (4-2)
       xjonc(i,j4i,1)=x4pi
       xjonc(i,j4i,2)=y4pi
       do j=j4i,j2i
       xjonc(i,j,1)=u(i,j,3)
       xjonc(i,j,2)=v(i,j,3)
       end do
       np42=j2i-j4i+1

c       write (*,*) "j41 j2i j2f",j4i,j2i,j2f

c      Parabola 4 (extrados transition lower rod) 11 points
       ai=xintra(ng,6)*rib(i,5)/100.0d0
       bi=-xintra(ng,7)*rib(i,5)/100.0d0
       nsi=xintra(ng,8)*rib(i,5)/100.0d0
       do k=1,11
       xloc(k)=ai*dfloat(k-1)/10.0d0
       yloc(k)=(bi/(ai**nsi))*xloc(k)**nsi
c       write (*,*) "4",k,xloc(k),yloc(k)
       end do
       angle=-180.0d0+
     + (180.0d0/pi)*datan((v(i,j4i+2,3)-y4pi)/(u(i,j4i+2,3)-x4pi))
       call loc2glo2D(11,xloc,yloc,xglo,yglo,x4pi,y4pi,angle)
c       write (*,*) 11,x4p1,y4pi,angle
       do k=1,11
       xglo4(k)=xglo(12-k)
       yglo4(k)=yglo(12-k)
c       write (*,*) "glo4",k,xglo4(k),yglo4(k)
       end do
       xglo4(0)=xglo4(1)+(xglo4(11)-xglo4(10))
       yglo4(0)=yglo4(1)+(yglo4(11)-yglo4(10))
       np4=11

c      Intrados segment
       xjonc(i,j2i,1)=x2pi
       xjonc(i,j2i,2)=y2pi
       do j=j2i+1,j2f
       xjonc(i,j,1)=u(i,j,3)
       xjonc(i,j,2)=v(i,j,3)
       end do
       xjonc(i,j2f,1)=x2pf
       xjonc(i,j2f,2)=y2pf
       np2=j2f-j2i+1

c      Add jonc deflections
       k1=(xextra(ng,3)*rib(i,5)/100.0d0)/((x1f-x1i)**xextra(ng,4))
       k2=(xintra(ng,3)*rib(i,5)/100.0d0)/((x2f-x2i)**xintra(ng,4))

       do j=j2i+2,j2f+3
       xjonc(i,j,2)=xjonc(i,j,2)+k2*((xjonc(i,j,1))-x2i)**xintra(ng,4)
       end do

c      SECOND ROD (2) 4+nose42+2mod
       npo=np4+np42+np2-2
c       write (*,*) npo,np4,np42,np2

c       write (*,*) i,npo,np4,np42,np2
c      Reformat jonc
       do j=0,np4
       xjonc(i,j,1)=xglo4(j)
       xjonc(i,j,2)=yglo4(j)
c       write (*,*) i,j,xjonc(i,j,1),xjonc(i,j,2)
       end do

       do j=np4+1,npo
       xjonc(i,j,3)=xjonc(i,j+j4i-np4,1)
       xjonc(i,j,4)=xjonc(i,j+j4i-np4,2)
       end do

c      Reformat jonc again (change name)
       do j=np4+1,npo
       xjonc(i,j,1)=xjonc(i,j,3)
       xjonc(i,j,2)=xjonc(i,j,4)
c       write (*,*) i,j,xjonc(i,j,1),xjonc(i,j,2)
       end do

c      Restitution of virtual point 3
       if (atp.eq."ss".and.xintra(ng,2).gt.rib(i,12)) then 
       j=np(i,2)+np(i,3)-1
       u(i,j+1,3)=x4
       v(i,j+1,3)=y4
       end if
  
       end if ! ng.ne.0

       return
       end



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE JONCS2
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       SUBROUTINE joncs2(i,u,v,rib,x21,xjonc,nparc,atp,np,m,ng)

       real*8 u(0:100,500,99),v(0:100,500,99)
       real*8 x21(20,100,20)
       real*8 xjonc(0:100,0:500,10),k1,k2
       real*8 x1i,x1f,x2i,x2f
       real*8 x1pi,x1pf,x2pi,x2pf
       real*8 x0,y0,x1,x2,x3,x4,y1,y2,y3,y4
       real*8 b,c,d,f,g,xp231,xp221,xp012,xp013
       real*8 dx,dy,alpha,radi,pi,xsign,xsign2
       real*8 rib(0:100,500)
       integer np(0:100,9)
       character*2 atp

       dx=(x21(m,ng,3)-x21(m,ng,1))/dfloat(nparc-1)
       dy=(x21(m,ng,4)-x21(m,ng,2))/dfloat(nparc-1)
       pi=4.0d0*datan(1.0d0)
       
c      Define straight rod
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (dabs(x21(m,ng,5)).lt.0.01) then
       do j=1,nparc
       xjonc(i,j,1)=(x21(m,ng,1)+dx*dfloat(j-1))*rib(i,5)/100.0d0
       xjonc(i,j,2)=(x21(m,ng,2)+dy*dfloat(j-1))*rib(i,5)/100.0d0
       end do
       rib(i,167)=(rib(i,5)/100.0)*dsqrt((x21(m,ng,3)-x21(m,ng,1))**2.+
     + (x21(m,ng,4)-x21(m,ng,2))**2.)
       end if

c      Define an arc jonc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (dabs(x21(m,ng,5)).ge.0.01) then

c      Control signs
       if (x21(m,ng,5).gt.0.01) then
       xsign=1.0d0
       end if
       if (x21(m,ng,5).le.-0.01) then
       xsign=-1.0d0
       end if
       if (x21(m,ng,2).gt.x21(m,ng,4)) then
       xsign2=1.0d0
       end if
       if (x21(m,ng,2).lt.x21(m,ng,4)) then
       xsign2=-1.0d0
       end if
       if (x21(m,ng,2).gt.x21(m,ng,4).and.x21(m,ng,3).gt.x21(m,ng,1)) 
     + then
       xsign3=-1.0d0
       end if
       if (x21(m,ng,2).gt.x21(m,ng,4).and.x21(m,ng,3).lt.x21(m,ng,1))
     + then
       xsign3=1.0d0
       end if

c      Segment 1-4-2
       x1=x21(m,ng,1)
       y1=x21(m,ng,2)
       x2=x21(m,ng,3)
       y2=x21(m,ng,4)
       x4=(x1+x2)*0.5
       y4=(y1+y2)*0.5

c      Point 3 
c       x3=x4-dabs(x21(m,ng,5))*dsin(alpha)
c       y3=y4+dabs(x21(m,ng,5))*dcos(alpha)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case segmentation in y axis
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (dabs(y2-y1).gt.dabs(x2-x1)) then

       alpha=datan((y2-y1)/(x2-x1))

       if (x1.eq.x2.and.y2.gt.y1) then
       alpha=-pi/2.0d0
       end if

       if (x1.eq.x2.and.y2.lt.y1) then
       alpha=pi/2.0d0
       end if

c      Circle by tree points, geometric method
       d=0.5d0*dsqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
       f=dabs(x21(m,ng,5))
       radi=(f*f+d*d)/(2.0d0*f)
       g=radi-f
       x0=x4+xsign*g*dsin(alpha)
       y0=y4-xsign*g*dcos(alpha)

       do j=1,nparc
       xjonc(i,j,2)=(x21(m,ng,2)+dy*dfloat(j-1))
       b=-2.*x0
       c=x0*x0+xjonc(i,j,2)*xjonc(i,j,2)-2*xjonc(i,j,2)*y0+y0*y0-
     + radi*radi
       xjonc(i,j,1)=(-b-xsign*xsign2*xsign3*dsqrt(b*b-4.*c))/2.0d0
       end do

       end if ! case y-axis

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case segmentation in x axis
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (dabs(y2-y1).le.dabs(x2-x1)) then

       alpha=datan((y2-y1)/(x2-x1))

c      Circle by tree points, geometric method
       d=0.5d0*dsqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
       f=dabs(x21(m,ng,5))
       radi=(f*f+d*d)/(2.0d0*f)
       g=radi-f
       x0=x4+xsign*g*dsin(alpha)
       y0=y4-xsign*g*dcos(alpha)

       do j=1,nparc
       xjonc(i,j,1)=(x21(m,ng,1)+dx*dfloat(j-1))
       b=-2.*y0
       c=x0*x0+xjonc(i,j,1)*xjonc(i,j,1)-2*xjonc(i,j,1)*x0+y0*y0-
     + radi*radi
       xjonc(i,j,2)=(-b+xsign*dsqrt(b*b-4.*c))/2.0d0
       end do

       end if ! case x-axis

c      Set to scale
       do j=1,nparc
       xjonc(i,j,1)=xjonc(i,j,1)*rib(i,5)/100.0d0
       xjonc(i,j,2)=xjonc(i,j,2)*rib(i,5)/100.0d0
       end do

       end if ! case circle

       return

       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE LINEAL INTERPOLATION
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       SUBROUTINE interpola(x1,y1,x2,y2,x3,y3)

       real*8 x1,y1,x2,y2,x3,y3,xm,xb

       xm=(y2-y1)/(x2-x1)
       xb=y2-xm*x2
       y3=xm*x3+xb

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE LINEAL INTERPOLATION AT DISTANCE D
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       SUBROUTINE interpd(x1,y1,x2,y2,x3,y3,xd)

       real*8 x1,y1,x2,y2,x3,y3
       real*8 xd,xdx,xdy,xd13,yd13,xdi

       xdx=x2-x1
       xdy=y2-y1
       xdi=dsqrt((x2-x1)**2.0d0+(y2-y1)**2.0d0)

       if (xdi.ne.0.0d0) then
       xd13=xdx*xd/xdi
       yd13=xdy*xd/xdi
       x3=x1+xd13
       y3=y1+yd13
       else
       x3=x1
       y3=y1
       end if

c       write (*,*) i,xd,xdi,x3,y3

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE PRINT JONCS
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       SUBROUTINE pjoncs(i,xjonc,npo,sjo,sepx,sepy,rib,xkf)

       real*8 xjonc(0:100,0:500,10),sjo(0:100,10),sepx,sepy
       real*8 rib(0:100,500),joini(0:100,10),jofin(0:100,10)
       real*8 uo(0:500,2),vo(0:500,2)
       real*8 xo,x1,x2,x3,x4,y1,y2,y3,y4,xkf,xad4,xad7

       real*8 xcir,alpha

       xcir=0.1d0*typm2(1)
       xad4=2530.0d0*xkf
       xad7=1260.0d0*5.0d0*xkf

       ng=rib(i,166)

c      Jonc length
       rib(i,167)=0.0d0
       do j=1,npo-1
       rib(i,167)=rib(i,167)+sqrt(((xjonc(i,j+1,1)-xjonc(i,j,1))**2)+
     + ((xjonc(i,j+1,2)-xjonc(i,j,2))**2))
       end do
       
c      Load jonc base line
       do j=0,npo+1
       uo(j,1)=xjonc(i,j,1)
       vo(j,1)=xjonc(i,j,2)
       end do

c      Print line 1 (jonc)
       xo=-sjo(ng,1)
       call loffset2(uo,vo,npo,xo)
       do j=1,npo-1
       call line(sepx+uo(j,2),sepy-vo(j,2),
     + sepx+uo(j+1,2),sepy-vo(j+1,2),150)
       end do

c      Print init and final point
       call point(sepx+uo(1,2),sepy-vo(1,2),160)
       call point(sepx+uo(j,2),sepy-vo(j,2),160)
       call point(2530.*xkf+sepx+uo(1,2),sepy-vo(1,2),160)
       call point(2530.*xkf+sepx+uo(j,2),sepy-vo(j,2),160)


       x1=uo(1,2)
       y1=vo(1,2)
       x4=uo(npo,2)
       y4=vo(npo,2)

c      Print line 2 (jonc)
       xo=-(sjo(ng,1)+sjo(ng,3))
       call loffset2(uo,vo,npo,xo)
       do j=1,npo-1
       call line(sepx+uo(j,2),sepy-vo(j,2),
     + sepx+uo(j+1,2),sepy-vo(j+1,2),150)
       end do

c      Print init and final point
       call point(sepx+uo(1,2),sepy-vo(1,2),160)
       call point(sepx+uo(j,2),sepy-vo(j,2),160)
       call point(2530.*xkf+sepx+uo(1,2),sepy-vo(1,2),160)
       call point(2530.*xkf+sepx+uo(j,2),sepy-vo(j,2),160)

       x2=uo(1,2)
       y2=vo(1,2)
       x3=uo(npo,2)
       y3=vo(npo,2)

c      Joncs points ini and fin
       joini(i,1)=0.5d0*(x1+x2)
       joini(i,2)=0.5d0*(y1+y2)
       jofin(i,1)=0.5d0*(x3+x4)
       jofin(i,2)=0.5d0*(y3+y4)

c      Print line 3
       xo=sjo(ng,2)-sjo(ng,1)
       call loffset2(uo,vo,npo,xo)
       do j=1,npo-1
       call line(sepx+uo(j,2),sepy-vo(j,2),      ! BOX(1,2)
     + sepx+uo(j+1,2),sepy-vo(j+1,2),30)
       call line(sepx+uo(j,2)+xad7,sepy-vo(j,2), ! BOX(1,7)
     + sepx+uo(j+1,2)+xad7,sepy-vo(j+1,2),30)
       end do

       x1=uo(1,2)
       y1=vo(1,2)
       x4=uo(npo,2)
       y4=vo(npo,2)

c      Print line 4
       xo=-(sjo(ng,1)+sjo(ng,3)+sjo(ng,4))
       call loffset2(uo,vo,npo,xo)
       do j=1,npo-1
       call line(sepx+uo(j,2),sepy-vo(j,2),      ! BOX(1,2)
     + sepx+uo(j+1,2),sepy-vo(j+1,2),30)
       call line(sepx+uo(j,2)+xad7,sepy-vo(j,2), ! BOX(1,7)
     + sepx+uo(j+1,2)+xad7,sepy-vo(j+1,2),30)

       end do

       x2=uo(1,2)
       y2=vo(1,2)
       x3=uo(npo,2)
       y3=vo(npo,2)

c      BOX(1,2)
       call line(sepx+x1,sepy-y1,sepx+x2,sepy-y2,30)
       call line(sepx+x3,sepy-y3,sepx+x4,sepy-y4,30)

c      Print points jonc ini and fin BOX(1,2)
c       call pointg(sepx+joini(i,1),sepy-joini(i,2),xcir,150)
c       call pointg(sepx+jofin(i,1),sepy-jofin(i,2),xcir,150)

c      Laser cuting BOX(1,4)
c       call point(sepx+joini(i,1)+xad4,sepy-joini(i,2),150)
c       call point(sepx+jofin(i,1)+xad4,sepy-jofin(i,2),150)

c      BOX(1,7)
       call line(sepx+x1+xad7,sepy-y1,sepx+x2+xad7,sepy-y2,30)
       call line(sepx+x3+xad7,sepy-y3,sepx+x4+xad7,sepy-y4,30)

c      Romano number in jonc BOX(1,7)
       if (typm5(9).gt.0.01d0) then
       ijo=dint(dfloat(npo)*0.25d0)
       alpha=datan((xjonc(i,ijo,2)-xjonc(i,ijo-1,2))/
     + (xjonc(i,ijo,1)-xjonc(i,ijo-1,1)))
       call romano(i,sepx+xad7+xjonc(i,ijo,1),sepy-xjonc(i,ijo,2),
     + alpha,typm5(9)*0.1,30)
       end if

       return
       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE PRINT JONCS B12
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       SUBROUTINE pjoncs_b12(i,xjonc,npo,sjo,sepx,sepy,rib,xkf)

       real*8 xjonc(0:100,0:500,10),sjo(0:100,10),sepx,sepy
       real*8 rib(0:100,500),joini(0:100,10),jofin(0:100,10)
       real*8 uo(0:500,2),vo(0:500,2)
       real*8 xo,x1,x2,x3,x4,y1,y2,y3,y4,xkf,xad4,xad7

       real*8 xcir,alpha

       xcir=0.1d0*typm2(1)
       xad4=2530.0d0*xkf
       xad7=1260.0d0*5.0d0*xkf

       ng=rib(i,166)

c      Jonc length
       rib(i,167)=0.0d0
       do j=1,npo-1
       rib(i,167)=rib(i,167)+sqrt(((xjonc(i,j+1,1)-xjonc(i,j,1))**2)+
     + ((xjonc(i,j+1,2)-xjonc(i,j,2))**2))
       end do
       
c      Load jonc base line
       do j=0,npo+1
       uo(j,1)=xjonc(i,j,1)
       vo(j,1)=xjonc(i,j,2)
       end do

c      Print line 1 (jonc)
       xo=-sjo(ng,1)
       call loffset2(uo,vo,npo,xo)
       do j=1,npo-1
       call line(sepx+uo(j,2),sepy-vo(j,2),
     + sepx+uo(j+1,2),sepy-vo(j+1,2),150)
       end do

c      Print init and final point
       call point(sepx+uo(1,2),sepy-vo(1,2),160)
       call point(sepx+uo(j,2),sepy-vo(j,2),160)
       call point(2530.*xkf+sepx+uo(1,2),sepy-vo(1,2),160)
       call point(2530.*xkf+sepx+uo(j,2),sepy-vo(j,2),160)

       x1=uo(1,2)
       y1=vo(1,2)
       x4=uo(npo,2)
       y4=vo(npo,2)

c      Print line 2 (jonc)
       xo=-(sjo(ng,1)+sjo(ng,3))
       call loffset2(uo,vo,npo,xo)
       do j=1,npo-1
       call line(sepx+uo(j,2),sepy-vo(j,2),
     + sepx+uo(j+1,2),sepy-vo(j+1,2),150)
       end do

c      Print init and final point
       call point(sepx+uo(1,2),sepy-vo(1,2),160)
       call point(sepx+uo(j,2),sepy-vo(j,2),160)
       call point(2530.*xkf+sepx+uo(1,2),sepy-vo(1,2),160)
       call point(2530.*xkf+sepx+uo(j,2),sepy-vo(j,2),160)

       x2=uo(1,2)
       y2=vo(1,2)
       x3=uo(npo,2)
       y3=vo(npo,2)

c      Joncs points ini and fin
       joini(i,1)=0.5d0*(x1+x2)
       joini(i,2)=0.5d0*(y1+y2)
       jofin(i,1)=0.5d0*(x3+x4)
       jofin(i,2)=0.5d0*(y3+y4)

c      Print line 3
       xo=sjo(ng,2)-sjo(ng,1)
       call loffset2(uo,vo,npo,xo)
       do j=1,npo-1
       call line(sepx+uo(j,2),sepy-vo(j,2),      ! BOX(1,2)
     + sepx+uo(j+1,2),sepy-vo(j+1,2),30)
       end do

       x1=uo(1,2)
       y1=vo(1,2)
       x4=uo(npo,2)
       y4=vo(npo,2)

c      Print line 4
       xo=-(sjo(ng,1)+sjo(ng,3)+sjo(ng,4))
       call loffset2(uo,vo,npo,xo)
       do j=1,npo-1
       call line(sepx+uo(j,2),sepy-vo(j,2),      ! BOX(1,2)
     + sepx+uo(j+1,2),sepy-vo(j+1,2),30)
       end do

       x2=uo(1,2)
       y2=vo(1,2)
       x3=uo(npo,2)
       y3=vo(npo,2)

c      BOX(1,2)
       call line(sepx+x1,sepy-y1,sepx+x2,sepy-y2,30)
       call line(sepx+x3,sepy-y3,sepx+x4,sepy-y4,30)

c      Laser cuting BOX(1,4)
c       call point(sepx+joini(i,1)+xad4,sepy-joini(i,2),150)
c       call point(sepx+jofin(i,1)+xad4,sepy-jofin(i,2),150)

       return
       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE PRINT JONCS B17
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       SUBROUTINE pjoncs_b17(i,xjonc,npo,sjo,sepx,sepy,rib,xkf)

       real*8 xjonc(0:100,0:500,10),sjo(0:100,10),sepx,sepy
       real*8 rib(0:100,500),joini(0:100,10),jofin(0:100,10)
       real*8 uo(0:500,2),vo(0:500,2)
       real*8 xo,x1,x2,x3,x4,y1,y2,y3,y4,xkf,xad4,xad7

       real*8 xcir,alpha

       xcir=0.1d0*typm2(1)
       xad4=2530.0d0*xkf
       xad7=1260.0d0*5.0d0*xkf

       ng=rib(i,166)

c      Jonc length
       rib(i,167)=0.0d0
       do j=1,npo-1
       rib(i,167)=rib(i,167)+sqrt(((xjonc(i,j+1,1)-xjonc(i,j,1))**2)+
     + ((xjonc(i,j+1,2)-xjonc(i,j,2))**2))
       end do
       
c      Load jonc base line
       do j=0,npo+1
       uo(j,1)=xjonc(i,j,1)
       vo(j,1)=xjonc(i,j,2)
       end do

c      Print line 1 (jonc)
       xo=-sjo(ng,1)
       call loffset2(uo,vo,npo,xo)

c      Print init and final point
c       call point(2530.*xkf+sepx+uo(1,2),sepy-vo(1,2),160)
c       call point(2530.*xkf+sepx+uo(j,2),sepy-vo(j,2),160)

       x1=uo(1,2)
       y1=vo(1,2)
       x4=uo(npo,2)
       y4=vo(npo,2)

c      Print line 2 (jonc)
       xo=-(sjo(ng,1)+sjo(ng,3))
       call loffset2(uo,vo,npo,xo)

c      Print init and final point
c       call point(2530.*xkf+sepx+uo(1,2),sepy-vo(1,2),160)
c       call point(2530.*xkf+sepx+uo(j,2),sepy-vo(j,2),160)

       x2=uo(1,2)
       y2=vo(1,2)
       x3=uo(npo,2)
       y3=vo(npo,2)

c      Joncs points ini and fin
       joini(i,1)=0.5d0*(x1+x2)
       joini(i,2)=0.5d0*(y1+y2)
       jofin(i,1)=0.5d0*(x3+x4)
       jofin(i,2)=0.5d0*(y3+y4)

c      Print line 3
       xo=sjo(ng,2)-sjo(ng,1)
       call loffset2(uo,vo,npo,xo)
       do j=1,npo-1
       call line(sepx+uo(j,2)+xad7,sepy-vo(j,2), ! BOX(1,7)
     + sepx+uo(j+1,2)+xad7,sepy-vo(j+1,2),30)
       end do

       x1=uo(1,2)
       y1=vo(1,2)
       x4=uo(npo,2)
       y4=vo(npo,2)

c      Print line 4
       xo=-(sjo(ng,1)+sjo(ng,3)+sjo(ng,4))
       call loffset2(uo,vo,npo,xo)
       do j=1,npo-1
       call line(sepx+uo(j,2)+xad7,sepy-vo(j,2), ! BOX(1,7)
     + sepx+uo(j+1,2)+xad7,sepy-vo(j+1,2),30)

       end do

       x2=uo(1,2)
       y2=vo(1,2)
       x3=uo(npo,2)
       y3=vo(npo,2)

c      BOX(1,7)
       call line(sepx+x1+xad7,sepy-y1,sepx+x2+xad7,sepy-y2,30)
       call line(sepx+x3+xad7,sepy-y3,sepx+x4+xad7,sepy-y4,30)

c      Romano number in jonc BOX(1,7)
       if (typm5(9).gt.0.01d0) then
       ijo=dint(dfloat(npo)*0.25d0)
       alpha=datan((xjonc(i,ijo,2)-xjonc(i,ijo-1,2))/
     + (xjonc(i,ijo,1)-xjonc(i,ijo-1,1)))
       call romano(i,sepx+xad7+xjonc(i,ijo,1),sepy-xjonc(i,ijo,2),
     + alpha,typm5(9)*0.1,30)
       end if

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE PRINT JONCS2
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       SUBROUTINE pjoncs2(i,xjonc,nparc,sjo,sepx,sepy,rib,xkf)

       real*8 xjonc(0:100,0:500,10),sjo(0:100,10),sepx,sepy
       real*8 rib(0:100,500),joini(0:100,10),jofin(0:100,10)
       real*8 uo(0:500,2),vo(0:500,2)
       real*8 xo,x1,x2,x3,x4,y1,y2,y3,y4,xkf,xad4,xad7,alpha

       real*8 xcir

       xcir=0.1d0*typm2(1)
       xad4=2530.0d0*xkf
       xad7=1260.0d0*5.0d0*xkf

       ng=rib(i,166)
       npo=nparc

c      Jonc length
       rib(i,167)=0.0d0
       do j=1,npo-1
       rib(i,167)=rib(i,167)+sqrt(((xjonc(i,j+1,1)-xjonc(i,j,1))**2)+
     + ((xjonc(i,j+1,2)-xjonc(i,j,2))**2))
       end do
       
c      Load jonc base line
       do j=1,npo+1
       uo(j,1)=xjonc(i,j,1)
       vo(j,1)=xjonc(i,j,2)
       end do
c      Print line 1
       xo=-sjo(ng,3)/2.0d0
       call loffset2(uo,vo,npo,xo)
       do j=1,npo-1
       call line(sepx+uo(j,2),sepy-vo(j,2),
     + sepx+uo(j+1,2),sepy-vo(j+1,2),150)
       end do

c      Print init and final point
       call point(sepx+uo(1,2),sepy-vo(1,2),160)
       call point(sepx+uo(j,2),sepy-vo(j,2),160)
       call point(2530.*xkf+sepx+uo(1,2),sepy-vo(1,2),160)
       call point(2530.*xkf+sepx+uo(j,2),sepy-vo(j,2),160)

c      Load jonc base line
       do j=1,npo+1
       uo(j,1)=xjonc(i,j,1)
       vo(j,1)=xjonc(i,j,2)
       end do
c      Print line 2
       xo=sjo(ng,3)/2.0d0
       call loffset2(uo,vo,npo,xo)
       do j=1,npo-1
       call line(sepx+uo(j,2),sepy-vo(j,2),   ! BOX(1,2)
     + sepx+uo(j+1,2),sepy-vo(j+1,2),150)
       end do

c      Print init and final point
       call point(sepx+uo(1,2),sepy-vo(1,2),160)
       call point(sepx+uo(j,2),sepy-vo(j,2),160)
       call point(2530.*xkf+sepx+uo(1,2),sepy-vo(1,2),160)
       call point(2530.*xkf+sepx+uo(j,2),sepy-vo(j,2),160)

c      Load jonc base line
       do j=1,npo+1
       uo(j,1)=xjonc(i,j,1)
       vo(j,1)=xjonc(i,j,2)
       end do
c      Print line 3
       xo=-sjo(ng,3)/2.0d0-sjo(ng,2)
       call loffset2(uo,vo,npo,xo)
       do j=1,npo-1
       call line(sepx+uo(j,2),sepy-vo(j,2),      ! BOX(1,2)
     + sepx+uo(j+1,2),sepy-vo(j+1,2),30)
       call line(sepx+uo(j,2)+xad7,sepy-vo(j,2), ! BOX(1,7)
     + sepx+uo(j+1,2)+xad7,sepy-vo(j+1,2),10)
       end do

       x1=uo(1,2)
       y1=vo(1,2)
       x4=uo(npo,2)
       y4=vo(npo,2)

c      Load jonc base line
       do j=1,npo+1
       uo(j,1)=xjonc(i,j,1)
       vo(j,1)=xjonc(i,j,2)
       end do
c      Print line 4
       xo=sjo(ng,3)/2.0d0+sjo(ng,4)
       call loffset2(uo,vo,npo,xo)
       do j=1,npo-1
       call line(sepx+uo(j,2),sepy-vo(j,2),      ! BOX(1,2)
     + sepx+uo(j+1,2),sepy-vo(j+1,2),30)
       call line(sepx+uo(j,2)+xad7,sepy-vo(j,2), ! BOX(1,7)
     + sepx+uo(j+1,2)+xad7,sepy-vo(j+1,2),10)
       end do

       x2=uo(1,2)
       y2=vo(1,2)
       x3=uo(npo,2)
       y3=vo(npo,2)

c      Print end and final segments
       call line(sepx+x1,sepy-y1,      ! BOX(1,2)
     + sepx+x2,sepy-y2,30)
       call line(sepx+x1+xad7,sepy-y1, ! BOX(1,7)
     + sepx+x2+xad7,sepy-y2,10)
       call line(sepx+x3,sepy-y3,      ! BOX(1,2)
     + sepx+x4,sepy-y4,30)
       call line(sepx+x3+xad7,sepy-y3, ! BOX(1,7)
     + sepx+x4+xad7,sepy-y4,10)

c      Joncs points ini and fin
       joini(i,1)=0.5d0*(x1+x2)
       joini(i,2)=0.5d0*(y1+y2)
       jofin(i,1)=0.5d0*(x3+x4)
       jofin(i,2)=0.5d0*(y3+y4)

c      Print points jonc ini and fin BOX(1,2)
c       call pointg(sepx+joini(i,1),sepy-joini(i,2),xcir,150)
c       call pointg(sepx+jofin(i,1),sepy-jofin(i,2),xcir,150)

c      Laser cuting BOX(1,4)
c       call point(sepx+joini(i,1)+xad4,sepy-joini(i,2),150)
c       call point(sepx+jofin(i,1)+xad4,sepy-jofin(i,2),150)

c      Print romano points BOX(1,7) -exp-
c       call romanop(i,1,sepx+uo(1,1)+xad7,sepy-vo(1,1), ! BOX(1,7)
c     + sepx+uo(npo,1)+xad7,sepy-vo(npo,1),0,0,0,xkf)


c      Romano number in jonc BOX(1,7)
       if (typm5(9).gt.0.01d0) then
       ijo=dint(dfloat(npo)*0.25d0)
       alpha=datan((xjonc(i,ijo,2)-xjonc(i,ijo-1,2))/
     + (xjonc(i,ijo,1)-xjonc(i,ijo-1,1)))
       call romano(i,sepx+xad7+xjonc(i,ijo,1),sepy-xjonc(i,ijo,2),
     + alpha,typm5(9)*0.1,10)
       end if

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE LINE OFFSET
c      WARNING!!! Obsolete???? Use OFFSET2!!!!
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

