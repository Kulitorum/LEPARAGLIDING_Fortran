!> Draw the first cross-panel edge when generating printed patterns.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
!! @param[in] u Developed-panel horizontal coordinates.
!! @param[in] v Developed-panel vertical coordinates.
!! @param[in] icase Output mode; the segment is drawn when `icase` is 1.
       SUBROUTINE print01(i,psep,psey,u,v,icase)

       real*8 u(0:100,500,99),v(0:100,500,99),w(0:100,500,99)
       real*8 psep,psey
       integer j,icase,np(0:100,9)

c      Incorpore subroutine extpoints
      
c      Segments

       j=1

c       write (*,*) i

       if (icase.eq.1) then ! case print
       call line(psep+u(i,j,9),psey-v(i,j,9),
     + psep+u(i,j,10),psey-v(i,j,10),1)
       end if

       return

       end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Print initial i final points
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Draw registration marks at both ends of both panel sides.
!! Marks are emitted for the print layout and for the horizontally displaced
!! laser layout.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in] npi First profile-point index to mark.
!! @param[in] npf Last profile-point index to mark.
!! @param[in] u Developed-panel horizontal coordinates.
!! @param[in] v Developed-panel vertical coordinates.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
!! @param[in] xcir Printed registration-circle radius.
!! @param[in] xdes Normal displacement from the seam edge.
!! @param[in] xkf Global drawing scale used to locate the laser copy.
       SUBROUTINE prinifp(i,npi,npf,u,v,psep,psey,xcir,xdes,xkf)
       
       real*8 u(0:100,500,99),v(0:100,500,99),psep,psey,xcir
       real*8 xu,xv,xdes,xkf
       real*8 alp
       integer i,j,npi,npf

       j=npi
       xu=u(i,j,9)
       xv=v(i,j,9)
       alp=abs(datan((v(i,j+1,9)-v(i,j,9))/(u(i,j+1,9)-u(i,j,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)
       call pointg(psep+xu,psey-xv,xcir,4)     
       call point(psep+xu+2520.*xkf,-xv+psey,7)

       j=npf
       xu=u(i,j,9)
       xv=v(i,j,9)
       alp=abs(datan((v(i,j-1,9)-v(i,j,9))/(u(i,j-1,9)-u(i,j,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)
       call pointg(psep+xu,psey-xv,xcir,4)     
       call point(psep+xu+2520.*xkf,-xv+psey,7)

       j=npi
       xu=u(i,j,10)
       xv=v(i,j,10)
       alp=abs(datan((v(i,j+1,10)-v(i,j,10))/(u(i,j+1,10)-u(i,j,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)
       call pointg(psep+xu,psey-xv,xcir,4)
       call point(psep+xu+2520.*xkf,-xv+psey,7)

       j=npf
       xu=u(i,j,10)
       xv=v(i,j,10)
       alp=abs(datan((v(i,j-1,10)-v(i,j,10))/(u(i,j-1,10)-u(i,j,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)
       call pointg(psep+xu,psey-xv,xcir,4)
       call point(psep+xu+2520.*xkf,-xv+psey,7)

       return

       end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE prinfpv - Print final points vents
c      Case print, case laser (ic1)
c      Case type vent (ic2)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Draw final-point registration marks for the selected vent variant.
!! @param[in] i Panel or rib index.
!! @param[in] j9 Profile index on the left side (coordinate slot 9).
!! @param[in] j10 Profile index on the right side (coordinate slot 10).
!! @param[in] ufr Developed vent-panel horizontal coordinates.
!! @param[in] vfr Developed vent-panel vertical coordinates.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
!! @param[in] xcir Printed registration-circle radius.
!! @param[in] xdes Normal displacement from the seam edge.
!! @param[in] xkf Global drawing scale used to locate the laser copy.
!! @param[in] ic1 Output process selector: 1 print; 2 laser.
!! @param[in] ic2 Legacy vent construction variant.
       SUBROUTINE prinfpv(i,j9,j10,ufr,vfr,psep,psey,xcir,xdes,xkf,
     + ic1,ic2)
       
       real*8 ufr(0:100,500,50),vfr(0:100,500,50),psep,psey,xcir
       real*8 xu,xv,xdes,xkf
       real*8 alp
       integer i,ic1,ic2,j9,j10

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Print final points vents, case print, vent +1 +6
c      ic1=1 ic2=1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (ic1.eq.1) then
       if (ic2.eq.1.or.ic2.eq.6) then
c      Point left
       j=j9
       xu=ufr(i,j,9)
       xv=vfr(i,j,9)
       alp=abs(datan((vfr(i,j,9)-vfr(i,j-1,9))/
     + (ufr(i,j,9)-ufr(i,j-1,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)
       call pointg(psep+xu,psey-xv,xcir,4)     
c      Point right
       j=j10
       xu=ufr(i,j,10)
       xv=vfr(i,j,10)
       alp=abs(datan((vfr(i,j,10)-vfr(i,j-1,10))/
     + (ufr(i,j,10)-ufr(i,j-1,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)
       call pointg(psep+xu,psey-xv,xcir,4)
       end if
       end if
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Print final points vents, case laser, vent +1 +6
c      ic1=2 ic2=1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (ic1.eq.2) then
       if (ic2.eq.1.or.ic2.eq.6) then
c      Point left
       j=j9
       xu=ufr(i,j,9)
       xv=vfr(i,j,9)
       alp=abs(datan((vfr(i,j,9)-vfr(i,j-1,9))/
     + (ufr(i,j,9)-ufr(i,j-1,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)   
       call point(psep+xu,-xv+psey,7)
c      Point right
       j=j10
       xu=ufr(i,j,10)
       xv=vfr(i,j,10)
       alp=abs(datan((vfr(i,j,10)-vfr(i,j-1,10))/
     + (ufr(i,j,10)-ufr(i,j-1,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)
       call point(psep+xu,-xv+psey,7)
       end if
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Print final points vents, case print, vent -1 -6
c      ic1=1 ic2=-1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (ic1.eq.1) then
       if (ic2.eq.-1.or.ic2.eq.-6) then
c      Point left
       j=j9
       xu=ufr(i,j,9)
       xv=vfr(i,j,9)
       alp=abs(datan((vfr(i,j+1,9)-vfr(i,j,9))/
     + (ufr(i,j+1,9)-ufr(i,j,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)
       call pointg(psep+xu,psey-xv,xcir,4)     
c      Point right
       j=j10
       xu=ufr(i,j,10)
       xv=vfr(i,j,10)
       alp=abs(datan((vfr(i,j+1,10)-vfr(i,j,10))/
     + (ufr(i,j+1,10)-ufr(i,j,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)
       call pointg(psep+xu,psey-xv,xcir,4)
       end if
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Print final points vents, case laser, vent -1 -6
c      ic1=2 ic2=-1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (ic1.eq.2) then
       if (ic2.eq.-1.or.ic2.eq.-6) then
c      Point left
       j=j9
       xu=ufr(i,j,9)
       xv=vfr(i,j,9)
       alp=abs(datan((vfr(i,j+1,9)-vfr(i,j,9))/
     + (ufr(i,j+1,9)-ufr(i,j,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)   
       call point(psep+xu,-xv+psey,7)
c      Point right
       j=j10
       xu=ufr(i,j,10)
       xv=vfr(i,j,10)
       alp=abs(datan((vfr(i,j+1,10)-vfr(i,j,10))/
     + (ufr(i,j+1,10)-ufr(i,j,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)
       call point(psep+xu,-xv+psey,7)
       end if
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Print final points vents, case print, vent -2
c      ic1=1 ic2=-2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (ic1.eq.1.and.ic2.eq.-2) then

c      Point left
       j=j9
       xu=ufr(i,j,9)
       xv=vfr(i,j,9)
       alp=abs(datan((vfr(i,j+1,9)-vfr(i,j,9))/
     + (ufr(i,j+1,9)-ufr(i,j,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)
c       call pointg(psep+xu,psey-xv,xcir,4)     
c      Point right
       j=j10
       xu=ufr(i,j,10)
       xv=vfr(i,j,10)
       alp=abs(datan((vfr(i,j+1,10)-vfr(i,j,10))/
     + (ufr(i,j+1,10)-ufr(i,j,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)
       call pointg(psep+xu,psey-xv,xcir,4)
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Print final points vents, case laser, vent -2
c      ic1=2 ic2=-2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (ic1.eq.2.and.ic2.eq.-2) then
c      Point left
       j=j9
       xu=ufr(i,j,9)
       xv=vfr(i,j,9)
       alp=abs(datan((vfr(i,j+1,9)-vfr(i,j,9))/
     + (ufr(i,j+1,9)-ufr(i,j,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)   
c       call point(psep+xu,-xv+psey,7)
c      Point right
       j=j10
       xu=ufr(i,j,10)
       xv=vfr(i,j,10)
       alp=abs(datan((vfr(i,j+1,10)-vfr(i,j,10))/
     + (ufr(i,j+1,10)-ufr(i,j,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)
       call point(psep+xu,-xv+psey,7)
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Print final points vents, case print, vent -3
c      ic1=1 ic2=-3
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (ic1.eq.1.and.ic2.eq.-3) then

c      Point left
       j=j9
       xu=ufr(i,j,9)
       xv=vfr(i,j,9)
       alp=abs(datan((vfr(i,j+1,9)-vfr(i,j,9))/
     + (ufr(i,j+1,9)-ufr(i,j,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)
       call pointg(psep+xu,psey-xv,xcir,4)     
c      Point right
       j=j10
       xu=ufr(i,j,10)
       xv=vfr(i,j,10)
       alp=abs(datan((vfr(i,j+1,10)-vfr(i,j,10))/
     + (ufr(i,j+1,10)-ufr(i,j,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)
c       call pointg(psep+xu,psey-xv,xcir,4)
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Print final points vents, case laser, vent -3
c      ic1=2 ic2=-3
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (ic1.eq.2.and.ic2.eq.-3) then
c      Point left
       j=j9
       xu=ufr(i,j,9)
       xv=vfr(i,j,9)
       alp=abs(datan((vfr(i,j+1,9)-vfr(i,j,9))/
     + (ufr(i,j+1,9)-ufr(i,j,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)   
       call point(psep+xu,-xv+psey,7)
c      Point right
       j=j10
       xu=ufr(i,j,10)
       xv=vfr(i,j,10)
       alp=abs(datan((vfr(i,j+1,10)-vfr(i,j,10))/
     + (ufr(i,j+1,10)-ufr(i,j,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)
c       call point(psep+xu,-xv+psey,7)
       end if


       return

       end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE llarlr
c      Computes length at left and right of the partial rectagular
c      panel i formed by npo points
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Calculate the developed lengths of a panel's left and right sides.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in] i12 Surface/category index used in the result arrays.
!! @param[in] i22 Panel-variant index used in the result arrays.
!! @param[in] npo Number of profile points in the partial panel.
!! @param[in] ufr Developed-panel horizontal coordinates.
!! @param[in] vfr Developed-panel vertical coordinates.
!! @param[in,out] llarl Left-side lengths; the selected entry is overwritten.
!! @param[in,out] llarr Right-side lengths; the selected entry is overwritten.
       SUBROUTINE llarlr(i,i12,i22,npo,ufr,vfr,llarl,llarr)

       real*8 ufr(0:100,500,50),vfr(0:100,500,50)
       real*8 llarl(0:100,3,100),llarr(0:100,3,100)
       integer i,j,i12,i22,npo

c      Set panel side lengths 
       llarl(i,i12,i22)=0.0d0
       llarr(i,i12,i22)=0.0d0      
       do j=1,npo-1
       llarl(i,i12,i22)=llarl(i,i12,i22)+dsqrt((ufr(i,j+1,9)-
     + ufr(i,j,9))**2.+(vfr(i,j+1,9)-vfr(i,j,9))**2.)
       llarr(i,i12,i22)=llarr(i,i12,i22)+dsqrt((ufr(i,j+1,10)-
     + ufr(i,j,10))**2.+(vfr(i,j+1,10)-vfr(i,j,10))**2.)
       end do

c       write (*,*) i,i12,i22,npo,llarl(i,i12,i22),llarr(i,i12,i22)

       return

       end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE xmarksi
c      Makes internal marks in a rectangular panel
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Place regularly spaced internal registration marks on both panel sides.
!! Traversal direction follows the upper/lower-surface selector, and the final
!! residual spacings are returned for continuity with the next panel section.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in] i1 Surface selector: 1 extrados; 2 intrados.
!! @param[in] i2 Panel-section index used by the length/result arrays.
!! @param[in] npo Number of profile points in the panel.
!! @param[in] u Developed-panel horizontal coordinates.
!! @param[in] v Developed-panel vertical coordinates.
!! @param[in] rib Rib data table retained by the legacy interface.
!! @param[in] xinil Initial distance to the first mark on the left side.
!! @param[in] xinir Initial distance to the first mark on the right side.
!! @param[in,out] xfinl Residual left-side spacing for the following section.
!! @param[in,out] xfinr Residual right-side spacing for the following section.
!! @param[in] xmark Nominal interval between successive marks.
!! @param[in] llarl Previously calculated left-side lengths.
!! @param[in] llarr Previously calculated right-side lengths.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
!! @param[in] xcir Printed registration-circle radius.
!! @param[in] xdes Normal displacement from the seam edge.
!! @param[in] xkf Global drawing scale used to locate the laser copy.
       SUBROUTINE xmarksi(i,i1,i2,npo,u,v,rib,xinil,xinir,
     + xfinl,xfinr,xmark,llarl,llarr,psep,psey,xcir,xdes,xkf)

       real*8 u(0:100,500,50),v(0:100,500,50),rib(0:100,500)
       real*8 llarl(0:100,3,100),llarr(0:100,3,100)
       real*8 xmk,xmk0,xmark,xmklast,xprev,xpost,xacu(0:100)
       real*8 dist,dist1,dist2,alp,xu,xv,psep,psey,xcir,xdes,xkf
       real*8 xinil,xinir
       real*8 xfinl(0:100,3,100),xfinr(0:100,3,100)
       integer i,j,k,i1,i2,npo

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      STILL RECOMMENDED APPLY AMPLIFICATION COEFFICIENTS 
c      set in 11. rib(i,194) rib(i,195) extra
c      and        rib(i,200) rib(i,201) intra
c      Separate left and right calculus using proper coefficient
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      For extrados panels (i1=1)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc  

c      SEPARAR L i R !!!!!!!!!!!!!!!
            
       if (i1.eq.1) then
       
c      Set successive distance marks
c      WARNING!!!! Compute proper amplification coefficient!!!!!!!
c      Use amplification coefficient (panel border/rib)
c      Use different coeeficients for left and right
c      xmk=xmk*rib(i,36) ! amplificacio de segment

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Draw at left 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
c      Iterate in marks
       do k=1,60

       xacu(i)=0.0d0

c       write (*,*) "xinil ",i,xinil

c      Set successive distance marks
       if (k.eq.1) then
       xmk=xinil
       end if
       if (k.gt.1) then
       xmk=xmark*float(k-1)*1.0d0+xinil
       end if
      
       do j=1,npo-1
       xprev=xacu(i)
       xacu(i)=xacu(i)+sqrt((u(i,j,9)-u(i,j+1,9))**2.+((v(i,j,9)
     + -v(i,j+1,9))**2.))
       xpost=xacu(i)

c      Detect segment where is mark
       if(xmk.le.xpost.and.xmk.ge.xprev.and.xmk.le.llarl(i,i1,i2)) then

c      dibuixa marca

       dist=dsqrt((u(i,j,9)-u(i,j+1,9))**2.+((v(i,j,9)
     + -v(i,j+1,9))**2.))

       dist1=xmk-xprev

       xu=u(i,j,9)+(u(i,j+1,9)-u(i,j,9))*(dist1/dist)
       xv=v(i,j,9)+(v(i,j+1,9)-v(i,j,9))*(dist1/dist)

c      REVISSAR SIGNES DESPLAÇAMENT!!!!!!!
c      Despla a vores punts de control de costures
       alp=abs(datan((v(i,j+1,9)-v(i,j,9))/(u(i,j+1,9)-u(i,j,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)
       
       if (xinil.ge.0.01d0.or.k.gt.1) then
c      Point imp
       call pointg(psep+xu,psey-xv,xcir,3)
c      Point laser
       call point(psep+xu+2520.*xkf,-xv+psey,7)
       end if
       
       xmklast=xmk

       end if
       end do ! j left
      
       end do ! k

       xfinl(i,i1,i2)=xmark*1.0d0-(llarl(i,i1,i2)-xmklast)

c       write (*,*) "L ",i,i1,i2,xmklast,llarl(i,i1,i2),xfinl(i,i1,i2)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Draw at right
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       xacu(i)=xinir  

c      Iterate in marks
       do k=1,60

       xacu(i)=0.0d0

c      Set successive distance marks
       if (k.eq.1) then
       xmk=xinir
       end if
       if (k.gt.1) then
       xmk=xmark*float(k-1)*1.0d0+xinir
       end if
  
       do j=1,npo-1
       xprev=xacu(i)
       xacu(i)=xacu(i)+sqrt((u(i,j,10)-u(i,j+1,10))**2.+((v(i,j,10)
     + -v(i,j+1,10))**2.))
       xpost=xacu(i)

c      Detect segment where is mark
       if(xmk.le.xpost.and.xmk.ge.xprev.and.xmk.le.llarr(i,i1,i2)) then

c      dibuixa marca

       dist=dsqrt((u(i,j,10)-u(i,j+1,10))**2.+((v(i,j,10)
     + -v(i,j+1,10))**2.))

       dist1=xmk-xprev

       xu=u(i,j,10)+(u(i,j+1,10)-u(i,j,10))*(dist1/dist)
       xv=v(i,j,10)+(v(i,j+1,10)-v(i,j,10))*(dist1/dist)

c      REVISSAR SIGNES DESPLAÇAMENT!!!!!!!
c      I COHERENCIA AMB ALTRES MARQUES LATERALS
c      Despla a vores punts de control de costures
c      Segons verificació, aquí està be!!!!!!!!
c      Veure vents i altres zones!!!!!!!!!!
       alp=abs(datan((v(i,j+1,10)-v(i,j,10))/(u(i,j+1,10)-u(i,j,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)
c      SIGNE CREC QUE CORREGIT!!!! :)

       if (xinir.ge.0.01d0.or.k.gt.1) then
c      Point imp
       call pointg(psep+xu,psey-xv,xcir,3)
c      Point laser
       call point(psep+xu+2520.*xkf,-xv+psey,7)
       end if
       
       xmklast=xmk

       end if ! mark xmk
       end do ! j right

       end do ! k iteration

       xfinr(i,i1,i2)=xmark*1.0d0-(llarr(i,i1,i2)-xmklast)
c       write (*,*) "R ",i,i1,i2,xmklast,llarr(i,i1,i2),xfinr(i,i1,i2)

       end if ! i1=1 Extrados panels


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      For intrados panels (i1=2)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc              
       if (i1.eq.2) then
 

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Draw at left 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
c      write (*,*) i,"2 ",u(i,npo-4,9),v(i,npo-4,9)


c      Iterate in marks
       do k=1,60

       xacu(i)=0.0d0

c      Set successive distance marks
       if (k.eq.1) then
       xmk=xinil
       end if
       if (k.gt.1) then
       xmk=xmark*float(k-1)*1.0d0+xinil
       end if
      
       do j=npo-1,1,-1
       xprev=xacu(i)
       xacu(i)=xacu(i)+sqrt((u(i,j,9)-u(i,j+1,9))**2.+((v(i,j,9)
     + -v(i,j+1,9))**2.))
       xpost=xacu(i)

c      Detect segment where is mark
       if(xmk.le.xpost.and.xmk.ge.xprev.and.xmk.le.llarl(i,i1,i2)) then

c      dibuixa marca

       dist=dsqrt((u(i,j,9)-u(i,j+1,9))**2.+((v(i,j,9)
     + -v(i,j+1,9))**2.))

       dist1=xmk-xprev
       dist2=dist-dist1

       xu=u(i,j,9)+(u(i,j+1,9)-u(i,j,9))*(dist2/dist)
       xv=v(i,j,9)+(v(i,j+1,9)-v(i,j,9))*(dist2/dist)

c      REVISSAR SIGNES DESPLAÇAMENT!!!!!!!
c      Despla a vores punts de control de costures
       alp=abs(datan((v(i,j+1,9)-v(i,j,9))/(u(i,j+1,9)-u(i,j,9))))
       xu=xu-xdes*dsin(alp)
       xv=xv+xdes*dcos(alp)
       
       if (xinil.ge.0.01d0.or.k.gt.1) then
c      Point imp
       call pointg(psep+xu,psey-xv,xcir,3)
c      Point laser
       call point(psep+xu+2520.*xkf,-xv+psey,7)
       end if
       
       xmklast=xmk

       end if
       end do ! j left
      
       end do ! k

       xfinl(i,i1,i2)=xmark*1.0d0-(llarl(i,i1,i2)-xmklast)

c       write (*,*) "L ",i,i1,i2,xmklast,llarl(i,i1,i2),xfinl(i,i1,i2)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Draw at right
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       xacu(i)=xinir  

c      Iterate in marks
       do k=1,60

       xacu(i)=0.0d0

c      Set successive distance marks
       if (k.eq.1) then
       xmk=xinir
       end if
       if (k.gt.1) then
       xmk=xmark*float(k-1)*1.0d0+xinir
       end if
  
       do j=npo-1,1,-1
       xprev=xacu(i)
       xacu(i)=xacu(i)+sqrt((u(i,j,10)-u(i,j+1,10))**2.+((v(i,j,10)
     + -v(i,j+1,10))**2.))
       xpost=xacu(i)

c      Detect segment where is mark
       if(xmk.le.xpost.and.xmk.ge.xprev.and.xmk.le.llarr(i,i1,i2)) then

c      dibuixa marca

       dist=dsqrt((u(i,j,10)-u(i,j+1,10))**2.+((v(i,j,10)
     + -v(i,j+1,10))**2.))

       dist1=xmk-xprev
       dist2=dist-dist1

       xu=u(i,j,10)+(u(i,j+1,10)-u(i,j,10))*(dist2/dist)
       xv=v(i,j,10)+(v(i,j+1,10)-v(i,j,10))*(dist2/dist)

c      REVISSAR SIGNES DESPLAÇAMENT!!!!!!!
c      I COHERENCIA AMB ALTRES MARQUES LATERALS
c      Despla a vores punts de control de costures
c      Segons verificació, aquí està be!!!!!!!!
c      Veure vents i altres zones!!!!!!!!!!
       alp=abs(datan((v(i,j+1,10)-v(i,j,10))/(u(i,j+1,10)-u(i,j,10))))
       xu=xu+xdes*dsin(alp)
       xv=xv-xdes*dcos(alp)
c      SIGNE CORREGIT!!!!!!!! :)

       if (xinir.ge.0.01d0.or.k.gt.1) then
c      Point imp
       call pointg(psep+xu,psey-xv,xcir,3)
c      Point laser
       call point(psep+xu+2520.*xkf,-xv+psey,7)
       end if
       
       xmklast=xmk

       end if ! mark xmk
       end do ! j right

       end do ! k iteration

       xfinr(i,i1,i2)=xmark*1.0d0-(llarr(i,i1,i2)-xmklast)
c       write (*,*) "R ",i,i1,i2,xmklast,llarr(i,i1,i2),xfinr(i,i1,i2)

       end if ! i1=2 Intrados panels

       return
       end



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE iam
c      intrados anchor marks
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Locate and draw intrados anchor marks on both sides of a panel.
!! Anchor distances are read from the rib table, interpolated along the
!! developed seam edges, stored as planar anchor coordinates, and rendered in
!! the print and laser layouts according to the configured mark style.
!! @param[in] i Panel index in the coordinate arrays.
!! @param[in] i1 Surface selector retained by the panel-mark interface.
!! @param[in] i2 Panel-section selector; vent sections receive a base shift.
!! @param[in] np Profile point-count table retained by the legacy interface.
!! @param[in] npo Number of profile points in the developed panel.
!! @param[in] u Developed-panel horizontal coordinates.
!! @param[in] v Developed-panel vertical coordinates.
!! @param[in,out] rib Rib and anchor-distance data; scratch interpolation
!!                     entries 107 and 108 are updated.
!! @param[in] xinil Initial accumulated distance on the left side.
!! @param[in] xinir Initial accumulated distance on the right side.
!! @param[in,out] xfinl Legacy continuity-result array.
!! @param[in,out] xfinr Legacy continuity-result array.
!! @param[in] xmark Legacy regular-mark interval.
!! @param[in] llarl Previously calculated left-side lengths.
!! @param[in] llarr Previously calculated right-side lengths.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
!! @param[in] xcir Printed mark radius retained by the shared interface.
!! @param[in] xdes Normal displacement from the seam edge.
!! @param[in] xkf Global drawing scale used to locate the laser copy.
!! @param[in] mark1 Integer mark configuration retained by the interface.
!! @param[in] mark2 Real mark configuration retained by the interface.
!! @param[in] mark3 Real mark configuration retained by the interface.
!! @param[in] mark4 Mark style selectors; entry 5 controls anchor marks.
!! @param[in] mark5 Mark sizes; entry 5 controls anchor marks.
!! @param[in] mark6 Mark offsets; entry 5 controls anchor marks.
!! @param[in] xrib Nominal rib-mark width.
       SUBROUTINE iam(i,i1,i2,np,npo,u,v,rib,xinil,xinir,
     + xfinl,xfinr,xmark,llarl,llarr,psep,psey,xcir,xdes,xkf,
     + mark1,mark2,mark3,mark4,mark5,mark6,xrib)

       real*8 u(0:100,500,50),v(0:100,500,50),rib(0:100,500)
       real*8 llarl(0:100,3,100),llarr(0:100,3,100)
       integer np(0:100,9)
       real*8 xmk,xmk0,xmark,xmklast,xprev,xpost,xacu(0:100)
       real*8 dist,dist1,dist2,alp,xu,xv,psep,psey,xcir,xdes,xkf
       real*8 xinil,xinir
       real*8 xfinl(0:100,3,100),xfinr(0:100,3,100)
       integer i,j,k,i1,i2,npo

       integer klz
       real*8 xlen,xlenp,xequis,yequis,xdu,xdv,alpha,pi
       real*8 xanchor(0:100,6),yanchor(0:100,6)
       real*8 xanchoril(0:100,6),yanchoril(0:100,6)
       real*8 xanchorir(0:100,6),yanchorir(0:100,6)
c       real*8 xpeq,ypeq,xdesp,xdesp1x,xdesp1y,xdesp2x,xdesp2y

       integer mark1(50),mark4(50)
       real*8 mark2(50),mark3(50),mark5(50),mark6(50)
       real*8 xrib


       pi=4.*atan(1.)


c       write (*,*) i, "Ep..."

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Left side
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do klz=1,int(rib(i,15)) ! rib(i,15)=anchors number

       xlen=xinil
       j=npo
       xlenp=xlen+dsqrt((v(i,j,9)-v(i,j-1,9))**2.+
     + (u(i,j,9)-u(i,j-1,9))**2.)

       do j=npo,2,-1

c      Detect and draw anchor point
       if (rib(i,130+klz).ge.xlen.and.rib(i,130+klz).le.xlenp) then

       rib(i,107)=rib(i,130+klz)-xlen
       rib(i,108)=dsqrt((v(i,j,9)-v(i,j-1,9))**2.+(u(i,j,9)-u(i,j-1,9))
     + **2.)

c      Interpolate
       xequis=u(i,j,9)-(rib(i,107)*(u(i,j,9)-u(i,j-1,9)))/
     + rib(i,108)
       yequis=v(i,j,9)-(rib(i,107)*(v(i,j,9)-v(i,j-1,9)))/
     + rib(i,108)

c      SOLUCIÓ PROVISIONAL translació a base vent:
       if (i2.eq.4) then
       xequis=xequis-(u(i,npo,9)-u(i,1,9))
       yequis=yequis-(v(i,npo,9)-v(i,1,9))
       end if

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
c       xdesp=mark6(5)
c      REVISAR

       xdesp1x=xdesp*dsin(-alpha)
       xdesp1y=-xdesp*dcos(-alpha)
       xdesp2x=2.*xdesp*dsin(-alpha)
       xdesp2y=-2.*xdesp*dcos(-alpha)

c      Case 1: classic 3 orange points
       if (mark4(5).eq.1) then
       call point (psep+xpeq+2520*xkf,psey-ypeq,30)
       call point (psep+xpeq+xdesp1x+2520*xkf,psey-ypeq-xdesp1y,30)
       call point (psep+xpeq+xdesp2x+2520*xkf,psey-ypeq-xdesp2y,30)
       end if

c      Case 2: Controled 3 orange points
       if (mark4(5).eq.2) then
       xpeq=xequis+1.*mark6(5)*dsin(-alpha)
       ypeq=yequis-1.*mark6(5)*dcos(-alpha)
       xdesp1x=mark5(5)*dsin(-alpha)
       xdesp1y=-mark5(5)*dcos(-alpha)
       xdesp2x=2.*mark5(5)*dsin(-alpha)
       xdesp2y=-2.*mark5(5)*dcos(-alpha)
       call point (psep+xpeq+2520*xkf,psey-ypeq,30)
       call point (psep+xpeq+xdesp1x+2520*xkf,psey-ypeq-xdesp1y,30)
       call point (psep+xpeq+xdesp2x+2520*xkf,psey-ypeq-xdesp2y,30)
       end if

c      Case 3: triangle h mm
       if (mark4(5).eq.3) then
       xpeq=xequis-mark6(5)*dsin(alpha)
       ypeq=yequis+mark6(5)*dcos(alpha)
       call mtriangle(psep+xpeq+2520*xkf,psey-ypeq,mark5(5),-alpha,1)
       end if

       end if

       xlen=xlen+sqrt((v(i,j,9)-v(i,j-1,9))**2.+(u(i,j,9)-u(i,j-1,9))
     + **2.)
       if (j.gt.2) then
       xlenp=xlen+sqrt((v(i,j-1,9)-v(i,max(1,j-2),9))**2.+
     + (u(i,j-1,9)-u(i,max(1,j-2),9))**2.)
       end if

       end do ! j

       end do ! klz

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Right side
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do klz=1,rib(i+1,15)

       xlen=xinir
       j=npo
       xlenp=xlen+dsqrt((v(i,j,10)-v(i,j-1,10))**2.+
     + (u(i,j,10)-u(i,j-1,10))**2.)

       do j=npo,2,-1

c      Detect and draw anchor point
       if (rib(i+1,130+klz).ge.xlen.and.rib(i+1,130+klz).le.xlenp) then

       rib(i+1,107)=rib(i+1,130+klz)-xlen
       rib(i+1,108)=dsqrt((v(i,j,10)-v(i,j-1,10))**2.+(u(i,j,10)-
     + u(i,j-1,10))**2.)

c      Interpolate
       xequis=u(i,j,10)-(rib(i+1,107)*(u(i,j,10)-u(i,j-1,10)))/
     + rib(i+1,108)
       yequis=v(i,j,10)-(rib(i+1,107)*(v(i,j,10)-v(i,j-1,10)))/
     + rib(i+1,108)

c      SOLUCIÓ PROVISIONAL translació a base vent:
       if (i2.eq.4) then
       xequis=xequis-(u(i,npo,10)-u(i,1,10))
       yequis=yequis-(v(i,npo,10)-v(i,1,10))
       end if

c      Define anchor points in planar panel
       xanchorir(i+1,klz)=xequis
       yanchorir(i+1,klz)=yequis

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
c       xdesp=mark6(5)

       xdesp1x=xdesp*dsin(-alpha)
       xdesp1y=-xdesp*dcos(-alpha)
       xdesp2x=2.*xdesp*dsin(-alpha)
       xdesp2y=-2.*xdesp*dcos(-alpha)

c      Case 1: classic 3 orange points
       if (mark4(5).eq.1) then
       call point (psep+xpeq+2520*xkf,psey-ypeq,30)
       call point (psep+xpeq-xdesp1x+2520*xkf,psey-ypeq+xdesp1y,30)
       call point (psep+xpeq-xdesp2x+2520*xkf,psey-ypeq+xdesp2y,30)
       end if

c      Case 2: controled 3 orange points
       if (mark4(5).eq.2) then
       xpeq=xequis+1.*mark6(5)*dsin(-alpha)
       ypeq=yequis-1.*mark6(5)*dcos(-alpha)
       xdesp1x=mark5(5)*dsin(-alpha)
       xdesp1y=-mark5(5)*dcos(-alpha)
       xdesp2x=2.*mark5(5)*dsin(-alpha)
       xdesp2y=-2.*mark5(5)*dcos(-alpha)
       call point (psep+xpeq+2520*xkf,psey-ypeq,30)
       call point (psep+xpeq-xdesp1x+2520*xkf,psey-ypeq+xdesp1y,30)
       call point (psep+xpeq-xdesp2x+2520*xkf,psey-ypeq+xdesp2y,30)
       end if

c      Case 3: triangle 2 mm
       if (mark4(5).eq.3) then
       xpeq=xequis-mark6(5)*dsin(-alpha)
       ypeq=yequis+mark6(5)*dcos(-alpha)
       call mtriangle(psep+xpeq+2520*xkf,psey-ypeq,mark5(5),-alpha+pi,1)
       end if

       end if

       xlen=xlen+dsqrt((v(i,j,10)-v(i,j-1,10))**2.+(u(i,j,10)-
     + u(i,j-1,10))**2.)
       if (j.gt.2) then
       xlenp=xlen+dsqrt((v(i,j-1,10)-v(i,max(1,j-2),10))**2.+
     + (u(i,j-1,10)-u(i,max(1,j-2),10))**2.)
       end if

       end do ! j

       end do ! klz right side

       return
       end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUROUTINE ROMANO POINT I LINE 1-2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Place a numbered Romano mark relative to a straight panel segment.
!! @param[in] i Zero-based number source; the rendered label is `i + 1`.
!! @param[in] is Surface orientation: 1 extrados; -1 intrados.
!! @param[in] x1 Horizontal coordinate of the mark's reference endpoint.
!! @param[in] y1 Vertical coordinate of the mark's reference endpoint.
!! @param[in] x2 Horizontal coordinate defining the segment direction.
!! @param[in] y2 Vertical coordinate defining the segment direction.
!! @param[in] y3 Legacy reference coordinate retained for call compatibility.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
!! @param[in] xkf Global drawing scale used to locate the laser copy.
       SUBROUTINE romanop(i,is,x1,y1,x2,y2,y3,psep,psey,xkf)

       real*8 x1,x2,x3,x4,y1,y2,y3,y4,xkf,psep,psey
       integer is
       real*8 alpha1,alpha2
       real*8 xu,xv,horz,vert,hipo
       real*8 u1,v1,u2,v2
c      is=1    Extrados
c      is=-1   Intrados
       if (is.eq.1) then
       alpha1=-datan((y2-y1)/(x2-x1))
       end if
       if (is.eq.-1) then
       alpha1=-datan((y2-y1)/(x2-x1))+4.*atan(1.)
       end if
       horz=typm2(8)*dsqrt((x2-x1)**2.+(y2-y1)**2.)
       vert=typm3(8)*0.10d0
       hipo=dsqrt(horz*horz+vert*vert)
       alpha2=2.*datan(1.0d0)+alpha1

       u1=hipo*dcos(alpha1)
       v1=-hipo*dsin(alpha1)
       u2=u1-vert*dcos(alpha2)
       v2=v1+vert*dsin(alpha2)

       call romano(i+1,psep+x1+u2,y1+v2+psey,alpha1,typm6(8)*0.1,7)
       call romano(i+1,psep+x1+u2+2520.*xkf,y1+v2+psey,alpha1,
     + typm6(8)*0.1,7)

       return
       end





cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUROUTINE ROMANO POINT IN ARC using points 1-2
c      call subroutine arc3parc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Place a numbered Romano mark tangent to a sagitta-defined panel arc.
!! @param[in] i Zero-based number source; the rendered label is `i + 1`.
!! @param[in] iq Surface orientation: 1 extrados; -1 intrados.
!! @param[in] npunt Profile-point index containing the arc endpoints.
!! @param[in] uf Developed-panel horizontal coordinates.
!! @param[in] vf Developed-panel vertical coordinates.
!! @param[in,out] dfle Arc sagitta; its sign is normalized internally.
!! @param[in] is Sagitta-side selector: 1 positive; -1 negative.
!! @param[in] psep Horizontal drawing offset.
!! @param[in] psey Vertical drawing offset.
!! @param[in] ic Output process selector retained by the legacy interface.
!! @param[in] xkf Global drawing scale used to locate the laser copy.
       SUBROUTINE romanoparc(i,iq,npunt,uf,vf,dfle,is,psep,psey,ic,xkf)

c      SUBROUTINE romanoparc(i,uu1,vv1,uu2,vv2,y3,psep,psey,xkf)


       real*8 uf(0:100,500,50),vf(0:100,500,50)
       real*8 psep,psey,dfle,sfle,tetha,omega,puntu(0:10),puntv(0:10)
       real*8 a,b,c,d,e,f,ep,eps,ep1,ep3,radi,epinc,bv,cv,xupp
       real*8 g,mu,xi,parcu(0:21),parcv(0:21),parcul(0:21),parcvl(0:21)
       real*8 parcve(0:21),parcue(0:21)
       real*8 xru(2),xrv(2),xsu(2),xsv(2)
       real*8 uu1,vv1,uu2,vv2,xlen1,xlen2,xlenco
       integer npunt,j,iq,ic,is,isn,kini,kfin,ii

       real*8 xkf
       real*8 alpha1,alpha2
       real*8 vert,hipo
       real*8 u1,v1,u2,v2
c      Parameters interpretation:
c      iq=1  extrados
c      iq=-1 intrados
c      is=1  fletxa +
c      is=-1 fletxa -
c      ic=1  case print
c      ic=2  case laser


c      Sign control (negative 3D)
       isn=is
       ii=1
       if (dfle.lt.0.) then
       ii=-1
       end if
       isn=isn*ii

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      1. Selection points in the arc (based in subroutine arc3p)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
       xlenco=typm2(8)  ! fraction of arc
       xupp=1.0d0 ! (no cal aquí)
       j=npunt

c      Evita radi infinit
       if (dabs(dfle).lt.0.000001d0) then
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
       if (isn.eq.1) then
       puntu(2)=puntu(4)+dfle*dsin(omega)
       puntv(2)=puntv(4)+dfle*dcos(omega)
       end if
       if (isn.eq.-1) then
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

c      Arc global coordinates (u,v) using 21 points
       if (iq.eq.1) then ! extrados orientation
       do k=1,21
       ep=mu-(2.0d0*mu/20.0d0)*dfloat(k-1)
       parcu(k-1)=puntu(0)-radi*dsin(ep)
       parcv(k-1)=puntv(0)+radi*dcos(ep)
       end do
       end if
       if (iq.eq.-1) then ! intrados orientation
       do k=1,21
       ep=mu-(2.0d0*mu/20.0d0)*dfloat(k-1)
       parcu(21-k)=puntu(0)-radi*dsin(ep)
       parcv(21-k)=puntv(0)+radi*dcos(ep)
       end do
       end if

c      Arc local coordinates (u',v')
       do k=0,20
       parcul(k)=parcu(k)-puntu(1)
       parcvl(k)=dfloat(isn)*(parcv(k)-puntv(1))
       end do

c      Rotate local coordinates around punt 1
       do k=0,20
       parcu(k)=parcul(k)*dcos(xi)-parcvl(k)*dsin(xi)+puntu(1)
       parcv(k)=parcul(k)*dsin(xi)+parcvl(k)*dcos(xi)+puntv(1)
       end do

c      Draw rotated arc
       do k=1,20
c       call line(psep+parcu(k-1),psey-parcv(k-1)-5.,
c     + psep+parcu(k),psey-parcv(k)-5.,2)
       end do

c      Arc length
       xlen1=0.0d0
       do k=0,20-1
       xlen1=xlen1+dsqrt((parcu(k+1)-parcu(k))**2.+
     + (parcv(k+1)-parcv(k))**2.)
       end do

c      Detect kini and kfin
       xlen2=0.0d0
       do k=0,20-1
       xlen2=xlen2+dsqrt((parcu(k+1)-parcu(k))**2.+
     + (parcv(k+1)-parcv(k))**2.)
       if (xlen2.le.xlen1*xlenco) then
       kini=k
       kfin=k+2
       end if
       end do

c       write (*,*) i,kini,kfin,xlen1,xlen1*xlenco

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

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      2. Romano calculus
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       alpha1=datan((vv2-vv1)/(uu2-uu1))
       vert=typm3(8)*0.10d0
       alpha2=2.*datan(1.0d0)+alpha1

       if (iq.eq.1) then
       u1=uu1
       v1=vv1
       u2=u1-vert*dcos(alpha2)
       v2=v1-vert*dsin(alpha2)
       end if

       if (iq.eq.-1) then
       u1=uu1
       v1=vv1
       u2=u1+vert*dcos(alpha2)
       v2=v1+vert*dsin(alpha2)
       alpha1=alpha1+4.*atan(1.)
       end if
       
       call romano(i+1,psep+u2,psey-v2,alpha1,typm6(8)*0.1,7)
       call romano(i+1,psep+u2+2520.*xkf,psey-v2,alpha1,typm6(8)*0.1,7)

       return
       end





cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUROUTINE REFORMAT DAT AIRFOILS
c      call subroutine datair
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

