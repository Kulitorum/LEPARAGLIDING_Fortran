!> Interpolate a point at a requested arc length along a 2D polyline.
!! @param[in] jini First source point index.
!! @param[in] jfin Last source point index.
!! @param[out] jc Index of the source segment containing the requested distance.
!! @param[out] ldif Distance from the beginning of segment `jc`.
!! @param[in] useg Source polyline horizontal coordinates.
!! @param[in] vseg Source polyline vertical coordinates.
!! @param[out] ltotal Total length of the selected polyline interval.
!! @param[in] linter Requested distance from source point `jini`.
!! @param[out] upoint Result horizontal coordinate, returned in element 1.
!! @param[out] vpoint Result vertical coordinate, returned in element 1.
!! @note The selected segment must not be vertical because the legacy Y
!!       interpolation uses its slope-intercept equation.
       SUBROUTINE interpseg(jini,jfin,jc,ldif,
     + useg,vseg,ltotal,linter,upoint,vpoint)

       real*8 useg(1000),vseg(1000)
       real*8 lsegment,lsegmentc,ltotal,linter,pi
       real*8 lparcial,lparcial1,lparcial2,ldif
       real*8 xk,xb,upoint(100),vpoint(100)
       integer npunts,i,jini,jfin,jc

       pi=4.0d0*datan(1.0d0)

c      Polyline lenght
       ltotal=0.0d0
       do j=jini,jfin-1
       lsegment=dsqrt((useg(j+1)-useg(j))**2.+(vseg(j+1)-vseg(j))**2.)
       ltotal=ltotal+lsegment
       end do

c      Detect segment
       lparcial=0.0d0
       lparcial1=0.0d0
       lparcial2=0.0d0
       do j=jini,jfin-1
       lsegment=dsqrt((useg(j+1)-useg(j))**2.+(vseg(j+1)-vseg(j))**2.)
       lparcial1=lparcial
       lparcial2=lparcial+lsegment
       if (linter.ge.lparcial1.and.linter.le.lparcial2) then
       jc=j  ! control point
       ldif=linter-lparcial1
       lsegmentc=lsegment
       end if
       lparcial=lparcial2
       end do

c      Interpolation point upoint(1),vpoint(1)
       xk=(vseg(jc+1)-vseg(jc))/(useg(jc+1)-useg(jc))
       xb=vseg(jc)-xk*useg(jc)
       upoint(1)=useg(jc)+(ldif/lsegmentc)*(useg(jc+1)-useg(jc))
       vpoint(1)=xk*upoint(1)+xb

       return
       end



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     SUBROUTINE        nonan(filename)
c     The answer by DeepSeek at my question:
c     "Translate these bash code: 
c     sed -i 's/NaN/0.0/g' leparagliding.dxf"
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c     DXF not working!!! :-(



