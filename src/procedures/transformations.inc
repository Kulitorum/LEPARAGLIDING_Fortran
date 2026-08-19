!> Rotate and translate a sequence of local 2D points into global coordinates.
!! @param[in] n Number of valid points in the coordinate arrays.
!! @param[in] xloc Local horizontal coordinates.
!! @param[in] yloc Local vertical coordinates.
!! @param[out] xglo Transformed global horizontal coordinates.
!! @param[out] yglo Transformed global vertical coordinates.
!! @param[in] x0 Global horizontal coordinate of the local origin.
!! @param[in] y0 Global vertical coordinate of the local origin.
!! @param[in] angle Counter-clockwise rotation in degrees.
      SUBROUTINE loc2glo2D(n,xloc,yloc,xglo,yglo,x0,y0,angle)

      implicit none
c     Local coordinates xloc, yloc
      real*8 xloc(0:3000),yloc(0:3000)
      real*8 xglo(0:3000),yglo(0:3000)
      real*8 x0,y0,angle
      integer n,k
      type(vector_2d) local,origin,global
      type(rotation_2d) rotation

      origin=vector_2d(x0,y0)
      rotation=rotation_from_degrees(angle)

      do k=1,n
      local=vector_2d(xloc(k),yloc(k))
      global=rotate_and_translate(local,origin,rotation)
      xglo(k)=global%x
      yglo(k)=global%y
      end do

      return
      end




