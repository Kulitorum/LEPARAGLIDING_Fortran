!> Add final labels to the already constructed 2D suspension-line drawing.
!!
!! Traverses finalized suspension and brake records, placing stable identifiers
!! beside the corresponding tree segments without changing line geometry.
c     20. Draw labels in 2D (in tree of lines)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Labels in lines
       corda1=cordam

       do i=1,corda1

       x0=(1260.-160.)*xkf
       y0=1800.*xkf

       x00=1260.*xkf*float(corda(i,1)-1)

       xxk=0.7 ! controls label positions

       xlabel=x1line(corda(i,1),corda(i,2),corda(i,3))+
     + (x2line(corda(i,1),corda(i,2),corda(i,3))-
     + x1line(corda(i,1),corda(i,2),corda(i,3)))*xxk+x0+x00

       zlabel=z1line(corda(i,1),corda(i,2),corda(i,3))+
     + (z2line(corda(i,1),corda(i,2),corda(i,3))-
     + z1line(corda(i,1),corda(i,2),corda(i,3)))*xxk+y0

c       xlabel=((x1line(corda(i,1),corda(i,2),corda(i,3))+x0+x00)+
c     + (x2line(corda(i,1),corda(i,2),corda(i,3))+x0+x00))*0.5d0

c       zlabel=((z1line(corda(i,1),corda(i,2),corda(i,3))+y0)+
c     + (z2line(corda(i,1),corda(i,2),corda(i,3))+y0))*0.5d0

c      Text size = typm3(9)
       xtext=ln4(i)
       call txt(xlabel,zlabel,typm3(9),0.0d0,xtext,7)

       end do
   
c      Labels in brakes
       do i=cordam+1,cordat

       x0=(1260.-160.)*xkf
       y0=(1800.+890.95)*xkf

       x00=1260.*xkf*float(corda(i,1)-1)

       xlabel=((x1line(corda(i,1),corda(i,2),corda(i,3))+x0+x00)+
     + (x2line(corda(i,1),corda(i,2),corda(i,3))+x0+x00))*0.5d0

       zlabel=((z1line(corda(i,1),corda(i,2),corda(i,3))+y0)+
     + (z2line(corda(i,1),corda(i,2),corda(i,3))+y0))*0.5d0

c      Text size = typm3(9)   
       xtext=ln4(i)
       call txt(xlabel,zlabel,typm3(9),0.0d0,xtext,7)

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
