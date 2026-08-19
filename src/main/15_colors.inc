c      15. Marques de colors
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      15.1 Extrados marks
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Marques colors extrados panell i
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do k=1,npce ! ribs number with color marks

c      l=mark number in k rib
       do l=1,npc2e(k) ! marks in rib k

c      Detect if mark is exactly in the rib

       if (xpc2e(k,l).eq.0) then

       i=npc1e(k) ! rib number corresponding to k order

c      Detect segment (j,j+1) where color changes
       do j=1,np(i,2)-1

       if ((100.-(u(i,j,2)).le.xpc1e(k,l).and.
     + (100.-u(i,j+1,2)).gt.xpc1e(k,l))) then

       jcontrol=j        

       end if

       end do

c      Calcula longitud extrados fins punt j

       xle(k,l)=0.
       xleinc(k,l)=0.

       do j=1,jcontrol-1
       xle(k,l)=xle(k,l)+sqrt(((v(i,j+1,2)-v(i,j,2))**2)+
     + ((u(i,j+1,2)-u(i,j,2))**2))
       end do

c      Interpolacio punt color

       xmc=(v(i,j+1,2)-v(i,j,2))/(u(i,j+1,2)-u(i,j,2))
       xbc=v(i,j,2)-xmc*u(i,j,2)
       
       xpc3e(k,l)=100.-xpc1e(k,l)
       ypc3e(k,l)=xmc*xpc3e(k,l)+xbc

       xleinc(k,l)=dsqrt(((ypc3e(k,l)-v(i,j,2))**2)+
     + ((xpc3e(k,l)-u(i,j,2))**2))

       xle(k,l)=xle(k,l)+xleinc(k,l)
       
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Dibuixa marques color esquerra panell i
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Localitza punts 
       psep=1970.*xkf+seppix(i)*1.0d0
       psey=xyshift*xkf

       j=jcontrol

       xu=u(i,j,9)
       xv=v(i,j,9)

c      Calcula punt de color
       alp=abs(datan((v(i,j+1,9)-v(i,j,9))/(u(i,j+1,9)-u(i,j,9))))
       xu=xu+(xleinc(k,l)*(rib(i,5)/100.))*dcos(alp)
       xv=xv+(xleinc(k,l)*(rib(i,5)/100.))*dsin(alp)

c      Dibuixa creu
       call line(psep+xu-3.,-(xv)+psey,psep+xu+3.,
     + -(xv)+psey,7)
       call line(psep+xu,-(xv-3.)+psey,psep+xu,
     + -(xv+3.)+psey,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Dibuixa marques color dreta panell i-1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c       if (i.eq.nribss) then


       i=i-1

c      Localitza punts 
       psep=1970.*xkf+seppix(i)*1.0d0
       psey=xyshift*xkf

       j=jcontrol

       xu=u(i,j,10)
       xv=v(i,j,10)

c      Calcula punt de color
       alp=abs(datan((v(i,j+1,10)-v(i,j,10))/(u(i,j+1,10)-u(i,j,10))))
       xu=xu+(xleinc(k,l)*(rib(i,5)/100.))*dcos(alp)
       xv=xv+(xleinc(k,l)*(rib(i,5)/100.))*dsin(alp)

c      Dibuixa creu
       call line(psep+xu-3.,-(xv)+psey,psep+xu+3.,
     + -(xv)+psey,7)
       call line(psep+xu,-(xv-3.)+psey,psep+xu,
     + -(xv+3.)+psey,7)

c       i=i+1


       end if

       end do

       end do ! rib c colors
 


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      15.2 Intrados marks
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Control if type is not "ss"
       if (atp.ne."ss") then
       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Marques colors intrados panell i
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do k=1,npci ! ribs number with color marks

c      l=mark number in k rib
       do l=1,npc2i(k) ! marks in rib k

c      Detect if mark is exactly in the rib

       if (xpc2i(k,l).eq.0) then

       i=npc1i(k) ! rib number corresponding to k order

c      Detect segment (j,j-1) where color changes

       do j=np(i,1),np(i,2)+1,-1

       if ((100.-(u(i,j,2)).le.xpc1i(k,l).and.
     + (100.-u(i,j-1,2)).gt.xpc1i(k,l))) then

       jcontrol=j        

       end if

       end do  ! in j

c      Calcula longitud extrados fins punt j

       xli(k,l)=0.
       xliinc(k,l)=0.

       do j=np(i,1),jcontrol+1,-1
       xli(k,l)=xli(k,l)+sqrt(((v(i,j-1,2)-v(i,j,2))**2)+
     + ((u(i,j-1,2)-u(i,j,2))**2))
       end do

c      Interpolacio punt color

       xmc=(v(i,j,2)-v(i,j-1,2))/(u(i,j,2)-u(i,j-1,2))
       xbc=v(i,j-1,2)-xmc*u(i,j-1,2)
       
       xpc3i(k,l)=100.-xpc1i(k,l)
       ypc3i(k,l)=xmc*xpc3i(k,l)+xbc

       xliinc(k,l)=dsqrt(((ypc3i(k,l)-v(i,j,2))**2)+
     + ((xpc3i(k,l)-u(i,j,2))**2))

       xli(k,l)=xli(k,l)+xliinc(k,l)
       
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Dibuixa marques color esquerra panell i
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Localitza punts 
       psep=1970.*xkf+seppix(i)*1.0d0
       psey=(890.95+xyshift)*xkf

       j=jcontrol

       xu=u(i,j,9)
       xv=v(i,j,9)

c      Calcula punt de color
       alp=abs(datan((v(i,j,9)-v(i,j-1,9))/(u(i,j,9)-u(i,j-1,9))))
       xu=xu-(xliinc(k,l)*(rib(i,5)/100.))*dcos(alp)
       xv=xv-(xliinc(k,l)*(rib(i,5)/100.))*dsin(alp)

c      Dibuixa creu
       call line(psep+xu-3.,-(xv)+psey,psep+xu+3.,
     + -(xv)+psey,7)
       call line(psep+xu,-(xv-3.)+psey,psep+xu,
     + -(xv+3.)+psey,7)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Dibuixa marques color dreta panell i-1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       i=i-1

c      Localitza punts 
       psep=1970.*xkf+seppix(i)*1.0d0
       psey=(890.95+xyshift)*xkf

       j=jcontrol

       xu=u(i,j,10)
       xv=v(i,j,10)

c      Calcula punt de color
       alp=abs(datan((v(i,j,10)-v(i,j-1,10))/(u(i,j,10)-u(i,j-1,10))))
       xu=xu-(xliinc(k,l)*(rib(i,5)/100.))*dcos(alp)
       xv=xv-(xliinc(k,l)*(rib(i,5)/100.))*dsin(alp)

c      Dibuixa creu
       call line(psep+xu-3.,-(xv)+psey,psep+xu+3.,
     + -(xv)+psey,7)
       call line(psep+xu,-(xv-3.)+psey,psep+xu,
     + -(xv+3.)+psey,7)

       i=1+1

       end if

       end do

       end do ! rib c colors intra

       end if ! not "ss"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
