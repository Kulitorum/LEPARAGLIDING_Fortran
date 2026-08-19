c      10. CALAGE
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
c        write (*,*) "pi 10. =",pi

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      10.1 Basic calculus
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       calag=(calage/100.0d0)*rib(1,5) ! calage in cm
       hcp=clengl+clengr            ! height karabiners-canopy
       hcpp=hcp ! hcp value saved (lost further...!)
       cple=(cpress/100.0d0)*rib(1,5)  ! center or pressure in cm
       assiette=dasin(((cple-calag)/hcp))*(180.0d0/pi)
       afinesse=(datan(1.0d0/planeig))*(180.0d0/pi)
       aoa=afinesse-assiette

c       write (*,*) ">>>>>>>>>>>>>>>>"
c       write (*,*) planeig,pi
c       write (*,*) afinesse,assiette



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      10.1+ Angle beetween glide path and airfoil plane (phii)
c            Angle beetween glide path and chord line (chii)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      afinesse = glide path angle       
       alpha_ii=afinesse*pi/180.0d0 ! conversion to radians

c      assiette angle (tetha), by convention positive BELOW horizon, and negative OVER horizon
c      (gamma=alpha+tetha)
       assiette_ii=assiette*pi/180.0d0 ! conversion to radians
       if (iflag_assiette.eq.1) then ! use assiette defined by user
       assiette_ii=assiette2003*pi/180.0d0 ! conversion to radians
       end if

c      Now let's use a trick...
c      We want to compare the angles between profile planes or chords with the flight path
c      We will modify the angle of trajectory to take into account the angle of assiette, 
c      since the ones we are interested in calculating are the relative angles phii and chii. 
c      That is, instead of turning the whole wing the angle of assiette, 
c      we will turn the angle of trajectory, which is easier.
       alpha_ii=alpha_ii-assiette_ii

c      For each rib
       do i=0,nribss

c      Plane defined by the space airfoil using points
c      j=1, j=int((np(i,2)/2), j=np(i,2)
c      points: initial, medium upper surface, leading edge
c      Warning! Solve the case thickness = 0.0 !!!
       j1=1
       j2=int((np(i,2)/2.0))
       j3=np(i,2)
       x1=x(i,j1)
c       x2=x(i,j2)
       x2=x_apap(i)
       x3=x(i,j3)
       y1=y(i,j1)
c       y2=y(i,j2)
       y2=y_apap(i)
       y3=y(i,j3)
       z1=z(i,j1)
c       z2=z(i,j2)
       z2=z_apap(i)
       z3=z(i,j3)

       Apla=(y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
       Bpla=(z2-z1)*(x3-x1)-(x2-x1)*(z3-z1)
       Cpla=(x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)

c      Director cosines of glidepath line
       l_line=0.0d0
       m_line=-dcos(alpha_ii)
       n_line=dsin(alpha_ii)

c      Angle phii between plane and line
c      Handbook of Mathematics, Bronshtein and Semendyayev. Mir, Moscow.
       phii(i)=dasin((Apla*l_line+Bpla*m_line+Cpla*n_line)/
     + (dsqrt((Apla*Apla+Bpla*Bpla+Cpla*Cpla)*
     + (l_line*l_line+m_line*m_line+n_line*n_line))))

c      Director cosines of the chord line
c      First point is nose point j=np(i,6), last point is nose point j=1
       l2_line=(x(i,1)-x(i,np(i,6)))/rib(i,5)
       m2_line=(y(i,1)-y(i,np(i,6)))/rib(i,5)
       n2_line=(z(i,1)-z(i,np(i,6)))/rib(i,5)

c      Angle between two lines in space
c      Handbook of Mathematics, Bronshtein and Semendyayev. Mir, Moscow.
       chii(i)=pi-dacos((l_line*l2_line+m_line*m2_line+n_line*n2_line)/
     + dsqrt((l_line*l_line+m_line*m_line+n_line*n_line)*
     + (l2_line*l2_line+m2_line*m2_line+n2_line*n2_line)))

c       write (*,*) i,np(i,6), -phii(i)*180.0d0/pi,chii(i)*180.0d0/pi
c       write (*,*) i,phii(i)*180.0d0/pi,chii(i)*180.0d0/pi,
c     + l2_line,m2_line,n2_line

       end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      10.2 Karabiners location
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       xkar=clengk/2.
       ykar=calag+rib(1,3)
       zkar=hcp

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      10.3 Dibuixa calage
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Box (2,1)

       x0=0.
       y0=900.*xkf

c      Central airfoil
       i=1

       do j=1,np(i,1)-1

       call line(x0+u(i,j,3),-v(i,j,3)+y0,x0+u(i,j+1,3),
     + -v(i,j+1,3)+y0,1)

       end do

c      Chord
       call line(x0,y0,x0+rib(i,5),y0,8)

c      Pilot-CP and Pilot-C
       call line(x0+ykar,y0+0.,x0+ykar,y0+zkar,1)
       call line(x0+cple,y0+0.,x0+ykar,y0+zkar,3)

c      Assiette and AoA angles
       call line(x0+cple,y0+0.,x0-100.,y0-(100.+cple)*
     + dtan(-assiette*pi/180.),4)
       call line(x0+cple,y0+0.,x0-100.,y0+(100.+cple)*
     + dtan(aoa*pi/180.),5)

       xtext="pilot"
       call txt(x0+ykar+20.,y0+zkar,10.0d0,0.0d0,xtext,7)
       xtext="C"
       call txt(x0+ykar,y0-10.,10.0d0,0.0d0,xtext,7)
       xtext="Pp"
       call txt(x0+cple,y0-10.,10.0d0,0.0d0,xtext,7)
       
c      Write text about calage parameters
       xtext="calage= "
       call txt(x0-220*xkf,y0+60.,10.0d0,0.0d0,xtext,7)
       write (xtext, '(F5.2)') calage
       call txt(x0-50*xkf,y0+60.,10.0d0,0.0d0,xtext,7)
       
       xtext="plumb point= "
       call txt(x0-220*xkf,y0+80,10.0d0,0.0d0,xtext,7)
       write (xtext, '(F5.2)') cpress
       call txt(x0-50*xkf,y0+80,10.0d0,0.0d0,xtext,7)
       
       xtext="glide ratio= "
       call txt(x0-220*xkf,y0+100,10.0d0,0.0d0,xtext,7)
       write (xtext, '(F5.2)') planeig
       call txt(x0-50*xkf,y0+100,10.0d0,0.0d0,xtext,7)
       
       xtext="glide angle= "
       call txt(x0-220*xkf,y0+120,10.0d0,0.0d0,xtext,7)
       write (xtext, '(F5.2)') afinesse
       call txt(x0-50*xkf,y0+120,10.0d0,0.0d0,xtext,7)
       
       xtext="angle of attack= "
       call txt(x0-220*xkf,y0+140,10.0d0,0.0d0,xtext,7)
       write (xtext, '(F5.2)') aoa
       call txt(x0-50*xkf,y0+140,10.0d0,0.0d0,xtext,7)
       
       xtext="assiette= "
       call txt(x0-220*xkf,y0+160,10.0d0,0.0d0,xtext,7)
       write (xtext, '(F5.2)') assiette
       call txt(x0-50*xkf,y0+160,10.0d0,0.0d0,xtext,7)


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
