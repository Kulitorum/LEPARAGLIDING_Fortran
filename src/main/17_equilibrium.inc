c     17. CALAGE CALCULUS (SOLVE EQUILIBRIUM)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (k35d.eq.1) then ! only if section 35 active

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     17.0 Initial calculus
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Quick links mass (maillons)
       xlimog(16)=x35qlmass*dfloat(nrisers)*2./1000. ! kg

c      Lines drag
c      Drag in each line or riser xlimo(i,22) (N)
       x35linesd=0.0
       do i=1,cordat
       xlimo(i,22)=xlimo(i,21)*0.5*x35ro*x35v*x35v*(xlimo(i,3)*1e-6)
       x35linesd=x35linesd+xlimo(i,22)
       end do
       x35linesd=x35linesd*2. ! total N

c       write (*,*) "Ep1:",x35linesd
c       write (*,*) x35dlcoef
c       write (*,*) "Ep2:",0.5*x35ro*x35v*x35v*x35dlcoef*xlimog(3)

c      Total lines drag center of application, ponderation by line drag
c      xlimo(1,11) and xlimo(i,12) are (y,z) coordinates of line center
c      xlimo(i,22) is line drag (N)
       x35dly=0.0d0
       x35dlz=0.0d0
       do i=1,cordat
       x35dly=x35dly+xlimo(i,11)*xlimo(i,22)  
       x35dlz=x35dlz+xlimo(i,12)*xlimo(i,22)
       end do
       x35dly=(x35dly/100.)/(x35linesd*0.5)
       x35dlz=(x35dlz/100.)/(x35linesd*0.5)

c      Pilot drag (N)
       x35pilotd=x35dpcoef*0.5*x35ro*x35v*x35v*x35spilot

c      Flat area used as reference for aerodynamic forces
       farea=acit*2./10000. ! Flat area m2

c      Area. Select farea or parea
       farea=(rib(1,4)-rib(1,3))*rib(1,2)
       parea=(rib(1,4)-rib(1,3))*rib(1,6)
       do i=1,nribss-1
       farea=farea+(rib(i,4)-rib(i,3)+rib(i+1,4)-rib(i+1,3))*0.5*
     + (rib(i+1,2)-rib(i,2))
       parea=parea+(rib(i,4)-rib(i,3)+rib(i+1,4)-rib(i+1,3))*0.5*
     + (rib(i+1,6)-rib(i,6))
       end do
       farea=farea*2./10000.
       parea=parea*2./10000.

c      Select flat or projected area (default is flat)
       if (i37fparea.eq.1) then
       fparea=farea
       else
       fparea=parea
       end if

c      Aspect ratio is required by the instrumental glide estimate below
       fspan=2.*rib(nribss,2)/100.
       x35lambda=fspan*fspan/farea

c      Wing lift (N) previous
       x35wlp=x35cl*x35cle*0.5*x35ro*x35v*x35v*fparea

c      Wing drag (N) previous
       x35wdp=x35cd*x35cde*0.5*x35ro*x35v*x35v*fparea

c      Solve equilibrium equations
c      Simplified notations
c      Uniformice forces to Newtons (!)
       x35_a=x35wlp  ! Lift_p (previous)
       x35_b=x35wdp  ! Drag_p (previous)
       x35_c=(x35wingmass+xlimog(2)+xlimog(16)+x35pilotmass)*x35grav ! Weight (N)
       xlimog(17)=x35_c ! Total Weight (N)
       xlimog(18)=xlimog(17)/x35grav ! kg total mass
       x35_d=x35linesd ! Lines drag
       x35_e=x35pilotd ! Pilot drag

c      Set instrumental glide angle (no used)
       x35finesse=2.*x35lambda-2.     ! Set finesse proportional to AR!!!
       x35gammai=datan(1./x35finesse) ! Set instrumental glide angle

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Solve for gamma and K (vertical and horizontal equilibrium) (I+II)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      17.1 Method 1 (i37solve=1) Solve gamma and K simultaneous
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (i37solve.eq.1) then

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Solve in first aproximate
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       x35zeroprev=100.
       do x35gamma=0.01,0.8,0.005 ! explore 1 to 45 deg aprox
c      Vertical equilibrium
       x35_k1=(x35_c-(x35_d+x35_e)*dsin(x35gamma))/
     + (x35_a*dcos(x35gamma)+x35_b*dsin(x35gamma))
c      Horizontal equilibrium
       x35_k2=-((x35_d+x35_e)*dcos(x35gamma))/
     + (-x35_a*dsin(x35gamma)+x35_b*dcos(x35gamma))
       x35zero=dabs(x35_k1-x35_k2)
c      Solve zero by brute force
       if (x35zero.le.x35zeroprev.and.dabs(x35_k2).le.2.0d0) then
       x35gammafi=x35gamma
       x35zeroprev=x35zero
       x35_k1fi=x35_k1
       x35_k2fi=x35_k2
       end if
       end do ! gamma

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Solve again, with increased precision
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       x35zeroprev=100.
       do x35gamma=x35gammafi-0.08,x35gammafi+0.08,0.0016 ! 100 iterations +-5 deg
c      Vertical equilibrium
       x35_k1=(x35_c-(x35_d+x35_e)*dsin(x35gamma))/
     + (x35_a*dcos(x35gamma)+x35_b*dsin(x35gamma))
c      Horizontal equilibrium
       x35_k2=-((x35_d+x35_e)*dcos(x35gamma))/
     + (-x35_a*dsin(x35gamma)+x35_b*dcos(x35gamma))
       x35zero=dabs(x35_k1-x35_k2)
c      Solve zero by brute force
       if (x35zero.le.x35zeroprev.and.dabs(x35_k2).le.2.0d0) then
       x35gammafi=x35gamma
       x35zeroprev=x35zero
       x35_k1fi=x35_k1
       x35_k2fi=x35_k2
       end if
c       write (*,*) "Ep ",x35gamma*180./pi,x35gammafi*180./pi,
c     + x35_k1,x35_k2,x35zero
       end do ! gamma

c      k and gamma
       x35_k1=x35_k1fi
       x35_k2=x35_k2fi
       x35gamma=x35gammafi
       x35theta=x35gamma-x35alpha
       x35_k=(x35_k1+x35_k2)/2.
       x35finesse=1./dtan(x35gamma)

c      Initial flight speed
       x35vi=dsqrt((xlimog(17)*dcos(x35gamma))/
     + (0.5*x35ro*fparea*x35cl*x35cle*x35_k))

c      x35_a = "Lift"
       x35wl=x35cl*x35_k*0.5*x35ro*x35vi*x35vi*fparea
c      Lift extra
       x35liftextra=x35cl*x35_k*(x35cle-1.)*0.5*x35ro*x35vi*x35vi*fparea
c      x35_b = "Drag"
       x35wd=x35cd*x35_k*0.5*x35ro*x35vi*x35vi*fparea
c      Drag extra
       x35dragextra=x35cd*x35_k*(x35cde-1.)*0.5*x35ro*x35vi*x35vi*fparea

c      Put warnings and hints if no convergence (!!!)
c      If weight is excessive, there may be no angle that achieves vertical balance
c      If the lift at angle 0 is greater than the weight, there is no balance

c      Verify convergence
       i35m1contr=0
       x35m1contr=0.0d0

       end if ! End method 1

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      17.2 Method 2 (i37solve=2) solve for gamma i dragextra
c      WARNING! Is not logical! Do not use!
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (i37solve.eq.2) then

       x35_k=1.0
       x35_k1=1.0
       x35_k2=1.0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Solve dragextra in first aproximate
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       x35zeroprev=100.
       do x35gamma=0.01,0.8,0.005 ! explore 1 to 45 deg aprox
c      Vertical equilibrium
       x35_k1d=(x35_c-x35_a*dcos(x35gamma)-(x35_b+x35_d+x35_e)*
     + dsin(x35gamma))/dsin(x35gamma)
c      Horizontal equilibrium
       x35_k2d=(x35_a*dsin(x35gamma)-(x35_b+x35_d+x35_e)*
     + dcos(x35gamma))/dcos(x35gamma)
c      Zero
       x35zero=dabs(x35_k1d-x35_k2d)
c      Solve zero by brute force
       if (x35zero.le.x35zeroprev) then
       x35gammafi=x35gamma
       x35zeroprev=x35zero
       x35_k1fi=x35_k1d
       x35_k2fi=x35_k2d
       end if
c       write (*,*) "Ep ",x35gamma*180./pi,x35gammafi*180./pi,
c     + x35_k1d,x35_k2d,x35zero
       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Solve again, with increased precision
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       x35zeroprev=100.
       do x35gamma=x35gammafi-0.08,x35gammafi+0.08,0.0016 ! 100 iterations +-5 deg
c      Vertical equilibrium
       x35_k1d=(x35_c-x35_a*dcos(x35gamma)-(x35_b+x35_d+x35_e)*
     + dsin(x35gamma))/dsin(x35gamma)
c      Horizontal equilibrium
       x35_k2d=(x35_a*dsin(x35gamma)-(x35_b+x35_d+x35_e)*
     + dcos(x35gamma))/dcos(x35gamma)
c      Zero
       x35zero=dabs(x35_k1d-x35_k2d)
c      Solve zero by brute force
       if (x35zero.le.x35zeroprev) then
       x35gammafi=x35gamma
       x35zeroprev=x35zero
       x35_k1fi=x35_k1d
       x35_k2fi=x35_k2d
       end if
c       write (*,*) "Ep ",x35gamma*180./pi,x35gammafi*180./pi,
c     + x35_k1d,x35_k2d,x35zero
       end do

       x35_k1d=x35_k1fi
       x35_k2d=x35_k2fi
       x35gamma=x35gammafi
       x35theta=x35gamma-x35alpha
       x35dragextra=(x35_k1d+x35_k2d)/2.
       x35finesse=1./dtan(x35gamma)
       x35_k1=1.0
       x35_k2=1.0
       x35_k=1.0
       x35wl=x35wlp*x35_k            ! set Lift final regularized
       x35wd=x35wdp*x35_k            ! set Drag final regularized
c      Put warnings and hints if no convergence (!!!)
c      If weight is excessive, there may be no angle that achieves vertical balance
c      If the lift at angle 0 is greater than the weight, there is no balance

c       x35_k=1.0
c       x35_k1=1.0
c       x35_k2=1.0

c      Verify convergence WARNING Improve criteria !!!!
       i35m2contr=0        ! 0 = converges
       x35m2contr=0.0d0
       x35m2contr=dabs((x35_k1fi-x35_k2fi)/x35_k1fi)*100.
       if (x35m2contr.gt.15.or.x35_k1fi.eq.0.0d0) then
       i35m2contr=1 ! Does not converge (!)
       end if
c       write (*,*) 
c       write (*,*) i35m2contr,x35m2contr

       end if ! End method 2


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      17.3 Method 3 (i37solve=2) solve for (gamma, K) and then (gamma dragextra)
c      WARNING! Is not logical! Do not use!
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (i37solve.eq.3) then

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Uniformize Lift and Drag using first method 1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       x35zeroprev=100.
       do x35gamma=0.01,0.8,0.005 ! explore 1 to 45 deg aprox
c      Vertical equilibrium
       x35_k1=(x35_c-(x35_d+x35_e)*dsin(x35gamma))/
     + (x35_a*dcos(x35gamma)+x35_b*dsin(x35gamma))
c      Horizontal equilibrium
       x35_k2=-((x35_d+x35_e)*dcos(x35gamma))/
     + (-x35_a*dsin(x35gamma)+x35_b*dcos(x35gamma))
       x35zero=dabs(x35_k1-x35_k2)
c      Solve zero by brute force
       if (x35zero.le.x35zeroprev.and.dabs(x35_k2).le.2.0d0) then
       x35gammafi=x35gamma
       x35zeroprev=x35zero
       x35_k1fi=x35_k1
       x35_k2fi=x35_k2
       end if
c       write (*,*) "Ep ",x35gamma*180./pi,x35gammafi*180./pi,
c     + x35_k1,x35_k2,x35zero
       end do
       x35_k1=x35_k1fi
       x35_k2=x35_k2fi
       x35gamma=x35gammafi
       x35theta=x35gamma-x35alpha
       x35_k=(x35_k1+x35_k2)/2.
       x35finesse=1./dtan(x35gamma)
       x35wl=x35wlp*x35_k ! Lift unifomized
       x35wd=x35wdp*x35_k ! Drag uniformized
       x35_a=x35wl
       x35_b=x35wd

c       x35gamma=10.125*pi/180.
c       write (*,*) "gamma ",x35gamma*180./pi
c       write (*,*) "VERTICAL FORCES:"
c       write (*,*) "Lift_v ",x35_a*dcos(x35gamma)
c       write (*,*) "Drag_v ",x35_b*dsin(x35gamma)
c       write (*,*) "Drage_v ",x35dragextra*dsin(x35gamma)
c       write (*,*) "Dragl_v ",x35_d*dsin(x35gamma)
c       write (*,*) "Dragp_v ",x35_e*dsin(x35gamma)
c       write (*,*) "Total_v ",x35_a*dcos(x35gamma)+(x35_b+
c     + x35dragextra+x35_d+x35_e)*dsin(x35gamma)
c       write (*,*) "Weight_v ",x35_c

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Solve dragextra in first aproximate
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       x35zeroprev=100.
       do x35gamma=0.01,0.8,0.005 ! explore 1 to 45 deg aprox
c      Vertical equilibrium
       x35_k1d=(x35_c-x35_a*dcos(x35gamma)-(x35_b+x35_d+x35_e)*
     + dsin(x35gamma))/dsin(x35gamma)
c      Horizontal equilibrium
       x35_k2d=(x35_a*dsin(x35gamma)-(x35_b+x35_d+x35_e)*
     + dcos(x35gamma))/dcos(x35gamma)
c      Zero
       x35zero=dabs(x35_k1d-x35_k2d)
c      Solve zero by brute force
       if (x35zero.le.x35zeroprev) then
       x35gammafi=x35gamma
       x35zeroprev=x35zero
       x35_k1fi=x35_k1d
       x35_k2fi=x35_k2d
       end if
c       write (*,*) "Ep ",x35gamma*180./pi,x35gammafi*180./pi,
c     + x35_k1d,x35_k2d,x35zero
       end do
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Solve again, with increased precision
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       x35zeroprev=100.
       do x35gamma=x35gammafi-0.08,x35gammafi+0.08,0.0016 ! 100 iterations +-5 deg
c      Vertical equilibrium
       x35_k1d=(x35_c-x35_a*dcos(x35gamma)-(x35_b+x35_d+x35_e)*
     + dsin(x35gamma))/dsin(x35gamma)
c      Horizontal equilibrium
       x35_k2d=(x35_a*dsin(x35gamma)-(x35_b+x35_d+x35_e)*
     + dcos(x35gamma))/dcos(x35gamma)
c      Zero
       x35zero=dabs(x35_k1d-x35_k2d)
c      Solve zero by brute force
       if (x35zero.le.x35zeroprev) then
       x35gammafi=x35gamma
       x35zeroprev=x35zero
       x35_k1fi=x35_k1d
       x35_k2fi=x35_k2d
       end if
c       write (*,*) "Ep ",x35gamma*180./pi,x35gammafi*180./pi,
c     + x35_k1d,x35_k2d,x35zero
       end do

       x35_k1d=x35_k1fi
       x35_k2d=x35_k2fi
       x35gamma=x35gammafi
       x35theta=x35gamma-x35alpha
       x35dragextra=(x35_k1d+x35_k2d)/2.
       x35finesse=1./dtan(x35gamma)
c      Put warnings and hints if no convergence (!!!)
c      If weight is excessive, there may be no angle that achieves vertical balance
c      If the lift at angle 0 is greater than the weight, there is no balance

c       x35_k=1.0
c       x35_k1=1.0
c       x35_k2=1.0

c      Verify convergence
       i35m3contr=0
       x35m3contr=0.0d0

       end if ! End method 3

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      17.4 Method 4 (i37solve=4) solve for gamma and Cde
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (i37solve.eq.4) then

c      De Villiers method 4, solving glide angle directly:
c      gamma --> Analytical solution from horizontal equilibrium
       x35gamma=2.*datan((2.*(dsqrt(x35_a*x35_a+x35_b*x35_b+
     + 2.*x35_b*(x35_d+x35_e)+x35_d*x35_d+2.*x35_d*x35_e+x35_e*x35_e))
     + -2.*x35_a)/(2.*(x35_b+x35_d+x35_e)))

       x35theta=x35gamma-x35alpha
       x35finesse=1./dtan(x35gamma)

c      Do not regularize
       x35_k=1.0
       x35_k1=1.0
       x35_k2=1.0

c      Initial flight speed
       x35vi=dsqrt((xlimog(17)*dcos(x35gamma))/
     + (0.5*x35ro*fparea*x35cl*x35cle*x35_k))

c      x35_a = "Lift"
       x35wl=x35cl*x35_k*0.5*x35ro*x35vi*x35vi*fparea
c      Lift extra
       x35liftextra=x35cl*x35_k*(x35cle-1.)*0.5*x35ro*x35vi*x35vi*fparea
c      x35_b = "Drag"
       x35wd=x35cd*x35_k*0.5*x35ro*x35vi*x35vi*fparea
c      Drag extra
       x35dragextra=x35cd*x35_k*(x35cde-1.)*0.5*x35ro*x35vi*x35vi*fparea

c      Verify convergence, not used because method is analitycal
       i35m4contr=0
       x35m4contr=0.0d0

       end if ! End method 4


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      17.5 Method 5 (i37solve=5) solve for gamma and V
c      OK, but excessive loops
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (i37solve.eq.5) then

c      Set drag extra, attention to Kd coefficient, initially camouflaged in Cde (!)
c      Cde transports Kd and then returns to 1.0 like Cle
       x35kd=x35cde   ! Set kd
       x35cde=1.0     ! Restore cde

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      17.5.1 Do the first RUN loop using a rought interval (steeps 0.5 m/s)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Loop in V from 5 to 15 m/s
       x35zero=0.0d0          ! Zero
       x35zeroprev=1000.0d0   ! First value close to zero
       x35linesdf=0.0d0       ! Final drag lines
       do x35v=5.,15.,0.5 ! Do only 20 steps in first run

c      x35_a = "Lift"
       x35_a=x35cl*x35cle*0.5*x35ro*x35v*x35v*fparea
c      x35_b = "Drag"
       x35_b=x35cd*x35cde*0.5*x35ro*x35v*x35v*fparea
c      x35_f = "Drag+Drag extra"
       x35_f=x35cd*x35cde*x35kd*0.5*x35ro*x35v*x35v*fparea

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      x35_d= "Lines drag" ! --> necessary calc all lines drag again (!!!)
c      x35ro = air mass density 1.225 kg/m3
c      x35v  = speed m/s
c      x35mu = air dynamic viscosity 18.46 x 1e-6 Pascals·s
c      Reynolds numbers by types tyline(k,20)
       do k=1,k34n
       tyline(k,20)=(x35ro*x35v*tyline(k,2)*1e-3)/(x35mu*1e-6)
       end do
c      Reynolds number operating in each line xlimo(1,20)
       do i=1,cordat
       xlimo(i,20)=tyline(corda(i,40),20)
       end do
c      Drag coefficient in each line xlimo(1,21)
c      Weisener and its simple equation for a cylinder
c      Source: De Villiers, HEGALA v6 report
       do i=1,cordat
       xlimo(i,21)=(10.*xlimo(i,20)**(-2./3.))+1.0d0
c      Correction for rectangular risers, increment Cd
       if (corda(i,2).eq.1.and.tylinea(corda(i,40),1).eq."r") then
       xlimo(i,21)=1.98
       end if
       end do
c      Drag in each line or riser xlimo(i,22) (N)
       x35linesd=0.0
       do i=1,cordat
       xlimo(i,22)=xlimo(i,21)*0.5*x35ro*x35v*x35v*(xlimo(i,3)*1e-6)
       x35linesd=x35linesd+xlimo(i,22)
       end do
       x35linesd=x35linesd*2. ! total N
       x35_d=x35linesd        ! lines drag
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      x35_e= "Pilot drag"
       x35_e=x35dpcoef*0.5*x35ro*x35v*x35v*x35spilot

c      De Villiers method 5, solving glide angle directly:
c      x35_a Lift
c      x35_b Drag
c      x35_c Total weight (N)
c      x35_d Lines drag
c      x35_e Pilot drag
c      x35_f Drag+Dragextra
c      gamma --> Analytical solution from horizontal equilibrium
       x35gamma=2.*datan((2.*(dsqrt(x35_a*x35_a+x35_f*x35_f+
     + 2.*x35_f*(x35_d+x35_e)+x35_d*x35_d+2.*x35_d*x35_e+x35_e*x35_e))
     + -2.*x35_a)/(2.*(x35_f+x35_d+x35_e)))

c      Calc f(V)=0. Auxiliar notations
       x355_1=x35_d*dsin(x35gamma)    
       x355_2=x35_e*dsin(x35gamma)
       x355_3=x35_a*dcos(x35gamma)
       x355_4=x35_f*dsin(x35gamma)

c      Vertical equilibrium equation f(speed)=0
       x35zero=x355_1+x355_2+x355_3+x355_4-x35_c

c      Find root, using condition "detect the minimun value"
       if (dabs(x35zero).le.x35zeroprev) then
       x35zeroprev=dabs(x35zero)
       x35vi=x35v        ! final speed
       x35linesdf=x35_d  ! final lines drag
       end if
c      Convergence verification
       if (x35vi.gt.5.0d0.and.x35vi.lt.15.0d0) then ! Solution was found
       i35m5contr=0 ! method converges
       else
       i35m5contr=1 ! Not found
       end if

c       write (*,*) "Here1: ",x35v,x35zero

       end do ! Close loop in V
c       write (*,*) x35vi,x35zeroprev
c       write (*,*)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      17.5.2 Do a second RUN loop using a fine interval (steeps 0.01 m/s)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Loop in V
       x35zero=0.0d0
       x35zeroprev=1000.0d0
       x35linesdf=0.0d0
       do x35v=x35vi-0.5,x35vi+0.5,0.01 ! Using 100 steps in 1 m/2

c      x35_a = "Lift"
       x35_a=x35cl*x35cle*0.5*x35ro*x35v*x35v*fparea
c      x35_b = "Drag"
       x35_b=x35cd*x35cde*0.5*x35ro*x35v*x35v*fparea
c      x35_f = "Drag+Drag extra"
       x35_f=x35cd*x35cde*x35kd*0.5*x35ro*x35v*x35v*fparea

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      x35_d= "Lines drag" ! --> necessary calc all lines drag again (!!!)
c      x35ro = air mass density 1.225 kg/m3
c      x35v  = speed m/s
c      x35mu = air dynamic viscosity 18.46 x 1e-6 Pascals·s
c      Reynolds numbers by types tyline(k,20)
       do k=1,k34n
       tyline(k,20)=(x35ro*x35v*tyline(k,2)*1e-3)/(x35mu*1e-6)
       end do
c      Reynolds number operating in each line xlimo(1,20)
       do i=1,cordat
       xlimo(i,20)=tyline(corda(i,40),20)
       end do
c      Drag coefficient in each line xlimo(1,21)
c      Weisener and its simple equation for a cylinder
c      Source: De Villiers, HEGALA v6 report
       do i=1,cordat
       xlimo(i,21)=(10.*xlimo(i,20)**(-2./3.))+1.0d0
c      Correction for rectangular risers, increment Cd
       if (corda(i,2).eq.1.and.tylinea(corda(i,40),1).eq."r") then
       xlimo(i,21)=1.98
       end if
       end do
c      Drag in each line or riser xlimo(i,22) (N)
       x35linesd=0.0
       do i=1,cordat
       xlimo(i,22)=xlimo(i,21)*0.5*x35ro*x35v*x35v*(xlimo(i,3)*1e-6)
       x35linesd=x35linesd+xlimo(i,22)
       end do
       x35linesd=x35linesd*2. ! total N
       x35_d=x35linesd        ! lines drag
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      x35_e= "Pilot drag"
       x35_e=x35dpcoef*0.5*x35ro*x35v*x35v*x35spilot

c      De Villiers method 5, solving glide angle directly:
c      x35_a Lift
c      x35_b Drag
c      x35_c Total weight (N)
c      x35_d Lines drag
c      x35_e Pilot drag
c      x35_f Drag+Dragextra
c      gamma --> Analytical solution from horizontal equilibrium
       x35gamma=2.*datan((2.*(dsqrt(x35_a*x35_a+x35_f*x35_f+
     + 2.*x35_f*(x35_d+x35_e)+x35_d*x35_d+2.*x35_d*x35_e+x35_e*x35_e))
     + -2.*x35_a)/(2.*(x35_f+x35_d+x35_e)))

c      Calc f(V)=0. Auxiliar notations
       x355_1=x35_d*dsin(x35gamma)    
       x355_2=x35_e*dsin(x35gamma)
       x355_3=x35_a*dcos(x35gamma)
       x355_4=x35_f*dsin(x35gamma)

c      Vertical equilibrium equation f(speed)=0
       x35zero=x355_1+x355_2+x355_3+x355_4-x35_c

c      Find root, using condition "detect the minimun value"
       if (dabs(x35zero).le.x35zeroprev) then
       x35zeroprev=dabs(x35zero)
       x35vi=x35v        ! final speed
       x35linesdf=x35_d  ! final lines drag
       end if

c       write (*,*) "Here2: ",x35v,x35zero

       end do ! Close loop in V
c       write (*,*) x35vi,x35zeroprev
c       write (*,*)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      17.5.3 Now we have V :) calcule values again
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       x35v=x35vi ! Speed

c      Calc main values again
c      x35_a = "Lift"
       x35_a=x35cl*x35cle*0.5*x35ro*x35vi*x35vi*fparea
c      x35_b = "Drag"
       x35_b=x35cd*x35cde*0.5*x35ro*x35vi*x35vi*fparea
c      x35_f = "Drag+Drag extra"
       x35_f=x35cd*x35cde*x35kd*0.5*x35ro*x35vi*x35vi*fparea
c      Drag extra again
       x35dragextra=x35_b*(x35kd-1.0d0)
c      Calc gamma again
       x35gamma=2.*datan((2.*(dsqrt(x35_a*x35_a+x35_f*x35_f+
     + 2.*x35_f*(x35_d+x35_e)+x35_d*x35_d+2.*x35_d*x35_e+x35_e*x35_e))
     + -2.*x35_a)/(2.*(x35_f+x35_d+x35_e)))

c      Derived values theta="assiette" and GR="finesse"
       x35theta=x35gamma-x35alpha
       x35finesse=1./dtan(x35gamma)

c      Set final lift and drag
       x35wl=x35_a
       x35wd=x35_b

c      Set final lines drag
       x35linesd=x35linesdf

c      Set final pilot drag
       x35pilotd=x35dpcoef*0.5*x35ro*x35vi*x35vi*x35spilot

c      Set default k values (not used in this method)
       x35_k=1.0
       x35_k1=1.0
       x35_k2=1.0

c      Restore cde
       x35cde=x35kd

c      x35_a = "Lift"
       x35wl=x35cl*x35_k*0.5*x35ro*x35vi*x35vi*fparea
c      Lift extra
       x35liftextra=x35cl*x35_k*(x35cle-1.)*0.5*x35ro*x35vi*x35vi*fparea
c      x35_b = "Drag"
       x35wd=x35cd*x35_k*0.5*x35ro*x35vi*x35vi*fparea
c      Drag extra
       x35dragextra=x35cd*x35_k*(x35cde-1.)*0.5*x35ro*x35vi*x35vi*fparea

       end if ! End method 5

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      17.6 Method 6
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Find gamma explicity and V independent
c      Find V explicity
c      Use cle and cde values

       if (i37solve.eq.6) then

c      Solve gamma from horizontal equilibrium equation
c      Define values
       x35var(1)=fparea*fparea*x35cd*x35cde*x35cd*x35cde
       x35var(2)=fparea*fparea*x35cl*x35cle*x35cl*x35cle
       x35var(3)=2.*fparea*x35spilot*x35dpcoef*x35cd*x35cde
       x35var(4)=2.*fparea*xlimog(3)*x35dlcoef*x35cd*x35cde
       x35var(5)=x35spilot*x35spilot*x35dpcoef*x35dpcoef
       x35var(6)=2.*x35spilot*xlimog(3)*x35dlcoef*x35dpcoef
       x35var(7)=xlimog(3)*xlimog(3)*x35dlcoef*x35dlcoef
       x35var(8)=fparea*x35cl*x35cle
       x35var(9)=x35spilot*x35dpcoef
       x35var(10)=xlimog(3)*x35dlcoef
       x35var(11)=fparea*x35cd*x35cde

c      Glide angle is not function of V (!)
       x35gamma=2.*datan((dsqrt(x35var(1)+x35var(2)+x35var(3)+x35var(4)+
     + x35var(5)+x35var(6)+x35var(7))-x35var(8))/
     + (x35var(9)+x35var(10)+x35var(11)))

c       write (*,*) "xlimog(3)",xlimog(3)
c       write (*,*) "x35dlcoef",x35dlcoef
c       write (*,*) "x35dpcoef",x35dpcoef
c       write (*,*) "x35spilot",x35spilot
c       do k=1,11
c       write (*,*) k,x35var(k)
c       end do
c       write (*,*) "Ep gamma",x35gamma*180./pi

c      Solve V from vertical equilibrium equation
c      Define values
       x35var(14)=x35wingmass+xlimog(2)+xlimog(16)+x35pilotmass
       x35var(15)=x35spilot*x35dpcoef*dsin(x35gamma)
       x35var(16)=xlimog(3)*x35dlcoef*dsin(x35gamma)
       x35var(17)=fparea*x35cl*x35cle*dcos(x35gamma)
       x35var(18)=fparea*x35cd*x35cde*dsin(x35gamma)
       x35var(19)=x35spilot*x35dpcoef*dsin(x35gamma)
       x35var(20)=xlimog(3)*x35dlcoef*dsin(x35gamma)
       x35var(21)=fparea*x35cl*x35cle*dcos(x35gamma)
       x35var(22)=fparea*x35cd*x35cde*dsin(x35gamma)

c      Speed of flight
       x35vi=dsqrt(2.0d0)*dsqrt(x35ro*x35grav*x35var(14)*(x35var(15)+
     + x35var(16)+x35var(17)+x35var(18)))/(x35ro*(x35var(19)+
     + x35var(20)+x35var(21)+x35var(22)))

c      Calcule values Lift and Drag using new speed
c      x35_a = "Lift"
       x35_a=x35cl*0.5*x35ro*x35vi*x35vi*fparea
c      Lift extra
       x35liftextra=x35cl*(x35cle-1.)*0.5*x35ro*x35vi*x35vi*fparea
c      x35_b = "Drag"
       x35_b=x35cd*0.5*x35ro*x35vi*x35vi*fparea
c      Drag extra
       x35dragextra=x35cd*(x35cde-1.)*0.5*x35ro*x35vi*x35vi*fparea

c      Derived values theta="assiette" and GR="finesse"
       x35theta=x35gamma-x35alpha
       x35finesse=1./dtan(x35gamma)

c      Set final lift and drag
       x35wl=x35_a
       x35wd=x35_b

c      Set final lines drag
       x35linesd=x35dlcoef*0.5*x35ro*x35vi*x35vi*xlimog(3)

c      Set final pilot drag
       x35pilotd=x35dpcoef*0.5*x35ro*x35vi*x35vi*x35spilot

c      Set default k values (not used in this method)
       x35_k=1.0
       x35_k1=1.0
       x35_k2=1.0

       end if ! method 6

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      17.7 Common for all methods
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Other derived parameters
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Total drag
       x35totald=x35wd+x35dragextra+x35linesd+x35pilotd

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Initial flight speed x35vi m/s used in methods 1,2,3,4
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (i37solve.ne.5.and.i37solve.ne.6) then
       x35vi=dsqrt((xlimog(17)*dcos(x35gamma))/
     + (0.5*x35ro*fparea*x35cl*x35cle*x35_k))
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Solve for moments equilibrium (III)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
       x35_a=-dcos(x35theta)*x35wingmass*x35grav*(x35ycp-wcdgy3d/100.)
     + -(x35zcp-wcdgz3d/100.)*dsin(x35theta)*x35wingmass*x35grav

       x35_b=-dcos(x35theta)*xlimog(2)*x35grav*(x35ycp-xlimog(11)/100.)
     + -(x35zcp-xlimog(12)/100.)*dsin(x35theta)*xlimog(2)*x35grav

       x35_c=-dcos(x35theta)*x35pilotmass*x35grav*x35ycp
     + -(x35zcp-((hcp/100.)+x35pmc*dcos(x35theta)))*dsin(x35theta)*
     + x35pilotmass*x35grav

       x35_d=(x35ycp-x35dly)*x35linesd*dsin(x35alpha)
     + +(x35zcp-x35dlz)*x35linesd*dcos(x35alpha)

       x35_e= x35ycp*x35pilotd*dsin(x35alpha)
     + +(x35zcp-((hcp/100.)+x35pmc*dcos(x35theta)))*x35pilotd*
     + dcos(x35alpha)

c      Calage (m):
       x35py=((x35_a+x35_b+x35_c+x35_d+x35_e)/
     + (x35pilotd*dsin(x35alpha)-x35pilotmass*x35grav*dcos(x35theta)))+
     + x35pmc*dsin(x35theta)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Pilot plumb point
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       x35ypp=x35py+((hcp/100.)+x35pmc)*dtan(x35theta)

c      Induced drag estimation (N)
c      MODELISATION D'UN PARAPENTE - JEROME SARTHE - SEPTEMBRE 1999, page 22
       x35oswald=0.9
       x35xi=x35cl*x35cl*x35cle*x35cle/(pi*x35lambda*x35oswald)  ! Cd induced drag
       x35inducedd=x35xi*0.5*x35ro*x35vi*x35vi*farea*x35_k ! Induced drag (N)
c      WARNING: Induced drag seems excessibe using these formula (!)
c       write (*,*) "Ep... ",pi,x35lambda,x35xi,x35inducedd

c      Pilot+harness mass center (not in main carabiners P)
       x35pmcx=0.0d0
       x35pmcy=x35py-x35pmc*dsin(x35theta)
       x35pmcz=hcp/100.+x35pmc*dcos(x35theta)

c      Quicklinks mass center
       x35qlmcx=0.0d0
       x35qlmcy=0.0d0
       x35qlmcz=0.0d0
       do i=1,cordam
       if (corda(i,2).eq.1) then
       x35qlmcy=x35qlmcy+ycorda(i,2)
       x35qlmcz=x35qlmcz+zcorda(i,2)
c       write (*,*) "Ep...",i,ycorda(i,2),zcorda(i,2)
       end if
       end do
       x35qlmcy=x35qlmcy/(100.*dfloat(nrisers))
       x35qlmcz=x35qlmcz/(100.*dfloat(nrisers))
c       write (*,*) x35qlmcy,x35qlmcz

c      Total mass center (wing + lines+risers + ql + pilot+harness)
       x35totmcx=0.0d0
       x35totmcy=((wcdgy3d/100.)*x35wingmass+  ! wing
     + (xlimog(11)/100.)*xlimog(2)+            ! lines
     + x35qlmcy*xlimog(16)+                    ! ql
     + x35pmcy*x35pilotmass)/xlimog(18)        ! pilot
       x35totmcz=((wcdgz3d/100.)*x35wingmass+  ! wing
     + (xlimog(12)/100.)*xlimog(2)+            ! lines
     + x35qlmcz*xlimog(16)+                    ! ql
     + x35pmcz*x35pilotmass)/xlimog(18)        ! pilot

c      Distance(wingcg(y,z),wingcp(y,z)
       x35distwcp=1000.*dsqrt(((wcdgy3d/100.)-x35ycp)**2.+
     + ((wcdgz3d/100.)-x35zcp)**2.)            ! mm

       end if ! case 35 active


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
