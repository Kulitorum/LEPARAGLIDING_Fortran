c      22. TEXT NOTES
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      22.1 Text in boxes

       xtext=bname
       call txt(-630.*xkf,-100.*xkf,50.0d0,0.0d0,xtext,3)
   
       xtext=wname
       call txt(600.*xkf,-100.*xkf,50.0d0,0.0d0,xtext,1)

       xtext=lepv
       call txt(1400.*xkf,-100.*xkf,50.0d0,0.0d0,xtext,3)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (n1draw.eq.1) then
c      Row -1

       xpos=1260.*2*xkf
       ypos=-(91.+891.)*xkf
       xtext="-1-3 EXTRADOS PANELS WITH 3D-PARTS (PRINTER)"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

       xpos=1260.*4*xkf
       ypos=-(91.+891.)*xkf
       xtext="-1-5 EXTRADOS PANELS WITH 3D-PARTS (LASER)"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

c      Row 0

       xpos=1260.*2*xkf
       ypos=-91.*xkf
       xtext="0-3 INTRADOS PANELS WITH 3D-PARTS (PRINTER)"
       call txt(xpos,ypos,10.0d0,0.0d0,xtext,7)

       xpos=1260.*4*xkf
       ypos=-91.*xkf
       xtext="0-5 INTRADOS PANELS WITH 3D-PARTS (LASER)"
       call txt(xpos,ypos,10.0d0,0.0d0,xtext,7)
       end if ! n1draw
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Row 1

       xpos=0.*xkf
       ypos=800.*xkf
       xtext="1-1 PLANFORM AND VAULT"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)
       xtext=bname
       call txt(xpos,ypos-300.*xkf,10.0d0,0.0d0,xtext,7)
       xtext=wname
       call txt(xpos+190.,ypos-300.*xkf,10.0d0,0.0d0,xtext,7)
       xtext="Flat area (m2) : "
       call txt(xpos,ypos-280.*xkf,10.0d0,0.0d0,xtext,7)
       write (xtext, '(F5.2)') farea
       call txt(xpos+170.,ypos-280.*xkf,10.0d0,0.0d0,xtext,7)
       xtext="Flat span (m) : "
       call txt(xpos,ypos-260.*xkf,10.0d0,0.0d0,xtext,7)
       write (xtext, '(F5.2)') fspan
       call txt(xpos+170.,ypos-260.*xkf,10.0d0,0.0d0,xtext,7)
       xtext="Flat aspect ratio : "
       call txt(xpos,ypos-240.*xkf,10.0d0,0.0d0,xtext,7)
       write (xtext, '(F5.2)') faratio
       call txt(xpos+170.,ypos-240.*xkf,10.0d0,0.0d0,xtext,7)
       xtext="Cells number : "
       call txt(xpos,ypos-220.*xkf,10.0d0,0.0d0,xtext,7)
       write (xtext, '(I3)') nribss*2-1
       if (rib(1,2).le.0.01) then
       write (xtext, '(I3)') nribss*2-2
       end if
       call txt(xpos+170.,ypos-220.*xkf,10.0d0,0.0d0,xtext,7)
       if (iflag2000.eq.1) then
       write (xtext,'(A13,x,A)') "Edition date:", a37date
       call txt(xpos,ypos-200.*xkf,10.0d0,0.0d0,xtext,7)
       end if


       xpos=xpos+1260.*xkf
       xtext="1-2 RIBS"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="1-3 EXTRADOS PANELS"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)
       xtext="Leading edge"
       call txt(xpos,ypos-810.*xkf,10.0d0,0.0d0,xtext,7)
       xtext="Trailing edge"
       call txt(xpos,ypos-160.*xkf,10.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="1-4 RIBS (FOR CUTTING TABLE)"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="1-5 EXTRADOS PANELS (FOR CUTTING TABLE)"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)
       xtext="Leading edge"
       call txt(xpos,ypos-810.*xkf,10.0d0,0.0d0,xtext,7)
       xtext="Trailing edge"
       call txt(xpos,ypos-160.*xkf,10.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="1-6 MIDDLE UNLOADED RIBS"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="1-7 NYLON RODS POCKETS, NOSE MYLARS"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

c      Print nylon-rod blocks in separate columns in BOX(1,7)
       if (k21d.ge.1) then
       do m=1,k21blocs
       xqw=0.0d0
       x0=(350.0d0*float(m-1)-900.0d0)*xkf
       y1=300.0d0*xkf

       write (xtext,'(A25,I2,A1)') "List of nylon rods (bloc ",
     + m,")"

       ng21=k21blocf(m,3)
       call txt(xpos+400.*xkf+x0,ypos+y1+(-700.+xqw)*
     + xkf,xmida1,0.0d0,xtext,7)
       do ng=1,ng21
       xqw=xqw+15.
       write (xtext,'(A5,I3)') "Group ",ng
       call txt(xpos+400*xkf+x0,ypos+y1+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       ngo(ng,2)=ngoo(m,ng,2)
       ngo(ng,3)=ngoo(m,ng,3)
       do i=ngo(ng,2),ngo(ng,3)
       xqw=xqw+15.
       if (k21blocf(m,2).le.2) then
       write (xtext,'(A5,I3,2x,A4,1x,I2,2x,F7.1)') "Jonc ",i,
     + "type",k21blocf(m,2),joncf(i,m,ng,2)
       call txt(xpos+400*xkf+x0,ypos+y1+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       end if
       if (k21blocf(m,2).eq.3) then
       write (xtext,'(A5,I3,x,A4,x,I2,x,A3,x,F7.1,x,A3,x,F7.1)')
     + "Jonc ",i,"type",k21blocf(m,2),"sup",joncf(i,m,ng,2),
     + "inf",joncf(i,m,ng,3)
       call txt(xpos+400*xkf+x0,ypos+y1+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       end if
       if (k21blocf(m,2).eq.4) then
       write (xtext,'(A5,I3,2x,A4,1x,I2,2x,F7.1)') "Jonc ",i,
     + "type",k21blocf(m,2),joncf(i,m,ng,4)
       call txt(xpos+400*xkf+x0,ypos+y1+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       end if
       if (k21blocf(m,2).eq.5) then
       write (xtext,'(A5,I3,2x,A4,1x,I2,2x,F7.1)') "Jonc ",i,
     + "type",k21blocf(m,2),joncf(i,m,ng,5)
       call txt(xpos+400*xkf+x0,ypos+y1+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       end if
       end do ! ngo
       end do ! ng

       end do ! m
       end if

       xpos=xpos+1260.*xkf
       xtext="1-8 MIDDLE AND MIDDLE OVALIZED AIRFOILS"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

c      Row 2

       xpos=0.*xkf
       ypos=ypos+890.95*xkf
       xtext="2-1 CALAGE ESTIMATION"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="2-2 RIBS WASHIN ANGLE"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="2-3 INTRADOS PANELS"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)
       xtext="Trailing edge"
       call txt(xpos,ypos-810.*xkf,10.0d0,0.0d0,xtext,7)
       xtext="Leading edge"
       call txt(xpos,ypos-160.*xkf,10.0d0,0.0d0,xtext,7)


       xpos=xpos+1260.*xkf
       xtext="2-4 MINI-RIBS"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="2-5 INTRADOS PANELS (FOR CUTTING TABLE)"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)
       xtext="Trailing edge"
       call txt(xpos,ypos-810.*xkf,10.0d0,0.0d0,xtext,7)
       xtext="Leading edge"
       call txt(xpos,ypos-160.*xkf,10.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="2-6 FULL DIAGONAL RIBS"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="2-7"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="2-8"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

c      Row 3

       xpos=0.*xkf
       ypos=ypos+890.95*xkf
       xtext="3-1 UPPER VIEW"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="3-2 LINES A"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Write lines in plan A,B,C,D,E,F
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      write lines in plan A
       if (slp.ge.1) then
       xqw=0.
       xtext="Line - Label - Length"
       call txt(xpos-500.*xkf,ypos+(-718.+xqw)*
     + xkf,typm3(9),0.0d0,xtext,7)
       do i=1,slpi(2)-1
       xqw=xqw+18.
       write (xtext,'(I3,3x,A4,3x,F5.1)') i,ln4(i),xlifi(i)
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,typm3(9),0.0d0,xtext,7)
       end do
       end if

       xpos=xpos+1260.*xkf
       xtext="3-3 LINES B"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

c      write lines in plan B
       if (slp.ge.2) then
       if (slp.eq.2) slpi(3)=cordam+1
       xqw=0.
       xtext="Line - Label - Length"
       call txt(xpos-500.*xkf,ypos+(-718.+xqw)*
     + xkf,typm3(9),0.0d0,xtext,7)
       do i=slpi(2),slpi(3)-1
       xqw=xqw+18.
       write (xtext,'(I3,3x,A4,3x,F5.1)') i,ln4(i),xlifi(i)
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,typm3(9),0.0d0,xtext,7)
       end do
       end if

       xpos=xpos+1260.*xkf
       xtext="3-4 LINES C"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

c      write lines in plan C
       if (slp.ge.3) then
       if (slp.eq.3) slpi(4)=cordam+1
       xqw=0.
       xtext="Line - Label - Length"
       call txt(xpos-500.*xkf,ypos+(-718.+xqw)*
     + xkf,typm3(9),0.0d0,xtext,7)
       do i=slpi(3),slpi(4)-1
       xqw=xqw+18.
       write (xtext,'(I3,3x,A4,3x,F5.1)') i,ln4(i),xlifi(i)
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,typm3(9),0.0d0,xtext,7)
       end do
       end if

       xpos=xpos+1260.*xkf
       xtext="3-5 LINES D"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

c      write lines in plan D
       if (slp.ge.4) then
       if (slp.eq.4) slpi(5)=cordam+1
       xqw=0.
       xtext="Line - Label - Length"
       call txt(xpos-500.*xkf,ypos+(-718.+xqw)*
     + xkf,typm3(9),0.0d0,xtext,7)
       do i=slpi(4),slpi(5)-1
       xqw=xqw+18.
       write (xtext,'(I3,3x,A4,3x,F5.1)') i,ln4(i),xlifi(i)
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,typm3(9),0.0d0,xtext,7)
       end do
       end if

c      Box for V-rib Type-6

       xpos=xpos+1260.*xkf
       xtext="3-6 V-rib Type-6"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

c      Box(3,7)

       xpos=xpos+1260.*xkf
       xtext="3-7"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="3-8"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

c      Row 4

       xpos=0.*xkf
       ypos=ypos+890.95*xkf
       xtext="4-1 VAULT VIEW"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="4-2 LATERAL VIEW"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="4-3 BRAKE DISTRIBUTION"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="4-4"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="4-5 BRAKES"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

c      write lines in brakes
       xqw=0.
       xtext="Line - Label - Length"
       call txt(xpos-500.*xkf,ypos+(-718.+xqw)*
     + xkf,typm3(9),0.0d0,xtext,7)
       do i=cordam+1,cordat
       xqw=xqw+18.
       write (xtext,'(I3,3x,A4,3x,F5.1)') i,ln4(i),xlifi(i)
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,typm3(9),0.0d0,xtext,7)
       end do

       xpos=xpos+1260.*xkf
       xtext="4-6"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

       xpos=xpos+1260.*xkf
       xtext="4-7 GENERAL NOTES"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

c      Print general notes BOX(4,7)

c      Notes about plans, first column

       xqw=0.
       xtext="1-1: 111111111111111111111111111111111111111111111"  

       xtext="PLANS GENERAL NOTES"
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida1,0.0d0,xtext,7)
       xqw=xqw+30.

       xtext="1-1: Planform and vault view (informative)"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="1-2: Ribs for plotter, one side"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="1-3: Extrados panels for plotter, one side"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="1-4: Ribs for laser cutting, one side. Units cm"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="1-5: Extrados for laser cutting, one side.Units cm"  
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="1-6: Middle unloaded ribs for laser cutting, "
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="     one side Units cm"  
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="1-7: Rods pockets and nylons lengths, mylars"  
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="1-8: Intermediate and ovalized airfoils"  
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)

       xqw=xqw+30.
       xtext="2-1: Calage estimation, speed and trim systems"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="2-2: Ribs printed with washin angle (informative)"
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="2-3: Intrados panels for plotter, one side"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="2-4: Mini-ribs horizontal and diagonal"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="2-5: Intrados for laser cutting, one side"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="2-6: Full diagonal ribs laser, one side"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="2-7: Free"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)

       xqw=xqw+30.
       xtext="3-1: Upper view 3D (informative)"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="3-2: Lines A"
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="3-3: Lines B"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="3-4: Lines C"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="3-5: Lines D"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="3-6: V-rib type-6"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="3-7: Free"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)

       xqw=xqw+30.
       xtext="4-1: Vault view (informative)"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="4-2: Lateral view (informative)"
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="4-3: Brake distribution (informative)"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="4-4: Free"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="4-5: Brake lines"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="4-6: Free"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext="4-7: General notes"       
       call txt(xpos-500.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)

c      Notes, second column

       xqw=0.
       xtext="1-1: 111111111111111111111111111111111111111111111"    

       xtext="UNITS"
       call txt(xpos-100.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida1,0.0d0,xtext,7)
       xqw=xqw+30.

       xtext="Main units are centimeters. Scale x10 to use in mm"
       call txt(xpos-100.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+30.

       xtext="WIDTHS FOR SEWING AND OFFSETS "
       call txt(xpos-100.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida1,0.0d0,xtext,7)
       xqw=xqw+30.

       write (xtext,'(A32,F6.2)') "Lateral width in extrados (mm): ",
     + xupp
       call txt(xpos-100.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       write (xtext,'(A31,F6.2)') "Width in leading edge ex (mm): ",
     + xupple
       call txt(xpos-100.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       write (xtext,'(A32,F6.2)') "Width in trailing edge ex (mm): ",
     + xuppte
       call txt(xpos-100.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       write (xtext,'(A32,F6.2)') "Lateral width in intrados (mm): ",
     + xlow
       call txt(xpos-100.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       write (xtext,'(A31,F6.2)') "Width in leading edge in (mm): ",
     + xlowle
       call txt(xpos-100.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       write (xtext,'(A32,F6.2)') "Width in trailing edge in (mm): ",
     + xlowte
       call txt(xpos-100.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       write (xtext,'(A28,F6.2)') "Lateral width in ribs (mm): ",
     + xrib
       call txt(xpos-100.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       write (xtext,'(A30,F6.2)') "Lateral width in V-ribs (mm): ",
     + xvrib*10.
       call txt(xpos-100.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.
       
       write(xtext,'(A35,F6.2)') "General offset lateral points (mm): ",
     + typm6(1)
       call txt(xpos-100.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+30.

       write(xtext,'(A28,A14,F6.2)') "Distance between equidistant",
     + " points (cm): ",xmark
       call txt(xpos-100.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       xqw=xqw+15.


c      Notes, third column

       xqw=0.
       xtext="1-1: 111111111111111111111111111111111111111111111"    

       xtext='"ROMAN" NUMBERS CODIFICATION'
       call txt(xpos+300.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida1,0.0d0,xtext,7)
       xqw=xqw+15.
       xtext='Numbering panels, ribs, mini-ribs, V-ribs'
       call txt(xpos+300.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida1,0.0d0,xtext,7)
       xqw=xqw+30.

       do i=1,nribss
       write (xtext,'(A7,I3,A3)') "Number ",i," = "
       call txt(xpos+300.*xkf,ypos+(-700.+xqw)*
     + xkf,xmida2,0.0d0,xtext,7)
       call romano(i,xpos+400.*xkf,ypos+(-700.+xqw)*xkf,0.0d0,3.0d0,7)
       xqw=xqw+15.
       end do

       xpos=xpos+1260.*xkf
       xtext="4-8"
       call txt(xpos,ypos,12.0d0,0.0d0,xtext,7)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
