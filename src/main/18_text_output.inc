!> Write the detailed human-readable design report to `lep-out.txt`.
!!
!! Serializes geometry, area, aspect ratio, profiles, suspension data, panel
!! lengths, HVR definitions, rods, and enabled option summaries after all
!! calculations are complete.  This stage reports state but does not mutate
!! the production geometry.
c     18. TXT OUTPUT lep-out.txt
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     18.1 Main output
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       write (23,'(A31,1x,A6,1x,A7,1x,A50)') 
     + " LABORATORI D'ENVOL PARAGLIDING",lepv,"version",lepc
       write (23,*) "by Pere Casellas 2010-2026"
       write (23,*) "http://www.laboratoridenvol.com"
       write (23,*) "General Public License GNU/GPL 3.0"
c      Customized header
       if (ich.eq.1) then
       write (23,*) 
       write (23,*) "**************************************************"
       write (23,*) "Special version for:"
       write (23,'(1x,A72)') lepuser
       write (23,*) "**************************************************"
       else
       end if
       write (23,*)
       write (23,'(A6,x,A)') "Brand:", bname
       if (atp.eq."pc") then
       write (23,'(A6,x,A)') "Model:", wname
       else
       write (23,'(A6,x,A)') "Model:", wname
       end if
       
       if(iflag2000.eq.1) then
       write (23,'(A13,x,A)') "Edition date:", a37date
       end if

       write (23,'(A6,x,F7.5)') "Scale:", xwf
       if (atp.eq."ds") then
       write (23,'(A,x,A,x,A)') "Wing type is:", atp,"(double surface)"
       end if
       if (atp.eq."sk") then
       write (23,'(A,x,A,x,A)') "Wing type is:", atp,"(single skin)"
       end if
       if (atp.eq."pc") then
       write (23,'(A,x,A,x,A)') "Wing type is:", atp,"(parachute)"
       end if

c      Area

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

c      Span

       fspan=2.*rib(nribss,2)/100.
       pspan=2.*rib(nribss,6)/100.

c      A/R Aspect Ratio

       faratio=fspan*fspan/farea
       paratio=pspan*pspan/parea

       write (23,*)
       write (23,*) "1. MAIN PARAMETERS:"
       write (23,*) "-------------------------------------------------"
       write (23,'(A,1x,F5.2,A,F6.1,A)') "Flat area = ", farea, " m2 ",
     + farea*10.7639," ft2"
       write (23,'(A,1x,F5.2,A,F6.1,A)') "Flat span = ",fspan, " m  ",
     + fspan/0.3048," ft" 
       write (23,'(A,3x,F4.2)') "Flat A/R = ", faratio
       write (23,*)
       write (23,'(A,1x,F5.2,A,F6.1,A)') "Projected area = ", parea, 
     + " m2 ",parea*10.7639," ft2"
       write (23,'(A,1x,F5.2,A,F6.1,A)') "Projected span = ",pspan, 
     + " m  ",pspan/0.3048," ft" 
       write (23,'(A,1x,F4.2)') "Projected A/R =   ", paratio
       write (23,'(A,5x,F5.2)') "Flattening = ", ((farea-parea)/farea)*100.

c      More geometric parameters
       write (23,*)
       write (23,'(A)') "More geometric parameters:"
       varrow=(rib(nribss,7)-rib(1,7))*xwf/100
       write (23,'(A,1x,F5.2,A)') "Vault arrow = ", varrow, " m"
       write (23,'(A,1x,F5.2)') "Proj_span/arrow = ", pspan/varrow
       write (23,'(A,1x,F5.2,A)') "Line heigth (included risers) = ", 
     + (clengr+clengl)/100, " m"
       write (23,'(A,1x,F5.2)') "Proj_span/Line_heigth = ", 
     + pspan*100/(clengl)
       clli=(dsqrt((clengl-rib(nribss,7)*xwf)*(clengl-rib(nribss,7)*
     + xwf)+(pspan*0.50d0*100.0d0)*(pspan*0.5d0*100.0d0)))/100.0d0
       write (23,'(A,2x,F5.2,A)') "Karabiners - wingtip = ", clli, " m"
       write (23,'(A,1x,F5.2)') "Proj_span/(Karabiners - wingtip) = ",
     + pspan/clli

c      Center of gravity
       write (23,*)
       write (23,'(A,F5.2,A)') 
     + "Planform geometric center of gravity at ", cdg, 
     + " % from leading edge"
c      Wing 3d mass center
       write (23,'(A)') "Wing 3d estimated mass center:"
       write (23,'(A3,F7.2,x,A2)') "x= ",wcdgx3d,"cm"
       write (23,'(A3,F7.2,x,A2)') "y= ",wcdgy3d,"cm"
       write (23,'(A3,F7.2,x,A2)') "z= ",wcdgz3d,"cm"

c      Calage properties     

       write (23,*)
       write (23,*) "2. CALAGE PROPERTIES:"
       write (23,*) "-------------------------------------------------"
       write (23,'(A,1x,F5.2)') "Finesse GR  ", planeig
       write (23,'(A,1x,F5.2,x,A3)') "Glide angle ", afinesse,"deg"
       write (23,'(A,9x,F5.2,x,A3)') "AoA ", aoa,"deg"
       write (23,'(A,4x,F5.2,x,A3)') "Assiette ", assiette,"deg"
       write (23,'(A,1x,F6.0,A)') "Total height hcp (inc risers)", 
     + hcpp, " cm"
       write (23,'(A,1x,F6.0,A)') "Risers", clengr, " cm"
       write (23,'(A,1x,F6.2,x,A,x,F6.2,x,A2)') 
     + "calage, pilot centering ", calage,"%", calag,"cm"
       write (23,'(A,1x,F6.2,x,A,x,F6.2,x,A2)') 
     + "plumb point (estimation)", cpress,"%", cple,"cm"
       write (23,'(A,3(2x,F7.2))') "karabiners (x,y,z)    ",
     + xkar,ykar,zkar
       write (23,*)

c      Rib properties

       write (23,*) "3. RIB PROPERTIES:"
       write (23,*) "-------------------------------------------------"
       write (23,*) "Ribs number  = ", nribss*2
       write (23,*) "Cells number = ", nribss*2-1
       if (rib(1,2).lt.0.01) then
       write (23,*) "Zero-thickness central cell"
       end if
       write (23,*)

       write (23,*) "Rib - Chord - washin - beta - Rot_z"
       write (23,*) "-------------------------------------------------"

       do i=1,nribss

       write (23,'(I2,5x,F6.2,2x,F6.3,2x,F5.2,2x,F5.2)') i,rib(i,5), 
     + rib(i,8),rib(i,9),rib(i,250)

       end do

c      Cell properties

       write(23,*)

       write (23,*) "Cell   width (cm)"
      
       write (23,'(I2,7x,F5.2)') 0, 2.*rib(1,2)
       
       do i=1,nribss-1

       write (23,'(I2,5x,F5.2)') i, rib(i+1,2)-rib(i,2)

       end do

c      Anchor points

c      Lines

       xlength=0.

       write (23,*)
       write (23,*) "4. LINE MATRIX, LINE LENGTHS,AND LINE LOADS:"
       write (23,*) "Line-plan- level- order- ap  - r - row - rib -  L  
     +  -   length -length_e - load (Kg) - Type"
       write (23,*) "------------------------------------------------",
     + "----------------------------------------------"

       do i=1,cordat

       write (23,'(8(I3,3x),A,3x,2(F6.2,3x),F7.2,7x,I2)') i,corda(i,1),
     + corda(i,2),corda(i,3),corda(i,4),corda(i,5),corda(i,6),
     + corda(i,7)," L = ",xline(i), xlifi(i), xload(i), 
     + corda(i,30+corda(i,2))

c      Exclude risers and loops
       if (corda(i,2).ne.1) then
       xlength=xlength+xline(i)
       end if
       end do

c      Include brakes
       xlength=xlength+xline(cordam+1)

c      Note: xlength no longuer used. Calculus in section 14.8

       write (23,*)
       write (23,'(A,A,x,F8.2,x,A)') "Total line lengths ",
     + "(without loops) = ", xlength2,"m"
       write (23,'(A,A,x,F8.2,x,A)') "Total line lengths ",
     + "(with loops) = ", xlength1,"m"
       write (23,*) "(loops lengths according section 34 ",
     + "or internal predefined values)"
       write (23,*)

c      Print lengths by types
       write (23,*) "Minimal lengths by types, including loops:"
       do k=1,k34n
       if (lentype(k).ge.1.0d0) then
       write (23,'(A5,x,I2,x,F7.2,x,A,x,A15,x,A15,x,F6.2,x,A2,x,A4,x,
     + F6.2,x,A2)')  "Type ",jltype(k),lentype(k)/50.,"m",
     + tylinea(k,4),tylinea(k,6),tyline(k,2),"mm",
     + "loop",tyline(k,9),"cm"
       end if
       end do

c      Additional detailed lines properties
       write (23,*)
       write (23,*) "Additional detailed lines properties:"
       write (23,*)
       write (23,*) "Line - Lenght - LenghtL - Weight(g) - Area(mm2)",
     + " -  x   -   y   -   z" 
       do i=1,cordat
       write (23,'(I3,7(x,F9.2))') 
     + i, xlimo(i,0), xlimo(i,1), xlimo(i,2), xlimo(i,3), 
     + xlimo(i,10),xlimo(i,11),xlimo(i,12)
       end do

       write (23,*)
       write (23,'(A,x,F8.3,x,A2)') 
     + "Total lines weight included risers:",xlimog(2),"kg"
       write (23,'(A,x,F8.3,x,A2)') 
     + "Total lines area included risers:",xlimog(3),"m2"
       write (23,'(A,x,F8.3,x,A2)') 
     + "Lines x-coordinate mass center:",xlimog(10),"cm"
       write (23,'(A,x,F8.3,x,A2)') 
     + "Lines y-coordinate mass center:",xlimog(11),"cm"
       write (23,'(A,x,F8.3,x,A2)') 
     + "Lines z-coordinate mass center:",xlimog(12),"cm"

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     18.2 ADJUSTEMENT SEAM PARAMETERS
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  
c      Ajust de costura

       write (23,*)
       write (23,*) "5. Adjustment seam parameters on the inner side" 
       write (23,*) "of extrados panels (fig. 10 of the manual)"
       write (23,*) "-------------------------------------------------"

       if (ndif.eq.1000) then
       write (23,*) "Not used ( ndif=1000 )"
       else
       write (23,*) "Points number= ", ndif, "  scale= ",xndif
       do i=1,nribss-1
       write (23,*) "Rib= ", i, " dif= ", rib(i,81)
       end do
       end if

       write (23,*)

c      Panels and rib differences report

       write (23,*)
       write (23,*) "6. VERIFICATION Panels and rib differences (mm):"
       write (23,*) "-------------------------------------------------"
       write (23,*) "NOTE: Maximal accuracy ",
     + "<< 0.02 mm in extrados and intrados panels"
       write (23,*)
       write (23,*) "Number - Panel at left - Rib - Panel at right -", 
     + " max dif (mm)"

       if (atp.eq."ss") then
       do i=1,nribss-1
       xmax=max(abs(rib(i,30)-rib(i,31)),abs(rib(i,32)-rib(i,31)))
       write(23,'(I2,3x,F10.2,F10.2,F10.2,F10.2,F10.2)') 
     + i,10.*rib(i,30)+10.*rib(i,26),10.*rib(i,31)+10.*rib(i,26),
     + 10.*rib(i,32)+10.*rib(i,26),10.*xmax
       end do
       end if

       if (atp.ne."ss") then

c      Ribs 1 to nribss-1
       do i=1,nribss-1
       xmaxe=max(abs(rib(i,30)-rib(i,31)),abs(rib(i,32)-rib(i,31)))
       xmaxi=max(abs(rib(i,33)-rib(i,34)),abs(rib(i,35)-rib(i,34)))
       write (23,*)
       write (23,'(I2,2x,F10.2,F10.2,F10.2,F10.3,A6)') 
     + i,10*rib(i,30),10*rib(i,31),10*rib(i,32),10.*xmaxe," extra"
       write (23,'(I2,2x,F10.2,F10.2,F10.2,F10.3,A6)') 
     + i,10*rib(i,33),10*rib(i,34),10*rib(i,35),10.*xmaxi," intra"
       end do

c      Last rib
       i=nribss
       xmaxe=abs(rib(i,30)-rib(i,31))
       xmaxi=abs(rib(i,33)-rib(i,34))
       write (23,*)
       write (23,'(I2,2x,F10.2,F10.2,A10,F10.3,A6)') 
     + i,10*rib(i,30),10*rib(i,31),"   -    ",10.*xmaxe," extra"
       write (23,'(I2,2x,F10.2,F10.2,A10,F10.3,A6)') 
     + i,10*rib(i,33),10*rib(i,34),"   -    ",10.*xmaxi," intra"

       end if

       write (23,*)

c      Print distorsions

       write (23,*) "Panel distorsions in the leading edge (mm):"
       write (23,*) "-------------------------------------------"
       write (23,*) "Panel   D. extra   D. intra"
       do i=0,nribss-1
       write (23,'(I3,2x,F10.4,F10.4)')
     + i,(rib(i,97)-rib(i,96))*10.,(rib(i,99)-rib(i,98))*10.
       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      18.3 Speed system amb trim calculus
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (k28d.eq.1) then
       
       write(23,*)
       write(23,*) "7. CALAGE AND RISERS VARIATIONS WITH ANGLE"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      18.3.1 a) Speed system pivot in last riser
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       a1281=a128/dfloat(n128)

       do j=1,n128+1 ! itera en angles

       alpha28(j)=a1281*dfloat(j-1) 

c      Initial and final lengths
       
       do k=1,nriser28

       x1=p28(k)*rib(1,5)/100.0d0
       y1=0.0d0
       x2=calage*rib(1,5)/100.0d0
       y2=clengl+clengr
       lini28(j,k)=dsqrt(((x2-x1)*(x2-x1))+((y2-y1)*(y2-y1)))

       hhh=(p28(nriser28)-p28(k))*rib(1,5)/100.0d0
       xxx=hhh*dcos(alpha28(j)*pi/180.0d0)
       yyy=hhh*dsin(alpha28(j)*pi/180.0d0)
       x3=x1+hhh-xxx
       y3=yyy
       lfin28(j,k)=dsqrt(((x2-x3)*(x2-x3))+((y2-y3)*(y2-y3)))

       end do

c      New calage, use values k=nriser28

       xru(1)=p28(nriser28)*rib(1,5)/100.0d0
       xrv(1)=0.0d0
       hhh=(p28(nriser28)-p28(1))*rib(1,5)/100.0d0
       xxx=hhh*dcos(alpha28(j)*pi/180.0d0)
       yyy=hhh*dsin(alpha28(j)*pi/180.0d0)
       xru(2)=(p28(1)*rib(1,5)/100.0d0)+hhh-xxx
       xrv(2)=-yyy

       xsu(1)=calage*rib(1,5)/100.0d0
       xsv(1)=clengl+clengr
       xsu(2)=xsu(1)+100.0d0*dtan(alpha28(j)*pi/180.0d0)
       xsv(2)=xsv(1)-100.0d0

       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)

       if (alpha28(j).eq.0.0d0) then
       xtu=calage*rib(1,5)/100.0d0
       xtv=0.0d0
       end if
       
       p28max=(p28(nriser28))*rib(i,5)/100.0d0
       xru(3)=p28max*(1.0d0-dcos(alpha28(j)*pi/180.0d0))
       xrv(3)=-p28max*sin(alpha28(j)*pi/180.0d0)

       calagnew(j)=(dsqrt(((xtu-xru(3))*(xtu-xru(3)))+
     + ((xtv-xrv(3))*(xtv-xrv(3)))))*100.0d0/rib(1,5)

       cnewtps(j)=calagnew(j)
       cnewcms(j)=lini28(j,1)-lfin28(j,1)

       end do

c      Print case 2 risers
       if (nriser28.eq.2) then
       write(23,*)
       write(23,*) "a) Speed system pivot in last riser:"
       write (23,*) "-------------------------------------------"
       write(23,'(A2,2x,A6,2x,A6,2x,A6)') "i","alpha",
     + "A","B","Calage"
       do j=1,n128+1
       write(*,'(I2,2x,F6.2,2x,F6.2,2x,F6.2,2x,A6)') 
     + j,alpha28(j),lini28(j,1)-lfin28(j,1),
     + lini28(j,2)-lfin28(j,2), calagnew(j)
       end do
       end if

c      Print case 3 risers
       if (nriser28.eq.3) then
       write(23,*)
       write(23,*) "a) Speed system pivot in last riser:"
       write (23,*) "-------------------------------------------"
       write(23,'(A2,2x,A6,2x,A6,2x,A6,2x,A6,2x,A6)') "i","alpha",
     + "A","B","C","Calage"
       do j=1,n128+1
       write(23,'(I2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,A6)') 
     + j,alpha28(j),lini28(j,1)-lfin28(j,1),
     + lini28(j,2)-lfin28(j,2),lini28(j,3)-lfin28(j,3), calagnew(j)
       end do
       end if

c      Print case 4 risers
       if (nriser28.eq.4) then
       write(23,*)
       write(23,*) "a) Speed system pivot in last riser:"
       write (23,*) "-------------------------------------------"
       write(23,'(A2,2x,A6,2x,A6,2x,A6,2x,A6,2x,A6,2x,A6)') "i","alpha",
     + "A","B","C","D","Calage"
       do j=1,n128+1
       write(23,'(I2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F5.2,2x,
     + A6)') j,alpha28(j),lini28(j,1)-lfin28(j,1),lini28(j,2)-lfin28
     + (j,2),lini28(j,3)-lfin28(j,3),lini28(j,4)-lfin28(j,4),calagnew(j)
       end do
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      18.3.2 b) Speed system pivot in first riser
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       a1281=a128/dfloat(n128)

       do j=1,n128+1 ! itera en angles

       alpha28(j)=a1281*dfloat(j-1) 

c      Initial and final lengths
       
       do k=1,nriser28

       x1=p28(k)*rib(1,5)/100.0d0
       y1=0.0d0
       x2=calage*rib(1,5)/100.0d0
       y2=clengl+clengr
       lini28(j,k)=dsqrt(((x2-x1)*(x2-x1))+((y2-y1)*(y2-y1)))

       hhh=(p28(k)-p28(1))*rib(1,5)/100.0d0
       xxx=hhh*dcos(alpha28(j)*pi/180.0d0)
       yyy=hhh*dsin(alpha28(j)*pi/180.0d0)
       x3=(p28(1)*rib(1,5)/100.0d0)+xxx
       y3=yyy
       lfin28(j,k)=dsqrt(((x2-x3)*(x2-x3))+((y2-y3)*(y2-y3)))

       end do

c      New calage, use values k=nriser28

       xru(1)=p28(1)*rib(1,5)/100.0d0
       xrv(1)=0.0d0
       hhh=(p28(nriser28)-p28(1))*rib(1,5)/100.0d0
       xxx=hhh*dcos(alpha28(j)*pi/180.0d0)
       yyy=hhh*dsin(alpha28(j)*pi/180.0d0)
       xru(2)=xru(1)+xxx
       xrv(2)=yyy

       xsu(1)=calage*rib(1,5)/100.0d0
       xsv(1)=clengl+clengr
       xsu(2)=xsu(1)+100.0d0*dtan(alpha28(j)*pi/180.0d0)
       xsv(2)=xsv(1)-100.0d0

       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)

       if (alpha28(j).eq.0.0d0) then
       xtu=calage*rib(1,5)/100.0d0
       xtv=0.0d0
       end if

       xru(3)=xru(1)*(1.0d0-dcos(alpha28(j)*pi/180.0d0))
       xrv(3)=-xru(1)*dsin(alpha28(j)*pi/180.0d0)

       calagnew(j)=(dsqrt(((xtu-xru(3))*(xtu-xru(3)))+
     + ((xtv-xrv(3))*(xtv-xrv(3)))))*100.0d0/rib(1,5)

       end do

c      Print case 2 risers
       if (nriser28.eq.2) then
       write(23,*)
       write(23,*) "b) Speed system pivot in first riser:"
       write (23,*) "-------------------------------------------"
       write(23,'(A2,2x,A6,2x,A6,2x,A6)') "i","alpha",
     + "A","B","Calage"
       do j=1,n128+1
       write(*,'(I2,2x,F6.2,2x,F6.2,2x,F6.2,2x,A6)') 
     + j,alpha28(j),lfin28(j,1)-lini28(j,1),
     + lfin28(j,2)-lini28(j,2), calagnew(j)
       end do
       end if

c      Print case 3 risers
       if (nriser28.eq.3) then
       write(23,*)
       write(23,*) "b) Speed system pivot in first riser:"
       write (23,*) "-------------------------------------------"
       write(23,'(A2,2x,A6,2x,A6,2x,A6,2x,A6,2x,A6)') "i","alpha",
     + "A","B","C","Calage"
       do j=1,n128+1
       write(23,'(I2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,A6)') 
     + j,alpha28(j),lfin28(j,1)-lini28(j,1),
     + lfin28(j,2)-lini28(j,2),lfin28(j,3)-lini28(j,3), calagnew(j)
       end do
       end if

c      Print case 4 risers
       if (nriser28.eq.4) then
       write(23,*)
       write(23,*) "b) Speed system pivot in first riser:"
       write (23,*) "-------------------------------------------"
       write(23,'(A2,2x,A6,2x,A6,2x,A6,2x,A6,2x,A6,2x,A6)') "i","alpha",
     + "A","B","C","D","Calage"
       do j=1,n128+1
       write(23,'(I2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,
     + A6)') j,alpha28(j),lfin28(j,1)-lini28(j,1),lfin28(j,2)-lini28
     + (j,2),lfin28(j,3)-lini28(j,3),lfin28(j,4)-lini28(j,4),calagnew(j)
       end do
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      18.3.3 c) Trimer system pivot in first riser
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       a2282=a228/dfloat(n228)

       do j=1,n228+1 ! itera en angles

       alpha28(j)=a2282*dfloat(j-1) 

c      Initial and final lengths
       
       do k=1,nriser28

       x1=p28(k)*rib(1,5)/100.0d0
       y1=0.0d0
       x2=calage*rib(1,5)/100.0d0
       y2=clengl+clengr
       lini28(j,k)=dsqrt(((x2-x1)*(x2-x1))+((y2-y1)*(y2-y1)))

       hhh=(p28(k)-p28(1))*rib(1,5)/100.0d0
       xxx=hhh*dcos(alpha28(j)*pi/180.0d0)
       yyy=hhh*dsin(alpha28(j)*pi/180.0d0)
       x3=(p28(1)*rib(1,5)/100.0d0)+xxx
       y3=yyy
       lfin28(j,k)=dsqrt(((x2-x3)*(x2-x3))+((y2-y3)*(y2-y3)))

       end do

c      New calage, use values k=nriser28

       xru(1)=p28(1)*rib(1,5)/100.0d0
       xrv(1)=0.0d0
       hhh=(p28(nriser28)-p28(1))*rib(1,5)/100.0d0
       xxx=hhh*dcos(alpha28(j)*pi/180.0d0)
       yyy=hhh*dsin(alpha28(j)*pi/180.0d0)
       xru(2)=xru(1)+xxx
       xrv(2)=yyy

       xsu(1)=calage*rib(1,5)/100.0d0
       xsv(1)=clengl+clengr
       xsu(2)=xsu(1)+100.0d0*dtan(alpha28(j)*pi/180.0d0)
       xsv(2)=xsv(1)-100.0d0

       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)

       if (alpha28(j).eq.0.0d0) then
       xtu=calage*rib(1,5)/100.0d0
       xtv=0.0d0
       end if

       xru(3)=xru(1)*(1.0d0-dcos(alpha28(j)*pi/180.0d0))
       xrv(3)=-xru(1)*dsin(alpha28(j)*pi/180.0d0)

       calagnew(j)=(dsqrt(((xtu-xru(3))*(xtu-xru(3)))+
     + ((xtv-xrv(3))*(xtv-xrv(3)))))*100.0d0/rib(1,5)

       cnewtpt(j)=calagnew(j)
       cnewcmt(j)=lini28(j,nriser28)-lfin28(j,nriser28)

       end do

c      Print case 2 risers
       if (nriser28.eq.2) then
       write(23,*)
       write(23,*) "c) Trimer system pivot in first riser:"
       write (23,*) "-------------------------------------------"
       write(23,'(A2,2x,A6,2x,A6,2x,A6)') "i","alpha",
     + "A","B","Calage"
       do j=1,n228+1
       write(*,'(I2,2x,F6.2,2x,F6.2,2x,F6.2,2x,A6)') 
     + j,alpha28(j),lfin28(j,1)-lini28(j,1),
     + lfin28(j,2)-lini28(j,2), calagnew(j)
       end do
       end if

c      Print case 3 risers
       if (nriser28.eq.3) then
       write(23,*)
       write(23,*) "c) Trimer system pivot in first riser:"
       write (23,*) "-------------------------------------------"
       write(23,'(A2,2x,A6,2x,A6,2x,A6,2x,A6,2x,A6)') "i","alpha",
     + "A","B","C","Calage"
       do j=1,n228+1
       write(23,'(I2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,A6)') 
     + j,alpha28(j),lfin28(j,1)-lini28(j,1),
     + lfin28(j,2)-lini28(j,2),lfin28(j,3)-lini28(j,3), calagnew(j)
       end do
       end if

c      Print case 4 risers
       if (nriser28.eq.4) then
       write(23,*)
       write(23,*) "c) Trimer system pivot in first riser:"
       write (23,*) "-------------------------------------------"
       write(23,'(A2,2x,A6,2x,A6,2x,A6,2x,A6,2x,A6,2x,A6)') "i","alpha",
     + "A","B","C","D","Calage"
       do j=1,n228+1
       write(23,'(I2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,
     + A6)') j,alpha28(j),lfin28(j,1)-lini28(j,1),lfin28(j,2)-lini28
     + (j,2),lfin28(j,3)-lini28(j,3),lfin28(j,4)-lini28(j,4),calagnew(j)
       end do
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      18.3.4 d) Trimer system pivot in last riser
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

    
       a2282=a228/dfloat(n228)

       do j=1,n228+1 ! itera en angles

       alpha28(j)=a2282*dfloat(j-1) 

c      Initial and final lengths
       
       do k=1,nriser28

       x1=p28(k)*rib(1,5)/100.0d0
       y1=0.0d0
       x2=calage*rib(1,5)/100.0d0
       y2=clengl+clengr
       lini28(j,k)=dsqrt(((x2-x1)*(x2-x1))+((y2-y1)*(y2-y1)))

       hhh=(p28(nriser28)-p28(k))*rib(1,5)/100.0d0
       xxx=hhh*dcos(alpha28(j)*pi/180.0d0)
       yyy=hhh*dsin(alpha28(j)*pi/180.0d0)
       x3=x1+hhh-xxx
       y3=yyy
       lfin28(j,k)=dsqrt(((x2-x3)*(x2-x3))+((y2-y3)*(y2-y3)))

       end do

c      New calage, use values k=nriser28

       xru(1)=p28(nriser28)*rib(1,5)/100.0d0
       xrv(1)=0.0d0
       hhh=(p28(nriser28)-p28(1))*rib(1,5)/100.0d0
       xxx=hhh*dcos(alpha28(j)*pi/180.0d0)
       yyy=hhh*dsin(alpha28(j)*pi/180.0d0)
       xru(2)=(p28(1)*rib(1,5)/100.0d0)+hhh-xxx
       xrv(2)=-yyy

       xsu(1)=calage*rib(1,5)/100.0d0
       xsv(1)=clengl+clengr
       xsu(2)=xsu(1)+100.0d0*dtan(alpha28(j)*pi/180.0d0)
       xsv(2)=xsv(1)-100.0d0

       call xrxs(xru,xrv,xsu,xsv,xtu,xtv)

       if (alpha28(j).eq.0.0d0) then
       xtu=calage*rib(1,5)/100.0d0
       xtv=0.0d0
       end if

       p28max=(p28(nriser28))*rib(i,5)/100.0d0
       xru(3)=p28max*(1.0d0-dcos(alpha28(j)*pi/180.0d0))
       xrv(3)=-p28max*dsin(alpha28(j)*pi/180.0d0)

       calagnew(j)=(dsqrt(((xtu-xru(3))*(xtu-xru(3)))+
     + ((xtv-xrv(3))*(xtv-xrv(3)))))*100.0d0/rib(1,5)

       end do

c      Print case 2 risers
       if (nriser28.eq.2) then
       write(23,*)
       write(23,*) "d) Trimer system pivost in last riser:"
       write (23,*) "-------------------------------------------"
       write(23,'(A2,2x,A6,2x,A6,2x,A6)') "i","alpha",
     + "A","B","Calage"
       do j=1,n228+1
       write(*,'(I2,2x,F6.2,2x,F6.2,2x,F6.2,2x,A6)') 
     + j,alpha28(j),lini28(j,1)-lfin28(j,1),
     + lini28(j,2)-lfin28(j,2), calagnew(j)
       end do
       end if

c      Print case 3 risers
       if (nriser28.eq.3) then
       write(23,*)
       write(23,*) "d) Trimer system pivot in last riser:"
       write (23,*) "-------------------------------------------"
       write(23,'(A2,2x,A6,2x,A6,2x,A6,2x,A6,2x,A6)') "i","alpha",
     + "A","B","C","Calage"
       do j=1,n228+1
       write(23,'(I2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,A6)') 
     + j,alpha28(j),lini28(j,1)-lfin28(j,1),
     + lini28(j,2)-lfin28(j,2),lini28(j,3)-lfin28(j,3), calagnew(j)
       end do
       end if

c      Print case 4 risers
       if (nriser28.eq.4) then
       write(23,*)
       write(23,*) "d) Trimer system pivot in last riser:"
       write (23,*) "-------------------------------------------"
       write(23,'(A2,2x,A6,2x,A6,2x,A6,2x,A6,2x,A6,2x,A6)') "i","alpha",
     + "A","B","C","D","Calage"
       do j=1,n228+1
       write(23,'(I2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,F6.2,2x,
     + A6)') j,alpha28(j),lini28(j,1)-lfin28(j,1),lini28(j,2)-lfin28
     + (j,2),lini28(j,3)-lfin28(j,3),lini28(j,4)-lfin28(j,4),calagnew(j)
       end do
       end if






cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      18.3.5 Draw calage variations
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c     Box (2,1)

       x0=0.-400.0*xkf
       y0=900.*xkf+600.*xkf

c      Draw box

       xakk=2.*xkf
       yakk=2.*xkf
       xa=100.*xakk
       ya=100.*yakk

       call line(x0,y0,x0+xa,y0,7)
       call line(x0,y0,x0,y0-ya,7)
       call line(x0+xa,y0,x0+xa,y0-ya,7)
       call line(x0,y0-ya,x0+xa,y0-ya,7)

       do j=1,9
       call line(x0+10.*float(j)*xakk,y0,x0+10.*float(j)*xakk,y0-ya,8)
       end do

       do j=1,19
       call line(x0,y0-5.*float(j)*yakk,x0+xa,y0-5.*float(j)*yakk,9)
       end do

       xtext="Calage %"
       call txt(x0+xa/2.0d0-10.0d0,y0+25.0d0,8.0d0,0.0d0,xtext,7)
       xtext="0 %"
       call txt(x0-10.,y0+12.,8.0d0,0.0d0,xtext,7)
       write (xtext,'(F5.2)') calage
       call txt(x0+calage*xakk-10.,y0+12.,8.0d0,0.0d0,xtext,7)
       xtext="100 %"
       call txt(x0+xa,y0+12.,8.0d0,0.0d0,xtext,7)

       xtext="Speed system (cm)"
       call txt(x0-20.,y0-20,8.0d0,90.0d0,xtext,1)
       xtext="0"
       call txt(x0-10.,y0,8.0d0,0.0d0,xtext,1)
       xtext="20"
       call txt(x0-10.,y0-ya,8.0d0,0.0d0,xtext,1)

       xtext="Trimer system (cm)"
       call txt(x0+xa+20.,y0-20,8.0d0,90.0d0,xtext,5)
       xtext="0"
       call txt(x0+xa+10.,y0,8.0d0,0.0d0,xtext,5)
       xtext="20"
       call txt(x0+xa+10.,y0-ya,8.0d0,0.0d0,xtext,5)

       do j=1,n128

       call line(x0+cnewtps(j)*xakk,y0+cnewcms(j)*5.*yakk,
     + x0+cnewtps(j+1)*xakk,y0+cnewcms(j+1)*5.*yakk,1)

       end do

       do j=1,n228

       call line(x0+cnewtpt(j)*xakk,y0-cnewcmt(j)*5.*yakk,
     + x0+cnewtpt(j+1)*xakk,y0-cnewcmt(j+1)*5.*yakk,5)

       end do

       end if ! Case k28p=1


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      18.4 Print joncs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (k21d.ge.1) then
       x1=0.
       write (23,*)
       write (23,*) "8. NYLON RODS (JONCS)"
       write (23,*) "-------------------------------------------"
       do m=1,k21blocs
       write (23,'(A5,I3,A8,I2)') "BLOC ",m,"   TYPE ",k21blocf(m,2) 
       write (23,*) "-------------------------------------------"
       do ng=1,k21blocf(m,3)
       write (23,'(A5,I3)') "Group",ng
       write (23,*) "----------------------------"
       do i=ngoo(m,ng,2),ngoo(m,ng,3)
c      Case jonc type 1 or 2
       if (k21blocf(m,2).le.2) then
       x1=x1+joncf(i,m,ng,2)
       write (23,'(A5,I3,2x,A4,1x,I2,2x,F7.1)') "Jonc ",i,
     + "type",k21blocf(m,2),joncf(i,m,ng,2)
       end if
c      Case jonc type 3 (shark nose)
       if (k21blocf(m,2).eq.3) then
       x1=x1+joncf(i,m,ng,2)+joncf(i,m,ng,3)
       write (23,'(A5,I3,x,A4,x,I2,x,A3,x,F7.1,x,A3,x,F7.1)')
     + "Jonc ",i,"type",k21blocf(m,2),"sup",joncf(i,m,ng,2),
     + "inf",joncf(i,m,ng,3)
       end if
c      Case jonc type 4 (long extrados rod)
       if (k21blocf(m,2).eq.4) then
       x1=x1+joncf(i,m,ng,4)
       write (23,'(A5,I3,2x,A4,1x,I2,2x,F7.1)') "Jonc ",i,
     + "type",k21blocf(m,2),joncf(i,m,ng,4)
       end if
c      Case jonc type 5 (long intrados rod)
       if (k21blocf(m,2).eq.5) then
       x1=x1+joncf(i,m,ng,5)
       write (23,'(A5,I3,2x,A4,1x,I2,2x,F7.1)') "Jonc ",i,
     + "type",k21blocf(m,2),joncf(i,m,ng,5)
       end if
       end do
       write (23,*) "----------------------------"

       end do
       end do
       write (23,'(A,F8.2)') "Joncs total length m (both sides) ",
     + x1*2./100.
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      18.5 Print 3D-shaping details
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

 
c      Case k29d=0     
       if (k29d.eq.0) then
       write (23,*)
       write (23,*) "9. 3D-SHAPING DETAILS"
       write (23,*) "-------------------------------------------"
       write (23,*) 
       end if

c      Case k29d=1
       if (k29d.eq.1) then
       write (23,*)
       write (23,*) "9. 3D-SHAPING DETAILS"
       write (23,*) "-------------------------------------------"
       write (23,*) 

       ng=rib(i,169)

       do i=1,nribss

       if (uppcuts(ng).ge.1.or.lowcuts(ng).ge.1) then
       write (23,*) "-------------------------------------"
       write (23,*) "zone   rib      d1       d2    d2-d1 "
       write (23,*) "-------------------------------------"
       end if

       if (uppcuts(ng).eq.1) then
       write (23,'(A4,3x,I3,3x,F6.2,3x,F6.2,3x,F6.2)') 
     + "z1 ",i,zinf(i,1,1),zinf(i,2,1),zinf(i,5,1)
       write (23,'(A4,3x,I3,3x,F6.2,3x,F6.2,3x,F6.2)') 
     + "z3 ",i,zinf(i,1,3),zinf(i,2,3),zinf(i,5,3)
       write (23,'(A4,3x,I3,3x,F6.2)') "f13 ",i,zinf(i,6,1)
       end if

       if (uppcuts(ng).eq.2) then
       write (23,'(A4,3x,I3,3x,F6.2,3x,F6.2,3x,F6.2)') 
     + "z1 ",i,zinf(i,1,1),zinf(i,2,1),zinf(i,5,1)
       write (23,'(A4,3x,I3,3x,F6.2,3x,F6.2,3x,F6.2)') 
     + "z2 ",i,zinf(i,1,2),zinf(i,2,2),zinf(i,5,2)
       write (23,'(A4,3x,I3,3x,F6.2,3x,F6.2,3x,F6.2)') 
     + "z3 ",i,zinf(i,1,3),zinf(i,2,3),zinf(i,5,3)
       write (23,'(A4,3x,I3,3x,F6.2)') "f12 ",i,zinf(i,6,1)
       write (23,'(A4,3x,I3,3x,F6.2)') "f23 ",i,zinf(i,6,2)
       end if

       if (uppcuts(ng).ge.1) then
       write (23,'(A4,3x,I3,3x,F6.2,3x,F6.2,3x,F6.2)') 
     + "z4 ",i,zinf(i,1,4),zinf(i,2,4),zinf(i,5,4)
       end if

       if (lowcuts(ng).eq.1) then
       write (23,'(A4,3x,I3,3x,F6.2,3x,F6.2,3x,F6.2)') 
     + "z5 ",i,zinf(i,1,5),zinf(i,2,5),zinf(i,5,5)
       write (23,'(A4,3x,I3,3x,F6.2,3x,F6.2,3x,F6.2)') 
     + "z6 ",i,zinf(i,1,6),zinf(i,2,6),zinf(i,5,6)
       write (23,'(A4,3x,I3,3x,F6.2)') "f56 ",i,zinf(i,6,4)
       end if
       end do

       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      18.6 Print informative extrados and intrados coeeficents
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
       write (23,*)
       write (23,*) "10. INFORMATIVE EXTRADOS AND INTRADOS COEFFICIENTS"
       write (23,*) "-------------------------------------------"
c       write (23,*) 

       write (23,*) "Rib             panel/rib (left) panel/rib (right)"
       do i=0,nribss-1
       write (23,'(I3,3x,A11,5x,F7.5,9x,F7.5)') i,"extrados ",
     + rib(i,194),rib(i,195)
       end do
       write (23,*) 
       do i=0,nribss-1
       write (23,'(I3,3x,A11,5x,F7.5,9x,F7.5)') i,"intrados ",
     + rib(i,200),rib(i,201)
       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      18.7 Print information about profil points
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
       write (23,*)
       write (23,*) "11. INFORMATION ABOUT AIRFOIL POINTS"
       write (23,*) "-------------------------------------------"
c       write (23,*) 

       write (23,*) "Rib  Total Extra Vents Intra"
       do i=0,nribss
       write (23,'(I3,3x,I3,3x,I3,3x,I3,3x,I3)') i,np(i,1),np(i,2),
     + np(i,3),np(i,4)
       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      18.8 Print angles between airfoil plane and glidepath
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       write (23,*)
       write (23,*) "12. ANGLES BETWEEN AIRFOIL PLANE AND GLIDEPATH LINE
     +  (phi) and local AoA (chi)"
       write (23,*) "-------------------------------------------------",
     + "---------------------------"
c       write (23,*) 
       write (23,*) "Informative angles. Change rotation values"
       write (23,*) "in column 10 of SECTION 1, using phi to improve the 
     + results"
       write (23,*) "phi=angle between airfoil plane and glidepath line"
       write (23,*) 'chi=angle between chord line and glidepath line (lo
     +cal AoA)'
       write (23,*)
       write (23,'(A,F6.2)') "glide angle (deg) = ",afinesse
       write (23,'(A,F6.2)') "central AoA (deg) = ",aoa
       write (23,'(A,F6.2)') "assiette (deg) = ",assiette
       if (iflag_assiette.eq.1) then
       write (23,'(A,F6.2)') "forced assiette (deg) = ",assiette2003
       end if
       write (23,*)

       write (23,*) "Rib    phi      chi"
       do i=1,nribss
       write (23,'(I3,3x,F6.2,3x,F6.2)') i,-phii(i)*180.0d0/pi,
     + chii(i)*180.0d0/pi
       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      18.9 Risers information
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       write (23,*)
       write (23,*) "14. RISERS INFORMATION"
       write (23,*) "-------------------------------------------------",
     + "---------------------------"

c      Case 0 Standard risers, equal length
       if (k33d.eq.0) then
       write (23,*) "Standard risers, equal length"
       write (23,'(A,x,I2)') " Risers number: ", nrisers
       if (slp.ge.1) then
       write (23,'(A,2x,F7.2,x,A)') " Riser A: ",  x33(1), "cm"
       if (slp.ge.2) then
       write (23,'(A,2x,F7.2,x,A)') " Riser B: ",  x33(2), "cm"
       if (slp.ge.3) then
       write (23,'(A,2x,F7.2,x,A)') " Riser C: ",  x33(3), "cm"
       if (slp.ge.4) then
       write (23,'(A,2x,F7.2,x,A)') " Riser D: ",  x33(4), "cm"
       if (slp.ge.5) then
       write (23,'(A,2x,F7.2,x,A)') " Riser E: ",  x33(5), "cm"
       end if
       end if
       end if
       end if
       end if
       end if

c      Case 1 Standard risers, different length
       if (k33d.eq.1) then
       write (23,*) "Standard risers, different length"
       write (23,'(A,x,I2)') " Risers number: ", nrisers
       if (slp.ge.1) then
       write (23,'(A,2x,F7.2,x,A)') " Riser A: ",  x33(1), "cm"
       if (slp.ge.2) then
       write (23,'(A,2x,F7.2,x,A)') " Riser B: ",  x33(2), "cm"
       if (slp.ge.3) then
       write (23,'(A,2x,F7.2,x,A)') " Riser C: ",  x33(3), "cm"
       if (slp.ge.4) then
       write (23,'(A,2x,F7.2,x,A)') " Riser D: ",  x33(4), "cm"
       if (slp.ge.5) then
       write (23,'(A,2x,F7.2,x,A)') " Riser E: ",  x33(5), "cm"
       end if
       end if
       end if
       end if
       end if
       end if

c      Case 2 Vario seat two points

c      Case 3 Vario seat three points

c      Case 4 Vario seat four points


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      18.10 Lines characteristics table
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       write (23,*)
       write (23,*) "16. LINES CHARACTERISTICS TABLE"
       write (23,*) "-------------------------------------------------",
     + "----------------------------------"
       if (k34d.eq.0) then
       write (23,*) "Used default types table"
       end if
       if (k34d.eq.1) then
       write (23,*) "Used custom types table"
       end if
       write (23,'(A20,x,I2)') " Total types number: ",k34n

       write (23,'(A2,x,A1,x,A6,x,A6,x,A13,x,A8,x,A3,x,A15,x,
     + A6,x,A3,x,A1,x,A6,x,A2)')
     + " T","s","a","b","Label        ","Strength","daN",
     + "Material       ",
     + "Weight","g","l","Length","cm"
       write (23,*) "-------------------------------------------------",
     + "----------------------------------"

       do i=1,k34n
       if (tylinea(i,1).eq."r") then
       write (23,'(I2,x,A1,x,F6.2,x,F6.2,x,A15,x,F6.1,x,A3,x,A15,x,
     + F6.2,x,A3,x,A1,x,F6.2,x,A2)')
     + i,tylinea(i,1),tyline(i,2),tyline(i,3),tylinea(i,4),tyline(i,5)
     + ,"daN",tylinea(i,6),tyline(i,7),"g",
     + tylinea(i,8),tyline(i,9),"cm"
       end if
       if (tylinea(i,1).eq."c") then
       write (23,'(I2,x,A1,x,F6.2,x,A6,x,A15,x,F6.1,x,A3,x,A15,x,
     + F6.2,x,A3,x,A1,x,F6.2,x,A2)')
     + i,tylinea(i,1),tyline(i,2),"",tylinea(i,4),tyline(i,5)
     + ,"daN",tylinea(i,6),tyline(i,7),"g",
     + tylinea(i,8),tyline(i,9),"cm"
       end if
       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      18.11 Solve equilibrium equations
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       write (23,*)
       write (23,*) "17. LONGITUDINAL EQUILIBRIUM (informative)"
       write (23,*) "-------------------------------------------------",
     + "---------------------------"

       if (k35d.eq.0) then
       write (23,*) "Don't try to solve equilibrium equacions"
       end if

       if (k35d.eq.1) then ! write output case 35

       write (23,*) "An attempt to solve the paraglider longitudinal",
     + " equilibrium. ", 
     + "Use interactively until you get convincing results."
       write (23,*) "It is necessary to use external programs such as",
     + " XFLR5 and or CFD to obtain the aerodynamic coefficients",
     + " and the center of pressure"
       write (23,*) "Remember that the axes used in LEparagliding are:"
       write (23,*) "Origin (0,0,0)= at the nose of the central ",
     + "profile section."
       write (23,*) "X-axis horizontal and in the span direction"
       write (23,*) "Y-axis along the central chord"
       write (23,*) "Z-axis perpendicular to the XY plane and pointing",
     + " down (not coincides with gravity axis)"


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Print normal output (regular users)
c      i37output=0
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (i37output.eq.0) then 

       write (23,*)
       write (23,*) "Section 35 data and initial parameters:"
       write (23,*) "-----------------------------------------------"
       write (23,'(A16,x,F10.4,x,A)') "g=",x35grav,
     + "m/s2 gravity of Eart"
       write (23,'(A16,x,F10.4,x,A)') "ro=",x35ro,
     + "kg/m3 air mass density"
       write (23,'(A16,x,F10.4,x,A)') "mu=",x35mu,
     + "microPascals x s air dynamic viscosity"
       write (23,'(A16,x,F10.4,x,A)') "V=",x35v,
     + "m/s estimated initial flow speed"
       write (23,'(A16,x,F10.4,x,A)') "Alpha=",x35alpha*180./pi,
     + "deg estimated wing angle of attack AOA"
       write (23,'(A16,x,F10.4,x,A)') "Cl=",x35cl,
     + "wing lift coefficient"
       write (23,'(A16,x,F10.4,x,A)') "Cle=",x35cle,
     + "wing lift adjustment coefficient"
       write (23,'(A16,x,F10.4,x,A)') "Cd=",x35cd,
     + "wing drag coefficient"
       write (23,'(A16,x,F10.4,x,A)') "Cde=",x35cde,
     + "wing drag adjustment coefficient"
       write (23,'(A16,x,F10.4,x,A)') "Cm=",x35cm,
     + "wing moment coefficient"
       write (23,'(A16,x,F10.4,x,A,x,A,F8.3,x,A)') "Cdp=",x35dpcoef,
     + "pilot drag coefficient"
       write (23,'(A16,x,F10.4,x,A2)') "Spilot=",x35spilot,"m2" 
       write (23,'(A16,x,F10.3,x,A2)') "Wing mass=",x35wingmass,"kg"
       write (23,'(A16,x,F10.3,x,A2,x,A39)') "Pilot mass=",x35pilotmass
     + ,"kg","included harness, instruments, water..."
       write (23,'(A16,x,F10.3,x,A)') "Pilot mass desp=",x35pmc,
     + "m  below main karabiners"
       write (23,'(A16,x,F10.3,x,A)') "x-coordinate Cp=",0.0d0,"m"
       write (23,'(A16,x,F10.3,x,A)') "y-coordinate Cp=",x35ycp,"m"
       write (23,'(A16,x,F10.3,x,A)') "z-coordinate Cp=",x35zcp,"m"

c      Results
       write (23,*)
       write (23,*) "Results:"
       write (23,*) "-----------------------------------------------"
c      Print method
       write (23,'(A16,7x,I1)') "Solve method=",i37solve
c      Convergence control
       if (i35m1contr.eq.1) then
       write (23,*) "WARNING!!! Method does not converge! Review data!"
       end if
       if (i35m2contr.eq.1) then
       write (23,*) "WARNING!!! Method does not converge! Review data!"
       end if
       if (i35m3contr.eq.1) then
       write (23,*) "WARNING!!! Method does not converge! Review data!"
       end if
       if (i35m4contr.eq.1) then
       write (23,*) "WARNING!!! Method does not converge! Review data!"
       end if
       if (i35m5contr.eq.1) then
       write (23,*) "WARNING!!! Method does not converge! Review data!"
       end if
c      Masses
       write (23,*)
       write (23,*) "Masses and wingload:"
       write (23,'(A16,x,F10.3,x,A2,x,A)') "Lines mass=",xlimog(2),
     + "kg","(risers included)"
       write (23,'(A16,x,F10.3,x,A2)') "Risers mass=",xlimog(14),
     + "kg"
       write (23,'(A16,x,F10.3,x,A2,3x,I2,x,A)') 
     + "Quickli mass=",xlimog(16),"kg",nrisers*2,"units"
       write (23,'(A16,x,F10.3,x,A2)') "Total mass=",xlimog(16)+
     + xlimog(2)+x35wingmass+x35pilotmass,"kg"
c      Surfaces
       write (23,'(A16,x,F10.4,x,A2)') "Wing flatS=",farea,"m2"
       write (23,'(A16,x,F10.4,x,A2)') "Wing projS=",parea,"m2"
c      Wing load
       write (23,'(A16,x,F10.3,x,A5,x,A)') "Wingload=",xlimog(18)/farea,
     + "kg/m2","(using flat wing area)"
       write (23,'(A16,x,F10.3,x,A5,x,A)') "Wingload=",xlimog(18)/parea,
     + "kg/m2","(using projected wing area)"
c      Lines calculus
       write (23,*)
       write (23,*) "Lines calculus:"
       write (23,'(A16,x,F10.4,x,A,x,A)') "Lines surf=",xlimog(3),
     + "m2 (risers included)" 
       write (23,'(A16,x,F10.4,x,A2)') "Risers surf=",xlimog(15),
     + "m2"
       write (23,'(A16,x,F10.4,x,A)') "y-Lines drag=",x35dly,"m"
       write (23,'(A16,x,F10.4,x,A)') "z-Lines drag=",x35dlz,"m"
       write (23,'(A16,x,F10.4)') "Global Cdlines=",x35dlcoef

c      Aero forces
       write (23,*)
       write (23,*) "Aerodynamic forces:"
       write (23,'(A16,x,F10.4,x,F10.4,x,F10.4)') "k1,k2,k=",
     + x35_k1,x35_k2,x35_k
       write (23,'(A16,x,F10.4,x,A)') "Wing lift=",x35wl,"N"
       write (23,'(A16,x,F10.4,x,A)') "Wing lift ex=",x35liftextra,"N"
       write (23,'(A16,x,F10.4,x,A)') "Wing drag=",x35wd,"N"
       write (23,'(A16,x,F10.4,x,A)') "Wing drag ex=",x35dragextra,"N"
       write (23,'(A16,x,F10.4,x,A)') "Lines drag=",x35linesd,
     + "N (risers included)" 
       write (23,'(A16,x,F10.4,x,A)') "Pilot drag=",x35pilotd,"N"
       write (23,'(A16,x,F10.4,x,A)') "Total drag=",x35totald,"N"  

c      Speed
       write (23,*)
       write (23,'(A16,x,F10.3,x,A3,x,F10.2,x,A4)') "Theoric speed=",
     + x35vi,"m/s",x35vi*3.6,"km/h"
       write (23,'(A16,x,F10.3,x,A3,x,F10.2,x,A4)') "Horiz speed=",
     + x35vi*dcos(x35gamma),"m/s",x35vi*dcos(x35gamma)*3.6,"km/h"
       write (23,'(A16,x,F10.3,x,A3,x,F10.2,x,A4)') "Vert speed=",
     + x35vi*dsin(x35gamma),"m/s",x35vi*dsin(x35gamma)*3.6,"km/h"

c      Wing 3d mass center (repeat above
       write (23,*)
       write (23,*) "Mass centers:"
       write (23,'(A16)') "Wing mc:"
c       write (23,'(A16,x,F10.3,x,A2)') "x=",wcdgx3d/100.,"m"
       write (23,'(A16,x,F10.3,x,A)') "y=",wcdgy3d/100.,"m"
       write (23,'(A16,x,F10.3,x,A)') "z=",wcdgz3d/100.,"m"
       write (23,'(A16)') "Lines+risers mc:"
c       write (23,'(A16,x,F10.3,x,A2)') "x=",0.0d0,"m"
       write (23,'(A16,x,F10.3,x,A)') "y=",xlimog(11)/100.,"m"
       write (23,'(A16,x,F10.3,x,A)') "z=",xlimog(12)/100.,"m"
       write (23,'(A16)') "Quicklinks mc:"
c       write (23,'(A16,x,F10.3,x,A)') "x=",x35qlmcx,"m"
       write (23,'(A16,x,F10.3,x,A)') "y=",x35qlmcy,"m"
       write (23,'(A16,x,F10.3,x,A)') "z=",x35qlmcz,"m"
       write (23,'(A16)') "Pilot+harnes mc:"
c       write (23,'(A16,x,F10.3,x,A)') "x=",x35pmcx,"m"
       write (23,'(A16,x,F10.3,x,A)') "y=",x35pmcy,"m"
       write (23,'(A16,x,F10.3,x,A)') "z=",x35pmcz,"m"
       write (23,'(A16)') "Total mc:"
c       write (23,'(A16,x,F10.3,x,A)') "x=",x35totmcx,"m"
       write (23,'(A16,x,F10.3,x,A)') "y=",x35totmcy,"m"
       write (23,'(A16,x,F10.3,x,A)') "z=",x35totmcz,"m"

c      Angles and calage
       write (23,*)
       write (23,'(A)') "Angles and calage:"
       write (23,'(A,A)') "WARNING: ",
     + "Iterate initial values until convergence!"
       write (23,'(A,A)') "These results are ",
     + "for informational purposes only!"
       write (23,'(A,A)') "The REAL design parameters ",
     + "must be entered manually in section 8 of leparagliding.txt"
       write (23,'(A16,x,F10.3,x,A3)') "Theta=",x35theta*180./pi,"deg"
       write (23,'(A16,x,F10.3,x,A3)') "Gamma=",x35gamma*180./pi,"deg"
       write (23,'(A16,x,F10.3)') "Glide ratio=",x35finesse
       write (23,'(A16,x,F10.3,x,A)') "Main-K x-coord=",clengk/200.,"m"
       write (23,'(A16,x,F10.3,x,A)') "Main-K y-coord=",x35py,"m"
       write (23,'(A16,x,F10.3,x,A)') "Main-K z-coord=",hcp/100.,"m"
       write (23,'(A16,x,F10.2,x,A,x,I5,x,A2)') "Cp %=",
     + x35ycp*1e4/rib(1,5),"%",int(x35ycp*1000.),"mm"
       write (23,'(A16,x,F10.2,x,A,x,I5,x,A2)') "Pp %=",
     + x35ypp*1e4/rib(1,5),"%",int(x35ypp*1000.),"mm"
       write (23,'(A16,x,F10.2,x,A,x,I5,x,A7)') "Calage %="
     + ,x35py*1e4/rib(1,5),"%",int(x35py*1000.),"mm (Py)"

       end if ! i37output=0 normal users
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Print detailed output (advanced and research users)
c      i37output=1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (i37output.eq.1.or.i37output.eq.2) then 

       if (i37output.eq.2) then  ! Add only by De Villiers request :)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Basic data
       write (23,*)
       write (23,*) "Basic project data:"
       write (23,*) "-----------------------------------------------"
       write (23,'(A16,1x,F6.2,A,F6.1,A)') "Flat area= ", farea, 
     + " m2 ",farea*10.7639," ft2"
       write (23,'(A16,1x,F6.2,A,F6.1,A)') "Flat span= ",fspan, " m  ",
     + fspan/0.3048," ft" 
       write (23,'(A16,1x,F6.2)') "Flat A/R= ", faratio
       write (23,'(A16,1x,F6.2,A,F6.1,A)') "Projected area= ", parea, 
     + " m2 ",parea*10.7639," ft2"
       write (23,'(A16,1x,F6.2,A,F6.1,A)') "Projected span= ",pspan, 
     + " m  ",pspan/0.3048," ft" 
       write (23,'(A16,1x,F6.2)') "Projected A/R= ", paratio
       write (23,'(A16,1x,F6.2)') "Flattening= ", 
     + ((farea-parea)/farea)*100.
c      More geometric parameters
       varrow=(rib(nribss,7)-rib(1,7))*xwf/100
       write (23,'(A16,1x,F6.2,A)') "Vault arrow= ", varrow, " m"
       write (23,'(A16,1x,F6.2)') "Proj_span/arrow= ", pspan/varrow
       write (23,'(A16,1x,F6.2,A)') "Line heigth= ", 
     + (clengr+clengl)/100, " m included risers"
       write (23,'(A,1x,F6.2)') "Proj_span/Line_heigth= ", 
     + pspan*100/(clengl)
       clli=(dsqrt((clengl-rib(nribss,7)*xwf)*(clengl-rib(nribss,7)*
     + xwf)+(pspan*0.50d0*100.0d0)*(pspan*0.5d0*100.0d0)))/100.0d0
       write (23,'(A,1x,F6.2,A)') "Karabiners - wingtip= ", 
     + clli, " m"
       write (23,'(A,1x,F6.2)') "Proj_span/(Karabiners - wingtip)= ",
     + pspan/clli
c      Wing type
       write (23,'(A16,A)') "Wing type is= ", atp
c      Center of gravity
       write (23,'(A,F6.2,A,x,I4,x,A2)') 
     + "Planform geometric center of gravity at ", cdg, 
     + " % from leading edge",int((cdg/100.)*rib(1,5)*10.),"mm"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       end if

       write (23,*)
       write (23,*) "Section 35 data and initial parameters:"
       write (23,*) "-----------------------------------------------"
       write (23,'(A16,x,F10.4,x,A)') "g=",x35grav,
     + "m/s2 gravity of Eart"
       write (23,'(A16,x,F10.4,x,A)') "ro=",x35ro,
     + "kg/m3 air mass density"
       write (23,'(A16,x,F10.4,x,A)') "mu=",x35mu,
     + "microPascals x s air dynamic viscosity"
       write (23,'(A16,x,F10.4,x,A)') "V=",x35v,
     + "m/s estimated initial flow speed"
       write (23,'(A16,x,F10.4,x,A)') "Alpha=",x35alpha*180./pi,
     + "deg estimated wing angle of attack AOA"
       write (23,'(A16,x,F10.4,x,A)') "Cl=",x35cl,
     + "wing lift coefficient"
       write (23,'(A16,x,F10.4,x,A)') "Cle=",x35cle,
     + "wing lift adjustment coefficient"
       write (23,'(A16,x,F10.4,x,A)') "Cd=",x35cd,
     + "wing drag coefficient"
       write (23,'(A16,x,F10.4,x,A)') "Cde=",x35cde,
     + "wing drag adjustment coefficient"
       write (23,'(A16,x,F10.4,x,A)') "Cm=",x35cm,
     + "wing moment coefficient"
       write (23,'(A16,x,F10.4,x,A,x,A,F8.3,x,A)') "Cdp=",x35dpcoef,
     + "pilot drag coefficient"
       write (23,'(A16,x,F10.4,x,A2)') "Spilot=",x35spilot,"m2" 
       write (23,'(A16,x,F10.3,x,A2)') "Wing mass=",x35wingmass,"kg"
       write (23,'(A16,x,F10.3,x,A2,x,A39)') "Pilot mass=",x35pilotmass
     + ,"kg","included harness, instruments, water..."
       write (23,'(A16,x,F10.3,x,A)') "Pilot mass desp=",x35pmc,
     + "m  below main karabiners"
       write (23,'(A16,x,F10.3,x,A)') "x-coordinate Cp=",0.0d0,"m"
       write (23,'(A16,x,F10.3,x,A)') "y-coordinate Cp=",x35ycp,"m"
       write (23,'(A16,x,F10.3,x,A)') "z-coordinate Cp=",x35zcp,"m"

c      Results
       write (23,*)
       write (23,*) "Results:"
       write (23,*) "-----------------------------------------------"
c      Print method
       write (23,'(A16,7x,I1)') "Solve method=",i37solve
c      Convergence control
       if (i35m1contr.eq.1) then
       write (23,*) "WARNING!!! Method does not converge! Review data!"
       end if
       if (i35m2contr.eq.1) then
       write (23,*) "WARNING!!! Method does not converge! Review data!"
       end if
       if (i35m3contr.eq.1) then
       write (23,*) "WARNING!!! Method does not converge! Review data!"
       end if
       if (i35m4contr.eq.1) then
       write (23,*) "WARNING!!! Method does not converge! Review data!"
       end if
       if (i35m5contr.eq.1) then
       write (23,*) "WARNING!!! Method does not converge! Review data!"
       end if
c      Masses
       write (23,*)
       write (23,*) "Masses and wingload:"
       write (23,'(A16,x,F10.3,x,A2,x,A)') "Lines mass=",xlimog(2),
     + "kg","(risers included)"
       write (23,'(A16,x,F10.3,x,A2)') "Risers mass=",xlimog(14),
     + "kg"
       write (23,'(A16,x,F10.3,x,A2,3x,I2,x,A)') 
     + "Quickli mass=",xlimog(16),"kg",nrisers*2,"units"
       write (23,'(A16,x,F10.3,x,A2)') "Total mass=",xlimog(16)+
     + xlimog(2)+x35wingmass+x35pilotmass,"kg"
c      Surfaces
       write (23,'(A16,x,F10.4,x,A2)') "Wing flatS=",farea,"m2"
       write (23,'(A16,x,F10.4,x,A2)') "Wing projS=",parea,"m2"
c      Wing load
       write (23,'(A16,x,F10.3,x,A5,x,A)') "Wingload=",xlimog(18)/farea,
     + "kg/m2","(using flat wing area)"
       write (23,'(A16,x,F10.3,x,A5,x,A)') "Wingload=",xlimog(18)/parea,
     + "kg/m2","(using projected wing area)"
c      Lines calculus
       write (23,*)
       write (23,*) "Lines calculus:"
       write (23,'(A16,x,F10.4,x,A,x,A)') "Lines surf=",xlimog(3),
     + "m2 (risers included)" 
       write (23,'(A16,x,F10.4,x,A2)') "Risers surf=",xlimog(15),
     + "m2"
       write (23,'(A16,x,F10.4,x,A)') "y-Lines drag=",x35dly,"m"
       write (23,'(A16,x,F10.4,x,A)') "z-Lines drag=",x35dlz,"m"
       write (23,'(A16,x,F10.4)') "Global Cdlines=",x35dlcoef

c      Aero forces
       write (23,*)
       write (23,*) "Aerodynamic forces:"
       write (23,'(A16,x,F10.4,x,F10.4,x,F10.4)') "k1,k2,k=",
     + x35_k1,x35_k2,x35_k
       write (23,'(A16,x,F10.4,x,A)') "Wing lift=",x35wl,"N"
       write (23,'(A16,x,F10.4,x,A)') "Wing lift ex=",x35liftextra,"N"
       write (23,'(A16,x,F10.4,x,A)') "Wing lift tot=",
     + x35wl+x35liftextra,"N"
       write (23,'(A16,x,F10.4,x,A)') "Wing drag=",x35wd,"N"
       write (23,'(A16,x,F10.4,x,A)') "Wing drag ex=",x35dragextra,"N"
       write (23,'(A16,x,F10.4,x,A)') "Wing drag tot=",
     + x35wd+x35dragextra,"N"
       write (23,'(A16,x,F10.4,x,A)') "Lines drag=",x35linesd,
     + "N (risers included)" 
       write (23,'(A16,x,F10.4,x,A)') "Pilot drag=",x35pilotd,"N"
       write (23,'(A16,x,F10.4,x,A)') "Total drag=",x35totald,"N"  
       write (23,'(A16,x,F10.4,x,A)') "Induced drag=",
     + x35inducedd,"N (theoretical by AR, and included in wing drag)"

c      Lift/Drag ratios
       write (23,*)
       write (23,*) "Lift to Drag ratios:"
       write (23,'(A16,x,F10.4,x,A)') "Wing Cl/Cd=",x35cl/x35cd
       write (23,'(A16,x,F10.4,x,A)') "Cl*Cle/(Cd*Cde)=",
     + x35cl*x35cle/(x35cd*x35cde)
       write (23,'(A16,x,F10.4,x,A)') "L/(D+Dextra)=",
     + x35wl/(x35wd+x35dragextra)
       write (23,'(A16,x,F10.4,x,A)') "L/(Drag total)=",
     + x35wl/x35totald

c      Speed
       write (23,*)
       write (23,'(A16,x,F10.3,x,A3,x,F10.2,x,A4)') "Theoric speed=",
     + x35vi,"m/s",x35vi*3.6,"km/h"
       write (23,'(A16,x,F10.3,x,A3,x,F10.2,x,A4)') "Horiz speed=",
     + x35vi*dcos(x35gamma),"m/s",x35vi*dcos(x35gamma)*3.6,"km/h"
       write (23,'(A16,x,F10.3,x,A3,x,F10.2,x,A4)') "Vert speed=",
     + x35vi*dsin(x35gamma),"m/s",x35vi*dsin(x35gamma)*3.6,"km/h"

c      Wing 3d mass center (repeat above
       write (23,*)
       write (23,*) "Mass centers:"
       write (23,'(A16)') "Wing mc:"
c       write (23,'(A16,x,F10.3,x,A2)') "x=",wcdgx3d/100.,"m"
       write (23,'(A16,x,F10.3,x,A)') "y=",wcdgy3d/100.,"m"
       write (23,'(A16,x,F10.3,x,A)') "z=",wcdgz3d/100.,"m"
       write (23,'(A16)') "Lines+risers mc:"
c       write (23,'(A16,x,F10.3,x,A2)') "x=",0.0d0,"m"
       write (23,'(A16,x,F10.3,x,A)') "y=",xlimog(11)/100.,"m"
       write (23,'(A16,x,F10.3,x,A)') "z=",xlimog(12)/100.,"m"
       write (23,'(A16)') "Quicklinks mc:"
c       write (23,'(A16,x,F10.3,x,A)') "x=",x35qlmcx,"m"
       write (23,'(A16,x,F10.3,x,A)') "y=",x35qlmcy,"m"
       write (23,'(A16,x,F10.3,x,A)') "z=",x35qlmcz,"m"
       write (23,'(A16)') "Pilot+harnes mc:"
c       write (23,'(A16,x,F10.3,x,A)') "x=",x35pmcx,"m"
       write (23,'(A16,x,F10.3,x,A)') "y=",x35pmcy,"m"
       write (23,'(A16,x,F10.3,x,A)') "z=",x35pmcz,"m"
       write (23,'(A16)') "Total mc:"
c       write (23,'(A16,x,F10.3,x,A)') "x=",x35totmcx,"m"
       write (23,'(A16,x,F10.3,x,A)') "y=",x35totmcy,"m"
       write (23,'(A16,x,F10.3,x,A)') "z=",x35totmcz,"m"

c      Angles and calage
       write (23,*)
       write (23,'(A)') "Angles and calage:"
       write (23,'(A,A)') "WARNING: ",
     + "Iterate initial values until convergence!"
       write (23,'(A,A)') "These results are ",
     + "for informational purposes only!"
       write (23,'(A,A)') "The REAL design parameters ",
     + "must be entered manually in section 8 of leparagliding.txt"
       write (23,'(A16,x,F10.3,x,A3)') "Theta=",x35theta*180./pi,"deg"
       write (23,'(A16,x,F10.3,x,A3)') "Gamma=",x35gamma*180./pi,"deg"
       write (23,'(A16,x,F10.3)') "Glide ratio=",x35finesse
       write (23,'(A16,x,F10.3,x,A)') "Main-K x-coord=",clengk/200.,"m"
       write (23,'(A16,x,F10.3,x,A)') "Main-K y-coord=",x35py,"m"
       write (23,'(A16,x,F10.3,x,A)') "Main-K z-coord=",hcp/100.,"m"
       write (23,'(A16,x,F10.2,x,A,x,I5,x,A2)') "Cp %=",
     + x35ycp*1e4/rib(1,5),"%",int(x35ycp*1000.),"mm"
       write (23,'(A16,x,F10.2,x,A,x,I5,x,A2)') "Pp %=",
     + x35ypp*1e4/rib(1,5),"%",int(x35ypp*1000.),"mm"
       write (23,'(A16,x,F10.2,x,A,x,I5,x,A7)') "Calage %="
     + ,x35py*1e4/rib(1,5),"%",int(x35py*1000.),"mm (Py)"
c      Complements
       write (23,*)
       write (23,*) "Complements:"
       write (23,'(A16,x,F10.2,x,A,x,I5,x,A2)') "Pp-Py %=",
     + (x35ypp-x35py)*1e4/rib(i,5),"%",int((x35ypp-x35py)*1e3),"mm"
       write (23,'(A16,x,F10.2,x,A,x,I5,x,A2)') "Py-Cp %=",
     + (x35py-x35ycp)*1e4/rib(i,5),"%",int((x35py-x35ycp)*1e3),"mm"
       write (23,'(A16,x,F10.2,x,A,x,I5,x,A2)') "Pp-Cp %=",
     + (x35ypp-x35ycp)*1e4/rib(i,5),"%",int((x35ypp-x35ycp)*1e3),"mm"
       write (23,'(A16,x,I4,x,A)') "wcg-wcp=",
     + int(x35distwcp),"mm Distance(wingcg(y,z),wingcp(y,z))"
       write (23,'(A16,x,I4,x,A)') "wcg_y-wcp_y=",
     + int((wcdgy3d*0.01-x35ycp)*1000.),"mm"
       write (23,'(A16,x,I4,x,A)') "wcg_z-wcp_z=",
     + int((wcdgz3d*0.01-x35zcp)*1000.),"mm"

       end if ! i37output=1 for advanced and research users
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Debug informations
c       write (23,*)
c       write (23,*) "Debug informations:"
c       write (23,*) "-----------------------------------------------"
c       write (23,'(A16,x,F8.2,x,A)') "Lift at gamma=0 is",x35_up,"N"
c       write (23,'(A16,x,F8.2,x,A)') "Total weight is",x35_down,"N"
c       if (x35_up.gt.x35_down) then
c       write (23,*) "Warning! Wing lift is excessive or pilot",
c     + " too light for the initial estimated speed"
c       end if

c      Speeds V,Vh,Vv

c      Write detailed lines drag information
       write (23,*)
       write (23,*) "Detailed lines drag information:"
       write (23,*) "-----------------------------------------------"
       write (23,*) "Line - Surf (mm2) - Reynolds -  Cdline  ",
     + "-  Line drag (N) "
       do i=1,cordat
       write (23,'(I4,3x,F9.0,3x,F9.0,3x,F8.3,3x,F8.2)') 
     + i,xlimo(i,3),xlimo(i,20),xlimo(i,21),xlimo(i,22) 
       end do

       end if ! case 35 active

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      18.12 Some parameters used in XFLR5 analysis
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       write (23,*)
       write (23,*) "18. SOME PANEL PARAMETERS FOR XFLR5 ANALYSIS"
       write (23,*) "-------------------------------------------------",
     + "---------------------------"

       if (k36d.eq.0) then
       write (23,*) "No files for XFLR5"
       end if

       if (k36d.eq.1) then
       write (23,*) "Read files inside xfl5 folder"
       end if


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      18.14 Some special parameters
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       write (23,*)
       write (23,*) "19. SOME SPECIAL PARAMETERS"
       write (23,*) "-------------------------------------------------",
     + "---------------------------"

       if (k37d.eq.0) then
       write (23,*) "No special parameters used in section 37"
       end if

       if (k37d.eq.1) then
       write (23,'(I3,x,A)') k37n, "special codes used"
       do i=1,k37n
       write (23,'(x,I3,2x,I4)') i,icode37(i)
       end do
       end if


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
