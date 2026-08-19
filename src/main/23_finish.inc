c      23. END OF MAIN PROGRAM
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc   

       call dxfend(20)

       do i=1,2
       j=valu2(i)

       write (j,*)

       if (atp.eq."pc") then
       write (j,'(A,1x,F6.2,A,F7.1,A)') " Total line length = ",
     + xlength2," m ",(xlength2)/0.3048," ft"
       write (j,*)
       else
       write (j,'(A,1x,F6.2,A)') " Total line length = ",
     + xlength2," m"
       write (j,*)
       end if

       if (atp.eq."ds".or.atp.eq."ss") then
       write (j,*) "OK, paraglider calculated!"
       end if
       if (atp.eq."pc") then
       write (j,*) "OK, parachute calculated!"
       end if
       write (j,*)
       write (j,*) "Please open the following files:" 
       write (j,*)
       write (j,*) "   leparagliding.dxf"
       write (j,*) "   lep-3d.dxf"
       if (pp29(4,1).eq.1) then ! External DXF
       write (j,*) "   lep-3d-surfaces.dxf"
       end if
       write (j,*) "   lep-out.txt"
       write (j,*) "   lines.txt"
       write (j,*) "   run-log.txt"
       if (pp29(5,1).eq.1) then ! External STL SCAD
       write (j,*) "   ../stl/lep-3d-surfaces.stl"
       write (j,*) "   ../stl/lep-3d-surfaces.scad"
       end if
       if (k36d.eq.1) then ! External XFLR5 files
       write (j,*) "   ../xflr5/ *.xwimp"
       write (j,*) "   ../xflr5/ *.dat"
       end if

       write (j,*)

       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
c      Verificació de subrutines
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       punt1(1)=0.0
       punt1(2)=0.0
       punt1(3)=0.0
       punt2(1)=1.0
       punt2(2)=0.0
       punt2(3)=0.0
       punt3(1)=0.0
       punt3(2)=2.0
       punt3(3)=0.0

c       call planeby123(punt1,punt2,punt3,aplane,bplane,cplane,
c     + dplane)

c       write (*,*) "A B C D"
c       write (*,*) aplane,bplane,cplane,dplane

       dp0=-20.0
       punt0(1)=1.0
       punt0(2)=2.0
       punt0(3)=1.0

c       call pointp(punt0,aplane,bplane,cplane,dp0,punt4)

c       write (*,*) punt4(1),punt4(2),punt4(3)






cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Perform second RUN !
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (irun.eq.1) then
       write (*,*) "Attention! Doing second RUN...!"
       irun=0
       goto 1
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Call noNaN subroutine
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 

c       write (*,*) "Cleaning dxf (noNaN)..."
c       call test_read("leparagliding.dxf")
c       call fix_dxf_nan("leparagliding.dxf")


c       Close files
        close(50)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      END MAIN PROGRAM
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 

