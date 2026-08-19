c      19. lines.txt List of lines - labels in human readable format
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       write (30,*) "LEparagliding"
       write (30,*) "List of lines, human readable format"
       write (30,*) "Ready also to import in .ods or .xls formats"
       write (30,*)
       write (30,*) "Line number - Label - Lenght (cm) - Type"
       write (30,*) 

c      Missing line-plan groups remain empty in the notes section
       slpi=cordam+1

       do i=1, cordat

c      Select letter as final row of the line
       write (ln1,'(I1)') corda (i,2)

       if (corda(i,6).eq.1) ln2="A"
       if (corda(i,6).eq.2) ln2="B"
       if (corda(i,6).eq.3) ln2="C"
       if (corda(i,6).eq.4) ln2="D"
       if (corda(i,6).eq.5) ln2="E"
       if (corda(i,6).eq.6) ln2="F"

c      If only two levels
c      Is OK

c      If tree levels
c      1 and 2 level will be named acording lineplan

c      Renames letters if levels 1 or 2

       if (corda(i,2).le.2.and.corda(i,5).ge.3) then

       if (corda(i,1).eq.1) ln2="A"
       if (corda(i,1).eq.2) ln2="B"
       if (corda(i,1).eq.3) ln2="C"
       if (corda(i,1).eq.4) ln2="D"
       if (corda(i,1).eq.5) ln2="E"
       if (corda(i,1).eq.6) ln2="F"

       end if

       if (corda(i,2).eq.1.and.corda(i,5).eq.2) then

       if (corda(i,1).eq.1) ln2="A"
       if (corda(i,1).eq.2) ln2="B"
       if (corda(i,1).eq.3) ln2="C"
       if (corda(i,1).eq.4) ln2="D"
       if (corda(i,1).eq.5) ln2="E"
       if (corda(i,1).eq.6) ln2="F"

       end if

c      Adjust brake letter to F

       if (i.gt.cordam) ln2="F"

c      Put 2 characters
       
c       if (corda(i,3).le.9)  write (ln3,'(I,I)') 0,corda(i,3)
c       if (corda(i,3).gt.9)  write (ln3,'(I2)') corda(i,3)

c       write (ln3,'(I2)') corda(i,3)

       if (corda(i,3).ge.10) then
       write (ln3,'(I2)') corda(i,3)
       end if

       if (corda(i,3).lt.10) then
       write (ln3,'(I1)') corda(i,3)
       end if


c      Change line order by final rib if upper line

       if (corda(i,2).eq.corda(i,5)) then

c       if (corda(i,7).le.9)  write (ln3,'(I,I)') 0,corda(i,7)
c       if (corda(i,7).gt.9)  write (ln3,'(I2)') corda(i,7)

       if (corda(i,7).ge.10) then
       write (ln3,'(I2)') corda(i,7)
       end if

       if (corda(i,7).lt.10) then
       write (ln3,'(I1)') corda(i,7)
       end if


       end if

c      Write line labels

       write (ln4(i),'(A1,A1,A2)') ln1,ln2,ln3
c       write (*,*) i , ln4(i)
c       write (ln4(i),'(A1,A1)') ln1,ln2


       if (corda(i,6).eq.6) xlifi(i)=xline(i)

c      Row plan A

       if (i.eq.1) then 
       slpi(1)=i
       write (30,*)
       write (30,*) "Plan A"
       write (30,*) 
       end if

c      Rows plan B,C,D,E,F

       if (i.ge.2) then
       if (corda(max(1,i-1),1).lt.corda(i,1)) then

       if (corda(i,1).eq.2) then
       slpi(2)=i
       write (30,*)
       write (30,*) "Plan B"
       write (30,*) 
       end if

       if (corda(i,1).eq.3) then
       slpi(3)=i
       write (30,*)
       write (30,*) "Plan C"
       write (30,*) 
       end if

       if (corda(i,1).eq.4.and.i.le.cordam) then
       slpi(4)=i
       write (30,*)
       write (30,*) "Plan D"
       write (30,*) 
       end if

       if (corda(i,1).eq.5.and.i.le.cordam) then
       slpi(5)=i
       write (30,*)
       write (30,*) "Plan E"
       write (30,*) 
       end if

       end if
       end if

c      Plan F

       if (i.eq.cordam+1) then
       slpi(6)=i
       write (30,*)
       write (30,*) "Brake lines"
       write (30,*) 
       end if

       itype=corda(i,30+corda(i,2))
c       corda(i,40)=itype
       write (30,'(I3,3x,A4,3x,F5.1,A,x,I2,x,A15,x,F6.2,x,A2)')  
     + i, ln4(i), xlifi(i), 
     + " cm  -  type", itype, tylinea(itype,4), tyline(itype,2), "mm"

       end do

       write (30,*)
       write (30,'(A,A,x,F8.2,x,A)') "Total line lengths ",
     + "(without loops) = ", xlength2,"m"
       write (30,'(A,A,x,F8.2,x,A)') "Total line lengths ",
     + "(with loops) = ", xlength1,"m"
       write (30,*) "(loops lengths according section 34 ",
     + "or internal predefined values)"
       write (30,*)

c      Print lengths by types
       write (30,*) "Minimal lengths by types, including loops:"
       do k=1,k34n
       if (lentype(k).ge.1.0d0) then
       write (30,'(A5,x,I2,x,F7.2,x,A,x,A15,x,A15,x,F6.2,x,A2,x,A4,x,
     + F6.2,x,A2)')  "Type ",jltype(k),lentype(k)/50.,"m",
     + tylinea(k,4),tylinea(k,6),tyline(k,2),"mm",
     + "loop",tyline(k,9),"cm"
       end if
       end do


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
