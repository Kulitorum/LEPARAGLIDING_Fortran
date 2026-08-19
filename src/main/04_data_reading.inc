c      4. DATA READING
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 1 Geometry (basic data and geometry)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c      Lectura de dades del fitxer

       rewind (22)
       rewind (23)
       rewind (30)

c      Read basic data
       do i=1,9
       read (22,*)
       end do

       read (22,*) bname
       read (22,*)
       read (22,*) wname
       read (22,*)
       read (22,*) xkf
       read (22,*)
       read (22,*) xwf
       read (22,*)
       read (22,*) ncells
       read (22,*)
       read (22,*) nribst
       read (22,*)
       read (22,*) alpham, kbbb         ! case 0,1

c      Read case "2"
       if (kbbb.eq.2) then
       backspace(22)
       read (22,*) alpham, kbbb, alphac ! case 2
       alphat=alpham-alphac
       end if

       read (22,*)
       read (22,*) atp, kaaa
       read (22,*)
       read (22,*)

c      Read ribs geometry
       nribss=int(ncells/2.)+1

c      Ribs geometry rib,x,LE,TE,chord,x',z,beta,RP 

c      Count words in first row of geometry matrix
       read (22,'(A)',end=9) entireline
9      continue
       backspace(22)

c      Count words
       words = ""
       read (entireline,*,iostat=ierr) words
       n_words = count(words /= "")

c      Read matrix of geometry
       do i=1,nribss

       if (n_words.eq.11) then ! case lep >= 3.16
       read (22,*) rib(i,1), rib(i,2), rib(i,3), rib(i,4), rib(i,6), 
     + rib(i,7), rib(i,9), rib(i,10), rib(i,51), rib(i,250), rib(i,251)
       end if

       if (n_words.eq.9) then ! case lep < 3.16
       read (22,*) rib(i,1), rib(i,2), rib(i,3), rib(i,4), rib(i,6), 
     + rib(i,7), rib(i,9), rib(i,10), rib(i,51)
       rib(i,250)=0.0d0
       rib(i,251)=0.0d0
       end if

c      Anticipates tan(0.0) if beta=0.
       if (rib(i,10).eq.0.) then
       rib(i,10)=0.01
       end if

c      Anticipates xp=0 in central airfoil
       if (rib(1,6).eq.0.) then
       rib(i,6)=0.01
       end if

c      central cell width control
       cencell=rib(1,2)

c      Scale geometry to absolute
       rib(i,2)=rib(i,2)*xwf
       rib(i,3)=rib(i,3)*xwf
       rib(i,4)=rib(i,4)*xwf
       rib(i,6)=rib(i,6)*xwf
       rib(i,7)=rib(i,7)*xwf

c      Chord
       rib(i,5)=rib(i,4)-rib(i,3)

c      Washin calculus

c      Case 0
       
       if (kbbb.eq.0) then
       rib(i,8)=rib(i,51)
       end if

c      Case 1
       if (kbbb.eq.1) then
       ribdif=rib(1,5)-rib(i,5)
       ribdim=rib(1,5)-rib(nribss,5)
       rib(i,8)=alpham*ribdif/ribdim
       end if

c      Case 2
       if (kbbb.eq.2) then
       ribdif=rib(1,5)-rib(i,5)
       ribdim=rib(1,5)-rib(nribss,5)
       rib(i,8)=(alphat*ribdif/ribdim)+alphac
       end if

       end do

c      Add virtual rib nribss+1 (used in V-ribs Type-5)

       if (nribss.ge.2) then
       do j=1,10
       rib(nribss+1,j)=rib(nribss-1,j)
       end do
       rib(nribss+1,6)=rib(nribss,6)+(rib(nribss,6)-rib(nribss-1,6))
       rib(nribss+1,7)=rib(nribss,7)+(rib(nribss,7)-rib(nribss-1,7))
       rib(nribss+1,51)=rib(nribss-1,51)
       rib(nribss+1,250)=rib(nribss-1,250)
       rib(nribss+1,251)=rib(nribss-1,251)
       end if

       write (*,*) "01-Planform read"
       write (50,*) "01-Planform read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 2 Airfoils
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Airfoil data: name, intakes location, open cells, disp
       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*)
       do i=1,nribss
       read (22,*) rib(i,1),nomair(i),rib(i,11),rib(i,12),rib(i,14)
     + ,rib(i,50),rib(i,55),rib(i,56)
       end do

       nomair(nribss+1)=nomair(nribss-1) ! Additional virtual rib
       rib(nribss+1,11)=rib(nribss-1,11)
       rib(nribss+1,12)=rib(nribss-1,12)
       rib(nribss+1,14)=rib(nribss-1,14)
       rib(nribss+1,50)=rib(nribss-1,50)
       rib(nribss+1,55)=rib(nribss-1,55)
       rib(nribss+1,56)=rib(nribss-1,56)

       write (*,*) "02-Airfoils read"
       write (50,*) "02-Airfoils read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 3 Anchor points
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Airfoil data: anchor points A,B,C,D,E,F location

       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*)
       do i=1,nribss
       read (22,*) rib(i,1),rib(i,15),rib(i,16),rib(i,17),rib(i,18),
     + rib(i,19),rib(i,20),rib(i,21)
       end do

       do j=15,21 ! Additional virtual rib
       rib(nribss+1,j)=rib(nribss-1,j)
       end do

c      Load rib 0 data
       do k=1,300  ! Be sure all rib parameters are assigned (!)
       rib(0,k)=rib(1,k)
       end do
       rib(0,2)=-rib(1,2)
       rib(0,6)=-rib(1,6)
       
       write (*,*) "03-Anchors position read"
       write (50,*) "03-Anchors position read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 4 holes
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) ndis

       do m=1,ndis
       
       read (22,*) nrib1
       read (22,*) nrib2
       read (22,*) nhols

       ir=nrib1

       do l=1,nhols

       hol(ir,l,1)=float(nhols)

       read(22,*) hol(ir,l,9),hol(ir,l,2),hol(ir,l,3),hol(ir,l,4),
     + hol(ir,l,5),hol(ir,l,6),hol(ir,l,7),hol(ir,l,8)

       end do
       
       do ii=nrib1,nrib2

       do l=1,nhols

       hol(ii,l,1)=hol(ir,l,1)
       hol(ii,l,2)=hol(ir,l,2)
       hol(ii,l,3)=hol(ir,l,3)
       hol(ii,l,4)=hol(ir,l,4)
       hol(ii,l,5)=hol(ir,l,5)
       hol(ii,l,6)=hol(ir,l,6)
       hol(ii,l,7)=hol(ir,l,7)
       hol(ii,l,8)=hol(ir,l,8)
       hol(ii,l,9)=hol(ir,l,9)

       if (hol(ii,l,9).eq.11) then ! parameters for unloaded
       ii11=ii
       nhols11=nhols
       end if

       end do

       end do

       end do

       write (*,*) "04-Airfoil holes read"
       write (50,*) "04-Airfoil holes read"

       ir=1
       
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 5 skin tension data
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc  

       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*)

       do k=1,6
       read (22,*) skin(k,1),skin(k,2),skin(k,3),skin(k,4)
       end do
       read (22,*) htens
       htensi=htens
       read (22,*) ndif, xndif

       write (*,*) "05-Skin tension read"
       write (50,*) "05-Skin tension read"
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 6 sewing allowances
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) xupp, xupple, xuppte
       read (22,*) xlow, xlowle, xlowte
       read (22,*) xrib
       read (22,*) xvrib

       write (*,*) "06-Sewing allowances read"
       write (50,*) "06-Sewing allowances read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 7 marks
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) xmark, xcir, xdes

       write (*,*) "07-Marks read"
       write (50,*) "07-Marks read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 8 calage estimation parameters
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) finesse
       read (22,*)
       read (22,*) cpress
       read (22,*)
       read (22,*) calage
       read (22,*)
       read (22,*) clengr
       read (22,*)
       read (22,*) clengl
       clengl=clengl*xwf
       read (22,*)
       read (22,*) clengk

       write (*,*) "08-Calage read"
       write (50,*) "08-Calage read"

       planeig=finesse

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 9 suspension lines description
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       read (22,*)
       read (22,*)
       read (22,*)

c      Read pondered media type zcontrol
       read (22,*) zcontrol

c      Read plans number
       read (22,*) slp

c      Detect if lines properties were added
c      WARNING only case levels le 4 considered !!!!
c      Case 5 levels still not developed
       read (22,*) i
c      Count number of columns and see if lines properties were added
       read (22,'(A)',end=11) entireline
11     continue
       backspace(22)
       backspace(22)
       words = ""
       read (entireline,*,iostat=ierr) words
       n_words = count(words /= "")
c      Define icase_lines
       if (n_words.eq.11) then
       icase_lines=0 ! set automatic properties
       end if
       if (n_words.gt.11) then
       icase_lines=1 ! use table properties
       end if

c       Write (*,*) "icase ", icase_lines

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case icase_lines=0 (type not added)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (icase_lines.eq.0) then

c      Read all plans
       do ii=1,int(slp) ! Brakes not included

c      Read paths in plane ii
       read (22,*) cam(ii)

c      Read path i
       do i=1,cam(ii)

c      Read levels number
       read (22,*) mc(ii,i,1)
       backspace(22)

c      Read normally levels 1 to 4
       if (mc(ii,i,1).le.4) then
       read (22,*) mc(ii,i,1), mc(ii,i,2), mc(ii,i,3), mc(ii,i,4), 
     + mc(ii,i,5),mc(ii,i,6), mc(ii,i,7), mc(ii,i,8), mc(ii,i,9),
     + mc(ii,i,14), mc(ii,i,15)
       mc(ii,i,10)=0.
       mc(ii,i,11)=0.
       end if

c      Read additional level number 5 (WARNING still not used)
       if (mc(ii,i,1).eq.5) then
       read (22,*) mc(ii,i,1), mc(ii,i,2), mc(ii,i,3), mc(ii,i,4), 
     + mc(ii,i,5),mc(ii,i,6), mc(ii,i,7), mc(ii,i,8), mc(ii,i,9),
     + mc(ii,i,10), mc(ii,i,11), mc(ii,i,14), mc(ii,i,15)
       end if

c      Set default lines type (1,2,3,4)
       j=mc(ii,i,1) ! levels number
       do k=1,j
       mc(ii,i,30+k)=k
       end do

       end do ! i paths

       end do ! ii plans

       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case icase_lines=1 (type specified)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (icase_lines.eq.1) then

c      Read all plans
       do ii=1,int(slp) ! Brakes not included

c      Read paths in plane ii
       read (22,*) cam(ii)

c      Read path i
       do i=1,cam(ii)

c      Read levels number
       read (22,*) mc(ii,i,1)
       backspace(22)

c      Read levels 1 to 4 - and lines type

c      Two levels
       if (mc(ii,i,1).eq.2) then
c      Read up to 2 levels
       if (mc(ii,i,1).le.4) then
       read (22,*) mc(ii,i,1), mc(ii,i,2), mc(ii,i,3), mc(ii,i,4), 
     + mc(ii,i,5),mc(ii,i,6), mc(ii,i,7), mc(ii,i,8), mc(ii,i,9),
     + mc(ii,i,14), mc(ii,i,15), gname, mc(ii,i,31), mc(ii,i,32)
       mc(ii,i,10)=0.
       mc(ii,i,11)=0.
       mc(ii,i,33)=0.
       mc(ii,i,34)=0.
       end if
c      Read additional level number 5 (WARNING still not used)
       if (mc(ii,i,1).eq.5) then
       read (22,*) mc(ii,i,1), mc(ii,i,2), mc(ii,i,3), mc(ii,i,4), 
     + mc(ii,i,5),mc(ii,i,6), mc(ii,i,7), mc(ii,i,8), mc(ii,i,9),
     + mc(ii,i,10), mc(ii,i,11), mc(ii,i,14), mc(ii,i,15),
     + gname, mc(ii,i,31), mc(ii,i,32)
       end if
       end if ! 2 levels

c      Three levels
       if (mc(ii,i,1).eq.3) then
c      Read up to 3 levels
       if (mc(ii,i,1).le.4) then
       read (22,*) mc(ii,i,1), mc(ii,i,2), mc(ii,i,3), mc(ii,i,4), 
     + mc(ii,i,5),mc(ii,i,6), mc(ii,i,7), mc(ii,i,8), mc(ii,i,9),
     + mc(ii,i,14), mc(ii,i,15), gname, mc(ii,i,31), mc(ii,i,32),
     + mc(ii,i,33)
       mc(ii,i,10)=0.
       mc(ii,i,11)=0.
       mc(ii,i,34)=0.

c       write (*,*) ii,i,mc(ii,i,31),mc(ii,i,32),mc(ii,i,33)

       end if
c      Read additional level number 5 (WARNING still not used)
       if (mc(ii,i,1).eq.5) then
       read (22,*) mc(ii,i,1), mc(ii,i,2), mc(ii,i,3), mc(ii,i,4), 
     + mc(ii,i,5),mc(ii,i,6), mc(ii,i,7), mc(ii,i,8), mc(ii,i,9),
     + mc(ii,i,10), mc(ii,i,11), mc(ii,i,14), mc(ii,i,15),
     + gname, mc(ii,i,31), mc(ii,i,32), mc(ii,1,33)
       end if
       end if ! 3 levels

c      Four levels
       if (mc(ii,i,1).eq.4) then
c      Read up to 4 levels
       if (mc(ii,i,1).le.4) then
       read (22,*) mc(ii,i,1), mc(ii,i,2), mc(ii,i,3), mc(ii,i,4), 
     + mc(ii,i,5),mc(ii,i,6), mc(ii,i,7), mc(ii,i,8), mc(ii,i,9),
     + mc(ii,i,14), mc(ii,i,15), gname, mc(ii,i,31), mc(ii,i,32),
     + mc(ii,i,33), mc(ii,i,34)
       mc(ii,i,10)=0.
       mc(ii,i,11)=0.
c       write (*,*) ii,i,mc(ii,i,31),mc(ii,i,32),mc(ii,i,33),mc(ii,i,34)

       end if
c      Read additional level number 5 (WARNING still not used)
       if (mc(ii,i,1).eq.5) then
       read (22,*) mc(ii,i,1), mc(ii,i,2), mc(ii,i,3), mc(ii,i,4), 
     + mc(ii,i,5),mc(ii,i,6), mc(ii,i,7), mc(ii,i,8), mc(ii,i,9),
     + mc(ii,i,10), mc(ii,i,11), mc(ii,i,14), mc(ii,i,15),
     + gname, mc(ii,i,31), mc(ii,i,32),mc(ii,i,33), mc(ii,i,34)
       end if
       end if ! 4 levels

       end do ! i paths

       end do ! ii plans

       end if

       write (*,*) "09-Lines read"
       write (50,*) "09-Lines read"

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 10 brakes
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       ii=slp+1

       read (22,*)
       read (22,*)
       read (22,*)

c      Rean main brake length

       read (22,*) clengb

       clengb=clengb*xwf

c      Read paths
       read (22,*) cam(ii)

c      Detect if brake lines properties were added
c      Count number of columns and see if lines properties were added
       read (22,'(A)',end=12) entireline
12     continue
       backspace(22)
       words = ""
       read (entireline,*,iostat=ierr) words
       n_words = count(words /= "")
c      Define icase_lines
       if (n_words.eq.11) then
       icase_brakes=0 ! set automatic properties
       end if
       if (n_words.gt.11) then
       icase_brakes=1 ! use table properties
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case icase_brakes=0 (type not added)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (icase_brakes.eq.0) then
c      Read path i
       do i=1,cam(ii)
       read (22,*) mc(ii,i,1), mc(ii,i,2), mc(ii,i,3), mc(ii,i,4), 
     + mc(ii,i,5),mc(ii,i,6), mc(ii,i,7), mc(ii,i,8), mc(ii,i,9),
     + mc(ii,i,14), brake(i,3)
c      Set default lines type (1,2,3,4)
       j=mc(ii,i,1) ! levels number
       do k=1,j
       mc(ii,i,30+k)=k
       end do
c      Fractional anchors option
       brake(i,1)=dfloat(int(brake(i,3)))
       brake(i,2)=brake(i,3)-brake(i,1)
       mc(ii,i,15)=int(brake(i,3))
       end do ! i paths
       end if ! icase_brakes=0

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case icase_brakes=1 (type added)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (icase_brakes.eq.1) then
c      Read path i
       do i=1,cam(ii)

c      Read levels number
       read (22,*) mc(ii,i,1)
       backspace(22)

c      Case two levels
       if (mc(ii,i,1).eq.2) then
       read (22,*) mc(ii,i,1), mc(ii,i,2), mc(ii,i,3), mc(ii,i,4), 
     + mc(ii,i,5),mc(ii,i,6), mc(ii,i,7), mc(ii,i,8), mc(ii,i,9),
     + mc(ii,i,14), brake(i,3), gname, mc(ii,i,31),mc(ii,i,32)
       end if

c      Case three levels
       if (mc(ii,i,1).eq.3) then
       read (22,*) mc(ii,i,1), mc(ii,i,2), mc(ii,i,3), mc(ii,i,4), 
     + mc(ii,i,5),mc(ii,i,6), mc(ii,i,7), mc(ii,i,8), mc(ii,i,9),
     + mc(ii,i,14), brake(i,3), gname, mc(ii,i,31),mc(ii,i,32),
     + mc(ii,i,33)
       end if

c      Case four levels
       if (mc(ii,i,1).eq.4) then
       read (22,*) mc(ii,i,1), mc(ii,i,2), mc(ii,i,3), mc(ii,i,4), 
     + mc(ii,i,5),mc(ii,i,6), mc(ii,i,7), mc(ii,i,8), mc(ii,i,9),
     + mc(ii,i,14), brake(i,3), gname, mc(ii,i,31),mc(ii,i,32),
     + mc(ii,i,33),mc(ii,i,34)
c       write (*,*) ii,i,mc(ii,i,31),mc(ii,i,32),mc(ii,i,33),mc(ii,i,34)

       end if

c      Fractional anchors option
       brake(i,1)=dfloat(int(brake(i,3)))
       brake(i,2)=brake(i,3)-brake(i,1)
       mc(ii,i,15)=int(brake(i,3))
       end do ! i paths
       end if ! icase_brakes=1

c      Read Brake distribution
       read (22,*)
       read (22,*) bd(1,1), bd(2,1), bd(3,1), bd(4,1), bd(5,1)
       read (22,*) bd(1,2), bd(2,2), bd(3,2), bd(4,2), bd(5,2)
       bd(1,2)=bd(1,2)*xwf
       bd(2,2)=bd(2,2)*xwf 
       bd(3,2)=bd(3,2)*xwf 
       bd(4,2)=bd(4,2)*xwf 
       bd(5,2)=bd(5,2)*xwf

       write (*,*) "10-Brakes read"       
       write (50,*) "10-Brakes read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 11 ramification lengths
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       read (22,*)
       read (22,*)
       read (22,*)

       read (22,*) raml(3,1), raml(3,3)
       read (22,*) raml(4,1), raml(4,3), raml(4,4)
       read (22,*) raml(5,1), raml(5,3)
       read (22,*) raml(6,1), raml(6,3), raml(6,4)

       raml(3,1)=raml(3,1)*xwf 
       raml(3,3)=raml(3,3)*xwf
       raml(4,1)=raml(4,1)*xwf
       raml(4,3)=raml(4,3)*xwf  
       raml(4,4)=raml(4,4)*xwf
       raml(5,1)=raml(5,1)*xwf
       raml(5,3)=raml(5,3)*xwf
       raml(6,1)=raml(6,1)*xwf
       raml(6,3)=raml(6,3)*xwf
       raml(6,4)=raml(6,4)*xwf

       write (*,*) "11-Ramifications read"
       write (50,*) "11-Ramifications read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 12 H V and HV ribs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       read (22,*)
       read (22,*)
       read (22,*)

       read (22,*) nhvr
       read (22,*) xrsep, yrsep

       xrsep=xrsep*xkf
       yrsep=yrsep*xkf

       if (nhvr.ne.0) then
       
       do i=1,nhvr
       read (22,*) hvr(i,1),hvr(i,2),hvr(i,3),hvr(i,4),hvr(i,5),hvr(i,6)
     + ,hvr(i,7),hvr(i,8),hvr(i,9),hvr(i,10)

c      Case 3 special, use f- and r+ in % of chord, instead of absolute cm
c      Yuri request 2016-08-25
c      Set column 7 and 8 in %, and column number 9 to "1"
       if (hvr(i,2).eq.3.and.hvr(i,9).eq.1) then
       hvr(i,7)=(hvr(i,7)/100)*(rib(int(hvr(i,3)),5))

       if (hvr(i,5).eq.1.and.hvr(i,6).eq.0) then
       hvr(i,8)=(hvr(i,8)/100)*(rib(int(hvr(i,3))-1,5))
       end if
       if (hvr(i,5).eq.0.and.hvr(i,6).eq.1) then
       hvr(i,8)=(hvr(i,8)/100)*(rib(int(hvr(i,3))+1,5))
       end if
       if (hvr(i,5).eq.1.and.hvr(i,6).eq.1) then
       hvr(i,8)=(hvr(i,8)/100)*(rib(int(hvr(i,3)),5))
       end if

       end if !  case Type 3

       hvr(i,15)=0.0 ! Set increment left
       hvr(i,16)=0.0 ! Set increment right

       end do

c      Re-read file
       do i=1,nhvr
       backspace(22)
       end do

c      Allow read type 6 in lep >= 2.49
c      Feature will be deleted when updated data file format
       do i=1,nhvr
       if (hvr(i,2).eq.6.or.hvr(i,2).eq.16) then
       read (22,*) hvr(i,1),hvr(i,2),hvr(i,3),hvr(i,4),hvr(i,5),hvr(i,6)
     + ,hvr(i,7),hvr(i,8),hvr(i,9),hvr(i,10),hvr(i,11),hvr(i,12)
       else
       read (22,*) hvr(i,1),hvr(i,2),hvr(i,3),hvr(i,4),hvr(i,5),hvr(i,6)
     + ,hvr(i,7),hvr(i,8),hvr(i,9),hvr(i,10)
       hvr(i,15)=0.0 ! Set increment left
       hvr(i,16)=0.0 ! Set increment right
       end if
       end do
       
c      Inicialitzar valors a 0
       do i=1,nhvr
       hvr(i,15)=0.0d0
       hvr(i,16)=0.0d0
       hvr(i,17)=0.0d0
       hvr(i,18)=0.0d0
       hvr(i,19)=0.0d0
       hvr(i,20)=0.0d0
       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Adjust some absolute values in cm by xwf
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       do i=1,nhvr

       if (hvr(i,2).eq.1) then
       hvr(i,7)=hvr(i,7)*1
       end if

       if (hvr(i,2).eq.2) then
       hvr(i,7)=hvr(i,7)*xwf
       hvr(i,8)=hvr(i,8)*xwf
       end if

       if (hvr(i,2).eq.3.and.hvr(i,9).ne.1.) then
       hvr(i,7)=hvr(i,7)*xwf
       hvr(i,8)=hvr(i,8)*xwf
       end if

       if (hvr(i,2).eq.4) then
       hvr(i,7)=hvr(i,7)*xwf
       hvr(i,8)=hvr(i,8)*xwf
       end if

       if (hvr(i,2).eq.5) then
       hvr(i,10)=hvr(i,10)*xwf
       end if

       if (hvr(i,2).eq.6) then
       hvr(i,6)=hvr(i,6)*xwf
       hvr(i,7)=hvr(i,7)*xwf
       hvr(i,11)=hvr(i,11)*xwf
       hvr(i,12)=hvr(i,12)*xwf
       end if

       end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Reformat new Types in %
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       do i=1,nhvr

c      Reformat Type 11 in % to Type 1 in cm
       if (hvr(i,2).eq.11) then
       hvr(i,17)=hvr(i,7)
       hvr(i,7)=(hvr(i,17)/100)*(rib(int(hvr(i,3)),5))
       hvr(i,20)=(hvr(i,17)/100)*(rib(int(hvr(i,5)),5))-hvr(i,7)
       hvr(i,2)=1
       end if

c      Reformat Type 12 in % to Type 2 in cm
       if (hvr(i,2).eq.12) then
       hvr(i,17)=hvr(i,7)
       hvr(i,18)=hvr(i,8)
       hvr(i,7)=(hvr(i,17)/100)*(rib(int(hvr(i,3)),5))
       hvr(i,8)=(hvr(i,18)/100)*(rib(int(hvr(i,3)),5))
c      Proportional value at left 
       hvr(i,15)=(hvr(i,18)/100)*(rib(int(hvr(i,3))-1,5))-hvr(i,8)
c      Proportional value at right
       hvr(i,17)=(hvr(i,18)/100)*(rib(int(hvr(i,3))+1,5))-hvr(i,8)
       hvr(i,2)=2

c       write (*,*) "1>",hvr(i,1),hvr(i,15),hvr(i,8),hvr(i,17)

       end if

c      Reformat Type 13 in % to Type 3 in cm
       if (hvr(i,2).eq.13) then
       hvr(i,17)=hvr(i,7)
       hvr(i,18)=hvr(i,8)
       hvr(i,7)=(hvr(i,17)/100)*(rib(int(hvr(i,3)),5))
       hvr(i,8)=(hvr(i,18)/100)*(rib(int(hvr(i,3)),5))
c      Proportional value at left 
       hvr(i,15)=(hvr(i,18)/100)*(rib(int(hvr(i,3))-1,5))-hvr(i,8)
c      Proportional value at right
       hvr(i,16)=(hvr(i,18)/100)*(rib(int(hvr(i,3))+1,5))-hvr(i,8)
       hvr(i,2)=3
       end if

c      Reformat Type 14 in % to Type 4 in cm
       if (hvr(i,2).eq.14) then
       hvr(i,17)=hvr(i,7)
       hvr(i,18)=hvr(i,8)
       hvr(i,7)=(hvr(i,7)/100)*(rib(int(hvr(i,3)),5))
       hvr(i,8)=(hvr(i,8)/100)*(rib(int(hvr(i,3)),5))
c      Proportional value at left 
       hvr(i,15)=(hvr(i,17)/100)*(rib(int(hvr(i,3))-1,5))-hvr(i,7)
c      Proportional value at right
       hvr(i,16)=(hvr(i,17)/100)*(rib(int(hvr(i,3))+2,5))-hvr(i,7)
       hvr(i,2)=4
       end if

c      Reformat Type 15 in % to Type 5 in cm
       if (hvr(i,2).eq.15) then
       hvr(i,10)=(hvr(i,10)/100)*(rib(int(hvr(i,3)),5))
       hvr(i,2)=5
       end if

c      Reformat Type 16 in % to Type 6 in cm
       if (hvr(i,2).eq.16) then
       hvr(i,6)=(hvr(i,6)/100)*(rib(int(hvr(i,3)),5))
       hvr(i,7)=(hvr(i,7)/100)*(rib(int(hvr(i,3)),5))
       hvr(i,11)=(hvr(i,11)/100)*(rib(int(hvr(i,8)),5))
       hvr(i,12)=(hvr(i,12)/100)*(rib(int(hvr(i,8)),5))
       hvr(i,2)=6
       end if

c      NOTA: valors hvr(i,11) i hvr(i,12) són per a donar continuitat 
c      creant amples ajustat per dreta-esquerra segons corda rib

       end do

       end if

       write (*,*) "12-H V VH ribs read"
       write (50,*) "12-H V VH ribs read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 15 extrados colors
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) npce

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case classic
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (npce.ge.1) then
c      k=total ribs with colors marks
       do k=1,npce
       read (22,*) npc1e(k), npc2e(k)
c      l=mark number in k rib
       do l=1,npc2e(k)
       read (22,*) npc3e(k,l), xpc1e(k,l), xpc2e(k,l)
       end do
       end do  
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case -2 read cuts by panel (Pawel Lipski method) 2024-09-06
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (npce.eq.-2) then
       read (22,*) npace
c      k=total panels with colors marks
       do k=1,npace
       read (22,*) npac1e(k), npac2e(k)
c      l=cut number in panel k
       do l=1,npac2e(k)
       read (22,*) npac3e(k,l), xpac1e(k,l), xpac2e(k,l),
     + xpac3e(k,l), xpac4e(k,l)
       end do
       end do  
       end if

       write (*,*) "15-Extrados colors read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 16 intrados colors
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) npci

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case classic
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (npci.ge.1) then
c      k=total ribs with colors marks
       do k=1,npci
       read (22,*) npc1i(k), npc2i(k)
c      l=mark number in k rib
       do l=1,npc2i(k)
       read (22,*) npc3i(k,l), xpc1i(k,l), xpc2i(k,l)
       end do
       end do
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Case -2 read cuts by panel (Pawel Lipski method) 2024-09-06
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (npci.eq.-2) then
       read (22,*) npaci
c      k=total panels with colors marks
       do k=1,npaci
       read (22,*) npac1i(k), npac2i(k)
c      l=cut number in panel k
       do l=1,npac2i(k)
       read (22,*) npac3i(k,l), xpac1i(k,l), xpac2i(k,l),
     + xpac3i(k,l), xpac4i(k,l)
       end do
       end do  
       end if

       write (*,*) "16-Intrados colors read"
       write (50,*) "16-Intrados colors read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 17 Aditional rib points
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       read (22,*)
       read (22,*)
       read (22,*)

       read (22,*) narp

       if (narp.eq.0) then
c      Do noting
       end if

       if (narp.ne.0) then
       do i=1,narp
       read (22,*) xarp(i), yarp(i)
       end do
       end if

       write (*,*) "17-Aditional rib points read"
       write (50,*) "17-Aditional rib points read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 18 Elastic lines corrections
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       read (22,*)
       read (22,*)
       read (22,*)

       read (22,*) csusl

       read (22,*) cdis(2,1), cdis(2,2)
       read (22,*) cdis(3,1), cdis(3,2), cdis(3,3)
       read (22,*) cdis(4,1), cdis(4,2), cdis(4,3), cdis(4,4)
       read (22,*) cdis(5,1), cdis(5,2), cdis(5,3),cdis(5,4),cdis(5,5)

       do i=1,5

       read (22,*) csus(i,1), csus(i,2), csus(i,3), csus(i,4)

       end do

       write (*,*) "18-Elastic lines corrections read"
       write (50,*) "18-Elastic lines corrections read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SECTIONS 4.19,4.20 added since version >= 2.70
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 19 DXF layer names
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       read (22,*)
       read (22,*)
       read (22,*)

       read (22,*) k19d

       do i=1,k19d
       read(22,*) gname,lyname(i)
       end do

       write (*,*) "19-DXF layer names read"
       write (50,*) "19-DXF layer names read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 20 marks types
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       read (22,*)
       read (22,*)
       read (22,*)

       read (22,*) k20d

       do i=1,k20d
       read(22,*) gname,typm1(i),typm2(i),typm3(i),
     + typm4(i),typm5(i),typm6(i)
       end do

       write (*,*) "20-DXF mark types read"
       write (50,*) "20-DXF mark types read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 21 joncs definition
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k21d

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Do nothing if rods scheme 0
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (k21d.eq.0) then
       k21blocs=0
       end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read data for nose rods scheme 1
c      Only one bloc using rods type 1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (k21d.eq.1) then

       m=1 ! Only one bloc
       read (22,*) ng21 ! max groups in section 21

       do ng=1,ng21 ! read in group
       read (22,*) ngoo(m,ng,1),ngoo(m,ng,2),ngoo(m,ng,3)
       read (22,*) xextraa(m,ng,1),xextraa(m,ng,2),xextraa(m,ng,3),
     + xextraa(m,ng,4)
       read (22,*) xintraa(m,ng,1),xintraa(m,ng,2),xintraa(m,ng,3),
     + xintraa(m,ng,4)
       read (22,*) sjoo(m,ng,1),sjoo(m,ng,2),sjoo(m,ng,3),sjoo(m,ng,4)
       end do
       
c      Define jonc group rib(i,166) - NOT USED ??????
       do i=1,nribss
       rib(i,166)=0.
       end do
       do ng=1,ng21
       do i=1,nribss
       if (i.ge.ngo(ng,2).and.i.le.ngo(ng,3)) then
       rib(i,166)=float(ng)
       end if
c       write(*,*) "NG=",i,ng,rib(i,166)
       end do ! ribs
       end do ! groups

       k21blocs=1 ! set number of blocs 1
       k21blocf(1,1)=1 ! set bloc 1
       k21blocf(1,2)=1 ! set jonc type 1
       k21blocf(1,3)=ng21 ! set jonc type 1

       end if ! Type 1

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read data for nose rods scheme 2
c      In scheme 2 we have various blocs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (k21d.eq.2) then

       read (22,*) k21blocs ! number of blocs

c      Read each type
       do m=1,k21blocs

       read (22,*) k21blocf(m,1), k21blocf(m,2)

c      Case classic joncs in the nose
       if (k21blocf(m,2).eq.1) then

       read (22,*) k21blocf(m,3) ! Number of groups in bloc m
       do ng=1,k21blocf(m,3) ! read in group
       read (22,*) ngoo(m,ng,1),ngoo(m,ng,2),ngoo(m,ng,3)
       read (22,*) xextraa(m,ng,1),xextraa(m,ng,2),xextraa(m,ng,3),
     + xextraa(m,ng,4)
       read (22,*) xintraa(m,ng,1),xintraa(m,ng,2),xintraa(m,ng,3),
     + xintraa(m,ng,4)
       read (22,*) sjoo(m,ng,1),sjoo(m,ng,2),sjoo(m,ng,3),sjoo(m,ng,4)
       end do

       end if

c      Case arc joncs
       if (k21blocf(m,2).eq.2) then

       read (22,*) k21blocf(m,3) ! Number of groups in bloc m
       do ng=1,k21blocf(m,3) ! read in group
       read (22,*) ngoo(m,ng,1),ngoo(m,ng,2),ngoo(m,ng,3)
       read (22,*) x21(m,ng,1),x21(m,ng,2),x21(m,ng,3),x21(m,ng,4),
     + x21(m,ng,5)
       read (22,*) sjoo(m,ng,1),sjoo(m,ng,2),sjoo(m,ng,3),sjoo(m,ng,4)
       end do ! ng group

       end if


c      Case shark nose rods
       if (k21blocf(m,2).eq.3) then

       read (22,*) k21blocf(m,3) ! Number of groups in bloc m
       do ng=1,k21blocf(m,3) ! read in group
       read (22,*) ngoo(m,ng,1),ngoo(m,ng,2),ngoo(m,ng,3)
       read (22,*) xextraa(m,ng,1),xextraa(m,ng,2),xextraa(m,ng,3),
     + xextraa(m,ng,4)
       read (22,*) xextraa(m,ng,5),xextraa(m,ng,6),xextraa(m,ng,7),
     + xextraa(m,ng,8)
       read (22,*) xintraa(m,ng,5),xintraa(m,ng,6),xintraa(m,ng,7),
     + xintraa(m,ng,8)
       read (22,*) xintraa(m,ng,1),xintraa(m,ng,2),xintraa(m,ng,3),
     + xintraa(m,ng,4)
       read (22,*) sjoo(m,ng,1),sjoo(m,ng,2),sjoo(m,ng,3),sjoo(m,ng,4)
       end do

       end if


       end do ! bloc m
   
       end if ! Type 2

       write (*,*) "21-Joncs definition read"
       write (50,*) "21-Joncs definition read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 22 nose mylars definition
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k22d

c      Do nothing!
       if (k22d.eq.0) then
       end if

c      Read data for nose mylars type 1
       if (k22d.eq.1) then

       read (22,*) ng22 ! max groups in section 21

       do ng=1,ng22 ! read in group
       read (22,*) ngo(ng,1),ngo(ng,2),ngo(ng,3)
       read (22,*) xmy(ng,1),xmy(ng,2),xmy(ng,3),xmy(ng,4),
     + xmy(ng,5),xmy(ng,6)
       end do
       
       end if ! Type 1

c      Define mylar group rib(i,168)
       do i=1,nribss
       rib(i,168)=0.
       end do
       do ng=1,ng22
       do i=1,nribss
       if (i.ge.ngo(ng,2).and.i.le.ngo(ng,3)) then
       rib(i,168)=float(ng)
       end if
       end do
       end do

       write (*,*) "22-Nose mylars definition read"
       write (50,*) "22-Nose mylars definition read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 23 tab reinforcements definition
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k23d
       if (k23d.eq.0) then

c      Define default parameters

       else

c      Read custom parameters

       end if

       write (*,*) "23-Tab reinforcements definition read"
       write (50,*) "23-Tab reinforcements definition read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 24 general 2D DXF options
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k24d
       if (k24d.eq.0) then

c      Define default parameters
       iccolor(1)=1    ! red
       iccolor(2)=30   ! orange
       iccolor(3)=3    ! green
       iccolor(4)=4    ! cyan
       iccolor(5)=6    ! magenta
       iccolor(6)=5    ! blue

       else

c      Read custom parameters
       read (22,*) gname, iccolor(1)
       read (22,*) gname, iccolor(2)
       read (22,*) gname, iccolor(3)
       read (22,*) gname, iccolor(4)
       read (22,*) gname, iccolor(5)
       read (22,*) gname, iccolor(6)

       end if

       write (*,*) "24-General 2D DXF options read"
       write (50,*) "24-General 2D DXF options read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 25 general 3D DXF options
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k25d
       if (k25d.eq.0) then

c      Define default parameters
       iccolor(11)=8    ! red
       iccolor(12)=8  ! orange
       iccolor(13)=8    ! green
       iccolor(14)=8    ! cyan
       iccolor(15)=8    ! magenta
       iccolor(16)=30    ! blue
       ele3d(11)=0
       ele3dc(11)=5	!blue extrados
       ele3d(12)=0
       ele3dc(12)=1	!red vents
       ele3d(13)=0
       ele3dc(13)=3	!green intrados

       else

c      Read custom parameters
       read (22,*) gname, iccolor(11)
       read (22,*) gname, iccolor(12)
       read (22,*) gname, iccolor(13)
       read (22,*) gname, iccolor(14)
       read (22,*) gname, iccolor(15)
       read (22,*) gname, iccolor(16)
       read (22,*) gname, ele3d(11), ele3dc(11)
       read (22,*) gname, ele3d(12), ele3dc(12)
       read (22,*) gname, ele3d(13), ele3dc(13)

       end if

       write (*,*) "25-General 3D DXF options read"
       write (50,*) "25-General 3D DXF options read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 26 Glue vents
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k26d

       if (k26d.eq.0) then
       else
       end if

       if (k26d.eq.1) then
       do i=1,nribss

       read (22,*) j, rib(i,165)
      
       if (rib(i,165).eq.4.or.rib(i,165).eq.-4) then
       backspace(22)
       read (22,*) j, rib(i,165),csi(i,19),csi(i,20)
       if (i.eq.1) then
       csi(i,20)=csi(i,19) ! Set coherent central cell
       end if
       end if

       if (rib(i,165).eq.5.or.rib(i,165).eq.-5) then
       backspace(22)
       read (22,*) j, rib(i,165),csi(i,19),csi(i,20),csi(i,18)
       if (i.eq.1) then
       csi(i,20)=csi(i,19) ! Set coherent central cell
       end if
       end if

       if (rib(i,165).eq.6.or.rib(i,165).eq.-6) then
       backspace(22)
       read (22,*) j, rib(i,165),csi(i,19),csi(i,20)
       end if

       end do ! i
       end if

       rib(0,165)=rib(1,165)

       write (*,*) "26-Glue vents read"
       write (50,*) "26-Glue vents read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 27 Special wingtip
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k27d

       if (k27d.eq.0) then
       else
       end if

c      Angle wingtip
       if (k27d.eq.1) then
       read (22,*) gname, alc1
       alc1=alc1*pi/180.
       read (22,*) gname, alc2
       alc2=alc2*pi/180.
       rib(nribss,3)=rib(nribss-1,3)+
     + (rib(nribss,2)-rib(nribss-1,2))*tan(alc1)
       rib(nribss,4)=rib(nribss-1,4)+
     + (rib(nribss,2)-rib(nribss-1,2))*tan(alc2)
       rib(nribss,5)=rib(nribss,4)-rib(nribss,3)
       end if

c      Parabolic wingtip
       if (k27d.eq.2) then
       read (22,*) gname, sectx(27,3)  ! Distance
       read (22,*) gname, sectx(27,4)  ! Angle
       alc1=alc1*pi/180.
       end if

c      Last rib displacement and rotation
       if (k27d.eq.4) then
       read (22,*) gname, sectx(27,10),sectx(27,11)  ! Distance
       end if

       write (*,*) "27-Special wingtip read"
       write (50,*) "27-Special wingtip read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 28 Parameters for calage variation
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k28d
       if (k28d.eq.0) then ! set default values

       
       else
 
       if (k28d.eq.1) then
       read (22,*) nriser28
       read (22,*) p28(1), p28(2), p28(3), p28(4), p28(5), p28(6)
       read (22,*) a128, n128, a228, n228
       end if
       
       end if

       write (*,*) "28-Parameters for calage variation read"
       write (50,*) "28-Parameters for calage variation read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 29 3D-SHAPING
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k29d ! Data control
  
c      Do nothing
       if (k29d.eq.0) then
c      Set default values for no cuts
       k29dd=1
       ng29=1
       ini29(1)=1
       fin29(1)=nribss
       uppcuts(1)=0
       upptype(1)=1
       lowcuts(1)=0
       lowtype(1)=1
       do i=1,nribss
       rib(i,169)=0.
       end do
c      Set default print parameters
       do i=1,5
       pp29(i,1)=0
       pp29(i,2)=0
       pp29(i,3)=1
       pp29(i,4)=1
       pp29(i,5)=0
       end do
       end if

c       k29dd=0

c      3D-type type 1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (k29d.eq.1) then

c      Initialize defaults
c      Define 3d group rib(i,169)
       do i=1,nribss
       rib(i,169)=0.
       end do

       read (22,*) k29dd  ! 3D-type (not used)

       read (22,*) gname,ng29

       do ng=1,ng29  ! Iterate in each group

       read (22,*) gname, k, ini29(ng), fin29(ng)
       read (22,*) gname, uppcuts(ng), upptype(ng)

       do i=ini29(ng),fin29(ng)
       rib(i,169)=k
       end do

       if (uppcuts(ng).eq.0) then
       end if
     
       if (uppcuts(ng).eq.1) then
       read (22,*) iupp(1,1,ng),iupp(1,2,ng),iupp(1,3,ng),kiupp(1,ng)
       end if

       if (uppcuts(ng).eq.2) then
       read (22,*) iupp(1,1,ng),iupp(1,2,ng),iupp(1,3,ng),kiupp(1,ng)
       read (22,*) iupp(2,1,ng),iupp(2,2,ng),iupp(2,3,ng),kiupp(2,ng)
       end if

       read (22,*) gname, lowcuts(ng), lowtype(ng)

       if (lowcuts(ng).eq.0) then
       end if
     
       if (lowcuts(ng).eq.1) then
       read (22,*) ilow(1,1,ng),ilow(1,2,ng),ilow(1,3,ng),kilow(1,ng)
       end if

       end do ! In each group

       end if ! 3D active type 1

c      3D-type type 2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (k29d.eq.2) then

c      Read data

c      Initialize defaults
c      Define 3d group rib(i,169)
       do i=1,nribss
       rib(i,169)=0.
       end do

       read (22,*) k29dd  ! 3D-type (not used)

       read (22,*) gname,ng29

       do ng=1,ng29  ! Iterate in each group

       read (22,*) gname, k, ini29(ng), fin29(ng)
       read (22,*) gname, uppcuts(ng), upptype(ng)

       do i=ini29(ng),fin29(ng)
       rib(i,169)=k
       end do

       if (uppcuts(ng).eq.0) then
       end if
     
       if (uppcuts(ng).eq.1) then
       read (22,*) iupp(1,1,ng),xiupp(1,2,ng),xiupp(1,3,ng),kiupp(1,ng)
       end if

       if (uppcuts(ng).eq.2) then
       read (22,*) iupp(1,1,ng),xiupp(1,2,ng),xiupp(1,3,ng),kiupp(1,ng)
       read (22,*) iupp(2,1,ng),xiupp(2,2,ng),xiupp(2,3,ng),kiupp(2,ng)
       end if

       read (22,*) gname, lowcuts(ng), lowtype(ng)

       if (lowcuts(ng).eq.0) then
       end if
     
       if (lowcuts(ng).eq.1) then
       read (22,*) ilow(1,1,ng),xilow(1,2,ng),xilow(1,3,ng),kilow(1,ng)
       end if

       end do ! In each group

c      Compute discrete points ir airfoils section

     
c      Return to type 1
       k29d=1

       end if ! 3D active type 2

c      Read print parameters
       if (k29d.ne.0) then
       read (22,*)
       read (22,*) gname,pp29(1,1),pp29(1,2),pp29(1,3),pp29(1,4)
       read (22,*) gname,pp29(2,1),pp29(2,2),pp29(2,3),pp29(2,4)
       read (22,*) gname,pp29(3,1),pp29(3,2),pp29(3,3),pp29(3,4)
       read (22,*) gname,pp29(4,1),pp29(4,2),pp29(4,3),pp29(4,4)
       read (22,*) gname,pp29(5,1),pp29(5,2),pp29(5,3),pp29(5,4)
       end if

c      Print panels using 3D subroutines
       if (k29d.eq.0) then
       k29d=1
       end if

c      Compute discrete points
       rib(0,169)=rib(1,169)

       write (*,*) "29-3D-Shaping read"
       write (50,*) "29-3D-Shaping read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 30 AIRFOIL THICKNESS MODIFICATION
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k30d
       if (k30d.eq.0) then

       do i=1,nribss
       rib(i,160)=1.0 ! Standard thickness
       end do

       else

       if (k30d.eq.1) then

       do i=1,nribss

       read (22,*) i9, rib(i,160)

       end do

       end if

       end if

       write (*,*) "30-Airfoil modification read"
       write (50,*) "30-Airfoil modification read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 31 new skin tension
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k31d

       if (k31d.eq.0) then

       else

       if (k31d.eq.1) then

       read (22,*) ngskt ! max groups

       do ng=1,ngskt ! read in group
    
       read (22,*)
       read (22,*) ngi, nribini, nribfin, npoints, ntype31

       do i=nribini,nribfin
       skinpoints(i)=npoints
       ngroup31(i)=ngi
       ntypei31(i)=ntype31
       end do

       do k=1,npoints ! read points
       read (22,*) ksk, skinew(k,1),skinew(k,2),
     + skinew(k,3),skinew(k,4)

       do i=nribini,nribfin ! assign in each rib group
       skinnew(i,k,1)=skinew(k,1)
       skinnew(i,k,2)=skinew(k,2)
       skinnew(i,k,3)=skinew(k,3)
       skinnew(i,k,4)=skinew(k,4)
       end do

       end do ! read all points in group

       end do

       end if

c      Set 0 rib
       skinpoints(0)=skinpoints(1)
       ngroup31(0)=ngroup31(1)
       ntypei31(0)=ntypei31(1)
       do k=1,npoints
       skinnew(0,k,1)=skinnew(1,k,1)
       skinnew(0,k,2)=skinnew(1,k,2)
       skinnew(0,k,3)=skinnew(1,k,3)
       skinnew(0,k,4)=skinnew(1,k,4)
       end do

       end if

       write (*,*) "31-New skin tension read"
       write (50,*) "31-New skin tension read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 32 parameters for part separation
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k32d

       if (k32d.eq.0) then

       panel_x_coe=1.0
       panel_x_min=1.0
       panel_y_coe=1.0
       rib_x_coe=1.0
       rib_y_coe=1.0
       rib_1_coe=1.0
       jonc_x_coe=1.0
       jonc_y_coe=1.0
       jonc_i_coe=1.0

       else

       if (k32d.eq.1) then
       read (22,*) gname, panel_x_coe
       read (22,*) gname, panel_x_min
       read (22,*) gname, panel_y_coe
       read (22,*) gname, rib_x_coe
       read (22,*) gname, rib_y_coe
       read (22,*) gname, rib_1_coe
       read (22,*) gname, jonc_x_coe
       read (22,*) gname, jonc_y_coe
       read (22,*) gname, jonc_i_coe
       read (22,*) gname, gname
       end if

       end if

       write (*,*) "32-Parameters for part separation read"
       write (50,*) "32-Parameters for part separation read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 33 Detailed risers
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k33d

       nrisers=int(slp) ! Defined in section 9

c      Case risers same length
c      Use lengths defined in section 8
       if (k33d.eq.0) then

       x33(1)=clengr
       x33(2)=clengr
       x33(3)=clengr
       x33(4)=clengr
       x33(5)=clengr

       else

       read (22,*) k33t

c      Case normal risers
       if (k33t.eq.1) then
       do i=1,nrisers
       read (22,*) gname, x33(i)
       end do
       end if

c      Case vario seat two points
       if (k33t.eq.2) then
       end if

c      Case vario seat three points
       if (k33t.eq.3) then
       end if

c      Case vario seat four points
       if (k33t.eq.4) then
       end if

       end if

       write (*,*) "33-Parameters for detailed risers read"
       write (50,*) "33-Parameters for detailed risers read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 34 Lines characteristics table
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k34d

c      Set default values
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (k34d.eq.0) then
c      Set default table
       k34n=6
       tylinea(1,1)="r"
       tyline(1,2)=25.
       tyline(1,3)=2.
       tylinea(1,4)="Riser"
       tyline(1,5)=1000.
       tylinea(1,6)="polyester"
       tyline(1,7)=20.
       tylinea(1,8)="s"
       tyline(1,9)=12.
       tyline(1,10)=7

       tylinea(2,1)="c"
       tyline(2,2)=1.90
       tyline(2,3)=2.
       tylinea(2,4)="Line275"
       tyline(2,5)=275.
       tylinea(2,6)="s_dyneem"
       tyline(2,7)=2.26
       tylinea(2,8)="s"
       tyline(2,9)=12.
       tyline(2,10)=1

       tylinea(3,1)="c"
       tyline(3,2)=1.40
       tyline(3,3)=2.
       tylinea(3,4)="Line160"
       tyline(3,5)=160.
       tylinea(3,6)="s_dyneem"
       tyline(3,7)=1.34
       tylinea(3,8)="s"
       tyline(3,9)=10.
       tyline(3,10)=30

       tylinea(4,1)="c"
       tyline(4,2)=1.15
       tyline(4,3)=2.
       tylinea(4,4)="Line120"
       tyline(4,5)=120.
       tylinea(4,6)="s_dyneem"
       tyline(4,7)=1.00
       tylinea(4,8)="s"
       tyline(4,9)=10.
       tyline(4,10)=5

       tylinea(5,1)="c"
       tyline(5,2)=0.80
       tyline(5,3)=2.
       tylinea(5,4)="Line100U"
       tyline(5,5)=100.
       tylinea(5,6)="u_dyneem"
       tyline(5,7)=0.43
       tylinea(5,8)="p"
       tyline(5,9)=8.
       tyline(5,10)=2

       tylinea(6,1)="c"
       tyline(6,2)=2.00
       tyline(6,3)=2.
       tylinea(6,4)="Line200B"
       tyline(6,5)=160.
       tylinea(6,6)="s_dyneem"
       tyline(6,7)=0.2
       tylinea(6,8)="s"
       tyline(6,9)=12.
       tyline(6,10)=6

       do i=1,6
       jltype(i)=i ! set type number
       end do

       end if ! default values table

c      Read characteristics table
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (k34d.eq.1) then
       read (22,*) k34n

       do i=1,k34n ! Read all lines types

       read (22,*) jltype(i), tylinea(i,1)
       backspace(22)

c      Read line case r
       if (tylinea(i,1).eq."r") then
       read (22,*) j,tylinea(i,1),tyline(i,2),tyline(i,3),tylinea(i,4),
     + tyline(i,5),gname,tylinea(i,6),tyline(i,7),gname,
     + tylinea(i,8),tyline(i,9),gname,tyline(i,10)
       end if

c      Read line case c
       if (tylinea(i,1).eq."c") then
       read (22,*) j,tylinea(i,1),tyline(i,2),tylinea(i,4),
     + tyline(i,5),gname,tylinea(i,6),tyline(i,7),gname,
     + tylinea(i,8),tyline(i,9),gname,tyline(i,10)
       end if

       end do

       end if ! Case read custom table

       write (*,*) "34-Parameters for lines characteristics table read"
       write (50,*) "34-Parameters for lines characteristics table read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 35 Solve equilibrium equations
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k35d

c      Case don't solve equilibrium
       if (k35d.eq.0) then
       end if

c      Case solve equilibrium
       if (k35d.eq.1) then
       read (22,*) gname, x35grav
       read (22,*) gname, x35ro
       read (22,*) gname, x35mu
       read (22,*) gname, x35v
       read (22,*) gname, x35alpha
       read (22,*) gname, x35cl
       read (22,*) gname, x35cle
       read (22,*) gname, x35cd
       read (22,*) gname, x35cde
       read (22,*) gname, x35cm
       read (22,*) gname, x35spilot
       read (22,*) gname, x35dpcoef
       read (22,*) gname, x35wingmass
       read (22,*) gname, x35pilotmass
       read (22,*) gname, x35pmc
       read (22,*) gname, x35qlmass
       read (22,*) gname, x35ycp
       read (22,*) gname, x35zcp
       end if

       x35alpha=x35alpha*pi/180. ! convert to radians

       write (*,*) "35-Parameters for solve equilibrium read"
       write (50,*) "35-Parameters for solve equilibrium read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 36 Create files for XFLR5 analysis
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k36d

c      Case don't do xflr5 analysis
       if (k36d.eq.0) then
       end if

c      Case set parameters for .xwimp
       if (k36d.eq.1) then
       read (22,*) gname
       read (22,*) k36_a
       read (22,*) k36_b
       read (22,*) k36_c
       read (22,*) k36_d
       read (22,*) gname
       read (22,*) k36_e
       end if

       write (*,*) "36-Parameters for xflr5 analysis read"
       write (50,*) "36-Parameters for xflr5 analysis read"

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Read 37 Read some special parameters
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Set default values
       nsegments=12 ! Section 8.5.0.2 Call tessellation
       chord_pcdg=36. ! Center of mass of a typical wing section (is 4.6)
       i37c=0 ! Set lines colors by plans
       i37solve=1 ! solve equilibrium equations by Casellas method (1) or De Villiers (2) or combined (3)
       i37fparea=2 ! Solve Lift, drag, Speed for Flat area (1) projected (2)
       i37output=0 ! 0=simplified output, 1=detailed output
c      End default values

       read (22,*)
       read (22,*)
       read (22,*)
       read (22,*) k37d

c      Case no special parameters, set defaults
       if (k37d.eq.0) then
       end if

c      Case read special parameters
       if (k37d.eq.1) then
       read (22,*) k37n ! Number of lines to read

       do i=1,k37n

       read (22,*) icode
       backspace (22)

c      Code 1291, set nsegments for stl surfaces
       if (icode.eq.1291) then
       read (22,*) icode, nsegments
       end if

c      Code 1146, set cdg of a typical wing section
       if (icode.eq.1146) then
       read (22,*) icode, chord_pcdg
       end if

c      Code 1341, set CAD colors for each lyne type
       if (icode.eq.1341) then
       read (22,*) icode,i37c
       end if

c      Code 1351, change solve method for equilibrium equations
       if (icode.eq.1351) then
       read (22,*) icode,i37solve
       end if

c      Code 1352, solve for flat (1) or projected (2) areas
       if (icode.eq.1352) then
       read (22,*) icode,i37fparea
       end if

c      Code 1353, normal (0) or detailed output (1)
       if (icode.eq.1353) then
       read (22,*) icode,i37output
       end if

c      Code 2000, print edition date
       iflag2000=0
       if (icode.eq.2000) then
       read (22,*) icode,a37date
       iflag2000=1
       end if

c      Code 2003, force assiette angle defined by the user
c      Used in phii and chii calculus section 10.1
       if (icode.eq.2003) then
       read (22,*) icode,assiette2003 ! set in degrees, positive below, negative below horizon
       iflag_assiette=1
       end if

c      Code 3001, set minirib transitions
       i3001(2)=0 ! by default no modification
       if (icode.eq.3001) then
       read (22,*) i3001(1),i3001(2)
       backspace(22)
       if (i3001(2).eq.0) then ! do nothing
       end if
       if (i3001(2).eq.1) then ! read linear
       read (22,*) i3001(1),i3001(2),c3001(3),c3001(4)
       end if
       if (i3001(2).eq.2) then ! read parabolic
       read (22,*) i3001(1),i3001(2),c3001(3),c3001(4)
       end if
       if (i3001(2).eq.3) then ! read cosinuidal
       read (22,*) i3001(1),i3001(2),c3001(3),c3001(4)
       end if
       end if

       end do ! read all lines

       end if

       write (*,*) "37-Some special parameters read"
       write (50,*) "37-Some special parameters read"


c      END OF READ SECTION


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      4.2 Write xwimp geometry file for XFLR5
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (k36d.eq.1) then

c      Make xflr5 folder
       folder = 'xflr5'
       makedirectory = 'mkdir -p ' // trim('../') // "/" // trim(folder)
       call system(makedirectory)

c      open xwimp file
       l=0
       k=len(wname)
       do i=1,k
       if (wname(i:i).ne.' ') then
       l=l+1
       end if
       end do
       write (wname2, '(A,A)') wname(1:l),".xwimp"
       open(unit=34,file='../xflr5/'//wname2)

c      Set airfoil names

c      Thicknees suffix nomair3
       if (k30d.eq.1) then
       do i=1,nribss
       write (nomair3(i),'(F5.0)') rib(i,160)*1000
       l=0
       k=len(nomair3(i))
       do j=1,k
       if (nomair3(i)(j:j).ne.' ') then
       l=l+1
       end if
       end do

c      Add zeros al left of suffix name
       if (l.eq.4) then
       write (nomair3(i)(1:4),'(A,A)') "0",nomair3(i)(2:4)
       end if
       if (l.eq.3) then
       write (nomair3(i)(1:4),'(A,A)') "00",nomair3(i)(3:4)
       end if
       if (l.eq.2) then
       write (nomair3(i)(1:4),'(A,A)') "000",nomair3(i)(4:4)
       end if
       end do
       end if
       
c      Airfoil names nomair2, including thicknees suffix
       do i=1,nribss
       l=0
       k=len(nomair(i))
       do j=1,k
       if (nomair(i)(j:j).ne.' ') then
       l=l+1
       end if
       end do
       write (nomair2(i), '(A,A,A)') nomair(i)(1:l-4),"-",
     + nomair3(i)(1:4)
       end do

c      Add .dat extension to names
       do i=1,nribss
       l=0
       k=len(nomair2(i))
       do j=1,k
       if (nomair2(i)(j:j).ne.' ') then
       l=l+1
       end if
       end do
       write (nomair4(i), '(A,A)') nomair2(i)(1:l),".dat"
       end do

       write (34,*) wname

c      Write main xwimp matrix

c      Case paraglider odd number of cells (senar)
c      Add additional line
       if (rib(1,2).gt.0.1d0) then
       i=1
       write (34,'(F9.5,x,F9.5,x,F9.5,x,F9.5,x,F9.5,x,I3,x,I3,x,I2,x,I2,
     + x,A30,x,A30)')
     + 0.0d0, rib(i,5)/100., rib(i,3)/100.,
     + 0.0d0, rib(i,8),
     +  k36_a,k36_b,k36_c,k36_d,nomair2(i),nomair2(i)
       end if ! case odd

c      Case paraglider even number of cells (parell)
       do i=1,nribss-1
       xdihedral(i)=(180./pi)*
     + datan((rib(i+1,7)-rib(i,7))/(rib(i+1,6)-rib(i,6)))
       write (34,'(F9.5,x,F9.5,x,F9.5,x,F9.5,x,F9.5,x,I3,x,I3,x,I2,x,I2,
     + x,A30,x,A30)')
     + rib(i,2)/100., rib(i,5)/100., rib(i,3)/100.,
     + -xdihedral(i), rib(i,8),
     + k36_a,k36_b,k36_c,k36_d,nomair2(i),nomair2(i)
       end do
       i=nribss
       write (34,'(F9.5,x,F9.5,x,F9.5,x,F9.5,x,F9.5,x,I3,x,I3,x,I2,x,I2,
     + x,A30,x,A30)')
     + rib(i,2)/100., rib(i,5)/100., rib(i,3)/100.,
     + -xdihedral(i-1), rib(i,8),
     +  k36_a,k36_b,k36_c,k36_d,nomair2(i),nomair2(i)

       close(34)

c      Write airfoils .dat files

       do i=1,nribss
 
       open (24, file=nomair(i))
       rewind (24)

       read (24,*) 
       read (24,*) np(i,1) ! total points
       read (24,*) np(i,2) ! extrados points
       read (24,*) np(i,3) ! intake points
       read (24,*) np(i,4) ! intrados points

       open (35, file='../xflr5/'//nomair4(i))
       write (35, *) nomair2(i)
       do j=1,np(i,1)
       read (24,*) u(i,j,1),v(i,j,1)
       write (35,'(F11.8,3x,F11.8)') u(i,j,1),v(i,j,1)*rib(i,160)
       end do
       close (35)

       close (24)

       end do

       end if ! End creation of xflr5 files


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      4.3 Write some warnings and errors found in data
c      Do not cover all errors, but some of habitual
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Number of ribs and cells case even odd 
c      Ribs even > cells -1
c      Ribs odd > cells

       if (ncells.ne.nribst-1) then
       write (*,*) "ERROR: number of cells will be number of ribs -1"
       write (50,*) "ERROR: number of cells will be number of ribs -1"
       end if

       
c     COMPLETE


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       write(*,*) "All data read! Calculus start...please wait a moment"
       write(50,*)"All data read! Calculus start...please wait a moment"
       write (*,*)
       write (50,*)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      4.3 Make stl/ folder and open some optional files
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (pp29(5,1).eq.1) then ! case stl and scad

c      Make stl folder
       folder = 'stl'
       makedirectory = 'mkdir -p ' // trim('../') // "/" // trim(folder)
       call system(makedirectory)


       if (pp29(4,1).eq.1) then ! Open external DXF
       open(unit=26,file='lep-3d-surfaces.dxf') 
       call dxfinit(26)
       end if

       if (pp29(5,1).eq.1) then ! Open external stls
       open(unit=27,file='../stl/lep-3d-surfaces.scad') 
       open(unit=28,file='../stl/lep-3d-surfaces.stl') 
       open(unit=29,file='../stl/Upper-surface.stl')
       open(unit=32,file='../stl/Vents-surface.stl')
       open(unit=31,file='../stl/Lower-surface.stl')
       end if

       end if ! case stl and scad

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      4.4 Alguns paràmetres de mides
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       xmida1=12.0d0*xkf/1.5d0  ! Mida de lletra 1
       xmida2=10.0d0*xkf/1.5d0  ! Mida de lletra 2  
       xmida3=6.0d0*xkf/1.5d0   ! Mida de lletra 3 petita
       xmida4=3.0d0*xkf/1.5d0   ! Mida de lletra 4 molt petita 

       xmida3=xmida3 ! primer us
       xmida4=xmida4   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      PARAMETERS TO BE DEFINED IN DATA FILE
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Revise all overrides
c      Try to normalize to cm as soon as possible      
       typepoint=typm4(1)
       typevent=typm4(4)
       typetab=typm4(5)
       typejonc=typm4(6)

       if (typm4(1).eq.1) then
       xdes=typm6(1)/10.0d0 !override classic xdes
       end if

       if (typm4(1).eq.2) then
       xrad=typm5(1)
       xdes=typm6(1)/10.0d0 !override classic xdes
       end if

c      Compte amb aquesta definició!
       if (typm4(5).le.3) then
       typm5(5)=typm5(5)/10.0d0 !override classic 1 cm
       typm6(5)=typm6(5)/10.0d0 !override classic xdes 1.2 mm
       end if

       xdes1=typm3(1)/10.0d0
       xdes2=typm6(1)/10.0d0
       xcir1=typm2(1)/10.0d0
       xcir2=typm5(1)/10.0d0

c       write (*,*) "xdes=",xdes

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      4.5 Center of gravity calculus (2D)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Planform

       acit=rib(1,2)*rib(1,5)
       cdgx=0.5*rib(1,2)*acit
       cdgy=0.5*(rib(1,3)+rib(1,4))*acit

       do i=1,nribss-1

       aci=0.5*(rib(i,5)+rib(i+1,5))*(rib(i+1,2)-rib(i,2))
       xci=0.5*(rib(i,2)+rib(i+1,2))
       yci=0.25*(rib(i,3)+rib(i,4)+rib(i+1,3)+rib(i+1,4))

       cdgx=cdgx+xci*aci
       cdgy=cdgy+yci*aci

       acit=acit+aci

       end do

       cdgx=cdgx/acit
       cdgy=cdgy/acit

       cdg=100.*((cdgy-rib(1,3))/rib(1,5))

       if (atp.eq."ds".or.atp.eq."ss") then
       write (*,'(A,F5.2,A)') " Area = ", acit*2./10000., " m2"
       write (50,'(A,F5.2,A)') " Area = ", acit*2./10000., " m2"
       else
       write(*,'(A,F5.2,A,F7.1,A)') " Area = ", acit*2./10000., " m2 ",
     + 10.7639*acit*2./10000.," ft2"
       write(50,'(A,F5.2,A,F7.1,A)') " Area = ", acit*2./10000., " m2 ",
     + 10.7639*acit*2./10000.," ft2"
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      4.6 Center of gravity calculus (3D)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Simplified method. Use only chord lines.
c      Do a weighted arithmetic mean of mass centers
c      Predefine the gravity center of a typical chord line.

c       if (k37d.eq.0) then ! set default value
c       chord_pcdg=36. ! Center of mass of a typical wing section
c       end if

c      Detect if true cells number is even
       neven=0
       if (rib(1,2).lt.0.2d0) then ! case true even cells number
       neven=1
       end if

c      Case odd
       if (neven.eq.0) then
c      Sum of all chords
       chordsum=0.0d0
       do i=1,nribss
       chordsum=chordsum+2.*rib(i,5)
       end do
c      wcdgx3d
       wcdgx3d=0.0d0
c      wcdgy3d
       wcdgy3d=0.0d0
       do i=1,nribss
       chordy=(rib(i,3)*(100.-chord_pcdg)+rib(i,4)*chord_pcdg)/100.
       wcdgy3d=wcdgy3d+chordy*rib(i,5)*2.
       end do
       wcdgy3d=wcdgy3d/chordsum
c      wcdgz3d
       wcdgz3d=0.0d0
       do i=1,nribss
       wcdgz3d=wcdgz3d+rib(i,7)*rib(i,5)*2.
       end do
       wcdgz3d=wcdgz3d/chordsum
       end if
       
c      Case even
       if (neven.eq.1) then
c      Sum of all chords
       chordsum=0.0d0
       do i=2,nribss
       chordsum=chordsum+2.*rib(i,5)
       end do
       chordsum=chordsum+rib(1,5)
c      wcdgx3d
       wcdgx3d=0.0d0
c      wcdgy3d
       wcdgy3d=0.0d0
       do i=2,nribss
       chordy=(rib(i,3)*(100.-chord_pcdg)+rib(i,4)*chord_pcdg)/100.
       wcdgy3d=wcdgy3d+chordy*rib(i,5)*2.
       end do
       chordy=(rib(1,3)*(100.-chord_pcdg)+rib(1,4)*chord_pcdg)/100.
       wcdgy3d=(wcdgy3d+chordy*rib(1,5))/chordsum
c      wcdgz3d
       wcdgz3d=0.0d0
       do i=2,nribss
       wcdgz3d=wcdgz3d+rib(i,7)*rib(i,5)*2.
       end do
       wcdgz3d=(wcdgz3d+rib(1,7)*rib(1,5))/chordsum
       end if ! case even

c       write (*,*) "Case ",neven
c       write (*,*) "wcdgx3d ",wcdgx3d 
c       write (*,*) "wcdgy3d ",wcdgy3d 
c       write (*,*) "wcdgz3d ",wcdgz3d 


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      4.7 Adjust some parameters
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      Ample panells extrados a efectes de separació
       amplemig=0.0d0
       npaes=int(nribss/2)
       do i=2,npaes
       amplemig=amplemig+rib(i,2)-rib(i-1,2)
       end do
       amplemig=amplemig/float(npaes-1)

c      Escala descensos de ribs no suspentats
c      WARNING revisar concepte (!)
       do i=1,nribss

       rib(i,50)=rib(i,50)*xwf

       end do

c      Parts separation parameters (SECTION 32)
c      Default parameters
       amplerix=1.0*rib_x_coe
       ampleriy=1.0*rib_y_coe
       amplepix=1.1*panel_x_coe

c      Rib separation parameters
       seprix=350.*xwf*amplerix
       sepriy=90.*xwf*ampleriy
       sepri1_y=(1.-rib_1_coe)*50.*xwf

c      Jonc separation parameters
       sepjox=350.*xwf*jonc_x_coe
       sepjoy=90.*xwf*jonc_y_coe

c      Panels separation
c      seppix(i)=60.*xwf
c      seppix(i)=(amplemig*1.7+(0.20d0*xupp))*amplepix

c      Define separation between panels proportional to its width

       call eo(ncells,evenodd)

c       if (evenodd.eq.1) then
       seppix(0)=-30.0d0
       do i=1,nribss
       seppix(i)=(seppix(i-1)+xwf*(rib(i,2)-rib(i-1,2))*amplepix+
     + 0.2d0*xupp+10.0d0*panel_x_min)
       end do
c       end if

       if (evenodd.eq.3) then
       seppix(0)=-30.0d0
       do i=1,nribss-1
       seppix(i)=(seppix(i-1)+xwf*(rib(i+1,2)-rib(i,2))*amplepix+
     + 0.2d0*xupp+10.0d0*panel_x_min)
       end do
       end if
      
c      Compute some ribs additional parameters
c      Lengths from LE to anchor points A,B,...E

      do i=0,nribss

      rib(i,66)=(rib(i,16)*rib(i,5)/100.)
      rib(i,67)=(rib(i,17)*rib(i,5)/100.)
      rib(i,68)=(rib(i,18)*rib(i,5)/100.)
      rib(i,69)=(rib(i,19)*rib(i,5)/100.)
      rib(i,70)=(rib(i,20)*rib(i,5)/100.)

      end do

c     y separation for panels parts

      ysautt=(rib(1,5)+rib(nribss-2,5))*0.5d0*xwf*0.2d0*panel_y_coe

c     Some parameters for internal marks (subroutine xmarsi)
      xini=0.0d0
      xfin=0.0d0

c     Adjust beta rotation in rib 0
      rib(0,9)=-rib(1,9)
c     Adjust rot_z rotation in rib 0
      rib(0,250)=-rib(1,250)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     4.8 DXF layers  (project)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
