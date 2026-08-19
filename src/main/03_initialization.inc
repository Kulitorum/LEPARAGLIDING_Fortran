       pi=4.0d0*datan(1.0d0)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Run numbers control
       irun=0
 1     continue
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c      Substitution of BOX(1,3) BOX(1,5) by BOX(-1,3) BOX(-1,5)
c      Set n1draw=1 xyextra=2.0 yxintra=1.0 to return previous version

       n1draw=0 ! parameter to control classic plans. If set to 0 no print.
       xyextra=0.0d0   ! Control BOX Y-position extrados
       xyintra=-1.0d0
       xyshift=500.0d0 ! Control Y-position extrados
       iflag_assiette=0 ! Assiette flag

c      Safe defaults for values populated only by optional input sections
       alphat=0.0d0
       cencell=0.0d0
       ii11=0
       nhols11=0
       iflag2000=0
       jcontrole=1
       jcontroli=1
       xminirib=0.0d0
       xequise=0.0d0
       yequise=0.0d0
       ysaut=0.0d0
       x35_k1fi=1.0d0
       x35_k2fi=1.0d0
       x35gammafi=0.01d0
       x35gamma=0.0d0
       x35theta=0.0d0
       i35m2contr=0
       i35m5contr=0
       x35distwcp=0.0d0
       x35ypp=0.0d0

c      integer color
c      common typepoint

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Basic LEparagliding data version
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       lepv="3.28"              ! Version
       lepd="2026-05-01"        ! Date
       lepc='"Jardins"'         ! Code name
       lepuser="GENERAL"  
       ich=0                    ! 0=general version, 1=special version

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      3. INIT
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Write output; console (unit=6); file (unit=50)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       open(unit=50,file='run-log.txt') 

       data valu2 /6,50/

       do i=1,2
       j=valu2(i)

       write (j,*) "LABORATORI D'ENVOL PARAGLIDING"
       write (j,*) "Paragliders and parachutes design program"
       write (j,*)
       write (j,'(A14,1x,A6,1x,A7,1x,A50)') " LEparagliding",lepv,
     + "version",lepc
       write (j,'(1x,A10)') lepd
c      Customized header
       if (ich.eq.1) then
       write (j,*) 
       write (j,*) "**************************************************"
       write (j,*) "Special version for:"
       write (j,'(1x,A72)') lepuser
       write (j,*) "**************************************************"
       else
       end if
       write (j,*)
       write (j,*) "Pere Casellas"
       write (j,*) "pere@laboratoridenvol.com"
       write (j,*) "GNU General Public License 3.0 http://www.gnu.org"
       write (j,*)

       end do

c      Open units
       open(unit=20,file='leparagliding.dxf')
       open(unit=22,file='leparagliding.txt')
       open(unit=23,file='lep-out.txt')
       open(unit=30,file='lines.txt')
       open(unit=25,file='lep-3d.dxf')
   

       call dxfinit(20)

       call dxfinit(25)

c       call mtriangle(0.0d0,200.0d0,10.0d0,0.0d0,1)
c       call mtriangle(20.0d0,200.0d0,10.0d0,pi/2.0d0,2)
c       call mtriangle(40.0d0,200.0d0,10.0d0,pi/3.0d0,3)
c       call mtriangle(60.0d0,200.0d0,10.0d0,pi,4)


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
