!> Declare the shared state used by the ordered legacy calculation stages.
!!
!! This include is the schema for the fixed-form main pipeline.  Array slot
!! meanings are documented next to their declarations; new independent state
!! belongs in typed modules rather than new anonymous columns.  Later includes
!! depend on this file being included before the first executable statement.
c      1. VARIABLE NAMES
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      ncells: number of cells
c      nribst: total ribs
c      nribss: in semi-wing
c      rib(i,j): rib i parameter j
c            j=1 rib number
c            j=2 x_rib
c            j=3 y_LE
c            j=4 y_TE
c            j=5 chord
c            j=6 x'
c            j=7 z_rib
c            j=8 alpha washin
c            j=9 beta
c            j=10 rotation point
c            j=11 vent in
c            j=12 vent out
c            j=14 open=1 closed=0
c            j=15 anchors number
c            j=16 A %
c            j=17 B %
c            j=18 C %
c            J=19 D %
c            j=20 E %
c            j=21 F % brake
c            j=22 cell wide (i to i+1)
c            j=23 extrados length
c            j=24 intrados wide
c            j=25 intrados length
c            j=26 inlet length
c            j=30 panel length to the left of rib i - extrados
c            j=31 rib i length - extrados
c            j=32 panel length to the right of rib i - extrados
c            j=33 panel length to the left of rib i - intrados
c            j=33 rib i length - intradocall joncst3ss
c            j=35 panel length to the right of rib i - intrados
c            j=36,37,38,39 mark amplification coeficients
c      alpha: airfoil washin angle
c      alpham: max washin
c      beta: airfoil vertical angle
c      calag: calage
c      cple: center of pressure
c      hcp: height pilote - center of pressure
c      assiette:angle between horizontal line and chord
c      finesse: glide ratio
c      aoa: angle of attack AoA
c      nomair(i): airfoil archive
c      np(i,1): airfoil points number
c      u(i,j,20) v(i,j,20): airfoils coordinates
c        K = 1 = original coordinates
c            2 = 100*coordinates
c            3 = scaled coordinates
c            4 = washin coordinates
c            5 = espace coordinates
c            6 = singular points
c                j=1 A anchor point 3D
c                j=2 B anchor point 3D
c                j=3 C anchor point 3D
c                j=4 D anchor point 3D
c                j=5 E anchor point 3D
c                j=6 F anchor point - brake 3D
c                j=7 B intake in point 3D
c                j=8 B intake out point 3D
c            7 = overwide local left
c            8 = overwide local right
c            9 = coordinates overwide left
c            10 = coordinates overwide right
c            11 = coordinates left sewing border
c            12 = coordinates right sewing border
c            14 = panel extreme points left
c            15 = panel extreme points right
c            16 = airfoil borders
c            18 = anchor space coordinates
c            19 = anchor absolute coordinates
c      x(i,j) y(i,j) z(i,j): absolute airfoil coordinates
c      xx(1,j) yy(1,j) zz(1,j): central airfoil (or i=0)
c      hol(100,20,20) airfoil holes properties
c      xl(i,j) xr(i,j) panels length
c      skin(k,j) skin tension
c      xupp, xupple, xuppte, xlow, xlowle, xlowte, xrib sewing allowances
c      mc(ii,j,k) suspension matrix
c           ii = line plan
c           j = path number
c           k = matrix column
c               1 ramifications number of the path
c               2 1 (ramification level 1)
c               3 order in the level 1
c               4 2 (ramification level 2)
c               5 order in the level 2
c               6 3 (ramification level 3)
c               7 order in the level 3
c               8 4 (ramification level 4)
c               9 order in the level 4
c               10 anchor line (1=A,2=B,3=C,4=c,5=D,6=freno)
c               11 anchor rib number
c      cam(ii) paths in plan ii
c      slp suspension line plans
c      corda(i,k) ramification properties of line i
c           k = 1 line plan
c           k = 2 line level (1=riser)
c           k = 3 liner order (in the same level, and left to right)
c           k = 4 action points associated
c           k = 5 path ramifications
c           k = 6 final anchor row 1=A 2=B ...
c           k = 7 anchor rib number
c      x1line(corda(i,1),corda(i,2),corda(i,3)) line i initial x-coordinate
c      y1line(corda(i,1),corda(i,2),corda(i,3)) line i initial y-coordinate
c      z1line(corda(i,1),corda(i,2),corda(i,3)) line i initial z-coordinate
c      x2line(corda(i,1),corda(i,2),corda(i,3)) line i final x-coordinate
c      y2line(corda(i,1),corda(i,2),corda(i,3)) line i final y-coordinate
c      z2line(corda(i,1),corda(i,2),corda(i,3)) line i final z-coordinate
c      x3line(corda(i,1),corda(i,2),corda(i,3)) line i action point x-coordinate
c      y3line(corda(i,1),corda(i,2),corda(i,3)) line i action point y-coordinate
c      z3line(corda(i,1),corda(i,2),corda(i,3)) line i action point z-coordinate
c      xline(i) line i length
c      raml(i,j) ramfication lengths
c
c      hvr(i,j) H and V ribs definiton
c      j=1 H-strap
c      j=2 V-rib partial
c      j=3 V-rib full
c      j=4 VH rib (3 cells)
c
c      V-ribs absolute coordinates
c      rx1(),ry1(),rz1()
c      rx2(),ry2(),rz2()
c      rx3(),ry3(),rz3()
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      2. VARIABLES TYPE DECLARATION
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       2.1 Local coordinates
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       real*8 x0,y0,x1,y1,z1,x2,y2,z2,x3,y3,z3
       real*8 xinc,yinc,zinc
       real*8 xpunt(0:1000),ypunt(0:1000) ! generic vector
       real*8 xloc(0:3000),yloc(0:3000),zloc(0:3000)
       real*8 xglo(0:3000),yglo(0:3000),zglo(0:3000)

       real*8 Apla,Bpla,Cpla,l_line,m_line,n_line
       real*8 l2_line,m2_line,n2_line

       real*8 p1x,p1y,p1z,p2x,p2y,p2z,p3x,p3y,p3z,p4x,p4y,p4z
       real*8 distance,param1,param2,param3 ! general parameters
       real*8 nose_distance_sq

c       real*8 amte,amter,amtel,amle,amler,amlel
       real*8 amle,amler

       real*8 tetha1,tetha2,tetha3,tetha4,angle
c       real*8 angle1
       real*8 angle2

       real*8 xx0,yy0,x00,xxa,yyb

       real*8 o1x,o1y,o2x,o2y,o3x,o3y,o4x,o4y

       real*8 xn,xm,xa,xb,xn1,xm1,xn2,xm2,xn3,xm3,xk

       real*8 xu,xv
c       real*8 xlu1,xlv1,xlu2,xlv2

       real*8 a,b,aa,bb,a1,b1,c1

       real*8 alptri,atri,btri,rtri,satri,h1tri,h2tri,h3tri

       real*8 agor,bgor,cgor,cggor,step1

       character*50 gname

       character*50 wname2


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      2.2 Global coordinates
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       character*50 wname,bname,nomair(100),nomairext(100),lyname(50)
       character*50 nomair2(100),nomair3(100),nomair4(100)
       character*50 xtext,lepv,lepd,lepc
       character*2 atp

       character*50 xstring, string1, string2, string3

       character*1 ln1,ln2
       character*2 ln3
       character*4 ln4(500)

       integer valu2(2)

       real*8 xmk,xmark,xcir,xdes,xdesx,xdesy,xprev,xpost
       real*8 xdes1,xdes2,xcir1,xcir2

       real*8 xcos

       integer ncells,nribst,evenodd,evenodd2

       real*8 xkf,xwf
       real*8 ribdif, htens, xndif

c      Coordenades airfoil en gir Z
       real*8 unew(500),vnew(500),wnew(500)
       real*8 pos, rot_z

       real*8 rib(0:100,500)

       real*8 alpha,tetha,alpham,alphac,alphat,alpha1,alpha2,alpr,alpl
       real*8 alprr(500),alpll(500)
       real*8 alple,alp
       real*8 phii(0:100),alpha_ii,assiette_ii,chii(0:100)
       real*8 x_apap(0:100),y_apap(0:100),z_apap(0:100)

       real*8 u(0:100,500,99),v(0:100,500,99),w(0:100,500,99) ! airfoil 3D
       real*8 uf(0:100,500,50),vf(0:100,500,50)   ! flat panels development
       real*8 ufe(0:100,500,50),vfe(0:100,500,50) ! panels extrados
       real*8 ufv(0:100,500,99),vfv(0:100,500,99) ! panels vents
       real*8 ufi(0:100,500,50),vfi(0:100,500,50) ! panels intrados
       real*8 ufa(0:100,500,50),vfa(0:100,500,50) ! auxiliar a
       real*8 ufb(0:100,500,50),vfb(0:100,500,50) ! auxiliar b
       real*8 ufc(0:100,500,50),vfc(0:100,500,50) ! auxiliar b
       real*8 uft(0:100,500,50),vft(0:100,500,50) ! auxiliar t
       real*8 ufr(0:100,500,50),vfr(0:100,500,50) ! auxiliar r
       real*8 u_aux(0:100,500,10),v_aux(0:100,500,10),
     + w_aux(0:100,500,10) ! auxiliar general

       real*8 xrsep,yrsep,psep,psey
       real*8 asep,bsep,sepx,sepy,sepxx,sepyy

       real*8 cs1x,cs1y,cs2x,cs2y

       real*8 usalvat(0:100,500,50),vsalvat(0:100,500,50)

       real*8 ru(0:100,500,99),rv(0:100,500,99),rw(0:100,500,99)

       real*8 x(0:100,500),y(0:100,500),z(0:100,500)

       real*8 rx(0:100,500),ry(0:100,500),rz(0:100,500)

       real*8 hx2(0:100,50,10), hy2(0:100,50,10), hz2(0:100,50,10)
       real*8 hx3(0:100,50,10), hy3(0:100,50,10), hz3(0:100,50,10)

       real*8 rx1(0:100,500,10), ry1(0:100,500,10), rz1(0:100,500,10)
       real*8 rx2(0:100,500,10), ry2(0:100,500,10), rz2(0:100,500,10)
       real*8 rx3(0:100,500,10), ry3(0:100,500,10), rz3(0:100,500,10)

       real*8 sx1(0:100,50,10), sy1(0:100,50,10), sz1(0:100,50,10)
       real*8 sx2(0:100,50,10), sy2(0:100,50,10), sz2(0:100,50,10)
       real*8 sx3(0:100,50,10), sy3(0:100,50,10), sz3(0:100,50,10)
       real*8 sx4(0:100,50,10), sy4(0:100,50,10), sz4(0:100,50,10)

       integer np(0:100,9)
       real*8 xx(1,500),yy(1,500),zz(1,500)

       real*8 px0,py0,ptheta
       real*8 pa,pb,pc,pd,pe,pf
       real*8 pa1l,pa2l,phl,pa1r,pa2r,phr
       real*8 pb1t,pb2t,pht,phu,pw1

       real*8 pl1x(0:100,500),pl1y(0:100,500),pl2x(0:100,500),
     + pl2y(0:100,500)
       real*8 pr1x(0:100,500),pr1y(0:100,500),pr2x(0:100,500),
     + pr2y(0:100,500)

c      Typed read-only views used while migrating legacy rib and neutral-panel
c      arrays.  They own snapshots and never alias the numbered global slots.
       type(rib_identity), allocatable :: wing_rib_identities(:)
       type(profile_topology), allocatable :: wing_profile_topologies(:)
       type(spatial_rib_geometry_3d), allocatable :: wing_spatial_ribs(:)
       type(neutral_panel_2d) :: neutral_extrados_panel,neutral_intake_panel
       type(neutral_panel_2d), allocatable :: neutral_intrados_panels(:)
       type(neutral_boundary_edge_2d) :: terminal_extrados_edge,
     + terminal_intake_edge,terminal_intrados_edge
       type(skin_tension_law), allocatable ::
     + typed_extrados_tension_laws(:),typed_intrados_tension_laws(:)
       type(shaped_panel_side_2d) :: typed_intrados_lower_side
       type(production_boundary_edge_2d) ::
     + typed_terminal_intrados_production_edge,
     + typed_terminal_intrados_reformatted_edge
       type(boundary_length_match_control) ::
     + typed_terminal_intrados_reformat_control
       type(preceding_join_support_2d) ::
     + typed_terminal_intrados_join_support,
     + typed_terminal_intrados_reformatted_join_support
       real*8, allocatable :: typed_terminal_intrados_offsets(:)
       type(quadrilateral_distances_3d), allocatable ::
     + typed_extrados_source_distances(:),typed_intake_source_distances(:),
     + typed_intrados_source_distances(:)
       logical :: domain_model_valid,panel_zero_active,rib_parity_consistent,
     + neutral_gap_valid,typed_coordinates_match
       character(len=256) :: domain_model_message
       real*8 :: typed_geometry_metric,typed_skin_tension_offset

       real*8 hol(0:100,200,20),skin(10,10)

       real*8 xsob(10),ysob(10)

       real*8 xupp, xupple, xuppte, xlow, xlowle, xlowte, xrib, xvrib
       real*8 xlowsaved

       real*8 brake(0:100,10)

       integer mc(10,100,50), cam(10), corda(500,50)

       integer cordam, cordat, t

       real*8 xcorda(500,5), ycorda(500,5), zcorda(500,5), raml(10,5)

       real*8 x1line(10,5,100),y1line(10,5,100),z1line(10,5,100)
       
       real*8 x2line(10,5,100),y2line(10,5,100),z2line(10,5,100)

       real*8 phi1(10,5,100),phi2(10,5,100),phi0(10,5,100)

       real*8 calag,cple,hcp,hcpp,assiette,afinesse,aoa,finesse,planeig
       real*8 assiette2003
       real*8 calage,cpress,clengr,clengl,clengk,clengb

       real*8 zcontrol, csusl, control

       real*8 acit,xci,yci,aci,cdgx,cdgy,cdg,xpoi,xdis

       real*8 dist,dist1,cdl,cdm,cdn

       real*8 xkar,ykar,zkar

       real*8 farea,parea,fparea,fspan,pspan,faratio,paratio

       real*8 comp1(10),comp2(10), xline(500),  xline2(500)

       real*8 hvr(0:200,50)

       real*8 ucnt(0:500,10,20), vcnt(0:500,10,20)

       real*8 ucnt1(0:100,10,500), vcnt1(0:100,10,500)
       real*8 ucnt2(0:100,10,500), vcnt2(0:100,10,500)
       real*8 ucnt3(0:100,10,500), vcnt3(0:100,10,500)
       real*8 ucnt4(0:100,10,500), vcnt4(0:100,10,500)

       integer jcon(0:200,10,500)
       integer jcon2(0:200,10,500),jcon4(0:200,10,500)
       integer jcon9(0:200,10,500),jcon11(0:200,10,500)

c      Sections 15 and 16 (colors)
!
!      Classic records are rib based:
!        npc1* = rib number, npc2* = boundaries on that rib,
!        npc3* = boundary identifier, xpc1* = chord percentage, and
!        xpc2* = nonzero only for historical off-rib marks.
!      The same boundary identifier on ribs i and i+1 defines one straight
!      open division across developed panel i.  `e` selects extrados and `i`
!      selects intrados.  Panel construction is emitted after skin tension has
!      populated developed-edge coordinate slots u/v(:,:,9:10).
       integer npce, npc1e(100), npc2e(100), npc3e(100,100)
       integer npci, npc1i(100), npc2i(100), npc3i(100,100)
       real*8 xpc1e(100,100), xpc2e(100,100) 
       real*8 xpc1i(100,100), xpc2i(100,100)

       integer npace, npac1e(100), npac2e(100), npac3e(100,100)
       integer npaci, npac1i(100), npac2i(100), npac3i(100,100)
       real*8 xpac1e(100,100), xpac2e(100,100) 
       real*8 xpac1i(100,100), xpac2i(100,100)
       real*8 xpac3e(100,100), xpac4e(100,100) 
       real*8 xpac3i(100,100), xpac4i(100,100)

!      Matching marks on both color pieces are moved this distance inward from
!      their allowance cut edges.  Millimetres match the section-6 input unit.
       real*8, parameter :: color_mark_inset_mm=1.1d0

       real*8 xarp(10), yarp(10)

       real*8 hdist(0:100), hangle(0:100)

       real*8 xtri(50),ytri(50)

       real*8 csus(10,10), cdis(10,10)

       real*8 aload(100,10), xload(500), xlide(500), xlifi(500)
       real*8 lvcx(0:500,500),lvcy(0:500,500)
       real*8 rvcx(0:500,500),rvcy(0:500,500)

       real*8 anccont(0:100,10),ancconti(0:100,10)

       real*8 bd(10,10)

       real*8 xpt1,ypt1,zpt1,xpt2,ypt2,zpt2,xpt3,ypt3,zpt3
       real*8 xpt4,ypt4,zpt4,xpt6,ypt6,zpt6

       integer slpi(10), slp

       real*8 xlin1(5000),ylin1(5000)
       real*8 xlin3(5000),ylin3(5000)

       real*8 xru(3),xrv(3),xsu(3),xsv(3)

       real*8 xtu2(100),xtv2(100),xtu4(100),xtv4(100)
       real*8 xtu9(100),xtv9(100),xtu11(100),xtv11(100)

       real*8 xlte11(100),xl911(100),xlle9(100)
       real*8 xrte11(100),xr911(100),xrle9(100)
       real*8 xc24(100)

c       real*8 px9i(300),py9i(300)
       real*8 px9o(500),py9o(500)

       real*8 xanchor(100,6),yanchor(100,6)
       real*8 xanchoril(0:100,6),yanchoril(0:100,6)
       real*8 xanchorir(0:100,6),yanchorir(0:100,6)

       real*8 xprb(0:100,6,0:10),yprb(0:100,6,0:10)
       integer jconi(6),jconf(6)
       real*8 xkprb(6)
       real*8 jcve(100),jcvi(100)
       real*8 xirl(0:500),xirr(0:500),jirl(0:500),jirr(0:500)
       real*8 distee(0:500),anglee(0:500),siu(0:500),siv(0:500)
       real*8 alprom,xdu,xdv,xpo1,xpo2,ypo1,ypo2

c       real*8 xlll,xrrr,yrrr,ylll
       real*8 xgir
       real*8 xpos,ypos,xpx2,xpy2,xr,xs,xrm,xsm
       real*8 xequis,yequis, xth1, xth2
       real*8 xlabel, zlabel, clli, varrow

       integer typepoint,typetab,typevent,typejonc
       integer iccolor(50),ele3d(50),ele3dc(50),iccolor34(500)
       real*8 xrad

c      Section 21

       integer ngo(100,3)
       real*8 xextra(0:100,10),xintra(0:100,10)
       real*8 xjonc(0:100,0:500,10),sjo(0:100,10)
       real*8 xjonc2(0:100,0:500,10),sjo2(0:100,10)

       integer k21blocs,k21blocf(20,10)
       integer ngoo(20,100,3)
       real*8 sjoo(20,100,10)
       real*8 xextraa(20,0:100,10),xintraa(20,0:100,10)
       real*8 x21(20,100,20)
       real*8 joncf(0:100,20,100,10)

c      Section 22
       real*8 xmy(0:100,10)

c      Section 27
       real*8 alc1, alc2

c      Section 28

       real*8 p28(10), a128, a228, lini28(100,10), lfin28(100,10)
       real*8 alpha28(100),calagnew(100)
       real*8 cnewtps(100),cnewcms(100),cnewtpt(100),cnewcmt(100)
       integer n128, n228

c      Section 29

       integer k29d,k29dd,ncuts(200),ini29(200),fin29(200)

       real*8  cutamp(10),cut29(0:10,200)
       real*8 s,sm,haut

       integer iupp(10,10,200),ilow(10,10,200)
       integer xiupp(10,10,200),xilow(10,10,200)
       real*8 kiupp(10,200),kilow(10,200)
       integer uppcuts(200),upptype(200),lowcuts(200),lowtype(200)
       real*8 hautok(0:100,500), zinf(0:100,10,10)
       integer pp29(10,10) ! Print parameters

c      Subroutines used in section 29: planeby123,pointp

       real*8 punt0(3),punt1(3),punt2(3),punt3(3),punt4(3),unitv(3)
       real*8 aplane,bplane,cplane,dplane,xt,dp0

       real*8 csi(0:100,60) ! Brute force in 8.5.5/drwvent
     
c      Section 31
       integer ksk,ng,ngi,ngskt,nribini,nribfin,npoints,ntype31
       integer skinpoints(0:100),ngroup31(0:100),ntypei31(0:100)

c       real*8 xxx,yyy,hhh,a128,a1281,a228,a2282
       real*8 xxx,yyy,hhh,a1281,a2282
       real*8 skinew(101,10), skinnew(0:100,101,10)
       real*8 xsobnew(0:100,101),ysobnew(0:100,101)
       real*8 xmida1,xmida2,xmida3,xmida4
       real*8 xpoint(101),ypoint(101),xvalue,yvalue
       real*8 xx1,xx2,yy1,yy2,zz1,zz2
       real*8 atext,htext

c      Section 32
       real*8 panel_x_coe,panel_x_min,panel_y_coe,rib_x_coe,rib_y_coe
       real*8 rib_1_coe
       real*8 jonc_x_coe,jonc_y_coe,jonc_i_coe
       real*8 section32_unused
       real*8 sepjox,sepjoy,sepxjo,sepyjo

c      Section 33
       integer k33d, k33t, nrisers
       real*8  x33(0:10)

c      Section 34
       integer k34d, k34n, jltype(0:50)
       real*8 tyline(0:50,20),rvalue,lentype(0:50),lentypewl(0:50)

       character*30 tylinea(0:50,20)
       
c      Section 35
       integer k35d, k35dl, k35lm
       real*8  x35ro,x35mu,x35v,x35alpha,x35cl,x35cd,x35cm
       real*8  x35slines,x35dlcoef,x35spilot,x35dpcoef
       real*8  x35wingmass,x35pilotmass,x35linesmass
       real*8  x35ycp,x35zcp,x35ypp,x35vh,x35vv,x35qlmass
       real*8  x35wl,x35wd,x35twl,x35twd,x35wlp,x35wdp
       real*8  x35linesd,x35pilotd
       real*8  x35oswald,x35lambda,x35xi,x35inducedd

       real*8  x35gamma,x35theta,x35zero,x35zeroprev
       real*8  x35_a,x35_b,x35_c,x35_d,x35_e,x35_f,x35_i,x35_j
       real*8  x35_up,x35_down,x35_left,x35_right

       real*8  x35cle,x35cde,x35finesse,x35_k1,x35_k2,x35_k
       real*8  x35_k1fi,x35_k2fi
       real*8  x35gammai,x35vi,x35gammafi

       real*8  x35py,x35dly,x35dlz

       real*8 x35dragextra,x35_k1d,x35_k2d,x35liftextra
       real*8 x35grav,x35pmc,x35totald
       real*8 x35pmcx,x35pmcy,x35pmcz ! pilot mass center
       real*8 x35qlmcx,x35qlmcy,x35qlmcz ! ql mass center
       real*8 x35totmcx,x35totmcy,x35totmcz ! total mass center
       real*8 x35distwcp,x35kd

       real*8 x35m1contr,x35m2contr,x35m3contr,x35m4contr,x35m5contr  ! convergence control
       real*8 x355_a,x355_b,x355_c,x355_d
       real*8 x35linesdf
       real*8 x35var(100) ! auxiliar real variables

c      Internal section 4.6
       real*8 chordsum, chord_pcdg, chordy
       real*8 wcdgx3d,wcdgy3d,wcdgz3d

c      Internal section 14.9
       real*8 xlimo(0:100,0:50) ! Lines more properties
       real*8 xlimog(0:50) ! General properties

c      Section 37
       character*30 a37date
       real*8 c3001(20) ! Miniribs values
       integer i3001(20),icode37(250),ellipse_segments
       integer i37_2005,i37_2006

c      Sections 38 and 39
       type(hvr_hole_config) hvr_holes
       type(hvr_hole_spec) hvr_hole
       type(hvr_position_config) hvr_positions
       logical hvr_sections_present,hvr_hole_found
       character*160 hvr_config_error
       integer hvr_config_status,hvr_hole_index
       integer hvr_gap_start,hvr_gap_end,hvr_edge_start,hvr_openings
       real*8 hvr_span_fraction,hvr_hmax,hvr_hole_height

c      xwimp

       character*100 makedirectory
       character*100 command_shell
       integer directory_status
       real*8 xdihedral(0:100)

c      Section 4.20 control parameters
       real*8 amplemig,amplerix,ampleriy,amplepix

c      Especial vents
       real*8 lvalp,lv1u,lv1v,lv2u,lv2v,lv3u,lv3v,lv4u,lv4v

c      Geometryc subroutines

c      l,m,n director cosinus       
       real*8 lcosd(3),mcosd(3),ncosd(3)

c      Parameter panels impression

       real*8 ysautt,ysaut

c      Side linghts in panels, use for equidistant marks

       real*8 llarl(0:100,3,100),llarr(0:100,3,100)
       real*8 xinil,xinir
       real*8 xfinl(0:100,3,100),xfinr(0:100,3,100)
       integer iq

       integer ich ! i control header
       character*72 lepuser 

c      Subroutine remapcont
       integer npini,npfin,npobj
       real*8 ucont(0:100,500),vcont(0:100,500)

c      Subroutine elliquad
       real*8 pgx(100),pgy(100) ! generic points
    

c      Auuxiliar values
       real*8 xlen,ylen
       real*8 xyextra,xyintra,xyshift

       real*8 seppix(0:100)

c      Subroutine drwvent
       real*8 xpoly(500),ypoly(500),zpoly(500)
       real*8 x_poly,y_poly,z_poly,x_poly1,y_poly1,z_poly1,
     + x_poly2,y_poly2,z_poly2,xpolylen
       real*8 xlenl,xlenr,xlenlr,xlenrr

c      Subroutine drwvent
       real*8 distrel,distrel1,distrel2

c      Subroutine tessella
       real*8 tesse3d(3,0:100,500,99)

c      Subroutine interpseg (polyline interpolation)
       real*8 useg(1000),vseg(1000)
       real*8 lsegment,lsegmentc,ltotal,linter,ldif
       real*8 upoint(100),vpoint(100)

c      Absolute variables used in sections
       real*8 sectx(100,100)
       integer secti(100,100)
       character*50 secta(100,100)

c      Miniribs code (6.12.0)
       real*8 xminirib,xminimod,xminisup,xminiinf
       real*8 xminiloc,yminiloc
       real*8 xequise,yequise,xequisi,yequisi
       
       integer n_words
       integer, parameter   :: nlen=1000
       character (len=nlen) :: entireline
       character (len=100)  :: words(nlen)        

c      Mark drawing configuration is owned by leparagliding_mark_types.

c      Parameters
       real*8 pi
