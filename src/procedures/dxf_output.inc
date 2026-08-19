ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      24. GRAPHICAL SUBROUTINES
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE pointg (radius xcir)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Draw the configured construction-point symbol on DXF unit 20.
!! @param[in] xu Point X coordinate.
!! @param[in,out] xv Point Y coordinate; legacy cross mode reverses its sign.
!! @param[out] xcir Effective symbol radius derived from mark configuration.
!! @param[in] pointcolor AutoCAD color index.
!! @note Uses category 1 from `leparagliding_mark_types`.
       SUBROUTINE pointg(xu,xv,xcir,pointcolor)

       integer pointcolor

       real*8 pi,angle1,angle2,xu,xv,xcir

       pi=4.0d0*datan(1.0d0)
       xcir=0.1*typm2(1)

c      Case 1: Draw constructed points
       if (typm1(1).eq.1) then
       xv=-xv
c      Draw cross
       call line(xu-xcir,-(xv),xu+xcir,-(xv),pointcolor)
       call line(xu,-(xv-xcir),xu,-(xv+xcir),pointcolor)

c      Draw circle
       do l=1,8

       angle1=float(l-1)*2.*pi/8.
       xlu1=xcir*dcos(angle1)
       xlv1=xcir*dsin(angle1)
       angle2=float(l)*2.*pi/8.
       xlu2=xcir*dcos(angle2)
       xlv2=xcir*dsin(angle2)

       call line(xu+xlu1,-(xv+xlv1),xu+xlu2,-(xv+xlv2),pointcolor)

       end do
       end if

c      Case 2: Draw minicircles
       if (typm1(1).eq.2) then
       call dxf_circle_entity(xu,xv,xcir,"mcircles",pointcolor)
       end if

       return

       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE POINT 2D
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Write a configured POINT or small CIRCLE entity to DXF unit 20.
!! @param[in] x1 Point X coordinate.
!! @param[in] y1 Point Y coordinate in design coordinates.
!! @param[in] pointcolor AutoCAD color index.
!! @note The Y coordinate is negated for the 2D DXF convention.
       SUBROUTINE point(x1,y1,pointcolor)
c      line P1-P2

       real*8 x1,y1
       integer pointcolor,typepoint

       typepoint=typm4(1)

c       write (*,*) typepoint,lyname(4)

c      Euclidean point
       if (typepoint.eq.1) then
       call dxf_point_entity(x1,y1,"points",pointcolor)
       end if

c      Point defined as circle diameter 0.4 mm
       if (typepoint.eq.2) then
       call dxf_circle_entity(x1,y1,0.1*typm5(1),"mcircles",
     + pointcolor)
       end if

       return
       end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE POINTLAYER 2D
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Write a circle used as a point marker to a selected DXF layer.
!! @param[in] x1 Circle-center X coordinate.
!! @param[in] y1 Circle-center Y coordinate in design coordinates.
!! @param[in] xrad Circle radius.
!! @param[in] gname DXF layer name.
!! @param[in] pointcolor AutoCAD color index.
       SUBROUTINE poinc(x1,y1,xrad,gname,pointcolor)
c      Point as minicicle radius xrad in layer gname

       character*50 gname
       real*8 x1,y1,xrad
       integer pointcolor

c      Point defined as circle radius xrad (0.2 mm)
       call dxf_circle_entity(x1,y1,xrad,gname,pointcolor)

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE POINTLAYER 2D
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Write a Euclidean POINT entity to a selected DXF layer.
!! @param[in] x1 Point X coordinate.
!! @param[in] y1 Point Y coordinate in design coordinates.
!! @param[in] gname DXF layer name.
!! @param[in] pointcolor AutoCAD color index.
       SUBROUTINE poinl(x1,y1,gname,pointcolor)
c      Euclidean point in layer gname

       character*50 gname
       real*8 x1,y1
       integer pointcolor

       call dxf_point_entity(x1,y1,gname,pointcolor)

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE CIRCLE
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Write a general circle to DXF unit 20.
!! @param[in] x1 Circle-center X coordinate.
!! @param[in] y1 Circle-center Y coordinate in design coordinates.
!! @param[in] radc Circle radius.
!! @param[in] gname DXF layer name.
!! @param[in] pointcolor AutoCAD color index.
       SUBROUTINE circle(x1,y1,radc,gname,pointcolor)
c      Circle radius radc in layer gname

       character*50 gname
       real*8 x1,y1,radc
       integer pointcolor

       call dxf_circle_entity(x1,y1,radc,gname,pointcolor)

       return
       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE MTRIANGLE
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Draw an equilateral triangular mark on the `triangles` DXF layer.
!! @param[in] p1x X coordinate of the triangle reference vertex.
!! @param[in] p1y Y coordinate of the triangle reference vertex.
!! @param[in] xh Triangle height.
!! @param[in] xrot Rotation angle in radians.
!! @param[in] linecolor AutoCAD color index.
       SUBROUTINE mtriangle(p1x,p1y,xh,xrot,linecolor)
c      line P1-P2

       real*8 p1x,p1y

       real*8 xh,xrot,pi

       pi=4.0d0*datan(1.0d0)

c       write (*,*) i,xrot

       x1=p1x
       y1=p1y
       xx2=xh*dtan((pi/6.))
       yy2=xh
       xx3=-xh*dtan((pi/6.))
       yy3=xh

c      Rotation matrix in xrot
       x2=p1x+xx2*dcos(xrot)+yy2*sin(xrot)
       y2=p1y-xx2*dsin(xrot)+yy2*cos(xrot)
       x3=p1x+xx3*dcos(xrot)+yy3*sin(xrot)
       y3=p1y-xx3*dsin(xrot)+yy3*cos(xrot)

       write(20,'(A,/,I1,/,A)') "LINE",8,"triangles"
       write(20,'(I1,/,A)') 6,"CONTINUOUS"
       write(20,'(I2,/,F14.4,/,I2,/,F14.4)') 10,x1,20,-y1
       write(20,'(I2,/,F14.4,/,I2,/,F14.4)') 11,x2,21,-y2
       write(20,'(I2,/,I2,/,I2,/,I2,/,I2)') 39,0,62,linecolor,0

       write(20,'(A,/,I1,/,A)') "LINE",8,"triangles"
       write(20,'(I1,/,A)') 6,"CONTINUOUS"
       write(20,'(I2,/,F14.4,/,I2,/,F14.4)') 10,x2,20,-y2
       write(20,'(I2,/,F14.4,/,I2,/,F14.4)') 11,x3,21,-y3
       write(20,'(I2,/,I2,/,I2,/,I2,/,I2)') 39,0,62,linecolor,0
       
       write(20,'(A,/,I1,/,A)') "LINE",8,"triangles"
       write(20,'(I1,/,A)') 6,"CONTINUOUS"
       write(20,'(I2,/,F14.4,/,I2,/,F14.4)') 10,x3,20,-y3
       write(20,'(I2,/,F14.4,/,I2,/,F14.4)') 11,x1,21,-y1
       write(20,'(I2,/,I2,/,I2,/,I2,/,I2)') 39,0,62,linecolor,0

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE LINEVENT
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Write a 2D line segment on the `vents` DXF layer.
!! @param[in] p1x Horizontal coordinate of the first endpoint.
!! @param[in] p1y Vertical coordinate of the first endpoint.
!! @param[in] p2x Horizontal coordinate of the second endpoint.
!! @param[in] p2y Vertical coordinate of the second endpoint.
!! @param[in] linecolor AutoCAD color index.
       SUBROUTINE linevent(p1x,p1y,p2x,p2y,linecolor)
c      line P1-P2

       real*8 p1x,p1y,p2x,p2y

       write(20,'(A,/,I1,/,A)') "LINE",8,"vents"
       write(20,'(I1,/,A)') 6,"CONTINUOUS"
       write(20,'(I2,/,F14.4,/,I2,/,F14.4)') 10,p1x,20,-p1y
       write(20,'(I2,/,F14.4,/,I2,/,F14.4)') 11,p2x,21,-p2y
       write(20,'(I2,/,I2,/,I2,/,I2,/,I2)') 39,0,62,linecolor,0
       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE SEGMENT101
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Draw the first and last thirds of a segment, leaving its middle third open.
!! @param[in] p1x Horizontal coordinate of the first endpoint.
!! @param[in] p1y Vertical coordinate of the first endpoint.
!! @param[in] p2x Horizontal coordinate of the second endpoint.
!! @param[in] p2y Vertical coordinate of the second endpoint.
!! @param[in] linecolor AutoCAD color index.
!! @note Writes both pieces to the `segment101` layer on DXF unit 20.
       SUBROUTINE segment101(p1x,p1y,p2x,p2y,linecolor)
c      line P1-P2

       real*8 p1x,p1y,p2x,p2y

       x3=p1x+(p2x-p1x)/3.
       y3=p1y+(p2y-p1y)/3.
       x4=p1x+(p2x-p1x)*(2./3.)
       y4=p1y+(p2y-p1y)*(2./3.)

       write(20,'(A,/,I1,/,A)') "LINE",8,"segment101"
       write(20,'(I1,/,A)') 6,"CONTINUOUS"
       write(20,'(I2,/,F14.4,/,I2,/,F14.4)') 10,p1x,20,-p1y
       write(20,'(I2,/,F14.4,/,I2,/,F14.4)') 11,x3,21,-y3
       write(20,'(I2,/,I2,/,I2,/,I2,/,I2)') 39,0,62,linecolor,0

       write(20,'(A,/,I1,/,A)') "LINE",8,"segment101"
       write(20,'(I1,/,A)') 6,"CONTINUOUS"
       write(20,'(I2,/,F14.4,/,I2,/,F14.4)') 10,x4,20,-y4
       write(20,'(I2,/,F14.4,/,I2,/,F14.4)') 11,p2x,21,-p2y
       write(20,'(I2,/,I2,/,I2,/,I2,/,I2)') 39,0,62,linecolor,0

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE LINE 2D
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Write a 2D line segment to the default layer on DXF unit 20.
!! @param[in] p1x Horizontal coordinate of the first endpoint.
!! @param[in] p1y Vertical coordinate of the first endpoint.
!! @param[in] p2x Horizontal coordinate of the second endpoint.
!! @param[in] p2y Vertical coordinate of the second endpoint.
!! @param[in] linecolor AutoCAD color index.
       SUBROUTINE line(p1x,p1y,p2x,p2y,linecolor)
c      line P1-P2

       real*8 p1x,p1y,p2x,p2y

       write(20,'(A,/,I1,/,A)') "LINE",8,"default"
       write(20,'(I1,/,A)') 6,"CONTINUOUS"
       write(20,'(I2,/,F14.4,/,I2,/,F14.4)') 10,p1x,20,-p1y
       write(20,'(I2,/,F14.4,/,I2,/,F14.4)') 11,p2x,21,-p2y
       write(20,'(I2,/,I2,/,I2,/,I3,/,I2)') 39,0,62,linecolor,0
       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc        
c     SUBROUTINE LINE 3D
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Write a 3D line segment to the default layer on DXF unit 25.
!! @param[in] p1x X coordinate of the first endpoint.
!! @param[in] p1y Y coordinate of the first endpoint.
!! @param[in] p1z Z coordinate of the first endpoint.
!! @param[in] p2x X coordinate of the second endpoint.
!! @param[in] p2y Y coordinate of the second endpoint.
!! @param[in] p2z Z coordinate of the second endpoint.
!! @param[in] linecolor AutoCAD color index.
       SUBROUTINE line3d(p1x,p1y,p1z,p2x,p2y,p2z,linecolor)
c      line P1-P2

       real*8 p1x,p1y,p1z,p2x,p2y,p2z

       call dxf_line3d_entity(25,p1x,p1y,p1z,p2x,p2y,p2z,
     + linecolor)
       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc        
c     SUBROUTINE LINE 3D IN FILE NUMBER 26
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Write a 3D line segment to the auxiliary DXF stream on unit 26.
!! @param[in] p1x X coordinate of the first endpoint.
!! @param[in] p1y Y coordinate of the first endpoint.
!! @param[in] p1z Z coordinate of the first endpoint.
!! @param[in] p2x X coordinate of the second endpoint.
!! @param[in] p2y Y coordinate of the second endpoint.
!! @param[in] p2z Z coordinate of the second endpoint.
!! @param[in] linecolor AutoCAD color index.
       SUBROUTINE line3dn(p1x,p1y,p1z,p2x,p2y,p2z,linecolor)
c      line P1-P2

       real*8 p1x,p1y,p1z,p2x,p2y,p2z

       call dxf_line3d_entity(26,p1x,p1y,p1z,p2x,p2y,p2z,
     + linecolor)
       return
       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      POLYLINE 2D
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Write an open 2D POLYLINE entity to DXF unit 20.
!! @param[in] plx X coordinates of the vertices.
!! @param[in] ply Y coordinates of the vertices in DXF coordinates.
!! @param[in] nvertex Number of vertices to emit from the arrays.
!! @param[in] linecolor AutoCAD color index.
       SUBROUTINE poly2d(plx,ply,nvertex,linecolor)

       real*8 plx(500),ply(500)

       write (20,'(A,/,I1,/,I1,/,I2)') "POLYLINE",8,0,62
       write (20,'(I3,/,I2,/,I1)') linecolor,66,1
       write (20,'(I2,/,F3.1,/,I2,/,F3.1,/,I2,/,F3.1,/,I1)') 
     + 10,0.0,20,0.0,30,0.0,0
       
       do k=1,nvertex

       write (20,'(A,/,I1,/,I1,/,I2)') "VERTEX",8,0,62
       write (20,'(I3,/,I2,/,I1)') linecolor,66,1 
       write (20,'(I2,/,F9.3,/,I2,/,F9.3,/,I2,/,F9.3,/,I1)') 
     + 10,plx(k),20,ply(k),30,0.0,0

       end do
       
       write (20,'(A,/,I1,/,I1,/,I2)') "SEQEND",8,0,62
       write (20,'(I3,/,I1)') linecolor,0 

c       write (*,'(A,/,I1,/,I1,/,I2)') "POLYLINE",8,0,62
c       write (*,'(I3,/,I2,/,I1)') linecolor,66,1
c       write (*,'(I2,/,F3.1,/,I2,/,F3.1,/,I2,/,F3.1,/,I1)') 
c     + 10,0.0,20,0.0,30,0.0,0
       
c       do k=1,nvertex

c       write (*,'(A,/,I1,/,I1,/,I2)') "VERTEX",8,0,62
c       write (*,'(I3,/,I2,/,I1)') linecolor,66,1 
c       write (*,'(I2,/,F9.3,/,I2,/,F9.3,/,I2,/,F9.3,/,I1)') 
c     + 10,plx(k),20,ply(k),30,0.0,0

c       end do

c       write (*,'(A,/,I1,/,I1,/,I2)') "SEQEND",8,0,62
c       write (*,'(I3,/,I1)') linecolor,0 

       return

       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      ELLIPSE
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Approximate and draw a rotated ellipse with 39 line segments.
!! @param[in] u0 Horizontal coordinate of the ellipse center.
!! @param[in] v0 Vertical coordinate of the ellipse center.
!! @param[in] a First ellipse semiaxis.
!! @param[in] b Second ellipse semiaxis.
!! @param[in] tet0 Ellipse rotation in radians.
!! @param[in] linecolor AutoCAD color index.
       SUBROUTINE ellipse(u0,v0,a,b,tet0,linecolor)

       real*8 xe(500),ye(500)

       real*8 pi,u0,v0,a,b,tet,tet0

       real*8 p1x,p1y,p2x,p2y

       pi=4.0d0*datan(1.0d0)

       do ll=1,40

       tet=2.*pi*((float(ll)-1.)/39.)

c      write (*,*) ll,float(ll),tet," ",pi,"---"

       xe(ll)=u0+a*dcos(tet)*dcos(tet0)-b*dsin(tet)*dsin(tet0)
       ye(ll)=v0+a*dcos(tet)*dsin(tet0)+b*dsin(tet)*dcos(tet0)

       end do

       do ll=1,39

       p1x=xe(ll)
       p2x=xe(ll+1)
       p1y=ye(ll)
       p2y=ye(ll+1)

       call line(p1x,p1y,p2x,p2y,linecolor)

c       write (*,*) ll,tet*180./pi,xe(ll),ye(ll)

       end do

       return

       end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE ROMANO
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Draw a Romano-style sequence of point marks for an integer label.
!! @param[in] rn Integer value encoded by the mark sequence.
!! @param[in] rx0 Horizontal starting position in design coordinates.
!! @param[in] ry0 Vertical starting position in design coordinates.
!! @param[in] ralp Baseline angle in radians.
!! @param[in] requi Spacing between symbol positions.
!! @param[in] rcolor AutoCAD color index.
       SUBROUTINE romano(rn,rx0,ry0,ralp,requi,rcolor)

       integer rn, rn1, rn2, rn3, rcolor
       real*8 rx0, ry0, ralp, requi, dx
        
c      AQUI ES EL PROBLEMA!!!!!!!!!!!!!!!!!!!!!!!!!!!
c      Points separation in romano

c      requi=0.350d0

       dx=0.0d0

       rn1=int(float(rn)/10)
       rn2=int((float(rn)-10*float(rn1))/5)
       rn3=rn-10*rn1-5*rn2

c      write (*,*) "rn1= ", rn, rn1, rn2, rn3

       do i=1,rn1

       call point(rx0+dx*dcos(ralp),ry0-dx*dsin(ralp),rcolor)
       call point(rx0+dx*dcos(ralp)+requi*dsin(ralp),
     + ry0-dx*dsin(ralp)+requi*dcos(ralp),rcolor)
       dx=dx+requi
       call point(rx0+dx*dcos(ralp),ry0-dx*dsin(ralp),rcolor)
       call point(rx0+dx*dcos(ralp)+requi*dsin(ralp),
     + ry0-dx*dsin(ralp)+requi*dcos(ralp),rcolor)
       dx=dx+requi

       end do

       do i=1,rn2

       call point(rx0+dx*dcos(ralp),ry0-dx*dsin(ralp),rcolor)
       call point(rx0+(dx+0.5d0*requi)*dcos(ralp)+requi*dsin(ralp),
     + ry0-(dx+0.5d0*requi)*dsin(ralp)+requi*dcos(ralp),rcolor)
       dx=dx+requi
       call point(rx0+dx*dcos(ralp),ry0-dx*dsin(ralp),rcolor)
       dx=dx+requi

       end do

       do i=1,rn3

       call point(rx0+dx*dcos(ralp),ry0-dx*dsin(ralp),rcolor)
       dx=dx+requi

       end do

       return

       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE TEXT
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Write a character-string TEXT entity to DXF unit 20.
!! @param[in] p1x Horizontal coordinate of the text insertion point.
!! @param[in] p1y Vertical coordinate of the text insertion point.
!! @param[in] htext Text height.
!! @param[in] atext Text rotation in DXF angle units (degrees).
!! @param[in] xtext Fixed-width text to write.
!! @param[in] txtcolor AutoCAD color index.
       SUBROUTINE txt(p1x,p1y,htext,atext,xtext,txtcolor)
c      line P1-P2

       real*8 atext,htext,p1x,p1y
       character*50 xtext
       integer txtcolor

       call dxf_text_header(p1x,p1y,htext,txtcolor)
       write(20,'(I2,/,A50)') 1, xtext
       call dxf_text_footer(atext)

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE ITEXT
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Write an integer TEXT entity using a twelve-column integer field.
!! @param[in] p1x Horizontal coordinate of the text insertion point.
!! @param[in] p1y Vertical coordinate of the text insertion point.
!! @param[in] htext Text height.
!! @param[in] atext Text rotation in degrees.
!! @param[in] itext Integer label value.
!! @param[in] txtcolor AutoCAD color index.
       SUBROUTINE itxt(p1x,p1y,htext,atext,itext,txtcolor)
c      line P1-P2

       real*8 atext,htext,p1x,p1y
       integer itext, txtcolor

       call dxf_text_header(p1x,p1y,htext,txtcolor)
       write(20,'(I2,/,I12)') 1, itext
       call dxf_text_footer(atext)

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE ITEXT2
c      Only format two digits
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Write an integer TEXT entity using the legacy two-column field.
!! @param[in] p1x Horizontal coordinate of the text insertion point.
!! @param[in] p1y Vertical coordinate of the text insertion point.
!! @param[in] htext Text height.
!! @param[in] atext Text rotation in degrees.
!! @param[in] itext Integer label value, normally limited to two digits.
!! @param[in] txtcolor AutoCAD color index.
       SUBROUTINE itxt2(p1x,p1y,htext,atext,itext,txtcolor)
c      line P1-P2

       real*8 atext,htext,p1x,p1y
       integer itext, txtcolor

       call dxf_text_header(p1x,p1y,htext,txtcolor)
       write(20,'(I2,/,I2)') 1, itext
       call dxf_text_footer(atext)

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Private DXF entity writers shared by the public drawing routines.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Emit the common DXF records for a POINT entity on unit 20.
!! @param[in] x1 Horizontal point coordinate.
!! @param[in] y1 Vertical point coordinate, converted to the DXF convention.
!! @param[in] gname DXF layer name.
!! @param[in] pointcolor AutoCAD color index.
!! @private
       SUBROUTINE dxf_point_entity(x1,y1,gname,pointcolor)
       implicit none
       character*(*) gname
       real*8 x1,y1
       integer pointcolor

       write(20,'(A,/,I1,/,A)') "POINT",8,gname
       write(20,'(I1,/,A)') 6,"CONTINUOUS"
       write(20,'(I2,/,F12.2,/,I2,/,F12.2)') 10,x1,20,-y1
       write(20,'(I2,/,I2,/,I2,/,I3,/,I2)') 39,0,62,pointcolor,0
       end

!> Emit the common DXF records for a CIRCLE entity on unit 20.
!! @param[in] x1 Horizontal center coordinate.
!! @param[in] y1 Vertical center coordinate, converted to the DXF convention.
!! @param[in] xrad Circle radius.
!! @param[in] gname DXF layer name.
!! @param[in] pointcolor AutoCAD color index.
!! @private
       SUBROUTINE dxf_circle_entity(x1,y1,xrad,gname,pointcolor)
       implicit none
       character*(*) gname
       real*8 x1,y1,xrad
       integer pointcolor

       write(20,'(A,/,I1,/,A)') "CIRCLE",8,gname
       write(20,'(I1,/,A)') 6,"CONTINUOUS"
       write(20,'(I2,/,F12.2,/,I2,/,F12.2)') 10,x1,20,-y1
       write(20,'(I2,/,F12.3,/,I2,/,I3,/,I2)') 40,xrad,62,
     + pointcolor,0
       end

!> Emit the common DXF records for a 3D LINE entity.
!! @param[in] nunit Open Fortran unit receiving the entity.
!! @param[in] p1x X coordinate of the first endpoint.
!! @param[in] p1y Y coordinate of the first endpoint.
!! @param[in] p1z Z coordinate of the first endpoint.
!! @param[in] p2x X coordinate of the second endpoint.
!! @param[in] p2y Y coordinate of the second endpoint.
!! @param[in] p2z Z coordinate of the second endpoint.
!! @param[in] linecolor AutoCAD color index.
!! @private
       SUBROUTINE dxf_line3d_entity(nunit,p1x,p1y,p1z,p2x,p2y,p2z,
     + linecolor)
       implicit none
       integer nunit,linecolor
       real*8 p1x,p1y,p1z,p2x,p2y,p2z

       write(nunit,'(A,/,I1,/,A)') "LINE",8,"default"
       write(nunit,'(I1,/,A)') 6,"CONTINUOUS"
       write(nunit,'(I2,/,F8.3,/,I2,/,F8.3,/,I2,/,F8.3)')
     + 10,p1x,20,p1y,30,p1z
       write(nunit,'(I2,/,F8.3,/,I2,/,F8.3,/,I2,/,F8.3)')
     + 11,p2x,21,p2y,31,p2z
       write(nunit,'(I2,/,I2,/,I2,/,I3,/,I2)') 39,0,62,
     + linecolor,0
       end

!> Emit the records common to all TEXT entities before their value record.
!! @param[in] p1x Horizontal coordinate of the text insertion point.
!! @param[in] p1y Vertical coordinate of the text insertion point.
!! @param[in] htext Text height.
!! @param[in] txtcolor AutoCAD color index.
!! @private
       SUBROUTINE dxf_text_header(p1x,p1y,htext,txtcolor)
       implicit none
       real*8 p1x,p1y,htext
       integer txtcolor

       write(20,'(A,/,I1,/,A)') "TEXT",5,"10A38"
       write(20,'(I1,/,I1)') 8,0
       write(20,'(I1,/,A)') 6,"CONTINUOUS"
       write(20,'(I2,/,I3)') 62,txtcolor
       write(20,'(I2,/,F12.2,/,I2,/,F12.2)') 10,p1x,20,-p1y
       write(20,'(I2,/,F12.2)') 30,0.0
       write(20,'(I2,/,F12.2)') 40,htext
       end

!> Emit the rotation and terminator records for a TEXT entity.
!! @param[in] atext Text rotation in degrees.
!! @private
       SUBROUTINE dxf_text_footer(atext)
       implicit none
       real*8 atext

       write(20,'(I2,/,F12.2,/I1)') 50,atext,0
       end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      DXF init
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
!> Start a minimal DXF document and open its ENTITIES section.
!! @param[in] nunit Open Fortran output unit.
!! @note The historical fixed drawing extents are preserved.
       SUBROUTINE dxfinit(nunit)
       
       write(nunit,'(I1,/,A,/,I1)') 0,"SECTION",2
       write(nunit,'(A)') "HEADER"
       write(nunit,'(I1,/,A)') 9,"$EXTMAX"
       write(nunit,'(I2,/,F12.3,/,I2,/,F12.3)') 10,-670.,20,-3630.
       write(nunit,'(I1,/,A)') 9,"$EXTMIN"
       write(nunit,'(I2,/,F12.3,/,I2,/,F12.3)') 10,7000.,20,120.
       write(nunit,'(I1,/,A,/,I1)') 0,"ENDSEC",0
       write(nunit,'(A,/,I1)') "SECTION",2
       write(nunit,'(A,/,I1)') "ENTITIES",0

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      DXF end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Close the ENTITIES section and terminate a DXF document.
!! @param[in] nunit Open Fortran output unit initialized by `dxfinit`.
       SUBROUTINE dxfend(nunit)

       write(nunit,'(A,/,I1,/,A)') "ENDSEC",0,"EOF"
       return
       end

 
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE vectors redistribution             c
c                                                    c
c      Read polyline (xlin1,ylin1) in n1vr points    c
c      and redistribute in polyline                  c
c      (xlin3,ylin3) in n2vr points                  c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc

