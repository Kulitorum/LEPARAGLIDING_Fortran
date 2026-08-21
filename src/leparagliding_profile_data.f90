!> Legacy-compatible profile input and auxiliary-profile transformations.
!!
!! This module owns the complete executable group formerly compiled from
!! `procedures/profile_data.inc`.  Arithmetic kinds, literal kinds, expression
!! ordering, unit 24 input, and diagnostic output remain compatible with the
!! fixed-form implementation while callers gain explicit module interfaces.
module leparagliding_profile_data
  use, intrinsic :: iso_fortran_env, only : real64
  implicit none
  private

  public :: datair
  public :: remapcont
  public :: xyzt

contains

  !> Read and prepare the profile assigned to one rib from open unit 24.
  !!
  !! @param[in] i Rib index receiving the profile.
  !! @param[in] rib Rib settings, including inlet start/end percentages.
  !! @param[in,out] np Profile topology. On success columns 1--5 contain total
  !! points, extrados endpoint, intake count, intrados count, and the shared
  !! intake/intrados endpoint.
  !! @param[in,out] u Profile horizontal coordinates; slot 1 is populated and
  !! may be adjusted to place inlet-boundary points exactly.
  !! @param[in,out] v Profile vertical coordinates; slot 1 is populated and
  !! may be adjusted to place inlet-boundary points exactly.
  !! @note The routine rewinds unit 24 and preserves the historical verbose
  !! diagnostics. Input is buffered for at most 1,000 points before the caller
  !! arrays are modified.
  subroutine datair(i, rib, np, u, v)
    integer, intent(in) :: i
    real(real64), intent(in) :: rib(0:100, 500)
    integer, intent(inout) :: np(0:100, 9)
    real(real64), intent(inout) :: u(0:100, 500, 99)
    real(real64), intent(inout) :: v(0:100, 500, 99)

    integer :: npini, npfin, npobj
    integer :: jkini, jkfin, read_count, point_capacity
    integer :: ini_action, fin_action, ini_source_index, fin_source_index
    integer :: ini_final_index, fin_final_index, insertion_count
    integer :: output_index, j, io
    real(real64) :: xini, xfin, yini, yfin, d0, d1, d2, d3, dm
    real(real64) :: point_u, point_v, boundary_tolerance, xm, xb
    real(real64) :: ucont(500), vcont(500)
    real(real64) :: read_u(1000), read_v(1000)
    logical :: ini_segment

    rewind (24)
    read (24, *)

    read_count = 0
    do 100 j = 1, 1000
      read (24, *, iostat=io) read_u(j), read_v(j)
      if (io < 0) goto 200
      if (io > 0) then
        write (*, *) "ERROR: malformed profile coordinate at line", j + 1
        stop 1
      end if

      read_count = read_count + 1
      write (*, *) i, read_count, read_u(j), read_v(j)
100 continue
200 continue

    point_capacity = ubound(u, 2)
    if (read_count > point_capacity) then
      write (*, *) "ERROR: profile has", read_count, "points; capacity is", &
          point_capacity
      stop 1
    end if

    write (*, *) "Num punts initial", read_count

    xini = rib(i, 11) / 100.
    xfin = rib(i, 12) / 100.
    jkini = 0
    jkfin = 0
    ini_action = 0
    fin_action = 0
    ini_source_index = 0
    fin_source_index = 0
    yini = 0.0d0
    yfin = 0.0d0

    do j = 2, read_count - 2
      ini_segment = .false.
      if (read_u(j) <= xini .and. read_u(j + 1) > xini .and. &
          read_v(j) <= 0.0d0 .and. rib(i, 11) >= 0.0d0) then
        ini_segment = .true.
      end if
      if (read_u(j) >= xini .and. read_u(j + 1) < xini .and. &
          read_v(j) > 0.0d0 .and. rib(i, 11) < 0.0d0) then
        ini_segment = .true.
      end if

      if (ini_segment) then
        jkini = j
        write (*, *) "jkini= ", j

        d0 = dsqrt((read_u(j) - read_u(j - 1))**2. + &
            (read_v(j) - read_v(j - 1))**2.)
        xm = (read_v(j + 1) - read_v(j)) / (read_u(j + 1) - read_u(j))
        xb = read_v(j) - xm * read_u(j)
        yini = xm * xini + xb
        d1 = dsqrt((xini - read_u(j))**2. + (yini - read_v(j))**2.)
        d2 = dsqrt((xini - read_u(j + 1))**2. + &
            (yini - read_v(j + 1))**2.)
        d3 = dsqrt((read_u(j + 2) - read_u(j + 1))**2. + &
            (read_v(j + 2) - read_v(j + 1))**2.)
        dm = (d0 + d1 + d2 + d3) / 3.0d0
        boundary_tolerance = dm / 5.0d0

        write (*, *) "Ep ini"
        write (*, *) read_u(j), xini
        write (*, *) read_v(j), yini
        write (*, *) d0, d1, d2, d3, dm

        if (d1 < boundary_tolerance .and. d1 <= d2) then
          ini_action = 1
          ini_source_index = j
        else if (d2 < boundary_tolerance) then
          ini_action = 2
          ini_source_index = j + 1
        else
          ini_action = 3
          ini_source_index = 0
        end if
      end if

      if (read_u(j) <= xfin .and. read_u(j + 1) > xfin .and. &
          read_v(j) <= 0.0d0) then
        jkfin = j
        write (*, *) "jkfin= ", j

        d0 = dsqrt((read_u(j) - read_u(j - 1))**2. + &
            (read_v(j) - read_v(j - 1))**2.)
        xm = (read_v(j + 1) - read_v(j)) / (read_u(j + 1) - read_u(j))
        xb = read_v(j) - xm * read_u(j)
        yfin = xm * xfin + xb
        d1 = dsqrt((xfin - read_u(j))**2. + (yfin - read_v(j))**2.)
        d2 = dsqrt((xfin - read_u(j + 1))**2. + &
            (yfin - read_v(j + 1))**2.)
        d3 = dsqrt((read_u(j + 2) - read_u(j + 1))**2. + &
            (read_v(j + 2) - read_v(j + 1))**2.)
        dm = (d0 + d1 + d2 + d3) / 3.0d0
        boundary_tolerance = dm / 5.0d0

        write (*, *) "Ep fin"
        write (*, *) read_u(j), xfin
        write (*, *) read_v(j), yfin
        write (*, *) d0, d1, d2, d3, dm

        if (d1 < boundary_tolerance .and. d1 <= d2) then
          fin_action = 1
          fin_source_index = j
        else if (d2 < boundary_tolerance) then
          fin_action = 2
          fin_source_index = j + 1
        else
          fin_action = 3
          fin_source_index = 0
        end if
      end if
    end do

    if (jkini == 0 .or. jkfin == 0 .or. ini_action == 0 .or. &
        fin_action == 0) then
      write (*, *) "ERROR: unable to locate profile inlet boundaries"
      stop 1
    end if

    if (ini_action /= 3 .and. fin_action /= 3 .and. &
        ini_source_index == fin_source_index .and. &
        dabs(xini - xfin) > 1.0d-12) then
      write (*, *) "ERROR: inlet boundaries collapse onto one profile point"
      stop 1
    end if

    insertion_count = 0
    if (ini_action == 3) insertion_count = insertion_count + 1
    if (fin_action == 3) insertion_count = insertion_count + 1
    if (read_count + insertion_count > point_capacity) then
      write (*, *) "ERROR: prepared profile has", &
          read_count + insertion_count, "points; capacity is", point_capacity
      stop 1
    end if

    output_index = 0
    ini_final_index = 0
    fin_final_index = 0

    do j = 1, read_count
      point_u = read_u(j)
      point_v = read_v(j)

      if (ini_action /= 3 .and. j == ini_source_index) then
        point_u = xini
        point_v = yini
      end if
      if (fin_action /= 3 .and. j == fin_source_index) then
        point_u = xfin
        point_v = yfin
      end if

      output_index = output_index + 1
      u(i, output_index, 1) = point_u
      v(i, output_index, 1) = point_v
      if (ini_action /= 3 .and. j == ini_source_index) then
        ini_final_index = output_index
      end if
      if (fin_action /= 3 .and. j == fin_source_index) then
        fin_final_index = output_index
      end if

      if (ini_action == 3 .and. j == jkini) then
        output_index = output_index + 1
        u(i, output_index, 1) = xini
        v(i, output_index, 1) = yini
        ini_final_index = output_index
      end if
      if (fin_action == 3 .and. j == jkfin) then
        output_index = output_index + 1
        u(i, output_index, 1) = xfin
        v(i, output_index, 1) = yfin
        fin_final_index = output_index
      end if
    end do

    if (ini_final_index < 1 .or. fin_final_index <= ini_final_index) then
      write (*, *) "ERROR: inlet boundary order is invalid"
      stop 1
    end if

    np(i, 1) = output_index
    np(i, 2) = ini_final_index
    np(i, 5) = fin_final_index
    np(i, 3) = np(i, 5) - np(i, 2) + 1
    np(i, 4) = np(i, 1) - np(i, 5) + 1

    write (*, *) "np(i,1)= ", np(i, 1)
    write (*, *) "np(i,2)= ", np(i, 2)
    write (*, *) "np(i,3)= ", np(i, 3)
    write (*, *) "np(i,4)= ", np(i, 4)

    npini = 1
    npfin = np(i, 2)
    npobj = 66
    do j = npini, npfin
      ucont(j) = u(i, j, 1)
      vcont(j) = v(i, j, 1)
    end do

    call remapcont(npini, npfin, npobj, ucont, vcont)
  end subroutine datair

  !> Analyze the spacing law for a proposed contour remapping.
  !!
  !! @param[in] npini First contour point index.
  !! @param[in] npfin Last contour point index.
  !! @param[in] npobj Requested number of points in the proposed remapping.
  !! @param[in] ucont Source contour horizontal coordinates.
  !! @param[in] vcont Source contour vertical coordinates.
  !! @note This legacy diagnostic calculates and prints spacing arrays but does
  !! not write resampled coordinates back to `ucont` or `vcont`.
  subroutine remapcont(npini, npfin, npobj, ucont, vcont)
    integer, intent(in) :: npini, npfin, npobj
    real(real64), intent(in) :: ucont(500), vcont(500)

    real(real64) :: dx(500), dy(500), da(0:500), ds(500)
    real(real64) :: dxn(0:500), dyn(0:500), dsn(0:500), dan(0:500)
    real(real64) :: xl, dm, dmn, xln
    integer :: j, k

    xl = 0.0d0
    da(npini - 1) = 0.0d0

    do j = npini, npfin - 1
      xl = xl + dsqrt((ucont(j) - ucont(j + 1))**2.0 + &
          (vcont(j) - vcont(j + 1))**2.0)
    end do

    dm = xl / dfloat(npfin - npini)

    write (*, *) "Ep reformat"
    write (*, *) npini, npfin

    do j = npini, npfin - 1
      dy(j) = dsqrt((ucont(j) - ucont(j + 1))**2. + &
          (vcont(j) - vcont(j + 1))**2.) / dm
      ds(j) = dsqrt((ucont(j) - ucont(j + 1))**2.0 + &
          (vcont(j) - vcont(j + 1))**2.0)
      da(j) = da(j - 1) + ds(j)
      dx(j) = da(j - 1) / xl
      write (*, *) j, ds(j) * 100., da(j) * 100., dx(j)
    end do
    dx(npfin) = 1.0d0
    dy(npfin) = dy(npfin - 1)
    write (*, *) xl * 100.

    write (*, *) "Law of points distribution"
    do j = npini, npfin
      write (*, *) j, dx(j), dy(j)
    end do

    dmn = 1.0d0 / dfloat(npobj - 1)
    dan(0) = 0.0d0
    dsn(0) = 0.0d0
    xln = 0.0d0

    do k = 1, npobj - 1
      dxn(k) = dmn * dfloat(k - 1)

      do j = npini, npfin
        if (dxn(k) >= dx(j) .and. dxn(k) < dx(j + 1)) then
          dyn(k) = dy(j)
        end if
      end do

      dsn(k) = dmn * dyn(k)
      dan(k) = dan(k - 1) + dsn(k - 1)
      xln = xln + dsn(k - 1)

      write (*, *) k, dsn(k) * 100., dan(k) * 100.
    end do

    write (*, *) "XLN= ", xln * 100.

    do k = 1, npobj - 1
      do j = npini, npfin - 1
        if (dan(k) >= da(j) .and. dan(k) < da(j + 1)) then
          dyn(k) = dy(j)
        end if
      end do
    end do
  end subroutine remapcont

  !> Transform one auxiliary profile point into absolute wing coordinates.
  !!
  !! @param[in] i Rib index controlling the transformation.
  !! @param[in] j Profile point index.
  !! @param[in] u,v,w Main coordinates retained for interface compatibility.
  !! @param[in] rib Rib chord, rotation, and absolute-position parameters.
  !! @param[in] np Profile counts retained for interface compatibility.
  !! @param[in,out] u_aux,v_aux,w_aux Auxiliary coordinates. Slot 1 is local
  !! input and slots 2--5 receive the staged transformations.
  subroutine xyzt(i, j, u, v, w, rib, np, u_aux, v_aux, w_aux)
    integer, intent(in) :: i, j
    real(real64), intent(in) :: u(0:100, 500, 99)
    real(real64), intent(in) :: v(0:100, 500, 99)
    real(real64), intent(in) :: w(0:100, 500, 99)
    real(real64), intent(in) :: rib(0:100, 500)
    integer, intent(in) :: np(0:100, 9)
    real(real64), intent(inout) :: u_aux(0:100, 500, 10)
    real(real64), intent(inout) :: v_aux(0:100, 500, 10)
    real(real64), intent(inout) :: w_aux(0:100, 500, 10)

    real(real64) :: pi, tetha, rot_z, pos

    pi = 4.0d0 * datan(1.0d0)
    tetha = rib(i, 8) * pi / 180.0d0
    rot_z = rib(i, 250) * pi / 180.0d0
    pos = rib(i, 5) * rib(i, 251) / 100.0d0

    u_aux(i, j, 2) = &
        (u_aux(i, j, 1) - (rib(i, 10) / 100.) * rib(i, 5)) * &
        dcos(tetha) + v_aux(i, j, 1) * dsin(tetha) + &
        (rib(i, 10) / 100.) * rib(i, 5)
    v_aux(i, j, 2) = &
        (-u_aux(i, j, 1) + (rib(i, 10) / 100.) * rib(i, 5)) * &
        dsin(tetha) + v_aux(i, j, 1) * dcos(tetha)
    w_aux(i, j, 2) = 0.0d0

    w_aux(i, j, 3) = -u_aux(i, j, 2) * dsin(rot_z) + pos * dsin(rot_z)
    u_aux(i, j, 3) = u_aux(i, j, 2) * dcos(rot_z) + &
        pos * (1 - dcos(rot_z))
    v_aux(i, j, 3) = v_aux(i, j, 2)

    w_aux(i, j, 4) = -w_aux(i, j, 3) * &
        dcos(rib(i, 9) * pi / 180.) - &
        v_aux(i, j, 3) * dsin(rib(i, 9) * pi / 180.)
    u_aux(i, j, 4) = u_aux(i, j, 3)
    v_aux(i, j, 4) = -w_aux(i, j, 3) * &
        dsin(rib(i, 9) * pi / 180.) + &
        v_aux(i, j, 3) * dcos(rib(i, 9) * pi / 180.)

    w_aux(i, j, 5) = rib(i, 6) - w_aux(i, j, 4)
    u_aux(i, j, 5) = rib(i, 3) + u_aux(i, j, 4)
    v_aux(i, j, 5) = rib(i, 7) - v_aux(i, j, 4)
  end subroutine xyzt

end module leparagliding_profile_data
