!> Legacy-compatible two-dimensional geometry calculations.
!!
!! These routines were formerly compiled from `procedures/geometry_2d.inc`.
!! Keeping them in a free-form module gives both the application and unit
!! tests explicit interfaces without changing the historical arithmetic.
module leparagliding_geometry_2d
  use, intrinsic :: iso_fortran_env, only : real64
  implicit none
  private

  public :: vredis
  public :: xrxs
  public :: flatt
  public :: axisch
  public :: angdis2
  public :: vrib_hole_ellipse

contains

  !> Redistribute a 2D polyline to approximately spaced points.
  !!
  !! The historical algorithm samples each input segment in tenths and then
  !! selects from that expanded sequence. It is intentionally retained rather
  !! than replaced by an exact arc-length resampler.
  !!
  !! @param[in] xlin1 Source horizontal coordinates.
  !! @param[in] ylin1 Source vertical coordinates.
  !! @param[in,out] xlin3 Redistributed horizontal coordinates; unused entries
  !! remain unchanged.
  !! @param[in,out] ylin3 Redistributed vertical coordinates; unused entries
  !! remain unchanged.
  !! @param[in] n1vr Number of source points; must be at least two.
  !! @param[in] n2vr Requested number of output points; must be at least two.
  subroutine vredis(xlin1, ylin1, xlin3, ylin3, n1vr, n2vr)
    real(real64), intent(in) :: xlin1(5000), ylin1(5000)
    real(real64), intent(inout) :: xlin3(5000), ylin3(5000)
    integer, intent(in) :: n1vr, n2vr

    real(real64) :: xlin2(50000), ylin2(50000)
    ! The original undeclared temporaries were default real. Preserve that
    ! precision because their rounding is part of the compatibility output.
    real :: xj, yj, xjm1, yjm1, stvr, disx, disy
    integer :: j2vr, j1vr, kvr, j2max, icount, iespai, isobra
    integer :: itotes, j3vr, j22vr, iplus, ijkvr, j

    j2vr = 0
    do j1vr = 1, n1vr - 1
      xj = xlin1(j1vr)
      yj = ylin1(j1vr)
      xjm1 = xlin1(j1vr + 1)
      yjm1 = ylin1(j1vr + 1)

      do kvr = 0, 10
        stvr = real(kvr) / 10.0
        j2vr = j2vr + 1
        xlin2(j2vr) = xj + stvr * (xjm1 - xj)
        ylin2(j2vr) = yj + stvr * (yjm1 - yj)
      end do
      j2vr = j2vr - 1
    end do

    j2max = j2vr + 1
    icount = int(real((10 * n1vr - 1) / (n2vr - 1)))
    iespai = icount * (n2vr - 1)
    isobra = 10 * (n1vr - 1) - iespai
    itotes = 10 * (n1vr - 1)

    j3vr = 1
    j2vr = 1
    do j22vr = 1, j2max
      xlin3(j3vr) = xlin2(j2vr)
      ylin3(j3vr) = ylin2(j2vr)

      if (j3vr == n2vr) then
        xlin3(j3vr) = xlin1(n1vr)
        ylin3(j3vr) = ylin1(n1vr)
      end if

      iplus = 0
      if (j3vr <= isobra) iplus = 1

      do ijkvr = 1, icount + iplus
        j2vr = j2vr + 1
      end do
      j3vr = j3vr + 1
    end do

    if (n1vr == 2) then
      disx = (xlin1(2) - xlin1(1)) / real(n2vr - 1)
      disy = (ylin1(2) - ylin1(1)) / real(n2vr - 1)

      do j = 1, n2vr
        xlin3(j) = xlin1(1) + disx * real(j - 1)
        ylin3(j) = ylin1(1) + disy * real(j - 1)
      end do
    end if
  end subroutine vredis

  !> Find the intersection of two infinite 2D lines.
  !!
  !! @param[in] xru Horizontal coordinates of two points defining line R.
  !! @param[in] xrv Vertical coordinates of two points defining line R.
  !! @param[in] xsu Horizontal coordinates of two points defining line S.
  !! @param[in] xsv Vertical coordinates of two points defining line S.
  !! @param[out] xtu Horizontal coordinate of the intersection.
  !! @param[out] xtv Vertical coordinate of the intersection.
  !! @note Vertical lines receive the same special handling as the original
  !! implementation. Parallel coincident lines have no defined result.
  subroutine xrxs(xru, xrv, xsu, xsv, xtu, xtv)
    real(real64), intent(in) :: xru(2), xrv(2), xsu(2), xsv(2)
    ! These values were implicit default real in the fixed-form routine.
    real, intent(out) :: xtu, xtv
    real :: xmr, xbr, xms, xbs

    xmr = (xrv(2) - xrv(1)) / (xru(2) - xru(1))
    xbr = xrv(1) - xmr * xru(1)
    xms = (xsv(2) - xsv(1)) / (xsu(2) - xsu(1))
    xbs = xsv(1) - xms * xsu(1)

    xtu = (xbs - xbr) / (xmr - xms)
    xtv = xmr * xtu + xbr

    if (abs(xsu(2) - xsu(1)) <= 0.0001_real64) then
      xtu = xsu(1)
      xtv = xmr * xsu(1) + xbr
    end if

    if (abs(xru(2) - xru(1)) <= 0.0001_real64) then
      xtu = xru(1)
      xtv = xms * xru(1) + xbs
    end if
  end subroutine xrxs

  !> Develop the quadrilateral strip between ribs `ni` and `ni+1` into 2D.
  !!
  !! @param[in] ni First rib index in the strip.
  !! @param[in] npunts Number of quadrilaterals to develop.
  !! @param[in] rx Source X coordinates of the rib grid.
  !! @param[in] ry Source Y coordinates of the rib grid.
  !! @param[in] rz Source Z coordinates of the rib grid.
  !! @param[in,out] pl1x,pl1y Developed first-left point per quadrilateral.
  !! @param[in,out] pl2x,pl2y Developed second-left point per quadrilateral.
  !! @param[in,out] pr1x,pr1y Developed first-right point per quadrilateral.
  !! @param[in,out] pr2x,pr2y Developed second-right point per quadrilateral.
  !! @note Source edge and diagonal lengths are preserved by triangulation.
  subroutine flatt(ni, npunts, rx, ry, rz, &
      pl1x, pl1y, pl2x, pl2y, pr1x, pr1y, pr2x, pr2y)
    integer, intent(in) :: ni, npunts
    real(real64), intent(in) :: rx(0:100, 500), ry(0:100, 500)
    real(real64), intent(in) :: rz(0:100, 500)
    real(real64), intent(inout) :: pl1x(0:100, 500), pl1y(0:100, 500)
    real(real64), intent(inout) :: pl2x(0:100, 500), pl2y(0:100, 500)
    real(real64), intent(inout) :: pr1x(0:100, 500), pr1y(0:100, 500)
    real(real64), intent(inout) :: pr2x(0:100, 500), pr2y(0:100, 500)

    real(real64) :: phr, pa1r, pa2r, phl, pa1l, pa2l, px0, py0
    real(real64) :: ptheta, pw1, pa, pb, pc, pd, pe, pf
    ! Retain the default-real intermediates used by the historical routine.
    real :: pb2t, pb1t, pht, phu
    integer :: i, j

    i = ni
    px0 = 0.0_real64
    py0 = 0.0_real64
    ptheta = 0.0_real64

    do j = 1, npunts
      pa = sqrt((rx(i + 1, j) - rx(i, j))**2.0_real64 + &
          (ry(i + 1, j) - ry(i, j))**2.0_real64 + &
          (rz(i + 1, j) - rz(i, j))**2.0_real64)
      pb = sqrt((rx(i + 1, j + 1) - rx(i, j))**2.0_real64 + &
          (ry(i + 1, j + 1) - ry(i, j))**2.0_real64 + &
          (rz(i + 1, j + 1) - rz(i, j))**2.0_real64)
      pc = sqrt((rx(i + 1, j + 1) - rx(i + 1, j))**2.0_real64 + &
          (ry(i + 1, j + 1) - ry(i + 1, j))**2.0 + &
          (rz(i + 1, j + 1) - rz(i + 1, j))**2.0_real64)
      pd = sqrt((rx(i + 1, j) - rx(i, j + 1))**2.0_real64 + &
          (ry(i + 1, j) - ry(i, j + 1))**2.0_real64 + &
          (rz(i + 1, j) - rz(i, j + 1))**2.0_real64)
      pe = sqrt((rx(i, j + 1) - rx(i, j))**2.0_real64 + &
          (ry(i, j + 1) - ry(i, j))**2.0_real64 + &
          (rz(i, j + 1) - rz(i, j))**2.0_real64)
      pf = sqrt((rx(i + 1, j + 1) - rx(i, j + 1))**2.0_real64 + &
          (ry(i + 1, j + 1) - ry(i, j + 1))**2.0_real64 + &
          (rz(i + 1, j + 1) - rz(i, j + 1))**2.0_real64)

      pa2r = (pa * pa - pb * pb + pc * pc) / (2.0_real64 * pa)
      pa1r = pa - pa2r
      phr = sqrt(pc * pc - pa2r * pa2r)

      pa2l = (pa * pa - pe * pe + pd * pd) / (2.0_real64 * pa)
      pa1l = pa - pa2l
      phl = sqrt(pd * pd - pa2l * pa2l)

      pb2t = (pb * pb - pe * pe + pf * pf) / (2.0_real64 * pb)
      pb1t = pb - pb2t
      pht = sqrt(pf * pf - pb2t * pb2t)
      pw1 = atan(phr / pa1r)
      phu = pb1t * tan(pw1)

      pl1x(i, j) = px0
      pl1y(i, j) = py0
      pr1x(i, j) = pa * cos(ptheta) + px0
      pr1y(i, j) = pa * sin(ptheta) + py0
      pl2x(i, j) = pa1l * cos(ptheta) - phl * sin(ptheta) + px0
      pl2y(i, j) = pa1l * sin(ptheta) + phl * cos(ptheta) + py0
      pr2x(i, j) = pa1r * cos(ptheta) - phr * sin(ptheta) + px0
      pr2y(i, j) = pa1r * sin(ptheta) + phr * cos(ptheta) + py0

      px0 = pl2x(i, j)
      py0 = pl2y(i, j)
      ptheta = atan((pr2y(i, j) - pl2y(i, j)) / &
          (pr2x(i, j) - pl2x(i, j)))
    end do
  end subroutine flatt

  !> Copy a 2D point sequence through the legacy axis-change rotation.
  !!
  !! @param[in] npunts Number of valid points.
  !! @param[out] angle Applied angle in degrees; historically forced to zero.
  !! @param[in] px9i Input horizontal coordinates.
  !! @param[in] py9i Input vertical coordinates.
  !! @param[in,out] px9o Output horizontal coordinates; unused entries remain.
  !! @param[in,out] py9o Output vertical coordinates; unused entries remain.
  !! @note Current behavior is an identity copy because `angle` is reset.
  subroutine axisch(npunts, angle, px9i, py9i, px9o, py9o)
    integer, intent(in) :: npunts
    real(real64), intent(out) :: angle
    real(real64), intent(in) :: px9i(500), py9i(500)
    real(real64), intent(inout) :: px9o(500), py9o(500)

    real(real64) :: xc, xs, pi
    integer :: j

    pi = 4.0_real64 * atan(1.0_real64)
    angle = 0.0_real64
    do j = 1, npunts
      xc = cos(angle * pi / 180.0_real64)
      xs = sin(angle * pi / 180.0_real64)
      px9o(j) = xc * px9i(j) - xs * py9i(j)
      py9o(j) = xs * px9i(j) + xc * py9i(j)
    end do
  end subroutine axisch

  !> Preserve the unfinished legacy angle/distance helper for compatibility.
  !!
  !! @warning The original calculations that would set `angl`, `p3u`, and
  !! `p3v` were commented out. This routine deliberately leaves them unchanged.
  subroutine angdis2(p1u, p1v, p2u, p2v, p3u, p3v, angl, dist)
    real(real64), intent(in) :: p1u, p1v, p2u, p2v, dist
    real(real64), intent(inout) :: p3u, p3v, angl
    real(real64) :: du, dv

    du = p2u - p1u
    dv = p2v - p1v

    if (du /= 0.0_real64) then
      ! Historical calculation intentionally disabled.
    end if
    if (du >= 0.0_real64 .and. dv >= 0.0_real64) then
      ! Historical calculation intentionally disabled.
    end if
    if (du < 0.0_real64 .and. dv >= 0.0_real64) then
      ! Historical calculation intentionally disabled.
    end if
    if (du >= 0.0_real64 .and. dv <= 0.0_real64) then
      ! Historical calculation intentionally disabled.
    end if
    if (du < 0.0_real64 .and. dv <= 0.0_real64) then
      ! Historical calculation intentionally disabled.
    end if
  end subroutine angdis2

  !> Fit an ellipse inside a four-corner H/V-rib strip.
  !!
  !! @param[in] lx1,ly1 Left edge at the first longitudinal station.
  !! @param[in] rx1,ry1 Right edge at the first longitudinal station.
  !! @param[in] lx2,ly2 Left edge at the second longitudinal station.
  !! @param[in] rx2,ry2 Right edge at the second longitudinal station.
  !! @param[in] span_fraction Position between the stations, in `[0,1]`.
  !! @param[in] height_percent Semiaxis along the local strip, percent.
  !! @param[in] width_percent Perpendicular semiaxis, percent.
  !! @param[in] center_fraction Center position from left to right.
  !! @param[out] x0,y0 Ellipse center.
  !! @param[out] a,b Ellipse semiaxes.
  !! @param[out] angle Rotation of semiaxis `a`, in radians.
  subroutine vrib_hole_ellipse(lx1, ly1, rx1, ry1, lx2, ly2, rx2, ry2, &
      span_fraction, height_percent, width_percent, center_fraction, &
      x0, y0, a, b, angle)
    real(real64), intent(in) :: lx1, ly1, rx1, ry1, lx2, ly2, rx2, ry2
    real(real64), intent(in) :: span_fraction, height_percent, width_percent
    real(real64), intent(in) :: center_fraction
    real(real64), intent(out) :: x0, y0, a, b, angle
    real(real64) :: lx, ly, rx, ry, height

    lx = lx1 + span_fraction * (lx2 - lx1)
    ly = ly1 + span_fraction * (ly2 - ly1)
    rx = rx1 + span_fraction * (rx2 - rx1)
    ry = ry1 + span_fraction * (ry2 - ry1)
    height = sqrt((rx - lx)**2 + (ry - ly)**2)
    angle = atan2(ry - ly, rx - lx)
    x0 = lx + center_fraction * (rx - lx)
    y0 = ly + center_fraction * (ry - ly)
    a = 0.005_real64 * height * height_percent
    b = 0.005_real64 * height * width_percent
  end subroutine vrib_hole_ellipse

end module leparagliding_geometry_2d
