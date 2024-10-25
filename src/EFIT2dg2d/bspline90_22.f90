! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!
!   VERSION 2.2
!
!   f90 VERSION
!
!   This library contains routines for B-spline interpolation in
!   one, two, and three dimensions. Part of the routines are based
!   on the book by Carl de Boor: A practical guide to Splines (Springer,
!   New-York 1978) and have the same calling sequence and names as
!   the corresponding routines from the IMSL library. For documen-
!   tation see the additional files. NOTE: The results in the demo
!   routines may vary slightly on different architectures.
!
!   by W. Schadow 12/04/99
!   last changed by W. Schadow 07/28/2000
!
!
!   Wolfgang Schadow
!   TRIUMF
!   4004 Wesbrook Mall
!   Vancouver, B.C. V6T 2A3
!   Canada
!
!   email: schadow@triumf.ca  or  schadow@physik.uni-bonn.de
!
!   www  : http://www.triumf.ca/people/schadow
!
!
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!
!   Copyright (C) 2000 Wolfgang Schadow
!
!   This library is free software; you can redistribute it and/or
!   modify it under the terms of the GNU Library General Public
!   License as published by the Free Software Foundation; either
!   version 2 of the License, or (at your option) any later version.
!
!   This library is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!   Library General Public License for more details.
!
!   You should have received a copy of the GNU Library General Public
!   License along with this library; if not, write to the
!   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
!   Boston, MA  02111-1307, USA.
!
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


module numeric

  integer, parameter :: sgl = kind(1.0)
  integer, parameter :: dbl = kind(1.0d0)

end module numeric


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


module bspline

!
!  ------------------------------------------------------------------
!
!
!   The following routines are included:
!
!            dbsnak
!
!            dbsint
!            dbsval
!            dbsder
!            dbs1gd
!
!            dbs2in
!            dbs2dr
!            dbs2vl
!            dbs2gd
!
!            dbs3in
!            dbs3vl
!            dbs3dr
!            dbs3gd
!
!  ------------------------------------------------------------------
!

  private

  public dbsnak
  public dbsint, dbsval, dbsder, dbs1gd
  public dbs2in, dbs2dr, dbs2vl, dbs2gd
  public dbs3in, dbs3vl, dbs3dr, dbs3gd


contains


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  subroutine dbsnak(nx,xvec,kxord,xknot)

!
!  Compute the `not-a-knot' spline knot sequence.
!  (see de Boor p. 167)
!
!   nx     - number of data points.  (input)
!   xvec   - array of length ndata containing the location of the
!            data points.  (input)
!   kxord  - order of the spline.  (input)
!   xknot  - array of length ndata+korder containing the knot
!            sequence.  (output)
!

    use numeric

    implicit none

    integer, intent(in) :: nx, kxord

    real(kind=dbl), dimension(nx), intent(in)        :: xvec
    real(kind=dbl), dimension(nx+kxord), intent(out) :: xknot

    real(kind=dbl) :: eps
    integer        :: ix
    logical        :: first = .true.

    save first,eps


    if (first) then
       first=.false.
       eps = epsilon(1.0_dbl)
       write(6,*) "subroutine dbsnak: "
       write(6,*) "eps = ",eps
    endif

    if((kxord .lt. 0) .or. (kxord .gt. nx)) then
       write(6,*) "subroutine dbsnak: error"
       write(6,*) "0 <= kxord <= nx is required."
       write(6,*) "kxord = ", kxord, " and nx = ", nx,  " is given."
       stop
    endif

    do ix = 1, kxord
       xknot(ix) = xvec(1)
    end do

    if(mod(kxord,2) .eq. 0) then
       do ix = kxord+1, nx
          xknot(ix) = xvec(ix-kxord/2)
       end do
    else
       do ix = kxord+1, nx
          xknot(ix) = 0.5_dbl * (xvec(ix-kxord/2) + xvec(ix-kxord/2-1))
       end do
    endif

    do ix = nx+1, nx+kxord
!
! Introduce the sign function here for negative xvec.
!
       xknot(ix) = xvec(nx) * (1.0_dbl + sign(eps,xvec(nx)))
    end do

  end subroutine dbsnak


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  subroutine dbsint(nx,xvec,xdata,kx,xknot,bcoef)

!
!  Computes the spline interpolant, returning the B-spline coefficients.
!  (see de Boor p. 204)
!
!   nx     - number of data points.  (input)
!   xvec   - array of length nx containing the data point
!            abscissas.  (input)
!   xdata  - array of length ndata containing the data point
!            ordinates.  (input)
!   kx     - order of the spline.  (input)
!            korder must be less than or equal to ndata.
!   xknot  - array of length nx+kx containing the knot
!            sequence.  (input)
!            xknot must be nondecreasing.
!   bscoef - array of length ndata containing the B-spline
!            coefficients.  (output)
!

    use numeric

    implicit none

    integer, intent(in)                          :: nx, kx
    real(kind=dbl), dimension(nx), intent(in)    :: xdata, xvec
    real(kind=dbl), dimension(nx+kx), intent(in) :: xknot
    real(kind=dbl), dimension(nx), intent(out)   :: bcoef

    integer                                :: nxp1, kxm1, kpkm2, leftx, lenq
    integer                                :: ix, ik,ilp1mx, jj, iflag
    real(kind=dbl)                         :: xveci
    real(kind=dbl), dimension((2*kx-1)*nx) :: work


    nxp1  = nx + 1
    kxm1  = kx - 1
    kpkm2 = 2 * kxm1
    leftx = kx
    lenq  = nx * (kx + kxm1)

    do ix = 1, lenq
       work(ix) = 0.0_dbl
    end do

    do  ix = 1, nx
       xveci  = xvec(ix)
       ilp1mx = min0(ix+kx,nxp1)
       leftx   = max0(leftx,ix)
       if (xveci .lt. xknot(leftx)) goto 998
30     if (xveci .lt. xknot(leftx+1)) go to 40
       leftx = leftx + 1
       if (leftx .lt. ilp1mx) go to 30
       leftx = leftx - 1
       if (xveci .gt. xknot(leftx+1)) goto 998
40     call bsplvb (xknot,nx+kx,kx,1,xveci,leftx,bcoef)
       jj = ix - leftx + 1 + (leftx - kx) * (kx + kxm1)
       do ik = 1, kx
          jj       = jj + kpkm2
          work(jj) = bcoef(ik)
       end do
    end do

    call banfac(work,kx+kxm1,nx,kxm1,kxm1,iflag)

    if (iflag .ne. 1) then
       write(6,*) "subroutine dbsint: error"
       write(6,*) "no solution of linear equation system !!!"
       stop
    end if

    do ix = 1, nx
       bcoef(ix) = xdata(ix)
    end do

    call banslv(work,kx+kxm1,nx,kxm1,kxm1,bcoef)

    return

998 write(6,*) "subroutine dbsint:"
    write(6,*) "xknot(ix) <= xknot(ix+1) required."
    write(6,*) ix,xknot(ix),xknot(ix+1)

    stop

  end subroutine dbsint


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  function dbsval(x,kx,xknot,nx,bcoef)

!
!  Evaluates a spline, given its B-spline representation.
!
!   x      - point at which the spline is to be evaluated.  (input)
!   kx     - order of the spline.  (input)
!   xknot  - array of length nx+kx containing the knot
!            sequence.  (input)
!            xknot must be nondecreasing.
!   nx     - number of B-spline coefficients.  (input)
!   bcoef  - array of length nx containing the B-spline
!            coefficients.  (input)
!   dbsval - value of the spline at x.  (output)
!

    use numeric

    implicit none

    integer, intent(in)                          :: nx, kx
    real(kind=dbl)                               :: dbsval
    real(kind=dbl)                               :: x
    real(kind=dbl), dimension(nx+kx), intent(in) :: xknot
    real(kind=dbl), dimension(nx), intent(in)    :: bcoef

    integer                       :: il, ik, ix, leftx
    real(kind=dbl)                :: save1, save2
    real(kind=dbl), dimension(kx) :: work, dl, dr

!
!     check if xknot(i) <= xknot(i+1) and calculation of i so that
!     xknot(i) <= x < xknot(i+1)
!

    leftx = 0

    do ix = 1,nx+kx-1
       if (xknot(ix) .gt. xknot(ix+1)) then
          write(6,*) "subroutine dbsval:"
          write(6,*) "xknot(ix) <= xknot(ix+1) required."
          write(6,*) ix,xknot(ix),xknot(ix+1)
          stop
       endif
       if((xknot(ix) .le. x) .and. (x .lt. xknot(ix+1))) leftx = ix
    end do

    if(leftx .eq. 0) then
       write(6,*) "subroutine dbsval:"
       write(6,*) "ix with xknot(ix) <= x < xknot(ix+1) required."
       write(6,*) "x = ", x
       stop
    endif

    do ik = 1, kx-1
       work(ik) = bcoef(leftx+ik-kx)
       dl(ik)   = x - xknot(leftx+ik-kx)
       dr(ik)   = xknot(leftx+ik) - x
    end do

    work(kx)  = bcoef(leftx)
    dl(kx)    = x - xknot(leftx)

    do ik = 1, kx-1
       save2 = work(ik)
       do il = ik+1, kx
          save1 = work(il)
          work(il) = (dl(il) * work(il) + dr(il-ik) * save2)                  &
               &           / (dl(il) + dr(il - ik))
          save2 = save1
       end do
    end do

    dbsval = work(kx)

  end function dbsval


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  function dbsder(iderx,x,kx,xknot,nx,bcoef)

!
!  Evaluates the derivative of a spline, given its B-spline representation.
!
!
!   iderx  - order of the derivative to be evaluated.  (input)
!            in particular, iderx = 0 returns the value of the
!            spline.
!   x      - point at which the spline is to be evaluated.  (input)
!   kx     - order of the spline.  (input)
!   xknot  - array of length nx+kx containing the knot
!            sequence.  (input)
!            xknot must be nondecreasing.
!   nx     - number of B-spline coefficients.  (input)
!   bcoef  - array of length nx containing the B-spline
!            coefficients.  (input)
!   dbsder - value of the iderx-th derivative of the spline at x.
!            (output)
!

    use numeric

    implicit none

    integer, intent(in)                          :: iderx, kx, nx
    real(kind=dbl)                               :: dbsder
    real(kind=dbl), intent(in)                   :: x
    real(kind=dbl), dimension(nx+kx), intent(in) :: xknot
    real(kind=dbl), dimension(nx), intent(in)    :: bcoef

    integer                       :: ix, ik, il, leftx
    real(kind=dbl)                :: save, save1, save2, y, sum, dik
    real(kind=dbl), dimension(kx) :: work, dl, dr,bsp

!
!     check if xknot(i) <= xknot(i+1) and calculation of i so that
!     xknot(i) <= x < xknot(i+1)
!

    leftx = 0
    do ix = 1,nx+kx-1
       if (xknot(ix) .gt. xknot(ix+1)) then
          write(6,*) "subroutine dbsder:"
          write(6,*) "xknot(ix) <= xknot(ix+1) required."
          stop
       endif
       if ((xknot(ix) .le. x) .and. (x .lt. xknot(ix+1))) leftx = ix
    end do

    if (leftx .eq. 0) then
       write(6,*) "subroutine dbsder:"
       write(6,*) "ix with xknot(ix) <= x < xknot(ix+1) required."
       write(6,*) "xknot(1)     = ", xknot(1)
       write(6,*) "xknot(nx+kx) = ", xknot(nx+kx)
       write(6,*) "         x   = ", x
       stop
    endif

    if (iderx .eq. 0) then

       do ik = 1,kx-1
          work(ik) = bcoef(leftx+ik-kx)
          dl(ik)   = x - xknot(leftx+ik-kx)
          dr(ik)   = xknot(leftx+ik) - x
       end do

       work(kx)  = bcoef(leftx)
       dl(kx)    = x - xknot(leftx)

       do ik = 1,kx-1
          save2 = work(ik)
          do il = ik+1,kx
             save1 = work(il)
             work(il) = (dl(il) * work(il) + dr(il-ik) * save2)               &
                  &              / (dl(il) + dr(il - ik))
             save2 = save1
          end do
       end do

       dbsder = work(kx)

    elseif ((iderx .ge. 1) .and. (iderx .lt. kx)) then

       bsp(1) = 1.0_dbl
       do ik = 1,kx-iderx-1
          dr(ik) = xknot(leftx+ik) - x
          dl(ik) = x - xknot(leftx+1-ik)
          save   = bsp(1)
          bsp(1) = 0.0_dbl
          do il = 1, ik
             y         = save / (dr(il) + dl(ik+1-il))
             bsp(il)   = bsp(il) + dr(il) * y
             save      = bsp(il+1)
             bsp(il+1) = dl(ik+1-il) * y
          end do
       end do

       do ik = 1, kx
          work(ik) = bcoef(leftx+ik-kx)
          dr(ik)   = xknot(leftx+ik) - x
          dl(ik)   = x - xknot(leftx+ik-kx)
       end do

       do ik = 1, iderx
          dik   = dble(kx - ik)
          save2 = work(ik)
          do il = ik+1, kx
             save1    = work(il)
             work(il) = dik * (work(il) - save2) /(dl(il) + dr(il-ik))
             save2    = save1
          end do
       end do

       sum = 0.0_dbl

       do ix = 1, kx-iderx
          sum = sum + bsp(ix) * work(iderx+ix)
       end do

       dbsder = sum

    else
       dbsder = 0.0_dbl
    endif

  end function dbsder


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  subroutine dbs1gd(iderx,nxvec,xvec,kx,xknot,nx,bcoef,val)

!
!  Evaluates the derivative of a spline on a grid, given its B-spline
!  representation.
!
!   iderx  - order of the derivative to be evaluated.  (input)
!            in particular, iderx = 0 returns the value of the
!            spline.
!   nxvec  - length of vector xvec.  (input)
!   xvec   - array of length nxvec containing the points at which the
!            spline is to be evaluated.  (input)
!            xvec should be strictly increasing.
!   kx     - order of the spline.  (input)
!   xknot  - array of length nx+kx containing the knot
!            sequence.  (input)
!            xknot must be nondecreasing.
!   nx     - number of B-spline coefficients.  (input)
!   bcoef  - array of length nx containing the B-spline
!            coefficients.  (input)
!   val    - array of length nxvec containing the values of the
!            iderx-th derivative of the spline at the points in
!            xvec.  (output)
!

    use numeric

    implicit none

    integer, intent(in)                           :: iderx, nxvec, kx, nx
    real(kind=dbl), dimension(nxvec), intent(in)  :: xvec
    real(kind=dbl), dimension(nx), intent(in)     :: bcoef
    real(kind=dbl), dimension(nx+kx), intent(in)  :: xknot
    real(kind=dbl), dimension(nxvec), intent(out) :: val

    integer                             :: i, il, ik, ix
    integer, dimension(nxvec)           :: leftx
    real(kind=dbl)                      :: dik
    real(kind=dbl), dimension(nxvec,kx) :: dl, dr, biatx, work
    real(kind=dbl), dimension(nxvec)    :: save1, save2, term

    logical :: same, next


    leftx(1) = 0

    call huntn(xknot,nx+kx,kx,xvec(1),leftx(1))

    do ix = 2, nxvec
       leftx(ix) = leftx(ix-1)
       same = (xknot(leftx(ix)) .le. xvec(ix))                                &
            &        .and. (xvec(ix) .le. xknot(leftx(ix)+1))
       if(.not. same ) then
          leftx(ix) = leftx(ix) + 1
          next      = (xknot(leftx(ix)) .le. xvec(ix))                        &
               &           .and. (xvec(ix) .le. xknot(leftx(ix)+1))
          if (.not. next)                                                     &
               &           call huntn(xknot,nx+kx,kx,xvec(ix),leftx(ix))
       endif
    end do

    do ix = 1, nx+kx-1
       if (xknot(ix) .gt. xknot(ix+1)) then
          write(6,*) "subroutine dbs1gd:"
          write(6,*) "xknot(ix) <= xknot(ix+1) required."
          write(6,*) ix, xknot(ix), xknot(ix+1)
          write(6,*)
          write(6,*) xknot
          stop
       endif
    end do

    do ix = 1, nxvec
       if ((xvec(ix).lt.xknot(1)).or.(xvec(ix).gt.xknot(nx+kx))) then
          write(6,*) "subroutine dbs1gd:"
          write(6,*) "ix with xknot(ix) <= x < xknot(ix+1) required."
          write(6,*) "x = ", xvec(ix)
          stop
       endif
    end do

    if (iderx .eq. 0) then

       do ix = 1,nxvec
          biatx(ix,1) = 1._dbl
          val(ix)     = 0._dbl
       end do

       do ik = 1, kx-1
          do ix = 1, nxvec
             dr(ix,ik) = xknot(leftx(ix)+ik) - xvec(ix)
             dl(ix,ik) = xvec(ix) - xknot(leftx(ix)+1-ik)
             save1(ix) = 0._dbl
          end do

          do il = 1, ik
             do ix = 1,nxvec
                term(ix)     = biatx(ix,il)                                   &
                     &                 / (dr(ix,il) + dl(ix,ik+1-il))
                biatx(ix,il) = save1(ix) + dr(ix,il) * term(ix)
                save1(ix)    = dl(ix,ik+1-il) * term(ix)
             end do
          end do

          do ix = 1, nxvec
             biatx(ix,ik+1) = save1(ix)
          end do
       end do

       do ik = 1, kx
          do ix = 1, nxvec
             val(ix) = val(ix) + biatx(ix,ik) * bcoef(leftx(ix)-kx+ik)
          end do
       end do

    elseif ((iderx .ge. 1) .and. (iderx .lt. kx)) then

       do ix = 1, nxvec
          biatx(ix,1) = 1._dbl
          val(ix)     = 0._dbl
       end do

       do ik = 1, kx-iderx-1
          do ix = 1, nxvec
             dr(ix,ik)   = xknot(leftx(ix)+ik) - xvec(ix)
             dl(ix,ik)   = xvec(ix) - xknot(leftx(ix)+1-ik)
             save1(ix)    = biatx(ix,1)
             biatx(ix,1) = 0.0_dbl
             do il = 1, ik
                term(ix)       = save1(ix)                                    &
                     &                 / (dr(ix,il) + dl(ix,ik+1-il))
                biatx(ix,il)   = biatx(ix,il) + dr(ix,il) * term(ix)
                save1(ix)      = biatx(ix,il+1)
                biatx(ix,il+1) = dl(ix,ik+1-il) * term(ix)
             end do
          end do
       end do

       do ik = 1, kx
          do ix = 1, nxvec
             work(ix,ik) = bcoef(leftx(ix)+ik-kx)
             dr(ix,ik)   = xknot(leftx(ix)+ik) - xvec(ix)
             dl(ix,ik)   = xvec(ix) - xknot(leftx(ix)+ik-kx)
          end do
       end do

       do ik = 1, iderx
          dik   = dble(kx - ik)
          do ix = 1, nxvec
             save2(ix) = work(ix,ik)
             do il = ik+1, kx
                save1(ix)   = work(ix,il)
                work(ix,il) = dik * (work(ix,il) - save2(ix))                 &
                     &                 /(dl(ix,il) + dr(ix,il-ik))
                save2(ix)   = save1(ix)
             end do
          end do
       end do

       do i = 1, kx-iderx
          do ix = 1, nxvec
             val(ix) = val(ix) + biatx(ix,i) * work(ix,iderx+i)
          end do
       end do

    else

       do ix = 1, nxvec
          val(ix) = 0.0_dbl
       end do

    endif

  end subroutine dbs1gd


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  function dbsdca(iderx,x,kx,xknot,nx,bcoef,leftx)

!
! This routine is equivalent to the routine dbsder, but it does not
! check the parameters!!!
!
! Evaluates the derivative of a spline, given its B-spline representation.
!
!
!   iderx  - order of the derivative to be evaluated.  (input)
!            in particular, iderx = 0 returns the value of the
!            spline.
!   x      - point at which the spline is to be evaluated.  (input)
!   kx     - order of the spline.  (input)
!   xknot  - array of length nx+kx containing the knot
!            sequence.  (input)
!            xknot must be nondecreasing.
!   nx     - number of B-spline coefficients.  (input)
!   bcoef  - array of length nx containing the B-spline
!            coefficients.  (input)
!   leftx  - number of the intervall of xknot that includes x
!   dbsdca - value of the ideriv-th derivative of the spline at x.
!            (output)
!

    use numeric

    implicit none

    integer, intent(in)                          :: iderx, kx, nx
    real(kind=dbl)                               :: dbsdca
    real(kind=dbl), intent(in)                   :: x
    real(kind=dbl), dimension(nx+kx), intent(in) :: xknot
    real(kind=dbl), dimension(nx), intent(in)    :: bcoef

    integer                       :: i, ik, il, leftx
    real(kind=dbl)                :: save, save1, save2, y, sum, dik
    real(kind=dbl), dimension(kx) :: work, dl, dr,bsp


    if (iderx .eq. 0) then

       do ik = 1, kx-1
          work(ik) = bcoef(leftx+ik-kx)
          dl(ik)   = x - xknot(leftx+ik-kx)
          dr(ik)   = xknot(leftx+ik) - x
       end do

       work(kx)  = bcoef(leftx)
       dl(kx)    = x - xknot(leftx)

       do ik = 1, kx-1
          save2 = work(ik)
          do il = ik+1, kx
             save1 = work(il)
             work(il) = (dl(il) * work(il) + dr(il-ik) * save2)               &
                  &              / (dl(il) + dr(il - ik))
             save2 = save1
          end do
       end do

       dbsdca = work(kx)

    elseif ((iderx .ge. 1) .and. (iderx .lt. kx)) then
       bsp(1) = 1.0_dbl
       do ik = 1,kx-iderx-1
          dr(ik) = xknot(leftx+ik) - x
          dl(ik) = x - xknot(leftx+1-ik)
          save   = bsp(1)
          bsp(1) = 0.0_dbl
          do il = 1, ik
             y         = save / (dr(il) + dl(ik+1-il))
             bsp(il)   = bsp(il) + dr(il) * y
             save      = bsp(il+1)
             bsp(il+1) = dl(ik+1-il) * y
          end do
       end do

       do ik = 1, kx
          work(ik) = bcoef(leftx+ik-kx)
          dr(ik)   = xknot(leftx+ik) - x
          dl(ik)   = x - xknot(leftx+ik-kx)
       end do

       do ik = 1, iderx
          dik   = dble(kx - ik)
          save2 = work(ik)
          do il = ik+1, kx
             save1    = work(il)
             work(il) = dik * (work(il) - save2) /(dl(il) + dr(il-ik))
             save2    = save1
          end do
       end do

       sum = 0.0_dbl

       do i = 1, kx-iderx
          sum = sum + bsp(i) * work(iderx+i)
       end do

       dbsdca = sum

    else
       dbsdca = 0.0_dbl
    endif

  end function dbsdca


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  subroutine dbs2in(nx,xvec,ny,yvec,xydata,ldf,kx,ky,xknot,yknot,bcoef)

!
!  Computes a two-dimensional tensor-product spline interpolant,
!  returning the tensor-product B-spline coefficients.
!
!    nx     - number of data points in the x-direction.  (input)
!    xvec   - array of length nx containing the data points in
!             the x-direction.  (input)
!             xdata must be strictly increasing.
!    ny     - number of data points in the y-direction.  (input)
!    yvec   - array of length ny containing the data points in
!             the y-direction.  (input)
!             ydata must be strictly increasing.
!    xydata - array of size nx by nydata containing the values to
!             be interpolated.  (input)
!             fdata(i,j) is the value at (xdata(i),ydata(j)).
!    ldf    - the leading dimension of fdata exactly as specified in
!             the dimension statement of the calling program.
!             (input)
!    kx     - order of the spline in the x-direction.  (input)
!             kxord must be less than or equal to nxdata.
!    ky     - order of the spline in the y-direction.  (input)
!             kyord must be less than or equal to nydata.
!    xknot  - array of length nx+kx containing the knot
!             sequence in the x-direction.  (input)
!             xknot must be nondecreasing.
!    yknot  - array of length ny+ky containing the knot
!             sequence in the y-direction.  (input)
!             yknot must be nondecreasing.
!    bcoef  - array of length nx*ny containing the
!             tensor-product B-spline coefficients.  (output)
!             bscoef is treated internally as a matrix of size nxdata
!             by nydata.
!

    use numeric

    implicit none

    integer, intent(in)                           :: nx, ny, kx, ky, ldf

    real(kind=dbl), dimension(nx), intent(in)     :: xvec
    real(kind=dbl), dimension(ny), intent(in)     :: yvec
    real(kind=dbl), dimension(nx+kx), intent(in)  :: xknot
    real(kind=dbl), dimension(ny+ky), intent(in)  :: yknot
    real(kind=dbl), dimension(ldf,*), intent(in)  :: xydata
    real(kind=dbl), dimension(nx,ny), intent(out) :: bcoef

    real(kind=dbl), dimension(max(nx,ny),max(nx,ny))        :: work1
    real(kind=dbl), dimension(max(nx,ny))                   :: work2
    real(kind=dbl), dimension(max((2*kx-1)*nx,(2*ky-1)*ny)) :: work3


    call spli2d(xvec,ldf,xydata,xknot,nx,kx,ny,work2,work3,work1)
    call spli2d(yvec,ny, work1, yknot,ny,ky,nx,work2,work3,bcoef)

  end subroutine dbs2in


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  subroutine spli2d(xyvec,ld,xydata,xyknot,n,k,m,work2,work3,bcoef)

    use numeric

    implicit none


    integer, intent(in)                         :: ld, n, k, m
    real(kind=dbl), dimension(n), intent(in)    :: xyvec
    real(kind=dbl), dimension(n+k), intent(in)  :: xyknot
    real(kind=dbl), dimension(ld,m), intent(in) :: xydata
    real(kind=dbl), dimension(m,n), intent(out) :: bcoef

    real(kind=dbl), dimension(n), intent(out)         :: work2
    real(kind=dbl), dimension((2*k-1)*n), intent(out) :: work3


    integer        :: np1, km1, kpkm2, left, lenq, i, iflag, ilp1mx, j, jj
    real(kind=dbl) :: xyveci

    np1   = n + 1
    km1   = k - 1
    kpkm2 = 2 * km1
    left  = k
    lenq  = n * (k + km1)

    do i = 1,lenq
       work3(i) = 0.0_dbl
    end do

    do i = 1, n
       xyveci  = xyvec(i)
       ilp1mx = min0(i+k,np1)
       left   = max0(left,i)
       if (xyveci .lt. xyknot(left)) go to 998
30     if (xyveci .lt. xyknot(left+1)) go to 40
       left = left + 1
       if (left .lt. ilp1mx) go to 30
       left = left - 1
       if (xyveci .gt. xyknot(left+1)) go to 998
40     call bsplvb(xyknot,n+k,k,1,xyveci,left,work2)
       jj = i - left + 1 + (left - k) * (k + km1)
       do j = 1, k
          jj        = jj + kpkm2
          work3(jj) = work2(j)
       end do
    end do

    call banfac(work3,k+km1,n,km1,km1,iflag )

    if (iflag .ne. 1) then
       write(6,*) "subroutine dbs2in: error"
       write(6,*) "no solution of linear equation system !!!"
       stop
    end if

    do j = 1, m
       do i = 1, n
          work2(i) = xydata(i,j)
       end do

       call banslv(work3,k+km1,n,km1,km1,work2)

       do i = 1, n
          bcoef(j,i) = work2(i)
       end do
    end do

    return

998 write(6,*) "subroutine db2in:"
    write(6,*) "i with knot(i) <= x/y < knot(i+1) required."
    write(6,*) "knot(1)   = ", xyknot(1)
    write(6,*) "knot(n+k) = ", xyknot(n+k)
    write(6,*) "      x/y = ", xyveci

    stop

  end subroutine spli2d


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  function dbs2vl(x,y,kx,ky,xknot,yknot,nx,ny,bcoef)

!
!  evaluates a two-dimensional tensor-product spline, given its
!  tensor-product B-spline representation.    use numeric
!
!   x      - x-coordinate of the point at which the spline is to be
!            evaluated.  (input)
!   y      - y-coordinate of the point at which the spline is to be
!            evaluated.  (input)
!   kx     - order of the spline in the x-direction.  (input)
!   ky     - order of the spline in the y-direction.  (input)
!   xknot  - array of length nx+kx containing the knot
!            sequence in the x-direction.  (input)
!            xknot must be nondecreasing.
!   yknot  - array of length ny+ky containing the knot
!            sequence in the y-direction.  (input)
!            yknot must be nondecreasing.
!   nx     - number of B-spline coefficients in the x-direction.
!            (input)
!   ny     - number of B-spline coefficients in the y-direction.
!            (input)
!   bcoef  - array of length nx*ny containing the
!            tensor-product B-spline coefficients.  (input)
!            bscoef is treated internally as a matrix of size nx
!            by ny.
!   dbs2vl - value of the spline at (x,y).  (output)
!

    use numeric

    implicit none

    integer, intent(in)                          :: nx, ny, kx, ky
    real(kind=dbl), intent(in)                   :: x, y
    real(kind=dbl), dimension(nx+kx), intent(in) :: xknot
    real(kind=dbl), dimension(ny+ky), intent(in) :: yknot
    real(kind=dbl), dimension(nx,ny), intent(in) :: bcoef
    real(kind=dbl)                               :: dbs2vl

    integer                       :: ix, iy, iky, leftx, lefty
    real(kind=dbl), dimension(ky) :: work

!
!     check if knot(i) <= knot(i+1) and calculation of i so that
!     knot(i) <= x < knot(i+1)
!

    leftx = 0

    do ix = 1, nx+kx-1
       if (xknot(ix) .gt. xknot(ix+1)) then
          write(6,*) "subroutine dbs2vl:"
          write(6,*) "xknot(ix) <= xknot(ix+1) required."
          write(6,*) ix, xknot(ix), xknot(ix+1)
          write(6,*)
          write(6,*) xknot
          stop
       endif
       if((xknot(ix) .le. x) .and. (x .lt. xknot(ix+1))) leftx = ix
    end do

    if(leftx .eq. 0) then
       write(6,*) "subroutine dbs2vl:"
       write(6,*) "ix with xknot(ix) <= x < xknot(ix+1) required."
       write(6,*) "x = ", x
       write(6,*)
       write(6,*) xknot
       stop
    endif

    lefty = 0

    do iy = 1, ny+ky-1
       if (yknot(iy) .gt. yknot(iy+1)) then
          write(6,*) "subroutine dbs2vl:"
          write(6,*) "yknot(iy) <= yknot(iy+1) required."
          write(6,*) iy, yknot(iy), yknot(iy+1)
          stop
       endif
       if((yknot(iy) .le. y) .and. (y .lt. yknot(iy+1))) lefty = iy
    end do

    if(lefty .eq. 0) then
       write(6,*) "subroutine dbs2vl:"
       write(6,*) "iy with yknot(iy) <= y < yknot(iy+1) required."
       write(6,*) "yknot(iy)   = ", yknot(iy)
       write(6,*) "  y         = ", y
       write(6,*) "yknot(iy+1) = ", yknot(iy+1)
       stop
    endif

    do iky = 1, ky
       work(iky) = dbsdca(0,x,kx,xknot,nx,bcoef(1,lefty-ky+iky),leftx)
    end do

    dbs2vl = dbsval(y,ky,yknot(lefty-ky+1),ky,work)

  end function dbs2vl


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  function dbs2dr(iderx,idery,x,y,kx,ky,xknot,yknot,nx,ny,bcoef)

!
!  Evaluates the derivative of a two-dimensional tensor-product spline,
!  given its tensor-product B-spline representation.
!
!   iderx  - order of the derivative in the x-direction.  (input)
!   idery  - order of the derivative in the y-direction.  (input)
!   x      - x-coordinate of the point at which the spline is to be
!            evaluated.  (input)
!   y      - y-coordinate of the point at which the spline is to be
!            evaluated.  (input)
!   kx     - order of the spline in the x-direction.  (input)
!   ky     - order of the spline in the y-direction.  (input)
!   xknot  - array of length nx+kx containing the knot
!            sequence in the x-direction.  (input)
!            xknot must be nondecreasing.
!   yknot  - array of length ny+ky containing the knot
!            sequence in the y-direction.  (input)
!            yknot must be nondecreasing.
!   nx     - number of B-spline coefficients in the x-direction.
!            (input)
!   ny     - number of B-spline coefficients in the y-direction.
!            (input)
!   bcoef  - array of length nx*ny containing the
!            tensor-product B-spline coefficients.  (input)
!            bscoef is treated internally as a matrix of size nx
!            by ny.
!   dbs2dr  - value of the (iderx,idery) derivative of the spline at
!            (x,y).  (output)
!

    use numeric

    implicit none

    integer, intent(in)                          :: iderx, idery
    integer, intent(in)                          :: kx, nx, ky, ny
    real(kind=dbl)                               :: dbs2dr
    real(kind=dbl), intent(in)                   :: x, y
    real(kind=dbl), dimension(nx+kx), intent(in) :: xknot
    real(kind=dbl), dimension(ny+ky), intent(in) :: yknot
    real(kind=dbl), dimension(nx,ny), intent(in) :: bcoef

    integer                       :: ix, iy, iky, nintx, ninty
    real(kind=dbl), dimension(ky) :: work

!
!     check if knot(i) <= knot(i+1) and calculation of i so that
!     knot(i) <= x < knot(i+1)
!

    nintx = 0

    do ix = 1, nx+kx-1
       if (xknot(ix) .gt. xknot(ix+1)) then
          write(6,*) "subroutine dbs2dr:"
          write(6,*) "xknot(ix) <= xknot(ix+1) required."
          write(6,*) ix, xknot(ix), xknot(ix+1)
          stop
       endif
       if((xknot(ix) .le. x) .and. (x .lt. xknot(ix+1))) nintx = ix
    end do

    if(nintx .eq. 0) then
       write(6,*) "subroutine dbs2dr:"
       write(6,*) "ix with xknot(ix) <= x < xknot(ix+1) required."
       write(6,*) "x = ", x
       stop
    endif

    ninty = 0

    do iy = 1, ny+ky-1
       if (yknot(iy) .gt. yknot(iy+1)) then
          write(6,*) "subroutine dbs2dr:"
          write(6,*) "yknot(iy) <= yknot(iy+1) required."
          write(6,*) iy, yknot(iy), yknot(iy+1)
          stop
       endif
       if ((yknot(iy) .le. y) .and. (y .lt. yknot(iy+1))) ninty = iy
    end do

    if(ninty .eq. 0) then
       write(6,*) "subroutine dbs2dr:"
       write(6,*) "iy with yknot(iy) <= y < yknot(iy+1) required."
       write(6,*) "y = ", y
       stop
    endif

    do iky = 1, ky
       work(iky) =  dbsdca(iderx,x,kx,xknot,nx,bcoef(1,ninty-ky+iky),nintx)
    end do

    dbs2dr = dbsder(idery,y,ky,yknot(ninty-ky+1),ky,work)

  end function dbs2dr


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  subroutine dbs2gd(iderx,idery,nxvec,xvec,nyvec,yvec,kx,ky,xknot,yknot,      &
       & nx,ny,bcoef,val,ldf)

!
!  Evaluates the derivative of a two-dimensional tensor-product spline,
!  given its tensor-product B-spline representation on a grid.
!
!   iderx   - order of the derivative in the x-direction.  (input)
!   idery   - order of the derivative in the y-direction.  (input)
!   nxvec   - number of grid points in the x-direction.  (input)
!   xvec    - array of length nx containing the x-coordinates at
!             which the spline is to be evaluated.  (input)
!             the points in xvec should be strictly increasing.
!   nyvec   - number of grid points in the y-direction.  (input)
!   yvec    - array of length ny containing the y-coordinates at
!             which the spline is to be evaluated.  (input)
!             the points in yvec should be strictly increasing.
!   kx      - order of the spline in the x-direction.  (input)
!   ky      - order of the spline in the y-direction.  (input)
!   xknot   - array of length nx+kx containing the knot
!             sequence in the x-direction.  (input)
!             xknot must be nondecreasing.
!   yknot   - array of length ny+ky containing the knot
!             sequence in the y-direction.  (input)
!             yknot must be nondecreasing.
!   nx      - number of B-spline coefficients in the x-direction.
!             (input)
!   ny      - number of B-spline coefficients in the y-direction.
!             (input)
!   bcoef   - array of length nx*ny containing the
!             tensor-product B-spline coefficients.  (input)
!             bscoef is treated internally as a matrix of size nx
!             by ny.
!   val     - value of the (iderx,idery) derivative of the spline on
!             the nx by ny grid.  (output)
!             value(i,j) contains the derivative of the spline at the
!             point (xvec(i),yvec(j)).
!   ldf     - leading dimension of value exactly as specified in the
!             dimension statement of the calling program.  (input)
!

    use numeric

    implicit none

    integer, intent(in)                           :: iderx, idery
    integer, intent(in)                           :: nxvec, nyvec
    integer, intent(in)                           :: kx, nx, ky, ny
    integer, intent(in)                           :: ldf

    real(kind=dbl), dimension(nxvec), intent(in)  :: xvec
    real(kind=dbl), dimension(nyvec), intent(in)  :: yvec
    real(kind=dbl), dimension(nx+kx), intent(in)  :: xknot
    real(kind=dbl), dimension(ny+ky), intent(in)  :: yknot
    real(kind=dbl), dimension(nx,ny), intent(in)  :: bcoef
    real(kind=dbl), dimension(ldf,*), intent(out) :: val

    integer                                     :: i, ik, il, ix, iy, ikx, iky
    integer, dimension(nxvec)                   :: leftx
    integer, dimension(nyvec)                   :: lefty
    real(kind=dbl), dimension(nxvec,kx)         :: dl, dr
    real(kind=dbl), dimension(max(nxvec,nyvec)) :: save1
    real(kind=dbl), dimension(nxvec,kx)         :: biatx
    real(kind=dbl), dimension(nyvec,ky)         :: biaty
    real(kind=dbl), dimension(max(nxvec,nyvec)) :: term
    real(kind=dbl), dimension(ky)               :: work

    logical :: same,next


    leftx(1) = 0

    call huntn(xknot,nx+kx,kx,xvec(1),leftx(1))

    do ix = 2, nxvec
       leftx(ix) = leftx(ix-1)
       same = (xknot(leftx(ix)) .le. xvec(ix))                                &
            &        .and. (xvec(ix) .le. xknot(leftx(ix)+1))
       if(.not. same ) then
          leftx(ix) = leftx(ix) + 1
          next      = (xknot(leftx(ix)) .le. xvec(ix))                        &
               &           .and. (xvec(ix) .le. xknot(leftx(ix)+1))
          if (.not. next)                                                     &
               &           call huntn(xknot,nx+kx,kx,xvec(ix),leftx(ix))
       endif
    end do

    do i = 1, nx+kx-1
       if (xknot(i) .gt. xknot(i+1)) then
          write(6,*) "subroutine dbs2gd:"
          write(6,*) "xknot(i) <= xknot(i+1) required."
          write(6,*) i, xknot(i), xknot(i+1)
          write(6,*)
          write(6,*) xknot
          stop
       endif
    end do

    do i = 1, nxvec
       if ((xvec(i).lt.xknot(1)).or.(xvec(i).gt.xknot(nx+kx))) then
          write(6,*) "subroutine dbs2gd:"
          write(6,*) "ix with xknot(ix) <= x < xknot(ix+1) required."
          write(6,*) "x = ", xvec(i)
          stop
       endif
    end do

    lefty(1) = 0

    call huntn(yknot,ny+ky,ky,yvec(1),lefty(1))

    do iy = 2, nyvec
       lefty(iy) = lefty(iy-1)
       same = (yknot(lefty(iy)) .le. yvec(iy))                                &
            &        .and. (yvec(iy) .le. yknot(lefty(iy)+1))
       if(.not. same ) then
          lefty(iy) = lefty(iy) + 1
          next      = (yknot(lefty(iy)) .le. yvec(iy))                        &
               &           .and. (yvec(iy) .le. yknot(lefty(iy)+1))
          if (.not. next) call huntn(yknot,ny+ky,ky,yvec(iy),lefty(iy))
       endif
    end do

    do i = 1, ny+ky-1
       if (yknot(i) .gt. yknot(i+1)) then
          write(6,*) "subroutine dbs2gd:"
          write(6,*) "yknot(i) <= yknot(i+1) required."
          write(6,*) i, yknot(i), yknot(i+1)
          write(6,*)
          write(6,*) yknot
          stop
       endif
    end do

    do i = 1, nyvec
       if ((yvec(i).lt.yknot(1)).or.(yvec(i).gt.yknot(ny+ky))) then
          write(6,*) "subroutine dbs2gd:"
          write(6,*) "iy with yknot(iy) <= y < yknot(iy+1) required."
          write(6,*) "y = ", yvec(i)
          stop
       endif
    end do

    if ((iderx .eq. 0) .and. (idery .eq. 0)) then

       do ix = 1,nxvec
          biatx(ix,1) = 1._dbl
       end do

       do ik = 1, kx-1
          do ix = 1,nxvec
             dr(ix,ik) = xknot(leftx(ix)+ik) - xvec(ix)
             dl(ix,ik) = xvec(ix) - xknot(leftx(ix)+1-ik)
             save1(ix) = 0._dbl
          end do

          do il = 1,ik
             do ix = 1,nxvec
                term(ix)     = biatx(ix,il)                                   &
                     &                 / (dr(ix,il) + dl(ix,ik+1-il))
                biatx(ix,il) = save1(ix) + dr(ix,il) * term(ix)
                save1(ix)    = dl(ix,ik+1-il) * term(ix)
             end do
          end do

          do ix = 1, nxvec
             biatx(ix,ik+1) = save1(ix)
          end do
       end do

       do iy = 1, nyvec
          biaty(iy,1) = 1._dbl
       end do

       do ik = 1, ky-1
          do iy = 1, nyvec
             dr(iy,ik) = yknot(lefty(iy)+ik) - yvec(iy)
             dl(iy,ik) = yvec(iy) - yknot(lefty(iy)+1-ik)
             save1(iy) = 0._dbl
          end do

          do il = 1, ik
             do iy = 1,nyvec
                term(iy)     = biaty(iy,il)                                   &
                     &                 / (dr(iy,il) + dl(iy,ik+1-il))
                biaty(iy,il) = save1(iy) + dr(iy,il) * term(iy)
                save1(iy)    = dl(iy,ik+1-il) * term(iy)
             end do
          end do

          do iy = 1, nyvec
             biaty(iy,ik+1) = save1(iy)
          end do
       end do

       do iy = 1, nyvec
          do ix = 1, nxvec
             val(ix,iy) = 0.0_dbl
          end do
       end do

       do iky = 1, ky
          do ikx = 1, kx
             do iy = 1, nyvec
                do ix = 1, nxvec
                   val(ix,iy) = val(ix,iy)                                    &
                        & + biatx(ix,ikx) * biaty(iy,iky)                     &
                        & * bcoef(leftx(ix)-kx+ikx,lefty(iy)-ky+iky)
                end do
             end do
          end do
       end do

    elseif (((iderx .ge. 1) .or. (idery .ge. 1))                              &
         &  .and. ( (iderx .lt. kx) .and. (idery .lt. ky))) then

       do iy = 1, nyvec
          do ix = 1, nxvec
             do iky = 1, ky
                work(iky) = dbsdca(iderx,xvec(ix),kx,xknot,nx,                &
                     &             bcoef(1,lefty(iy)-ky+iky),leftx(ix))
             end do
             val(ix,iy) = dbsder(idery,yvec(iy),ky,                           &
                  &              yknot(lefty(iy)-ky+1),ky,work)
          end do
       end do

    else

       do iy = 1, nyvec
          do ix = 1, nxvec
             val(ix,iy) = 0.0_dbl
          end do
       end do

    endif

  end subroutine dbs2gd


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  subroutine dbs3in(nx,xvec,ny,yvec,nz,zvec,xyzdata,ldf,mdf,kx,ky,kz,         &
       & xknot,yknot,zknot,bcoef)

!
!  Computes a three-dimensional tensor-product spline interpolant,
!  returning the tensor-product B-spline coefficients.
!
!   nx      - number of data points in the x-direction.  (input)
!   xvec    - array of length nxdata containing the data points in
!             the x-direction.  (input)
!             xdata must be increasing.
!   ny      - number of data points in the y-direction.  (input)
!   yvec    - array of length nydata containing the data points in
!             the y-direction.  (input)
!             ydata must be increasing.
!   nz      - number of data points in the z-direction.  (input)
!   zvec    - array of length nzdata containing the data points in
!             the z-direction.  (input)
!             zdata must be increasing.
!   xyzdata - array of size nx by ny by nz containing the
!             values to be interpolated.  (input)
!             xyzdata(i,j,k) contains the value at
!             (xvec(i),yvec(j),zvec(k)).
!   ldf     - leading dimension of fdata exactly as specified in the
!             dimension statement of the calling program.  (input)
!   mdf     - middle dimension of fdata exactly as specified in the
!             dimension statement of the calling program.  (input)
!   kx      - order of the spline in the x-direction.  (input)
!             kxord must be less than or equal to nxdata.
!   ky      - order of the spline in the y-direction.  (input)
!             kyord must be less than or equal to nydata.
!   kz      - order of the spline in the z-direction.  (input)
!             kzord must be less than or equal to nzdata.
!   xknot   - array of length nx+kx containing the knot
!             sequence in the x-direction.  (input)
!             xknot must be nondecreasing.
!   yknot   - array of length ny+ky containing the knot
!             sequence in the y-direction.  (input)
!             yknot must be nondecreasing.
!   zknot   - array of length nz+kz containing the knot
!             sequence in the z-direction.  (input)
!             zknot must be nondecreasing.
!   bcoef   - array of length nx*ny*nz containing the
!             tensor-product B-spline coefficients.  (output)
!             bscoef is treated internally as a matrix of size nx
!             by ny by nz.
!

    use numeric

    implicit none

    integer, intent(in) :: nx, ny, nz, kx, ky, kz
    integer, intent(in) :: ldf, mdf

    real(kind=dbl), dimension(nx), intent(in)         :: xvec
    real(kind=dbl), dimension(ny), intent(in)         :: yvec
    real(kind=dbl), dimension(nz), intent(in)         :: zvec
    real(kind=dbl), dimension(nx+kx), intent(in)      :: xknot
    real(kind=dbl), dimension(ny+ky), intent(in)      :: yknot
    real(kind=dbl), dimension(nz+kz), intent(in)      :: zknot
    real(kind=dbl), dimension(ldf,mdf,nz), intent(in) :: xyzdata
    real(kind=dbl), dimension(nx,ny,nz), intent(out)  :: bcoef

    integer                                :: iz
    real(kind=dbl), dimension(nx,ny,nz)    :: work1
    real(kind=dbl), dimension(nz)          :: work2
    real(kind=dbl), dimension((2*kz-1)*nz) :: work3


    call spli3d(zvec,ldf,mdf,xyzdata,zknot,nz,kz,nx,ny,work2,work3,work1,     &
         &     nx,ny,nz)

    do iz = 1, nz
       call dbs2in(nx,xvec,ny,yvec,work1(1,1,iz),nx,kx,ky,xknot,yknot,        &
            &        bcoef(1,1,iz))
    end do

  end subroutine dbs3in


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  subroutine spli3d(xyzvec,ldf,mdf,xyzdata,xyzknot,n,k,m,l,work2,work3,       &
       & bcoef,nx,ny,nz)

    use numeric

    implicit none

    integer, intent(in)                               :: ldf, mdf, n, k, m, l
    integer, intent(in)                               :: nx, ny, nz
    real(kind=dbl), dimension(n), intent(in)          :: xyzvec
    real(kind=dbl), dimension(n+k), intent(in)        :: xyzknot
    real(kind=dbl), dimension(ldf,mdf,*), intent(in)  :: xyzdata
    real(kind=dbl), dimension(nx,ny,nz), intent(out)  :: bcoef
    real(kind=dbl), dimension(n), intent(out)         :: work2
    real(kind=dbl), dimension((2*k-1)*n), intent(out) :: work3

    integer        :: np1, km1, kpkm2, left, lenq, i, ilp1mx, j, jj, iflag, in
    real(kind=dbl) :: xyzveci


    np1   = n + 1
    km1   = k - 1
    kpkm2 = 2 * km1
    left  = k
    lenq  = n * (k + km1)

    do i = 1, lenq
       work3(i) = 0._dbl
    end do

    do i = 1, n
       xyzveci = xyzvec(i)
       ilp1mx  = min0(i+k,np1)
       left    = max0(left,i)
       if (xyzveci .lt. xyzknot(left)) go to 998
30     if (xyzveci .lt. xyzknot(left+1)) go to 40
       left = left + 1
       if (left .lt. ilp1mx) go to 30
       left = left - 1
       if (xyzveci .gt. xyzknot(left+1)) go to 998
40     call bsplvb(xyzknot,n+k,k,1,xyzveci,left,work2)
       jj = i - left + 1 + (left - k) * (k + km1)
       do j = 1, k
          jj    = jj + kpkm2
          work3(jj) = work2(j)
       end do
    end do

    call banfac(work3,k+km1,n,km1,km1,iflag)

    if (iflag .ne. 1) then
       write(6,*) "subroutine dbs3in: error"
       write(6,*) "no solution of linear equation system !!!"
       stop
    end if

    do j = 1, l
       do i = 1, m
          do in = 1, n
             work2(in) = xyzdata(i,j,in)
          end do

          call banslv(work3,k+km1,n,km1,km1,work2)

          do in = 1, n
             bcoef(i,j,in) = work2(in)
          end do

       end do
    end do

    return

998 write(6,*) "subroutine db3in:"
    write(6,*) "i with knot(i) <= x/y/z < knot(i+1) required."
    write(6,*) "knot(1)   = ", xyzknot(1)
    write(6,*) "knot(n+k) = ", xyzknot(n+k)
    write(6,*) "    x/y/z = ", xyzveci

    stop

  end subroutine spli3d


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  function dbs3vl(x,y,z,kx,ky,kz,xknot,yknot,zknot,nx,ny,nz,bcoef)

!
!  Evaluates a three-dimensional tensor-product spline, given its
!  tensor-product B-spline representation.
!
!   x      - x-coordinate of the point at which the spline is to be
!            evaluated.  (input)
!   y      - y-coordinate of the point at which the spline is to be
!            evaluated.  (input)
!   z      - z-coordinate of the point at which the spline is to be
!            evaluated.  (input)
!   kx     - order of the spline in the x-direction.  (input)
!   ky     - order of the spline in the y-direction.  (input)
!   kz     - order of the spline in the z-direction.  (input)
!   xknot  - array of length nx+kx containing the knot
!            sequence in the x-direction.  (input)
!            xknot must be nondecreasing.
!   yknot  - array of length ny+ky containing the knot
!            sequence in the y-direction.  (input)
!            yknot must be nondecreasing.
!   zknot  - array of length nz+kz containing the knot
!            sequence in the z-direction.  (input)
!            zknot must be nondecreasing.
!   nx     - number of B-spline coefficients in the x-direction.
!            (input)
!   ny     - number of B-spline coefficients in the y-direction.
!            (input)
!   nz     - number of B-spline coefficients in the z-direction.
!            (input)
!   bcoef  - array of length nx*ny*nz containing the
!            tensor-product B-spline coefficients.  (input)
!            bscoef is treated internally as a matrix of size nx
!            by ny by nz.
!   dbs3vl - value of the spline at (x,y,z).  (output)
!

    use numeric

    implicit none

    integer, intent(in)                             :: nx, ny, nz, kx, ky, kz
    real(kind=dbl), intent(in)                      :: x, y, z
    real(kind=dbl), dimension(nx+kx), intent(in)    :: xknot
    real(kind=dbl), dimension(ny+ky), intent(in)    :: yknot
    real(kind=dbl), dimension(nz+kz), intent(in)    :: zknot
    real(kind=dbl), dimension(nx,ny,nz), intent(in) :: bcoef
    real(kind=dbl)                                  :: dbs3vl

    integer                       :: iz, nintz
    real(kind=dbl), dimension(kz) :: work

!
!     check if knot(i) <= knot(i+1) and calculation of i so that
!     knot(i) <= x < knot(i+1)
!

    nintz = 0

    do iz = 1, nz+kz-1
       if (zknot(iz) .gt. zknot(iz + 1)) then
          write(6,*) "subroutine dbs3vl:"
          write(6,*) "zknot(iz) <= zknot(iz+1) required."
          write(6,*) iz, zknot(iz), zknot(iz+1)
          stop
       endif
       if((zknot(iz) .le. z) .and. (z .lt. zknot(iz + 1))) nintz = iz
    end do

    if(nintz .eq. 0) then
       write(6,*) "subroutine dbs3vl:"
       write(6,*) "iz with zknot(iz) <= z < zknot(iz+1) required."
       write(6,*) "zknot(iz)   = ", zknot(iz)
       write(6,*) "  z         = ", z
       write(6,*) "zknot(iz+1) = ", zknot(iz+1)
       stop
    endif

    do iz = 1, kz
       work(iz) = dbs2vl(x,y,kx,ky,xknot,yknot,nx,ny,bcoef(1,1,nintz-kz+iz))
    end do

    dbs3vl = dbsval(z,kz,zknot(nintz-kz+1),kz,work)

  end function dbs3vl


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  function dbs3dr(iderx,idery,iderz,x,y,z,kx,ky,kz,xknot,yknot,zknot,         &
       & nx,ny,nz,bcoef)

!
!  Evaluates the derivative of a three-dimensional tensor-product spline,
!  given its tensor-product B-spline representation.
!
!   iderx  - order of the x-derivative.  (input)
!   idery  - order of the y-derivative.  (input)
!   iderz  - order of the z-derivative.  (input)
!   x      - x-coordinate of the point at which the spline is to be
!            evaluated.  (input)
!   y      - y-coordinate of the point at which the spline is to be
!            evaluated.  (input)
!   z      - z-coordinate of the point at which the spline is to be
!            evaluated.  (input)
!   kx     - order of the spline in the x-direction.  (input)
!   ky     - order of the spline in the y-direction.  (input)
!   kz     - order of the spline in the z-direction.  (input)
!   xknot  - array of length nx+kx containing the knot
!            sequence in the x-direction.  (input)
!            xknot must be nondecreasing.
!   yknot  - array of length ny+ky containing the knot
!            sequence in the y-direction.  (input)
!            yknot must be nondecreasing.
!   zknot  - array of length nz+kz containing the knot
!            sequence in the z-direction.  (input)
!            zknot must be nondecreasing.
!   nx     - number of B-spline coefficients in the x-direction.
!            (input)
!   ny     - number of B-spline coefficients in the y-direction.
!            (input)
!   nz     - number of B-spline coefficients in the z-direction.
!            (input)
!   bcoef  - array of length nx*ny*nz containing the
!            tensor-product B-spline coefficients.  (input)
!            bscoef is treated internally as a matrix of size nx
!            by ny by nz.
!   dbs3dr - value of the (iderx,idery,iderz) derivative of the
!            spline at (x,y,z).  (output)
!

    use numeric

    implicit none

    integer, intent(in)                              :: iderx, idery, iderz
    integer, intent(in)                              :: nx, ny, nz, kx, ky, kz
    real(kind=dbl), intent(in)                       :: x, y, z
    real(kind=dbl), dimension(nx+kx), intent(in)     :: xknot
    real(kind=dbl), dimension(ny+ky), intent(in)     :: yknot
    real(kind=dbl), dimension(nz+kz), intent(in)     :: zknot
    real(kind=dbl), dimension(nx,ny,nz), intent(in)  :: bcoef
    real(kind=dbl)                                   :: dbs3dr

    integer                       :: iz, nintz
    real(kind=dbl), dimension(kz) :: work

!
!     check if knot(i) <= knot(i+1) and calculation of i so that
!     knot(i) <= x < knot(i+1)
!

    nintz = 0

    do iz = 1, nz+kz-1
       if (zknot(iz) .gt. zknot(iz + 1)) then
          write(6,*) "subroutine dbs3vl:"
          write(6,*) "zknot(iz) <= zknot(iz+1) required."
          write(6,*) iz, zknot(iz), zknot(iz+1)
          stop
       endif
       if((zknot(iz) .le. z) .and. (z .lt. zknot(iz + 1))) nintz = iz
    end do

    if(nintz .eq. 0) then
       write(6,*) "subroutine dbs3dr:"
       write(6,*) "iz with zknot(iz) <= z < zknot(iz+1) required."
       write(6,*) "zknot(iz)   = ", zknot(iz)
       write(6,*) "  z         = ", z
       write(6,*) "zknot(iz+1) = ", zknot(iz+1)
       stop
    endif

    do iz = 1, kz
       work(iz) = dbs2dr(iderx,idery,x,y,kx,ky,xknot,yknot,nx,ny,             &
            &        bcoef(1,1,nintz-kz+iz))
    end do

    dbs3dr = dbsder(iderz,z,kz,zknot(nintz-kz+1),kz,work)

  end function dbs3dr


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  subroutine dbs3gd(iderx,idery,iderz,nxvec,xvec,nyvec,yvec,nzvec,zvec,       &
       & kx,ky,kz,xknot,yknot,zknot,nx,ny,nz,bcoef,val,ldf,mdf)

!
!  Evaluates the derivative of a three-dimensional tensor-product spline,
!  given its tensor-product B-spline representation on a grid.
!
!   iderx  - order of the x-derivative.  (input)
!   idery  - order of the y-derivative.  (input)
!   iderz  - order of the z-derivative.  (input)
!   nx     - number of grid points in the x-direction.  (input)
!   xvec   - array of length nx containing the x-coordinates at
!            which the spline is to be evaluated.  (input)
!            the points in xvec should be strictly increasing.
!   ny     - number of grid points in the y-direction.  (input)
!   yvec   - array of length ny containing the y-coordinates at
!            which the spline is to be evaluated.  (input)
!            the points in yvec should be strictly increasing.
!   nz     - number of grid points in the z-direction.  (input)
!   zvec   - array of length nz containing the z-coordinates at
!            which the spline is to be evaluated.  (input)
!            the points in yvec should be strictly increasing.
!   kx     - order of the spline in the x-direction.  (input)
!   ky     - order of the spline in the y-direction.  (input)
!   kz     - order of the spline in the z-direction.  (input)
!   xknot  - array of length nx+kx containing the knot
!            sequence in the x-direction.  (input)
!            xknot must be nondecreasing.
!   yknot  - array of length ny+ky containing the knot
!            sequence in the y-direction.  (input)
!            yknot must be nondecreasing.
!   zknot  - array of length nz+kz containing the knot
!            sequence in the z-direction.  (input)
!            zknot must be nondecreasing.
!   nx     - number of B-spline coefficients in the x-direction.
!            (input)
!   ny     - number of B-spline coefficients in the y-direction.
!            (input)
!   nz     - number of B-spline coefficients in the z-direction.
!            (input)
!   bcoef  - array of length nx*ny*nz containing the
!            tensor-product B-spline coefficients.  (input)
!            bscoef is treated internally as a matrix of size nx
!            by ny by nz.
!   val    - array of size nx by ny by nz containing the values of
!            the (iderx,idery,iderz) derivative of the spline on the
!            nx by ny by nz grid.  (output)
!            value(i,j,k) contains the derivative of the spline at
!            the point (xvec(i), yvec(j), zvec(k)).
!   ldf    - leading dimension of value exactly as specified in the
!            dimension statement of the calling program.  (input)
!   mdf    - middle dimension of value exactly as specified in the
!            dimension statement of the calling program.  (input)
!

    use numeric

    implicit none

    integer, intent(in)                               :: iderx, idery, iderz
    integer, intent(in)                               :: nxvec, nyvec, nzvec
    integer, intent(in)                               :: kx, nx, ky, ny, kz, nz
    integer, intent(in)                               :: ldf,mdf

    real(kind=dbl), dimension(nxvec), intent(in)      :: xvec
    real(kind=dbl), dimension(nyvec), intent(in)      :: yvec
    real(kind=dbl), dimension(nzvec), intent(in)      :: zvec
    real(kind=dbl), dimension(nx+kx), intent(in)      :: xknot
    real(kind=dbl), dimension(ny+ky), intent(in)      :: yknot
    real(kind=dbl), dimension(nz+kz), intent(in)      :: zknot
    real(kind=dbl), dimension(nx,ny,nz), intent(in)   :: bcoef
    real(kind=dbl), dimension(ldf,mdf,*), intent(out) :: val

    integer                                           :: i, ik, il, ix, iy, iz
    integer                                           :: ikx, iky, ikz
    integer, dimension(nxvec)                         :: leftx
    integer, dimension(nyvec)                         :: lefty
    integer, dimension(nzvec)                         :: leftz
    real(kind=dbl), dimension(nxvec,kx)               :: biatx
    real(kind=dbl), dimension(nyvec,ky)               :: biaty
    real(kind=dbl), dimension(nzvec,kz)               :: biatz
    real(kind=dbl), dimension(max(nxvec,nyvec,nzvec)) :: term, save1

    real(kind=dbl), dimension(max(nxvec,nyvec,nzvec), max(kx,ky,kz)) :: dl, dr

    logical :: same,next


    do i = 1, nx+kx-1
       if (xknot(i) .gt. xknot(i+1)) then
          write(6,*) "subroutine dbs3gd:"
          write(6,*) "xknot(i) <= xknot(i+1) required."
          write(6,*) i, xknot(i), xknot(i+1)
          write(6,*)
          write(6,*) xknot
          stop
       endif
    end do

    do i = 1, nxvec
       if ((xvec(i).lt.xknot(1)).or.(xvec(i).gt.xknot(nx+kx))) then
          write(6,*) "subroutine dbs3gd:"
          write(6,*) "ix with xknot(ix) <= x < xknot(ix+1) required."
          write(6,*) "x = ", xvec(i)
          stop
       endif
    end do

    leftx(1) = 0

    call huntn(xknot,nx+kx,kx,xvec(1),leftx(1))

    do ix = 2, nxvec
       leftx(ix) = leftx(ix-1)
       same = (xknot(leftx(ix)) .le. xvec(ix))                                &
            &        .and. (xvec(ix) .le. xknot(leftx(ix)+1))
       if(.not. same ) then
          leftx(ix) = leftx(ix) + 1
          next      = (xknot(leftx(ix)) .le. xvec(ix))                        &
               &           .and. (xvec(ix) .le. xknot(leftx(ix)+1))
          if (.not. next) call huntn(xknot,nx+kx,kx,xvec(ix),leftx(ix))
       endif
    end do

    do i = 1, ny+ky-1
       if (yknot(i) .gt. yknot(i+1)) then
          write(6,*) "subroutine dbs3gd:"
          write(6,*) "yknot(i) <= yknot(i+1) required."
          write(6,*) i, yknot(i), yknot(i+1)
          write(6,*)
          write(6,*) yknot
          stop
       endif
    end do

    do i = 1, nyvec
       if ((yvec(i).lt.yknot(1)).or.(yvec(i).gt.yknot(ny+ky))) then
          write(6,*) "subroutine dbs3gd:"
          write(6,*) "iy with yknot(iy) <= y < yknot(iy+1) required."
          write(6,*) "y = ", yvec(i)
          stop
       endif
    end do

    lefty(1) = 0

    call huntn(yknot,ny+ky,ky,yvec(1),lefty(1))

    do iy = 2, nyvec
       lefty(iy) = lefty(iy-1)
       same = (yknot(lefty(iy)) .le. yvec(iy))                                &
            &        .and. (yvec(iy) .le. yknot(lefty(iy)+1))
       if(.not. same ) then
          lefty(iy) = lefty(iy) + 1
          next      = (yknot(lefty(iy)) .le. yvec(iy))                        &
               &           .and. (yvec(iy) .le. yknot(lefty(iy)+1))
          if (.not. next) call huntn(yknot,ny+ky,ky,yvec(iy),lefty(iy))
       endif
    end do

    do i = 1,nz+kz-1
       if (zknot(i) .gt. zknot(i+1)) then
          write(6,*) "subroutine dbs3gd:"
          write(6,*) "zknot(i) <= zknot(i+1) required."
          write(6,*) i, zknot(i), zknot(i+1)
          write(6,*)
          write(6,*) zknot
          stop
       endif
    end do

    do i = 1, nzvec
       if ((zvec(i).lt.zknot(1)).or.(zvec(i).gt.zknot(nz+kz))) then
          write(6,*) "subroutine dbs3gd:"
          write(6,*) "iz with zknot(iz) <= z < zknot(iz+1) required."
          write(6,*) "z = ", zvec(i)
          stop
       endif
    end do

    leftz(1) = 0

    call huntn(zknot,nz+kz,kz,zvec(1),leftz(1))

    do iz = 2, nzvec
       leftz(iz) = leftz(iz-1)
       same = (zknot(leftz(iz)) .le. zvec(iz))                                &
            &        .and. (zvec(iz) .le. zknot(leftz(iz)+1))
       if(.not. same ) then
          leftz(iz) = leftz(iz) + 1
          next      = (zknot(leftz(iz)) .le. zvec(iz))                        &
               &           .and. (zvec(iz) .le. zknot(leftz(iz)+1))
          if (.not. next) call huntn(zknot,nz+kz,kz,zvec(iz),leftz(iz))
       endif
    end do

    if ((iderx .eq. 0) .and. (idery .eq. 0) .and. (iderz .eq.0)) then

       do ix = 1, nxvec
          biatx(ix,1) = 1.0_dbl
       end do

       do ik = 1, kx-1
          do ix = 1, nxvec
             dr(ix,ik) = xknot(leftx(ix)+ik) - xvec(ix)
             dl(ix,ik) = xvec(ix) - xknot(leftx(ix)+1-ik)
             save1(ix) = 0._dbl
          end do

          do il = 1, ik
             do ix = 1, nxvec
                term(ix)     = biatx(ix,il) / (dr(ix,il) + dl(ix,ik+1-il))
                biatx(ix,il) = save1(ix) + dr(ix,il) * term(ix)
                save1(ix)    = dl(ix,ik+1-il) * term(ix)
             end do
          end do

          do ix = 1, nxvec
             biatx(ix,ik+1) = save1(ix)
          end do
       end do

       do iy = 1, nyvec
          biaty(iy,1) = 1.0_dbl
       end do

       do ik = 1, ky-1
          do iy = 1, nyvec
             dr(iy,ik) = yknot(lefty(iy)+ik) - yvec(iy)
             dl(iy,ik) = yvec(iy) - yknot(lefty(iy)+1-ik)
             save1(iy) = 0._dbl
          end do

          do il = 1,ik
             do iy = 1,nyvec
                term(iy)     = biaty(iy,il) / (dr(iy,il) + dl(iy,ik+1-il))
                biaty(iy,il) = save1(iy) + dr(iy,il) * term(iy)
                save1(iy)    = dl(iy,ik+1-il) * term(iy)
             end do
          end do

          do iy = 1,nyvec
             biaty(iy,ik+1) = save1(iy)
          end do
       end do

       do iz = 1,nzvec
          biatz(iz,1) = 1.0_dbl
       end do

       do ik = 1, kz-1
          do iz = 1, nzvec
             dr(iz,ik) = zknot(leftz(iz)+ik) - zvec(iz)
             dl(iz,ik) = zvec(iz) - zknot(leftz(iz)+1-ik)
             save1(iz) = 0._dbl
          end do

          do il = 1, ik
             do iz = 1, nzvec
                term(iz)     = biatz(iz,il) / (dr(iz,il) + dl(iz,ik+1-il))
                biatz(iz,il) = save1(iz) + dr(iz,il) * term(iz)
                save1(iz)    = dl(iz,ik+1-il) * term(iz)
             end do
          end do

          do iz = 1, nzvec
             biatz(iz,ik+1) = save1(iz)
          end do
       end do

       do iz = 1,nzvec
          do iy = 1,nyvec
             do ix = 1,nxvec
                val(ix,iy,iz) = 0.0_dbl
             end do
          end do
       end do

       do ikz = 1, kz
          do iky = 1, ky
             do ikx = 1, kx
                do iz = 1, nzvec
                   do iy = 1, nyvec
                      do ix = 1, nxvec
                         val(ix,iy,iz) = val(ix,iy,iz)                        &
                              &  + biatx(ix,ikx) * biaty(iy,iky)              &
                              &  * biatz(iz,ikz)                              &
                              &  * bcoef(leftx(ix)-kx+ikx,                    &
                              &          lefty(iy)-ky+iky,leftz(iz)-kz+ikz)
                      end do
                   end do
                end do
             end do
          end do
       end do

    else

       do iz = 1, nzvec
          do iy = 1, nyvec
             do ix = 1, nxvec
                val(ix,iy,iz) = dbs3dr(iderx,idery,iderz,xvec(ix),            &
                     &  yvec(iy),zvec(iz),kx,ky,kz,xknot,yknot,               &
                     &  zknot,nx,ny,nz,bcoef)
             end do
          end do
       end do

    endif

  end subroutine dbs3gd


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  subroutine bsplvb(t,n,jhigh,index,x,left,biatx)

    use numeric

    implicit none

    integer, intent(in) :: n, jhigh, index, left

    real(kind=dbl), intent(in)                    :: x
    real(kind=dbl), dimension(n), intent(in)      :: t
    real(kind=dbl), dimension(jhigh), intent(out) :: biatx

    integer                          :: j = 1
    integer                          :: i, jp1
    real(kind=dbl)                   :: saved, term
    real(kind=dbl), dimension(jhigh) :: dl, dr


    if (index .eq. 1) then
       j = 1
       biatx(1) = 1.0_dbl
       if (j .ge. jhigh) return
    end if

20  jp1 = j + 1

    dr(j) = t(left+j) - x
    dl(j) = x - t(left+1-j)
    saved = 0._dbl

    do i = 1, j
       term     = biatx(i) / (dr(i) + dl(jp1-i))
       biatx(i) = saved + dr(i) * term
       saved    = dl(jp1-i) * term
    end do

    biatx(jp1) = saved
    j          = jp1

    if (j .lt. jhigh) go to 20

  end subroutine bsplvb


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  subroutine banfac(w,nroww,nrow,nbandl,nbandu,iflag)

    use numeric

    implicit none

    integer, intent(in)                                  :: nroww,nrow
    integer, intent(in)                                  :: nbandl,nbandu
    integer, intent(out)                                 :: iflag
    real(kind=dbl), dimension(nroww,nrow), intent(inout) :: w

    real(kind=dbl) :: pivot, factor
    integer        :: middle, nrowm1, jmax, kmax, ipk, midmk, i, j, k


    iflag  = 1
    middle = nbandu + 1
    nrowm1 = nrow - 1

    if (nrowm1 .lt. 0) goto 999
    if (nrowm1 .eq. 0) goto 900
    if (nrowm1 .gt. 0) goto 10

10  if (nbandl .gt. 0) go to 30

    do i = 1, nrowm1
       if (w(middle,i) .eq. 0._dbl) go to 999
    end do

    go to 900

30  if (nbandu .gt. 0) go to 60

    do i = 1, nrowm1
       pivot = w(middle,i)
       if(pivot .eq. 0._dbl) go to 999
       jmax = min0(nbandl, nrow - i)
       do j = 1, jmax
          w(middle+j,i) = w(middle+j,i) / pivot
       end do
    end do

    return

60  do i = 1, nrowm1
       pivot = w(middle,i)
       if (pivot .eq. 0._dbl) go to 999
       jmax = min0(nbandl,nrow - i)
       do j = 1,jmax
          w(middle+j,i) = w(middle+j,i) / pivot
       end do

       kmax = min0(nbandu,nrow - i)

       do k = 1, kmax
          ipk    = i + k
          midmk  = middle - k
          factor = w(midmk,ipk)
          do j = 1, jmax
             w(midmk+j,ipk) = w(midmk+j,ipk) - w(middle+j,i)                  &
                  &              * factor
          end do
       end do
    end do

900 if (w(middle,nrow) .ne. 0._dbl) return
999 iflag = 2

  end subroutine banfac


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  subroutine banslv(w,nroww,nrow,nbandl,nbandu,b)

    use numeric

    implicit none

    integer, intent(in)                               :: nroww,nrow
    integer, intent(in)                               :: nbandl,nbandu
    real(kind=dbl), dimension(nroww,nrow), intent(in) :: w
    real(kind=dbl), dimension(nrow), intent(inout)    :: b

    integer :: middle, nrowm1, jmax, i, j

    middle = nbandu + 1
    if (nrow .eq. 1) goto 99
    nrowm1 = nrow - 1
    if (nbandl .eq. 0) goto 30

    do i = 1, nrowm1
       jmax = min0(nbandl, nrow - i)
       do j = 1, jmax
          b(i+j) = b(i+j) - b(i) * w(middle+j,i)
       end do
    end do

30  if (nbandu .gt. 0)  goto 50

    do i = 1, nrow
       b(i) = b(i) / w(1,i)
    end do

    return

50  do i = nrow, 2, -1
       b(i) = b(i)/w(middle,i)
       jmax = min0(nbandu,i-1)
       do j = 1, jmax
          b(i-j) = b(i-j) - b(i) * w(middle-j,i)
       end do
    end do

99  b(1) = b(1) / w(middle,1)

  end subroutine banslv


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  subroutine huntn(xx,n,kord,x,jlo)

    use numeric

    implicit none

    integer, intent(in)                      :: n, kord
    real(kind=dbl), intent(in)               :: x
    real(kind=dbl), dimension(n), intent(in) :: xx

    integer, intent(inout)                   :: jlo

    integer :: max, null, jhi, jm, inc

!
!     works only for B-Splines (order n)
!

    max  = n - kord
    null = kord

    if (jlo.le.null.or.jlo.gt.max) then
       jlo = null
       jhi = max+1
       goto 30
    endif

    inc = 1

    if (x .ge. xx(jlo)) then
10     jhi = jlo + inc
       if (jhi .gt. max) then
          jhi = max + 1
       else if (x .ge. xx(jhi)) then
          jlo = jhi
          inc = inc + inc
          goto 10
       endif
    else
       jhi = jlo
20     jlo = jhi - inc
       if (jlo .le. null) then
          jlo = null
       else if (x .lt. xx(jlo)) then
          jhi = jlo
          inc = inc + inc
          goto 20
       endif
    endif

30  if (jhi-jlo.eq.1) return

    jm = (jhi + jlo) / 2
    if (x .gt. xx(jm)) then
       jlo = jm
    else
       jhi = jm
    endif

    goto 30

  end subroutine huntn


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


end module bspline

!
! Routines borrowed from Hager's gengrid2 package, primarily from
! node3.F90.  These have been simplified for use with DEGAS 2's
! efit2dg2d routine and adapted to use the bspline90_22.f90 spline
! fitting routines.
!
 
! RK4 routine to follow the flux surface
subroutine get_next_point_tang(x_in,dltheta,direction,x_out,dist)
  implicit none
  real (kind=8), intent(in) :: x_in(2), dltheta
  real (kind=8), intent(inout) :: x_out(2), dist
  integer, intent(in) :: direction
  real (kind=8), dimension(2) :: polv
  real (kind=8) :: abspolv, dlt, dlc
  real (kind=8) :: xp(2,4), x(2,5)
  real (kind=8), external :: psi_interp
  integer :: i
  integer, parameter :: maxsteps=50

  x(:,5)=x_in
  ! Plan for maxsteps/2 iterations -->
  dlt=dltheta/real(maxsteps/2,8)
  dist=0.D0
  dlc=0

  do i=1,maxsteps
    x(:,1)=x(:,5)
    ! Direction is counter-clockwise!
    polv(1)=-psi_interp(x(1,1),x(2,1),0,1)
    polv(2)=psi_interp(x(1,1),x(2,1),1,0)
    abspolv=dsqrt(polv(1)**2+polv(2)**2)
    xp(:,1)=polv/abspolv
    ! I need to implement a fallback in case we are on the separatrix:
    !if (abspolv .lt. 1d-13) then
    !   print *, 'get_next_point_gl: Warning, gradient smaller than threshold!'
    !endif
    ! Predictor steps
    x(:,2)=x(:,1)+direction*(0.5D0*dlt)*xp(:,1)
    polv(1)=-psi_interp(x(1,2),x(2,2),0,1)
    polv(2)=psi_interp(x(1,2),x(2,2),1,0)
    abspolv=dsqrt(polv(1)**2+polv(2)**2)
    xp(:,2)=polv/abspolv

    x(:,3)=x(:,1)+direction*(0.5D0*dlt)*xp(:,2)
    polv(1)=-psi_interp(x(1,3),x(2,3),0,1)
    polv(2)=psi_interp(x(1,3),x(2,3),1,0)
    abspolv=dsqrt(polv(1)**2+polv(2)**2)
    xp(:,3)=polv/abspolv

    x(:,4)=x(:,1)+direction*dlt*xp(:,3)
    polv(1)=-psi_interp(x(1,4),x(2,4),0,1)
    polv(2)=psi_interp(x(1,4),x(2,4),1,0)
    abspolv=dsqrt(polv(1)**2+polv(2)**2)
    xp(:,4)=polv/abspolv

    ! Corrector step
    x(:,5)=x(:,1)+direction*(dlt/6D0)*(xp(:,1)+2D0*(xp(:,2)+xp(:,3))+xp(:,4))
    dlc=dlc+dsqrt(sum((x(:,5)-x(:,1))**2))
    if (dlc .ge. dltheta) then
      if (dlc .gt. 1.5D0*dltheta) then
        ! We have gone too far, go one step back
        dist=dlc-dsqrt(sum((x(:,5)-x(:,1))**2))
        x(:,5)=x(:,1)
      else
        dist=dlc
      endif
      ! exit the loop
      x_out=x(:,5)
      exit
    endif
    dist=dlc
  enddo
end subroutine get_next_point_tang

! RK2 routine to follow the Psi gradient a given distance dlperp
subroutine get_next_point_perp(x_in,dlperp,direction,x_out,dist)
  implicit none
  real (kind=8), intent(in) :: x_in(2), dlperp
  real (kind=8), intent(inout) :: x_out(2), dist
  integer, intent(in) :: direction
  real (kind=8), dimension(2) :: gradpsi
  real (kind=8) :: x1(2),x2(2), absgradpsi, dlp, dlc
  real (kind=8), external :: psi_interp
  integer :: i
  integer, parameter :: maxsteps=20

  x2=x_in
  ! Plan for maxsteps/2 iterations -->
  dlp=dlperp/real(maxsteps/2,8)
  dist=0.D0
  dlc=0

  do i=1,maxsteps
    x1=x2
    gradpsi(1)=psi_interp(x1(1),x1(2),1,0)
    gradpsi(2)=psi_interp(x1(1),x1(2),0,1)
    absgradpsi=dsqrt(gradpsi(1)**2+gradpsi(2)**2)
    ! I need to implement a fallback in case we are on the separatrix:
    if (absgradpsi .lt. 1d-13) then
       print *, 'get_next_point_perp: Warning, gradient smaller than threshold!'
    endif
    ! Predictor step
    x2=x1+direction*(0.5D0*dlp)*gradpsi/absgradpsi
    gradpsi(1)=psi_interp(x2(1),x2(2),1,0)
    gradpsi(2)=psi_interp(x2(1),x2(2),0,1)
    absgradpsi=dsqrt(gradpsi(1)**2+gradpsi(2)**2)
    ! Corrector step
    x2=x1+direction*dlp*gradpsi/absgradpsi
    dlc=dlc+dsqrt(sum((x2-x1)**2))
    if (dlc .ge. dlperp) then
      if (dlc .gt. 1.5D0*dlperp) then
        ! We have gone too far, go one step back
        dist=dlc-dsqrt(sum((x2-x1)**2))
        x2=x1
      else
        dist=dlc
      endif
      ! exit the loop
      x_out=x2
      exit
    endif
    dist=dlc
  enddo
end subroutine get_next_point_perp

subroutine fsrefine_xgca(x_in,psi_ref,dr,nlevels,maxe,success)
!!$  use itp_module
!!$  use eqd_module
!!$  use grid_module
!!$  use EZspline_obj
!!$  use EZspline

  use bspline
  implicit none
  integer, intent(in) :: nlevels
  real (kind=8), intent(in) :: psi_ref, dr
  real (kind=8), intent(inout) :: x_in(2)
  integer, intent(out) :: success
  real (kind=8), intent(out) :: maxe
  real (kind=8), external :: psi_interp
  real (kind=8), dimension(nlevels) :: rline, zline, psi_line, dline
  integer :: i, j, center, nlevels2, fs_sort(nlevels)
  real (kind=8) :: error, errorold, dist, error_init
  integer, parameter :: maxsteps=20
  real (kind=8) :: minerr, x_old(2), x_new(2)
  integer, parameter :: korder_rz=5
  real (kind=8), allocatable :: psi_line_knot(:), rline_coef(:), zline_coef(:)
!
! Set up a grid of nlevels points centered on the input point 
! x_in.  Will then step dr in direction perpendicular to psi in
! either direction.
! 
  center=(nlevels-1)/2+1
  nlevels2=center-1
  success=0

  ! Find inner and outer points for psi correction
  rline(center)=x_in(1)
  zline(center)=x_in(2)
  dline(center)=0D0
  psi_line(center)=psi_interp(x_in(1),x_in(2),0,0)

  error_init=abs(psi_ref-psi_line(center))

  x_old=x_in
  do i=1,nlevels2
    call get_next_point_perp(x_old,dr,1,x_new,dist)
    rline(center+i)=x_new(1)
    zline(center+i)=x_new(2)
    dline(center+i)=dline(center+(i-1))+dist
    psi_line(center+i)=psi_interp(x_new(1),x_new(2),0,0)
    x_old=x_new
  enddo
  x_old=x_in
  do i=1,nlevels2
    call get_next_point_perp(x_old,dr,-1,x_new,dist)
    rline(center-i)=x_new(1)
    zline(center-i)=x_new(2)
    dline(center-i)=dline(center-(i-1))-dist
    psi_line(center-i)=psi_interp(x_new(1),x_new(2),0,0)
    x_old=x_new
  enddo

  !Test if psi_ref is inside bounds of psi_line
  x_old(1)=rline(1)
  x_old(2)=zline(1)
  do while (psi_line(1) > psi_ref)
    call get_next_point_perp(x_old,dr,-1,x_new,dist)
    rline(1)=x_new(1)
    zline(1)=x_new(2)
    dline(1)=dline(1)-dist
    psi_line(1)=psi_interp(x_new(1),x_new(2),0,0)
    x_old=x_new
  enddo

  x_old(1)=rline(nlevels)
  x_old(2)=zline(nlevels)
  do while (psi_line(nlevels) < psi_ref)
    call get_next_point_perp(x_old,dr,1,x_new,dist)
    rline(nlevels)=x_new(1)
    zline(nlevels)=x_new(2)
    dline(nlevels)=dline(nlevels)+dist
    psi_line(nlevels)=psi_interp(x_new(1),x_new(2),0,0)
    x_old=x_new
  enddo

  ! Check whether psi_line is monotonic
  do i=2,nlevels
    if (psi_line(i) .lt. psi_line(i-1)) then
      return
    endif
  enddo

  error=abs(psi_line(center)-psi_ref)
  maxe=error
  errorold=error+1d0
  minerr=1.D10

  do while(dabs(error-errorold) .gt. 1.0D-15 .and. i .lt. maxsteps)
    errorold=error
    do j=1,nlevels
      fs_sort(j)=j
    enddo
    call qsort_int(1,nlevels)
!
!!$    call init_1d_interpolation(spl_psi1,psi_line(fs_sort),rline(fs_sort),nlevels,.false.)
!!$    call init_1d_interpolation(spl_psi2,psi_line(fs_sort),zline(fs_sort),nlevels,.false.)
!!$    rline(center)=interpol_1d(psi_ref,spl_psi1)
!!$    zline(center)=interpol_1d(psi_ref,spl_psi2)
!!$    call finalize_1d_interpolation(spl_psi1)
!!$    call finalize_1d_interpolation(spl_psi2)

    allocate(psi_line_knot(nlevels+korder_rz),rline_coef(nlevels),       &
         zline_coef(nlevels))
    call dbsnak(nlevels,psi_line(fs_sort),korder_rz,psi_line_knot)
    call dbsint(nlevels,psi_line(fs_sort),rline(fs_sort),korder_rz,      &
                psi_line_knot,rline_coef)
    call dbsint(nlevels,psi_line(fs_sort),zline(fs_sort),korder_rz,      &
         psi_line_knot,zline_coef)
    rline(center)=dbsval(psi_ref,korder_rz,psi_line_knot,nlevels,rline_coef)
    zline(center)=dbsval(psi_ref,korder_rz,psi_line_knot,nlevels,zline_coef)
    deallocate(psi_line_knot,rline_coef,zline_coef)


    psi_line(center)=psi_interp(rline(center),zline(center),0,0)
    error=dabs(psi_line(center)-psi_ref)
    if (error .lt. minerr) minerr=error
!    if(.false.) then
    if (error .gt. 2*minerr) then
      !print *, 'fsrefine_xgca: Convergence error'
      if (error .lt. error_init) then
        maxe=error
        x_in(1)=rline(center)
        x_in(2)=zline(center)
      else
        maxe=error_init
      endif
      return
    endif
  enddo

  success=1
  x_in(1)=rline(center)
  x_in(2)=zline(center)
  maxe=error

  contains

    recursive subroutine qsort_int(left,right)
      implicit none
      integer :: left,right
      integer :: i,j,p, store
      !initialize
      p=(left+right)/2
      if(right<=left) return
      
      ! partition
      call sort_swap_int(right,p) ! move pivot to right position
      p=right                 ! indicate pivot
      store=left-1            ! set to zero
      do i=left, right-1
         if(compare_int(i,p)<0) then
            store=store+1
            call sort_swap_int(store,i)
         endif
      enddo
      call sort_swap_int(p,store+1)
      p=store+1
      !//resursion
      call qsort_int(left,p-1)
      call qsort_int(p+1,right)
    end subroutine qsort_int
    subroutine sort_swap_int(i,j)
      implicit none
      integer :: i,j
      integer :: tmp
      
      if(i==j) return
      tmp=fs_sort(i)
      fs_sort(i)=fs_sort(j)
      fs_sort(j)=tmp
    end subroutine sort_swap_int
    integer function compare_int(iin,jin)
      implicit none
      integer, intent(in) :: iin,jin
      integer :: i,j
      real (kind=8) :: i1,j1
      
      i=fs_sort(iin)
      j=fs_sort(jin)

      i1=psi_line(i)
      j1=psi_line(j)

      if( i1<j1) then
         compare_int=-1
      else if(i1==j1) then
         compare_int=0
      else
         compare_int=1
      endif
    end function compare_int

end subroutine fsrefine_xgca

!******************************************
! This algorithm is based on the FORTRAN routine by
! W. Randolf Franklin 
! (http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html).
! As described there, the logical expressions are carefully chosen
! to deal with the various special cases via the method of
! Simulation of Simplicity [H. Edelsbrunner and E. P. Mucke, ACM
! Trans. Graphics 9, 66 (1990)].
!
! (x,y) is the input point in question
! (xlim(i),ylim(i)) i=1,nlim represents the polygon
! The returned values are:
! -1 if the point is outside the polygon
!  0 if the point is on an edge or at a vertex
!  1 if the point is inside the polygon
!
! However, a detailed investigation of the algorithm shows
! that it does not always return 0 if the point is on an
! edge or at a vertex.  In particular, it will NOT return 0
! if that edge is purely horizontal or vertical.  Consequently,
! this routine should not be used to identify points precisely
! on the boundary.
!
!******************************************
integer function pointinpoly(x,y,xlim,ylim,nlim)

 implicit none
 integer, intent(in) :: nlim
 real (kind=8), intent(in) :: x,y,xlim(nlim),ylim(nlim)
 real (kind=8) :: xdiff(nlim),ydiff(nlim),det
 integer :: i,j
 logical :: xi,xj,yi,yj

 xdiff=xlim-x
 ydiff=ylim-y

 pointinpoly=-1
 do i=1,nlim
    j=1+mod(i,nlim)
    xi = (xdiff(i) >= 0D0)
    xj = (xdiff(j) >= 0D0)
    yi = (ydiff(i) >= 0D0)
    yj = (ydiff(j) >= 0D0)
    if (.not. ( .not.((yi .or. yj).and.(xi .or. xj)) .or. (xi .and. xj))) then
       if (.not.(yi .and. yj .and. (xi .or. xj) .and. .not. (xi .and. xj))) then
!
!  The above logic prevents cases in which this denominator 
!  vanishes from getting to this point.
!
          det=(ydiff(i)*xdiff(j) - xdiff(i)*ydiff(j)) / (xdiff(j) - xdiff(i))
          if (det == 0D0) then
             pointinpoly=0 
             return
          else if (det > 0D0) then
             pointinpoly=-pointinpoly
          end if
       else
          pointinpoly=-pointinpoly
       end if
    end if
 end do

 end function pointinpoly

