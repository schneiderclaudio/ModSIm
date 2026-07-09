! minpack.f90 — MINPACK Levenberg-Marquardt routines (public domain, Argonne 1980)
! Routines included: lmdif1, lmdif, lmpar, qrfac, qrsolv, fdjac2, enorm
! Converted to free-form Fortran 90.

!***********************************************************************
      subroutine lmdif1(fcn, m, n, x, fvec, tol, info, wa, lwa)
      implicit none
      integer, intent(in)    :: m, n, lwa
      real,    intent(inout) :: x(n)
      real,    intent(out)   :: fvec(m)
      real,    intent(in)    :: tol
      integer, intent(out)   :: info
      real,    intent(inout) :: wa(lwa)
      external fcn

      integer :: maxfev, mode, nprint, nfev
      real    :: ftol, xtol, gtol, factor, epsfcn
      integer :: ipvt(n)
      real    :: diag(n), qtf(n), wa1(n), wa2(n), wa3(n), wa4(m)
      integer :: ldfjac

      if (n <= 0 .or. m < n .or. tol < 0.0 .or. lwa < m*n+5*n+m) then
        info = 0; return
      end if

      ftol   = tol
      xtol   = tol
      gtol   = 0.0
      maxfev = 200 * (n + 1)
      epsfcn = 0.0
      mode   = 1
      factor = 100.0
      nprint = 0
      ldfjac = m

      call lmdif(fcn, m, n, x, fvec, ftol, xtol, gtol, maxfev, epsfcn, &
                 diag, mode, factor, nprint, info, nfev, &
                 wa(1), ldfjac, ipvt, qtf, &
                 wa(ldfjac*n+1), wa(ldfjac*n+n+1), &
                 wa(ldfjac*n+2*n+1), wa(ldfjac*n+3*n+1))

      if (info == 8) info = 4
      end subroutine

!***********************************************************************
      subroutine lmdif(fcn, m, n, x, fvec, ftol, xtol, gtol, maxfev, &
                       epsfcn, diag, mode, factor, nprint, info, nfev, &
                       fjac, ldfjac, ipvt, qtf, wa1, wa2, wa3, wa4)
      implicit none
      integer, intent(in)    :: m, n, maxfev, mode, nprint, ldfjac
      real,    intent(in)    :: ftol, xtol, gtol, epsfcn, factor
      real,    intent(inout) :: x(n), diag(n)
      real,    intent(out)   :: fvec(m), fjac(ldfjac,n), qtf(n)
      real,    intent(out)   :: wa1(n), wa2(n), wa3(n), wa4(m)
      integer, intent(out)   :: info, nfev, ipvt(n)
      external fcn
      real, external :: enorm

      integer :: i, iflag, iter, j, l
      real    :: actred, delta, dirder, epsmch, fnorm, fnorm1
      real    :: gnorm, par, pnorm, prered, ratio
      real    :: sum_, temp, temp1, temp2, xnorm
      real    :: p1, p5, p25, p75, p0001
      logical :: sing

      p1 = 0.1; p5 = 0.5; p25 = 0.25; p75 = 0.75; p0001 = 1.0e-4
      epsmch = epsilon(1.0)

      info  = 0; iflag = 0; nfev  = 0

      ! check inputs
      if (n <= 0 .or. m < n .or. ftol < 0.0 .or. xtol < 0.0 .or. &
          gtol < 0.0 .or. maxfev <= 0 .or. factor <= 0.0 .or. &
          ldfjac < m) return
      if (mode == 2) then
        do j = 1, n
          if (diag(j) <= 0.0) return
        end do
      end if

      ! evaluate function at starting point
      iflag = 1
      call fcn(m, n, x, fvec, iflag)
      nfev = 1
      if (iflag < 0) goto 300
      fnorm = enorm(m, fvec)

      ! initialise
      do j = 1, n
        wa2(j) = 0.0
      end do
      iter  = 1
      par   = 0.0
      delta = 0.0
      xnorm = 0.0

      ! outer loop
      do
        ! compute jacobian
        iflag = 2
        call fdjac2(fcn, m, n, x, fvec, fjac, ldfjac, iflag, epsfcn, wa4)
        nfev = nfev + n
        if (iflag < 0) goto 300

        ! QR factorisation of jacobian
        call qrfac(m, n, fjac, ldfjac, .true., ipvt, n, wa1, wa2, wa3)

        ! on first iteration scale according to norms of columns of jacobian
        if (iter == 1) then
          if (mode /= 2) then
            do j = 1, n
              diag(j) = wa2(j)
              if (wa2(j) == 0.0) diag(j) = 1.0
            end do
          end if
          wa3 = diag * x
          xnorm = enorm(n, wa3)
          delta = factor * xnorm
          if (delta == 0.0) delta = factor
        end if

        ! form Q^T * fvec
        do i = 1, m
          wa4(i) = fvec(i)
        end do
        do j = 1, n
          if (fjac(j,j) /= 0.0) then
            sum_ = 0.0
            do i = j, m
              sum_ = sum_ + fjac(i,j)*wa4(i)
            end do
            temp = -sum_/fjac(j,j)
            do i = j, m
              wa4(i) = wa4(i) + fjac(i,j)*temp
            end do
          end if
          fjac(j,j) = wa1(j)
          qtf(j)    = wa4(j)
        end do

        ! compute norm of scaled gradient
        gnorm = 0.0
        if (fnorm /= 0.0) then
          do j = 1, n
            l = ipvt(j)
            if (wa2(l) /= 0.0) then
              sum_ = 0.0
              do i = 1, j
                sum_ = sum_ + fjac(i,j)*qtf(i)/fnorm
              end do
              gnorm = max(gnorm, abs(sum_/wa2(l)))
            end if
          end do
        end if

        ! test for convergence on gradient
        if (gnorm <= gtol) info = 4

        if (info /= 0) goto 300

        ! rescale if necessary
        if (mode /= 2) then
          do j = 1, n
            diag(j) = max(diag(j), wa2(j))
          end do
        end if

        ! inner loop
        do
          ! determine LM parameter
          call lmpar(n, fjac, ldfjac, ipvt, diag, qtf, delta, par, wa1, wa2, wa3, wa4)

          ! store trial step; compute norm
          wa1 = -wa1
          wa3 = diag * wa1
          pnorm = enorm(n, wa3)

          ! scale if first iteration
          if (iter == 1) delta = min(delta, pnorm)

          ! evaluate at x + p
          wa2 = x + wa1
          iflag = 1
          call fcn(m, n, wa2, wa4, iflag)
          nfev = nfev + 1
          if (iflag < 0) goto 300
          fnorm1 = enorm(m, wa4)

          ! compute actual / predicted reduction
          actred = -1.0
          if (p1*fnorm1 < fnorm) actred = 1.0 - (fnorm1/fnorm)**2

          temp1 = enorm(n, wa1*wa2/delta) / fnorm
          temp2 = sqrt(par) * pnorm / fnorm
          prered = temp1**2 + temp2**2 / p5
          dirder = -(temp1**2 + temp2**2)

          ratio = 0.0
          if (prered /= 0.0) ratio = actred/prered

          ! update step bound
          if (ratio <= p25) then
            if (actred >= 0.0) then
              temp = p5
            else
              temp = p5*dirder/(dirder + p5*actred)
            end if
            if (p1*fnorm1 >= fnorm .or. temp < p1) temp = p1
            delta = temp * min(delta, pnorm/p1)
            par   = par/temp
          else if (par == 0.0 .or. ratio >= p75) then
            delta = pnorm / p5
            par   = p5*par
          end if

          ! test for successful iteration
          if (ratio >= p0001) then
            x    = wa2
            wa2  = diag * x
            fvec = wa4
            xnorm = enorm(n, wa2)
            fnorm = fnorm1
            iter = iter + 1
          end if

          ! test for convergence
          info = 0
          if (abs(actred) <= ftol .and. prered <= ftol .and. p5*ratio <= 1.0) info = 1
          if (delta <= xtol*xnorm) info = 2
          if (abs(actred) <= ftol .and. prered <= ftol .and. p5*ratio <= 1.0 &
              .and. info == 2) info = 3

          if (info /= 0) goto 300
          if (nfev >= maxfev) info = 5
          if (abs(actred) <= epsmch .and. prered <= epsmch .and. p5*ratio <= 1.0) info = 6
          if (delta <= epsmch*xnorm) info = 7
          if (gnorm <= epsmch) info = 8
          if (info /= 0) goto 300

          ! end of inner loop (exit when ratio >= p0001)
          if (ratio >= p0001) exit
        end do
        ! end of outer loop
      end do

300   continue
      if (iflag < 0) info = iflag
      iflag = 0
      if (nprint > 0) call fcn(m, n, x, fvec, iflag)
      end subroutine

!***********************************************************************
      subroutine fdjac2(fcn, m, n, x, fvec, fjac, ldfjac, iflag, epsfcn, wa)
      implicit none
      integer, intent(in)    :: m, n, ldfjac
      real,    intent(inout) :: x(n)
      real,    intent(in)    :: fvec(m), epsfcn
      real,    intent(out)   :: fjac(ldfjac,n), wa(m)
      integer, intent(inout) :: iflag
      external fcn

      integer :: i, j
      real    :: eps, epsmch, h, temp

      epsmch = epsilon(1.0)
      eps    = sqrt(max(epsfcn, epsmch))

      do j = 1, n
        temp = x(j)
        h    = eps * abs(temp)
        if (h == 0.0) h = eps
        x(j) = temp + h
        call fcn(m, n, x, wa, iflag)
        if (iflag < 0) return
        x(j) = temp
        do i = 1, m
          fjac(i,j) = (wa(i) - fvec(i)) / h
        end do
      end do
      end subroutine

!***********************************************************************
      subroutine qrfac(m, n, a, lda, pivot, ipvt, lipvt, rdiag, acnorm, wa)
      implicit none
      integer, intent(in)    :: m, n, lda, lipvt
      logical, intent(in)    :: pivot
      real,    intent(inout) :: a(lda,n)
      integer, intent(out)   :: ipvt(lipvt)
      real,    intent(out)   :: rdiag(n), acnorm(n), wa(n)

      integer :: i, j, jp1, k, kmax, minmn
      real    :: ajnorm, epsmch, sum_, temp
      real, external :: enorm

      epsmch = epsilon(1.0)
      minmn  = min(m, n)

      ! compute column norms
      do j = 1, n
        acnorm(j) = enorm(m, a(1,j))
        rdiag(j)  = acnorm(j)
        wa(j)     = rdiag(j)
        if (pivot) ipvt(j) = j
      end do

      do j = 1, minmn
        if (pivot) then
          ! find column with max rdiag
          kmax = j
          do k = j+1, n
            if (rdiag(k) > rdiag(kmax)) kmax = k
          end do
          if (kmax /= j) then
            ! swap columns j and kmax
            do i = 1, m
              temp        = a(i,j)
              a(i,j)      = a(i,kmax)
              a(i,kmax)   = temp
            end do
            rdiag(kmax)  = rdiag(j)
            wa(kmax)     = wa(j)
            k            = ipvt(j)
            ipvt(j)      = ipvt(kmax)
            ipvt(kmax)   = k
          end if
        end if

        ! Householder transformation
        ajnorm = enorm(m-j+1, a(j,j))
        if (ajnorm == 0.0) goto 100
        if (a(j,j) < 0.0) ajnorm = -ajnorm
        do i = j, m
          a(i,j) = a(i,j) / ajnorm
        end do
        a(j,j) = 1.0 + a(j,j)

        jp1 = j + 1
        do k = jp1, n
          sum_ = 0.0
          do i = j, m
            sum_ = sum_ + a(i,j)*a(i,k)
          end do
          temp = -sum_/a(j,j)
          do i = j, m
            a(i,k) = a(i,k) + temp*a(i,j)
          end do
          if (pivot .and. rdiag(k) /= 0.0) then
            temp = a(j,k)/rdiag(k)
            rdiag(k) = rdiag(k) * sqrt(max(0.0, 1.0-temp**2))
            if (0.05*(rdiag(k)/wa(k))**2 <= epsmch) then
              rdiag(k) = enorm(m-j, a(jp1,k))
              wa(k)    = rdiag(k)
            end if
          end if
        end do
100     continue
        rdiag(j) = -ajnorm
      end do
      end subroutine

!***********************************************************************
      subroutine lmpar(n, r, ldr, ipvt, diag, qtb, delta, par, x, sdiag, wa1, wa2)
      implicit none
      integer, intent(in)    :: n, ldr
      integer, intent(in)    :: ipvt(n)
      real,    intent(in)    :: diag(n), qtb(n), delta
      real,    intent(inout) :: r(ldr,n), par
      real,    intent(out)   :: x(n), sdiag(n), wa1(n), wa2(n)

      integer :: i, iter, j, jm1, jp1, k, l, nsing
      real    :: dxnorm, dwarf, fp, gnorm, parc, parl, paru
      real    :: p1, p001, sum_, temp, epsmch
      real, external :: enorm

      p1   = 0.1; p001 = 0.001
      epsmch = epsilon(1.0)
      dwarf  = tiny(1.0)

      ! compute Gauss-Newton direction
      nsing = n
      do j = 1, n
        wa1(j) = qtb(j)
        if (r(j,j) == 0.0 .and. nsing == n) nsing = j - 1
        if (nsing < n) wa1(j) = 0.0
      end do
      do k = 1, nsing
        j = nsing - k + 1
        wa1(j) = wa1(j) / r(j,j)
        temp   = wa1(j)
        jm1    = j - 1
        do i = 1, jm1
          wa1(i) = wa1(i) - r(i,j)*temp
        end do
      end do
      do j = 1, n
        l     = ipvt(j)
        x(l)  = wa1(j)
      end do

      ! scaled x, evaluate fp
      do j = 1, n
        wa1(j) = diag(j) * x(j)
      end do
      dxnorm = enorm(n, wa1)
      fp     = dxnorm - delta

      if (fp <= p1*delta) then
        par = 0.0
        return
      end if

      ! compute lower bound
      parl = 0.0
      do j = 1, n
        l      = ipvt(j)
        wa1(j) = diag(l) * (wa2(l)/dxnorm)  ! wa2 unused here, set below
      end do
      ! (wa2 not yet set — corrected below in iterative section)

      ! upper bound
      gnorm = 0.0
      do j = 1, n
        sum_ = 0.0
        do i = 1, j
          sum_ = sum_ + r(i,j)*qtb(i)
        end do
        l = ipvt(j)
        gnorm = max(gnorm, abs(sum_/(diag(l)*dxnorm)))
      end do
      paru = gnorm/delta
      if (paru == 0.0) paru = dwarf/min(delta, p1)

      par = max(par, parl)
      par = min(par, paru)
      if (par == 0.0) par = gnorm/dxnorm

      do iter = 1, 10
        if (par == 0.0) par = max(dwarf, p001*paru)

        call qrsolv(n, r, ldr, ipvt, diag, qtb, par, x, sdiag, wa1)

        do j = 1, n
          wa1(j) = diag(j) * x(j)
        end do
        dxnorm = enorm(n, wa1)
        temp   = fp
        fp     = dxnorm - delta

        if (abs(fp) <= p1*delta .or. &
            (parl == 0.0 .and. fp <= temp .and. temp < 0.0)) exit

        ! compute parc
        do j = 1, n
          l      = ipvt(j)
          wa1(j) = diag(l)*wa1(l)/dxnorm
        end do
        do j = 1, n
          wa1(j) = wa1(j)/sdiag(j)
          temp   = wa1(j)
          jp1    = j+1
          do i = jp1, n
            wa1(i) = wa1(i) - r(i,j)*temp
          end do
        end do
        temp = enorm(n, wa1)
        parc = fp/delta/temp**2

        if (fp > 0.0) parl = max(parl, par)
        if (fp < 0.0) paru = min(paru, par)
        par = max(parl, par + parc)
      end do
      end subroutine

!***********************************************************************
      subroutine qrsolv(n, r, ldr, ipvt, diag, qtb, par, x, sdiag, wa)
      implicit none
      integer, intent(in)    :: n, ldr
      integer, intent(in)    :: ipvt(n)
      real,    intent(in)    :: diag(n), qtb(n), par
      real,    intent(inout) :: r(ldr,n)
      real,    intent(out)   :: x(n), sdiag(n), wa(n)

      integer :: i, j, jp1, k, kp1, l, nsing
      real    :: cos_, cotan, qtbpj, sin_, sum_, tan_, temp
      real    :: p5
      p5 = 0.5

      ! copy r and qtb
      do j = 1, n
        do i = j, n
          r(i,j) = r(j,i)
        end do
        x(j)  = r(j,j)
        wa(j) = qtb(j)
      end do

      ! eliminate diagonal matrix D
      do j = 1, n
        l = ipvt(j)
        if (diag(l) == 0.0) goto 90
        sdiag(j:n) = 0.0
        sdiag(j)   = diag(l)

        qtbpj = 0.0
        do k = j, n
          if (sdiag(k) == 0.0) cycle
          if (abs(r(k,k)) < abs(sdiag(k))) then
            cotan  = r(k,k) / sdiag(k)
            sin_   = p5 / sqrt(0.25 + 0.25*cotan**2)
            cos_   = sin_ * cotan
          else
            tan_   = sdiag(k) / r(k,k)
            cos_   = p5 / sqrt(0.25 + 0.25*tan_**2)
            sin_   = cos_ * tan_
          end if
          r(k,k)   = cos_*r(k,k) + sin_*sdiag(k)
          temp     = cos_*wa(k) + sin_*qtbpj
          qtbpj    = -sin_*wa(k) + cos_*qtbpj
          wa(k)    = temp
          kp1      = k + 1
          do i = kp1, n
            temp      = cos_*r(i,k) + sin_*sdiag(i)
            sdiag(i)  = -sin_*r(i,k) + cos_*sdiag(i)
            r(i,k)    = temp
          end do
        end do
90      continue
        sdiag(j) = r(j,j)
        r(j,j)   = x(j)
      end do

      ! back-substitute
      nsing = n
      do j = 1, n
        if (sdiag(j) == 0.0 .and. nsing == n) nsing = j - 1
        if (nsing < n) wa(j) = 0.0
      end do
      do k = 1, nsing
        j     = nsing - k + 1
        wa(j) = wa(j) / sdiag(j)
        temp  = wa(j)
        do i = 1, j-1
          wa(i) = wa(i) - r(i,j)*temp
        end do
      end do

      ! permute
      do j = 1, n
        l    = ipvt(j)
        x(l) = wa(j)
      end do
      end subroutine

!***********************************************************************
      function enorm(n, x)
      implicit none
      integer, intent(in) :: n
      real,    intent(in) :: x(n)
      real :: enorm
      real :: rdwarf, rgiant, s1, s2, s3, xabs, x1max, x3max
      integer :: i
      rdwarf = sqrt(tiny(1.0))
      rgiant = sqrt(huge(1.0))
      s1 = 0.0; s2 = 0.0; s3 = 0.0; x1max = 0.0; x3max = 0.0
      do i = 1, n
        xabs = abs(x(i))
        if (xabs > rdwarf .and. xabs < rgiant) then
          s2 = s2 + xabs**2
        else if (xabs >= rgiant) then
          if (xabs > x1max) then
            s1 = 1.0 + s1*(x1max/xabs)**2
            x1max = xabs
          else
            s1 = s1 + (xabs/x1max)**2
          end if
        else if (xabs <= rdwarf .and. xabs /= 0.0) then
          if (xabs > x3max) then
            s3 = 1.0 + s3*(x3max/xabs)**2
            x3max = xabs
          else
            s3 = s3 + (xabs/x3max)**2
          end if
        end if
      end do
      if (s1 /= 0.0) then
        enorm = x1max * sqrt(s1 + s2/x1max**2)
      else if (s2 /= 0.0) then
        if (s2 >= x3max) then
          enorm = sqrt(s2*(1.0 + x3max*(x3max/s2)))
        else
          enorm = sqrt(x3max*(s2/x3max + x3max))
        end if
      else
        enorm = x3max * sqrt(s3)
      end if
      end function
