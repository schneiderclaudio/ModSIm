! NumericalReplace.f90
! Pure-Fortran / MKL replacements for all IMSL routines used in ModsimCurveFit.f90
! Module BCLSF_State must come first (before subroutines that USE it).

!=======================================================================
! Module to hold state for the bounded-LM wrapper (BCLSF / BCLSF_FCN_WRAP)
!=======================================================================
      Module BCLSF_State
        Integer, Save :: BCLSF_N = 0
        Real,    Save, Allocatable :: BCLSF_LB(:), BCLSF_UB(:)
      End Module

!=======================================================================
! STUBS
!=======================================================================

      Subroutine UMACH(iopt, iunit)
      ! No-op: IMSL I/O unit redirection not needed
      Integer iopt, iunit
      Return
      End Subroutine

      Subroutine ERSET(iersvr, ipact, isact)
      ! No-op: IMSL error handling not needed
      Integer iersvr, ipact, isact
      Return
      End Subroutine

!=======================================================================
! U4LSF — set default IPARAM/RPARAM for LM optimiser
!=======================================================================
      Subroutine U4LSF(IPARAM, RPARAM)
      Integer IPARAM(6)
      Real    RPARAM(7)
      IPARAM = 0
      RPARAM = 0.0
      RPARAM(1) = 1.0e-6  ! gradient tolerance
      RPARAM(2) = 1.0e-6  ! step tolerance
      RPARAM(3) = 1.0e-6  ! function tolerance
      Return
      End Subroutine

!=======================================================================
! WRRRL — print a real matrix to a Fortran unit
!=======================================================================
      Subroutine WRRRL(iunit, title, nrow, ncol, A, lda)
      Integer iunit, nrow, ncol, lda
      Character*(*) title(*)
      Real A(lda, *)
      Integer i, j
      Write(iunit,'(A)') TRIM(title(1))
      Do i = 1, nrow
        Write(iunit,'(*(G12.4))') (A(i,j), j=1,ncol)
      End Do
      Return
      End Subroutine

!=======================================================================
! WRCRN — print a complex matrix to a Fortran unit
!=======================================================================
      Subroutine WRCRN(iunit, title, nrow, ncol, A, lda)
      Integer iunit, nrow, ncol, lda
      Character*(*) title(*)
      Complex A(lda, *)
      Integer i, j
      Write(iunit,'(A)') TRIM(title(1))
      Do i = 1, nrow
        Write(iunit,'(*(''('',G12.4,'','',G12.4,'')''))') (A(i,j), j=1,ncol)
      End Do
      Return
      End Subroutine

!=======================================================================
! LINRG — matrix inverse via Gauss-Jordan elimination (single precision)
!   Signature: LINRG(n, A, lda, Ainv, ldainv)
!   A is NOT overwritten (copied to work array first).
!=======================================================================
      Subroutine LINRG(n, A, lda, Ainv, ldainv)
      Implicit None
      Integer, Intent(In)  :: n, lda, ldainv
      Real,    Intent(In)  :: A(lda, n)
      Real,    Intent(Out) :: Ainv(ldainv, n)
      Real :: W(n, 2*n)
      Real :: pivot, ratio
      Integer :: i, j, k

      Do i = 1, n
        Do j = 1, n
          W(i, j) = A(i, j)
        End Do
        Do j = 1, n
          W(i, n+j) = 0.0
        End Do
        W(i, n+i) = 1.0
      End Do

      Do k = 1, n
        pivot = ABS(W(k,k))
        Do i = k+1, n
          If (ABS(W(i,k)) .GT. pivot) Then
            pivot = ABS(W(i,k))
            Do j = 1, 2*n
              ratio = W(k,j); W(k,j) = W(i,j); W(i,j) = ratio
            End Do
          End If
        End Do
        If (ABS(W(k,k)) .LT. 1.0e-30) Then
          Do i = 1, n; Do j = 1, n; Ainv(i,j) = 0.0; End Do; Ainv(i,i) = 1.0; End Do
          Return
        End If
        pivot = W(k,k)
        Do j = 1, 2*n
          W(k,j) = W(k,j) / pivot
        End Do
        Do i = 1, n
          If (i .NE. k) Then
            ratio = W(i,k)
            Do j = 1, 2*n
              W(i,j) = W(i,j) - ratio * W(k,j)
            End Do
          End If
        End Do
      End Do

      Do i = 1, n
        Do j = 1, n
          Ainv(i,j) = W(i, n+j)
        End Do
      End Do
      Return
      End Subroutine

!=======================================================================
! EVCRG — eigenvalues and right eigenvectors via MKL SGEEV
!   Signature: EVCRG(n, A, lda, EigVals, EigVects, ldv)
!   EigVals(n): Complex, EigVects(ldv,n): Complex (column eigenvectors)
!=======================================================================
      Subroutine EVCRG(n, A, lda, EigVals, EigVects, ldv)
      Implicit None
      Integer,  Intent(In)  :: n, lda, ldv
      Real,     Intent(In)  :: A(lda, n)
      Complex,  Intent(Out) :: EigVals(n)
      Complex,  Intent(Out) :: EigVects(ldv, n)

      Real :: Acopy(n,n), WR(n), WI(n), VL(1,1), VR(n,n)
      Integer :: lwork, info, i, j
      Real, Allocatable :: work(:)

      Do i = 1, n
        Do j = 1, n
          Acopy(i,j) = A(i,j)
        End Do
      End Do

      lwork = -1
      Allocate(work(1))
      Call SGEEV('N','V', n, Acopy, n, WR, WI, VL, 1, VR, n, work, lwork, info)
      lwork = INT(work(1))
      Deallocate(work)
      Allocate(work(lwork))
      Call SGEEV('N','V', n, Acopy, n, WR, WI, VL, 1, VR, n, work, lwork, info)
      Deallocate(work)

      i = 1
      Do While (i .LE. n)
        EigVals(i) = CMPLX(WR(i), WI(i))
        If (WI(i) .NE. 0.0 .AND. i .LT. n) Then
          EigVals(i+1) = CMPLX(WR(i+1), WI(i+1))
          Do j = 1, n
            EigVects(j, i)   = CMPLX( VR(j,i),  VR(j,i+1))
            EigVects(j, i+1) = CMPLX( VR(j,i), -VR(j,i+1))
          End Do
          i = i + 2
        Else
          Do j = 1, n
            EigVects(j, i) = CMPLX(VR(j,i), 0.0)
          End Do
          i = i + 1
        End If
      End Do
      Return
      End Subroutine

!=======================================================================
! FIN_REPL — upper-tail critical value of F(df1,df2), i.e. (1-p) quantile
!=======================================================================
      Function FIN_REPL(p, df1, df2)
      Implicit None
      Real, Intent(In) :: p, df1, df2
      Real :: FIN_REPL
      Real*8 :: lo, hi, mid, cdf, dp, dd1, dd2
      Real*8, External :: IBETA_DP_FIN
      Integer :: iter

      dp  = DBLE(p)
      dd1 = DBLE(df1)
      dd2 = DBLE(df2)

      lo = 0.0D0
      hi = 1.0D6
      Do iter = 1, 100
        mid = 0.5D0*(lo + hi)
        cdf = IBETA_DP_FIN(dd2/(dd2 + dd1*mid), dd2*0.5D0, dd1*0.5D0)
        If (cdf .GT. dp) Then
          lo = mid
        Else
          hi = mid
        End If
        If ((hi - lo) .LT. 1.0D-6 * MAX(mid, 1.0D0)) Exit
      End Do
      FIN_REPL = REAL(0.5D0*(lo + hi))
      Return
      End Function

      Function IBETA_DP_FIN(x, a, b)
      Implicit None
      Real*8, Intent(In)  :: x, a, b
      Real*8 :: IBETA_DP_FIN
      Real*8 :: lbeta, bt, cf
      Real*8, External :: BETACF_FIN

      If (x .LE. 0.0D0) Then; IBETA_DP_FIN = 0.0D0; Return; End If
      If (x .GE. 1.0D0) Then; IBETA_DP_FIN = 1.0D0; Return; End If
      lbeta = LOG_GAMMA(a+b) - LOG_GAMMA(a) - LOG_GAMMA(b)
      bt    = EXP(lbeta + a*LOG(x) + b*LOG(1.0D0-x))
      If (x .LT. (a+1.0D0)/(a+b+2.0D0)) Then
        cf = BETACF_FIN(x,a,b)
        IBETA_DP_FIN = bt*cf/a
      Else
        cf = BETACF_FIN(1.0D0-x,b,a)
        IBETA_DP_FIN = 1.0D0 - bt*cf/b
      End If
      Return
      End Function

      Function BETACF_FIN(x, a, b)
      Implicit None
      Real*8, Intent(In) :: x, a, b
      Real*8 :: BETACF_FIN
      Integer :: m
      Real*8  :: aa, c, d, del, h, qab, qam, qap
      Real*8, Parameter :: FPMIN = 1.0D-30, EPS = 3.0D-7
      Integer, Parameter :: MAXIT = 200
      qab=a+b; qap=a+1.0D0; qam=a-1.0D0
      c=1.0D0; d=1.0D0-qab*x/qap
      If (ABS(d).LT.FPMIN) d=FPMIN
      d=1.0D0/d; h=d
      Do m=1,MAXIT
        aa=DBLE(m)*(b-DBLE(m))*x/((qam+DBLE(2*m))*(a+DBLE(2*m)))
        d=1.0D0+aa*d; If(ABS(d).LT.FPMIN) d=FPMIN
        c=1.0D0+aa/c; If(ABS(c).LT.FPMIN) c=FPMIN
        d=1.0D0/d; h=h*d*c
        aa=-(a+DBLE(m))*(qab+DBLE(m))*x/((a+DBLE(2*m))*(qap+DBLE(2*m)))
        d=1.0D0+aa*d; If(ABS(d).LT.FPMIN) d=FPMIN
        c=1.0D0+aa/c; If(ABS(c).LT.FPMIN) c=FPMIN
        d=1.0D0/d; del=d*c; h=h*del
        If (ABS(del-1.0D0).LT.EPS) Exit
      End Do
      BETACF_FIN=h
      Return
      End Function

!=======================================================================
! UNLSF — unconstrained Levenberg-Marquardt (MINPACK lmdif1 wrapper)
!=======================================================================
      Subroutine UNLSF(FCN, m, n, xguess, xscale, fscale, &
                       iparam, rparam, xout, res, fjac, ldfjac)
      Implicit None
      Integer,  Intent(In)    :: m, n, ldfjac
      Real,     Intent(In)    :: xguess(n), xscale(n), fscale(m)
      Integer,  Intent(InOut) :: iparam(6)
      Real,     Intent(InOut) :: rparam(7)
      Real,     Intent(Out)   :: xout(n), res(m), fjac(ldfjac, n)
      External FCN

      Real    :: x(n), fvec(m)
      Real    :: tol
      Integer :: info, lwa
      Real, Allocatable :: wa(:)

      x   = xguess
      tol = 1.0e-6
      If (rparam(3) .GT. 0.0) tol = rparam(3)

      lwa = m*n + 5*n + m
      Allocate(wa(lwa))
      Call lmdif1(FCN, m, n, x, fvec, tol, info, wa, lwa)
      xout = x
      res  = fvec
      iparam(3) = 0; iparam(4) = 0; iparam(5) = 0
      Deallocate(wa)
      Return
      End Subroutine

!=======================================================================
! BCLSF_SetBounds — store bounds in module for use by BCLSF_FCN_WRAP
!=======================================================================
      Subroutine BCLSF_SetBounds(n, lb, ub)
      Use BCLSF_State
      Implicit None
      Integer, Intent(In) :: n
      Real,    Intent(In) :: lb(n), ub(n)
      BCLSF_N = n
      If (Allocated(BCLSF_LB)) Deallocate(BCLSF_LB)
      If (Allocated(BCLSF_UB)) Deallocate(BCLSF_UB)
      Allocate(BCLSF_LB(n), BCLSF_UB(n))
      BCLSF_LB = lb
      BCLSF_UB = ub
      End Subroutine

!=======================================================================
! BCLSF_FCN_WRAP — back-transforms t->x then calls TERMS
!   NOTE: This calls TERMS directly (the only FCN ever passed to BCLSF
!   in this codebase).  A general callback is not needed.
!=======================================================================
      Subroutine BCLSF_FCN_WRAP(m, n, t, fvec, iflag)
      Use BCLSF_State
      Implicit None
      Integer, Intent(In)    :: m, n, iflag
      Real,    Intent(In)    :: t(n)
      Real,    Intent(Out)   :: fvec(m)
      External TERMS
      Real :: x(n)
      Integer :: i
      Do i = 1, n
        x(i) = BCLSF_LB(i) + (BCLSF_UB(i) - BCLSF_LB(i)) * SIN(t(i))**2
      End Do
      Call TERMS(m, n, x, fvec, iflag)
      Return
      End Subroutine

!=======================================================================
! BCLSF — bounded Levenberg-Marquardt via sin^2 variable transformation
!=======================================================================
      Subroutine BCLSF(FCN, m, n, xguess, ibtype, xlb, xub, &
                       xscale, fscale, iparam, rparam, &
                       xout, res, fjac, ldfjac)
      Use BCLSF_State
      Implicit None
      Integer,  Intent(In)    :: m, n, ldfjac, ibtype
      Real,     Intent(In)    :: xguess(n), xlb(n), xub(n)
      Real,     Intent(In)    :: xscale(n), fscale(m)
      Integer,  Intent(InOut) :: iparam(6)
      Real,     Intent(InOut) :: rparam(7)
      Real,     Intent(Out)   :: xout(n), res(m), fjac(ldfjac, n)
      External FCN
      External BCLSF_FCN_WRAP
      Real    :: t(n), fvec(m)
      Real    :: tol, range_i
      Integer :: info, lwa, i
      Real, Allocatable :: wa(:)

      Call BCLSF_SetBounds(n, xlb, xub)

      Do i = 1, n
        range_i = xlb(i) - xub(i)
        If (ABS(range_i) .LT. 1.0e-30) range_i = 1.0e-30
        t(i) = ASIN(SQRT(MAX(0.0, MIN(1.0, (xguess(i)-xlb(i))/ABS(range_i)))))
      End Do

      tol = 1.0e-6
      If (rparam(3) .GT. 0.0) tol = rparam(3)

      lwa = m*n + 5*n + m
      Allocate(wa(lwa))
      Call lmdif1(BCLSF_FCN_WRAP, m, n, t, fvec, tol, info, wa, lwa)

      Do i = 1, n
        range_i = xub(i) - xlb(i)
        xout(i) = xlb(i) + range_i * SIN(t(i))**2
      End Do
      res = fvec
      iparam(3) = 0; iparam(4) = 0; iparam(5) = 0
      Deallocate(wa)
      Return
      End Subroutine
