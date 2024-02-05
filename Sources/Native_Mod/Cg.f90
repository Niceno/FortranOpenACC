!==============================================================================!
  subroutine Cg(Nat, A, x, b, miter)
!------------------------------------------------------------------------------!
!   Note: This algorithm is based on: "Templates for the Solution of Linear    !
!         Systems: Building Blocks for Iterative Methods", available for       !
!         download here: https://netlib.org/linalg/html_templates/report.html  !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Native_Type), target, intent(in)    :: Nat      !! parent class
  type(Matrix_Type),  target, intent(in)    :: A        !! system matrix
  real,                       intent(out)   :: x(-Nat % pnt_grid % n_bnd_cells:&
                                                  Nat % pnt_grid % n_cells)
    !! unknown vector, the solution of the linear system
  real,                       intent(inout) :: b( Nat % pnt_grid % n_cells)
    !! right-hand side vector
  integer,                    intent(in)    :: miter    !! maximum iterations
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type),   pointer :: Grid
  type(Matrix_Type), pointer :: M
  real, contiguous,  pointer :: r(:), p(:), q(:)
  real                       :: alpha, beta, pq, rho, rho_old, res, tol
  integer                    :: nc, iter
!==============================================================================!

  ! Take aliases
  r    => Nat % r
  p    => Nat % p
  q    => Nat % q
  M    => Nat % D
  Grid => Nat % pnt_grid
  nc   =  Grid % n_cells

  ! Set tolerance
  tol = 1.0 / Grid % n_cells

  !----------------!
  !   r = b - Ax   !     =-->  (q used for temporary storing Ax)
  !----------------!
  call Linalg % Mat_X_Vec(q, A, x(1:nc))        ! Ax = A * x
  call Linalg % Vec_P_Sca_X_Vec(r, b, -1.0, q)  ! r  = b - Ax

  !-----------!
  !   p = r   !
  !-----------!
  call Linalg % Vec_Copy(p, r)

  do iter = 1, miter

    !---------------!
    !   z = r / M   !    =--> (q used for z)
    !---------------!
    call Linalg % Mat_X_Vec(q, M, r)  ! q = r / M

    !-----------------!
    !   rho = r * z   !  =--> (q used for z)
    !-----------------!
    call Linalg % Vec_D_Vec(rho, r, q)  ! rho = r * q

    if(iter .eq. 1) then

      !-----------!
      !   p = z   !  =--> (q used for z)
      !-----------!
      call Linalg % Vec_Copy(p, q)  ! p = q
    else

      !--------------------------!
      !   beta = rho / rho_old   !
      !   p = z + beta * p       !  =--> (q used for p)
      !--------------------------!
      beta = rho / rho_old
      call Linalg % Vec_P_Sca_X_Vec(p, q, beta, p)   ! p = q + beta p
    end if

    !------------!
    !   q = Ap   !
    !------------!
    call Linalg % Mat_X_Vec(q, A, p)   ! q  = A * p

    !---------------------------!
    !   alfa =  rho / (p * q)   !
    !---------------------------!
    call Linalg % Vec_D_Vec(pq, p, q)  ! pq = p * q
    alpha = rho / pq

    !---------------------!
    !   x = x + alfa p    !
    !   r = r - alfa q    !
    !---------------------!
    call Linalg % Vec_P_Sca_X_Vec(x(1:nc), x(1:nc), +alpha, p)  ! x = x + alpha p
    call Linalg % Vec_P_Sca_X_Vec(r,       r,       -alpha, q)  ! r = r - alpha q

    !--------------------!
    !   Check residual   !
    !--------------------!
    call Linalg % Vec_D_Vec(res, r, r)  ! res = r * r

    if(mod(iter,8) .eq. 0) print '(a,i12,es12.3)', ' iter, res = ', iter, res
    if(res .lt. tol) goto 1

    rho_old = rho
  end do

1 continue

  end subroutine
