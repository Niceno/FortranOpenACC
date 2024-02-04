!==============================================================================!
  subroutine Test_005
!------------------------------------------------------------------------------!
!>  Tests steps the Conjugate Gradient (CG) algorithm
!------------------------------------------------------------------------------!
!   Note: This algorithm is based on: "Templates for the Solution of Linear    !
!         Systems: Building Blocks for Iterative Methods", available for       !
!         download here: https://netlib.org/linalg/html_templates/report.html  !
!------------------------------------------------------------------------------!
  use Linalg_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Grid_Type)   :: G   !! computational grid
  type(Sparse_Type) :: As  !! system matrix
  type(Vector_Type) :: X   !! solution, dependent variable
  type(Vector_Type) :: B   !! right-hand side vector
  type(Vector_Type) :: R   !! residual vector
  type(Vector_Type) :: P   !! helping vector
  type(Vector_Type) :: Q   !! helping vector
  integer           :: n, nx, ny, nz, iter
  real              :: ts, te
  real              :: alpha, beta, pq, rho, rho_old, res, tol
  integer           :: i, j, k
!==============================================================================!

  nx  = 301
  ny  = 301
  nz  = 301
  n   = nx * ny * nz
  tol = 1.0 / n

  print '(a)',        ' #----------------------------------------------------'
  print '(a)',        ' # TEST 5: Performing Conjugate Gradient steps'
  print '(a, i12)',   ' #         The problem size is set to: ', n
  print '(a,es12.3)', ' #         Target solver tolerace is : ', tol
  print '(a)',        ' #-----------------------------------------------------'

  print '(a)', ' # Creating a grid'
  call G % Create_Grid(1.0, 1.0, 1.0, nx, ny, nz)

  call As % Create_Sparse(G, singular=.false.)

  print '(a)', ' # Creating two vectors for solution and right hand side'
  call X  % Allocate_Vector(n)
  call B  % Allocate_Vector(n)

  ! Allocate vectors related to CG algorithm
  call R  % Allocate_Vector(n)
  call P  % Allocate_Vector(n)
  call Q  % Allocate_Vector(n)

  ! Initialize right-hand side, the source
  X % val(:) = 0.0
  B % val(:) = G % dx * G % dy * G % dz

  ! Copy components of the linear system to the device
  call As % Copy_Sparse_To_Device()
  call X  % Copy_Vector_To_Device()
  call B  % Copy_Vector_To_Device()

  ! Allocate vectors related to CG algorithm on the device
  call R  % Create_Vector_On_Device()
  call P  % Create_Vector_On_Device()
  call Q  % Create_Vector_On_Device()

  !-----------------------------------------------!
  !   Performing a fake time loop on the device   !
  !-----------------------------------------------!
  print '(a)', ' # Performing a demo of the preconditioned CG method'
  call cpu_time(ts)

  !----------------!
  !   r = b - Ax   !     =-->  (Q used for Ax)
  !----------------!
  call Linalg % Spa_X_Vec(Q, As, X)             ! Ax = A * X
  call Linalg % Vec_P_Sca_X_Vec(R, B, -1.0, Q)  ! R  = b - AX

  !-----------!
  !   p = r   !
  !-----------!
  call Linalg % Vec_Copy(P, R)

  do iter = 1, n

    !---------------!
    !   z = r / M   !    =--> (A used for M, Q for Z)
    !---------------!
    call Linalg % Vec_O_Dia(Q, As, R)  ! Q = R / As

    !-----------------!
    !   rho = r * z   !  =--> (Q used for Z)
    !-----------------!
    call Linalg % Vec_D_Vec(rho, R, Q)  ! rho = R * Q

    if(iter .eq. 1) then

      !-----------!
      !   p = z   !  =--> (Q used for Z)
      !-----------!
      call Linalg % Vec_Copy(P, Q)  ! P = Q
    else

      !--------------------------!
      !   beta = rho / rho_old   !
      !   p = z + beta * p       !  =--> (Q used for P)
      !--------------------------!
      beta = rho / rho_old
      call Linalg % Vec_P_Sca_X_Vec(P, Q, beta, P)   ! P = Q + beta P
    end if

    !------------!
    !   q = Ap   !
    !------------!
    call Linalg % Spa_X_Vec(Q, As, P)   ! Q  = A * P

    !---------------------------!
    !   alfa =  rho / (p * q)   !
    !---------------------------!
    call Linalg % Vec_D_Vec(pq, P, Q)  ! pq = P * Q
    alpha = rho / pq

    !---------------------!
    !   x = x + alfa p    !
    !   r = r - alfa Q    !
    !---------------------!
    call Linalg % Vec_P_Sca_X_Vec(X, X, +alpha, P)  ! X = X + alpha P
    call Linalg % Vec_P_Sca_X_Vec(R, R, -alpha, Q)  ! R = R - alpha Q

    !--------------------!
    !   Check residual   !
    !--------------------!
    call Linalg % Vec_D_Vec(res, R, R)  ! res = R * R
    if(mod(iter,8) .eq. 0) print '(a,i12,es12.3)', ' iter, res = ', iter, res
    if(res .lt. tol) goto 1

    rho_old = rho
  end do
1 continue
  call cpu_time(te)

  ! Copy results back to host
  call X % Copy_Vector_To_Host()

  ! Destroy data on the device, you don't need them anymore
  call As % Destroy_Sparse_On_Device()
  call X  % Destroy_Vector_On_Device()
  call B  % Destroy_Vector_On_Device()

  call R  % Destroy_Vector_On_Device()
  call P  % Destroy_Vector_On_Device()
  call Q  % Destroy_Vector_On_Device()

  ! Print result
  print '(a,es12.3)', ' Vector X(1  ):', X % val(1  )
  print '(a,es12.3)', ' Vector X(2  ):', X % val(2  )
  print '(a,es12.3)', ' Vector X(n-1):', X % val(n-1)
  print '(a,es12.3)', ' Vector X(n  ):', X % val(n  )

  ! Save results
  call G % Save_Vtk_Debug(X % val)

  print '(a,f12.3,a)', ' # Time elapsed for TEST 5: ', te-ts, ' [s]'

  end subroutine
