!==============================================================================!
  subroutine Test_005
!------------------------------------------------------------------------------!
!>  Tests steps the Conjugate Gradient (CG) algorithm.  This subroutine will
!>  probably be superseeded by Test_006.  If that happens, it will probably
!>  be replaced by Test_006 and all the other tests will shift up one place.
!------------------------------------------------------------------------------!
!   Note: This algorithm is based on: "Templates for the Solution of Linear    !
!         Systems: Building Blocks for Iterative Methods", available for       !
!         download here: https://netlib.org/linalg/html_templates/report.html  !
!------------------------------------------------------------------------------!
  use Linalg_Mod
  use Gpu_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Grid_Type)   :: Grid  !! computational grid
  type(Matrix_Type) :: A     !! system matrix
  real, allocatable :: x(:)  !! solution, dependent variable
  real, allocatable :: b(:)  !! right-hand side vector
  real, allocatable :: r(:)  !! residual vector
  real, allocatable :: p(:)  !! helping vector
  real, allocatable :: q(:)  !! helping vector
  integer           :: nx, ny, nz, iter
  real              :: ts, te
  real              :: alpha, beta, pq, rho, rho_old, res, tol
!==============================================================================!

  nx  = 301
  ny  = 301
  nz  = 301
  tol = 1.0 / (nx * ny * nz)

  print '(a)',        ' #----------------------------------------------------'
  print '(a)',        ' # TEST 5: Performing Conjugate Gradient steps'
  print '(a, i12)',   ' #         The problem size is set to: ', nx * ny * nz
  print '(a,es12.3)', ' #         Target solver tolerace is : ', tol
  print '(a)',        ' #-----------------------------------------------------'

  print '(a)', ' # Creating a grid'
  call Grid % Create_Grid(1.0, 1.0, 1.0, nx, ny, nz)

  call A % Create_Matrix(Grid, singular=.false.)

  print '(a)', ' # Creating two vectors for solution and right hand side'
  allocate(x(Grid % n_cells))
  allocate(b(Grid % n_cells))

  ! Allocate vectors related to CG algorithm
  allocate(r(Grid % n_cells))
  allocate(p(Grid % n_cells))
  allocate(q(Grid % n_cells))

  ! Initialize right-hand side, the source
  x(:) = 0.0
  b(:) = Grid % dx * Grid % dy * Grid % dz

  ! Copy components of the linear system to the device
  call Gpu % Matrix_Copy_To_Device(A)
  call Gpu % Vector_Copy_To_Device(x)
  call Gpu % Vector_Copy_To_Device(b)

  ! Allocate vectors related to CG algorithm on the device
  call Gpu % Vector_Create_On_Device(r)
  call Gpu % Vector_Create_On_Device(p)
  call Gpu % Vector_Create_On_Device(q)

  !-----------------------------------------------!
  !   Performing a fake time loop on the device   !
  !-----------------------------------------------!
  print '(a)', ' # Performing a demo of the preconditioned CG method'
  call cpu_time(ts)

  !----------------!
  !   r = b - Ax   !     =-->  (q used for temporary storing Ax)
  !----------------!
  call Linalg % Mat_X_Vec(q, A, x)              ! Ax = A * x
  call Linalg % Vec_P_Sca_X_Vec(r, b, -1.0, q)  ! r  = b - Ax

  !-----------!
  !   p = r   !
  !-----------!
  call Linalg % Vec_Copy(p, r)

  do iter = 1, Grid % n_cells

    !---------------!
    !   z = r / M   !    =--> (A used for M, q for z)
    !---------------!
    call Linalg % Vec_O_Dia(q, A, r)  ! q = r / A

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
    call Linalg % Vec_P_Sca_X_Vec(x, x, +alpha, p)  ! x = x + alpha p
    call Linalg % Vec_P_Sca_X_Vec(r, r, -alpha, q)  ! r = r - alpha q

    !--------------------!
    !   Check residual   !
    !--------------------!
    call Linalg % Vec_D_Vec(res, r, r)  ! res = r * r
    if(mod(iter,8) .eq. 0) print '(a,i12,es12.3)', ' iter, res = ', iter, res
    if(res .lt. tol) goto 1

    rho_old = rho
  end do
1 continue
  call cpu_time(te)

  ! Copy results back to host
  call Gpu % Vector_Copy_To_Host(x)

  ! Destroy data on the device, you don't need them anymore
  call Gpu % Matrix_Destroy_On_Device(A)
  call Gpu % Vector_Destroy_On_Device(x)
  call Gpu % Vector_Destroy_On_Device(b)

  call Gpu % Vector_Destroy_On_Device(r)
  call Gpu % Vector_Destroy_On_Device(p)
  call Gpu % Vector_Destroy_On_Device(q)

  ! Print result
  print '(a,es12.3)', ' vector x(1  ):', x(1)
  print '(a,es12.3)', ' vector x(2  ):', x(2)
  print '(a,es12.3)', ' vector x(n-1):', x(Grid % n_cells-1)
  print '(a,es12.3)', ' vector x(n  ):', x(Grid % n_cells)

  ! Save results
  call Grid % Save_Vtk_Debug("solution.vtk", x)

  print '(a,f12.3,a)', ' # Time elapsed for TEST 5: ', te-ts, ' [s]'

  end subroutine
