!==============================================================================!
  subroutine Test_004
!------------------------------------------------------------------------------!
!>  Tests calling of the CG algorithm from the Native_Mod
!------------------------------------------------------------------------------!
  use Native_Mod
  use Gpu_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Grid_Type)   :: Grid  !! computational grid
  type(Matrix_Type) :: A0    !! system matrix
  type(Matrix_Type) :: A     !! system matrix
  type(Native_Type) :: Nat   !! linear solver suite
  real, allocatable :: x(:)  !! solution, dependent variable
  real, allocatable :: b(:)  !! right-hand side vector
  real              :: ts, te, tol = 1.0e-12
  integer           :: n
!==============================================================================!

  print '(a)', ' #====================================================='
  print '(a)', ' # TEST 4: Call Conjugate Gradient from Native_Mod'
  print '(a)', ' #====================================================='

  print '(a)', ' # Creating a grid'
  call Grid % Load_Grid("test_004_cube.ini")

  n = Grid % n_cells
  print '(a, i12)',   ' # The problem size is: ', n
  print '(a,es12.3)', ' # Solver tolerace is : ', tol

  print '(a)', ' #----------------------------------------------------'
  print '(a)', ' # Be careful with memory usage.  If you exceed the'
  print '(a)', ' # 90% (as a rule of thumb) of the memory your GPU'
  print '(a)', ' # card has the program will become memory bound no'
  print '(a)', ' # matter how you wrote it, and it may even crash.'
  print '(a)', ' #----------------------------------------------------'

  call A0 % Create_Matrix(Grid, b)
  call A  % Create_Matrix_From_Matrix(A0)          ! to see if copy works

  print '(a)', ' # Creating a native solver, system matrix and right hand side'
  call Nat % Create_Native(A)

  print '(a)', ' # Creating a solution vector'
  allocate(x(-Grid % n_bnd_cells:Grid % n_cells))

  ! Initialize right-hand side, the source
  x(:) = 0.0

  ! Copy components of the linear system to the device
  call Gpu % Matrix_Copy_To_Device(A)
  call Gpu % Vector_Copy_To_Device(x)
  call Gpu % Vector_Copy_To_Device(b)

  ! Allocate vectors related to CG algorithm on the device
  call Gpu % Native_Transfer_To_Device(Nat)

  !-----------------------------------------------!
  !   Performing a fake time loop on the device   !
  !-----------------------------------------------!
  print '(a)', ' # Performing a demo of the preconditioned CG method'
  call cpu_time(ts)
  call Nat % Cg(A, x, b, Grid % n_cells, tol)
  call cpu_time(te)

  ! Copy results back to host
  call Gpu % Vector_Copy_To_Host(x)

  ! Destroy data on the device, you don't need them anymore
  call Gpu % Matrix_Destroy_On_Device(A)
  call Gpu % Vector_Destroy_On_Device(x)
  call Gpu % Vector_Destroy_On_Device(b)

  call Gpu % Native_Destroy_On_Device(Nat)

  ! Print result
  print '(a,es12.3)', ' vector x(1  ):', x(1)
  print '(a,es12.3)', ' vector x(2  ):', x(2)
  print '(a,es12.3)', ' vector x(n-1):', x(Grid % n_cells-1)
  print '(a,es12.3)', ' vector x(n  ):', x(Grid % n_cells)

  ! Save results
  call Grid % Save_Vtk_Scalar("solution.vtk", x(1:Grid % n_cells))

  print '(a,f12.3,a)', ' # Time elapsed for TEST 4: ', te-ts, ' [s]'

  end subroutine
