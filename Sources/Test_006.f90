!==============================================================================!
  subroutine Test_006
!------------------------------------------------------------------------------!
!>  Tests calling of the CG algorithm from the Native_Mod
!------------------------------------------------------------------------------!
  use Native_Mod
  use Gpu_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Grid_Type)   :: Grid  !! computational grid
  type(Matrix_Type) :: A     !! system matrix
  type(Native_Type) :: Nat   !! linear solver suite
  real, allocatable :: x(:)  !! solution, dependent variable
  real, allocatable :: b(:)  !! right-hand side vector
  integer           :: nx, ny, nz
  real              :: ts, te
!==============================================================================!

  nx  = 301
  ny  = 301
  nz  = 301

  print '(a)',        ' #----------------------------------------------------'
  print '(a)',        ' # TEST 6: Call Conjugate Gradient from Native_Mod'
  print '(a, i12)',   ' #         The problem size is set to: ', nx * ny * nz
  print '(a,es12.3)', ' #         Target solver tolerace is : ', 1.0/(nx*ny*nz)
  print '(a)',        ' #-----------------------------------------------------'

  print '(a)', ' # Creating a grid'
  call Grid % Create_Grid(1.0, 1.0, 1.0, nx, ny, nz)

  call A % Create_Matrix(Grid, singular=.false.)

  print '(a)', ' # Creating two vectors for solution and right hand side'
  allocate(x(-Grid % n_bnd_cells:Grid % n_cells))
  allocate(b(Grid % n_cells))

  print '(a)', ' # Creating a native solver'
  call Nat % Create_Native(Grid)

  ! Initialize right-hand side, the source
  x(:) = 0.0
  b(:) = Grid % dx * Grid % dy * Grid % dz

  ! Copy components of the linear system to the device
  call Gpu % Matrix_Copy_To_Device(A)
  call Gpu % Vector_Copy_To_Device(x)
  call Gpu % Vector_Copy_To_Device(b)

  ! Allocate vectors related to CG algorithm on the device
  call Gpu % Native_Create_On_Device(Nat)

  !-----------------------------------------------!
  !   Performing a fake time loop on the device   !
  !-----------------------------------------------!
  print '(a)', ' # Performing a demo of the preconditioned CG method'
  call cpu_time(ts)
  call Nat % Cg(A, x, b, Grid % n_cells)
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
  call Grid % Save_Vtk_Debug("solution.vtk", x)

  print '(a,f12.3,a)', ' # Time elapsed for TEST 6: ', te-ts, ' [s]'

  end subroutine
