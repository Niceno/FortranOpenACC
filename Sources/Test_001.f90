!==============================================================================!
  subroutine Test_001
!------------------------------------------------------------------------------!
!>  Tests matrix-vector product
!------------------------------------------------------------------------------!
  use Linalg_Mod
  use Process_Mod
  use Gpu_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Matrix_Type)  :: A0, A
  real, allocatable  :: b(:), c(:)
  type(Grid_Type)    :: Grid
  integer            :: n, time_step
  integer, parameter :: N_STEPS = 1200  ! spend enough time on device
  real               :: ts, te
!==============================================================================!

  print '(a)', ' #===================================================='
  print '(a)', ' # TEST 1: Performing a sparse-matrix vector product'
  print '(a)', ' #===================================================='

  print '(a)', ' # Creating a grid'
  call Grid % Load_Grid("test_001_cube.ini")

  n = Grid % n_cells
  print '(a,i12)', ' # The problem size is: ', n

  print '(a)', ' #----------------------------------------------------'
  print '(a)', ' # Be careful with memory usage.  If you exceed the'
  print '(a)', ' # 90% (as a rule of thumb) of the memory your GPU'
  print '(a)', ' # card has the program will become memory bound no'
  print '(a)', ' # matter how you wrote it, and it may even crash.'
  print '(a)', ' #----------------------------------------------------'

  print '(a)', ' # Creating a singular sparse matrix and two vectors'
  call A0 % Create_Matrix(Grid)

  ! Discretize the matrix for diffusion
  call Process % Discretize_Diffusion(Grid, A0)

  ! To see if copy works
  call A % Create_Matrix_From_Matrix(A0)

  allocate(b(n))
  allocate(c(n))

  b(:) = 2.0

  ! Copy operand matrix and vector to the device ...
  ! ... and reserve memory for result vector on device
  call Gpu % Matrix_Copy_To_Device(A)
  call Gpu % Vector_Copy_To_Device(b)
  call Gpu % Vector_Create_On_Device(c)

  !-----------------------------------------------!
  !   Performing a fake time loop on the device   !
  !-----------------------------------------------!
  print '(a,i6,a)', ' # Performing ', N_STEPS, ' sparse-matrix vector products'
  call cpu_time(ts)
  do time_step = 1, N_STEPS
    call Linalg % Mat_X_Vec(n, c, A, b)
  end do
  call cpu_time(te)

  ! Copy results back to host
  call Gpu % Vector_Copy_To_Host(c)

  ! Destroy data on the device, you don't need them anymore
  call Gpu % Matrix_Destroy_On_Device(A)
  call Gpu % Vector_Destroy_On_Device(b)
  call Gpu % Vector_Destroy_On_Device(c)

  ! Print result
  print '(a,es12.3)', ' vector c(1  ):', c(1)
  print '(a,es12.3)', ' vector c(2  ):', c(2)
  print '(a,es12.3)', ' vector c(n-1):', c(Grid % n_cells - 1)
  print '(a,es12.3)', ' vector c(n  ):', c(Grid % n_cells)

  print '(a,f12.3,a)', ' # Time elapsed for TEST 1: ', te-ts, ' [s]'

  end subroutine
