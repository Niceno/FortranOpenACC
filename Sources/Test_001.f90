!==============================================================================!
  subroutine Test_001
!------------------------------------------------------------------------------!
!>  Tests matrix-vector product
!------------------------------------------------------------------------------!
  use Linalg_Mod
  use Gpu_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Matrix_Type) :: A
  real, allocatable :: b(:), c(:)
  type(Grid_Type)   :: Grid
  integer           :: time_step
  real              :: ts, te
!==============================================================================!

  print '(a)',     ' #----------------------------------------------------'
  print '(a)',     ' # TEST 1: Performing a sparse-matrix vector product'
  print '(a)',     ' #----------------------------------------------------'

  print '(a)', ' # Creating a grid'
  call Grid % Load_Grid("400_cube.ini")

  print '(a,i12)', ' # The problem size is: ', Grid % n_cells

  print '(a)', ' # Creating a singular sparse matrix and two vectors'
  call A % Create_Matrix(Grid, singular=.true.)
  allocate(b(Grid % n_cells))
  allocate(c(Grid % n_cells))

  b(:) = 2.0

  ! Copy operand matrix and vector to the device ...
  ! ... and reserve memory for result vector on device
  call Gpu % Matrix_Copy_To_Device(A)
  call Gpu % Vector_Copy_To_Device(b)
  call Gpu % Vector_Create_On_Device(c)

  !-----------------------------------------------!
  !   Performing a fake time loop on the device   !
  !-----------------------------------------------!
  print '(a)', ' # Performing a sparse-matrix vector product'
  call cpu_time(ts)
  do time_step = 1, 60
    call Linalg % Mat_X_Vec(c, A, b)
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
