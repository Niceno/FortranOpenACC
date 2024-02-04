!==============================================================================!
  subroutine Test_004
!------------------------------------------------------------------------------!
!>  Tests diagonal preconditioner
!------------------------------------------------------------------------------!
  use Linalg_Mod
  use Gpu_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Matrix_Type) :: A
  real, allocatable :: b(:), c(:)
  type(Grid_Type)   :: Grid
  integer           :: nx, ny, nz, time_step
  real              :: ts, te
!==============================================================================!

  nx = 300
  ny = 300
  nz = 300
  print '(a)',     ' #-----------------------------------------------------'
  print '(a)',     ' # TEST 4: Performing diagonal preconditioning'
  print '(a,i12)', ' #         The problem size is set to ', nx * ny * nz
  print '(a)',     ' #-----------------------------------------------------'

  print '(a)', ' # Creating a grid'
  call Grid % Create_Grid(1.0, 1.0, 1.0, nx, ny, nz)

  call A % Create_Matrix(Grid, singular=.false.)
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
  print '(a)', ' # Performing diagonal preconditioning'
  call cpu_time(ts)
  do time_step = 1, 60
    call Linalg % Vec_O_Dia(c, A, b)
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
  print '(a,es12.3)', ' vector c(n-1):', c(Grid % n_cells-1)
  print '(a,es12.3)', ' vector c(n  ):', c(Grid % n_cells)

  print '(a,f12.3,a)', ' # Time elapsed for TEST 4: ', te-ts, ' [s]'

  end subroutine
