!==============================================================================!
  subroutine Test_001
!------------------------------------------------------------------------------!
!>  Tests Sparse-matrix with vector product
!------------------------------------------------------------------------------!
  use Linalg_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Vector_Type)  :: B, C
  type(Sparse_Type)  :: As
  type(Grid_Type)    :: G
  integer            :: n, nx, ny, nz, time_step
  real               :: ts, te
!==============================================================================!

  nx = 400
  ny = 400
  nz = 400
  n  = nx * ny * nz
  print '(a)',     ' #----------------------------------------------------'
  print '(a)',     ' # TEST 1: Performing a sparse-matrix vector product'
  print '(a,i12)', ' #         The problem size is set to ', n
  print '(a)',     ' #----------------------------------------------------'

  print '(a)', ' # Creating a grid'
  call G % Create_Grid(1.0, 1.0, 1.0, nx, ny, nz)

  print '(a)', ' # Creating a singular sparse matrix and two vectors'
  call As % Create_Sparse(G, singular=.true.)
  call B  % Allocate_Vector(n)
  call C  % Allocate_Vector(n)

  B % val(:) = 2.0

  ! Copy operand matrix and vector to the device ...
  ! ... and reserve memory for result vector on device
  call As % Copy_Sparse_To_Device()
  call B  % Copy_Vector_To_Device()
  call C  % Create_Vector_On_Device()

  !-----------------------------------------------!
  !   Performing a fake time loop on the device   !
  !-----------------------------------------------!
  print '(a)', ' # Performing a sparse-matrix vector product'
  call cpu_time(ts)
  do time_step = 1, 60
    call Linalg % Spa_X_Vec(C, As, B)
  end do
  call cpu_time(te)

  ! Copy results back to host
  call C % Copy_Vector_To_Host()

  ! Destroy data on the device, you don't need them anymore
  call As % Destroy_Sparse_On_Device()
  call B  % Destroy_Vector_On_Device()
  call C  % Destroy_Vector_On_Device()

  ! Print result
  print '(a,es12.3)', ' Vector C(1  ):', C % val(1  )
  print '(a,es12.3)', ' Vector C(2  ):', C % val(2  )
  print '(a,es12.3)', ' Vector C(n-1):', C % val(n-1)
  print '(a,es12.3)', ' Vector C(n  ):', C % val(n  )

  print '(a,f12.3,a)', ' # Time elapsed for TEST 1: ', te-ts, ' [s]'

  end subroutine
