!==============================================================================!
  subroutine Test_004
!------------------------------------------------------------------------------!
!>  Tests diagonal preconditioner
!------------------------------------------------------------------------------!
  use Linalg_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Vector_Type)  :: B, C
  type(Matrix_Type)  :: A
  type(Grid_Type)    :: G
  integer            :: n, nx, ny, nz, time_step
  real               :: ts, te
!==============================================================================!

  nx = 300
  ny = 300
  nz = 300
  n  = nx * ny * nz
  print '(a)',     ' #-----------------------------------------------------'
  print '(a)',     ' # TEST 4: Performing diagonal preconditioning'
  print '(a,i12)', ' #         The problem size is set to ', n
  print '(a)',     ' #-----------------------------------------------------'

  print '(a)', ' # Creating a grid'
  call G % Create_Grid(1.0, 1.0, 1.0, nx, ny, nz)

  call A % Create_Matrix(G, singular=.false.)
  call B % Allocate_Vector(n)
  call C % Allocate_Vector(n)

  B % val(:) = 1.0

  ! Copy operand matrix and vector to the device ...
  ! ... and reserve memory for result vector on device
  call A % Copy_Matrix_To_Device()
  call B % Copy_Vector_To_Device()
  call C % Create_Vector_On_Device()

  !-----------------------------------------------!
  !   Performing a fake time loop on the device   !
  !-----------------------------------------------!
  print '(a)', ' # Performing diagonal preconditioning'
  call cpu_time(ts)
  do time_step = 1, 60
    call Linalg % Vec_O_Dia(C, A, B)
  end do
  call cpu_time(te)

  ! Copy results back to host
  call C % Copy_Vector_To_Host()

  ! Destroy data on the device, you don't need them anymore
  call A % Destroy_Matrix_On_Device()
  call B % Destroy_Vector_On_Device()
  call C % Destroy_Vector_On_Device()

  ! Print result
  print '(a,es12.3)', ' Vector C(1  ):', C % val(1  )
  print '(a,es12.3)', ' Vector C(2  ):', C % val(2  )
  print '(a,es12.3)', ' Vector C(n-1):', C % val(n-1)
  print '(a,es12.3)', ' Vector C(n  ):', C % val(n  )

  print '(a,f12.3,a)', ' # Time elapsed for TEST 4: ', te-ts, ' [s]'

  end subroutine
