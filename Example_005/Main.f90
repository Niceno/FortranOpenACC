!==============================================================================!
  program matrix_addition
!------------------------------------------------------------------------------!
  use Vector_Mod
  use Matrix_Mod
  use Compute_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer, parameter :: N = 10000
  type(Matrix_Type)  :: AA, BB, CC
  type(Vector_Type)  :: B, C
!==============================================================================!

  !---------------------------------!
  !   Try some matrix-matrix copy   !
  !---------------------------------!

  ! Allocate matrices
  call AA % Matrix_Allocate(N)
  call BB % Matrix_Allocate(N)
  call CC % Matrix_Allocate(N)

  ! Initialize matrices on the host
  AA % val(:,:) = 1.0
  BB % val(:,:) = 2.0
  CC % val(:,:) = 0.0

  ! Copy matrices to the device
  call AA % Matrix_Copy_To_Device()
  call BB % Matrix_Copy_To_Device()
  call CC % Matrix_Copy_To_Device()

  ! Perform global computations
  call Global % Compute_Mat_Mat_Mul(CC, AA, BB)

  ! Copy results back to host
  call CC % Matrix_Copy_To_Host()

  ! Print result
  print *, 'Matrix CC(1,  1  ):', CC % val(1,   1)
  print *, 'Matrix CC(2,  2  ):', CC % val(2,   2)
  print *, 'Matrix CC(n-1,n-1):', CC % val(N-1, N-1)
  print *, 'Matrix CC(n,  n  ):', CC % val(N,   N)

  !---------------------------------!
  !   Try some matrix-vector copy   !
  !---------------------------------!

  ! Allocate vectors
  call B % Vector_Allocate(N)
  call C % Vector_Allocate(N)

  ! Initialize vectors
  B % val(:) = 2.0
  C % val(:) = 0.0

  ! Copy vectors to the device
  call B % Vector_Copy_To_Device()
  call C % Vector_Copy_To_Device()

  call Global % Compute_Mat_Vec_Mul(C, AA, B)

  ! Copy results back to host
  call C % Vector_Copy_To_Host()

  ! Print result
  print *, 'Vector C(1  ):', C % val(1  )
  print *, 'Vector C(2  ):', C % val(2  )
  print *, 'Vector C(n-1):', C % val(N-1)
  print *, 'Vector C(n  ):', C % val(N  )

  end program

