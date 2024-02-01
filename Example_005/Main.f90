!==============================================================================!
  program matrix_addition
!------------------------------------------------------------------------------!
  use Vector_Mod
  use Full_Matrix_Mod
  use Compute_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer, parameter     :: N = 10000
  type(Full_Matrix_Type) :: Mat_A, Mat_B, Mat_C
  type(Vector_Type)      :: Vec_A, Vec_B, Vec_C
!==============================================================================!

  !---------------------------------!
  !   Try some matrix-matrix copy   !
  !---------------------------------!

  ! Allocate matrices
  call Mat_A % Full_Matrix_Allocate(N)
  call Mat_B % Full_Matrix_Allocate(N)
  call Mat_C % Full_Matrix_Allocate(N)

  ! Initialize matrices on the host
  Mat_A % val(:,:) = 1.0
  Mat_B % val(:,:) = 2.0
  Mat_C % val(:,:) = 0.0

  ! Copy matrices to the device
  call Mat_A % Full_Matrix_Copy_To_Device()
  call Mat_B % Full_Matrix_Copy_To_Device()
  call Mat_C % Full_Matrix_Copy_To_Device()

  ! Perform global computations
  call Global_Compute % Compute_Mat_Mat_Mul(N, Mat_C % val, Mat_A % val, Mat_B % val)

  call Mat_C % Full_Matrix_Copy_From_Device()

  ! Print result
  print *, 'Matrix c(1,  1  ):', Mat_C % val(1,   1)
  print *, 'Matrix c(2,  2  ):', Mat_C % val(2,   2)
  print *, 'Matrix c(n-1,n-1):', Mat_C % val(N-1, N-1)
  print *, 'Matrix c(n,  n  ):', Mat_C % val(N,   N)

  !---------------------------------!
  !   Try some matrix-vector copy   !
  !---------------------------------!

  ! Allocate vectors
  call Vec_A % Vector_Allocate(N)
  call Vec_B % Vector_Allocate(N)
  call Vec_C % Vector_Allocate(N)

  ! Initialize vectors
  Vec_A % val(:) = 1.0
  Vec_B % val(:) = 2.0
  Vec_C % val(:) = 0.0

  ! Copy vectors to the device
  call Vec_A % Vector_Copy_To_Device()
  call Vec_B % Vector_Copy_To_Device()
  call Vec_C % Vector_Copy_To_Device()

  call Global_Compute % Compute_Mat_Vec_Mul(N, Vec_C % val, Mat_A % val, Vec_B % val)

  call Vec_C % Vector_Copy_From_Device()

  ! Print result
  print *, 'Vector c(1  ):', Vec_C % val(1  )
  print *, 'Vector c(2  ):', Vec_C % val(2  )
  print *, 'Vector c(n-1):', Vec_C % val(N-1)
  print *, 'Vector c(n  ):', Vec_C % val(N  )

  end program

