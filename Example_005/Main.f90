!==============================================================================!
  program matrix_addition
!------------------------------------------------------------------------------!
  use Grid_Mod
  use Vector_Mod
  use Matrix_Mod
  use Sparse_Mod
  use Compute_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer, parameter :: NX = 25             ! these three ...
  integer, parameter :: NY = 25             ! ... mimic an ...
  integer, parameter :: NZ = 25             ! ... orthogonal grid
  integer, parameter :: N  = NX * NY * NZ   ! total number of unkowns
  type(Vector_Type)  :: B, C
  type(Matrix_Type)  :: Am, Bm, Cm
  type(Sparse_Type)  :: As
  type(Grid_Type)    :: G
!==============================================================================!

  !---------------------------------!
  !   Try some matrix-matrix copy   !
  !---------------------------------!
  print *, '# Performing a dense-matrix dense-matrix product'

  ! Allocate matrices
  call Am % Allocate_Matrix(N)
  call Bm % Allocate_Matrix(N)
  call Cm % Allocate_Matrix(N)

  ! Initialize matrices on the host
  Am % val(:,:) = 1.0
  Bm % val(:,:) = 2.0
  Cm % val(:,:) = 0.0

  ! Copy dense matrices to the device
  call Am % Copy_Matrix_To_Device()
  call Bm % Copy_Matrix_To_Device()
  call Cm % Copy_Matrix_To_Device()

  ! Perform global computations
  call Global % Compute_Mat_Mat_Mul(Cm, Am, Bm)

  ! Copy results back to host
  call Cm % Copy_Matrix_To_Host()

  ! Print result
  print *, 'Matrix Cm(1,  1  ):', Cm % val(1,   1)
  print *, 'Matrix Cm(2,  2  ):', Cm % val(2,   2)
  print *, 'Matrix Cm(n-1,n-1):', Cm % val(N-1, N-1)
  print *, 'Matrix Cm(n,  n  ):', Cm % val(N,   N)

  !---------------------------------!
  !   Try some matrix-vector copy   !
  !---------------------------------!
  print *, '# Performing a dense-matrix vector product'

  ! Allocate vectors
  call B % Allocate_Vector(N)
  call C % Allocate_Vector(N)

  ! Initialize vectors
  B % val(:) = 2.0
  C % val(:) = 0.0

  ! Copy vectors to the device
  call B % Copy_Vector_To_Device()
  call C % Copy_Vector_To_Device()

  call Global % Compute_Mat_Vec_Mul(C, Am, B)

  ! Copy results back to host
  call C % Copy_Vector_To_Host()

  ! Print result
  print *, 'Vector C(1  ):', C % val(1  )
  print *, 'Vector C(2  ):', C % val(2  )
  print *, 'Vector C(n-1):', C % val(N-1)
  print *, 'Vector C(n  ):', C % val(N  )

  !--------------------------!
  !   Try to create a grid   !
  !--------------------------!

  print *, '# Creating a grid'
  call G % Create_Grid(1.0, 1.0, 1.0, NX, NY, NZ)

  print *, '# Creating a sparse matrix from that grid'
  call As % Create_Sparse(G)

  ! Copy sparse matrix to the device
  call As % Copy_Sparse_To_Device()
  call B  % Copy_Vector_To_Device()
  call C  % Copy_Vector_To_Device()

  print *, '# Performing a sparse-matrix vector product'
  call Global % Compute_Spa_Vec_Mul(C, As, B)

  ! Copy results back to host
  call C % Copy_Vector_To_Host()

  ! Print result
  print *, 'Vector C(1  ):', C % val(1  )
  print *, 'Vector C(2  ):', C % val(2  )
  print *, 'Vector C(n-1):', C % val(N-1)
  print *, 'Vector C(n  ):', C % val(N  )

  end program

