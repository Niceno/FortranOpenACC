!==============================================================================!
  subroutine Test_003
!------------------------------------------------------------------------------!
!>  Tests parse-matrix with vector product
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
    print *, '#-----------------------------------------------------'
    print *, '# TEST  3: Performing a sparse-matrix vector product'
    print *, '#          The problem size is set to ', n
    print *, '#-----------------------------------------------------'

    print *, '# Creating a grid'
    call G % Create_Grid(1.0, 1.0, 1.0, nx, ny, nz)

    print *, '# Creating a sparse matrix and two vectors for that grid'
    call As % Create_Sparse(G)
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
    print *, '# Performing a sparse-matrix vector product'
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
    print *, 'Vector C(1  ):', C % val(1  )
    print *, 'Vector C(2  ):', C % val(2  )
    print *, 'Vector C(n-1):', C % val(n-1)
    print *, 'Vector C(n  ):', C % val(n  )

    print '(a,f12.3,a)', '# Time elapsed for TEST  3: ', te-ts, ' [s]'

  end subroutine
