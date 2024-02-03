!==============================================================================!
  program Main
!------------------------------------------------------------------------------!
  use Grid_Mod
  use Vector_Mod
  use Matrix_Mod
  use Sparse_Mod
  use Linalg_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Vector_Type)  :: A, B, C
  type(Matrix_Type)  :: Am, Bm, Cm
  type(Sparse_Type)  :: As
  type(Grid_Type)    :: G
  integer            :: n, nx, ny, nz, time_step
  logical            :: fail = .false.
  character(80)      :: arg
  real               :: dot, ts, te
!==============================================================================!

  !-----------------------------------!
  !                                   !
  !   Handle command line arguments   !
  !                                   !
  !-----------------------------------!
  if(command_argument_count() .ne. 1) fail = .true.
  if(.not. fail) then
    call get_command_argument(1, arg)
    if(arg .ne. '1' .and. arg .ne. '2' .and.  &
       arg .ne. '3' .and. arg .ne. '4') fail = .true.
  end if

  if(fail) then
    print *, 'Failed to invoke the program correctly.'
    print *, 'Correct invocation is:'
    print *, ''
    print *, './Program <test>'
    print *, ''
    print *, 'where <test> can be from 1 to 4, depending if you want to test:'
    print *, '  1 - dense-matrix dense-matrix product'
    print *, '  2 - dense-matrix vector product'
    print *, '  3 - sperse-matrix vector product'
    print *, '  4 - vector vector dot product'
    return
  end if

  !-----------------------------------------------------!
  !                                                     !
  !   Try some dense-matrix with dense-matrix product   !
  !                                                     !
  !-----------------------------------------------------!
  if(arg .eq. '1') then

    n = 10000
    print *, '#----------------------------------------------------------'
    print *, '# TEST  1: Performing a dense-matrix dense-matrix product'
    print *, '#          The problem size is set to ', n
    print *, '#----------------------------------------------------------'

    ! Allocate matrices
    call Am % Allocate_Matrix(n)
    call Bm % Allocate_Matrix(n)
    call Cm % Allocate_Matrix(n)

    ! Initialize matrices on the host
    Am % val(:,:) = 1.0
    Bm % val(:,:) = 2.0
    Cm % val(:,:) = 0.0

    ! Copy dense matrices to the device
    call Am % Copy_Matrix_To_Device()
    call Bm % Copy_Matrix_To_Device()
    call Cm % Copy_Matrix_To_Device()

    !-----------------------------------------------!
    !   Performing a fake time loop on the device   !
    !-----------------------------------------------!
    call cpu_time(ts)
    do time_step = 1, 60
      call Linalg % Mat_Mat_Mul(Cm, Am, Bm)
    end do
    call cpu_time(te)

    ! Copy results back to host
    call Cm % Copy_Matrix_To_Host()

    ! Destroy results on the device, you don't need them anymore
    call Am % Destroy_Matrix_On_Device()
    call Bm % Destroy_Matrix_On_Device()
    call Cm % Destroy_Matrix_On_Device()

    ! Print result
    print *, 'Matrix Cm(1,  1  ):', Cm % val(1,   1)
    print *, 'Matrix Cm(2,  2  ):', Cm % val(2,   2)
    print *, 'Matrix Cm(n-1,n-1):', Cm % val(n-1, n-1)
    print *, 'Matrix Cm(n,  n  ):', Cm % val(n,   n)

    print '(a,f12.3,a)', '# Time elapsed for TEST  1: ', te-ts, ' [s]'
  end if

  !-----------------------------------------------!
  !                                               !
  !   Try some dense-matrix with vector product   !
  !                                               !
  !-----------------------------------------------!
  if(arg .eq. '2') then

    n = 10000
    print *, '#----------------------------------------------------'
    print *, '# TEST  2: Performing a dense-matrix vector product'
    print *, '#          The problem size is set to ', n
    print *, '#----------------------------------------------------'

    ! Allocate matrix and vectors
    call Am % Allocate_Matrix(n)
    call B  % Allocate_Vector(n)
    call C  % Allocate_Vector(n)

    ! Initialize matrix and vectors
    Am % val(:,:) = 1.0
    B  % val(:)   = 2.0
    C  % val(:)   = 0.0

    ! Copy matrix and vectors to the device
    call Am % Copy_Matrix_To_Device()
    call B  % Copy_Vector_To_Device()
    call C  % Copy_Vector_To_Device()

    !-----------------------------------------------!
    !   Performing a fake time loop on the device   !
    !-----------------------------------------------!
    call cpu_time(ts)
    do time_step = 1, 60
      call Linalg % Mat_Vec_Mul(C, Am, B)
    end do
    call cpu_time(te)

    ! Copy results back to host
    call C % Copy_Vector_To_Host()

    ! Destroy data on the device, you don't need them anymore
    call Am % Destroy_Matrix_On_Device()
    call B  % Destroy_Vector_On_Device()
    call C  % Destroy_Vector_On_Device()

    ! Print result
    print *, 'Vector C(1  ):', C % val(1  )
    print *, 'Vector C(2  ):', C % val(2  )
    print *, 'Vector C(n-1):', C % val(n-1)
    print *, 'Vector C(n  ):', C % val(n  )

    print '(a,f12.3,a)', '# Time elapsed for TEST  2: ', te-ts, ' [s]'
  end if

  !------------------------------------------------!
  !                                                !
  !   Try some sparse-matrix with vector product   !
  !                                                !
  !------------------------------------------------!
  if(arg .eq. '3') then

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
    C % val(:) = 0.0

    ! Copy sparse matrix and vectors to the device
    call As % Copy_Sparse_To_Device()
    call B  % Copy_Vector_To_Device()
    call C  % Copy_Vector_To_Device()

    !-----------------------------------------------!
    !   Performing a fake time loop on the device   !
    !-----------------------------------------------!
    print *, '# Performing a sparse-matrix vector product'
    call cpu_time(ts)
    do time_step = 1, 60
      call Linalg % Spa_Vec_Mul(C, As, B)
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
  end if

  !----------------------------------------!
  !                                        !
  !   Try some vector vector dot product   !
  !                                        !
  !----------------------------------------!
  if(arg .eq. '4') then

    nx = 800
    ny = 800
    nz = 800
    n  = nx * ny * nz
    print *, '#--------------------------------------------------'
    print *, '# TEST  4: Performing a vector vector dot product'
    print *, '#          The problem size is set to ', n
    print *, '#--------------------------------------------------'

    print *, '# Creating two vectors for that grid'
    call A % Allocate_Vector(n)
    call B % Allocate_Vector(n)

    A % val(:) = 1.0
    B % val(:) = 2.0

    ! Copy vectors to the device
    call A % Copy_Vector_To_Device()
    call B % Copy_Vector_To_Device()

    !-----------------------------------------------!
    !   Performing a fake time loop on the device   !
    !-----------------------------------------------!
    print *, '# Performing a vector vector dot product'
    call cpu_time(ts)
    do time_step = 1, 60
      call Linalg % Vec_Vec_Dot(dot, A, B)
    end do
    call cpu_time(te)

    ! Destroy data on the device, you don't need them anymore
    call A % Destroy_Vector_On_Device()
    call B % Destroy_Vector_On_Device()

    ! Print result
    print *, 'dot product: ', dot

    print '(a,f12.3,a)', '# Time elapsed for TEST  4: ', te-ts, ' [s]'
  end if

  end program

