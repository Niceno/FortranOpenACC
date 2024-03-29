!==============================================================================!
  program Main
!------------------------------------------------------------------------------!
  use Grid_Mod
  use Vector_Mod
  use Dense_Mod
  use Sparse_Mod
  use Compute_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Vector_Type) :: B, C
  type(Dense_Type)  :: Am, Bm, Cm
  type(Sparse_Type) :: As
  type(Grid_Type)   :: G
  integer           :: n, nx, ny, nz, time_step
  logical           :: fail = .false.
  character(80)     :: arg
  real              :: ts, te
!==============================================================================!

  !-----------------------------------!
  !                                   !
  !   Handle command line arguments   !
  !                                   !
  !-----------------------------------!
  if(command_argument_count() .ne. 1) fail = .true.
  if(.not. fail) then
    call get_command_argument(1, arg)
    if(arg .ne. '1' .and. arg .ne. '2' .and. arg .ne. '3') fail = .true.
  end if

  if(fail) then
    print *, 'Failed to invoke the program correctly.'
    print *, 'Correct invocation is:'
    print *, ''
    print *, './Program <test>'
    print *, ''
    print *, 'where <test> can be 1, 2 or 3, depending if you want to test:'
    print *, '  1 - dense-dense multiplication'
    print *, '  2 - dense-vector multiplication'
    print *, '  3 - sperse-vector multiplication'
    return
  end if

  !------------------------------------------------------------!
  !                                                            !
  !   Try some dense-matrix with dense-matrix multiplication   !
  !                                                            !
  !------------------------------------------------------------!
  if(arg .eq. '1') then

    n = 10000
    print *, '#----------------------------------------------------------'
    print *, '# TEST  1: Performing a dense-matrix dense-matrix product'
    print *, '#          The problem size is set to ', n
    print *, '#----------------------------------------------------------'

    ! Allocate matrices
    call Am % Allocate_Dense(n)
    call Bm % Allocate_Dense(n)
    call Cm % Allocate_Dense(n)

    ! Initialize matrices on the host
    Am % val(:,:) = 1.0
    Bm % val(:,:) = 2.0
    Cm % val(:,:) = 0.0

    ! Copy dense matrices to the device
    call Am % Copy_Dense_To_Device()
    call Bm % Copy_Dense_To_Device()
    call Cm % Copy_Dense_To_Device()

    !-----------------------------------------------!
    !   Performing a fake time loop on the device   !
    !-----------------------------------------------!
    call cpu_time(ts)
    do time_step = 1, 60
      call Global % Compute_Den_Den_Mul(Cm, Am, Bm)
    end do
    call cpu_time(te)

    ! Copy results back to host
    call Cm % Copy_Dense_To_Host()

    ! Destroy results on the device, you don't need them anymore
    call Am % Destroy_Dense_On_Device()
    call Bm % Destroy_Dense_On_Device()
    call Cm % Destroy_Dense_On_Device()

    ! Print result
    print *, 'Dense Cm(1,  1  ):', Cm % val(1,   1)
    print *, 'Dense Cm(2,  2  ):', Cm % val(2,   2)
    print *, 'Dense Cm(n-1,n-1):', Cm % val(n-1, n-1)
    print *, 'Dense Cm(n,  n  ):', Cm % val(n,   n)

    print '(a,f12.3,a)', '# Time elapsed for TEST  1: ', te-ts, ' [s]'
  end if

  !------------------------------------------------------!
  !                                                      !
  !   Try some dense-matrix with vector multiplication   !
  !                                                      !
  !------------------------------------------------------!
  if(arg .eq. '2') then

    n = 10000
    print *, '#----------------------------------------------------'
    print *, '# TEST  2: Performing a dense-matrix vector product'
    print *, '#          The problem size is set to ', n
    print *, '#----------------------------------------------------'

    ! Allocate matrix and vectors
    call Am % Allocate_Dense(n)
    call B  % Allocate_Vector(n)
    call C  % Allocate_Vector(n)

    ! Initialize matrix and vectors
    Am % val(:,:) = 1.0
    B  % val(:)   = 2.0
    C  % val(:)   = 0.0

    ! Copy matrix and vectors to the device
    call Am % Copy_Dense_To_Device()
    call B  % Copy_Vector_To_Device()
    call C  % Copy_Vector_To_Device()

    !-----------------------------------------------!
    !   Performing a fake time loop on the device   !
    !-----------------------------------------------!
    call cpu_time(ts)
    do time_step = 1, 60
      call Global % Compute_Den_Vec_Mul(C, Am, B)
    end do
    call cpu_time(te)

    ! Copy results back to host
    call C % Copy_Vector_To_Host()

    ! Destroy data on the device, you don't need them anymore
    call Am % Destroy_Dense_On_Device()
    call B  % Destroy_Vector_On_Device()
    call C  % Destroy_Vector_On_Device()

    ! Print result
    print *, 'Vector C(1  ):', C % val(1  )
    print *, 'Vector C(2  ):', C % val(2  )
    print *, 'Vector C(n-1):', C % val(n-1)
    print *, 'Vector C(n  ):', C % val(n  )

    print '(a,f12.3,a)', '# Time elapsed for TEST  2: ', te-ts, ' [s]'
  end if

  !-------------------------------------------------------!
  !                                                       !
  !   Try some sparse-matrix with vector multiplication   !
  !                                                       !
  !-------------------------------------------------------!
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
      call Global % Compute_Spa_Vec_Mul(C, As, B)
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

  end program

