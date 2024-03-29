!==============================================================================!
  subroutine Test_002
!------------------------------------------------------------------------------!
!>  Tests dense-matrix with vector product
!------------------------------------------------------------------------------!
  use Linalg_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Vector_Type) :: B, C
  type(Dense_Type)  :: Am
  integer           :: n, time_step
  real              :: ts, te
!==============================================================================!

    n = 10000
    print *, '#----------------------------------------------------'
    print *, '# TEST  2: Performing a dense-vector product'
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
      call Linalg % Den_X_Vec(C, Am, B)
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

  end subroutine
