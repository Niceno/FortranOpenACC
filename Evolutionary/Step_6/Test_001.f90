!==============================================================================!
  subroutine Test_001
!------------------------------------------------------------------------------!
!>  Tests dense-matrix with dense-matrix product
!------------------------------------------------------------------------------!
  use Linalg_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Dense_Type) :: Am, Bm, Cm
  integer           :: n, time_step
  real              :: ts, te
!==============================================================================!

    n = 10000
    print *, '#----------------------------------------------'
    print *, '# TEST  1: Performing a dense-dense product'
    print *, '#          The problem size is set to ', n
    print *, '#----------------------------------------------'

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
      call Linalg % Den_X_Den(Cm, Am, Bm)
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

  end subroutine
