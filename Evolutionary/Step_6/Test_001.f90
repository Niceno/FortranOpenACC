!==============================================================================!
  subroutine Test_001
!------------------------------------------------------------------------------!
!>  Tests dense-matrix with dense-matrix product
!------------------------------------------------------------------------------!
  use Linalg_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Matrix_Type) :: Am, Bm, Cm
  integer           :: n, time_step
  real              :: ts, te
!==============================================================================!

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
      call Linalg % Mat_X_Mat(Cm, Am, Bm)
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

  end subroutine
