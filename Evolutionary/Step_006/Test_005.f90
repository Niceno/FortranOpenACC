!==============================================================================!
  subroutine Test_005
!------------------------------------------------------------------------------!
!>  Tests vector operations of the form C = A +/- s * B
!------------------------------------------------------------------------------!
  use Linalg_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Vector_Type)  :: A, B, C, D
  integer            :: n, nx, ny, nz, time_step
  real               :: ts, te
!==============================================================================!

    nx = 600
    ny = 600
    nz = 600
    n  = nx * ny * nz
    print *, '#-----------------------------------------------------'
    print *, '# TEST  5: Performing vector operations:'
    print *, '#          C = A + s * B  and  C = A - s * B'
    print *, '#          The problem size is set to ', n
    print *, '#-----------------------------------------------------'

    print *, '# Creating three vectors'
    call A % Allocate_Vector(n)
    call B % Allocate_Vector(n)
    call C % Allocate_Vector(n)
    call D % Allocate_Vector(n)

    A % val(:) = 1.0
    B % val(:) = 2.0
    C % val(:) = 0.0
    D % val(:) = 0.0

    ! Copy vectors to the device
    call A % Copy_Vector_To_Device()
    call B % Copy_Vector_To_Device()
    call C % Copy_Vector_To_Device()
    call D % Copy_Vector_To_Device()

    !-----------------------------------------------!
    !   Performing a fake time loop on the device   !
    !-----------------------------------------------!
    print *, '# Performing a sparse-matrix vector product'
    call cpu_time(ts)
    do time_step = 1, 60
      call Linalg % Vec_P_Sca_X_Vec(C, A, 2.0, B)  ! result should be  5
      call Linalg % Vec_M_Sca_X_Vec(D, C, 2.0, B)  ! result should be  1
    end do
    call cpu_time(te)

    ! Copy results back to host
    call C % Copy_Vector_To_Host()
    call D % Copy_Vector_To_Host()

    ! Destroy data on the device, you don't need them anymore
    call A % Destroy_Vector_On_Device()
    call B % Destroy_Vector_On_Device()
    call C % Destroy_Vector_On_Device()
    call D % Destroy_Vector_On_Device()

    ! Print results
    print *, 'Vector C(1  ):', C % val(1  )
    print *, 'Vector C(2  ):', C % val(2  )
    print *, 'Vector C(n-1):', C % val(n-1)
    print *, 'Vector C(n  ):', C % val(n  )
    print *, 'Vector D(1  ):', D % val(1  )
    print *, 'Vector D(2  ):', D % val(2  )
    print *, 'Vector D(n-1):', D % val(n-1)
    print *, 'Vector D(n  ):', D % val(n  )

    print '(a,f12.3,a)', '# Time elapsed for TEST  5: ', te-ts, ' [s]'

  end subroutine
