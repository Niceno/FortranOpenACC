!==============================================================================!
  subroutine Test_004
!------------------------------------------------------------------------------!
  use Linalg_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Vector_Type)  :: A, B
  integer            :: n, nx, ny, nz, time_step
  real               :: dot, ts, te
!==============================================================================!

    nx = 800
    ny = 800
    nz = 800
    n  = nx * ny * nz
    print *, '#--------------------------------------------------'
    print *, '# TEST  4: Performing a vector vector dot product'
    print *, '#          The problem size is set to ', n
    print *, '#--------------------------------------------------'

    print *, '# Creating two vectors'
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
      call Linalg % Vec_D_Vec(dot, A, B)
    end do
    call cpu_time(te)

    ! Destroy data on the device, you don't need them anymore
    call A % Destroy_Vector_On_Device()
    call B % Destroy_Vector_On_Device()

    ! Print result
    print *, 'dot product: ', dot

    print '(a,f12.3,a)', '# Time elapsed for TEST  4: ', te-ts, ' [s]'

  end subroutine
