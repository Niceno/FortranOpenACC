!==============================================================================!
  subroutine Test_002
!------------------------------------------------------------------------------!
  use Gpu_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  real, allocatable :: a(:), b(:)
  integer           :: n, nx, ny, nz, time_step
  real              :: dot, ts, te
!==============================================================================!

  print '(a)',     ' #-------------------------------------------------'
  print '(a)',     ' # TEST 2: Performing a vector vector dot product'
  print '(a)',     ' #-------------------------------------------------'

  nx = 800
  ny = 800
  nz = 800
  n  = nx * ny * nz
  print '(a,i12)', ' # The problem size is: ', n

  print '(a)', ' # Creating two vectors'
  allocate(a(n))
  allocate(b(n))

  a(:) = 1.0
  b(:) = 2.0

  ! Copy vectors to the device
  call Gpu % Vector_Copy_To_Device(a)
  call Gpu % Vector_Copy_To_Device(b)

  !-----------------------------------------------!
  !   Performing a fake time loop on the device   !
  !-----------------------------------------------!
  print '(a)', ' # Performing a vector vector dot product'
  call cpu_time(ts)
  do time_step = 1, 60
    call Linalg % Vec_D_Vec(dot, a, b)
  end do
  call cpu_time(te)

  ! Destroy data on the device, you don't need them anymore
  call Gpu % Vector_Destroy_On_Device(a)
  call Gpu % Vector_Destroy_On_Device(b)

  ! Print result
  print '(a,es12.3)', ' Dot product: ', dot

  print '(a,f12.3,a)', ' # Time elapsed for TEST 2: ', te-ts, ' [s]'

  end subroutine
