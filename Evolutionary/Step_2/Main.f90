!==============================================================================!
  program Main
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer, parameter   :: N = 10000
  real, dimension(N,N) :: a, b, c
  integer              :: i, j, iter
!==============================================================================!

  ! Initialize matrices; create data on "host"
  a = 1.0
  b = 2.0
  c = 0.0

  !  Copy data to "device", perform computations there, and get result back
  call Copy_To_Device   (N, a)        ! operand in c = a + b, copy its contents
  call Copy_To_Device   (N, b)        ! operand in c = a + b. copy its contents
  call Create_On_Device (N, c)        ! result in c = a + b, create memory only
  call Compute_On_Device(N, a, b, c)  ! compute on the "device"
  call Copy_From_Device (N, c)        ! retrieve the result

  ! Print (post-process) the result from the "host"
  print *, 'Matrix c(1,1):', c(1,1)
  print *, 'Matrix c(n,n):', c(N,N)

  end program

!==============================================================================!
  subroutine Copy_To_Device(n, c)
!------------------------------------------------------------------------------!
!   This subroutines creates (allocates) memory on "device" for varible c,     !
!   and also coppies its contents to the device.  It is useful for data which  !
!   are operands (as opposed to results) in the computations performed on the  !
!   "device".                                                                  !
!                                                                              !
!   Subroutine related to this one is "Create_On_Device" which allocates       !
!   memory on the "device", but does not copy the contents of the variable     !
!   from the "host" to the "device".                                           !
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer              :: n
  real, dimension(n,n) :: c
!==============================================================================!

  !$acc enter data copyin(c)

  end subroutine

!==============================================================================!
  subroutine Create_On_Device(n, c)
!------------------------------------------------------------------------------!
!   This subroutines creates (allocates) memory on "device" for varible c, but !
!   it doesn't copy its contents to the device.  It is useful for data which   !
!   doesn't depend on its initial values, the data which is completelly over-  !
!   written by the calculations on the "device", the data which is the result  !
!   of computations on the device.                                             !
!                                                                              !
!   Subroutine related to this one is "Copy_To_Device" which allocates memory  !
!   on the "device", but also coppies the contents to the "device".            !
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer              :: n
  real, dimension(n,n) :: c
!==============================================================================!

  !$acc enter data create(c)

  end subroutine

!==============================================================================!
  subroutine Copy_From_Device(n, c)
!------------------------------------------------------------------------------!
!   Explicitly retrieve data stored in c, from "device" to the "host".  This   !
!   is useful for results obtained on the "device", which need further post-   !
!   processing on the "host".
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer              :: n
  real, dimension(n,n) :: c
!==============================================================================!

  !$acc exit data copyout(c)

  end subroutine

!==============================================================================!
  subroutine Compute_On_Device(n, a, b, c)
!------------------------------------------------------------------------------!
!   This subroutine performs computations on "device", on the data which was   !
!   previously (in the main function) transferred to the "device".             !
!                                                                              !
!   This subroutine uses statement "!$acc data present(a, b, c)", which must   !
!   be coupled with "!$acc end data" for compiler to work, for which I belived !
!   were just checks, to make sure that data is on the "device".  However, I   !
!   have learned later that the combination of these two functions is actually !
!   physically copying data back to host.  Hence, in order to improve perfor-  !
!   mance, maybe we should be more brave, and just leave these statements      !
!   behind.  Indeed, try to compile the code without them to see that the code !
!   works just the same.                                                       !
!                                                                              !
!   To put it in other words, avoiding the "!$acc data present(a, b, c)" in    !
!   the code, tells the men from the boys :-)                                  !
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer              :: n
  real, dimension(n,n) :: a, b, c
  integer :: i, j, iter
!==============================================================================!

  !$acc data present(a, b, c)
  !$acc kernels
  do iter = 1, 60
    do j = 1, n
      do i = 1, n
        c(i,j) = a(i,j) + b(i,j)
      end do
    end do
  end do
  !$acc end kernels
  !$acc end data

  end subroutine

