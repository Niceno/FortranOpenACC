!==============================================================================!
  program Main
!------------------------------------------------------------------------------!
!   In a nutshell, this program serves the same function as the previous step  !
!   (stored in ../Step_001), which is to compute the sum of three matrices:    !
!   c = a + b on a GPU.                                                        !
!                                                                              !
!   Although the first step to write a Fortran program and couple it with GPU  !
!   functionality was a success, and although that first step entailed all the !
!   functionality a typical CFD simulation on GPUs would entail (1 - creation  !
!   of data on the "host"; 2 - copying the data to the "device"; 3- performing !
!   the computations on the "device"; 4 - copying the computed data back to    !
!   "host" and 5 - postprocessing the results on the "host", many questions    !
!   remained open.                                                             !
!                                                                              !
!   Given that the first step had all the functionality within one source      !
!   file, spanning a mere few dozen of lines, the qustions which arises next   !
!   is how to embed this functionality within a bigger project?  How to deal   !
!   with Fortran-GPU coupling inside a program which has many modules, a       !
!   program whose data is passed from one module to another, a program which   !
!   several hundreds of line of code?                                          !
!                                                                              !
!   Although these questions are mind-boggling, it is probably possible to     !
!   address them one at a time.  This was the motivation to take this step.    !
!   This step should have the same functionality as the first one, but it is   !
!   supposed to be spread over several functions, global functions at this     !
!   time.                                                                      !
!                                                                              !
!   What this step brings is the main function which creates the data on the   !
!   "host" and makes the post-processing (still only printing), but copying to !
!   "device", computations on "device" and copying the data back to the "host" !
!   is performed by different global functions here.                           !
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
  call copy_to_device   (N, a)        ! operand in c = a + b, copy its contents
  call copy_to_device   (N, b)        ! operand in c = a + b. copy its contents
  call create_on_device (N, c)        ! result in c = a + b, create memory only
  call compute_on_device(N, a, b, c)  ! compute on the "device"
  call copy_from_device (N, c)        ! retrieve the result

  ! Print (post-process) the result from the "host"
  print *, 'Matrix c(1,1):', c(1,1)
  print *, 'Matrix c(n,n):', c(N,N)

  end program

!==============================================================================!
  subroutine copy_to_device(n, c)
!------------------------------------------------------------------------------!
!   This subroutines creates (allocates) memory on "device" for varible c, and !
!   it also coppies its contents to the device.  It is useful for data which   !
!   are operands (as opposed to results) in the computations performed on the  !
!   "device".                                                                  !
!                                                                              !
!   Subroutine related to this one is "create_one_device" which allocates      !
!   memory  on the "device", but does not copy the contents to the "device".   !
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer              :: n
  real, dimension(n,n) :: c
!==============================================================================!

  !$acc enter data copyin(c)

  end subroutine

!==============================================================================!
  subroutine create_on_device(n, c)
!------------------------------------------------------------------------------!
!   This subroutines creates (allocates) memory on "device" for varible c, but !
!   it doesn't copy its contents to the device.  It is useful for data which   !
!   doesn't depend on its initial values, the data which is completelly over-  !
!   written by the calculations on the "device", the data which is the result  !
!   of computations on the device.                                             !
!                                                                              !
!   Subroutine related to this one is "copy_to_device" which allocates memory  !
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
  subroutine copy_from_device(n, c)
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
  subroutine compute_on_device(n, a, b, c)
!------------------------------------------------------------------------------!
!   This subroutine performs computations on "device", on the data which was   !
!   previously (see the main funciton) transferred to the "device".            !
!                                                                              !
!   This subroutine uses statement "!$acc data present(a, b, c)", which must   !
!   be coupled with "!$acc end data" for compiler to work, for which I belived !
!   were just checks, to make sure that data is on the "device".  However, I   !
!   have learned later that the combination of these two functions is actually !
!   implicitly copying data back to host.  Hence, in order to improve perfor-  !
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

