!==============================================================================!
!   The first evolutionary step (stored in ../Step_001) showed the basic       !
!   functionality of Fortran and OpenACC coupling, all within a single         !
!   programming unit (main function).  The second evolutionary step (stored    !
!   in ../Step_002) showed that OpenACC commands can spread over different     !
!   programming units; meaning subroutines in Fortran.                         !
!                                                                              !
!   This example goes a step further and shows how data enclosed inside more   !
!   complex data types can be transferred to and from GPUs.  The program in-   !
!   troduces new type (Full_Matrix_Type) inside the module (Full_Matrix_Mod)   !
!   with two data members.  It uses main function to transfer the data from    !
!   the Full_Matrix_Type to device and back, using the global functions        !
!   copy_to_device and copy_from_device and the computations are done on       !
!   the device calling the global function compute_on_device.                  !
!                                                                              !
!   An important aspect when transferring data belonging to more complex types !
!   to and from a "device" is that the derived data types can't be transfered, !
!   only its basic data type components, which can be seen in the main         !
!   function where transfer takes place.                                       !
!                                                                              !
!   Another important aspect is that the subroutines performing calculations   !
!   on the host, do not need "!$acc data present ..." and "!$acc end data"     !
!   clauses, if one is sure data is on the device.                             !
!------------------------------------------------------------------------------!

!==============================================================================!
  module Full_Matrix_Mod
!------------------------------------------------------------------------------!
!   Introduces a more complex data type to be transferred to and back from     !
!   the "device".  It does not have member function which do the transfer,     !
!   the transfer is still done from the main function.                         !
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !----------------------!
  !   Full Matrix type   !
  !----------------------!
  type Full_Matrix_Type

    integer           :: len
    real, allocatable :: val(:,:)

    contains
      procedure :: Allocate_Full_Matrix

  end type

  contains

!==============================================================================!
  subroutine Allocate_Full_Matrix(A, n)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Full_Matrix_Type) :: A
  integer, intent(in)     :: n
!==============================================================================!

  ! Store the length
  A % len = n

  ! Allocate the memory
  allocate(A % val(n, n))

  end subroutine

  end module

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
!                                                                              !
!   Note that this subroutine is the same as in previous step (../Step_002)    !
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
!                                                                              !
!   Note that this subroutine is the same as in previous step (../Step_002)    !
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
!   processing on the "host".                                                  !
!                                                                              !
!   Note that this subroutine is the same as in previous step (../Step_002)    !
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
!   This subroutine differs from the one in the previous step (../Step_002)    !
!   in the absence of statements "!$acc data present(a, b, c)", coupled with   !
!   "!$acc end data", because I learned that the combination of these two      !
!   functions is also copying data back to host. (Maybe even coppies data to   !
!   host, that should be double checked.                                       !
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer              :: n
  real, dimension(n,n) :: a, b, c
  integer :: i, j, iter
!==============================================================================!

  !$acc kernels
  do iter = 1, 60
    do j = 1, n
      do i = 1, n
        c(i,j) = a(i,j) + b(i,j)
      end do
    end do
  end do
  !$acc end kernels

  end subroutine

!==============================================================================!
  program Main
!------------------------------------------------------------------------------!
  use Full_Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer, parameter     :: N = 10000
  integer                :: i, j, iter
  type(Full_Matrix_Type) :: A, B, C
!==============================================================================!

  ! Allocate matrices
  call A % Allocate_Full_Matrix(N)
  call B % Allocate_Full_Matrix(N)
  call C % Allocate_Full_Matrix(N)

  ! Initialize matrices
  A % val(:,:) = 1.0
  B % val(:,:) = 2.0
  C % val(:,:) = 0.0

  ! Transfer the basic data members from the matrix structure.  One can not
  ! transfer the derived data type with OpenACC, as it is unaware of it.
  call copy_to_device   (N, A % val)
  call copy_to_device   (N, B % val)
  call create_on_device (N, C % val)

  ! Perform computations on device, using only "!$acc kernels" and
  ! "!$acc end kernels" statement to avoid redundant copying of
  ! data back and forth.
  call compute_on_device(N, A % val, B % val, C % val)

  ! Retrieve results from the device
  call copy_from_device (N, C % val)

  ! Print result
  print *, 'Matrix c(1,1):', C % val(1,1)
  print *, 'Matrix c(n,n):', C % val(N,N)

  end program

