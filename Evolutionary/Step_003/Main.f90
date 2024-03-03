!------------------------------------------------------------------------------!
!   Described in the readme.md file.  Compiled with the script compiled.sh.    !
!------------------------------------------------------------------------------!

!==============================================================================!
  module Dense_Mod
!------------------------------------------------------------------------------!
!   Introduces a more complex data type to be transferred to and back from     !
!   the "device".  It does not have member function which do the transfer,     !
!   the transfer is still done from the main function.                         !
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !-----------------------!
  !   Dense matrix type   !
  !-----------------------!
  type Dense_Type

    integer           :: len
    real, allocatable :: val(:,:)

    contains
      procedure :: Allocate_Dense

  end type

  contains

!==============================================================================!
  subroutine Allocate_Dense(A, n)
!------------------------------------------------------------------------------!
!   Allocates memory for the data member "val" and stores its size, matrix     !
!   dimension in this case, into data member "n".                              !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Dense_Type) :: A
  integer, intent(in)     :: n
!==============================================================================!

  ! Store the dimension fo the matrix
  A % len = n

  ! Allocate the memory
  allocate(A % val(n, n))

  end subroutine

  end module

!==============================================================================!
  subroutine Copy_To_Device(n, c)
!------------------------------------------------------------------------------!
!   This subroutines creates (allocates) memory on "device" for varible c, and !
!   it also coppies its contents to the device.  It is useful for data which   !
!   are operands (as opposed to results) in the computations performed on the  !
!   "device".                                                                  !
!                                                                              !
!   Subroutine related to this one is "Create_On_Device" which allocates       !
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
  subroutine Copy_From_Device(n, c)
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
  subroutine Compute_On_Device(n, a, b, c)
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
  use Dense_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer, parameter :: N = 10000
  integer            :: i, j, iter
  type(Dense_Type)   :: A, B, C
!==============================================================================!

  ! Allocate matrices
  call A % Allocate_Dense(N)
  call B % Allocate_Dense(N)
  call C % Allocate_Dense(N)

  ! Initialize matrices
  A % val(:,:) = 1.0
  B % val(:,:) = 2.0
  C % val(:,:) = 0.0

  ! Transfer the basic data members from the matrix structure.  One can not
  ! transfer the derived data type with OpenACC, as it is unaware of it.
  call Copy_To_Device  (N, A % val)
  call Copy_To_Device  (N, B % val)
  call Create_On_Device(N, C % val)

  ! Perform computations on device, using only "!$acc kernels" and
  ! "!$acc end kernels" statement to avoid redundant copying of
  ! data back and forth.
  call Compute_On_Device(N, A % val, B % val, C % val)

  ! Retrieve results from the device
  call Copy_From_Device (N, C % val)

  ! Print result
  print *, 'Matrix c(1,1):', C % val(1,1)
  print *, 'Matrix c(n,n):', C % val(N,N)

  end program

