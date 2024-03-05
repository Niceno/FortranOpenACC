!==============================================================================!
  subroutine Den_P_Den_Acc(Lin, n, c, a, b)
!------------------------------------------------------------------------------!
!>  This subroutine computes dense-dense addition on a device,
!>  without checking if variables are present on the device.
!------------------------------------------------------------------------------!
!   Notes:                                                                     !
!                                                                              !
!   * This subroutine used to have directives:                                 !
!     !$acc data present(c, a, b)                                              !
!     ...                                                                      !
!     !$acc end data                                                           !
!     around the loop, but there was a problem with that.  The "end data"      !
!     would destroy data on the device which I don't want in an iterative      !
!     procedure, and "data present" couldn't hang here without "end data"      !
!                                                                              !
!   * Using intent clause here, was causing slower runs.                       !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin     !! parent class
  integer            :: n       !! matrix dimensions
  real               :: c(n,n)  !! result matrix
  real               :: a(n,n)  !! operand matrix
  real               :: b(n,n)  !! operand matrix
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Lin)
!==============================================================================!

  !$acc kernels
  do j = 1, n
    do i = 1, n
      c(i,j) = a(i,j) + b(i,j)
    end do
  end do
  !$acc end kernels

  end subroutine

