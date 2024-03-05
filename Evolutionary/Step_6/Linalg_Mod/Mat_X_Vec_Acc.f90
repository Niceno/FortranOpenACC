!==============================================================================!
  subroutine Mat_X_Vec_Acc(Lin, n, c, a, b)
!------------------------------------------------------------------------------!
!>  This subroutine computes dense-matrix vector multiplication on
!>  a device without checking if variables are present on the device.
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
  integer            :: n       !! matrix and vector dimension
  real               :: c(n)    !! result vector
  real               :: a(n,n)  !! operand matrix
  real               :: b(n)    !! operand vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j
  real    :: temp
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Lin)
!==============================================================================!

  !$acc kernels
  do j = 1, n
    temp = 0.0
    do i = 1, n
      temp = temp + a(j, i) * b(i)
    end do
    c(j) = temp
  end do
  !$acc end kernels

  end subroutine

