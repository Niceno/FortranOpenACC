!==============================================================================!
  subroutine Compute_Den_Vec_Mul_Acc(Comp, n, c, a, b)
!------------------------------------------------------------------------------!
!>  This subroutine computes dense-matrix vector multiplication on
!>  a device without checking if variables are present on the device.
!------------------------------------------------------------------------------!
!   Note:                                                                      !
!                                                                              !
!   * This subroutine used to have directives:                                 !
!     !$acc data present(c, a, b)                                              !
!     !$acc end data                                                           !
!     around the loop, but there was a problem with that.  The "end data"      !
!     would destroy data on the device which I don't want in an iterative      !
!     procedure, and "data present" couldn't hang here without "end data"      !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Compute_Type)  :: Comp  !! parent class
  integer              :: n     !! matrix and vector dimension
  real, dimension(n)   :: c     !! result vector
  real, dimension(n,n) :: a     !! operand matrix
  real, dimension(n)   :: b     !! operand vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j
  real    :: temp
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Comp)
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

