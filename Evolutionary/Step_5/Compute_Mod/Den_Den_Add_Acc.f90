!==============================================================================!
  subroutine Compute_Den_Den_Add_Acc(Comp, n, c, a, b)
!------------------------------------------------------------------------------!
!>  This subroutine computes dense-matrix dense-matrix addition on a device,
!>  without checking if variables are present on the device.
!------------------------------------------------------------------------------!
!   Note:                                                                      !
!                                                                              !
!   * This subroutine used to have directives:                                 !
!     acc data present(c, a, b)                                                !
!     acc end data                                                             !
!     around the loop, but there was a problem with that.  The "end data"      !
!     would destroy data on the device which I don't want in an iterative      !
!     procedure, and "data present" couldn't hang here without "end data"      !
!------------------------------------------------------------------------------!
    implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Compute_Type)  :: Comp  !! parent class
  integer, intent(in)  :: n     !! matrix dimensions
  real, dimension(n,n) :: c     !! result matrix
  real, dimension(n,n) :: a, b  !! operand matrices
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Comp)
!==============================================================================!

  !$acc kernels
  do j = 1, n
    do i = 1, n
      c(i,j) = a(i,j) + b(i,j)
    end do
  end do
  !$acc end kernels

  end subroutine

