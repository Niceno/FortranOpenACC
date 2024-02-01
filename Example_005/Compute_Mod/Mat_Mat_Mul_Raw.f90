!==============================================================================!
  subroutine Compute_Mat_Mat_Mul_Raw(Comp, n, c, a, b)
!------------------------------------------------------------------------------!
    implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Compute_Type)  :: Comp
  integer              :: n
  real, dimension(n,n) :: c, a, b
!-----------------------------------[Locals]-----------------------------------!
  integer :: iter, i, j
!==============================================================================!

  !$acc data present(c, a, b)
  !$acc kernels
  do iter = 1, 60
    do j = 1, n
      do i = 1, n
        c(i,j) = a(i,j) * b(i,j)
      end do
    end do
  end do
  !$acc end kernels
  !$acc end data

  end subroutine

