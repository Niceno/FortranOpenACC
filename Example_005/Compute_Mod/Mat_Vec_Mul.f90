!==============================================================================!
  subroutine Compute_Mat_Vec_Mul(Comp, n, c, a, b)
!------------------------------------------------------------------------------!
    implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Compute_Type)  :: Comp
  integer              :: n
  real, dimension(n)   :: c
  real, dimension(n,n) :: a
  real, dimension(n)   :: b
!-----------------------------------[Locals]-----------------------------------!
  integer :: iter, i, j
  real    :: temp
!==============================================================================!

  !$acc data present(c, a, b)
  !$acc kernels
  do j = 1, n
    temp = 0.0
    do i = 1, n
      temp = temp + a(j, i) * b(i)
    end do
    c(j) = temp
  end do
  !$acc end kernels
  !$acc end data

  end subroutine

