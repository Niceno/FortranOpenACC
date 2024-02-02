!==============================================================================!
  subroutine Compute_Mat_Add(Comp, n, a, b, c)
!------------------------------------------------------------------------------!
    implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Compute_Type)  :: Comp
  integer              :: n
  real, dimension(n,n) :: a, b, c
!-----------------------------------[Locals]-----------------------------------!
  integer :: iter, i, j
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

