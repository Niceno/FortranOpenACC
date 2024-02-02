!==============================================================================!
  subroutine Compute_Spa_Vec_Mul_Raw(Comp, n, nz, c, a_val, a_col, a_row, b)
!------------------------------------------------------------------------------!
    implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Compute_Type)     :: Comp
  integer, intent(in)     :: n
  integer, intent(in)     :: nz
  real,    dimension(n)   :: c
  real,    dimension(nz)  :: a_val
  integer, dimension(nz)  :: a_col
  integer, dimension(n+1) :: a_row
  real,    dimension(n)   :: b
!-----------------------------------[Locals]-----------------------------------!
  integer :: iter, i, j, ij
  real    :: temp
!==============================================================================!

  !$acc data present(c, a_val, a_col, a_row, b)
  !$acc parallel loop
  do i = 1, n
    temp = 0.0

    !$acc loop reduction(+:temp)
    do ij = a_row(i), a_row(i+1) - 1
      j = a_col(ij)
      temp = temp + a_val(ij) * b(j)
    end do
    c(i) = temp
  end do
  !$acc end data

  end subroutine

