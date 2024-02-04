!==============================================================================!
  subroutine Vec_O_Dia_Acc(Lin, n, nz, c, a_val, a_dia, b)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin        !! parent class
  integer            :: n          !! matrix and vector dimension
  integer            :: nz         !! number of nonzeros
  real               :: c(n)       !! result vector
  real               :: a_val(nz)  !! operand matrix values
  integer            :: a_dia(n)   !! operand matrix diagonal positions
  real               :: b(n)       !! operand vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, k
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Lin)
!==============================================================================!

  !$acc kernels
  do i = 1, n
    k = a_dia(i)
    c(i) = b(i) / a_val(k)
  end do
  !$acc end kernels

  end subroutine

