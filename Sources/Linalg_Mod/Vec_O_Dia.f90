!==============================================================================!
  subroutine Vec_O_Dia(Lin, c, A, b)
!------------------------------------------------------------------------------!
!>  Front-end for dividing a vector with sparse matrix's diagonal.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin   !! parent class
  real               :: c(:)  !! result vector
  type(Matrix_Type)  :: A     !! operand matrix
  real               :: b(:)  !! operand vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: n, nz
!==============================================================================!

  n  = size(c)
  nz = A % nonzeros

  call Lin % Vec_O_Dia_Acc(n,        &
                           nz,       &
                           c,        &
                           A % val,  &
                           A % dia,  &
                           b)

  end subroutine

