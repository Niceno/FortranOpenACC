!==============================================================================!
  subroutine Compute_Spa_Vec_Mul(Comp, C, A, B)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Compute_Type) :: Comp
  type(Vector_Type)   :: C
  type(Sparse_Type)   :: A
  type(Vector_Type)   :: B
!-----------------------------------[Locals]-----------------------------------!
  integer :: n, nz
!==============================================================================!

  n  = C % len
  nz = A % nonzeros

  call Comp % Compute_Spa_Vec_Mul_Raw(n,        &
                                      nz,       &
                                      C % val,  &
                                      A % val,  &
                                      A % col,  &
                                      A % row,  &
                                      B % val)

  end subroutine

