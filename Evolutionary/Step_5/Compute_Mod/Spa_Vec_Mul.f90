!==============================================================================!
  subroutine Compute_Spa_Vec_Mul(Comp, C, A, B)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of sparse-matrix vector multiplication.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Compute_Type) :: Comp  !! parent class
  type(Vector_Type)   :: C     !! result vector
  type(Sparse_Type)   :: A     !! operand matrix
  type(Vector_Type)   :: B     !! operand vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: n, nz
!==============================================================================!

  n  = C % len
  nz = A % nonzeros

  call Comp % Compute_Spa_Vec_Mul_Acc(n,        &
                                      nz,       &
                                      C % val,  &
                                      A % val,  &
                                      A % col,  &
                                      A % row,  &
                                      B % val)

  end subroutine

