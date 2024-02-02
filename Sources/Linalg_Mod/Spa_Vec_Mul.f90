!==============================================================================!
  subroutine Linalg_Spa_Vec_Mul(Lin, C, A, B)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of sparse-matrix vector multiplication.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin  !! parent class
  type(Vector_Type)  :: C    !! result vector
  type(Sparse_Type)  :: A    !! operand matrix
  type(Vector_Type)  :: B    !! operand vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: nz
!==============================================================================!

  nz = A % nonzeros

  call Lin % Linalg_Spa_Vec_Mul_Raw(C % len,  &
                                    nz,       &
                                    C % val,  &
                                    A % val,  &
                                    A % col,  &
                                    A % row,  &
                                    B % val)

  end subroutine

