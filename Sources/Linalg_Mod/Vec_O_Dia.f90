!==============================================================================!
  subroutine Vec_O_Dia(Lin, C, A, B)
!------------------------------------------------------------------------------!
!>  Front-end for dividing a vector with sparse matrix's diagonal.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin  !! parent class
  type(Vector_Type)  :: C    !! result vector
  type(Matrix_Type)  :: A    !! operand matrix
  type(Vector_Type)  :: B    !! operand vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: nz
!==============================================================================!

  nz = A % nonzeros

  call Lin % Vec_O_Dia_Acc(C % len,  &
                           nz,       &
                           C % val,  &
                           A % val,  &
                           A % dia,  &
                           B % val)

  end subroutine

