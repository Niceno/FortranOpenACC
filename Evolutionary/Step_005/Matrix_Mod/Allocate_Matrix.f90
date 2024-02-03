!==============================================================================!
  subroutine Allocate_Matrix(A, n)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Matrix_Type)  :: A
  integer, intent(in) :: n
!==============================================================================!

  ! Store the length
  A % len = n

  ! Allocate the memory
  allocate(A % val(n, n))

  end subroutine