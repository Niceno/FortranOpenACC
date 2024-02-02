!==============================================================================!
  subroutine Full_Matrix_Allocate(A, n)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Full_Matrix_Type) :: A
  integer, intent(in)     :: n
!==============================================================================!

  ! Store the length
  A % len = n

  ! Allocate the memory
  allocate(A % val(n, n))

  end subroutine
