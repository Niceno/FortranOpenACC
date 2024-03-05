!==============================================================================!
  subroutine Dense_Allocate(A, n)
!------------------------------------------------------------------------------!
!   Allocates memory for the data member "val" and stores dense matrix's size, !
!   (a.k.a. matrix dimension) into data member "n".                            !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Dense_Type)   :: A
  integer, intent(in) :: n
!==============================================================================!

  ! Store the dimension of the matrix
  A % len = n

  ! Allocate the memory
  allocate(A % val(n, n))

  end subroutine
