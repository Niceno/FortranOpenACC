!==============================================================================!
  logical function Test_Array_Int(Mem, a, i)
!------------------------------------------------------------------------------!
!>  Checks if index i is within bounds of integer array a
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Memory_Type),   intent(in) :: Mem   !! parent class
  integer, allocatable, intent(in) :: a(:)  !! opearand array
  integer,              intent(in) :: i     !! array index
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Mem)
!==============================================================================!

  Test_Array_Int = (i >= lbound(a, 1) .and. i <= ubound(a, 1))

  end function