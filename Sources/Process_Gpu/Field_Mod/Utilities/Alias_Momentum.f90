!==============================================================================!
  subroutine Alias_Momentum(Flow, u, v, w)
!------------------------------------------------------------------------------!
!   Creates aliases for velocity components.                                   !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Field_Type), target :: Flow
  type(Var_Type),   pointer :: u, v, w
!==============================================================================!

  u => Flow % u
  v => Flow % v
  w => Flow % w

  end subroutine