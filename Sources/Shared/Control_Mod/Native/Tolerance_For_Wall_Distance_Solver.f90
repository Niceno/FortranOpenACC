!==============================================================================!
  subroutine Tolerance_For_Wall_Distance_Solver(Control, val, verbose)
!------------------------------------------------------------------------------!
!>  Reads linear solver tolerance for wall distance from the control file.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Control_Type) :: Control  !! parent class
  real,   intent(out) :: val      !! tolerance
  logical,   optional :: verbose  !! controls output verbosity
!==============================================================================!

  call Control % Read_Real_Item('TOLERANCE_FOR_WALL_DISTANCE_SOLVER',  &
                                 1.0e-6, val, verbose)

  end subroutine