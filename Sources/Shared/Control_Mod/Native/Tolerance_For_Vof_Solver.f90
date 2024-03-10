!==============================================================================!
  subroutine Tolerance_For_Vof_Solver(Control, val, verbose)
!------------------------------------------------------------------------------!
!>  Reads linear solver tolerance for VOF function from the control file.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Control_Type) :: Control  !! parent class
  real,   intent(out) :: val      !! tolerance
  logical,   optional :: verbose  !! controls output verbosity
!==============================================================================!

  call Control % Read_Real_Item('TOLERANCE_FOR_VOF_SOLVER',  &
                                 1.0e-6, val, verbose)

  end subroutine