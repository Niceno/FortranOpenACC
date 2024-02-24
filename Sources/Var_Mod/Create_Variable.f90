!==============================================================================!
  subroutine Var_Mod_Create_Variable(phi, Grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Var_Type)          :: phi        !! variable object being created
  type(Grid_Type), target :: Grid       !! grid on which it is defined
!==============================================================================!

  ! Store Grid for which the variable is defined
  phi % pnt_grid => Grid

  ! Values in the new (n) and the old (o) time step
  allocate (phi % n(-Grid % n_bnd_cells:Grid % n_cells));  phi % n = 0.0
  allocate (phi % o(-Grid % n_bnd_cells:Grid % n_cells));  phi % o = 0.0

  end subroutine
