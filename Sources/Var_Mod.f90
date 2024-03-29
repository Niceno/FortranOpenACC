#include "Assert.h90"
#include "Unused.h90"

!==============================================================================!
  module Var_Mod
!------------------------------------------------------------------------------!
  use Grid_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!

  !--------------!
  !   Var type   !
  !--------------!
  type Var_Type

    type(Grid_Type), pointer :: pnt_grid    !! the grid on which it's defined

    real, allocatable :: n(:)   !! new value (current time step)
    real, allocatable :: o(:)   !! old value (previous time step)

    real, allocatable :: x(:)   !! gradient in x direction
    real, allocatable :: y(:)   !! gradient in x direction
    real, allocatable :: z(:)   !! gradient in x direction

    real :: tol = PICO  !! linear solver tolerance
  end type

  contains

#   include "Var_Mod/Create_Variable.f90"

  end module
