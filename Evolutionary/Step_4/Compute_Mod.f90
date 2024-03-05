!==============================================================================!
  module Compute_Mod
!------------------------------------------------------------------------------!
!   The module containing only one procedure, a procedure to add two dense     !
!   matrices, defined as basic Fortran's two-dimensional arrays.  In other     !
!   words: it does not accept the derived data type Dense_Type.                !
!                                                                              !
!   The module also introduces the singleton object of the type Compute_Type   !
!   called "Global", indicating that it is globally accessible, that its only  !
!   procedure can be called from anywhere.                                     !
!----------------------------------[Modules]-----------------------------------!
  use Dense_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !------------------!
  !   Compute type   !
  !------------------!
  type Compute_Type

    contains
      procedure :: Compute_Mat_Add

  end type

  !-----------------------------------!
  !   Singletone type global object   !
  !-----------------------------------!
  type(Compute_Type) :: Global

  contains

# include "Compute_Mod/Mat_Add.f90"

  end module
