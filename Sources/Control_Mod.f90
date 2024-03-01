#include "Assert.h90"
#include "Unused.h90"

!==============================================================================!
  module Control_Mod
!----------------------------------[Modules]-----------------------------------!
  use Const_Mod
  use Field_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !------------------!
  !   Control type   !
  !------------------!
  type Control_Type

    integer :: time_steps = 1200
    real    :: time_step  =    0.01

    integer :: max_simple = 6
    integer :: min_simple = 3

    integer :: save_int   =  120  !! result save interval

    real :: blend = 0.5  !! blending coefficent for advection

    contains
      procedure :: Load_Control

  end type

  ! Singleton control object
  type(Control_Type) :: Control

  contains

#   include "Control_Mod/Load_Control.f90"

  end module
