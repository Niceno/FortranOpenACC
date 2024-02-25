!==============================================================================!
  module Const_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  ! Standard string length
  integer, parameter :: GIGABYTE = 1024**3

  ! Big and small numbers in metric system to avoid ghost numbers
  real, parameter :: YOTTA = 1.e+24  !! avoid ghost number 1.0e+24
  real, parameter :: ZETTA = 1.e+21  !! avoid ghost number 1.0e+21
  real, parameter :: EXA   = 1.e+18  !! avoid ghost number 1.0e+18
  real, parameter :: PETA  = 1.e+15  !! avoid ghost number 1.0e+15
  real, parameter :: TERA  = 1.e+12  !! avoid ghost number 1.0e+12
  real, parameter :: GIGA  = 1.e+9   !! avoid ghost number 1.0e+9
  real, parameter :: MEGA  = 1.e+6   !! avoid ghost number 1.0e+6
  real, parameter :: KILO  = 1.e+3   !! avoid ghost number 1.0e+3
  real, parameter :: MILI  = 1.e-3   !! avoid ghost number 1.0e-3
  real, parameter :: MICRO = 1.e-6   !! avoid ghost number 1.0e-6
  real, parameter :: NANO  = 1.e-9   !! avoid ghost number 1.0e-9
  real, parameter :: PICO  = 1.e-12  !! avoid ghost number 1.0e-12
  real, parameter :: FEMTO = 1.e-15  !! avoid ghost number 1.0e-15
  real, parameter :: ATTO  = 1.e-18  !! avoid ghost number 1.0e-18
  real, parameter :: ZEPTO = 1.e-21  !! avoid ghost number 1.0e-21
  real, parameter :: YOCTO = 1.e-24  !! avoid ghost number 1.0e-24

  end module
