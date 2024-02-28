!==============================================================================!
  module Const_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  ! Standard string lengths
  integer, parameter :: VL =   4  !! variable name length
  integer, parameter :: SL =  80  !! standard string length (like page width)
  integer, parameter :: DL = 160  !! double string length (twice the page width)

  ! Double and single precision constants definitions
  integer, parameter :: DP = 8  !! double precisions for real and long integer
  integer, parameter :: SP = 4  !! single precisions for real and short integer

  ! This is used when estimating how much memory is used on GPUs
  integer, parameter :: GIGABYTE = 1024**3  !! number of bytes in a giga byte

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

  real, parameter :: TINY = FEMTO

  end module
