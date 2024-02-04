!==============================================================================!
  module Gpu_Mod
!----------------------------------[Modules]-----------------------------------!
  use Native_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !--------------!
  !              !
  !   GPU type   !
  !              !
  !--------------!
  type Gpu_Type

    contains

      ! Procedures to copy vectors to device
      procedure :: Vector_Copy_To_Device
      procedure :: Vector_Copy_To_Host
      procedure :: Vector_Create_On_Device
      procedure :: Vector_Destroy_On_Device

      ! Procedures to copy matrices to device
      procedure :: Matrix_Copy_To_Device
      procedure :: Matrix_Destroy_On_Device

      ! Procedures to copy native solver to device
      procedure :: Native_Create_On_Device
      procedure :: Native_Destroy_On_Device

  end type

  type(Gpu_Type) :: Gpu

  contains

    ! Procedures to copy vectors to device
#   include "Gpu_Mod/Vector/Copy_To_Device.f90"
#   include "Gpu_Mod/Vector/Copy_To_Host.f90"
#   include "Gpu_Mod/Vector/Create_On_Device.f90"
#   include "Gpu_Mod/Vector/Destroy_On_Device.f90"

    ! Procedures to copy vectors to device
#   include "Gpu_Mod/Matrix/Copy_To_Device.f90"
#   include "Gpu_Mod/Matrix/Destroy_On_Device.f90"

    ! Procedures to copy native solver to device
#   include "Gpu_Mod/Native/Create_On_Device.f90"
#   include "Gpu_Mod/Native/Destroy_On_Device.f90"

  end module
