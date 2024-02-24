#include "Unused.h90"

!==============================================================================!
  module Gpu_Mod
!----------------------------------[Modules]-----------------------------------!
  use Const_Mod
  use Native_Mod
  use Field_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !--------------!
  !              !
  !   GPU type   !
  !              !
  !--------------!
  type Gpu_Type

    real :: gb_used = 0.0

    contains

      ! Procedures to copy vectors to device
      procedure :: Vector_Copy_To_Device
      procedure :: Vector_Create_On_Device
      procedure :: Vector_Destroy_On_Device
      procedure :: Vector_Update_Device
      procedure :: Vector_Update_Host

      ! Procedures to copy matrices to device
      procedure :: Matrix_Copy_To_Device
      procedure :: Matrix_Destroy_On_Device

      ! Procedures to copy native solver to device
      procedure :: Native_Transfer_To_Device
      procedure :: Native_Destroy_On_Device

      ! Procedures to copy portions of grid to device
      procedure :: Grid_Cell_Cell_Connectivity_Copy_To_Device
      procedure :: Grid_Cell_Cell_Connectivity_Destroy_On_Device
      procedure :: Grid_Cell_Coordinates_Copy_To_Device
      procedure :: Grid_Cell_Coordinates_Destroy_On_Device

      procedure :: Field_Grad_Matrix_Copy_To_Device
      procedure :: Field_Grad_Matrix_Destroy_On_Device

  end type

  type(Gpu_Type) :: Gpu

  contains

    ! Procedures to copy vectors to device
#   include "Gpu_Mod/Vector/Copy_To_Device.f90"
#   include "Gpu_Mod/Vector/Create_On_Device.f90"
#   include "Gpu_Mod/Vector/Destroy_On_Device.f90"
#   include "Gpu_Mod/Vector/Update_Device.f90"
#   include "Gpu_Mod/Vector/Update_Host.f90"

    ! Procedures to copy vectors to device
#   include "Gpu_Mod/Matrix/Copy_To_Device.f90"
#   include "Gpu_Mod/Matrix/Destroy_On_Device.f90"

    ! Procedures to copy native solver to device
#   include "Gpu_Mod/Native/Transfer_To_Device.f90"
#   include "Gpu_Mod/Native/Destroy_On_Device.f90"

    ! Procedures to copy portions of grid to device
#   include "Gpu_Mod/Grid/Cell_Cell_Connectivity_Copy_To_Device.f90"
#   include "Gpu_Mod/Grid/Cell_Cell_Connectivity_Destroy_On_Device.f90"
#   include "Gpu_Mod/Grid/Cell_Coordinates_Copy_To_Device.f90"
#   include "Gpu_Mod/Grid/Cell_Coordinates_Destroy_On_Device.f90"

#   include "Gpu_Mod/Field/Grad_Matrix_Copy_To_Device.f90"
#   include "Gpu_Mod/Field/Grad_Matrix_Destroy_On_Device.f90"

  end module
