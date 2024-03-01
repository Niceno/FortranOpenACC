#include "Assert.h90"

!==============================================================================!
  module Field_Mod
!----------------------------------[Modules]-----------------------------------!
  use Assert_Mod
  use Sparse_Mod
  use Native_Mod
  use Var_mod
  use Gpu_mod
  use Profiler_mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !----------------!
  !   Field type   !
  !----------------!
  type Field_Type

    type(Sparse_Type), pointer :: pnt_matrix  !! pointer to the matrix
    type(Grid_Type),   pointer :: pnt_grid    !! grid for which it is defined

    type(Native_Type) :: Nat

    ! Three velocity components, new and old
    type(Var_Type) :: u
    type(Var_Type) :: v
    type(Var_Type) :: w

    ! Pressure variable
    type(Var_Type) :: pp  ! pressure correction term
    type(Var_Type) :: p   ! pressure

    ! Volume flux through faces
    real, allocatable :: v_flux(:)

    ! Time step
    real :: dt = 0.01

    ! Relevant physical properties
    real :: density   = 1.0    !! [kg / m^3]
    real :: viscosity = 0.001  !! [kg / (m s)]

    ! Pressure drops
    real :: p_drop_x = 0.0  !! [kg/(m^2 s^2)]
    real :: p_drop_y = 0.0  !! [kg/(m^2 s^2)]
    real :: p_drop_z = 0.0  !! [kg/(m^2 s^2)]

    ! Gradient matrices for cells to cells (c2c)
    real, allocatable :: grad_c2c(:,:)  !! gradient matrices [1/m^2]

    ! Some numerical parameters
    real :: blend  !! bleding coefficient for momentum

    contains

      procedure :: Create_Field
      procedure :: Calculate_Grad_Matrix
      procedure :: Grad_Component
      procedure :: Grad_Pressure
      procedure :: Grad_Pressure_Obstacle

  end type

  contains
#   include "Field_Mod/Create_Field.f90"
#   include "Field_Mod/Calculate_Grad_Matrix.f90"
#   include "Field_Mod/Grad_Component.f90"
#   include "Field_Mod/Grad_Pressure.f90"
#   include "Field_Mod/Grad_Pressure_Obstacle.f90"

  end module
