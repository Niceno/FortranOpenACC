#include "Assert.h90"

!==============================================================================!
  module Field_Mod
!----------------------------------[Modules]-----------------------------------!
  use Assert_Mod
  use Matrix_Mod
  use Native_Mod
  use Var_mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !----------------!
  !   Field type   !
  !----------------!
  type Field_Type

    type(Matrix_Type), pointer :: pnt_matrix  !! pointer to the matrix
    type(Grid_Type),   pointer :: pnt_grid    !! grid for which it is defined

    type(Native_Type) :: Nat

    ! Three velocity components, new and old
    type(Var_Type) :: u
    type(Var_Type) :: v
    type(Var_Type) :: w

    ! Pressure variable
    type(Var_Type) :: p

    ! Volume flux through faces
    real, allocatable :: v_flux(:)

    ! Gradient matrices for cells to cells (c2c)
    real, allocatable :: grad_c2c(:,:)  !! gradient matrices [1/m^2]

    contains

      procedure :: Create_Field
      procedure :: Calculate_Grad_Matrix
      procedure :: Grad_Component

  end type

  contains
#   include "Field_Mod/Create_Field.f90"
#   include "Field_Mod/Calculate_Grad_Matrix.f90"
#   include "Field_Mod/Grad_Component.f90"

  end module
