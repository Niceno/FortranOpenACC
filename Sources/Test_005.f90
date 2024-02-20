!==============================================================================!
  subroutine Test_005
!------------------------------------------------------------------------------!
!>  Tests towards gradient calculation
!------------------------------------------------------------------------------!
  use Field_Mod
  use Gpu_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Grid_Type)   :: Grid  !! computational grid
  type(Matrix_Type) :: A     !! system matrix
  real, allocatable :: b(:)  !! right hand side
  type(Field_Type)  :: F     !! flow field
  real              :: ts, te
  integer           :: n
!==============================================================================!

  print '(a)',        ' #----------------------------------------------------'
  print '(a)',        ' # TEST 5: Creating a flow field and gradient matrix'
  print '(a)',        ' #-----------------------------------------------------'

  print '(a)', ' # Creating a grid'
  call Grid % Load_Grid("101_cube.ini")

  n = Grid % n_cells
  print '(a, i12)',   ' # The problem size is: ', n

  call A % Create_Matrix(Grid, b)

  print '(a)', ' # Creating a field'
  call F % Create_Field(Grid)

  print '(a)', ' # Calculating gradient matrix for the field'
  call F % Calculate_Grad_Matrix()

  print '(a,f12.3,a)', ' # Time elapsed for TEST 5: ', te-ts, ' [s]'

  end subroutine
