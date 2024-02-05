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
  type(Field_Type)  :: F     !! flow field
  integer           :: nx, ny, nz
  real              :: ts, te
!==============================================================================!

  nx  = 101
  ny  = 101
  nz  = 101

  print '(a)',        ' #----------------------------------------------------'
  print '(a)',        ' # TEST 6: Creating a flow field and gradient matrix'
  print '(a, i12)',   ' #         The problem size is set to: ', nx * ny * nz
  print '(a)',        ' #-----------------------------------------------------'

  print '(a)', ' # Creating a grid'
  call Grid % Create_Grid(1.0, 1.0, 1.0, nx, ny, nz)

  call A % Create_Matrix(Grid, singular=.false.)

  print '(a)', ' # Creating a field'
  call F % Create_Field(A)

  print '(a)', ' # Calculating gradient matrix for the field'
  call F % Calculate_Grad_Matrix()

  print '(a,f12.3,a)', ' # Time elapsed for TEST 5: ', te-ts, ' [s]'

  end subroutine
