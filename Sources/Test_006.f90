!==============================================================================!
  subroutine Test_006
!------------------------------------------------------------------------------!
!>  Tests towards gradient calculation
!------------------------------------------------------------------------------!
  use Field_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Grid_Type)   :: G     !! computational grid
  type(Matrix_Type) :: A     !! system matrix
  type(Field_Type)  :: F     !! flow field
  integer           :: n, nx, ny, nz
  real              :: ts, te
!==============================================================================!

  nx  = 101
  ny  = 101
  nz  = 101
  n   = nx * ny * nz

  print '(a)',        ' #----------------------------------------------------'
  print '(a)',        ' # TEST 6: Creating a flow field and gradient matrix'
  print '(a, i12)',   ' #         The problem size is set to: ', n
  print '(a)',        ' #-----------------------------------------------------'

  print '(a)', ' # Creating a grid'
  call G % Create_Grid(1.0, 1.0, 1.0, nx, ny, nz)

  call A % Create_Matrix(G, singular=.false.)

  print '(a)', ' # Creating a field'
  call F % Create_Field(A)

  print '(a)', ' # Calculating gradient matrix for the field'
  call F % Calculate_Grad_Matrix()

  print '(a,f12.3,a)', ' # Time elapsed for TEST 6: ', te-ts, ' [s]'

  end subroutine
