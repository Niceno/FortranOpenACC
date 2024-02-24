!==============================================================================!
  subroutine Create_Matrix_From_Matrix(A, B)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Matrix_Type)        :: A
  type(Matrix_Type), target :: B
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type), pointer :: Grid
!==============================================================================!

  ! Fetch the alias
  Grid => B % pnt_grid

  ! Store pointer to the grid
  A % pnt_grid => Grid

  print '(a)', ' # Creating a sparse matrix from another matrix'

  ! Copy all significant fields from the matrix
  A % nonzeros = B % nonzeros
  allocate (A % val(A % nonzeros));       A % val(:)   = B % val(:)
  allocate (A % col(A % nonzeros));       A % col(:)   = B % col(:)
  allocate (A % row(Grid % n_cells+1));   A % row(:)   = B % row(:)
  allocate (A % dia(Grid % n_cells));     A % dia(:)   = B % dia(:)
  allocate (A % mir(A % nonzeros));       A % mir(:)   = B % mir(:)
  allocate (A % pos(2, Grid % n_faces));  A % pos(:,:) = B % pos(:,:)
  allocate (A % d_inv(Grid % n_cells));   A % d_inv(:) = B % d_inv(:)

  end subroutine
