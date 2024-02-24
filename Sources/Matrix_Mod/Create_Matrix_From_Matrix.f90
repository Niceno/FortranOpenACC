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

  ! Establish symbolic links to connectivity
  A % pnt_grid => Conn % pnt_grid
  A % n        => Conn % n
  A % nonzeros => Conn % nonzeros
  A % row      => Conn % row
  A % col      => Conn % col
  A % dia      => Conn % dia
  A % mir      => Conn % mir
  A % pos      => Conn % pos

  ! Make physical coppies of the real fields
  allocate (A % val(A % nonzeros));       A % val(:)   = B % val(:)
  allocate (A % d_inv(Grid % n_cells));   A % d_inv(:) = B % d_inv(:)

  end subroutine
