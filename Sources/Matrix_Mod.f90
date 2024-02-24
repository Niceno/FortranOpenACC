#include "Assert.h90"

!==============================================================================!
  module Matrix_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Grid_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   Sparse matrix type                                                         !
!                                                                              !
!   Sparse matrix is stored in compressed row format.                          !
!   (See: http://netlib.org/linalg/html_templates/node91.html)                 !
!                                                                              !
!   Example:                                                                   !
!                                                                              !
!       c   c  .    c                                                          !
!       o   o  .    o                                                          !
!       l   l       l                                                          !
!                                                                              !
!       1   2       n                                                          !
!                                                                              !
!    [ 10   0   4   5 ]  --> row 1                                             !
!    [  2  12  -1   0 ]  --> rows store discretized control volumes            !
!    [  0   1  99   7 ]  ...                                                   !
!    [ -3  11   0  53 ]  --> row n                                             !
!                                                                              !
!   Compressed row storage of the above matrix reads:                          !
!                                                                              !
!   A % val = [  10   4   5   2  12  -1   1  99   7  -3  11  53 ]              !
!   A % col = [   1   3   4   1   2   3   2   3   4   1   2   4 ]              !
!   A % row = [   1   4   7  10 ]                                              !
!                                                                              !
!   A % dia = [   1   5   9  12 ]                                              !
!==============================================================================!

  !-----------------------------!
  !                             !
  !   Connectivit Matrix Type   !
  !                             !
  !-----------------------------!
  type Connectivity_Matrix_Type

    type(Grid_Type), pointer :: pnt_grid  !! pointer to grid

    integer              :: n
    integer              :: nonzeros  !! number of nonzero entries
    integer, allocatable :: row(:)    !! column positions
    integer, allocatable :: col(:)    !! beginning of each row
    integer, allocatable :: dia(:)    !! diagonal positions
    integer, allocatable :: mir(:)    !! position of the mirror entry
    integer, allocatable :: pos(:,:)  !! face-based position in the matrix

  end type

  type(Connectivity_Matrix_Type), target :: Conn

  !-----------------!
  !                 !
  !   Matrix Type   !
  !                 !
  !-----------------!
  type Matrix_Type

    type(Grid_Type), pointer :: pnt_grid  !! pointer to grid

    integer,             pointer :: n
    integer,             pointer :: nonzeros  !! number of nonzero entries
    integer, contiguous, pointer :: row(:)    !! column positions
    integer, contiguous, pointer :: col(:)    !! beginning of each row
    integer, contiguous, pointer :: dia(:)    !! diagonal positions
    integer, contiguous, pointer :: mir(:)    !! position of the mirror entry
    integer, contiguous, pointer :: pos(:,:)  !! face-based position in matrix
    real,            allocatable :: val(:)    !! value
    real,            allocatable :: d_inv(:)  !! inverse dia. for preconditioner

    contains
      procedure :: Create_Matrix
      procedure :: Create_Matrix_From_Matrix

  end type

  contains
#   include "Matrix_Mod/Create_Matrix.f90"
#   include "Matrix_Mod/Create_Matrix_From_Matrix.f90"

  end module
