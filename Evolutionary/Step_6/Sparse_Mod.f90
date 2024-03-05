!==============================================================================!
  module Sparse_Mod
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

  !-----------------!
  !                 !
  !   Sparse Type   !
  !                 !
  !-----------------!
  type Sparse_Type
    integer              :: n = 0     ! matrix dimension
    integer              :: nonzeros  ! number of nonzero entries
    real,    allocatable :: val(:)    ! value
    integer, allocatable :: col(:)    ! beginning of each row
    integer, allocatable :: row(:)    ! column positions
    integer, allocatable :: dia(:)    ! diagonal positions 
    integer, allocatable :: mir(:)    ! position of the mirror entry

    contains
      procedure :: Create_Sparse
      procedure :: Copy_Sparse_To_Device
      procedure :: Copy_Sparse_To_Host
      procedure :: Destroy_Sparse_On_Device

  end type

  contains
#   include "Sparse_Mod/Create_Sparse.f90"
#   include "Sparse_Mod/Copy_Sparse_To_Device.f90"
#   include "Sparse_Mod/Copy_Sparse_To_Host.f90"
#   include "Sparse_Mod/Destroy_Sparse_On_Device.f90"

  end module
