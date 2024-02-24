!==============================================================================!
  subroutine Create_Matrix(Q, Grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Matrix_Type)      :: Q       !! parent class
  type(Grid_Type), target :: Grid    !! grid on which it is created
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, ni, nj, nk, non_z
  integer :: c, w, e, s, n, b, t, c1, c2
  integer :: col_a, col_b, row_a, row_b, pos_a, pos_b
!==============================================================================!

  !---------------------------------------------!
  !                                             !
  !   Continue with connectivity matrix, only   !
  !    if its pointer to grid wasn't set yet    !
  !                                             !
  !---------------------------------------------!
  if(.not. associated(Conn % pnt_grid)) then
    Conn % pnt_grid => Grid
  
    print '(a)', ' # Creating a sparse matrix'
  
    ni = Grid % nx
    nj = Grid % ny
    nk = Grid % nz
  
    !------------------------------------------!
    !   Count non-zeroes and allocate memory   !
    !------------------------------------------!
    non_z = 0
  
    ! Browse in A way in which cell number "A" will increase one by one
    do k = 1, nk
      do j = 1, nj
        do i = 1, ni
          if(k > 1) non_z = non_z + 1
          if(j > 1) non_z = non_z + 1
          if(i > 1) non_z = non_z + 1
          non_z = non_z + 1
          if(i < ni) non_z = non_z + 1
          if(j < nj) non_z = non_z + 1
          if(k < nk) non_z = non_z + 1
        end do
      end do
    end do
  
    !-----------------------------------------------------------!
    !   Allocate memory for the singleton connectivity matrix   !
    !-----------------------------------------------------------!
    print '(a,i15)', ' # Number of nonzeros: ', non_z
    Conn % nonzeros = non_z
    Conn % n = ni * nj * nk
    Assert(Conn % n .eq. Grid % n_cells)
    allocate(Conn % row(Grid % n_cells+1));   Conn % row = 0
    allocate(Conn % col(non_z));              Conn % col = 0
    allocate(Conn % dia(Grid % n_cells));     Conn % dia = 0
    allocate(Conn % mir(non_z));              Conn % mir = 0
    Assert(Grid % n_faces .gt. 0)
    allocate(Conn % pos(2, Grid % n_faces));  Conn % pos = 0
  
    !--------------------------------!
    !   Form the compressed matrix   !
    !--------------------------------!
    non_z = 0
  
    ! Browse in A way in which cell number "A" will increase one by one
    do k = 1, nk
      do j = 1, nj
        do i = 1, ni
          c = Grid % Cell_Number(i, j, k)
  
          ! First neighbours
          b = c-ni*nj
          s = c-ni
          w = c-1
          e = c+1
          n = c+ni
          t = c+ni*nj
  
          ! Set row index
          Conn % row(c) = non_z + 1
  
          ! Set columns for neighbours
          if(k > 1) then        ! bottom
            non_z = non_z + 1
            Conn % col(non_z) = b
          end if
  
          if(j > 1) then        ! south
            non_z = non_z + 1
            Conn % col(non_z) = s
          end if
  
          if(i > 1) then        ! west
            non_z = non_z + 1
            Conn % col(non_z) = w
          end if
  
          non_z = non_z + 1     ! central
          Conn % col(non_z) = c
  
          if(i < ni) then       ! east
            non_z = non_z + 1
            Conn % col(non_z) = e
          end if
  
          if(j < nj) then       ! north
            non_z = non_z + 1
            Conn % col(non_z) = n
          end if
  
          if(k < nk) then       ! top
            non_z = non_z + 1
            Conn % col(non_z) = t
          end if
  
        end do
      end do
    end do
  
    ! Wrap it up
    Conn % row(ni*nj*nk+1) = non_z + 1
  
    !---------------------------------!
    !   Find positions of diagonals   !
    !---------------------------------!
    do row_a = 1, Grid % n_cells
      do pos_a = Conn % row(row_a), Conn % row(row_a + 1) - 1
        col_a = Conn % col(pos_a)  ! at this point you have row_a and col_a
        if(col_a == row_a) then
          Conn % dia(row_a) = pos_a
          goto 1
        end if
      end do
  1   continue
    end do
  
    !----------------------!
    !   Find it's mirror   !
    !----------------------!
    do row_a = 1, Grid % n_cells
      do pos_a = Conn % row(row_a), Conn % row(row_a + 1) - 1
        col_a = Conn % col(pos_a)  ! at this point you have row_a and col_a
  
        row_b = col_a
        do pos_b = Conn % row(row_b), Conn % row(row_b + 1) - 1
          col_b = Conn % col(pos_b)  ! at this point you have row_b and col_b
  
          if( (col_b == row_a) .and. (row_b == col_a) ) then
            Conn % mir(pos_a) = pos_b
            Conn % mir(pos_b) = pos_a
            goto 2  ! done with the inner loop, get out
          end if
        end do
  2     continue
      end do
    end do
  
    !---------------------------------------!
    !   Connect faces with matrix entries   !
    !---------------------------------------!
    do s = Grid % n_bnd_cells + 1, Grid % n_faces
      c1 = Grid % faces_c(1,s)
      c2 = Grid % faces_c(2,s)
  
      ! Where is matrix(c1,c2) and ...
      do c = Conn % row(c1), Conn % row(c1+1) - 1
        if(Conn % col(c) .eq. c2) then
          Conn % pos(1, s) = c
          exit
        end if
      end do
  
      ! ... where is matrix(c2,c1)
      do c = Conn % row(c2), Conn % row(c2+1) - 1
        if(Conn % col(c) .eq. c1) then
          Conn % pos(2, s) = c
          exit
        end if
      end do
    end do
  end if

  !-------------------------------------------!
  !                                           !
  !   Now handle the "real" matrix you need   !
  !                                           !
  !-------------------------------------------!

  ! These fields need to be allocate, they are different for every matrix
  allocate(Q % val(non_z));             Q % val   = 0
  allocate(Q % d_inv(Grid % n_cells));  Q % d_inv = 0

  ! These fields can be linked to singleton connectivity matrix
   Q % pnt_grid => Conn % pnt_grid
   Q % n        => Conn % n
   Q % nonzeros => Conn % nonzeros
   Q % row      => Conn % row
   Q % col      => Conn % col
   Q % dia      => Conn % dia
   Q % mir      => Conn % mir
   Q % pos      => Conn % pos

  end subroutine
