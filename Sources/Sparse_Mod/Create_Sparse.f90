!==============================================================================!
  subroutine Create_Sparse(A, Grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Sparse_Type)      :: A       !! parent class
  type(Grid_Type), target :: Grid    !! grid on which it is created
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, ni, nj, nk, non_z, run
  integer :: c, w, e, s, n, b, t, c1, c2, cols
  integer :: col_a, col_b, row_a, row_b, pos_a, pos_b
!==============================================================================!

  ! Store pointer to the grid
  A % pnt_grid => Grid

  print '(a)', ' # Creating a sparse matrix'

  ni = Grid % nx
  nj = Grid % ny
  nk = Grid % nz

  !--------------------------------!
  !   Form the compressed matrix   !
  !--------------------------------!
  do run = 1, 2

    non_z = 0  ! reset the counter

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

          ! Set next row index, one cell will be present for sure, own self
          if(run .eq. 2) A % row(c) = non_z + 1

          !----------------------!
          !   Cell is in fliud   !
          !----------------------!
          if(Grid % fluid(c) .eq. 1) then

            ! Bottom
            if(k > 1) then
              if(Grid % fluid(b) .eq. 1) then
                non_z = non_z + 1
                if(run .eq. 2) A % col(non_z) = b
              end if
            else
              Assert(k .eq. 1)
              b = Grid % Cell_Number(i, j, nk)  ! new bottom cell
              if(Grid % fluid(b) .eq. 1) then
                if(Grid % bc % b_type .eq. PERIODIC) then
                  non_z = non_z + 1
                  if(run .eq. 2) A % col(non_z) = b
                end if
              end if
            end if

            ! South
            if(j > 1) then
              if(Grid % fluid(s) .eq. 1) then
                non_z = non_z + 1
                if(run .eq. 2) A % col(non_z) = s
              end if
            else
              Assert(j .eq. 1)
              s = Grid % Cell_Number(i, nj, k)  ! new south cell
              if(Grid % fluid(s) .eq. 1) then
                if(Grid % bc % s_type .eq. PERIODIC) then
                  non_z = non_z + 1
                  if(run .eq. 2) A % col(non_z) = s
                end if
              end if
            end if

            ! West
            if(i > 1) then
              if(Grid % fluid(w) .eq. 1) then
                non_z = non_z + 1
                if(run .eq. 2) A % col(non_z) = w
              end if
            else
              Assert(i .eq. 1)
              w = Grid % Cell_Number(ni, j, k)  ! new west cell
              if(Grid % fluid(w) .eq. 1) then
                if(Grid % bc % w_type .eq. PERIODIC) then
                  non_z = non_z + 1
                  if(run .eq. 2) A % col(non_z) = w
                end if
              end if
            end if

          end if  ! cell is not in obstacle

          !--------------------------------------------------!
          !   Central - store it, be it in obstacle or not   !
          !--------------------------------------------------!
          non_z = non_z + 1
          if(run .eq. 2) A % col(non_z) = c

          !-----------------------------!
          !   Cell is not in obstacle   !
          !-----------------------------!
          if(Grid % fluid(c) .eq. 1) then

            ! East
            if(i < ni) then
              if(Grid % fluid(e) .eq. 1) then
                non_z = non_z + 1
                if(run .eq. 2) A % col(non_z) = e
              end if
            else
              Assert(i .eq. ni)
              e = Grid % Cell_Number(1, j, k)  ! new east cell
              if(Grid % fluid(e) .eq. 1) then
                if(Grid % bc % e_type .eq. PERIODIC) then
                 non_z = non_z + 1
                 if(run .eq. 2) A % col(non_z) = e
                end if
              end if
            end if

            ! North
            if(j < nj) then
              if(Grid % fluid(n) .eq. 1) then
                non_z = non_z + 1
                if(run .eq. 2) A % col(non_z) = n
              end if
            else
              Assert(j .eq. nj)
              n = Grid % Cell_Number(i, 1, k)  ! new north cell
              if(Grid % fluid(n) .eq. 1) then
                if(Grid % bc % n_type .eq. PERIODIC) then
                  non_z = non_z + 1
                  if(run .eq. 2) A % col(non_z) = n
                end if
              end if
            end if

            ! Top
            if(k < nk) then       ! top
              if(Grid % fluid(t) .eq. 1) then
                non_z = non_z + 1
                if(run .eq. 2) A % col(non_z) = t
              end if
            else
              Assert(k .eq. nk)
              t = Grid % Cell_Number(i, j, 1)  ! new top cell
              if(Grid % fluid(t) .eq. 1) then
                if(Grid % bc % t_type .eq. PERIODIC) then
                  non_z = non_z + 1
                  if(run .eq. 2) A % col(non_z) = t
                end if
              end if
            end if

          end if  ! cell is not in obstacle

        end do  ! i
      end do    ! j
    end do      ! k

    print '(a,i15)', ' # Number of nonzeros: ', non_z

    if(run .eq. 1) then
      A % nonzeros = non_z
      A % n = ni * nj * nk
      Assert(A % n .eq. Grid % n_cells)
      allocate(A % row(Grid % n_cells+1));   A % row = 0
      allocate(A % dia(Grid % n_cells));     A % dia = 0
      allocate(A % col(non_z));              A % col = 0
      allocate(A % val(non_z));              A % val = 0
      allocate(A % fc (Grid % n_faces));     A % fc  = 0
      allocate(A % mir(non_z));              A % mir = 0
      Assert(Grid % n_faces .gt. 0)
      allocate(A % pos(2, Grid % n_faces));  A % pos   = 0
      allocate(A % d_inv(Grid % n_cells));   A % d_inv = 0
      allocate(A % v_m  (Grid % n_cells));   A % v_m   = 0
    end if

    ! Wrap it up
    if(run .eq. 2) A % row(ni*nj*nk+1) = non_z + 1

  end do  ! run

  !--------------------------------------!
  !   Sort each row in ascending order   !
  !--------------------------------------!
  do c = 1, Grid % n_cells
    call Sort_Mod_Int(A % col(A % row(c) : A % row(c+1)-1))
  end do

  ! Check 1: Each cell in the obstacle must have
  !          one colum reserved just for itself 
  do c = 1, Grid % n_cells
    if(Grid % fluid(c) .eq. 0) then
      cols = A % row(c+1) - A % row(c)
      Assert(cols .eq. 1)
    end if
  end do

  ! Check 2: No cell in fluid, should have
  !          any neighbors in the obstacle
  do c = 1, Grid % n_cells
    if(Grid % fluid(c) .eq. 1) then
      do cols = A % row(c), A % row(c + 1) - 1
        Assert(Grid % fluid(A % col(cols)) .eq. 1)
      end do
    end if
  end do

  !---------------------------------!
  !   Find positions of diagonals   !
  !---------------------------------!
  do row_a = 1, Grid % n_cells
    do pos_a = A % row(row_a), A % row(row_a + 1) - 1
      col_a = A % col(pos_a)  ! at this point you have row_a and col_a
      if(col_a == row_a) then
        A % dia(row_a) = pos_a
        goto 1
      end if
    end do
1   continue
  end do

  !----------------------!
  !   Find it's mirror   !
  !----------------------!
  do row_a = 1, Grid % n_cells
    do pos_a = A % row(row_a), A % row(row_a + 1) - 1
      col_a = A % col(pos_a)  ! at this point you have row_a and col_a

      row_b = col_a
      do pos_b = A % row(row_b), A % row(row_b + 1) - 1
        col_b = A % col(pos_b)  ! at this point you have row_b and col_b

        if( (col_b == row_a) .and. (row_b == col_a) ) then
          A % mir(pos_a) = pos_b
          A % mir(pos_b) = pos_a
          goto 2  ! done with the inner loop, get out
        end if
      end do
2     continue
    end do
  end do

  !-----------------------------------!
  !   Bare-bone matrix coefficients   !
  !-----------------------------------!
  do s = 1, Grid % n_faces

    Assert(Grid % s(s) .gt. TINY)
    Assert(Grid % d(s) .gt. TINY)

    A % fc(s) = Grid % s(s) / Grid % d(s)

  end do

  !---------------------------------------!
  !   Connect faces with matrix entries   !
  !---------------------------------------!
  do s = Grid % n_bnd_cells + 1, Grid % n_faces
    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)

    ! Connect only if both cells arae immersed in fluid
    if(Grid % fluid(c1) + Grid % fluid(c2) .eq. 2) then

      ! Where is matrix(c1,c2) and ...
      do c = A % row(c1), A % row(c1+1)-1
        if(A % col(c) .eq. c2) then
          A % pos(1, s) = c
          exit
        end if
      end do

      ! ... where is matrix(c2,c1)
      do c=A % row(c2),A % row(c2+1)-1
        if(A % col(c) .eq. c1) then
          A % pos(2, s) = c
          exit
        end if
      end do
    end if

  end do

  end subroutine
