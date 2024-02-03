!==============================================================================!
  subroutine Create_Sparse(a, Grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Sparse_Type) :: a
  type(Grid_Type)    :: Grid
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, ni, nj, nk, non_zeros
  integer :: c, w, e, s, n, b, t
  integer :: col_a, col_b, row_a, row_b, pos_a, pos_b
  real    :: dx, dy, dz
!==============================================================================!

  print *, '# Creating a sparse matrix'

  ni = Grid % nx
  nj = Grid % ny
  nk = Grid % nz

  dx = Grid % lx / Grid % nx
  dy = Grid % ly / Grid % ny
  dz = Grid % lz / Grid % nz

  !------------------------------------------!
  !   Count non-zeroes and allocate memory   !
  !------------------------------------------!
  non_zeros = 0

  ! Browse in a way in which cell number "a" will increase one by one
  do k = 1, nk
    do j = 1, nj
      do i = 1, ni
        c = (k-1)*ni*nj + (j-1)*ni + i

        if(k > 1) non_zeros = non_zeros + 1
        if(j > 1) non_zeros = non_zeros + 1
        if(i > 1) non_zeros = non_zeros + 1
        non_zeros = non_zeros + 1
        if(i < ni) non_zeros = non_zeros + 1
        if(j < nj) non_zeros = non_zeros + 1
        if(k < nk) non_zeros = non_zeros + 1

      end do
    end do
  end do

  print *, '# Number of nonzeros: ', non_zeros
  a % n        = ni*nj*nk
  a % nonzeros = non_zeros
  allocate (a % row(ni*nj*nk+1)); a % row = 0
  allocate (a % dia(ni*nj*nk));   a % dia = 0
  allocate (a % col(non_zeros));  a % col = 0
  allocate (a % val(non_zeros));  a % val = 0
  allocate (a % mir(non_zeros));  a % mir = 0

  !--------------------------------!
  !   Form the compressed matrix   !
  !--------------------------------!
  non_zeros = 0

  ! Browse in a way in which cell number "a" will increase one by one
  do k = 1, nk
    do j = 1, nj
      do i = 1, ni
        c = (k-1)*ni*nj + (j-1)*ni + i

        ! First neighbours
        e = c+1
        w = c-1
        n = c+ni
        s = c-ni
        t = c+ni*nj
        b = c-ni*nj

        ! If second pass, set row index
        a % row(c) = non_zeros + 1

        !-------!
        !   B   !
        !-------!
        if(k > 1) then
          non_zeros = non_zeros + 1
          a % col(non_zeros) = b
          a % val(non_zeros) = -(dx*dy) / dz
        end if

        !-------!
        !   S   !
        !-------!
        if(j > 1) then
          non_zeros = non_zeros + 1
          a % col(non_zeros) = s
          a % val(non_zeros) = -(dx*dz) / dy
        end if

        !-------!
        !   W   !
        !-------!
        if(i > 1) then
          non_zeros = non_zeros + 1
          a % col(non_zeros) = w
          a % val(non_zeros) = -(dy*dz) / dx
        end if

        !-------!
        !   C   !
        !-------!
        non_zeros = non_zeros + 1
        a % col(non_zeros) = c
        a % val(non_zeros) = 4.0 * ( dx*dy/dz + dx*dz/dy + dy*dz/dx )

        !-------!
        !   E   !
        !-------!
        if(i < ni) then
          non_zeros = non_zeros + 1
          a % col(non_zeros) = e
          a % val(non_zeros) = -(dy*dz) / dx
        end if

        !-------!
        !   N   !
        !-------!
        if(j < nj) then
          non_zeros = non_zeros + 1
          a % col(non_zeros) = n
          a % val(non_zeros) = -(dx*dz) / dy
        end if

        !-------!
        !   T   !
        !-------!
        if(k < nk) then
          non_zeros = non_zeros + 1
          a % col(non_zeros) = t
          a % val(non_zeros) = -(dx*dy) / dz
        end if

      end do
    end do
  end do

  a % row(ni*nj*nk+1) = non_zeros+1

  !---------------------------------!
  !   Find positions of diagonals   !
  !---------------------------------!
  do row_a = 1, a % n
    do pos_a = a % row(row_a), a % row(row_a + 1) - 1
      col_a = a % col(pos_a)  ! at this point you have row_a and col_a
      if(col_a == row_a) then
        a % dia(row_a) = pos_a
        goto 1
      end if
    end do
1   continue
  end do

  !----------------------!
  !   Find it's mirror   !
  !----------------------!

  ! Outer loop
  do row_a = 1, a % n
    do pos_a = a % row(row_a), a % row(row_a + 1) - 1
      col_a = a % col(pos_a)  ! at this point you have row_a and col_a

      row_b = col_a
      do pos_b = a % row(row_b), a % row(row_b + 1) - 1
        col_b = a % col(pos_b)  ! at this point you have row_b and col_b

        if( (col_b == row_a) .and. (row_b == col_a) ) then
          a % mir(pos_a) = pos_b
          a % mir(pos_b) = pos_a
          goto 2  ! done with the inner loop, get out
        end if
      end do
2     continue
    end do
  end do

  end subroutine
