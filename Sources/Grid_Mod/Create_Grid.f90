!==============================================================================!
  subroutine Create_Grid(Grid, lx, ly, lz, nx, ny, nz)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Grid_Type)    :: Grid
  real,    intent(in) :: lx, ly, lz
  integer, intent(in) :: nx, ny, nz
!-----------------------------------[Locals]-----------------------------------!
  integer           :: s, c, c1, c2, e, n, t, i, j, k, bc, per_start
  integer           :: ox, oy, oz
  real, allocatable :: work(:)
  real              :: dx, dy, dz, cdot
!==============================================================================!

  ! The minimum number of cells in each direction is three
  Assert(nx > 1)
  Assert(ny > 1)
  Assert(nz > 1)

  ! Some abbreviations
  dx = lx / nx
  dy = ly / ny
  dz = lz / nz

  !-----------!
  !           !
  !   Nodes   !
  !           !
  !-----------!

  ! Allocate memory for node coordinates
  allocate(Grid % xn(0:nx))
  allocate(Grid % yn(0:ny))
  allocate(Grid % zn(0:nz))

  ! Calculate node coordinates (it rarelly gets simpler than this)
  do i = 0, nx
    Grid % xn(i) = real(i) * lx / real(nx)
  end do
  do j = 0, ny
    Grid % yn(j) = real(j) * ly / real(ny)
  end do
  do k = 0, nz
    Grid % zn(k) = real(k) * lz / real(nz)
  end do

  !---------------!
  !               !
  !   Cells (1)   !
  !               !
  !---------------!

  Grid % n_cells     = nx * ny * nz
  Grid % n_bnd_cells = 0
  if(Grid % bc % w_type .ne. PERIODIC)   Grid % n_bnd_cells  &
                                       = Grid % n_bnd_cells + 2*ny*nz
  if(Grid % bc % s_type .ne. PERIODIC)   Grid % n_bnd_cells  &
                                       = Grid % n_bnd_cells + 2*nx*nz
  if(Grid % bc % b_type .ne. PERIODIC)   Grid % n_bnd_cells  &
                                       = Grid % n_bnd_cells + 2*nx*ny
  print '(a,i12,a)', ' # Grid should have ', Grid % n_bnd_cells,  &
                     ' boundary cells'

  Grid % lx = lx
  Grid % ly = ly
  Grid % lz = lz

  Grid % nx = nx
  Grid % ny = ny
  Grid % nz = nz

  !----------------------------------!
  !   Cell-based memory allocation   !
  !----------------------------------!

  ! Allocate memory for cell coordinates
  allocate(Grid % xc(-Grid % n_bnd_cells:Grid % n_cells))
  allocate(Grid % yc(-Grid % n_bnd_cells:Grid % n_cells))
  allocate(Grid % zc(-Grid % n_bnd_cells:Grid % n_cells))

  ! Memory for cell volumes
  allocate(Grid % vol(-Grid % n_bnd_cells:Grid % n_cells))

  ! Calculate cell coordinates and volumens
  do k = 1, nz
    do j = 1, ny
      do i = 1, nx
        c = Grid % Cell_Number(i, j, k)

        ! Cell center
        Grid % xc(c) = 0.5 * (Grid % xn(i-1) + Grid % xn(i))
        Grid % yc(c) = 0.5 * (Grid % yn(j-1) + Grid % yn(j))
        Grid % zc(c) = 0.5 * (Grid % zn(k-1) + Grid % zn(k))

        ! Cell volume
        Grid % vol(c) = dx * dy * dz
      end do
    end do
  end do

  ! Allocate memory for cells to cells connectivity
  allocate(Grid % cells_n_cells(-Grid % n_bnd_cells:Grid % n_cells))
  allocate(Grid % cells_c(6,    -Grid % n_bnd_cells:Grid % n_cells))
  allocate(Grid % cells_f(6,    -Grid % n_bnd_cells:Grid % n_cells))

  !------------------------!
  !                        !
  !   Play with obstacle   !
  !                        !
  !------------------------!

  ! Perform if obstacle is sane
  if(Grid % has_obstacle) then
    ox = Grid % o_i_max - Grid % o_i_min + 1
    oy = Grid % o_j_max - Grid % o_j_min + 1
    oz = Grid % o_k_max - Grid % o_k_min + 1
    Assert(ox .gt. 0)
    Assert(oy .gt. 0)
    Assert(oz .gt. 0)
  end if

  allocate(Grid % fluid(Grid % n_cells));  Grid % fluid(:) = 1
  if(Grid % has_obstacle) then
    do c = 1, Grid % n_cells
      call Grid % Cells_I_J_K(c, i, j, k)
      if(i .ge. Grid % o_i_min  .and. i .le. Grid % o_i_max  .and.  &
         j .ge. Grid % o_j_min  .and. j .le. Grid % o_j_max  .and.  &
         k .ge. Grid % o_k_min  .and. k .le. Grid % o_k_max)        &
        Grid % fluid(c) = 0
    end do
  end if

# if VFS_DEBUG == 1
  allocate(work(Grid % n_cells)); work(:) = 0.0
  do c = 1, Grid % n_cells
    if(Grid % has_obstacle) work(c) = Grid % fluid(c)
  end do
  call Grid % Save_Vtk_Scalar("fluid.vtk", work)
# endif

  !-----------!
  !           !
  !   Faces   !  (some of them are also boundary cells, in fact)
  !           !
  !-----------!

  !------------------------------------!
  !   Do some checks for periodicity   !
  !------------------------------------!
  if(Grid % bc % w_type .eq. PERIODIC .or.  &
     Grid % bc % e_type .eq. PERIODIC) then
    Assert(Grid % bc % w_type .eq. Grid % bc % e_type)
  end if
  if(Grid % bc % s_type .eq. PERIODIC .or.  &
     Grid % bc % n_type .eq. PERIODIC) then
    Assert(Grid % bc % s_type .eq. Grid % bc % n_type)
  end if
  if(Grid % bc % b_type .eq. PERIODIC .or.  &
     Grid % bc % t_type .eq. PERIODIC) then
    Assert(Grid % bc % b_type .eq. Grid % bc % t_type)
  end if

  Grid % n_faces = 3 * nx * ny * nz
  if(Grid % bc % e_type .ne. PERIODIC) Grid % n_faces = Grid % n_faces + ny*nz
  if(Grid % bc % n_type .ne. PERIODIC) Grid % n_faces = Grid % n_faces + nz*nx
  if(Grid % bc % t_type .ne. PERIODIC) Grid % n_faces = Grid % n_faces + nx*ny

  print '(a,i12,a)', ' # Grid should have ', Grid % n_faces, ' faces'

  !--------------------------------!
  !   Allocate face-based memory   !
  !--------------------------------!
  allocate(Grid % faces_c(2, Grid % n_faces))

  allocate(Grid % sx(Grid % n_faces))
  allocate(Grid % sy(Grid % n_faces))
  allocate(Grid % sz(Grid % n_faces))
  allocate(Grid % s (Grid % n_faces))

  allocate(Grid % dx(Grid % n_faces))
  allocate(Grid % dy(Grid % n_faces))
  allocate(Grid % dz(Grid % n_faces))
  allocate(Grid % d (Grid % n_faces))

  s  = 0
  bc = 0
  Grid % sx(:) = 0.0
  Grid % sy(:) = 0.0
  Grid % sz(:) = 0.0

  !-----------------------------------!
  !   Handle the domain in the west   !
  !-----------------------------------!
  if(Grid % bc % w_type .ne. PERIODIC) then
    do c = 1, Grid % n_cells
      call Grid % Cells_I_J_K(c, i, j, k)
      if(i .eq. 1) then
        s  = s  + 1
        bc = bc + 1

        Grid % faces_c(1,s) =  c
        Grid % faces_c(2,s) = -bc
        Grid % sx(s) = -dy*dz

        Grid % xc(-bc) = Grid % xn(0)
        Grid % yc(-bc) = 0.5 * (Grid % yn(j-1) + Grid % yn(j))
        Grid % zc(-bc) = 0.5 * (Grid % zn(k-1) + Grid % zn(k))

      end if
    end do

  !-----------------------------------!
  !   Handle the domain in the east   !
  !-----------------------------------!
    do c = 1, Grid % n_cells
      call Grid % Cells_I_J_K(c, i, j, k)
      if(i .eq. Grid % nx) then
        s  = s  + 1
        bc = bc + 1

        Grid % faces_c(1,s) =  c
        Grid % faces_c(2,s) = -bc
        Grid % sx(s) = +dy*dz

        Grid % xc(-bc) = Grid % xn(nx)
        Grid % yc(-bc) = 0.5 * (Grid % yn(j-1) + Grid % yn(j))
        Grid % zc(-bc) = 0.5 * (Grid % zn(k-1) + Grid % zn(k))
      end if
    end do
  end if  ! not periodic

  !------------------------------------!
  !   Handle the domain in the south   !
  !------------------------------------!
  if(Grid % bc % s_type .ne. PERIODIC) then

    do c = 1, Grid % n_cells
      call Grid % Cells_I_J_K(c, i, j, k)
      if(j .eq. 1) then
        s  = s  + 1
        bc = bc + 1

        Grid % faces_c(1,s) =  c
        Grid % faces_c(2,s) = -bc
        Grid % sy(s) = -dx*dz

        Grid % xc(-bc) = 0.5 * (Grid % xn(i-1) + Grid % xn(i))
        Grid % yc(-bc) = Grid % yn(0)
        Grid % zc(-bc) = 0.5 * (Grid % zn(k-1) + Grid % zn(k))
      end if
    end do

  !------------------------------------!
  !   Handle the domain in the north   !
  !------------------------------------!
    do c = 1, Grid % n_cells
      call Grid % Cells_I_J_K(c, i, j, k)
      if(j .eq. Grid % ny) then
        s  = s  + 1
        bc = bc + 1

        Grid % faces_c(1,s) =  c
        Grid % faces_c(2,s) = -bc
        Grid % sy(s) = +dx*dz

        Grid % xc(-bc) = 0.5 * (Grid % xn(i-1) + Grid % xn(i))
        Grid % yc(-bc) = Grid % yn(ny)
        Grid % zc(-bc) = 0.5 * (Grid % zn(k-1) + Grid % zn(k))
      end if
    end do
  end if

  !-------------------------------------!
  !   Handle the domain at the bottom   !
  !-------------------------------------!
  if(Grid % bc % b_type .ne. PERIODIC) then
    do c = 1, Grid % n_cells
      call Grid % Cells_I_J_K(c, i, j, k)
      if(k .eq. 1) then
        s  = s  + 1
        bc = bc + 1

        Grid % faces_c(1,s) =  c
        Grid % faces_c(2,s) = -bc
        Grid % sz(s) = -dx*dy

        Grid % xc(-bc) = 0.5 * (Grid % xn(i-1) + Grid % xn(i))
        Grid % yc(-bc) = 0.5 * (Grid % yn(j-1) + Grid % yn(j))
        Grid % zc(-bc) = Grid % zn(0)
      end if
    end do

  !----------------------------------!
  !   Handle the domain at the top   !
  !----------------------------------!
    do c = 1, Grid % n_cells
      call Grid % Cells_I_J_K(c, i, j, k)
      if(k .eq. Grid % nz) then
        s  = s  + 1
        bc = bc + 1

        Grid % faces_c(1,s) =  c
        Grid % faces_c(2,s) = -bc
        Grid % sz(s) = +dx*dy

        Grid % xc(-bc) = 0.5 * (Grid % xn(i-1) + Grid % xn(i))
        Grid % yc(-bc) = 0.5 * (Grid % yn(j-1) + Grid % yn(j))
        Grid % zc(-bc) = Grid % zn(nz)
      end if
    end do
  end if

  print '(a,i12,a)', ' # Found ', s,  ' faces at boundaries'
  print '(a,i12,a)', ' # Found ', bc, ' cells at boundaries'
  Assert(Grid % n_bnd_cells .eq. bc)

  !------------------------------------!
  !   Handle faces inside the domain   !
  !------------------------------------!
  do k = 1, Grid % nz
    do j = 1, Grid % ny
      do i = 1, Grid % nx

        c = Grid % Cell_Number(i, j, k)
        e = c + 1
        n = c + Grid % nx
        t = c + Grid % nx * Grid % ny

        if(i .lt. Grid % nx) then
          s = s + 1
          Grid % faces_c(1,s) = c
          Grid % faces_c(2,s) = e
          Grid % sx(s) = +dy*dz
        end if

        if(j .lt. Grid % ny) then
          s = s + 1
          Grid % faces_c(1,s) = c
          Grid % faces_c(2,s) = n
          Grid % sy(s) = +dx*dz
        end if

        if(k .lt. Grid % nz) then
          s = s + 1
          Grid % faces_c(1,s) = c
          Grid % faces_c(2,s) = t
          Grid % sz(s) = +dx*dy
        end if
      end do
    end do
  end do

  per_start = s

  !------------------------------------------------------!
  !   Handle the periodicity of the domain in the west   !
  !------------------------------------------------------!
  if(Grid % bc % w_type .eq. PERIODIC) then
    do c = 1, Grid % n_cells
      call Grid % Cells_I_J_K(c, i, j, k)
      if(i .eq. 1) then
        s  = s  + 1

        Grid % faces_c(1,s) =  c
        Grid % faces_c(2,s) =  Grid % Cell_Number(nx, j, k)
        Grid % sx(s) = -dy*dz
      end if
    end do
  end if  ! it is periodic

  !-------------------------------------------------------!
  !   Handle the periodicity of the domain in the south   !
  !-------------------------------------------------------!
  if(Grid % bc % s_type .eq. PERIODIC) then
    do c = 1, Grid % n_cells
      call Grid % Cells_I_J_K(c, i, j, k)
      if(j .eq. 1) then
        s  = s  + 1

        Grid % faces_c(1,s) =  c
        Grid % faces_c(2,s) =  Grid % Cell_Number(i, ny, k)
        Grid % sy(s) = -dx*dz
      end if
    end do
  end if  ! it is periodic

  !--------------------------------------------------------!
  !   Handle the periodicity of the domain at the bottom   !
  !--------------------------------------------------------!
  if(Grid % bc % b_type .eq. PERIODIC) then
    do c = 1, Grid % n_cells
      call Grid % Cells_I_J_K(c, i, j, k)
      if(k .eq. 1) then
        s  = s  + 1

        Grid % faces_c(1,s) =  c
        Grid % faces_c(2,s) =  Grid % Cell_Number(i, j, nz)
        Grid % sz(s) = -dx*dy
      end if
    end do
  end if

  print '(a,i12,a)', ' # Found ', s,  ' faces at boundaries'
  print '(a,i12,a)', ' # Found ', bc, ' cells at boundaries'
  Assert(Grid % n_bnd_cells .eq. bc)
  print '(a,i12,a)', ' # Found a total of ', s, ' faces'
  Assert(s .eq. Grid % n_faces)

  !---------------------------------------------!
  !   Form dx, dy and dz, then finish s and d   !
  !---------------------------------------------!
  do s = 1, Grid % n_faces
    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)
    Assert(c2 .ne. c1)
    if(c2 .gt. 0) Assert(c2 .gt. c1)

    Grid % dx(s) = Grid % xc(c2) - Grid % xc(c1)
    Grid % dy(s) = Grid % yc(c2) - Grid % yc(c1)
    Grid % dz(s) = Grid % zc(c2) - Grid % zc(c1)

    ! If you are in periodic region
    if(s .gt. per_start) then
      if(Grid % bc % w_type .eq. PERIODIC) then
        if(abs(Grid % dx(s)) .gt. abs(Grid % dy(s)) + abs(Grid % dz(s)))  &
          Grid % dx(s) = Grid % dx(s) - Grid % lx
      end if
      if(Grid % bc % s_type .eq. PERIODIC) then
        if(abs(Grid % dy(s)) .gt. abs(Grid % dx(s)) + abs(Grid % dz(s)))  &
          Grid % dy(s) = Grid % dy(s) - Grid % ly
      end if
      if(Grid % bc % b_type .eq. PERIODIC) then
        if(abs(Grid % dz(s)) .gt. abs(Grid % dx(s)) + abs(Grid % dy(s)))  &
          Grid % dz(s) = Grid % dz(s) - Grid % lz
      end if
    end if

    cdot = Grid % dx(s) * Grid % sx(s)  &
         + Grid % dy(s) * Grid % sy(s)  &
         + Grid % dz(s) * Grid % sz(s)
    Assert(cdot > 0.0)

    Grid % s(s) = sqrt(Grid % sx(s)**2 + Grid % sy(s)**2 + Grid % sz(s)**2)
    Grid % d(s) = sqrt(Grid % dx(s)**2 + Grid % dy(s)**2 + Grid % dz(s)**2)
  end do

  !---------------!
  !               !
  !   Cells (2)   !
  !               !
  !---------------!
  Grid % cells_n_cells(:) = 0
  Grid % cells_c(:,:)     = 0
  Grid % cells_f(:,:)     = 0
  do s = 1, Grid % n_bnd_cells  ! boundary faces
    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)
    Assert(c1 .gt. 0)
    Assert(c2 .lt. 0)
    Grid % cells_n_cells(c1) = Grid % cells_n_cells(c1) + 1
    i = Grid % cells_n_cells(c1)  ! i like index, let's say
    Grid % cells_c(i,c1) = c2
    Grid % cells_f(i,c1) = s
    Grid % cells_n_cells(c2) = 1
    Grid % cells_c(1,c2) = c1
    Grid % cells_f(1,c2) = s
  end do

  do s = Grid % n_bnd_cells + 1, Grid % n_faces  ! inside faces
    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)
    Assert(c1 .gt. 0)
    Assert(c2 .gt. 0)
    Grid % cells_n_cells(c1) = Grid % cells_n_cells(c1) + 1
    Grid % cells_n_cells(c2) = Grid % cells_n_cells(c2) + 1
    i = Grid % cells_n_cells(c1)  ! i like index, let's say
    j = Grid % cells_n_cells(c2)  ! j is like, following i
    Grid % cells_c(i,c1) = c2
    Grid % cells_f(i,c1) = s
    Grid % cells_c(j,c2) = c1
    Grid % cells_f(j,c2) = s
  end do

  do c = 1, Grid % n_cells
    Assert(Grid % cells_n_cells(c) .le. 6)
  end do

  print '(a,2es12.3)', ' # Min/Max sx: ',  &
           minval(Grid % sx(:)), maxval(Grid % sx(:))
  print '(a,2es12.3)', ' # Min/Max sy: ',  &
           minval(Grid % sy(:)), maxval(Grid % sy(:))
  print '(a,2es12.3)', ' # Min/Max sz: ',  &
           minval(Grid % sz(:)), maxval(Grid % sz(:))
  print '(a,2es12.3)', ' # Min/Max dx: ',  &
           minval(Grid % dx(:)), maxval(Grid % dx(:))
  print '(a,2es12.3)', ' # Min/Max dy: ',  &
           minval(Grid % dy(:)), maxval(Grid % dy(:))
  print '(a,2es12.3)', ' # Min/Max dz: ',  &
           minval(Grid % dz(:)), maxval(Grid % dz(:))

  !---------------------------------!
  !   Check the faces_c structure   !
  !---------------------------------!
# if VFS_DEBUG == 1
  work(:) = 0.0
  do s = 1, Grid % n_faces
    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)
    if(c2 .gt. 0) then
      work(c1) = work(c1) + 1.0
      work(c2) = work(c2) + 1.0
    end if
  end do
  call Grid % Save_Vtk_Scalar("visited.vtk", work)
# endif

  end subroutine

