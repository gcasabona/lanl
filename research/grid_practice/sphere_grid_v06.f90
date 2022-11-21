
! Compile this code for example with 
!
! gfortran -O0 -g -fimplicit-none  -Wall  -Wline-truncation  -Wcharacter-truncation  -Wsurprising  -Waliasing -Wimplicit-interface ../sphere_grid_v06.f90

! gfortran -O2 ../sphere_grid_v06.f90

!*******************************************************************************
! Grid generation code for a sphere.
!
! This is Version 8.1 (April 10, 2021).
!
! This code generates a 3D grid over a sphere centered at the origin.
!
! ------------------------------------------------------------------------------
!
!  Input: See input parameter module or sample input files.
!
!
! Output: Various output files: Tecplot files, .ugrid, .su2, .vtk, etc.
!
!
!        written by Dr. Katate Masatsuka (info[at]cfdbooks.com),
!
! the author of useful CFD books, "I do like CFD" (http://www.cfdbooks.com).
!
! Version 8.1
! 04-10-2021: Fixed a bug in the construction_type = 2.
!
! Version 8
! 04-10-2021: Added an option to generate a grid based on an inner layer
!             with a constant spacing plus an outer layer with a specified
!             # of cells to a specified outer boundary or with a specified
!             growth-rate.
!
!             Also, added an input paramete module, so that input parameters
!             are now specified in the namelist file "input.nml."
!
! Version 7
! 10-31-2019: Fixed bugs in the iteration for a stretching factor.
!
! Version 6
! 07-24-2019: Generates .su2 grid file.
!             Added nodal perturbation as an option.
!
! Version 4
! 04-11-2016: Comments updated.
!
! This F90 code is written and made available for an educational purpose.
! This file may be updated in future.
!
! Katate Masatsuka, http://www.cfdbooks.com
!
!*******************************************************************************

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!  Input parameter module
!
!-------------------------------------------------------------------------------
!
!  Sample input file: 'input.nml' to generate a grid.
!  ------------------------------------------------------
!
! &input_parameters
!               debug_mode = F
!               igrid_type = 1       !1=Prism, 2=Tet, 3=Prism/=Tet
!                    nr_gs = 8       !size of surface grid
!        construction_type = 2       !1=with target Re/y_plus, 2 =with dz
!                     rmax = 0.1     !(construct 1): thickness of BL region
!    target_reynolds_number= 1.0e+05 !(construct 1): target Re
!             target_yplus = 1.0e-3  !(construct 1): target y+
!                       dz = 0.01    !(construct 2): first spacing
!               n_dz_layer = 32      !(construct 2): # of cells in the first layer.
!            n_outer_layer = 32      !(construct 2): # of cells in the outer layer.
!                   Rinner = 0.5     !Innner sphere radius
!                   Router = 10.0    !Outer sphere radius
!              growth_rate = 1.2     !Use growth rate if Router < 0.
!            perturb_nodes = F       !T=perturb nodes, F=not perturb(regular)
!  perturb_nodes_parameter = 1.5     !parameter for perturb_nodes = T
!           smoothing_type = 0       !=0 no smoothing, =1 smooth, =2 constraind smooth
!      remove_corner_nodes = "none"  !"yes" to remove some nodes, "no" not to remove.
!          generate_k_file = F       !=T write structured indices, =F do not write.
!       generate_line_file = F       !=T generate line info, =F do not write
!              line_extent = 2       !=1 lines in BL, =2 all the way to outer.
!      generate_ugrid_file = T       !T = Write a .ugrid file.(required by the coarsening program)
!   ugrid_file_unformatted = T       !T = unformatted .ugrid/.ufmt, F = formatted .ugrid/.p3d
!      generate_tec_file_b = F       !T = Write a boundary grid (Tecplot)
!      generate_tec_file_v = F       !T = Write a volume grid (Tecplot)
!    generate_su2grid_file = F       !T = Write .su2 grid file.
!    generate_vtk_file     = F       !T = Write .vtk grid file.
! /
!
!  ------------------------------------------------------
!
!  Note: No need to specify all namelist variables.
!        In the above, those not shown are given their default values
!        as defined below.
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 module input_parameter_module

  implicit none

  integer , parameter :: dp = selected_real_kind(P=15)

  public

!----------------------------
! Default input values
!----------------------------

  logical  :: debug_mode = .false.

!----------------------------
!  igrid_type = Element type as listed below.
!                            1 = Prismatic
!                            2 = Tetrahedral
!                            3 = Mixed - Prism/Tetra

  integer :: igrid_type = 1

!----------------------------
!  nr_gs is the # of elements that determines the size of the surface
!  triangulation.

  integer :: nr_gs = 8

!----------------------------
!  construction type
!    = 1 : specify BL thickness, target Reynolds number
!    = 2 : 

  integer :: construction_type = 1 !or =2


!------------------------------------------------------
! Construction type = 1 parameters
!------------------------------------------------------
!----------------------------
! Boundary ;ayer (BL) thickness
!  (Note: Distance from the wall within which you assume"
!      the boundary layer exists. This is the region "
!      where the prismatic layer is constructed in"
!      case of mixed grids. Nodes will be placed"
!      normal to the wall by geometric stretching in this region."

  real(dp) :: rmax = 1.0e-01_dp

!----------------------------
! Target Reynolds number (to specify y+).
! (Note: Input a negative value to specify the grid spacing in the grid unit. "
!        E.g., type in -10, then input target_y_plus manually next.         ) "
!
  real(dp) :: target_reynolds_number = 0.5_dp

!----------------------------
! Target y_plus value. Or if  target_reynolds_number is negative,
! this is used as the vertical spacing off the wall relative to the
! diameter of the inner sphere.
!
  real(dp) :: target_yplus = 1.0e-02_dp

!------------------------------------------------------
! End of Construction type = 1 parameters
!------------------------------------------------------

!------------------------------------------------------
! Construction type = 2 parameters
!------------------------------------------------------
!----------------------------
! First-off-the-wall distance

  real(dp) :: dz = 1.0e-02_dp

!----------------------------
! !# of cells in the radial direction within the constant-dz layer.
!
  integer :: n_dz_layer = 32

!----------------------------
! !# of cells in the radial direction within the outer layer.
!
  integer :: n_outer_layer = 32

!----------------------------
! Radius of the inner sphere 
!
  real(dp) :: Rinner = 0.5_dp

!----------------------------
! Radius of the outer sphere 
!
  real(dp) :: Router = 10.0_dp

!----------------------------
! Growth rate in the outer region (activated if Router<0.0)
!
  real(dp) :: growth_rate = 1.2_dp

!------------------------------------------------------
! End of Construction type = 2 parameters
!------------------------------------------------------


!----------------------------
! perturb_nodes = T to add nodal perturbation
!                 F not to add (generate a regular grid).

  logical :: perturb_nodes = .false.

!----------------------------
! Parameter for nodal perturbation
!
  real(dp) ::  perturb_nodes_parameter = 1.0_dp 

!----------------------------
!  smoothing_type = Smooth the surface triangulation.
!   " Smoothing the sphere surface triangulation"
!   "   0 : No smoothing"
!   "   1 : Smoothing"
!   "   2 : Constrained smoothing"

   integer ::   smoothing_type = 0 

!----------------------------
! remove_corner_nodes = Remove corner nodes for smooth triangulation.
!                    "yes" = remove;  "no" = don't remove
!
!  "--- Node removal recommended if smoothing applied..."
!  " Remove_corner_nodes? (Input yes or no)"
!  " NOTE: This should be 'no' for k1-k2-k3-k4 structured grid"

  character(80) :: remove_corner_nodes = "no"

!----------------------------
! To write a structured-grid indices of an unstructured grid.

  logical :: generate_k_file     = .false.

!----------------------------
! To write a line file.

  logical :: generate_line_file  = .false.

!----------------------------
! The type of the line file
!  1 = Lines within a thin boundary layer region"
!  2 = Lines go all the way to the outer boundary."

   integer ::  line_extent = 2

!----------------------------
! generate_ugrid_file = T to write .ugrid file
!                       F not to write.

  logical :: generate_ugrid_file = .true.

!----------------------------
! ugrid_file_unformatted = T: unformatted, F: formatted

  logical :: ugrid_file_unformatted = .true.

!----------------------------
! generate_tec_file_b = T: Write a Tecplot file for boundary.

  logical :: generate_tec_file_b = .false.

!----------------------------
! generate_tec_file_v = T: Write a Tecplot file for the entire volume grid.

  logical :: generate_tec_file_v = .false.

!----------------------------
! generate_su2grid_file = T to write .su2 file
!                         F not to write.

  logical :: generate_su2grid_file = .false. 

!----------------------------
! generate_vtk_file = T to write .vtk file
!                     F not to write.

  logical :: generate_vtk_file = .false. 

!----------------------------
! End of Default input values
!----------------------------

!------------------------------------------------------------------------
! List of all input variables:
!------------------------------------------------------------------------

  namelist / input_parameters /   &
                      debug_mode, &
                      igrid_type, &
                           nr_gs, &
               construction_type, &
                            rmax, & 
          target_reynolds_number, &
                    target_yplus, &
                              dz, &
                      n_dz_layer, &
                   n_outer_layer, &
                          Rinner, &
                          Router, &
                     growth_rate, &
                   perturb_nodes, &
         perturb_nodes_parameter, &
                  smoothing_type, & 
             remove_corner_nodes, &
                 generate_k_file, &
              generate_line_file, & 
                    line_extent , &
             generate_ugrid_file, &
          ugrid_file_unformatted, &
             generate_tec_file_b, &
             generate_tec_file_v, &
           generate_su2grid_file, &
               generate_vtk_file

!------------------------------------------------------------------------
! End of List of all input variables:
!------------------------------------------------------------------------

 contains

!*****************************************************************************
!* Read input_parameters in the input file: file name = namelist_file
!*****************************************************************************
  subroutine read_nml_input_parameters(namelist_file)

  implicit none
  character(9), intent(in) :: namelist_file
  integer :: os

  write(*,*) "**************************************************************"
  write(*,*) " List of namelist variables and their values"
  write(*,*)

  open(unit=10,file=namelist_file,form='formatted',status='old',iostat=os)
  read(unit=10,nml=input_parameters)

  write(*,nml=input_parameters) ! Print the namelist variables.
  close(10)


  end subroutine read_nml_input_parameters

 end module input_parameter_module
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!  End of input parameter module
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------


 program sphere_grid


  use input_parameter_module

 implicit none

! Parameters
  real(dp), parameter ::  zero = 0.0_dp
  real(dp), parameter ::   one = 1.0_dp
  real(dp), parameter ::   two = 2.0_dp
  real(dp), parameter :: three = 3.0_dp
  real(dp), parameter ::   six = 6.0_dp
  real(dp), parameter ::  half = 0.5_dp
  real(dp), parameter ::    pi = 3.14159265358979323846_dp

! Custom data types
  type tria_data
   integer, dimension(3) :: v    !Vertices (nodes) of the triangle
   integer               :: type !Type of triangle (upward, downward, cylinder)
  end type tria_data

  type node_data_xy
    integer :: gnode     !Global node number
   real(dp) :: x, y, z   !Coordinates in xy-plane.
  end type node_data_xy

  type node_data
   real(dp) :: x,y,z       !Coordinates in xyz space
   real(dp) :: nx,ny,nz    !Unit vector normal to the surface
   real(dp) :: nx2,ny2,nz2 !Unit vector along the second direction
   logical  :: circum_node
  end type node_data

! Input variables

! Output File names

! (1) Tecplot files for viewing intermediate/final grids for debugging.
  character(80) :: filename_gs        = "debug_generating_sector.dat"
  character(80) :: filename_disk      = "debug_disk.dat"
  character(80) :: filename_surface   = "debug_hemisphere_surface.dat"
  character(80) :: filename_surface_s = "debug_sphere_surface.dat"
  character(80) :: filename_tecplot_b
  character(80) :: filename_tecplot_v

! (2) These are the files used for CFD computations
  character(80) :: filename_mapbc
  character(80) :: filename_lines
  character(80) :: filename_ugrid
  character(80) :: filename_k

  character(80) :: filename_su2grid
  character(80) :: filename_vtk

! Local variables
   integer :: os           !IO constant
  real(dp) :: distance     !Distance to the outer boundary from the body
   integer :: i,j,k,inode

   integer :: ntrias_gs    !Number of triangles in the generating sector
   integer :: nnodes_gs    !Number of nodes in the generating sector
  real(dp) :: Rd           !Radius of the sphere
  real(dp) :: r_gs         !Radius of the generating sector
  real(dp) :: dr_gs        !Spacing along the side of the generating sector
  real(dp) :: r2, dtheta, theta
  type(node_data_xy), dimension(:),     pointer :: nodes1, nodes2, node_gs
  integer, dimension(:),     pointer :: k1_gs, k2_gs, k3_gs
  type(tria_data)   , dimension(:), allocatable :: tria_gs

  integer :: nnodes_disk, ntrias_disk
  type(node_data_xy), dimension(:),     pointer :: node_disk
  integer, dimension(:),     pointer :: k1_disk
  integer, dimension(:),     pointer :: k2_disk
  integer, dimension(:),     pointer :: k3_disk
  type(tria_data)   , dimension(:), allocatable :: tria_disk

  real(dp) :: xp, yp, zp

  integer  :: nnodes_hs
  real(dp) :: s
  type(node_data), dimension(:),     pointer :: node_body_hs
  type(tria_data), dimension(:), allocatable :: tria_hs
  integer, dimension(:),     pointer :: k1_body_hs, k2_body_hs, k3_body_hs, k4_body_hs
  integer :: ntrias_hs

  integer  :: nnodes
  type(node_data), dimension(:),     pointer :: node_body
  type(tria_data), dimension(:), allocatable :: tria
  integer, dimension(:),     pointer :: k1_body, k2_body, k3_body, k4_body
  integer :: ntrias

  integer, dimension(:), allocatable :: node_map
  integer :: node1, node2, node3

  integer :: nnodes_circum
  integer, dimension(:)  , allocatable :: nodes_circum
  type(node_data), dimension(:), pointer :: node
  integer, dimension(:)  , allocatable :: k1, k2, k3, k4

  integer  :: nr, nm
  real(dp) :: drN, dr1, dr_outer, gr
  real(dp) :: sf, drnmp1 !Stretching factor in the outer region
  real(dp), allocatable, dimension(:) :: rs
  real(dp), allocatable, dimension(:) :: vspacing
  real(dp), allocatable, dimension(:) :: vspacing_nm

  integer :: nnodes_body
  integer :: node4, node5, node6
  integer :: ntrias_b, nquads_b, ntet, nprs
  integer,  dimension(:,:), allocatable :: tri, quad, tet, prs
  integer,  dimension(:)  , allocatable :: node_above

  real(dp) :: xc, yc, zc
  real(dp) :: dirx, diry, dirz

! Remove 6 nodes (Optional)
 
  integer, dimension(6)   :: nodes_removed, nodes_removed_nghbr
  integer, dimension(6,2) :: nodes_changed
  integer :: rnode, newnode
  logical :: deleted

! For smoothing applied on the disk triangulation
  integer  :: i_smoothing, max_smoothing, v(3)
  real(dp) :: smoothing_factor, dxy_norm, dxyz_norm
  real(dp), dimension(:,:), allocatable :: dxy
  real(dp), dimension(:,:), allocatable :: dxyz

  integer :: mglevels

  real(dp) :: cf
  integer  :: sk12p, sk23p, sk31p
  integer  :: sk12m, sk23m, sk31m
  integer  :: n_sections

  real(dp) :: ds, ds_outer, rs_nmm1, rs_nmp1, rs_nm, rs_nr, rs_nrm1
  real(dp) :: dr_spacing, err_dr_spacing
  integer  :: i_nr


! Variables used for perturbing the nodal coordiantes.
  real(dp) :: rn, ap
  integer, allocatable, dimension(:)   :: n_node2tet, bmark
  integer, allocatable, dimension(:,:) :: node2tet
  real(dp) :: volume, x1,x2,x3,x4, y1,y2,y3,y4, z1,z2,z3,z4
  real(dp) :: x0, y0, z0, vol_max, vol_min, vol_ave, vol_tot
  logical  :: negative_volume_exists
  integer  :: n_negative_vol_detected, it, ii
  real(dp), allocatable, dimension(:)   :: ave_length

  integer  :: nb
  character(80), dimension(4) :: bnames  !Boundary names

  integer  :: nhex
  integer,  dimension(:,:), allocatable :: hex

  integer  :: itr, stat
  real(dp) :: sf_initial, xi, cx, final_x
  real(dp), allocatable, dimension(:) :: xpp,ypp,zpp

!*******************************************************************************
! Read the input parameters, defined in the file named as 'input.nml'.
!*******************************************************************************

   write(*,*) "Reading the input file: input.nml..... "
   write(*,*)
   call read_nml_input_parameters('input.nml')
   write(*,*)

 !No hexa grid at the moment...
   nhex = 0
   allocate( hex(1,1) )

      nprs = 0
      ntet = 0
  ntrias_b = 0
  nquads_b = 0

    n_sections = 6 ! Full geometry, always.

!*******************************************************************************
! Sphere Geometry:
!
!
!        z
!        |                      y
!        |                      / 
!        |           Sphere    /     
!        |                * * /
!        |             *       *
!        |            *     /   *
!        |-----------------o---------------------------------> x
!                     *  Origin * 
!                      *       *
!                         * * 
!
!
!
!  The sphere is centered at the origin with the radius of "Rinner" specified in the input file.
!
!  Outer boundary is also a spherical surface centered at the origin with the radius
!  s[ecify by "Routerr" specified in the input file.
!
!*******************************************************************************

   Rd =  Rinner ! Sphere radius

!*******************************************************************************
! 0. Select the type of grid to generate.
!*******************************************************************************
!if (1==0) then

! (2)A parameter for nodal perturbation (larger value for larger perturbation).

  if (perturb_nodes) then

  write(*,*)
  write(*,*) " perturb_nodes = ", perturb_nodes
  write(*,*) " OK, nodes will be perturbed! (only for a tetra grid) "

  write(*,*)
  write(*,*) " For nodal perturbation:"
  write(*,*) "  perturb_nodes_parameter = ", perturb_nodes_parameter
  write(*,*)
  write(*,*) "-------------------------------------------"
  write(*,*)

  endif

!-------------------------------------------------------------------
   write(*,*)
   write(*,*) 

   if (igrid_type/=1 .and. igrid_type/=2 .and. igrid_type/=3) then
    write(*,*) " >>> Invalid input value: igrid_type = ", igrid_type
    write(*,*) " >>> It must be 1, 2, or 3."
    stop
   endif

  if (perturb_nodes .and. igrid_type/=2) then
      perturb_nodes = .false.
   write(*,*) 
   write(*,*) " Oh, nodal perturbation is only applicable to tetra grids..."
   write(*,*) " please set perturb_nodes = F in the input file, and try again."
   write(*,*) 
   stop
  endif

!-------------------------------------------------------------------

   if ( big_endian_io(9999) ) then
     write(*,*) 'The system is big Endian'
     write(*,*) ' Ensure big Endian -> setenv F_UFMTENDIAN big'
   else
     write(*,*) 'The system is little Endian'
   endif

!-------------------------------------------------------------------
  !(1) Prismatic grid
  if     ( igrid_type ==1  ) then

   filename_mapbc     = "sphere_prism.1.mapbc"
   filename_lines     = "sphere_prism.1.lines_fmt"
   if ( ugrid_file_unformatted ) then
     if ( big_endian_io(9999) ) then
      filename_ugrid     = "sphere_prism.1.b8.ugrid"
     else
      filename_ugrid     = "sphere_prism.1.lb8.ugrid"
     end if
   else
   filename_ugrid     = "sphere_prism.1.ugrid"
   endif
   filename_k         = "sphere_prism.1.k"
   filename_tecplot_b = "sphere_prism_boundary_tec.1.dat"
   filename_tecplot_v = "sphere_prism_volume_tec.1.dat"
   filename_su2grid   = "sphere_prism.1.su2"
   filename_vtk       = "sphere_prism.1.vtk"

!-------------------------------------------------------------------
  !(2) Tetrahedral grid
  elseif ( igrid_type ==2  ) then

   filename_mapbc     = "sphere_tetra.1.mapbc"
   filename_lines     = "sphere_tetra.1.lines_fmt"
   if ( ugrid_file_unformatted ) then
     if ( big_endian_io(9999) ) then
      filename_ugrid     = "sphere_tetra.1.b8.ugrid"
     else
      filename_ugrid     = "sphere_tetra.1.lb8.ugrid"
     end if
   else
   filename_ugrid     = "sphere_tetra.1.ugrid"
   endif
   filename_k         = "sphere_tetra.1.k"
   filename_tecplot_b = "sphere_tetra_boundary_tec.1.dat"
   filename_tecplot_v = "sphere_tetra_volume_tec.1.dat"
   filename_su2grid   = "sphere_tetra.1.su2"
   filename_vtk       = "sphere_tetra.1.vtk"

!-------------------------------------------------------------------
  !(3) Mixed grid: Prism near the surface and Tetra otherwise.
  elseif ( igrid_type ==3  ) then

   filename_mapbc     = "sphere_mixed.1.mapbc"
   filename_lines     = "sphere_mixed.1.lines_fmt"
   if ( ugrid_file_unformatted ) then
     if ( big_endian_io(9999) ) then
      filename_ugrid     = "sphere_mixed.1.b8.ugrid"
     else
      filename_ugrid     = "sphere_mixed.1.lb8.ugrid"
     end if
   else
   filename_ugrid     = "sphere_mixed.1.ugrid"
   endif
   filename_k         = "sphere_mixed.1.k"
   filename_tecplot_b = "sphere_mixed_boundary_tec.1.dat"
   filename_tecplot_v = "sphere_mixed_volume_tec.1.dat"
   filename_su2grid   = "sphere_mixed.1.su2"
   filename_vtk       = "sphere_mixed.1.vtk"

  endif

!-------------------------------------------------------------------
!  Distance to the outer boundary given by iout variable Router

   distance = Router

   write(*,*)
   write(*,*) "***********************************************************"
   write(*,*) " 1. Triangulate the generating sector, the building block"
   write(*,*) "***********************************************************"
   write(*,*)
   write(*,*) "Division of the generating sector = ", nr_gs
   write(*,*)

!*******************************************************************************
! 1. Systematic triangulation of the generating sector (isotropic).
!    Resolution of this sector will determine other dimensions.
!
! This is a sector with the central angle 60 degrees.
! It is called here the generating sector.
! It is located in the xy-plane.
! We first triangulate this, and use it to build a triangulation of the
! whole circle (the disk).
!
!       ____________
!       \/\/\/\/\/\/    This is an example corresponding to nr_gs = 6.
!        \/\/\/\/\/
!         \/\/\/\/        y ^
!          \/\/\/           |
!           \/\/            |
!            \/             ----> x
!
! NOTE: The number of triangles = 1 + 3 + 5 + ... + 2*nr_gs-1 = nr_gs*nr_gs
!       The number of nodes  = 1 + 2 + 3 + ... + nr_gs+1
!
! NOTE: Important to distinguish two types of triangles.
!       They will define the way the prism is subdivided into 3 tets.
!       The subdivision must be done carefully to match the triangular faces
!       between two adjacent tetrahedra.
!
!*******************************************************************************

   allocate(nodes1(1))
   allocate(nodes2(1))

   k = 0
   do
    k = k + 1
    if (mod(nr_gs,2**(k-1))==0) then
      mglevels = k
    else
      write(*,*)
      write(*,*) " Maximum multigrid level is ", mglevels
      write(*,*)
      exit
    endif
   end do

   r_gs = half*pi*Rd          ! Radius of the isotropic triangle
   write(*,*) " Radius of the isotropic triangle, r_gs = ", r_gs

  dr_gs = r_gs/real(nr_gs,dp) ! Uniform refinement
   write(*,*) " dr_gs = ", dr_gs

  nnodes_gs = (nr_gs+1)*((nr_gs+1)+1)/2
   write(*,*) " nnodes_gs = ", nnodes_gs
  ntrias_gs = nr_gs**2
   write(*,*) " ntrias_gs = ", ntrias_gs
  allocate(node_gs(nnodes_gs))
  allocate(tria_gs(ntrias_gs))
  allocate(k1_gs(nnodes_gs))
  allocate(k2_gs(nnodes_gs))
  allocate(k3_gs(nnodes_gs))

  nnodes_gs = 0
  ntrias_gs = 0

  triangulate : do i = 1, nr_gs

!   r1 = dr_gs * real(i-1,dp) ! Current arc
    r2 = dr_gs * real(i,dp)   !    Next arc

! Nodes on the current arc (r = r1)
   call my_alloc_ndxy_ptr(nodes1,i)

   if (i ==  1) then

      nodes1(1)%x = zero
      nodes1(1)%y = zero
      nodes1(1)%gnode = 1

        nnodes_gs = 1
     node_gs(1)%x = zero
     node_gs(1)%y = zero

     k1_gs(1) = 0
     k2_gs(1) = 0
     k3_gs(1) = - ( k1_gs(1) + k2_gs(1) )

   else

    do k = 1, i
     nodes1(k)%x     = nodes2(k)%x
     nodes1(k)%y     = nodes2(k)%y
     nodes1(k)%gnode = nodes2(k)%gnode
    end do

   endif

! Nodes on the next arc (r = r2): New nodes

    call my_alloc_ndxy_ptr(nodes2,i+1)
    dtheta = (pi/three) / real(i,dp)
    do k = 1, i+1
     theta = dtheta * real(k-1,dp)
     nodes2(k)%x = r2 * cos(theta)
     nodes2(k)%y = r2 * sin(theta)

     nnodes_gs = nnodes_gs + 1
     nodes2(k)%gnode = nnodes_gs
     node_gs(nnodes_gs)%x = nodes2(k)%x
     node_gs(nnodes_gs)%y = nodes2(k)%y


     k1_gs(nnodes_gs) = i + (1 - k)
     k2_gs(nnodes_gs) = k - 1
     k3_gs(nnodes_gs) = - ( k1_gs(nnodes_gs) + k2_gs(nnodes_gs) )


!    Keep the record of corner nodes and their neighbors for node removal
     if (trim(remove_corner_nodes) == "yes") then
      if (i==nr_gs-1 .and. k==1  ) nodes_removed_nghbr(1) = nnodes_gs
      if (i==nr_gs   .and. k==1  ) nodes_removed(1)       = nnodes_gs
      if (i==nr_gs-1 .and. k==i+1) nodes_removed_nghbr(2) = nnodes_gs
      if (i==nr_gs   .and. k==i+1) nodes_removed(2)       = nnodes_gs
     endif

    end do

! Triangulate the region between nodes1 and nodes2.
! Right to left

! NOTE: Nodes are ordered clockwise at this point.
!       It will be switched to counter-clockwise later.

! Type 1 triangle
!
! nodes2(k+1)   nodes2(k)
!      1             2
!       o-----------o
!        \         /  
!         \       /
!          \     /
!           \   /
!            \ /
!             o
!             3
!         nodes1(k)

   downward_tria : do k = 1, i
    ntrias_gs = ntrias_gs + 1
    tria_gs(ntrias_gs)%v(1) = nodes2(k+1)%gnode
    tria_gs(ntrias_gs)%v(2) = nodes2(k  )%gnode
    tria_gs(ntrias_gs)%v(3) = nodes1(k  )%gnode
    tria_gs(ntrias_gs)%type = 1
   end do downward_tria


! Type 2 triangle
!
!         nodes2(k+1)
!             3
!             o
!            / \
!           /   \
!          /     \ 
!         /       \
!        /         \
!       o-----------o
!      2             1
! nodes1(k+1)   nodes1(k)

   if (i > 1) then
    upward_tria : do k = 1, i-1
     ntrias_gs = ntrias_gs + 1
     tria_gs(ntrias_gs)%v(1) = nodes1(k  )%gnode
     tria_gs(ntrias_gs)%v(2) = nodes1(k+1)%gnode
     tria_gs(ntrias_gs)%v(3) = nodes2(k+1)%gnode
     tria_gs(ntrias_gs)%type = 2

     if (trim(remove_corner_nodes) == "yes") then
      if (i==nr_gs .and. k==i-1) then
       tria_gs(ntrias_gs)%type = 20
      endif
     endif

    end do upward_tria
   endif

  end do triangulate

! So, now I have a triangulation defined by tria_gs and node_gs.
! Number of triangles = ntrias_gs
! Number of nodes     = nnodes_gs

!*******************************************************************************
! Write a Tecplot file for the generating sector
!******************************************************************************
 debug_mode_01 : if (debug_mode) then
 open(unit=1, file=filename_gs, status="unknown", iostat=os)

  write(1,*) 'TITLE = "GRID"'
  write(1,*) 'VARIABLES = "x","y","z","k1+k2"'
  write(1,*) 'ZONE  N=', nnodes_gs,',E=', ntrias_gs,' , ET=triangle, F=FEPOINT'

! Nodes
  do i = 1, nnodes_gs
    write(1,'(3ES20.10,i10)') node_gs(i)%x, node_gs(i)%y, 0.0, k1_gs(i)+k2_gs(i)
  end do

! Triangles
  do i = 1, ntrias_gs
   write(1,'(3I10)') tria_gs(i)%v(1), tria_gs(i)%v(2), tria_gs(i)%v(3)
  end do

 close(1)

 write(*,*)
 write(*,*) "Tecplot file has been written: ", filename_gs
 endif debug_mode_01
!*******************************************************************************

!*******************************************************************************
! 2. Generate a triangulated disk.
!
! Now, rotate and copy the sector triangulation (as indicated by 'TS' below) onto
! 5 places (indicated by 1,2,3,4,5 below) to form a triangulation of a whole disk
! (6 patches in total):
!
!           .  . 
!        \        /
!     .   \   1  /   .
!    .     \    /     .
!   .   2   \  /  TS   .
!   _________\/_________
!   .        /\        .
!   .   3   /  \   5   .
!    .     /    \     .
!     .   /      \   .
!        /   4    \ .
!           .  .
!
! Generate new data: tria_disk and node_disk
! Number of triangles = ntrias_disk
! Number of nodes     = nnodes_disk
!*******************************************************************************

  write(*,*)
  write(*,*) "***********************************************************"
  write(*,*) " 2. Generate a disk by copying the generating sector"
  write(*,*) "***********************************************************"

  nnodes_disk = nnodes_gs * n_sections !<- More than enough (by the # of overlapping ndoes)
  ntrias_disk = ntrias_gs * n_sections
  allocate(node_disk(nnodes_disk))
  allocate(tria_disk(ntrias_disk))
  allocate(k1_disk(nnodes_disk))
  allocate(k2_disk(nnodes_disk))
  allocate(k3_disk(nnodes_disk))

  nnodes_disk = 0
  ntrias_disk = 0

! Copy the data from the generating-sector data to the disk data.

!  Copy the node data
!  This is the first one: theta = 0 to 60 degrees

   do i = 1, nnodes_gs
    node_disk(i)%x = node_gs(i)%x
    node_disk(i)%y = node_gs(i)%y

        k1_disk(i) = k1_gs(i)
        k2_disk(i) = k2_gs(i)
        k3_disk(i) = k3_gs(i)

   end do

   nnodes_disk = nnodes_gs

!  Copy the triangle data
!  This is the first one: theta = 0 to 60 degrees

   do i = 1, ntrias_gs
    tria_disk(i)%v    = tria_gs(i)%v
    tria_disk(i)%type = tria_gs(i)%type
   end do

   ntrias_disk = ntrias_gs

! Now generate other parts of the disk: i=1,2,3,4,5
! 1. theta =  60 to 120 degrees
! 2. theta = 120 to 180 degrees
! 3. theta = 180 to 240 degrees
! 4. theta = 240 to 300 degrees
! 5. theta = 300 to 360 degrees

! Each part has (n+1)*(n+2)/2 nodes: 1+2+3+...+(nr_gs+1) = (n_gs+1)*(n_gs+2)/2
  allocate(node_map((nr_gs + 1)*(nr_gs + 2)/2))

 new_sectors : do i = 1, 5

! (1)Generate new nodes and construct a node map (one to one)
!    inode = local node numbering for node_map(:)

  node_map(1) = 1 !Node at the origin

  do k = 2, nr_gs + 1 !Origin to outer boundary
   do j = 1, k        !Right to left

    inode = (k-1)*k/2 + j !Local node number = Right-most node + increment(1,2,..,k)

!   Right side nodes are existing nodes: Left side nodes of the previous sector
    if (j==1) then

     if     (i == 1) then
      node_map(inode) = (k-1)*k/2 + k         !Left-most node of the original sector
     elseif (i == 2) then
      node_map(inode) = nnodes_gs + (k-1)*k/2 !Left-most node of the second sector
     else
      node_map(inode) = nnodes_gs + (nnodes_gs-(nr_gs+1))*(i-2) + (k-1)*k/2
     endif

!   Left side of the last one is the right side of the original sector
    elseif (i==5 .and. j==k) then

      node_map(inode) = (k-1)*k/2 + 1         !Right-most node of the original sector

!   New nodes: Rotate the original nodes by theta = i*pi/3 (i times 60 degrees).
    else

     theta = (pi/three) * real(i,dp)
     nnodes_disk = nnodes_disk + 1
     node_map(inode) = nnodes_disk
     node_disk(nnodes_disk)%x = cos(theta)*node_gs(inode)%x - sin(theta)*node_gs(inode)%y
     node_disk(nnodes_disk)%y = sin(theta)*node_gs(inode)%x + cos(theta)*node_gs(inode)%y

     if (i==1) then

      k1_disk(nnodes_disk) = -k2_gs(inode)
      k2_disk(nnodes_disk) = -k3_gs(inode)
      k3_disk(nnodes_disk) = -k1_gs(inode)

     elseif(i==2) then

      k1_disk(nnodes_disk) =  k3_gs(inode)
      k2_disk(nnodes_disk) =  k1_gs(inode)
      k3_disk(nnodes_disk) =  k2_gs(inode)
 
     elseif(i==3) then
 
      k1_disk(nnodes_disk) = -k1_gs(inode)
      k2_disk(nnodes_disk) = -k2_gs(inode)
      k3_disk(nnodes_disk) = -k3_gs(inode)

     elseif(i==4) then
 
      k1_disk(nnodes_disk) =  k2_gs(inode)
      k2_disk(nnodes_disk) =  k3_gs(inode)
      k3_disk(nnodes_disk) =  k1_gs(inode)

     elseif(i==5) then
 
      k1_disk(nnodes_disk) = -k3_gs(inode)
      k2_disk(nnodes_disk) = -k1_gs(inode)
      k3_disk(nnodes_disk) = -k2_gs(inode)

     endif

!    Keep the record of corner nodes and their neighbors
     if (trim(remove_corner_nodes) == "yes") then
      if (j == k .and. k == nr_gs+1 .and. i < 5) nodes_removed(i+2)       = nnodes_disk
      if (j == k .and. k == nr_gs   .and. i < 5) nodes_removed_nghbr(i+2) = nnodes_disk
     endif

    endif
   end do
  end do

! (2)Generate triangles on the new sector.

  do k = 1, ntrias_gs
   ntrias_disk = ntrias_disk + 1
   tria_disk(ntrias_disk)%v    = node_map(tria_gs(k)%v)
   tria_disk(ntrias_disk)%type = tria_gs(k)%type
  end do

 end do new_sectors

  write(*,*) " nnodes_disk = ", nnodes_disk
  write(*,*) " ntrias_disk = ", ntrias_disk

!--------------------------------------------------------------------------------
! (If requested) Remove the six nodes (or equivalently remove 12 triangles)
! to avoid locally small cells typically caused by smoothing.
!
 if (trim(remove_corner_nodes) == "yes" .and. n_sections == 6) then

 remove_node : do k = 1, 6
    rnode = nodes_removed(k)
  newnode = nodes_removed_nghbr(k)

! (1)Remove the "rnode"

!  Move newnode to the boundary (rnode position)
   node_disk(newnode)%x = node_disk(rnode)%x
   node_disk(newnode)%y = node_disk(rnode)%y
!  Change the last node index to rnode to save/keep the last node
   node_disk(rnode)%x   = node_disk(nnodes_disk)%x
   node_disk(rnode)%y   = node_disk(nnodes_disk)%y
   nodes_changed(k,1)   = nnodes_disk
   nodes_changed(k,2)   = rnode
!  Reduce the dimension: "rnode" information is gone now.
   nnodes_disk = nnodes_disk - 1

! (2)Delete triangles with "rnode" and modify those having "nnodes_disk+1"

   i = 0
  do
   i = i + 1
   if ( i > ntrias_disk) exit

!  Delete if the triangle has "rnode" as its vertex.
   deleted = .false.
   if (tria_disk(i)%v(1) == rnode .or. &
       tria_disk(i)%v(2) == rnode .or. &
       tria_disk(i)%v(3) == rnode        ) then

       tria_disk(i)%v    = tria_disk(ntrias_disk)%v 
       tria_disk(i)%type = tria_disk(ntrias_disk)%type
       ntrias_disk = ntrias_disk - 1
       deleted = .true.
   endif

!  Check if the triangle has the node "nnodes_disk+1" (which no longer exists)
!  as its vertex. If so, overwrite it by rnode.
   if(.not.deleted) then

    if     (tria_disk(i)%v(1) == nnodes_disk+1) then
     tria_disk(i)%v(1) = rnode
    elseif (tria_disk(i)%v(2) == nnodes_disk+1) then
     tria_disk(i)%v(2) = rnode
    elseif (tria_disk(i)%v(3) == nnodes_disk+1) then
     tria_disk(i)%v(3) = rnode
    endif

   endif

!  If the triangle is removed, then shift the index to inspect the new
!  triangle ('i' is again a new triangle coming from ntrias_disk).
   if (deleted) i = i - 1

  end do

 end do remove_node

  write(*,*) " 6 corner nodes have been removed."
  write(*,*) " Updated nnodes_disk = ", nnodes_disk
  write(*,*) " Updated ntrias_disk = ", ntrias_disk

 endif

!--------------------------------------------------------------------------------
! (If requested) Apply smoothing on the disk triangulation

 write(*,*) " smoothing_type = ", smoothing_type

 smooth_it : if (smoothing_type /= 0 .and. n_sections == 6) then

  write(*,*) "Applying smoothing..."

  allocate(dxy(nnodes_disk,2))
  smoothing_factor = 0.05_dp
     max_smoothing = 1000

 smoothing : do i_smoothing = 1, max_smoothing

  dxy = zero !Initialize the changes

! Accumulate the changes by looping over triangles
  do i = 1, ntrias_disk

   v = tria_disk(i)%v

!  x-coordinate
    dxy(v(1),1) = dxy(v(1),1) + ( node_disk(v(2))%x - node_disk(v(1))%x )
    dxy(v(1),1) = dxy(v(1),1) + ( node_disk(v(3))%x - node_disk(v(1))%x )

    dxy(v(2),1) = dxy(v(2),1) + ( node_disk(v(1))%x - node_disk(v(2))%x )
    dxy(v(2),1) = dxy(v(2),1) + ( node_disk(v(3))%x - node_disk(v(2))%x )

    dxy(v(3),1) = dxy(v(3),1) + ( node_disk(v(1))%x - node_disk(v(3))%x )
    dxy(v(3),1) = dxy(v(3),1) + ( node_disk(v(2))%x - node_disk(v(3))%x )


!  y-coordinate
    dxy(v(1),2) = dxy(v(1),2) + ( node_disk(v(2))%y - node_disk(v(1))%y )
    dxy(v(1),2) = dxy(v(1),2) + ( node_disk(v(3))%y - node_disk(v(1))%y )

    dxy(v(2),2) = dxy(v(2),2) + ( node_disk(v(1))%y - node_disk(v(2))%y )
    dxy(v(2),2) = dxy(v(2),2) + ( node_disk(v(3))%y - node_disk(v(2))%y )


    dxy(v(3),2) = dxy(v(3),2) + ( node_disk(v(1))%y - node_disk(v(3))%y )
    dxy(v(3),2) = dxy(v(3),2) + ( node_disk(v(2))%y - node_disk(v(3))%y )

  end do

! Make changes to each node except the boundary nodes.
  dxy_norm = -1000000.0_dp
  do i=1, nnodes_disk

   if ( abs( sqrt(node_disk(i)%x**2 + node_disk(i)%y**2) - r_gs ) < 1.0e-14 ) then
   else

!  Constrained smoothing: skip nodes in the sector boundaries.
   if (smoothing_type == 2) then

    if ( abs(atan2(node_disk(i)%y, node_disk(i)%x) - pi/three)     < 1.0e-13 ) cycle
    if ( abs(atan2(node_disk(i)%y, node_disk(i)%x) + pi/three)     < 1.0e-13 ) cycle
    if ( abs(atan2(node_disk(i)%y, node_disk(i)%x) - two*pi/three) < 1.0e-13 ) cycle
    if ( abs(atan2(node_disk(i)%y, node_disk(i)%x) + two*pi/three) < 1.0e-13 ) cycle
    if ( abs(node_disk(i)%y) < 1.0e-13 ) cycle

   endif

     node_disk(i)%x = node_disk(i)%x + smoothing_factor * dxy(i,1)
     node_disk(i)%y = node_disk(i)%y + smoothing_factor * dxy(i,2)

!    L_inf norm of changes scaled by the typical mesh spacing, dr_gs.
     dxy_norm = max( dxy_norm, abs(dxy(i,1)**2 + dxy(i,2)**2)/dr_gs )

   endif

 end do

! Exit if converged
  if ( dxy_norm < 1.0e-04) then
   write(*,*) " Smoothing converged at ", i_smoothing
   exit smoothing
  elseif (i_smoothing == max_smoothing) then
   write(*,*) " Smoothing didn't converge... ", "  dxy_norm = ", dxy_norm
  endif

 end do smoothing

 deallocate(dxy)

 endif smooth_it

 deallocate(k1_gs,k2_gs,k3_gs,node_gs,tria_gs,node_map)

!--------------------------------------------------------------------------------

! Now, at this point, we have a triangulation of a disk defined by
! tria_disk and node_disk.
! Number of triangles = ntrias_disk
! Number of nodes     = nnodes_disk

!*******************************************************************************
! Write a Tecplot file for the triangulated disk.
!******************************************************************************
 debug_mode_02 : if (debug_mode) then
 open(unit=2, file=filename_disk, status="unknown", iostat=os)

  write(2,*) 'TITLE = "GRID"'
  write(2,*) 'VARIABLES = "x","y","z","k1","k2","k3","kd"'
  write(2,*) 'ZONE  N=', nnodes_disk,',E=', ntrias_disk,' , ET=triangle, F=FEPOINT'

! Nodes
  do i = 1, nnodes_disk

   sk12p = ( 1 + int_sign( k1_disk(i)*k2_disk(i) ) )/2
   sk23p = ( 1 + int_sign( k2_disk(i)*k3_disk(i) ) )/2
   sk31p = ( 1 + int_sign( k3_disk(i)*k1_disk(i) ) )/2

   sk12m = ( 1 - int_sign( k1_disk(i)*k2_disk(i) ) )/2
   sk23m = ( 1 - int_sign( k2_disk(i)*k3_disk(i) ) )/2
   sk31m = ( 1 - int_sign( k3_disk(i)*k1_disk(i) ) )/2

   if     (sk12p > 0) then

     sk23p = 0
     sk23m = 1

     sk31p = 0
     sk31m = 1

   elseif (sk23p > 0) then

     sk12p = 0
     sk12m = 1

     sk31p = 0
     sk31m = 1

   elseif (sk31p > 0) then

     sk12p = 0
     sk12m = 1

     sk23p = 0
     sk23m = 1

   endif

    write(2,'(3ES20.10,4i10)') node_disk(i)%x, node_disk(i)%y, 0.0, k1_disk(i),k2_disk(i),k3_disk(i), &
    sk31m*sk23m*sk12p*abs(k3_disk(i)) + &
    sk31m*sk12m*sk23p*abs(k1_disk(i)) + &
    sk23m*sk12m*sk31p*abs(k2_disk(i))

  end do

! Triangles
  do i = 1, ntrias_disk
   write(2,'(3I10)') tria_disk(i)%v(1), tria_disk(i)%v(2), tria_disk(i)%v(3)
  end do

 close(2)

 write(*,*)
 write(*,*) "Tecplot file has been written: ", filename_disk
 endif debug_mode_02
!*******************************************************************************

!*******************************************************************************
! 3. Map the disk triangulation onto the hemisphere surface.
!
! OK, now, let's gently attach the disk onto the hemisphere.
! This is done locally at each node, just by using the node_disk data.
! It doesn't matter how they are ordered.
! Connectivity data are unchanged.
!*******************************************************************************

  write(*,*)
  write(*,*) "***********************************************************"
  write(*,*) " 3. Place the disk onto the hemisphere "
  write(*,*) "***********************************************************"

  nnodes_circum = n_sections*nr_gs

  if (n_sections == 1) nnodes_circum = nnodes_circum + 1

  nnodes = nnodes_disk
  allocate(node_body_hs(nnodes))
  allocate(k1_body_hs(nnodes))
  allocate(k2_body_hs(nnodes))
  allocate(k3_body_hs(nnodes))
  allocate(k4_body_hs(nnodes))

   nnodes_hs = 0

  do i = 1, nnodes_disk

   k1_body_hs(i) = k1_disk(i) ! Flip the sign JFC
   k2_body_hs(i) = k2_disk(i) ! Flip the sign JFC
   k3_body_hs(i) = k3_disk(i) ! Flip the sign JFC
   k4_body_hs(i) =  0          ! <- On the hemisphere

! Push the node onto the circle located at z=0.0.
   s = sqrt(node_disk(i)%x**2 + node_disk(i)%y**2)
   if (i==1) then
    xp = zero
    yp = zero
   else
    xp = node_disk(i)%x/s * Rd !Extend it to the circle of radius Rd
    yp = node_disk(i)%y/s * Rd !Extend it to the circle of radius Rd
   endif

!    zp = 0.0  The circle is located at x = 0.0.

! Now, the node (xp,yp,zp) is located on the perimeter of the circle at z=0.0.
! Rotate the node onto the sphere.
            theta = s/Rd
           nnodes_hs = nnodes_hs + 1
   node_body_hs(nnodes_hs)%x = xp*sin(theta)
   node_body_hs(nnodes_hs)%y = yp*sin(theta)
   node_body_hs(nnodes_hs)%z = Rd*cos(theta)


! Rotate the hemisphere by 30 degrees.
!  do i = 1, nnodes_hs
   theta = pi/six
   xc = node_body_hs(i)%x
   yc = node_body_hs(i)%y
   node_body_hs(i)%x = cos(theta)*xc - sin(theta)*yc
   node_body_hs(i)%y = sin(theta)*xc + cos(theta)*yc
!  end do


!  Surface normal direction along which we go up to generate prismatic elements.
   node_body_hs(nnodes_hs)%nx = node_body_hs(nnodes_hs)%x
   node_body_hs(nnodes_hs)%ny = node_body_hs(nnodes_hs)%y
   node_body_hs(nnodes_hs)%nz = node_body_hs(nnodes_hs)%z

!  Make it the unit vector (well, probably already a unit vector, though...)
   sf = sqrt(node_body_hs(nnodes_hs)%nx**2 + node_body_hs(nnodes_hs)%ny**2 + node_body_hs(nnodes_hs)%nz**2)
   node_body_hs(nnodes_hs)%nx = node_body_hs(nnodes_hs)%nx / sf
   node_body_hs(nnodes_hs)%ny = node_body_hs(nnodes_hs)%ny / sf
   node_body_hs(nnodes_hs)%nz = node_body_hs(nnodes_hs)%nz / sf

  end do

 deallocate(k1_disk,k2_disk,k3_disk,node_disk)
 write(*,*) " nnodes_hs, nnodes_disk = ", nnodes_hs, nnodes_disk

!*******************************************************************************
! Write a Tecplot file for the triangulated hemisphere surface.
!******************************************************************************
 debug_mode_03 : if (debug_mode) then
 open(unit=3, file=filename_surface, status="unknown", iostat=os)

  write(3,*) 'TITLE = "GRID"'
  write(3,*) 'VARIABLES = "x","y","z","k1+k2"'
  write(3,*) 'ZONE  N=', nnodes_hs,',E=', ntrias_disk,' , ET=triangle, F=FEPOINT'

! Nodes
  do i = 1, nnodes_hs

    write(3,'(3ES20.10,i10)') node_body_hs(i)%x,  node_body_hs(i)%y, node_body_hs(i)%z, &
                          k1_body_hs(i)+ k2_body_hs(i)
  end do

! Triangles
  do i = 1, ntrias_disk
   write(3,'(3I10)') tria_disk(i)%v(1), tria_disk(i)%v(2), tria_disk(i)%v(3)
  end do

 close(3)

 write(*,*)
 write(*,*) "Tecplot file has been written: ", filename_surface
 endif debug_mode_03
!*******************************************************************************

!*******************************************************************************
! 4. Create some temporary data, and modify some data.
!
!    At this point, the surface grid is still a hemisphere.
!
!                  z
!                  ^
!        o  o      |
!     o        o   |
!    o__________o  -------> x
!               
!
!*******************************************************************************
!-------------------------------------------------------------------------------
! Construct a 2D node list around the circle of the bottom of the hemisphere.
!
!  nodes_circum(k), k=1,nnodes_circum
!
!         <-
!        o  o    
!     o        o     y
!    o          o    ^
!    o          o    |
!     o        o     |
!        o  o        -------> x
!         ->

  allocate(nodes_circum(nnodes_circum+1))
  write(*,*) " nnodes around the circle of the hemisphere = ", nnodes_circum

  nnodes_circum = 0

! Circum of generating sector: Counter-clockwise ordering.
!
  do k = 1, nr_gs+1
   nnodes_circum = nnodes_circum + 1
   nodes_circum(nnodes_circum) = nr_gs*(nr_gs+1)/2 + k
  end do

! Circum of other sectors
  do i = 1, 4
   do k = 1, nr_gs
    nnodes_circum = nnodes_circum + 1
    nodes_circum(nnodes_circum) = nnodes_gs + (i-1)*(nr_gs+1)*nr_gs/2 + (nr_gs-1)*nr_gs/2 + k
   end do
  end do

! i == 5
   do k = 1, nr_gs-1
    nnodes_circum = nnodes_circum + 1
    nodes_circum(nnodes_circum) = nnodes_gs + (5-1)*(nr_gs+1)*nr_gs/2 + (nr_gs-2)*(nr_gs-1)/2 + k
   end do

  nodes_circum(nnodes_circum + 1) = nodes_circum(1)

! Fix the node number if 6 nodes have been removed.

  if (trim(remove_corner_nodes) == "yes" .and. n_sections == 6) then

   do i = 1, nnodes_circum + 1

    do k = 1, 6

     if (nodes_circum(i) == nodes_removed(k)) then

      nodes_circum(i) = nodes_removed_nghbr(k)

     elseif (nodes_circum(i) == nodes_changed(k,1)) then

      nodes_circum(i) = nodes_changed(k,2)

     endif

    end do

   end do

  endif

!-------------------------------------------------------------------------------
! Generate new array for triangles.

  ntrias_hs = ntrias_disk
  allocate(tria_hs(ntrias_hs))

! Copy the data for triangles.
! Reverse the node-ordering.
! Triangles are now ordered in counter-clockwise manner,
! so that it points towards inside the domain.
!
!                            1
!                            o
!  Interior domain         . .
!                     <---.- .
!                        .   .
!                       o----o
!                      2     3

  do i = 1, ntrias_hs

   node1 = tria_disk(i)%v(1)
   node2 = tria_disk(i)%v(2)
   node3 = tria_disk(i)%v(3)

    tria_hs(i)%v(1) = node3
    tria_hs(i)%v(2) = node2
    tria_hs(i)%v(3) = node1

   tria_hs(i)%type = tria_disk(i)%type

  end do

  deallocate(tria_disk)

!*******************************************************************************
! Generate a spherical surface grid by duplicating and combining the hemisphere
! surface grids. 
!
!                  z
!                  ^
!        o  o      |
!     o        o   |
!    o__________o  -------> x
!    o          o
!     o        o
!        o  o
!               
!
!*******************************************************************************

  write(*,*)
  write(*,*) "***********************************************************"
  write(*,*) " 4. Generate a spherical surface grid by copy and combine "
  write(*,*) "***********************************************************"

  nnodes = 2*nnodes_hs - nnodes_circum
  allocate(node_body(nnodes))
  allocate(k1_body(nnodes))
  allocate(k2_body(nnodes))
  allocate(k3_body(nnodes))
  allocate(k4_body(nnodes))

! Copy the hemisphere surface data.

  do i = 1, nnodes_hs

   node_body(i)%x   = node_body_hs(i)%x
   node_body(i)%y   = node_body_hs(i)%y
   node_body(i)%z   = node_body_hs(i)%z

   node_body(i)%nx  = node_body_hs(i)%nx
   node_body(i)%ny  = node_body_hs(i)%ny
   node_body(i)%nz  = node_body_hs(i)%nz

   node_body(i)%nx2 = node_body_hs(i)%nx2
   node_body(i)%ny2 = node_body_hs(i)%ny2
   node_body(i)%nz2 = node_body_hs(i)%nz2

   k1_body(i) = k1_body_hs(i)
   k2_body(i) = k2_body_hs(i)
   k3_body(i) = k3_body_hs(i)
   k4_body(i) = k4_body_hs(i)

  end do

  ntrias = 2*ntrias_hs
  allocate(tria(ntrias))

  do i = 1, ntrias_hs
   tria(i)%v(1) = tria_hs(i)%v(1)
   tria(i)%v(2) = tria_hs(i)%v(2)
   tria(i)%v(3) = tria_hs(i)%v(3)
   tria(i)%type = tria_hs(i)%type
  end do

! Generate the other half:
!   Flip the hemisphere wrt z-axis and add it to
!   the original hemisphere.

  do i = 1, nnodes_hs
   node_body_hs( i )%circum_node = .false.
  end do

  do i = 1, nnodes_circum
   node_body_hs( nodes_circum(i) )%circum_node = .true.
  end do

  allocate(node_map(nnodes_hs))

  nnodes = nnodes_hs
 
  do i = 1, nnodes_hs

   if ( node_body_hs(i)%circum_node ) then
    node_map(i)           = i
    cycle
   endif

                  nnodes =   nnodes + 1
   node_map(i)           =   nnodes

   node_body(nnodes)%x   =   node_body_hs(i)%x
   node_body(nnodes)%y   =   node_body_hs(i)%y
   node_body(nnodes)%z   = - node_body_hs(i)%z ! Flip

   node_body(nnodes)%nx  =   node_body_hs(i)%nx
   node_body(nnodes)%ny  =   node_body_hs(i)%ny
   node_body(nnodes)%nz  = - node_body_hs(i)%nz ! Flip

   node_body(nnodes)%nx2 =   node_body_hs(i)%nx2
   node_body(nnodes)%ny2 =   node_body_hs(i)%ny2
   node_body(nnodes)%nz2 = - node_body_hs(i)%nz2 ! Flip

   k1_body(nnodes) = k1_body_hs(i)
   k2_body(nnodes) = k2_body_hs(i)
   k3_body(nnodes) = k3_body_hs(i)
   k4_body(nnodes) = k4_body_hs(i)

  end do

  ntrias = ntrias_hs

  do i = 1, ntrias_hs

         ntrias = ntrias + 1

   tria(ntrias)%v(1) = node_map( tria_hs(i)%v(1) )
   tria(ntrias)%v(2) = node_map( tria_hs(i)%v(3) )
   tria(ntrias)%v(3) = node_map( tria_hs(i)%v(2) )
   tria(ntrias)%type = tria_hs(i)%type

  end do

! Rotate the sphere wrt y-axis: 90 degrees anticlockwise in (x,z) plane around the origin.

  do i = 1, nnodes

   theta = pi/two
   xc = node_body(i)%x
   zc = node_body(i)%z
   node_body(i)%x = cos(theta)*xc - sin(theta)*zc
   node_body(i)%z = sin(theta)*xc + cos(theta)*zc

!  Surface normal direction along which we go up to generate prismatic elements.
   node_body(i)%nx = node_body(i)%x
   node_body(i)%ny = node_body(i)%y
   node_body(i)%nz = node_body(i)%z

!  Make it the unit vector (well, probably already a unit vector, though...)
   sf = sqrt(node_body(i)%nx**2 + node_body(i)%ny**2 + node_body(i)%nz**2)
   node_body(i)%nx = node_body(i)%nx / sf
   node_body(i)%ny = node_body(i)%ny / sf
   node_body(i)%nz = node_body(i)%nz / sf

  end do

  deallocate(node_body_hs,tria_hs,k1_body_hs,k2_body_hs,k3_body_hs,k4_body_hs)

!--------------------------------------------------------------------------------
! (If requested) Apply smoothing on the surface triangulation

 smooth_it_surface : if (smoothing_type /= 0 .and. n_sections == 6) then

  write(*,*)
  write(*,*) "  Applying smoothing on the sphere surface..."

  allocate(dxyz(nnodes,3))
  smoothing_factor = 0.05_dp
     max_smoothing = 1000

 smoothing_surface : do i_smoothing = 1, max_smoothing

  dxyz = zero !Initialize the changes

! Accumulate the changes by looping over triangles
  do i = 1, ntrias

   v = tria(i)%v

!  x-coordinate
    dxyz(v(1),1) = dxyz(v(1),1) + ( node_body(v(2))%x - node_body(v(1))%x )
    dxyz(v(1),1) = dxyz(v(1),1) + ( node_body(v(3))%x - node_body(v(1))%x )

    dxyz(v(2),1) = dxyz(v(2),1) + ( node_body(v(1))%x - node_body(v(2))%x )
    dxyz(v(2),1) = dxyz(v(2),1) + ( node_body(v(3))%x - node_body(v(2))%x )

    dxyz(v(3),1) = dxyz(v(3),1) + ( node_body(v(1))%x - node_body(v(3))%x )
    dxyz(v(3),1) = dxyz(v(3),1) + ( node_body(v(2))%x - node_body(v(3))%x )


!  y-coordinate
    dxyz(v(1),2) = dxyz(v(1),2) + ( node_body(v(2))%y - node_body(v(1))%y )
    dxyz(v(1),2) = dxyz(v(1),2) + ( node_body(v(3))%y - node_body(v(1))%y )

    dxyz(v(2),2) = dxyz(v(2),2) + ( node_body(v(1))%y - node_body(v(2))%y )
    dxyz(v(2),2) = dxyz(v(2),2) + ( node_body(v(3))%y - node_body(v(2))%y )

    dxyz(v(3),2) = dxyz(v(3),2) + ( node_body(v(1))%y - node_body(v(3))%y )
    dxyz(v(3),2) = dxyz(v(3),2) + ( node_body(v(2))%y - node_body(v(3))%y )


!  z-coordinate
    dxyz(v(1),3) = dxyz(v(1),3) + ( node_body(v(2))%z - node_body(v(1))%z )
    dxyz(v(1),3) = dxyz(v(1),3) + ( node_body(v(3))%z - node_body(v(1))%z )

    dxyz(v(2),3) = dxyz(v(2),3) + ( node_body(v(1))%z - node_body(v(2))%z )
    dxyz(v(2),3) = dxyz(v(2),3) + ( node_body(v(3))%z - node_body(v(2))%z )

    dxyz(v(3),3) = dxyz(v(3),3) + ( node_body(v(1))%z - node_body(v(3))%z )
    dxyz(v(3),3) = dxyz(v(3),3) + ( node_body(v(2))%z - node_body(v(3))%z )

  end do

! Make changes to each node except the boundary nodes.
  dxyz_norm = -1000000.0_dp
  do i = 1, nnodes

     xc = node_body(i)%x
     yc = node_body(i)%y
     zc = node_body(i)%z

     node_body(i)%x = node_body(i)%x + smoothing_factor * dxyz(i,1)
     node_body(i)%y = node_body(i)%y + smoothing_factor * dxyz(i,2)
     node_body(i)%z = node_body(i)%z + smoothing_factor * dxyz(i,3)

   !Project back onto the surface.
     sf = sqrt(node_body(i)%x**2 + node_body(i)%y**2 + node_body(i)%z**2)
     node_body(i)%x = node_body(i)%x/sf * Rd
     node_body(i)%y = node_body(i)%y/sf * Rd
     node_body(i)%z = node_body(i)%z/sf * Rd

!    L_inf norm of changes.
     dxyz_norm = max( dxyz_norm, abs((xc-node_body(i)%x)**2         &
               + (yc-node_body(i)%y)**2 + (zc-node_body(i)%z)**2) )

 end do

! Exit if converged
  if ( dxyz_norm < 1.0e-04) then
   write(*,*) " Smoothing converged at ", i_smoothing
   exit smoothing_surface
  elseif (i_smoothing == max_smoothing) then
   write(*,*) " Smoothing didn't converge... ", "  dxyz_norm = ", dxyz_norm
  endif

 end do smoothing_surface

 deallocate(dxyz)

  write(*,*)

! Re-compute the normals.

  do i = 1, nnodes
   node_body(i)%nx = node_body(i)%x
   node_body(i)%ny = node_body(i)%y
   node_body(i)%nz = node_body(i)%z
   sf = sqrt(node_body(i)%nx**2 + node_body(i)%ny**2 + node_body(i)%nz**2)
   node_body(i)%nx = node_body(i)%nx / sf
   node_body(i)%ny = node_body(i)%ny / sf
   node_body(i)%nz = node_body(i)%nz / sf
  end do

 endif smooth_it_surface

!*******************************************************************************
! Write a Tecplot file for the triangulated sphere surface.
!******************************************************************************
 debug_mode_033 : if (debug_mode) then
 open(unit=3, file=filename_surface_s, status="unknown", iostat=os)

  write(3,*) 'TITLE = "GRID"'
  write(3,*) 'VARIABLES = "x","y","z","k1+k2"'
  write(3,*) 'ZONE  N=', nnodes,',E=', ntrias,' , ET=triangle, F=FEPOINT'

! Nodes
  do i = 1, nnodes

    write(3,'(3ES20.10,i10)') node_body(i)%x,  node_body(i)%y, node_body(i)%z, &
                          k1_body(i)+ k2_body(i)
  end do

! Triangles
  do i = 1, ntrias
   write(3,'(3I10)') tria(i)%v(1), tria(i)%v(2), tria(i)%v(3)
  end do

 close(3)

 write(*,*)
 write(*,*) "Tecplot file has been written: ", filename_surface_s
 endif debug_mode_033
!*******************************************************************************

!*******************************************************************************
!
! Contrustion type == 1. Old code.
!
! 5. We now go up to generate the interior nodes.
!
!                  z
!                  ^
!        o  o      |
!     o        o   -------> x
!    o__________o------------------------------->o   Outer boundary point.
!    o          o  Go up and generate nodes ->
!     o        o
!        o  o
!
! Determine the vertical spacing, vspacing: r(i+1) = vspacing(i) + r(i),
! and generate interior nodes by going up in the direction of surface normal.
!
!   Geometric sequence       Exponential stretching
! o-o--o---o----o-----o-----x-------x----------x--------------x        -----> r
! 1 2  3              nm                                 nr
!   Prismatic layer       Outer tetrahedral region
!
! NOTE: Here, r indicates the direction of surface normal.
! NOTE: The number of nodes in the prismatic layer (nm) is automatically determined
!       for a specified the aspect ratio of the first element on the body = 50
!
! NOTE: The exponential stretching is determined by iterations to equate
!       the spacing of the elements below and above the nm-th node.
!
!*******************************************************************************

 construction : if (construction_type == 1) then

  write(*,*) "***********************************************************"
  write(*,*)
  write(*,*) " 5. Go up to the outer boundary and generate interior nodes"
  write(*,*)
  write(*,*) " Construction-type == 1"
  write(*,*)
  write(*,*) "***********************************************************"

!  Estimate of the arc length of an edge along the hemisphere.

    ds = (half*pi*Rd) / real(nr_gs,dp)
    write(*,*) "Mesh spacing along the hemisphere (body)= ", ds

!  Estimate of the arc length of one cell along the outer boundary.

    ds_outer = distance*(ds/Rd)
    write(*,*) "Mesh spacing along the hemisphere (farfirled) = ", ds_outer

! rmax = thickness of the prismatic region (boundary layer):
! If a negative value is given, we set this to be 0.1:
  if (rmax < zero) rmax = 1.0e-01_dp

  write(*,*) " Boundary layer grid thickness = ", rmax

! Compute the parameters for geometric sequence.
! Determine the number of nodes in the r-direction for a given max AR.

  drN = ds !* 0.5_dp !Spacing of the outer most cell in the layer.

  if (drN > rmax) then
   write(*,*) ">>>>> drN > rmax !! Impossible... drN = ds = ", drN
   drN = rmax *0.5_dp
   write(*,*) ">>>>> Modified drN = rmax *0.5_dp = ", drN
  endif

!---------------------------------------------------------------------
! dr1 is the first vertical spacing off the wall.

  set_dr1 : if ( target_reynolds_number > zero) then

   write(*,*) ">>>>>>>>>>>>>>> dr1 from a target Re and y_plus:"
    cf = 0.026_dp/target_reynolds_number**(1.0_dp/7.0_dp)
   dr1 = ( sqrt(2.0_dp/cf)/target_reynolds_number) * target_yplus
   write(*,*) "--- dr1 determined by the target Re and y_plus:"
   write(*,*) " target_reynolds_number = ", target_reynolds_number
   write(*,*) " target_yplus = ", target_yplus
   write(*,*) " --> dr1 = ", dr1
   write(*,*) "      AR = ", ds/dr1
   write(*,*) ">>>>>>>>>>>>>>> "

  else

   dr1 = target_yplus

  endif set_dr1
!---------------------------------------------------------------------

   if (dr1 > rmax) then
    dr1 = 0.1_dp * ds
    write(*,*) " dr1 too large: >> rmax = ", rmax
    write(*,*) " --> Adjusted: dr1 = ", dr1 
   endif

   gr = (rmax-dr1)/(rmax-drN)
   write(*,*) "      gr = ", gr
   nm = ceiling( log(gr*drN/dr1)/log(gr) )
   gr = (drN/dr1)**( one/(real(nm,dp)-one) )

   write(*,*)
   write(*,*) " NOTE: dr1 is a non-dimensionalized length. "
   write(*,*) "       Reference length is the diameter of the hemisphere (=1.0 in the grid)."
   write(*,*)

   write(*,*) "ds (mesh spacing along the hemisphere) = ", ds
   write(*,*) "rmax (the end of the boundary layer)      = ", rmax
   write(*,*) "dr1 = ", dr1
   write(*,*) "drN = ", drN
   write(*,*) "gr = ", gr
   write(*,*) "dr1*gr**(nm-1) = ", dr1*gr**(nm-1)

!  At this point, nm is the number of elements within the layer.
!  Add 1 to make it the number of nodes.
   nm = nm + 1 ! where change the type of cell from prism to tetra for a mixed grid.
   write(*,*) "nm (nodes) = ", nm

!  Allocate the vertical spacing array
   allocate(vspacing_nm(nm-1))

  do i = 1, nm-1

   if (i == 1) then

    vspacing_nm(i) = dr1

!  Geometric sequence for the cell spacing inside the layer.
   elseif (i < nm) then

    vspacing_nm(i) = dr1*gr**(i-1)

   endif

  end do

  write(*,*)

! NOTE: The last node in the prismatic layer is nm-th node.
!       The spacing array vspacing() goes from 1 to nm-1.

!  Take a guess on the number of nodes in the outer tet region.

     nr = nm + int(ceiling( (distance - rmax) / ds )/25.0_dp)

   write(*,*) "Initial value of nr = ", nr

   rs_nmm1 = zero
  do i = 1, nm-2
   rs_nmm1 = rs_nmm1 + vspacing_nm(i)
  end do

! Uniform spacing coordinate in the outer region for now.
   rs_nm   = rs_nmm1 + vspacing_nm(nm-1)
  write(*,*) "  rs_nmm1 = ", rs_nmm1
  write(*,*) "  rs_nm   = ", rs_nm

  write(*,*)
  write(*,*) ">>>>>>>>>>>>>>> Start iteration for nr."
!-------------------------------------------------------------------
 determine_nr_itaratively : do i_nr = 1, 800
!-------------------------------------------------------------------

  write(*,*)

   dr_outer = (distance - rs_nm)/real(nr-nm,dp)
   rs_nmp1 = rs_nm   + dr_outer
  write(*,*) "  rs_nmp1 = ", rs_nmp1

!-------------------------------------------------------------------
! Compute the stretching factor for a smooth transition from
! the boundary layer to outer region.
! Target spacing: 1.2 times the spacing below for a smooth transition

    s = (rs_nm-rs_nmm1) * 1.2_dp
    write(*,*) "  s = (rs_nm-rs_nmm1) = ", s

!  Initial value
   sf = 2.0_dp

!  Determine sf iteratively
   do k = 1, 500
    drnmp1 = (one-exp(sf*(rs_nmp1-rs_nm)/(distance-rs_nm)))/(one-exp(sf)) * ((distance-Rd)-rs_nm)
    if (abs(drnmp1 - s)/s < 1.0e-02_dp) exit
    if (drnmp1 > s ) then
      sf = sf + 0.01_dp
    else
      sf = sf - 0.01_dp
    endif
   end do

!-------------------------------------------------------------------

  write(*,*) "Determined stretching factor = ",sf, " at iterations = ", k

  rs_nr   = distance-Rd
! rs_nrm1 = rs_nr - dr_outer !<- This was a wrong value...
  rs_nrm1 = distance-rs_nm - dr_outer
  rs_nrm1 = rs_nm + (one-exp(sf*(rs_nrm1-rs_nm)/(distance-rs_nm)))/(one-exp(sf)) * ((distance-Rd)-rs_nm)

  write(*,*) "  rs_nr   = ", rs_nr
  write(*,*) "  rs_nrm1 = ", rs_nrm1

  dr_spacing = rs_nr - rs_nrm1
  write(*,*) "  dr_spacing (cell spacing at outer boudnary)   = ", dr_spacing 
  write(*,*) "    ds_outer (cell spacing at outer boudnary)   = ", ds_outer
  err_dr_spacing = (dr_spacing-ds_outer)/ds_outer
  write(*,*) "  Difference(dr_spacing,ds_outer)=", err_dr_spacing

  if (abs(err_dr_spacing) < 1.0e-08_dp) then
   write(*,*)
   write(*,*) " Iteration converged for nr: final nr = ", nr
   exit determine_nr_itaratively
  endif

 !If not satisfactory, adjust nr and try again.
  if (err_dr_spacing < 0.0_dp) then
   nr = nr - 1
   write(*,*) " Decrease nr = ", nr
  else
   nr = nr + 1
   write(*,*) " Increase nr = ", nr
  endif

!-------------------------------------------------------------------
  end do determine_nr_itaratively
!-------------------------------------------------------------------

  write(*,*)
  write(*,*) ">>>>>>>>>>>>>>> End of iteration for nr."

! Create a vertical coordinate array.
! Note: rs=0        on the hemisphere surface.
!       rs=distance at the outer boudnary.
  allocate(rs(nr))
  rs(1) = zero

! Compute the vertical coordinate based on the spacing generated by
! the geometric sequence as above.
  do i = 2, nm
   rs(i) = vspacing_nm(i-1) + rs(i-1)
  end do

  allocate(vspacing(nr))
  do i = 1, nm-1
   vspacing(i) = vspacing_nm(i)
  end do
   vspacing(nm) = vspacing_nm(nm-1)

  deallocate(vspacing_nm)

! Uniform spacing coordinate:
   dr_outer = (distance - rs(nm))/real(nr-nm,dp)
  do i = nm+1, nr
   rs(i) = rs(nm) + real(i-nm,dp)*dr_outer
  end do

  do i = nm+1, nr
   rs(i) = rs(nm) + (one-exp(sf*(rs(i)-rs_nm)/(distance-rs_nm)))/(one-exp(sf)) * ((distance-Rd)-rs(nm))
  end do
  !Adjust the last one, so that we have the outer boundary precisely as specified.
  ! rs(nr) = distance-Rd

! Re-compute the spacing modified by the stretching.
  do i = nm+1, nr
   vspacing(i-1) = rs(i) - rs(i-1)
  end do

!*******************************************************************************
!
! Contrustion type == 2. Simple method.
!
! 5. We now go up to generate the interior nodes.
!
!                  z
!                  ^
!        o  o      |
!     o        o   -------> x
!    o__________o------------------------------->o   Outer boundary point.
!    o          o  Go up and generate nodes ->
!     o        o
!        o  o
!
! Determine the vertical spacing, vspacing: r(i+1) = vspacing(i) + r(i),
! and generate interior nodes by going up in the direction of surface normal.
!
! Constant spacing       Exponential stretching (or constant spacing)
! o--o--o--o--o--o-----x-------x----------x------------------------x      ---  -----> r
! 1  2  3        nm   nm+1                                         nr
! <-Inner layers-><-----             Outer region              --->
!
! NOTE: Here, r indicates the direction of surface normal.
!
! NOTE: The exponential stretching is determined by iterations to equate
!       the spacing of the elements below and above the nm-th node.
!
!*******************************************************************************

 else

  write(*,*) "***********************************************************"
  write(*,*)
  write(*,*) " 5. Go up to the outer boundary and generate interior nodes"
  write(*,*)
  write(*,*) " Construction-type == 2" 
  write(*,*)
  write(*,*) "***********************************************************"

! Total number of nodes

  nr = (n_dz_layer + 1) + n_outer_layer

! Create a vertical coordinate array.
! Note: rs=0        on the sphere surface.
!       rs=distance at the outer boudnary.

  allocate(rs(nr))

  rs(1) = zero

!--------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------
! (1) Compute the vertical coordinate based on the input dz and n_dz_layer.

  do i = 2, n_dz_layer + 1
   rs(i) = rs(i-1) + dz
  end do

! If a mixed grid is requested, we put prisms in this latyer.
! nm is the # of nodes up to the node that divides prism and tet.
  nm = n_dz_layer + 1

  write(*,*)
  write(*,*) "    nm  = ", nm
  write(*,*) " rs(nm) = ", rs(nm)

! Compute the vertical spacing.

  allocate(vspacing(nr))

  do i = 1, nm-1
   vspacing(i) = dz
  end do

! Up to this point, we have points up to i=nm. The end of the fisrt layer (or 
! a prismatic layer). And vspacein(1:nm-1).

  write(*,*)
  write(*,*) " The inner layers begin at r = ", Rinner + rs( 1), " with the initial spacing = ", vspacing(1)
  write(*,*)
  write(*,*) " The inner layers end   at r = ", Rinner + rs(nm), " with the last    spacing = ", vspacing(nm-1)
  write(*,*)

!--------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------
! (2) Outer region

 !----------------------------------------------------
 !----------------------------------------------------
 ! Option 1: Specified distance
 
  if (Router > 0.0_dp) then

   dr1 = ( (distance-Rinner) - rs(nm) )/real(n_outer_layer,dp)

   write(*,*)
   write(*,*) " n_outer_layer = ", n_outer_layer
   write(*,*) "     distance  = ", distance
   write(*,*) "       rs(nm)  = ", rs(nm)
   write(*,*) "           dz  = ", dz
   write(*,*) "           dr1 = ", dr1
   write(*,*)

  !-------------------------------------------------------------------------------------
  !(1-1) Apply stretching if the uniform spacing is larger than the inner spacing dz.

   if ( dr1 > dz ) then 

    !First, we need to determine the stretching factor.
    !solve target_x - cx*(one-exp(sf*xi))/(one-exp(sf)) = 0 for sf.

    write(*,*) " We may apply stretching....."
    write(*,*) " ----- Determine the stretching factor..."

            dr1 = 1.1_dp * dz                   !Target spacing = 1.1*dz
             cx = (distance-Rinner) - rs(nm)    !Total distance to be covered.
             xi = 1.0_dp/real(n_outer_layer,dp) !First increment in xi=[0,1]
     sf_initial = 1.0_dp                        !Initial stretching factor

     call compute_stretching_factor(dr1,cx,xi,sf_initial, final_x,sf,itr,stat )

     write(*,*)
     write(*,*) "      cx = ", cx
     write(*,*) "     itr = ", itr
     write(*,*) " final_x = ", final_x 
     write(*,*) "     sf  = ", sf

     write(*,*) " xi=1/n: rs(nm) +cx*(one-exp(sf*xi    ))/(one-exp(sf)) = ", &
                          rs(nm) +cx*(one-exp(sf*xi    ))/(one-exp(sf))
     write(*,*) " xi=1  : rs(nm) +cx*(one-exp(sf*1.0_dp))/(one-exp(sf)) = ", &
                          rs(nm) +cx*(one-exp(sf*1.0_dp))/(one-exp(sf))

   !Generate nodal coordinates in the outer layer: i=nm+1 to i=nr.
    do i = nm+1, nr
              xi = real(i-nm,dp)/real(n_outer_layer,dp) !i-nm = nm to nr-nm=n_outer_layer
           rs(i) = rs(nm) + cx*(one-exp(sf*xi))/(one-exp(sf))
    end do

  !-------------------------------------------------------------------------------------
  !(1-2) Stretching doesn't make sense (too many cells). Use uniform spacing.
   else

    write(*,*) " No stretching... Too many elements left. Use uniform in the outer"

    do i = nm+1, nr
           rs(i) = rs(nm) + real(i-nm,dp)*dr1
    end do    

   endif
  !-------------------------------------------------------------------------------------

   !Compute and save the spacings in the outer region.
    do i = nm, nr-1
     vspacing(i) = rs(i+1)-rs(i)
    end do

 !----------------------------------------------------
 !----------------------------------------------------
 ! Option 2: Specified growth rate

  elseif (Router < 0.0_dp .and. growth_rate > 0.0_dp ) then

  !Just define the spacings that increase at the specified rate.

   write(*,*) " Using a specified growth rate: (rip1-ri) = growth_rate*(ri-rim1)"

   do i = nm, nr-1
    vspacing(i) = growth_rate * vspacing(i-1) ! e.g., growth_rate = 1.2
        rs(i+1) = rs(nm) + vspacing(i)
   end do
    
 !----------------------------------------------------
 !----------------------------------------------------
 ! Option 3: Just uniform up to Router with the # of
 ! elements, n_outer_layer.

  elseif (Router <= 0.0_dp .and. growth_rate <= 0.0_dp ) then

  !----------------------------------------------------

     cx = (abs(distance)-Rinner) - rs(nm)

    do i = nm+1, nr
     rs(i) = rs(nm) + cx*real(i-nm,dp)/real(n_outer_layer,dp)
    end do

  !----------------------------------------------------

   !Compute and save the spacings in the outer region.
    do i = nm, nr-1
     vspacing(i) = rs(i+1)-rs(i)
    end do

  endif
 !----------------------------------------------------
 !----------------------------------------------------

 !---------------------------------------------------------------
 ! Check the spacings (however they are determined).
 !---------------------------------------------------------------

   do i = 1, nr-1
    if ( vspacing(i) <= 0.0_dp ) then
     write(*,*) " NEGATIVE SPACING!!! ", i, " vspacing(i) = ", vspacing(i), rs(i), rs(i+1)
     stop
    endif
   end do
    
  if (debug_mode) then
    write(*,*) " i, nm, nr,  rs(i), vspacing(i) "
   do i = 1, nr
    if (i<nr) then
    write(*,*) i, nm, nr,  rs(i), vspacing(i)
    else
    write(*,*) i, nm, nr,  rs(i)
    endif
   end do
  endif
 
  write(*,*)
  write(*,*) " The outer layers begin at = ", Rinner + rs(nm), " with the initial spacing = ", vspacing(nm)
  write(*,*) " The outer layers end   at = ", Rinner + rs(nr), " with the    last spacing = ", vspacing(nr-1)
  write(*,*)
  write(*,*) " The outer boundary radius = ", Rinner + rs(nr)
  write(*,*)

 endif construction

!*******************************************************************************
!
! Generate nodes by going up along the surface normal from each node on the body.
! 
!
!*******************************************************************************

  nnodes_body = nnodes
       nnodes = nnodes*nr
  allocate(node(nnodes))
  allocate(node_above(nr*nnodes_body))
  allocate(k1(nnodes))
  allocate(k2(nnodes))
  allocate(k3(nnodes))
  allocate(k4(nnodes))

  write(*,*) " nnodes_body =", nnodes_body 
  do i = 1, nnodes_body
   node(i)%x  = node_body(i)%x
   node(i)%y  = node_body(i)%y
   node(i)%z  = node_body(i)%z
   node(i)%nx = node_body(i)%nx
   node(i)%ny = node_body(i)%ny
   node(i)%nz = node_body(i)%nz
   k1(i)  = k1_body(i)
   k2(i)  = k2_body(i)
   k3(i)  = k3_body(i)
   k4(i)  = k4_body(i)
  end do

  deallocate(node_body)

! We now go up and generate interior nodes

  nnodes = nnodes_body
  node_on_body : do i = 1, nnodes_body

!  Second node
   nnodes = nnodes + 1
   node(nnodes)%x = vspacing(1)*node(i)%nx + node(i)%x
   node(nnodes)%y = vspacing(1)*node(i)%ny + node(i)%y
   node(nnodes)%z = vspacing(1)*node(i)%nz + node(i)%z
   xp = node(nnodes)%x
   yp = node(nnodes)%y
   zp = node(nnodes)%z
   node_above(i) = nnodes

   k1(nnodes)  = k1_body(i)
   k2(nnodes)  = k2_body(i)
   k3(nnodes)  = k3_body(i)
   k4(nnodes)  = 1

!  Third node to nm-th node
!  Nodes in the prismatic layer along the surface normal. 
   do k = 3, nm

    nnodes = nnodes + 1
    node(nnodes)%x = vspacing(k-1)*node(i)%nx + xp
    node(nnodes)%y = vspacing(k-1)*node(i)%ny + yp
    node(nnodes)%z = vspacing(k-1)*node(i)%nz + zp
    node_above(nnodes-1) = nnodes

    xp = node(nnodes)%x
    yp = node(nnodes)%y
    zp = node(nnodes)%z

    k1(nnodes)  = k1_body(i)
    k2(nnodes)  = k2_body(i)
    k3(nnodes)  = k3_body(i)
    k4(nnodes)  = k-1

   end do

!  Outer region
   do k = nm+1, nr

    dirx = node(i)%nx
    diry = node(i)%ny
    dirz = node(i)%nz

!   Go up in the normal direction and generate a new interior node.
    nnodes = nnodes + 1
    node(nnodes)%x = vspacing(k-1)*dirx + xp
    node(nnodes)%y = vspacing(k-1)*diry + yp
    node(nnodes)%z = vspacing(k-1)*dirz + zp
    node_above(nnodes-1) = nnodes

    xp = node(nnodes)%x
    yp = node(nnodes)%y
    zp = node(nnodes)%z

    k1(nnodes)  = k1_body(i)
    k2(nnodes)  = k2_body(i)
    k3(nnodes)  = k3_body(i)
    k4(nnodes)  = k-1

   end do

  end do node_on_body

 deallocate(k1_body,k2_body,k3_body,k4_body)

! At this point, all nodes have been generated.
! It is now a matter of how to connect them (i.e., type of elements).

!*******************************************************************************
! 6. Write a map file: boundary marks
!
! There are four boundary parts:
!
! 1. Hemishpere
! 2. Symmetry wrt z-axis
! 3. Inflow boundary
! 4. Outer boundary
!
! NOTE: The boundary condition numbers (e.g., 4000) are specific to a solver.
!       Appropriate number needs to be assigned for your solver.
!
!*******************************************************************************

  write(*,*)
  write(*,*) "***********************************************************"
  write(*,*) " 6. Write a boudnary info file"
  write(*,*) "***********************************************************"

  open(unit=16, file=filename_mapbc, status="unknown", iostat=os)

   write(16,'(a57)') "3       !Number of boundary parts (boundary conditions)"
   write(16,'(a21)') "1, 4000 !Viscous wall"
   write(16,'(a15)') "2, 5050 !Inflow"
   write(16,'(a16)') "3, 5051 !Outflow"

   nb = 3
   bnames(1) = "VISCOUS_WALL"
   bnames(2) = "INFLOW"
   bnames(3) = "OUTFLOW"

  close(16)

  write(*,*)
  write(*,*) " Boundary info file written:", filename_mapbc

!*******************************************************************************
! 7. Generate elements
!
!    Nodes have already defined.
!    Different grids will be generated by connecting the existing nodes.
!
!    igrid_type = Choice of element type:
!
!      1 = Prismatic grid
!      2 = Tetrahedral grid
!      3 = Mixed grid (Prism/Tet)
!
!*******************************************************************************
!*******************************************************************************
!* (1). Generate a prismatic grid
!*******************************************************************************
 if (igrid_type == 1) then

  write(*,*)
  write(*,*) "***********************************************************"
  write(*,*) " 7. Generate a prismatic grid"
  write(*,*) "***********************************************************"

 call prismatic_grid

 write(*,*) " Prismatic elements generated."

!*******************************************************************************
!* (2). Generate a tet grid
!*******************************************************************************
 elseif ( igrid_type == 2 ) then

  write(*,*)
  write(*,*) "***********************************************************"
  write(*,*) " 7. Generate a tetrahedral grid"
  write(*,*) "***********************************************************"

  call tet_grid

 write(*,*) " Tetrahedral elements generated."

!*******************************************************************************
! Perturb the nodal coordinates.
!*******************************************************************************

!*******************************************************************************
! Construct data for tetrahedra that share a node.
!*******************************************************************************

! THIS IS NOT WORKING RIGHT NOW... NEED TO DEBUG....

if (1==0) then

   write(*,*)
   write(*,*) " Constructing data for tetrahedra that share a node....."
   write(*,*)

  allocate( n_node2tet(nnodes)  )
  allocate(      bmark(nnodes)  )
  allocate( ave_length(nnodes)  )

! Count the number of tetrahedra sharing the same node:
    n_node2tet = 0
  do i = 1, ntet
   do k = 1, 4
    n_node2tet( tet(i,k) ) = n_node2tet( tet(i,k) ) + 1 !# of tets around the node tet(i,k).
   end do
  end do

! Allocate the array to store the neighboring tetrahedra at each node.
! Use the maximum n_node2tet.

  allocate( node2tet(nnodes, maxval(n_node2tet) ) )

! Construct the list of tetrahedra around each node.

  n_node2tet = 0

  do i = 1, ntet
   do k = 1, 4
    n_node2tet( tet(i,k) ) = n_node2tet( tet(i,k) ) + 1 !# of tets around the node tet(i,k).
      node2tet( tet(i,k), n_node2tet( tet(i,k) ) ) = i    !Add tet i to the node tet(i,k).
   end do
  end do

    bmark = 0

  do i = 1, ntrias
   do j = 1, 3

      inode = tria(i)%v(j)
       bmark(inode) = 1 !<- Inner surface

    do k = 2, nr-1
      inode = node_above(inode)
       bmark(inode) = 0 !<- Interior node, which can be perturbed.
    end do

      inode = node_above(inode)
       bmark(inode) = 2 !<- Outer surface

   end do
  end do

  ave_length = 0.0_dp
  do i = 1, ntet

         x1 = node(tet(i,1))%x; x2 = node(tet(i,2))%x; x3 = node(tet(i,3))%x; x4 = node(tet(i,4))%x;
         y1 = node(tet(i,1))%y; y2 = node(tet(i,2))%y; y3 = node(tet(i,3))%y; y4 = node(tet(i,4))%y;
         z1 = node(tet(i,1))%z; z2 = node(tet(i,2))%z; z3 = node(tet(i,3))%z; z4 = node(tet(i,4))%z;
    volume = tet_volume(x1,x2,x3,x4, y1,y2,y3,y4, z1,z2,z3,z4)

   do k = 1, 4
    ave_length( tet(i,k) ) = ave_length( tet(i,k) ) + volume**(1.0_dp/3.0_dp)
   end do

  end do

  do i = 1, nnodes
   ave_length(i) = ave_length(i) / n_node2tet(i)
  end do

! Randomly perturb the nodal coordinates.
! Check the volume. If negative, reduce the perturbation and try again.

 nodal_pertb : if (perturb_nodes) then

   write(*,*)
   write(*,*) " Start perturbing the nodal coordinates....."
   write(*,*)

    n_negative_vol_detected = 0

   node_ptb : do inode = 1, nnodes
   if (bmark(inode) > 0) cycle

     if ( inode == 1 )  write(*,*) " Perturbing the nodal coordinates....."

          x0 = node(inode)%x
          y0 = node(inode)%y
          z0 = node(inode)%z

       negative_volume_exists = .true.
                           ap = perturb_nodes_parameter
       do while (negative_volume_exists)

        call random_number(rn)
        node(inode)%x = x0 + (rn-0.5_dp)*ave_length(inode) * ap
        call random_number(rn)
        node(inode)%y = y0 + (rn-0.5_dp)*ave_length(inode) * ap
        call random_number(rn)
        node(inode)%z = z0 + (rn-0.5_dp)*ave_length(inode) * ap

       !Compute the volume of all surrounding tetrahedra.
        negative_volume_exists = .false.
        do ii = 1, n_node2tet(inode)
         it = node2tet(inode,ii)
         x1 = node(tet(it,1))%x; x2 = node(tet(it,2))%x; x3 = node(tet(it,3))%x; x4 = node(tet(it,4))%x;
         y1 = node(tet(it,1))%y; y2 = node(tet(it,2))%y; y3 = node(tet(it,3))%y; y4 = node(tet(it,4))%y;
         z1 = node(tet(it,1))%z; z2 = node(tet(it,2))%z; z3 = node(tet(it,3))%z; z4 = node(tet(it,4))%z;
         volume = tet_volume(x1,x2,x3,x4, y1,y2,y3,y4, z1,z2,z3,z4)
         if (volume < 0.0_dp) negative_volume_exists = .true.
        end do

       !If negative volume is detected, recude the perturbation parameter and try again.
         if (negative_volume_exists) then
          ap = ap*0.9_dp
          n_negative_vol_detected = n_negative_vol_detected + 1
          !write(*,*) " Negative volume created. Adjusting a parameter..."
         endif

       end do

    write(1,*)
    write(1,*) " inode = ", inode, " bmark = ", bmark(inode)
    write(1,*) " x0, y0, z0 = ", x0, y0, z0
    write(1,*) " x1, y1, z1 = ", node(inode)%x, node(inode)%y, node(inode)%z

    end do node_ptb

   write(*,*)
   write(*,*) " # of negative volume fix = ", n_negative_vol_detected
   write(*,*)

   write(*,*)
   write(*,*) " Finished perturbing the nodal coordinates....."
   write(*,*)

  else

   write(*,*)
   write(*,*) " Skipped perturbing the nodal coordinates....."
   write(*,*)

  endif nodal_pertb

endif

!*******************************************************************************
! Final volume check
!*******************************************************************************

   vol_max = -1.0_dp
   vol_min =  1.0e+15_dp
   vol_ave =  0.0_dp

   do i = 1, ntet

         x1 = node(tet(i,1))%x; x2 = node(tet(i,2))%x; x3 = node(tet(i,3))%x; x4 = node(tet(i,4))%x;
         y1 = node(tet(i,1))%y; y2 = node(tet(i,2))%y; y3 = node(tet(i,3))%y; y4 = node(tet(i,4))%y;
         z1 = node(tet(i,1))%z; z2 = node(tet(i,2))%z; z3 = node(tet(i,3))%z; z4 = node(tet(i,4))%z;
    volume = tet_volume(x1,x2,x3,x4, y1,y2,y3,y4, z1,z2,z3,z4)

    vol_max = max(vol_max,volume)
    vol_min = min(vol_min,volume)
    vol_ave = vol_ave + volume

   end do

   vol_tot = vol_ave
   vol_ave = vol_tot / real(ntet,dp)

   if (vol_min > 0.0_dp) then
     write(*,*)
     write(*,*) " Negative volume successfully avoided."
     write(*,*)
   endif

   write(*,*)
   write(*,*) " Minimum tetrahedral volume = ", vol_min ! <- Should be positive!
   write(*,*) " Maximum tetrahedral volume = ", vol_max
   write(*,*) " Average tetrahedral volume = ", vol_ave
   write(*,*) " Total tetrahedral volume   = ", vol_tot
   write(*,*) " which should be close to but less than the true value = ", 4.0/3.0*pi*( (distance+0.0*Rd)**3 - Rd**3)
   write(*,*)

   if (vol_min < 0.0_dp) then
     write(*,*) " Tetrahedra with negative volume created... Stop."
    stop
   endif
 
!*******************************************************************************
!* (3). Generate a mixed grid  (Prism/Tet)
!*******************************************************************************
 elseif ( igrid_type == 3 ) then

  write(*,*)
  write(*,*) "***********************************************************"
  write(*,*) " 7. Generate a mixed grid"
  write(*,*) "***********************************************************"

 call mixed_grid

 write(*,*) " Mixed elements generated."

!*******************************************************************************
!* (4). Error
!*******************************************************************************
 else

  write(*,*)
  write(*,*) "Invalid input: igrid_type = ", igrid_type
  write(*,*) "               igrid_type must be 1, 2, or 3. Try again."
  stop

 endif

!*******************************************************************************
! 8. Generate output files.
!
!    - line file: line_extent==1 -> lines within a boundary layer (first layer).
!                 line_extent==2 -> lines all the way to the outer boundary.
!
!    - Tecplot boundary-grid file
!    - Tecplot   volume-grid file
!    - .k file
!    - .ugrid file
!    - .su2 file
!    - .vtk file
!
!*******************************************************************************

  write(*,*) "***********************************************************"
  write(*,*) " 8. Writing output files as requested..."
  write(*,*) "***********************************************************"

!-------------------------------------
! Generate two line files:
!
!    (1) sphere.lines_fmt     - Lines are within the viscous layer.
!    (2) sphere.lines_fmt_all - Lines go up to the outer boundary.
!

  if( generate_line_file ) then

  !----------------------------------------------
  ! Lines within the viscous layer
    if (line_extent==1) then

    write(*,*)
    write(*,*) "--------------------------------------------"
    write(*,*) "Writing a regular line-info file...."
    call line_info_file(nm)

  !----------------------------------------------
  ! Lines go all the way up to the outer boundary (all nodes are in lines)
    elseif (line_extent==2) then

    write(*,*)
    write(*,*) "--------------------------------------------"
    write(*,*) "Writing an all-node line-info file...."
    call line_info_file_all(nr)

  !----------------------------------------------
  ! Invalid input
    else

    write(*,*)
    write(*,*) "--------------------------------------------"
    write(*,*) " Invalid input for line_extent:", line_extent
    write(*,*) " line_extent must be 1 or 2..."
    write(*,*) " Try again. Stop."
    stop

    endif

  !Do not write a line file.
  else

    write(*,*)
    write(*,*) "--------------------------------------------"
    write(*,*) "Skipping write of line-info files"
    write(*,*) "To enable it, set generate_line_file=.true. with line_extent = 1 or 2."
    write(*,*)

  endif

!-------------------------------------
! Tecplot boundary-grid file

  if (debug_mode .or. generate_tec_file_b) then

   write(*,*)
    write(*,*) "--------------------------------------------"
   write(*,*) "Writing a Tecplot file for boundaries.... ", trim(filename_tecplot_b)
   call write_tecplot_boundary_file

  else
   write(*,*)
    write(*,*) "--------------------------------------------"
   write(*,*) "Skipping write of Teplot boundary file."
   write(*,*) "To enable it, set generate_tec_file_b = .true."
   write(*,*)
  endif

!-------------------------------------
! Tecplot volume-grid file

  if (generate_tec_file_v) then

   write(*,*)
    write(*,*) "--------------------------------------------"
   write(*,*) "Writing a Tecplot file for volumes...."
   call write_tecplot_volume_file
   write(*,*) "Tecplot file for volume grid written : ", filename_tecplot_v

  else
   write(*,*)
    write(*,*) "--------------------------------------------"
   write(*,*) "Skipping write of Teplot volume file."
   write(*,*) "To enable it, set generate_tec_file_v = .true."
  endif

!-------------------------------------
! .k file

  if (generate_k_file) then
   write(*,*)
    write(*,*) "--------------------------------------------"
    write(*,*) "Writing a k file.... ", trim(filename_k)
    call write_k_file
  else
   write(*,*)
    write(*,*) "--------------------------------------------"
    write(*,*) "Skipping write of the k file : "
    write(*,*) "To enable it, set generate_k_file = .true."
    write(*,*)
  endif

!-------------------------------------
! .ugrid file

  if (generate_ugrid_file) then

   write(*,*)
    write(*,*) "--------------------------------------------"
   write(*,*) "Writing a ugrid file.... ", trim(filename_ugrid)
   call write_ugrid_file

  else
   write(*,*)
   write(*,*) "--------------------------------------------"
   write(*,*) "Skipping write of a .ugrid grid file.."
   write(*,*) "To enable it, set generate_ugrid_file = .true."
  endif

!-------------------------------------
! .su2 file

  if (generate_su2grid_file) then

    write(*,*)
    write(*,*) "--------------------------------------------"
    write(*,*) "Writing a SU2 grid file...."
    call write_su2grid_file(nb,bnames)
    write(*,*) "SU2GRID file written : ", filename_su2grid

  else
   write(*,*)
   write(*,*) "--------------------------------------------"
   write(*,*) "Skipping write of a SU2 grid file.."
   write(*,*) "To enable it, set filename_su2grid = .true."
  endif

!-------------------------------------
! .vtk file

  if (generate_vtk_file) then

   allocate( xpp(nnodes), ypp(nnodes), zpp(nnodes) )

   do i = 1, nnodes
    xpp(i) = node(i)%x
    ypp(i) = node(i)%y
    zpp(i) = node(i)%z
   end do

   write(*,*)
    write(*,*) "--------------------------------------------"
   write(*,*) "Writing a .vtk file for volumes...."
   call write_vtk_file(filename_vtk, nnodes,xpp,ypp,zpp, ntet,tet, nprs,prs, nhex,hex )
   write(*,*) "Tecplot file for volume grid written : ", filename_vtk

  else
   write(*,*)
   write(*,*) "--------------------------------------------"
   write(*,*) "Skipping write of Teplot volume file."
   write(*,*) "To enable it, set generate_vtk_file = .true."
  endif

!*******************************************************************************

 write(*,*)
 write(*,*) "--------------------------------------------"
 write(*,*) "--------------------------------------------"
 write(*,*) "--------------------------------------------"
 write(*,*) "Congratulations!"
 write(*,*) "Grid generation successfully completed."

 stop


contains 

!*******************************************************************************
!* Prismatic Grid Generation
!*******************************************************************************
 subroutine prismatic_grid
 implicit none
!*******************************************************************************
! Generate prismatic grid
!*******************************************************************************
  ntrias_b = ntrias + ntrias        !Inner and outer boundaries
  allocate(tri(ntrias_b,5))

  if (n_sections==6) then
   nquads_b = nnodes_circum * (nr-1) !Symmetry boundary
   allocate(quad(nquads_b,5))
  endif

  nprs = ntrias*(nr-1)
  allocate(prs(nprs,6) )
  ntet = 0

  write(*,*)
  write(*,*) " 1. Prismatic grid "
  write(*,*)
  write(*,*) "----- Predicted dimensions ------"
  write(*,*)
  write(*,*) "    nodes      = ", nnodes
  write(*,*) "  Volume elements:"
  write(*,*) "    prisms     = ", nprs
  write(*,*) "    tetrahedra = ", ntet
  write(*,*) "  Boundary elements:"
  write(*,*) "     triangles = ", ntrias_b
  write(*,*) "         quads = ", nquads_b
  write(*,*)
  write(*,*) "---------------------------------"
  write(*,*)

  nprs = 0
  ntrias_b = 0
  nquads_b = 0

! Copy the triangulation on the body

  do i = 1, ntrias
          ntrias_b = ntrias_b + 1
   tri(ntrias_b,5) = 1
   tri(ntrias_b,1) = tria(i)%v(1)
   tri(ntrias_b,2) = tria(i)%v(2)
   tri(ntrias_b,3) = tria(i)%v(3)
  end do

! Generate prisms

  do i = 1, ntrias

   node1 = tria(i)%v(1)
   node2 = tria(i)%v(2)
   node3 = tria(i)%v(3)
   node4 = node_above(tria(i)%v(1))
   node5 = node_above(tria(i)%v(2))
   node6 = node_above(tria(i)%v(3))

     nprs = nprs + 1

     prs(nprs,1) = node1
     prs(nprs,2) = node2
     prs(nprs,3) = node3
     prs(nprs,4) = node4
     prs(nprs,5) = node5
     prs(nprs,6) = node6

   do k = 2, nr-1

    node1 = node4
    node2 = node5
    node3 = node6
    node4 = node_above(node1)
    node5 = node_above(node2)
    node6 = node_above(node3)

    nprs = nprs + 1
    prs(nprs,1) = node1
    prs(nprs,2) = node2
    prs(nprs,3) = node3
    prs(nprs,4) = node4
    prs(nprs,5) = node5
    prs(nprs,6) = node6


   end do

  !This is a triangle on the outer boundary

     ntrias_b = ntrias_b + 1
     tri(ntrias_b,1) = node6
     tri(ntrias_b,2) = node5
     tri(ntrias_b,3) = node4

     if ( node(node4)%x < -1.0e-13_dp .or. &
          node(node5)%x < -1.0e-13_dp .or. &
          node(node6)%x < -1.0e-13_dp      ) then
     !(1)Inflow
       tri(ntrias_b,5) = 2
     else
     !(2)Outflow
       tri(ntrias_b,5) = 3
     endif

  end do

  write(*,*) "------- Actual dimensions -------"
  write(*,*)
  write(*,*) "    nodes      = ", nnodes
  write(*,*) "  Volume elements:"
  write(*,*) "    prisms     = ", nprs
  write(*,*) "    tetrahedra = ", ntet
  write(*,*) "  Boundary elements:"
  write(*,*) "     triangles = ", ntrias_b
  write(*,*) "         quads = ", nquads_b
  write(*,*)
  write(*,*) "---------------------------------"
  write(*,*)

 end subroutine prismatic_grid


!*******************************************************************************
!* Tet grid Generation
!
!*******************************************************************************
 subroutine tet_grid
 implicit none
 integer :: tria_type
!*******************************************************************************
! Generate tet grid
!*******************************************************************************

  if (n_sections==6) then
   ntrias_b = ntrias + ntrias + 2*nnodes_circum * (nr-1) !Inner and outer boundaries
   allocate(tri(ntrias_b,5))
  elseif (n_sections==1) then
   ntrias_b = ntrias + ntrias + 2*nnodes_circum * (nr-1) !Inner and outer boundaries
   ntrias_b = ntrias_b + 2*2*(nr_gs)*(nr-1)
   allocate(tri(ntrias_b,5))
  endif

!  ntrias_b = ntrias + ntrias + 2*nnodes_circum * (nr-1) !Inner and outer boundaries
!  allocate(tri(ntrias_b,5))

  nquads_b = 0
  nprs = 0
  ntet = ntrias*(nr-1)*3
  allocate(tet(ntet,6) )

  write(*,*)
  write(*,*) "2. Tetrahedral grid "
  write(*,*)
  write(*,*) "----- Predicted dimensions ------"
  write(*,*)
  write(*,*) "    nodes      = ", nnodes
  write(*,*) "  Volume elements:"
  write(*,*) "    prisms     = ", nprs
  write(*,*) "    tetrahedra = ", ntet
  write(*,*) "  Boundary elements:"
  write(*,*) "     triangles = ", ntrias_b
  write(*,*) "         quads = ", nquads_b
  write(*,*)
  write(*,*) "---------------------------------"
  write(*,*)

  nprs = 0
  ntet = 0
  ntrias_b = 0
  nquads_b = 0

! Copy the triangulation on the body: Reverse the orientation.

  do i = 1, ntrias
          ntrias_b = ntrias_b + 1
   tri(ntrias_b,5) = 1
   tri(ntrias_b,1) = tria(i)%v(1)
   tri(ntrias_b,2) = tria(i)%v(2)
   tri(ntrias_b,3) = tria(i)%v(3)
  end do

! Generate tetrahedra

  do i = 1, ntrias

   do k = 1, nr-1

    if (k == 1) then

     node1 = tria(i)%v(1)
     node2 = tria(i)%v(2)
     node3 = tria(i)%v(3)
     node4 = node_above(tria(i)%v(1))
     node5 = node_above(tria(i)%v(2))
     node6 = node_above(tria(i)%v(3))

     tria_type = tria(i)%type

    else

     node1 = node4
     node2 = node5
     node3 = node6
     node4 = node_above(node1)
     node5 = node_above(node2)
     node6 = node_above(node3)

    endif

    t_side : if (i < ntrias_hs+1) then

     t_type :if (tria_type == 1) then

      ntet = ntet + 1
      tet(ntet,1) = node1
      tet(ntet,2) = node2
      tet(ntet,3) = node3
      tet(ntet,4) = node4

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node6
      tet(ntet,3) = node2
      tet(ntet,4) = node4

      ntet = ntet + 1
      tet(ntet,1) = node6
      tet(ntet,2) = node3
      tet(ntet,3) = node2
      tet(ntet,4) = node4

     elseif (tria_type == 2) then

      ntet = ntet + 1
      tet(ntet,1) = node4
      tet(ntet,2) = node6
      tet(ntet,3) = node5
      tet(ntet,4) = node1

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node6
      tet(ntet,3) = node3
      tet(ntet,4) = node1

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node3
      tet(ntet,3) = node2
      tet(ntet,4) = node1

     endif t_type

    else

     t_type2 :if (tria_type == 1) then

      ntet = ntet + 1
      tet(ntet,1) = node1
      tet(ntet,2) = node2
      tet(ntet,3) = node3
      tet(ntet,4) = node4

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node6
      tet(ntet,3) = node3
      tet(ntet,4) = node4

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node3
      tet(ntet,3) = node2
      tet(ntet,4) = node4

     elseif (tria_type == 2) then

      ntet = ntet + 1
      tet(ntet,1) = node4
      tet(ntet,2) = node6
      tet(ntet,3) = node5
      tet(ntet,4) = node1

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node6
      tet(ntet,3) = node2
      tet(ntet,4) = node1

      ntet = ntet + 1
      tet(ntet,1) = node6
      tet(ntet,2) = node3
      tet(ntet,3) = node2
      tet(ntet,4) = node1

     endif t_type2

    endif t_side

   end do

  !This is a triangle on the outer boundary

     ntrias_b = ntrias_b + 1
     tri(ntrias_b,1) = node6
     tri(ntrias_b,2) = node5
     tri(ntrias_b,3) = node4

     if ( node(node4)%x < -1.0e-13_dp .or. &
          node(node5)%x < -1.0e-13_dp .or. &
          node(node6)%x < -1.0e-13_dp      ) then
     !(1)Inflow
       tri(ntrias_b,5) = 2
     else
     !(2)Outflow
       tri(ntrias_b,5) = 3
     endif

  end do

  write(*,*)
  write(*,*) "------- Actual dimensions -------"
  write(*,*)
  write(*,*) "    nodes      = ", nnodes
  write(*,*) "  Volume elements:"
  write(*,*) "    prisms     = ", nprs
  write(*,*) "    tetrahedra = ", ntet
  write(*,*) "  Boundary elements:"
  write(*,*) "     triangles = ", ntrias_b
  write(*,*) "         quads = ", nquads_b
  write(*,*)
  write(*,*) "---------------------------------"
  write(*,*)

 end subroutine tet_grid

!*******************************************************************************
!* Mixed grid Generation
!
!*******************************************************************************
 subroutine mixed_grid
 implicit none
 integer :: tria_type
!*******************************************************************************
! Generate mixed grid
!*******************************************************************************

  ntrias_b = 2*ntrias + 2*nnodes_circum * (nr-1)-(nm-1) !All boundaries
!  allocate(tri(ntrias_b,5))
  nquads_b = nnodes_circum * (nm-1) !A part of symmetry boundary
!  allocate(quad(nquads_b,5))

  nprs = ntrias*(nm-1)              !Boundary layer
  allocate(prs(nprs,6) )
  ntet = ntrias*((nr-1)-(nm-1))*3   !Outer region
  allocate(tet(ntet,6) )

  allocate(tri(ntrias_b,5))
  allocate(quad(nquads_b,5))

  write(*,*)
  write(*,*) "3. Mixed grid "
  write(*,*)
  write(*,*) "----- Predicted dimensions ------"
  write(*,*)
  write(*,*) "    nodes      = ", nnodes
  write(*,*) "  Volume elements:"
  write(*,*) "    prisms     = ", nprs
  write(*,*) "    tetrahedra = ", ntet
  write(*,*) "  Boundary elements:"
  write(*,*) "     triangles = ", ntrias_b
  write(*,*) "         quads = ", nquads_b
  write(*,*)
  write(*,*) "---------------------------------"
  write(*,*)

  nprs = 0
  ntet = 0
  ntrias_b = 0
  nquads_b = 0

! Copy the triangulation on the body

  do i = 1, ntrias
          ntrias_b = ntrias_b + 1
   tri(ntrias_b,5) = 1
   tri(ntrias_b,1) = tria(i)%v(1)
   tri(ntrias_b,2) = tria(i)%v(2)
   tri(ntrias_b,3) = tria(i)%v(3)
  end do

! Generate prisms

  do i = 1, ntrias

   node1 = tria(i)%v(1)
   node2 = tria(i)%v(2)
   node3 = tria(i)%v(3)
   node4 = node_above(tria(i)%v(1))
   node5 = node_above(tria(i)%v(2))
   node6 = node_above(tria(i)%v(3))
   tria_type = tria(i)%type

     nprs = nprs + 1

     prs(nprs,1) = node1
     prs(nprs,2) = node2
     prs(nprs,3) = node3
     prs(nprs,4) = node4
     prs(nprs,5) = node5
     prs(nprs,6) = node6

   do k = 2, nr-1

    node1 = node4
    node2 = node5
    node3 = node6
    node4 = node_above(node1)
    node5 = node_above(node2)
    node6 = node_above(node3)

    prs_or_tet : if (k < nm) then

     nprs = nprs + 1
     prs(nprs,1) = node1
     prs(nprs,2) = node2
     prs(nprs,3) = node3
     prs(nprs,4) = node4
     prs(nprs,5) = node5
     prs(nprs,6) = node6

    else


    t_side : if (i < ntrias_hs+1) then

     t_type :if (tria_type == 1) then

      ntet = ntet + 1
      tet(ntet,1) = node1
      tet(ntet,2) = node2
      tet(ntet,3) = node3
      tet(ntet,4) = node4

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node6
      tet(ntet,3) = node2
      tet(ntet,4) = node4

      ntet = ntet + 1
      tet(ntet,1) = node6
      tet(ntet,2) = node3
      tet(ntet,3) = node2
      tet(ntet,4) = node4

     elseif (tria_type == 2) then

      ntet = ntet + 1
      tet(ntet,1) = node4
      tet(ntet,2) = node6
      tet(ntet,3) = node5
      tet(ntet,4) = node1

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node6
      tet(ntet,3) = node3
      tet(ntet,4) = node1

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node3
      tet(ntet,3) = node2
      tet(ntet,4) = node1

     endif t_type

    else

     t_type2 :if (tria_type == 1) then

      ntet = ntet + 1
      tet(ntet,1) = node1
      tet(ntet,2) = node2
      tet(ntet,3) = node3
      tet(ntet,4) = node4

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node6
      tet(ntet,3) = node3
      tet(ntet,4) = node4

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node3
      tet(ntet,3) = node2
      tet(ntet,4) = node4

     elseif (tria_type == 2) then

      ntet = ntet + 1
      tet(ntet,1) = node4
      tet(ntet,2) = node6
      tet(ntet,3) = node5
      tet(ntet,4) = node1

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node6
      tet(ntet,3) = node2
      tet(ntet,4) = node1

      ntet = ntet + 1
      tet(ntet,1) = node6
      tet(ntet,2) = node3
      tet(ntet,3) = node2
      tet(ntet,4) = node1

     endif t_type2

    endif t_side

    endif prs_or_tet

   end do

  !This is a triangle on the outer boundary

     ntrias_b = ntrias_b + 1
     tri(ntrias_b,1) = node6
     tri(ntrias_b,2) = node5
     tri(ntrias_b,3) = node4

     if ( node(node4)%x < -1.0e-13_dp .or. &
          node(node5)%x < -1.0e-13_dp .or. &
          node(node6)%x < -1.0e-13_dp      ) then
     !(1)Inflow
       tri(ntrias_b,5) = 2
     else
     !(2)Outflow
       tri(ntrias_b,5) = 3
     endif

  end do


  write(*,*) "------- Actual dimensions -------"
  write(*,*)
  write(*,*) "    nodes      = ", nnodes
  write(*,*) "  Volume elements:"
  write(*,*) "    prisms     = ", nprs
  write(*,*) "    tetrahedra = ", ntet
  write(*,*) "  Boundary elements:"
  write(*,*) "     triangles = ", ntrias_b
  write(*,*) "         quads = ", nquads_b
  write(*,*)
  write(*,*) "---------------------------------"
  write(*,*)

 end subroutine mixed_grid

!*******************************************************************************
!* Write out a file containing line info (only within the viscous layer)):
!* This information is for the implicit line relaxations or line agglomeration.
!* This format is used by FUN3D.
!*******************************************************************************
 subroutine line_info_file(n_points)
 implicit none

 integer, intent(in) :: n_points

 integer :: node_below
 integer :: i, k, n_lines, n_total_points, max_points, min_points, i_count, inode,os

  open(unit=1, file=filename_lines, status="unknown", iostat=os)
  max_points = n_points
  min_points = n_points
  n_lines = nnodes_body
  n_total_points = n_lines * n_points

  write(*,*) " Total number of lines = ", n_lines
  write(*,*) "            max points = ", max_points
  write(*,*) "            min points = ", min_points

  write(1,*) n_lines, n_total_points, "Total lines and points"
  write(1,*) min_points, max_points, "Min and max points in line"

  i_count = 0

! Go up and write out the node info for every node on the body.
! NOTE: Go up to the given limit: n_points

  do i = 1, nnodes_body

     i_count = i_count + 1

!  1. First node (on the body)
     write(1,*) n_points, " Points in line for line = ", i_count
     write(1,'(i10,a10,3es30.20)') i, " x/y/z= ", node(i)%x, node(i)%y, node(i)%z
     node_below = i

!  2. Second node to the node below the last one.
   do k = 2, n_points-1
     write(1,'(i10)') node_above(node_below)
     node_below = node_above(node_below)
   end do

!  3. Last node
     inode = node_above(node_below)
     write(1,'(i10,a10,3es30.20)') inode, " x/y/z= ", node(inode)%x, node(inode)%y, node(inode)%z

  end do

 if (i_count /= n_lines) write(*,*) "Error: i_count /= n_lines"
 write(*,*)
 write(*,*) "lines_fmt file has been written: ", filename_lines

 close(1)
 end subroutine line_info_file

!*******************************************************************************
!* Write out a file containing line info (Lines go all the way up to the outer):
!* This information is for the implicit line relaxations or line agglomeration.
!* This format is used by FUN3D.
!*******************************************************************************
 subroutine line_info_file_all(n_points)
 implicit none

 integer, intent(in) :: n_points

 integer :: node_below
 integer :: i, k, n_lines, n_total_points, max_points, min_points, i_count, inode,os

  open(unit=1, file=filename_lines, status="unknown", iostat=os)
  max_points = n_points
  min_points = n_points
  n_lines = nnodes_body
  n_total_points = n_lines * n_points

  write(*,*) " Total number of lines = ", n_lines
  write(*,*) "            max points = ", max_points
  write(*,*) "            min points = ", min_points

  write(1,*) n_lines, n_total_points, "Total lines and points"
  write(1,*) min_points, max_points, "Min and max points in line"

  i_count = 0

! Go up and write out the node info for every node on the body.
! NOTE: Go up to the given limit: n_points

  do i = 1, nnodes_body

     i_count = i_count + 1

!  1. First node (on the body)
     write(1,*) n_points, " Points in line for line = ", i_count
     write(1,'(i10,a10,3es30.20)') i, " x/y/z= ", node(i)%x, node(i)%y, node(i)%z
     node_below = i

!  2. Second node to the node below the last one.
   do k = 2, n_points-1
     write(1,'(i10)') node_above(node_below)
     node_below = node_above(node_below)
   end do

!  3. Last node
     inode = node_above(node_below)
     write(1,'(i10,a10,3es30.20)') inode, " x/y/z= ", node(inode)%x, node(inode)%y, node(inode)%z

  end do

 if (i_count /= n_lines) write(*,*) "Error: i_count /= n_lines"
 write(*,*)
 write(*,*) "lines_fmt file has been written: ", filename_lines

 close(1)
 end subroutine line_info_file_all


!*******************************************************************************
! This subroutine writes a Tecplot file for the volume grid.
!*******************************************************************************
 subroutine write_tecplot_volume_file

 open(unit=8, file=filename_tecplot_v, status="unknown", iostat=os)
 write(8,*) 'TITLE = "GRID"'
 write(8,*) 'VARIABLES = "x","y","z","k1","k2","k3","k4"'

! Tetra Zone
  if (ntet > 0) then

   write(8,*) 'zone  n=', nnodes,',e=', ntet,' , et=tetrahedron, f=fepoint'
   do i = 1, nnodes
     write(8,'(3es20.10,4i13)') node(i)%x, node(i)%y, node(i)%z, &
                                k1(i),k2(i),k3(i),k4(i)
   end do

   do i = 1, ntet
    write(8,'(4i10)') tet(i,1), tet(i,2), tet(i,3), tet(i,4)
   end do

  endif

! Prism zone
  if (nprs > 0) then

   write(8,*) 'zone  n=', nnodes,',e=', nprs,' , et=brick, f=fepoint'
   do i = 1, nnodes
     write(8,'(3es20.10,4i13)') node(i)%x, node(i)%y, node(i)%z, &
                                k1(i),k2(i),k3(i),k4(i)
   end do

   do i = 1, nprs
    write(8,'(8i10)') prs(i,1), prs(i,2), prs(i,3), prs(i,3), &
                      prs(i,4), prs(i,5), prs(i,6), prs(i,6)
   end do

  endif

 close(8)

 end subroutine write_tecplot_volume_file

!*******************************************************************************
! This subroutine writes  a Tecplot file for boundaries.
!******************************************************************************
 subroutine write_tecplot_boundary_file

 integer :: ntrias_outer

 open(unit=7, file=filename_tecplot_b, status="unknown", iostat=os)
 write(7,*) 'TITLE = "GRID"'
 write(7,*) 'VARIABLES = "x","y","z","k1","k2","k3","k4"'

!-------------------------------------------------------
! Triangles on the sphere surface
!-------------------------------------------------------
 write(7,*) 'ZONE T="Body"  N=', nnodes,',E=', ntrias,' , ET=quadrilateral, F=FEPOINT'
  do i = 1, nnodes
    write(7,'(3ES20.10,4i13)') node(i)%x, node(i)%y, node(i)%z, &
                               k1(i),k2(i),k3(i),k4(i)
  end do
  do i = 1, ntrias
   write(7,'(4I10)') tri(i,1), tri(i,2), tri(i,3), tri(i,3)
  end do

!-------------------------------------------------------
! Triangles on the outer INFLOW boundary.
!-------------------------------------------------------
  ntrias_outer = 0
  do i = ntrias+1, ntrias_b
   if ( tri(i,5) == 2 ) ntrias_outer = ntrias_outer + 1
  end do

 write(7,*) 'ZONE T="Inflow"  N=', nnodes,',E=', ntrias_outer, &
            ' , ET=quadrilateral, F=FEPOINT'
  do i = 1, nnodes
    write(7,'(3ES20.10,4i13)') node(i)%x, node(i)%y, node(i)%z, &
                               k1(i),k2(i),k3(i),k4(i)
  end do
  do i = ntrias+1, ntrias_b
   if ( tri(i,5) == 2 ) write(7,'(4I10)') tri(i,1), tri(i,2), tri(i,3), tri(i,3)
  end do

!-------------------------------------------------------
! Triangles on the outer OUTFLOW boundary.
!-------------------------------------------------------
  ntrias_outer = 0
  do i = ntrias+1, ntrias_b
   if ( tri(i,5) == 3 ) ntrias_outer = ntrias_outer + 1
  end do

 write(7,*) 'ZONE T="Outflow"  N=', nnodes,',E=', ntrias_outer, &
            ' , ET=quadrilateral, F=FEPOINT'
  do i = 1, nnodes
    write(7,'(3ES20.10,4i13)') node(i)%x, node(i)%y, node(i)%z, &
                               k1(i),k2(i),k3(i),k4(i)
  end do
  do i = ntrias+1, ntrias_b
   if ( tri(i,5) == 3 ) write(7,'(4I10)') tri(i,1), tri(i,2), tri(i,3), tri(i,3)
  end do

 close(7)

 end subroutine write_tecplot_boundary_file


!*******************************************************************************
! This subroutine writes a ugrid file.
!*******************************************************************************
 subroutine write_ugrid_file

  if ( ugrid_file_unformatted  ) then
    open(unit=9, file=filename_ugrid, form='unformatted',access="stream",&
                                      status='unknown', iostat=os )
    write(9) nnodes,   ntrias_b,    nquads_b,   ntet,    0, nprs, 0
  else
    open(unit=9, file=filename_ugrid, status="unknown", iostat=os)
    !                    #nodes, #tri_faces, #quad_faces, #tetra, #pyr, #prz,
    !                    #hex
    write(9,'(7I20)') nnodes,   ntrias_b,    nquads_b,   ntet,    0, nprs, 0
  endif

!-----------------------------------------------------------
!-----------------------------------------------------------
   if ( ugrid_file_unformatted  ) then

! Nodes
  do i = 1, nnodes
   write(9) node(i)%x, node(i)%y, node(i)%z
  end do

! Triangular faces = ntri
  if (ntrias_b > 0) then
   do i = 1, ntrias_b
    write(9) tri(i,1), tri(i,2), tri(i,3)
   end do
  endif

! Quad faces = nquad
  if (nquads_b > 0) then
   do i = 1, nquads_b
    write(9) quad(i,1), quad(i,2), quad(i,3), quad(i,4)
   end do
  endif

! Face tag
  if (ntrias_b > 0) then
   do i = 1, ntrias_b
    write(9)  tri(i,5)
   end do
  endif

  if (nquads_b > 0) then
   do i = 1, nquads_b
    write(9) quad(i,5)
   end do
  endif

! tet
  if (ntet > 0) then
   do i = 1, ntet
    write(9) tet(i,1), tet(i,2), tet(i,3), tet(i,4)
   end do
  endif

! Prism
  if (nprs > 0) then
   do i = 1, nprs
    write(9) prs(i,1), prs(i,2), prs(i,3), &
                      prs(i,4), prs(i,5), prs(i,6)
   end do
  endif

!-----------------------------------------------------------
!-----------------------------------------------------------
 else

! Nodes
  do i = 1, nnodes
   write(9,'(3ES26.15)') node(i)%x, node(i)%y, node(i)%z
  end do

! Triangular faces = ntri
  if (ntrias_b > 0) then
   do i = 1, ntrias_b
    write(9,'(3I20)') tri(i,1), tri(i,2), tri(i,3)
   end do
  endif

! Quad faces = nquad
  if (nquads_b > 0) then
   do i = 1, nquads_b
    write(9,'(4I20)') quad(i,1), quad(i,2), quad(i,3), quad(i,4)
   end do
  endif

! Face tag
  if (ntrias_b > 0) then
   do i = 1, ntrias_b
    write(9,'(I110)')  tri(i,5)
   end do
  endif

  if (nquads_b > 0) then
   do i = 1, nquads_b
    write(9,'(I110)') quad(i,5)
   end do
  endif

! tet
  if (ntet > 0) then
   do i = 1, ntet
    write(9,'(4I20)') tet(i,1), tet(i,2), tet(i,3), tet(i,4)
   end do
  endif

! Prism
  if (nprs > 0) then
   do i = 1, nprs
    write(9,'(6I20)') prs(i,1), prs(i,2), prs(i,3), &
                      prs(i,4), prs(i,5), prs(i,6)
   end do
  endif

 endif
!-----------------------------------------------------------
!-----------------------------------------------------------

  close(9)

 end subroutine write_ugrid_file

!*******************************************************************************
! This subroutine writes an k file
!******************************************************************************
 subroutine write_k_file

 open(unit=10, file=filename_k, status="unknown", iostat=os)

! Write node_number, k1, k2, k3, k4

  write(10,*) nnodes

 do i = 1, nnodes
  write(10,'(5i13)') i, k1(i),k2(i),k3(i),k4(i)
 end do

 close(10)

 end subroutine write_k_file


!********************************************************************************
!* This subroutine is useful to expand or shrink type(node_data_yz) arrays.
!*
!*  Array, x, will be allocated if the requested dimension is 1 (i.e., n=1)
!*                     expanded to the requested dimension, n, if n > dim(x).
!*                     shrunk to the requested dimension, n, if n < dim(x).
!*
!********************************************************************************
  subroutine my_alloc_ndxy_ptr(x,n)
  implicit none

  integer,                    intent(in   ) :: n
  type(node_data_xy), dimension(:), pointer :: x

  integer :: i
  type(node_data_xy), dimension(:), pointer :: temp

  if (n <= 0) then
   write(*,*) "my_alloc_ndxy_ptr received non-positive dimension. Stop."
   stop
  endif

! If initial, allocate and return
  if (.not.(associated(x))) then
   allocate(x(n))
   return
  endif

! If reallocation, create a pointer with a target of new dimension.
  allocate(temp(n))
    temp(n)%gnode = 0
    temp(n)%x     = zero
    temp(n)%y     = zero

! (1) Expand the array dimension
  if ( n > size(x) ) then

   do i = 1, size(x) 
    temp(i)%gnode = x(i)%gnode
    temp(i)%x     = x(i)%y
    temp(i)%y     = x(i)%z
  end do

! (2) Shrink the array dimension: the extra data, x(n+1:size(x)), discarded.
  else

   do i = 1, n
    temp(i)%gnode = x(i)%gnode
    temp(i)%y     = x(i)%y
    temp(i)%z     = x(i)%z
   end do

  endif

! Destroy the target of x
  deallocate(x)

! Re-assign the pointer
   x => temp

  return


  end subroutine my_alloc_ndxy_ptr
!********************************************************************************

!*******************************************************************************
! Compute the volume of a tetrahedron defined by 4 vertices:
!
!       (x1,y1,z1), (x2,y2,z2), (x3,y3,z3), (x4,y4,z4),
!
! which are ordered as follows:
!
!            1
!            o
!           /| .
!          / |   .
!         /  |     .
!        /   |       .
!     2 o----|-------o 3
!        \   |     .
!         \  |    .
!          \ |  .
!           \|.
!            o
!            4
!
! Note: Volume = volume integral of 1 = 1/3 * volume integral of div(x,y,z) dV
!              = surface integral of (x,y,z)*dS
!              = sum of [ (xc,yc,zc)*area_vector ] over triangular faces.
!
! where the last step is eact because (x,y,z) vary linearly over the triangle.
! There are other ways to compute the volume, of course.
!
!*******************************************************************************
 function tet_volume(x1,x2,x3,x4, y1,y2,y3,y4, z1,z2,z3,z4)

 implicit none

 integer , parameter :: dp = selected_real_kind(15) !Double precision

 !Input
 real(dp), intent(in)   :: x1,x2,x3,x4, y1,y2,y3,y4, z1,z2,z3,z4
 !Output
 real(dp)               :: tet_volume

 real(dp)               :: xc, yc, zc
 real(dp), dimension(3) :: area
 integer                :: ix=1, iy=2, iz=3


 tet_volume = 0.0_dp

! Triangle 1-3-2

   !Centroid of the triangular face
      xc = (x1+x3+x2)/3.0_dp
      yc = (y1+y3+y2)/3.0_dp
      zc = (z1+z3+z2)/3.0_dp
   !Outward normal surface vector
   area = triangle_area_vector(x1,x3,x2, y1,y3,y2, z1,z3,z2)

   tet_volume = tet_volume + ( xc*area(ix) + yc*area(iy) + zc*area(iz) )

! Triangle 1-4-3

   !Centroid of the triangular face
      xc = (x1+x4+x3)/3.0_dp
      yc = (y1+y4+y3)/3.0_dp
      zc = (z1+z4+z3)/3.0_dp
   !Outward normal surface vector
   area = triangle_area_vector(x1,x4,x3, y1,y4,y3, z1,z4,z3)

   tet_volume = tet_volume + ( xc*area(ix) + yc*area(iy) + zc*area(iz) )

! Triangle 1-2-4

   !Centroid of the triangular face
      xc = (x1+x2+x4)/3.0_dp
      yc = (y1+y2+y4)/3.0_dp
      zc = (z1+z2+z4)/3.0_dp
   !Outward normal surface vector
   area = triangle_area_vector(x1,x2,x4, y1,y2,y4, z1,z2,z4)

   tet_volume = tet_volume + ( xc*area(ix) + yc*area(iy) + zc*area(iz) )

! Triangle 2-3-4

   !Centroid of the triangular face
      xc = (x2+x3+x4)/3.0_dp
      yc = (y2+y3+y4)/3.0_dp
      zc = (z2+z3+z4)/3.0_dp
   !Outward normal surface vector
   area = triangle_area_vector(x2,x3,x4, y2,y3,y4, z2,z3,z4)

   tet_volume = tet_volume + ( xc*area(ix) + yc*area(iy) + zc*area(iz) )

   tet_volume = tet_volume / 3.0_dp

 end function tet_volume

!*******************************************************************************
! Compute the area of a triangle in 3D defined by 3 vertices:
!
!       (x1,y1,z1), (x2,y2,z2), (x3,y3,z3),
!
! which is assumed to be ordered clockwise.
!
!     1             2
!      o------------o
!       \         .
!        \       . --------->
!         \    .
!          \ .
!           o
!           3
!
! Note: Area is a vector based on the right-hand rule: 
!       when wrapping the right hand around the triangle with the fingers in the
!       direction of the vertices [1,2,3], the thumb points in the positive
!       direction of the area.
!
! Note: Area vector is computed as the cross product of edge vectors [31] and [32].
!
!*******************************************************************************
 function triangle_area_vector(x1,x2,x3, y1,y2,y3, z1,z2,z3) result(area_vector)
 
 implicit none
 integer , parameter :: dp = selected_real_kind(15) !Double precision

 !Input
  real(dp), intent(in)   :: x1,x2,x3, y1,y2,y3, z1,z2,z3
 !Output
  real(dp), dimension(3) :: area_vector

  integer :: ix=1, iy=2, iz=3

   area_vector(ix) = 0.5_dp*( (y1-y3)*(z2-z3)-(z1-z3)*(y2-y3) )
   area_vector(iy) = 0.5_dp*( (z1-z3)*(x2-x3)-(x1-x3)*(z2-z3) )
   area_vector(iz) = 0.5_dp*( (x1-x3)*(y2-y3)-(y1-y3)*(x2-x3) )

 end function triangle_area_vector



function int_sign(n)
implicit none
integer, intent(in) :: n
integer :: int_sign

if (n >= 0) then
 int_sign = 1
else
 int_sign = -1
endif

end function int_sign



 function big_endian_io( opt_unit )

 integer, intent(in) :: opt_unit
 logical             :: big_endian_io
! one-byte integer
 integer, parameter :: i1 = selected_int_kind(2)
! two-byte integer
 integer, parameter :: i2 = selected_int_kind(4)
 integer(i1) :: byte_one, byte_two
! 00000000 00000001 big-endian binary
 integer(i2) :: two_byte_int = 1_i2

    open(opt_unit,status='scratch',form='unformatted')
      write( opt_unit) two_byte_int
      rewind(opt_unit)
      read(  opt_unit) byte_one, byte_two
    close(opt_unit)
    big_endian_io = ( byte_one == 0 .and. byte_two == 1 )

 end function big_endian_io


!*******************************************************************************
! This subroutine writes a su2 grid file.
!
! Note: Nodes -> i = 0,1,2,...; Elements -> i = 0,1,2,...
!
!*******************************************************************************
 subroutine write_su2grid_file(nb,bnames)

 integer                      , intent(in) :: nb
 character(80), dimension(:)  , intent(in) :: bnames

 integer :: k, itag, i, ib

  open(unit=7, file=filename_su2grid, status="unknown", iostat=os)

  write(7,*) "%"
  write(7,*) "% Problem dimension"
  write(7,*) "%"
  write(7,5) 3
5 format('NDIME= ',i12)

   write(7,*) "%"
   write(7,*) "% Inner element connectivity"
   k = ntet + nprs + nhex
   write(7,10) k
10 format('NELEM= ',i12)

   k = 0

 !-------------------------------------------------------------------------
 ! Elements

  ! tet
    if (ntet > 0) then
     do i = 1, ntet
      write(7,'(6i20)') 10, tet(i,1)-1, tet(i,2)-1, tet(i,3)-1, tet(i,4)-1, k
      k = k + 1
     end do
    endif

  ! Prism: Orietation is reversed (See VTK format).
    if (nprs > 0) then
     do i = 1, nprs
      write(7,'(8i20)') 13, prs(i,3)-1, prs(i,2)-1, prs(i,1)-1, &
                            prs(i,6)-1, prs(i,5)-1, prs(i,4)-1, k
      k = k + 1
     end do
    endif

  ! Hex
    if (nhex > 0) then
     do i = 1, nhex
      write(7,'(10i20)') 12, hex(i,1)-1, hex(i,2)-1, hex(i,3)-1, hex(i,4)-1, &
                             hex(i,5)-1, hex(i,6)-1, hex(i,7)-1, hex(i,8)-1, k
      k = k + 1
     end do
    endif

   write(*,*) "  --- elm check", ntet + nprs + nhex, k

 !--------------------------------------------------------------------------
 ! Nodes

   write(7,*) "%"
   write(7,*) "% Node coordinates"
   write(7,*) "%"
   write(7,20) nnodes
20 format('NPOIN= ', i12)

   k = 0

  ! Nodes
    do i = 1, nnodes
     write(7,'(3es26.15,i20)') node(i)%x, node(i)%y, node(i)%z, k
      k = k + 1
    end do

   write(*,*) "  --- node check", nnodes, k

 !--------------------------------------------------------------------------
 ! Boundary

    write(7,*) "%"
    write(7,*) "% Boundary elements"
    write(7,*) "%"
    write(7,30) 3
30 format('NMARK= ',i12)

40 format('MARKER_TAG= ',a)
50 format('MARKER_ELEMS= ', i12)

   write(*,*)
   write(*,*)

   do ib = 1, nb

    write(7,40) trim(bnames(ib))

    itag = ib
       k = 0

   ! Triangular faces = ntri
     if (ntrias_b > 0) then
      do i = 1, ntrias_b
       if ( tri(i,5) == itag ) k = k + 1
      end do
     endif

   ! Quad faces = nquad
     if (nquads_b > 0) then
      do i = 1, nquads_b
       if ( quad(i,5) == itag ) k = k + 1
      end do
     endif

   write(7,50) k

   !-------------------------
   !Just to print on screen
    write(*,40) trim(bnames(ib))
    write(*,50) k
   !-------------------------

   ! Triangular faces = ntri
     if (ntrias_b > 0) then
      do i = 1, ntrias_b
       if ( tri(i,5) == itag ) write(7,'(4i20)') 5, tri(i,1)-1, tri(i,2)-1, tri(i,3)-1
      end do
     endif

   ! Quad faces = nquad
     if (nquads_b > 0) then
      do i = 1, nquads_b
       if ( quad(i,5) == itag ) write(7,'(5i20)') 9, quad(i,1)-1, quad(i,2)-1, quad(i,3)-1, quad(i,4)-1
      end do
     endif

   end do

   write(*,*)
   write(*,*)

 !--------------------------------------------------------------------------
 !--------------------------------------------------------------------------

  close(7)

 end subroutine write_su2grid_file
!**********************************************************************

!*******************************************************************************
! This subroutine writes a .vtk file for the grid whose name is defined by
! filename_vtk.
!
!  Identifier:
!  Line          3
!  Triangle      5
!  Quadrilateral 9
!  Tetrahedral  10
!  Hexahedral   12
!  Prism        13
!  Pyramid      14
!
! Note: This version is only for tet, prs, and hex. Need to add pyramids and others
!       if needed.
!
! Use Paraview to read .vtk and visualize it.  https://www.paraview.org
!
! Search in Google for 'vkt format' to learn .vtk file format.
!*******************************************************************************
 subroutine write_vtk_file(filename, nnodes,xp,yp,zp, ntet,tet, nprs,prs, nhex,hex )

  implicit none

  character(80),                 intent(in) :: filename
  integer      ,                 intent(in) :: nnodes
  real(dp)     , dimension(:  ), intent(in) :: xp, yp, zp
  integer      ,                 intent(in) :: ntet, nprs, nhex
  integer      , dimension(:,:), intent(in) :: tet
  integer      , dimension(:,:), intent(in) :: prs
  integer      , dimension(:,:), intent(in) :: hex

!Local variables
  integer :: i, j, os

 !------------------------------------------------------------------------------
 !------------------------------------------------------------------------------
 !------------------------------------------------------------------------------

 !Open the output file.
  open(unit=8, file=filename, status="unknown", iostat=os)

!---------------------------------------------------------------------------
! Header information

  write(8,'(a)') '# vtk DataFile Version 3.0'
  write(8,'(a)') filename
  write(8,'(a)') 'ASCII'
  write(8,'(a)') 'DATASET UNSTRUCTURED_GRID'

!---------------------------------------------------------------------------
! Nodal information
!
! Note: These nodes i=1,nnodes are interpreted as i=0,nnodes-1 in .vtk file.
!       So, later below, the connectivity list for tria and quad will be
!       shifted by -1.

   write(8,*) 'POINTS ', nnodes, ' double'

   do j = 1, nnodes
    write(8,'(3es25.15)') xp(j), yp(j), zp(j)
   end do

!---------------------------------------------------------------------------
! Cell information.

  !CELLS: # of total cells (ntet+nprs+nhex), total size of the cell list.

  write(8,'(a,i12,i12)') 'CELLS ',ntet+nprs+nhex, (4+1)*ntet + (6+1)*nprs + (8+1)*nhex

  ! Note: The latter is the number of integer values written below as data.
  !           5 for tets   (# of vertices + 4 vertices),
  !           7 for prisms (# of vertices + 6 vertices),
  !           9 for hexa   (# of vertices + 8 vertices).

  !---------------------------------
  ! 2.1 List of tets

   if (ntet > 0) then
    do i = 1, ntet
     write(8,'(a,4i12)') '4', tet(i,1)-1, tet(i,2)-1, tet(i,3)-1
                         ! -1 since VTK reads the nodes as 0,1,2,3,..., not 1,2,3,..
    end do
   endif

  !---------------------------------
  ! 2.2 List of prisms

   if (nprs > 0) then
    do i = 1, nprs
     write(8,'(a,6i12)') '6',  prs(i,3)-1, prs(i,2)-1, prs(i,1)-1, &
                               prs(i,6)-1, prs(i,5)-1, prs(i,4)-1
                         ! -1 since VTK reads the nodes as 0,1,2,3,..., not 1,2,3,..
    end do
   endif

  !---------------------------------
  ! 2.3 List of hexa

   if (nhex > 0) then
    do i = 1, nhex
     write(8,'(a,8i12)') '8',  hex(i,1)-1, hex(i,2)-1, hex(i,3)-1, hex(i,4)-1, &
                               hex(i,5)-1, hex(i,6)-1, hex(i,7)-1, hex(i,8)-1
                         ! -1 since VTK reads the nodes as 0,1,2,3,..., not 1,2,3,..
    end do
   endif

!---------------------------------------------------------------------------
! Cell type information.

                                   !# of all cells
  write(8,'(a,i11)') 'CELL_TYPES ', ntet+nprs+nhex

  !Tetrahedron is classified as the cell type 10 in the .vtk format.

  if (ntet > 0) then
   do i = 1, ntet
    write(8,'(i3)') 10
   end do
  endif

  !Prism is classified as the cell type 13 in the .vtk format.

  if (nprs > 0) then
   do i = 1, nprs
    write(8,'(i3)') 13
   end do
  endif

  !Hexahedron is classified as the cell type 12 in the .vtk format.

  if (nhex > 0) then
   do i = 1, nhex
    write(8,'(i3)') 12
   end do
  endif

!---------------------------------------------------------------------------

  close(8)


 end subroutine write_vtk_file
!********************************************************************************


!********************************************************************************
! This solves target_x - cx*(one-exp(sf*xi))/(one-exp(sf)) for sf.
!
! E.g., If one wants to find the stretching factor sf such that
!        (rmax-rmin)*(one-exp(sf*xi))/(one-exp(sf)) = desired spacing,
!       where xi = 1*(rmax-rmin)/n (=1st increment in the uniform spacing),
!       Set cx = rmax-rmin, and target_x=desired spacing.
!
!********************************************************************************
 subroutine compute_stretching_factor(target_x,cx,xi,sf_initial, final_x,sf,itr,stat )
 implicit none

 real(dp), intent( in) :: target_x, cx, xi, sf_initial
 real(dp), intent(out) :: final_x, sf
 integer , intent(out) :: itr, stat

 real(dp)              :: res, dres, gr

 
  stat = 0
   itr = 0
    sf = sf_initial

 do

   !Equation to sovle: res = 0.0.

       gr = (one-exp(sf*xi))/(one-exp(sf))
      res =  target_x - cx*gr


      final_x = gr*cx

     if (debug_mode) write(1000,'(a,i6,a,es10.2,a,es10.2,a,es10.2,a,es10.2)') &
       "   itr=", itr, " current_x = ", final_x, " target_x = ", target_x, &
       " sf=",sf,"  res=", abs(res/target_x)

     if (abs(res)/target_x < 1.0e-08_dp) exit !<--Smaller the better.

   ! The following doesn't work. This creates a trivial solution sf=0
   ! and Newton's method converges to it.
   !     res =  dr1*(one-exp(sf)) - rmax*(one-exp(sf*xi))
   !    dres = -dr1*     exp(sf)  +   xi*rmax*exp(sf*xi)

   !So, we solve directly: target_x - cx*(one-exp(sf*xi))/(one-exp(sf))
   !Its derivative is 
    dres = - cx*( -xi*exp(sf*xi)*(one-exp(sf)) + exp(sf)*(one-exp(sf*xi)) )/(one-exp(sf))**2

   !Newton iteration:
      sf = sf - res/dres

  itr = itr + 1
  if (itr > 500) then
    stat = 1
    exit
  endif

  if (abs(sf) > 1.0e+04) then
   write(1000,*) " Too large sf ... = ", sf 
   stop
  endif

 end do

   if (sf < 0.0_dp) stat = 2 !Negative stretching factor.

 end subroutine compute_stretching_factor
!********************************************************************************

end program sphere_grid
