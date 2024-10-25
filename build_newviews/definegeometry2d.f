      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      program definegeometry2d
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      include 'mpif.h'
      integer mpi_err
      integer mpi_status
      dimension mpi_status(MPI_STATUS_SIZE)
      character*96 filename
      call MPI_init(mpi_err)
      mpi_degas2_root=0
      call MPI_Comm_dup(MPI_COMM_WORLD,comm_world_dup,mpi_err)
      call MPI_comm_rank(comm_world_dup,mpi_rank,mpi_err)
      call MPI_comm_size(comm_world_dup,mpi_size,mpi_err)
      call readfilenames
      call command_arg(1,filename)
      call nc_read_materials
      call def_geom_2d_main(filename)
      call MPI_barrier(comm_world_dup,mpi_err)
      call MPI_finalize(mpi_err)
      stop
      end
      subroutine def_geom_2d_main(filename)
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      use gi_mod
      
      use zn_mod
      
      use g2_mod
      
      use sc_mod
      
      use ma_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*96 filename
      integer diskin2,length,p,b,e,open_stat,zone,fileid,prep_done,i_inc
     &_g2,i_prop,nxpt
      integer current_int_props(1:3)
      REAL(kind=DOUBLE)rtest
      REAL(kind=DOUBLE)current_real_props(1:5)
      character*300 line,keyword
      character*96 tmpfilename
      integer ielement,section,i,inode,miss_elem,start,end,iwall,iseg,ie
     &lement2,skip_elem,poly_elem,num_h_elems,num_mesh_elems,mesh_elem,e
     &xp_inc
      REAL(kind=DOUBLE)temp_x,temp_z,mult
      integer mesh_sense,ix,iz,n,mesh_pt,poly_pt,start_pt,end_pt,inc_pt
      logical new_pt
      REAL(kind=DOUBLE)yhat(3)
      REAL(kind=DOUBLE)test_vec_1(3)
      REAL(kind=DOUBLE)test_vec_2(3)
      REAL(kind=DOUBLE)test_vec_3(3)
      REAL(kind=DOUBLE)center(3)
      REAL(kind=DOUBLE)x_min,x_max,z_min,z_max,xb_min,xb_max,yb_min,yb_m
     &ax,zb_min,zb_max,vol
      REAL(kind=DOUBLE)min_corner(3)
      REAL(kind=DOUBLE)max_corner(3)
      integer nxd_0,nzd_0,ix_min,ix_max,iz_min,iz_max,nx_nz_max,mesh_tot
     &_elements,iedge,itip,new_node,mesh_tot_nodes
      integer mesh_edge_num_elements(4),mesh_ix_start(4),mesh_ix_end(4),
     &mesh_iz_start(4),mesh_iz_end(4),mesh_ix_step(4),mesh_iz_step(4),me
     &sh_corner(0:1,4)
      REAL(kind=DOUBLE)test_node(1:2)
      integer nunit
      character*300 file_format
      character*96 walloutfile
      integer num_new_walls,this_node,with_sonnet
      REAL(kind=DOUBLE)x_wall,z_wall,xz_tmp
      character*96 wallinfile
      character*17 exp_string
      integer symmetry
      integer solid_faces(0:1),solid_zone(0:0),end_faces(0:1)
      character*300 one_zone_type(0:0)
      REAL(kind=DOUBLE)coeff(10),poly4(1:2,0:4)
      REAL(kind=DOUBLE)y_border,cos_y,sin_y
      REAL(kind=DOUBLE)solid_ys(0:1)
      REAL(kind=DOUBLE)a_y(3)
      REAL(kind=DOUBLE)b_y(3)
      integer process_polygon,ntriangles,refine,j,k,temp_num_stratum_pts
     &,temp_aux_stratum,temp_aux_segment,i_aux,nt,i_2,i_2_range,i_2_loop
      integer temp_int_props(1:3),temp_segment(0:3,0:100000-1),temp_stra
     &tum_pts(0:2000-1)
      REAL(kind=DOUBLE)temp_polygon(1:2,0:2000-1),temp_triangles(1:2,0:3
     &,0:100000-1),temp_real_props(1:5),temp_holes(1:2,0:0)
      character*300 new_zone_type,new_zone_type_2
      integer write_poly_nc
      character*96 polygon_nc_file
      integer face1,num_zone1,num_zone2,k_zone1,k_zone2,solid_zone_p,sol
     &id_face,other_zone,other_face,this_poly,i_poly,this_seg,i_sect,sol
     &id_sector,other_sector,other_type,y_max,this_stratum,this_mat,new_
     &solid_sector,new_other_sector,other_stratum,other_seg
      REAL(kind=DOUBLE)this_temp,this_rc
      integer i_diag,i_grp,is_aux_sector
      integer grp_sectors(80000)
      integer y_mat(0:1),y_stratum(0:1),y_p_stratum(0:1),end_zones(0:1)
      integer num_nodes,num_elements,num_walls,dim_nodes,dim_elements,di
     &m_walls,nxd,nzd,num_y,y_div,dim_y,dim_ym,num_aux_sectors,dim_aux_s
     &ectors,num_diags,dim_diags,num_dg_poly,dim_dg_poly
      REAL(kind=DOUBLE),dimension(:,:),pointer::nodes
      integer,dimension(:),pointer::node_type
      integer,dimension(:),pointer::node_element_count
      integer,dimension(:,:),pointer::dg_elements_list
      integer,dimension(:),pointer::element_missing
      integer,dimension(:),pointer::element_skipped
      integer,dimension(:),pointer::element_assigned
      integer,dimension(:,:),pointer::dg_polygons
      integer,dimension(:),pointer::dg_poly_num_elements
      integer,dimension(:),pointer::dg_poly_num_meshcon
      integer,dimension(:,:,:),pointer::dg_poly_meshcon
      integer,dimension(:,:,:),pointer::dg_poly_meshcon_hv
      integer,dimension(:,:),pointer::wall_nodes
      integer,dimension(:,:),pointer::wall_elements
      integer,dimension(:),pointer::wall_segment_count
      REAL(kind=DOUBLE),dimension(:),pointer::y_values
      integer,dimension(:),pointer::facearray
      integer,dimension(:),pointer::zonearray
      character(len=(300)),dimension(:),pointer::zone_type_array
      integer,dimension(:),pointer::sect_zone1
      integer,dimension(:),pointer::sect_zone2
      integer,dimension(:),pointer::aux_stratum
      integer,dimension(:),pointer::aux_stratum_poly
      integer,dimension(:),pointer::aux_stratum_points
      integer,dimension(:),pointer::aux_stratum_segment
      character(len=(40)),dimension(:),pointer::diag_name
      integer,dimension(:),pointer::diag_stratum
      integer,dimension(:),pointer::diag_solid
      integer,dimension(:),pointer::diag_variable
      integer,dimension(:),pointer::diag_tab_index
      REAL(kind=DOUBLE),dimension(:),pointer::diag_var_min
      REAL(kind=DOUBLE),dimension(:),pointer::diag_var_max
      REAL(kind=DOUBLE),dimension(:),pointer::diag_mult
      integer,dimension(:),pointer::diag_spacing
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::mesh_xz
      REAL(kind=DOUBLE),dimension(:,:),pointer::mesh_nodes
      integer,dimension(:,:),pointer::mesh_elements
      integer,dimension(:,:),pointer::mesh_edge_elements
      integer,dimension(:,:),pointer::mesh_edge_dg_label
      integer,dimension(:),pointer::mesh_edge_hv
      integer,dimension(:),pointer::mesh_curve_num
      integer,dimension(:),pointer::mesh_scratch
      integer,dimension(:,:),pointer::poly_int_props
      REAL(kind=DOUBLE),dimension(:,:),pointer::poly_real_props
      interface
      function mem_alloc_c1(size,l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1
      character(len=size),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_c2(size,l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2
      character(len=size),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_c3(size,l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3
      character(len=size),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_c4(size,l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4
      character(len=size),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i1(l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      integer,dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i2(l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      integer,dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i3(l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      integer,dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i4(l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      integer,dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i5(l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      integer,dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r1(l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      REAL(kind=DOUBLE),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r2(l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      REAL(kind=DOUBLE),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r3(l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r4(l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r5(l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c1(p,size,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,nu
      character(len=size),dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c2(p,size,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,nu
      character(len=size),dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c3(p,size,l1,u1,l2,u2,l3,u3,nu,name)result(np
     &)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,nu
      character(len=size),dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c4(p,size,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)res
     &ult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4,nu
      character(len=size),dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i1(p,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,nu
      integer,dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i2(p,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,nu
      integer,dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i3(p,l1,u1,l2,u2,l3,u3,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,nu
      integer,dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i4(p,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)result(n
     &p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,nu
      integer,dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu,name)re
     &sult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu
      integer,dimension(:,:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r1(p,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,nu
      REAL(kind=DOUBLE),dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r2(p,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,nu
      REAL(kind=DOUBLE),dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r3(p,l1,u1,l2,u2,l3,u3,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,nu
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r4(p,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)result(n
     &p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,nu
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu,name)re
     &sult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      subroutine mem_free_c1(p,size,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1
      character(len=size),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_c2(p,size,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2
      character(len=size),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_c3(p,size,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3
      character(len=size),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_c4(p,size,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4
      character(len=size),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i1(p,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      integer,dimension(:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i2(p,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      integer,dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i3(p,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      integer,dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i4(p,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      integer,dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      integer,dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r1(p,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      REAL(kind=DOUBLE),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r2(p,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      REAL(kind=DOUBLE),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r3(p,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r4(p,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      end interface
      
      
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      REAL(kind=DOUBLE)vector_temp(3)
      integer nc_dims(5),nc_corner(5),nc_edge(5),nc_stat,nc_size,nc_rank
     &,nc_attr,nc_type
      character*128 nc_dummy
      external ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf
     &_inq_format
      integer ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf_
     &inq_format
      integer g2_points_ind0_id
      integer g2_points_ind_id
      integer g2_points_tot_ind0_id
      integer g2_points_tot_ind_id
      integer g2_xz_ind_id
      integer g2_num_polygons_id
      integer g2_poly_ind_id
      integer g2_polygon_xz_id
      integer g2_polygon_segment_id
      integer g2_polygon_points_id
      integer g2_polygon_zone_id
      integer g2_polygon_stratum_id
      
      external define_surface,define_surface_a,start_cell,polygon_volume
     &,define_sector,lookup_surface
      integer define_surface,define_surface_a,start_cell,define_sector,l
     &ookup_surface
      REAL(kind=DOUBLE)polygon_volume
      
      open(unit=20,file=filename,status='old',form='formatted')
      diskin2=20+1
      zone=0
      g2_num_polygons=0
      symmetry=0
      xb_min=(0.0_DOUBLE)
      xb_max=(0.0_DOUBLE)
      yb_min=(0.0_DOUBLE)
      yb_max=(0.0_DOUBLE)
      zb_min=(0.0_DOUBLE)
      zb_max=(0.0_DOUBLE)
      nxd=0
      nzd=0
      num_walls=0
      num_nodes=0
      num_elements=0
      num_dg_poly=0
      num_aux_sectors=0
      num_diags=0
      dim_walls=100
      dim_nodes=100
      dim_elements=100
      dim_aux_sectors=100
      dim_diags=100
      dim_dg_poly=100
      nodes =>mem_alloc_r2((1),(2),(1),(dim_nodes),'nodes')
      node_type =>mem_alloc_i1((1),(dim_nodes),'node_type')
      node_element_count =>mem_alloc_i1((1),(dim_nodes),'node_element_co
     &unt')
      dg_elements_list =>mem_alloc_i2((0),(1),(1),(dim_elements),'dg_ele
     &ments_list')
      element_missing =>mem_alloc_i1((1),(dim_elements),'element_missing
     &')
      element_skipped =>mem_alloc_i1((1),(dim_elements),'element_skipped
     &')
      element_assigned =>mem_alloc_i1((1),(dim_elements),'element_assign
     &ed')
      dg_polygons =>mem_alloc_i2((1),(2000),(1),(dim_dg_poly),'dg_polygo
     &ns')
      dg_poly_num_elements =>mem_alloc_i1((1),(dim_dg_poly),'dg_poly_num
     &_elements')
      dg_poly_num_meshcon =>mem_alloc_i1((1),(dim_dg_poly),'dg_poly_num_
     &meshcon')
      dg_poly_meshcon =>mem_alloc_i3((1),(2),(1),(2),(1),(dim_dg_poly),'
     &dg_poly_meshcon')
      dg_poly_meshcon_hv =>mem_alloc_i3((1),(2),(1),(2),(1),(dim_dg_poly
     &),'dg_poly_meshcon_hv')
      wall_nodes =>mem_alloc_i2((0),(200000-1),(1),(dim_walls),'wall_nod
     &es')
      wall_elements =>mem_alloc_i2((1),(200000),(1),(dim_walls),'wall_el
     &ements')
      wall_segment_count =>mem_alloc_i1((1),(dim_walls),'wall_segment_co
     &unt')
      y_div=1
      dim_y=100
      dim_ym=100-1
      y_values =>mem_alloc_r1((0),(dim_ym),'y_values')
      facearray =>mem_alloc_i1((0),(dim_ym),'facearray')
      zonearray =>mem_alloc_i1((0),(dim_y-1),'zonearray')
      zone_type_array =>mem_alloc_c1((300),(0),(dim_y-1),'zone_type_arra
     &y')
      y_values(0)=(2.0e30_DOUBLE)
      y_values(1)=(2.0e30_DOUBLE)
      y_mat(0)=2000000000
      y_mat(1)=2000000000
      y_stratum(0)=2000000000
      y_stratum(1)=2000000000
      y_p_stratum(0)=2000000000
      y_p_stratum(1)=2000000000
      end_zones(0)=2000000000
      end_zones(1)=2000000000
      aux_stratum =>mem_alloc_i1((1),(dim_aux_sectors),'aux_stratum')
      aux_stratum_poly =>mem_alloc_i1((1),(dim_aux_sectors),'aux_stratum
     &_poly')
      aux_stratum_points =>mem_alloc_i1((1),(dim_aux_sectors),'aux_strat
     &um_points')
      aux_stratum_segment =>mem_alloc_i1((1),(dim_aux_sectors),'aux_stra
     &tum_segment')
      diag_name =>mem_alloc_c1((40),(1),(dim_diags),'diag_name')
      diag_stratum =>mem_alloc_i1((1),(dim_diags),'diag_stratum')
      diag_solid =>mem_alloc_i1((1),(dim_diags),'diag_solid')
      diag_variable =>mem_alloc_i1((1),(dim_diags),'diag_variable')
      diag_tab_index =>mem_alloc_i1((1),(dim_diags),'diag_tab_index')
      diag_var_min =>mem_alloc_r1((1),(dim_diags),'diag_var_min')
      diag_var_max =>mem_alloc_r1((1),(dim_diags),'diag_var_max')
      diag_mult =>mem_alloc_r1((1),(dim_diags),'diag_mult')
      diag_spacing =>mem_alloc_i1((1),(dim_diags),'diag_spacing')
      prep_done=0
      current_int_props(1)=1000000000
      current_int_props(2)=0
      current_int_props(3)=0
      current_real_props(1)=(3.e2_DOUBLE)
      current_real_props(2)=(1.0_DOUBLE)
      current_real_props(3)=(1.e-3_DOUBLE)
      current_real_props(4)=(1.0e30_DOUBLE)
      current_real_props(5)=(1.0e30_DOUBLE)
      write_poly_nc=1
      polygon_nc_file='polygon.nc'
      
90012 continue
      if(read_string(20,line,length))continue
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      keyword=line(b:e)
      if(keyword.EQ.'dg_file')then
      if(next_token(line,b,e,p))continue
      tmpfilename=line(b:e)
      open(unit=diskin2,file=tmpfilename,status='old',form='formatted',i
     &ostat=open_stat)
      if(open_stat.NE.0)then
      write(0,*)' Cannot open file ',line(b:e),', error number ',open_st
     &at
      return
      end if
      ielement=0
      section=0
      if(index(line(b:e),'.dgo').GT.0)then
      exp_inc=-3
      else
      exp_inc=0
      end if
90000 continue
      if(read_string(diskin2,line,length))then
      if(length.LE.len(line))continue
      do i=1,length
      if(line(i:i).EQ.',')line(i:i)=' '
      end do
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.'p1')then
      section=1
      else if(line(b:e).EQ.'p2')then
      section=2
      else if(line(b:e).EQ.'misselem')then
      section=3
      else if(line(b:e).EQ.'skipelem')then
      section=4
      else if(line(b:e).EQ.'polygon')then
      if(next_token(line,b,e,p))continue
      num_dg_poly=num_dg_poly+1
      dim_dg_poly=max(dim_dg_poly,num_dg_poly)
      if(mod(((dim_dg_poly)-(1)+1),100).EQ.1)then
      dg_polygons =>mem_realloc_i2(dg_polygons,(1),(2000),(1),(((int((((
     &(dim_dg_poly)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_d
     &g_poly)-(1)+1))+100-1)/100)*100)+(1)-1),'dg_polygons')
      end if
      if(mod(((dim_dg_poly)-(1)+1),100).EQ.1)then
      dg_poly_num_elements =>mem_realloc_i1(dg_poly_num_elements,(1),(((
     &int(((((dim_dg_poly)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((
     &((dim_dg_poly)-(1)+1))+100-1)/100)*100)+(1)-1),'dg_poly_num_elemen
     &ts')
      end if
      if(mod(((dim_dg_poly)-(1)+1),100).EQ.1)then
      dg_poly_num_meshcon =>mem_realloc_i1(dg_poly_num_meshcon,(1),(((in
     &t(((((dim_dg_poly)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((
     &dim_dg_poly)-(1)+1))+100-1)/100)*100)+(1)-1),'dg_poly_num_meshcon'
     &)
      end if
      if(mod(((dim_dg_poly)-(1)+1),100).EQ.1)then
      dg_poly_meshcon =>mem_realloc_i3(dg_poly_meshcon,(1),(2),(1),(2),(
     &1),(((int(((((dim_dg_poly)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((
     &int(((((dim_dg_poly)-(1)+1))+100-1)/100)*100)+(1)-1),'dg_poly_mesh
     &con')
      end if
      if(mod(((dim_dg_poly)-(1)+1),100).EQ.1)then
      dg_poly_meshcon_hv =>mem_realloc_i3(dg_poly_meshcon_hv,(1),(2),(1)
     &,(2),(1),(((int(((((dim_dg_poly)-(1)+1))+100-1)/100)*100)-100)+(1)
     &-1),((int(((((dim_dg_poly)-(1)+1))+100-1)/100)*100)+(1)-1),'dg_pol
     &y_meshcon_hv')
      end if
      if(num_dg_poly.EQ.read_integer(line(b:e)))continue
      dg_poly_num_elements(num_dg_poly)=0
      dg_poly_num_meshcon(num_dg_poly)=0
      do i=1,2
      do j=1,2
      dg_poly_meshcon(j,i,num_dg_poly)=2000000000
      dg_poly_meshcon_hv(j,i,num_dg_poly)=2000000000
      end do
      end do
      section=5
      else if(line(b:e).EQ.'wall')then
      if(section.EQ.5)continue
      section=6
      else if(line(b:e).EQ.'polymat')then
      if(section.EQ.6)continue
      else if(line(b:e).EQ.'meshcon1')then
      if(section.EQ.6)continue
      section=7
      else if(line(b:e).EQ.'meshcon2')then
      if(section.EQ.7)continue
      section=8
      else if(line(b:e).EQ.'finish')then
      section=40
      else
      if(section.EQ.1.OR.section.EQ.2)then
      temp_x=read_real_scaled(line(b:e),exp_inc)
      if(temp_x.NE.(1.0e30_DOUBLE))then
      if(next_token(line,b,e,p))continue
      temp_z=read_real_scaled(line(b:e),exp_inc)
      end if
      if(temp_x.EQ.(1.0e30_DOUBLE).OR.temp_z.EQ.(1.0e30_DOUBLE))then
      section=0
      go to 90000
      end if
      inode=0
      if(num_nodes.GT.0)then
      do i=1,num_nodes
      if(nodes(1,i).EQ.temp_x)then
      if(nodes(2,i).EQ.temp_z)then
      if(inode.EQ.0)continue
      inode=i
      end if
      end if
      end do
      end if
      if(inode.EQ.0)then
      num_nodes=num_nodes+1
      dim_nodes=max(dim_nodes,num_nodes)
      if(mod(((dim_nodes)-(1)+1),100).EQ.1)then
      nodes =>mem_realloc_r2(nodes,(1),(2),(1),(((int(((((dim_nodes)-(1)
     &+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_nodes)-(1)+1))+100
     &-1)/100)*100)+(1)-1),'nodes')
      end if
      if(mod(((dim_nodes)-(1)+1),100).EQ.1)then
      node_type =>mem_realloc_i1(node_type,(1),(((int(((((dim_nodes)-(1)
     &+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_nodes)-(1)+1))+100
     &-1)/100)*100)+(1)-1),'node_type')
      end if
      if(mod(((dim_nodes)-(1)+1),100).EQ.1)then
      node_element_count =>mem_realloc_i1(node_element_count,(1),(((int(
     &((((dim_nodes)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_
     &nodes)-(1)+1))+100-1)/100)*100)+(1)-1),'node_element_count')
      end if
      inode=num_nodes
      nodes(1,num_nodes)=temp_x
      nodes(2,num_nodes)=temp_z
      end if
      if(section.EQ.1)then
      num_elements=num_elements+1
      dim_elements=max(dim_elements,num_elements)
      if(mod(((dim_elements)-(1)+1),100).EQ.1)then
      dg_elements_list =>mem_realloc_i2(dg_elements_list,(0),(1),(1),(((
     &int(((((dim_elements)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int((
     &(((dim_elements)-(1)+1))+100-1)/100)*100)+(1)-1),'dg_elements_list
     &')
      end if
      if(mod(((dim_elements)-(1)+1),100).EQ.1)then
      element_missing =>mem_realloc_i1(element_missing,(1),(((int(((((di
     &m_elements)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_ele
     &ments)-(1)+1))+100-1)/100)*100)+(1)-1),'element_missing')
      end if
      if(mod(((dim_elements)-(1)+1),100).EQ.1)then
      element_skipped =>mem_realloc_i1(element_skipped,(1),(((int(((((di
     &m_elements)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_ele
     &ments)-(1)+1))+100-1)/100)*100)+(1)-1),'element_skipped')
      end if
      if(mod(((dim_elements)-(1)+1),100).EQ.1)then
      element_assigned =>mem_realloc_i1(element_assigned,(1),(((int(((((
     &dim_elements)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_e
     &lements)-(1)+1))+100-1)/100)*100)+(1)-1),'element_assigned')
      end if
      dg_elements_list(0,num_elements)=inode
      element_missing(num_elements)=0
      element_skipped(num_elements)=0
      element_assigned(num_elements)=0
      else if(section.EQ.2)then
      ielement=ielement+1
      dg_elements_list(1,ielement)=inode
      end if
      else if(section.EQ.3)then
      if(ielement.EQ.num_elements)continue
      miss_elem=read_int_soft_fail(line(b:e))
      if(miss_elem.GT.0.AND.miss_elem.LE.num_elements)then
      element_missing(miss_elem)=1
      else
      if(miss_elem.EQ.1000000000)continue
      section=0
      go to 90000
      end if
      else if(section.EQ.4)then
      if(ielement.EQ.num_elements)continue
      skip_elem=read_int_soft_fail(line(b:e))
      if(skip_elem.GT.0.AND.skip_elem.LE.num_elements)then
      element_skipped(skip_elem)=1
      else
      if(skip_elem.EQ.1000000000)continue
      section=0
      go to 90000
      end if
      else if(section.EQ.6)then
      poly_elem=read_int_soft_fail(line(b:e))
      if((poly_elem.GT.0).AND.(poly_elem.LE.num_elements))continue
      if(num_dg_poly.GT.0)continue
      dg_poly_num_elements(num_dg_poly)=dg_poly_num_elements(num_dg_poly
     &)+1
      dg_polygons(dg_poly_num_elements(num_dg_poly),num_dg_poly)=poly_el
     &em
      else if((section.EQ.7).OR.(section.EQ.8))then
      num_h_elems=read_int_soft_fail(line(b:e))
      if((num_h_elems.GE.0).AND.(num_h_elems.LE.2))continue
      num_mesh_elems=0
90001 continue
      if(read_string(diskin2,line,length))continue
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      mesh_elem=read_int_soft_fail(line(b:e))
      if(mesh_elem.NE.1000000000)then
      if(num_mesh_elems.EQ.0)then
      dg_poly_num_meshcon(num_dg_poly)=dg_poly_num_meshcon(num_dg_poly)+
     &1
      if(dg_poly_num_meshcon(num_dg_poly).LE.2)continue
      end if
      num_mesh_elems=num_mesh_elems+1
      if(num_mesh_elems.LE.2)continue
      dg_poly_meshcon(num_mesh_elems,dg_poly_num_meshcon(num_dg_poly),nu
     &m_dg_poly)=mesh_elem
      if(num_mesh_elems.LE.num_h_elems)then
      dg_poly_meshcon_hv(num_mesh_elems,dg_poly_num_meshcon(num_dg_poly)
     &,num_dg_poly)=0
      else
      dg_poly_meshcon_hv(num_mesh_elems,dg_poly_num_meshcon(num_dg_poly)
     &,num_dg_poly)=1
      end if
      go to 90001
      else
      backspace(diskin2)
      if(num_mesh_elems.GE.num_h_elems)continue
      if((num_mesh_elems.EQ.0).OR.(num_mesh_elems.EQ.2))continue
      if(section.EQ.8)section=0
      go to 90000
      end if
      end if
      end if
      if(section.NE.40)go to 90000
      end if
      close(unit=diskin2)
      do inode=1,num_nodes
      node_element_count(inode)=0
      start=0
      end=0
      do ielement=1,num_elements
      if((element_missing(ielement).EQ.0).AND.(element_skipped(ielement)
     &.EQ.0))then
      if(dg_elements_list(0,ielement).EQ.inode)then
      node_element_count(inode)=node_element_count(inode)+1
      start=1
      else if(dg_elements_list(1,ielement).EQ.inode)then
      node_element_count(inode)=node_element_count(inode)+1
      end=1
      end if
      end if
      end do
      if(node_element_count(inode).EQ.0)then
      node_type(inode)=2
      else if(node_element_count(inode).EQ.1)then
      if(start.EQ.1.OR.end.EQ.1)continue
      node_type(inode)=3
      else if(node_element_count(inode).EQ.2)then
      if(start.EQ.1.AND.end.EQ.1)then
      node_type(inode)=1
      else
      node_type(inode)=5
      end if
      else if(node_element_count(inode).GT.2)then
      node_type(inode)=4
      end if
      end do
      do inode=1,num_nodes
      if(node_type(inode).NE.1.AND.node_type(inode).NE.2)then
      do ielement=1,num_elements
      if(dg_elements_list(0,ielement).EQ.inode)then
      num_walls=num_walls+1
      dim_walls=max(dim_walls,num_walls)
      if(mod(((dim_walls)-(1)+1),100).EQ.1)then
      wall_nodes =>mem_realloc_i2(wall_nodes,(0),(200000-1),(1),(((int((
     &(((dim_walls)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_w
     &alls)-(1)+1))+100-1)/100)*100)+(1)-1),'wall_nodes')
      end if
      if(mod(((dim_walls)-(1)+1),100).EQ.1)then
      wall_elements =>mem_realloc_i2(wall_elements,(1),(200000),(1),(((i
     &nt(((((dim_walls)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((d
     &im_walls)-(1)+1))+100-1)/100)*100)+(1)-1),'wall_elements')
      end if
      if(mod(((dim_walls)-(1)+1),100).EQ.1)then
      wall_segment_count =>mem_realloc_i1(wall_segment_count,(1),(((int(
     &((((dim_walls)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_
     &walls)-(1)+1))+100-1)/100)*100)+(1)-1),'wall_segment_count')
      end if
      wall_elements(1,num_walls)=ielement
      wall_nodes(0,num_walls)=inode
      element_assigned(ielement)=1
      end if
      end do
      end if
      end do
      do iwall=1,num_walls
      iseg=1
90002 continue
      inode=dg_elements_list(1,wall_elements(iseg,iwall))
      if(inode.GT.0.AND.inode.LE.num_nodes)continue
      wall_nodes(iseg,iwall)=inode
      if(node_type(inode).EQ.1)then
      iseg=iseg+1
      if(iseg.LE.200000)continue
      wall_elements(iseg,iwall)=0
      do ielement=1,num_elements
      if((dg_elements_list(0,ielement).EQ.inode).AND.(element_skipped(ie
     &lement).EQ.0))then
      if(wall_elements(iseg,iwall).EQ.0)continue
      wall_elements(iseg,iwall)=ielement
      element_assigned(ielement)=1
      go to 90002
      end if
      end do
      else
      wall_segment_count(iwall)=iseg
      end if
      end do
      do ielement=1,num_elements
      if((element_missing(ielement).EQ.0).AND.(element_skipped(ielement)
     &.EQ.0).AND.(element_assigned(ielement).EQ.0))then
      num_walls=num_walls+1
      dim_walls=max(dim_walls,num_walls)
      if(mod(((dim_walls)-(1)+1),100).EQ.1)then
      wall_nodes =>mem_realloc_i2(wall_nodes,(0),(200000-1),(1),(((int((
     &(((dim_walls)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_w
     &alls)-(1)+1))+100-1)/100)*100)+(1)-1),'wall_nodes')
      end if
      if(mod(((dim_walls)-(1)+1),100).EQ.1)then
      wall_elements =>mem_realloc_i2(wall_elements,(1),(200000),(1),(((i
     &nt(((((dim_walls)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((d
     &im_walls)-(1)+1))+100-1)/100)*100)+(1)-1),'wall_elements')
      end if
      if(mod(((dim_walls)-(1)+1),100).EQ.1)then
      wall_segment_count =>mem_realloc_i1(wall_segment_count,(1),(((int(
     &((((dim_walls)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_
     &walls)-(1)+1))+100-1)/100)*100)+(1)-1),'wall_segment_count')
      end if
      wall_elements(1,num_walls)=ielement
      wall_nodes(0,num_walls)=dg_elements_list(0,ielement)
      element_assigned(ielement)=1
      iseg=1
90003 continue
      inode=dg_elements_list(1,wall_elements(iseg,num_walls))
      if(inode.GT.0.AND.inode.LE.num_nodes)continue
      wall_nodes(iseg,num_walls)=inode
      if(node_type(inode).EQ.1)continue
      do ielement2=ielement+1,num_elements
      if((dg_elements_list(0,ielement2).EQ.inode).AND.(element_assigned(
     &ielement2).EQ.0))then
      iseg=iseg+1
      if(iseg.LE.200000)continue
      wall_elements(iseg,num_walls)=ielement2
      element_assigned(ielement2)=1
      go to 90003
      end if
      end do
      wall_segment_count(num_walls)=iseg
      end if
      end do
      
      else if(keyword.EQ.'sonnet_mesh'.OR.keyword.EQ.'uedge_mesh')then
      if(next_token(line,b,e,p))continue
      tmpfilename=line(b:e)
      open(unit=diskin2,file=tmpfilename,status='old',form='formatted',i
     &ostat=open_stat)
      if(open_stat.NE.0)then
      write(0,*)' Cannot open file ',line(b:e),', error number ',open_st
     &at
      return
      end if
      if(keyword.EQ.'sonnet_mesh')then
      if(next_token(line,b,e,p))continue
      nxd_0=read_integer(line(b:e))
      if(next_token(line,b,e,p))continue
      nzd_0=read_integer(line(b:e))
      if(next_token(line,b,e,p))then
      ix_min=read_integer(line(b:e))
      if(next_token(line,b,e,p))continue
      ix_max=read_integer(line(b:e))
      if(next_token(line,b,e,p))continue
      iz_min=read_integer(line(b:e))
      if(next_token(line,b,e,p))continue
      iz_max=read_integer(line(b:e))
      nxd=ix_max-ix_min+1
      nzd=iz_max-iz_min+1
      else
      nxd=nxd_0
      nzd=nzd_0
      ix_min=1
      ix_max=nxd
      iz_min=1
      iz_max=nzd
      end if
      else if(keyword.EQ.'uedge_mesh')then
      read(diskin2,*)
      read(diskin2,*)nxd,nzd,nxpt
      end if
      mesh_xz =>mem_alloc_r3((1),(2),(0),(4),(1),(nxd*nzd),'mesh_xz')
      mesh_nodes =>mem_alloc_r2((1),(2),(1),(4*(nxd+nzd)),'mesh_nodes')
      mesh_elements =>mem_alloc_i2((0),(1),(1),(2*(nxd+nzd)),'mesh_eleme
     &nts')
      nx_nz_max=max(nxd,nzd)
      mesh_edge_elements =>mem_alloc_i2((1),(4),(1),(nx_nz_max),'mesh_ed
     &ge_elements')
      mesh_edge_dg_label =>mem_alloc_i2((1),(4),(1),(nx_nz_max),'mesh_ed
     &ge_dg_label')
      mesh_edge_hv =>mem_alloc_i1((1),(4),'mesh_edge_hv')
      mesh_curve_num =>mem_alloc_i1((1),(2*(nxd+nzd)),'mesh_curve_num')
      mesh_scratch =>mem_alloc_i1((1),(2*(nxd+nzd)),'mesh_scratch')
      if(keyword.EQ.'sonnet_mesh')then
      call read_sonnet_mesh(diskin2,nxd_0,nzd_0,ix_min,ix_max,iz_min,iz_
     &max,nxd,nzd,mesh_xz)
      else if(keyword.EQ.'uedge_mesh')then
      call read_uedge_mesh(diskin2,nxd,nzd,nxpt,mesh_xz)
      nxd_0=nxd
      nzd_0=nzd
      ix_min=1
      ix_max=nxd
      iz_min=1
      iz_max=nzd
      end if
      else if(keyword.EQ.'wallfile')then
      
      if(next_token(line,b,e,p))continue
      wallinfile=line(b:e)
      if(next_token(line,b,e,p))then
      if(line(b:e).EQ.'with_sonnet')continue
      with_sonnet=1
      else
      with_sonnet=0
      end if
      open(unit=diskin2,file=wallinfile,status='old',form='formatted')
      if(read_string(diskin2,line,length))continue
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      num_new_walls=read_int_soft_fail(line(b:e))
      if((int((((dim_walls)-(1)+1)+100-1)/100)*100).NE.(int((((num_walls
     &+num_new_walls)-(1)+1)+100-1)/100)*100))then
      wall_nodes =>mem_realloc_i2(wall_nodes,(0),(200000-1),(1),((int(((
     &(dim_walls)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((num_walls+num_
     &new_walls)-(1)+1)+100-1)/100)*100)+(1)-1),'wall_nodes')
      end if
      if((int((((dim_walls)-(1)+1)+100-1)/100)*100).NE.(int((((num_walls
     &+num_new_walls)-(1)+1)+100-1)/100)*100))then
      wall_elements =>mem_realloc_i2(wall_elements,(1),(200000),(1),((in
     &t((((dim_walls)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((num_walls+
     &num_new_walls)-(1)+1)+100-1)/100)*100)+(1)-1),'wall_elements')
      end if
      if((int((((dim_walls)-(1)+1)+100-1)/100)*100).NE.(int((((num_walls
     &+num_new_walls)-(1)+1)+100-1)/100)*100))then
      wall_segment_count =>mem_realloc_i1(wall_segment_count,(1),((int((
     &((dim_walls)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((num_walls+num
     &_new_walls)-(1)+1)+100-1)/100)*100)+(1)-1),'wall_segment_count')
      end if
      if(read_string(diskin2,line,length))continue
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      do iwall=1,num_new_walls
      if(.NOT.next_token(line,b,e,p))then
      if(read_string(diskin2,line,length))continue
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      end if
      wall_segment_count(num_walls+iwall)=read_int_soft_fail(line(b:e))
      end do
      do iwall=num_walls+1,num_walls+num_new_walls
      wall_segment_count(iwall)=wall_segment_count(iwall)-1
      do iseg=0,wall_segment_count(iwall)
      if(read_string(diskin2,line,length))continue
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      xz_tmp=read_real_soft_fail(line(b:e))
      if(with_sonnet.EQ.1)then
      write(exp_string,'(e17.10)')xz_tmp
      read(exp_string,'(e17.10)')x_wall
      else
      x_wall=xz_tmp
      end if
      if(x_wall.NE.(1.0e30_DOUBLE))continue
      if(next_token(line,b,e,p))continue
      xz_tmp=read_real_soft_fail(line(b:e))
      if(with_sonnet.EQ.1)then
      write(exp_string,'(e17.10)')xz_tmp
      read(exp_string,'(e17.10)')z_wall
      else
      z_wall=xz_tmp
      end if
      if(z_wall.NE.(1.0e30_DOUBLE))continue
      if(num_nodes.GT.0)then
      do inode=1,num_nodes
      if((abs(x_wall-nodes(1,inode)).LT.(1.0e-8_DOUBLE)).AND.(abs(z_wall
     &-nodes(2,inode)).LT.(1.0e-8_DOUBLE)))then
      this_node=inode
      go to 90004
      end if
      end do
      end if
      num_nodes=num_nodes+1
      dim_nodes=max(dim_nodes,num_nodes)
      if(mod(((dim_nodes)-(1)+1),100).EQ.1)then
      nodes =>mem_realloc_r2(nodes,(1),(2),(1),(((int(((((dim_nodes)-(1)
     &+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_nodes)-(1)+1))+100
     &-1)/100)*100)+(1)-1),'nodes')
      end if
      if(mod(((dim_nodes)-(1)+1),100).EQ.1)then
      node_type =>mem_realloc_i1(node_type,(1),(((int(((((dim_nodes)-(1)
     &+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_nodes)-(1)+1))+100
     &-1)/100)*100)+(1)-1),'node_type')
      end if
      if(mod(((dim_nodes)-(1)+1),100).EQ.1)then
      node_element_count =>mem_realloc_i1(node_element_count,(1),(((int(
     &((((dim_nodes)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_
     &nodes)-(1)+1))+100-1)/100)*100)+(1)-1),'node_element_count')
      end if
      nodes(1,num_nodes)=x_wall
      nodes(2,num_nodes)=z_wall
      this_node=num_nodes
90004 continue
      wall_nodes(iseg,iwall)=this_node
      end do
      end do
      num_walls=num_walls+(num_new_walls)
      dim_walls=max(dim_walls,num_walls)
      close(unit=diskin2)
      
      else if(keyword.EQ.'print_min_max')then
      call find_min_max(num_nodes,nodes,node_type,x_min,x_max,z_min,z_ma
     &x)
      write(6,*)' X range = ',x_min,' -> ',x_max
      write(6,*)' Z range = ',z_min,' -> ',z_max
      else if(keyword.EQ.'bounds')then
      if(next_token(line,b,e,p))continue
      xb_min=read_real(line(b:e))
      if(next_token(line,b,e,p))continue
      xb_max=read_real(line(b:e))
      if(next_token(line,b,e,p))continue
      zb_min=read_real(line(b:e))
      if(next_token(line,b,e,p))continue
      zb_max=read_real(line(b:e))
      if(next_token(line,b,e,p))then
      yb_min=read_real(line(b:e))
      if(next_token(line,b,e,p))continue
      yb_max=read_real(line(b:e))
      else
      if((symmetry.EQ.1).OR.(symmetry.EQ.3))then
      yb_min=-(1.0_DOUBLE)
      yb_max=(1.0_DOUBLE)
      else if((symmetry.EQ.2).OR.(symmetry.EQ.5))then
      yb_min=(0.0_DOUBLE)
      yb_max=(3.6e2_DOUBLE)
      else
      yb_min=(0.0_DOUBLE)
      yb_max=(1.0_DOUBLE)
      end if
      end if
      if((symmetry.EQ.2).OR.(symmetry.EQ.5).OR.(symmetry.EQ.6))then
      yb_min=yb_min*(atan2((0.0_DOUBLE),-(1.0_DOUBLE))/(1.8e2_DOUBLE))
      yb_max=yb_max*(atan2((0.0_DOUBLE),-(1.0_DOUBLE))/(1.8e2_DOUBLE))
      else
      if((symmetry.EQ.1).OR.(symmetry.EQ.4).OR.(symmetry.EQ.3))continue
      end if
      
      else if(keyword.EQ.'symmetry')then
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.'cylindrical')then
      symmetry=2
      else if(line(b:e).EQ.'plane')then
      symmetry=1
      else if(line(b:e).EQ.'oned')then
      symmetry=3
      else if(line(b:e).EQ.'plane_hw')then
      symmetry=4
      else if(line(b:e).EQ.'cylindrical_hw')then
      symmetry=5
      else if(line(b:e).EQ.'cylindrical_section')then
      symmetry=6
      else
      if('Unexpected symmetry specification'.EQ.' ')continue
      end if
      
      else if(keyword.EQ.'print_walls')then
      if(next_token(line,b,e,p))continue
      file_format=line(b:e)
      if(next_token(line,b,e,p))then
      walloutfile=line(b:e)
      nunit=31
      else
      walloutfile='undefined'
      nunit=6
      end if
      call print_walls(nunit,file_format,walloutfile,num_walls,wall_segm
     &ent_count,wall_nodes,nodes)
      else if(keyword.EQ.'uniform_ys')then
      if((symmetry.EQ.4).OR.(symmetry.EQ.6))then
      if(yb_max-yb_min.GT.(0.0_DOUBLE))continue
      y_border=(1.e-2_DOUBLE)*(yb_max-yb_min)
      else if(symmetry.EQ.5)then
      y_border=(0.0_DOUBLE)
      if(yb_max-yb_min.EQ.(2.0_DOUBLE)*atan2((0.0_DOUBLE),-(1.0_DOUBLE))
     &)continue
      else if((symmetry.EQ.3).OR.(symmetry.EQ.1).OR.(symmetry.EQ.2))then
      if('Cannot specify y_values with this symmetry'.EQ.' ')continue
      else if(symmetry.EQ.0)then
      if('Need to specify symmetry before y values'.EQ.' ')continue
      end if
      if(next_token(line,b,e,p))continue
      num_y=read_integer(line(b:e))
      if(num_y.GT.0)continue
      y_div=num_y-1
      dim_ym=max(dim_ym,y_div)
      if(mod(((dim_ym)-(0)+1),100).EQ.1)then
      y_values =>mem_realloc_r1(y_values,(0),(((int(((((dim_ym)-(0)+1))+
     &100-1)/100)*100)-100)+(0)-1),((int(((((dim_ym)-(0)+1))+100-1)/100)
     &*100)+(0)-1),'y_values')
      end if
      do i=0,y_div
      y_values(i)=(yb_min+y_border)+((yb_max-y_border)-(yb_min+y_border)
     &)*REAL(i,DOUBLE)/REAL(y_div,DOUBLE)
      end do
      else if(keyword.EQ.'y_values')then
      if((symmetry.EQ.5).OR.(symmetry.EQ.6))then
      mult=atan2((0.0_DOUBLE),-(1.0_DOUBLE))/(1.8e2_DOUBLE)
      else if(symmetry.EQ.4)then
      mult=(1.0_DOUBLE)
      end if
      if(next_token(line,b,e,p))continue
      rtest=read_real_soft_fail(line(b:e))
      if(rtest.EQ.(1.0e30_DOUBLE))then
      tmpfilename=line(b:e)
      open(unit=diskin2,file=tmpfilename,status='old',form='formatted',i
     &ostat=open_stat)
      if(open_stat.NE.0)then
      write(0,*)' Cannot open file ',line(b:e),', error number ',open_st
     &at
      return
      end if
      y_div=-1
90008 continue
      if(read_string(diskin2,line,length))then
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      y_div=y_div+1
      dim_y=max(dim_y,y_div)
      dim_ym=max(dim_ym,y_div)
      if(mod(((dim_ym)-(0)+1),100).EQ.1)then
      y_values =>mem_realloc_r1(y_values,(0),(((int(((((dim_ym)-(0)+1))+
     &100-1)/100)*100)-100)+(0)-1),((int(((((dim_ym)-(0)+1))+100-1)/100)
     &*100)+(0)-1),'y_values')
      end if
      if(mod(((dim_ym)-(0)+1),100).EQ.1)then
      facearray =>mem_realloc_i1(facearray,(0),(((int(((((dim_ym)-(0)+1)
     &)+100-1)/100)*100)-100)+(0)-1),((int(((((dim_ym)-(0)+1))+100-1)/10
     &0)*100)+(0)-1),'facearray')
      end if
      if(mod(((dim_y-1)-(0)+1),100).EQ.1)then
      zonearray =>mem_realloc_i1(zonearray,(0),(((int(((((dim_y-1)-(0)+1
     &))+100-1)/100)*100)-100)+(0)-1),((int(((((dim_y-1)-(0)+1))+100-1)/
     &100)*100)+(0)-1),'zonearray')
      end if
      if(mod(((dim_y-1)-(0)+1),100).EQ.1)then
      zone_type_array =>mem_realloc_c1(zone_type_array,(300),(0),(((int(
     &((((dim_y-1)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((dim_y-
     &1)-(0)+1))+100-1)/100)*100)+(0)-1),'zone_type_array')
      end if
      y_values(y_div)=read_real(line(b:e))*mult
      if(y_div.GT.0)then
      if(y_values(y_div).GT.y_values(y_div-1))continue
      end if
      if(y_values(y_div).GE.yb_min)continue
      if(y_values(y_div).LE.yb_max)continue
      goto 90008
      end if
      else
      y_div=0
      y_values(0)=read_real(line(b:e))*mult
90009 continue
      if(next_token(line,b,e,p))then
      y_div=y_div+1
      dim_y=max(dim_y,y_div)
      dim_ym=max(dim_ym,y_div)
      if(mod(((dim_ym)-(0)+1),100).EQ.1)then
      y_values =>mem_realloc_r1(y_values,(0),(((int(((((dim_ym)-(0)+1))+
     &100-1)/100)*100)-100)+(0)-1),((int(((((dim_ym)-(0)+1))+100-1)/100)
     &*100)+(0)-1),'y_values')
      end if
      if(mod(((dim_ym)-(0)+1),100).EQ.1)then
      facearray =>mem_realloc_i1(facearray,(0),(((int(((((dim_ym)-(0)+1)
     &)+100-1)/100)*100)-100)+(0)-1),((int(((((dim_ym)-(0)+1))+100-1)/10
     &0)*100)+(0)-1),'facearray')
      end if
      if(mod(((dim_y-1)-(0)+1),100).EQ.1)then
      zonearray =>mem_realloc_i1(zonearray,(0),(((int(((((dim_y-1)-(0)+1
     &))+100-1)/100)*100)-100)+(0)-1),((int(((((dim_y-1)-(0)+1))+100-1)/
     &100)*100)+(0)-1),'zonearray')
      end if
      if(mod(((dim_y-1)-(0)+1),100).EQ.1)then
      zone_type_array =>mem_realloc_c1(zone_type_array,(300),(0),(((int(
     &((((dim_y-1)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((dim_y-
     &1)-(0)+1))+100-1)/100)*100)+(0)-1),'zone_type_array')
      end if
      y_values(y_div)=read_real(line(b:e))*mult
      if(y_div.GT.0)then
      if(y_values(y_div).GT.y_values(y_div-1))continue
      end if
      goto 90009
      end if
      end if
      else if(keyword.EQ.'end_prep')then
      
      dim_elements=num_elements
      if(mod(((dim_elements)-(1)+1),100).NE.0)then
      dg_elements_list =>mem_realloc_i2(dg_elements_list,(0),(1),(1),((i
     &nt(((((dim_elements)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_elements
     &),'dg_elements_list')
      end if
      if(mod(((dim_elements)-(1)+1),100).NE.0)then
      element_missing =>mem_realloc_i1(element_missing,(1),((int(((((dim
     &_elements)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_elements),'element
     &_missing')
      end if
      if(mod(((dim_elements)-(1)+1),100).NE.0)then
      element_skipped =>mem_realloc_i1(element_skipped,(1),((int(((((dim
     &_elements)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_elements),'element
     &_skipped')
      end if
      if(mod(((dim_elements)-(1)+1),100).NE.0)then
      element_assigned =>mem_realloc_i1(element_assigned,(1),((int(((((d
     &im_elements)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_elements),'eleme
     &nt_assigned')
      end if
      dim_nodes=num_nodes
      if(mod(((dim_nodes)-(1)+1),100).NE.0)then
      nodes =>mem_realloc_r2(nodes,(1),(2),(1),((int(((((dim_nodes)-(1)+
     &1))+100-1)/100)*100)+(1)-1),(dim_nodes),'nodes')
      end if
      if(mod(((dim_nodes)-(1)+1),100).NE.0)then
      node_type =>mem_realloc_i1(node_type,(1),((int(((((dim_nodes)-(1)+
     &1))+100-1)/100)*100)+(1)-1),(dim_nodes),'node_type')
      end if
      if(mod(((dim_nodes)-(1)+1),100).NE.0)then
      node_element_count =>mem_realloc_i1(node_element_count,(1),((int((
     &(((dim_nodes)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_nodes),'node_el
     &ement_count')
      end if
      do i=1,num_nodes
      if(abs(nodes(1,i)-xb_min).LT.(1.0e-8_DOUBLE))then
      if(abs(nodes(2,i)-zb_min).LT.(1.0e-8_DOUBLE))then
      nodes(1,i)=xb_min
      nodes(2,i)=zb_min
      else if(abs(nodes(2,i)-zb_max).LT.(1.0e-8_DOUBLE))then
      nodes(1,i)=xb_min
      nodes(2,i)=zb_max
      end if
      else if(abs(nodes(1,i)-xb_max).LT.(1.0e-8_DOUBLE))then
      if(abs(nodes(2,i)-zb_min).LT.(1.0e-8_DOUBLE))then
      nodes(1,i)=xb_max
      nodes(2,i)=zb_min
      else if(abs(nodes(2,i)-zb_max).LT.(1.0e-8_DOUBLE))then
      nodes(1,i)=xb_max
      nodes(2,i)=zb_max
      end if
      end if
      end do
      dim_walls=num_walls
      if(mod(((dim_walls)-(1)+1),100).NE.0)then
      wall_nodes =>mem_realloc_i2(wall_nodes,(0),(200000-1),(1),((int(((
     &((dim_walls)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_walls),'wall_nod
     &es')
      end if
      if(mod(((dim_walls)-(1)+1),100).NE.0)then
      wall_elements =>mem_realloc_i2(wall_elements,(1),(200000),(1),((in
     &t(((((dim_walls)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_walls),'wall
     &_elements')
      end if
      if(mod(((dim_walls)-(1)+1),100).NE.0)then
      wall_segment_count =>mem_realloc_i1(wall_segment_count,(1),((int((
     &(((dim_walls)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_walls),'wall_se
     &gment_count')
      end if
      dim_dg_poly=num_dg_poly
      if(mod(((dim_dg_poly)-(1)+1),100).NE.0)then
      dg_polygons =>mem_realloc_i2(dg_polygons,(1),(2000),(1),((int(((((
     &dim_dg_poly)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_dg_poly),'dg_pol
     &ygons')
      end if
      if(mod(((dim_dg_poly)-(1)+1),100).NE.0)then
      dg_poly_num_elements =>mem_realloc_i1(dg_poly_num_elements,(1),((i
     &nt(((((dim_dg_poly)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_dg_poly),
     &'dg_poly_num_elements')
      end if
      if(mod(((dim_dg_poly)-(1)+1),100).NE.0)then
      dg_poly_num_meshcon =>mem_realloc_i1(dg_poly_num_meshcon,(1),((int
     &(((((dim_dg_poly)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_dg_poly),'d
     &g_poly_num_meshcon')
      end if
      if(mod(((dim_dg_poly)-(1)+1),100).NE.0)then
      dg_poly_meshcon =>mem_realloc_i3(dg_poly_meshcon,(1),(2),(1),(2),(
     &1),((int(((((dim_dg_poly)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_dg_
     &poly),'dg_poly_meshcon')
      end if
      if(mod(((dim_dg_poly)-(1)+1),100).NE.0)then
      dg_poly_meshcon_hv =>mem_realloc_i3(dg_poly_meshcon_hv,(1),(2),(1)
     &,(2),(1),((int(((((dim_dg_poly)-(1)+1))+100-1)/100)*100)+(1)-1),(d
     &im_dg_poly),'dg_poly_meshcon_hv')
      end if
      dim_y=y_div
      dim_ym=y_div
      if(mod(((dim_ym)-(0)+1),100).NE.0)then
      y_values =>mem_realloc_r1(y_values,(0),((int(((((dim_ym)-(0)+1))+1
     &00-1)/100)*100)+(0)-1),(dim_ym),'y_values')
      end if
      if(mod(((dim_ym)-(0)+1),100).NE.0)then
      facearray =>mem_realloc_i1(facearray,(0),((int(((((dim_ym)-(0)+1))
     &+100-1)/100)*100)+(0)-1),(dim_ym),'facearray')
      end if
      if(mod(((dim_y-1)-(0)+1),100).NE.0)then
      zonearray =>mem_realloc_i1(zonearray,(0),((int(((((dim_y-1)-(0)+1)
     &)+100-1)/100)*100)+(0)-1),(dim_y-1),'zonearray')
      end if
      if(mod(((dim_y-1)-(0)+1),100).NE.0)then
      zone_type_array =>mem_realloc_c1(zone_type_array,(300),(0),((int((
     &(((dim_y-1)-(0)+1))+100-1)/100)*100)+(0)-1),(dim_y-1),'zone_type_a
     &rray')
      end if
      call init_geometry
      if(symmetry.EQ.6)then
      if(yb_max-yb_min.LT.(2.0_DOUBLE)*atan2((0.0_DOUBLE),-(1.0_DOUBLE))
     &)continue
      end if
      min_corner(1)=xb_min
      min_corner(2)=yb_min
      min_corner(3)=zb_min
      max_corner(1)=xb_max
      max_corner(2)=yb_max
      max_corner(3)=zb_max
      call universal_cell_3d(symmetry,min_corner,max_corner,vol)
      solid_ys(0)=y_values(0)
      solid_ys(1)=y_values(y_div)
      solid_faces(0)=2000000000
      solid_faces(1)=2000000000
      if((symmetry.EQ.2).OR.(symmetry.EQ.1))then
      facearray(0)=2000000000
      facearray(1)=2000000000
      else
      do i=0,y_div
      if(symmetry.EQ.4)then
      a_y(1)=(0.0_DOUBLE)
      a_y(2)=(1.0_DOUBLE)
      a_y(3)=(0.0_DOUBLE)
      b_y(1)=xb_max
      b_y(2)=y_values(i)
      b_y(3)=zb_max
      else
      if((symmetry.EQ.5).OR.(symmetry.EQ.6))continue
      if((symmetry.EQ.5).AND.(i.EQ.y_div))then
      cos_y=cos(y_values(0))
      sin_y=sin(y_values(0))
      else
      cos_y=cos(y_values(i))
      sin_y=sin(y_values(i))
      end if
      a_y(1)=-sin_y
      a_y(2)=cos_y
      a_y(3)=(0.0_DOUBLE)
      b_y(1)=xb_max*cos_y
      b_y(2)=xb_max*sin_y
      b_y(3)=zb_max
      end if
      call plane(b_y,a_y,coeff)
      facearray(i)=define_surface(coeff,.TRUE.)
      end do
      if((symmetry.EQ.4).OR.(symmetry.EQ.6))then
      solid_faces(0)=facearray(0)
      solid_faces(1)=facearray(y_div)
      end if
      end if
      if(nxd*nzd.GT.0)then
      yhat(1)=(0.0_DOUBLE)
      yhat(2)=(1.0_DOUBLE)
      yhat(3)=(0.0_DOUBLE)
      test_vec_1(1)=mesh_xz(1,2,(1-1)*nxd+1)-mesh_xz(1,1,(1-1)*nxd+1)
      test_vec_1(2)=(0.0_DOUBLE)
      test_vec_1(3)=mesh_xz(2,2,(1-1)*nxd+1)-mesh_xz(2,1,(1-1)*nxd+1)
      test_vec_2(1)=mesh_xz(1,3,(1-1)*nxd+1)-mesh_xz(1,2,(1-1)*nxd+1)
      test_vec_2(2)=(0.0_DOUBLE)
      test_vec_2(3)=mesh_xz(2,3,(1-1)*nxd+1)-mesh_xz(2,2,(1-1)*nxd+1)
      vector_temp(1)=test_vec_1(2)*test_vec_2(3)-test_vec_1(3)*test_vec_
     &2(2)
      vector_temp(2)=test_vec_1(3)*test_vec_2(1)-test_vec_1(1)*test_vec_
     &2(3)
      test_vec_3(3)=test_vec_1(1)*test_vec_2(2)-test_vec_1(2)*test_vec_2
     &(1)
      test_vec_3(1)=vector_temp(1)
      test_vec_3(2)=vector_temp(2)
      if((test_vec_3(1)*yhat(1)+test_vec_3(2)*yhat(2)+test_vec_3(3)*yhat
     &(3)).GT.(0.0_DOUBLE))then
      mesh_sense=1
      else if((test_vec_3(1)*yhat(1)+test_vec_3(2)*yhat(2)+test_vec_3(3)
     &*yhat(3)).LT.(0.0_DOUBLE))then
      mesh_sense=2
      else
      if('First cell of mesh degenerate'.EQ.' ')continue
      end if
!$omp parallel for default(private)
      do ix=1,nxd
      do iz=1,nzd
      zone=ix*(nzd-1)+iz
      n=0
      g2_num_polygons=g2_num_polygons+1
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      g2_polygon_xz =>mem_realloc_r3(g2_polygon_xz,(1),(2),(0),(2000-1),
     &(1),(((int(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-
     &1),((int(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'g2_
     &polygon_xz')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      g2_polygon_segment =>mem_realloc_i2(g2_polygon_segment,(0),(2000-1
     &),(1),(((int(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1
     &)-1),((int(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'g
     &2_polygon_segment')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      g2_polygon_points =>mem_realloc_i1(g2_polygon_points,(1),(((int(((
     &((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((
     &g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'g2_polygon_point
     &s')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      g2_polygon_zone =>mem_realloc_i1(g2_polygon_zone,(1),(((int(((((g2
     &_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((g2_n
     &um_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'g2_polygon_zone')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      g2_polygon_stratum =>mem_realloc_i1(g2_polygon_stratum,(1),(((int(
     &((((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((
     &((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'g2_polygon_str
     &atum')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      poly_int_props =>mem_realloc_i2(poly_int_props,(1),(3),(1),(((int(
     &((((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((
     &((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'poly_int_props
     &')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      poly_real_props =>mem_realloc_r2(poly_real_props,(1),(5),(1),(((in
     &t(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(
     &((((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'poly_real_pr
     &ops')
      end if
      g2_polygon_stratum(g2_num_polygons)=2000000000
      do i_inc_g2=0,2000-1
      g2_polygon_xz(1,i_inc_g2,g2_num_polygons)=(0.0_DOUBLE)
      g2_polygon_xz(2,i_inc_g2,g2_num_polygons)=(0.0_DOUBLE)
      g2_polygon_segment(i_inc_g2,g2_num_polygons)=2000000000
      end do
      g2_polygon_xz(1,n,g2_num_polygons)=mesh_xz(1,1,(iz-1)*nxd+ix)
      g2_polygon_xz(2,n,g2_num_polygons)=mesh_xz(2,1,(iz-1)*nxd+ix)
      if(mesh_sense.EQ.1)then
      start_pt=2
      end_pt=4
      inc_pt=1
      else if(mesh_sense.EQ.2)then
      start_pt=4
      end_pt=2
      inc_pt=-1
      else
      if('mesh_sense improperly set'.EQ.' ')continue
      end if
      do mesh_pt=start_pt,end_pt,inc_pt
      new_pt=.TRUE.
      do poly_pt=0,n
      if((mesh_xz(1,mesh_pt,(iz-1)*nxd+ix).EQ.g2_polygon_xz(1,poly_pt,g2
     &_num_polygons)).AND.(mesh_xz(2,mesh_pt,(iz-1)*nxd+ix).EQ.g2_polygo
     &n_xz(2,poly_pt,g2_num_polygons)))then
      new_pt=.FALSE.
      end if
      end do
      if(new_pt)then
      n=n+1
      g2_polygon_xz(1,n,g2_num_polygons)=mesh_xz(1,mesh_pt,(iz-1)*nxd+ix
     &)
      g2_polygon_xz(2,n,g2_num_polygons)=mesh_xz(2,mesh_pt,(iz-1)*nxd+ix
     &)
      end if
      end do
      n=n+1
      g2_polygon_xz(1,n,g2_num_polygons)=g2_polygon_xz(1,0,g2_num_polygo
     &ns)
      g2_polygon_xz(2,n,g2_num_polygons)=g2_polygon_xz(2,0,g2_num_polygo
     &ns)
      do i=0,n-1
      g2_polygon_segment(i,g2_num_polygons)=i
      end do
      g2_polygon_segment(n,g2_num_polygons)=0
      g2_polygon_points(g2_num_polygons)=n
      g2_polygon_zone(g2_num_polygons)=zone
      do i_prop=1,3
      poly_int_props(i_prop,g2_num_polygons)=current_int_props(i_prop)
      end do
      do i_prop=1,5
      poly_real_props(i_prop,g2_num_polygons)=current_real_props(i_prop)
      end do
      do i=0,y_div-1
      if(i.GT.0)zone=zone+1
      zonearray(i)=zone
      zone_type_array(i)="plasma"
      end do
      call decompose_polygon(n,g2_polygon_xz(1,0,g2_num_polygons),zonear
     &ray,y_div,facearray)
      if((mesh_xz(1,0,(iz-1)*nxd+ix).EQ.(0.0_DOUBLE)).AND.(mesh_xz(2,0,(
     &iz-1)*nxd+ix).EQ.(0.0_DOUBLE)))then
      if(((mesh_xz(1,4,(iz-1)*nxd+ix).EQ.mesh_xz(1,1,(iz-1)*nxd+ix)).AND
     &.(mesh_xz(2,4,(iz-1)*nxd+ix).EQ.mesh_xz(2,1,(iz-1)*nxd+ix))).OR.((
     &mesh_xz(1,4,(iz-1)*nxd+ix).EQ.mesh_xz(1,3,(iz-1)*nxd+ix)).AND.(mes
     &h_xz(2,4,(iz-1)*nxd+ix).EQ.mesh_xz(2,3,(iz-1)*nxd+ix))))then
      call triangle_centroid(mesh_xz(1,1,(iz-1)*nxd+ix),center)
      else
      call quad_center(mesh_xz(1,1,(iz-1)*nxd+ix),center)
      end if
      else
      center(1)=mesh_xz(1,0,(iz-1)*nxd+ix)
      center(2)=(0.0_DOUBLE)
      center(3)=mesh_xz(2,0,(iz-1)*nxd+ix)
      end if
      call update_zone_info(zonearray,ix,iz,zone_type_array,n,g2_polygon
     &_xz(1,0,g2_num_polygons),center,y_div,y_values)
      enddo
      enddo
!$omp end parallel 
      
      
      mesh_ix_start(1)=1
      mesh_ix_end(1)=nxd
      mesh_iz_start(1)=1
      mesh_iz_end(1)=1
      mesh_edge_num_elements(1)=nxd
      mesh_ix_step(1)=1
      mesh_iz_step(1)=0
      mesh_corner(0,1)=1
      mesh_corner(1,1)=4
      mesh_edge_hv(1)=0
      mesh_ix_start(2)=nxd
      mesh_ix_end(2)=nxd
      mesh_iz_start(2)=1
      mesh_iz_end(2)=nzd
      mesh_edge_num_elements(2)=nzd
      mesh_ix_step(2)=0
      mesh_iz_step(2)=1
      mesh_corner(0,2)=4
      mesh_corner(1,2)=3
      mesh_edge_hv(2)=1
      mesh_ix_start(3)=nxd
      mesh_ix_end(3)=1
      mesh_iz_start(3)=nzd
      mesh_iz_end(3)=nzd
      mesh_edge_num_elements(3)=nxd
      mesh_ix_step(3)=-1
      mesh_iz_step(3)=0
      mesh_corner(0,3)=3
      mesh_corner(1,3)=2
      mesh_edge_hv(3)=0
      mesh_ix_start(4)=1
      mesh_ix_end(4)=1
      mesh_iz_start(4)=nzd
      mesh_iz_end(4)=1
      mesh_edge_num_elements(4)=nzd
      mesh_ix_step(4)=0
      mesh_iz_step(4)=-1
      mesh_corner(0,4)=2
      mesh_corner(1,4)=1
      mesh_edge_hv(4)=1
      mesh_tot_elements=0
      mesh_tot_nodes=0
      do iedge=1,4
      ix=mesh_ix_start(iedge)
      iz=mesh_iz_start(iedge)
      do iseg=1,mesh_edge_num_elements(iedge)
      mesh_tot_elements=mesh_tot_elements+1
      mesh_edge_elements(iedge,iseg)=mesh_tot_elements
      mesh_edge_dg_label(iedge,iseg)=(iz+(iz_min-1)-1)*nxd_0+(ix+(ix_min
     &-1)-1)
      if(iedge.EQ.2)then
      if(ix_max.LT.nxd_0)then
      mesh_edge_dg_label(iedge,iseg)=mesh_edge_dg_label(iedge,iseg)+1
      else
      mesh_edge_dg_label(iedge,iseg)=-(mesh_edge_dg_label(iedge,iseg)+1)
      end if
      else if(iedge.EQ.3)then
      if(iz_max.LT.nzd_0)then
      mesh_edge_dg_label(iedge,iseg)=mesh_edge_dg_label(iedge,iseg)+(nxd
     &_0)
      else
      mesh_edge_dg_label(iedge,iseg)=-(mesh_edge_dg_label(iedge,iseg)+1)
      end if
      end if
      do itip=0,1
      test_node(1)=mesh_xz(1,mesh_corner(itip,iedge),(iz-1)*nxd+ix)
      test_node(2)=mesh_xz(2,mesh_corner(itip,iedge),(iz-1)*nxd+ix)
      new_node=1
      if(mesh_tot_nodes.GT.0)then
      do inode=1,mesh_tot_nodes
      if((mesh_nodes(1,inode).EQ.test_node(1)).AND.(mesh_nodes(2,inode).
     &EQ.test_node(2)))then
      if(new_node.EQ.1)continue
      new_node=0
      this_node=inode
      end if
      end do
      end if
      if(new_node.EQ.1)then
      mesh_tot_nodes=mesh_tot_nodes+1
      mesh_nodes(1,mesh_tot_nodes)=test_node(1)
      mesh_nodes(2,mesh_tot_nodes)=test_node(2)
      this_node=mesh_tot_nodes
      end if
      mesh_elements(itip,mesh_tot_elements)=this_node
      end do
      ix=ix+(mesh_ix_step(iedge))
      iz=iz+(mesh_iz_step(iedge))
      end do
      if(ix.EQ.mesh_ix_end(iedge)+mesh_ix_step(iedge))continue
      if(iz.EQ.mesh_iz_end(iedge)+mesh_iz_step(iedge))continue
      end do
      if(mesh_tot_nodes.LT.4*(nxd+nzd))continue
      if(mesh_tot_elements.EQ.2*(nxd+nzd))continue
      
      else
      nxd=1
      nzd=1
      mesh_xz =>mem_alloc_r3((1),(2),(0),(4),(1),(nxd*nzd),'mesh_xz')
      mesh_nodes =>mem_alloc_r2((1),(2),(1),(4*(nxd+nzd)),'mesh_nodes')
      mesh_elements =>mem_alloc_i2((0),(1),(1),(2*(nxd+nzd)),'mesh_eleme
     &nts')
      nx_nz_max=1
      mesh_edge_elements =>mem_alloc_i2((1),(4),(1),(nx_nz_max),'mesh_ed
     &ge_elements')
      mesh_edge_dg_label =>mem_alloc_i2((1),(4),(1),(nx_nz_max),'mesh_ed
     &ge_dg_label')
      mesh_edge_hv =>mem_alloc_i1((1),(4),'mesh_edge_hv')
      mesh_curve_num =>mem_alloc_i1((1),(2*(nxd+nzd)),'mesh_curve_num')
      mesh_scratch =>mem_alloc_i1((1),(2*(nxd+nzd)),'mesh_scratch')
      end if
      prep_done=1
      
      else if(keyword.EQ.'new_zone')then
      zone=zone+1
      if(next_token(line,b,e,p))continue
      new_zone_type=line(b:e)
      if(next_token(line,b,e,p))then
      new_zone_type_2=line(b:e)
      if((symmetry.EQ.4).OR.(symmetry.EQ.5).OR.(symmetry.EQ.6))continue
      do i=0,y_div-1
      zone_type_array(i)=new_zone_type
      end do
      new_zone_type="mixed"
90010 continue
      if(next_token(line,b,e,p))then
      if(line(b:e).NE.'-')then
      i_2=read_integer(line(b:e))
      if((i_2.GE.0).AND.(i_2.LE.y_div-1))continue
      zone_type_array(i_2)=new_zone_type_2
      else
      if(next_token(line,b,e,p))continue
      i_2_range=read_integer(line(b:e))
      if((i_2_range.GE.0).AND.(i_2_range.LE.y_div-1))continue
      if(i_2_range.GT.i_2)continue
      do i_2_loop=i_2+1,i_2_range
      zone_type_array(i_2_loop)=new_zone_type_2
      end do
      end if
      goto 90010
      end if
      end if
      else if(keyword.EQ.'new_polygon')then
      if(prep_done.EQ.1)continue
      call specify_polygon(20,num_walls,wall_segment_count,wall_nodes,no
     &des,dg_elements_list,num_dg_poly,dg_poly_num_elements,dg_polygons,
     &dg_poly_num_meshcon,dg_poly_meshcon,dg_poly_meshcon_hv,xb_min,xb_m
     &ax,zb_min,zb_max,nxd,nzd,mesh_xz,mesh_nodes,mesh_elements,mesh_edg
     &e_num_elements,mesh_edge_elements,mesh_edge_dg_label,mesh_edge_hv,
     &mesh_curve_num,mesh_scratch,temp_polygon(1,0),n,temp_int_props,tem
     &p_real_props,temp_aux_stratum,temp_stratum_pts,temp_num_stratum_pt
     &s,process_polygon)
      if(process_polygon.NE.0)then
      ix=0
      iz=0
      do i_prop=1,3
      if(temp_int_props(i_prop).NE.314159265)current_int_props(i_prop)=t
     &emp_int_props(i_prop)
      end do
      do i_prop=1,5
      if(temp_real_props(i_prop).NE.(314159265.3589793238462_DOUBLE))cur
     &rent_real_props(i_prop)=temp_real_props(i_prop)
      end do
      if(process_polygon.EQ.1)then
      g2_num_polygons=g2_num_polygons+1
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      g2_polygon_xz =>mem_realloc_r3(g2_polygon_xz,(1),(2),(0),(2000-1),
     &(1),(((int(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-
     &1),((int(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'g2_
     &polygon_xz')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      g2_polygon_segment =>mem_realloc_i2(g2_polygon_segment,(0),(2000-1
     &),(1),(((int(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1
     &)-1),((int(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'g
     &2_polygon_segment')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      g2_polygon_points =>mem_realloc_i1(g2_polygon_points,(1),(((int(((
     &((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((
     &g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'g2_polygon_point
     &s')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      g2_polygon_zone =>mem_realloc_i1(g2_polygon_zone,(1),(((int(((((g2
     &_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((g2_n
     &um_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'g2_polygon_zone')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      g2_polygon_stratum =>mem_realloc_i1(g2_polygon_stratum,(1),(((int(
     &((((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((
     &((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'g2_polygon_str
     &atum')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      poly_int_props =>mem_realloc_i2(poly_int_props,(1),(3),(1),(((int(
     &((((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((
     &((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'poly_int_props
     &')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      poly_real_props =>mem_realloc_r2(poly_real_props,(1),(5),(1),(((in
     &t(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(
     &((((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'poly_real_pr
     &ops')
      end if
      g2_polygon_stratum(g2_num_polygons)=2000000000
      do i_inc_g2=0,2000-1
      g2_polygon_xz(1,i_inc_g2,g2_num_polygons)=(0.0_DOUBLE)
      g2_polygon_xz(2,i_inc_g2,g2_num_polygons)=(0.0_DOUBLE)
      g2_polygon_segment(i_inc_g2,g2_num_polygons)=2000000000
      end do
      do i=0,n
      g2_polygon_xz(1,i,g2_num_polygons)=temp_polygon(1,i)
      g2_polygon_xz(2,i,g2_num_polygons)=temp_polygon(2,i)
      if(i.LT.n)then
      g2_polygon_segment(i,g2_num_polygons)=i
      else
      g2_polygon_segment(i,g2_num_polygons)=0
      end if
      end do
      g2_polygon_points(g2_num_polygons)=n
      g2_polygon_zone(g2_num_polygons)=zone
      g2_polygon_stratum(g2_num_polygons)=current_int_props(1)
      do i_prop=1,3
      poly_int_props(i_prop,g2_num_polygons)=current_int_props(i_prop)
      end do
      do i_prop=1,5
      poly_real_props(i_prop,g2_num_polygons)=current_real_props(i_prop)
      end do
      solid_zone(0)=zone
      call decompose_polygon(n,g2_polygon_xz(1,0,g2_num_polygons),solid_
     &zone,1,solid_faces)
      if(n.EQ.3)then
      call triangle_centroid(temp_polygon(1,0),center)
      else if(n.EQ.4)then
      call quad_center(temp_polygon(1,0),center)
      else
      center(1)=temp_polygon(1,0)
      center(2)=(0.0_DOUBLE)
      center(3)=temp_polygon(2,0)
      end if
      one_zone_type(0)=new_zone_type
      call update_zone_info(solid_zone,ix,iz,one_zone_type,n,g2_polygon_
     &xz(1,0,g2_num_polygons),center,1,solid_ys)
      if(temp_num_stratum_pts.GT.0)then
      do i=1,temp_num_stratum_pts
      num_aux_sectors=num_aux_sectors+1
      dim_aux_sectors=max(dim_aux_sectors,num_aux_sectors)
      if(mod(((dim_aux_sectors)-(1)+1),100).EQ.1)then
      aux_stratum =>mem_realloc_i1(aux_stratum,(1),(((int(((((dim_aux_se
     &ctors)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_aux_sect
     &ors)-(1)+1))+100-1)/100)*100)+(1)-1),'aux_stratum')
      end if
      if(mod(((dim_aux_sectors)-(1)+1),100).EQ.1)then
      aux_stratum_poly =>mem_realloc_i1(aux_stratum_poly,(1),(((int(((((
     &dim_aux_sectors)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((di
     &m_aux_sectors)-(1)+1))+100-1)/100)*100)+(1)-1),'aux_stratum_poly')
      end if
      if(mod(((dim_aux_sectors)-(1)+1),100).EQ.1)then
      aux_stratum_points =>mem_realloc_i1(aux_stratum_points,(1),(((int(
     &((((dim_aux_sectors)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((
     &((dim_aux_sectors)-(1)+1))+100-1)/100)*100)+(1)-1),'aux_stratum_po
     &ints')
      end if
      if(mod(((dim_aux_sectors)-(1)+1),100).EQ.1)then
      aux_stratum_segment =>mem_realloc_i1(aux_stratum_segment,(1),(((in
     &t(((((dim_aux_sectors)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(
     &((((dim_aux_sectors)-(1)+1))+100-1)/100)*100)+(1)-1),'aux_stratum_
     &segment')
      end if
      aux_stratum(num_aux_sectors)=314159265
      aux_stratum_poly(num_aux_sectors)=314159265
      aux_stratum_points(num_aux_sectors)=314159265
      aux_stratum_segment(num_aux_sectors)=314159265
      aux_stratum(num_aux_sectors)=temp_aux_stratum
      aux_stratum_poly(num_aux_sectors)=g2_num_polygons
      aux_stratum_points(num_aux_sectors)=temp_stratum_pts(i)
      temp_aux_segment=1
      if(num_aux_sectors.GT.1)then
      do i_aux=num_aux_sectors-1,1,-1
      if(aux_stratum(i_aux).EQ.temp_aux_stratum)then
      temp_aux_segment=aux_stratum_segment(i_aux)+1
      go to 90005
      end if
      end do
90005 continue
      end if
      aux_stratum_segment(num_aux_sectors)=temp_aux_segment
      end do
      end if
      else if(process_polygon.EQ.2.OR.process_polygon.EQ.3)then
      if(process_polygon.EQ.3)then
      refine=1
      else
      refine=0
      end if
      if(current_int_props(3).GT.0)then
      if(current_int_props(3).EQ.1)continue
      temp_holes(1,0)=current_real_props(4)
      temp_holes(2,0)=current_real_props(5)
      end if
      call poly2triangles(n,temp_polygon(1,0),current_real_props(3),curr
     &ent_int_props(3),temp_holes(1,0),refine,ntriangles,temp_triangles(
     &1,0,0),temp_segment(0,0))
      if(ntriangles.LT.100000)continue
      nt=3
      do j=0,ntriangles-1
      g2_num_polygons=g2_num_polygons+1
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      g2_polygon_xz =>mem_realloc_r3(g2_polygon_xz,(1),(2),(0),(2000-1),
     &(1),(((int(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-
     &1),((int(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'g2_
     &polygon_xz')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      g2_polygon_segment =>mem_realloc_i2(g2_polygon_segment,(0),(2000-1
     &),(1),(((int(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1
     &)-1),((int(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'g
     &2_polygon_segment')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      g2_polygon_points =>mem_realloc_i1(g2_polygon_points,(1),(((int(((
     &((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((
     &g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'g2_polygon_point
     &s')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      g2_polygon_zone =>mem_realloc_i1(g2_polygon_zone,(1),(((int(((((g2
     &_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((g2_n
     &um_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'g2_polygon_zone')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      g2_polygon_stratum =>mem_realloc_i1(g2_polygon_stratum,(1),(((int(
     &((((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((
     &((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'g2_polygon_str
     &atum')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      poly_int_props =>mem_realloc_i2(poly_int_props,(1),(3),(1),(((int(
     &((((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((
     &((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'poly_int_props
     &')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).EQ.1)then
      poly_real_props =>mem_realloc_r2(poly_real_props,(1),(5),(1),(((in
     &t(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(
     &((((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),'poly_real_pr
     &ops')
      end if
      g2_polygon_stratum(g2_num_polygons)=2000000000
      do i_inc_g2=0,2000-1
      g2_polygon_xz(1,i_inc_g2,g2_num_polygons)=(0.0_DOUBLE)
      g2_polygon_xz(2,i_inc_g2,g2_num_polygons)=(0.0_DOUBLE)
      g2_polygon_segment(i_inc_g2,g2_num_polygons)=2000000000
      end do
      do i=0,nt
      g2_polygon_xz(1,i,g2_num_polygons)=temp_triangles(1,i,j)
      g2_polygon_xz(2,i,g2_num_polygons)=temp_triangles(2,i,j)
      g2_polygon_segment(i,g2_num_polygons)=temp_segment(i,j)
      if((temp_num_stratum_pts.GT.0).AND.(i.NE.3))then
      do k=1,temp_num_stratum_pts
      if((temp_segment(i,j).EQ.temp_stratum_pts(k)).AND.(temp_segment(i+
     &1,j).EQ.mod(temp_stratum_pts(k)+1,n)))then
      if(i.NE.3)continue
      num_aux_sectors=num_aux_sectors+1
      dim_aux_sectors=max(dim_aux_sectors,num_aux_sectors)
      if(mod(((dim_aux_sectors)-(1)+1),100).EQ.1)then
      aux_stratum =>mem_realloc_i1(aux_stratum,(1),(((int(((((dim_aux_se
     &ctors)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_aux_sect
     &ors)-(1)+1))+100-1)/100)*100)+(1)-1),'aux_stratum')
      end if
      if(mod(((dim_aux_sectors)-(1)+1),100).EQ.1)then
      aux_stratum_poly =>mem_realloc_i1(aux_stratum_poly,(1),(((int(((((
     &dim_aux_sectors)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((di
     &m_aux_sectors)-(1)+1))+100-1)/100)*100)+(1)-1),'aux_stratum_poly')
      end if
      if(mod(((dim_aux_sectors)-(1)+1),100).EQ.1)then
      aux_stratum_points =>mem_realloc_i1(aux_stratum_points,(1),(((int(
     &((((dim_aux_sectors)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((
     &((dim_aux_sectors)-(1)+1))+100-1)/100)*100)+(1)-1),'aux_stratum_po
     &ints')
      end if
      if(mod(((dim_aux_sectors)-(1)+1),100).EQ.1)then
      aux_stratum_segment =>mem_realloc_i1(aux_stratum_segment,(1),(((in
     &t(((((dim_aux_sectors)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(
     &((((dim_aux_sectors)-(1)+1))+100-1)/100)*100)+(1)-1),'aux_stratum_
     &segment')
      end if
      aux_stratum(num_aux_sectors)=314159265
      aux_stratum_poly(num_aux_sectors)=314159265
      aux_stratum_points(num_aux_sectors)=314159265
      aux_stratum_segment(num_aux_sectors)=314159265
      aux_stratum(num_aux_sectors)=temp_aux_stratum
      aux_stratum_poly(num_aux_sectors)=g2_num_polygons
      aux_stratum_points(num_aux_sectors)=i
      temp_aux_segment=1
      if(num_aux_sectors.GT.1)then
      do i_aux=num_aux_sectors-1,1,-1
      if(aux_stratum(i_aux).EQ.temp_aux_stratum)then
      temp_aux_segment=aux_stratum_segment(i_aux)+1
      go to 90006
      end if
      end do
90006 continue
      end if
      aux_stratum_segment(num_aux_sectors)=temp_aux_segment
      end if
      end do
      end if
      end do
      g2_polygon_points(g2_num_polygons)=nt
      if(process_polygon.EQ.3.AND.j.GT.0)zone=zone+1
      g2_polygon_zone(g2_num_polygons)=zone
      g2_polygon_stratum(g2_num_polygons)=current_int_props(1)
      do i_prop=1,3
      poly_int_props(i_prop,g2_num_polygons)=current_int_props(i_prop)
      end do
      do i_prop=1,5
      poly_real_props(i_prop,g2_num_polygons)=current_real_props(i_prop)
      end do
      if(process_polygon.EQ.2)then
      solid_zone(0)=zone
      one_zone_type(0)=new_zone_type
      call decompose_polygon(nt,g2_polygon_xz(1,0,g2_num_polygons),solid
     &_zone,1,solid_faces)
      else
      if(process_polygon.EQ.3)continue
      do i=0,y_div-1
      if(i.GT.0)zone=zone+1
      zonearray(i)=zone
      if(new_zone_type.NE."mixed")zone_type_array(i)=new_zone_type
      end do
      call decompose_polygon(nt,g2_polygon_xz(1,0,g2_num_polygons),zonea
     &rray,y_div,facearray)
      end if
      if(j.EQ.0.OR.process_polygon.EQ.3)call triangle_centroid(temp_tria
     &ngles(1,0,j),center)
      if(process_polygon.EQ.2)then
      call update_zone_info(solid_zone,ix,iz,one_zone_type,nt,g2_polygon
     &_xz(1,0,g2_num_polygons),center,1,solid_ys)
      else
      call update_zone_info(zonearray,ix,iz,zone_type_array,nt,g2_polygo
     &n_xz(1,0,g2_num_polygons),center,y_div,y_values)
      end if
      end do
      end if
      end if
      
      else if(keyword.EQ.'new_diagnostic')then
      if(next_token(line,b,e,p))continue
      num_diags=num_diags+1
      dim_diags=max(dim_diags,num_diags)
      if(mod(((dim_diags)-(1)+1),100).EQ.1)then
      diag_name =>mem_realloc_c1(diag_name,(40),(1),(((int(((((dim_diags
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_diags)-(1)+1)
     &)+100-1)/100)*100)+(1)-1),'diag_name')
      end if
      if(mod(((dim_diags)-(1)+1),100).EQ.1)then
      diag_stratum =>mem_realloc_i1(diag_stratum,(1),(((int(((((dim_diag
     &s)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_diags)-(1)+1
     &))+100-1)/100)*100)+(1)-1),'diag_stratum')
      end if
      if(mod(((dim_diags)-(1)+1),100).EQ.1)then
      diag_solid =>mem_realloc_i1(diag_solid,(1),(((int(((((dim_diags)-(
     &1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_diags)-(1)+1))+1
     &00-1)/100)*100)+(1)-1),'diag_solid')
      end if
      if(mod(((dim_diags)-(1)+1),100).EQ.1)then
      diag_variable =>mem_realloc_i1(diag_variable,(1),(((int(((((dim_di
     &ags)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_diags)-(1)
     &+1))+100-1)/100)*100)+(1)-1),'diag_variable')
      end if
      if(mod(((dim_diags)-(1)+1),100).EQ.1)then
      diag_tab_index =>mem_realloc_i1(diag_tab_index,(1),(((int(((((dim_
     &diags)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_diags)-(
     &1)+1))+100-1)/100)*100)+(1)-1),'diag_tab_index')
      end if
      if(mod(((dim_diags)-(1)+1),100).EQ.1)then
      diag_var_min =>mem_realloc_r1(diag_var_min,(1),(((int(((((dim_diag
     &s)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_diags)-(1)+1
     &))+100-1)/100)*100)+(1)-1),'diag_var_min')
      end if
      if(mod(((dim_diags)-(1)+1),100).EQ.1)then
      diag_var_max =>mem_realloc_r1(diag_var_max,(1),(((int(((((dim_diag
     &s)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_diags)-(1)+1
     &))+100-1)/100)*100)+(1)-1),'diag_var_max')
      end if
      if(mod(((dim_diags)-(1)+1),100).EQ.1)then
      diag_mult =>mem_realloc_r1(diag_mult,(1),(((int(((((dim_diags)-(1)
     &+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_diags)-(1)+1))+100
     &-1)/100)*100)+(1)-1),'diag_mult')
      end if
      if(mod(((dim_diags)-(1)+1),100).EQ.1)then
      diag_spacing =>mem_realloc_i1(diag_spacing,(1),(((int(((((dim_diag
     &s)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_diags)-(1)+1
     &))+100-1)/100)*100)+(1)-1),'diag_spacing')
      end if
      diag_name(num_diags)='UNINITIALIZED'
      diag_stratum(num_diags)=314159265
      diag_solid(num_diags)=314159265
      diag_variable(num_diags)=314159265
      diag_tab_index(num_diags)=314159265
      diag_var_min(num_diags)=(314159265.3589793238462_DOUBLE)
      diag_var_max(num_diags)=(314159265.3589793238462_DOUBLE)
      diag_mult(num_diags)=(314159265.3589793238462_DOUBLE)
      diag_spacing(num_diags)=314159265
      diag_name(num_diags)=line(b:)
      call specify_diagnostic(20,diag_stratum(num_diags),diag_solid(num_
     &diags),diag_variable(num_diags),diag_tab_index(num_diags),diag_var
     &_min(num_diags),diag_var_max(num_diags),diag_mult(num_diags),diag_
     &spacing(num_diags))
      else if(keyword.EQ.'y_min_zone')then
      if((symmetry.EQ.4).OR.(symmetry.EQ.6))continue
      if(next_token(line,b,e,p))continue
      y_mat(0)=string_lookup(line(b:e),materials_sy,ma_num)
      if((y_mat(0).GT.0.AND.y_mat(0).LE.ma_num))continue
      if(next_token(line,b,e,p))continue
      y_stratum(0)=read_integer(line(b:e))
      if(next_token(line,b,e,p))then
      y_p_stratum(0)=read_integer(line(b:e))
      end if
      else if(keyword.EQ.'y_max_zone')then
      if((symmetry.EQ.4).OR.(symmetry.EQ.6))continue
      if(next_token(line,b,e,p))continue
      y_mat(1)=string_lookup(line(b:e),materials_sy,ma_num)
      if((y_mat(1).GT.0.AND.y_mat(1).LE.ma_num))continue
      if(next_token(line,b,e,p))continue
      y_stratum(1)=read_integer(line(b:e))
      if(next_token(line,b,e,p))then
      y_p_stratum(1)=read_integer(line(b:e))
      end if
      else if(keyword.EQ.'polygon_nc_file')then
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.'none'.OR.line(b:e).EQ.'NONE')then
      write_poly_nc=0
      else
      polygon_nc_file=line(b:e)
      end if
      else if(keyword.EQ.'quit'.OR.keyword.EQ.'end')then
      go to 90015
      end if
      go to 90012
90015 continue
      close(unit=20)
      if(mod(((g2_num_polygons)-(1)+1),100).NE.0)then
      g2_polygon_xz =>mem_realloc_r3(g2_polygon_xz,(1),(2),(0),(2000-1),
     &(1),((int(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),(g2
     &_num_polygons),'g2_polygon_xz')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).NE.0)then
      g2_polygon_segment =>mem_realloc_i2(g2_polygon_segment,(0),(2000-1
     &),(1),((int(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),(
     &g2_num_polygons),'g2_polygon_segment')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).NE.0)then
      g2_polygon_points =>mem_realloc_i1(g2_polygon_points,(1),((int((((
     &(g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),(g2_num_polygons
     &),'g2_polygon_points')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).NE.0)then
      g2_polygon_zone =>mem_realloc_i1(g2_polygon_zone,(1),((int(((((g2_
     &num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),(g2_num_polygons),'g
     &2_polygon_zone')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).NE.0)then
      g2_polygon_stratum =>mem_realloc_i1(g2_polygon_stratum,(1),((int((
     &(((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),(g2_num_polygo
     &ns),'g2_polygon_stratum')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).NE.0)then
      poly_int_props =>mem_realloc_i2(poly_int_props,(1),(3),(1),((int((
     &(((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),(g2_num_polygo
     &ns),'poly_int_props')
      end if
      if(mod(((g2_num_polygons)-(1)+1),100).NE.0)then
      poly_real_props =>mem_realloc_r2(poly_real_props,(1),(5),(1),((int
     &(((((g2_num_polygons)-(1)+1))+100-1)/100)*100)+(1)-1),(g2_num_poly
     &gons),'poly_real_props')
      end if
      if(mod(((dim_aux_sectors)-(1)+1),100).NE.0)then
      aux_stratum =>mem_realloc_i1(aux_stratum,(1),((int(((((dim_aux_sec
     &tors)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_aux_sectors),'aux_strat
     &um')
      end if
      if(mod(((dim_aux_sectors)-(1)+1),100).NE.0)then
      aux_stratum_poly =>mem_realloc_i1(aux_stratum_poly,(1),((int(((((d
     &im_aux_sectors)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_aux_sectors),
     &'aux_stratum_poly')
      end if
      if(mod(((dim_aux_sectors)-(1)+1),100).NE.0)then
      aux_stratum_points =>mem_realloc_i1(aux_stratum_points,(1),((int((
     &(((dim_aux_sectors)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_aux_secto
     &rs),'aux_stratum_points')
      end if
      if(mod(((dim_aux_sectors)-(1)+1),100).NE.0)then
      aux_stratum_segment =>mem_realloc_i1(aux_stratum_segment,(1),((int
     &(((((dim_aux_sectors)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_aux_sec
     &tors),'aux_stratum_segment')
      end if
      if(mod(((dim_diags)-(1)+1),100).NE.0)then
      diag_name =>mem_realloc_c1(diag_name,(40),(1),((int(((((dim_diags)
     &-(1)+1))+100-1)/100)*100)+(1)-1),(dim_diags),'diag_name')
      end if
      if(mod(((dim_diags)-(1)+1),100).NE.0)then
      diag_stratum =>mem_realloc_i1(diag_stratum,(1),((int(((((dim_diags
     &)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_diags),'diag_stratum')
      end if
      if(mod(((dim_diags)-(1)+1),100).NE.0)then
      diag_solid =>mem_realloc_i1(diag_solid,(1),((int(((((dim_diags)-(1
     &)+1))+100-1)/100)*100)+(1)-1),(dim_diags),'diag_solid')
      end if
      if(mod(((dim_diags)-(1)+1),100).NE.0)then
      diag_variable =>mem_realloc_i1(diag_variable,(1),((int(((((dim_dia
     &gs)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_diags),'diag_variable')
      end if
      if(mod(((dim_diags)-(1)+1),100).NE.0)then
      diag_tab_index =>mem_realloc_i1(diag_tab_index,(1),((int(((((dim_d
     &iags)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_diags),'diag_tab_index'
     &)
      end if
      if(mod(((dim_diags)-(1)+1),100).NE.0)then
      diag_var_min =>mem_realloc_r1(diag_var_min,(1),((int(((((dim_diags
     &)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_diags),'diag_var_min')
      end if
      if(mod(((dim_diags)-(1)+1),100).NE.0)then
      diag_var_max =>mem_realloc_r1(diag_var_max,(1),((int(((((dim_diags
     &)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_diags),'diag_var_max')
      end if
      if(mod(((dim_diags)-(1)+1),100).NE.0)then
      diag_mult =>mem_realloc_r1(diag_mult,(1),((int(((((dim_diags)-(1)+
     &1))+100-1)/100)*100)+(1)-1),(dim_diags),'diag_mult')
      end if
      if(mod(((dim_diags)-(1)+1),100).NE.0)then
      diag_spacing =>mem_realloc_i1(diag_spacing,(1),((int(((((dim_diags
     &)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_diags),'diag_spacing')
      end if
      sect_zone1 =>mem_alloc_i1((1),(g2_num_polygons),'sect_zone1')
      sect_zone2 =>mem_alloc_i1((1),(g2_num_polygons),'sect_zone2')
      if(write_poly_nc.EQ.1)then
      fileid=nccre(polygon_nc_file  (1:string_length(polygon_nc_file)),o
     &r(0,512),nc_stat)
      g2_points_ind0_id=ncddef(fileid,'g2_points_ind0',((2000-1)-(0)+1),
     &nc_stat)
      g2_points_ind_id=ncddef(fileid,'g2_points_ind',((2000)-(1)+1),nc_s
     &tat)
      g2_points_tot_ind0_id=ncddef(fileid,'g2_points_tot_ind0',((200000-
     &1)-(0)+1),nc_stat)
      g2_points_tot_ind_id=ncddef(fileid,'g2_points_tot_ind',((200000)-(
     &1)+1),nc_stat)
      g2_xz_ind_id=ncddef(fileid,'g2_xz_ind',((2)-(1)+1),nc_stat)
      g2_num_polygons_id=ncvdef(fileid,'g2_num_polygons',4,0,nc_dims,nc_
     &stat)
      g2_poly_ind_id=ncddef(fileid,'g2_poly_ind',((g2_num_polygons)-(1)+
     &1),nc_stat)
      nc_dims(1)=g2_xz_ind_id
      nc_dims(2)=g2_points_ind0_id
      nc_dims(3)=g2_poly_ind_id
      g2_polygon_xz_id=ncvdef(fileid,'g2_polygon_xz',6,3,nc_dims,nc_stat
     &)
      nc_dims(1)=g2_points_ind0_id
      nc_dims(2)=g2_poly_ind_id
      g2_polygon_segment_id=ncvdef(fileid,'g2_polygon_segment',4,2,nc_di
     &ms,nc_stat)
      nc_dims(1)=g2_poly_ind_id
      g2_polygon_points_id=ncvdef(fileid,'g2_polygon_points',4,1,nc_dims
     &,nc_stat)
      nc_dims(1)=g2_poly_ind_id
      g2_polygon_zone_id=ncvdef(fileid,'g2_polygon_zone',4,1,nc_dims,nc_
     &stat)
      nc_dims(1)=g2_poly_ind_id
      g2_polygon_stratum_id=ncvdef(fileid,'g2_polygon_stratum',4,1,nc_di
     &ms,nc_stat)
      
      call ncendf(fileid,nc_stat)
      call ncvpt(fileid,g2_num_polygons_id,nc_corner,nc_edge,g2_num_poly
     &gons,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((2)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((2000-1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((g2_num_polygons)-(1)+1)
      call ncvpt(fileid,g2_polygon_xz_id,nc_corner,nc_edge,g2_polygon_xz
     &,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((2000-1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((g2_num_polygons)-(1)+1)
      call ncvpt(fileid,g2_polygon_segment_id,nc_corner,nc_edge,g2_polyg
     &on_segment,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((g2_num_polygons)-(1)+1)
      call ncvpt(fileid,g2_polygon_points_id,nc_corner,nc_edge,g2_polygo
     &n_points,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((g2_num_polygons)-(1)+1)
      call ncvpt(fileid,g2_polygon_zone_id,nc_corner,nc_edge,g2_polygon_
     &zone,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((g2_num_polygons)-(1)+1)
      call ncvpt(fileid,g2_polygon_stratum_id,nc_corner,nc_edge,g2_polyg
     &on_stratum,nc_stat)
      
      call ncclos(fileid,nc_stat)
      end if
      if(keyword.EQ.'end')then
      if(symmetry.EQ.4)then
      a_y(1)=(0.0_DOUBLE)
      a_y(2)=(1.0_DOUBLE)
      a_y(3)=(0.0_DOUBLE)
      call plane(min_corner,a_y,coeff)
      ix=0
      iz=0
      end_faces(0)=define_surface(coeff,.TRUE.)
      end_faces(1)=solid_faces(0)
      zone=zone+1
      n=0
      poly4(1,n)=min_corner(1)
      poly4(2,n)=min_corner(3)
      n=n+1
      poly4(1,n)=min_corner(1)
      poly4(2,n)=max_corner(3)
      n=n+1
      poly4(1,n)=max_corner(1)
      poly4(2,n)=max_corner(3)
      n=n+1
      poly4(1,n)=max_corner(1)
      poly4(2,n)=min_corner(3)
      n=n+1
      poly4(1,n)=poly4(1,0)
      poly4(2,n)=poly4(2,0)
      solid_zone(0)=zone
      end_zones(0)=zone
      call decompose_polygon(n,poly4(1,0),solid_zone,1,end_faces)
      call quad_center(poly4(1,0),center)
      one_zone_type(0)="solid"
      solid_ys(0)=min_corner(2)
      solid_ys(1)=y_values(0)
      call update_zone_info(solid_zone,ix,iz,one_zone_type,n,poly4(1,0),
     &center,1,solid_ys)
      end_faces(0)=solid_faces(1)
      call plane(max_corner,a_y,coeff)
      end_faces(1)=define_surface(coeff,.TRUE.)
      zone=zone+1
      solid_zone(0)=zone
      end_zones(1)=zone
      call decompose_polygon(n,poly4(1,0),solid_zone,1,end_faces)
      one_zone_type(0)="solid"
      solid_ys(0)=y_values(y_div)
      solid_ys(1)=max_corner(2)
      call update_zone_info(solid_zone,ix,iz,one_zone_type,n,poly4(1,0),
     &center,1,solid_ys)
      else if(symmetry.EQ.6)then
      cos_y=cos(min_corner(2))
      sin_y=sin(min_corner(2))
      a_y(1)=-sin_y
      a_y(2)=cos_y
      a_y(3)=(0.0_DOUBLE)
      b_y(1)=max_corner(1)*cos_y
      b_y(2)=max_corner(1)*sin_y
      b_y(3)=max_corner(3)
      call plane(b_y,a_y,coeff)
      ix=0
      iz=0
      end_faces(0)=define_surface(coeff,.TRUE.)
      end_faces(1)=solid_faces(0)
      zone=zone+1
      n=0
      poly4(1,n)=min_corner(1)
      poly4(2,n)=min_corner(3)
      n=n+1
      poly4(1,n)=min_corner(1)
      poly4(2,n)=max_corner(3)
      n=n+1
      poly4(1,n)=max_corner(1)
      poly4(2,n)=max_corner(3)
      n=n+1
      poly4(1,n)=max_corner(1)
      poly4(2,n)=min_corner(3)
      n=n+1
      poly4(1,n)=poly4(1,0)
      poly4(2,n)=poly4(2,0)
      solid_zone(0)=zone
      end_zones(0)=zone
      call decompose_polygon(n,poly4(1,0),solid_zone,1,end_faces)
      call quad_center(poly4(1,0),center)
      one_zone_type(0)="solid"
      solid_ys(0)=min_corner(2)
      solid_ys(1)=y_values(0)
      call update_zone_info(solid_zone,ix,iz,one_zone_type,n,poly4(1,0),
     &center,1,solid_ys)
      if(abs(max_corner(2)-min_corner(2)-atan2((0.0_DOUBLE),-(1.0_DOUBLE
     &))).GT.(1.0e-10_DOUBLE))then
      cos_y=cos(max_corner(2))
      sin_y=sin(max_corner(2))
      a_y(1)=-sin_y
      a_y(2)=cos_y
      a_y(3)=(0.0_DOUBLE)
      b_y(1)=max_corner(1)*cos_y
      b_y(2)=max_corner(1)*sin_y
      b_y(3)=max_corner(3)
      call plane(b_y,a_y,coeff)
      end_faces(1)=define_surface(coeff,.TRUE.)
      else
      end_faces(1)=-end_faces(0)
      end if
      end_faces(0)=solid_faces(1)
      zone=zone+1
      solid_zone(0)=zone
      end_zones(1)=zone
      call decompose_polygon(n,poly4(1,0),solid_zone,1,end_faces)
      one_zone_type(0)="solid"
      solid_ys(0)=y_values(y_div)
      solid_ys(1)=max_corner(2)
      call update_zone_info(solid_zone,ix,iz,one_zone_type,n,poly4(1,0),
     &center,1,solid_ys)
      end if
      
      call boundaries_neighbors
      call setup_sectors(g2_num_polygons,g2_polygon_points,g2_polygon_xz
     &,g2_polygon_segment,g2_polygon_zone,poly_int_props,poly_real_props
     &,num_aux_sectors,aux_stratum,aux_stratum_poly,aux_stratum_points,a
     &ux_stratum_segment,y_div,sect_zone1,sect_zone2)
      if((symmetry.EQ.5).OR.(symmetry.EQ.6).OR.(symmetry.EQ.4))then
      if(y_div.GT.0)continue
      if(symmetry.EQ.5)then
      y_max=y_div-1
      else
      y_max=y_div
      end if
      solid_sector=1000000000
      other_sector=1000000000
      do i=0,y_max
      face1=abs(facearray(i))
      call find_poly_zone(face1,0,0,g2_num_polygons,sect_zone1,num_zone1
     &,sect_zone2,num_zone2)
      if((num_zone1.GT.0).AND.(num_zone2.GT.0))then
      other_seg=0
      do k_zone1=1,num_zone1
      do k_zone2=1,num_zone2
      if((num_zone1.EQ.num_zone2).OR.(num_zone1.EQ.1).OR.(num_zone2.EQ.1
     &))continue
      if(((num_zone1.EQ.1).OR.(num_zone2.EQ.1).OR.(zone_index(4,sect_zon
     &e2(k_zone2)).EQ.zone_index(4,sect_zone1(k_zone1)))).AND.(zone_type
     &(sect_zone1(k_zone1)).NE.zone_type(sect_zone2(k_zone2))).AND.(zone
     &_type(sect_zone1(k_zone1)).NE.4).AND.(zone_type(sect_zone2(k_zone2
     &)).NE.4))then
      if(zone_type(sect_zone1(k_zone1)).EQ.3)then
      solid_zone_p=sect_zone1(k_zone1)
      solid_face=face1
      other_zone=sect_zone2(k_zone2)
      other_face=-face1
      else if(zone_type(sect_zone2(k_zone2)).EQ.3)then
      solid_zone_p=sect_zone2(k_zone2)
      solid_face=-face1
      other_zone=sect_zone1(k_zone1)
      other_face=face1
      end if
      if(((symmetry.EQ.4).OR.(symmetry.EQ.6)).AND.((solid_zone_p.EQ.end_
     &zones(0)).OR.(solid_zone_p.EQ.end_zones(1))))then
      if(solid_zone_p.EQ.end_zones(0))then
      this_stratum=y_stratum(0)
      this_mat=y_mat(0)
      other_stratum=y_p_stratum(0)
      else
      this_stratum=y_stratum(1)
      this_mat=y_mat(1)
      other_stratum=y_p_stratum(1)
      end if
      this_seg=1
      other_seg=other_seg+1
      this_temp=(3.e2_DOUBLE)*(1.380658e-23_DOUBLE)
      this_rc=(1.0_DOUBLE)
      else
      this_poly=1000000000
      do i_poly=1,g2_num_polygons
      if(g2_polygon_zone(i_poly).EQ.zone_index(4,solid_zone_p))then
      if(this_poly.EQ.1000000000)continue
      this_poly=i_poly
      end if
      end do
      if(this_poly.NE.1000000000)continue
      this_stratum=poly_int_props(1,this_poly)
      other_stratum=this_stratum
      this_mat=poly_int_props(2,this_poly)
      this_temp=poly_real_props(1,this_poly)*(1.380658e-23_DOUBLE)
      this_rc=poly_real_props(2,this_poly)
      this_seg=1000000000
      do i_sect=1,nsectors
      if(strata(i_sect).EQ.poly_int_props(1,this_poly))then
      if(this_seg.EQ.1000000000)then
      this_seg=sector_strata_segment(i_sect)
      else
      this_seg=max(this_seg,sector_strata_segment(i_sect))
      end if
      end if
      end do
      if((this_seg.GE.0).AND.(this_seg.NE.1000000000))continue
      this_seg=this_seg+1
      other_seg=this_seg
      end if
      new_solid_sector=1
      if(solid_sector.NE.1000000000)then
      if(((solid_zone_p.EQ.sector_zone(solid_sector)).AND.(solid_face.EQ
     &.sector_surface(solid_sector)).AND.(this_stratum.EQ.strata(solid_s
     &ector)).AND.(this_seg.EQ.sector_strata_segment(solid_sector))))new
     &_solid_sector=0
      end if
      new_other_sector=1
      if(other_sector.NE.1000000000)then
      if(((other_zone.EQ.sector_zone(other_sector)).AND.(other_face.EQ.s
     &ector_surface(other_sector)).AND.(other_stratum.EQ.strata(other_se
     &ctor)).AND.(other_seg.EQ.sector_strata_segment(other_sector))))new
     &_other_sector=0
      end if
      if(new_solid_sector.EQ.1)then
      solid_sector=define_sector(this_stratum,this_seg,solid_face,solid_
     &zone_p,other_zone)
      other_type=zone_type(other_zone)
      if(other_type.EQ.2)then
      sc_target_num=sc_target_num+1
      if(mod(((sc_target_num)-(0)+1),100).EQ.1)then
      target_sector =>mem_realloc_i1(target_sector,(0),(((int(((((sc_tar
     &get_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_target_
     &num)-(0)+1))+100-1)/100)*100)+(0)-1),'target_sector')
      end if
      if(mod(((sc_target_num)-(0)+1),100).EQ.1)then
      target_material =>mem_realloc_i1(target_material,(0),(((int(((((sc
     &_target_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_tar
     &get_num)-(0)+1))+100-1)/100)*100)+(0)-1),'target_material')
      end if
      if(mod(((sc_target_num)-(0)+1),100).EQ.1)then
      target_temperature =>mem_realloc_r1(target_temperature,(0),(((int(
     &((((sc_target_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((
     &sc_target_num)-(0)+1))+100-1)/100)*100)+(0)-1),'target_temperature
     &')
      end if
      if(mod(((sc_target_num)-(0)+1),100).EQ.1)then
      target_recyc_coef =>mem_realloc_r1(target_recyc_coef,(0),(((int(((
     &((sc_target_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc
     &_target_num)-(0)+1))+100-1)/100)*100)+(0)-1),'target_recyc_coef')
      end if
      target_sector(sc_target_num)=solid_sector
      sector_type_pointer(3,solid_sector)=sc_target_num
      target_material(sc_target_num)=this_mat
      target_temperature(sc_target_num)=this_temp
      target_recyc_coef(sc_target_num)=this_rc
      else if(other_type.EQ.1)then
      sc_wall_num=sc_wall_num+1
      if(mod(((sc_wall_num)-(0)+1),100).EQ.1)then
      wall_sector =>mem_realloc_i1(wall_sector,(0),(((int(((((sc_wall_nu
     &m)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_wall_num)-(0)
     &+1))+100-1)/100)*100)+(0)-1),'wall_sector')
      end if
      if(mod(((sc_wall_num)-(0)+1),100).EQ.1)then
      wall_material =>mem_realloc_i1(wall_material,(0),(((int(((((sc_wal
     &l_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_wall_num)
     &-(0)+1))+100-1)/100)*100)+(0)-1),'wall_material')
      end if
      if(mod(((sc_wall_num)-(0)+1),100).EQ.1)then
      wall_temperature =>mem_realloc_r1(wall_temperature,(0),(((int(((((
     &sc_wall_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_wal
     &l_num)-(0)+1))+100-1)/100)*100)+(0)-1),'wall_temperature')
      end if
      if(mod(((sc_wall_num)-(0)+1),100).EQ.1)then
      wall_recyc_coef =>mem_realloc_r1(wall_recyc_coef,(0),(((int(((((sc
     &_wall_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_wall_
     &num)-(0)+1))+100-1)/100)*100)+(0)-1),'wall_recyc_coef')
      end if
      wall_sector(sc_wall_num)=solid_sector
      sector_type_pointer(4,solid_sector)=sc_wall_num
      wall_material(sc_wall_num)=this_mat
      wall_temperature(sc_wall_num)=this_temp
      wall_recyc_coef(sc_wall_num)=this_rc
      else
      if(' Unexpected zone type'.EQ.' ')continue
      end if
      end if
      if((new_other_sector.EQ.1).AND.(other_stratum.NE.2000000000))then
      other_sector=define_sector(other_stratum,other_seg,other_face,othe
     &r_zone,solid_zone_p)
      other_type=zone_type(other_zone)
      if(other_type.EQ.2)then
      sc_plasma_num=sc_plasma_num+1
      if(mod(((sc_plasma_num)-(0)+1),100).EQ.1)then
      plasma_sector =>mem_realloc_i1(plasma_sector,(0),(((int(((((sc_pla
     &sma_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_plasma_
     &num)-(0)+1))+100-1)/100)*100)+(0)-1),'plasma_sector')
      end if
      plasma_sector(sc_plasma_num)=other_sector
      sector_type_pointer(2,other_sector)=sc_plasma_num
      
      else if(other_type.EQ.1)then
      sc_vacuum_num=sc_vacuum_num+1
      if(mod(((sc_vacuum_num)-(0)+1),100).EQ.1)then
      vacuum_sector =>mem_realloc_i1(vacuum_sector,(0),(((int(((((sc_vac
     &uum_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_vacuum_
     &num)-(0)+1))+100-1)/100)*100)+(0)-1),'vacuum_sector')
      end if
      vacuum_sector(sc_vacuum_num)=other_sector
      sector_type_pointer(1,other_sector)=sc_vacuum_num
      else
      if(' Unexpected zone type'.EQ.' ')continue
      end if
      end if
      end if
      end do
      end do
      end if
      end do
      end if
      
      call default_diag_setup
      if(num_diags.GT.0)then
      do i_diag=1,num_diags
      i_grp=0
      do i_sect=1,nsectors
      if(strata(i_sect).EQ.diag_stratum(i_diag))then
      is_aux_sector=0
      do i_aux=1,num_aux_sectors
      if((strata(i_sect).EQ.aux_stratum(i_aux)).AND.(sector_strata_segme
     &nt(i_sect).EQ.aux_stratum_segment(i_aux)))then
      is_aux_sector=1
      end if
      end do
      if(is_aux_sector.EQ.1)then
      i_grp=i_grp+1
      if(i_grp.LE.80000)continue
      grp_sectors(i_grp)=i_sect
      else
      if(((diag_solid(i_diag).EQ.1).AND.((sector_type_pointer(3,i_sect).
     &GT.0.AND.sector_type_pointer(3,i_sect).LE.sc_target_num).OR.(secto
     &r_type_pointer(4,i_sect).GT.0.AND.sector_type_pointer(4,i_sect).LE
     &.sc_wall_num).OR.(sector_type_pointer(5,i_sect).GT.0.AND.sector_ty
     &pe_pointer(5,i_sect).LE.sc_exit_num))).OR.((diag_solid(i_diag).EQ.
     &0).AND.((sector_type_pointer(2,i_sect).GT.0.AND.sector_type_pointe
     &r(2,i_sect).LE.sc_plasma_num).OR.(sector_type_pointer(1,i_sect).GT
     &.0.AND.sector_type_pointer(1,i_sect).LE.sc_vacuum_num))))then
      i_grp=i_grp+1
      if(i_grp.LE.80000)continue
      grp_sectors(i_grp)=i_sect
      end if
      end if
      end if
      end do
      if(i_grp.GT.0)continue
      if(diag_name(i_diag).NE.'UNINITIALIZED')continue
      call diag_grp_init(diag_name(i_diag),i_grp,diag_variable(i_diag),d
     &iag_tab_index(i_diag),diag_var_min(i_diag),diag_var_max(i_diag),di
     &ag_mult(i_diag),diag_spacing(i_diag),grp_sectors)
      end do
      end if
      
      call end_sectors
      call detector_setup
      call end_detectors
      call check_geometry
      call pixel_map_test
      call write_geometry
      call erase_geometry
      end if
      call mem_free_r2(nodes,(1),(2),(1),(dim_nodes),'nodes')
      call mem_free_i1(node_type,(1),(dim_nodes),'node_type')
      call mem_free_i1(node_element_count,(1),(dim_nodes),'node_element_
     &count')
      call mem_free_i2(dg_elements_list,(0),(1),(1),(dim_elements),'dg_e
     &lements_list')
      call mem_free_i1(element_missing,(1),(dim_elements),'element_missi
     &ng')
      call mem_free_i1(element_skipped,(1),(dim_elements),'element_skipp
     &ed')
      call mem_free_i1(element_assigned,(1),(dim_elements),'element_assi
     &gned')
      call mem_free_i2(wall_nodes,(0),(200000-1),(1),(dim_walls),'wall_n
     &odes')
      call mem_free_i2(wall_elements,(1),(200000),(1),(dim_walls),'wall_
     &elements')
      call mem_free_i1(wall_segment_count,(1),(dim_walls),'wall_segment_
     &count')
      call mem_free_i2(dg_polygons,(1),(2000),(1),(dim_dg_poly),'dg_poly
     &gons')
      call mem_free_i1(dg_poly_num_elements,(1),(dim_dg_poly),'dg_poly_n
     &um_elements')
      call mem_free_i1(dg_poly_num_meshcon,(1),(dim_dg_poly),'dg_poly_nu
     &m_meshcon')
      call mem_free_i3(dg_poly_meshcon,(1),(2),(1),(2),(1),(dim_dg_poly)
     &,'dg_poly_meshcon')
      call mem_free_i3(dg_poly_meshcon_hv,(1),(2),(1),(2),(1),(dim_dg_po
     &ly),'dg_poly_meshcon_hv')
      call mem_free_r1(y_values,(0),(dim_ym),'y_values')
      call mem_free_i1(facearray,(0),(dim_ym),'facearray')
      call mem_free_i1(zonearray,(0),(dim_y-1),'zonearray')
      call mem_free_c1(zone_type_array,(300),(0),(dim_y-1),'zone_type_ar
     &ray')
      call mem_free_i1(sect_zone1,(1),(g2_num_polygons),'sect_zone1')
      call mem_free_i1(sect_zone2,(1),(g2_num_polygons),'sect_zone2')
      call mem_free_i1(aux_stratum,(1),(dim_aux_sectors),'aux_stratum')
      call mem_free_i1(aux_stratum_poly,(1),(dim_aux_sectors),'aux_strat
     &um_poly')
      call mem_free_i1(aux_stratum_points,(1),(dim_aux_sectors),'aux_str
     &atum_points')
      call mem_free_i1(aux_stratum_segment,(1),(dim_aux_sectors),'aux_st
     &ratum_segment')
      call mem_free_c1(diag_name,(40),(1),(dim_diags),'diag_name')
      call mem_free_i1(diag_stratum,(1),(dim_diags),'diag_stratum')
      call mem_free_i1(diag_solid,(1),(dim_diags),'diag_solid')
      call mem_free_i1(diag_variable,(1),(dim_diags),'diag_variable')
      call mem_free_i1(diag_tab_index,(1),(dim_diags),'diag_tab_index')
      call mem_free_r1(diag_var_min,(1),(dim_diags),'diag_var_min')
      call mem_free_r1(diag_var_max,(1),(dim_diags),'diag_var_max')
      call mem_free_r1(diag_mult,(1),(dim_diags),'diag_mult')
      call mem_free_i1(diag_spacing,(1),(dim_diags),'diag_spacing')
      call mem_free_r3(mesh_xz,(1),(2),(0),(4),(1),(nxd*nzd),'mesh_xz')
      call mem_free_r2(mesh_nodes,(1),(2),(1),(4*(nxd+nzd)),'mesh_nodes'
     &)
      call mem_free_i2(mesh_elements,(0),(1),(1),(2*(nxd+nzd)),'mesh_ele
     &ments')
      call mem_free_i2(mesh_edge_elements,(1),(4),(1),(nx_nz_max),'mesh_
     &edge_elements')
      call mem_free_i2(mesh_edge_dg_label,(1),(4),(1),(nx_nz_max),'mesh_
     &edge_dg_label')
      call mem_free_i1(mesh_edge_hv,(1),(4),'mesh_edge_hv')
      call mem_free_i1(mesh_curve_num,(1),(2*(nxd+nzd)),'mesh_curve_num'
     &)
      call mem_free_i1(mesh_scratch,(1),(2*(nxd+nzd)),'mesh_scratch')
      call mem_free_i2(poly_int_props,(1),(3),(1),(g2_num_polygons),'pol
     &y_int_props')
      call mem_free_r2(poly_real_props,(1),(5),(1),(g2_num_polygons),'po
     &ly_real_props')
      return
      end
      subroutine read_uedge_mesh(nunit,nxd,nzd,nxpt,mesh_xz)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer nunit,nxd,nzd,nxpt
      REAL(kind=DOUBLE)mesh_xz(1:2,0:4,nxd,nzd)
      integer ix,iz,i
      integer corner_ptr(0:4),iysptrx1(2),iysptrx2(2),ixlb(2),ixpt1(2),i
     &xmdp(2),ixpt2(2),ixrb(2)
      REAL(kind=DOUBLE)dummy
      if((nxpt.GE.1).AND.(nxpt.LE.2))continue
      do i=1,nxpt
      read(nunit,*)iysptrx1(i),iysptrx2(i)
      read(nunit,*)ixlb(i),ixpt1(i),ixmdp(i),ixpt2(i),ixrb(i)
      end do
      corner_ptr(0)=0
      corner_ptr(1)=1
      corner_ptr(2)=4
      corner_ptr(3)=2
      corner_ptr(4)=3
      read(nunit,*) (((mesh_xz(1,corner_ptr(i),ix,iz),ix=1,nxd),iz=1,nzd
     &),i=0,4)
      read(nunit,*) (((mesh_xz(2,corner_ptr(i),ix,iz),ix=1,nxd),iz=1,nzd
     &),i=0,4)
      dummy=(0.0_DOUBLE)
      open(unit=nunit+1,file='uedge_sonnet',status='unknown',form='forma
     &tted')
      write(nunit+1,*)
      write(nunit+1,*)'  Element output:'
      write(nunit+1,*)
      write(nunit+1,'(a,f16.13)')'    R*Btor =   ',dummy
      write(nunit+1,*)'   ncut   =  0'
      write(nunit+1,*)
      do iz=1,nzd
      do ix=1,nxd
      write(nunit+1,'(a,i4,a,i3,a,i3,a,e17.10,a,e17.10,a,6x,a,e17.10,a,e
     &17.10,a)')'   Element ',(iz-1)*nxd+ix-1,' = (',ix-1,',',iz-1,'): (
     &',mesh_xz(1,2,ix,iz),',',mesh_xz(2,2,ix,iz),')','(',mesh_xz(1,3,ix
     &,iz),',',mesh_xz(2,3,ix,iz),')'
      write(nunit+1,'(a,e17.10,13x,a,e17.10,a,e17.10,a)')'   Field ratio
     &  = ',dummy,'(',mesh_xz(1,0,ix,iz),',',mesh_xz(2,0,ix,iz),')'
      write(nunit+1,'(29x,a,e17.10,a,e17.10,a,6x,a,e17.10,a,e17.10,a)')'
     &(',mesh_xz(1,1,ix,iz),',',mesh_xz(2,1,ix,iz),')','(',mesh_xz(1,4,i
     &x,iz),',',mesh_xz(2,4,ix,iz),')'
      write(nunit+1,*)'  -----------------------------------------------
     &------------------------------'
      end do
      end do
      close(unit=nunit+1)
      return
      end
      subroutine find_min_max(num_nodes,nodes,node_type,x_min,x_max,z_mi
     &n,z_max)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer num_nodes
      integer node_type(*)
      REAL(kind=DOUBLE)nodes(1:2,*)
      REAL(kind=DOUBLE)x_min,x_max,z_min,z_max
      integer inode
      x_min=(1.e5_DOUBLE)
      x_max=(0.0_DOUBLE)
      z_min=(1.e5_DOUBLE)
      z_max=(0.0_DOUBLE)
      do inode=1,num_nodes
      if(node_type(inode).NE.2)then
      x_min=min(x_min,nodes(1,inode))
      x_max=max(x_max,nodes(1,inode))
      z_min=min(z_min,nodes(2,inode))
      z_max=max(z_max,nodes(2,inode))
      end if
      end do
      if(x_max.GT.x_min)continue
      if(z_max.GT.z_min)continue
      return
      end
      subroutine print_walls(nunit,file_format,walloutfile,num_walls,wal
     &l_segment_count,wall_nodes,nodes)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer nunit,num_walls
      integer wall_segment_count(*),wall_nodes(0:200000-1,*)
      REAL(kind=DOUBLE)nodes(1:2,*)
      character*300 file_format
      character*96 walloutfile
      integer num_sections,isec,istart,iend,max_segs,iwall,iseg,inode,b,
     &e
      character*2 csec
      character*130 wall_line
      character*31 wall_chunk
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      if(file_format.EQ.'linear')then
      if(walloutfile.NE.'undefined')then
      open(unit=nunit,file=walloutfile  (1:string_length(walloutfile)),s
     &tatus='unknown')
      end if
      write(nunit,*)num_walls
      write(nunit,*) (wall_segment_count(iwall)+1,iwall=1,num_walls)
      do iwall=1,num_walls
      do iseg=0,wall_segment_count(iwall)
      inode=wall_nodes(iseg,iwall)
      write(nunit,'(1x,1pe18.10,2x,e18.10)')nodes(1,inode),nodes(2,inode
     &)
      end do
      end do
      else if(file_format.EQ.'tabular')then
      num_sections=(num_walls/4)
      if(num_sections*4.LT.num_walls)then
      num_sections=num_sections+1
      else if(num_sections*4.GT.num_walls)then
      if('Problem calculating num_sections'.EQ.' ')continue
      end if
      do isec=1,num_sections
      if(walloutfile.NE.'undefined')then
      write(csec,'(i2.2)')isec
      open(unit=nunit,file=walloutfile  (1:string_length(walloutfile))//
     &csec,status='unknown')
      end if
      istart=4*(isec-1)+1
      iend=min(4*isec,num_walls)
      max_segs=0
      do iwall=istart,iend
      max_segs=max(max_segs,wall_segment_count(iwall))
      end do
      do iseg=-1,max_segs
      wall_line='  '
      b=1
      e=2
      do iwall=istart,iend
      if(iseg.EQ.-1)then
      write(wall_chunk,'(5x,a,i2.2,11x,a,i2.2,6x)')'X_',iwall,'Z_',iwall
      else if(iseg.LE.wall_segment_count(iwall))then
      inode=wall_nodes(iseg,iwall)
      write(wall_chunk,'(1pe14.6,1x,e14.6,2x)')nodes(1,inode),nodes(2,in
     &ode)
      else
      write(wall_chunk,'(7x,a,14x,a,7x)')'.','.'
      end if
      wall_line=wall_line(b:e)//wall_chunk
      e=e+(30)
      end do
      write(nunit,*)wall_line(b:e)
      end do
      if(walloutfile.EQ.'undefined')then
      write(nunit,*)
      write(nunit,*)'---------------------------------------------------
     &-----'
      write(nunit,*)
      else
      close(unit=31)
      end if
      end do
      else
      write(0,*)' The format for the wall file must be either "linear" o
     &r "tabular"'
      end if
      return
      end
      subroutine specify_polygon(nunit,num_walls,wall_segment_count,wall
     &_nodes,nodes,dg_elements_list,num_dg_poly,dg_poly_num_elements,dg_
     &polygons,dg_poly_num_meshcon,dg_poly_meshcon,dg_poly_meshcon_hv,xb
     &_min,xb_max,zb_min,zb_max,nxd,nzd,mesh_xz,mesh_nodes,mesh_elements
     &,mesh_edge_num_elements,mesh_edge_elements,mesh_edge_dg_label,mesh
     &_edge_hv,mesh_curve_num,mesh_scratch,polygon,n,int_props,real_prop
     &s,aux_stratum,aux_stratum_pts,num_aux_stratum_pts,process_polygon)
      
      use ma_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer nunit,num_walls,num_dg_poly,nxd,nzd
      integer wall_segment_count(*),wall_nodes(0:200000-1,*),dg_elements
     &_list(0:1,*),dg_poly_num_elements(*),dg_polygons(1:2000,*),dg_poly
     &_num_meshcon(*),dg_poly_meshcon(1:2,1:2,*),dg_poly_meshcon_hv(1:2,
     &1:2,*),mesh_elements(0:1,*),mesh_edge_num_elements(4),mesh_edge_el
     &ements(4,*),mesh_edge_dg_label(4,*),mesh_edge_hv(4),mesh_curve_num
     &(*),mesh_scratch(*)
      REAL(kind=DOUBLE)xb_min,xb_max,zb_min,zb_max
      REAL(kind=DOUBLE)nodes(1:2,*),mesh_xz(1:2,0:4,nxd,nzd),mesh_nodes(
     &1:2,*)
      integer n,process_polygon,aux_stratum,num_aux_stratum_pts
      integer int_props(1:3),aux_stratum_pts(0:2000-1)
      REAL(kind=DOUBLE)polygon(1:2,0:2000-1),real_props(1:5)
      integer length,p,b,e,i,wall,start,stop,temp,ix_start,ix_stop,iz_st
     &art,iz_stop,ix_step,iz_step,corner,ixi,izi,num,ix,iz,nout,step,i_p
     &rop,aux_stratum_def,aux_stratum_start,poly,j,num_ends,end_point,en
     &d_tip,print_polygon,iedge_points,num_curves,num_iedges,i_tot,close
     &d_curve,j_min,j_max,n_iedge,iedge_order,i_mc,swap_edges,tmp_edge,t
     &mp_seg,j_init,j_fin,common_node,edge_reverse,xcut
      integer end_nodes(2*2000),end_elements(2*2000),end_tips(2*2000),cu
     &rve_type(5),curve_start(5),curve_start_tip(5),iedges(4),mc_edge(1:
     &2,1:2),mc_seg(1:2,1:2),mc_node(4)
      REAL(kind=DOUBLE)poly_area,min_dist
      REAL(kind=DOUBLE)iedge_temp(1:2,0:2000-1),dist(4)
      character*96 polyoutfile
      character*300 line,keyword
      REAL(kind=DOUBLE)poly_p(3,2)
      REAL(kind=DOUBLE)iedge_p(3,2)
      REAL(kind=DOUBLE)delta(3)
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      REAL(kind=DOUBLE)vector_temp(3)
      external polygon_area
      REAL(kind=DOUBLE)polygon_area
      process_polygon=1000000000
      print_polygon=0
      n=0
      poly=1000000000
      iedge_points=0
      num_iedges=0
      do i_prop=1,3
      int_props(i_prop)=314159265
      end do
      do i_prop=1,5
      real_props(i_prop)=(314159265.3589793238462_DOUBLE)
      end do
      aux_stratum=314159265
      num_aux_stratum_pts=0
      aux_stratum_def=0
90024 continue
      if(read_string(20,line,length))continue
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      keyword=line(b:e)
      if(keyword.EQ.'outer')then
90025 continue
      if(next_token(line,b,e,p))then
      i=read_integer(line(b:e))
      if(i.EQ.0.OR.i.EQ.4)then
      polygon(1,n)=xb_min
      polygon(2,n)=zb_min
      else if(i.EQ.1)then
      polygon(1,n)=xb_min
      polygon(2,n)=zb_max
      else if(i.EQ.2)then
      polygon(1,n)=xb_max
      polygon(2,n)=zb_max
      else if(i.EQ.3)then
      polygon(1,n)=xb_max
      polygon(2,n)=zb_min
      end if
      n=n+1
      if(n.LE.2000-1)continue
      go to 90025
      end if
      
      else if(keyword.EQ.'wall')then
      if(next_token(line,b,e,p))continue
      wall=read_integer(line(b:e))
      if(wall.GT.0.AND.wall.LE.num_walls)continue
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.'*')then
      start=0
      stop=wall_segment_count(wall)
      else
      start=read_integer(line(b:e))
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.'*')then
      stop=wall_segment_count(wall)
      else
      stop=read_integer(line(b:e))
      end if
      end if
      if(next_token(line,b,e,p))then
      if(line(b:e).EQ.'reverse')continue
      temp=start
      start=stop
      stop=temp
      end if
      if(start.LE.stop)then
      step=1
      else if(start.GT.stop)then
      step=-1
      end if
      do i=start,stop,step
      if(i.GE.0.AND.i.LE.wall_segment_count(wall))continue
      polygon(1,n)=nodes(1,wall_nodes(i,wall))
      polygon(2,n)=nodes(2,wall_nodes(i,wall))
      n=n+1
      if(n.LE.2000-1)continue
      end do
      
      else if(keyword.EQ.'edge')then
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.'*')then
      ix_start=0
      ix_stop=nxd
      else
      ix_start=read_integer(line(b:e))
      if(next_token(line,b,e,p))continue
      ix_stop=read_integer(line(b:e))
      end if
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.'*')then
      iz_start=0
      iz_stop=nzd
      else
      iz_start=read_integer(line(b:e))
      if(next_token(line,b,e,p))continue
      iz_stop=read_integer(line(b:e))
      end if
      edge_reverse=0
      xcut=0
      if(next_token(line,b,e,p))then
      if(line(b:e).EQ.'reverse')then
      edge_reverse=1
      if(next_token(line,b,e,p))then
      if(line(b:e).EQ.'xcut')continue
      xcut=1
      end if
      else
      if(line(b:e).EQ.'xcut')continue
      xcut=1
      if(next_token(line,b,e,p))then
      if(line(b:e).EQ.'reverse')continue
      edge_reverse=1
      end if
      end if
      end if
      if(edge_reverse.EQ.1)then
      temp=ix_start
      ix_start=ix_stop
      ix_stop=temp
      temp=iz_start
      iz_start=iz_stop
      iz_stop=temp
      end if
      if(iz_stop.EQ.iz_start)then
      iz_step=0
      if(ix_stop.GE.ix_start)then
      ix_step=1
      num=ix_stop-ix_start+1
      else
      ix_step=-1
      num=ix_start-ix_stop+1
      end if
      else if(ix_stop.EQ.ix_start)then
      ix_step=0
      if(iz_stop.GE.iz_start)then
      iz_step=1
      num=iz_stop-iz_start+1
      else
      iz_step=-1
      num=iz_start-iz_stop+1
      end if
      else
      if(' Not following a mesh edge'.EQ.' ')continue
      end if
      ix=ix_start
      iz=iz_start
      do i=1,num
      if(xcut.EQ.0)then
      if(ix.EQ.0.AND.iz.EQ.0)then
      corner=1
      ixi=1
      izi=1
      else if(ix.EQ.0)then
      corner=2
      ixi=1
      izi=iz
      else if(iz.EQ.0)then
      corner=4
      ixi=ix
      izi=1
      else
      corner=3
      ixi=ix
      izi=iz
      end if
      else
      if(ix.NE.0)continue
      if(iz.EQ.0)then
      corner=1
      ixi=ix
      izi=1
      else
      corner=2
      ixi=ix
      izi=iz
      end if
      end if
      polygon(1,n)=mesh_xz(1,corner,ixi,izi)
      polygon(2,n)=mesh_xz(2,corner,ixi,izi)
      ix=ix+(ix_step)
      iz=iz+(iz_step)
      n=n+1
      if(n.LE.2000-1)continue
      end do
      
      else if(keyword.EQ.'dg_polygon')then
      if(next_token(line,b,e,p))continue
      poly=read_integer(line(b:e))
      if(poly.GT.0.AND.poly.LE.num_dg_poly)continue
      if(n.EQ.0)continue
      call find_endpoints(dg_elements_list,dg_poly_num_elements(poly),dg
     &_polygons(1,poly),num_ends,end_nodes,end_elements,end_tips)
      if(num_ends.EQ.0)then
      end_point=1
      end_tip=0
      else if(num_ends.EQ.2)then
      end_point=end_elements(1)
      end_tip=end_tips(1)
      else
      if('should have 0 or 2 endpoints here'.EQ.' ')continue
      end if
      call elements_to_polygon(nodes,dg_elements_list,dg_poly_num_elemen
     &ts(poly),dg_polygons(1,poly),end_point,end_tip,n,polygon(1,0))
      if(num_ends.EQ.0)then
      if(n.EQ.dg_poly_num_elements(poly))continue
      if((polygon(1,n).EQ.polygon(1,0)).AND.(polygon(2,n).EQ.polygon(2,0
     &)))continue
      poly_area=polygon_area(n,polygon(1,0))
      if(poly_area.LT.(0.0_DOUBLE))call reverse_polygon(n,polygon(1,0))
      else
      if(n.EQ.dg_poly_num_elements(poly)+1)continue
      if((polygon(1,n-1).NE.polygon(1,0)).OR.(polygon(2,n-1).NE.polygon(
     &2,0)))continue
      end if
      
      else if(keyword.EQ.'iedge')then
      if(num_iedges.EQ.0)continue
90026 continue
      if(next_token(line,b,e,p))then
      num_iedges=num_iedges+1
      if(num_iedges.LE.4)continue
      if(line(b:e).EQ.'S')then
      iedges(num_iedges)=1
      else if(line(b:e).EQ.'E')then
      iedges(num_iedges)=2
      else if(line(b:e).EQ.'N')then
      iedges(num_iedges)=3
      else if(line(b:e).EQ.'W')then
      iedges(num_iedges)=4
      else
      write(0,*)' Unexpected iedge specification, ',line(b:e)
      if(.FALSE.)continue
      end if
      go to 90026
      end if
      if(num_iedges.GT.0)continue
      if(num_iedges.GT.1)then
      iedge_order=0
      do i=1,num_iedges-1
      if(mod(iedges(i+1),4).EQ.mod((iedges(i)+1),4))then
      if(iedge_order.EQ.0)then
      iedge_order=1
      else
      if(iedge_order.EQ.1)continue
      end if
      else if(mod(iedges(i+1),4).EQ.mod((iedges(i)-1),4))then
      if(iedge_order.EQ.0)then
      iedge_order=-1
      else
      if(iedge_order.EQ.-1)continue
      end if
      else
      if(' Invalid iedge ordering!'.EQ.' ')continue
      end if
      end do
      if(iedge_order.EQ.-1)then
      do i=1,num_iedges/2
      j=iedges(i)
      iedges(i)=iedges(num_iedges-i+1)
      iedges(num_iedges-i+1)=j
      end do
      else
      if(iedge_order.EQ.1)continue
      end if
      end if
      
      else if(keyword.EQ.'reverse')then
      if(n.GT.0)continue
      call reverse_polygon(n-1,polygon(1,0))
      else if(keyword.EQ.'stratum')then
      if(next_token(line,b,e,p))continue
      int_props(1)=read_integer(line(b:e))
      else if(keyword.EQ.'material')then
      if(next_token(line,b,e,p))continue
      int_props(2)=string_lookup(line(b:e),materials_sy,ma_num)
      if((int_props(2).GT.0.AND.int_props(2).LE.ma_num))continue
      else if(keyword.EQ.'temperature')then
      if(next_token(line,b,e,p))continue
      real_props(1)=read_real(line(b:e))
      if(real_props(1).GT.(0.0_DOUBLE))continue
      else if(keyword.EQ.'recyc_coef')then
      if(next_token(line,b,e,p))continue
      real_props(2)=read_real(line(b:e))
      if(real_props(2).GE.(0.0_DOUBLE).AND.real_props(2).LE.(1.0_DOUBLE)
     &)continue
      else if(keyword.EQ.'triangle_area')then
      if(next_token(line,b,e,p))continue
      real_props(3)=read_real(line(b:e))
      if(real_props(3).GT.(0.0_DOUBLE))continue
      else if(keyword.EQ.'triangle_hole')then
      if(next_token(line,b,e,p))continue
      int_props(3)=read_integer(line(b:e))
      if(int_props(3).GT.0)then
      if(int_props(3).EQ.1)continue
      if(next_token(line,b,e,p))continue
      real_props(4)=read_real(line(b:e))
      if(next_token(line,b,e,p))continue
      real_props(5)=read_real(line(b:e))
      end if
      else if(keyword.EQ.'aux_stratum')then
      if(next_token(line,b,e,p))continue
      if(aux_stratum.EQ.314159265)then
      aux_stratum=read_integer(line(b:e))
      if(aux_stratum.NE.314159265)continue
      else
      if(aux_stratum.EQ.read_integer(line(b:e)))continue
      end if
      aux_stratum_def=1
      aux_stratum_start=n
      else if(keyword.EQ.'end_aux_stratum')then
      if(aux_stratum_def.EQ.1)continue
      do i=aux_stratum_start,n-1
      num_aux_stratum_pts=num_aux_stratum_pts+1
      aux_stratum_pts(num_aux_stratum_pts)=i
      end do
      if(num_aux_stratum_pts.GT.0)continue
      aux_stratum_def=0
      else if(keyword.EQ.'print_polygon')then
      print_polygon=1
      if(next_token(line,b,e,p))then
      polyoutfile=line(b:e)
      nout=31
      else
      polyoutfile='undefined'
      nout=6
      end if
      else if(keyword.EQ.'breakup_polygon')then
      process_polygon=1
      else if(keyword.EQ.'triangulate_polygon')then
      process_polygon=2
      else if(keyword.EQ.'triangulate_to_zones')then
      process_polygon=3
      else if(keyword.EQ.'clear_polygon')then
      process_polygon=0
      else
      write(0,*)' Unexpected polygon keyword'
      process_polygon=0
      end if
      if(process_polygon.EQ.1000000000)go to 90024
      if((n.EQ.0).AND.(num_iedges.GT.0))then
      if(poly.EQ.1000000000)continue
      i_tot=0
      do i=1,num_iedges
      do j=1,mesh_edge_num_elements(iedges(i))
      i_tot=i_tot+1
      mesh_scratch(i_tot)=mesh_edge_elements(iedges(i),j)
      end do
      end do
      call count_curves(mesh_elements,i_tot,mesh_scratch,num_curves,curv
     &e_type,curve_start,curve_start_tip,mesh_curve_num)
      if(num_curves.GE.1)continue
      closed_curve=0
      do i=1,num_curves
      if(curve_type(i).EQ.2)then
      if(closed_curve.EQ.0)continue
      closed_curve=i
      end if
      end do
      if(closed_curve.NE.0)continue
      call elements_to_polygon(mesh_nodes,mesh_elements,i_tot,mesh_scrat
     &ch,curve_start(closed_curve),curve_start_tip(closed_curve),n,polyg
     &on(1,0))
      poly_area=polygon_area(n,polygon(1,0))
      if(poly_area.LT.(0.0_DOUBLE))call reverse_polygon(n,polygon(1,0))
      
      else if(n.GT.0)then
      if((num_iedges.EQ.0).AND.(poly.NE.1000000000))then
      if(dg_poly_num_meshcon(poly).GT.0)then
      if(dg_poly_num_meshcon(poly).EQ.1)continue
      call get_mc_edge_seg(dg_poly_meshcon(1,1,poly),dg_poly_meshcon_hv(
     &1,1,poly),mesh_edge_num_elements,mesh_edge_dg_label,mesh_edge_hv,m
     &c_edge(1,1),mc_seg(1,1))
      mc_node(1)=mesh_elements(0,mesh_edge_elements(mc_edge(1,1),mc_seg(
     &1,1)))
      mc_node(2)=mesh_elements(1,mesh_edge_elements(mc_edge(1,1),mc_seg(
     &1,1)))
      mc_node(3)=mesh_elements(0,mesh_edge_elements(mc_edge(2,1),mc_seg(
     &2,1)))
      mc_node(4)=mesh_elements(1,mesh_edge_elements(mc_edge(2,1),mc_seg(
     &2,1)))
      if((mc_node(1).EQ.mc_node(3)).OR.(mc_node(2).EQ.mc_node(3)))then
      common_node=mc_node(3)
      else if((mc_node(1).EQ.mc_node(4)).OR.(mc_node(2).EQ.mc_node(4)))t
     &hen
      common_node=mc_node(4)
      else
      if('Mesh connection elements do not have a common node!'.EQ.' ')co
     &ntinue
      end if
      polygon(1,n)=mesh_nodes(1,common_node)
      polygon(2,n)=mesh_nodes(2,common_node)
      n=n+1
      polygon(1,n)=polygon(1,0)
      polygon(2,n)=polygon(2,0)
      poly_area=polygon_area(n,polygon(1,0))
      if(poly_area.LT.(0.0_DOUBLE))call reverse_polygon(n,polygon(1,0))
      
      else
      if(dg_poly_num_meshcon(poly).EQ.0)continue
      end if
      else if(num_iedges.GT.0)then
      if(poly.NE.1000000000)continue
      if(dg_poly_num_meshcon(poly).EQ.2)continue
      do i_mc=1,2
      call get_mc_edge_seg(dg_poly_meshcon(1,i_mc,poly),dg_poly_meshcon_
     &hv(1,i_mc,poly),mesh_edge_num_elements,mesh_edge_dg_label,mesh_edg
     &e_hv,mc_edge(1,i_mc),mc_seg(1,i_mc))
      if(mod(mc_edge(2,i_mc),4).EQ.mod((mc_edge(1,i_mc)+1),4))then
      swap_edges=0
      if(mc_seg(1,i_mc).EQ.mesh_edge_num_elements(mc_edge(1,i_mc)))conti
     &nue
      if(mc_seg(2,i_mc).EQ.1)continue
      else if(mod(mc_edge(2,i_mc),4).EQ.mod((mc_edge(1,i_mc)-1),4))then
      swap_edges=1
      if(mc_seg(1,i_mc).EQ.1)continue
      if(mc_seg(2,i_mc).EQ.mesh_edge_num_elements(mc_edge(2,i_mc)))conti
     &nue
      else
      if(mc_edge(2,i_mc).EQ.mc_edge(1,i_mc))continue
      if(mc_seg(2,i_mc).EQ.mc_seg(1,i_mc)+1)then
      swap_edges=0
      else if(mc_seg(2,i_mc).EQ.mc_seg(1,i_mc)-1)then
      swap_edges=1
      else
      if('Bad mesh connection data'.EQ.' ')continue
      end if
      end if
      if(swap_edges.EQ.1)then
      tmp_edge=mc_edge(1,i_mc)
      tmp_seg=mc_seg(1,i_mc)
      mc_edge(1,i_mc)=mc_edge(2,i_mc)
      mc_seg(1,i_mc)=mc_seg(2,i_mc)
      mc_edge(2,i_mc)=tmp_edge
      mc_seg(2,i_mc)=tmp_seg
      end if
      end do
      if((mc_edge(2,1).EQ.iedges(1)).AND.(mc_edge(1,2).EQ.iedges(num_ied
     &ges)))then
      j_init=mc_seg(2,1)
      j_fin=mc_seg(1,2)
      else if((mc_edge(2,2).EQ.iedges(1)).AND.(mc_edge(1,1).EQ.iedges(nu
     &m_iedges)))then
      j_init=mc_seg(2,2)
      j_fin=mc_seg(1,1)
      else
      if('Mesh connection data and iedge specification inconsistent!'.EQ
     &.' ')continue
      end if
      i_tot=0
      do i=1,num_iedges
      if(i.EQ.1)then
      j_min=j_init
      else
      j_min=1
      end if
      if(i.EQ.num_iedges)then
      j_max=j_fin
      else
      j_max=mesh_edge_num_elements(iedges(i))
      end if
      do j=j_min,j_max
      i_tot=i_tot+1
      mesh_scratch(i_tot)=mesh_edge_elements(iedges(i),j)
      end do
      end do
      call elements_to_polygon(mesh_nodes,mesh_elements,i_tot,mesh_scrat
     &ch,1,0,n_iedge,iedge_temp(1,0))
      poly_p(1,1)=polygon(1,0)
      poly_p(2,1)=(0.0_DOUBLE)
      poly_p(3,1)=polygon(2,0)
      poly_p(1,2)=polygon(1,n-1)
      poly_p(2,2)=(0.0_DOUBLE)
      poly_p(3,2)=polygon(2,n-1)
      iedge_p(1,1)=iedge_temp(1,0)
      iedge_p(2,1)=(0.0_DOUBLE)
      iedge_p(3,1)=iedge_temp(2,0)
      iedge_p(1,2)=iedge_temp(1,n_iedge-1)
      iedge_p(2,2)=(0.0_DOUBLE)
      iedge_p(3,2)=iedge_temp(2,n_iedge-1)
      delta(1)=poly_p(1,2)-iedge_p(1,1)
      delta(2)=poly_p(2,2)-iedge_p(2,1)
      delta(3)=poly_p(3,2)-iedge_p(3,1)
      
      dist(1)=sqrt((delta(1)**2+delta(2)**2+delta(3)**2))
      delta(1)=poly_p(1,1)-iedge_p(1,1)
      delta(2)=poly_p(2,1)-iedge_p(2,1)
      delta(3)=poly_p(3,1)-iedge_p(3,1)
      
      dist(2)=sqrt((delta(1)**2+delta(2)**2+delta(3)**2))
      delta(1)=poly_p(1,1)-iedge_p(1,2)
      delta(2)=poly_p(2,1)-iedge_p(2,2)
      delta(3)=poly_p(3,1)-iedge_p(3,2)
      
      dist(3)=sqrt((delta(1)**2+delta(2)**2+delta(3)**2))
      delta(1)=poly_p(1,2)-iedge_p(1,2)
      delta(2)=poly_p(2,2)-iedge_p(2,2)
      delta(3)=poly_p(3,2)-iedge_p(3,2)
      
      dist(4)=sqrt((delta(1)**2+delta(2)**2+delta(3)**2))
      min_dist=min(dist(1),dist(2),dist(3),dist(4))
      if((min_dist.EQ.dist(2)).OR.(min_dist.EQ.dist(4)))then
      call reverse_polygon(n_iedge-1,iedge_temp(1,0))
      end if
      do i=0,n_iedge-1
      i_tot=n+i
      if(i_tot.LE.2000-1)continue
      polygon(1,i_tot)=iedge_temp(1,i)
      polygon(2,i_tot)=iedge_temp(2,i)
      end do
      n=i_tot+1
      polygon(1,n)=polygon(1,0)
      polygon(2,n)=polygon(2,0)
      poly_area=polygon_area(n,polygon(1,0))
      if(poly_area.LT.(0.0_DOUBLE))call reverse_polygon(n,polygon(1,0))
      
      else
      if((num_iedges.EQ.0).AND.(poly.EQ.1000000000))continue
      end if
      else
      if((process_polygon.NE.0).AND.(n.EQ.0).AND.(num_iedges.EQ.0))then
      if('Null polygon!'.EQ.' ')continue
      end if
      end if
      
      if(process_polygon.NE.0)then
      polygon(1,n)=polygon(1,0)
      polygon(2,n)=polygon(2,0)
      end if
      if(print_polygon.EQ.1)then
      if(polyoutfile.NE.'undefined')then
      open(unit=nout,file=polyoutfile  (1:string_length(polyoutfile)),st
     &atus='unknown')
      end if
      write(nout,'(8x,a,15x,a)')'X','Z'
      do i=0,n-1
      write(nout,'(1x,1pe14.6,2x,e14.6)')polygon(1,i),polygon(2,i)
      end do
      if(polyoutfile.NE.'undefined')then
      close(unit=nout)
      end if
      end if
      return
      end
      subroutine specify_diagnostic(nunit,stratum,solid,variable,tab_ind
     &ex,var_min,var_max,mult,spacing)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer nunit
      integer stratum,solid,variable,tab_index,spacing
      REAL(kind=DOUBLE)var_min,var_max,mult
      integer length,p,b,e
      character*300 line,keyword
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      stratum=314159265
      solid=1
      variable=0
      tab_index=0
      var_min=(0.0_DOUBLE)
      var_max=(0.0_DOUBLE)
      mult=(0.0_DOUBLE)
      spacing=0
90029 continue
      if(read_string(nunit,line,length))continue
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      keyword=line(b:e)
      if(keyword.EQ.'stratum')then
      if(next_token(line,b,e,p))continue
      stratum=read_integer(line(b:e))
      if(stratum.NE.314159265)continue
      if(next_token(line,b,e,p))then
      keyword=line(b:e)
      if(keyword.EQ.'nonsolid')then
      solid=0
      else if(keyword.EQ.'solid')then
      solid=1
      else
      write(0,*)' Unexpected sector type, ',line(b:e)
      if(.FALSE.)continue
      end if
      end if
      else if(keyword.EQ.'variable')then
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.'energy')then
      variable=1
      if(next_token(line,b,e,p))then
      if(line(b:e).EQ.'J')then
      mult=(1.0_DOUBLE)
      else if(line(b:e).EQ.'eV')then
      mult=(1.60217733e-19_DOUBLE)
      else
      write(0,*)' Unexpected energy unit, ',line(b:e)
      if(.FALSE.)continue
      end if
      else
      mult=(1.60217733e-19_DOUBLE)
      end if
      else if(line(b:e).EQ.'angle')then
      variable=2
      if(next_token(line,b,e,p))then
      if(line(b:e).EQ.'radians')then
      mult=(1.0_DOUBLE)
      else if(line(b:e).EQ.'degrees')then
      mult=atan2((0.0_DOUBLE),-(1.0_DOUBLE))/(1.8e2_DOUBLE)
      else
      write(0,*)' Unexpected angle unit, ',line(b:e)
      if(.FALSE.)continue
      end if
      else
      mult=atan2((0.0_DOUBLE),-(1.0_DOUBLE))/(1.8e2_DOUBLE)
      end if
      else
      write(0,*)' Unexpected diagnostic variable, ',line(b:e)
      if(.FALSE.)continue
      end if
      
      else if(keyword.EQ.'number')then
      if(next_token(line,b,e,p))continue
      tab_index=read_integer(line(b:e))
      if(tab_index.GT.0)continue
      else if(keyword.EQ.'minimum')then
      if(next_token(line,b,e,p))continue
      var_min=read_real(line(b:e))
      else if(keyword.EQ.'maximum')then
      if(next_token(line,b,e,p))continue
      var_max=read_real(line(b:e))
      else if(keyword.EQ.'multiplier')then
      if(next_token(line,b,e,p))continue
      mult=read_real(line(b:e))
      if(mult.NE.(0.0_DOUBLE))continue
      else if(keyword.EQ.'spacing')then
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.'linear')then
      spacing=1
      else if(line(b:e).EQ.'log')then
      spacing=2
      else
      write(0,*)' Unexpected spacing, ',line(b:e)
      if(.FALSE.)continue
      end if
      else if(keyword.EQ.'end_diagnostic')then
      if(stratum.NE.314159265)continue
      if(variable.NE.0)then
      if(tab_index.GT.0)continue
      if(mult.NE.(0.0_DOUBLE))continue
      if(var_min.NE.var_max)continue
      if(spacing.NE.0)continue
      if(spacing.EQ.2)then
      if(var_min*mult.GT.(0.0_DOUBLE))continue
      if(var_max*mult.GT.(0.0_DOUBLE))continue
      end if
      end if
      return
      end if
      go to 90029
      end
      subroutine update_zone_info(zonearray,ind_x,ind_z,zone_type_array,
     &n,polygon,center,y_div,y_values)
      
      use zn_mod
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer ind_x,ind_z,n,y_div
      REAL(kind=DOUBLE)polygon(1:2,0:*),y_values(0:y_div)
      integer zonearray(0:y_div-1)
      character*(*)zone_type_array(0:y_div-1)
      REAL(kind=DOUBLE)center(3)
      integer iy
      REAL(kind=DOUBLE)phi_mid,y_frac
      REAL(kind=DOUBLE)vector_temp(3)
      external define_surface,define_surface_a,start_cell,polygon_volume
     &,define_sector,lookup_surface
      integer define_surface,define_surface_a,start_cell,define_sector,l
     &ookup_surface
      REAL(kind=DOUBLE)polygon_volume
      
      do iy=0,y_div-1
      if(zone_volume(zonearray(iy)).EQ.(1.0e30_DOUBLE))then
      zone_volume(zonearray(iy))=(0.0_DOUBLE)
      zone_index(1,zonearray(iy))=ind_x
      zone_index(2,zonearray(iy))=ind_z
      zone_index(3,zonearray(iy))=iy
      zone_index(4,zonearray(iy))=zonearray(0)
      if(zone_type_array(iy).EQ."solid")then
      zone_type(zonearray(iy))=3
      zone_type_num(3)=zone_type_num(3)+1
      zone_pointer(zonearray(iy))=zone_type_num(3)
      else if(zone_type_array(iy).EQ."exit")then
      zone_type(zonearray(iy))=4
      zone_type_num(4)=zone_type_num(4)+1
      zone_pointer(zonearray(iy))=zone_type_num(4)
      else if(zone_type_array(iy).EQ."vacuum")then
      zone_type(zonearray(iy))=1
      zone_type_num(1)=zone_type_num(1)+1
      zone_pointer(zonearray(iy))=zone_type_num(1)
      else if(zone_type_array(iy).EQ."plasma")then
      zone_type(zonearray(iy))=2
      zone_type_num(2)=zone_type_num(2)+1
      zone_pointer(zonearray(iy))=zone_type_num(2)
      else if(zone_type_array(iy).EQ."exit")then
      zone_type(zonearray(iy))=4
      zone_type_num(4)=zone_type_num(4)+1
      zone_pointer(zonearray(iy))=zone_type_num(4)
      else
      write(0,*)'Unknown zone type: ',zone_type_array(iy)
      if(.FALSE.)continue
      end if
      call set_zn_min_max(n,polygon(1,0),zonearray(iy),.TRUE.)
      if((geometry_symmetry.EQ.4).OR.(geometry_symmetry.EQ.5).OR.(geomet
     &ry_symmetry.EQ.6))then
      if(y_values(iy).LT.y_values(iy+1))then
      zone_min(2,zonearray(iy))=y_values(iy)
      zone_max(2,zonearray(iy))=y_values(iy+1)
      else if(y_values(iy).GT.y_values(iy+1))then
      zone_max(2,zonearray(iy))=y_values(iy)
      zone_min(2,zonearray(iy))=y_values(iy+1)
      end if
      end if
      zone_center(1,zonearray(iy))=center(1)
      zone_center(2,zonearray(iy))=center(2)
      zone_center(3,zonearray(iy))=center(3)
      
      if(geometry_symmetry.EQ.4)then
      zone_center(2,zonearray(iy))=(0.5_DOUBLE)*(y_values(iy)+y_values(i
     &y+1))
      else if(geometry_symmetry.EQ.5.OR.geometry_symmetry.EQ.6)then
      if(center(2).EQ.(0.0_DOUBLE))continue
      phi_mid=(0.5_DOUBLE)*(y_values(iy)+y_values(iy+1))
      zone_center(1,zonearray(iy))=center(1)*cos(phi_mid)
      zone_center(2,zonearray(iy))=center(1)*sin(phi_mid)
      else
      if((geometry_symmetry.EQ.3).OR.(geometry_symmetry.EQ.1).OR.(geomet
     &ry_symmetry.EQ.2))continue
      end if
      else
      call set_zn_min_max(n,polygon(1,0),zonearray(iy),.FALSE.)
      end if
      if(geometry_symmetry.EQ.4.OR.geometry_symmetry.EQ.5.OR.geometry_sy
     &mmetry.EQ.6)then
      y_frac=abs(y_values(iy+1)-y_values(iy))/(universal_cell_max(2)-uni
     &versal_cell_min(2))
      else
      y_frac=(1.0_DOUBLE)
      end if
      zone_volume(zonearray(iy))=zone_volume(zonearray(iy))+y_frac*polyg
     &on_volume(n,polygon(1,0))
      end do
      return
      end
      subroutine universal_cell_3d(symmetry,min_corner,max_corner,vol)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer symmetry
      REAL(kind=DOUBLE)min_corner(3),max_corner(3)
      REAL(kind=DOUBLE)vol
      integer face,face1,face2,surf_type,i
      REAL(kind=DOUBLE)cos_y,sin_y
      REAL(kind=DOUBLE)x(3,0:4),apex(3),a(3),b(3),x0(3),x1(3),x2(3),x3(3
     &),coeff(10),tx(3,4)
      external define_surface,define_surface_a,start_cell,polygon_volume
     &,define_sector,lookup_surface
      integer define_surface,define_surface_a,start_cell,define_sector,l
     &ookup_surface
      REAL(kind=DOUBLE)polygon_volume
      
      external vector_compare
      integer vector_compare
      geometry_symmetry=symmetry
      
      if(min_corner(1).LT.max_corner(1))continue
      
      
      if(min_corner(2).LT.max_corner(2))continue
      
      
      if(min_corner(3).LT.max_corner(3))continue
      
      
      universal_cell_min(1)=min_corner(1)
      universal_cell_min(2)=min_corner(2)
      universal_cell_min(3)=min_corner(3)
      
      universal_cell_max(1)=max_corner(1)
      universal_cell_max(2)=max_corner(2)
      universal_cell_max(3)=max_corner(3)
      
      i=0
      x(1,i)=min_corner(1)
      x(2,i)=(0.0_DOUBLE)
      x(3,i)=min_corner(3)
      i=i+1
      x(1,i)=min_corner(1)
      x(2,i)=(0.0_DOUBLE)
      x(3,i)=max_corner(3)
      i=i+1
      x(1,i)=max_corner(1)
      x(2,i)=(0.0_DOUBLE)
      x(3,i)=max_corner(3)
      i=i+1
      x(1,i)=max_corner(1)
      x(2,i)=(0.0_DOUBLE)
      x(3,i)=min_corner(3)
      i=i+1
      x(1,i)=x(1,0)
      x(2,i)=x(2,0)
      x(3,i)=x(3,0)
      
      if((symmetry.EQ.2).OR.(symmetry.EQ.5).OR.(symmetry.EQ.6))then
      do i=0,3
      call conea(x(1,i),x(1,i+1),coeff,surf_type,apex)
      face=define_surface(coeff,.TRUE.)
      if(face.GT.0)then
      surface_points(1,0,face)=x(1,i)
      surface_points(2,0,face)=x(2,i)
      surface_points(3,0,face)=x(3,i)
      
      surface_points(1,1,face)=x(1,i+1)
      surface_points(2,1,face)=x(2,i+1)
      surface_points(3,1,face)=x(3,i+1)
      
      end if
      call add_surface(face,0,.TRUE.)
      end do
      if(symmetry.EQ.6)then
      cos_y=cos(min_corner(2))
      sin_y=sin(min_corner(2))
      a(1)=-sin_y
      a(2)=cos_y
      a(3)=(0.0_DOUBLE)
      b(1)=max_corner(1)*cos_y
      b(2)=max_corner(1)*sin_y
      b(3)=max_corner(3)
      call plane(b,a,coeff)
      face1=define_surface(coeff,.TRUE.)
      call add_surface(face1,0,.TRUE.)
      if(abs(max_corner(2)-min_corner(2)-atan2((0.0_DOUBLE),-(1.0_DOUBLE
     &))).GT.(1.0e-10_DOUBLE))then
      cos_y=cos(max_corner(2))
      sin_y=sin(max_corner(2))
      a(1)=-sin_y
      a(2)=cos_y
      a(3)=(0.0_DOUBLE)
      b(1)=max_corner(1)*cos_y
      b(2)=max_corner(1)*sin_y
      b(3)=max_corner(3)
      call plane(b,a,coeff)
      face2=define_surface(coeff,.TRUE.)
      call add_surface(-face2,0,.TRUE.)
      end if
      end if
      vol=(0.5_DOUBLE)*(max_corner(2)-min_corner(2))*(max_corner(1)**2-m
     &in_corner(1)**2)*(max_corner(3)-min_corner(3))
      else if((symmetry.EQ.1).OR.(symmetry.EQ.3).OR.(symmetry.EQ.4))then
      a(1)=(0.0_DOUBLE)
      a(2)=(1.0_DOUBLE)
      a(3)=(0.0_DOUBLE)
      call plane(min_corner,a,coeff)
      face1=define_surface(coeff,.TRUE.)
      call add_surface(face1,0,.TRUE.)
      call plane(max_corner,a,coeff)
      face2=define_surface(coeff,.TRUE.)
      call add_surface(-face2,0,.TRUE.)
      if(symmetry.NE.4)then
      call init_identity(tx)
      x0(1)=(0.0_DOUBLE)
      x0(2)=-(max_corner(2)-min_corner(2))
      x0(3)=(0.0_DOUBLE)
      call geom_translate(x0,tx)
      call add_transform(face1,face2,tx)
      call invert(tx,tx)
      call add_transform(-face2,-face1,tx)
      end if
      do i=0,3
      x1(1)=x(1,i)
      x1(2)=x(2,i)
      x1(3)=x(3,i)
      
      x2(1)=x(1,i+1)
      x2(2)=x(2,i+1)
      x2(3)=x(3,i+1)
      
      if(vector_compare(x1,x2).GT.0)then
      x3(1)=x1(1)
      x3(2)=(1.0_DOUBLE)
      x3(3)=x1(3)
      else
      x3(1)=x2(1)
      x3(2)=(1.0_DOUBLE)
      x3(3)=x2(3)
      end if
      call planea(x1,x3,x2,coeff)
      face=define_surface_a(coeff,x1,x2)
      if(i.EQ.3)face1=face
      if(i.EQ.1)face2=-face
      if(face.GT.0)then
      surface_points(1,0,face)=x1(1)
      surface_points(2,0,face)=x1(2)
      surface_points(3,0,face)=x1(3)
      
      surface_points(1,1,face)=x2(1)
      surface_points(2,1,face)=x2(2)
      surface_points(3,1,face)=x2(3)
      
      end if
      call add_surface(face,0,.TRUE.)
      end do
      if(symmetry.EQ.3)then
      call init_identity(tx)
      x0(1)=(0.0_DOUBLE)
      x0(2)=(0.0_DOUBLE)
      x0(3)=-(max_corner(3)-min_corner(3))
      call geom_translate(x0,tx)
      call add_transform(face1,face2,tx)
      call invert(tx,tx)
      call add_transform(-face2,-face1,tx)
      end if
      vol=(max_corner(1)-min_corner(1))*(max_corner(2)-min_corner(2))*(m
     &ax_corner(3)-min_corner(3))
      end if
      universal_cell_vol=vol
      return
      end
      subroutine triangle_centroid(triangle,center)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)triangle(1:2,0:3)
      REAL(kind=DOUBLE)center(3)
      REAL(kind=DOUBLE)area,k,a,b,c,alpha,gamma,den,lc
      REAL(kind=DOUBLE)a_vertex(3)
      REAL(kind=DOUBLE)b_vertex(3)
      REAL(kind=DOUBLE)c_vertex(3)
      REAL(kind=DOUBLE)a_vec(3)
      REAL(kind=DOUBLE)b_vec(3)
      REAL(kind=DOUBLE)c_vec(3)
      REAL(kind=DOUBLE)a_hat(3)
      REAL(kind=DOUBLE)c_hat(3)
      REAL(kind=DOUBLE)bc_cross(3)
      REAL(kind=DOUBLE)c_perp(3)
      REAL(kind=DOUBLE)neg_y(3)
      REAL(kind=DOUBLE)m_test1(3)
      REAL(kind=DOUBLE)m_test2(3)
      REAL(kind=DOUBLE)vector_temp(3)
      a_vertex(1)=triangle(1,0)
      a_vertex(2)=(0.0_DOUBLE)
      a_vertex(3)=triangle(2,0)
      b_vertex(1)=triangle(1,1)
      b_vertex(2)=(0.0_DOUBLE)
      b_vertex(3)=triangle(2,1)
      c_vertex(1)=triangle(1,2)
      c_vertex(2)=(0.0_DOUBLE)
      c_vertex(3)=triangle(2,2)
      c_vec(1)=b_vertex(1)-a_vertex(1)
      c_vec(2)=b_vertex(2)-a_vertex(2)
      c_vec(3)=b_vertex(3)-a_vertex(3)
      
      vector_temp(1)=sqrt((c_vec(1)**2+c_vec(2)**2+c_vec(3)**2))
      if(vector_temp(1).GT.(0.0_DOUBLE))continue
      c_hat(1)=((1.0_DOUBLE)/vector_temp(1))*c_vec(1)
      c_hat(2)=((1.0_DOUBLE)/vector_temp(1))*c_vec(2)
      c_hat(3)=((1.0_DOUBLE)/vector_temp(1))*c_vec(3)
      
      a_vec(1)=c_vertex(1)-b_vertex(1)
      a_vec(2)=c_vertex(2)-b_vertex(2)
      a_vec(3)=c_vertex(3)-b_vertex(3)
      
      vector_temp(1)=sqrt((a_vec(1)**2+a_vec(2)**2+a_vec(3)**2))
      if(vector_temp(1).GT.(0.0_DOUBLE))continue
      a_hat(1)=((1.0_DOUBLE)/vector_temp(1))*a_vec(1)
      a_hat(2)=((1.0_DOUBLE)/vector_temp(1))*a_vec(2)
      a_hat(3)=((1.0_DOUBLE)/vector_temp(1))*a_vec(3)
      
      neg_y(1)=(0.0_DOUBLE)
      neg_y(2)=-(1.0_DOUBLE)
      neg_y(3)=(0.0_DOUBLE)
      vector_temp(1)=c_hat(2)*neg_y(3)-c_hat(3)*neg_y(2)
      vector_temp(2)=c_hat(3)*neg_y(1)-c_hat(1)*neg_y(3)
      c_perp(3)=c_hat(1)*neg_y(2)-c_hat(2)*neg_y(1)
      c_perp(1)=vector_temp(1)
      c_perp(2)=vector_temp(2)
      b_vec(1)=a_vertex(1)-c_vertex(1)
      b_vec(2)=a_vertex(2)-c_vertex(2)
      b_vec(3)=a_vertex(3)-c_vertex(3)
      
      a=sqrt((a_vec(1)**2+a_vec(2)**2+a_vec(3)**2))
      b=sqrt((b_vec(1)**2+b_vec(2)**2+b_vec(3)**2))
      c=sqrt((c_vec(1)**2+c_vec(2)**2+c_vec(3)**2))
      vector_temp(1)=b_vec(2)*c_vec(3)-b_vec(3)*c_vec(2)
      vector_temp(2)=b_vec(3)*c_vec(1)-b_vec(1)*c_vec(3)
      bc_cross(3)=b_vec(1)*c_vec(2)-b_vec(2)*c_vec(1)
      bc_cross(1)=vector_temp(1)
      bc_cross(2)=vector_temp(2)
      area=(0.5_DOUBLE)*sqrt((bc_cross(1)**2+bc_cross(2)**2+bc_cross(3)*
     &*2))
      alpha=(1.0_DOUBLE)/a
      gamma=(1.0_DOUBLE)/c
      k=(2.0_DOUBLE)*area/(3._DOUBLE)
      den=a_hat(1)*c_hat(3)-a_hat(3)*c_hat(1)
      if(den.GT.(0.0_DOUBLE))continue
      lc=(-k*alpha+gamma*k*(a_hat(1)*c_hat(1)+a_hat(3)*c_hat(3))+a_hat(3
     &)*(a_vertex(1)-c_vertex(1))+a_hat(1)*(c_vertex(3)-a_vertex(3)))/de
     &n
      center(1)=a_vertex(1)+c_hat(1)*(lc)
      center(2)=a_vertex(2)+c_hat(2)*(lc)
      center(3)=a_vertex(3)+c_hat(3)*(lc)
      
      center(1)=center(1)+c_perp(1)*(k*gamma)
      center(2)=center(2)+c_perp(2)*(k*gamma)
      center(3)=center(3)+c_perp(3)*(k*gamma)
      
      m_test1(1)=center(1)-a_vertex(1)
      m_test1(2)=center(2)-a_vertex(2)
      m_test1(3)=center(3)-a_vertex(3)
      
      vector_temp(1)=m_test1(2)*c_vec(3)-m_test1(3)*c_vec(2)
      vector_temp(2)=m_test1(3)*c_vec(1)-m_test1(1)*c_vec(3)
      m_test2(3)=m_test1(1)*c_vec(2)-m_test1(2)*c_vec(1)
      m_test2(1)=vector_temp(1)
      m_test2(2)=vector_temp(2)
      if((m_test2(1)*neg_y(1)+m_test2(2)*neg_y(2)+m_test2(3)*neg_y(3)).G
     &T.(0.0_DOUBLE))continue
      m_test1(1)=center(1)-b_vertex(1)
      m_test1(2)=center(2)-b_vertex(2)
      m_test1(3)=center(3)-b_vertex(3)
      
      vector_temp(1)=m_test1(2)*a_vec(3)-m_test1(3)*a_vec(2)
      vector_temp(2)=m_test1(3)*a_vec(1)-m_test1(1)*a_vec(3)
      m_test2(3)=m_test1(1)*a_vec(2)-m_test1(2)*a_vec(1)
      m_test2(1)=vector_temp(1)
      m_test2(2)=vector_temp(2)
      if((m_test2(1)*neg_y(1)+m_test2(2)*neg_y(2)+m_test2(3)*neg_y(3)).G
     &T.(0.0_DOUBLE))continue
      m_test1(1)=center(1)-c_vertex(1)
      m_test1(2)=center(2)-c_vertex(2)
      m_test1(3)=center(3)-c_vertex(3)
      
      vector_temp(1)=m_test1(2)*b_vec(3)-m_test1(3)*b_vec(2)
      vector_temp(2)=m_test1(3)*b_vec(1)-m_test1(1)*b_vec(3)
      m_test2(3)=m_test1(1)*b_vec(2)-m_test1(2)*b_vec(1)
      m_test2(1)=vector_temp(1)
      m_test2(2)=vector_temp(2)
      if((m_test2(1)*neg_y(1)+m_test2(2)*neg_y(2)+m_test2(3)*neg_y(3)).G
     &T.(0.0_DOUBLE))continue
      return
      end
      subroutine quad_center(quad,center)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)quad(1:2,0:4)
      REAL(kind=DOUBLE)center(3)
      center(1)=(0.25_DOUBLE)*(quad(1,0)+quad(1,1)+quad(1,2)+quad(1,3))
      center(2)=(0.0_DOUBLE)
      center(3)=(0.25_DOUBLE)*(quad(2,0)+quad(2,1)+quad(2,2)+quad(2,3))
      return
      end
      function polygon_area(n,x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)polygon_area
      integer n
      REAL(kind=DOUBLE)x(1:2,0:n)
      integer i
      REAL(kind=DOUBLE)area
      if(x(1,0).EQ.x(1,n))continue
      if(x(2,0).EQ.x(2,n))continue
      area=(0.0_DOUBLE)
      do i=0,n-1
      area=area+((0.5_DOUBLE)*(x(1,i)+x(1,i+1))*(x(2,i)-x(2,i+1)))
      end do
      polygon_area=area
      return
      end
      subroutine reverse_polygon(n,x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer n
      REAL(kind=DOUBLE)x(1:2,0:n)
      integer i
      REAL(kind=DOUBLE)x_temp(1:2)
      do i=0,(n-1)/2
      x_temp(1)=x(1,n-i)
      x_temp(2)=x(2,n-i)
      x(1,n-i)=x(1,i)
      x(2,n-i)=x(2,i)
      x(1,i)=x_temp(1)
      x(2,i)=x_temp(2)
      end do
      return
      end
      subroutine find_endpoints(elements_list,n,poly_elements,num_ends,e
     &nd_nodes,end_elements,end_tips)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer n
      integer elements_list(0:1,*),poly_elements(n)
      integer num_ends
      integer end_nodes(*),end_elements(*),end_tips(*)
      integer i,tip,test_node,match,j
      num_ends=0
      do i=1,n
      do tip=0,1
      test_node=elements_list(tip,poly_elements(i))
      match=0
      do j=1,n
      if((j.NE.i).AND.((elements_list(0,poly_elements(j)).EQ.test_node).
     &OR.(elements_list(1,poly_elements(j)).EQ.test_node)))then
      if(match.EQ.0)continue
      match=1
      end if
      end do
      if(match.EQ.0)then
      num_ends=num_ends+1
      end_nodes(num_ends)=test_node
      end_elements(num_ends)=i
      end_tips(num_ends)=tip
      end if
      end do
      end do
      if((2*(num_ends/2)-num_ends).EQ.0)continue
      return
      end
      subroutine elements_to_polygon(nodes,elements_list,n,poly_elements
     &,initial_element,initial_tip,poly_num_points,poly_x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer n,initial_element,initial_tip
      integer elements_list(0:1,*),poly_elements(n)
      REAL(kind=DOUBLE)nodes(1:2,*)
      integer poly_num_points
      REAL(kind=DOUBLE)poly_x(1:2,0:*)
      integer i,j_match,node,tip,j,match,tip_match
      if(initial_element.GE.1)continue
      if(initial_element.LE.n)continue
      if((initial_tip.EQ.0).OR.(initial_tip.EQ.1))continue
      poly_num_points=0
      i=initial_element
      j_match=0
      node=elements_list(initial_tip,poly_elements(initial_element))
      poly_x(1,poly_num_points)=nodes(1,node)
      poly_x(2,poly_num_points)=nodes(2,node)
      poly_num_points=poly_num_points+1
      if(poly_num_points.LE.2000-1)continue
      if(initial_tip.EQ.0)then
      tip=1
      else
      tip=0
      end if
90030 continue
      node=elements_list(tip,poly_elements(i))
      poly_x(1,poly_num_points)=nodes(1,node)
      poly_x(2,poly_num_points)=nodes(2,node)
      poly_num_points=poly_num_points+1
      if(poly_num_points.LE.2000-1)continue
      match=0
      do j=1,n
      if((j.NE.i).AND.(elements_list(0,poly_elements(j)).EQ.elements_lis
     &t(tip,poly_elements(i))))then
      if(match.EQ.0)continue
      match=1
      j_match=j
      tip_match=1
      else if((j.NE.i).AND.(elements_list(1,poly_elements(j)).EQ.element
     &s_list(tip,poly_elements(i))))then
      if(match.EQ.0)continue
      match=1
      j_match=j
      tip_match=0
      end if
      end do
      if(j_match.NE.initial_element)then
      if(match.EQ.1)then
      i=j_match
      j_match=0
      tip=tip_match
      go to 90030
      else
      if((poly_num_points-1).LE.n)continue
      end if
      else
      if((poly_num_points-1).LE.n)continue
      poly_num_points=poly_num_points-1
      end if
      return
      end
      subroutine count_curves(elements_list,n,poly_elements,num_curves,c
     &urve_type,curve_start,curve_start_tip,element_curve_num)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer n
      integer elements_list(0:1,*),poly_elements(n)
      integer num_curves
      integer curve_type(5),curve_start(5),curve_start_tip(5),element_cu
     &rve_num(*)
      integer i,i_init,j_match,tip,node,match,j,tip_match,num_ends,i_tot
      integer end_nodes(2*2000),end_elements(2*2000),end_tips(2*2000)
      num_curves=0
      if(n.GT.1)continue
      do i=1,n
      element_curve_num(i)=0
      end do
      call find_endpoints(elements_list,n,poly_elements,num_ends,end_nod
     &es,end_elements,end_tips)
      if(num_ends.LE.2*2000)continue
      do i_tot=1,n+num_ends
      if(i_tot.LE.num_ends)then
      i_init=end_elements(i_tot)
      if(end_tips(i_tot).EQ.0)then
      tip=1
      else
      tip=0
      end if
      else
      i_init=i_tot-num_ends
      tip=1
      end if
      if(element_curve_num(i_init).EQ.0)then
      num_curves=num_curves+1
      if(num_curves.LE.5)continue
      i=i_init
      element_curve_num(i)=num_curves
      curve_start(num_curves)=i
      if(tip.EQ.0)then
      curve_start_tip(num_curves)=1
      else
      curve_start_tip(num_curves)=0
      end if
      j_match=0
90030 continue
      node=elements_list(tip,poly_elements(i))
      match=0
      do j=1,n
      if((j.NE.i).AND.(elements_list(0,poly_elements(j)).EQ.elements_lis
     &t(tip,poly_elements(i))))then
      if(match.EQ.0)continue
      match=1
      j_match=j
      tip_match=1
      else if((j.NE.i).AND.(elements_list(1,poly_elements(j)).EQ.element
     &s_list(tip,poly_elements(i))))then
      if(match.EQ.0)continue
      match=1
      j_match=j
      tip_match=0
      end if
      end do
      if(match.EQ.0)then
      curve_type(num_curves)=1
      else
      if(j_match.NE.i_init)then
      if(element_curve_num(j_match).EQ.0)continue
      element_curve_num(j_match)=num_curves
      i=j_match
      j_match=0
      tip=tip_match
      go to 90030
      else
      curve_type(num_curves)=2
      end if
      end if
      else
      if(element_curve_num(i_init).GT.0)continue
      if(element_curve_num(i_init).LE.num_curves)continue
      end if
      end do
      if(num_curves.GT.0)continue
      return
      end
      subroutine get_mc_edge_seg(meshcon,meshcon_hv,mesh_edge_num_elemen
     &ts,mesh_edge_dg_label,mesh_edge_hv,mc_edge,mc_seg)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer meshcon(1:2),meshcon_hv(1:2),mesh_edge_num_elements(4),mes
     &h_edge_dg_label(4,*),mesh_edge_hv(4)
      integer mc_edge(1:2),mc_seg(1:2)
      integer i_elem,i_edge,j_seg
      do i_elem=1,2
      mc_edge(i_elem)=1000000000
      mc_seg(i_elem)=1000000000
      do i_edge=1,4
      if(mesh_edge_hv(i_edge).EQ.meshcon_hv(i_elem))then
      do j_seg=1,mesh_edge_num_elements(i_edge)
      if(meshcon(i_elem).EQ.mesh_edge_dg_label(i_edge,j_seg))then
      mc_edge(i_elem)=i_edge
      mc_seg(i_elem)=j_seg
      end if
      end do
      end if
      end do
      if(mc_edge(i_elem).NE.1000000000)continue
      if(mc_seg(i_elem).NE.1000000000)continue
      end do
      return
      end
      subroutine match_edge_to_poly(poly_p,mesh_nodes,mesh_elements,mesh
     &_edge_num_elements,mesh_edge_elements,iedge1,iedge2,mesh_scratch,e
     &dge_chosen,closest_node,closest_elements,closest_tips)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer iedge1,iedge2
      integer mesh_elements(0:1,*),mesh_edge_num_elements(4),mesh_edge_e
     &lements(4,*),mesh_scratch(*)
      REAL(kind=DOUBLE)mesh_nodes(1:2,*)
      REAL(kind=DOUBLE)poly_p(3)
      integer edge_chosen,closest_node
      integer closest_elements(2),closest_tips(2)
      integer i,j
      integer iedges(2),close_node(2),close_elements(2,2),close_tips(2,2
     &)
      REAL(kind=DOUBLE)dist_p_edge(2)
      REAL(kind=DOUBLE)edge_p(3,2)
      REAL(kind=DOUBLE)delta_p(3,2)
      iedges(1)=iedge1
      iedges(2)=iedge2
      do i=1,2
      do j=1,mesh_edge_num_elements(iedges(i))
      mesh_scratch(j)=mesh_edge_elements(iedges(i),j)
      end do
      call closest_point(poly_p(1),mesh_nodes,mesh_elements,mesh_edge_nu
     &m_elements(iedges(i)),mesh_scratch,close_node(i),close_elements(1,
     &i),close_tips(1,i))
      edge_p(1,i)=mesh_nodes(1,close_node(i))
      edge_p(2,i)=(0.0_DOUBLE)
      edge_p(3,i)=mesh_nodes(2,close_node(i))
      delta_p(1,i)=edge_p(1,i)-poly_p(1)
      delta_p(2,i)=edge_p(2,i)-poly_p(2)
      delta_p(3,i)=edge_p(3,i)-poly_p(3)
      
      dist_p_edge(i)=sqrt((delta_p(1,i)**2+delta_p(2,i)**2+delta_p(3,i)*
     &*2))
      end do
      if(dist_p_edge(1).LE.dist_p_edge(2))then
      i=1
      else
      i=2
      end if
      edge_chosen=iedges(i)
      closest_node=close_node(i)
      do j=1,2
      closest_elements(j)=close_elements(j,i)
      closest_tips(j)=close_tips(j,i)
      end do
      return
      end
      subroutine closest_point(poly_p,mesh_nodes,mesh_elements,edge_num_
     &elements,edge_elements,closest_node,closest_elements,closest_tips)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer edge_num_elements
      integer mesh_elements(0:1,*),edge_elements(*)
      REAL(kind=DOUBLE)mesh_nodes(1:2,*)
      REAL(kind=DOUBLE)poly_p(3)
      integer closest_node
      integer closest_elements(2),closest_tips(2)
      integer num_closest,i,j
      REAL(kind=DOUBLE)closest_distance,dist
      REAL(kind=DOUBLE)edge_point(3)
      REAL(kind=DOUBLE)delta(3)
      closest_distance=(1.0e16_DOUBLE)
      num_closest=0
      do i=1,edge_num_elements
      do j=0,1
      edge_point(1)=mesh_nodes(1,mesh_elements(j,edge_elements(i)))
      edge_point(2)=(0.0_DOUBLE)
      edge_point(3)=mesh_nodes(2,mesh_elements(j,edge_elements(i)))
      delta(1)=poly_p(1)-edge_point(1)
      delta(2)=poly_p(2)-edge_point(2)
      delta(3)=poly_p(3)-edge_point(3)
      
      dist=sqrt((delta(1)**2+delta(2)**2+delta(3)**2))
      if(dist.LT.closest_distance)then
      closest_node=mesh_elements(j,edge_elements(i))
      closest_elements(1)=i
      closest_tips(1)=j
      num_closest=1
      closest_distance=dist
      else if(dist.EQ.closest_distance)then
      if(mesh_elements(j,edge_elements(i)).EQ.closest_node)continue
      closest_elements(2)=i
      closest_tips(2)=j
      num_closest=2
      end if
      end do
      end do
      if(num_closest.GT.0)continue
      if(num_closest.EQ.1)then
      closest_elements(2)=closest_elements(1)
      closest_tips(2)=closest_tips(1)
      end if
      return
      end
      subroutine setup_sectors(num_polygons,polygon_points,polygon_xz,po
     &lygon_segment,polygon_zone,poly_int_props,poly_real_props,num_aux_
     &sectors,aux_stratum,aux_stratum_poly,aux_stratum_points,aux_stratu
     &m_segment,y_div,sect_zone1,sect_zone2)
      
      use zn_mod
      
      use sc_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer num_polygons,num_aux_sectors,y_div
      integer polygon_segment(0:2000-1,*),polygon_points(*),polygon_zone
     &(*),poly_int_props(1:3,*),aux_stratum(*),aux_stratum_poly(*),aux_s
     &tratum_points(*),aux_stratum_segment(*)
      REAL(kind=DOUBLE)polygon_xz(1:2,0:2000-1,*),poly_real_props(1:5,*)
      integer sect_zone1(*),sect_zone2(*)
      integer i_poly,j,sector1,face1,i_aux,num_zone1,num_zone2,k_zone2,k
     &_zone1,side1_done,aux1_done
      REAL(kind=DOUBLE)x1(3)
      REAL(kind=DOUBLE)x2(3)
      interface
      function mem_alloc_c1(size,l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1
      character(len=size),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_c2(size,l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2
      character(len=size),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_c3(size,l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3
      character(len=size),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_c4(size,l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4
      character(len=size),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i1(l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      integer,dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i2(l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      integer,dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i3(l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      integer,dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i4(l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      integer,dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_i5(l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      integer,dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r1(l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      REAL(kind=DOUBLE),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r2(l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      REAL(kind=DOUBLE),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r3(l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r4(l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_alloc_r5(l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c1(p,size,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,nu
      character(len=size),dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c2(p,size,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,nu
      character(len=size),dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c3(p,size,l1,u1,l2,u2,l3,u3,nu,name)result(np
     &)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,nu
      character(len=size),dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_c4(p,size,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)res
     &ult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4,nu
      character(len=size),dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i1(p,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,nu
      integer,dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i2(p,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,nu
      integer,dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i3(p,l1,u1,l2,u2,l3,u3,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,nu
      integer,dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i4(p,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)result(n
     &p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,nu
      integer,dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_i5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu,name)re
     &sult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu
      integer,dimension(:,:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r1(p,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,nu
      REAL(kind=DOUBLE),dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r2(p,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,nu
      REAL(kind=DOUBLE),dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r3(p,l1,u1,l2,u2,l3,u3,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,nu
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r4(p,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)result(n
     &p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,nu
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      function mem_realloc_r5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu,name)re
     &sult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      end function
      subroutine mem_free_c1(p,size,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1
      character(len=size),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_c2(p,size,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2
      character(len=size),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_c3(p,size,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3
      character(len=size),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_c4(p,size,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4
      character(len=size),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i1(p,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      integer,dimension(:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i2(p,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      integer,dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i3(p,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      integer,dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i4(p,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      integer,dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_i5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      integer,dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r1(p,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      REAL(kind=DOUBLE),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r2(p,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      REAL(kind=DOUBLE),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r3(p,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r4(p,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      subroutine mem_free_r5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end subroutine
      end interface
      
      
      external define_surface,define_surface_a,start_cell,polygon_volume
     &,define_sector,lookup_surface
      integer define_surface,define_surface_a,start_cell,define_sector,l
     &ookup_surface
      REAL(kind=DOUBLE)polygon_volume
      
      do i_poly=1,num_polygons
      do j=0,polygon_points(i_poly)-1
      if(polygon_xz(1,j,i_poly).NE.polygon_xz(1,j+1,i_poly).OR.polygon_x
     &z(2,j,i_poly).NE.polygon_xz(2,j+1,i_poly))then
      x1(1)=polygon_xz(1,j,i_poly)
      x1(2)=(0.0_DOUBLE)
      x1(3)=polygon_xz(2,j,i_poly)
      x2(1)=polygon_xz(1,j+1,i_poly)
      x2(2)=(0.0_DOUBLE)
      x2(3)=polygon_xz(2,j+1,i_poly)
      face1=lookup_surface(x1,x2)
      call find_poly_zone(face1,zone_type(polygon_zone(i_poly)),polygon_
     &zone(i_poly),y_div,sect_zone1,num_zone1,sect_zone2,num_zone2)
      side1_done=0
      aux1_done=0
      if((num_zone1.GT.0).AND.(num_zone2.GT.0))then
      do k_zone1=1,num_zone1
      do k_zone2=1,num_zone2
      if((num_zone1.NE.num_zone2).OR.(zone_index(3,sect_zone2(k_zone2)).
     &EQ.zone_index(3,sect_zone1(k_zone1))))then
      if((zone_type(sect_zone1(k_zone1)).EQ.3).AND.(zone_type(sect_zone2
     &(k_zone2)).EQ.2))then
      if((num_zone1.EQ.1).OR.(num_zone1.EQ.num_zone2))continue
      sector1=define_sector(poly_int_props(1,i_poly),polygon_segment(j,i
     &_poly),-face1,sect_zone2(k_zone2),sect_zone1(k_zone1))
      sc_plasma_num=sc_plasma_num+1
      if(mod(((sc_plasma_num)-(0)+1),100).EQ.1)then
      plasma_sector =>mem_realloc_i1(plasma_sector,(0),(((int(((((sc_pla
     &sma_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_plasma_
     &num)-(0)+1))+100-1)/100)*100)+(0)-1),'plasma_sector')
      end if
      plasma_sector(sc_plasma_num)=sector1
      sector_type_pointer(2,sector1)=sc_plasma_num
      
      if((num_zone1.GT.1).OR.(side1_done.EQ.0))then
      sector1=define_sector(poly_int_props(1,i_poly),polygon_segment(j,i
     &_poly),face1,sect_zone1(k_zone1),sect_zone2(k_zone2))
      sc_target_num=sc_target_num+1
      if(mod(((sc_target_num)-(0)+1),100).EQ.1)then
      target_sector =>mem_realloc_i1(target_sector,(0),(((int(((((sc_tar
     &get_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_target_
     &num)-(0)+1))+100-1)/100)*100)+(0)-1),'target_sector')
      end if
      if(mod(((sc_target_num)-(0)+1),100).EQ.1)then
      target_material =>mem_realloc_i1(target_material,(0),(((int(((((sc
     &_target_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_tar
     &get_num)-(0)+1))+100-1)/100)*100)+(0)-1),'target_material')
      end if
      if(mod(((sc_target_num)-(0)+1),100).EQ.1)then
      target_temperature =>mem_realloc_r1(target_temperature,(0),(((int(
     &((((sc_target_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((
     &sc_target_num)-(0)+1))+100-1)/100)*100)+(0)-1),'target_temperature
     &')
      end if
      if(mod(((sc_target_num)-(0)+1),100).EQ.1)then
      target_recyc_coef =>mem_realloc_r1(target_recyc_coef,(0),(((int(((
     &((sc_target_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc
     &_target_num)-(0)+1))+100-1)/100)*100)+(0)-1),'target_recyc_coef')
      end if
      target_sector(sc_target_num)=sector1
      sector_type_pointer(3,sector1)=sc_target_num
      target_material(sc_target_num)=poly_int_props(2,i_poly)
      target_temperature(sc_target_num)=poly_real_props(1,i_poly)*(1.380
     &658e-23_DOUBLE)
      target_recyc_coef(sc_target_num)=poly_real_props(2,i_poly)
      side1_done=1
      end if
      else if((zone_type(sect_zone1(k_zone1)).EQ.3).AND.(zone_type(sect_
     &zone2(k_zone2)).EQ.1))then
      if((num_zone1.EQ.1).OR.(num_zone1.EQ.num_zone2))continue
      sector1=define_sector(poly_int_props(1,i_poly),polygon_segment(j,i
     &_poly),-face1,sect_zone2(k_zone2),sect_zone1(k_zone1))
      sc_vacuum_num=sc_vacuum_num+1
      if(mod(((sc_vacuum_num)-(0)+1),100).EQ.1)then
      vacuum_sector =>mem_realloc_i1(vacuum_sector,(0),(((int(((((sc_vac
     &uum_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_vacuum_
     &num)-(0)+1))+100-1)/100)*100)+(0)-1),'vacuum_sector')
      end if
      vacuum_sector(sc_vacuum_num)=sector1
      sector_type_pointer(1,sector1)=sc_vacuum_num
      if((num_zone1.GT.1).OR.(side1_done.EQ.0))then
      sector1=define_sector(poly_int_props(1,i_poly),polygon_segment(j,i
     &_poly),face1,sect_zone1(k_zone1),sect_zone2(k_zone2))
      sc_wall_num=sc_wall_num+1
      if(mod(((sc_wall_num)-(0)+1),100).EQ.1)then
      wall_sector =>mem_realloc_i1(wall_sector,(0),(((int(((((sc_wall_nu
     &m)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_wall_num)-(0)
     &+1))+100-1)/100)*100)+(0)-1),'wall_sector')
      end if
      if(mod(((sc_wall_num)-(0)+1),100).EQ.1)then
      wall_material =>mem_realloc_i1(wall_material,(0),(((int(((((sc_wal
     &l_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_wall_num)
     &-(0)+1))+100-1)/100)*100)+(0)-1),'wall_material')
      end if
      if(mod(((sc_wall_num)-(0)+1),100).EQ.1)then
      wall_temperature =>mem_realloc_r1(wall_temperature,(0),(((int(((((
     &sc_wall_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_wal
     &l_num)-(0)+1))+100-1)/100)*100)+(0)-1),'wall_temperature')
      end if
      if(mod(((sc_wall_num)-(0)+1),100).EQ.1)then
      wall_recyc_coef =>mem_realloc_r1(wall_recyc_coef,(0),(((int(((((sc
     &_wall_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_wall_
     &num)-(0)+1))+100-1)/100)*100)+(0)-1),'wall_recyc_coef')
      end if
      wall_sector(sc_wall_num)=sector1
      sector_type_pointer(4,sector1)=sc_wall_num
      wall_material(sc_wall_num)=poly_int_props(2,i_poly)
      wall_temperature(sc_wall_num)=poly_real_props(1,i_poly)*(1.380658e
     &-23_DOUBLE)
      wall_recyc_coef(sc_wall_num)=poly_real_props(2,i_poly)
      side1_done=1
      end if
      else if((zone_type(sect_zone1(k_zone1)).EQ.4).AND.(zone_type(sect_
     &zone2(k_zone2)).EQ.2))then
      if((num_zone1.EQ.1).OR.(num_zone1.EQ.num_zone2))continue
      sector1=define_sector(poly_int_props(1,i_poly),polygon_segment(j,i
     &_poly),-face1,sect_zone2(k_zone2),sect_zone1(k_zone1))
      sc_plasma_num=sc_plasma_num+1
      if(mod(((sc_plasma_num)-(0)+1),100).EQ.1)then
      plasma_sector =>mem_realloc_i1(plasma_sector,(0),(((int(((((sc_pla
     &sma_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_plasma_
     &num)-(0)+1))+100-1)/100)*100)+(0)-1),'plasma_sector')
      end if
      plasma_sector(sc_plasma_num)=sector1
      sector_type_pointer(2,sector1)=sc_plasma_num
      
      if((num_zone1.GT.1).OR.(side1_done.EQ.0))then
      sector1=define_sector(poly_int_props(1,i_poly),polygon_segment(j,i
     &_poly),face1,sect_zone1(k_zone1),sect_zone2(k_zone2))
      sc_exit_num=sc_exit_num+1
      if(mod(((sc_exit_num)-(0)+1),100).EQ.1)then
      exit_sector =>mem_realloc_i1(exit_sector,(0),(((int(((((sc_exit_nu
     &m)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_exit_num)-(0)
     &+1))+100-1)/100)*100)+(0)-1),'exit_sector')
      end if
      exit_sector(sc_exit_num)=sector1
      sector_type_pointer(5,sector1)=sc_exit_num
      side1_done=1
      end if
      else if((zone_type(sect_zone1(k_zone1)).EQ.4).AND.(zone_type(sect_
     &zone2(k_zone2)).EQ.1))then
      if((num_zone1.EQ.1).OR.(num_zone1.EQ.num_zone2))continue
      sector1=define_sector(poly_int_props(1,i_poly),polygon_segment(j,i
     &_poly),-face1,sect_zone2(k_zone2),sect_zone1(k_zone1))
      sc_vacuum_num=sc_vacuum_num+1
      if(mod(((sc_vacuum_num)-(0)+1),100).EQ.1)then
      vacuum_sector =>mem_realloc_i1(vacuum_sector,(0),(((int(((((sc_vac
     &uum_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_vacuum_
     &num)-(0)+1))+100-1)/100)*100)+(0)-1),'vacuum_sector')
      end if
      vacuum_sector(sc_vacuum_num)=sector1
      sector_type_pointer(1,sector1)=sc_vacuum_num
      if((num_zone1.GT.1).OR.(side1_done.EQ.0))then
      sector1=define_sector(poly_int_props(1,i_poly),polygon_segment(j,i
     &_poly),face1,sect_zone1(k_zone1),sect_zone2(k_zone2))
      sc_exit_num=sc_exit_num+1
      if(mod(((sc_exit_num)-(0)+1),100).EQ.1)then
      exit_sector =>mem_realloc_i1(exit_sector,(0),(((int(((((sc_exit_nu
     &m)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_exit_num)-(0)
     &+1))+100-1)/100)*100)+(0)-1),'exit_sector')
      end if
      exit_sector(sc_exit_num)=sector1
      sector_type_pointer(5,sector1)=sc_exit_num
      side1_done=1
      end if
      end if
      if(num_aux_sectors.GT.0)then
      do i_aux=1,num_aux_sectors
      if((aux_stratum_poly(i_aux).EQ.i_poly).AND.(aux_stratum_points(i_a
     &ux).EQ.j).AND.((num_zone1.GT.1).OR.(aux1_done.EQ.0)))then
      sector1=define_sector(aux_stratum(i_aux),aux_stratum_segment(i_aux
     &),face1,sect_zone1(k_zone1),sect_zone2(k_zone2))
      aux1_done=1
      end if
      end do
      end if
      end if
      end do
      end do
      end if
      end if
      end do
      end do
      return
      end
      
