      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      program ucd_plot
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      include 'mpif.h'
      integer mpi_err
      integer mpi_status
      dimension mpi_status(MPI_STATUS_SIZE)
      integer nargs,poly_nc_fileid,ext_file_unit
      character*96 polygon_nc_file,ext_file_list
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
      
      
      external get_env,wall_time,cpu_time,get_pid,arg_count,set_cwd
      logical get_env,set_cwd
      integer get_pid,arg_count
      REAL(kind=DOUBLE)cpu_time,wall_time
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer nc_dims(5),nc_corner(5),nc_edge(5),nc_stat,nc_size,nc_rank
     &,nc_attr,nc_type
      character*128 nc_dummy
      external ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf
     &_inq_format
      integer ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf_
     &inq_format
      call MPI_init(mpi_err)
      mpi_degas2_root=0
      call MPI_Comm_dup(MPI_COMM_WORLD,comm_world_dup,mpi_err)
      call MPI_comm_rank(comm_world_dup,mpi_rank,mpi_err)
      call MPI_comm_size(comm_world_dup,mpi_size,mpi_err)
      call readfilenames
      call degas_init
      call nc_read_output
      nargs=arg_count()
      if(nargs.LT.1)then
      if('Command line must specify the name of the polygon netCDF file'
     &.EQ.' ')continue
      end if
      call command_arg(1,polygon_nc_file)
      poly_nc_fileid=ncopn(polygon_nc_file,0,nc_stat)
      if(nc_stat.NE.0)then
      if('That polygon netCDF file cannot be opened!'.EQ.' ')continue
      end if
      if(nargs.EQ.2)then
      call command_arg(2,ext_file_list)
      ext_file_unit=20
      open(unit=ext_file_unit,file=ext_file_list,status='old',form='form
     &atted')
      else
      ext_file_unit=2000000000
      end if
      call make_plots(poly_nc_fileid,ext_file_unit)
      call MPI_barrier(comm_world_dup,mpi_err)
      call MPI_finalize(mpi_err)
      stop
      end
      subroutine make_plots(poly_nc_fileid,ext_file_unit)
      
      
      
      
      
      
      
      
      
      
      
      
      use sp_mod
      
      use pr_mod
      
      use tl_mod
      
      use ou_mod
      
      use bk_mod
      
      use zn_mod
      
      use de_mod
      
      use rf_mod
      
      use sc_mod
      
      use g2_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer nargs,poly_nc_fileid,ipoly,zone,num_nodes,num_points,mesh_
     &sense,i,j,this_node,sector,num_poly_points,i_this,dbfile,ret,ret2,
     &nshapetypes,ndims,n_ext_file,ext_file_unit,length,beg,e,p,iview,te
     &st,back,ifile,itxt,open_stat,ind_tmp,iv,last_plasma_polygon,dbfile
     &_td,inum,ifrag
      integer shapesize(1),shapecounts(1),shapetype(1),this_polygon(0:20
     &00-1),index_parameters(100)
      logical dbfile_td_open
      REAL(kind=DOUBLE)max_v,ha_temp,density,ha_rate,pressure,halpha_tot
     &,zone_frag
      character*96 polygon_nc_file,silo_file,zonelist,mesh_name,ext_silo
     &_file
      character*4 ivlab
      character*1 ind_sy(3)
      character*3 iflab
      character*3 vtag,auth
      character*40 vname
      character*8 clean_sy
      character*300 line,ext_var_name,ext_var_units,ext_var_format
      data ind_sy/'1','2','3'/
      external extract_output_datum
      REAL(kind=DOUBLE)extract_output_datum
      REAL(kind=DOUBLE)yhat(3)
      REAL(kind=DOUBLE)test_vec_1(3)
      REAL(kind=DOUBLE)test_vec_2(3)
      REAL(kind=DOUBLE)test_vec_3(3)
      REAL(kind=DOUBLE)node_vec(3)
      REAL(kind=DOUBLE),dimension(:),pointer::polygon_data
      REAL(kind=DOUBLE),dimension(:),pointer::zone_data
      character(len=(96)),dimension(:),pointer::ext_filenames
      REAL(kind=DOUBLE),dimension(:,:),pointer::nodes
      REAL(kind=DOUBLE),dimension(:),pointer::node_x
      REAL(kind=DOUBLE),dimension(:),pointer::node_z
      integer,dimension(:),pointer::boundary_nodes
      integer,dimension(:),pointer::poly_pointlist
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
      
      
      external get_env,wall_time,cpu_time,get_pid,arg_count,set_cwd
      logical get_env,set_cwd
      integer get_pid,arg_count
      REAL(kind=DOUBLE)cpu_time,wall_time
      REAL(kind=DOUBLE)vector_temp(3)
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
      
      integer nc_dims(5),nc_corner(5),nc_edge(5),nc_stat,nc_size,nc_rank
     &,nc_attr,nc_type
      character*128 nc_dummy
      external ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf
     &_inq_format
      integer ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf_
     &inq_format
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      logical check_tally
      integer inc
      logical check_zone
      include 'silo.inc'
      e=index(filenames_array(20),'.nc')-1
      if(e.GT.0)continue
      silo_file=filenames_array(20) (:e)//'_ucd.silo'
      ret=dbcreate(silo_file  (1:string_length(silo_file)),string_length
     &(silo_file),DB_CLOBBER,DB_LOCAL,DB_F77NULL,0,DB_PDB,dbfile)
      if((ret.NE.-1).AND.(dbfile.NE.-1))continue
      dbfile_td_open=.FALSE.
      num_nodes=0
      num_poly_points=0
      nodes =>mem_alloc_r2((1),(2),(1),(num_nodes),'nodes')
      boundary_nodes =>mem_alloc_i1((1),(num_nodes),'boundary_nodes')
      poly_pointlist =>mem_alloc_i1((1),(num_poly_points),'poly_pointlis
     &t')
      zone_data =>mem_alloc_r1((1),(zn_num),'zone_data')
      n_ext_file=0
      ext_filenames =>mem_alloc_c1((96),(1),(n_ext_file),'ext_filenames'
     &)
      if(ext_file_unit.NE.2000000000)then
90000 continue
      if(read_string(ext_file_unit,line,length))then
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,beg,e,p))continue
      n_ext_file=n_ext_file+1
      if(mod(((n_ext_file)-(1)+1),100).EQ.1)then
      ext_filenames =>mem_realloc_c1(ext_filenames,(96),(1),(((int(((((n
     &_ext_file)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((n_ext_fi
     &le)-(1)+1))+100-1)/100)*100)+(1)-1),'ext_filenames')
      end if
      ext_filenames(n_ext_file)=line(beg:e)
      go to 90000
      end if
      close(unit=ext_file_unit)
      end if
      g2_points_ind0_id=ncdid(poly_nc_fileid,'g2_points_ind0',nc_stat)
      call ncdinq(poly_nc_fileid,g2_points_ind0_id,nc_dummy,nc_size,nc_s
     &tat)
      if(nc_size.EQ.((2000-1)-(0)+1))continue
      g2_points_ind_id=ncdid(poly_nc_fileid,'g2_points_ind',nc_stat)
      call ncdinq(poly_nc_fileid,g2_points_ind_id,nc_dummy,nc_size,nc_st
     &at)
      if(nc_size.EQ.((2000)-(1)+1))continue
      g2_points_tot_ind0_id=ncdid(poly_nc_fileid,'g2_points_tot_ind0',nc
     &_stat)
      call ncdinq(poly_nc_fileid,g2_points_tot_ind0_id,nc_dummy,nc_size,
     &nc_stat)
      if(nc_size.EQ.((200000-1)-(0)+1))continue
      g2_points_tot_ind_id=ncdid(poly_nc_fileid,'g2_points_tot_ind',nc_s
     &tat)
      call ncdinq(poly_nc_fileid,g2_points_tot_ind_id,nc_dummy,nc_size,n
     &c_stat)
      if(nc_size.EQ.((200000)-(1)+1))continue
      g2_xz_ind_id=ncdid(poly_nc_fileid,'g2_xz_ind',nc_stat)
      call ncdinq(poly_nc_fileid,g2_xz_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((2)-(1)+1))continue
      g2_num_polygons_id=ncvid(poly_nc_fileid,'g2_num_polygons',nc_stat)
      call ncvinq(poly_nc_fileid,g2_num_polygons_id,nc_dummy,nc_type,nc_
     &rank,nc_dims,nc_attr,nc_stat)
      
      call ncvgt(poly_nc_fileid,g2_num_polygons_id,nc_corner,nc_edge,g2_
     &num_polygons,nc_stat)
      g2_poly_ind_id=ncdid(poly_nc_fileid,'g2_poly_ind',nc_stat)
      call ncdinq(poly_nc_fileid,g2_poly_ind_id,nc_dummy,nc_size,nc_stat
     &)
      if(nc_size.EQ.((g2_num_polygons)-(1)+1))continue
      g2_polygon_xz_id=ncvid(poly_nc_fileid,'g2_polygon_xz',nc_stat)
      call ncvinq(poly_nc_fileid,g2_polygon_xz_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.g2_xz_ind_id)continue
      if(nc_dims(2).EQ.g2_points_ind0_id)continue
      if(nc_dims(3).EQ.g2_poly_ind_id)continue
      
      g2_polygon_xz =>mem_alloc_r3((1),(2),(0),(2000-1),(1),(g2_num_poly
     &gons),'g2_polygon_xz')
      nc_corner(1)=1
      nc_edge(1)=((2)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((2000-1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((g2_num_polygons)-(1)+1)
      call ncvgt(poly_nc_fileid,g2_polygon_xz_id,nc_corner,nc_edge,g2_po
     &lygon_xz,nc_stat)
      g2_polygon_segment_id=ncvid(poly_nc_fileid,'g2_polygon_segment',nc
     &_stat)
      call ncvinq(poly_nc_fileid,g2_polygon_segment_id,nc_dummy,nc_type,
     &nc_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.g2_points_ind0_id)continue
      if(nc_dims(2).EQ.g2_poly_ind_id)continue
      
      g2_polygon_segment =>mem_alloc_i2((0),(2000-1),(1),(g2_num_polygon
     &s),'g2_polygon_segment')
      nc_corner(1)=1
      nc_edge(1)=((2000-1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((g2_num_polygons)-(1)+1)
      call ncvgt(poly_nc_fileid,g2_polygon_segment_id,nc_corner,nc_edge,
     &g2_polygon_segment,nc_stat)
      g2_polygon_points_id=ncvid(poly_nc_fileid,'g2_polygon_points',nc_s
     &tat)
      call ncvinq(poly_nc_fileid,g2_polygon_points_id,nc_dummy,nc_type,n
     &c_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.g2_poly_ind_id)continue
      
      g2_polygon_points =>mem_alloc_i1((1),(g2_num_polygons),'g2_polygon
     &_points')
      nc_corner(1)=1
      nc_edge(1)=((g2_num_polygons)-(1)+1)
      call ncvgt(poly_nc_fileid,g2_polygon_points_id,nc_corner,nc_edge,g
     &2_polygon_points,nc_stat)
      g2_polygon_zone_id=ncvid(poly_nc_fileid,'g2_polygon_zone',nc_stat)
      call ncvinq(poly_nc_fileid,g2_polygon_zone_id,nc_dummy,nc_type,nc_
     &rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.g2_poly_ind_id)continue
      
      g2_polygon_zone =>mem_alloc_i1((1),(g2_num_polygons),'g2_polygon_z
     &one')
      nc_corner(1)=1
      nc_edge(1)=((g2_num_polygons)-(1)+1)
      call ncvgt(poly_nc_fileid,g2_polygon_zone_id,nc_corner,nc_edge,g2_
     &polygon_zone,nc_stat)
      g2_polygon_stratum_id=ncvid(poly_nc_fileid,'g2_polygon_stratum',nc
     &_stat)
      call ncvinq(poly_nc_fileid,g2_polygon_stratum_id,nc_dummy,nc_type,
     &nc_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.g2_poly_ind_id)continue
      
      g2_polygon_stratum =>mem_alloc_i1((1),(g2_num_polygons),'g2_polygo
     &n_stratum')
      nc_corner(1)=1
      nc_edge(1)=((g2_num_polygons)-(1)+1)
      call ncvgt(poly_nc_fileid,g2_polygon_stratum_id,nc_corner,nc_edge,
     &g2_polygon_stratum,nc_stat)
      
      last_plasma_polygon=0
      do ipoly=1,g2_num_polygons
      zone=g2_polygon_zone(ipoly)
      if(last_plasma_polygon.EQ.0)then
      if((zone_type(zone).NE.2).AND.(zone_type(zone).NE.1))then
      last_plasma_polygon=ipoly-1
      if(last_plasma_polygon.GT.0)continue
      end if
      else
      if((zone_type(zone).NE.2).AND.(zone_type(zone).NE.1))continue
      end if
      end do
      g2_num_polygons=last_plasma_polygon
      polygon_data =>mem_alloc_r1((1),(g2_num_polygons),'polygon_data')
      yhat(1)=(0.0_DOUBLE)
      yhat(2)=(1.0_DOUBLE)
      yhat(3)=(0.0_DOUBLE)
      do ipoly=1,g2_num_polygons
      test_vec_1(1)=g2_polygon_xz(1,1,ipoly)-g2_polygon_xz(1,0,ipoly)
      test_vec_1(2)=(0.0_DOUBLE)
      test_vec_1(3)=g2_polygon_xz(2,1,ipoly)-g2_polygon_xz(2,0,ipoly)
      test_vec_2(1)=g2_polygon_xz(1,2,ipoly)-g2_polygon_xz(1,1,ipoly)
      test_vec_2(2)=(0.0_DOUBLE)
      test_vec_2(3)=g2_polygon_xz(2,2,ipoly)-g2_polygon_xz(2,1,ipoly)
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
      if('Mesh cell degenerate'.EQ.' ')continue
      end if
      num_points=g2_polygon_points(ipoly)
      if((g2_polygon_xz(1,num_points-1,ipoly).EQ.g2_polygon_xz(1,num_poi
     &nts,ipoly)).AND.(g2_polygon_xz(2,num_points-1,ipoly).EQ.g2_polygon
     &_xz(2,num_points,ipoly)))num_points=num_points-1
      do i=0,num_points-1
      this_node=0
      if(num_nodes.GT.0)then
      do j=1,num_nodes
      if((nodes(1,j).EQ.g2_polygon_xz(1,i,ipoly)).AND.(nodes(2,j).EQ.g2_
     &polygon_xz(2,i,ipoly)))then
      this_node=j
      end if
      end do
      end if
      if(this_node.EQ.0)then
      num_nodes=num_nodes+1
      if(mod(((num_nodes)-(1)+1),100).EQ.1)then
      nodes =>mem_realloc_r2(nodes,(1),(2),(1),(((int(((((num_nodes)-(1)
     &+1))+100-1)/100)*100)-100)+(1)-1),((int(((((num_nodes)-(1)+1))+100
     &-1)/100)*100)+(1)-1),'nodes')
      end if
      if(mod(((num_nodes)-(1)+1),100).EQ.1)then
      boundary_nodes =>mem_realloc_i1(boundary_nodes,(1),(((int(((((num_
     &nodes)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((num_nodes)-(
     &1)+1))+100-1)/100)*100)+(1)-1),'boundary_nodes')
      end if
      nodes(1,num_nodes)=g2_polygon_xz(1,i,ipoly)
      nodes(2,num_nodes)=g2_polygon_xz(2,i,ipoly)
      node_vec(1)=nodes(1,num_nodes)
      node_vec(2)=(0.0_DOUBLE)
      node_vec(3)=nodes(2,num_nodes)
      boundary_nodes(num_nodes)=0
      do sector=1,nsectors
      if((((sector_type_pointer(3,sector).GT.0.AND.sector_type_pointer(3
     &,sector).LE.sc_target_num)).OR.((sector_type_pointer(4,sector).GT.
     &0.AND.sector_type_pointer(4,sector).LE.sc_wall_num)).OR.((sector_t
     &ype_pointer(5,sector).GT.0.AND.sector_type_pointer(5,sector).LE.sc
     &_exit_num))).AND.(((node_vec(1).EQ.sector_points(1,0,sector).AND.n
     &ode_vec(2).EQ.sector_points(2,0,sector).AND.node_vec(3).EQ.sector_
     &points(3,0,sector))).OR.((node_vec(1).EQ.sector_points(1,1,sector)
     &.AND.node_vec(2).EQ.sector_points(2,1,sector).AND.node_vec(3).EQ.s
     &ector_points(3,1,sector)))))then
      boundary_nodes(num_nodes)=1
      end if
      end do
      this_node=num_nodes
      end if
      this_polygon(i)=this_node
      end do
      num_poly_points=num_poly_points+1
      if(mod(((num_poly_points)-(1)+1),100).EQ.1)then
      poly_pointlist =>mem_realloc_i1(poly_pointlist,(1),(((int(((((num_
     &poly_points)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((num_po
     &ly_points)-(1)+1))+100-1)/100)*100)+(1)-1),'poly_pointlist')
      end if
      poly_pointlist(num_poly_points)=num_points
      do i=0,num_points-1
      num_poly_points=num_poly_points+1
      if(mod(((num_poly_points)-(1)+1),100).EQ.1)then
      poly_pointlist =>mem_realloc_i1(poly_pointlist,(1),(((int(((((num_
     &poly_points)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((num_po
     &ly_points)-(1)+1))+100-1)/100)*100)+(1)-1),'poly_pointlist')
      end if
      if(mesh_sense.EQ.1)then
      i_this=num_points-1-i
      else
      i_this=i
      end if
      poly_pointlist(num_poly_points)=this_polygon(i_this)
      end do
      end do
      if(mod(((num_nodes)-(1)+1),100).NE.0)then
      nodes =>mem_realloc_r2(nodes,(1),(2),(1),((int(((((num_nodes)-(1)+
     &1))+100-1)/100)*100)+(1)-1),(num_nodes),'nodes')
      end if
      if(mod(((num_nodes)-(1)+1),100).NE.0)then
      boundary_nodes =>mem_realloc_i1(boundary_nodes,(1),((int(((((num_n
     &odes)-(1)+1))+100-1)/100)*100)+(1)-1),(num_nodes),'boundary_nodes'
     &)
      end if
      if(mod(((num_poly_points)-(1)+1),100).NE.0)then
      poly_pointlist =>mem_realloc_i1(poly_pointlist,(1),((int(((((num_p
     &oly_points)-(1)+1))+100-1)/100)*100)+(1)-1),(num_poly_points),'pol
     &y_pointlist')
      end if
      nshapetypes=1
      ndims=2
      shapesize(1)=0
      shapecounts(1)=g2_num_polygons
      shapetype(1)=DB_ZONETYPE_POLYGON
      zonelist='polygon_zonelist'
      ret=dbputzl2(dbfile,zonelist  (1:string_length(zonelist)),string_l
     &ength(zonelist),g2_num_polygons,ndims,poly_pointlist,num_poly_poin
     &ts,1,0,0,shapetype,shapesize,shapecounts,nshapetypes,DB_F77NULL,re
     &t2)
      if((ret.NE.-1).AND.(ret2.NE.-1))continue
      mesh_name='polygon_mesh'
      node_x =>mem_alloc_r1((1),(num_nodes),'node_x')
      node_z =>mem_alloc_r1((1),(num_nodes),'node_z')
      do i=1,num_nodes
      node_x(i)=nodes(1,i)
      node_z(i)=nodes(2,i)
      end do
      ret=dbputum(dbfile,mesh_name  (1:string_length(mesh_name)),string_
     &length(mesh_name),ndims,node_x,node_z,DB_F77NULL,"x",1,"z",1,DB_F7
     &7NULLSTRING,0,DB_DOUBLE,num_nodes,g2_num_polygons,zonelist  (1:str
     &ing_length(zonelist)),string_length(zonelist),DB_F77NULLSTRING,0,D
     &B_F77NULL,ret2)
      if((ret.NE.-1).AND.(ret2.NE.-1))continue
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      polygon_data(i)=REAL(3+mod(97*zone,255-3),DOUBLE)
      end do
      call write_silo_data('zone_function',polygon_data,' ','E11.3',dbfi
     &le,mesh_name)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      polygon_data(i)=REAL(zone,DOUBLE)
      end do
      call write_silo_data('zone_number',polygon_data,' ','E11.3',dbfile
     &,mesh_name)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      polygon_data(i)=zone_volume(zone)
      end do
      call write_silo_data('zone_volume',polygon_data,'m**3','E11.3',dbf
     &ile,mesh_name)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      polygon_data(i)=REAL(zone_type(zone),DOUBLE)
      end do
      call write_silo_data('zone_type',polygon_data,' ','E11.3',dbfile,m
     &esh_name)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2)then
      polygon_data(i)=background_n(1,zone_pointer(zone))
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      call write_silo_data('electron_density',polygon_data,'m**-3','E11.
     &3',dbfile,mesh_name)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2)then
      polygon_data(i)=background_n(2,zone_pointer(zone))
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      call write_silo_data('ion_density',polygon_data,'m**-3','E11.3',db
     &file,mesh_name)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2)then
      polygon_data(i)=background_temp(1,zone_pointer(zone))/(1.60217733e
     &-19_DOUBLE)
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      call write_silo_data('electron_temperature',polygon_data,'eV','E11
     &.3',dbfile,mesh_name)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2)then
      polygon_data(i)=background_temp(2,zone_pointer(zone))/(1.60217733e
     &-19_DOUBLE)
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      call write_silo_data('ion_temperature',polygon_data,'eV','E11.3',d
     &bfile,mesh_name)
      do j=1,3
      max_v=(0.0_DOUBLE)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2)then
      polygon_data(i)=background_v(j,2,zone_pointer(zone))
      max_v=max(max_v,abs(polygon_data(i)))
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      if(max_v.GT.(0.0_DOUBLE))then
      call write_silo_data('ion_velocity_'//ind_sy(j),polygon_data,'m/s'
     &,'E11.3',dbfile,mesh_name)
      end if
      end do
      if(output_old_file.EQ.1)continue
      do j=2,pr_test_num
      index_parameters(3)=j
      ha_temp=(0.0_DOUBLE)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2.OR.zone_type(zone).EQ.1)then
      index_parameters(1)=zone
      density=extract_output_datum(index_parameters,1,out_post_all,0,'ne
     &utral density')
      polygon_data(i)=density
      ha_temp=ha_temp+(density*zone_volume(zone))
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      clean_sy=species_sy(problem_test_sp(j))
      ind_tmp=index(clean_sy,'(')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,')')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,'|')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,'+')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='p'
      call write_silo_data('sp'//clean_sy  (1:string_length(clean_sy))//
     &'_density',polygon_data,'m**-3','E11.3',dbfile,mesh_name)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2.OR.zone_type(zone).EQ.1)then
      index_parameters(1)=zone
      density=extract_output_datum(index_parameters,1,output_all,1,'neut
     &ral density')
      polygon_data(i)=density
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      call write_silo_data('sp'//clean_sy  (1:string_length(clean_sy))//
     &'_density_rsd',polygon_data,' ','E11.3',dbfile,mesh_name)
      if(string_lookup('neutral density - snapshot',tally_name,tl_num).G
     &T.0)then
      ha_temp=(0.0_DOUBLE)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2.OR.zone_type(zone).EQ.1)then
      index_parameters(1)=zone
      density=extract_output_datum(index_parameters,1,out_post_all,0,'ne
     &utral density - snapshot')
      polygon_data(i)=density
      ha_temp=ha_temp+(density*zone_volume(zone))
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      clean_sy=species_sy(problem_test_sp(j))
      ind_tmp=index(clean_sy,'(')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,')')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,'|')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,'+')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='p'
      call write_silo_data('sp'//clean_sy  (1:string_length(clean_sy))//
     &'_density_snap',polygon_data,'m**-3','E11.3',dbfile,mesh_name)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2.OR.zone_type(zone).EQ.1)then
      index_parameters(1)=zone
      density=extract_output_datum(index_parameters,1,output_all,1,'neut
     &ral density - snapshot')
      polygon_data(i)=density
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      call write_silo_data('sp'//clean_sy  (1:string_length(clean_sy))//
     &'_density_snap_rsd',polygon_data,' ','E11.3',dbfile,mesh_name)
      end if
      if(check_tally(string_lookup('neutral velocity vector',tally_name,
     &tl_num)))then
      vname='neutral velocity vector'
      vtag='vel'
      else
      if(check_tally(string_lookup('neutral flux vector',tally_name,tl_n
     &um)))continue
      vname='neutral flux vector'
      vtag='flx'
      end if
      do iv=1,3
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2.OR.zone_type(zone).EQ.1)then
      index_parameters(1)=zone
      ha_rate=extract_output_datum(index_parameters,iv,out_post_all,0,vn
     &ame  (1:string_length(vname)))
      polygon_data(i)=ha_rate
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      call write_silo_data(clean_sy  (1:string_length(clean_sy))//vtag//
     &ind_sy(iv),polygon_data,'m/s','E11.3',dbfile,mesh_name)
      end do
      if(string_lookup('neutral pressure',tally_name,tl_num).GT.0)then
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2.OR.zone_type(zone).EQ.1)then
      index_parameters(1)=zone
      pressure=extract_output_datum(index_parameters,1,out_post_all,0,'n
     &eutral pressure')
      polygon_data(i)=pressure/(1.3332e-1_DOUBLE)
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      call write_silo_data('sp'//clean_sy  (1:string_length(clean_sy))//
     &'_pressure',polygon_data,'mTorr','E11.3',dbfile,mesh_name)
      end if
      end do
      do i=1,g2_num_polygons
      polygon_data(i)=(0.0_DOUBLE)
      end do
      do test=1,pr_test_num
      if(((species_sy(problem_test_sp(test)).EQ.'H').OR.(species_sy(prob
     &lem_test_sp(test)).EQ.'D').OR.(species_sy(problem_test_sp(test)).E
     &Q.'T')).AND.string_lookup(species_sy(problem_test_sp(test)) (1:str
     &ing_length(species_sy(problem_test_sp(test))))//'alpha emission ra
     &te',tally_name,tl_num).GT.0)then
      ha_temp=(0.0_DOUBLE)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2)then
      index_parameters(1)=zone
      ha_rate=extract_output_datum(index_parameters,1,out_post_all,0,spe
     &cies_sy(problem_test_sp(test)) (1:string_length(species_sy(problem
     &_test_sp(test))))//'alpha emission rate')/((1.8881944_DOUBLE)*(1.6
     &0217733e-19_DOUBLE))
      polygon_data(i)=polygon_data(i)+(ha_rate)
      ha_temp=ha_temp+(ha_rate*zone_volume(zone))
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      end if
      end do
      halpha_tot=(0.0_DOUBLE)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      halpha_tot=halpha_tot+(polygon_data(i)*zone_volume(zone))
      end do
      if(halpha_tot.GT.(0.0_DOUBLE))then
      call write_silo_data('H_alpha_rate',polygon_data,'photons / (m**3 
     &s)','E11.3',dbfile,mesh_name)
      end if
      do i=1,g2_num_polygons
      polygon_data(i)=(0.0_DOUBLE)
      end do
      do test=1,pr_test_num
      if(string_lookup('Dalpha emission rate by species',tally_name,tl_n
     &um).GT.0)then
      clean_sy=species_sy(problem_test_sp(test))
      ind_tmp=index(clean_sy,'(')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,')')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,'|')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,'+')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='p'
      ha_temp=(0.0_DOUBLE)
      index_parameters(3)=test
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2)then
      index_parameters(1)=zone
      ha_rate=extract_output_datum(index_parameters,1,out_post_all,0,'Da
     &lpha emission rate by species')/((1.8881944_DOUBLE)*(1.60217733e-1
     &9_DOUBLE))
      polygon_data(i)=ha_rate
      ha_temp=ha_temp+(ha_rate*zone_volume(zone))
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      end if
      halpha_tot=(0.0_DOUBLE)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      halpha_tot=halpha_tot+(polygon_data(i)*zone_volume(zone))
      end do
      if(halpha_tot.GT.(0.0_DOUBLE))then
      call write_silo_data('H_alpha_rate_'//clean_sy  (1:string_length(c
     &lean_sy)),polygon_data,'photons / (m**3 s)','E11.3',dbfile,mesh_na
     &me)
      end if
      end do
      do i=1,g2_num_polygons
      polygon_data(i)=(0.0_DOUBLE)
      end do
      do test=1,pr_test_num
      if(((species_sy(problem_test_sp(test)).EQ.'H').OR.(species_sy(prob
     &lem_test_sp(test)).EQ.'D').OR.(species_sy(problem_test_sp(test)).E
     &Q.'T')).AND.string_lookup('D beta emission rate',tally_name,tl_num
     &).GT.0)then
      ha_temp=(0.0_DOUBLE)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2)then
      index_parameters(1)=zone
      ha_rate=extract_output_datum(index_parameters,1,out_post_all,0,'D 
     &beta emission rate')/((2.549_DOUBLE)*(1.60217733e-19_DOUBLE))
      polygon_data(i)=polygon_data(i)+(ha_rate)
      ha_temp=ha_temp+(ha_rate*zone_volume(zone))
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      end if
      end do
      halpha_tot=(0.0_DOUBLE)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      halpha_tot=halpha_tot+(polygon_data(i)*zone_volume(zone))
      end do
      if(halpha_tot.GT.(0.0_DOUBLE))then
      call write_silo_data('Balmer_beta_rate',polygon_data,'photons / (m
     &**3 s)','E11.3',dbfile,mesh_name)
      end if
      do i=1,g2_num_polygons
      polygon_data(i)=(0.0_DOUBLE)
      end do
      do test=1,pr_test_num
      if(string_lookup('D beta emission rate by species',tally_name,tl_n
     &um).GT.0)then
      clean_sy=species_sy(problem_test_sp(test))
      ind_tmp=index(clean_sy,'(')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,')')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,'|')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,'+')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='p'
      ha_temp=(0.0_DOUBLE)
      index_parameters(3)=test
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2)then
      index_parameters(1)=zone
      ha_rate=extract_output_datum(index_parameters,1,out_post_all,0,'D 
     &beta emission rate by species')/((2.549_DOUBLE)*(1.60217733e-19_DO
     &UBLE))
      polygon_data(i)=ha_rate
      ha_temp=ha_temp+(ha_rate*zone_volume(zone))
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      end if
      halpha_tot=(0.0_DOUBLE)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      halpha_tot=halpha_tot+(polygon_data(i)*zone_volume(zone))
      end do
      if(halpha_tot.GT.(0.0_DOUBLE))then
      call write_silo_data('Balmer_beta_rate_'//clean_sy  (1:string_leng
     &th(clean_sy)),polygon_data,'photons / (m**3 s)','E11.3',dbfile,mes
     &h_name)
      end if
      end do
      do i=1,g2_num_polygons
      polygon_data(i)=(0.0_DOUBLE)
      end do
      do test=1,pr_test_num
      if(((species_sy(problem_test_sp(test)).EQ.'He').AND.string_lookup(
     &'He 5877 emission rate',tally_name,tl_num).GT.0))then
      ha_temp=(0.0_DOUBLE)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2)then
      index_parameters(1)=zone
      ha_rate=extract_output_datum(index_parameters,1,out_post_all,0,'He
     & 5877 emission rate')/((2.109566_DOUBLE)*(1.60217733e-19_DOUBLE))
      polygon_data(i)=polygon_data(i)+(ha_rate)
      ha_temp=ha_temp+(ha_rate*zone_volume(zone))
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      end if
      end do
      halpha_tot=(0.0_DOUBLE)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      halpha_tot=halpha_tot+(polygon_data(i)*zone_volume(zone))
      end do
      if(halpha_tot.GT.(0.0_DOUBLE))then
      call write_silo_data('He_5877_rate',polygon_data,'photons / (m**3 
     &s)','E11.3',dbfile,mesh_name)
      end if
      if((pr_reaction_num.GT.0).AND.(string_lookup('ion source rate',tal
     &ly_name,tl_num).GT.0))then
      do back=1,pr_background_num
      clean_sy=species_sy(problem_background_sp(back))
      ind_tmp=index(clean_sy,'(')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,')')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,'|')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,'+')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='p'
      if(clean_sy  (1:string_length(clean_sy)).NE.'e')then
      index_parameters(4)=back
      ha_temp=(0.0_DOUBLE)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2)then
      index_parameters(1)=zone
      ha_rate=extract_output_datum(index_parameters,1,out_post_all,0,'io
     &n source rate')
      polygon_data(i)=ha_rate/zone_volume(zone)
      ha_temp=ha_temp+(ha_rate)
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      call write_silo_data(clean_sy  (1:string_length(clean_sy))//'_Ion_
     &Source_rate',polygon_data,'m**-3 s**-1','E11.3',dbfile,mesh_name)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2)then
      index_parameters(1)=zone
      ha_rate=extract_output_datum(index_parameters,1,output_all,1,'ion 
     &source rate')
      polygon_data(i)=ha_rate
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      call write_silo_data(clean_sy  (1:string_length(clean_sy))//'_Ion_
     &Source_rate_rsd',polygon_data,' ','E11.3',dbfile,mesh_name)
      end if
      end do
      do back=1,pr_background_num
      clean_sy=species_sy(problem_background_sp(back))
      ind_tmp=index(clean_sy,'(')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,')')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,'|')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,'+')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='p'
      if(clean_sy  (1:string_length(clean_sy)).NE.'e')then
      index_parameters(4)=back
      do iv=1,3
      ha_temp=(0.0_DOUBLE)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2)then
      index_parameters(1)=zone
      ha_rate=extract_output_datum(index_parameters,iv,out_post_all,0,'i
     &on momentum source vector')
      polygon_data(i)=ha_rate/zone_volume(zone)
      ha_temp=ha_temp+(ha_rate)
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      call write_silo_data(clean_sy  (1:string_length(clean_sy))//'_Mom_
     &'//ind_sy(iv)//'_Source_rate',polygon_data,'N m**-3','E11.3',dbfil
     &e,mesh_name)
      end do
      do iv=1,3
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2)then
      index_parameters(1)=zone
      ha_rate=extract_output_datum(index_parameters,iv,output_all,1,'ion
     & momentum source vector')
      polygon_data(i)=ha_rate
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      call write_silo_data(clean_sy  (1:string_length(clean_sy))//'_Mom_
     &'//ind_sy(iv)//'_Source_rate_rsd',polygon_data,' ','E11.3',dbfile,
     &mesh_name)
      end do
      end if
      end do
      do back=1,pr_background_num
      clean_sy=species_sy(problem_background_sp(back))
      ind_tmp=index(clean_sy,'(')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,')')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,'|')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='_'
      ind_tmp=index(clean_sy,'+')
      if(ind_tmp.GT.0)clean_sy(ind_tmp:ind_tmp)='p'
      index_parameters(4)=back
      ha_temp=(0.0_DOUBLE)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2)then
      index_parameters(1)=zone
      ha_rate=extract_output_datum(index_parameters,1,out_post_all,0,'io
     &n energy source')
      polygon_data(i)=ha_rate/zone_volume(zone)
      ha_temp=ha_temp+(ha_rate)
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      call write_silo_data(clean_sy  (1:string_length(clean_sy))//'_Ener
     &gy_Source_rate',polygon_data,'W m**-3','E11.3',dbfile,mesh_name)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      if(zone_type(zone).EQ.2)then
      index_parameters(1)=zone
      ha_rate=extract_output_datum(index_parameters,1,output_all,1,'ion 
     &energy source')
      polygon_data(i)=ha_rate
      else
      polygon_data(i)=(0.0_DOUBLE)
      end if
      end do
      call write_silo_data(clean_sy  (1:string_length(clean_sy))//'_Ener
     &gy_Source_rate_rsd',polygon_data,' ','E11.3',dbfile,mesh_name)
      end do
      end if
      if(n_ext_file.GT.0)then
      do ifile=1,n_ext_file
      do i=1,zn_num
      zone_data(i)=(0.0_DOUBLE)
      end do
      open(unit=20,file=ext_filenames(ifile),status='old',form='formatte
     &d',iostat=open_stat)
      if(open_stat.EQ.0)then
      if(ifile.LT.1000)continue
      write(iflab,'(i3.3)')ifile
      ext_var_name='ext_var'//iflab
      ext_var_units=' '
      ext_var_format='E11.3'
90002 continue
      if(read_string(20,line,length))then
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,beg,e,p))continue
      if(line(beg:e).EQ.'name')then
      if(next_token(line,beg,e,p))continue
      ext_var_name=line(beg:e)
      else if(line(beg:e).EQ.'units')then
      if(next_token(line,beg,e,p))continue
      ext_var_units=line(beg:e)
      else if(line(beg:e).EQ.'format')then
      if(next_token(line,beg,e,p))continue
      ext_var_format=line(beg:e)
      else
      zone=read_int_soft_fail(line(beg:e))
      if(check_zone(zone))then
      if(next_token(line,beg,e,p))continue
      zone_data(zone)=read_real(line(beg:e))
      else
      write(0,*)'Improper zone number ',zone,' in external file ',ext_fi
     &lenames(ifile)
      end if
      end if
      go to 90002
      end if
      close(unit=20)
      do i=1,g2_num_polygons
      zone=g2_polygon_zone(i)
      polygon_data(i)=zone_data(zone)
      end do
      i=index(ext_var_name,'$')
      if(i.GT.0)then
      if(dbfile_td_open)then
      ret=dbclose(dbfile_td)
      dbfile_td_open=.FALSE.
      if(ret.NE.-1)continue
      end if
      itxt=index(ext_filenames(ifile),'.txt')
      ext_silo_file=ext_filenames(ifile) (:itxt)//'silo'
      ret=dbcreate(ext_silo_file  (1:string_length(ext_silo_file)),strin
     &g_length(ext_silo_file),DB_CLOBBER,DB_LOCAL,DB_F77NULL,0,DB_PDB,db
     &file_td)
      if((ret.NE.-1).AND.(dbfile_td.NE.-1))continue
      dbfile_td_open=.TRUE.
      ret=dbputzl2(dbfile_td,zonelist  (1:string_length(zonelist)),strin
     &g_length(zonelist),g2_num_polygons,ndims,poly_pointlist,num_poly_p
     &oints,1,0,0,shapetype,shapesize,shapecounts,nshapetypes,DB_F77NULL
     &,ret2)
      if((ret.NE.-1).AND.(ret2.NE.-1))continue
      ret=dbputum(dbfile_td,mesh_name  (1:string_length(mesh_name)),stri
     &ng_length(mesh_name),ndims,node_x,node_z,DB_F77NULL,"x",1,"z",1,DB
     &_F77NULLSTRING,0,DB_DOUBLE,num_nodes,g2_num_polygons,zonelist  (1:
     &string_length(zonelist)),string_length(zonelist),DB_F77NULLSTRING,
     &0,DB_F77NULL,ret2)
      if((ret.NE.-1).AND.(ret2.NE.-1))continue
      call write_silo_data(ext_var_name  (1:string_length(ext_var_name))
     &,polygon_data,ext_var_units  (1:string_length(ext_var_units)),ext_
     &var_format  (1:string_length(ext_var_format)),dbfile_td,mesh_name)
      else
      call write_silo_data(ext_var_name  (1:string_length(ext_var_name))
     &,polygon_data,ext_var_units  (1:string_length(ext_var_units)),ext_
     &var_format  (1:string_length(ext_var_format)),dbfile,mesh_name)
      end if
      else
      write(0,*)'Cannot open external file ',ext_filenames(ifile),', err
     &or number ',open_stat
      end if
      end do
      end if
      ret=dbclose(dbfile)
      if(ret.NE.-1)continue
      if(dbfile_td_open)then
      ret=dbclose(dbfile_td)
      if(ret.NE.-1)continue
      end if
      call mem_free_r2(nodes,(1),(2),(1),(num_nodes),'nodes')
      call mem_free_r1(node_x,(1),(num_nodes),'node_x')
      call mem_free_r1(node_z,(1),(num_nodes),'node_z')
      call mem_free_i1(boundary_nodes,(1),(num_nodes),'boundary_nodes')
      call mem_free_i1(poly_pointlist,(1),(num_poly_points),'poly_pointl
     &ist')
      call mem_free_r1(polygon_data,(1),(g2_num_polygons),'polygon_data'
     &)
      call mem_free_r1(zone_data,(1),(zn_num),'zone_data')
      return
      end
      subroutine write_silo_data(var_name,polygon_data,cunits,cformt,dbf
     &ile,mesh_name)
      
      use g2_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer dbfile
      character*(*)var_name,cunits,cformt,mesh_name
      REAL(kind=DOUBLE)polygon_data(g2_num_polygons)
      REAL(kind=DOUBLE)time_slice
      integer ret,ret2,var_opts,time_ind
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      include 'silo.inc'
      ret=dbmkoptlist(1,var_opts)
      if(ret.NE.-1)continue
      if(len(cunits).GT.0)then
      ret=dbaddcopt(var_opts,DBOPT_UNITS,cunits,len(cunits))
      if(ret.NE.-1)continue
      end if
      time_ind=index(var_name,'$')
      if(time_ind.GT.0)then
      time_slice=REAL(read_integer(var_name(time_ind+1:)),DOUBLE)
      ret=dbadddopt(var_opts,DBOPT_DTIME,time_slice)
      if(ret.NE.-1)continue
      var_name=var_name(:time_ind-1)
      end if
      ret=dbputuv1(dbfile,var_name  (1:string_length(var_name)),string_l
     &ength(var_name),mesh_name  (1:string_length(mesh_name)),string_len
     &gth(mesh_name),polygon_data,g2_num_polygons,DB_F77NULL,0,DB_DOUBLE
     &,DB_ZONECENT,var_opts,ret2)
      if((ret.NE.-1).AND.(ret2.NE.-1))continue
      return
      end
      
      
