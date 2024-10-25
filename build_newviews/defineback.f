      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      program defineback
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      include 'mpif.h'
      integer mpi_err
      integer mpi_status
      dimension mpi_status(MPI_STATUS_SIZE)
      integer nargs,geom_modified
      character*96 inputfile
      external get_env,wall_time,cpu_time,get_pid,arg_count,set_cwd
      logical get_env,set_cwd
      integer get_pid,arg_count
      REAL(kind=DOUBLE)cpu_time,wall_time
      call MPI_init(mpi_err)
      mpi_degas2_root=0
      call MPI_Comm_dup(MPI_COMM_WORLD,comm_world_dup,mpi_err)
      call MPI_comm_rank(comm_world_dup,mpi_rank,mpi_err)
      call MPI_comm_size(comm_world_dup,mpi_size,mpi_err)
      nargs=arg_count()
      if(nargs.NE.1)then
      if(' Command line must specify an input file'.EQ.' ')continue
      end if
      call command_arg(1,inputfile)
      call readfilenames
      call read_geometry
      call nc_read_elements
      call nc_read_species
      call nc_read_reactions
      call nc_read_materials
      call nc_read_pmi
      call nc_read_problem
      call setup_back_arrays(geom_modified)
      call setup_background(inputfile)
      if(geom_modified.EQ.1)call write_geometry
      call erase_geometry
      call MPI_barrier(comm_world_dup,mpi_err)
      call MPI_finalize(mpi_err)
      stop
      end
      subroutine setup_background(inputfile)
      
      
      
      
      
      
      
      
      
      use gi_mod
      
      use zn_mod
      
      use bk_mod
      
      use pr_mod
      
      use so_mod
      
      use sc_mod
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*96 inputfile
      REAL(kind=DOUBLE)density,temperature,dummy,circum,area,rtest,real_
     &mult,mult,puff_temp,puff_exponent,e_bin_min,e_bin_max,e_bin_spacin
     &g,total,cumul
      integer type,geom,nflights,length,p,b,e,nunit_n,specify_flux,nunit
     &_t,num_segments,iseg,i,open_stat,diskin2,dim_segments,itest,data_t
     &ype,data_code,update,old_grps,old_seg_tot,grp,iterative,jr,iparam,
     &time_varn,standalone,e_bin_num,i_bin,i_code,e_bin_entry
      character*1 back_label
      character*300 line,type_string,nt_string,file_format
      character*96 source_file
      external phi_sheath
      REAL(kind=DOUBLE)phi_sheath
      integer species
      integer root_sp
      integer back
      integer zone
      integer sector
      REAL(kind=DOUBLE)xdiff(3)
      REAL(kind=DOUBLE)targ_v(3)
      integer,dimension(:,:),pointer::source_int_params
      REAL(kind=DOUBLE),dimension(:,:),pointer::source_real_params
      REAL(kind=DOUBLE),dimension(:),pointer::old_current
      REAL(kind=DOUBLE),dimension(:),pointer::old_rel_wt
      REAL(kind=DOUBLE),dimension(:),pointer::old_tot_curr
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
      open(unit=20,file=inputfile,status='old',form='formatted',iostat=o
     &pen_stat)
      if(open_stat.EQ.0)continue
      call nc_read_old_sources(update)
      if(update.EQ.0)then
      so_restart=0
      so_seed_decimal='12'
      so_spaced_seeds=0
      so_seed_spacing=100000000
      so_sampling=0
      so_time_dependent=0
      so_time_initialization=0
      so_time_initial=(0.0_DOUBLE)
      so_time_final=(0.0_DOUBLE)
      source_name(1)='plate'
      source_name(2)='puff'
      source_name(3)='recomb'
      source_name(4)='vol_source'
      source_name(5)='snapshot'
      source_name(6)='plt_e_bins'
      so_rel_wt_min=(5.e-1_DOUBLE)
      so_rel_wt_max=(2._DOUBLE)
      so_wt_norm_min=(5.e-1_DOUBLE)
      so_wt_norm_max=(2._DOUBLE)
      so_grps=0
      so_seg_tot=0
      so_gparams_list_size=0
      so_gparams_list_dim=1
      so_params_list_size=0
      so_params_list_dim=1
      so_params_data_size=0
      so_params_data_dim=1
      so_giparams_list_size=0
      so_giparams_list_dim=1
      so_iparams_list_size=0
      so_iparams_list_dim=1
      so_iparams_data_size=0
      so_iparams_data_dim=1
      source_gparameters_list =>mem_alloc_i1((1),(so_gparams_list_dim),'
     &source_gparameters_list')
      source_parameters_list =>mem_alloc_i1((1),(so_params_list_dim),'so
     &urce_parameters_list')
      source_gparameters_data =>mem_alloc_r1((1),(so_gparams_list_dim),'
     &source_gparameters_data')
      source_parameters_data =>mem_alloc_r1((1),(so_params_data_dim),'so
     &urce_parameters_data')
      source_giparameters_list =>mem_alloc_i1((1),(so_giparams_list_dim)
     &,'source_giparameters_list')
      source_iparameters_list =>mem_alloc_i1((1),(so_iparams_list_dim),'
     &source_iparameters_list')
      source_giparameters_data =>mem_alloc_i1((1),(so_giparams_list_dim)
     &,'source_giparameters_data')
      source_iparameters_data =>mem_alloc_i1((1),(so_iparams_data_dim),'
     &source_iparameters_data')
      else
      if(so_grps.GT.0)continue
      if(so_seg_tot.GT.0)continue
      old_current =>mem_alloc_r1((1),(so_seg_tot),'old_current')
      old_rel_wt =>mem_alloc_r1((1),(so_seg_tot),'old_rel_wt')
      old_tot_curr =>mem_alloc_r1((1),(so_grps),'old_tot_curr')
      old_grps=so_grps
      old_seg_tot=so_seg_tot
      do grp=1,so_grps
      old_tot_curr(grp)=source_total_current(grp)
      end do
      do iseg=1,so_seg_tot
      old_current(iseg)=source_current(iseg)
      old_rel_wt(iseg)=source_segment_rel_wt(iseg)
      end do
      end if
      dim_segments=0
      iterative=0
      if((geometry_symmetry.EQ.1).OR.(geometry_symmetry.EQ.4).OR.(geomet
     &ry_symmetry.EQ.3))then
      background_coords=1
      else if((geometry_symmetry.EQ.2).OR.(geometry_symmetry.EQ.5).OR.(g
     &eometry_symmetry.EQ.6))then
      background_coords=2
      else
      if('Unexpected value of geometry_symmetry'.EQ.' ')continue
      end if
90027 continue
      if(.NOT.read_string(20,line,length))then
      close(unit=20)
      goto 90014
      else
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.'plasma_file')then
      if(next_token(line,b,e,p))continue
      nt_string=line(b:)
      if(next_token(line,b,e,p))then
      file_format=line(b:e)
      else
      file_format='undefined'
      end if
      if(file_format.EQ.'row')then
      call get_n_t_row(nt_string)
      else
      if(update.EQ.1)then
      if(iterative.EQ.1)continue
      end if
      call get_n_t(nt_string)
      end if
      else if(line(b:e).EQ.'plasma_coords')then
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.'cartesian')then
      background_coords=1
      else if(line(b:e).EQ.'cylindrical')then
      background_coords=2
      else
      if(' Illegal value of plasma_coords'.EQ.' ')continue
      end if
      else if(line(b:e).EQ.'iterative_run')then
      iterative=1
      else if(line(b:e).EQ.'time_interval')then
      if(next_token(line,b,e,p))continue
      so_time_initial=read_real(line(b:e))
      if(next_token(line,b,e,p))continue
      so_time_final=read_real(line(b:e))
      if(so_time_final.GT.so_time_initial)then
      so_time_dependent=1
      else
      so_time_dependent=0
      end if
      else if(line(b:e).EQ.'time_initialization')then
      so_time_initialization=1
      else if(line(b:e).EQ.'new_source_group')then
      so_grps=so_grps+1
      type=1000000000
      geom=1000000000
      species=1000000000
      root_sp=1000000000
      puff_temp=(3.e2_DOUBLE)*(1.380658e-23_DOUBLE)
      puff_exponent=(1.0_DOUBLE)
      e_bin_num=4
      e_bin_min=(1.0_DOUBLE)*(1.60217733e-19_DOUBLE)
      e_bin_max=(1.e4_DOUBLE)*(1.60217733e-19_DOUBLE)
      e_bin_spacing=2
      nflights=100
      specify_flux=1
      num_segments=314159265
      time_varn=0
      
      if(dim_segments.GT.0)then
      do iseg=1,dim_segments
      do i=1,6
      source_int_params(i,iseg)=314159265
      end do
      source_int_params(6,iseg)=0
      source_int_params(5,iseg)=0
      do i=11,24+15-1
      source_real_params(i,iseg)=(314159265.3589793238462_DOUBLE)
      end do
      end do
      end if
      else if(line(b:e).EQ.'source_type')then
      if(next_token(line,b,e,p))continue
      type_string=line(b:e)
      if(type.EQ.1000000000)continue
      do i=1,6
      if(type_string.EQ.source_name(i))then
      type=i
      end if
      end do
      if(type.NE.1000000000)continue
      else if(line(b:e).EQ.'source_geom')then
      if(next_token(line,b,e,p))continue
      if(geom.EQ.1000000000)continue
      if(line(b:e).EQ.'point')then
      geom=0
      else if(line(b:e).EQ.'line')then
      geom=1
      else if(line(b:e).EQ.'surface')then
      geom=2
      else if(line(b:e).EQ.'volume')then
      geom=3
      end if
      if(geom.NE.1000000000)continue
      else if(line(b:e).EQ.'source_species')then
      if(next_token(line,b,e,p))continue
      if(species.EQ.1000000000)continue
      species=string_lookup(line(b:e),species_sy,sp_num)
      if((species.GT.0.AND.species.LE.sp_num))continue
      else if(line(b:e).EQ.'source_root_sp')then
      if(next_token(line,b,e,p))continue
      if(root_sp.EQ.1000000000)continue
      root_sp=string_lookup(line(b:e),species_sy,sp_num)
      if((root_sp.GT.0.AND.root_sp.LE.sp_num))continue
      else if(line(b:e).EQ.'source_nflights')then
      if(next_token(line,b,e,p))continue
      nflights=read_integer(line(b:e))
      else if(line(b:e).EQ.'source_puff_temp')then
      if(next_token(line,b,e,p))continue
      puff_temp=read_real(line(b:e))*(1.380658e-23_DOUBLE)
      if(puff_temp.GE.(0.0_DOUBLE))continue
      else if(line(b:e).EQ.'source_puff_exponent')then
      if(next_token(line,b,e,p))continue
      puff_exponent=read_real(line(b:e))
      if(puff_exponent.GT.(-1.0_DOUBLE))continue
      else if(line(b:e).EQ.'source_e_bin_num')then
      if(next_token(line,b,e,p))continue
      e_bin_num=read_integer(line(b:e))
      if((e_bin_num.GT.0).AND.(e_bin_num.LE.15))continue
      else if(line(b:e).EQ.'source_e_bin_min')then
      if(next_token(line,b,e,p))continue
      e_bin_min=read_real(line(b:e))*(1.60217733e-19_DOUBLE)
      if(e_bin_min.GT.(0.0_DOUBLE))continue
      else if(line(b:e).EQ.'source_e_bin_max')then
      if(next_token(line,b,e,p))continue
      e_bin_max=read_real(line(b:e))*(1.60217733e-19_DOUBLE)
      if(e_bin_max.GT.(0.0_DOUBLE))continue
      else if(line(b:e).EQ.'source_e_bin_spacing')then
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.'linear')then
      e_bin_spacing=1
      else if(line(b:e).EQ.'log')then
      e_bin_spacing=2
      else
      if(' Unexpected e_bin_spacing option '.EQ.' ')continue
      end if
      else if(line(b:e).EQ.'specify_flux')then
      specify_flux=1
      else if(line(b:e).EQ.'specify_current')then
      specify_flux=0
      else if(line(b:e).EQ.'source_stratum')then
      iseg=0
90015 continue
      if(.NOT.next_token(line,b,e,p))then
      if(num_segments.EQ.314159265)then
      num_segments=iseg
      else
      if(num_segments.EQ.iseg)continue
      end if
      goto 90016
      else
      iseg=iseg+1
      if(iseg.GT.dim_segments)then
      dim_segments=iseg
      if(mod(((dim_segments)-(1)+1),100).EQ.1)then
      source_int_params =>mem_realloc_i2(source_int_params,(1),(6),(1),(
     &((int(((((dim_segments)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int
     &(((((dim_segments)-(1)+1))+100-1)/100)*100)+(1)-1),'source_int_par
     &ams')
      end if
      if(mod(((dim_segments)-(1)+1),100).EQ.1)then
      source_real_params =>mem_realloc_r2(source_real_params,(11),(24+15
     &-1),(1),(((int(((((dim_segments)-(1)+1))+100-1)/100)*100)-100)+(1)
     &-1),((int(((((dim_segments)-(1)+1))+100-1)/100)*100)+(1)-1),'sourc
     &e_real_params')
      end if
      do i=1,6
      source_int_params(i,iseg)=314159265
      end do
      source_int_params(6,iseg)=0
      source_int_params(5,iseg)=0
      do i=11,24+15-1
      source_real_params(i,iseg)=(314159265.3589793238462_DOUBLE)
      end do
      end if
      source_int_params(2,iseg)=read_integer(line(b:e))
      end if
      goto 90015
90016 continue
      else if(line(b:e).EQ.'source_segment')then
      iseg=0
90017 continue
      if(.NOT.next_token(line,b,e,p))then
      if(num_segments.EQ.314159265)then
      num_segments=iseg
      else
      if(num_segments.EQ.iseg)continue
      end if
      goto 90018
      else
      iseg=iseg+1
      if(iseg.GT.dim_segments)then
      dim_segments=iseg
      if(mod(((dim_segments)-(1)+1),100).EQ.1)then
      source_int_params =>mem_realloc_i2(source_int_params,(1),(6),(1),(
     &((int(((((dim_segments)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int
     &(((((dim_segments)-(1)+1))+100-1)/100)*100)+(1)-1),'source_int_par
     &ams')
      end if
      if(mod(((dim_segments)-(1)+1),100).EQ.1)then
      source_real_params =>mem_realloc_r2(source_real_params,(11),(24+15
     &-1),(1),(((int(((((dim_segments)-(1)+1))+100-1)/100)*100)-100)+(1)
     &-1),((int(((((dim_segments)-(1)+1))+100-1)/100)*100)+(1)-1),'sourc
     &e_real_params')
      end if
      do i=1,6
      source_int_params(i,iseg)=314159265
      end do
      source_int_params(6,iseg)=0
      source_int_params(5,iseg)=0
      do i=11,24+15-1
      source_real_params(i,iseg)=(314159265.3589793238462_DOUBLE)
      end do
      end if
      if(line(b:e).EQ.'*')then
      source_int_params(6,iseg)=1
      source_int_params(3,iseg)=2000000000
      else
      source_int_params(6,iseg)=0
      source_int_params(3,iseg)=read_integer(line(b:e))
      end if
      end if
      goto 90017
90018 continue
      else if(line(b:e).EQ.'source_segment_iy')then
      iseg=0
90019 continue
      if(.NOT.next_token(line,b,e,p))then
      if(num_segments.EQ.314159265)then
      num_segments=iseg
      else
      if(num_segments.EQ.iseg)continue
      end if
      goto 90020
      else
      iseg=iseg+1
      if(iseg.GT.dim_segments)then
      dim_segments=iseg
      if(mod(((dim_segments)-(1)+1),100).EQ.1)then
      source_int_params =>mem_realloc_i2(source_int_params,(1),(6),(1),(
     &((int(((((dim_segments)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int
     &(((((dim_segments)-(1)+1))+100-1)/100)*100)+(1)-1),'source_int_par
     &ams')
      end if
      if(mod(((dim_segments)-(1)+1),100).EQ.1)then
      source_real_params =>mem_realloc_r2(source_real_params,(11),(24+15
     &-1),(1),(((int(((((dim_segments)-(1)+1))+100-1)/100)*100)-100)+(1)
     &-1),((int(((((dim_segments)-(1)+1))+100-1)/100)*100)+(1)-1),'sourc
     &e_real_params')
      end if
      do i=1,6
      source_int_params(i,iseg)=314159265
      end do
      source_int_params(6,iseg)=0
      source_int_params(5,iseg)=0
      do i=11,24+15-1
      source_real_params(i,iseg)=(314159265.3589793238462_DOUBLE)
      end do
      end if
      source_int_params(4,iseg)=read_integer(line(b:e))
      end if
      goto 90019
90020 continue
      else if(line(b:e).EQ.'source_strength')then
      iseg=0
90021 continue
      if(.NOT.next_token(line,b,e,p))then
      if(num_segments.EQ.314159265)then
      num_segments=iseg
      else
      if(num_segments.EQ.iseg)continue
      end if
      goto 90022
      else
      iseg=iseg+1
      if(iseg.GT.dim_segments)then
      dim_segments=iseg
      if(mod(((dim_segments)-(1)+1),100).EQ.1)then
      source_int_params =>mem_realloc_i2(source_int_params,(1),(6),(1),(
     &((int(((((dim_segments)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int
     &(((((dim_segments)-(1)+1))+100-1)/100)*100)+(1)-1),'source_int_par
     &ams')
      end if
      if(mod(((dim_segments)-(1)+1),100).EQ.1)then
      source_real_params =>mem_realloc_r2(source_real_params,(11),(24+15
     &-1),(1),(((int(((((dim_segments)-(1)+1))+100-1)/100)*100)-100)+(1)
     &-1),((int(((((dim_segments)-(1)+1))+100-1)/100)*100)+(1)-1),'sourc
     &e_real_params')
      end if
      do i=1,6
      source_int_params(i,iseg)=314159265
      end do
      source_int_params(6,iseg)=0
      source_int_params(5,iseg)=0
      do i=11,24+15-1
      source_real_params(i,iseg)=(314159265.3589793238462_DOUBLE)
      end do
      end if
      source_real_params(20,iseg)=read_real(line(b:e))
      end if
      goto 90021
90022 continue
      else if(line(b:e).EQ.'source_file')then
      iseg=0
      if(next_token(line,b,e,p))continue
      source_file=line(b:e)
      diskin2=20+1
      open(unit=diskin2,file=source_file,status='old',form='formatted',i
     &ostat=open_stat)
      if(open_stat.EQ.0)continue
      if(next_token(line,b,e,p))then
      file_format=line(b:e)
      else
      file_format='tabular'
      end if
      if(file_format.EQ.'tabular')then
90023 continue
      if(read_string(diskin2,line,length))then
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      iseg=iseg+1
      if(iseg.GT.dim_segments)then
      dim_segments=iseg
      if(mod(((dim_segments)-(1)+1),100).EQ.1)then
      source_int_params =>mem_realloc_i2(source_int_params,(1),(6),(1),(
     &((int(((((dim_segments)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int
     &(((((dim_segments)-(1)+1))+100-1)/100)*100)+(1)-1),'source_int_par
     &ams')
      end if
      if(mod(((dim_segments)-(1)+1),100).EQ.1)then
      source_real_params =>mem_realloc_r2(source_real_params,(11),(24+15
     &-1),(1),(((int(((((dim_segments)-(1)+1))+100-1)/100)*100)-100)+(1)
     &-1),((int(((((dim_segments)-(1)+1))+100-1)/100)*100)+(1)-1),'sourc
     &e_real_params')
      end if
      do i=1,6
      source_int_params(i,iseg)=314159265
      end do
      source_int_params(6,iseg)=0
      source_int_params(5,iseg)=0
      do i=11,24+15-1
      source_real_params(i,iseg)=(314159265.3589793238462_DOUBLE)
      end do
      end if
      if(next_token(line,b,e,p))continue
      source_int_params(2,iseg)=read_integer(line(b:e))
      if(next_token(line,b,e,p))continue
      source_int_params(3,iseg)=read_integer(line(b:e))
      if(next_token(line,b,e,p))continue
      source_real_params(20,iseg)=read_real(line(b:e))
      goto 90023
      end if
      if(num_segments.EQ.314159265)continue
      num_segments=iseg
      close(unit=diskin2)
      else if(file_format.EQ.'row')then
      
90024 continue
      if(read_string(diskin2,line,length))then
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      itest=read_int_soft_fail(line(b:e))
      rtest=read_real_soft_fail(line(b:e))
      if(itest.NE.1000000000)then
      data_type=1
      else if(rtest.NE.(1.0e30_DOUBLE))then
      data_type=2
      else
      data_type=0
      end if
      if(data_type.EQ.0)then
      if(num_segments.EQ.314159265)then
      if(iseg.NE.0)num_segments=iseg
      else
      if(num_segments.EQ.iseg)continue
      end if
      call parse_label(line,data_code,back)
      if(data_code.NE.0)continue
      iseg=0
      e_bin_entry=0
      real_mult=(1.0_DOUBLE)
      if(data_code.EQ.13)then
      real_mult=(1.60217733e-19_DOUBLE)
      data_code=12
      end if
      if(data_code.EQ.12)then
      if((back.GT.0.AND.back.LE.bk_num))then
      if(species_sy(problem_background_sp(back)).EQ.'e')data_code=14
      end if
      end if
      goto 90024
      else
      if(data_code.NE.0)continue
90025 continue
      if(e_bin_entry.EQ.0)then
      iseg=iseg+1
      if(iseg.GT.dim_segments)then
      dim_segments=iseg
      if(mod(((dim_segments)-(1)+1),100).EQ.1)then
      source_int_params =>mem_realloc_i2(source_int_params,(1),(6),(1),(
     &((int(((((dim_segments)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int
     &(((((dim_segments)-(1)+1))+100-1)/100)*100)+(1)-1),'source_int_par
     &ams')
      end if
      if(mod(((dim_segments)-(1)+1),100).EQ.1)then
      source_real_params =>mem_realloc_r2(source_real_params,(11),(24+15
     &-1),(1),(((int(((((dim_segments)-(1)+1))+100-1)/100)*100)-100)+(1)
     &-1),((int(((((dim_segments)-(1)+1))+100-1)/100)*100)+(1)-1),'sourc
     &e_real_params')
      end if
      do i=1,6
      source_int_params(i,iseg)=314159265
      end do
      source_int_params(6,iseg)=0
      source_int_params(5,iseg)=0
      do i=11,24+15-1
      source_real_params(i,iseg)=(314159265.3589793238462_DOUBLE)
      end do
      end if
      end if
      if(data_type.EQ.1)then
      source_int_params(data_code,iseg)=read_integer(line(b:e))
      else if(data_type.EQ.2)then
      source_real_params(data_code,iseg)=read_real(line(b:e))*real_mult
      if(data_code.GE.24)then
      e_bin_entry=1
      if(data_code.LT.24+e_bin_num-1)then
      data_code=data_code+1
      else
      e_bin_entry=0
      total=(0.0_DOUBLE)
      do i_bin=1,e_bin_num
      i_code=24+i_bin-1
      total=total+(source_real_params(i_code,iseg))
      end do
      if(total.GT.(0.0_DOUBLE))continue
      cumul=(0.0_DOUBLE)
      do i_bin=1,e_bin_num
      i_code=24+i_bin-1
      cumul=cumul+(source_real_params(i_code,iseg)/total)
      source_real_params(i_code,iseg)=cumul
      end do
      if(source_real_params(data_code,iseg).EQ.(1.0_DOUBLE))continue
      data_code=24
      end if
      end if
      end if
      if(next_token(line,b,e,p))then
      goto 90025
      else
      goto 90024
      end if
      end if
      end if
      close(unit=diskin2)
      
      end if
      else if(line(b:e).EQ.'source_time_variation')then
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.'delta')then
      time_varn=0
      if(so_time_initialization.EQ.0)continue
      else if(line(b:e).EQ.'uniform')then
      time_varn=1
      else
      if('Unsupported time variation'.EQ.line(b:e))continue
      end if
      else if(line(b:e).EQ.'end_source_group')then
      if(num_segments.GT.0)continue
      if(source_real_params(20,1).EQ.(314159265.3589793238462_DOUBLE))th
     &en
      if(source_real_params(11,1).NE.(314159265.3589793238462_DOUBLE))co
     &ntinue
      if(source_real_params(18,1).NE.(314159265.3589793238462_DOUBLE))co
     &ntinue
      if(source_real_params(19,1).NE.(314159265.3589793238462_DOUBLE))co
     &ntinue
      if(specify_flux.EQ.0)continue
      do iseg=1,num_segments
      source_real_params(20,iseg)=source_real_params(11,iseg)*abs(source
     &_real_params(18,iseg))*source_real_params(19,iseg)
      end do
      end if
      if((source_real_params(12,1).NE.(314159265.3589793238462_DOUBLE)).
     &AND.(source_real_params(14,1).NE.(314159265.3589793238462_DOUBLE))
     &.AND.((source_real_params(18,1).NE.(314159265.3589793238462_DOUBLE
     &)).OR.(source_real_params(15,1).NE.(314159265.3589793238462_DOUBLE
     &))))then
      do iseg=1,num_segments
      if(source_real_params(12,iseg).GT.(0.0_DOUBLE))continue
      if(source_real_params(14,iseg).GT.(0.0_DOUBLE))continue
      if((source_real_params(18,iseg).NE.(314159265.3589793238462_DOUBLE
     &)).AND.(source_real_params(15,iseg).EQ.(314159265.3589793238462_DO
     &UBLE)))then
      targ_v(1)=source_real_params(18,iseg)
      targ_v(2)=(0.0_DOUBLE)
      targ_v(3)=(0.0_DOUBLE)
      else
      targ_v(1)=source_real_params(15,iseg)
      targ_v(2)=source_real_params(16,iseg)
      targ_v(3)=source_real_params(17,iseg)
      end if
      if(sqrt((targ_v(1)**2+targ_v(2)**2+targ_v(3)**2)).GT.(0.0_DOUBLE))
     &continue
      source_real_params(21,iseg)=(3.0_DOUBLE)*source_real_params(12,ise
     &g)+(0.5_DOUBLE)*source_real_params(14,iseg)
      source_real_params(23,iseg)=phi_sheath(targ_v(1),source_real_param
     &s(12,iseg),source_real_params(14,iseg))*source_real_params(14,iseg
     &)
      source_real_params(22,iseg)=(0.0_DOUBLE)
      end do
      end if
      if(update.EQ.0)then
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_base_ptr =>mem_realloc_i1(source_base_ptr,(1),(((int(((((so
     &_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((so_grps)-(1)
     &+1))+100-1)/100)*100)+(1)-1),'source_base_ptr')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_num_segments =>mem_realloc_i1(source_num_segments,(1),(((in
     &t(((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((so_g
     &rps)-(1)+1))+100-1)/100)*100)+(1)-1),'source_num_segments')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_type =>mem_realloc_i1(source_type,(1),(((int(((((so_grps)-(
     &1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((so_grps)-(1)+1))+100
     &-1)/100)*100)+(1)-1),'source_type')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_geometry =>mem_realloc_i1(source_geometry,(1),(((int(((((so
     &_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((so_grps)-(1)
     &+1))+100-1)/100)*100)+(1)-1),'source_geometry')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_num_flights =>mem_realloc_i1(source_num_flights,(1),(((int(
     &((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((so_grp
     &s)-(1)+1))+100-1)/100)*100)+(1)-1),'source_num_flights')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_num_checkpoints =>mem_realloc_i1(source_num_checkpoints,(1)
     &,(((int(((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((
     &((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),'source_num_checkpoints
     &')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_species =>mem_realloc_i1(source_species,(1),(((int(((((so_g
     &rps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((so_grps)-(1)+1
     &))+100-1)/100)*100)+(1)-1),'source_species')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_root_species =>mem_realloc_i1(source_root_species,(1),(((in
     &t(((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((so_g
     &rps)-(1)+1))+100-1)/100)*100)+(1)-1),'source_root_species')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_time_variation =>mem_realloc_i1(source_time_variation,(1),(
     &((int(((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((
     &so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),'source_time_variation')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_num_gparameters =>mem_realloc_i1(source_num_gparameters,(1)
     &,(((int(((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((
     &((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),'source_num_gparameters
     &')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_num_parameters =>mem_realloc_i1(source_num_parameters,(1),(
     &((int(((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((
     &so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),'source_num_parameters')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_gparameters_base =>mem_realloc_i1(source_gparameters_base,(
     &1),(((int(((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(
     &((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),'source_gparameters_b
     &ase')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_parameters_base =>mem_realloc_i1(source_parameters_base,(1)
     &,(((int(((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((
     &((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),'source_parameters_base
     &')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_parameters_data_base =>mem_realloc_i1(source_parameters_dat
     &a_base,(1),(((int(((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1
     &),((int(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),'source_param
     &eters_data_base')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_num_giparameters =>mem_realloc_i1(source_num_giparameters,(
     &1),(((int(((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(
     &((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),'source_num_giparamet
     &ers')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_num_iparameters =>mem_realloc_i1(source_num_iparameters,(1)
     &,(((int(((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((
     &((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),'source_num_iparameters
     &')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_giparameters_base =>mem_realloc_i1(source_giparameters_base
     &,(1),(((int(((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((in
     &t(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),'source_giparameter
     &s_base')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_iparameters_base =>mem_realloc_i1(source_iparameters_base,(
     &1),(((int(((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(
     &((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),'source_iparameters_b
     &ase')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_iparameters_data_base =>mem_realloc_i1(source_iparameters_d
     &ata_base,(1),(((int(((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)
     &-1),((int(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),'source_ipa
     &rameters_data_base')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_total_current =>mem_realloc_r1(source_total_current,(1),(((
     &int(((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((so
     &_grps)-(1)+1))+100-1)/100)*100)+(1)-1),'source_total_current')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_weight_norm =>mem_realloc_r1(source_weight_norm,(1),(((int(
     &((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((so_grp
     &s)-(1)+1))+100-1)/100)*100)+(1)-1),'source_weight_norm')
      end if
      if(mod(((so_grps)-(1)+1),100).EQ.1)then
      source_scale_factor =>mem_realloc_r1(source_scale_factor,(1),(((in
     &t(((((so_grps)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((so_g
     &rps)-(1)+1))+100-1)/100)*100)+(1)-1),'source_scale_factor')
      end if
      source_base_ptr(so_grps)=so_seg_tot+1
      if(type.NE.1000000000)continue
      source_type(so_grps)=type
      if(geom.NE.1000000000)continue
      source_geometry(so_grps)=geom
      source_num_flights(so_grps)=nflights
      source_num_checkpoints(so_grps)=0
      if((species.GT.0.AND.species.LE.sp_num))continue
      source_species(so_grps)=species
      if((root_sp.GT.0.AND.root_sp.LE.sp_num))continue
      source_root_species(so_grps)=root_sp
      source_time_variation(so_grps)=time_varn
      source_num_gparameters(so_grps)=0
      source_num_parameters(so_grps)=0
      source_gparameters_base(so_grps)=so_gparams_list_size
      source_parameters_base(so_grps)=so_params_list_size
      source_parameters_data_base(so_grps)=so_params_data_size
      source_num_giparameters(so_grps)=0
      source_num_iparameters(so_grps)=0
      source_giparameters_base(so_grps)=so_giparams_list_size
      source_iparameters_base(so_grps)=so_iparams_list_size
      source_iparameters_data_base(so_grps)=so_iparams_data_size
      source_total_current(so_grps)=(0.0_DOUBLE)
      source_scale_factor(so_grps)=(1.0_DOUBLE)
      else
      if(iterative.EQ.1)continue
      if(source_base_ptr(so_grps).EQ.so_seg_tot+1)continue
      if(source_type(so_grps).EQ.type)continue
      if(source_geometry(so_grps).EQ.geom)continue
      source_total_current(so_grps)=(0.0_DOUBLE)
      end if
      if(source_type(so_grps).EQ.2)then
      source_num_gparameters(so_grps)=2
      so_gparams_list_size=so_gparams_list_size+(source_num_gparameters(
     &so_grps))
      if(so_gparams_list_size.GT.so_gparams_list_dim)then
      source_gparameters_list =>mem_realloc_i1(source_gparameters_list,(
     &1),so_gparams_list_dim,so_gparams_list_size,'source_gparameters_li
     &st')
      source_gparameters_data =>mem_realloc_r1(source_gparameters_data,(
     &1),so_gparams_list_dim,so_gparams_list_size,'source_gparameters_da
     &ta')
      so_gparams_list_dim=so_gparams_list_size
      end if
      source_gparameters_list(source_gparameters_base(so_grps)+1)=1
      source_gparameters_data(source_gparameters_base(so_grps)+1)=puff_t
     &emp
      source_gparameters_list(source_gparameters_base(so_grps)+2)=2
      source_gparameters_data(source_gparameters_base(so_grps)+2)=puff_e
     &xponent
      else if(source_type(so_grps).EQ.1)then
      source_num_parameters(so_grps)=3
      so_params_list_size=so_params_list_size+(source_num_parameters(so_
     &grps))
      if(so_params_list_size.GT.so_params_list_dim)then
      source_parameters_list =>mem_realloc_i1(source_parameters_list,(1)
     &,so_params_list_dim,so_params_list_size,'source_parameters_list')
      so_params_list_dim=so_params_list_size
      end if
      source_parameters_list(source_parameters_base(so_grps)+1)=5
      source_parameters_list(source_parameters_base(so_grps)+2)=6
      source_parameters_list(source_parameters_base(so_grps)+3)=7
      else if(source_type(so_grps).EQ.4)then
      source_num_parameters(so_grps)=4
      so_params_list_size=so_params_list_size+(source_num_parameters(so_
     &grps))
      if(so_params_list_size.GT.so_params_list_dim)then
      source_parameters_list =>mem_realloc_i1(source_parameters_list,(1)
     &,so_params_list_dim,so_params_list_size,'source_parameters_list')
      so_params_list_dim=so_params_list_size
      end if
      source_parameters_list(source_parameters_base(so_grps)+1)=1
      source_parameters_list(source_parameters_base(so_grps)+2)=2
      source_parameters_list(source_parameters_base(so_grps)+3)=3
      source_parameters_list(source_parameters_base(so_grps)+4)=4
      else if(source_type(so_grps).EQ.6)then
      source_num_giparameters(so_grps)=2
      so_giparams_list_size=so_giparams_list_size+(source_num_giparamete
     &rs(so_grps))
      if(so_giparams_list_size.GT.so_giparams_list_dim)then
      source_giparameters_list =>mem_realloc_i1(source_giparameters_list
     &,(1),so_giparams_list_dim,so_giparams_list_size,'source_giparamete
     &rs_list')
      source_giparameters_data =>mem_realloc_i1(source_giparameters_data
     &,(1),so_giparams_list_dim,so_giparams_list_size,'source_giparamete
     &rs_data')
      so_giparams_list_dim=so_giparams_list_size
      end if
      source_giparameters_list(source_giparameters_base(so_grps)+1)=1
      source_giparameters_data(source_giparameters_base(so_grps)+1)=e_bi
     &n_num
      source_giparameters_list(source_giparameters_base(so_grps)+2)=2
      source_giparameters_data(source_giparameters_base(so_grps)+2)=e_bi
     &n_spacing
      source_num_gparameters(so_grps)=2
      so_gparams_list_size=so_gparams_list_size+(source_num_gparameters(
     &so_grps))
      if(so_gparams_list_size.GT.so_gparams_list_dim)then
      source_gparameters_list =>mem_realloc_i1(source_gparameters_list,(
     &1),so_gparams_list_dim,so_gparams_list_size,'source_gparameters_li
     &st')
      source_gparameters_data =>mem_realloc_r1(source_gparameters_data,(
     &1),so_gparams_list_dim,so_gparams_list_size,'source_gparameters_da
     &ta')
      so_gparams_list_dim=so_gparams_list_size
      end if
      source_gparameters_list(source_gparameters_base(so_grps)+1)=3
      source_gparameters_list(source_gparameters_base(so_grps)+2)=4
      if(e_bin_spacing.EQ.1)then
      source_gparameters_data(source_gparameters_base(so_grps)+1)=e_bin_
     &min
      source_gparameters_data(source_gparameters_base(so_grps)+2)=(e_bin
     &_max-e_bin_min)/REAL(e_bin_num,DOUBLE)
      else if(e_bin_spacing.EQ.2)then
      source_gparameters_data(source_gparameters_base(so_grps)+1)=log(e_
     &bin_min)
      source_gparameters_data(source_gparameters_base(so_grps)+2)=(log(e
     &_bin_max/e_bin_min))/REAL(e_bin_num,DOUBLE)
      end if
      source_num_parameters(so_grps)=e_bin_num
      so_params_list_size=so_params_list_size+(source_num_parameters(so_
     &grps))
      if(so_params_list_size.GT.so_params_list_dim)then
      source_parameters_list =>mem_realloc_i1(source_parameters_list,(1)
     &,so_params_list_dim,so_params_list_size,'source_parameters_list')
      so_params_list_dim=so_params_list_size
      end if
      do i_bin=1,e_bin_num
      source_parameters_list(source_parameters_base(so_grps)+i_bin)=15
      end do
      end if
      write(6,*)
      write(6,*)' Processing source group ',so_grps
      write(6,*)
      if(source_geometry(so_grps).EQ.2)then
      write(6,'(2x,a,12x,a,2x,a,30x,a)')'source','stratum','segment','se
     &ctor points'
      write(6,'(2x,a,2x,a,2x,a,4x,a,4x,a,6x,a,4x,a,4x,a,13x,a,5x,a,5x,a)
     &')'segment','stratum','segment','iy','sector','x1','start','x3','x
     &1','end','x3'
      do sector=1,nsectors
      do iseg=1,num_segments
      if(((sector_type_pointer(2,sector).GT.0.AND.sector_type_pointer(2,
     &sector).LE.sc_plasma_num).OR.(sector_type_pointer(1,sector).GT.0.A
     &ND.sector_type_pointer(1,sector).LE.sc_vacuum_num)).AND.(strata(se
     &ctor).EQ.source_int_params(2,iseg)).AND.((zone_index(3,sector_zone
     &(sector)).EQ.source_int_params(4,iseg)).OR.(source_int_params(4,is
     &eg).EQ.314159265)).AND.((sector_strata_segment(sector).EQ.source_i
     &nt_params(3,iseg)).OR.(source_int_params(6,iseg).EQ.1)))then
      source_int_params(5,iseg)=source_int_params(5,iseg)+1
      end if
      end do
      end do
      do sector=1,nsectors
      do iseg=1,num_segments
      if(((sector_type_pointer(2,sector).GT.0.AND.sector_type_pointer(2,
     &sector).LE.sc_plasma_num).OR.(sector_type_pointer(1,sector).GT.0.A
     &ND.sector_type_pointer(1,sector).LE.sc_vacuum_num)).AND.(strata(se
     &ctor).EQ.source_int_params(2,iseg)).AND.((zone_index(3,sector_zone
     &(sector)).EQ.source_int_params(4,iseg)).OR.(source_int_params(4,is
     &eg).EQ.314159265)).AND.((sector_strata_segment(sector).EQ.source_i
     &nt_params(3,iseg)).OR.(source_int_params(6,iseg).EQ.1)))then
      zone=sector_zone(sector)
      so_seg_tot=so_seg_tot+1
      write(6,'(2x,i4,5x,i4,5x,i4,5x,i4,4x,i5,1x,4(1pe13.5,2x))')so_seg_
     &tot-source_base_ptr(so_grps)+1,strata(sector),sector_strata_segmen
     &t(sector),zone_index(3,sector_zone(sector)),sector,sector_points(1
     &,0,sector),sector_points(3,0,sector),sector_points(1,1,sector),sec
     &tor_points(3,1,sector)
      if(update.EQ.0)then
      if(mod(((so_seg_tot)-(1)+1),100).EQ.1)then
      source_current =>mem_realloc_r1(source_current,(1),(((int(((((so_s
     &eg_tot)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((so_seg_tot)
     &-(1)+1))+100-1)/100)*100)+(1)-1),'source_current')
      end if
      if(mod(((so_seg_tot)-(1)+1),100).EQ.1)then
      source_segment_ptr =>mem_realloc_i1(source_segment_ptr,(1),(((int(
     &((((so_seg_tot)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((so_
     &seg_tot)-(1)+1))+100-1)/100)*100)+(1)-1),'source_segment_ptr')
      end if
      if(mod(((so_seg_tot)-(1)+1),100).EQ.1)then
      source_segment_rel_wt =>mem_realloc_r1(source_segment_rel_wt,(1),(
     &((int(((((so_seg_tot)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int((
     &(((so_seg_tot)-(1)+1))+100-1)/100)*100)+(1)-1),'source_segment_rel
     &_wt')
      end if
      if(mod(((so_seg_tot)-(1)+1),100).EQ.1)then
      source_segment_prob_alias =>mem_realloc_r1(source_segment_prob_ali
     &as,(1),(((int(((((so_seg_tot)-(1)+1))+100-1)/100)*100)-100)+(1)-1)
     &,((int(((((so_seg_tot)-(1)+1))+100-1)/100)*100)+(1)-1),'source_seg
     &ment_prob_alias')
      end if
      if(mod(((so_seg_tot)-(1)+1),100).EQ.1)then
      source_segment_ptr_alias =>mem_realloc_i1(source_segment_ptr_alias
     &,(1),(((int(((((so_seg_tot)-(1)+1))+100-1)/100)*100)-100)+(1)-1),(
     &(int(((((so_seg_tot)-(1)+1))+100-1)/100)*100)+(1)-1),'source_segme
     &nt_ptr_alias')
      end if
      if(source_num_parameters(so_grps).GT.0)then
      so_params_data_size=so_params_data_size+(source_num_parameters(so_
     &grps))
      if(so_params_data_size.GT.so_params_data_dim)then
      source_parameters_data =>mem_realloc_r1(source_parameters_data,(1)
     &,so_params_data_dim,so_params_data_size,'source_parameters_data')
      so_params_data_dim=so_params_data_size
      end if
      end if
      end if
      if(source_real_params(20,iseg).GT.(0.0_DOUBLE).AND.source_real_par
     &ams(20,iseg).NE.(314159265.3589793238462_DOUBLE))continue
      if(source_int_params(5,iseg).GT.0)continue
      mult=(1.0_DOUBLE)/float(source_int_params(5,iseg))
      if(specify_flux.EQ.1)then
      xdiff(1)=sector_points(1,0,sector)-sector_points(1,1,sector)
      xdiff(2)=sector_points(2,0,sector)-sector_points(2,1,sector)
      xdiff(3)=sector_points(3,0,sector)-sector_points(3,1,sector)
      
      if(xdiff(2).EQ.(0.0_DOUBLE))continue
      if(geometry_symmetry.EQ.1)then
      circum=universal_cell_max(2)-universal_cell_min(2)
      else if(geometry_symmetry.EQ.4)then
      circum=zone_max(2,zone)-zone_min(2,zone)
      else if(geometry_symmetry.EQ.2)then
      circum=atan2((0.0_DOUBLE),-(1.0_DOUBLE))*(sector_points(1,0,sector
     &)+sector_points(1,1,sector))
      else if((geometry_symmetry.EQ.5).OR.(geometry_symmetry.EQ.6))then
      circum=(0.5_DOUBLE)*(sector_points(1,0,sector)+sector_points(1,1,s
     &ector))*(zone_max(2,zone)-zone_min(2,zone))
      else
      if('Unexpected geometry symmetry in setting up source'.EQ.' ')cont
     &inue
      end if
      area=circum*sqrt((xdiff(1)**2+xdiff(2)**2+xdiff(3)**2))
      if(area.GT.(0.0_DOUBLE))continue
      source_current(so_seg_tot)=source_real_params(20,iseg)*area*mult
      
      else
      source_current(so_seg_tot)=source_real_params(20,iseg)*mult
      end if
      source_total_current(so_grps)=source_total_current(so_grps)+(sourc
     &e_current(so_seg_tot))
      if(update.EQ.0)then
      source_segment_ptr(so_seg_tot)=sector
      source_segment_rel_wt(so_seg_tot)=(1.0_DOUBLE)
      else
      if(source_segment_ptr(so_seg_tot).EQ.sector)continue
      end if
      if(source_type(so_grps).EQ.1)then
      if((source_real_params(21,iseg).NE.(314159265.3589793238462_DOUBLE
     &)).AND.(source_real_params(23,iseg).NE.(314159265.3589793238462_DO
     &UBLE)).AND.(source_real_params(22,iseg).NE.(314159265.358979323846
     &2_DOUBLE)))then
      if(source_num_parameters(so_grps).GT.0)continue
      do iparam=1,source_num_parameters(so_grps)
      if(source_parameters_list(source_parameters_base(so_grps)+iparam).
     &EQ.5)then
      source_parameters_data(source_parameters_data_base(so_grps)+(so_se
     &g_tot-source_base_ptr(so_grps))*source_num_parameters(so_grps)+ipa
     &ram)=source_real_params(21,iseg)
      else if(source_parameters_list(source_parameters_base(so_grps)+ipa
     &ram).EQ.7)then
      source_parameters_data(source_parameters_data_base(so_grps)+(so_se
     &g_tot-source_base_ptr(so_grps))*source_num_parameters(so_grps)+ipa
     &ram)=source_real_params(23,iseg)
      else if(source_parameters_list(source_parameters_base(so_grps)+ipa
     &ram).EQ.6)then
      source_parameters_data(source_parameters_data_base(so_grps)+(so_se
     &g_tot-source_base_ptr(so_grps))*source_num_parameters(so_grps)+ipa
     &ram)=source_real_params(22,iseg)
      end if
      end do
      else
      if(species_sy(problem_background_sp(1)).EQ.'e')continue
      if(source_num_parameters(so_grps).GT.0)continue
      do iparam=1,source_num_parameters(so_grps)
      if(source_parameters_list(source_parameters_base(so_grps)+iparam).
     &EQ.5)then
      if(background_temp(1,zone_pointer(zone)).GT.(0.0_DOUBLE))then
      source_parameters_data(source_parameters_data_base(so_grps)+(so_se
     &g_tot-source_base_ptr(so_grps))*source_num_parameters(so_grps)+ipa
     &ram)=(3._DOUBLE)*background_temp(1,zone_pointer(zone))
      else
      source_parameters_data(source_parameters_data_base(so_grps)+(so_se
     &g_tot-source_base_ptr(so_grps))*source_num_parameters(so_grps)+ipa
     &ram)=(3._DOUBLE)*(1.60217733e-19_DOUBLE)
      end if
      else if(source_parameters_list(source_parameters_base(so_grps)+ipa
     &ram).EQ.6)then
      source_parameters_data(source_parameters_data_base(so_grps)+(so_se
     &g_tot-source_base_ptr(so_grps))*source_num_parameters(so_grps)+ipa
     &ram)=(1.0_DOUBLE)
      else if(source_parameters_list(source_parameters_base(so_grps)+ipa
     &ram).EQ.7)then
      source_parameters_data(source_parameters_data_base(so_grps)+(so_se
     &g_tot-source_base_ptr(so_grps))*source_num_parameters(so_grps)+ipa
     &ram)=(0.0_DOUBLE)
      end if
      end do
      end if
      
      else if(source_type(so_grps).EQ.6)then
      if(source_num_parameters(so_grps).GT.0)continue
      i_bin=0
      do iparam=1,source_num_parameters(so_grps)
      if(source_parameters_list(source_parameters_base(so_grps)+iparam).
     &EQ.15)then
      source_parameters_data(source_parameters_data_base(so_grps)+(so_se
     &g_tot-source_base_ptr(so_grps))*source_num_parameters(so_grps)+ipa
     &ram)=source_real_params(24+i_bin,iseg)
      i_bin=i_bin+1
      end if
      end do
      if(i_bin.EQ.e_bin_num)continue
      end if
      end if
      end do
      end do
      if(update.EQ.0)then
      source_num_segments(so_grps)=so_seg_tot-source_base_ptr(so_grps)+1
      else
      if(source_num_segments(so_grps).EQ.so_seg_tot-source_base_ptr(so_g
     &rps)+1)continue
      end if
      else if(source_geometry(so_grps).EQ.3)then
      write(6,'(2x,a,2x,a,5x,a,7x,a,13x,a,13x,a)')'segment','zone','iy',
     &'x1','x2','x3'
      do zone=1,zn_num
      do iseg=1,num_segments
      if(((zone_type(zone).EQ.2).OR.(zone_type(zone).EQ.1)).AND.(source_
     &int_params(1,iseg).EQ.zone))then
      source_int_params(5,iseg)=source_int_params(5,iseg)+1
      end if
      end do
      end do
      do zone=1,zn_num
      do iseg=1,num_segments
      if(((zone_type(zone).EQ.2).OR.(zone_type(zone).EQ.1)).AND.(source_
     &int_params(1,iseg).EQ.zone))then
      so_seg_tot=so_seg_tot+1
      write(6,'(2x,i4,3x,i6,3x,i4,1x,3(1pe13.5,2x))')so_seg_tot-source_b
     &ase_ptr(so_grps)+1,zone,zone_index(3,zone),zone_center(1,zone),zon
     &e_center(2,zone),zone_center(3,zone)
      if(update.EQ.0)then
      if(mod(((so_seg_tot)-(1)+1),100).EQ.1)then
      source_current =>mem_realloc_r1(source_current,(1),(((int(((((so_s
     &eg_tot)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((so_seg_tot)
     &-(1)+1))+100-1)/100)*100)+(1)-1),'source_current')
      end if
      if(mod(((so_seg_tot)-(1)+1),100).EQ.1)then
      source_segment_ptr =>mem_realloc_i1(source_segment_ptr,(1),(((int(
     &((((so_seg_tot)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((so_
     &seg_tot)-(1)+1))+100-1)/100)*100)+(1)-1),'source_segment_ptr')
      end if
      if(mod(((so_seg_tot)-(1)+1),100).EQ.1)then
      source_segment_rel_wt =>mem_realloc_r1(source_segment_rel_wt,(1),(
     &((int(((((so_seg_tot)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int((
     &(((so_seg_tot)-(1)+1))+100-1)/100)*100)+(1)-1),'source_segment_rel
     &_wt')
      end if
      if(mod(((so_seg_tot)-(1)+1),100).EQ.1)then
      source_segment_prob_alias =>mem_realloc_r1(source_segment_prob_ali
     &as,(1),(((int(((((so_seg_tot)-(1)+1))+100-1)/100)*100)-100)+(1)-1)
     &,((int(((((so_seg_tot)-(1)+1))+100-1)/100)*100)+(1)-1),'source_seg
     &ment_prob_alias')
      end if
      if(mod(((so_seg_tot)-(1)+1),100).EQ.1)then
      source_segment_ptr_alias =>mem_realloc_i1(source_segment_ptr_alias
     &,(1),(((int(((((so_seg_tot)-(1)+1))+100-1)/100)*100)-100)+(1)-1),(
     &(int(((((so_seg_tot)-(1)+1))+100-1)/100)*100)+(1)-1),'source_segme
     &nt_ptr_alias')
      end if
      if(source_num_parameters(so_grps).GT.0)then
      so_params_data_size=so_params_data_size+(source_num_parameters(so_
     &grps))
      if(so_params_data_size.GT.so_params_data_dim)then
      source_parameters_data =>mem_realloc_r1(source_parameters_data,(1)
     &,so_params_data_dim,so_params_data_size,'source_parameters_data')
      so_params_data_dim=so_params_data_size
      end if
      end if
      end if
      if(source_real_params(20,iseg).GT.(0.0_DOUBLE).AND.source_real_par
     &ams(20,iseg).NE.(314159265.3589793238462_DOUBLE))continue
      if(source_int_params(5,iseg).GT.0)continue
      mult=(1.0_DOUBLE)/float(source_int_params(5,iseg))
      if(specify_flux.EQ.1)then
      if(zone_volume(zone).GT.(0.0_DOUBLE))continue
      source_current(so_seg_tot)=source_real_params(20,iseg)*zone_volume
     &(zone)*mult
      else
      source_current(so_seg_tot)=source_real_params(20,iseg)*mult
      end if
      source_total_current(so_grps)=source_total_current(so_grps)+(sourc
     &e_current(so_seg_tot))
      if(update.EQ.0)then
      source_segment_ptr(so_seg_tot)=zone
      source_segment_rel_wt(so_seg_tot)=(1.0_DOUBLE)
      else
      if(source_segment_ptr(so_seg_tot).EQ.zone)continue
      end if
      if(source_type(so_grps).EQ.4)then
      do iparam=1,source_num_parameters(so_grps)
      if(source_parameters_list(source_parameters_base(so_grps)+iparam).
     &EQ.4)then
      if((source_real_params(12,iseg).NE.(314159265.3589793238462_DOUBLE
     &)).AND.(source_real_params(12,iseg).GT.(0.0_DOUBLE)))continue
      source_parameters_data(source_parameters_data_base(so_grps)+(so_se
     &g_tot-source_base_ptr(so_grps))*source_num_parameters(so_grps)+ipa
     &ram)=source_real_params(12,iseg)
      else if(source_parameters_list(source_parameters_base(so_grps)+ipa
     &ram).EQ.1)then
      if(source_real_params(15,iseg).NE.(314159265.3589793238462_DOUBLE)
     &)then
      source_parameters_data(source_parameters_data_base(so_grps)+(so_se
     &g_tot-source_base_ptr(so_grps))*source_num_parameters(so_grps)+ipa
     &ram)=source_real_params(15,iseg)
      else
      source_parameters_data(source_parameters_data_base(so_grps)+(so_se
     &g_tot-source_base_ptr(so_grps))*source_num_parameters(so_grps)+ipa
     &ram)=(0.0_DOUBLE)
      end if
      else if(source_parameters_list(source_parameters_base(so_grps)+ipa
     &ram).EQ.2)then
      if(source_real_params(16,iseg).NE.(314159265.3589793238462_DOUBLE)
     &)then
      source_parameters_data(source_parameters_data_base(so_grps)+(so_se
     &g_tot-source_base_ptr(so_grps))*source_num_parameters(so_grps)+ipa
     &ram)=source_real_params(16,iseg)
      else
      source_parameters_data(source_parameters_data_base(so_grps)+(so_se
     &g_tot-source_base_ptr(so_grps))*source_num_parameters(so_grps)+ipa
     &ram)=(0.0_DOUBLE)
      end if
      else if(source_parameters_list(source_parameters_base(so_grps)+ipa
     &ram).EQ.3)then
      if(source_real_params(17,iseg).NE.(314159265.3589793238462_DOUBLE)
     &)then
      source_parameters_data(source_parameters_data_base(so_grps)+(so_se
     &g_tot-source_base_ptr(so_grps))*source_num_parameters(so_grps)+ipa
     &ram)=source_real_params(17,iseg)
      else
      source_parameters_data(source_parameters_data_base(so_grps)+(so_se
     &g_tot-source_base_ptr(so_grps))*source_num_parameters(so_grps)+ipa
     &ram)=(0.0_DOUBLE)
      end if
      end if
      end do
      end if
      end if
      end do
      end do
      if(update.EQ.0)then
      source_num_segments(so_grps)=so_seg_tot-source_base_ptr(so_grps)+1
      else
      if(source_num_segments(so_grps).EQ.so_seg_tot-source_base_ptr(so_g
     &rps)+1)continue
      end if
      else
      if(' This source geometry not handled in defineback'.EQ.' ')contin
     &ue
      end if
      
      else
      if('Unknown keyword'.EQ.line(b:e))continue
      end if
      end if
      goto 90027
90014 continue
      
      nunit_n=20+5
      dummy=(0.0_DOUBLE)
      open(unit=nunit_n,file='density_1.txt',status='unknown')
      write(nunit_n,'(i6.6)')zn_num
      do back=1,bk_num
      nunit_t=nunit_n+1
      write(back_label,'(i1)')back
      open(unit=nunit_t,file='temperature_'//back_label//'.txt',status='
     &unknown')
      write(nunit_t,'(i6.6)')zn_num
      do zone=1,zn_num
      if(zone_type(zone).EQ.2)then
      density=background_n(back,zone_pointer(zone))
      temperature=background_temp(back,zone_pointer(zone))
      else
      density=(0.0_DOUBLE)
      temperature=(0.0_DOUBLE)
      end if
      if(back.EQ.1)then
      write(nunit_n,'(4x,i6,6x,i4,6x,1pe13.5,2x,0pf7.4)')zone,back,densi
     &ty,dummy
      end if
      write(nunit_t,'(4x,i6,6x,i4,6x,1pe13.5,2x,0pf7.4)')zone,back,tempe
     &rature/(1.60217733e-19_DOUBLE),dummy
      end do
      close(unit=nunit_t)
      end do
      close(unit=nunit_n)
      if(update.EQ.0)then
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_base_ptr =>mem_realloc_i1(source_base_ptr,(1),((int(((((so_
     &grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'source_base_ptr')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_num_segments =>mem_realloc_i1(source_num_segments,(1),((int
     &(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'source_nu
     &m_segments')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_type =>mem_realloc_i1(source_type,(1),((int(((((so_grps)-(1
     &)+1))+100-1)/100)*100)+(1)-1),(so_grps),'source_type')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_geometry =>mem_realloc_i1(source_geometry,(1),((int(((((so_
     &grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'source_geometry')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_num_flights =>mem_realloc_i1(source_num_flights,(1),((int((
     &(((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'source_num_
     &flights')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_num_checkpoints =>mem_realloc_i1(source_num_checkpoints,(1)
     &,((int(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'sou
     &rce_num_checkpoints')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_species =>mem_realloc_i1(source_species,(1),((int(((((so_gr
     &ps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'source_species')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_root_species =>mem_realloc_i1(source_root_species,(1),((int
     &(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'source_ro
     &ot_species')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_time_variation =>mem_realloc_i1(source_time_variation,(1),(
     &(int(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'sourc
     &e_time_variation')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_num_gparameters =>mem_realloc_i1(source_num_gparameters,(1)
     &,((int(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'sou
     &rce_num_gparameters')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_num_parameters =>mem_realloc_i1(source_num_parameters,(1),(
     &(int(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'sourc
     &e_num_parameters')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_gparameters_base =>mem_realloc_i1(source_gparameters_base,(
     &1),((int(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'s
     &ource_gparameters_base')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_parameters_base =>mem_realloc_i1(source_parameters_base,(1)
     &,((int(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'sou
     &rce_parameters_base')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_parameters_data_base =>mem_realloc_i1(source_parameters_dat
     &a_base,(1),((int(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_
     &grps),'source_parameters_data_base')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_num_giparameters =>mem_realloc_i1(source_num_giparameters,(
     &1),((int(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'s
     &ource_num_giparameters')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_num_iparameters =>mem_realloc_i1(source_num_iparameters,(1)
     &,((int(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'sou
     &rce_num_iparameters')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_giparameters_base =>mem_realloc_i1(source_giparameters_base
     &,(1),((int(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),
     &'source_giparameters_base')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_iparameters_base =>mem_realloc_i1(source_iparameters_base,(
     &1),((int(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'s
     &ource_iparameters_base')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_iparameters_data_base =>mem_realloc_i1(source_iparameters_d
     &ata_base,(1),((int(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(s
     &o_grps),'source_iparameters_data_base')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_total_current =>mem_realloc_r1(source_total_current,(1),((i
     &nt(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'source_
     &total_current')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_weight_norm =>mem_realloc_r1(source_weight_norm,(1),((int((
     &(((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'source_weig
     &ht_norm')
      end if
      if(mod(((so_grps)-(1)+1),100).NE.0)then
      source_scale_factor =>mem_realloc_r1(source_scale_factor,(1),((int
     &(((((so_grps)-(1)+1))+100-1)/100)*100)+(1)-1),(so_grps),'source_sc
     &ale_factor')
      end if
      end if
      if(mod(((dim_segments)-(1)+1),100).NE.0)then
      source_int_params =>mem_realloc_i2(source_int_params,(1),(6),(1),(
     &(int(((((dim_segments)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_segmen
     &ts),'source_int_params')
      end if
      if(mod(((dim_segments)-(1)+1),100).NE.0)then
      source_real_params =>mem_realloc_r2(source_real_params,(11),(24+15
     &-1),(1),((int(((((dim_segments)-(1)+1))+100-1)/100)*100)+(1)-1),(d
     &im_segments),'source_real_params')
      end if
      if(so_seg_tot.EQ.0)then
      if('No sources were defined!'.EQ.' ')continue
      end if
      if(update.EQ.0)then
      call set_background_sources
      if(so_time_dependent.EQ.1)then
      standalone=1
      call set_snapshot_source(standalone)
      end if
      else
      if(update.EQ.1)continue
      if(iterative.EQ.1)continue
      if(pr_bkrc_num.GT.0)then
      do jr=1,pr_bkrc_num
      so_grps=so_grps+1
      so_seg_tot=source_base_ptr(so_grps)-1
      call set_background_rate(jr,so_grps,update)
      end do
      end if
      end if
      if(update.EQ.0)then
      call init_wt_alias(iterative)
      else
      if(iterative.EQ.1)continue
      if(so_grps.EQ.old_grps)continue
      if(so_seg_tot.EQ.old_seg_tot)continue
      call update_wt_alias(old_current,old_rel_wt,old_tot_curr)
      call mem_free_r1(old_current,(1),(so_seg_tot),'old_current')
      call mem_free_r1(old_tot_curr,(1),(so_grps),'old_tot_curr')
      call mem_free_r1(old_rel_wt,(1),(so_seg_tot),'old_rel_wt')
      end if
      call write_background
      call mem_free_i2(source_int_params,(1),(6),(1),(dim_segments),'sou
     &rce_int_params')
      call mem_free_r2(source_real_params,(11),(24+15-1),(1),(dim_segmen
     &ts),'source_real_params')
      return
      end
      subroutine parse_format(format_string,bk_num,format_code,count)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*300 format_string
      integer bk_num
      integer count
      integer format_code(300)
      integer p,b,e,length,b_repeat,b_code,bi,sub_count,i,j
      character*300 line
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
      line=format_string
      length=parse_string(line)
      p=0
      count=0
      b_repeat=0
      if(bk_num.LT.10)continue
90026 continue
      if(.NOT.next_token(line,b,e,p))goto 90004
      count=count+1
      if((line(b:e).EQ.'zone').OR.(line(b:e).EQ.'ZONE'))then
      format_code(count)=10
      else
      if((line(b:b).EQ.'n').OR.(line(b:b).EQ.'N'))then
      format_code(count)=11
      else if((line(b:b).EQ.'t').OR.(line(b:b).EQ.'T'))then
      format_code(count)=12
      else if((line(b:b).EQ.'v').OR.(line(b:b).EQ.'V'))then
      if(line(b+1:b+1).EQ.'1')then
      format_code(count)=13
      else if(line(b+1:b+1).EQ.'2')then
      format_code(count)=14
      else if(line(b+1:b+1).EQ.'3')then
      format_code(count)=15
      else
      write(0,*)' Illegal format token, ',line(b:e)
      if(.FALSE.)continue
      end if
      else
      write(0,*)' Illegal format token, ',line(b:e)
      if(.FALSE.)continue
      end if
      count=count+1
      b_code=index(line(b:e),'(')+1
      if(b_code.GT.1)continue
      b_code=b+b_code-1
      if(line(b_code:b_code).EQ.'b'.OR.line(b_code:b_code).EQ.'B')then
      b_repeat=1
      format_code(count)=1
      else
      if(b_repeat.EQ.0)continue
      bi=read_int_soft_fail(line(b_code:b_code))
      if(bi.EQ.1000000000)then
      write(0,*)' Bad background code in token ',line(b:e)
      if(.FALSE.)continue
      end if
      format_code(count)=bi
      end if
      end if
      goto 90026
90004 continue
      if(count.GT.0)continue
      if((b_repeat.EQ.1).AND.(bk_num.GT.1))then
      sub_count=count
      do i=2,bk_num
      do j=1,sub_count
      if((format_code(j).EQ.11).OR.(format_code(j).EQ.12).OR.(format_cod
     &e(j).EQ.13).OR.(format_code(j).EQ.14).OR.(format_code(j).EQ.15))th
     &en
      count=count+1
      format_code(count)=format_code(j)
      else if(format_code(j).NE.10)then
      count=count+1
      format_code(count)=i
      end if
      end do
      end do
      end if
      return
      end
      subroutine get_n_t_row(nt_string)
      
      
      
      
      
      use bk_mod
      
      use zn_mod
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*96 nt_string
      integer p,b,e,length,data_code,count,v_index,open_stat,diskin3,ite
     &st,dim_y,iy,symmetric_plasma
      REAL(kind=DOUBLE)temperature_mult,rtest
      character*300 line
      character*96 plasma_file
      integer zone
      integer iy_zone
      integer back
      integer,dimension(:),pointer::zone_list
      integer,dimension(:),pointer::n_iy
      integer,dimension(:),pointer::zone_iy_map
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
      logical check_zone
      zone_list =>mem_alloc_i1((1),(zn_num),'zone_list')
      n_iy =>mem_alloc_i1((1),(zn_num),'n_iy')
      dim_y=1
      do zone=1,zn_num
      zone_list(zone)=1000000000
      n_iy(zone)=0
      end do
      do zone=1,zn_num
      if(zone_index(4,zone).NE.zone)then
      n_iy(zone_index(4,zone))=n_iy(zone_index(4,zone))+1
      dim_y=max(n_iy(zone_index(4,zone)),dim_y)
      n_iy(zone)=n_iy(zone_index(4,zone))
      end if
      end do
      if((dim_y.EQ.1).OR.(geometry_symmetry.EQ.4).OR.(geometry_symmetry.
     &EQ.5).OR.(geometry_symmetry.EQ.6))continue
      zone_iy_map =>mem_alloc_i1((1),(dim_y*zn_num),'zone_iy_map')
      do zone=1,zn_num
      do iy=1,dim_y
      zone_iy_map((zone-1)*dim_y+iy)=1000000000
      end do
      end do
      if(dim_y.GT.1)then
      do zone=1,zn_num
      if(zone_index(4,zone).NE.zone)then
      if((n_iy(zone).GT.0).AND.(n_iy(zone).LE.dim_y))continue
      zone_iy_map((zone_index(4,zone)-1)*dim_y+n_iy(zone))=zone
      end if
      end do
      end if
      p=0
      if(next_token(nt_string,b,e,p))continue
      plasma_file=nt_string(b:e)
      if(next_token(nt_string,b,e,p))continue
      if(nt_string(b:e).EQ.'row')continue
      if(next_token(nt_string,b,e,p))then
      if(nt_string(b:e).EQ.'2D')then
      symmetric_plasma=1
      else if(nt_string(b:e).EQ.'3D')then
      symmetric_plasma=0
      else
      if(' Unexpected plasma symmetry specification '.EQ.nt_string(b:e))
     &continue
      end if
      else
      symmetric_plasma=1
      end if
      diskin3=20+2
      open(unit=diskin3,file=plasma_file,status='old',form='formatted',i
     &ostat=open_stat)
      if(open_stat.EQ.0)continue
      data_code=0
      count=0
90027 continue
      if(read_string(diskin3,line,length))then
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      itest=read_int_soft_fail(line(b:e))
      rtest=read_real_soft_fail(line(b:e))
      if((itest.EQ.1000000000).AND.(rtest.EQ.(1.0e30_DOUBLE)))then
      call parse_label(line,data_code,back)
      if(data_code.NE.0)continue
      count=0
      goto 90027
      else
      if(data_code.NE.0)continue
      if(data_code.EQ.1)then
90028 continue
      count=count+1
      zone_list(count)=read_integer(line(b:e))
      if(check_zone(zone_list(count)))continue
      if(zone_type(zone_list(count)).EQ.2)continue
      if((zone_index(4,zone_list(count)).EQ.zone_list(count)).OR.(symmet
     &ric_plasma.EQ.0))continue
      if(next_token(line,b,e,p))then
      goto 90028
      else
      goto 90027
      end if
      else if(data_code.EQ.11)then
90029 continue
      count=count+1
      if(check_zone(zone_list(count)))then
      zone=zone_list(count)
      else
      zone=count
      end if
      if(check_zone(zone))continue
      if((zone_index(4,zone).EQ.zone).OR.(symmetric_plasma.EQ.0))continu
     &e
      if(zone_type(zone).EQ.2)continue
      if((back.GT.0.AND.back.LE.bk_num))continue
      background_n(back,zone_pointer(zone))=read_real(line(b:e))
      if((dim_y.GT.1).AND.(symmetric_plasma.EQ.1))then
      do iy=1,dim_y
      iy_zone=zone_iy_map((zone-1)*dim_y+iy)
      if((check_zone(iy_zone)).AND.(zone_type(iy_zone).EQ.2))then
      background_n(back,zone_pointer(iy_zone))=background_n(back,zone_po
     &inter(zone))
      end if
      end do
      end if
      if(next_token(line,b,e,p))then
      goto 90029
      else
      goto 90027
      end if
      else if((data_code.EQ.12).OR.(data_code.EQ.13))then
90030 continue
      count=count+1
      if(check_zone(zone_list(count)))then
      zone=zone_list(count)
      else
      zone=count
      end if
      if(check_zone(zone))continue
      if((zone_index(4,zone).EQ.zone).OR.(symmetric_plasma.EQ.0))continu
     &e
      if(zone_type(zone).EQ.2)continue
      if((back.GT.0.AND.back.LE.bk_num))continue
      if(data_code.EQ.13)then
      temperature_mult=(1.60217733e-19_DOUBLE)
      else
      temperature_mult=(1.0_DOUBLE)
      end if
      background_temp(back,zone_pointer(zone))=read_real(line(b:e))*temp
     &erature_mult
      if((dim_y.GT.1).AND.(symmetric_plasma.EQ.1))then
      do iy=1,dim_y
      iy_zone=zone_iy_map((zone-1)*dim_y+iy)
      if((check_zone(iy_zone)).AND.(zone_type(iy_zone).EQ.2))then
      background_temp(back,zone_pointer(iy_zone))=background_temp(back,z
     &one_pointer(zone))
      end if
      end do
      end if
      if(next_token(line,b,e,p))then
      goto 90030
      else
      goto 90027
      end if
      else if((data_code.EQ.15).OR.(data_code.EQ.16).OR.(data_code.EQ.17
     &))then
90031 continue
      count=count+1
      if(check_zone(zone_list(count)))then
      zone=zone_list(count)
      else
      zone=count
      end if
      if(check_zone(zone))continue
      if((zone_index(4,zone).EQ.zone).OR.(symmetric_plasma.EQ.0))continu
     &e
      if(zone_type(zone).EQ.2)continue
      if((back.GT.0.AND.back.LE.bk_num))continue
      if(data_code.EQ.15)then
      v_index=1
      else if(data_code.EQ.16)then
      v_index=2
      else
      v_index=3
      end if
      if((v_index.GE.1).AND.(v_index.LE.3))continue
      background_v(v_index,back,zone_pointer(zone))=read_real(line(b:e))
      if((dim_y.GT.1).AND.(symmetric_plasma.EQ.1))then
      do iy=1,dim_y
      iy_zone=zone_iy_map((zone-1)*dim_y+iy)
      if((check_zone(iy_zone)).AND.(zone_type(iy_zone).EQ.2))then
      background_v(v_index,back,zone_pointer(iy_zone))=background_v(iy_z
     &one,back,zone_pointer(zone))
      end if
      end do
      end if
      if(next_token(line,b,e,p))then
      goto 90031
      else
      goto 90027
      end if
      else
      if(' Unexpected value of data_code'.EQ.' ')continue
      end if
      end if
      goto 90027
      end if
      close(unit=diskin3)
      return
      end
      subroutine parse_label(line,data_code,back)
      
      use bk_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*300 line
      integer data_code
      integer back
      integer p,b,e,p2,b2,e2,b_code
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
      data_code=0
      back=1000000000
      p=0
      if(next_token(line,b,e,p))continue
      if((line(b:e).EQ.'zone').OR.(line(b:e).EQ.'ZONE').OR.(line(b:e).EQ
     &.'Zone'))then
      data_code=1
      back=1000000000
      else if((line(b:e).EQ.'stratum').OR.(line(b:e).EQ.'STRATUM').OR.(l
     &ine(b:e).EQ.'Stratum'))then
      data_code=2
      back=1000000000
      else if((line(b:e).EQ.'segment').OR.(line(b:e).EQ.'SEGMENT').OR.(l
     &ine(b:e).EQ.'Segment'))then
      data_code=3
      back=1000000000
      else if((line(b:e).EQ.'segment_iy').OR.(line(b:e).EQ.'SEGMENT_IY')
     &.OR.(line(b:e).EQ.'Segment_iy'))then
      data_code=4
      back=1000000000
      else if((line(b:e).EQ.'area').OR.(line(b:e).EQ.'AREA').OR.(line(b:
     &e).EQ.'Area'))then
      data_code=19
      back=1000000000
      else if((line(b:e).EQ.'e_bin_prob').OR.(line(b:e).EQ.'E_BIN_PROB')
     &.OR.(line(b:e).EQ.'E_bin_prob'))then
      data_code=24
      back=1000000000
      else if((line(b:b).EQ.'n').OR.(line(b:b).EQ.'N'))then
      data_code=11
      else if((line(b:b).EQ.'f').OR.(line(b:b).EQ.'F'))then
      data_code=20
      else if((line(b:b).EQ.'t').OR.(line(b:b).EQ.'T'))then
      data_code=13
      p2=p
      if(next_token(line,b2,e2,p2))then
      if(line(b2:e2).EQ.'J')then
      data_code=12
      else if(line(b2:e2).EQ.'eV')then
      data_code=13
      else
      write(0,*)' Unexpected token, ',line(b2:e2)
      if(.FALSE.)continue
      end if
      end if
      else if((line(b:b).EQ.'v').OR.(line(b:b).EQ.'V'))then
      if(line(b+1:b+1).EQ.'1')then
      data_code=15
      else if(line(b+1:b+1).EQ.'2')then
      data_code=16
      else if(line(b+1:b+1).EQ.'3')then
      data_code=17
      else if((line(b+1:b+4).EQ.'_par').OR.(line(b+1:b+4).EQ.'_PAR').OR.
     &(line(b+1:b+4).EQ.'_Par'))then
      data_code=18
      else
      write(0,*)' Illegal format token, ',line(b:e)
      if(.FALSE.)continue
      end if
      end if
      if(data_code.NE.0)then
      b_code=index(line(b:e),'(')+1
      if(b_code.GT.1)then
      b_code=b+b_code-1
      back=read_int_soft_fail(line(b_code:b_code))
      if(bk_num.LT.10)continue
      end if
      end if
      return
      end
      
      
