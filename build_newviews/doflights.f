      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine do_flights_master(io_seed)
      
      
      
      
      
      
      use zn_mod
      
      use sp_mod
      
      use so_mod
      
      use pr_mod
      
      use de_mod
      
      use tl_mod
      
      use ou_mod
      
      use sa_mod
      
      use mp_mod
      
      use sn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer io_seed(0:8-1)
      integer i,j,is,test,new_dim,pr_reac,jscore,iview,det,ibin,iauth,js
     &core_detail,jscore_total,icp,num_chkpts,group_tot_flights,nflights
     &,nfrag,nparcel,num,nstart,nslaves,slave,send_data,post_arrays_allo
     &cd,isp,ip,last_sn_num_particles,have_snapshot_file
      integer index_parameters(100),time(8)
      REAL(kind=DOUBLE)mult,tot_scale_curr,prob,signal,wavelength,avg_pa
     &rt_per_sec
      REAL(kind=DOUBLE)sig_det(30)
      logical need_scores
      character*30 date_str
      integer seed(0:8-1)
      integer temp_seed(0:8-1)
      integer ran_index_temp_rand
      REAL(kind=DOUBLE)ran_array_temp_rand(0:100-1)
      REAL(kind=DOUBLE)random
      external random
      REAL(kind=DOUBLE)extract_output_datum
      external extract_output_datum
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
      include 'mpif.h'
      integer mpi_err
      integer mpi_status
      dimension mpi_status(MPI_STATUS_SIZE)
      integer,dimension(:),pointer::chkpt_grp
      integer,dimension(:),pointer::chkpt_write
      integer,dimension(:),pointer::chkpt_nflights
      REAL(kind=DOUBLE),dimension(:),pointer::estimator_factors
      estimator_factors =>mem_alloc_r1((1),(tl_num),'estimator_factors')
      if(10.GE.pr_background_num)continue
      if(so_restart.EQ.0)then
      output_grp =>mem_alloc_r3((0),(1),(0),(tally_size-1),(1),(so_grps)
     &,'output_grp')
      output_weight_grp =>mem_alloc_r1((1),(so_grps),'output_weight_grp'
     &)
      output_num_flights =>mem_alloc_i1((1),(so_grps),'output_num_flight
     &s')
      output_random_seed =>mem_alloc_i2((0),(8-1),(1),(so_grps),'output_
     &random_seed')
      post_arrays_allocd=0
      output_random_seed(0,1)=io_seed(0)
      output_random_seed(1,1)=io_seed(1)
      output_random_seed(2,1)=io_seed(2)
      output_random_seed(3,1)=io_seed(3)
      output_random_seed(4,1)=io_seed(4)
      output_random_seed(5,1)=io_seed(5)
      output_random_seed(6,1)=io_seed(6)
      output_random_seed(7,1)=io_seed(7)
      
      if(so_spaced_seeds.EQ.1.AND.so_grps.GT.1)then
      do is=2,so_grps
      output_random_seed(0,is)=output_random_seed(0,is-1)
      output_random_seed(1,is)=output_random_seed(1,is-1)
      output_random_seed(2,is)=output_random_seed(2,is-1)
      output_random_seed(3,is)=output_random_seed(3,is-1)
      output_random_seed(4,is)=output_random_seed(4,is-1)
      output_random_seed(5,is)=output_random_seed(5,is-1)
      output_random_seed(6,is)=output_random_seed(6,is-1)
      output_random_seed(7,is)=output_random_seed(7,is-1)
      
      call next_seed(so_seed_spacing,output_random_seed(0,is))
      end do
      end if
      do is=1,so_grps
      output_weight_grp(is)=(0.0_DOUBLE)
      output_num_flights(is)=0
      do i=0,tally_size-1
      output_grp(0,i,is)=(0.0_DOUBLE)
      output_grp(1,i,is)=(0.0_DOUBLE)
      end do
      end do
      
      output_index_1_min=zone_index_min(1)
      output_index_1_max=zone_index_max(1)
      output_index_2_min=zone_index_min(2)
      output_index_2_max=zone_index_max(2)
      sn_number_particles=0
      sn_particles_dim=100
      sn_particles_float =>mem_alloc_r2((1),(8),(1),(sn_particles_dim),'
     &sn_particles_float')
      sn_particles_int =>mem_alloc_i2((1),(11),(1),(sn_particles_dim),'s
     &n_particles_int')
      else if(so_restart.EQ.1)then
      if(output_old_file.EQ.1)continue
      post_arrays_allocd=1
      call nc_read_snapshot(have_snapshot_file)
      if(have_snapshot_file.EQ.0)then
      sn_number_particles=0
      sn_particles_dim=100
      sn_particles_float =>mem_alloc_r2((1),(8),(1),(sn_particles_dim),'
     &sn_particles_float')
      sn_particles_int =>mem_alloc_i2((1),(11),(1),(sn_particles_dim),'s
     &n_particles_int')
      end if
      else
      if('Illegal value of so_restart'.EQ.' ')continue
      end if
      if(so_sampling.EQ.1)then
      call decimal_to_seed(so_seed_decimal,temp_seed(0))
      call next_seed(-1,temp_seed(0))
      call random_init_d2(temp_seed(0),ran_index_temp_rand,ran_array_tem
     &p_rand(0))
      so_direct_delta=random(ran_index_temp_rand,ran_array_temp_rand(0))
      else if(so_sampling.EQ.0)then
      so_direct_delta=(0.0_DOUBLE)
      else
      if('Illegal value of so_sampling'.EQ.' ')continue
      end if
      call MPI_bcast(so_direct_delta,1,MPI_DOUBLE_PRECISION,mpi_degas2_r
     &oot,comm_world_dup,mpi_err)
      stat_comp_flt=1
      stat_comp_frag=1
      stat_comp_fin=0
      call stat_init
      output_checkpoint=1
      tot_scale_curr=(0.0_DOUBLE)
      num_chkpts=0
      if(mod(((num_chkpts)-(1)+1),100).EQ.1)then
      chkpt_grp =>mem_realloc_i1(chkpt_grp,(1),(((int(((((num_chkpts)-(1
     &)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((num_chkpts)-(1)+1))+1
     &00-1)/100)*100)+(1)-1),'chkpt_grp')
      end if
      if(mod(((num_chkpts)-(1)+1),100).EQ.1)then
      chkpt_write =>mem_realloc_i1(chkpt_write,(1),(((int(((((num_chkpts
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((num_chkpts)-(1)+1
     &))+100-1)/100)*100)+(1)-1),'chkpt_write')
      end if
      if(mod(((num_chkpts)-(1)+1),100).EQ.1)then
      chkpt_nflights =>mem_realloc_i1(chkpt_nflights,(1),(((int(((((num_
     &chkpts)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((num_chkpts)
     &-(1)+1))+100-1)/100)*100)+(1)-1),'chkpt_nflights')
      end if
      do is=1,so_grps
      group_tot_flights=0
      if(source_num_flights(is).GT.output_num_flights(is))then
      do icp=1,max(source_num_checkpoints(is),1)
      num_chkpts=num_chkpts+1
      if(mod(((num_chkpts)-(1)+1),100).EQ.1)then
      chkpt_grp =>mem_realloc_i1(chkpt_grp,(1),(((int(((((num_chkpts)-(1
     &)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((num_chkpts)-(1)+1))+1
     &00-1)/100)*100)+(1)-1),'chkpt_grp')
      end if
      if(mod(((num_chkpts)-(1)+1),100).EQ.1)then
      chkpt_write =>mem_realloc_i1(chkpt_write,(1),(((int(((((num_chkpts
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((num_chkpts)-(1)+1
     &))+100-1)/100)*100)+(1)-1),'chkpt_write')
      end if
      if(mod(((num_chkpts)-(1)+1),100).EQ.1)then
      chkpt_nflights =>mem_realloc_i1(chkpt_nflights,(1),(((int(((((num_
     &chkpts)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((num_chkpts)
     &-(1)+1))+100-1)/100)*100)+(1)-1),'chkpt_nflights')
      end if
      chkpt_grp(num_chkpts)=is
      if(source_num_checkpoints(is).EQ.0)then
      chkpt_write(num_chkpts)=0
      else
      chkpt_write(num_chkpts)=1
      end if
      if(icp.LT.max(source_num_checkpoints(is),1))then
      chkpt_nflights(num_chkpts)=(source_num_flights(is)-output_num_flig
     &hts(is))/max(source_num_checkpoints(is),1)
      group_tot_flights=group_tot_flights+(chkpt_nflights(num_chkpts))
      else
      chkpt_nflights(num_chkpts)=source_num_flights(is)-output_num_fligh
     &ts(is)-group_tot_flights
      end if
      end do
      else
      tot_scale_curr=tot_scale_curr+(source_scale_factor(is)*source_tota
     &l_current(is))
      end if
      end do
      if(mod(((num_chkpts)-(1)+1),100).NE.0)then
      chkpt_grp =>mem_realloc_i1(chkpt_grp,(1),((int(((((num_chkpts)-(1)
     &+1))+100-1)/100)*100)+(1)-1),(num_chkpts),'chkpt_grp')
      end if
      if(mod(((num_chkpts)-(1)+1),100).NE.0)then
      chkpt_write =>mem_realloc_i1(chkpt_write,(1),((int(((((num_chkpts)
     &-(1)+1))+100-1)/100)*100)+(1)-1),(num_chkpts),'chkpt_write')
      end if
      if(mod(((num_chkpts)-(1)+1),100).NE.0)then
      chkpt_nflights =>mem_realloc_i1(chkpt_nflights,(1),((int(((((num_c
     &hkpts)-(1)+1))+100-1)/100)*100)+(1)-1),(num_chkpts),'chkpt_nflight
     &s')
      end if
      is=0
      do icp=1,num_chkpts
      nflights=chkpt_nflights(icp)
      last_sn_num_particles=sn_number_particles
      if(is.NE.chkpt_grp(icp))then
      is=chkpt_grp(icp)
      seed(0)=output_random_seed(0,is)
      seed(1)=output_random_seed(1,is)
      seed(2)=output_random_seed(2,is)
      seed(3)=output_random_seed(3,is)
      seed(4)=output_random_seed(4,is)
      seed(5)=output_random_seed(5,is)
      seed(6)=output_random_seed(6,is)
      seed(7)=output_random_seed(7,is)
      
      if(so_spaced_seeds.EQ.0.OR.source_num_flights(is).LT.so_seed_spaci
     &ng)continue
      if(stat_comp_fin.EQ.0)continue
      do i=0,stat_wt_dim_fin-1
      stat_wt_fin(i)=output_weight_grp(is)
      end do
      end if
      nslaves=mpi_size-1
      nfrag=max(nflights/max(100,10*nslaves),1)
      nparcel=(nflights+nfrag-1)/nfrag
      do j=0,min(nparcel,nslaves)-1
      nstart=j*nfrag
      num=min(nfrag,nflights-nstart)
      slave=j+1
      call slave_send_flights(slave,output_num_flights(is)+nstart,num,is
     &,seed)
      call next_seed(num,seed)
      end do
      do j=min(nparcel,nslaves),nparcel-1
      nstart=j*nfrag
      num=min(nfrag,nflights-nstart)
      
      if(nslaves.GT.0)then
      call slave_receive_flag(slave)
      send_data=0
      call slave_send_flag(slave,send_data)
      call slave_send_flights(slave,output_num_flights(is)+nstart,num,is
     &,seed)
      else
      if('Why is nslaves = 0 in an MPI run?'.EQ.' ')continue
      call setup_flight_frag(output_num_flights(is)+nstart,num,is,seed)
      call do_flights(estimator_factors)
      end if
      call next_seed(num,seed)
      end do
      do j=0,min(nparcel,nslaves)-1
      call slave_receive_flag(slave)
      if(nslaves.LT.nparcel)then
      send_data=1
      else
      send_data=2
      end if
      call slave_send_flag(slave,send_data)
      end do
      do j=1,min(nparcel,nslaves)
      if(mpi_slave_map((mpi_nlevels-1)*mpi_nslaves+j).EQ.mpi_degas2_root
     &)then
      call slave_receive_flights(slave)
      
      if(stat_comp_frag.EQ.1)call stat_wt_balance(stat_wt_tot_frag,stat_
     &size_frag,stat_dim_frag,stat_frag,stat_wt_dim_frag,stat_wt_frag)
      if(min(stat_size_fin+stat_size_frag,tally_size).GT.stat_dim_fin)th
     &en
      if(stat_comp_fin.EQ.1)continue
      mult=max((1.0_DOUBLE),REAL(stat_size_frag,DOUBLE)/REAL(15000,DOUBL
     &E))
      new_dim=min(stat_dim_fin+int(mult*15000),tally_size)
      stat_fin =>mem_realloc_r2(stat_fin,(0),(1),(0),stat_dim_fin-1,new_
     &dim-1,'stat_fin')
      stat_ptr2full_fin =>mem_realloc_i1(stat_ptr2full_fin,(0),stat_pf_d
     &im_fin-1,new_dim-1,'stat_ptr2full_fin')
      stat_wt_fin =>mem_realloc_r1(stat_wt_fin,(0),stat_wt_dim_fin-1,new
     &_dim-1,'stat_wt_fin')
      stat_dim_fin=new_dim
      stat_pf_dim_fin=new_dim
      stat_wt_dim_fin=new_dim
      end if
      if(stat_comp_fin.EQ.0.AND.stat_dim_fin.EQ.tally_size)continue
      if(stat_wt_tot_frag.LT.(0.0_DOUBLE))then
      write(0,*)' Bad value of stat_wt_tot_frag = ',stat_wt_tot_frag
      write(0,*)' slave = ',slave
      if(stat_wt_tot_frag.GE.(0.0_DOUBLE))continue
      call flush(0)
      end if
      call stat_acc(stat_comp_frag,stat_wt_tot_frag,stat_size_frag,stat_
     &dim_frag,stat_frag,stat_pf_dim_frag,stat_ptr2full_frag,stat_ps_dim
     &_frag,stat_ptr2short_frag,stat_comp_fin,output_weight_grp(is),stat
     &_size_fin,stat_dim_fin,output_grp(0,0,is),stat_pf_dim_fin,stat_ptr
     &2full_fin,stat_ps_dim_fin,stat_ptr2short_fin,stat_wt_dim_fin,stat_
     &wt_fin)
      
      end if
      end do
      if(sn_number_particles.GT.last_sn_num_particles)then
      do ip=last_sn_num_particles+1,sn_number_particles
      sn_particles_float(2,ip)=sn_particles_float(2,ip)*(source_scale_fa
     &ctor(is)*source_total_current(is)/output_weight_grp(is))
      end do
      end if
      if(stat_comp_frag.EQ.1)call stat_wt_balance(output_weight_grp(is),
     &stat_size_fin,stat_dim_fin,output_grp(0,0,is),stat_wt_dim_fin,stat
     &_wt_fin)
      if(stat_comp_fin.EQ.1)call stat_decomp(stat_size_fin,stat_dim_fin,
     &stat_fin,stat_ps_dim_fin,stat_ptr2short_fin,tally_size,output_grp(
     &0,0,is))
      
      tot_scale_curr=tot_scale_curr+(source_scale_factor(is)*source_tota
     &l_current(is))
      output_num_flights(is)=output_num_flights(is)+(nflights)
      if(so_spaced_seeds.EQ.1)then
      output_random_seed(0,is)=seed(0)
      output_random_seed(1,is)=seed(1)
      output_random_seed(2,is)=seed(2)
      output_random_seed(3,is)=seed(3)
      output_random_seed(4,is)=seed(4)
      output_random_seed(5,is)=seed(5)
      output_random_seed(6,is)=seed(6)
      output_random_seed(7,is)=seed(7)
      
      else
      do isp=1,so_grps
      output_random_seed(0,isp)=seed(0)
      output_random_seed(1,isp)=seed(1)
      output_random_seed(2,isp)=seed(2)
      output_random_seed(3,isp)=seed(3)
      output_random_seed(4,isp)=seed(4)
      output_random_seed(5,isp)=seed(5)
      output_random_seed(6,isp)=seed(6)
      output_random_seed(7,isp)=seed(7)
      
      end do
      end if
      if(so_time_dependent.EQ.1)call seed_to_decimal(seed,sn_seed_decima
     &l)
      if(post_arrays_allocd.EQ.0.AND.(chkpt_write(icp).EQ.1.OR.icp.EQ.nu
     &m_chkpts))then
      output_all =>mem_alloc_r2((0),(1),(0),(tally_size-1),'output_all')
      out_post_grp =>mem_alloc_r3((0),(1),(0),(tally_size-1),(1),(so_grp
     &s),'out_post_grp')
      out_post_all =>mem_alloc_r2((0),(1),(0),(tally_size-1),'out_post_a
     &ll')
      output_2D_coupling =>mem_alloc_r5((output_index_1_min),(output_ind
     &ex_1_max),(output_index_2_min),(output_index_2_max),(1),(pr_backgr
     &ound_num),(1),(5),(1),(so_grps),'output_2D_coupling')
      post_arrays_allocd=1
      end if
      if(icp.LT.num_chkpts.AND.chkpt_write(icp).EQ.1)call nc_write_outpu
     &t
      end do
      do i=0,tally_size-1
      output_all(0,i)=(0.0_DOUBLE)
      output_all(1,i)=(0.0_DOUBLE)
      end do
      do is=1,so_grps
      do i=0,tally_size-1
      prob=source_scale_factor(is)*source_total_current(is)/tot_scale_cu
     &rr
      out_post_grp(0,i,is)=output_grp(0,i,is)*prob
      output_all(0,i)=output_all(0,i)+(out_post_grp(0,i,is))
      if(output_weight_grp(is).GT.(0.0_DOUBLE))then
      out_post_grp(1,i,is)=output_grp(1,i,is)*prob**2/(output_weight_grp
     &(is)-(1.0_DOUBLE))
      else
      if(output_num_flights(is).EQ.0.AND.output_grp(1,i,is).EQ.(0.0_DOUB
     &LE))continue
      out_post_grp(1,i,is)=(0.0_DOUBLE)
      end if
      output_all(1,i)=output_all(1,i)+(out_post_grp(1,i,is))
      end do
      end do
      if(so_time_dependent.EQ.1)then
      avg_part_per_sec=tot_scale_curr/(so_time_final-so_time_initial)
      else
      avg_part_per_sec=tot_scale_curr
      end if
      do i=0,tally_size-1
      output_all(1,i)=sqrt(output_all(1,i))/max(abs(output_all(0,i)),(1.
     &0e-100_DOUBLE))
      output_all(0,i)=output_all(0,i)*(avg_part_per_sec)
      do is=1,so_grps
      out_post_grp(1,i,is)=sqrt(out_post_grp(1,i,is))/max(abs(out_post_g
     &rp(0,i,is)),(1.0e-100_DOUBLE))
      out_post_grp(0,i,is)=out_post_grp(0,i,is)*(avg_part_per_sec)
      end do
      end do
      stat_comp_flt=0
      if(stat_dim_flt.LT.tally_size)then
      stat_flt =>mem_realloc_r2(stat_flt,(0),(1),(0),stat_dim_flt-1,tall
     &y_size-1,'stat_flt')
      stat_dim_flt=tally_size
      end if
      stat_size_flt=tally_size
      
      need_scores=.FALSE.
      do i=1,tally_type_num(3)
      do pr_reac=1,pr_reaction_num+6
      jscore=tally_type_base(3)+i-1
      if(tally_est_reaction(pr_reac,3,jscore).GT.(0.0_DOUBLE))then
      need_scores=.TRUE.
      end if
      end do
      end do
      do is=1,so_grps
      do i=0,tally_size-1
      stat_flt(0,i)=out_post_grp(0,i,is)
      end do
      if(stat_size_flt.EQ.tally_size)continue
      call final_conversions(2,stat_flt)
      if(need_scores)then
      call post_process_test_scores(is,estimator_factors)
      call post_process_source_scores(is,estimator_factors)
      end if
      call final_conversions(3,stat_flt)
      do i=0,tally_size-1
      out_post_grp(0,i,is)=stat_flt(0,i)
      if(is.EQ.1)then
      out_post_all(0,i)=stat_flt(0,i)
      out_post_all(1,i)=output_all(1,i)
      else
      out_post_all(0,i)=out_post_all(0,i)+(stat_flt(0,i))
      end if
      end do
      end do
      do j=0,nslaves-1
      slave=j+1
      call slave_send_flights(slave,0,0,0,seed)
      end do
      call fill_coupling_arrays
      do test=2,pr_test_num
      jscore=string_lookup(species_sy(problem_test_sp(test)) (1:string_l
     &ength(species_sy(problem_test_sp(test))))//'alpha chord integrals'
     &,tally_name,tl_num)
      if(jscore.GT.0)then
      open(unit=31,file='chords_'//species_sy(problem_test_sp(test)) (1:
     &string_length(species_sy(problem_test_sp(test))))//'_alpha.out',st
     &atus='unknown')
      if(tally_rank(jscore).EQ.1)continue
      do iview=1,tally_tab_index(1,jscore)
      index_parameters(5)=iview
      signal=extract_output_datum(index_parameters,1,out_post_all,0,spec
     &ies_sy(problem_test_sp(test)) (1:string_length(species_sy(problem_
     &test_sp(test))))//'alpha chord integrals')
      write(31,'(2x,i3,6x,1pe13.4)')iview,signal
      end do
      close(unit=31)
      end if
      jscore_detail=string_lookup(species_sy(problem_test_sp(test)) (1:s
     &tring_length(species_sy(problem_test_sp(test))))//'alpha spectrum 
     &detail',tally_name,tl_num)
      jscore_total=string_lookup(species_sy(problem_test_sp(test)) (1:st
     &ring_length(species_sy(problem_test_sp(test))))//'alpha spectrum',
     &tally_name,tl_num)
      if(jscore_detail.GT.0.AND.jscore_total.GT.0)then
      open(unit=31,file='spectrum_'//species_sy(problem_test_sp(test)) (
     &1:string_length(species_sy(problem_test_sp(test))))//'_alpha.out',
     &status='unknown')
      if(tally_indep_var(3,jscore_detail).EQ.16)continue
      if(tally_indep_var(2,jscore_detail).EQ.6)continue
      if(tally_tab_index(1,jscore_detail).GE.1)continue
      det=string_lookup('Halpha spectrum',detector_name(1),de_grps)
      if(detector_spacing(det).EQ.1)continue
      if(detector_tab_index(det).EQ.tally_tab_index(3,jscore_detail))con
     &tinue
      if(tally_tab_index(2,jscore_detail).LE.30)continue
      if(tally_indep_var(2,jscore_total).EQ.16)continue
      if(tally_tab_index(1,jscore_total).GE.1)continue
      if(detector_tab_index(det).EQ.tally_tab_index(2,jscore_total))cont
     &inue
      do iview=1,tally_tab_index(1,jscore_detail)
      index_parameters(5)=iview
      write(31,*)'Spectrum view number ',iview
      do ibin=1,tally_tab_index(3,jscore_detail)
      index_parameters(16)=ibin
      do iauth=1,tally_tab_index(2,jscore_detail)
      index_parameters(6)=iauth
      signal=extract_output_datum(index_parameters,1,out_post_all,0,spec
     &ies_sy(problem_test_sp(test)) (1:string_length(species_sy(problem_
     &test_sp(test))))//'alpha spectrum detail')
      sig_det(iauth)=signal
      end do
      signal=extract_output_datum(index_parameters,1,out_post_all,0,spec
     &ies_sy(problem_test_sp(test)) (1:string_length(species_sy(problem_
     &test_sp(test))))//'alpha spectrum')
      wavelength=detector_min(det)+(REAL(ibin,DOUBLE)-0.5)*detector_delt
     &a(det)
      write(31,'(1pe16.6,22e13.4)')wavelength,signal,(sig_det(iauth),iau
     &th=1,tally_tab_index(2,jscore_detail))
      end do
      write(31,*)'-----------------------------------------------------'
      end do
      close(unit=31)
      end if
      end do
      output_checkpoint=0
      call nc_write_output
      io_seed(0)=output_random_seed(0,1)
      io_seed(1)=output_random_seed(1,1)
      io_seed(2)=output_random_seed(2,1)
      io_seed(3)=output_random_seed(3,1)
      io_seed(4)=output_random_seed(4,1)
      io_seed(5)=output_random_seed(5,1)
      io_seed(6)=output_random_seed(6,1)
      io_seed(7)=output_random_seed(7,1)
      
      call clear_stat
      call mem_free_i1(chkpt_grp,(1),(num_chkpts),'chkpt_grp')
      call mem_free_i1(chkpt_write,(1),(num_chkpts),'chkpt_write')
      call mem_free_i1(chkpt_nflights,(1),(num_chkpts),'chkpt_nflights')
      call mem_free_r1(estimator_factors,(1),(tl_num),'estimator_factors
     &')
      return
      end
      subroutine setup_flight_frag(nstart,nflights,isource,seed0)
      
      use ff_mod
      
      use tl_mod
      
      use pr_mod
      
      use so_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer nstart,nflights
      integer isource
      integer seed0(0:8-1)
      integer j,ipart,kseg,xseg
      integer seed(0:8-1)
      integer ran_index_rand
      REAL(kind=DOUBLE)ran_array_rand(0:100-1)
      integer species_x
      integer test_x
      REAL(kind=DOUBLE)time_x,weight_x,velocity_x(3)
      REAL(kind=DOUBLE)pos_x(3)
      integer cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x
      integer type_x,author_x
      logical init
      save init
      data init/.TRUE./
      integer ff_temp
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
      
      
      seed(0)=seed0(0)
      seed(1)=seed0(1)
      seed(2)=seed0(2)
      seed(3)=seed0(3)
      seed(4)=seed0(4)
      seed(5)=seed0(5)
      seed(6)=seed0(6)
      seed(7)=seed0(7)
      
      do j=nstart,nstart+nflights-1
      ipart=j-nstart+1
      call random_init_d2(seed,ran_index_rand,ran_array_rand(0))
      call sample_sources(2,isource,j,species_x,test_x,time_x,weight_x,p
     &os_x(1),cell_x,zone_x,surface_x,cell_next_x,zone_next_x,sector_x,s
     &ector_next_x,velocity_x(1),type_x,author_x,ran_index_rand,ran_arra
     &y_rand(0),kseg)
      xseg=source_segment_ptr(source_base_ptr(isource)+kseg)
      ff_particles_int(1,ipart)=j
      ff_particles_int(2,ipart)=isource
      ff_particles_int(3,ipart)=kseg
      ff_particles_int(4,ipart)=xseg
      ff_particles_int(5,ipart)=source_type(isource)
      ff_particles_int(6,ipart)=source_root_species(isource)
      ff_particles_int(7,ipart)=species_x
      ff_particles_int(8,ipart)=test_x
      ff_particles_int(9,ipart)=cell_x
      ff_particles_int(10,ipart)=zone_x
      ff_particles_int(11,ipart)=surface_x
      ff_particles_int(12,ipart)=cell_next_x
      ff_particles_int(13,ipart)=zone_next_x
      ff_particles_int(14,ipart)=sector_x
      ff_particles_int(15,ipart)=sector_next_x
      ff_particles_int(16,ipart)=type_x
      ff_particles_int(17,ipart)=author_x
      ff_particles_float(1,ipart)=time_x
      ff_particles_float(2,ipart)=weight_x
      ff_particles_float(3,ipart)=velocity_x(1)
      ff_particles_float(4,ipart)=velocity_x(2)
      ff_particles_float(5,ipart)=velocity_x(3)
      ff_particles_float(6,ipart)=pos_x(1)
      ff_particles_float(7,ipart)=pos_x(2)
      ff_particles_float(8,ipart)=pos_x(3)
      ff_ran_index(ipart)=ran_index_rand
      do ff_temp=0,100-1
      ff_ran_array(ff_temp,ipart)=ran_array_rand(ff_temp)
      end do
      call next_seed(1,seed)
      end do
      return
      end
      subroutine slave_send_flights(slave,nstart,nflights,isource,seed)
      
      use mp_mod
      
      use sa_mod
      
      use ff_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer slave,nstart,nflights
      integer isource
      integer seed(0:8-1)
      integer tag
      integer time(8)
      character*30 date_str
      include 'mpif.h'
      integer mpi_err
      integer mpi_status
      dimension mpi_status(MPI_STATUS_SIZE)
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
      
      
      if(nflights.EQ.0)then
      tag=100
      call MPI_send(nflights,1,MPI_INTEGER,slave,tag,comm_world_dup,mpi_
     &err)
      return
      end if
      tag=100
      call MPI_send(nflights,1,MPI_INTEGER,slave,tag,comm_world_dup,mpi_
     &err)
      tag=tag+1
      call MPI_send(nstart,1,MPI_INTEGER,slave,tag,comm_world_dup,mpi_er
     &r)
      tag=tag+1
      call MPI_send(isource,1,MPI_INTEGER,slave,tag,comm_world_dup,mpi_e
     &rr)
      tag=tag+1
      call MPI_send(seed,8,MPI_INTEGER,slave,tag,comm_world_dup,mpi_err)
      return
      end
      subroutine slave_receive_flag(slave)
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      include 'mpif.h'
      integer mpi_err
      integer mpi_status
      dimension mpi_status(MPI_STATUS_SIZE)
      integer slave
      integer tag,done_flag
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
      
      
      tag=200
      call MPI_recv(done_flag,1,MPI_INTEGER,MPI_ANY_SOURCE,tag,comm_worl
     &d_dup,mpi_status,mpi_err)
      slave=mpi_status(MPI_SOURCE)
      if(done_flag.EQ.1)continue
      return
      end
      subroutine slave_send_flag(slave,send_data)
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      include 'mpif.h'
      integer mpi_err
      integer mpi_status
      dimension mpi_status(MPI_STATUS_SIZE)
      integer slave,send_data
      integer tag
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
      
      
      tag=300
      call MPI_send(send_data,1,MPI_INTEGER,slave,tag,comm_world_dup,mpi
     &_err)
      return
      end
      subroutine slave_receive_flights(slave)
      
      use mp_mod
      
      use sa_mod
      
      use sn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      include 'mpif.h'
      integer mpi_err
      integer mpi_status
      dimension mpi_status(MPI_STATUS_SIZE)
      integer slave,nstart,nflights
      integer tag,new_dim,new_sn_num_particles
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
      
      
      tag=400
      call MPI_recv(stat_comp_frag,1,MPI_INTEGER,MPI_ANY_SOURCE,tag,comm
     &_world_dup,mpi_status,mpi_err)
      slave=mpi_status(MPI_SOURCE)
      
      tag=tag+1
      call MPI_recv(stat_wt_tot_frag,1,MPI_DOUBLE_PRECISION,slave,tag,co
     &mm_world_dup,mpi_status,mpi_err)
      tag=tag+1
      call MPI_recv(stat_size_frag,1,MPI_INTEGER,slave,tag,comm_world_du
     &p,mpi_status,mpi_err)
      tag=tag+1
      call MPI_recv(new_dim,1,MPI_INTEGER,slave,tag,comm_world_dup,mpi_s
     &tatus,mpi_err)
      if(new_dim.GT.stat_dim_frag)then
      stat_frag =>mem_realloc_r2(stat_frag,(0),(1),(0),stat_dim_frag-1,n
     &ew_dim-1,'stat_frag')
      stat_dim_frag=new_dim
      end if
      tag=tag+1
      call MPI_recv(stat_frag,(((1)-(0)+1)*((stat_dim_frag-1)-(0)+1)),MP
     &I_DOUBLE_PRECISION,slave,tag,comm_world_dup,mpi_status,mpi_err)
      tag=tag+1
      call MPI_recv(new_dim,1,MPI_INTEGER,slave,tag,comm_world_dup,mpi_s
     &tatus,mpi_err)
      if(new_dim.GT.stat_pf_dim_frag)then
      stat_ptr2full_frag =>mem_realloc_i1(stat_ptr2full_frag,(0),stat_pf
     &_dim_frag-1,new_dim-1,'stat_ptr2full_frag')
      stat_pf_dim_frag=new_dim
      end if
      tag=tag+1
      call MPI_recv(stat_ptr2full_frag,(((stat_pf_dim_frag-1)-(0)+1)),MP
     &I_INTEGER,slave,tag,comm_world_dup,mpi_status,mpi_err)
      tag=tag+1
      call MPI_recv(new_dim,1,MPI_INTEGER,slave,tag,comm_world_dup,mpi_s
     &tatus,mpi_err)
      if(new_dim.GT.stat_ps_dim_frag)then
      stat_ptr2short_frag =>mem_realloc_i1(stat_ptr2short_frag,(0),stat_
     &ps_dim_frag-1,new_dim-1,'stat_ptr2short_frag')
      stat_ps_dim_frag=new_dim
      end if
      tag=tag+1
      call MPI_recv(stat_ptr2short_frag,(((stat_ps_dim_frag-1)-(0)+1)),M
     &PI_INTEGER,slave,tag,comm_world_dup,mpi_status,mpi_err)
      tag=tag+1
      call MPI_recv(new_dim,1,MPI_INTEGER,slave,tag,comm_world_dup,mpi_s
     &tatus,mpi_err)
      if(new_dim.GT.stat_wt_dim_frag)then
      stat_wt_frag =>mem_realloc_r1(stat_wt_frag,(0),stat_wt_dim_frag-1,
     &new_dim-1,'stat_wt_frag')
      stat_wt_dim_frag=new_dim
      end if
      tag=tag+1
      call MPI_recv(stat_wt_frag,(((stat_wt_dim_frag-1)-(0)+1)),MPI_DOUB
     &LE_PRECISION,slave,tag,comm_world_dup,mpi_status,mpi_err)
      tag=tag+1
      call MPI_recv(new_sn_num_particles,1,MPI_INTEGER,slave,tag,comm_wo
     &rld_dup,mpi_status,mpi_err)
      if(new_sn_num_particles.GT.0)then
      if(sn_number_particles+new_sn_num_particles.GT.sn_particles_dim)th
     &en
      if((int((((sn_particles_dim)-(1)+1)+100-1)/100)*100).NE.(int((((sn
     &_number_particles+new_sn_num_particles)-(1)+1)+100-1)/100)*100))th
     &en
      sn_particles_float =>mem_realloc_r2(sn_particles_float,(1),(8),(1)
     &,((int((((sn_particles_dim)-(1)+1)+100-1)/100)*100)+(1)-1),((int((
     &((sn_number_particles+new_sn_num_particles)-(1)+1)+100-1)/100)*100
     &)+(1)-1),'sn_particles_float')
      end if
      if((int((((sn_particles_dim)-(1)+1)+100-1)/100)*100).NE.(int((((sn
     &_number_particles+new_sn_num_particles)-(1)+1)+100-1)/100)*100))th
     &en
      sn_particles_int =>mem_realloc_i2(sn_particles_int,(1),(11),(1),((
     &int((((sn_particles_dim)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((s
     &n_number_particles+new_sn_num_particles)-(1)+1)+100-1)/100)*100)+(
     &1)-1),'sn_particles_int')
      end if
      sn_particles_dim=((int((((sn_number_particles+new_sn_num_particles
     &)-(1)+1)+100-1)/100)*100)+(1)-1)
      end if
      tag=tag+1
      call MPI_recv(sn_particles_float(1,sn_number_particles+1),(((8)-(1
     &)+1))*new_sn_num_particles,MPI_DOUBLE_PRECISION,slave,tag,comm_wor
     &ld_dup,mpi_status,mpi_err)
      tag=tag+1
      call MPI_recv(sn_particles_int(1,sn_number_particles+1),(((11)-(1)
     &+1))*new_sn_num_particles,MPI_INTEGER,slave,tag,comm_world_dup,mpi
     &_status,mpi_err)
      sn_number_particles=sn_number_particles+(new_sn_num_particles)
      end if
90007 continue
      return
      end
      subroutine do_flights_slave
      
      use mp_mod
      
      use sa_mod
      
      use ff_mod
      
      use pr_mod
      
      use so_mod
      
      use tl_mod
      
      use sn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      include 'mpif.h'
      integer mpi_err
      integer mpi_status
      dimension mpi_status(MPI_STATUS_SIZE)
      integer nstart,nflights,tag,done_flag,send_data,i,j,ilevel,min_lev
     &el,islave,sending_slave,new_dim
      integer time(8)
      REAL(kind=DOUBLE)mult
      character*30 date_str
      logical init
      save init
      data init/.TRUE./
      integer isource
      integer seed(0:8-1)
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
      
      
      logical check_tally
      integer inc
      REAL(kind=DOUBLE),dimension(:),pointer::estimator_factors
      estimator_factors =>mem_alloc_r1((1),(tl_num),'estimator_factors')
      sn_number_particles=0
      sn_particles_dim=100
      sn_particles_float =>mem_alloc_r2((1),(8),(1),(sn_particles_dim),'
     &sn_particles_float')
      sn_particles_int =>mem_alloc_i2((1),(11),(1),(sn_particles_dim),'s
     &n_particles_int')
      call MPI_bcast(so_direct_delta,1,MPI_DOUBLE_PRECISION,mpi_degas2_r
     &oot,comm_world_dup,mpi_err)
      stat_comp_flt=1
      stat_comp_frag=1
      stat_comp_fin=1
      call stat_init
90000 continue
      tag=100
      call MPI_recv(ff_number_particles,1,MPI_INTEGER,mpi_degas2_root,ta
     &g,comm_world_dup,mpi_status,mpi_err)
      if(ff_number_particles.EQ.0)goto 90007
      tag=tag+1
      if(init)then
      ff_particles_dim=ff_number_particles
      ff_particles_int =>mem_alloc_i2((1),(17),(1),(ff_particles_dim),'f
     &f_particles_int')
      ff_particles_float =>mem_alloc_r2((1),(8),(1),(ff_particles_dim),'
     &ff_particles_float')
      ff_ran_index =>mem_alloc_i1((1),(ff_particles_dim),'ff_ran_index')
      ff_ran_array =>mem_alloc_r2((0),(100-1),(1),(ff_particles_dim),'ff
     &_ran_array')
      init=.FALSE.
      else if(ff_number_particles.GT.ff_particles_dim)then
      ff_particles_int =>mem_realloc_i2(ff_particles_int,(1),(17),(1),ff
     &_particles_dim,ff_number_particles,'ff_particles_int')
      ff_particles_float =>mem_realloc_r2(ff_particles_float,(1),(8),(1)
     &,ff_particles_dim,ff_number_particles,'ff_particles_float')
      ff_ran_index =>mem_realloc_i1(ff_ran_index,(1),ff_particles_dim,ff
     &_number_particles,'ff_ran_index')
      ff_ran_array =>mem_realloc_r2(ff_ran_array,(0),(100-1),(1),ff_part
     &icles_dim,ff_number_particles,'ff_ran_array')
      ff_particles_dim=ff_number_particles
      end if
      nflights=ff_number_particles
      call MPI_recv(nstart,1,MPI_INTEGER,mpi_degas2_root,tag,comm_world_
     &dup,mpi_status,mpi_err)
      tag=tag+1
      call MPI_recv(isource,1,MPI_INTEGER,mpi_degas2_root,tag,comm_world
     &_dup,mpi_status,mpi_err)
      tag=tag+1
      call MPI_recv(seed,8,MPI_INTEGER,mpi_degas2_root,tag,comm_world_du
     &p,mpi_status,mpi_err)
      call setup_flight_frag(nstart,nflights,isource,seed)
      call do_flights(estimator_factors)
      done_flag=1
      tag=200
      call MPI_send(done_flag,1,MPI_INTEGER,mpi_degas2_root,tag,comm_wor
     &ld_dup,mpi_err)
      tag=300
      call MPI_recv(send_data,1,MPI_INTEGER,mpi_degas2_root,tag,comm_wor
     &ld_dup,mpi_status,mpi_err)
      if(send_data.EQ.0)then
      goto 90000
      else if((send_data.EQ.1).OR.(send_data.EQ.2))then
      min_level=1
      if(send_data.EQ.2)min_level=mpi_nlevels
      do ilevel=min_level,mpi_nlevels
      if(mpi_slave_map((ilevel-1)*mpi_nslaves+mpi_rank).EQ.mpi_rank)then
      if(ilevel.EQ.min_level)then
      
      if(stat_comp_frag.EQ.1)call stat_wt_balance(stat_wt_tot_frag,stat_
     &size_frag,stat_dim_frag,stat_frag,stat_wt_dim_frag,stat_wt_frag)
      if(min(stat_size_fin+stat_size_frag,tally_size).GT.stat_dim_fin)th
     &en
      if(stat_comp_fin.EQ.1)continue
      mult=max((1.0_DOUBLE),REAL(stat_size_frag,DOUBLE)/REAL(15000,DOUBL
     &E))
      new_dim=min(stat_dim_fin+int(mult*15000),tally_size)
      stat_fin =>mem_realloc_r2(stat_fin,(0),(1),(0),stat_dim_fin-1,new_
     &dim-1,'stat_fin')
      stat_ptr2full_fin =>mem_realloc_i1(stat_ptr2full_fin,(0),stat_pf_d
     &im_fin-1,new_dim-1,'stat_ptr2full_fin')
      stat_wt_fin =>mem_realloc_r1(stat_wt_fin,(0),stat_wt_dim_fin-1,new
     &_dim-1,'stat_wt_fin')
      stat_dim_fin=new_dim
      stat_pf_dim_fin=new_dim
      stat_wt_dim_fin=new_dim
      end if
      if(stat_wt_tot_frag.LT.(0.0_DOUBLE))then
      write(0,*)' Bad value of stat_wt_tot_frag = ',stat_wt_tot_frag
      write(0,*)' slave = ',mpi_rank
      if(stat_wt_tot_frag.GE.(0.0_DOUBLE))continue
      call flush(0)
      end if
      call stat_acc(stat_comp_frag,stat_wt_tot_frag,stat_size_frag,stat_
     &dim_frag,stat_frag,stat_pf_dim_frag,stat_ptr2full_frag,stat_ps_dim
     &_frag,stat_ptr2short_frag,stat_comp_fin,stat_wt_tot_fin,stat_size_
     &fin,stat_dim_fin,stat_fin,stat_pf_dim_fin,stat_ptr2full_fin,stat_p
     &s_dim_fin,stat_ptr2short_fin,stat_wt_dim_fin,stat_wt_fin)
      
      end if
      do islave=1,mpi_nslaves
      if((islave.NE.mpi_rank).AND.(mpi_slave_map((ilevel-1)*mpi_nslaves+
     &islave).EQ.mpi_rank))then
      call slave_receive_flights(sending_slave)
      
      if(stat_comp_frag.EQ.1)call stat_wt_balance(stat_wt_tot_frag,stat_
     &size_frag,stat_dim_frag,stat_frag,stat_wt_dim_frag,stat_wt_frag)
      if(min(stat_size_fin+stat_size_frag,tally_size).GT.stat_dim_fin)th
     &en
      if(stat_comp_fin.EQ.1)continue
      mult=max((1.0_DOUBLE),REAL(stat_size_frag,DOUBLE)/REAL(15000,DOUBL
     &E))
      new_dim=min(stat_dim_fin+int(mult*15000),tally_size)
      stat_fin =>mem_realloc_r2(stat_fin,(0),(1),(0),stat_dim_fin-1,new_
     &dim-1,'stat_fin')
      stat_ptr2full_fin =>mem_realloc_i1(stat_ptr2full_fin,(0),stat_pf_d
     &im_fin-1,new_dim-1,'stat_ptr2full_fin')
      stat_wt_fin =>mem_realloc_r1(stat_wt_fin,(0),stat_wt_dim_fin-1,new
     &_dim-1,'stat_wt_fin')
      stat_dim_fin=new_dim
      stat_pf_dim_fin=new_dim
      stat_wt_dim_fin=new_dim
      end if
      if(stat_wt_tot_frag.LT.(0.0_DOUBLE))then
      write(0,*)' Bad value of stat_wt_tot_frag = ',stat_wt_tot_frag
      write(0,*)' slave = ',mpi_rank
      if(stat_wt_tot_frag.GE.(0.0_DOUBLE))continue
      call flush(0)
      end if
      call stat_acc(stat_comp_frag,stat_wt_tot_frag,stat_size_frag,stat_
     &dim_frag,stat_frag,stat_pf_dim_frag,stat_ptr2full_frag,stat_ps_dim
     &_frag,stat_ptr2short_frag,stat_comp_fin,stat_wt_tot_fin,stat_size_
     &fin,stat_dim_fin,stat_fin,stat_pf_dim_fin,stat_ptr2full_fin,stat_p
     &s_dim_fin,stat_ptr2short_fin,stat_wt_dim_fin,stat_wt_fin)
      
      end if
      end do
      else
      if(ilevel.EQ.min_level)then
      
      tag=400
      call MPI_send(stat_comp_frag,1,MPI_INTEGER,mpi_slave_map((ilevel-1
     &)*mpi_nslaves+mpi_rank),tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_wt_tot_frag,1,MPI_DOUBLE_PRECISION,mpi_slave_ma
     &p((ilevel-1)*mpi_nslaves+mpi_rank),tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_size_frag,1,MPI_INTEGER,mpi_slave_map((ilevel-1
     &)*mpi_nslaves+mpi_rank),tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_dim_frag,1,MPI_INTEGER,mpi_slave_map((ilevel-1)
     &*mpi_nslaves+mpi_rank),tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_frag,(((1)-(0)+1)*((stat_dim_frag-1)-(0)+1)),MP
     &I_DOUBLE_PRECISION,mpi_slave_map((ilevel-1)*mpi_nslaves+mpi_rank),
     &tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_pf_dim_frag,1,MPI_INTEGER,mpi_slave_map((ilevel
     &-1)*mpi_nslaves+mpi_rank),tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_ptr2full_frag,(((stat_pf_dim_frag-1)-(0)+1)),MP
     &I_INTEGER,mpi_slave_map((ilevel-1)*mpi_nslaves+mpi_rank),tag,comm_
     &world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_ps_dim_frag,1,MPI_INTEGER,mpi_slave_map((ilevel
     &-1)*mpi_nslaves+mpi_rank),tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_ptr2short_frag,(((stat_ps_dim_frag-1)-(0)+1)),M
     &PI_INTEGER,mpi_slave_map((ilevel-1)*mpi_nslaves+mpi_rank),tag,comm
     &_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_wt_dim_frag,1,MPI_INTEGER,mpi_slave_map((ilevel
     &-1)*mpi_nslaves+mpi_rank),tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_wt_frag,(((stat_wt_dim_frag-1)-(0)+1)),MPI_DOUB
     &LE_PRECISION,mpi_slave_map((ilevel-1)*mpi_nslaves+mpi_rank),tag,co
     &mm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(sn_number_particles,1,MPI_INTEGER,mpi_slave_map((ile
     &vel-1)*mpi_nslaves+mpi_rank),tag,comm_world_dup,mpi_err)
      if(sn_number_particles.GT.0)then
      tag=tag+1
      call MPI_send(sn_particles_float,(((8)-(1)+1))*sn_number_particles
     &,MPI_DOUBLE_PRECISION,mpi_slave_map((ilevel-1)*mpi_nslaves+mpi_ran
     &k),tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(sn_particles_int,(((11)-(1)+1))*sn_number_particles,
     &MPI_INTEGER,mpi_slave_map((ilevel-1)*mpi_nslaves+mpi_rank),tag,com
     &m_world_dup,mpi_err)
      end if
      call stat_zero(stat_wt_tot_frag,stat_comp_frag,stat_size_frag,stat
     &_ps_dim_frag,stat_ptr2short_frag,stat_dim_frag,stat_frag)
      
      else
      
      tag=400
      call MPI_send(stat_comp_fin,1,MPI_INTEGER,mpi_slave_map((ilevel-1)
     &*mpi_nslaves+mpi_rank),tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_wt_tot_fin,1,MPI_DOUBLE_PRECISION,mpi_slave_map
     &((ilevel-1)*mpi_nslaves+mpi_rank),tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_size_fin,1,MPI_INTEGER,mpi_slave_map((ilevel-1)
     &*mpi_nslaves+mpi_rank),tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_dim_fin,1,MPI_INTEGER,mpi_slave_map((ilevel-1)*
     &mpi_nslaves+mpi_rank),tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_fin,(((1)-(0)+1)*((stat_dim_fin-1)-(0)+1)),MPI_
     &DOUBLE_PRECISION,mpi_slave_map((ilevel-1)*mpi_nslaves+mpi_rank),ta
     &g,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_pf_dim_fin,1,MPI_INTEGER,mpi_slave_map((ilevel-
     &1)*mpi_nslaves+mpi_rank),tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_ptr2full_fin,(((stat_pf_dim_fin-1)-(0)+1)),MPI_
     &INTEGER,mpi_slave_map((ilevel-1)*mpi_nslaves+mpi_rank),tag,comm_wo
     &rld_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_ps_dim_fin,1,MPI_INTEGER,mpi_slave_map((ilevel-
     &1)*mpi_nslaves+mpi_rank),tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_ptr2short_fin,(((stat_ps_dim_fin-1)-(0)+1)),MPI
     &_INTEGER,mpi_slave_map((ilevel-1)*mpi_nslaves+mpi_rank),tag,comm_w
     &orld_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_wt_dim_fin,1,MPI_INTEGER,mpi_slave_map((ilevel-
     &1)*mpi_nslaves+mpi_rank),tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(stat_wt_fin,(((stat_wt_dim_fin-1)-(0)+1)),MPI_DOUBLE
     &_PRECISION,mpi_slave_map((ilevel-1)*mpi_nslaves+mpi_rank),tag,comm
     &_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(sn_number_particles,1,MPI_INTEGER,mpi_slave_map((ile
     &vel-1)*mpi_nslaves+mpi_rank),tag,comm_world_dup,mpi_err)
      if(sn_number_particles.GT.0)then
      tag=tag+1
      call MPI_send(sn_particles_float,(((8)-(1)+1))*sn_number_particles
     &,MPI_DOUBLE_PRECISION,mpi_slave_map((ilevel-1)*mpi_nslaves+mpi_ran
     &k),tag,comm_world_dup,mpi_err)
      tag=tag+1
      call MPI_send(sn_particles_int,(((11)-(1)+1))*sn_number_particles,
     &MPI_INTEGER,mpi_slave_map((ilevel-1)*mpi_nslaves+mpi_rank),tag,com
     &m_world_dup,mpi_err)
      end if
      call stat_zero(stat_wt_tot_fin,stat_comp_fin,stat_size_fin,stat_ps
     &_dim_fin,stat_ptr2short_fin,stat_dim_fin,stat_fin)
      
      end if
      sn_number_particles=0
      do i=1,sn_particles_dim
      do j=1,8
      sn_particles_float(j,i)=(314159265.3589793238462_DOUBLE)
      end do
      do j=1,11
      sn_particles_int(j,i)=314159265
      end do
      end do
      goto 90000
      end if
      call MPI_barrier(mpi_meta_slave_comms(ilevel),mpi_err)
      
      end do
      else
      if('Illegal value of send_data'.EQ.' ')continue
      end if
90007 continue
      call clear_stat
      call mem_free_r1(estimator_factors,(1),(tl_num),'estimator_factors
     &')
      call mem_free_r2(sn_particles_float,(1),(8),(1),(sn_particles_dim)
     &,'sn_particles_float')
      call mem_free_i2(sn_particles_int,(1),(11),(1),(sn_particles_dim),
     &'sn_particles_int')
      return
      end
      subroutine fill_coupling_arrays
      
      
      
      
      use zn_mod
      
      use sp_mod
      
      use bk_mod
      
      use pr_mod
      
      use so_mod
      
      use tl_mod
      
      use ou_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer zone,back,e_back,ix,iy,i,ix_1,ix_n,iy_1,iy_n,test,is,i_lis
     &t,first_back
      integer index_parameters(100)
      REAL(kind=DOUBLE)multiplier
      logical have_pressure
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
      logical check_tally
      integer inc
      logical check_zone
      external extract_output_datum
      REAL(kind=DOUBLE)extract_output_datum
      integer,dimension(:,:),pointer::zone_map
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::test_data
      zone_map =>mem_alloc_i2((output_index_1_min),(output_index_1_max),
     &(output_index_2_min),(output_index_2_max),'zone_map')
      test_data =>mem_alloc_r4((output_index_1_min),(output_index_1_max)
     &,(output_index_2_min),(output_index_2_max),(1),(pr_test_num),(1),(
     &5),'test_data')
      do zone=1,zn_num
      if((zone_type(zone).EQ.2).AND.(zone_index(4,zone).EQ.zone).AND.((z
     &one_index(1,zone).GE.output_index_1_min).AND.(zone_index(1,zone).L
     &E.output_index_1_max).AND.(zone_index(2,zone).GE.output_index_2_mi
     &n).AND.(zone_index(2,zone).LE.output_index_2_max)))then
      zone_map(zone_index(1,zone),zone_index(2,zone))=zone
      end if
      end do
      
      ix_1=max(1,output_index_1_min)
      ix_n=output_index_1_max
      iy_1=max(1,output_index_2_min)
      iy_n=output_index_2_max
      if(string_lookup('neutral pressure',tally_name,tl_num).GT.0)then
      have_pressure=.TRUE.
      else
      have_pressure=.FALSE.
      end if
      do test=2,pr_test_num
      index_parameters(3)=test
      do iy=iy_1,iy_n
      do ix=ix_1,ix_n
      if(check_zone(zone_map(ix,iy)))then
      index_parameters(1)=zone_map(ix,iy)
      test_data(ix,iy,test,1)=extract_output_datum(index_parameters,1,ou
     &t_post_all,0,'neutral density')
      if(have_pressure.AND.test_data(ix,iy,test,1).NE.(0.0_DOUBLE))then
      test_data(ix,iy,test,2)=extract_output_datum(index_parameters,1,ou
     &t_post_all,0,'neutral pressure')/test_data(ix,iy,test,1)
      else
      test_data(ix,iy,test,2)=(0.0_DOUBLE)
      end if
      do i=1,3
      test_data(ix,iy,test,3+i-1)=extract_output_datum(index_parameters,
     &i,out_post_all,0,'neutral flux vector')
      end do
      end if
      end do
      end do
      end do
      open(unit=31,file='testdata.out',status='unknown')
      write(31,*)ix_n-ix_1+1,iy_n-iy_1+1,pr_test_num-1
      do test=2,pr_test_num
      write(31,*)species_sy(problem_test_sp(test))
      end do
      multiplier=(1.0_DOUBLE)
      do test=2,pr_test_num
      call write_sources(test_data(output_index_1_min,output_index_2_min
     &,test,1),multiplier,ix_1,ix_n,iy_1,iy_n,output_index_1_min,output_
     &index_1_max,output_index_2_min,output_index_2_max,31)
      call write_sources(test_data(output_index_1_min,output_index_2_min
     &,test,2),multiplier,ix_1,ix_n,iy_1,iy_n,output_index_1_min,output_
     &index_1_max,output_index_2_min,output_index_2_max,31)
      end do
      do test=2,pr_test_num
      do i_list=3,5
      call write_sources(test_data(output_index_1_min,output_index_2_min
     &,test,i_list),multiplier,ix_1,ix_n,iy_1,iy_n,output_index_1_min,ou
     &tput_index_1_max,output_index_2_min,output_index_2_max,31)
      end do
      end do
      close(unit=31)
      e_back=problem_species_background(string_lookup('e',species_sy,sp_
     &num))
      if((pr_reaction_num.GT.0).AND.((e_back.GT.0.AND.e_back.LE.bk_num))
     &)then
      open(unit=31,file='sources.out',status='unknown')
      do is=1,so_grps
      first_back=0
      do back=1,pr_background_num
      index_parameters(4)=back
      do iy=iy_1,iy_n
      do ix=ix_1,ix_n
      if(check_zone(zone_map(ix,iy)))then
      index_parameters(1)=zone_map(ix,iy)
      output_2D_coupling(ix,iy,back,1,is)=extract_output_datum(index_par
     &ameters,1,out_post_grp(0,0,is),0,'ion source rate')
      output_2D_coupling(ix,iy,back,2,is)=extract_output_datum(index_par
     &ameters,1,out_post_grp(0,0,is),0,'ion momentum source vector')
      output_2D_coupling(ix,iy,back,3,is)=extract_output_datum(index_par
     &ameters,2,out_post_grp(0,0,is),0,'ion momentum source vector')
      output_2D_coupling(ix,iy,back,4,is)=extract_output_datum(index_par
     &ameters,3,out_post_grp(0,0,is),0,'ion momentum source vector')
      if(first_back.EQ.0.OR.back.EQ.e_back)then
      output_2D_coupling(ix,iy,back,5,is)=extract_output_datum(index_par
     &ameters,1,out_post_grp(0,0,is),0,'ion energy source')
      else
      output_2D_coupling(ix,iy,first_back,5,is)=output_2D_coupling(ix,iy
     &,first_back,5,is)+(extract_output_datum(index_parameters,1,out_pos
     &t_grp(0,0,is),0,'ion energy source'))
      end if
      else
      do i=1,5
      output_2D_coupling(ix,iy,back,i,is)=(0.0_DOUBLE)
      end do
      end if
      end do
      end do
      if(first_back.EQ.0.AND.back.NE.e_back)first_back=back
      end do
      multiplier=(1.0_DOUBLE)
      write(31,'(e12.6)')-source_scale_factor(is)*source_total_current(i
     &s)
      do back=1,pr_background_num
      if(back.NE.e_back)then
      call write_sources(output_2D_coupling(output_index_1_min,output_in
     &dex_2_min,back,1,is),multiplier,ix_1,ix_n,iy_1,iy_n,output_index_1
     &_min,output_index_1_max,output_index_2_min,output_index_2_max,31)
      call write_sources(output_2D_coupling(output_index_1_min,output_in
     &dex_2_min,back,2,is),multiplier,ix_1,ix_n,iy_1,iy_n,output_index_1
     &_min,output_index_1_max,output_index_2_min,output_index_2_max,31)
      call write_sources(output_2D_coupling(output_index_1_min,output_in
     &dex_2_min,back,3,is),multiplier,ix_1,ix_n,iy_1,iy_n,output_index_1
     &_min,output_index_1_max,output_index_2_min,output_index_2_max,31)
      call write_sources(output_2D_coupling(output_index_1_min,output_in
     &dex_2_min,back,4,is),multiplier,ix_1,ix_n,iy_1,iy_n,output_index_1
     &_min,output_index_1_max,output_index_2_min,output_index_2_max,31)
      end if
      end do
      call write_sources(output_2D_coupling(output_index_1_min,output_in
     &dex_2_min,e_back,5,is),multiplier,ix_1,ix_n,iy_1,iy_n,output_index
     &_1_min,output_index_1_max,output_index_2_min,output_index_2_max,31
     &)
      call write_sources(output_2D_coupling(output_index_1_min,output_in
     &dex_2_min,first_back,5,is),multiplier,ix_1,ix_n,iy_1,iy_n,output_i
     &ndex_1_min,output_index_1_max,output_index_2_min,output_index_2_ma
     &x,31)
      end do
      close(unit=31)
      end if
      call mem_free_i2(zone_map,(output_index_1_min),(output_index_1_max
     &),(output_index_2_min),(output_index_2_max),'zone_map')
      call mem_free_r4(test_data,(output_index_1_min),(output_index_1_ma
     &x),(output_index_2_min),(output_index_2_max),(1),(pr_test_num),(1)
     &,(5),'test_data')
      return
      end
      subroutine write_sources(two_D_data,multiplier,ix_1,ix_n,iy_1,iy_n
     &,dimx_1,dimx_n,dimy_1,dimy_n,unit_num)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer ix_1,ix_n,iy_1,iy_n,dimx_1,dimx_n,dimy_1,dimy_n,unit_num
      REAL(kind=DOUBLE)two_D_data(dimx_1:dimx_n,dimy_1:dimy_n)
      REAL(kind=DOUBLE)multiplier
      integer lim,iy,ix,j
      lim=((ix_n-ix_1+1)/5)*5-4
      do iy=iy_1,iy_n
      do ix=1,lim,5
      write(unit_num,'(5(e16.8))') (multiplier*two_D_data(ix_1+ix-1+j,iy
     &),j=0,4)
      end do
      if(ix_1+lim+3.NE.ix_n)then
      write(unit_num,'(5(e16.8))') (multiplier*two_D_data(ix,iy),ix=ix_1
     &+lim+4,ix_n)
      end if
      end do
      return
      end
      subroutine nc_write_output
      
      use rf_mod
      
      use so_mod
      
      use pr_mod
      
      use tl_mod
      
      use ou_mod
      
      use sn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
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
      integer fileid
      integer output_coupling_ind_id
      integer output_moments_ind_id
      integer ou_back_ind_id
      integer output_tab_ind_id
      integer output_grps_ind_id
      integer output_seed_ind_id
      integer output_index_1_min_id
      integer output_index_1_max_id
      integer output_index_2_min_id
      integer output_index_2_max_id
      integer output_ind_1_id
      integer output_ind_2_id
      integer output_checkpoint_id
      integer output_all_id
      integer output_grp_id
      integer out_post_all_id
      integer out_post_grp_id
      integer output_2D_coupling_id
      integer output_weight_grp_id
      integer output_num_flights_id
      integer output_random_seed_id
      
      integer sn_particles_dim_id
      integer snapshot_pdf_ind_id
      integer sn_pt_float_ind_id
      integer sn_pt_int_ind_id
      integer sn_seed_decimal_ind_id
      integer sn_number_particles_id
      integer sn_seed_decimal_id
      integer sn_particles_float_id
      integer sn_particles_int_id
      
      character*300 description,program_version
      character*96 tempfile
      program_version='DEGAS 2 Git commit: $Format:%H$, ref names: $Form
     &at:%d$'
      tempfile=filenames_array(20)
      if(tempfile.NE.'undefined')continue
      fileid=nccre(tempfile,or(0,512),nc_stat)
      description='Output from DEGAS 2'
      call ncaptc(fileid,0,'description',2,string_length(description),de
     &scription,nc_stat)
      call ncaptc(fileid,0,'program_version',2,string_length(program_ver
     &sion),program_version,nc_stat)
      output_coupling_ind_id=ncddef(fileid,'output_coupling_ind',((5)-(1
     &)+1),nc_stat)
      output_moments_ind_id=ncddef(fileid,'output_moments_ind',((1)-(0)+
     &1),nc_stat)
      ou_back_ind_id=ncddef(fileid,'ou_back_ind',((pr_background_num)-(1
     &)+1),nc_stat)
      output_tab_ind_id=ncddef(fileid,'output_tab_ind',((tally_size-1)-(
     &0)+1),nc_stat)
      output_grps_ind_id=ncddef(fileid,'output_grps_ind',((so_grps)-(1)+
     &1),nc_stat)
      output_seed_ind_id=ncddef(fileid,'output_seed_ind',((8-1)-(0)+1),n
     &c_stat)
      output_index_1_min_id=ncvdef(fileid,'output_index_1_min',4,0,nc_di
     &ms,nc_stat)
      output_index_1_max_id=ncvdef(fileid,'output_index_1_max',4,0,nc_di
     &ms,nc_stat)
      output_index_2_min_id=ncvdef(fileid,'output_index_2_min',4,0,nc_di
     &ms,nc_stat)
      output_index_2_max_id=ncvdef(fileid,'output_index_2_max',4,0,nc_di
     &ms,nc_stat)
      output_ind_1_id=ncddef(fileid,'output_ind_1',((output_index_1_max)
     &-(output_index_1_min)+1),nc_stat)
      output_ind_2_id=ncddef(fileid,'output_ind_2',((output_index_2_max)
     &-(output_index_2_min)+1),nc_stat)
      output_checkpoint_id=ncvdef(fileid,'output_checkpoint',4,0,nc_dims
     &,nc_stat)
      nc_dims(1)=output_moments_ind_id
      nc_dims(2)=output_tab_ind_id
      output_all_id=ncvdef(fileid,'output_all',6,2,nc_dims,nc_stat)
      nc_dims(1)=output_moments_ind_id
      nc_dims(2)=output_tab_ind_id
      nc_dims(3)=output_grps_ind_id
      output_grp_id=ncvdef(fileid,'output_grp',6,3,nc_dims,nc_stat)
      nc_dims(1)=output_moments_ind_id
      nc_dims(2)=output_tab_ind_id
      out_post_all_id=ncvdef(fileid,'out_post_all',6,2,nc_dims,nc_stat)
      nc_dims(1)=output_moments_ind_id
      nc_dims(2)=output_tab_ind_id
      nc_dims(3)=output_grps_ind_id
      out_post_grp_id=ncvdef(fileid,'out_post_grp',6,3,nc_dims,nc_stat)
      nc_dims(1)=output_ind_1_id
      nc_dims(2)=output_ind_2_id
      nc_dims(3)=ou_back_ind_id
      nc_dims(4)=output_coupling_ind_id
      nc_dims(5)=output_grps_ind_id
      output_2D_coupling_id=ncvdef(fileid,'output_2D_coupling',6,5,nc_di
     &ms,nc_stat)
      nc_dims(1)=output_grps_ind_id
      output_weight_grp_id=ncvdef(fileid,'output_weight_grp',6,1,nc_dims
     &,nc_stat)
      nc_dims(1)=output_grps_ind_id
      output_num_flights_id=ncvdef(fileid,'output_num_flights',4,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=output_seed_ind_id
      nc_dims(2)=output_grps_ind_id
      output_random_seed_id=ncvdef(fileid,'output_random_seed',4,2,nc_di
     &ms,nc_stat)
      
      call ncendf(fileid,nc_stat)
      call ncvpt(fileid,output_index_1_min_id,nc_corner,nc_edge,output_i
     &ndex_1_min,nc_stat)
      call ncvpt(fileid,output_index_1_max_id,nc_corner,nc_edge,output_i
     &ndex_1_max,nc_stat)
      call ncvpt(fileid,output_index_2_min_id,nc_corner,nc_edge,output_i
     &ndex_2_min,nc_stat)
      call ncvpt(fileid,output_index_2_max_id,nc_corner,nc_edge,output_i
     &ndex_2_max,nc_stat)
      call ncvpt(fileid,output_checkpoint_id,nc_corner,nc_edge,output_ch
     &eckpoint,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((tally_size-1)-(0)+1)
      call ncvpt(fileid,output_all_id,nc_corner,nc_edge,output_all,nc_st
     &at)
      nc_corner(1)=1
      nc_edge(1)=((1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((tally_size-1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((so_grps)-(1)+1)
      call ncvpt(fileid,output_grp_id,nc_corner,nc_edge,output_grp,nc_st
     &at)
      nc_corner(1)=1
      nc_edge(1)=((1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((tally_size-1)-(0)+1)
      call ncvpt(fileid,out_post_all_id,nc_corner,nc_edge,out_post_all,n
     &c_stat)
      nc_corner(1)=1
      nc_edge(1)=((1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((tally_size-1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((so_grps)-(1)+1)
      call ncvpt(fileid,out_post_grp_id,nc_corner,nc_edge,out_post_grp,n
     &c_stat)
      nc_corner(1)=1
      nc_edge(1)=((output_index_1_max)-(output_index_1_min)+1)
      nc_corner(2)=1
      nc_edge(2)=((output_index_2_max)-(output_index_2_min)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_background_num)-(1)+1)
      nc_corner(4)=1
      nc_edge(4)=((5)-(1)+1)
      nc_corner(5)=1
      nc_edge(5)=((so_grps)-(1)+1)
      call ncvpt(fileid,output_2D_coupling_id,nc_corner,nc_edge,output_2
     &D_coupling,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,output_weight_grp_id,nc_corner,nc_edge,output_we
     &ight_grp,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,output_num_flights_id,nc_corner,nc_edge,output_n
     &um_flights,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((8-1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((so_grps)-(1)+1)
      call ncvpt(fileid,output_random_seed_id,nc_corner,nc_edge,output_r
     &andom_seed,nc_stat)
      
      call ncclos(fileid,nc_stat)
      if(so_time_dependent.EQ.1)then
      tempfile=filenames_array(23)
      if(tempfile.NE.'undefined')continue
      fileid=nccre(tempfile,0,nc_stat)
      sn_particles_dim_id=ncvdef(fileid,'sn_particles_dim',4,0,nc_dims,n
     &c_stat)
      snapshot_pdf_ind_id=ncddef(fileid,'snapshot_pdf_ind',((sn_particle
     &s_dim)-(1)+1),nc_stat)
      sn_pt_float_ind_id=ncddef(fileid,'sn_pt_float_ind',((8)-(1)+1),nc_
     &stat)
      sn_pt_int_ind_id=ncddef(fileid,'sn_pt_int_ind',((11)-(1)+1),nc_sta
     &t)
      sn_seed_decimal_ind_id=ncddef(fileid,'sn_seed_decimal_ind',((34)-(
     &1)+1),nc_stat)
      sn_number_particles_id=ncvdef(fileid,'sn_number_particles',4,0,nc_
     &dims,nc_stat)
      nc_dims(1)=sn_seed_decimal_ind_id
      sn_seed_decimal_id=ncvdef(fileid,'sn_seed_decimal',2,1,nc_dims,nc_
     &stat)
      nc_dims(1)=sn_pt_float_ind_id
      nc_dims(2)=snapshot_pdf_ind_id
      sn_particles_float_id=ncvdef(fileid,'sn_particles_float',6,2,nc_di
     &ms,nc_stat)
      nc_dims(1)=sn_pt_int_ind_id
      nc_dims(2)=snapshot_pdf_ind_id
      sn_particles_int_id=ncvdef(fileid,'sn_particles_int',4,2,nc_dims,n
     &c_stat)
      
      call ncendf(fileid,nc_stat)
      call ncvpt(fileid,sn_particles_dim_id,nc_corner,nc_edge,sn_particl
     &es_dim,nc_stat)
      call ncvpt(fileid,sn_number_particles_id,nc_corner,nc_edge,sn_numb
     &er_particles,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((34)-(1)+1)
      call ncvptc(fileid,sn_seed_decimal_id,nc_corner,nc_edge,sn_seed_de
     &cimal,(((34)-(1)+1)),nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((8)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((sn_particles_dim)-(1)+1)
      call ncvpt(fileid,sn_particles_float_id,nc_corner,nc_edge,sn_parti
     &cles_float,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((11)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((sn_particles_dim)-(1)+1)
      call ncvpt(fileid,sn_particles_int_id,nc_corner,nc_edge,sn_particl
     &es_int,nc_stat)
      
      call ncclos(fileid,nc_stat)
      end if
      return
      end
      subroutine clear_output
      
      use so_mod
      
      use pr_mod
      
      use tl_mod
      
      use ou_mod
      
      use sn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical check_tally
      integer inc
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
      
      
      call mem_free_r2(output_all,(0),(1),(0),(tally_size-1),'output_all
     &')
      call mem_free_r3(output_grp,(0),(1),(0),(tally_size-1),(1),(so_grp
     &s),'output_grp')
      call mem_free_r1(output_weight_grp,(1),(so_grps),'output_weight_gr
     &p')
      call mem_free_i1(output_num_flights,(1),(so_grps),'output_num_flig
     &hts')
      call mem_free_i2(output_random_seed,(0),(8-1),(1),(so_grps),'outpu
     &t_random_seed')
      call mem_free_r2(out_post_all,(0),(1),(0),(tally_size-1),'out_post
     &_all')
      call mem_free_r3(out_post_grp,(0),(1),(0),(tally_size-1),(1),(so_g
     &rps),'out_post_grp')
      call mem_free_r5(output_2D_coupling,(output_index_1_min),(output_i
     &ndex_1_max),(output_index_2_min),(output_index_2_max),(1),(pr_back
     &ground_num),(1),(5),(1),(so_grps),'output_2D_coupling')
      call mem_free_r2(sn_particles_float,(1),(8),(1),(sn_particles_dim)
     &,'sn_particles_float')
      call mem_free_i2(sn_particles_int,(1),(11),(1),(sn_particles_dim),
     &'sn_particles_int')
      return
      end
      
      
