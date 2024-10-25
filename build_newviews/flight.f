      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine do_flights(estimator_factors)
      
      use zn_mod
      
      use pr_mod
      
      use tl_mod
      
      use sa_mod
      
      use ff_mod
      
      use sn_mod
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer nstart,nflights
      integer seed0(0:8-1)
      integer isource
      REAL(kind=DOUBLE)estimator_factors(*)
      integer i,j,new_dim
      REAL(kind=DOUBLE)mult
      integer seed(0:8-1)
      integer number_fx,source_fx,source_kseg_fx,source_xseg_fx,source_t
     &ype_fx,source_root_sp_fx,pointer_fx
      integer species_origin_fx
      integer test_origin_fx
      REAL(kind=DOUBLE)time_origin_fx,weight_origin_fx,velocity_origin_f
     &x(3)
      REAL(kind=DOUBLE)pos_origin_fx(3)
      integer cell_origin_fx,zone_origin_fx,surface_origin_fx,cell_next_
     &origin_fx,zone_next_origin_fx,sector_origin_fx,sector_next_origin_
     &fx
      integer type_origin_fx,author_origin_fx
      integer ran_index_fx
      REAL(kind=DOUBLE)ran_array_fx(0:100-1)
      integer species_stack_fx(40)
      integer test_stack_fx(40)
      REAL(kind=DOUBLE)time_stack_fx(40),weight_stack_fx(40),velocity_st
     &ack_fx(3,40)
      REAL(kind=DOUBLE)pos_stack_fx(3,40)
      integer cell_stack_fx(40),zone_stack_fx(40),surface_stack_fx(40),c
     &ell_next_stack_fx(40),zone_next_stack_fx(40),sector_stack_fx(40),s
     &ector_next_stack_fx(40)
      integer type_stack_fx(40),author_stack_fx(40)
      integer ff_temp
      logical check_tally
      integer inc
      external locate_point,check_location
      integer locate_point
      logical check_location
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
      
      
      
      if(stat_comp_frag.EQ.0.AND.stat_wt_tot_frag.EQ.(0.0_DOUBLE))then
      do i=0,stat_wt_dim_frag-1
      stat_wt_frag(i)=(0.0_DOUBLE)
      end do
      end if
      do j=1,ff_number_particles
      number_fx=ff_particles_int(1,j)
      source_fx=ff_particles_int(2,j)
      source_kseg_fx=ff_particles_int(3,j)
      source_xseg_fx=ff_particles_int(4,j)
      source_type_fx=ff_particles_int(5,j)
      source_root_sp_fx=ff_particles_int(6,j)
      species_origin_fx=ff_particles_int(7,j)
      test_origin_fx=ff_particles_int(8,j)
      pos_origin_fx(1)=ff_particles_float(6,j)
      pos_origin_fx(2)=ff_particles_float(7,j)
      pos_origin_fx(3)=ff_particles_float(8,j)
      cell_origin_fx=ff_particles_int(9,j)
      if(cell_origin_fx.EQ.314159265)then
      cell_origin_fx=locate_point(pos_origin_fx,zone_origin_fx)
      surface_origin_fx=0
      cell_next_origin_fx=0
      zone_next_origin_fx=0
      sector_origin_fx=0
      sector_next_origin_fx=0
      if((zone_type(zone_origin_fx).EQ.1).OR.(zone_type(zone_origin_fx).
     &EQ.2))continue
      else
      zone_origin_fx=ff_particles_int(10,j)
      surface_origin_fx=ff_particles_int(11,j)
      cell_next_origin_fx=ff_particles_int(12,j)
      zone_next_origin_fx=ff_particles_int(13,j)
      sector_origin_fx=ff_particles_int(14,j)
      sector_next_origin_fx=ff_particles_int(15,j)
      end if
      type_origin_fx=ff_particles_int(16,j)
      author_origin_fx=ff_particles_int(17,j)
      time_origin_fx=ff_particles_float(1,j)
      stat_wt_tot_flt=ff_particles_float(2,j)
      weight_origin_fx=(1.0_DOUBLE)
      velocity_origin_fx(1)=ff_particles_float(3,j)
      velocity_origin_fx(2)=ff_particles_float(4,j)
      velocity_origin_fx(3)=ff_particles_float(5,j)
      ran_index_fx=ff_ran_index(j)
      do ff_temp=0,100-1
      ran_array_fx(ff_temp)=ff_ran_array(ff_temp,j)
      end do
      pointer_fx=1
      species_stack_fx(pointer_fx)=species_origin_fx
      test_stack_fx(pointer_fx)=test_origin_fx
      time_stack_fx(pointer_fx)=time_origin_fx
      weight_stack_fx(pointer_fx)=weight_origin_fx
      velocity_stack_fx(1,pointer_fx)=velocity_origin_fx(1)
      velocity_stack_fx(2,pointer_fx)=velocity_origin_fx(2)
      velocity_stack_fx(3,pointer_fx)=velocity_origin_fx(3)
      
      pos_stack_fx(1,pointer_fx)=pos_origin_fx(1)
      pos_stack_fx(2,pointer_fx)=pos_origin_fx(2)
      pos_stack_fx(3,pointer_fx)=pos_origin_fx(3)
      
      cell_stack_fx(pointer_fx)=cell_origin_fx
      zone_stack_fx(pointer_fx)=zone_origin_fx
      surface_stack_fx(pointer_fx)=surface_origin_fx
      cell_next_stack_fx(pointer_fx)=cell_next_origin_fx
      zone_next_stack_fx(pointer_fx)=zone_next_origin_fx
      sector_stack_fx(pointer_fx)=sector_origin_fx
      sector_next_stack_fx(pointer_fx)=sector_next_origin_fx
      type_stack_fx(pointer_fx)=type_origin_fx
      author_stack_fx(pointer_fx)=author_origin_fx
      if((number_fx.GE.0.AND.(source_fx.GT.0).AND.(source_kseg_fx.GE.0).
     &AND.(source_xseg_fx.GE.0).AND.pointer_fx.GT.0.AND.pointer_fx.LE.40
     &.AND.((species_stack_fx(pointer_fx).GT.0.AND.species_stack_fx(poin
     &ter_fx).LE.sp_num).AND.weight_stack_fx(pointer_fx).GE.0.AND.check_
     &location(pos_stack_fx(1,pointer_fx),cell_stack_fx(pointer_fx),zone
     &_stack_fx(pointer_fx),surface_stack_fx(pointer_fx),cell_next_stack
     &_fx(pointer_fx),zone_next_stack_fx(pointer_fx),sector_stack_fx(poi
     &nter_fx),sector_next_stack_fx(pointer_fx))).AND.(test_stack_fx(poi
     &nter_fx).GT.0.AND.test_stack_fx(pointer_fx).LE.pr_test_num).AND.pr
     &oblem_test_sp(test_stack_fx(pointer_fx)).EQ.species_stack_fx(point
     &er_fx).AND.(author_stack_fx(pointer_fx).GT.0.AND.author_stack_fx(p
     &ointer_fx).LE.6+pr_reaction_num+pr_pmi_num)))continue
      call follow(number_fx,source_fx,source_kseg_fx,source_xseg_fx,sour
     &ce_type_fx,source_root_sp_fx,species_origin_fx,test_origin_fx,time
     &_origin_fx,weight_origin_fx,pos_origin_fx(1),cell_origin_fx,zone_o
     &rigin_fx,surface_origin_fx,cell_next_origin_fx,zone_next_origin_fx
     &,sector_origin_fx,sector_next_origin_fx,velocity_origin_fx(1),type
     &_origin_fx,author_origin_fx,ran_index_fx,ran_array_fx(0),species_s
     &tack_fx(1),test_stack_fx(1),time_stack_fx(1),weight_stack_fx(1),po
     &s_stack_fx(1,1),cell_stack_fx(1),zone_stack_fx(1),surface_stack_fx
     &(1),cell_next_stack_fx(1),zone_next_stack_fx(1),sector_stack_fx(1)
     &,sector_next_stack_fx(1),velocity_stack_fx(1,1),type_stack_fx(1),a
     &uthor_stack_fx(1),pointer_fx,estimator_factors)
      if(min(stat_size_frag+stat_size_flt,tally_size).GT.stat_dim_frag.A
     &ND.stat_comp_frag.EQ.1)then
      mult=max((1.0_DOUBLE),REAL(stat_size_flt,DOUBLE)/REAL(15000,DOUBLE
     &))
      new_dim=min(stat_dim_frag+int(mult*15000),tally_size)
      stat_frag =>mem_realloc_r2(stat_frag,(0),(1),(0),stat_dim_frag-1,n
     &ew_dim-1,'stat_frag')
      stat_ptr2full_frag =>mem_realloc_i1(stat_ptr2full_frag,(0),stat_pf
     &_dim_frag-1,new_dim-1,'stat_ptr2full_frag')
      stat_wt_frag =>mem_realloc_r1(stat_wt_frag,(0),stat_wt_dim_frag-1,
     &new_dim-1,'stat_wt_frag')
      stat_dim_frag=new_dim
      stat_pf_dim_frag=new_dim
      stat_wt_dim_frag=new_dim
      end if
      if(.NOT.(stat_wt_tot_flt.GE.(0.0_DOUBLE)))then
      write(0,*)' Bad value of stat_wt_tot_flt = ',stat_wt_tot_flt
      write(0,*)' Source = ',source_fx
      write(0,*)' Number = ',number_fx
      write(0,*)' Size = ',stat_size_flt
      call flush(0)
      if(stat_wt_tot_flt.GE.(0.0_DOUBLE))continue
      end if
      call stat_acc(stat_comp_flt,stat_wt_tot_flt,stat_size_flt,stat_dim
     &_flt,stat_flt,stat_pf_dim_flt,stat_ptr2full_flt,stat_ps_dim_flt,st
     &at_ptr2short_flt,stat_comp_frag,stat_wt_tot_frag,stat_size_frag,st
     &at_dim_frag,stat_frag,stat_pf_dim_frag,stat_ptr2full_frag,stat_ps_
     &dim_frag,stat_ptr2short_frag,stat_wt_dim_frag,stat_wt_frag)
      call next_seed(1,seed)
      end do
      return
      end
      subroutine follow(number_x,source_x,source_kseg_x,source_xseg_x,so
     &urce_type_x,source_root_sp_x,species_origin_x,test_origin_x,time_o
     &rigin_x,weight_origin_x,pos_origin_x,cell_origin_x,zone_origin_x,s
     &urface_origin_x,cell_next_origin_x,zone_next_origin_x,sector_origi
     &n_x,sector_next_origin_x,velocity_origin_x,type_origin_x,author_or
     &igin_x,ran_index_x,ran_array_x,species_stack_x,test_stack_x,time_s
     &tack_x,weight_stack_x,pos_stack_x,cell_stack_x,zone_stack_x,surfac
     &e_stack_x,cell_next_stack_x,zone_next_stack_x,sector_stack_x,secto
     &r_next_stack_x,velocity_stack_x,type_stack_x,author_stack_x,pointe
     &r_x,estimator_factors)
      
      use bk_mod
      
      use sp_mod
      
      use zn_mod
      
      use pr_mod
      
      use so_mod
      
      use rc_mod
      
      use tl_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      external particle_track
      logical particle_track
      integer number_x,source_x,source_kseg_x,source_xseg_x,source_type_
     &x,source_root_sp_x,pointer_x
      integer species_origin_x
      integer test_origin_x
      REAL(kind=DOUBLE)time_origin_x,weight_origin_x,velocity_origin_x(3
     &)
      REAL(kind=DOUBLE)pos_origin_x(3)
      integer cell_origin_x,zone_origin_x,surface_origin_x,cell_next_ori
     &gin_x,zone_next_origin_x,sector_origin_x,sector_next_origin_x
      integer type_origin_x,author_origin_x
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer species_stack_x(40)
      integer test_stack_x(40)
      REAL(kind=DOUBLE)time_stack_x(40),weight_stack_x(40),velocity_stac
     &k_x(3,40)
      REAL(kind=DOUBLE)pos_stack_x(3,40)
      integer cell_stack_x(40),zone_stack_x(40),surface_stack_x(40),cell
     &_next_stack_x(40),zone_next_stack_x(40),sector_stack_x(40),sector_
     &next_stack_x(40)
      integer type_stack_x(40),author_stack_x(40)
      integer i,nprod,jtemp
      REAL(kind=DOUBLE)t,tmax,ionize_rate,rnd,other_rate,rate(15),rate_a
     &ll(15),sum,t_fac
      REAL(kind=DOUBLE)estimator_factors(*)
      logical done
      integer species_prod(0:4)
      integer test_prod(0:4)
      REAL(kind=DOUBLE)time_prod(0:4),weight_prod(0:4),velocity_prod(3,0
     &:4)
      REAL(kind=DOUBLE)pos_prod(3,0:4)
      integer cell_prod(0:4),zone_prod(0:4),surface_prod(0:4),cell_next_
     &prod(0:4),zone_next_prod(0:4),sector_prod(0:4),sector_next_prod(0:
     &4)
      integer type_prod(0:4),author_prod(0:4)
      REAL(kind=DOUBLE)random
      external random
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      external locate_point,check_location
      integer locate_point
      logical check_location
      logical check_tally
      integer inc
      external find_rate
      REAL(kind=DOUBLE)find_rate
      call score_sources(2,number_x,source_x,source_kseg_x,source_xseg_x
     &,source_type_x,source_root_sp_x,species_origin_x,test_origin_x,tim
     &e_origin_x,weight_origin_x,pos_origin_x(1),cell_origin_x,zone_orig
     &in_x,surface_origin_x,cell_next_origin_x,zone_next_origin_x,sector
     &_origin_x,sector_next_origin_x,velocity_origin_x(1),type_origin_x,
     &author_origin_x,ran_index_x,ran_array_x(0),species_stack_x(1),test
     &_stack_x(1),time_stack_x(1),weight_stack_x(1),pos_stack_x(1,1),cel
     &l_stack_x(1),zone_stack_x(1),surface_stack_x(1),cell_next_stack_x(
     &1),zone_next_stack_x(1),sector_stack_x(1),sector_next_stack_x(1),v
     &elocity_stack_x(1,1),type_stack_x(1),author_stack_x(1),pointer_x,e
     &stimator_factors)
      if(.NOT.((zone_type(zone_stack_x(pointer_x)).EQ.2).OR.(zone_type(z
     &one_stack_x(pointer_x)).EQ.1)))then
      write(6,*)source_x,number_x
      write(6,*)pos_stack_x(1,pointer_x),pos_stack_x(2,pointer_x),pos_st
     &ack_x(3,pointer_x)
      if(' Bad flight origin'.EQ.' ')continue
      end if
90001 continue
      
90000 continue
      ionize_rate=(0.0_DOUBLE)
      other_rate=(0.0_DOUBLE)
      do i=1,problem_reaction_num(test_stack_x(pointer_x))
      rate(i)=(0.0_DOUBLE)
      rate_all(i)=(0.0_DOUBLE)
      end do
      if(zone_type(zone_stack_x(pointer_x)).EQ.2.AND.problem_reaction_nu
     &m(test_stack_x(pointer_x)).GT.0)then
      do i=1,problem_reaction_num(test_stack_x(pointer_x))
      rate(i)=find_rate(species_stack_x(pointer_x),test_stack_x(pointer_
     &x),time_stack_x(pointer_x),weight_stack_x(pointer_x),pos_stack_x(1
     &,pointer_x),cell_stack_x(pointer_x),zone_stack_x(pointer_x),surfac
     &e_stack_x(pointer_x),cell_next_stack_x(pointer_x),zone_next_stack_
     &x(pointer_x),sector_stack_x(pointer_x),sector_next_stack_x(pointer
     &_x),velocity_stack_x(1,pointer_x),type_stack_x(pointer_x),author_s
     &tack_x(pointer_x),problem_test_background(i,test_stack_x(pointer_x
     &)),problem_test_reaction(i,test_stack_x(pointer_x)),ran_index_x,ra
     &n_array_x(0))
      rate_all(i)=rate(i)
      if(reaction_type(problem_rc(problem_test_reaction(i,test_stack_x(p
     &ointer_x)))).EQ.'ionize_suppress')then
      ionize_rate=ionize_rate+rate(i)
      rate(i)=(0.0_DOUBLE)
      else
      other_rate=other_rate+rate(i)
      end if
      end do
      end if
      if(zone_type(zone_stack_x(pointer_x)).EQ.2.AND.other_rate.GT.(0.0_
     &DOUBLE))then
      tmax=-log(random(ran_index_x,ran_array_x(0)))/other_rate
      else
      tmax=(1.0e16_DOUBLE)
      end if
      if(so_time_dependent.EQ.1)tmax=min(tmax,so_time_final-time_stack_x
     &(pointer_x))
      if(weight_stack_x(pointer_x).LT.(1.e-3_DOUBLE))then
      if(random(ran_index_x,ran_array_x(0)).GT.(0.5_DOUBLE))then
      pointer_x=pointer_x-1
      if(pointer_x.LE.0)goto 90007
      if((number_x.GE.0.AND.(source_x.GT.0).AND.(source_kseg_x.GE.0).AND
     &.(source_xseg_x.GE.0).AND.pointer_x.GT.0.AND.pointer_x.LE.40.AND.(
     &(species_stack_x(pointer_x).GT.0.AND.species_stack_x(pointer_x).LE
     &.sp_num).AND.weight_stack_x(pointer_x).GE.0.AND.check_location(pos
     &_stack_x(1,pointer_x),cell_stack_x(pointer_x),zone_stack_x(pointer
     &_x),surface_stack_x(pointer_x),cell_next_stack_x(pointer_x),zone_n
     &ext_stack_x(pointer_x),sector_stack_x(pointer_x),sector_next_stack
     &_x(pointer_x))).AND.(test_stack_x(pointer_x).GT.0.AND.test_stack_x
     &(pointer_x).LE.pr_test_num).AND.problem_test_sp(test_stack_x(point
     &er_x)).EQ.species_stack_x(pointer_x).AND.(author_stack_x(pointer_x
     &).GT.0.AND.author_stack_x(pointer_x).LE.6+pr_reaction_num+pr_pmi_n
     &um)))continue
      goto 90001
      else
      weight_stack_x(pointer_x)=(2.0_DOUBLE)*weight_stack_x(pointer_x)
      end if
      else if((so_time_dependent.EQ.1).AND.(time_stack_x(pointer_x).GE.(
     &so_time_final-(5*EPSILON((0.0_DOUBLE))))))then
      pointer_x=pointer_x-1
      if(pointer_x.LE.0)goto 90007
      if((number_x.GE.0.AND.(source_x.GT.0).AND.(source_kseg_x.GE.0).AND
     &.(source_xseg_x.GE.0).AND.pointer_x.GT.0.AND.pointer_x.LE.40.AND.(
     &(species_stack_x(pointer_x).GT.0.AND.species_stack_x(pointer_x).LE
     &.sp_num).AND.weight_stack_x(pointer_x).GE.0.AND.check_location(pos
     &_stack_x(1,pointer_x),cell_stack_x(pointer_x),zone_stack_x(pointer
     &_x),surface_stack_x(pointer_x),cell_next_stack_x(pointer_x),zone_n
     &ext_stack_x(pointer_x),sector_stack_x(pointer_x),sector_next_stack
     &_x(pointer_x))).AND.(test_stack_x(pointer_x).GT.0.AND.test_stack_x
     &(pointer_x).LE.pr_test_num).AND.problem_test_sp(test_stack_x(point
     &er_x)).EQ.species_stack_x(pointer_x).AND.(author_stack_x(pointer_x
     &).GT.0.AND.author_stack_x(pointer_x).LE.6+pr_reaction_num+pr_pmi_n
     &um)))continue
      goto 90001
      end if
      done=particle_track(tmax,t,species_stack_x(pointer_x),test_stack_x
     &(pointer_x),time_stack_x(pointer_x),weight_stack_x(pointer_x),pos_
     &stack_x(1,pointer_x),cell_stack_x(pointer_x),zone_stack_x(pointer_
     &x),surface_stack_x(pointer_x),cell_next_stack_x(pointer_x),zone_ne
     &xt_stack_x(pointer_x),sector_stack_x(pointer_x),sector_next_stack_
     &x(pointer_x),velocity_stack_x(1,pointer_x),type_stack_x(pointer_x)
     &,author_stack_x(pointer_x))
      if(ionize_rate*t.GT.sqrt((5*EPSILON((0.0_DOUBLE)))))then
      t_fac=((1.0_DOUBLE)-exp(-ionize_rate*t))/ionize_rate
      else if(ionize_rate.GT.(5*EPSILON((0.0_DOUBLE))))then
      t_fac=t*((1.0_DOUBLE)-(0.5_DOUBLE)*ionize_rate*t)
      else
      t_fac=t
      end if
      call score_test(1,t_fac,species_stack_x(pointer_x),test_stack_x(po
     &inter_x),time_stack_x(pointer_x),weight_stack_x(pointer_x),pos_sta
     &ck_x(1,pointer_x),cell_stack_x(pointer_x),zone_stack_x(pointer_x),
     &surface_stack_x(pointer_x),cell_next_stack_x(pointer_x),zone_next_
     &stack_x(pointer_x),sector_stack_x(pointer_x),sector_next_stack_x(p
     &ointer_x),velocity_stack_x(1,pointer_x),type_stack_x(pointer_x),au
     &thor_stack_x(pointer_x),estimator_factors)
      species_prod(0)=species_stack_x(pointer_x)
      test_prod(0)=test_stack_x(pointer_x)
      time_prod(0)=time_stack_x(pointer_x)
      weight_prod(0)=weight_stack_x(pointer_x)
      velocity_prod(1,0)=velocity_stack_x(1,pointer_x)
      velocity_prod(2,0)=velocity_stack_x(2,pointer_x)
      velocity_prod(3,0)=velocity_stack_x(3,pointer_x)
      
      pos_prod(1,0)=pos_stack_x(1,pointer_x)
      pos_prod(2,0)=pos_stack_x(2,pointer_x)
      pos_prod(3,0)=pos_stack_x(3,pointer_x)
      
      cell_prod(0)=cell_stack_x(pointer_x)
      zone_prod(0)=zone_stack_x(pointer_x)
      surface_prod(0)=surface_stack_x(pointer_x)
      cell_next_prod(0)=cell_next_stack_x(pointer_x)
      zone_next_prod(0)=zone_next_stack_x(pointer_x)
      sector_prod(0)=sector_stack_x(pointer_x)
      sector_next_prod(0)=sector_next_stack_x(pointer_x)
      type_prod(0)=type_stack_x(pointer_x)
      author_prod(0)=author_stack_x(pointer_x)
      if(problem_reaction_num(test_stack_x(pointer_x)).GT.0)then
      do i=1,problem_reaction_num(test_stack_x(pointer_x))
      call score_reaction(1,t_fac,i,rate_all(i),nprod,species_prod(0),te
     &st_prod(0),time_prod(0),weight_prod(0),pos_prod(1,0),cell_prod(0),
     &zone_prod(0),surface_prod(0),cell_next_prod(0),zone_next_prod(0),s
     &ector_prod(0),sector_next_prod(0),velocity_prod(1,0),type_prod(0),
     &author_prod(0),estimator_factors,ran_index_x,ran_array_x(0))
      if(nprod.EQ.0)continue
      end do
      end if
      weight_stack_x(pointer_x)=exp(-ionize_rate*t)*weight_stack_x(point
     &er_x)
      if(done)then
      if((so_time_dependent.EQ.1).AND.(time_stack_x(pointer_x).GE.(so_ti
     &me_final-(5*EPSILON((0.0_DOUBLE))))))then
      t_fac=so_time_final-so_time_initial
      call score_test(4,t_fac,species_stack_x(pointer_x),test_stack_x(po
     &inter_x),time_stack_x(pointer_x),weight_stack_x(pointer_x),pos_sta
     &ck_x(1,pointer_x),cell_stack_x(pointer_x),zone_stack_x(pointer_x),
     &surface_stack_x(pointer_x),cell_next_stack_x(pointer_x),zone_next_
     &stack_x(pointer_x),sector_stack_x(pointer_x),sector_next_stack_x(p
     &ointer_x),velocity_stack_x(1,pointer_x),type_stack_x(pointer_x),au
     &thor_stack_x(pointer_x),estimator_factors)
      if(so_time_initialization.EQ.1)time_stack_x(pointer_x)=so_time_ini
     &tial
      else
      if(zone_type(zone_stack_x(pointer_x)).EQ.2)continue
      rnd=random(ran_index_x,ran_array_x(0))
      rnd=rnd*other_rate
      sum=(0.0_DOUBLE)
      if(problem_reaction_num(test_stack_x(pointer_x)).GT.0)continue
      do i=1,problem_reaction_num(test_stack_x(pointer_x))
      sum=sum+rate(i)
      if(sum.GE.rnd)then
      if(rate(i).GT.(0.0_DOUBLE))continue
      call score_test(2,(1.0_DOUBLE)/other_rate,species_stack_x(pointer_
     &x),test_stack_x(pointer_x),time_stack_x(pointer_x),weight_stack_x(
     &pointer_x),pos_stack_x(1,pointer_x),cell_stack_x(pointer_x),zone_s
     &tack_x(pointer_x),surface_stack_x(pointer_x),cell_next_stack_x(poi
     &nter_x),zone_next_stack_x(pointer_x),sector_stack_x(pointer_x),sec
     &tor_next_stack_x(pointer_x),velocity_stack_x(1,pointer_x),type_sta
     &ck_x(pointer_x),author_stack_x(pointer_x),estimator_factors)
      species_prod(0)=species_stack_x(pointer_x)
      test_prod(0)=test_stack_x(pointer_x)
      time_prod(0)=time_stack_x(pointer_x)
      weight_prod(0)=weight_stack_x(pointer_x)
      velocity_prod(1,0)=velocity_stack_x(1,pointer_x)
      velocity_prod(2,0)=velocity_stack_x(2,pointer_x)
      velocity_prod(3,0)=velocity_stack_x(3,pointer_x)
      
      pos_prod(1,0)=pos_stack_x(1,pointer_x)
      pos_prod(2,0)=pos_stack_x(2,pointer_x)
      pos_prod(3,0)=pos_stack_x(3,pointer_x)
      
      cell_prod(0)=cell_stack_x(pointer_x)
      zone_prod(0)=zone_stack_x(pointer_x)
      surface_prod(0)=surface_stack_x(pointer_x)
      cell_next_prod(0)=cell_next_stack_x(pointer_x)
      zone_next_prod(0)=zone_next_stack_x(pointer_x)
      sector_prod(0)=sector_stack_x(pointer_x)
      sector_next_prod(0)=sector_next_stack_x(pointer_x)
      type_prod(0)=type_stack_x(pointer_x)
      author_prod(0)=author_stack_x(pointer_x)
      call score_reaction(2,(1.0_DOUBLE)/rate(i),i,rate(i),nprod,species
     &_prod(0),test_prod(0),time_prod(0),weight_prod(0),pos_prod(1,0),ce
     &ll_prod(0),zone_prod(0),surface_prod(0),cell_next_prod(0),zone_nex
     &t_prod(0),sector_prod(0),sector_next_prod(0),velocity_prod(1,0),ty
     &pe_prod(0),author_prod(0),estimator_factors,ran_index_x,ran_array_
     &x(0))
      goto 90008
      end if
      end do
      if(.FALSE.)continue
90008 continue
      pointer_x=pointer_x-1
      if(nprod.GT.0)then
      do i=1,nprod
      if(test_prod(i).GT.0.AND.weight_prod(i).GT.(0.0_DOUBLE))then
      pointer_x=pointer_x+1
      species_stack_x(pointer_x)=species_prod(i)
      test_stack_x(pointer_x)=test_prod(i)
      time_stack_x(pointer_x)=time_prod(i)
      weight_stack_x(pointer_x)=weight_prod(i)
      velocity_stack_x(1,pointer_x)=velocity_prod(1,i)
      velocity_stack_x(2,pointer_x)=velocity_prod(2,i)
      velocity_stack_x(3,pointer_x)=velocity_prod(3,i)
      
      pos_stack_x(1,pointer_x)=pos_prod(1,i)
      pos_stack_x(2,pointer_x)=pos_prod(2,i)
      pos_stack_x(3,pointer_x)=pos_prod(3,i)
      
      cell_stack_x(pointer_x)=cell_prod(i)
      zone_stack_x(pointer_x)=zone_prod(i)
      surface_stack_x(pointer_x)=surface_prod(i)
      cell_next_stack_x(pointer_x)=cell_next_prod(i)
      zone_next_stack_x(pointer_x)=zone_next_prod(i)
      sector_stack_x(pointer_x)=sector_prod(i)
      sector_next_stack_x(pointer_x)=sector_next_prod(i)
      type_stack_x(pointer_x)=type_prod(i)
      author_stack_x(pointer_x)=author_prod(i)
      end if
      end do
      end if
      if(pointer_x.LE.0)goto 90007
      if((number_x.GE.0.AND.(source_x.GT.0).AND.(source_kseg_x.GE.0).AND
     &.(source_xseg_x.GE.0).AND.pointer_x.GT.0.AND.pointer_x.LE.40.AND.(
     &(species_stack_x(pointer_x).GT.0.AND.species_stack_x(pointer_x).LE
     &.sp_num).AND.weight_stack_x(pointer_x).GE.0.AND.check_location(pos
     &_stack_x(1,pointer_x),cell_stack_x(pointer_x),zone_stack_x(pointer
     &_x),surface_stack_x(pointer_x),cell_next_stack_x(pointer_x),zone_n
     &ext_stack_x(pointer_x),sector_stack_x(pointer_x),sector_next_stack
     &_x(pointer_x))).AND.(test_stack_x(pointer_x).GT.0.AND.test_stack_x
     &(pointer_x).LE.pr_test_num).AND.problem_test_sp(test_stack_x(point
     &er_x)).EQ.species_stack_x(pointer_x).AND.(author_stack_x(pointer_x
     &).GT.0.AND.author_stack_x(pointer_x).LE.6+pr_reaction_num+pr_pmi_n
     &um)))continue
      end if
      goto 90000
      else
      if(cell_next_stack_x(pointer_x).EQ.0)then
      if("Leaving universal cell".EQ." ")continue
      else if(sector_stack_x(pointer_x).NE.0.OR.sector_next_stack_x(poin
     &ter_x).NE.0)then
      call score_sector(number_x,source_x,source_kseg_x,source_xseg_x,so
     &urce_type_x,source_root_sp_x,species_origin_x,test_origin_x,time_o
     &rigin_x,weight_origin_x,pos_origin_x(1),cell_origin_x,zone_origin_
     &x,surface_origin_x,cell_next_origin_x,zone_next_origin_x,sector_or
     &igin_x,sector_next_origin_x,velocity_origin_x(1),type_origin_x,aut
     &hor_origin_x,ran_index_x,ran_array_x(0),species_stack_x(1),test_st
     &ack_x(1),time_stack_x(1),weight_stack_x(1),pos_stack_x(1,1),cell_s
     &tack_x(1),zone_stack_x(1),surface_stack_x(1),cell_next_stack_x(1),
     &zone_next_stack_x(1),sector_stack_x(1),sector_next_stack_x(1),velo
     &city_stack_x(1,1),type_stack_x(1),author_stack_x(1),pointer_x,esti
     &mator_factors)
      if(pointer_x.LE.0)goto 90007
      if((number_x.GE.0.AND.(source_x.GT.0).AND.(source_kseg_x.GE.0).AND
     &.(source_xseg_x.GE.0).AND.pointer_x.GT.0.AND.pointer_x.LE.40.AND.(
     &(species_stack_x(pointer_x).GT.0.AND.species_stack_x(pointer_x).LE
     &.sp_num).AND.weight_stack_x(pointer_x).GE.0.AND.check_location(pos
     &_stack_x(1,pointer_x),cell_stack_x(pointer_x),zone_stack_x(pointer
     &_x),surface_stack_x(pointer_x),cell_next_stack_x(pointer_x),zone_n
     &ext_stack_x(pointer_x),sector_stack_x(pointer_x),sector_next_stack
     &_x(pointer_x))).AND.(test_stack_x(pointer_x).GT.0.AND.test_stack_x
     &(pointer_x).LE.pr_test_num).AND.problem_test_sp(test_stack_x(point
     &er_x)).EQ.species_stack_x(pointer_x).AND.(author_stack_x(pointer_x
     &).GT.0.AND.author_stack_x(pointer_x).LE.6+pr_reaction_num+pr_pmi_n
     &um)))continue
      else
      if(surface_stack_x(pointer_x).NE.0)then
      surface_stack_x(pointer_x)=0
      cell_stack_x(pointer_x)=cell_next_stack_x(pointer_x)
      zone_stack_x(pointer_x)=zone_next_stack_x(pointer_x)
      sector_stack_x(pointer_x)=sector_next_stack_x(pointer_x)
      end if
      end if
      goto 90001
      end if
90007 continue
      return
      end
      subroutine score_sector(number_x,source_x,source_kseg_x,source_xse
     &g_x,source_type_x,source_root_sp_x,species_origin_x,test_origin_x,
     &time_origin_x,weight_origin_x,pos_origin_x,cell_origin_x,zone_orig
     &in_x,surface_origin_x,cell_next_origin_x,zone_next_origin_x,sector
     &_origin_x,sector_next_origin_x,velocity_origin_x,type_origin_x,aut
     &hor_origin_x,ran_index_x,ran_array_x,species_stack_x,test_stack_x,
     &time_stack_x,weight_stack_x,pos_stack_x,cell_stack_x,zone_stack_x,
     &surface_stack_x,cell_next_stack_x,zone_next_stack_x,sector_stack_x
     &,sector_next_stack_x,velocity_stack_x,type_stack_x,author_stack_x,
     &pointer_x,estimator_factors)
      
      use sc_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer number_x,source_x,source_kseg_x,source_xseg_x,source_type_
     &x,source_root_sp_x,pointer_x
      integer species_origin_x
      integer test_origin_x
      REAL(kind=DOUBLE)time_origin_x,weight_origin_x,velocity_origin_x(3
     &)
      REAL(kind=DOUBLE)pos_origin_x(3)
      integer cell_origin_x,zone_origin_x,surface_origin_x,cell_next_ori
     &gin_x,zone_next_origin_x,sector_origin_x,sector_next_origin_x
      integer type_origin_x,author_origin_x
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer species_stack_x(40)
      integer test_stack_x(40)
      REAL(kind=DOUBLE)time_stack_x(40),weight_stack_x(40),velocity_stac
     &k_x(3,40)
      REAL(kind=DOUBLE)pos_stack_x(3,40)
      integer cell_stack_x(40),zone_stack_x(40),surface_stack_x(40),cell
     &_next_stack_x(40),zone_next_stack_x(40),sector_stack_x(40),sector_
     &next_stack_x(40)
      integer type_stack_x(40),author_stack_x(40)
      REAL(kind=DOUBLE)estimator_factors(*)
      integer type,type_ptr_1,type_ptr_2,type_ptr,nprod,i
      REAL(kind=DOUBLE)mat_temp,mat_recyc_coef
      integer species_prod(0:4)
      integer test_prod(0:4)
      REAL(kind=DOUBLE)time_prod(0:4),weight_prod(0:4),velocity_prod(3,0
     &:4)
      REAL(kind=DOUBLE)pos_prod(3,0:4)
      integer cell_prod(0:4),zone_prod(0:4),surface_prod(0:4),cell_next_
     &prod(0:4),zone_next_prod(0:4),sector_prod(0:4),sector_next_prod(0:
     &4)
      integer type_prod(0:4),author_prod(0:4)
      integer sector1
      integer sector2
      integer mat_ref
      external locate_point,check_location
      integer locate_point
      logical check_location
      sector1=sector_stack_x(pointer_x)
      sector2=sector_next_stack_x(pointer_x)
      nprod=0
      species_prod(0)=species_stack_x(pointer_x)
      test_prod(0)=test_stack_x(pointer_x)
      time_prod(0)=time_stack_x(pointer_x)
      weight_prod(0)=weight_stack_x(pointer_x)
      velocity_prod(1,0)=velocity_stack_x(1,pointer_x)
      velocity_prod(2,0)=velocity_stack_x(2,pointer_x)
      velocity_prod(3,0)=velocity_stack_x(3,pointer_x)
      
      pos_prod(1,0)=pos_stack_x(1,pointer_x)
      pos_prod(2,0)=pos_stack_x(2,pointer_x)
      pos_prod(3,0)=pos_stack_x(3,pointer_x)
      
      cell_prod(0)=cell_stack_x(pointer_x)
      zone_prod(0)=zone_stack_x(pointer_x)
      surface_prod(0)=surface_stack_x(pointer_x)
      cell_next_prod(0)=cell_next_stack_x(pointer_x)
      zone_next_prod(0)=zone_next_stack_x(pointer_x)
      sector_prod(0)=sector_stack_x(pointer_x)
      sector_next_prod(0)=sector_next_stack_x(pointer_x)
      type_prod(0)=type_stack_x(pointer_x)
      author_prod(0)=author_stack_x(pointer_x)
      if(surface_stack_x(pointer_x).NE.0)then
      surface_stack_x(pointer_x)=0
      cell_stack_x(pointer_x)=cell_next_stack_x(pointer_x)
      zone_stack_x(pointer_x)=zone_next_stack_x(pointer_x)
      sector_stack_x(pointer_x)=sector_next_stack_x(pointer_x)
      end if
      do type=1,6+0-1
      type_ptr_1=sector_type_pointer(type,sector1)
      type_ptr_2=sector_type_pointer(type,sector2)
      if(type_ptr_1.NE.2000000000.AND.type_ptr_1.NE.1000000000)then
      type_ptr=type_ptr_1
      else if(type_ptr_2.NE.2000000000.AND.type_ptr_2.NE.1000000000)then
      if(type_ptr_1.EQ.2000000000.OR.type_ptr_1.EQ.1000000000)continue
      type_ptr=type_ptr_2
      else
      type_ptr=0
      end if
      if(type_ptr.NE.0)then
      if(nprod.EQ.0)continue
      if(type.EQ.5)then
      if((type_ptr.GT.0.AND.type_ptr.LE.sc_exit_num))continue
      pointer_x=pointer_x-1
      else if(type.EQ.4)then
      if((type_ptr.GT.0.AND.type_ptr.LE.sc_wall_num))continue
      mat_ref=wall_material(type_ptr)
      mat_temp=wall_temperature(type_ptr)
      mat_recyc_coef=wall_recyc_coef(type_ptr)
      call process_pmi(mat_ref,mat_temp,mat_recyc_coef,nprod,species_pro
     &d(0),test_prod(0),time_prod(0),weight_prod(0),pos_prod(1,0),cell_p
     &rod(0),zone_prod(0),surface_prod(0),cell_next_prod(0),zone_next_pr
     &od(0),sector_prod(0),sector_next_prod(0),velocity_prod(1,0),type_p
     &rod(0),author_prod(0),ran_index_x,ran_array_x(0))
      pointer_x=pointer_x-1
      else if(type.EQ.3)then
      if((type_ptr.GT.0.AND.type_ptr.LE.sc_target_num))continue
      mat_ref=target_material(type_ptr)
      mat_temp=target_temperature(type_ptr)
      mat_recyc_coef=target_recyc_coef(type_ptr)
      call process_pmi(mat_ref,mat_temp,mat_recyc_coef,nprod,species_pro
     &d(0),test_prod(0),time_prod(0),weight_prod(0),pos_prod(1,0),cell_p
     &rod(0),zone_prod(0),surface_prod(0),cell_next_prod(0),zone_next_pr
     &od(0),sector_prod(0),sector_next_prod(0),velocity_prod(1,0),type_p
     &rod(0),author_prod(0),ran_index_x,ran_array_x(0))
      pointer_x=pointer_x-1
      else if(type.EQ.2)then
      if((type_ptr.GT.0.AND.type_ptr.LE.sc_plasma_num))continue
      end if
      end if
      end do
      call score_diagnostics(nprod,species_prod(0),test_prod(0),time_pro
     &d(0),weight_prod(0),pos_prod(1,0),cell_prod(0),zone_prod(0),surfac
     &e_prod(0),cell_next_prod(0),zone_next_prod(0),sector_prod(0),secto
     &r_next_prod(0),velocity_prod(1,0),type_prod(0),author_prod(0),esti
     &mator_factors)
      if(nprod.GT.0)then
      do i=1,nprod
      if(test_prod(i).GT.0.AND.weight_prod(i).GT.(0.0_DOUBLE))then
      if(surface_prod(i).NE.0)then
      surface_prod(i)=0
      cell_prod(i)=cell_next_prod(i)
      zone_prod(i)=zone_next_prod(i)
      sector_prod(i)=sector_next_prod(i)
      end if
      if(check_location(pos_prod(1,i),cell_prod(i),zone_prod(i),surface_
     &prod(i),cell_next_prod(i),zone_next_prod(i),sector_prod(i),sector_
     &next_prod(i)))continue
      pointer_x=pointer_x+1
      species_stack_x(pointer_x)=species_prod(i)
      test_stack_x(pointer_x)=test_prod(i)
      time_stack_x(pointer_x)=time_prod(i)
      weight_stack_x(pointer_x)=weight_prod(i)
      velocity_stack_x(1,pointer_x)=velocity_prod(1,i)
      velocity_stack_x(2,pointer_x)=velocity_prod(2,i)
      velocity_stack_x(3,pointer_x)=velocity_prod(3,i)
      
      pos_stack_x(1,pointer_x)=pos_prod(1,i)
      pos_stack_x(2,pointer_x)=pos_prod(2,i)
      pos_stack_x(3,pointer_x)=pos_prod(3,i)
      
      cell_stack_x(pointer_x)=cell_prod(i)
      zone_stack_x(pointer_x)=zone_prod(i)
      surface_stack_x(pointer_x)=surface_prod(i)
      cell_next_stack_x(pointer_x)=cell_next_prod(i)
      zone_next_stack_x(pointer_x)=zone_next_prod(i)
      sector_stack_x(pointer_x)=sector_prod(i)
      sector_next_stack_x(pointer_x)=sector_next_prod(i)
      type_stack_x(pointer_x)=type_prod(i)
      author_stack_x(pointer_x)=author_prod(i)
      end if
      end do
      end if
      return
      end
      subroutine process_pmi(mat_ref,mat_temp,mat_recyc_coef,nprod,speci
     &es_prod,test_prod,time_prod,weight_prod,pos_prod,cell_prod,zone_pr
     &od,surface_prod,cell_next_prod,zone_next_prod,sector_prod,sector_n
     &ext_prod,velocity_prod,type_prod,author_prod,ran_index_x,ran_array
     &_x)
      
      use pr_mod
      
      use sp_mod
      
      use pm_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer mat_ref
      REAL(kind=DOUBLE)mat_temp,mat_recyc_coef
      integer nprod
      integer species_prod(0:4)
      integer test_prod(0:4)
      REAL(kind=DOUBLE)time_prod(0:4),weight_prod(0:4),velocity_prod(3,0
     &:4)
      REAL(kind=DOUBLE)pos_prod(3,0:4)
      integer cell_prod(0:4),zone_prod(0:4),surface_prod(0:4),cell_next_
     &prod(0:4),zone_next_prod(0:4),sector_prod(0:4),sector_next_prod(0:
     &4)
      integer type_prod(0:4),author_prod(0:4)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer i,imin
      REAL(kind=DOUBLE)yield(0:8),independent_parameters(10)
      REAL(kind=DOUBLE)cos_angle,sum,rnd,sum_refl
      integer ts_pmi
      integer default_pmi
      external find_yield,intersection_direction
      REAL(kind=DOUBLE)find_yield,intersection_direction
      REAL(kind=DOUBLE)random
      external random
      if(mat_ref.NE.0)continue
      do i=1,10
      independent_parameters(i)=(0.0_DOUBLE)
      end do
      independent_parameters(1)=(0.5_DOUBLE)*species_m(problem_test_sp(t
     &est_prod(0)))*(velocity_prod(1,0)**2+velocity_prod(2,0)**2+velocit
     &y_prod(3,0)**2)
      independent_parameters(2)=velocity_prod(1,0)
      independent_parameters(3)=velocity_prod(2,0)
      independent_parameters(4)=velocity_prod(3,0)
      cos_angle=intersection_direction(surface_prod(0),pos_prod(1,0),vel
     &ocity_prod(1,0))
      independent_parameters(10)=cos_angle
      independent_parameters(5)=acos(cos_angle)
      independent_parameters(6)=mat_temp
      if(mat_recyc_coef.GE.(0.0_DOUBLE))continue
      if(mat_recyc_coef.LE.(1.0_DOUBLE))continue
      yield(0)=(1.0_DOUBLE)-mat_recyc_coef
      sum=yield(0)
      sum_refl=(0.0_DOUBLE)
      default_pmi=0
      if(problem_pmi_case_num(test_prod(0)).GT.0)continue
      do ts_pmi=1,problem_pmi_case_num(test_prod(0))
      if(pmi_materials(problem_pmi_ref(problem_pmi_cases(ts_pmi,test_pro
     &d(0)))).EQ.mat_ref)then
      yield(ts_pmi)=find_yield(species_prod(0),test_prod(0),time_prod(0)
     &,weight_prod(0),pos_prod(1,0),cell_prod(0),zone_prod(0),surface_pr
     &od(0),cell_next_prod(0),zone_next_prod(0),sector_prod(0),sector_ne
     &xt_prod(0),velocity_prod(1,0),type_prod(0),author_prod(0),ts_pmi,i
     &ndependent_parameters,ran_index_x,ran_array_x(0))
      if(yield(ts_pmi).LT.(0.0_DOUBLE))then
      default_pmi=ts_pmi
      yield(ts_pmi)=(0.0_DOUBLE)
      end if
      else
      yield(ts_pmi)=(0.0_DOUBLE)
      end if
      sum=sum+(yield(ts_pmi))
      sum_refl=sum_refl+(yield(ts_pmi))
      end do
      if(default_pmi.GT.0.AND.sum.LT.(1.0_DOUBLE))yield(default_pmi)=(1.
     &0_DOUBLE)-sum
      rnd=random(ran_index_x,ran_array_x(0))
      sum=(0.0_DOUBLE)
      if(weight_prod(0).LT.(1.e4_DOUBLE).OR.(sum_refl+yield(0).GE.(1.0_D
     &OUBLE)))then
      imin=0
      else
      imin=1
      end if
      do i=imin,problem_pmi_case_num(test_prod(0))
      sum=sum+yield(i)
      if(sum.GE.rnd)then
      ts_pmi=i
      goto 90008
      end if
      end do
      ts_pmi=default_pmi
      if(sum.LT.(1.0_DOUBLE))continue
90008 continue
      if(ts_pmi.GT.0)then
      call pick_pmi(ts_pmi,independent_parameters,nprod,species_prod(0),
     &test_prod(0),time_prod(0),weight_prod(0),pos_prod(1,0),cell_prod(0
     &),zone_prod(0),surface_prod(0),cell_next_prod(0),zone_next_prod(0)
     &,sector_prod(0),sector_next_prod(0),velocity_prod(1,0),type_prod(0
     &),author_prod(0),ran_index_x,ran_array_x(0))
      if(ts_pmi.EQ.default_pmi.AND.yield(0).GT.(0.0_DOUBLE).AND.imin.EQ.
     &1)then
      do i=1,nprod
      weight_prod(i)=weight_prod(i)*((1.0_DOUBLE)-yield(0)/((1.0_DOUBLE)
     &-sum_refl))
      if(weight_prod(i).GT.(1.e-1_DOUBLE)*(1.e4_DOUBLE))continue
      end do
      end if
      else
      if(yield(0).GT.(0.0_DOUBLE))continue
      nprod=0
      end if
      return
      end
      
      
