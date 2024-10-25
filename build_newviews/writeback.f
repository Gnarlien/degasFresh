      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine read_uedge_background(file,update)
      
      use sp_mod
      
      use so_mod
      
      use sc_mod
      
      use pr_mod
      
      use bk_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer update
      character*96 file
      integer nx,ny,nxpt,ns
      integer ixlb(2),ixrb(2)
      REAL(kind=DOUBLE)rm(150,150,0:4),zm(150,150,0:4),ni(150,150,11),vr
     &(150,150,11),vz(150,150,11),vphi(150,150,11),te(150,150),ti(150,15
     &0),ni1(150,11,2),vr1(150,11,2),vz1(150,11,2),vphi1(150,11,2),ti1(1
     &50,2),te1(150,2),flux1(150,11,2),ni2(150,11,2),vr2(150,11,2),vz2(1
     &50,11,2),vphi2(150,11,2),ti2(150,2),te2(150,2),flux2(150,11,2),flu
     &x1_tot(11,2),flux2_tot(11,2)
      character*8 uedge_species(11)
      integer tot,js,isp,sp_back,itarg,grp,ixpt
      integer flux1_present(2),flux2_present(2),group_map(2,11,2)
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
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
      
      
      external neutralize_species
      integer neutralize_species
      call init_uedge_background(file,nx,ny,nxpt,ixlb,ixrb,rm,zm,ni,vr,v
     &z,vphi,ti,te,ni1,vr1,vz1,vphi1,ti1,te1,flux1,ni2,vr2,vz2,vphi2,ti2
     &,te2,flux2,flux1_tot,flux2_tot,ns,uedge_species)
      so_grps=0
      so_seg_tot=0
      do ixpt=1,nxpt
      flux1_present(ixpt)=0
      flux2_present(ixpt)=0
      do js=1,ns
      group_map(1,js,ixpt)=1000000000
      group_map(2,js,ixpt)=1000000000
      isp=string_lookup(uedge_species(js),species_sy,sp_num)
      if((isp.GT.0.AND.isp.LE.sp_num))then
      sp_back=problem_species_background(isp)
      if((sp_back.GT.0.AND.sp_back.LE.bk_num))then
      if(flux1_tot(js,ixpt).GT.(0.0_DOUBLE))then
      so_grps=so_grps+1
      so_seg_tot=so_seg_tot+(ny)
      group_map(1,js,ixpt)=so_grps
      flux1_present(ixpt)=1
      end if
      if(flux2_tot(js,ixpt).GT.(0.0_DOUBLE))then
      so_grps=so_grps+1
      so_seg_tot=so_seg_tot+(ny)
      group_map(2,js,ixpt)=so_grps
      flux2_present(ixpt)=1
      end if
      end if
      end if
      end do
      end do
      if(so_grps.EQ.0)then
      if('Zero fluxes in UEDGE background'.EQ.' ')continue
      end if
      if(update.EQ.1)then
      do grp=1,so_grps
      source_total_current(grp)=(0.0_DOUBLE)
      end do
      else
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
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_base_ptr =>mem_realloc_i1(source_base_ptr,(1),((int((((0)-(
     &1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1)+1)+100-1)/100)
     &*100)+(1)-1),'source_base_ptr')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_num_segments =>mem_realloc_i1(source_num_segments,(1),((int
     &((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1)+1)+100
     &-1)/100)*100)+(1)-1),'source_num_segments')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_type =>mem_realloc_i1(source_type,(1),((int((((0)-(1)+1)+10
     &0-1)/100)*100)+(1)-1),((int((((so_grps)-(1)+1)+100-1)/100)*100)+(1
     &)-1),'source_type')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_geometry =>mem_realloc_i1(source_geometry,(1),((int((((0)-(
     &1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1)+1)+100-1)/100)
     &*100)+(1)-1),'source_geometry')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_num_flights =>mem_realloc_i1(source_num_flights,(1),((int((
     &((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1)+1)+100-1
     &)/100)*100)+(1)-1),'source_num_flights')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_num_checkpoints =>mem_realloc_i1(source_num_checkpoints,(1)
     &,((int((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1)+
     &1)+100-1)/100)*100)+(1)-1),'source_num_checkpoints')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_species =>mem_realloc_i1(source_species,(1),((int((((0)-(1)
     &+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1)+1)+100-1)/100)*1
     &00)+(1)-1),'source_species')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_root_species =>mem_realloc_i1(source_root_species,(1),((int
     &((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1)+1)+100
     &-1)/100)*100)+(1)-1),'source_root_species')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_time_variation =>mem_realloc_i1(source_time_variation,(1),(
     &(int((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1)+1)
     &+100-1)/100)*100)+(1)-1),'source_time_variation')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_num_gparameters =>mem_realloc_i1(source_num_gparameters,(1)
     &,((int((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1)+
     &1)+100-1)/100)*100)+(1)-1),'source_num_gparameters')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_num_parameters =>mem_realloc_i1(source_num_parameters,(1),(
     &(int((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1)+1)
     &+100-1)/100)*100)+(1)-1),'source_num_parameters')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_gparameters_base =>mem_realloc_i1(source_gparameters_base,(
     &1),((int((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1
     &)+1)+100-1)/100)*100)+(1)-1),'source_gparameters_base')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_parameters_base =>mem_realloc_i1(source_parameters_base,(1)
     &,((int((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1)+
     &1)+100-1)/100)*100)+(1)-1),'source_parameters_base')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_parameters_data_base =>mem_realloc_i1(source_parameters_dat
     &a_base,(1),((int((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_
     &grps)-(1)+1)+100-1)/100)*100)+(1)-1),'source_parameters_data_base'
     &)
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_num_giparameters =>mem_realloc_i1(source_num_giparameters,(
     &1),((int((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1
     &)+1)+100-1)/100)*100)+(1)-1),'source_num_giparameters')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_num_iparameters =>mem_realloc_i1(source_num_iparameters,(1)
     &,((int((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1)+
     &1)+100-1)/100)*100)+(1)-1),'source_num_iparameters')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_giparameters_base =>mem_realloc_i1(source_giparameters_base
     &,(1),((int((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-
     &(1)+1)+100-1)/100)*100)+(1)-1),'source_giparameters_base')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_iparameters_base =>mem_realloc_i1(source_iparameters_base,(
     &1),((int((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1
     &)+1)+100-1)/100)*100)+(1)-1),'source_iparameters_base')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_iparameters_data_base =>mem_realloc_i1(source_iparameters_d
     &ata_base,(1),((int((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((s
     &o_grps)-(1)+1)+100-1)/100)*100)+(1)-1),'source_iparameters_data_ba
     &se')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_total_current =>mem_realloc_r1(source_total_current,(1),((i
     &nt((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1)+1)+1
     &00-1)/100)*100)+(1)-1),'source_total_current')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_weight_norm =>mem_realloc_r1(source_weight_norm,(1),((int((
     &((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1)+1)+100-1
     &)/100)*100)+(1)-1),'source_weight_norm')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_grps)-(1)+1)+1
     &00-1)/100)*100))then
      source_scale_factor =>mem_realloc_r1(source_scale_factor,(1),((int
     &((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_grps)-(1)+1)+100
     &-1)/100)*100)+(1)-1),'source_scale_factor')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_seg_tot)-(1)+1
     &)+100-1)/100)*100))then
      source_segment_ptr =>mem_realloc_i1(source_segment_ptr,(1),((int((
     &((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_seg_tot)-(1)+1)+10
     &0-1)/100)*100)+(1)-1),'source_segment_ptr')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_seg_tot)-(1)+1
     &)+100-1)/100)*100))then
      source_current =>mem_realloc_r1(source_current,(1),((int((((0)-(1)
     &+1)+100-1)/100)*100)+(1)-1),((int((((so_seg_tot)-(1)+1)+100-1)/100
     &)*100)+(1)-1),'source_current')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_seg_tot)-(1)+1
     &)+100-1)/100)*100))then
      source_segment_rel_wt =>mem_realloc_r1(source_segment_rel_wt,(1),(
     &(int((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_seg_tot)-(1)
     &+1)+100-1)/100)*100)+(1)-1),'source_segment_rel_wt')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_seg_tot)-(1)+1
     &)+100-1)/100)*100))then
      source_segment_prob_alias =>mem_realloc_r1(source_segment_prob_ali
     &as,(1),((int((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_seg_
     &tot)-(1)+1)+100-1)/100)*100)+(1)-1),'source_segment_prob_alias')
      end if
      if((int((((0)-(1)+1)+100-1)/100)*100).NE.(int((((so_seg_tot)-(1)+1
     &)+100-1)/100)*100))then
      source_segment_ptr_alias =>mem_realloc_i1(source_segment_ptr_alias
     &,(1),((int((((0)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((so_seg_to
     &t)-(1)+1)+100-1)/100)*100)+(1)-1),'source_segment_ptr_alias')
      end if
      tot=0
      do ixpt=1,nxpt
      do js=1,ns
      do itarg=1,2
      if(group_map(itarg,js,ixpt).NE.1000000000)then
      grp=group_map(itarg,js,ixpt)
      source_root_species(grp)=string_lookup(uedge_species(js),species_s
     &y,sp_num)
      if((source_root_species(grp).GT.0.AND.source_root_species(grp).LE.
     &sp_num))continue
      source_time_variation(grp)=0
      source_species(grp)=neutralize_species(source_root_species(grp))
      if((source_species(grp).GT.0.AND.source_species(grp).LE.sp_num))co
     &ntinue
      source_total_current(grp)=(0.0_DOUBLE)
      source_type(grp)=1
      source_num_segments(grp)=ny
      source_geometry(grp)=2
      source_scale_factor(grp)=(1.0_DOUBLE)
      source_num_flights(grp)=100
      source_num_checkpoints(grp)=0
      source_base_ptr(grp)=tot+1
      tot=tot+(ny)
      source_num_gparameters(grp)=0
      source_gparameters_base(grp)=so_gparams_list_size
      source_parameters_base(grp)=so_params_list_size
      source_parameters_data_base(grp)=so_params_data_size
      source_num_giparameters(grp)=0
      source_num_iparameters(grp)=0
      source_giparameters_base(grp)=so_giparams_list_size
      source_iparameters_base(grp)=so_iparams_list_size
      source_iparameters_data_base(grp)=so_iparams_data_size
      source_num_parameters(grp)=3
      so_params_list_size=so_params_list_size+(source_num_parameters(grp
     &))
      if(so_params_list_size.GT.so_params_list_dim)then
      source_parameters_list =>mem_realloc_i1(source_parameters_list,(1)
     &,so_params_list_dim,so_params_list_size,'source_parameters_list')
      so_params_list_dim=so_params_list_size
      end if
      source_parameters_list(source_parameters_base(grp)+1)=5
      source_parameters_list(source_parameters_base(grp)+2)=6
      source_parameters_list(source_parameters_base(grp)+3)=7
      end if
      end do
      end do
      end do
      end if
      call set_uedge_plate_sources(nx,ny,nxpt,ixlb,ixrb,ns,uedge_species
     &,rm,zm,ni1,vr1,vz1,vphi1,te1,ti1,flux1,ni2,vr2,vz2,vphi2,te2,ti2,f
     &lux2,flux1_present,flux2_present,group_map,update)
      return
      end
      subroutine init_uedge_background(file,nx,ny,nxpt,ixlb,ixrb,rm,zm,n
     &i,vr,vz,vphi,ti,te,ni1,vr1,vz1,vphi1,ti1,te1,flux1,ni2,vr2,vz2,vph
     &i2,ti2,te2,flux2,flux1_tot,flux2_tot,ns,uedge_species)
      
      
      
      
      
      
      
      
      
      
      
      use zn_mod
      
      use bk_mod
      
      use sp_mod
      
      use pr_mod
      
      use g2_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*96 file
      integer nx,ny,nxpt,ns
      integer ixlb(2),ixrb(2)
      REAL(kind=DOUBLE)rm(150,150,0:4),zm(150,150,0:4),ni(150,150,11),vr
     &(150,150,11),vz(150,150,11),vphi(150,150,11),te(150,150),ti(150,15
     &0),ni1(150,11,2),vr1(150,11,2),vz1(150,11,2),vphi1(150,11,2),ti1(1
     &50,2),te1(150,2),flux1(150,11,2),ni2(150,11,2),vr2(150,11,2),vz2(1
     &50,11,2),vphi2(150,11,2),ti2(150,2),te2(150,2),flux2(150,11,2),flu
     &x1_tot(11,2),flux2_tot(11,2)
      character*8 uedge_species(11)
      integer nu,i,j1,j2,j3,js,sp_back,length,p,b,e,nstratum,is,open_sta
     &t,fileid,ipoly,isp,dim_stratum,stratum,species_index,stratum_index
     &,zone,first_zone,match,diskin2,ixpt
      integer iysptrx1(2),iysptrx2(2),ixpt1(2),ixmdp(2),ixpt2(2)
      REAL(kind=DOUBLE)zone_ni(11),zone_vr(11),zone_vz(11),zone_vphi(11)
      REAL(kind=DOUBLE)dummy,zone_ti,zone_te,zone_ne
      character*300 line,keyword
      character*96 uedge_file,polygon_file
      REAL(kind=DOUBLE),dimension(:,:),pointer::bdy_ni
      REAL(kind=DOUBLE),dimension(:,:),pointer::bdy_vr
      REAL(kind=DOUBLE),dimension(:,:),pointer::bdy_vz
      REAL(kind=DOUBLE),dimension(:,:),pointer::bdy_vphi
      REAL(kind=DOUBLE),dimension(:),pointer::bdy_ti
      REAL(kind=DOUBLE),dimension(:),pointer::bdy_te
      integer,dimension(:),pointer::stratum_list
      integer,dimension(:),pointer::zone_stratum
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
      REAL(kind=DOUBLE)vector_temp(3)
      logical check_zone
      dim_stratum=100
      bdy_ni =>mem_alloc_r2((1),(11),(1),(dim_stratum),'bdy_ni')
      bdy_vr =>mem_alloc_r2((1),(11),(1),(dim_stratum),'bdy_vr')
      bdy_vz =>mem_alloc_r2((1),(11),(1),(dim_stratum),'bdy_vz')
      bdy_vphi =>mem_alloc_r2((1),(11),(1),(dim_stratum),'bdy_vphi')
      bdy_ti =>mem_alloc_r1((1),(dim_stratum),'bdy_ti')
      bdy_te =>mem_alloc_r1((1),(dim_stratum),'bdy_te')
      stratum_list =>mem_alloc_i1((1),(dim_stratum),'stratum_list')
      zone_stratum =>mem_alloc_i1((1),(zn_num),'zone_stratum')
      background_coords=2
      uedge_file='UNINITIALIZED'
      ns=0
      polygon_file='UNINITIALIZED'
      nstratum=0
      diskin2=20+2
      if(index(file,'.u').GT.0)then
      uedge_file=file
      ns=1
      uedge_species(ns)='D+'
      else
      open(unit=diskin2,file=file,status='old',form='formatted',iostat=o
     &pen_stat)
      if(open_stat.NE.0)then
      write(0,*)' Cannot open file ',file,', error number ',open_stat
      return
      end if
90001 continue
      if(read_string(diskin2,line,length))then
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      keyword=line(b:e)
      if(keyword.EQ.'uedge_file')then
      if(next_token(line,b,e,p))continue
      uedge_file=line(b:e)
      else if(keyword.EQ.'ion_species')then
90013 continue
      if(next_token(line,b,e,p))then
      ns=ns+1
      if(ns.LE.11)continue
      uedge_species(ns)=line(b:e)
      go to 90013
      end if
      else if(keyword.EQ.'polygon_file')then
      if(next_token(line,b,e,p))continue
      polygon_file=line(b:e)
      else if(keyword.EQ.'bdy_ni')then
      if(next_token(line,b,e,p))continue
      if(ns.GT.0)continue
      species_index=0
      do is=1,ns
      if(uedge_species(is).EQ.line(b:e))then
      if(species_index.EQ.0)continue
      species_index=is
      end if
      end do
      
      if(species_index.GT.0)continue
      if(next_token(line,b,e,p))continue
      stratum=read_integer(line(b:e))
      
      match=0
      if(nstratum.GT.0)then
      do is=1,nstratum
      if(stratum_list(is).EQ.stratum)then
      match=1
      stratum_index=is
      end if
      end do
      end if
      if(match.EQ.0)then
      nstratum=nstratum+1
      dim_stratum=max(dim_stratum,nstratum)
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_ni =>mem_realloc_r2(bdy_ni,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_ni')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vr =>mem_realloc_r2(bdy_vr,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_vr')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vz =>mem_realloc_r2(bdy_vz,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_vz')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vphi =>mem_realloc_r2(bdy_vphi,(1),(11),(1),(((int(((((dim_str
     &atum)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-
     &(1)+1))+100-1)/100)*100)+(1)-1),'bdy_vphi')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_te =>mem_realloc_r1(bdy_te,(1),(((int(((((dim_stratum)-(1)+1))
     &+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+1))+100-1
     &)/100)*100)+(1)-1),'bdy_te')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_ti =>mem_realloc_r1(bdy_ti,(1),(((int(((((dim_stratum)-(1)+1))
     &+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+1))+100-1
     &)/100)*100)+(1)-1),'bdy_ti')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      stratum_list =>mem_realloc_i1(stratum_list,(1),(((int(((((dim_stra
     &tum)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(
     &1)+1))+100-1)/100)*100)+(1)-1),'stratum_list')
      end if
      if(ns.GT.0)continue
      do is=1,ns
      bdy_ni(is,nstratum)=(0.0_DOUBLE)
      bdy_vr(is,nstratum)=(0.0_DOUBLE)
      bdy_vz(is,nstratum)=(0.0_DOUBLE)
      bdy_vphi(is,nstratum)=(0.0_DOUBLE)
      end do
      bdy_te(nstratum)=(0.0_DOUBLE)
      bdy_ti(nstratum)=(0.0_DOUBLE)
      stratum_index=nstratum
      stratum_list(stratum_index)=stratum
      end if
      
      if((stratum_index.GT.0).AND.(stratum_index.LE.nstratum))continue
      if(next_token(line,b,e,p))continue
      bdy_ni(species_index,stratum_index)=read_real(line(b:e))
      if(bdy_ni(species_index,stratum_index).GE.(0.0_DOUBLE))continue
      else if(keyword.EQ.'bdy_vr')then
      if(next_token(line,b,e,p))continue
      if(ns.GT.0)continue
      species_index=0
      do is=1,ns
      if(uedge_species(is).EQ.line(b:e))then
      if(species_index.EQ.0)continue
      species_index=is
      end if
      end do
      
      if(species_index.GT.0)continue
      if(next_token(line,b,e,p))continue
      stratum=read_integer(line(b:e))
      
      match=0
      if(nstratum.GT.0)then
      do is=1,nstratum
      if(stratum_list(is).EQ.stratum)then
      match=1
      stratum_index=is
      end if
      end do
      end if
      if(match.EQ.0)then
      nstratum=nstratum+1
      dim_stratum=max(dim_stratum,nstratum)
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_ni =>mem_realloc_r2(bdy_ni,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_ni')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vr =>mem_realloc_r2(bdy_vr,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_vr')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vz =>mem_realloc_r2(bdy_vz,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_vz')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vphi =>mem_realloc_r2(bdy_vphi,(1),(11),(1),(((int(((((dim_str
     &atum)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-
     &(1)+1))+100-1)/100)*100)+(1)-1),'bdy_vphi')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_te =>mem_realloc_r1(bdy_te,(1),(((int(((((dim_stratum)-(1)+1))
     &+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+1))+100-1
     &)/100)*100)+(1)-1),'bdy_te')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_ti =>mem_realloc_r1(bdy_ti,(1),(((int(((((dim_stratum)-(1)+1))
     &+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+1))+100-1
     &)/100)*100)+(1)-1),'bdy_ti')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      stratum_list =>mem_realloc_i1(stratum_list,(1),(((int(((((dim_stra
     &tum)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(
     &1)+1))+100-1)/100)*100)+(1)-1),'stratum_list')
      end if
      if(ns.GT.0)continue
      do is=1,ns
      bdy_ni(is,nstratum)=(0.0_DOUBLE)
      bdy_vr(is,nstratum)=(0.0_DOUBLE)
      bdy_vz(is,nstratum)=(0.0_DOUBLE)
      bdy_vphi(is,nstratum)=(0.0_DOUBLE)
      end do
      bdy_te(nstratum)=(0.0_DOUBLE)
      bdy_ti(nstratum)=(0.0_DOUBLE)
      stratum_index=nstratum
      stratum_list(stratum_index)=stratum
      end if
      
      if((stratum_index.GT.0).AND.(stratum_index.LE.nstratum))continue
      if(next_token(line,b,e,p))continue
      bdy_vr(species_index,stratum_index)=read_real(line(b:e))
      else if(keyword.EQ.'bdy_vz')then
      if(next_token(line,b,e,p))continue
      if(ns.GT.0)continue
      species_index=0
      do is=1,ns
      if(uedge_species(is).EQ.line(b:e))then
      if(species_index.EQ.0)continue
      species_index=is
      end if
      end do
      
      if(species_index.GT.0)continue
      if(next_token(line,b,e,p))continue
      stratum=read_integer(line(b:e))
      
      match=0
      if(nstratum.GT.0)then
      do is=1,nstratum
      if(stratum_list(is).EQ.stratum)then
      match=1
      stratum_index=is
      end if
      end do
      end if
      if(match.EQ.0)then
      nstratum=nstratum+1
      dim_stratum=max(dim_stratum,nstratum)
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_ni =>mem_realloc_r2(bdy_ni,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_ni')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vr =>mem_realloc_r2(bdy_vr,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_vr')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vz =>mem_realloc_r2(bdy_vz,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_vz')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vphi =>mem_realloc_r2(bdy_vphi,(1),(11),(1),(((int(((((dim_str
     &atum)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-
     &(1)+1))+100-1)/100)*100)+(1)-1),'bdy_vphi')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_te =>mem_realloc_r1(bdy_te,(1),(((int(((((dim_stratum)-(1)+1))
     &+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+1))+100-1
     &)/100)*100)+(1)-1),'bdy_te')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_ti =>mem_realloc_r1(bdy_ti,(1),(((int(((((dim_stratum)-(1)+1))
     &+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+1))+100-1
     &)/100)*100)+(1)-1),'bdy_ti')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      stratum_list =>mem_realloc_i1(stratum_list,(1),(((int(((((dim_stra
     &tum)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(
     &1)+1))+100-1)/100)*100)+(1)-1),'stratum_list')
      end if
      if(ns.GT.0)continue
      do is=1,ns
      bdy_ni(is,nstratum)=(0.0_DOUBLE)
      bdy_vr(is,nstratum)=(0.0_DOUBLE)
      bdy_vz(is,nstratum)=(0.0_DOUBLE)
      bdy_vphi(is,nstratum)=(0.0_DOUBLE)
      end do
      bdy_te(nstratum)=(0.0_DOUBLE)
      bdy_ti(nstratum)=(0.0_DOUBLE)
      stratum_index=nstratum
      stratum_list(stratum_index)=stratum
      end if
      
      if((stratum_index.GT.0).AND.(stratum_index.LE.nstratum))continue
      if(next_token(line,b,e,p))continue
      bdy_vz(species_index,stratum_index)=read_real(line(b:e))
      else if(keyword.EQ.'bdy_vphi')then
      if(next_token(line,b,e,p))continue
      if(ns.GT.0)continue
      species_index=0
      do is=1,ns
      if(uedge_species(is).EQ.line(b:e))then
      if(species_index.EQ.0)continue
      species_index=is
      end if
      end do
      
      if(species_index.GT.0)continue
      if(next_token(line,b,e,p))continue
      stratum=read_integer(line(b:e))
      
      match=0
      if(nstratum.GT.0)then
      do is=1,nstratum
      if(stratum_list(is).EQ.stratum)then
      match=1
      stratum_index=is
      end if
      end do
      end if
      if(match.EQ.0)then
      nstratum=nstratum+1
      dim_stratum=max(dim_stratum,nstratum)
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_ni =>mem_realloc_r2(bdy_ni,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_ni')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vr =>mem_realloc_r2(bdy_vr,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_vr')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vz =>mem_realloc_r2(bdy_vz,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_vz')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vphi =>mem_realloc_r2(bdy_vphi,(1),(11),(1),(((int(((((dim_str
     &atum)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-
     &(1)+1))+100-1)/100)*100)+(1)-1),'bdy_vphi')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_te =>mem_realloc_r1(bdy_te,(1),(((int(((((dim_stratum)-(1)+1))
     &+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+1))+100-1
     &)/100)*100)+(1)-1),'bdy_te')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_ti =>mem_realloc_r1(bdy_ti,(1),(((int(((((dim_stratum)-(1)+1))
     &+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+1))+100-1
     &)/100)*100)+(1)-1),'bdy_ti')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      stratum_list =>mem_realloc_i1(stratum_list,(1),(((int(((((dim_stra
     &tum)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(
     &1)+1))+100-1)/100)*100)+(1)-1),'stratum_list')
      end if
      if(ns.GT.0)continue
      do is=1,ns
      bdy_ni(is,nstratum)=(0.0_DOUBLE)
      bdy_vr(is,nstratum)=(0.0_DOUBLE)
      bdy_vz(is,nstratum)=(0.0_DOUBLE)
      bdy_vphi(is,nstratum)=(0.0_DOUBLE)
      end do
      bdy_te(nstratum)=(0.0_DOUBLE)
      bdy_ti(nstratum)=(0.0_DOUBLE)
      stratum_index=nstratum
      stratum_list(stratum_index)=stratum
      end if
      
      if((stratum_index.GT.0).AND.(stratum_index.LE.nstratum))continue
      if(next_token(line,b,e,p))continue
      bdy_vphi(species_index,stratum_index)=read_real(line(b:e))
      else if(keyword.EQ.'bdy_ti')then
      if(next_token(line,b,e,p))continue
      stratum=read_integer(line(b:e))
      
      match=0
      if(nstratum.GT.0)then
      do is=1,nstratum
      if(stratum_list(is).EQ.stratum)then
      match=1
      stratum_index=is
      end if
      end do
      end if
      if(match.EQ.0)then
      nstratum=nstratum+1
      dim_stratum=max(dim_stratum,nstratum)
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_ni =>mem_realloc_r2(bdy_ni,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_ni')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vr =>mem_realloc_r2(bdy_vr,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_vr')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vz =>mem_realloc_r2(bdy_vz,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_vz')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vphi =>mem_realloc_r2(bdy_vphi,(1),(11),(1),(((int(((((dim_str
     &atum)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-
     &(1)+1))+100-1)/100)*100)+(1)-1),'bdy_vphi')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_te =>mem_realloc_r1(bdy_te,(1),(((int(((((dim_stratum)-(1)+1))
     &+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+1))+100-1
     &)/100)*100)+(1)-1),'bdy_te')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_ti =>mem_realloc_r1(bdy_ti,(1),(((int(((((dim_stratum)-(1)+1))
     &+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+1))+100-1
     &)/100)*100)+(1)-1),'bdy_ti')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      stratum_list =>mem_realloc_i1(stratum_list,(1),(((int(((((dim_stra
     &tum)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(
     &1)+1))+100-1)/100)*100)+(1)-1),'stratum_list')
      end if
      if(ns.GT.0)continue
      do is=1,ns
      bdy_ni(is,nstratum)=(0.0_DOUBLE)
      bdy_vr(is,nstratum)=(0.0_DOUBLE)
      bdy_vz(is,nstratum)=(0.0_DOUBLE)
      bdy_vphi(is,nstratum)=(0.0_DOUBLE)
      end do
      bdy_te(nstratum)=(0.0_DOUBLE)
      bdy_ti(nstratum)=(0.0_DOUBLE)
      stratum_index=nstratum
      stratum_list(stratum_index)=stratum
      end if
      
      if((stratum_index.GT.0).AND.(stratum_index.LE.nstratum))continue
      if(next_token(line,b,e,p))continue
      bdy_ti(stratum_index)=read_real(line(b:e))*(1.60217733e-19_DOUBLE)
      if(bdy_ti(stratum_index).GE.(0.0_DOUBLE))continue
      else if(keyword.EQ.'bdy_te')then
      if(next_token(line,b,e,p))continue
      stratum=read_integer(line(b:e))
      
      match=0
      if(nstratum.GT.0)then
      do is=1,nstratum
      if(stratum_list(is).EQ.stratum)then
      match=1
      stratum_index=is
      end if
      end do
      end if
      if(match.EQ.0)then
      nstratum=nstratum+1
      dim_stratum=max(dim_stratum,nstratum)
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_ni =>mem_realloc_r2(bdy_ni,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_ni')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vr =>mem_realloc_r2(bdy_vr,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_vr')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vz =>mem_realloc_r2(bdy_vz,(1),(11),(1),(((int(((((dim_stratum
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+
     &1))+100-1)/100)*100)+(1)-1),'bdy_vz')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_vphi =>mem_realloc_r2(bdy_vphi,(1),(11),(1),(((int(((((dim_str
     &atum)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-
     &(1)+1))+100-1)/100)*100)+(1)-1),'bdy_vphi')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_te =>mem_realloc_r1(bdy_te,(1),(((int(((((dim_stratum)-(1)+1))
     &+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+1))+100-1
     &)/100)*100)+(1)-1),'bdy_te')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      bdy_ti =>mem_realloc_r1(bdy_ti,(1),(((int(((((dim_stratum)-(1)+1))
     &+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(1)+1))+100-1
     &)/100)*100)+(1)-1),'bdy_ti')
      end if
      if(mod(((dim_stratum)-(1)+1),100).EQ.1)then
      stratum_list =>mem_realloc_i1(stratum_list,(1),(((int(((((dim_stra
     &tum)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((dim_stratum)-(
     &1)+1))+100-1)/100)*100)+(1)-1),'stratum_list')
      end if
      if(ns.GT.0)continue
      do is=1,ns
      bdy_ni(is,nstratum)=(0.0_DOUBLE)
      bdy_vr(is,nstratum)=(0.0_DOUBLE)
      bdy_vz(is,nstratum)=(0.0_DOUBLE)
      bdy_vphi(is,nstratum)=(0.0_DOUBLE)
      end do
      bdy_te(nstratum)=(0.0_DOUBLE)
      bdy_ti(nstratum)=(0.0_DOUBLE)
      stratum_index=nstratum
      stratum_list(stratum_index)=stratum
      end if
      
      if((stratum_index.GT.0).AND.(stratum_index.LE.nstratum))continue
      if(next_token(line,b,e,p))continue
      bdy_te(stratum_index)=read_real(line(b:e))*(1.60217733e-19_DOUBLE)
      if(bdy_te(stratum_index).GE.(0.0_DOUBLE))continue
      else
      if('Unrecognized keyword'.EQ.' ')continue
      end if
      go to 90001
      end if
      close(unit=diskin2)
      end if
      open(unit=diskin2,file=uedge_file,status='old',form='formatted',io
     &stat=open_stat)
      if(open_stat.NE.0)then
      write(0,*)' Cannot open file ',uedge_file,', error number ',open_s
     &tat
      return
      end if
      nu=diskin2
      read(nu,*)
      read(nu,*)nx,ny,nxpt
      if((nxpt.GE.1).AND.(nxpt.LE.2))continue
      do i=1,nxpt
      read(nu,*)iysptrx1(i),iysptrx2(i)
      read(nu,*)ixlb(i),ixpt1(i),ixmdp(i),ixpt2(i),ixrb(i)
      end do
      if(nx.LE.150.AND.ny.LE.150)continue
      read(nu,*) (((rm(j1,j2,j3),j1=1,nx),j2=1,ny),j3=0,4)
      read(nu,*) (((zm(j1,j2,j3),j1=1,nx),j2=1,ny),j3=0,4)
      do i=1,3
      read(nu,*) (((dummy,j1=1,nx),j2=1,ny),j3=0,4)
      enddo
      do js=1,ns
      read(nu,*,err=6) ((ni(j1,j2,js),j1=1,nx),j2=1,ny)
      end do
      do js=1,ns
      read(nu,*,err=6) ((vr(j1,j2,js),j1=1,nx),j2=1,ny)
      end do
      do js=1,ns
      read(nu,*,err=6) ((vz(j1,j2,js),j1=1,nx),j2=1,ny)
      end do
      do js=1,ns
      read(nu,*,err=6) ((vphi(j1,j2,js),j1=1,nx),j2=1,ny)
      end do
      read(nu,*,err=6) ((ti(j1,j2),j1=1,nx),j2=1,ny)
      read(nu,*,err=6) ((te(j1,j2),j1=1,nx),j2=1,ny)
      do ixpt=1,nxpt
      do js=1,ns
      read(nu,*,err=6) (ni1(j2,js,ixpt),j2=1,ny)
      end do
      do js=1,ns
      read(nu,*,err=6) (vr1(j2,js,ixpt),j2=1,ny)
      end do
      do js=1,ns
      read(nu,*,err=6) (vz1(j2,js,ixpt),j2=1,ny)
      end do
      do js=1,ns
      read(nu,*,err=6) (vphi1(j2,js,ixpt),j2=1,ny)
      end do
      read(nu,*,err=6) (ti1(j2,ixpt),j2=1,ny)
      read(nu,*,err=6) (te1(j2,ixpt),j2=1,ny)
      do js=1,ns
      read(nu,*,err=6) (flux1(j2,js,ixpt),j2=1,ny)
      end do
      do js=1,ns
      read(nu,*,err=6) (ni2(j2,js,ixpt),j2=1,ny)
      end do
      do js=1,ns
      read(nu,*,err=6) (vr2(j2,js,ixpt),j2=1,ny)
      end do
      do js=1,ns
      read(nu,*,err=6) (vz2(j2,js,ixpt),j2=1,ny)
      end do
      do js=1,ns
      read(nu,*,err=6) (vphi2(j2,js,ixpt),j2=1,ny)
      end do
      read(nu,*,err=6) (ti2(j2,ixpt),j2=1,ny)
      read(nu,*,err=6) (te2(j2,ixpt),j2=1,ny)
      do js=1,ns
      read(nu,*,err=6) (flux2(j2,js,ixpt),j2=1,ny)
      end do
      end do
      close(unit=diskin2)
      do zone=1,zn_num
      zone_stratum(zone)=2000000000
      end do
      if(polygon_file.NE.'UNINITIALIZED')then
      fileid=ncopn(polygon_file,0,nc_stat)
      g2_points_ind0_id=ncdid(fileid,'g2_points_ind0',nc_stat)
      call ncdinq(fileid,g2_points_ind0_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((2000-1)-(0)+1))continue
      g2_points_ind_id=ncdid(fileid,'g2_points_ind',nc_stat)
      call ncdinq(fileid,g2_points_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((2000)-(1)+1))continue
      g2_points_tot_ind0_id=ncdid(fileid,'g2_points_tot_ind0',nc_stat)
      call ncdinq(fileid,g2_points_tot_ind0_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((200000-1)-(0)+1))continue
      g2_points_tot_ind_id=ncdid(fileid,'g2_points_tot_ind',nc_stat)
      call ncdinq(fileid,g2_points_tot_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((200000)-(1)+1))continue
      g2_xz_ind_id=ncdid(fileid,'g2_xz_ind',nc_stat)
      call ncdinq(fileid,g2_xz_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((2)-(1)+1))continue
      g2_num_polygons_id=ncvid(fileid,'g2_num_polygons',nc_stat)
      call ncvinq(fileid,g2_num_polygons_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,g2_num_polygons_id,nc_corner,nc_edge,g2_num_poly
     &gons,nc_stat)
      g2_poly_ind_id=ncdid(fileid,'g2_poly_ind',nc_stat)
      call ncdinq(fileid,g2_poly_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((g2_num_polygons)-(1)+1))continue
      g2_polygon_xz_id=ncvid(fileid,'g2_polygon_xz',nc_stat)
      call ncvinq(fileid,g2_polygon_xz_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
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
      call ncvgt(fileid,g2_polygon_xz_id,nc_corner,nc_edge,g2_polygon_xz
     &,nc_stat)
      g2_polygon_segment_id=ncvid(fileid,'g2_polygon_segment',nc_stat)
      call ncvinq(fileid,g2_polygon_segment_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.g2_points_ind0_id)continue
      if(nc_dims(2).EQ.g2_poly_ind_id)continue
      
      g2_polygon_segment =>mem_alloc_i2((0),(2000-1),(1),(g2_num_polygon
     &s),'g2_polygon_segment')
      nc_corner(1)=1
      nc_edge(1)=((2000-1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((g2_num_polygons)-(1)+1)
      call ncvgt(fileid,g2_polygon_segment_id,nc_corner,nc_edge,g2_polyg
     &on_segment,nc_stat)
      g2_polygon_points_id=ncvid(fileid,'g2_polygon_points',nc_stat)
      call ncvinq(fileid,g2_polygon_points_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.g2_poly_ind_id)continue
      
      g2_polygon_points =>mem_alloc_i1((1),(g2_num_polygons),'g2_polygon
     &_points')
      nc_corner(1)=1
      nc_edge(1)=((g2_num_polygons)-(1)+1)
      call ncvgt(fileid,g2_polygon_points_id,nc_corner,nc_edge,g2_polygo
     &n_points,nc_stat)
      g2_polygon_zone_id=ncvid(fileid,'g2_polygon_zone',nc_stat)
      call ncvinq(fileid,g2_polygon_zone_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.g2_poly_ind_id)continue
      
      g2_polygon_zone =>mem_alloc_i1((1),(g2_num_polygons),'g2_polygon_z
     &one')
      nc_corner(1)=1
      nc_edge(1)=((g2_num_polygons)-(1)+1)
      call ncvgt(fileid,g2_polygon_zone_id,nc_corner,nc_edge,g2_polygon_
     &zone,nc_stat)
      g2_polygon_stratum_id=ncvid(fileid,'g2_polygon_stratum',nc_stat)
      call ncvinq(fileid,g2_polygon_stratum_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.g2_poly_ind_id)continue
      
      g2_polygon_stratum =>mem_alloc_i1((1),(g2_num_polygons),'g2_polygo
     &n_stratum')
      nc_corner(1)=1
      nc_edge(1)=((g2_num_polygons)-(1)+1)
      call ncvgt(fileid,g2_polygon_stratum_id,nc_corner,nc_edge,g2_polyg
     &on_stratum,nc_stat)
      
      do ipoly=1,g2_num_polygons
      zone=g2_polygon_zone(ipoly)
      if(zone_stratum(zone).EQ.2000000000)then
      if(g2_polygon_stratum(ipoly).GE.0)continue
      zone_stratum(zone)=g2_polygon_stratum(ipoly)
      else
      if((zone_stratum(zone).EQ.g2_polygon_stratum(ipoly)).OR.(zone_type
     &(zone).NE.2))continue
      end if
      end do
      else
      if(nstratum.GT.0)write(0,*)'Need to specify polygon.nc file to set
     & boundary parameters!'
      end if
      if(nstratum.GT.0)then
      dim_stratum=nstratum
      if(mod(((dim_stratum)-(1)+1),100).NE.0)then
      bdy_ni =>mem_realloc_r2(bdy_ni,(1),(11),(1),((int(((((dim_stratum)
     &-(1)+1))+100-1)/100)*100)+(1)-1),(dim_stratum),'bdy_ni')
      end if
      if(mod(((dim_stratum)-(1)+1),100).NE.0)then
      bdy_vr =>mem_realloc_r2(bdy_vr,(1),(11),(1),((int(((((dim_stratum)
     &-(1)+1))+100-1)/100)*100)+(1)-1),(dim_stratum),'bdy_vr')
      end if
      if(mod(((dim_stratum)-(1)+1),100).NE.0)then
      bdy_vz =>mem_realloc_r2(bdy_vz,(1),(11),(1),((int(((((dim_stratum)
     &-(1)+1))+100-1)/100)*100)+(1)-1),(dim_stratum),'bdy_vz')
      end if
      if(mod(((dim_stratum)-(1)+1),100).NE.0)then
      bdy_vphi =>mem_realloc_r2(bdy_vphi,(1),(11),(1),((int(((((dim_stra
     &tum)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_stratum),'bdy_vphi')
      end if
      if(mod(((dim_stratum)-(1)+1),100).NE.0)then
      bdy_ti =>mem_realloc_r1(bdy_ti,(1),((int(((((dim_stratum)-(1)+1))+
     &100-1)/100)*100)+(1)-1),(dim_stratum),'bdy_ti')
      end if
      if(mod(((dim_stratum)-(1)+1),100).NE.0)then
      bdy_te =>mem_realloc_r1(bdy_te,(1),((int(((((dim_stratum)-(1)+1))+
     &100-1)/100)*100)+(1)-1),(dim_stratum),'bdy_te')
      end if
      if(mod(((dim_stratum)-(1)+1),100).NE.0)then
      stratum_list =>mem_realloc_i1(stratum_list,(1),((int(((((dim_strat
     &um)-(1)+1))+100-1)/100)*100)+(1)-1),(dim_stratum),'stratum_list')
      end if
      end if
      first_zone=1
      do zone=1,zn_num
      if(check_zone(zone))continue
      if(zone_type(zone).EQ.2)then
      if((zone_index(1,zone).GT.0).AND.(zone_index(2,zone).GT.0))then
      j1=zone_index(1,zone)
      if(j1.LE.nx)continue
      j2=zone_index(2,zone)
      if(j2.LE.ny)continue
      do js=1,ns
      zone_ni(js)=ni(j1,j2,js)
      zone_vr(js)=vr(j1,j2,js)
      zone_vz(js)=vz(j1,j2,js)
      zone_vphi(js)=vphi(j1,j2,js)
      end do
      zone_ti=ti(j1,j2)
      zone_te=te(j1,j2)
      else
      if(nstratum.GT.0)then
      is=int_lookup(zone_stratum(zone),stratum_list,nstratum)
      else
      is=0
      end if
      if(is.GT.0)then
      if(is.LE.nstratum)continue
      do js=1,ns
      zone_ni(js)=bdy_ni(js,is)
      zone_vr(js)=bdy_vr(js,is)
      zone_vz(js)=bdy_vz(js,is)
      zone_vphi(js)=bdy_vphi(js,is)
      end do
      zone_ti=bdy_ti(is)
      zone_te=bdy_te(is)
      else
      do js=1,ns
      zone_ni(js)=(0.0_DOUBLE)
      zone_vr(js)=(0.0_DOUBLE)
      zone_vz(js)=(0.0_DOUBLE)
      zone_vphi(js)=(0.0_DOUBLE)
      end do
      zone_ti=(0.0_DOUBLE)
      zone_te=(0.0_DOUBLE)
      end if
      end if
      zone_ne=(0.0_DOUBLE)
      do js=1,ns
      isp=string_lookup(uedge_species(js),species_sy,sp_num)
      if((isp.GT.0.AND.isp.LE.sp_num))then
      zone_ne=zone_ne+(zone_ni(js)*species_z(isp))
      else
      if(first_zone.EQ.1)write(0,*)'UEDGE species ',uedge_species(js),' 
     &not in reference species list'
      end if
      sp_back=problem_species_background(isp)
      if((sp_back.GT.0.AND.sp_back.LE.bk_num))then
      background_n(sp_back,zone_pointer(zone))=zone_ni(js)
      background_v(1,sp_back,zone_pointer(zone))=zone_vr(js)
      background_v(2,sp_back,zone_pointer(zone))=zone_vphi(js)
      background_v(3,sp_back,zone_pointer(zone))=zone_vz(js)
      background_temp(sp_back,zone_pointer(zone))=zone_ti
      else
      if(first_zone.EQ.1)write(0,*)'UEDGE species ',uedge_species(js),' 
     &not in problem species list'
      end if
      end do
      isp=string_lookup('e',species_sy,sp_num)
      if((isp.GT.0.AND.isp.LE.sp_num))then
      sp_back=problem_species_background(isp)
      if((sp_back.GT.0.AND.sp_back.LE.bk_num))then
      background_n(sp_back,zone_pointer(zone))=zone_ne
      background_temp(sp_back,zone_pointer(zone))=zone_te
      else
      if(first_zone.EQ.1)write(0,*)'e not listed as a background species
     &!'
      end if
      else
      if(first_zone.EQ.1)write(0,*)'e not listed as a species!'
      end if
      first_zone=0
      end if
      end do
      do ixpt=1,nxpt
      do js=1,ns
      flux1_tot(js,ixpt)=(0.0_DOUBLE)
      flux2_tot(js,ixpt)=(0.0_DOUBLE)
      do j2=1,ny
      flux1_tot(js,ixpt)=flux1_tot(js,ixpt)+(flux1(j2,js,ixpt))
      flux2_tot(js,ixpt)=flux2_tot(js,ixpt)+(flux2(j2,js,ixpt))
      end do
      end do
      end do
      go to 90004
6     if('error occurred in reading uedge data file'.EQ.' ')continue
90004 continue
      call mem_free_r2(bdy_ni,(1),(11),(1),(dim_stratum),'bdy_ni')
      call mem_free_r2(bdy_vr,(1),(11),(1),(dim_stratum),'bdy_vr')
      call mem_free_r2(bdy_vz,(1),(11),(1),(dim_stratum),'bdy_vz')
      call mem_free_r2(bdy_vphi,(1),(11),(1),(dim_stratum),'bdy_vphi')
      call mem_free_r1(bdy_ti,(1),(dim_stratum),'bdy_ti')
      call mem_free_r1(bdy_te,(1),(dim_stratum),'bdy_te')
      call mem_free_i1(stratum_list,(1),(dim_stratum),'stratum_list')
      call mem_free_i1(zone_stratum,(1),(zn_num),'zone_stratum')
      return
      end
      subroutine write_background
      
      use zn_mod
      
      use bk_mod
      
      use so_mod
      
      use rf_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer fileid
      character*96 tempfile
      integer bk_num_id
      integer background_ind_id
      integer bk_plasma_ind_id
      integer background_coords_id
      integer background_n_id
      integer background_v_id
      integer background_temp_id
      
      integer vector_id
      integer string_id
      
      integer nc_dims(5),nc_corner(5),nc_edge(5),nc_stat,nc_size,nc_rank
     &,nc_attr,nc_type
      character*128 nc_dummy
      external ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf
     &_inq_format
      integer ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf_
     &inq_format
      integer so_grps_id
      integer so_seg_tot_id
      integer so_restart_id
      integer so_spaced_seeds_id
      integer so_seed_spacing_id
      integer so_sampling_id
      integer so_time_dependent_id
      integer so_time_initialization_id
      integer so_time_initial_id
      integer so_time_final_id
      integer so_rel_wt_min_id
      integer so_rel_wt_max_id
      integer so_wt_norm_min_id
      integer so_wt_norm_max_id
      integer so_gparams_list_size_id
      integer so_gparams_list_dim_id
      integer so_params_list_size_id
      integer so_params_list_dim_id
      integer so_params_data_size_id
      integer so_params_data_dim_id
      integer so_giparams_list_size_id
      integer so_giparams_list_dim_id
      integer so_iparams_list_size_id
      integer so_iparams_list_dim_id
      integer so_iparams_data_size_id
      integer so_iparams_data_dim_id
      integer source_grp_ind_id
      integer source_seg_ind_id
      integer so_seed_decimal_ind_id
      integer so_name_ind_id
      integer so_type_ind_id
      integer source_gparams_ind_id
      integer source_params_ind_id
      integer source_params_data_ind_id
      integer source_giparams_ind_id
      integer source_iparams_ind_id
      integer source_iparams_data_ind_id
      integer so_seed_decimal_id
      integer source_name_id
      integer source_base_ptr_id
      integer source_num_segments_id
      integer source_type_id
      integer source_geometry_id
      integer source_num_flights_id
      integer source_num_checkpoints_id
      integer source_species_id
      integer source_root_species_id
      integer source_time_variation_id
      integer source_num_gparameters_id
      integer source_num_parameters_id
      integer source_gparameters_list_id
      integer source_parameters_list_id
      integer source_gparameters_base_id
      integer source_parameters_base_id
      integer source_parameters_data_base_id
      integer source_gparameters_data_id
      integer source_parameters_data_id
      integer source_num_giparameters_id
      integer source_num_iparameters_id
      integer source_giparameters_list_id
      integer source_iparameters_list_id
      integer source_giparameters_base_id
      integer source_iparameters_base_id
      integer source_iparameters_data_base_id
      integer source_giparameters_data_id
      integer source_iparameters_data_id
      integer source_total_current_id
      integer source_weight_norm_id
      integer source_scale_factor_id
      integer source_segment_ptr_id
      integer source_current_id
      integer source_segment_rel_wt_id
      integer source_segment_prob_alias_id
      integer source_segment_ptr_alias_id
      
      tempfile=filenames_array(2)
      if(tempfile.NE.'undefined')continue
      fileid=nccre(tempfile,0,nc_stat)
      vector_id=ncddef(fileid,'vector',((3)-(1)+1),nc_stat)
      string_id=ncddef(fileid,'string',((300)-(1)+1),nc_stat)
      
      bk_num_id=ncvdef(fileid,'bk_num',4,0,nc_dims,nc_stat)
      background_ind_id=ncddef(fileid,'background_ind',((bk_num)-(1)+1),
     &nc_stat)
      bk_plasma_ind_id=ncddef(fileid,'bk_plasma_ind',((zone_type_num(2))
     &-(1)+1),nc_stat)
      background_coords_id=ncvdef(fileid,'background_coords',4,0,nc_dims
     &,nc_stat)
      nc_dims(1)=background_ind_id
      nc_dims(2)=bk_plasma_ind_id
      background_n_id=ncvdef(fileid,'background_n',6,2,nc_dims,nc_stat)
      nc_dims(1)=vector_id
      nc_dims(2)=background_ind_id
      nc_dims(3)=bk_plasma_ind_id
      background_v_id=ncvdef(fileid,'background_v',6,3,nc_dims,nc_stat)
      nc_dims(1)=background_ind_id
      nc_dims(2)=bk_plasma_ind_id
      background_temp_id=ncvdef(fileid,'background_temp',6,2,nc_dims,nc_
     &stat)
      
      so_grps_id=ncvdef(fileid,'so_grps',4,0,nc_dims,nc_stat)
      so_seg_tot_id=ncvdef(fileid,'so_seg_tot',4,0,nc_dims,nc_stat)
      so_restart_id=ncvdef(fileid,'so_restart',4,0,nc_dims,nc_stat)
      so_spaced_seeds_id=ncvdef(fileid,'so_spaced_seeds',4,0,nc_dims,nc_
     &stat)
      so_seed_spacing_id=ncvdef(fileid,'so_seed_spacing',4,0,nc_dims,nc_
     &stat)
      so_sampling_id=ncvdef(fileid,'so_sampling',4,0,nc_dims,nc_stat)
      so_time_dependent_id=ncvdef(fileid,'so_time_dependent',4,0,nc_dims
     &,nc_stat)
      so_time_initialization_id=ncvdef(fileid,'so_time_initialization',4
     &,0,nc_dims,nc_stat)
      so_time_initial_id=ncvdef(fileid,'so_time_initial',6,0,nc_dims,nc_
     &stat)
      so_time_final_id=ncvdef(fileid,'so_time_final',6,0,nc_dims,nc_stat
     &)
      so_rel_wt_min_id=ncvdef(fileid,'so_rel_wt_min',6,0,nc_dims,nc_stat
     &)
      so_rel_wt_max_id=ncvdef(fileid,'so_rel_wt_max',6,0,nc_dims,nc_stat
     &)
      so_wt_norm_min_id=ncvdef(fileid,'so_wt_norm_min',6,0,nc_dims,nc_st
     &at)
      so_wt_norm_max_id=ncvdef(fileid,'so_wt_norm_max',6,0,nc_dims,nc_st
     &at)
      so_gparams_list_size_id=ncvdef(fileid,'so_gparams_list_size',4,0,n
     &c_dims,nc_stat)
      so_gparams_list_dim_id=ncvdef(fileid,'so_gparams_list_dim',4,0,nc_
     &dims,nc_stat)
      so_params_list_size_id=ncvdef(fileid,'so_params_list_size',4,0,nc_
     &dims,nc_stat)
      so_params_list_dim_id=ncvdef(fileid,'so_params_list_dim',4,0,nc_di
     &ms,nc_stat)
      so_params_data_size_id=ncvdef(fileid,'so_params_data_size',4,0,nc_
     &dims,nc_stat)
      so_params_data_dim_id=ncvdef(fileid,'so_params_data_dim',4,0,nc_di
     &ms,nc_stat)
      so_giparams_list_size_id=ncvdef(fileid,'so_giparams_list_size',4,0
     &,nc_dims,nc_stat)
      so_giparams_list_dim_id=ncvdef(fileid,'so_giparams_list_dim',4,0,n
     &c_dims,nc_stat)
      so_iparams_list_size_id=ncvdef(fileid,'so_iparams_list_size',4,0,n
     &c_dims,nc_stat)
      so_iparams_list_dim_id=ncvdef(fileid,'so_iparams_list_dim',4,0,nc_
     &dims,nc_stat)
      so_iparams_data_size_id=ncvdef(fileid,'so_iparams_data_size',4,0,n
     &c_dims,nc_stat)
      so_iparams_data_dim_id=ncvdef(fileid,'so_iparams_data_dim',4,0,nc_
     &dims,nc_stat)
      source_grp_ind_id=ncddef(fileid,'source_grp_ind',((so_grps)-(1)+1)
     &,nc_stat)
      source_seg_ind_id=ncddef(fileid,'source_seg_ind',((so_seg_tot)-(1)
     &+1),nc_stat)
      so_seed_decimal_ind_id=ncddef(fileid,'so_seed_decimal_ind',((34)-(
     &1)+1),nc_stat)
      so_name_ind_id=ncddef(fileid,'so_name_ind',((10)-(1)+1),nc_stat)
      so_type_ind_id=ncddef(fileid,'so_type_ind',((6)-(1)+1),nc_stat)
      source_gparams_ind_id=ncddef(fileid,'source_gparams_ind',((so_gpar
     &ams_list_dim)-(1)+1),nc_stat)
      source_params_ind_id=ncddef(fileid,'source_params_ind',((so_params
     &_list_dim)-(1)+1),nc_stat)
      source_params_data_ind_id=ncddef(fileid,'source_params_data_ind',(
     &(so_params_data_dim)-(1)+1),nc_stat)
      source_giparams_ind_id=ncddef(fileid,'source_giparams_ind',((so_gi
     &params_list_dim)-(1)+1),nc_stat)
      source_iparams_ind_id=ncddef(fileid,'source_iparams_ind',((so_ipar
     &ams_list_dim)-(1)+1),nc_stat)
      source_iparams_data_ind_id=ncddef(fileid,'source_iparams_data_ind'
     &,((so_iparams_data_dim)-(1)+1),nc_stat)
      nc_dims(1)=so_seed_decimal_ind_id
      so_seed_decimal_id=ncvdef(fileid,'so_seed_decimal',2,1,nc_dims,nc_
     &stat)
      nc_dims(1)=so_name_ind_id
      nc_dims(2)=so_type_ind_id
      source_name_id=ncvdef(fileid,'source_name',2,2,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_base_ptr_id=ncvdef(fileid,'source_base_ptr',4,1,nc_dims,nc_
     &stat)
      nc_dims(1)=source_grp_ind_id
      source_num_segments_id=ncvdef(fileid,'source_num_segments',4,1,nc_
     &dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_type_id=ncvdef(fileid,'source_type',4,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_geometry_id=ncvdef(fileid,'source_geometry',4,1,nc_dims,nc_
     &stat)
      nc_dims(1)=source_grp_ind_id
      source_num_flights_id=ncvdef(fileid,'source_num_flights',4,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_num_checkpoints_id=ncvdef(fileid,'source_num_checkpoints',4
     &,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_species_id=ncvdef(fileid,'source_species',4,1,nc_dims,nc_st
     &at)
      nc_dims(1)=source_grp_ind_id
      source_root_species_id=ncvdef(fileid,'source_root_species',4,1,nc_
     &dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_time_variation_id=ncvdef(fileid,'source_time_variation',4,1
     &,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_num_gparameters_id=ncvdef(fileid,'source_num_gparameters',4
     &,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_num_parameters_id=ncvdef(fileid,'source_num_parameters',4,1
     &,nc_dims,nc_stat)
      nc_dims(1)=source_gparams_ind_id
      source_gparameters_list_id=ncvdef(fileid,'source_gparameters_list'
     &,4,1,nc_dims,nc_stat)
      nc_dims(1)=source_params_ind_id
      source_parameters_list_id=ncvdef(fileid,'source_parameters_list',4
     &,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_gparameters_base_id=ncvdef(fileid,'source_gparameters_base'
     &,4,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_parameters_base_id=ncvdef(fileid,'source_parameters_base',4
     &,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_parameters_data_base_id=ncvdef(fileid,'source_parameters_da
     &ta_base',4,1,nc_dims,nc_stat)
      nc_dims(1)=source_gparams_ind_id
      source_gparameters_data_id=ncvdef(fileid,'source_gparameters_data'
     &,6,1,nc_dims,nc_stat)
      nc_dims(1)=source_params_data_ind_id
      source_parameters_data_id=ncvdef(fileid,'source_parameters_data',6
     &,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_num_giparameters_id=ncvdef(fileid,'source_num_giparameters'
     &,4,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_num_iparameters_id=ncvdef(fileid,'source_num_iparameters',4
     &,1,nc_dims,nc_stat)
      nc_dims(1)=source_giparams_ind_id
      source_giparameters_list_id=ncvdef(fileid,'source_giparameters_lis
     &t',4,1,nc_dims,nc_stat)
      nc_dims(1)=source_iparams_ind_id
      source_iparameters_list_id=ncvdef(fileid,'source_iparameters_list'
     &,4,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_giparameters_base_id=ncvdef(fileid,'source_giparameters_bas
     &e',4,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_iparameters_base_id=ncvdef(fileid,'source_iparameters_base'
     &,4,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_iparameters_data_base_id=ncvdef(fileid,'source_iparameters_
     &data_base',4,1,nc_dims,nc_stat)
      nc_dims(1)=source_giparams_ind_id
      source_giparameters_data_id=ncvdef(fileid,'source_giparameters_dat
     &a',4,1,nc_dims,nc_stat)
      nc_dims(1)=source_iparams_data_ind_id
      source_iparameters_data_id=ncvdef(fileid,'source_iparameters_data'
     &,4,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_total_current_id=ncvdef(fileid,'source_total_current',6,1,n
     &c_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_weight_norm_id=ncvdef(fileid,'source_weight_norm',6,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_scale_factor_id=ncvdef(fileid,'source_scale_factor',6,1,nc_
     &dims,nc_stat)
      nc_dims(1)=source_seg_ind_id
      source_segment_ptr_id=ncvdef(fileid,'source_segment_ptr',4,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=source_seg_ind_id
      source_current_id=ncvdef(fileid,'source_current',6,1,nc_dims,nc_st
     &at)
      nc_dims(1)=source_seg_ind_id
      source_segment_rel_wt_id=ncvdef(fileid,'source_segment_rel_wt',6,1
     &,nc_dims,nc_stat)
      nc_dims(1)=source_seg_ind_id
      source_segment_prob_alias_id=ncvdef(fileid,'source_segment_prob_al
     &ias',6,1,nc_dims,nc_stat)
      nc_dims(1)=source_seg_ind_id
      source_segment_ptr_alias_id=ncvdef(fileid,'source_segment_ptr_alia
     &s',4,1,nc_dims,nc_stat)
      
      call ncendf(fileid,nc_stat)
      
      
      call ncvpt(fileid,bk_num_id,nc_corner,nc_edge,bk_num,nc_stat)
      call ncvpt(fileid,background_coords_id,nc_corner,nc_edge,backgroun
     &d_coords,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((bk_num)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((zone_type_num(2))-(1)+1)
      call ncvpt(fileid,background_n_id,nc_corner,nc_edge,background_n,n
     &c_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((bk_num)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((zone_type_num(2))-(1)+1)
      call ncvpt(fileid,background_v_id,nc_corner,nc_edge,background_v,n
     &c_stat)
      nc_corner(1)=1
      nc_edge(1)=((bk_num)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((zone_type_num(2))-(1)+1)
      call ncvpt(fileid,background_temp_id,nc_corner,nc_edge,background_
     &temp,nc_stat)
      
      call ncvpt(fileid,so_grps_id,nc_corner,nc_edge,so_grps,nc_stat)
      call ncvpt(fileid,so_seg_tot_id,nc_corner,nc_edge,so_seg_tot,nc_st
     &at)
      call ncvpt(fileid,so_restart_id,nc_corner,nc_edge,so_restart,nc_st
     &at)
      call ncvpt(fileid,so_spaced_seeds_id,nc_corner,nc_edge,so_spaced_s
     &eeds,nc_stat)
      call ncvpt(fileid,so_seed_spacing_id,nc_corner,nc_edge,so_seed_spa
     &cing,nc_stat)
      call ncvpt(fileid,so_sampling_id,nc_corner,nc_edge,so_sampling,nc_
     &stat)
      call ncvpt(fileid,so_time_dependent_id,nc_corner,nc_edge,so_time_d
     &ependent,nc_stat)
      call ncvpt(fileid,so_time_initialization_id,nc_corner,nc_edge,so_t
     &ime_initialization,nc_stat)
      call ncvpt(fileid,so_time_initial_id,nc_corner,nc_edge,so_time_ini
     &tial,nc_stat)
      call ncvpt(fileid,so_time_final_id,nc_corner,nc_edge,so_time_final
     &,nc_stat)
      call ncvpt(fileid,so_rel_wt_min_id,nc_corner,nc_edge,so_rel_wt_min
     &,nc_stat)
      call ncvpt(fileid,so_rel_wt_max_id,nc_corner,nc_edge,so_rel_wt_max
     &,nc_stat)
      call ncvpt(fileid,so_wt_norm_min_id,nc_corner,nc_edge,so_wt_norm_m
     &in,nc_stat)
      call ncvpt(fileid,so_wt_norm_max_id,nc_corner,nc_edge,so_wt_norm_m
     &ax,nc_stat)
      call ncvpt(fileid,so_gparams_list_size_id,nc_corner,nc_edge,so_gpa
     &rams_list_size,nc_stat)
      call ncvpt(fileid,so_gparams_list_dim_id,nc_corner,nc_edge,so_gpar
     &ams_list_dim,nc_stat)
      call ncvpt(fileid,so_params_list_size_id,nc_corner,nc_edge,so_para
     &ms_list_size,nc_stat)
      call ncvpt(fileid,so_params_list_dim_id,nc_corner,nc_edge,so_param
     &s_list_dim,nc_stat)
      call ncvpt(fileid,so_params_data_size_id,nc_corner,nc_edge,so_para
     &ms_data_size,nc_stat)
      call ncvpt(fileid,so_params_data_dim_id,nc_corner,nc_edge,so_param
     &s_data_dim,nc_stat)
      call ncvpt(fileid,so_giparams_list_size_id,nc_corner,nc_edge,so_gi
     &params_list_size,nc_stat)
      call ncvpt(fileid,so_giparams_list_dim_id,nc_corner,nc_edge,so_gip
     &arams_list_dim,nc_stat)
      call ncvpt(fileid,so_iparams_list_size_id,nc_corner,nc_edge,so_ipa
     &rams_list_size,nc_stat)
      call ncvpt(fileid,so_iparams_list_dim_id,nc_corner,nc_edge,so_ipar
     &ams_list_dim,nc_stat)
      call ncvpt(fileid,so_iparams_data_size_id,nc_corner,nc_edge,so_ipa
     &rams_data_size,nc_stat)
      call ncvpt(fileid,so_iparams_data_dim_id,nc_corner,nc_edge,so_ipar
     &ams_data_dim,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((34)-(1)+1)
      call ncvptc(fileid,so_seed_decimal_id,nc_corner,nc_edge,so_seed_de
     &cimal,(((34)-(1)+1)),nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((10)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((6)-(1)+1)
      call ncvptc(fileid,source_name_id,nc_corner,nc_edge,source_name,((
     &(10)-(1)+1)*((6)-(1)+1)),nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_base_ptr_id,nc_corner,nc_edge,source_base
     &_ptr,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_num_segments_id,nc_corner,nc_edge,source_
     &num_segments,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_type_id,nc_corner,nc_edge,source_type,nc_
     &stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_geometry_id,nc_corner,nc_edge,source_geom
     &etry,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_num_flights_id,nc_corner,nc_edge,source_n
     &um_flights,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_num_checkpoints_id,nc_corner,nc_edge,sour
     &ce_num_checkpoints,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_species_id,nc_corner,nc_edge,source_speci
     &es,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_root_species_id,nc_corner,nc_edge,source_
     &root_species,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_time_variation_id,nc_corner,nc_edge,sourc
     &e_time_variation,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_num_gparameters_id,nc_corner,nc_edge,sour
     &ce_num_gparameters,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_num_parameters_id,nc_corner,nc_edge,sourc
     &e_num_parameters,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_gparams_list_dim)-(1)+1)
      call ncvpt(fileid,source_gparameters_list_id,nc_corner,nc_edge,sou
     &rce_gparameters_list,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_params_list_dim)-(1)+1)
      call ncvpt(fileid,source_parameters_list_id,nc_corner,nc_edge,sour
     &ce_parameters_list,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_gparameters_base_id,nc_corner,nc_edge,sou
     &rce_gparameters_base,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_parameters_base_id,nc_corner,nc_edge,sour
     &ce_parameters_base,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_parameters_data_base_id,nc_corner,nc_edge
     &,source_parameters_data_base,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_gparams_list_dim)-(1)+1)
      call ncvpt(fileid,source_gparameters_data_id,nc_corner,nc_edge,sou
     &rce_gparameters_data,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_params_data_dim)-(1)+1)
      call ncvpt(fileid,source_parameters_data_id,nc_corner,nc_edge,sour
     &ce_parameters_data,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_num_giparameters_id,nc_corner,nc_edge,sou
     &rce_num_giparameters,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_num_iparameters_id,nc_corner,nc_edge,sour
     &ce_num_iparameters,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_giparams_list_dim)-(1)+1)
      call ncvpt(fileid,source_giparameters_list_id,nc_corner,nc_edge,so
     &urce_giparameters_list,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_iparams_list_dim)-(1)+1)
      call ncvpt(fileid,source_iparameters_list_id,nc_corner,nc_edge,sou
     &rce_iparameters_list,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_giparameters_base_id,nc_corner,nc_edge,so
     &urce_giparameters_base,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_iparameters_base_id,nc_corner,nc_edge,sou
     &rce_iparameters_base,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_iparameters_data_base_id,nc_corner,nc_edg
     &e,source_iparameters_data_base,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_giparams_list_dim)-(1)+1)
      call ncvpt(fileid,source_giparameters_data_id,nc_corner,nc_edge,so
     &urce_giparameters_data,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_iparams_data_dim)-(1)+1)
      call ncvpt(fileid,source_iparameters_data_id,nc_corner,nc_edge,sou
     &rce_iparameters_data,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_total_current_id,nc_corner,nc_edge,source
     &_total_current,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_weight_norm_id,nc_corner,nc_edge,source_w
     &eight_norm,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_scale_factor_id,nc_corner,nc_edge,source_
     &scale_factor,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_seg_tot)-(1)+1)
      call ncvpt(fileid,source_segment_ptr_id,nc_corner,nc_edge,source_s
     &egment_ptr,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_seg_tot)-(1)+1)
      call ncvpt(fileid,source_current_id,nc_corner,nc_edge,source_curre
     &nt,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_seg_tot)-(1)+1)
      call ncvpt(fileid,source_segment_rel_wt_id,nc_corner,nc_edge,sourc
     &e_segment_rel_wt,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_seg_tot)-(1)+1)
      call ncvpt(fileid,source_segment_prob_alias_id,nc_corner,nc_edge,s
     &ource_segment_prob_alias,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_seg_tot)-(1)+1)
      call ncvpt(fileid,source_segment_ptr_alias_id,nc_corner,nc_edge,so
     &urce_segment_ptr_alias,nc_stat)
      
      call ncclos(fileid,nc_stat)
      return
      end
      subroutine write_old_sources
      
      use so_mod
      
      use rf_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer fileid
      character*96 tempfile
      integer nc_dims(5),nc_corner(5),nc_edge(5),nc_stat,nc_size,nc_rank
     &,nc_attr,nc_type
      character*128 nc_dummy
      external ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf
     &_inq_format
      integer ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf_
     &inq_format
      integer so_grps_id
      integer so_seg_tot_id
      integer so_restart_id
      integer so_spaced_seeds_id
      integer so_seed_spacing_id
      integer so_sampling_id
      integer so_time_dependent_id
      integer so_time_initialization_id
      integer so_time_initial_id
      integer so_time_final_id
      integer so_rel_wt_min_id
      integer so_rel_wt_max_id
      integer so_wt_norm_min_id
      integer so_wt_norm_max_id
      integer so_gparams_list_size_id
      integer so_gparams_list_dim_id
      integer so_params_list_size_id
      integer so_params_list_dim_id
      integer so_params_data_size_id
      integer so_params_data_dim_id
      integer so_giparams_list_size_id
      integer so_giparams_list_dim_id
      integer so_iparams_list_size_id
      integer so_iparams_list_dim_id
      integer so_iparams_data_size_id
      integer so_iparams_data_dim_id
      integer source_grp_ind_id
      integer source_seg_ind_id
      integer so_seed_decimal_ind_id
      integer so_name_ind_id
      integer so_type_ind_id
      integer source_gparams_ind_id
      integer source_params_ind_id
      integer source_params_data_ind_id
      integer source_giparams_ind_id
      integer source_iparams_ind_id
      integer source_iparams_data_ind_id
      integer so_seed_decimal_id
      integer source_name_id
      integer source_base_ptr_id
      integer source_num_segments_id
      integer source_type_id
      integer source_geometry_id
      integer source_num_flights_id
      integer source_num_checkpoints_id
      integer source_species_id
      integer source_root_species_id
      integer source_time_variation_id
      integer source_num_gparameters_id
      integer source_num_parameters_id
      integer source_gparameters_list_id
      integer source_parameters_list_id
      integer source_gparameters_base_id
      integer source_parameters_base_id
      integer source_parameters_data_base_id
      integer source_gparameters_data_id
      integer source_parameters_data_id
      integer source_num_giparameters_id
      integer source_num_iparameters_id
      integer source_giparameters_list_id
      integer source_iparameters_list_id
      integer source_giparameters_base_id
      integer source_iparameters_base_id
      integer source_iparameters_data_base_id
      integer source_giparameters_data_id
      integer source_iparameters_data_id
      integer source_total_current_id
      integer source_weight_norm_id
      integer source_scale_factor_id
      integer source_segment_ptr_id
      integer source_current_id
      integer source_segment_rel_wt_id
      integer source_segment_prob_alias_id
      integer source_segment_ptr_alias_id
      
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      tempfile=filenames_array(21)
      if(tempfile.NE.'undefined')then
      fileid=nccre(tempfile,0,nc_stat)
      so_grps_id=ncvdef(fileid,'so_grps',4,0,nc_dims,nc_stat)
      so_seg_tot_id=ncvdef(fileid,'so_seg_tot',4,0,nc_dims,nc_stat)
      so_restart_id=ncvdef(fileid,'so_restart',4,0,nc_dims,nc_stat)
      so_spaced_seeds_id=ncvdef(fileid,'so_spaced_seeds',4,0,nc_dims,nc_
     &stat)
      so_seed_spacing_id=ncvdef(fileid,'so_seed_spacing',4,0,nc_dims,nc_
     &stat)
      so_sampling_id=ncvdef(fileid,'so_sampling',4,0,nc_dims,nc_stat)
      so_time_dependent_id=ncvdef(fileid,'so_time_dependent',4,0,nc_dims
     &,nc_stat)
      so_time_initialization_id=ncvdef(fileid,'so_time_initialization',4
     &,0,nc_dims,nc_stat)
      so_time_initial_id=ncvdef(fileid,'so_time_initial',6,0,nc_dims,nc_
     &stat)
      so_time_final_id=ncvdef(fileid,'so_time_final',6,0,nc_dims,nc_stat
     &)
      so_rel_wt_min_id=ncvdef(fileid,'so_rel_wt_min',6,0,nc_dims,nc_stat
     &)
      so_rel_wt_max_id=ncvdef(fileid,'so_rel_wt_max',6,0,nc_dims,nc_stat
     &)
      so_wt_norm_min_id=ncvdef(fileid,'so_wt_norm_min',6,0,nc_dims,nc_st
     &at)
      so_wt_norm_max_id=ncvdef(fileid,'so_wt_norm_max',6,0,nc_dims,nc_st
     &at)
      so_gparams_list_size_id=ncvdef(fileid,'so_gparams_list_size',4,0,n
     &c_dims,nc_stat)
      so_gparams_list_dim_id=ncvdef(fileid,'so_gparams_list_dim',4,0,nc_
     &dims,nc_stat)
      so_params_list_size_id=ncvdef(fileid,'so_params_list_size',4,0,nc_
     &dims,nc_stat)
      so_params_list_dim_id=ncvdef(fileid,'so_params_list_dim',4,0,nc_di
     &ms,nc_stat)
      so_params_data_size_id=ncvdef(fileid,'so_params_data_size',4,0,nc_
     &dims,nc_stat)
      so_params_data_dim_id=ncvdef(fileid,'so_params_data_dim',4,0,nc_di
     &ms,nc_stat)
      so_giparams_list_size_id=ncvdef(fileid,'so_giparams_list_size',4,0
     &,nc_dims,nc_stat)
      so_giparams_list_dim_id=ncvdef(fileid,'so_giparams_list_dim',4,0,n
     &c_dims,nc_stat)
      so_iparams_list_size_id=ncvdef(fileid,'so_iparams_list_size',4,0,n
     &c_dims,nc_stat)
      so_iparams_list_dim_id=ncvdef(fileid,'so_iparams_list_dim',4,0,nc_
     &dims,nc_stat)
      so_iparams_data_size_id=ncvdef(fileid,'so_iparams_data_size',4,0,n
     &c_dims,nc_stat)
      so_iparams_data_dim_id=ncvdef(fileid,'so_iparams_data_dim',4,0,nc_
     &dims,nc_stat)
      source_grp_ind_id=ncddef(fileid,'source_grp_ind',((so_grps)-(1)+1)
     &,nc_stat)
      source_seg_ind_id=ncddef(fileid,'source_seg_ind',((so_seg_tot)-(1)
     &+1),nc_stat)
      so_seed_decimal_ind_id=ncddef(fileid,'so_seed_decimal_ind',((34)-(
     &1)+1),nc_stat)
      so_name_ind_id=ncddef(fileid,'so_name_ind',((10)-(1)+1),nc_stat)
      so_type_ind_id=ncddef(fileid,'so_type_ind',((6)-(1)+1),nc_stat)
      source_gparams_ind_id=ncddef(fileid,'source_gparams_ind',((so_gpar
     &ams_list_dim)-(1)+1),nc_stat)
      source_params_ind_id=ncddef(fileid,'source_params_ind',((so_params
     &_list_dim)-(1)+1),nc_stat)
      source_params_data_ind_id=ncddef(fileid,'source_params_data_ind',(
     &(so_params_data_dim)-(1)+1),nc_stat)
      source_giparams_ind_id=ncddef(fileid,'source_giparams_ind',((so_gi
     &params_list_dim)-(1)+1),nc_stat)
      source_iparams_ind_id=ncddef(fileid,'source_iparams_ind',((so_ipar
     &ams_list_dim)-(1)+1),nc_stat)
      source_iparams_data_ind_id=ncddef(fileid,'source_iparams_data_ind'
     &,((so_iparams_data_dim)-(1)+1),nc_stat)
      nc_dims(1)=so_seed_decimal_ind_id
      so_seed_decimal_id=ncvdef(fileid,'so_seed_decimal',2,1,nc_dims,nc_
     &stat)
      nc_dims(1)=so_name_ind_id
      nc_dims(2)=so_type_ind_id
      source_name_id=ncvdef(fileid,'source_name',2,2,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_base_ptr_id=ncvdef(fileid,'source_base_ptr',4,1,nc_dims,nc_
     &stat)
      nc_dims(1)=source_grp_ind_id
      source_num_segments_id=ncvdef(fileid,'source_num_segments',4,1,nc_
     &dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_type_id=ncvdef(fileid,'source_type',4,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_geometry_id=ncvdef(fileid,'source_geometry',4,1,nc_dims,nc_
     &stat)
      nc_dims(1)=source_grp_ind_id
      source_num_flights_id=ncvdef(fileid,'source_num_flights',4,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_num_checkpoints_id=ncvdef(fileid,'source_num_checkpoints',4
     &,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_species_id=ncvdef(fileid,'source_species',4,1,nc_dims,nc_st
     &at)
      nc_dims(1)=source_grp_ind_id
      source_root_species_id=ncvdef(fileid,'source_root_species',4,1,nc_
     &dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_time_variation_id=ncvdef(fileid,'source_time_variation',4,1
     &,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_num_gparameters_id=ncvdef(fileid,'source_num_gparameters',4
     &,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_num_parameters_id=ncvdef(fileid,'source_num_parameters',4,1
     &,nc_dims,nc_stat)
      nc_dims(1)=source_gparams_ind_id
      source_gparameters_list_id=ncvdef(fileid,'source_gparameters_list'
     &,4,1,nc_dims,nc_stat)
      nc_dims(1)=source_params_ind_id
      source_parameters_list_id=ncvdef(fileid,'source_parameters_list',4
     &,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_gparameters_base_id=ncvdef(fileid,'source_gparameters_base'
     &,4,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_parameters_base_id=ncvdef(fileid,'source_parameters_base',4
     &,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_parameters_data_base_id=ncvdef(fileid,'source_parameters_da
     &ta_base',4,1,nc_dims,nc_stat)
      nc_dims(1)=source_gparams_ind_id
      source_gparameters_data_id=ncvdef(fileid,'source_gparameters_data'
     &,6,1,nc_dims,nc_stat)
      nc_dims(1)=source_params_data_ind_id
      source_parameters_data_id=ncvdef(fileid,'source_parameters_data',6
     &,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_num_giparameters_id=ncvdef(fileid,'source_num_giparameters'
     &,4,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_num_iparameters_id=ncvdef(fileid,'source_num_iparameters',4
     &,1,nc_dims,nc_stat)
      nc_dims(1)=source_giparams_ind_id
      source_giparameters_list_id=ncvdef(fileid,'source_giparameters_lis
     &t',4,1,nc_dims,nc_stat)
      nc_dims(1)=source_iparams_ind_id
      source_iparameters_list_id=ncvdef(fileid,'source_iparameters_list'
     &,4,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_giparameters_base_id=ncvdef(fileid,'source_giparameters_bas
     &e',4,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_iparameters_base_id=ncvdef(fileid,'source_iparameters_base'
     &,4,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_iparameters_data_base_id=ncvdef(fileid,'source_iparameters_
     &data_base',4,1,nc_dims,nc_stat)
      nc_dims(1)=source_giparams_ind_id
      source_giparameters_data_id=ncvdef(fileid,'source_giparameters_dat
     &a',4,1,nc_dims,nc_stat)
      nc_dims(1)=source_iparams_data_ind_id
      source_iparameters_data_id=ncvdef(fileid,'source_iparameters_data'
     &,4,1,nc_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_total_current_id=ncvdef(fileid,'source_total_current',6,1,n
     &c_dims,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_weight_norm_id=ncvdef(fileid,'source_weight_norm',6,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=source_grp_ind_id
      source_scale_factor_id=ncvdef(fileid,'source_scale_factor',6,1,nc_
     &dims,nc_stat)
      nc_dims(1)=source_seg_ind_id
      source_segment_ptr_id=ncvdef(fileid,'source_segment_ptr',4,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=source_seg_ind_id
      source_current_id=ncvdef(fileid,'source_current',6,1,nc_dims,nc_st
     &at)
      nc_dims(1)=source_seg_ind_id
      source_segment_rel_wt_id=ncvdef(fileid,'source_segment_rel_wt',6,1
     &,nc_dims,nc_stat)
      nc_dims(1)=source_seg_ind_id
      source_segment_prob_alias_id=ncvdef(fileid,'source_segment_prob_al
     &ias',6,1,nc_dims,nc_stat)
      nc_dims(1)=source_seg_ind_id
      source_segment_ptr_alias_id=ncvdef(fileid,'source_segment_ptr_alia
     &s',4,1,nc_dims,nc_stat)
      
      call ncendf(fileid,nc_stat)
      
      call ncvpt(fileid,so_grps_id,nc_corner,nc_edge,so_grps,nc_stat)
      call ncvpt(fileid,so_seg_tot_id,nc_corner,nc_edge,so_seg_tot,nc_st
     &at)
      call ncvpt(fileid,so_restart_id,nc_corner,nc_edge,so_restart,nc_st
     &at)
      call ncvpt(fileid,so_spaced_seeds_id,nc_corner,nc_edge,so_spaced_s
     &eeds,nc_stat)
      call ncvpt(fileid,so_seed_spacing_id,nc_corner,nc_edge,so_seed_spa
     &cing,nc_stat)
      call ncvpt(fileid,so_sampling_id,nc_corner,nc_edge,so_sampling,nc_
     &stat)
      call ncvpt(fileid,so_time_dependent_id,nc_corner,nc_edge,so_time_d
     &ependent,nc_stat)
      call ncvpt(fileid,so_time_initialization_id,nc_corner,nc_edge,so_t
     &ime_initialization,nc_stat)
      call ncvpt(fileid,so_time_initial_id,nc_corner,nc_edge,so_time_ini
     &tial,nc_stat)
      call ncvpt(fileid,so_time_final_id,nc_corner,nc_edge,so_time_final
     &,nc_stat)
      call ncvpt(fileid,so_rel_wt_min_id,nc_corner,nc_edge,so_rel_wt_min
     &,nc_stat)
      call ncvpt(fileid,so_rel_wt_max_id,nc_corner,nc_edge,so_rel_wt_max
     &,nc_stat)
      call ncvpt(fileid,so_wt_norm_min_id,nc_corner,nc_edge,so_wt_norm_m
     &in,nc_stat)
      call ncvpt(fileid,so_wt_norm_max_id,nc_corner,nc_edge,so_wt_norm_m
     &ax,nc_stat)
      call ncvpt(fileid,so_gparams_list_size_id,nc_corner,nc_edge,so_gpa
     &rams_list_size,nc_stat)
      call ncvpt(fileid,so_gparams_list_dim_id,nc_corner,nc_edge,so_gpar
     &ams_list_dim,nc_stat)
      call ncvpt(fileid,so_params_list_size_id,nc_corner,nc_edge,so_para
     &ms_list_size,nc_stat)
      call ncvpt(fileid,so_params_list_dim_id,nc_corner,nc_edge,so_param
     &s_list_dim,nc_stat)
      call ncvpt(fileid,so_params_data_size_id,nc_corner,nc_edge,so_para
     &ms_data_size,nc_stat)
      call ncvpt(fileid,so_params_data_dim_id,nc_corner,nc_edge,so_param
     &s_data_dim,nc_stat)
      call ncvpt(fileid,so_giparams_list_size_id,nc_corner,nc_edge,so_gi
     &params_list_size,nc_stat)
      call ncvpt(fileid,so_giparams_list_dim_id,nc_corner,nc_edge,so_gip
     &arams_list_dim,nc_stat)
      call ncvpt(fileid,so_iparams_list_size_id,nc_corner,nc_edge,so_ipa
     &rams_list_size,nc_stat)
      call ncvpt(fileid,so_iparams_list_dim_id,nc_corner,nc_edge,so_ipar
     &ams_list_dim,nc_stat)
      call ncvpt(fileid,so_iparams_data_size_id,nc_corner,nc_edge,so_ipa
     &rams_data_size,nc_stat)
      call ncvpt(fileid,so_iparams_data_dim_id,nc_corner,nc_edge,so_ipar
     &ams_data_dim,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((34)-(1)+1)
      call ncvptc(fileid,so_seed_decimal_id,nc_corner,nc_edge,so_seed_de
     &cimal,(((34)-(1)+1)),nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((10)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((6)-(1)+1)
      call ncvptc(fileid,source_name_id,nc_corner,nc_edge,source_name,((
     &(10)-(1)+1)*((6)-(1)+1)),nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_base_ptr_id,nc_corner,nc_edge,source_base
     &_ptr,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_num_segments_id,nc_corner,nc_edge,source_
     &num_segments,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_type_id,nc_corner,nc_edge,source_type,nc_
     &stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_geometry_id,nc_corner,nc_edge,source_geom
     &etry,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_num_flights_id,nc_corner,nc_edge,source_n
     &um_flights,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_num_checkpoints_id,nc_corner,nc_edge,sour
     &ce_num_checkpoints,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_species_id,nc_corner,nc_edge,source_speci
     &es,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_root_species_id,nc_corner,nc_edge,source_
     &root_species,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_time_variation_id,nc_corner,nc_edge,sourc
     &e_time_variation,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_num_gparameters_id,nc_corner,nc_edge,sour
     &ce_num_gparameters,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_num_parameters_id,nc_corner,nc_edge,sourc
     &e_num_parameters,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_gparams_list_dim)-(1)+1)
      call ncvpt(fileid,source_gparameters_list_id,nc_corner,nc_edge,sou
     &rce_gparameters_list,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_params_list_dim)-(1)+1)
      call ncvpt(fileid,source_parameters_list_id,nc_corner,nc_edge,sour
     &ce_parameters_list,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_gparameters_base_id,nc_corner,nc_edge,sou
     &rce_gparameters_base,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_parameters_base_id,nc_corner,nc_edge,sour
     &ce_parameters_base,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_parameters_data_base_id,nc_corner,nc_edge
     &,source_parameters_data_base,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_gparams_list_dim)-(1)+1)
      call ncvpt(fileid,source_gparameters_data_id,nc_corner,nc_edge,sou
     &rce_gparameters_data,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_params_data_dim)-(1)+1)
      call ncvpt(fileid,source_parameters_data_id,nc_corner,nc_edge,sour
     &ce_parameters_data,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_num_giparameters_id,nc_corner,nc_edge,sou
     &rce_num_giparameters,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_num_iparameters_id,nc_corner,nc_edge,sour
     &ce_num_iparameters,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_giparams_list_dim)-(1)+1)
      call ncvpt(fileid,source_giparameters_list_id,nc_corner,nc_edge,so
     &urce_giparameters_list,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_iparams_list_dim)-(1)+1)
      call ncvpt(fileid,source_iparameters_list_id,nc_corner,nc_edge,sou
     &rce_iparameters_list,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_giparameters_base_id,nc_corner,nc_edge,so
     &urce_giparameters_base,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_iparameters_base_id,nc_corner,nc_edge,sou
     &rce_iparameters_base,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_iparameters_data_base_id,nc_corner,nc_edg
     &e,source_iparameters_data_base,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_giparams_list_dim)-(1)+1)
      call ncvpt(fileid,source_giparameters_data_id,nc_corner,nc_edge,so
     &urce_giparameters_data,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_iparams_data_dim)-(1)+1)
      call ncvpt(fileid,source_iparameters_data_id,nc_corner,nc_edge,sou
     &rce_iparameters_data,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_total_current_id,nc_corner,nc_edge,source
     &_total_current,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_weight_norm_id,nc_corner,nc_edge,source_w
     &eight_norm,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvpt(fileid,source_scale_factor_id,nc_corner,nc_edge,source_
     &scale_factor,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_seg_tot)-(1)+1)
      call ncvpt(fileid,source_segment_ptr_id,nc_corner,nc_edge,source_s
     &egment_ptr,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_seg_tot)-(1)+1)
      call ncvpt(fileid,source_current_id,nc_corner,nc_edge,source_curre
     &nt,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_seg_tot)-(1)+1)
      call ncvpt(fileid,source_segment_rel_wt_id,nc_corner,nc_edge,sourc
     &e_segment_rel_wt,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_seg_tot)-(1)+1)
      call ncvpt(fileid,source_segment_prob_alias_id,nc_corner,nc_edge,s
     &ource_segment_prob_alias,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((so_seg_tot)-(1)+1)
      call ncvpt(fileid,source_segment_ptr_alias_id,nc_corner,nc_edge,so
     &urce_segment_ptr_alias,nc_stat)
      
      call ncclos(fileid,nc_stat)
      write(6,*)'Old source file ',tempfile  (1:string_length(tempfile))
     &,' written'
      end if
      return
      end
      subroutine nc_read_old_sources(old_source_file)
      
      use so_mod
      
      use rf_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer old_source_file
      integer fileid
      character*96 tempfile
      integer nc_dims(5),nc_corner(5),nc_edge(5),nc_stat,nc_size,nc_rank
     &,nc_attr,nc_type
      character*128 nc_dummy
      external ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf
     &_inq_format
      integer ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf_
     &inq_format
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
      
      
      integer so_grps_id
      integer so_seg_tot_id
      integer so_restart_id
      integer so_spaced_seeds_id
      integer so_seed_spacing_id
      integer so_sampling_id
      integer so_time_dependent_id
      integer so_time_initialization_id
      integer so_time_initial_id
      integer so_time_final_id
      integer so_rel_wt_min_id
      integer so_rel_wt_max_id
      integer so_wt_norm_min_id
      integer so_wt_norm_max_id
      integer so_gparams_list_size_id
      integer so_gparams_list_dim_id
      integer so_params_list_size_id
      integer so_params_list_dim_id
      integer so_params_data_size_id
      integer so_params_data_dim_id
      integer so_giparams_list_size_id
      integer so_giparams_list_dim_id
      integer so_iparams_list_size_id
      integer so_iparams_list_dim_id
      integer so_iparams_data_size_id
      integer so_iparams_data_dim_id
      integer source_grp_ind_id
      integer source_seg_ind_id
      integer so_seed_decimal_ind_id
      integer so_name_ind_id
      integer so_type_ind_id
      integer source_gparams_ind_id
      integer source_params_ind_id
      integer source_params_data_ind_id
      integer source_giparams_ind_id
      integer source_iparams_ind_id
      integer source_iparams_data_ind_id
      integer so_seed_decimal_id
      integer source_name_id
      integer source_base_ptr_id
      integer source_num_segments_id
      integer source_type_id
      integer source_geometry_id
      integer source_num_flights_id
      integer source_num_checkpoints_id
      integer source_species_id
      integer source_root_species_id
      integer source_time_variation_id
      integer source_num_gparameters_id
      integer source_num_parameters_id
      integer source_gparameters_list_id
      integer source_parameters_list_id
      integer source_gparameters_base_id
      integer source_parameters_base_id
      integer source_parameters_data_base_id
      integer source_gparameters_data_id
      integer source_parameters_data_id
      integer source_num_giparameters_id
      integer source_num_iparameters_id
      integer source_giparameters_list_id
      integer source_iparameters_list_id
      integer source_giparameters_base_id
      integer source_iparameters_base_id
      integer source_iparameters_data_base_id
      integer source_giparameters_data_id
      integer source_iparameters_data_id
      integer source_total_current_id
      integer source_weight_norm_id
      integer source_scale_factor_id
      integer source_segment_ptr_id
      integer source_current_id
      integer source_segment_rel_wt_id
      integer source_segment_prob_alias_id
      integer source_segment_ptr_alias_id
      
      tempfile=filenames_array(21)
      if(tempfile.EQ.'undefined')then
      old_source_file=0
      return
      end if
      call ncpopt(0)
      fileid=ncopn(tempfile,0,nc_stat)
      if(nc_stat.NE.0)then
      old_source_file=0
      return
      else
      old_source_file=1
      end if
      call ncpopt(2+1)
      so_grps_id=ncvid(fileid,'so_grps',nc_stat)
      call ncvinq(fileid,so_grps_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_
     &attr,nc_stat)
      
      call ncvgt(fileid,so_grps_id,nc_corner,nc_edge,so_grps,nc_stat)
      so_seg_tot_id=ncvid(fileid,'so_seg_tot',nc_stat)
      call ncvinq(fileid,so_seg_tot_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      
      call ncvgt(fileid,so_seg_tot_id,nc_corner,nc_edge,so_seg_tot,nc_st
     &at)
      so_restart_id=ncvid(fileid,'so_restart',nc_stat)
      call ncvinq(fileid,so_restart_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      
      call ncvgt(fileid,so_restart_id,nc_corner,nc_edge,so_restart,nc_st
     &at)
      so_spaced_seeds_id=ncvid(fileid,'so_spaced_seeds',nc_stat)
      call ncvinq(fileid,so_spaced_seeds_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_spaced_seeds_id,nc_corner,nc_edge,so_spaced_s
     &eeds,nc_stat)
      so_seed_spacing_id=ncvid(fileid,'so_seed_spacing',nc_stat)
      call ncvinq(fileid,so_seed_spacing_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_seed_spacing_id,nc_corner,nc_edge,so_seed_spa
     &cing,nc_stat)
      so_sampling_id=ncvid(fileid,'so_sampling',nc_stat)
      call ncvinq(fileid,so_sampling_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_sampling_id,nc_corner,nc_edge,so_sampling,nc_
     &stat)
      so_time_dependent_id=ncvid(fileid,'so_time_dependent',nc_stat)
      call ncvinq(fileid,so_time_dependent_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_time_dependent_id,nc_corner,nc_edge,so_time_d
     &ependent,nc_stat)
      so_time_initialization_id=ncvid(fileid,'so_time_initialization',nc
     &_stat)
      call ncvinq(fileid,so_time_initialization_id,nc_dummy,nc_type,nc_r
     &ank,nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_time_initialization_id,nc_corner,nc_edge,so_t
     &ime_initialization,nc_stat)
      so_time_initial_id=ncvid(fileid,'so_time_initial',nc_stat)
      call ncvinq(fileid,so_time_initial_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_time_initial_id,nc_corner,nc_edge,so_time_ini
     &tial,nc_stat)
      so_time_final_id=ncvid(fileid,'so_time_final',nc_stat)
      call ncvinq(fileid,so_time_final_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_time_final_id,nc_corner,nc_edge,so_time_final
     &,nc_stat)
      so_rel_wt_min_id=ncvid(fileid,'so_rel_wt_min',nc_stat)
      call ncvinq(fileid,so_rel_wt_min_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_rel_wt_min_id,nc_corner,nc_edge,so_rel_wt_min
     &,nc_stat)
      so_rel_wt_max_id=ncvid(fileid,'so_rel_wt_max',nc_stat)
      call ncvinq(fileid,so_rel_wt_max_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_rel_wt_max_id,nc_corner,nc_edge,so_rel_wt_max
     &,nc_stat)
      so_wt_norm_min_id=ncvid(fileid,'so_wt_norm_min',nc_stat)
      call ncvinq(fileid,so_wt_norm_min_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_wt_norm_min_id,nc_corner,nc_edge,so_wt_norm_m
     &in,nc_stat)
      so_wt_norm_max_id=ncvid(fileid,'so_wt_norm_max',nc_stat)
      call ncvinq(fileid,so_wt_norm_max_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_wt_norm_max_id,nc_corner,nc_edge,so_wt_norm_m
     &ax,nc_stat)
      so_gparams_list_size_id=ncvid(fileid,'so_gparams_list_size',nc_sta
     &t)
      call ncvinq(fileid,so_gparams_list_size_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_gparams_list_size_id,nc_corner,nc_edge,so_gpa
     &rams_list_size,nc_stat)
      so_gparams_list_dim_id=ncvid(fileid,'so_gparams_list_dim',nc_stat)
      call ncvinq(fileid,so_gparams_list_dim_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_gparams_list_dim_id,nc_corner,nc_edge,so_gpar
     &ams_list_dim,nc_stat)
      so_params_list_size_id=ncvid(fileid,'so_params_list_size',nc_stat)
      call ncvinq(fileid,so_params_list_size_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_params_list_size_id,nc_corner,nc_edge,so_para
     &ms_list_size,nc_stat)
      so_params_list_dim_id=ncvid(fileid,'so_params_list_dim',nc_stat)
      call ncvinq(fileid,so_params_list_dim_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_params_list_dim_id,nc_corner,nc_edge,so_param
     &s_list_dim,nc_stat)
      so_params_data_size_id=ncvid(fileid,'so_params_data_size',nc_stat)
      call ncvinq(fileid,so_params_data_size_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_params_data_size_id,nc_corner,nc_edge,so_para
     &ms_data_size,nc_stat)
      so_params_data_dim_id=ncvid(fileid,'so_params_data_dim',nc_stat)
      call ncvinq(fileid,so_params_data_dim_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_params_data_dim_id,nc_corner,nc_edge,so_param
     &s_data_dim,nc_stat)
      so_giparams_list_size_id=ncvid(fileid,'so_giparams_list_size',nc_s
     &tat)
      call ncvinq(fileid,so_giparams_list_size_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_giparams_list_size_id,nc_corner,nc_edge,so_gi
     &params_list_size,nc_stat)
      so_giparams_list_dim_id=ncvid(fileid,'so_giparams_list_dim',nc_sta
     &t)
      call ncvinq(fileid,so_giparams_list_dim_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_giparams_list_dim_id,nc_corner,nc_edge,so_gip
     &arams_list_dim,nc_stat)
      so_iparams_list_size_id=ncvid(fileid,'so_iparams_list_size',nc_sta
     &t)
      call ncvinq(fileid,so_iparams_list_size_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_iparams_list_size_id,nc_corner,nc_edge,so_ipa
     &rams_list_size,nc_stat)
      so_iparams_list_dim_id=ncvid(fileid,'so_iparams_list_dim',nc_stat)
      call ncvinq(fileid,so_iparams_list_dim_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_iparams_list_dim_id,nc_corner,nc_edge,so_ipar
     &ams_list_dim,nc_stat)
      so_iparams_data_size_id=ncvid(fileid,'so_iparams_data_size',nc_sta
     &t)
      call ncvinq(fileid,so_iparams_data_size_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_iparams_data_size_id,nc_corner,nc_edge,so_ipa
     &rams_data_size,nc_stat)
      so_iparams_data_dim_id=ncvid(fileid,'so_iparams_data_dim',nc_stat)
      call ncvinq(fileid,so_iparams_data_dim_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,so_iparams_data_dim_id,nc_corner,nc_edge,so_ipar
     &ams_data_dim,nc_stat)
      source_grp_ind_id=ncdid(fileid,'source_grp_ind',nc_stat)
      call ncdinq(fileid,source_grp_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((so_grps)-(1)+1))continue
      source_seg_ind_id=ncdid(fileid,'source_seg_ind',nc_stat)
      call ncdinq(fileid,source_seg_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((so_seg_tot)-(1)+1))continue
      so_seed_decimal_ind_id=ncdid(fileid,'so_seed_decimal_ind',nc_stat)
      call ncdinq(fileid,so_seed_decimal_ind_id,nc_dummy,nc_size,nc_stat
     &)
      if(nc_size.EQ.((34)-(1)+1))continue
      so_name_ind_id=ncdid(fileid,'so_name_ind',nc_stat)
      call ncdinq(fileid,so_name_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((10)-(1)+1))continue
      so_type_ind_id=ncdid(fileid,'so_type_ind',nc_stat)
      call ncdinq(fileid,so_type_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((6)-(1)+1))continue
      source_gparams_ind_id=ncdid(fileid,'source_gparams_ind',nc_stat)
      call ncdinq(fileid,source_gparams_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((so_gparams_list_dim)-(1)+1))continue
      source_params_ind_id=ncdid(fileid,'source_params_ind',nc_stat)
      call ncdinq(fileid,source_params_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((so_params_list_dim)-(1)+1))continue
      source_params_data_ind_id=ncdid(fileid,'source_params_data_ind',nc
     &_stat)
      call ncdinq(fileid,source_params_data_ind_id,nc_dummy,nc_size,nc_s
     &tat)
      if(nc_size.EQ.((so_params_data_dim)-(1)+1))continue
      source_giparams_ind_id=ncdid(fileid,'source_giparams_ind',nc_stat)
      call ncdinq(fileid,source_giparams_ind_id,nc_dummy,nc_size,nc_stat
     &)
      if(nc_size.EQ.((so_giparams_list_dim)-(1)+1))continue
      source_iparams_ind_id=ncdid(fileid,'source_iparams_ind',nc_stat)
      call ncdinq(fileid,source_iparams_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((so_iparams_list_dim)-(1)+1))continue
      source_iparams_data_ind_id=ncdid(fileid,'source_iparams_data_ind',
     &nc_stat)
      call ncdinq(fileid,source_iparams_data_ind_id,nc_dummy,nc_size,nc_
     &stat)
      if(nc_size.EQ.((so_iparams_data_dim)-(1)+1))continue
      so_seed_decimal_id=ncvid(fileid,'so_seed_decimal',nc_stat)
      call ncvinq(fileid,so_seed_decimal_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.so_seed_decimal_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((34)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,so_seed_decimal_id,nc_corner,nc_ed
     &ge,so_seed_decimal)
      source_name_id=ncvid(fileid,'source_name',nc_stat)
      call ncvinq(fileid,source_name_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      if(nc_dims(1).EQ.so_name_ind_id)continue
      if(nc_dims(2).EQ.so_type_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((10)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((6)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,source_name_id,nc_corner,nc_edge,s
     &ource_name)
      source_base_ptr_id=ncvid(fileid,'source_base_ptr',nc_stat)
      call ncvinq(fileid,source_base_ptr_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_base_ptr =>mem_alloc_i1((1),(so_grps),'source_base_ptr')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_base_ptr_id,nc_corner,nc_edge,source_base
     &_ptr,nc_stat)
      source_num_segments_id=ncvid(fileid,'source_num_segments',nc_stat)
      call ncvinq(fileid,source_num_segments_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_num_segments =>mem_alloc_i1((1),(so_grps),'source_num_segme
     &nts')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_num_segments_id,nc_corner,nc_edge,source_
     &num_segments,nc_stat)
      source_type_id=ncvid(fileid,'source_type',nc_stat)
      call ncvinq(fileid,source_type_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_type =>mem_alloc_i1((1),(so_grps),'source_type')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_type_id,nc_corner,nc_edge,source_type,nc_
     &stat)
      source_geometry_id=ncvid(fileid,'source_geometry',nc_stat)
      call ncvinq(fileid,source_geometry_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_geometry =>mem_alloc_i1((1),(so_grps),'source_geometry')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_geometry_id,nc_corner,nc_edge,source_geom
     &etry,nc_stat)
      source_num_flights_id=ncvid(fileid,'source_num_flights',nc_stat)
      call ncvinq(fileid,source_num_flights_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_num_flights =>mem_alloc_i1((1),(so_grps),'source_num_flight
     &s')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_num_flights_id,nc_corner,nc_edge,source_n
     &um_flights,nc_stat)
      source_num_checkpoints_id=ncvid(fileid,'source_num_checkpoints',nc
     &_stat)
      call ncvinq(fileid,source_num_checkpoints_id,nc_dummy,nc_type,nc_r
     &ank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_num_checkpoints =>mem_alloc_i1((1),(so_grps),'source_num_ch
     &eckpoints')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_num_checkpoints_id,nc_corner,nc_edge,sour
     &ce_num_checkpoints,nc_stat)
      source_species_id=ncvid(fileid,'source_species',nc_stat)
      call ncvinq(fileid,source_species_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_species =>mem_alloc_i1((1),(so_grps),'source_species')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_species_id,nc_corner,nc_edge,source_speci
     &es,nc_stat)
      source_root_species_id=ncvid(fileid,'source_root_species',nc_stat)
      call ncvinq(fileid,source_root_species_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_root_species =>mem_alloc_i1((1),(so_grps),'source_root_spec
     &ies')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_root_species_id,nc_corner,nc_edge,source_
     &root_species,nc_stat)
      source_time_variation_id=ncvid(fileid,'source_time_variation',nc_s
     &tat)
      call ncvinq(fileid,source_time_variation_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_time_variation =>mem_alloc_i1((1),(so_grps),'source_time_va
     &riation')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_time_variation_id,nc_corner,nc_edge,sourc
     &e_time_variation,nc_stat)
      source_num_gparameters_id=ncvid(fileid,'source_num_gparameters',nc
     &_stat)
      call ncvinq(fileid,source_num_gparameters_id,nc_dummy,nc_type,nc_r
     &ank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_num_gparameters =>mem_alloc_i1((1),(so_grps),'source_num_gp
     &arameters')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_num_gparameters_id,nc_corner,nc_edge,sour
     &ce_num_gparameters,nc_stat)
      source_num_parameters_id=ncvid(fileid,'source_num_parameters',nc_s
     &tat)
      call ncvinq(fileid,source_num_parameters_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_num_parameters =>mem_alloc_i1((1),(so_grps),'source_num_par
     &ameters')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_num_parameters_id,nc_corner,nc_edge,sourc
     &e_num_parameters,nc_stat)
      source_gparameters_list_id=ncvid(fileid,'source_gparameters_list',
     &nc_stat)
      call ncvinq(fileid,source_gparameters_list_id,nc_dummy,nc_type,nc_
     &rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_gparams_ind_id)continue
      
      source_gparameters_list =>mem_alloc_i1((1),(so_gparams_list_dim),'
     &source_gparameters_list')
      nc_corner(1)=1
      nc_edge(1)=((so_gparams_list_dim)-(1)+1)
      call ncvgt(fileid,source_gparameters_list_id,nc_corner,nc_edge,sou
     &rce_gparameters_list,nc_stat)
      source_parameters_list_id=ncvid(fileid,'source_parameters_list',nc
     &_stat)
      call ncvinq(fileid,source_parameters_list_id,nc_dummy,nc_type,nc_r
     &ank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_params_ind_id)continue
      
      source_parameters_list =>mem_alloc_i1((1),(so_params_list_dim),'so
     &urce_parameters_list')
      nc_corner(1)=1
      nc_edge(1)=((so_params_list_dim)-(1)+1)
      call ncvgt(fileid,source_parameters_list_id,nc_corner,nc_edge,sour
     &ce_parameters_list,nc_stat)
      source_gparameters_base_id=ncvid(fileid,'source_gparameters_base',
     &nc_stat)
      call ncvinq(fileid,source_gparameters_base_id,nc_dummy,nc_type,nc_
     &rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_gparameters_base =>mem_alloc_i1((1),(so_grps),'source_gpara
     &meters_base')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_gparameters_base_id,nc_corner,nc_edge,sou
     &rce_gparameters_base,nc_stat)
      source_parameters_base_id=ncvid(fileid,'source_parameters_base',nc
     &_stat)
      call ncvinq(fileid,source_parameters_base_id,nc_dummy,nc_type,nc_r
     &ank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_parameters_base =>mem_alloc_i1((1),(so_grps),'source_parame
     &ters_base')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_parameters_base_id,nc_corner,nc_edge,sour
     &ce_parameters_base,nc_stat)
      source_parameters_data_base_id=ncvid(fileid,'source_parameters_dat
     &a_base',nc_stat)
      call ncvinq(fileid,source_parameters_data_base_id,nc_dummy,nc_type
     &,nc_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_parameters_data_base =>mem_alloc_i1((1),(so_grps),'source_p
     &arameters_data_base')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_parameters_data_base_id,nc_corner,nc_edge
     &,source_parameters_data_base,nc_stat)
      source_gparameters_data_id=ncvid(fileid,'source_gparameters_data',
     &nc_stat)
      call ncvinq(fileid,source_gparameters_data_id,nc_dummy,nc_type,nc_
     &rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_gparams_ind_id)continue
      
      source_gparameters_data =>mem_alloc_r1((1),(so_gparams_list_dim),'
     &source_gparameters_data')
      nc_corner(1)=1
      nc_edge(1)=((so_gparams_list_dim)-(1)+1)
      call ncvgt(fileid,source_gparameters_data_id,nc_corner,nc_edge,sou
     &rce_gparameters_data,nc_stat)
      source_parameters_data_id=ncvid(fileid,'source_parameters_data',nc
     &_stat)
      call ncvinq(fileid,source_parameters_data_id,nc_dummy,nc_type,nc_r
     &ank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_params_data_ind_id)continue
      
      source_parameters_data =>mem_alloc_r1((1),(so_params_data_dim),'so
     &urce_parameters_data')
      nc_corner(1)=1
      nc_edge(1)=((so_params_data_dim)-(1)+1)
      call ncvgt(fileid,source_parameters_data_id,nc_corner,nc_edge,sour
     &ce_parameters_data,nc_stat)
      source_num_giparameters_id=ncvid(fileid,'source_num_giparameters',
     &nc_stat)
      call ncvinq(fileid,source_num_giparameters_id,nc_dummy,nc_type,nc_
     &rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_num_giparameters =>mem_alloc_i1((1),(so_grps),'source_num_g
     &iparameters')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_num_giparameters_id,nc_corner,nc_edge,sou
     &rce_num_giparameters,nc_stat)
      source_num_iparameters_id=ncvid(fileid,'source_num_iparameters',nc
     &_stat)
      call ncvinq(fileid,source_num_iparameters_id,nc_dummy,nc_type,nc_r
     &ank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_num_iparameters =>mem_alloc_i1((1),(so_grps),'source_num_ip
     &arameters')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_num_iparameters_id,nc_corner,nc_edge,sour
     &ce_num_iparameters,nc_stat)
      source_giparameters_list_id=ncvid(fileid,'source_giparameters_list
     &',nc_stat)
      call ncvinq(fileid,source_giparameters_list_id,nc_dummy,nc_type,nc
     &_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_giparams_ind_id)continue
      
      source_giparameters_list =>mem_alloc_i1((1),(so_giparams_list_dim)
     &,'source_giparameters_list')
      nc_corner(1)=1
      nc_edge(1)=((so_giparams_list_dim)-(1)+1)
      call ncvgt(fileid,source_giparameters_list_id,nc_corner,nc_edge,so
     &urce_giparameters_list,nc_stat)
      source_iparameters_list_id=ncvid(fileid,'source_iparameters_list',
     &nc_stat)
      call ncvinq(fileid,source_iparameters_list_id,nc_dummy,nc_type,nc_
     &rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_iparams_ind_id)continue
      
      source_iparameters_list =>mem_alloc_i1((1),(so_iparams_list_dim),'
     &source_iparameters_list')
      nc_corner(1)=1
      nc_edge(1)=((so_iparams_list_dim)-(1)+1)
      call ncvgt(fileid,source_iparameters_list_id,nc_corner,nc_edge,sou
     &rce_iparameters_list,nc_stat)
      source_giparameters_base_id=ncvid(fileid,'source_giparameters_base
     &',nc_stat)
      call ncvinq(fileid,source_giparameters_base_id,nc_dummy,nc_type,nc
     &_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_giparameters_base =>mem_alloc_i1((1),(so_grps),'source_gipa
     &rameters_base')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_giparameters_base_id,nc_corner,nc_edge,so
     &urce_giparameters_base,nc_stat)
      source_iparameters_base_id=ncvid(fileid,'source_iparameters_base',
     &nc_stat)
      call ncvinq(fileid,source_iparameters_base_id,nc_dummy,nc_type,nc_
     &rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_iparameters_base =>mem_alloc_i1((1),(so_grps),'source_ipara
     &meters_base')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_iparameters_base_id,nc_corner,nc_edge,sou
     &rce_iparameters_base,nc_stat)
      source_iparameters_data_base_id=ncvid(fileid,'source_iparameters_d
     &ata_base',nc_stat)
      call ncvinq(fileid,source_iparameters_data_base_id,nc_dummy,nc_typ
     &e,nc_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_iparameters_data_base =>mem_alloc_i1((1),(so_grps),'source_
     &iparameters_data_base')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_iparameters_data_base_id,nc_corner,nc_edg
     &e,source_iparameters_data_base,nc_stat)
      source_giparameters_data_id=ncvid(fileid,'source_giparameters_data
     &',nc_stat)
      call ncvinq(fileid,source_giparameters_data_id,nc_dummy,nc_type,nc
     &_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_giparams_ind_id)continue
      
      source_giparameters_data =>mem_alloc_i1((1),(so_giparams_list_dim)
     &,'source_giparameters_data')
      nc_corner(1)=1
      nc_edge(1)=((so_giparams_list_dim)-(1)+1)
      call ncvgt(fileid,source_giparameters_data_id,nc_corner,nc_edge,so
     &urce_giparameters_data,nc_stat)
      source_iparameters_data_id=ncvid(fileid,'source_iparameters_data',
     &nc_stat)
      call ncvinq(fileid,source_iparameters_data_id,nc_dummy,nc_type,nc_
     &rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_iparams_data_ind_id)continue
      
      source_iparameters_data =>mem_alloc_i1((1),(so_iparams_data_dim),'
     &source_iparameters_data')
      nc_corner(1)=1
      nc_edge(1)=((so_iparams_data_dim)-(1)+1)
      call ncvgt(fileid,source_iparameters_data_id,nc_corner,nc_edge,sou
     &rce_iparameters_data,nc_stat)
      source_total_current_id=ncvid(fileid,'source_total_current',nc_sta
     &t)
      call ncvinq(fileid,source_total_current_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_total_current =>mem_alloc_r1((1),(so_grps),'source_total_cu
     &rrent')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_total_current_id,nc_corner,nc_edge,source
     &_total_current,nc_stat)
      source_weight_norm_id=ncvid(fileid,'source_weight_norm',nc_stat)
      call ncvinq(fileid,source_weight_norm_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_weight_norm =>mem_alloc_r1((1),(so_grps),'source_weight_nor
     &m')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_weight_norm_id,nc_corner,nc_edge,source_w
     &eight_norm,nc_stat)
      source_scale_factor_id=ncvid(fileid,'source_scale_factor',nc_stat)
      call ncvinq(fileid,source_scale_factor_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_grp_ind_id)continue
      
      source_scale_factor =>mem_alloc_r1((1),(so_grps),'source_scale_fac
     &tor')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,source_scale_factor_id,nc_corner,nc_edge,source_
     &scale_factor,nc_stat)
      source_segment_ptr_id=ncvid(fileid,'source_segment_ptr',nc_stat)
      call ncvinq(fileid,source_segment_ptr_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_seg_ind_id)continue
      
      source_segment_ptr =>mem_alloc_i1((1),(so_seg_tot),'source_segment
     &_ptr')
      nc_corner(1)=1
      nc_edge(1)=((so_seg_tot)-(1)+1)
      call ncvgt(fileid,source_segment_ptr_id,nc_corner,nc_edge,source_s
     &egment_ptr,nc_stat)
      source_current_id=ncvid(fileid,'source_current',nc_stat)
      call ncvinq(fileid,source_current_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_seg_ind_id)continue
      
      source_current =>mem_alloc_r1((1),(so_seg_tot),'source_current')
      nc_corner(1)=1
      nc_edge(1)=((so_seg_tot)-(1)+1)
      call ncvgt(fileid,source_current_id,nc_corner,nc_edge,source_curre
     &nt,nc_stat)
      source_segment_rel_wt_id=ncvid(fileid,'source_segment_rel_wt',nc_s
     &tat)
      call ncvinq(fileid,source_segment_rel_wt_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_seg_ind_id)continue
      
      source_segment_rel_wt =>mem_alloc_r1((1),(so_seg_tot),'source_segm
     &ent_rel_wt')
      nc_corner(1)=1
      nc_edge(1)=((so_seg_tot)-(1)+1)
      call ncvgt(fileid,source_segment_rel_wt_id,nc_corner,nc_edge,sourc
     &e_segment_rel_wt,nc_stat)
      source_segment_prob_alias_id=ncvid(fileid,'source_segment_prob_ali
     &as',nc_stat)
      call ncvinq(fileid,source_segment_prob_alias_id,nc_dummy,nc_type,n
     &c_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_seg_ind_id)continue
      
      source_segment_prob_alias =>mem_alloc_r1((1),(so_seg_tot),'source_
     &segment_prob_alias')
      nc_corner(1)=1
      nc_edge(1)=((so_seg_tot)-(1)+1)
      call ncvgt(fileid,source_segment_prob_alias_id,nc_corner,nc_edge,s
     &ource_segment_prob_alias,nc_stat)
      source_segment_ptr_alias_id=ncvid(fileid,'source_segment_ptr_alias
     &',nc_stat)
      call ncvinq(fileid,source_segment_ptr_alias_id,nc_dummy,nc_type,nc
     &_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.source_seg_ind_id)continue
      
      source_segment_ptr_alias =>mem_alloc_i1((1),(so_seg_tot),'source_s
     &egment_ptr_alias')
      nc_corner(1)=1
      nc_edge(1)=((so_seg_tot)-(1)+1)
      call ncvgt(fileid,source_segment_ptr_alias_id,nc_corner,nc_edge,so
     &urce_segment_ptr_alias,nc_stat)
      
      call ncclos(fileid,nc_stat)
      return
      end
      subroutine set_background_sources
      
      use pr_mod
      
      use bk_mod
      
      use so_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer jr,update
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
      
      
      if(pr_bkrc_num.GT.0)then
      source_base_ptr =>mem_realloc_i1(source_base_ptr,(1),so_grps,so_gr
     &ps+pr_bkrc_num,'source_base_ptr')
      source_num_segments =>mem_realloc_i1(source_num_segments,(1),so_gr
     &ps,so_grps+pr_bkrc_num,'source_num_segments')
      source_type =>mem_realloc_i1(source_type,(1),so_grps,so_grps+pr_bk
     &rc_num,'source_type')
      source_geometry =>mem_realloc_i1(source_geometry,(1),so_grps,so_gr
     &ps+pr_bkrc_num,'source_geometry')
      source_num_flights =>mem_realloc_i1(source_num_flights,(1),so_grps
     &,so_grps+pr_bkrc_num,'source_num_flights')
      source_num_checkpoints =>mem_realloc_i1(source_num_checkpoints,(1)
     &,so_grps,so_grps+pr_bkrc_num,'source_num_checkpoints')
      source_species =>mem_realloc_i1(source_species,(1),so_grps,so_grps
     &+pr_bkrc_num,'source_species')
      source_root_species =>mem_realloc_i1(source_root_species,(1),so_gr
     &ps,so_grps+pr_bkrc_num,'source_root_species')
      source_time_variation =>mem_realloc_i1(source_time_variation,(1),s
     &o_grps,so_grps+pr_bkrc_num,'source_time_variation')
      source_total_current =>mem_realloc_r1(source_total_current,(1),so_
     &grps,so_grps+pr_bkrc_num,'source_total_current')
      source_weight_norm =>mem_realloc_r1(source_weight_norm,(1),so_grps
     &,so_grps+pr_bkrc_num,'source_weight_norm')
      source_scale_factor =>mem_realloc_r1(source_scale_factor,(1),so_gr
     &ps,so_grps+pr_bkrc_num,'source_scale_factor')
      update=0
      do jr=1,pr_bkrc_num
      so_grps=so_grps+1
      source_base_ptr(so_grps)=so_seg_tot+1
      source_type(so_grps)=3
      source_species(so_grps)=problem_bkrc_products(1,jr)
      source_root_species(so_grps)=problem_background_sp(problem_bkrc_re
     &agents(2,jr))
      if(so_time_dependent.EQ.1)then
      if(so_time_final.GT.so_time_initial)continue
      source_time_variation(so_grps)=1
      else
      source_time_variation(so_grps)=0
      end if
      source_num_segments(so_grps)=0
      source_geometry(so_grps)=3
      source_num_flights(so_grps)=100
      source_num_checkpoints(so_grps)=0
      source_scale_factor(so_grps)=(1.0_DOUBLE)
      call set_background_rate(jr,so_grps,update)
      source_num_segments(so_grps)=so_seg_tot-source_base_ptr(so_grps)+1
      end do
      end if
      return
      end
      
      subroutine set_background_rate(jr,grp,update)
      
      use pr_mod
      
      use bk_mod
      
      use so_mod
      
      use gi_mod
      
      use zn_mod
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer jr,update
      integer grp
      integer zone
      REAL(kind=DOUBLE)rate
      integer species_bk_rg2
      integer test_bk_rg2
      REAL(kind=DOUBLE)time_bk_rg2,weight_bk_rg2,velocity_bk_rg2(3)
      REAL(kind=DOUBLE)pos_bk_rg2(3)
      integer cell_bk_rg2,zone_bk_rg2,surface_bk_rg2,cell_next_bk_rg2,zo
     &ne_next_bk_rg2,sector_bk_rg2,sector_next_bk_rg2
      integer type_bk_rg2,author_bk_rg2
      REAL(kind=DOUBLE)pos_zone_loc(3)
      integer cell_zone_loc,zone_zone_loc,surface_zone_loc,cell_next_zon
     &e_loc,zone_next_zone_loc,sector_zone_loc,sector_next_zone_loc
      integer ran_index_dummy_rand
      REAL(kind=DOUBLE)ran_array_dummy_rand(0:100-1)
      external find_rate
      REAL(kind=DOUBLE)find_rate
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
      
      
      external locate_point,check_location
      integer locate_point
      logical check_location
      REAL(kind=DOUBLE)random
      external random
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      species_bk_rg2=string_lookup(species_sy(problem_background_sp(prob
     &lem_bkrc_reagents(2,jr))),species_sy,sp_num)
      test_bk_rg2=problem_species_test(species_bk_rg2)
      if(species_m(species_bk_rg2).EQ.0)then
      type_bk_rg2=0
      else if(species_z(species_bk_rg2).EQ.0)then
      type_bk_rg2=1
      author_bk_rg2=source_type(grp)
      else
      type_bk_rg2=2
      author_bk_rg2=source_type(grp)
      end if
      weight_bk_rg2=(1.0_DOUBLE)
      source_total_current(grp)=(0.0_DOUBLE)
      do zone=1,zn_num
      if(zone_type(zone).EQ.2)then
      pos_zone_loc(1)=zone_center(1,zone)
      pos_zone_loc(2)=zone_center(2,zone)
      pos_zone_loc(3)=zone_center(3,zone)
      
      cell_zone_loc=locate_point(pos_zone_loc,zone_zone_loc)
      surface_zone_loc=0
      cell_next_zone_loc=0
      zone_next_zone_loc=0
      sector_zone_loc=0
      sector_next_zone_loc=0
      if(zone_zone_loc.EQ.zone)continue
      pos_bk_rg2(1)=pos_zone_loc(1)
      pos_bk_rg2(2)=pos_zone_loc(2)
      pos_bk_rg2(3)=pos_zone_loc(3)
      
      cell_bk_rg2=cell_zone_loc
      zone_bk_rg2=zone_zone_loc
      surface_bk_rg2=surface_zone_loc
      cell_next_bk_rg2=cell_next_zone_loc
      zone_next_bk_rg2=zone_next_zone_loc
      sector_bk_rg2=sector_zone_loc
      sector_next_bk_rg2=sector_next_zone_loc
      if((geometry_symmetry.EQ.2.OR.geometry_symmetry.EQ.5.OR.geometry_s
     &ymmetry.EQ.6).AND.(background_coords.EQ.2).AND.(pos_zone_loc(1)**2
     &+pos_zone_loc(2)**2.GT.(0.0_DOUBLE)))then
      velocity_bk_rg2(1)=(background_v(1,problem_bkrc_reagents(2,jr),zon
     &e_pointer(zone))*pos_zone_loc(1)-background_v(2,problem_bkrc_reage
     &nts(2,jr),zone_pointer(zone))*pos_zone_loc(2))/sqrt(pos_zone_loc(1
     &)**2+pos_zone_loc(2)**2)
      velocity_bk_rg2(2)=(background_v(1,problem_bkrc_reagents(2,jr),zon
     &e_pointer(zone))*pos_zone_loc(2)+background_v(2,problem_bkrc_reage
     &nts(2,jr),zone_pointer(zone))*pos_zone_loc(1))/sqrt(pos_zone_loc(1
     &)**2+pos_zone_loc(2)**2)
      velocity_bk_rg2(3)=background_v(3,problem_bkrc_reagents(2,jr),zone
     &_pointer(zone))
      else
      if(.NOT.((geometry_symmetry.EQ.0).AND.(background_coords.EQ.2)))co
     &ntinue
      velocity_bk_rg2(1)=background_v(1,problem_bkrc_reagents(2,jr),zone
     &_pointer(zone))
      velocity_bk_rg2(2)=background_v(2,problem_bkrc_reagents(2,jr),zone
     &_pointer(zone))
      velocity_bk_rg2(3)=background_v(3,problem_bkrc_reagents(2,jr),zone
     &_pointer(zone))
      
      end if
      rate=background_n(problem_bkrc_reagents(2,jr),zone_pointer(zone))*
     &find_rate(species_bk_rg2,test_bk_rg2,time_bk_rg2,weight_bk_rg2,pos
     &_bk_rg2(1),cell_bk_rg2,zone_bk_rg2,surface_bk_rg2,cell_next_bk_rg2
     &,zone_next_bk_rg2,sector_bk_rg2,sector_next_bk_rg2,velocity_bk_rg2
     &(1),type_bk_rg2,author_bk_rg2,problem_bkrc_reagents(1,jr),problem_
     &background_reaction(jr),ran_index_dummy_rand,ran_array_dummy_rand(
     &0))*zone_volume(zone)
      if(rate.GT.(0.0_DOUBLE))then
      so_seg_tot=so_seg_tot+1
      if(update.EQ.1)then
      if(source_segment_ptr(so_seg_tot).EQ.zone)continue
      else
      if(mod(((so_seg_tot)-(1)+1),100).EQ.1)then
      source_segment_ptr =>mem_realloc_i1(source_segment_ptr,(1),(((int(
     &((((so_seg_tot)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((so_
     &seg_tot)-(1)+1))+100-1)/100)*100)+(1)-1),'source_segment_ptr')
      end if
      if(mod(((so_seg_tot)-(1)+1),100).EQ.1)then
      source_current =>mem_realloc_r1(source_current,(1),(((int(((((so_s
     &eg_tot)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((so_seg_tot)
     &-(1)+1))+100-1)/100)*100)+(1)-1),'source_current')
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
      source_segment_ptr(so_seg_tot)=zone
      end if
      if(so_time_dependent.EQ.1)then
      if(so_time_final.GT.so_time_initial)continue
      rate=rate*((so_time_final-so_time_initial))
      end if
      source_current(so_seg_tot)=rate
      source_total_current(grp)=source_total_current(grp)+(rate)
      end if
      end if
      end do
      return
      end
      subroutine set_uedge_plate_sources(nx,ny,nxpt,ixlb,ixrb,ns,uedge_s
     &pecies,rm,zm,ni1,vr1,vz1,vphi1,te1,ti1,flux1,ni2,vr2,vz2,vphi2,te2
     &,ti2,flux2,flux1_present,flux2_present,group_map,update)
      
      use so_mod
      
      use sp_mod
      
      use sc_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer nx,ny,nxpt,ns,update
      integer ixlb(2),ixrb(2),flux1_present(2),flux2_present(2),group_ma
     &p(2,11,2)
      REAL(kind=DOUBLE)rm(150,150,0:4),zm(150,150,0:4),ni1(150,11,2),vr1
     &(150,11,2),vz1(150,11,2),vphi1(150,11,2),ti1(150,2),te1(150,2),flu
     &x1(150,11,2),ni2(150,11,2),vr2(150,11,2),vz2(150,11,2),vphi2(150,1
     &1,2),ti2(150,2),te2(150,2),flux2(150,11,2)
      character*8 uedge_species(11)
      integer i_seg,j1,j2,js,is_seg,isp,plasma_sc,ixpt,iparam
      REAL(kind=DOUBLE)ne
      integer grp
      integer sector
      integer plasma_sc_ptr
      REAL(kind=DOUBLE)x1(3)
      REAL(kind=DOUBLE)x2(3)
      REAL(kind=DOUBLE)v_isp(3)
      REAL(kind=DOUBLE)v_bdy(3)
      external lookup_sector,phi_sheath
      integer lookup_sector
      REAL(kind=DOUBLE)phi_sheath
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
      do ixpt=1,nxpt
      i_seg=0
      if(flux1_present(ixpt).EQ.1)then
      if((nxpt.EQ.2).AND.(ixpt.EQ.2))then
      j1=ixlb(ixpt)
      else
      j1=ixlb(ixpt)+1
      end if
      do j2=ny,1,-1
      i_seg=i_seg+1
      x1(1)=rm(j1,j2,3)
      x1(2)=(0.0_DOUBLE)
      x1(3)=zm(j1,j2,3)
      x2(1)=rm(j1,j2,1)
      x2(2)=(0.0_DOUBLE)
      x2(3)=zm(j1,j2,1)
      sector=lookup_sector(x1,x2)
      if((sector.GT.0.AND.sector.LE.nsectors))then
      plasma_sc_ptr=sector_type_pointer(2,sector)
      if((plasma_sc_ptr.GT.0.AND.plasma_sc_ptr.LE.sc_plasma_num))then
      plasma_sc=1
      else
      plasma_sc=0
      end if
      else
      plasma_sc=0
      end if
      if(plasma_sc.EQ.0)then
      sector=lookup_sector(x2,x1)
      if((sector.GT.0.AND.sector.LE.nsectors))continue
      plasma_sc_ptr=sector_type_pointer(2,sector)
      if((plasma_sc_ptr.GT.0.AND.plasma_sc_ptr.LE.sc_plasma_num))continu
     &e
      plasma_sc=1
      end if
      v_bdy(1)=(0.0_DOUBLE)
      v_bdy(2)=(0.0_DOUBLE)
      v_bdy(3)=(0.0_DOUBLE)
      ne=(0.0_DOUBLE)
      do js=1,ns
      if(group_map(1,js,ixpt).NE.1000000000)then
      grp=group_map(1,js,ixpt)
      is_seg=source_base_ptr(grp)+i_seg-1
      source_current(is_seg)=flux1(j2,js,ixpt)
      source_total_current(grp)=source_total_current(grp)+(source_curren
     &t(is_seg))
      if(update.EQ.0)then
      source_segment_ptr(is_seg)=sector
      else
      if(source_segment_ptr(is_seg).EQ.sector)continue
      end if
      end if
      isp=string_lookup(uedge_species(js),species_sy,sp_num)
      if((isp.GT.0.AND.isp.LE.sp_num))then
      v_isp(1)=vr1(j2,js,ixpt)
      v_isp(2)=vphi1(j2,js,ixpt)
      v_isp(3)=vz1(j2,js,ixpt)
      v_bdy(1)=v_bdy(1)+v_isp(1)*(species_z(isp)*ni1(j2,js,ixpt))
      v_bdy(2)=v_bdy(2)+v_isp(2)*(species_z(isp)*ni1(j2,js,ixpt))
      v_bdy(3)=v_bdy(3)+v_isp(3)*(species_z(isp)*ni1(j2,js,ixpt))
      
      ne=ne+(species_z(isp)*ni1(j2,js,ixpt))
      end if
      end do
      if(ne.GT.(0.0_DOUBLE))then
      v_bdy(1)=((1.0_DOUBLE)/ne)*v_bdy(1)
      v_bdy(2)=((1.0_DOUBLE)/ne)*v_bdy(2)
      v_bdy(3)=((1.0_DOUBLE)/ne)*v_bdy(3)
      
      end if
      if(source_num_parameters(grp).GT.0)continue
      if(i_seg.EQ.1)source_parameters_data_base(grp)=so_params_data_size
      so_params_data_size=so_params_data_size+(source_num_parameters(grp
     &))
      if(so_params_data_size.GT.so_params_data_dim)then
      source_parameters_data =>mem_realloc_r1(source_parameters_data,(1)
     &,so_params_data_dim,so_params_data_size,'source_parameters_data')
      so_params_data_dim=so_params_data_size
      end if
      do iparam=1,source_num_parameters(grp)
      if(source_parameters_list(source_parameters_base(grp)+iparam).EQ.5
     &)then
      if(sqrt((v_bdy(1)**2+v_bdy(2)**2+v_bdy(3)**2)).GT.(0.0_DOUBLE).OR.
     &te1(j2,ixpt).GT.(0.0_DOUBLE))then
      source_parameters_data(source_parameters_data_base(grp)+(is_seg-so
     &urce_base_ptr(grp))*source_num_parameters(grp)+iparam)=(3.0_DOUBLE
     &)*ti1(j2,ixpt)+(0.5_DOUBLE)*te1(j2,ixpt)
      else
      source_parameters_data(source_parameters_data_base(grp)+(is_seg-so
     &urce_base_ptr(grp))*source_num_parameters(grp)+iparam)=(0.0_DOUBLE
     &)
      end if
      else if(source_parameters_list(source_parameters_base(grp)+iparam)
     &.EQ.7)then
      if(sqrt((v_bdy(1)**2+v_bdy(2)**2+v_bdy(3)**2)).GT.(0.0_DOUBLE).OR.
     &te1(j2,ixpt).GT.(0.0_DOUBLE))then
      source_parameters_data(source_parameters_data_base(grp)+(is_seg-so
     &urce_base_ptr(grp))*source_num_parameters(grp)+iparam)=te1(j2,ixpt
     &)*phi_sheath(v_bdy(1),ti1(j2,ixpt),te1(j2,ixpt))
      else
      source_parameters_data(source_parameters_data_base(grp)+(is_seg-so
     &urce_base_ptr(grp))*source_num_parameters(grp)+iparam)=(0.0_DOUBLE
     &)
      end if
      else if(source_parameters_list(source_parameters_base(grp)+iparam)
     &.EQ.6)then
      if(sqrt((v_bdy(1)**2+v_bdy(2)**2+v_bdy(3)**2)).GT.(0.0_DOUBLE).OR.
     &te1(j2,ixpt).GT.(0.0_DOUBLE))then
      source_parameters_data(source_parameters_data_base(grp)+(is_seg-so
     &urce_base_ptr(grp))*source_num_parameters(grp)+iparam)=(0.0_DOUBLE
     &)
      else
      source_parameters_data(source_parameters_data_base(grp)+(is_seg-so
     &urce_base_ptr(grp))*source_num_parameters(grp)+iparam)=(1.0_DOUBLE
     &)
      end if
      end if
      end do
      end do
      end if
      i_seg=0
      if(flux2_present(ixpt).EQ.1)then
      if((nxpt.EQ.2).AND.(ixpt.EQ.1))then
      j1=ixrb(ixpt)+1
      else
      j1=ixrb(ixpt)
      end if
      do j2=1,ny
      i_seg=i_seg+1
      x1(1)=rm(j1,j2,4)
      x1(2)=(0.0_DOUBLE)
      x1(3)=zm(j1,j2,4)
      x2(1)=rm(j1,j2,2)
      x2(2)=(0.0_DOUBLE)
      x2(3)=zm(j1,j2,2)
      sector=lookup_sector(x1,x2)
      if((sector.GT.0.AND.sector.LE.nsectors))then
      plasma_sc_ptr=sector_type_pointer(2,sector)
      if((plasma_sc_ptr.GT.0.AND.plasma_sc_ptr.LE.sc_plasma_num))then
      plasma_sc=1
      else
      plasma_sc=0
      end if
      else
      plasma_sc=0
      end if
      if(plasma_sc.EQ.0)then
      sector=lookup_sector(x2,x1)
      if((sector.GT.0.AND.sector.LE.nsectors))continue
      plasma_sc_ptr=sector_type_pointer(2,sector)
      if((plasma_sc_ptr.GT.0.AND.plasma_sc_ptr.LE.sc_plasma_num))continu
     &e
      plasma_sc=1
      end if
      v_bdy(1)=(0.0_DOUBLE)
      v_bdy(2)=(0.0_DOUBLE)
      v_bdy(3)=(0.0_DOUBLE)
      ne=(0.0_DOUBLE)
      do js=1,ns
      if(group_map(2,js,ixpt).NE.1000000000)then
      grp=group_map(2,js,ixpt)
      is_seg=source_base_ptr(grp)+i_seg-1
      source_current(is_seg)=flux2(j2,js,ixpt)
      source_total_current(grp)=source_total_current(grp)+(source_curren
     &t(is_seg))
      if(update.EQ.0)then
      source_segment_ptr(is_seg)=sector
      else
      if(source_segment_ptr(is_seg).EQ.sector)continue
      end if
      end if
      isp=string_lookup(uedge_species(js),species_sy,sp_num)
      if((isp.GT.0.AND.isp.LE.sp_num))then
      v_isp(1)=vr2(j2,js,ixpt)
      v_isp(2)=vphi2(j2,js,ixpt)
      v_isp(3)=vz2(j2,js,ixpt)
      v_bdy(1)=v_bdy(1)+v_isp(1)*(species_z(isp)*ni2(j2,js,ixpt))
      v_bdy(2)=v_bdy(2)+v_isp(2)*(species_z(isp)*ni2(j2,js,ixpt))
      v_bdy(3)=v_bdy(3)+v_isp(3)*(species_z(isp)*ni2(j2,js,ixpt))
      
      ne=ne+(species_z(isp)*ni2(j2,js,ixpt))
      end if
      end do
      if(ne.GT.(0.0_DOUBLE))then
      v_bdy(1)=((1.0_DOUBLE)/ne)*v_bdy(1)
      v_bdy(2)=((1.0_DOUBLE)/ne)*v_bdy(2)
      v_bdy(3)=((1.0_DOUBLE)/ne)*v_bdy(3)
      
      end if
      if(source_num_parameters(grp).GT.0)continue
      if(i_seg.EQ.1)source_parameters_data_base(grp)=so_params_data_size
      so_params_data_size=so_params_data_size+(source_num_parameters(grp
     &))
      if(so_params_data_size.GT.so_params_data_dim)then
      source_parameters_data =>mem_realloc_r1(source_parameters_data,(1)
     &,so_params_data_dim,so_params_data_size,'source_parameters_data')
      so_params_data_dim=so_params_data_size
      end if
      do iparam=1,source_num_parameters(grp)
      if(source_parameters_list(source_parameters_base(grp)+iparam).EQ.5
     &)then
      if(sqrt((v_bdy(1)**2+v_bdy(2)**2+v_bdy(3)**2)).GT.(0.0_DOUBLE).OR.
     &te2(j2,ixpt).GT.(0.0_DOUBLE))then
      source_parameters_data(source_parameters_data_base(grp)+(is_seg-so
     &urce_base_ptr(grp))*source_num_parameters(grp)+iparam)=(3.0_DOUBLE
     &)*ti2(j2,ixpt)+(0.5_DOUBLE)*te2(j2,ixpt)
      else
      source_parameters_data(source_parameters_data_base(grp)+(is_seg-so
     &urce_base_ptr(grp))*source_num_parameters(grp)+iparam)=(0.0_DOUBLE
     &)
      end if
      else if(source_parameters_list(source_parameters_base(grp)+iparam)
     &.EQ.7)then
      if(sqrt((v_bdy(1)**2+v_bdy(2)**2+v_bdy(3)**2)).GT.(0.0_DOUBLE).OR.
     &te2(j2,ixpt).GT.(0.0_DOUBLE))then
      source_parameters_data(source_parameters_data_base(grp)+(is_seg-so
     &urce_base_ptr(grp))*source_num_parameters(grp)+iparam)=te2(j2,ixpt
     &)*phi_sheath(v_bdy(1),ti2(j2,ixpt),te2(j2,ixpt))
      else
      source_parameters_data(source_parameters_data_base(grp)+(is_seg-so
     &urce_base_ptr(grp))*source_num_parameters(grp)+iparam)=(0.0_DOUBLE
     &)
      end if
      else if(source_parameters_list(source_parameters_base(grp)+iparam)
     &.EQ.6)then
      if(sqrt((v_bdy(1)**2+v_bdy(2)**2+v_bdy(3)**2)).GT.(0.0_DOUBLE).OR.
     &te2(j2,ixpt).GT.(0.0_DOUBLE))then
      source_parameters_data(source_parameters_data_base(grp)+(is_seg-so
     &urce_base_ptr(grp))*source_num_parameters(grp)+iparam)=(0.0_DOUBLE
     &)
      else
      source_parameters_data(source_parameters_data_base(grp)+(is_seg-so
     &urce_base_ptr(grp))*source_num_parameters(grp)+iparam)=(1.0_DOUBLE
     &)
      end if
      end if
      end do
      end do
      end if
      end do
      return
      end
      function phi_sheath(vi,ti,te)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
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
      
      
      REAL(kind=DOUBLE)phi_sheath
      REAL(kind=DOUBLE)vi(3)
      REAL(kind=DOUBLE)ti,te
      REAL(kind=DOUBLE)ce
      ce=sqrt(te/(9.1093897e-31_DOUBLE))
      phi_sheath=-log(sqrt((2.0_DOUBLE)*atan2((0.0_DOUBLE),-(1.0_DOUBLE)
     &))*sqrt((vi(1)**2+vi(2)**2+vi(3)**2))/ce)
      return
      end
      subroutine nc_read_snapshot(have_snapshot_file)
      
      use sn_mod
      
      use rf_mod
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer have_snapshot_file
      integer fileid
      character*96 tempfile
      integer nc_dims(5),nc_corner(5),nc_edge(5),nc_stat,nc_size,nc_rank
     &,nc_attr,nc_type
      character*128 nc_dummy
      external ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf
     &_inq_format
      integer ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf_
     &inq_format
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
      
      
      integer sn_particles_dim_id
      integer snapshot_pdf_ind_id
      integer sn_pt_float_ind_id
      integer sn_pt_int_ind_id
      integer sn_seed_decimal_ind_id
      integer sn_number_particles_id
      integer sn_seed_decimal_id
      integer sn_particles_float_id
      integer sn_particles_int_id
      
      have_snapshot_file=0
      if((mpi_rank.EQ.mpi_degas2_root))then
      tempfile=filenames_array(23)
      if(tempfile.NE.'undefined')then
      call ncpopt(0)
      fileid=ncopn(tempfile,0,nc_stat)
      if(nc_stat.EQ.0)then
      have_snapshot_file=1
      call ncpopt(2+1)
      sn_particles_dim_id=ncvid(fileid,'sn_particles_dim',nc_stat)
      call ncvinq(fileid,sn_particles_dim_id,nc_dummy,nc_type,nc_rank,nc
     &_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,sn_particles_dim_id,nc_corner,nc_edge,sn_particl
     &es_dim,nc_stat)
      snapshot_pdf_ind_id=ncdid(fileid,'snapshot_pdf_ind',nc_stat)
      call ncdinq(fileid,snapshot_pdf_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((sn_particles_dim)-(1)+1))continue
      sn_pt_float_ind_id=ncdid(fileid,'sn_pt_float_ind',nc_stat)
      call ncdinq(fileid,sn_pt_float_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((8)-(1)+1))continue
      sn_pt_int_ind_id=ncdid(fileid,'sn_pt_int_ind',nc_stat)
      call ncdinq(fileid,sn_pt_int_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((11)-(1)+1))continue
      sn_seed_decimal_ind_id=ncdid(fileid,'sn_seed_decimal_ind',nc_stat)
      call ncdinq(fileid,sn_seed_decimal_ind_id,nc_dummy,nc_size,nc_stat
     &)
      if(nc_size.EQ.((34)-(1)+1))continue
      sn_number_particles_id=ncvid(fileid,'sn_number_particles',nc_stat)
      call ncvinq(fileid,sn_number_particles_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,sn_number_particles_id,nc_corner,nc_edge,sn_numb
     &er_particles,nc_stat)
      sn_seed_decimal_id=ncvid(fileid,'sn_seed_decimal',nc_stat)
      call ncvinq(fileid,sn_seed_decimal_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.sn_seed_decimal_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((34)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,sn_seed_decimal_id,nc_corner,nc_ed
     &ge,sn_seed_decimal)
      sn_particles_float_id=ncvid(fileid,'sn_particles_float',nc_stat)
      call ncvinq(fileid,sn_particles_float_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.sn_pt_float_ind_id)continue
      if(nc_dims(2).EQ.snapshot_pdf_ind_id)continue
      
      sn_particles_float =>mem_alloc_r2((1),(8),(1),(sn_particles_dim),'
     &sn_particles_float')
      nc_corner(1)=1
      nc_edge(1)=((8)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((sn_particles_dim)-(1)+1)
      call ncvgt(fileid,sn_particles_float_id,nc_corner,nc_edge,sn_parti
     &cles_float,nc_stat)
      sn_particles_int_id=ncvid(fileid,'sn_particles_int',nc_stat)
      call ncvinq(fileid,sn_particles_int_id,nc_dummy,nc_type,nc_rank,nc
     &_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.sn_pt_int_ind_id)continue
      if(nc_dims(2).EQ.snapshot_pdf_ind_id)continue
      
      sn_particles_int =>mem_alloc_i2((1),(11),(1),(sn_particles_dim),'s
     &n_particles_int')
      nc_corner(1)=1
      nc_edge(1)=((11)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((sn_particles_dim)-(1)+1)
      call ncvgt(fileid,sn_particles_int_id,nc_corner,nc_edge,sn_particl
     &es_int,nc_stat)
      
      call ncclos(fileid,nc_stat)
      end if
      end if
      end if
      call MPI_bcast(have_snapshot_file,1,MPI_INTEGER,mpi_degas2_root,co
     &mm_world_dup,mpi_err)
      if(have_snapshot_file.EQ.1)then
      call MPI_bcast(sn_particles_dim,1,MPI_INTEGER,mpi_degas2_root,comm
     &_world_dup,mpi_err)
      call MPI_bcast(sn_number_particles,1,MPI_INTEGER,mpi_degas2_root,c
     &omm_world_dup,mpi_err)
      call MPI_bcast(sn_seed_decimal,(((34)-(1)+1)),MPI_CHARACTER,mpi_de
     &gas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      sn_particles_float =>mem_alloc_r2((1),(8),(1),(sn_particles_dim),'
     &sn_particles_float')
      endif
      call MPI_bcast(sn_particles_float,(((8)-(1)+1)*((sn_particles_dim)
     &-(1)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_e
     &rr)
      if(mpi_rank.NE.mpi_degas2_root)then
      sn_particles_int =>mem_alloc_i2((1),(11),(1),(sn_particles_dim),'s
     &n_particles_int')
      endif
      call MPI_bcast(sn_particles_int,(((11)-(1)+1)*((sn_particles_dim)-
     &(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      
      end if
      return
      end
      subroutine set_snapshot_source(standalone)
      
      use so_mod
      
      use sn_mod
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer standalone
      integer have_snapshot_file,new_seg_tot,ip,iparam,igrp,iseg,snapsho
     &t_reset
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
      
      
      call nc_read_snapshot(have_snapshot_file)
      if((have_snapshot_file.EQ.0).OR.(sn_number_particles.LE.0))return
      snapshot_reset=0
      do igrp=1,so_grps
      if(source_type(igrp).EQ.5)then
      if(igrp.EQ.so_grps)continue
      snapshot_reset=1
      end if
      end do
      if(snapshot_reset.EQ.0)then
      source_base_ptr =>mem_realloc_i1(source_base_ptr,(1),so_grps,so_gr
     &ps+1,'source_base_ptr')
      source_num_segments =>mem_realloc_i1(source_num_segments,(1),so_gr
     &ps,so_grps+1,'source_num_segments')
      source_type =>mem_realloc_i1(source_type,(1),so_grps,so_grps+1,'so
     &urce_type')
      source_geometry =>mem_realloc_i1(source_geometry,(1),so_grps,so_gr
     &ps+1,'source_geometry')
      source_num_flights =>mem_realloc_i1(source_num_flights,(1),so_grps
     &,so_grps+1,'source_num_flights')
      source_num_checkpoints =>mem_realloc_i1(source_num_checkpoints,(1)
     &,so_grps,so_grps+1,'source_num_checkpoints')
      source_species =>mem_realloc_i1(source_species,(1),so_grps,so_grps
     &+1,'source_species')
      source_root_species =>mem_realloc_i1(source_root_species,(1),so_gr
     &ps,so_grps+1,'source_root_species')
      source_time_variation =>mem_realloc_i1(source_time_variation,(1),s
     &o_grps,so_grps+1,'source_time_variation')
      source_num_gparameters =>mem_realloc_i1(source_num_gparameters,(1)
     &,so_grps,so_grps+1,'source_num_gparameters')
      source_num_parameters =>mem_realloc_i1(source_num_parameters,(1),s
     &o_grps,so_grps+1,'source_num_parameters')
      source_gparameters_base =>mem_realloc_i1(source_gparameters_base,(
     &1),so_grps,so_grps+1,'source_gparameters_base')
      source_parameters_base =>mem_realloc_i1(source_parameters_base,(1)
     &,so_grps,so_grps+1,'source_parameters_base')
      source_parameters_data_base =>mem_realloc_i1(source_parameters_dat
     &a_base,(1),so_grps,so_grps+1,'source_parameters_data_base')
      source_num_giparameters =>mem_realloc_i1(source_num_giparameters,(
     &1),so_grps,so_grps+1,'source_num_giparameters')
      source_num_iparameters =>mem_realloc_i1(source_num_iparameters,(1)
     &,so_grps,so_grps+1,'source_num_iparameters')
      source_giparameters_base =>mem_realloc_i1(source_giparameters_base
     &,(1),so_grps,so_grps+1,'source_giparameters_base')
      source_iparameters_base =>mem_realloc_i1(source_iparameters_base,(
     &1),so_grps,so_grps+1,'source_iparameters_base')
      source_iparameters_data_base =>mem_realloc_i1(source_iparameters_d
     &ata_base,(1),so_grps,so_grps+1,'source_iparameters_data_base')
      source_total_current =>mem_realloc_r1(source_total_current,(1),so_
     &grps,so_grps+1,'source_total_current')
      source_weight_norm =>mem_realloc_r1(source_weight_norm,(1),so_grps
     &,so_grps+1,'source_weight_norm')
      source_scale_factor =>mem_realloc_r1(source_scale_factor,(1),so_gr
     &ps,so_grps+1,'source_scale_factor')
      so_grps=so_grps+1
      source_base_ptr(so_grps)=so_seg_tot+1
      source_type(so_grps)=5
      source_time_variation(so_grps)=0
      source_geometry(so_grps)=3
      source_num_flights(so_grps)=100
      source_num_checkpoints(so_grps)=0
      source_scale_factor(so_grps)=(1.0_DOUBLE)
      source_num_gparameters(so_grps)=0
      source_num_parameters(so_grps)=7
      source_gparameters_base(so_grps)=so_gparams_list_size
      source_parameters_base(so_grps)=so_params_list_size
      source_parameters_data_base(so_grps)=so_params_data_size
      source_num_giparameters(so_grps)=0
      source_num_iparameters(so_grps)=6
      source_giparameters_base(so_grps)=so_giparams_list_size
      source_iparameters_base(so_grps)=so_iparams_list_size
      source_iparameters_data_base(so_grps)=so_iparams_data_size
      so_params_list_size=so_params_list_size+(source_num_parameters(so_
     &grps))
      if(so_params_list_size.GT.so_params_list_dim)then
      source_parameters_list =>mem_realloc_i1(source_parameters_list,(1)
     &,so_params_list_dim,so_params_list_size,'source_parameters_list')
      so_params_list_dim=so_params_list_size
      end if
      source_parameters_list(source_parameters_base(so_grps)+1)=8
      source_parameters_list(source_parameters_base(so_grps)+2)=9
      source_parameters_list(source_parameters_base(so_grps)+3)=10
      source_parameters_list(source_parameters_base(so_grps)+4)=11
      source_parameters_list(source_parameters_base(so_grps)+5)=12
      source_parameters_list(source_parameters_base(so_grps)+6)=13
      source_parameters_list(source_parameters_base(so_grps)+7)=14
      so_iparams_list_size=so_iparams_list_size+(source_num_iparameters(
     &so_grps))
      if(so_iparams_list_size.GT.so_iparams_list_dim)then
      source_iparameters_list =>mem_realloc_i1(source_iparameters_list,(
     &1),so_iparams_list_dim,so_iparams_list_size,'source_iparameters_li
     &st')
      so_iparams_list_dim=so_iparams_list_size
      end if
      source_iparameters_list(source_iparameters_base(so_grps)+1)=1
      source_iparameters_list(source_iparameters_base(so_grps)+2)=2
      source_iparameters_list(source_iparameters_base(so_grps)+3)=3
      source_iparameters_list(source_iparameters_base(so_grps)+4)=4
      source_iparameters_list(source_iparameters_base(so_grps)+5)=5
      source_iparameters_list(source_iparameters_base(so_grps)+6)=6
      end if
      new_seg_tot=source_base_ptr(so_grps)-1+sn_number_particles
      if(standalone.EQ.1)then
      if((int((((so_seg_tot)-(1)+1)+100-1)/100)*100).NE.(int((((new_seg_
     &tot)-(1)+1)+100-1)/100)*100))then
      source_segment_ptr =>mem_realloc_i1(source_segment_ptr,(1),((int((
     &((so_seg_tot)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((new_seg_tot)
     &-(1)+1)+100-1)/100)*100)+(1)-1),'source_segment_ptr')
      end if
      if((int((((so_seg_tot)-(1)+1)+100-1)/100)*100).NE.(int((((new_seg_
     &tot)-(1)+1)+100-1)/100)*100))then
      source_current =>mem_realloc_r1(source_current,(1),((int((((so_seg
     &_tot)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((new_seg_tot)-(1)+1)+
     &100-1)/100)*100)+(1)-1),'source_current')
      end if
      if((int((((so_seg_tot)-(1)+1)+100-1)/100)*100).NE.(int((((new_seg_
     &tot)-(1)+1)+100-1)/100)*100))then
      source_segment_rel_wt =>mem_realloc_r1(source_segment_rel_wt,(1),(
     &(int((((so_seg_tot)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((new_se
     &g_tot)-(1)+1)+100-1)/100)*100)+(1)-1),'source_segment_rel_wt')
      end if
      if((int((((so_seg_tot)-(1)+1)+100-1)/100)*100).NE.(int((((new_seg_
     &tot)-(1)+1)+100-1)/100)*100))then
      source_segment_prob_alias =>mem_realloc_r1(source_segment_prob_ali
     &as,(1),((int((((so_seg_tot)-(1)+1)+100-1)/100)*100)+(1)-1),((int((
     &((new_seg_tot)-(1)+1)+100-1)/100)*100)+(1)-1),'source_segment_prob
     &_alias')
      end if
      if((int((((so_seg_tot)-(1)+1)+100-1)/100)*100).NE.(int((((new_seg_
     &tot)-(1)+1)+100-1)/100)*100))then
      source_segment_ptr_alias =>mem_realloc_i1(source_segment_ptr_alias
     &,(1),((int((((so_seg_tot)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((
     &new_seg_tot)-(1)+1)+100-1)/100)*100)+(1)-1),'source_segment_ptr_al
     &ias')
      end if
      else
      source_segment_ptr =>mem_realloc_i1(source_segment_ptr,(1),so_seg_
     &tot,new_seg_tot,'source_segment_ptr')
      source_current =>mem_realloc_r1(source_current,(1),so_seg_tot,new_
     &seg_tot,'source_current')
      source_segment_rel_wt =>mem_realloc_r1(source_segment_rel_wt,(1),s
     &o_seg_tot,new_seg_tot,'source_segment_rel_wt')
      source_segment_prob_alias =>mem_realloc_r1(source_segment_prob_ali
     &as,(1),so_seg_tot,new_seg_tot,'source_segment_prob_alias')
      source_segment_ptr_alias =>mem_realloc_i1(source_segment_ptr_alias
     &,(1),so_seg_tot,new_seg_tot,'source_segment_ptr_alias')
      end if
      so_params_data_size=source_parameters_data_base(so_grps)+source_nu
     &m_parameters(so_grps)*sn_number_particles
      if(so_params_data_size.GT.so_params_data_dim)then
      source_parameters_data =>mem_realloc_r1(source_parameters_data,(1)
     &,so_params_data_dim,so_params_data_size,'source_parameters_data')
      so_params_data_dim=so_params_data_size
      end if
      so_iparams_data_size=source_iparameters_data_base(so_grps)+source_
     &num_iparameters(so_grps)*sn_number_particles
      if(so_iparams_data_size.GT.so_iparams_data_dim)then
      source_iparameters_data =>mem_realloc_i1(source_iparameters_data,(
     &1),so_iparams_data_dim,so_iparams_data_size,'source_iparameters_da
     &ta')
      so_iparams_data_dim=so_iparams_data_size
      end if
      source_num_segments(so_grps)=sn_number_particles
      source_total_current(so_grps)=(0.0_DOUBLE)
      iseg=source_base_ptr(so_grps)-1
      do ip=1,sn_number_particles
      iseg=iseg+1
      source_segment_ptr(iseg)=ip
      source_current(iseg)=sn_particles_float(2,ip)
      source_total_current(so_grps)=source_total_current(so_grps)+(sn_pa
     &rticles_float(2,ip))
      do iparam=1,source_num_parameters(so_grps)
      if(source_parameters_list(source_parameters_base(so_grps)+iparam).
     &EQ.8)then
      source_parameters_data(source_parameters_data_base(so_grps)+(iseg-
     &source_base_ptr(so_grps))*source_num_parameters(so_grps)+iparam)=s
     &n_particles_float(1,ip)
      if((ip.EQ.1).AND.(mpi_rank.EQ.mpi_degas2_root))then
      if((abs(source_parameters_data(source_parameters_data_base(so_grps
     &)+(iseg-source_base_ptr(so_grps))*source_num_parameters(so_grps)+i
     &param)-so_time_initial))/(max(so_time_initial,(5*EPSILON((0.0_DOUB
     &LE))))).GT.(5*EPSILON((0.0_DOUBLE))))write(0,*)' Warning: snapshot
     & particle time does not match so_time_initial'
      end if
      else if(source_parameters_list(source_parameters_base(so_grps)+ipa
     &ram).EQ.9)then
      source_parameters_data(source_parameters_data_base(so_grps)+(iseg-
     &source_base_ptr(so_grps))*source_num_parameters(so_grps)+iparam)=s
     &n_particles_float(3,ip)
      else if(source_parameters_list(source_parameters_base(so_grps)+ipa
     &ram).EQ.10)then
      source_parameters_data(source_parameters_data_base(so_grps)+(iseg-
     &source_base_ptr(so_grps))*source_num_parameters(so_grps)+iparam)=s
     &n_particles_float(4,ip)
      else if(source_parameters_list(source_parameters_base(so_grps)+ipa
     &ram).EQ.11)then
      source_parameters_data(source_parameters_data_base(so_grps)+(iseg-
     &source_base_ptr(so_grps))*source_num_parameters(so_grps)+iparam)=s
     &n_particles_float(5,ip)
      else if(source_parameters_list(source_parameters_base(so_grps)+ipa
     &ram).EQ.12)then
      source_parameters_data(source_parameters_data_base(so_grps)+(iseg-
     &source_base_ptr(so_grps))*source_num_parameters(so_grps)+iparam)=s
     &n_particles_float(6,ip)
      else if(source_parameters_list(source_parameters_base(so_grps)+ipa
     &ram).EQ.13)then
      source_parameters_data(source_parameters_data_base(so_grps)+(iseg-
     &source_base_ptr(so_grps))*source_num_parameters(so_grps)+iparam)=s
     &n_particles_float(7,ip)
      else if(source_parameters_list(source_parameters_base(so_grps)+ipa
     &ram).EQ.14)then
      source_parameters_data(source_parameters_data_base(so_grps)+(iseg-
     &source_base_ptr(so_grps))*source_num_parameters(so_grps)+iparam)=s
     &n_particles_float(8,ip)
      else
      if(' Inconsistent number of snapshot parameters'.EQ.' ')continue
      end if
      end do
      do iparam=1,source_num_iparameters(so_grps)
      if(source_iparameters_list(source_iparameters_base(so_grps)+iparam
     &).EQ.1)then
      source_iparameters_data(source_iparameters_data_base(so_grps)+(ise
     &g-source_base_ptr(so_grps))*source_num_iparameters(so_grps)+iparam
     &)=sn_particles_int(1,ip)
      else if(source_iparameters_list(source_iparameters_base(so_grps)+i
     &param).EQ.2)then
      source_iparameters_data(source_iparameters_data_base(so_grps)+(ise
     &g-source_base_ptr(so_grps))*source_num_iparameters(so_grps)+iparam
     &)=sn_particles_int(2,ip)
      else if(source_iparameters_list(source_iparameters_base(so_grps)+i
     &param).EQ.3)then
      source_iparameters_data(source_iparameters_data_base(so_grps)+(ise
     &g-source_base_ptr(so_grps))*source_num_iparameters(so_grps)+iparam
     &)=sn_particles_int(3,ip)
      else if(source_iparameters_list(source_iparameters_base(so_grps)+i
     &param).EQ.4)then
      source_iparameters_data(source_iparameters_data_base(so_grps)+(ise
     &g-source_base_ptr(so_grps))*source_num_iparameters(so_grps)+iparam
     &)=sn_particles_int(4,ip)
      else if(source_iparameters_list(source_iparameters_base(so_grps)+i
     &param).EQ.5)then
      source_iparameters_data(source_iparameters_data_base(so_grps)+(ise
     &g-source_base_ptr(so_grps))*source_num_iparameters(so_grps)+iparam
     &)=sn_particles_int(10,ip)
      else if(source_iparameters_list(source_iparameters_base(so_grps)+i
     &param).EQ.6)then
      source_iparameters_data(source_iparameters_data_base(so_grps)+(ise
     &g-source_base_ptr(so_grps))*source_num_iparameters(so_grps)+iparam
     &)=sn_particles_int(11,ip)
      end if
      end do
      end do
      if(iseg.EQ.new_seg_tot)continue
      so_seg_tot=new_seg_tot
      so_seed_decimal=sn_seed_decimal
      call mem_free_r2(sn_particles_float,(1),(8),(1),(sn_particles_dim)
     &,'sn_particles_float')
      call mem_free_i2(sn_particles_int,(1),(11),(1),(sn_particles_dim),
     &'sn_particles_int')
      return
      end
      subroutine setup_back_arrays(geom_modified)
      
      use rc_mod
      
      use zn_mod
      
      use bk_mod
      
      use pr_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer i,bgk,geom_modified
      integer zone
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
      
      
      bk_num=pr_background_num
      bgk=0
      do i=1,pr_reaction_num
      if(reaction_type(problem_rc(i)).EQ.'bgk_elastic')bgk=1
      end do
      geom_modified=0
      if(bgk.EQ.1.AND.zone_type_num(1).GT.0)then
      do zone=1,zn_num
      if(zone_type(zone).EQ.1)then
      zone_type(zone)=2
      zone_type_num(2)=zone_type_num(2)+1
      zone_pointer(zone)=zone_type_num(2)
      geom_modified=1
      end if
      end do
      zone_type_num(1)=0
      end if
      background_n =>mem_alloc_r2((1),(bk_num),(1),(zone_type_num(2)),'b
     &ackground_n')
      background_v =>mem_alloc_r3((1),(3),(1),(bk_num),(1),(zone_type_nu
     &m(2)),'background_v')
      background_temp =>mem_alloc_r2((1),(bk_num),(1),(zone_type_num(2))
     &,'background_temp')
      call zero_back_arrays
      return
      end
      subroutine zero_back_arrays
      
      use bk_mod
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer zone
      integer back
      do zone=1,zn_num
      if(zone_type(zone).EQ.2)then
      do back=1,bk_num
      background_n(back,zone_pointer(zone))=(0.0_DOUBLE)
      background_temp(back,zone_pointer(zone))=(0.0_DOUBLE)
      background_v(1,back,zone_pointer(zone))=(0.0_DOUBLE)
      background_v(2,back,zone_pointer(zone))=(0.0_DOUBLE)
      background_v(3,back,zone_pointer(zone))=(0.0_DOUBLE)
      end do
      end if
      end do
      return
      end
      function neutralize_species(s)
      
      use el_mod
      
      use sp_mod
      
      use pr_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer neutralize_species
      integer s
      integer i,num_elec,num1,num2
      integer e_sp
      integer test_sp
      integer list1(10)
      integer list2(10)
      external species_add_check
      logical species_add_check
      num_elec=0
      do i=1,species_ncomp(s)
      if(element_z(species_el(i,s)).EQ.-1)then
      num_elec=num_elec+(species_count(i,s))
      end if
      end do
      list1(1)=s
      num1=1
      num2=1
      e_sp=string_lookup('e',species_sy,sp_num)
      do i=1,abs(num_elec)
      if(num_elec.LT.0)then
      list1(i+1)=e_sp
      num1=num1+1
      else
      list2(i+1)=e_sp
      num2=num2+1
      end if
      end do
      do i=1,pr_test_num
      test_sp=problem_test_sp(i)
      list2(1)=test_sp
      if(species_add_check(num1,list1,num2,list2))go to 90007
      end do
      if("No matching neutral species".EQ." ")continue
90007 continue
      neutralize_species=test_sp
      return
      end
      
      subroutine init_wt_alias(uedge)
      
      use so_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer uedge
      integer grp
      integer i
      do i=1,so_seg_tot
      source_segment_rel_wt(i)=(1.0_DOUBLE)
      end do
      do grp=1,so_grps
      call set_prob_alias(source_num_segments(grp),source_current(source
     &_base_ptr(grp)),source_segment_rel_wt(source_base_ptr(grp)),source
     &_segment_ptr(source_base_ptr(grp)),source_segment_prob_alias(sourc
     &e_base_ptr(grp)),source_segment_ptr_alias(source_base_ptr(grp)),so
     &urce_weight_norm(grp))
      end do
      if(uedge.EQ.1)call write_old_sources
      return
      end
      subroutine set_prob_alias(n,so,wt,x_ptr,pa,ya,wtnorm)
      
      
      
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer n
      REAL(kind=DOUBLE)so(n),wt(n)
      integer x_ptr(n)
      REAL(kind=DOUBLE)pa(n)
      REAL(kind=DOUBLE)wtnorm
      integer ya(n)
      integer i,j,inew
      REAL(kind=DOUBLE)sum,sum1,probnew
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
      
      
      
      REAL(kind=DOUBLE),dimension(:),pointer::prob
      integer,dimension(:),pointer::ia
      sum=(0.0_DOUBLE)
      sum1=(0.0_DOUBLE)
      do i=1,n
      if(wt(i).GT.(0.0_DOUBLE))then
      sum=sum+so(i)/wt(i)
      sum1=sum1+so(i)
      end if
      end do
      wtnorm=sum/sum1
      if(n.GT.100000)then
      pa(1)=(0.0_DOUBLE)
      ya(1)=2000000000
      do i=2,n
      pa(i)=pa(i-1)+so(i-1)/wt(i-1)
      ya(i)=2000000000
      end do
      do i=1,n
      pa(i)=pa(i)/(sum)
      end do
      if(abs((pa(n)+so(n)/(sum*wt(n)))-(1.0_DOUBLE)).LT.(5*EPSILON((0.0_
     &DOUBLE))))continue
      return
      else
      prob =>mem_alloc_r1((1),(n),'prob')
      ia =>mem_alloc_i1((1),(n),'ia')
      do i=1,n
      if(wt(i).GT.(0.0_DOUBLE))then
      prob(i)=so(i)/(sum*wt(i))
      else
      prob(i)=(0.0_DOUBLE)
      end if
      ia(i)=i
      end do
      call sort2(n,prob,ia)
      do i=n,1,-1
      pa(ia(1))=REAL(n,DOUBLE)*prob(1)
      ya(ia(1))=x_ptr(ia(i))
      probnew=prob(i)-((1.0_DOUBLE)/REAL(n,DOUBLE)-prob(1))
      inew=ia(i)
      if(i.GT.2)then
      do j=1,i-2
      if(probnew.LT.prob(j+1))then
      prob(j)=probnew
      ia(j)=inew
      goto 10
      end if
      prob(j)=prob(j+1)
      ia(j)=ia(j+1)
      end do
      prob(i-1)=probnew
      ia(i-1)=inew
10    continue
      else if(i.EQ.2)then
      prob(1)=probnew
      ia(1)=inew
      end if
      end do
      call mem_free_r1(prob,(1),(n),'prob')
      call mem_free_i1(ia,(1),(n),'ia')
      end if
      return
      end
      subroutine update_wt_alias(old_current,old_rel_wt,old_tot_curr)
      
      use so_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)old_current(*),old_rel_wt(*),old_tot_curr(*)
      integer i_seg,initialize,write_file
      integer grp
      write_file=0
      do grp=1,so_grps
      source_weight_norm(grp)=(0.0_DOUBLE)
      initialize=0
      do i_seg=source_base_ptr(grp),source_base_ptr(grp)+source_num_segm
     &ents(grp)-1
      if(old_current(i_seg).GT.(0.0_DOUBLE))then
      source_segment_rel_wt(i_seg)=old_rel_wt(i_seg)*source_current(i_se
     &g)/old_current(i_seg)
      else
      if(source_current(i_seg).EQ.(0.0_DOUBLE))continue
      end if
      if(source_segment_rel_wt(i_seg).GT.so_rel_wt_max.OR.source_segment
     &_rel_wt(i_seg).LT.so_rel_wt_min)then
      initialize=1
      go to 90007
      end if
      source_weight_norm(grp)=source_weight_norm(grp)+(source_current(i_
     &seg)/(source_total_current(grp)*source_segment_rel_wt(i_seg)))
      end do
      if(source_weight_norm(grp).GT.so_wt_norm_max.OR.source_weight_norm
     &(grp).LT.so_wt_norm_min)initialize=1
90007 continue
      if(initialize.EQ.1)then
      write(6,*)' Initializing sampling arrays for group ',grp
      write_file=1
      do i_seg=source_base_ptr(grp),source_base_ptr(grp)+source_num_segm
     &ents(grp)-1
      source_segment_rel_wt(i_seg)=(1.0_DOUBLE)
      end do
      call set_prob_alias(source_num_segments(grp),source_current(source
     &_base_ptr(grp)),source_segment_rel_wt(source_base_ptr(grp)),source
     &_segment_ptr(source_base_ptr(grp)),source_segment_prob_alias(sourc
     &e_base_ptr(grp)),source_segment_ptr_alias(source_base_ptr(grp)),so
     &urce_weight_norm(grp))
      end if
      end do
      if(write_file.EQ.1)call write_old_sources
      return
      end
      
      
