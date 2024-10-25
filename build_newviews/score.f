      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine score_test(est,est_fac,species_p,test_p,time_p,weight_p
     &,pos_p,cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,se
     &ctor_next_p,velocity_p,type_p,author_p,estimator_factors)
      
      use pr_mod
      
      use tl_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical check_tally
      integer inc
      integer est
      REAL(kind=DOUBLE)est_fac
      integer species_p
      integer test_p
      REAL(kind=DOUBLE)time_p,weight_p,velocity_p(3)
      REAL(kind=DOUBLE)pos_p(3)
      integer cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,s
     &ector_next_p
      integer type_p,author_p
      integer i,jscore,pr_reac
      REAL(kind=DOUBLE)scoring_data(280)
      REAL(kind=DOUBLE)estimator_factors(*)
      logical need_scores
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
      
      
      need_scores=.FALSE.
      do i=1,tally_type_num(2)
      jscore=tally_type_base(2)+i-1
      estimator_factors(jscore)=tally_est_test(est,jscore)*est_fac*weigh
     &t_p
      if(estimator_factors(jscore).GT.(0.0_DOUBLE))then
      need_scores=.TRUE.
      end if
      end do
      if(need_scores)then
      do i=1,280
      scoring_data(i)=(0.0_DOUBLE)
      end do
      call test_scoring_data(species_p,test_p,time_p,weight_p,pos_p(1),c
     &ell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,sector_nex
     &t_p,velocity_p(1),type_p,author_p,scoring_data)
      pr_reac=0
      call add_scores(2,species_p,test_p,time_p,weight_p,pos_p(1),cell_p
     &,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,sector_next_p,v
     &elocity_p(1),type_p,author_p,pr_reac,scoring_data,estimator_factor
     &s)
      end if
      if(est.EQ.4)call build_snapshot_pdf(species_p,test_p,time_p,weight
     &_p,pos_p(1),cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector
     &_p,sector_next_p,velocity_p(1),type_p,author_p)
      return
      end
      subroutine test_scoring_data(species_p,test_p,time_p,weight_p,pos_
     &p,cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,sector_
     &next_p,velocity_p,type_p,author_p,scoring_data)
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer species_p
      integer test_p
      REAL(kind=DOUBLE)time_p,weight_p,velocity_p(3)
      REAL(kind=DOUBLE)pos_p(3)
      integer cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,s
     &ector_next_p
      integer type_p,author_p
      REAL(kind=DOUBLE)scoring_data(280)
      scoring_data(2)=species_m(species_p)
      scoring_data(3)=species_m(species_p)*velocity_p(1)
      scoring_data(4)=species_m(species_p)*velocity_p(2)
      scoring_data(5)=species_m(species_p)*velocity_p(3)
      scoring_data(6)=(0.5_DOUBLE)*species_m(species_p)*(velocity_p(1)**
     &2+velocity_p(2)**2+velocity_p(3)**2)
      scoring_data(15)=species_m(species_p)*velocity_p(1)*velocity_p(2)
      return
      end
      subroutine score_diagnostics(nprod,species_prod,test_prod,time_pro
     &d,weight_prod,pos_prod,cell_prod,zone_prod,surface_prod,cell_next_
     &prod,zone_next_prod,sector_prod,sector_next_prod,velocity_prod,typ
     &e_prod,author_prod,estimator_factors)
      
      use pr_mod
      
      use tl_mod
      
      use gi_mod
      
      use sc_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical check_tally
      integer inc
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
      REAL(kind=DOUBLE)estimator_factors(*)
      integer i,j,jscore,pr_reac,idiag,i_side,sector,surface,count,i_sec
     &t,subsector,zone
      REAL(kind=DOUBLE)scoring_data(280)
      logical need_scores
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
      
      
      do j=0,nprod
      need_scores=.FALSE.
      do i=1,tally_type_num(1)
      jscore=tally_type_base(1)+i-1
      estimator_factors(jscore)=(0.0_DOUBLE)
      do i_side=1,2
      if(i_side.EQ.1)then
      sector=sector_prod(j)
      zone=zone_prod(j)
      else if(i_side.EQ.2)then
      sector=sector_next_prod(j)
      zone=zone_next_prod(j)
      else
      if(' You are seriously lost!'.EQ.' ')continue
      end if
      if((sector.GT.0.AND.sector.LE.nsectors))then
      surface=sector_surface(sector)
      count=surface_sectors((sign(1,surface)+1)/2,1,abs(surface))
      if(count.GE.1)continue
      do i_sect=1,count
      subsector=sectors(surface_sectors((sign(1,surface)+1)/2,0,abs(surf
     &ace))+i_sect-1)
      idiag=sector_type_pointer(6+tally_geometry_ptr(jscore)-1,subsector
     &)
      if(((idiag.GT.0.AND.idiag.LE.diagnostic_num_sectors(tally_geometry
     &_ptr(jscore))).AND.(sector_zone(subsector).EQ.zone)).AND.(((i_side
     &.EQ.1).AND..NOT.((tally_dep_var(jscore).GE.25).AND.(tally_dep_var(
     &jscore).LE.25+5-1))).OR.((i_side.EQ.2).AND..NOT.((tally_dep_var(js
     &core).GE.30).AND.(tally_dep_var(jscore).LE.30+5-1)))))then
      estimator_factors(jscore)=weight_prod(j)
      need_scores=.TRUE.
      end if
      end do
      end if
      end do
      end do
      if(need_scores)then
      do i=1,280
      scoring_data(i)=(0.0_DOUBLE)
      end do
      call sector_scoring_data(species_prod(j),test_prod(j),time_prod(j)
     &,weight_prod(j),pos_prod(1,j),cell_prod(j),zone_prod(j),surface_pr
     &od(j),cell_next_prod(j),zone_next_prod(j),sector_prod(j),sector_ne
     &xt_prod(j),velocity_prod(1,j),type_prod(j),author_prod(j),scoring_
     &data)
      pr_reac=0
      call add_scores(1,species_prod(j),test_prod(j),time_prod(j),weight
     &_prod(j),pos_prod(1,j),cell_prod(j),zone_prod(j),surface_prod(j),c
     &ell_next_prod(j),zone_next_prod(j),sector_prod(j),sector_next_prod
     &(j),velocity_prod(1,j),type_prod(j),author_prod(j),pr_reac,scoring
     &_data,estimator_factors)
      end if
      end do
      return
      end
      subroutine sector_scoring_data(species_p,test_p,time_p,weight_p,po
     &s_p,cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,secto
     &r_next_p,velocity_p,type_p,author_p,scoring_data)
      
      use sp_mod
      
      use sc_mod
      
      use pr_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer species_p
      integer test_p
      REAL(kind=DOUBLE)time_p,weight_p,velocity_p(3)
      REAL(kind=DOUBLE)pos_p(3)
      integer cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,s
     &ector_next_p
      integer type_p,author_p
      REAL(kind=DOUBLE)scoring_data(280)
      REAL(kind=DOUBLE)cos_angle,cos_angle_next
      integer psp
      external intersection_direction
      REAL(kind=DOUBLE)intersection_direction
      
      scoring_data(2)=species_m(species_p)
      if(((sector_p.GT.0.AND.sector_p.LE.nsectors)).AND.(sector_next_p.G
     &T.0.AND.sector_next_p.LE.nsectors))then
      cos_angle=intersection_direction(sector_surface(sector_p),pos_p(1)
     &,velocity_p(1))
      cos_angle_next=intersection_direction(sector_surface(sector_next_p
     &),pos_p(1),velocity_p(1))
      if(cos_angle+cos_angle_next.LT.(1.0e-8_DOUBLE))continue
      cos_angle=abs(cos_angle)
      else if((sector_p.GT.0.AND.sector_p.LE.nsectors))then
      cos_angle=abs(intersection_direction(sector_surface(sector_p),pos_
     &p(1),velocity_p(1)))
      else if((sector_next_p.GT.0.AND.sector_next_p.LE.nsectors))then
      cos_angle=abs(intersection_direction(sector_surface(sector_next_p)
     &,pos_p(1),velocity_p(1)))
      else
      if(' No valid sector in sector_scoring_data!'.EQ.' ')continue
      end if
      scoring_data(7)=acos(cos_angle)
      scoring_data(6)=(0.5_DOUBLE)*species_m(species_p)*(velocity_p(1)**
     &2+velocity_p(2)**2+velocity_p(3)**2)
      
      if((test_p.GT.0.AND.test_p.LE.pr_test_num))continue
      psp=(pr_background_num+test_p)
      scoring_data(30+15*psp)=scoring_data(2)
      scoring_data(31+15*psp)=species_m(species_p)*velocity_p(1)
      scoring_data(32+15*psp)=species_m(species_p)*velocity_p(2)
      scoring_data(33+15*psp)=species_m(species_p)*velocity_p(3)
      scoring_data(34+15*psp)=scoring_data(6)
      scoring_data(25+15*psp)=scoring_data(2)
      scoring_data(26+15*psp)=species_m(species_p)*velocity_p(1)
      scoring_data(27+15*psp)=species_m(species_p)*velocity_p(2)
      scoring_data(28+15*psp)=species_m(species_p)*velocity_p(3)
      scoring_data(29+15*psp)=scoring_data(6)
      return
      end
      subroutine score_reaction(est,est_fac,ts_reac,rate,nprod,species_p
     &rod,test_prod,time_prod,weight_prod,pos_prod,cell_prod,zone_prod,s
     &urface_prod,cell_next_prod,zone_next_prod,sector_prod,sector_next_
     &prod,velocity_prod,type_prod,author_prod,estimator_factors,ran_ind
     &ex_x,ran_array_x)
      
      use sp_mod
      
      use pr_mod
      
      use rc_mod
      
      use rd_mod
      
      use tl_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      integer est
      REAL(kind=DOUBLE)est_fac,rate
      integer ts_reac
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
      REAL(kind=DOUBLE)estimator_factors(*)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer pr_reac
      logical need_scores
      integer i,jscore
      REAL(kind=DOUBLE)independent_parameters(20),scoring_data(280)
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
      
      
      pr_reac=problem_test_reaction(ts_reac,test_prod(0))
      need_scores=.FALSE.
      nprod=0
      if(rate.EQ.(0.0_DOUBLE))return
      do i=1,tally_type_num(3)
      jscore=tally_type_base(3)+i-1
      if(rate.GT.(0.0_DOUBLE))continue
      estimator_factors(jscore)=tally_est_reaction(pr_reac,est,jscore)*w
     &eight_prod(0)*est_fac
      if(estimator_factors(jscore).GT.(0.0_DOUBLE))then
      need_scores=.TRUE.
      end if
      end do
      if(need_scores.OR.est.EQ.2)then
      do i=1,280
      scoring_data(i)=(0.0_DOUBLE)
      end do
      call set_indep_params(species_prod(0),test_prod(0),time_prod(0),we
     &ight_prod(0),pos_prod(1,0),cell_prod(0),zone_prod(0),surface_prod(
     &0),cell_next_prod(0),zone_next_prod(0),sector_prod(0),sector_next_
     &prod(0),velocity_prod(1,0),type_prod(0),author_prod(0),problem_tes
     &t_background(ts_reac,test_prod(0)),reaction_handling_num_rand(pr_r
     &eac),ran_index_x,ran_array_x(0),independent_parameters)
      if(est.EQ.1.OR.est.EQ.3)then
      call pick_track_reaction(test_prod(0),ts_reac,rate,independent_par
     &ameters,scoring_data,ran_index_x,ran_array_x(0))
      else if(est.EQ.2)then
      call pick_reaction(test_prod(0),ts_reac,rate,independent_parameter
     &s,nprod,species_prod(1),velocity_prod(1,1),weight_prod(1),scoring_
     &data,ran_index_x,ran_array_x(0))
      else
      if('Unsupported estimator'.EQ.' ')continue
      end if
      call add_scores(3,species_prod(0),test_prod(0),time_prod(0),weight
     &_prod(0),pos_prod(1,0),cell_prod(0),zone_prod(0),surface_prod(0),c
     &ell_next_prod(0),zone_next_prod(0),sector_prod(0),sector_next_prod
     &(0),velocity_prod(1,0),type_prod(0),author_prod(0),problem_test_re
     &action(ts_reac,test_prod(0)),scoring_data,estimator_factors)
      end if
      
      if(nprod.GT.0)then
      do i=1,nprod
      test_prod(i)=problem_species_test(species_prod(i))
      pos_prod(1,i)=pos_prod(1,0)
      pos_prod(2,i)=pos_prod(2,0)
      pos_prod(3,i)=pos_prod(3,0)
      
      cell_prod(i)=cell_prod(0)
      zone_prod(i)=zone_prod(0)
      surface_prod(i)=surface_prod(0)
      cell_next_prod(i)=cell_next_prod(0)
      zone_next_prod(i)=zone_next_prod(0)
      sector_prod(i)=sector_prod(0)
      sector_next_prod(i)=sector_next_prod(0)
      time_prod(i)=time_prod(0)
      weight_prod(i)=weight_prod(0)*weight_prod(i)
      author_prod(i)=6+pr_reac
      if(species_z(species_prod(i)).EQ.0)then
      type_prod(i)=1
      else
      type_prod(i)=2
      end if
      end do
      end if
      return
      end
      subroutine puff_data(est,number_x,source_x,source_kseg_x,source_xs
     &eg_x,source_type_x,source_root_sp_x,species_origin_x,test_origin_x
     &,time_origin_x,weight_origin_x,pos_origin_x,cell_origin_x,zone_ori
     &gin_x,surface_origin_x,cell_next_origin_x,zone_next_origin_x,secto
     &r_origin_x,sector_next_origin_x,velocity_origin_x,type_origin_x,au
     &thor_origin_x,ran_index_x,ran_array_x,species_stack_x,test_stack_x
     &,time_stack_x,weight_stack_x,pos_stack_x,cell_stack_x,zone_stack_x
     &,surface_stack_x,cell_next_stack_x,zone_next_stack_x,sector_stack_
     &x,sector_next_stack_x,velocity_stack_x,type_stack_x,author_stack_x
     &,pointer_x,scoring_data)
      
      use sp_mod
      
      use pr_mod
      
      use sc_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer est
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
      REAL(kind=DOUBLE)scoring_data(280)
      integer psp
      REAL(kind=DOUBLE)vpuff,energy
      integer sptest
      REAL(kind=DOUBLE)vcos(3)
      REAL(kind=DOUBLE)vsrc(3)
      sptest=species_stack_x(pointer_x)
      if(est.EQ.3)then
      vpuff=(3._DOUBLE)
      vcos(1)=(0.0_DOUBLE)
      vcos(2)=(0.0_DOUBLE)
      vcos(3)=(8._DOUBLE)/((3._DOUBLE)*sqrt((6._DOUBLE)*atan2((0.0_DOUBL
     &E),-(1.0_DOUBLE))))
      call puff_v_details(source_x,source_xseg_x,source_kseg_x,pos_stack
     &_x(1,pointer_x),cell_stack_x(pointer_x),zone_stack_x(pointer_x),su
     &rface_stack_x(pointer_x),cell_next_stack_x(pointer_x),zone_next_st
     &ack_x(pointer_x),sector_stack_x(pointer_x),sector_next_stack_x(poi
     &nter_x),vpuff,vcos(1),vsrc(1),energy)
      else if(est.EQ.2)then
      vsrc(1)=velocity_stack_x(1,pointer_x)
      vsrc(2)=velocity_stack_x(2,pointer_x)
      vsrc(3)=velocity_stack_x(3,pointer_x)
      
      energy=(0.5_DOUBLE)*species_m(sptest)*(vsrc(1)**2+vsrc(2)**2+vsrc(
     &3)**2)
      else
      if('Incorrect value of est'.EQ.' ')continue
      end if
      psp=(pr_background_num+test_stack_x(pointer_x))
      scoring_data(20+15*psp)=scoring_data(20+15*psp)+(species_m(sptest)
     &)
      scoring_data(21+15*psp)=scoring_data(21+15*psp)+(species_m(sptest)
     &*vsrc(1))
      scoring_data(22+15*psp)=scoring_data(22+15*psp)+(species_m(sptest)
     &*vsrc(2))
      scoring_data(23+15*psp)=scoring_data(23+15*psp)+(species_m(sptest)
     &*vsrc(3))
      scoring_data(24+15*psp)=scoring_data(24+15*psp)+(energy)
      return
      end
      subroutine plate_data(est,number_x,source_x,source_kseg_x,source_x
     &seg_x,source_type_x,source_root_sp_x,species_origin_x,test_origin_
     &x,time_origin_x,weight_origin_x,pos_origin_x,cell_origin_x,zone_or
     &igin_x,surface_origin_x,cell_next_origin_x,zone_next_origin_x,sect
     &or_origin_x,sector_next_origin_x,velocity_origin_x,type_origin_x,a
     &uthor_origin_x,ran_index_x,ran_array_x,species_stack_x,test_stack_
     &x,time_stack_x,weight_stack_x,pos_stack_x,cell_stack_x,zone_stack_
     &x,surface_stack_x,cell_next_stack_x,zone_next_stack_x,sector_stack
     &_x,sector_next_stack_x,velocity_stack_x,type_stack_x,author_stack_
     &x,pointer_x,scoring_data)
      
      use sp_mod
      
      use pr_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer est
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
      REAL(kind=DOUBLE)scoring_data(280)
      integer psp_1,psp_2,type
      REAL(kind=DOUBLE)wa,energy
      integer spion
      integer sptest
      REAL(kind=DOUBLE)vsrc(3)
      spion=source_root_sp_x
      type=source_type_x
      if(est.EQ.3)then
      if(type.EQ.1)then
      wa=(3._DOUBLE)
      call plate_v_details(source_x,source_xseg_x,source_kseg_x,pos_stac
     &k_x(1,pointer_x),cell_stack_x(pointer_x),zone_stack_x(pointer_x),s
     &urface_stack_x(pointer_x),cell_next_stack_x(pointer_x),zone_next_s
     &tack_x(pointer_x),sector_stack_x(pointer_x),sector_next_stack_x(po
     &inter_x),wa,vsrc(1),energy)
      else if(type.EQ.6)then
      wa=(2.0e30_DOUBLE)
      call plt_e_bins_v_details(source_x,source_xseg_x,source_kseg_x,pos
     &_stack_x(1,pointer_x),cell_stack_x(pointer_x),zone_stack_x(pointer
     &_x),surface_stack_x(pointer_x),cell_next_stack_x(pointer_x),zone_n
     &ext_stack_x(pointer_x),sector_stack_x(pointer_x),sector_next_stack
     &_x(pointer_x),wa,vsrc(1),energy)
      else
      if('Unexpected value of isource'.EQ.' ')continue
      end if
      else if(est.EQ.2)then
      vsrc(1)=velocity_stack_x(1,pointer_x)
      vsrc(2)=velocity_stack_x(2,pointer_x)
      vsrc(3)=velocity_stack_x(3,pointer_x)
      
      energy=(0.5_DOUBLE)*species_m(species_stack_x(pointer_x))*(vsrc(1)
     &**2+vsrc(2)**2+vsrc(3)**2)
      else
      if('Incorrect value of est'.EQ.' ')continue
      end if
      psp_1=problem_species_background(spion)
      scoring_data(20+15*psp_1)=scoring_data(20+15*psp_1)+(-species_m(sp
     &ion))
      scoring_data(21+15*psp_1)=scoring_data(21+15*psp_1)+(-species_m(sp
     &ion)*vsrc(1))
      scoring_data(22+15*psp_1)=scoring_data(22+15*psp_1)+(-species_m(sp
     &ion)*vsrc(2))
      scoring_data(23+15*psp_1)=scoring_data(23+15*psp_1)+(-species_m(sp
     &ion)*vsrc(3))
      scoring_data(24+15*psp_1)=scoring_data(24+15*psp_1)+(-energy)
      sptest=species_stack_x(pointer_x)
      psp_2=(pr_background_num+test_stack_x(pointer_x))
      scoring_data(20+15*psp_2)=scoring_data(20+15*psp_2)+(species_m(spt
     &est))
      scoring_data(21+15*psp_2)=scoring_data(21+15*psp_2)+(species_m(spt
     &est)*vsrc(1))
      scoring_data(22+15*psp_2)=scoring_data(22+15*psp_2)+(species_m(spt
     &est)*vsrc(2))
      scoring_data(23+15*psp_2)=scoring_data(23+15*psp_2)+(species_m(spt
     &est)*vsrc(3))
      scoring_data(24+15*psp_2)=scoring_data(24+15*psp_2)+(energy)
      return
      end
      subroutine vol_source_data(est,number_x,source_x,source_kseg_x,sou
     &rce_xseg_x,source_type_x,source_root_sp_x,species_origin_x,test_or
     &igin_x,time_origin_x,weight_origin_x,pos_origin_x,cell_origin_x,zo
     &ne_origin_x,surface_origin_x,cell_next_origin_x,zone_next_origin_x
     &,sector_origin_x,sector_next_origin_x,velocity_origin_x,type_origi
     &n_x,author_origin_x,ran_index_x,ran_array_x,species_stack_x,test_s
     &tack_x,time_stack_x,weight_stack_x,pos_stack_x,cell_stack_x,zone_s
     &tack_x,surface_stack_x,cell_next_stack_x,zone_next_stack_x,sector_
     &stack_x,sector_next_stack_x,velocity_stack_x,type_stack_x,author_s
     &tack_x,pointer_x,scoring_data)
      
      use sp_mod
      
      use pr_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer est
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
      REAL(kind=DOUBLE)scoring_data(280)
      integer psp
      REAL(kind=DOUBLE)temp,energy
      integer sptest
      REAL(kind=DOUBLE)v_gauss(3)
      REAL(kind=DOUBLE)vsrc(3)
      sptest=species_stack_x(pointer_x)
      if(est.EQ.3)then
      v_gauss(1)=(0.0_DOUBLE)
      v_gauss(2)=(0.0_DOUBLE)
      v_gauss(3)=(0.0_DOUBLE)
      call vol_source_v_details(source_x,source_xseg_x,source_kseg_x,pos
     &_stack_x(1,pointer_x),cell_stack_x(pointer_x),zone_stack_x(pointer
     &_x),surface_stack_x(pointer_x),cell_next_stack_x(pointer_x),zone_n
     &ext_stack_x(pointer_x),sector_stack_x(pointer_x),sector_next_stack
     &_x(pointer_x),v_gauss(1),vsrc(1),temp)
      energy=(1.5_DOUBLE)*temp+(0.5_DOUBLE)*species_m(sptest)*(vsrc(1)**
     &2+vsrc(2)**2+vsrc(3)**2)
      else if(est.EQ.2)then
      vsrc(1)=velocity_stack_x(1,pointer_x)
      vsrc(2)=velocity_stack_x(2,pointer_x)
      vsrc(3)=velocity_stack_x(3,pointer_x)
      
      energy=(0.5_DOUBLE)*species_m(sptest)*(vsrc(1)**2+vsrc(2)**2+vsrc(
     &3)**2)
      else
      if('Incorrect value of est'.EQ.' ')continue
      end if
      psp=(pr_background_num+test_stack_x(pointer_x))
      scoring_data(20+15*psp)=scoring_data(20+15*psp)+(species_m(sptest)
     &)
      scoring_data(21+15*psp)=scoring_data(21+15*psp)+(species_m(sptest)
     &*vsrc(1))
      scoring_data(22+15*psp)=scoring_data(22+15*psp)+(species_m(sptest)
     &*vsrc(2))
      scoring_data(23+15*psp)=scoring_data(23+15*psp)+(species_m(sptest)
     &*vsrc(3))
      scoring_data(24+15*psp)=scoring_data(24+15*psp)+(energy)
      return
      end
      subroutine snapshot_data(est,number_x,source_x,source_kseg_x,sourc
     &e_xseg_x,source_type_x,source_root_sp_x,species_origin_x,test_orig
     &in_x,time_origin_x,weight_origin_x,pos_origin_x,cell_origin_x,zone
     &_origin_x,surface_origin_x,cell_next_origin_x,zone_next_origin_x,s
     &ector_origin_x,sector_next_origin_x,velocity_origin_x,type_origin_
     &x,author_origin_x,ran_index_x,ran_array_x,species_stack_x,test_sta
     &ck_x,time_stack_x,weight_stack_x,pos_stack_x,cell_stack_x,zone_sta
     &ck_x,surface_stack_x,cell_next_stack_x,zone_next_stack_x,sector_st
     &ack_x,sector_next_stack_x,velocity_stack_x,type_stack_x,author_sta
     &ck_x,pointer_x,scoring_data)
      
      use sp_mod
      
      use pr_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer est
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
      REAL(kind=DOUBLE)scoring_data(280)
      integer psp
      REAL(kind=DOUBLE)energy
      integer sptest
      REAL(kind=DOUBLE)vsrc(3)
      sptest=species_stack_x(pointer_x)
      if(est.EQ.3)then
      if('Snapshot source cannot be post-processed'.EQ.' ')continue
      else if(est.EQ.2)then
      vsrc(1)=velocity_stack_x(1,pointer_x)
      vsrc(2)=velocity_stack_x(2,pointer_x)
      vsrc(3)=velocity_stack_x(3,pointer_x)
      
      energy=(0.5_DOUBLE)*species_m(sptest)*(vsrc(1)**2+vsrc(2)**2+vsrc(
     &3)**2)
      else
      if('Incorrect value of est'.EQ.' ')continue
      end if
      psp=(pr_background_num+test_stack_x(pointer_x))
      scoring_data(20+15*psp)=scoring_data(20+15*psp)+(species_m(sptest)
     &)
      scoring_data(21+15*psp)=scoring_data(21+15*psp)+(species_m(sptest)
     &*vsrc(1))
      scoring_data(22+15*psp)=scoring_data(22+15*psp)+(species_m(sptest)
     &*vsrc(2))
      scoring_data(23+15*psp)=scoring_data(23+15*psp)+(species_m(sptest)
     &*vsrc(3))
      scoring_data(24+15*psp)=scoring_data(24+15*psp)+(energy)
      return
      end
      subroutine add_scores(type,species_p,test_p,time_p,weight_p,pos_p,
     &cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,sector_ne
     &xt_p,velocity_p,type_p,author_p,pr_reac,scoring_data,estimator_fac
     &tors)
      
      use pr_mod
      
      use tl_mod
      
      use sc_mod
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical check_tally
      integer inc
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      integer type
      integer species_p
      integer test_p
      REAL(kind=DOUBLE)time_p,weight_p,velocity_p(3)
      REAL(kind=DOUBLE)pos_p(3)
      integer cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,s
     &ector_next_p
      integer type_p,author_p
      integer pr_reac
      REAL(kind=DOUBLE)scoring_data(280),estimator_factors(*)
      
      integer i,j,k,jscore,problem_sp,number_segments,geom_seg_label,geo
     &m_seg,bin_label,ibin,problem_sp_min,problem_sp_max,j_tl_cv,icv
      integer index_parameters(0:100),segments(60000),ind_val(5),number_
     &bins(60000),bins(200,60000)
      REAL(kind=DOUBLE)check_data
      REAL(kind=DOUBLE)geom_mult(200,60000),converted_data(280)
      logical need_track_conversions
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
      
      
      do i=0,100
      index_parameters(i)=0
      end do
      index_parameters(3)=test_p
      index_parameters(6)=author_p
      index_parameters(7)=pr_reac
      index_parameters(11)=sector_p
      index_parameters(12)=strata(sector_p)
      index_parameters(13)=sector_strata_segment(sector_p)
      index_parameters(18)=zone_index(1,zone_p)
      index_parameters(19)=zone_index(2,zone_p)
      do i=1,tally_type_num(type)
      jscore=tally_type_base(type)+i-1
      need_track_conversions=.FALSE.
      if(tally_num_conversions(jscore).GT.0)then
      do j_tl_cv=1,tally_num_conversions(jscore)
      icv=tally_cv_ptr(j_tl_cv,jscore)
      if(tally_cv_type(icv).EQ.1)need_track_conversions=.TRUE.
      end do
      end if
      check_data=(0.0_DOUBLE)
      if(tally_dep_var(jscore).GE.20.AND.tally_dep_var(jscore).LE.20+15-
     &1)then
      problem_sp_min=1
      problem_sp_max=pr_background_num+pr_test_num
      do problem_sp=1,pr_background_num+pr_test_num
      index_parameters(4)=problem_sp
      do j=1,tally_dep_var_dim(jscore)
      check_data=check_data+(abs(scoring_data(tally_dep_var(jscore)+j-1+
     &15*index_parameters(4))))
      end do
      end do
      else
      problem_sp_min=0
      problem_sp_max=0
      index_parameters(4)=0
      do j=1,tally_dep_var_dim(jscore)
      check_data=check_data+(abs(scoring_data(tally_dep_var(jscore)+j-1+
     &15*index_parameters(4))))
      end do
      end if
      if(estimator_factors(jscore)*check_data.GT.(0.0_DOUBLE))then
      call handle_geometry(species_p,test_p,time_p,weight_p,pos_p(1),cel
     &l_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,sector_next_
     &p,velocity_p(1),type_p,author_p,tally_geometry(jscore),tally_geome
     &try_ptr(jscore),tally_dep_var(jscore),scoring_data,number_segments
     &,segments,geom_seg_label,number_bins,bins,bin_label,geom_mult)
      do geom_seg=1,number_segments
      index_parameters(geom_seg_label)=segments(geom_seg)
      do ibin=1,number_bins(geom_seg)
      index_parameters(bin_label)=bins(ibin,geom_seg)
      do problem_sp=problem_sp_min,problem_sp_max
      index_parameters(4)=problem_sp
      do k=1,5
      if(k.LE.tally_rank(jscore))then
      ind_val(k)=index_parameters(tally_indep_var(k,jscore))-1
      if(ind_val(k).GE.0.AND.ind_val(k).LT.tally_tab_index(k,jscore))con
     &tinue
      else
      ind_val(k)=0
      end if
      end do
      if(need_track_conversions)then
      call track_conversions(jscore,species_p,test_p,time_p,weight_p,pos
     &_p(1),cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,sec
     &tor_next_p,velocity_p(1),type_p,author_p,index_parameters,scoring_
     &data,converted_data)
      do j=1,tally_dep_var_dim(jscore)
      call set_comp_scores(tally_base(jscore)+j-1+tally_dep_var_dim(jsco
     &re)*(ind_val(1)+tally_tab_index(1,jscore)*(ind_val(2)+tally_tab_in
     &dex(2,jscore)*(ind_val(3)+tally_tab_index(3,jscore)*(ind_val(4)+ta
     &lly_tab_index(4,jscore)*ind_val(5))))),converted_data(tally_dep_va
     &r(jscore)+j-1+15*index_parameters(4))*geom_mult(ibin,geom_seg)*est
     &imator_factors(jscore))
      end do
      else
      do j=1,tally_dep_var_dim(jscore)
      call set_comp_scores(tally_base(jscore)+j-1+tally_dep_var_dim(jsco
     &re)*(ind_val(1)+tally_tab_index(1,jscore)*(ind_val(2)+tally_tab_in
     &dex(2,jscore)*(ind_val(3)+tally_tab_index(3,jscore)*(ind_val(4)+ta
     &lly_tab_index(4,jscore)*ind_val(5))))),scoring_data(tally_dep_var(
     &jscore)+j-1+15*index_parameters(4))*geom_mult(ibin,geom_seg)*estim
     &ator_factors(jscore))
      end do
      end if
      end do
      end do
      end do
      end if
      end do
      return
      end
      subroutine set_comp_scores(ifull,datum)
      
      use sa_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer ifull
      REAL(kind=DOUBLE)datum
      integer ishort
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
      
      
      if(datum.EQ.(0.0_DOUBLE))return
      if(stat_comp_flt.EQ.1)then
      if(stat_ptr2short_flt(ifull).EQ.314159265)then
      stat_size_flt=stat_size_flt+1
      if(stat_size_flt.GT.stat_dim_flt)then
      stat_flt =>mem_realloc_r2(stat_flt,(0),(1),(0),stat_dim_flt-1,stat
     &_dim_flt+2500-1,'stat_flt')
      stat_ptr2full_flt =>mem_realloc_i1(stat_ptr2full_flt,(0),stat_pf_d
     &im_flt-1,stat_pf_dim_flt+2500-1,'stat_ptr2full_flt')
      stat_dim_flt=stat_dim_flt+(2500)
      stat_pf_dim_flt=stat_pf_dim_flt+(2500)
      end if
      ishort=stat_size_flt-1
      stat_ptr2full_flt(ishort)=ifull
      stat_ptr2short_flt(ifull)=ishort
      stat_flt(0,ishort)=(0.0_DOUBLE)
      stat_flt(1,ishort)=(0.0_DOUBLE)
      else
      ishort=stat_ptr2short_flt(ifull)
      end if
      else
      ishort=ifull
      end if
      stat_flt(0,ishort)=stat_flt(0,ishort)+(datum)
      return
      end
      subroutine handle_geometry(species_p,test_p,time_p,weight_p,pos_p,
     &cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,sector_ne
     &xt_p,velocity_p,type_p,author_p,geometry,pointer,dep_var,scoring_d
     &ata,number_segments,segments,geom_seg_label,number_bins,bins,bin_l
     &abel,geom_mult)
      
      use pr_mod
      
      use tl_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical check_tally
      integer inc
      integer species_p
      integer test_p
      REAL(kind=DOUBLE)time_p,weight_p,velocity_p(3)
      REAL(kind=DOUBLE)pos_p(3)
      integer cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,s
     &ector_next_p
      integer type_p,author_p
      integer geometry,pointer,dep_var
      REAL(kind=DOUBLE)scoring_data(280)
      
      integer number_segments,geom_seg_label,bin_label
      integer segments(60000),number_bins(60000),bins(200,60000)
      REAL(kind=DOUBLE)geom_mult(200,60000)
      number_segments=0
      segments(1)=0
      if(geometry.EQ.1)then
      number_segments=1
      segments(1)=zone_p
      geom_seg_label=1
      number_bins(1)=1
      bins(1,1)=0
      bin_label=0
      geom_mult(1,1)=(1.0_DOUBLE)
      else if(geometry.EQ.3)then
      call handle_detectors(species_p,test_p,time_p,weight_p,pos_p(1),ce
     &ll_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,sector_next
     &_p,velocity_p(1),type_p,author_p,pointer,dep_var,scoring_data,numb
     &er_segments,segments,number_bins,bins,bin_label,geom_mult)
      geom_seg_label=5
      else if(geometry.EQ.2)then
      call handle_diagnostics(species_p,test_p,time_p,weight_p,pos_p(1),
     &cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,sector_ne
     &xt_p,velocity_p(1),type_p,author_p,pointer,dep_var,scoring_data,nu
     &mber_segments,segments,number_bins,bins,bin_label,geom_mult)
      geom_seg_label=17
      else
      if("Untreated scoring geometry".EQ." ")continue
      end if
      return
      end
      subroutine handle_detectors(species_p,test_p,time_p,weight_p,pos_p
     &,cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,sector_n
     &ext_p,velocity_p,type_p,author_p,pointer,dep_var,scoring_data,numb
     &er_segments,segments,number_bins,bins,bin_label,geom_mult)
      
      use zn_mod
      
      use de_mod
      
      use pr_mod
      
      use bk_mod
      
      use tl_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      external particle_track
      logical particle_track
      logical check_tally
      integer inc
      integer species_p
      integer test_p
      REAL(kind=DOUBLE)time_p,weight_p,velocity_p(3)
      REAL(kind=DOUBLE)pos_p(3)
      integer cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,s
     &ector_next_p
      integer type_p,author_p
      integer pointer,dep_var
      REAL(kind=DOUBLE)scoring_data(280)
      integer number_segments,bin_label
      integer segments(60000),number_bins(60000),bins(200,60000)
      REAL(kind=DOUBLE)geom_mult(200,60000)
      integer de_group,iview,i,ibin,num_lines,i_e_rate,i_line,zone,ifrag
     &,inum
      integer lines(6),e_rates(6)
      REAL(kind=DOUBLE)lambda0,lambda,lambda_f,dl,vth,delta_l_th,f,densi
     &ty,delta_l_st,x_voigt,y_voigt,zone_frag
      character*40 line
      REAL(kind=DOUBLE)v(3)
      logical init
      external locate_point,check_location
      integer locate_point
      logical check_location
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      external find_lambda,voigt
      REAL(kind=DOUBLE)find_lambda,voigt
      save init,num_lines,lines,e_rates
      data init/.TRUE./
      if(init)then
      num_lines=0
      do i=1,pr_var0_num
      if(pr_var0_list(i) (1:10).EQ.'wavelength')then
      num_lines=num_lines+1
      lines(num_lines)=i
      line=pr_var0_list(i) (11:)
      i_e_rate=string_lookup('emission_rate'//line,pr_var0_list,pr_var0_
     &num)
      if(i_e_rate.GT.0)continue
      e_rates(num_lines)=i_e_rate
      end if
      end do
      init=.FALSE.
      end if
      de_group=pointer
      number_segments=0
      i_line=int_lookup(dep_var,e_rates,num_lines)
      if(i_line.GT.0)continue
      if(detector_var(de_group).EQ.1)then
      bin_label=16
      if(scoring_data(14).GT.(0.0_DOUBLE))then
      vth=scoring_data(14)
      v(1)=scoring_data(11)
      v(2)=scoring_data(12)
      v(3)=scoring_data(13)
      else
      vth=(0.0_DOUBLE)
      v(1)=scoring_data(8)
      v(2)=scoring_data(9)
      v(3)=scoring_data(10)
      if(sqrt((v(1)**2+v(2)**2+v(3)**2)).GT.(0.0_DOUBLE))continue
      end if
      lambda0=scoring_data(lines(i_line))
      if(lambda0.GT.(0.0_DOUBLE))continue
      else
      bin_label=0
      end if
      density=background_n(1,zone_pointer(zone_p))
      if(density.LT.(1.e20_DOUBLE))then
      delta_l_st=(0.1_DOUBLE)*(density/(1.e20_DOUBLE))
      else if(density.LT.(1.e21_DOUBLE))then
      delta_l_st=(0.1_DOUBLE)+((0.4_DOUBLE)/(9._DOUBLE))*(density/(1.e20
     &_DOUBLE)-(1.0_DOUBLE))
      else if(density.LT.(1.e22_DOUBLE))then
      delta_l_st=(0.5_DOUBLE)+((1.9_DOUBLE)/(9._DOUBLE))*(density/(1.e21
     &_DOUBLE)-(1.0_DOUBLE))
      else
      delta_l_st=(2.4_DOUBLE)
      end if
      delta_l_st=delta_l_st*((1.e-10_DOUBLE))
      zone=zone_p
      do i=1,detector_num_views(de_group)
      iview=de_view_tab(de_view_base(de_group)+i-1)
      zone_frag=(0.0_DOUBLE)
      if((zone.GE.de_zone_frags_min_zn(iview)).AND.(zone.LE.de_zone_frag
     &s_max_zn(iview)))then
      if(de_zone_frags_num(iview).GT.0)continue
      do inum=1,de_zone_frags_num(iview)
      ifrag=de_zone_frags_start(iview)+(inum-1)
      if(zone.EQ.de_zone_frags_zones(ifrag))then
      zone_frag=de_zone_frags(ifrag)
      go to 90007
      end if
      end do
90007 continue
      end if
      if(zone_frag.NE.(0.0_DOUBLE))then
      number_segments=number_segments+1
      if(number_segments.LE.60000)continue
      segments(number_segments)=i
      if(detector_var(de_group).EQ.1)then
      if(vth.EQ.(0.0_DOUBLE))then
      lambda=find_lambda(pos_p(1),v(1),de_view_points(1,0,iview),lambda0
     &)
      number_bins(number_segments)=1
      if(detector_spacing(de_group).EQ.2)lambda=log(lambda)
      if(detector_spacing(de_group).EQ.2.OR.detector_spacing(de_group).E
     &Q.1)then
      bins(1,number_segments)=int((lambda-detector_min(de_group))/detect
     &or_delta(de_group)+(1.0_DOUBLE))
      else
      if('Unsupported spacing'.EQ.' ')continue
      end if
      if(bins(1,number_segments).GT.0.AND.bins(1,number_segments).LE.det
     &ector_tab_index(de_group))then
      geom_mult(1,number_segments)=zone_frag
      else
      bins(1,number_segments)=1
      geom_mult(1,number_segments)=(0.0_DOUBLE)
      end if
      else if(vth.GT.(0.0_DOUBLE))then
      lambda_f=find_lambda(pos_p(1),v(1),de_view_points(1,0,iview),lambd
     &a0)
      delta_l_th=lambda0*vth/(2.99792458e8_DOUBLE)
      number_bins(number_segments)=detector_tab_index(de_group)
      if(number_bins(number_segments).LE.200)continue
      do ibin=1,number_bins(number_segments)
      bins(ibin,number_segments)=ibin
      lambda=detector_min(de_group)+(ibin-(0.5_DOUBLE))*detector_delta(d
     &e_group)
      if(detector_spacing(de_group).EQ.2)then
      lambda=exp(lambda)
      dl=lambda*detector_delta(de_group)
      else
      if(detector_spacing(de_group).EQ.1)continue
      dl=detector_delta(de_group)
      end if
      x_voigt=(lambda-lambda_f)/(sqrt((2.0_DOUBLE))*delta_l_th)
      f=exp(-x_voigt**2)
      f=f/((delta_l_th*sqrt((2.0_DOUBLE)*atan2((0.0_DOUBLE),-(1.0_DOUBLE
     &)))))
      geom_mult(ibin,number_segments)=f*dl*zone_frag
      end do
      else
      if('Incomplete emitter velocity specification'.EQ.' ')continue
      end if
      else
      number_bins(number_segments)=1
      bins(1,number_segments)=0
      geom_mult(1,number_segments)=zone_frag
      end if
      end if
      end do
      return
      end
      function find_lambda(x,v,x0_detector,lambda0)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)find_lambda
      REAL(kind=DOUBLE)x(3)
      REAL(kind=DOUBLE)v(3)
      REAL(kind=DOUBLE)x0_detector(3)
      REAL(kind=DOUBLE)lambda0
      REAL(kind=DOUBLE)rr,toroidal_angle,vrel
      REAL(kind=DOUBLE)vp(3)
      REAL(kind=DOUBLE)xp(3)
      REAL(kind=DOUBLE)line_of_sight(3)
      REAL(kind=DOUBLE)vector_temp(3)
      if(geometry_symmetry.EQ.1.OR.geometry_symmetry.EQ.3)then
      xp(1)=x(1)
      xp(2)=(0.0_DOUBLE)
      xp(3)=x(3)
      vp(1)=v(1)
      vp(2)=v(2)
      vp(3)=v(3)
      
      else if(geometry_symmetry.EQ.2)then
      rr=sqrt(x(1)**2+x(2)**2)
      xp(1)=rr
      xp(2)=(0.0_DOUBLE)
      xp(3)=x(3)
      if(x(1).EQ.(0.0_DOUBLE).AND.x(2).EQ.(0.0_DOUBLE))then
      toroidal_angle=(0.5_DOUBLE)*atan2((0.0_DOUBLE),-(1.0_DOUBLE))
      else
      toroidal_angle=atan2(x(2),x(1))
      if(toroidal_angle.LT.(0.0_DOUBLE))then
      toroidal_angle=toroidal_angle+((2.0_DOUBLE)*atan2((0.0_DOUBLE),-(1
     &.0_DOUBLE)))
      end if
      end if
      vp(1)=v(1)*cos(toroidal_angle)+v(2)*sin(toroidal_angle)
      vp(2)=-v(1)*sin(toroidal_angle)+v(2)*cos(toroidal_angle)
      vp(3)=v(3)
      else
      xp(1)=x(1)
      xp(2)=x(2)
      xp(3)=x(3)
      
      vp(1)=v(1)
      vp(2)=v(2)
      vp(3)=v(3)
      
      end if
      line_of_sight(1)=xp(1)-x0_detector(1)
      line_of_sight(2)=xp(2)-x0_detector(2)
      line_of_sight(3)=xp(3)-x0_detector(3)
      
      vector_temp(1)=sqrt((line_of_sight(1)**2+line_of_sight(2)**2+line_
     &of_sight(3)**2))
      if(vector_temp(1).GT.(0.0_DOUBLE))continue
      line_of_sight(1)=((1.0_DOUBLE)/vector_temp(1))*line_of_sight(1)
      line_of_sight(2)=((1.0_DOUBLE)/vector_temp(1))*line_of_sight(2)
      line_of_sight(3)=((1.0_DOUBLE)/vector_temp(1))*line_of_sight(3)
      
      vrel=(vp(1)*line_of_sight(1)+vp(2)*line_of_sight(2)+vp(3)*line_of_
     &sight(3))
      find_lambda=lambda0*((1.0_DOUBLE)+vrel/(2.99792458e8_DOUBLE))
      return
      end
      subroutine handle_diagnostics(species_p,test_p,time_p,weight_p,pos
     &_p,cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,sector
     &_next_p,velocity_p,type_p,author_p,pointer,dep_var,scoring_data,nu
     &mber_segments,segments,number_bins,bins,bin_label,geom_mult)
      
      use sc_mod
      
      use gi_mod
      
      use pr_mod
      
      use tl_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      external particle_track
      logical particle_track
      logical check_tally
      integer inc
      integer species_p
      integer test_p
      REAL(kind=DOUBLE)time_p,weight_p,velocity_p(3)
      REAL(kind=DOUBLE)pos_p(3)
      integer cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,s
     &ector_next_p
      integer type_p,author_p
      integer pointer,dep_var
      REAL(kind=DOUBLE)scoring_data(280)
      integer number_segments,bin_label
      integer segments(60000),number_bins(60000),bins(200,60000)
      REAL(kind=DOUBLE)geom_mult(200,60000)
      integer diag_grp,idiag,i_side,sector,i_sect,surface,count,subsecto
     &r,zone
      REAL(kind=DOUBLE)value
      external locate_point,check_location
      integer locate_point
      logical check_location
      diag_grp=pointer
      number_segments=0
      do i_side=1,2
      if(i_side.EQ.1)then
      sector=sector_p
      zone=zone_p
      else if(i_side.EQ.2)then
      sector=sector_next_p
      zone=zone_next_p
      else
      if(' You are seriously lost!'.EQ.' ')continue
      end if
      if((sector.GT.0.AND.sector.LE.nsectors))then
      surface=sector_surface(sector)
      count=surface_sectors((sign(1,surface)+1)/2,1,abs(surface))
      if(count.GE.1)continue
      do i_sect=1,count
      subsector=sectors(surface_sectors((sign(1,surface)+1)/2,0,abs(surf
     &ace))+i_sect-1)
      idiag=sector_type_pointer(6+diag_grp-1,subsector)
      if((idiag.GT.0.AND.idiag.LE.diagnostic_num_sectors(diag_grp)).AND.
     &(sector_zone(subsector).EQ.zone))then
      number_segments=number_segments+1
      if(number_segments.LE.60000)continue
      segments(number_segments)=idiag
      if(diagnostic_var(diag_grp).EQ.1)then
      bin_label=14
      value=scoring_data(6)
      else if(diagnostic_var(diag_grp).EQ.2)then
      bin_label=15
      value=scoring_data(7)
      else
      bin_label=0
      end if
      if(bin_label.NE.0)then
      number_bins(number_segments)=1
      if(diagnostic_spacing(diag_grp).EQ.2)value=log(value)
      if(diagnostic_spacing(diag_grp).EQ.2.OR.diagnostic_spacing(diag_gr
     &p).EQ.1)then
      bins(1,number_segments)=int((value-diagnostic_min(diag_grp))/diagn
     &ostic_delta(diag_grp)+(1.0_DOUBLE))
      else
      if('Unsupported spacing'.EQ.' ')continue
      end if
      if(bins(1,number_segments).GT.0.AND.bins(1,number_segments).LE.dia
     &gnostic_tab_index(diag_grp))then
      geom_mult(1,number_segments)=(1.0_DOUBLE)
      else
      bins(1,number_segments)=1
      geom_mult(1,number_segments)=(0.0_DOUBLE)
      end if
      else
      number_bins(number_segments)=1
      bins(1,number_segments)=0
      geom_mult(1,number_segments)=(1.0_DOUBLE)
      end if
      end if
      end do
      end if
      end do
      return
      end
      subroutine post_process_test_scores(is,estimator_factors)
      
      use zn_mod
      
      use pr_mod
      
      use tl_mod
      
      use sp_mod
      
      use so_mod
      
      use sa_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer is
      REAL(kind=DOUBLE)estimator_factors(*)
      integer jscore,jscore_v,i,j,k,l,m,irank_test,irank_zone,test,zone,
     &ir,ic,nprod
      integer index_parameters(0:100),ind_val(5)
      REAL(kind=DOUBLE)est_fac,particle_no,rate
      REAL(kind=DOUBLE)velocity(3)
      REAL(kind=DOUBLE)pos_zone_loc(3)
      integer cell_zone_loc,zone_zone_loc,surface_zone_loc,cell_next_zon
     &e_loc,zone_next_zone_loc,sector_zone_loc,sector_next_zone_loc
      integer species_fake_prod(0:4)
      integer test_fake_prod(0:4)
      REAL(kind=DOUBLE)time_fake_prod(0:4),weight_fake_prod(0:4),velocit
     &y_fake_prod(3,0:4)
      REAL(kind=DOUBLE)pos_fake_prod(3,0:4)
      integer cell_fake_prod(0:4),zone_fake_prod(0:4),surface_fake_prod(
     &0:4),cell_next_fake_prod(0:4),zone_next_fake_prod(0:4),sector_fake
     &_prod(0:4),sector_next_fake_prod(0:4)
      integer type_fake_prod(0:4),author_fake_prod(0:4)
      integer seed(0:8-1)
      integer ran_index_fake_rn
      REAL(kind=DOUBLE)ran_array_fake_rn(0:100-1)
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
      REAL(kind=DOUBLE)random
      external random
      logical check_zone
      REAL(kind=DOUBLE)find_rate
      external find_rate
      call decimal_to_seed(so_seed_decimal,seed(0))
      call random_init_d2(seed,ran_index_fake_rn,ran_array_fake_rn(0))
      jscore=string_lookup('particle number',tally_name,tl_num)
      jscore_v=string_lookup('neutral flux vector',tally_name,tl_num)
      if(jscore.GT.0.AND.jscore_v.GT.0)continue
      if(5.EQ.5)continue
      do i=1,5
      if(tally_indep_var(i,jscore).EQ.3)irank_test=i
      if(tally_indep_var(i,jscore).EQ.1)irank_zone=i
      if(tally_indep_var(i,jscore).EQ.tally_indep_var(i,jscore_v))contin
     &ue
      end do
      do m=1,tally_tab_index(5,jscore)
      index_parameters(tally_indep_var(5,jscore))=m
      ind_val(5)=m-1
      do l=1,tally_tab_index(4,jscore)
      index_parameters(tally_indep_var(4,jscore))=l
      ind_val(4)=l-1
      do k=1,tally_tab_index(3,jscore)
      index_parameters(tally_indep_var(3,jscore))=k
      ind_val(3)=k-1
      do j=1,tally_tab_index(2,jscore)
      index_parameters(tally_indep_var(2,jscore))=j
      ind_val(2)=j-1
      do i=1,tally_tab_index(1,jscore)
      index_parameters(tally_indep_var(1,jscore))=i
      ind_val(1)=i-1
      test=index_parameters(3)
      if((test.GT.0.AND.test.LE.pr_test_num))continue
      zone=index_parameters(1)
      if(check_zone(zone))continue
      if(zone_type(zone).EQ.2)then
      pos_zone_loc(1)=zone_center(1,zone)
      pos_zone_loc(2)=zone_center(2,zone)
      pos_zone_loc(3)=zone_center(3,zone)
      
      zone_zone_loc=zone
      zone_next_zone_loc=0
      cell_zone_loc=0
      cell_next_zone_loc=0
      sector_zone_loc=0
      sector_next_zone_loc=0
      surface_zone_loc=0
      particle_no=stat_flt(0,tally_base(jscore)+1-1+tally_dep_var_dim(js
     &core)*(ind_val(1)+tally_tab_index(1,jscore)*(ind_val(2)+tally_tab_
     &index(2,jscore)*(ind_val(3)+tally_tab_index(3,jscore)*(ind_val(4)+
     &tally_tab_index(4,jscore)*ind_val(5))))))
      do ic=1,tally_dep_var_dim(jscore_v)
      velocity(ic)=(1.0_DOUBLE)
      end do
      species_fake_prod(0)=string_lookup(species_sy(problem_test_sp(test
     &)),species_sy,sp_num)
      test_fake_prod(0)=problem_species_test(species_fake_prod(0))
      if(species_m(species_fake_prod(0)).EQ.0)then
      type_fake_prod(0)=0
      else if(species_z(species_fake_prod(0)).EQ.0)then
      type_fake_prod(0)=1
      author_fake_prod(0)=source_type(is)
      else
      type_fake_prod(0)=2
      author_fake_prod(0)=source_type(is)
      end if
      weight_fake_prod(0)=(1.0_DOUBLE)
      pos_fake_prod(1,0)=pos_zone_loc(1)
      pos_fake_prod(2,0)=pos_zone_loc(2)
      pos_fake_prod(3,0)=pos_zone_loc(3)
      
      cell_fake_prod(0)=cell_zone_loc
      zone_fake_prod(0)=zone_zone_loc
      surface_fake_prod(0)=surface_zone_loc
      cell_next_fake_prod(0)=cell_next_zone_loc
      zone_next_fake_prod(0)=zone_next_zone_loc
      sector_fake_prod(0)=sector_zone_loc
      sector_next_fake_prod(0)=sector_next_zone_loc
      weight_fake_prod(0)=particle_no
      velocity_fake_prod(1,0)=velocity(1)
      velocity_fake_prod(2,0)=velocity(2)
      velocity_fake_prod(3,0)=velocity(3)
      
      est_fac=(1.0_DOUBLE)
      if(problem_reaction_num(test_fake_prod(0)).GT.0.AND.(particle_no.G
     &T.0))then
      do ir=1,problem_reaction_num(test_fake_prod(0))
      rate=find_rate(species_fake_prod(0),test_fake_prod(0),time_fake_pr
     &od(0),weight_fake_prod(0),pos_fake_prod(1,0),cell_fake_prod(0),zon
     &e_fake_prod(0),surface_fake_prod(0),cell_next_fake_prod(0),zone_ne
     &xt_fake_prod(0),sector_fake_prod(0),sector_next_fake_prod(0),veloc
     &ity_fake_prod(1,0),type_fake_prod(0),author_fake_prod(0),problem_t
     &est_background(ir,test_fake_prod(0)),problem_test_reaction(ir,test
     &_fake_prod(0)),ran_index_fake_rn,ran_array_fake_rn(0))
      call score_reaction(3,est_fac,ir,rate,nprod,species_fake_prod(0),t
     &est_fake_prod(0),time_fake_prod(0),weight_fake_prod(0),pos_fake_pr
     &od(1,0),cell_fake_prod(0),zone_fake_prod(0),surface_fake_prod(0),c
     &ell_next_fake_prod(0),zone_next_fake_prod(0),sector_fake_prod(0),s
     &ector_next_fake_prod(0),velocity_fake_prod(1,0),type_fake_prod(0),
     &author_fake_prod(0),estimator_factors,ran_index_fake_rn,ran_array_
     &fake_rn(0))
      end do
      end if
      end if
      end do
      end do
      end do
      end do
      end do
      return
      end
      subroutine post_process_source_scores(is,estimator_factors)
      
      use pr_mod
      
      use so_mod
      
      use sp_mod
      
      use bk_mod
      
      use gi_mod
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer is
      REAL(kind=DOUBLE)estimator_factors(*)
      integer i,bk_rc_is,seg,seg_tot,kseg,xseg,est
      integer species_fake_pt
      integer test_fake_pt
      REAL(kind=DOUBLE)time_fake_pt,weight_fake_pt,velocity_fake_pt(3)
      REAL(kind=DOUBLE)pos_fake_pt(3)
      integer cell_fake_pt,zone_fake_pt,surface_fake_pt,cell_next_fake_p
     &t,zone_next_fake_pt,sector_fake_pt,sector_next_fake_pt
      integer type_fake_pt,author_fake_pt
      integer ran_index_fake_rn
      REAL(kind=DOUBLE)ran_array_fake_rn(0:100-1)
      integer number_fake_fl,source_fake_fl,source_kseg_fake_fl,source_x
     &seg_fake_fl,source_type_fake_fl,source_root_sp_fake_fl,pointer_fak
     &e_fl
      integer species_origin_fake_fl
      integer test_origin_fake_fl
      REAL(kind=DOUBLE)time_origin_fake_fl,weight_origin_fake_fl,velocit
     &y_origin_fake_fl(3)
      REAL(kind=DOUBLE)pos_origin_fake_fl(3)
      integer cell_origin_fake_fl,zone_origin_fake_fl,surface_origin_fak
     &e_fl,cell_next_origin_fake_fl,zone_next_origin_fake_fl,sector_orig
     &in_fake_fl,sector_next_origin_fake_fl
      integer type_origin_fake_fl,author_origin_fake_fl
      integer ran_index_fake_fl
      REAL(kind=DOUBLE)ran_array_fake_fl(0:100-1)
      integer species_stack_fake_fl(40)
      integer test_stack_fake_fl(40)
      REAL(kind=DOUBLE)time_stack_fake_fl(40),weight_stack_fake_fl(40),v
     &elocity_stack_fake_fl(3,40)
      REAL(kind=DOUBLE)pos_stack_fake_fl(3,40)
      integer cell_stack_fake_fl(40),zone_stack_fake_fl(40),surface_stac
     &k_fake_fl(40),cell_next_stack_fake_fl(40),zone_next_stack_fake_fl(
     &40),sector_stack_fake_fl(40),sector_next_stack_fake_fl(40)
      integer type_stack_fake_fl(40),author_stack_fake_fl(40)
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
      if(source_type(is).EQ.5)return
      do seg=1,source_num_segments(is)
      seg_tot=source_base_ptr(is)+seg-1
      kseg=seg-1
      xseg=source_segment_ptr(source_base_ptr(is)+kseg)
      est=3
      species_fake_pt=string_lookup(species_sy(source_species(is)),speci
     &es_sy,sp_num)
      test_fake_pt=problem_species_test(species_fake_pt)
      if(species_m(species_fake_pt).EQ.0)then
      type_fake_pt=0
      else if(species_z(species_fake_pt).EQ.0)then
      type_fake_pt=1
      author_fake_pt=source_type(is)
      else
      type_fake_pt=2
      author_fake_pt=source_type(is)
      end if
      weight_fake_pt=(1.0_DOUBLE)
      weight_fake_pt=source_scale_factor(is)*source_current(seg_tot)
      time_fake_pt=(0.0_DOUBLE)
      call set_source_x(est,is,xseg,kseg,ran_index_fake_rn,ran_array_fak
     &e_rn(0),pos_fake_pt(1),cell_fake_pt,zone_fake_pt,surface_fake_pt,c
     &ell_next_fake_pt,zone_next_fake_pt,sector_fake_pt,sector_next_fake
     &_pt)
      if(source_type(is).EQ.3)then
      bk_rc_is=0
      do i=1,pr_bkrc_num
      if(problem_bkrc_products(1,i).EQ.source_species(is))then
      if(bk_rc_is.EQ.0)continue
      bk_rc_is=i
      end if
      end do
      if(bk_rc_is.NE.0)continue
      if((geometry_symmetry.EQ.2.OR.geometry_symmetry.EQ.5.OR.geometry_s
     &ymmetry.EQ.6).AND.(background_coords.EQ.2).AND.(pos_fake_pt(1)**2+
     &pos_fake_pt(2)**2.GT.(0.0_DOUBLE)))then
      velocity_fake_pt(1)=(background_v(1,problem_species_background(sou
     &rce_root_species(is)),zone_pointer(zone_fake_pt))*pos_fake_pt(1)-b
     &ackground_v(2,problem_species_background(source_root_species(is)),
     &zone_pointer(zone_fake_pt))*pos_fake_pt(2))/sqrt(pos_fake_pt(1)**2
     &+pos_fake_pt(2)**2)
      velocity_fake_pt(2)=(background_v(1,problem_species_background(sou
     &rce_root_species(is)),zone_pointer(zone_fake_pt))*pos_fake_pt(2)+b
     &ackground_v(2,problem_species_background(source_root_species(is)),
     &zone_pointer(zone_fake_pt))*pos_fake_pt(1))/sqrt(pos_fake_pt(1)**2
     &+pos_fake_pt(2)**2)
      velocity_fake_pt(3)=background_v(3,problem_species_background(sour
     &ce_root_species(is)),zone_pointer(zone_fake_pt))
      else
      if(.NOT.((geometry_symmetry.EQ.0).AND.(background_coords.EQ.2)))co
     &ntinue
      velocity_fake_pt(1)=background_v(1,problem_species_background(sour
     &ce_root_species(is)),zone_pointer(zone_fake_pt))
      velocity_fake_pt(2)=background_v(2,problem_species_background(sour
     &ce_root_species(is)),zone_pointer(zone_fake_pt))
      velocity_fake_pt(3)=background_v(3,problem_species_background(sour
     &ce_root_species(is)),zone_pointer(zone_fake_pt))
      
      end if
      else
      velocity_fake_pt(1)=(0.0_DOUBLE)
      velocity_fake_pt(2)=(0.0_DOUBLE)
      velocity_fake_pt(3)=(0.0_DOUBLE)
      end if
      pointer_fake_fl=1
      species_stack_fake_fl(pointer_fake_fl)=species_fake_pt
      test_stack_fake_fl(pointer_fake_fl)=test_fake_pt
      time_stack_fake_fl(pointer_fake_fl)=time_fake_pt
      weight_stack_fake_fl(pointer_fake_fl)=weight_fake_pt
      velocity_stack_fake_fl(1,pointer_fake_fl)=velocity_fake_pt(1)
      velocity_stack_fake_fl(2,pointer_fake_fl)=velocity_fake_pt(2)
      velocity_stack_fake_fl(3,pointer_fake_fl)=velocity_fake_pt(3)
      
      pos_stack_fake_fl(1,pointer_fake_fl)=pos_fake_pt(1)
      pos_stack_fake_fl(2,pointer_fake_fl)=pos_fake_pt(2)
      pos_stack_fake_fl(3,pointer_fake_fl)=pos_fake_pt(3)
      
      cell_stack_fake_fl(pointer_fake_fl)=cell_fake_pt
      zone_stack_fake_fl(pointer_fake_fl)=zone_fake_pt
      surface_stack_fake_fl(pointer_fake_fl)=surface_fake_pt
      cell_next_stack_fake_fl(pointer_fake_fl)=cell_next_fake_pt
      zone_next_stack_fake_fl(pointer_fake_fl)=zone_next_fake_pt
      sector_stack_fake_fl(pointer_fake_fl)=sector_fake_pt
      sector_next_stack_fake_fl(pointer_fake_fl)=sector_next_fake_pt
      type_stack_fake_fl(pointer_fake_fl)=type_fake_pt
      author_stack_fake_fl(pointer_fake_fl)=author_fake_pt
      source_fake_fl=is
      source_kseg_fake_fl=kseg
      source_xseg_fake_fl=xseg
      source_type_fake_fl=source_type(is)
      source_root_sp_fake_fl=source_root_species(is)
      call score_sources(est,number_fake_fl,source_fake_fl,source_kseg_f
     &ake_fl,source_xseg_fake_fl,source_type_fake_fl,source_root_sp_fake
     &_fl,species_origin_fake_fl,test_origin_fake_fl,time_origin_fake_fl
     &,weight_origin_fake_fl,pos_origin_fake_fl(1),cell_origin_fake_fl,z
     &one_origin_fake_fl,surface_origin_fake_fl,cell_next_origin_fake_fl
     &,zone_next_origin_fake_fl,sector_origin_fake_fl,sector_next_origin
     &_fake_fl,velocity_origin_fake_fl(1),type_origin_fake_fl,author_ori
     &gin_fake_fl,ran_index_fake_fl,ran_array_fake_fl(0),species_stack_f
     &ake_fl(1),test_stack_fake_fl(1),time_stack_fake_fl(1),weight_stack
     &_fake_fl(1),pos_stack_fake_fl(1,1),cell_stack_fake_fl(1),zone_stac
     &k_fake_fl(1),surface_stack_fake_fl(1),cell_next_stack_fake_fl(1),z
     &one_next_stack_fake_fl(1),sector_stack_fake_fl(1),sector_next_stac
     &k_fake_fl(1),velocity_stack_fake_fl(1,1),type_stack_fake_fl(1),aut
     &hor_stack_fake_fl(1),pointer_fake_fl,estimator_factors)
      end do
      return
      end
      subroutine score_sources(est,number_x,source_x,source_kseg_x,sourc
     &e_xseg_x,source_type_x,source_root_sp_x,species_origin_x,test_orig
     &in_x,time_origin_x,weight_origin_x,pos_origin_x,cell_origin_x,zone
     &_origin_x,surface_origin_x,cell_next_origin_x,zone_next_origin_x,s
     &ector_origin_x,sector_next_origin_x,velocity_origin_x,type_origin_
     &x,author_origin_x,ran_index_x,ran_array_x,species_stack_x,test_sta
     &ck_x,time_stack_x,weight_stack_x,pos_stack_x,cell_stack_x,zone_sta
     &ck_x,surface_stack_x,cell_next_stack_x,zone_next_stack_x,sector_st
     &ack_x,sector_next_stack_x,velocity_stack_x,type_stack_x,author_sta
     &ck_x,pointer_x,estimator_factors)
      
      use pr_mod
      
      use tl_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer est
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
      integer kseg,xseg,i,jscore,prso_reac,type
      integer is
      REAL(kind=DOUBLE)scoring_data(280)
      REAL(kind=DOUBLE)est_fac
      logical need_scores
      REAL(kind=DOUBLE)random
      external random
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
      
      
      is=source_x
      kseg=source_kseg_x
      xseg=source_xseg_x
      type=source_type_x
      prso_reac=pr_reaction_num+type
      est_fac=(1.0_DOUBLE)
      need_scores=.FALSE.
      do i=1,tally_type_num(3)
      jscore=tally_type_base(3)+i-1
      estimator_factors(jscore)=tally_est_reaction(prso_reac,est,jscore)
     &*weight_stack_x(pointer_x)*est_fac
      if(estimator_factors(jscore).GT.(0.0_DOUBLE))then
      need_scores=.TRUE.
      end if
      end do
      if(need_scores)then
      do i=1,280
      scoring_data(i)=(0.0_DOUBLE)
      end do
      if(type.EQ.3)then
      call recombination_data(est,number_x,source_x,source_kseg_x,source
     &_xseg_x,source_type_x,source_root_sp_x,species_origin_x,test_origi
     &n_x,time_origin_x,weight_origin_x,pos_origin_x(1),cell_origin_x,zo
     &ne_origin_x,surface_origin_x,cell_next_origin_x,zone_next_origin_x
     &,sector_origin_x,sector_next_origin_x,velocity_origin_x(1),type_or
     &igin_x,author_origin_x,ran_index_x,ran_array_x(0),species_stack_x(
     &1),test_stack_x(1),time_stack_x(1),weight_stack_x(1),pos_stack_x(1
     &,1),cell_stack_x(1),zone_stack_x(1),surface_stack_x(1),cell_next_s
     &tack_x(1),zone_next_stack_x(1),sector_stack_x(1),sector_next_stack
     &_x(1),velocity_stack_x(1,1),type_stack_x(1),author_stack_x(1),poin
     &ter_x,scoring_data)
      else if((type.EQ.1).OR.(type.EQ.6))then
      call plate_data(est,number_x,source_x,source_kseg_x,source_xseg_x,
     &source_type_x,source_root_sp_x,species_origin_x,test_origin_x,time
     &_origin_x,weight_origin_x,pos_origin_x(1),cell_origin_x,zone_origi
     &n_x,surface_origin_x,cell_next_origin_x,zone_next_origin_x,sector_
     &origin_x,sector_next_origin_x,velocity_origin_x(1),type_origin_x,a
     &uthor_origin_x,ran_index_x,ran_array_x(0),species_stack_x(1),test_
     &stack_x(1),time_stack_x(1),weight_stack_x(1),pos_stack_x(1,1),cell
     &_stack_x(1),zone_stack_x(1),surface_stack_x(1),cell_next_stack_x(1
     &),zone_next_stack_x(1),sector_stack_x(1),sector_next_stack_x(1),ve
     &locity_stack_x(1,1),type_stack_x(1),author_stack_x(1),pointer_x,sc
     &oring_data)
      else if(type.EQ.2)then
      call puff_data(est,number_x,source_x,source_kseg_x,source_xseg_x,s
     &ource_type_x,source_root_sp_x,species_origin_x,test_origin_x,time_
     &origin_x,weight_origin_x,pos_origin_x(1),cell_origin_x,zone_origin
     &_x,surface_origin_x,cell_next_origin_x,zone_next_origin_x,sector_o
     &rigin_x,sector_next_origin_x,velocity_origin_x(1),type_origin_x,au
     &thor_origin_x,ran_index_x,ran_array_x(0),species_stack_x(1),test_s
     &tack_x(1),time_stack_x(1),weight_stack_x(1),pos_stack_x(1,1),cell_
     &stack_x(1),zone_stack_x(1),surface_stack_x(1),cell_next_stack_x(1)
     &,zone_next_stack_x(1),sector_stack_x(1),sector_next_stack_x(1),vel
     &ocity_stack_x(1,1),type_stack_x(1),author_stack_x(1),pointer_x,sco
     &ring_data)
      else if(type.EQ.4)then
      call vol_source_data(est,number_x,source_x,source_kseg_x,source_xs
     &eg_x,source_type_x,source_root_sp_x,species_origin_x,test_origin_x
     &,time_origin_x,weight_origin_x,pos_origin_x(1),cell_origin_x,zone_
     &origin_x,surface_origin_x,cell_next_origin_x,zone_next_origin_x,se
     &ctor_origin_x,sector_next_origin_x,velocity_origin_x(1),type_origi
     &n_x,author_origin_x,ran_index_x,ran_array_x(0),species_stack_x(1),
     &test_stack_x(1),time_stack_x(1),weight_stack_x(1),pos_stack_x(1,1)
     &,cell_stack_x(1),zone_stack_x(1),surface_stack_x(1),cell_next_stac
     &k_x(1),zone_next_stack_x(1),sector_stack_x(1),sector_next_stack_x(
     &1),velocity_stack_x(1,1),type_stack_x(1),author_stack_x(1),pointer
     &_x,scoring_data)
      else if(type.EQ.5)then
      call snapshot_data(est,number_x,source_x,source_kseg_x,source_xseg
     &_x,source_type_x,source_root_sp_x,species_origin_x,test_origin_x,t
     &ime_origin_x,weight_origin_x,pos_origin_x(1),cell_origin_x,zone_or
     &igin_x,surface_origin_x,cell_next_origin_x,zone_next_origin_x,sect
     &or_origin_x,sector_next_origin_x,velocity_origin_x(1),type_origin_
     &x,author_origin_x,ran_index_x,ran_array_x(0),species_stack_x(1),te
     &st_stack_x(1),time_stack_x(1),weight_stack_x(1),pos_stack_x(1,1),c
     &ell_stack_x(1),zone_stack_x(1),surface_stack_x(1),cell_next_stack_
     &x(1),zone_next_stack_x(1),sector_stack_x(1),sector_next_stack_x(1)
     &,velocity_stack_x(1,1),type_stack_x(1),author_stack_x(1),pointer_x
     &,scoring_data)
      end if
      call add_scores(3,species_stack_x(pointer_x),test_stack_x(pointer_
     &x),time_stack_x(pointer_x),weight_stack_x(pointer_x),pos_stack_x(1
     &,pointer_x),cell_stack_x(pointer_x),zone_stack_x(pointer_x),surfac
     &e_stack_x(pointer_x),cell_next_stack_x(pointer_x),zone_next_stack_
     &x(pointer_x),sector_stack_x(pointer_x),sector_next_stack_x(pointer
     &_x),velocity_stack_x(1,pointer_x),type_stack_x(pointer_x),author_s
     &tack_x(pointer_x),prso_reac,scoring_data,estimator_factors)
      end if
      return
      end
      subroutine build_snapshot_pdf(species_p,test_p,time_p,weight_p,pos
     &_p,cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,sector
     &_next_p,velocity_p,type_p,author_p)
      
      use sn_mod
      
      use sa_mod
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer species_p
      integer test_p
      REAL(kind=DOUBLE)time_p,weight_p,velocity_p(3)
      REAL(kind=DOUBLE)pos_p(3)
      integer cell_p,zone_p,surface_p,cell_next_p,zone_next_p,sector_p,s
     &ector_next_p
      integer type_p,author_p
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
      
      
      if(.NOT.((zone_type(zone_p).EQ.2).OR.(zone_type(zone_p).EQ.1)))the
     &n
      write(6,*)pos_p(1),pos_p(2),pos_p(3)
      if(' Bad snapshot particle'.EQ.' ')continue
      end if
      sn_number_particles=sn_number_particles+1
      if(sn_number_particles.GT.sn_particles_dim)then
      sn_particles_dim=max(sn_particles_dim,sn_number_particles)
      if(mod(((sn_particles_dim)-(1)+1),100).EQ.1)then
      sn_particles_float =>mem_realloc_r2(sn_particles_float,(1),(8),(1)
     &,(((int(((((sn_particles_dim)-(1)+1))+100-1)/100)*100)-100)+(1)-1)
     &,((int(((((sn_particles_dim)-(1)+1))+100-1)/100)*100)+(1)-1),'sn_p
     &articles_float')
      end if
      if(mod(((sn_particles_dim)-(1)+1),100).EQ.1)then
      sn_particles_int =>mem_realloc_i2(sn_particles_int,(1),(11),(1),((
     &(int(((((sn_particles_dim)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((
     &int(((((sn_particles_dim)-(1)+1))+100-1)/100)*100)+(1)-1),'sn_part
     &icles_int')
      end if
      sn_particles_dim=((int(((((sn_particles_dim)-(1)+1))+100-1)/100)*1
     &00)+(1)-1)
      end if
      sn_particles_int(1,sn_number_particles)=species_p
      sn_particles_int(2,sn_number_particles)=test_p
      sn_particles_float(1,sn_number_particles)=time_p
      sn_particles_float(2,sn_number_particles)=weight_p*stat_wt_tot_flt
      sn_particles_float(3,sn_number_particles)=velocity_p(1)
      sn_particles_float(4,sn_number_particles)=velocity_p(2)
      sn_particles_float(5,sn_number_particles)=velocity_p(3)
      sn_particles_float(6,sn_number_particles)=pos_p(1)
      sn_particles_float(7,sn_number_particles)=pos_p(2)
      sn_particles_float(8,sn_number_particles)=pos_p(3)
      sn_particles_int(3,sn_number_particles)=cell_p
      sn_particles_int(4,sn_number_particles)=zone_p
      sn_particles_int(5,sn_number_particles)=surface_p
      sn_particles_int(6,sn_number_particles)=cell_next_p
      sn_particles_int(7,sn_number_particles)=zone_next_p
      sn_particles_int(8,sn_number_particles)=sector_p
      sn_particles_int(9,sn_number_particles)=sector_next_p
      sn_particles_int(10,sn_number_particles)=type_p
      sn_particles_int(11,sn_number_particles)=author_p
      return
      end
      
      
