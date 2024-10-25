      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      program tallysetup
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      include 'mpif.h'
      integer mpi_err
      integer mpi_status
      dimension mpi_status(MPI_STATUS_SIZE)
      call MPI_init(mpi_err)
      mpi_degas2_root=0
      call MPI_Comm_dup(MPI_COMM_WORLD,comm_world_dup,mpi_err)
      call MPI_comm_rank(comm_world_dup,mpi_rank,mpi_err)
      call MPI_comm_size(comm_world_dup,mpi_size,mpi_err)
      call readfilenames
      call read_geometry
      call nc_read_elements
      call nc_read_species
      call nc_read_reactions
      call nc_read_materials
      call nc_read_pmi
      call nc_read_problem
      call init_tally
      call nc_write_tally
      call clear_tally
      call MPI_barrier(comm_world_dup,mpi_err)
      call MPI_finalize(mpi_err)
      stop
      end
      subroutine init_tally
      
      
      
      use de_mod
      
      use zn_mod
      
      use rc_mod
      
      use pr_mod
      
      use tl_mod
      
      use sp_mod
      
      use sc_mod
      
      use rf_mod
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer pointer,i,icv,test,pr_reac,num_conversions,type,geometry,s
     &ub_section,b,e,p,rank,length,generic,num_equiv
      integer indep_var(5),tab_index(5),conversions(5),estimator(13),con
     &version_packages(7,9),equivalents(11)
      character*1 est_name(0:4)
      character*40 cv_name(9)
      character*96 filename,name,name_equiv,dep_var,dep_var_equiv
      character*300 line
      integer,dimension(:),pointer::pr_rc_type
      integer,dimension(:),pointer::var_num
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
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
      
      
      tally_type_num =>mem_alloc_i1((1),(3),'tally_type_num')
      tally_type_base =>mem_alloc_i1((1),(3),'tally_type_base')
      pr_rc_type =>mem_alloc_i1((1),(pr_reaction_dim+6),'pr_rc_type')
      var_num =>mem_alloc_i1((0),(100),'var_num')
      tl_num=0
      do i=1,3
      tally_type_num(i)=0
      tally_type_base(i)=2000000000
      end do
      tally_size=0
      if(200.GE.max(sc_diag_max_bins,de_max_bins))continue
      if(280.GT.pr_var0_num)continue
      do pr_reac=1,pr_reaction_num+6
      pr_rc_type(pr_reac)=13
      end do
      if(pr_reaction_num.GT.0)then
      do pr_reac=1,pr_reaction_num
      if(reaction_type(problem_rc(pr_reac)).EQ.'ionize')then
      pr_rc_type(pr_reac)=1
      else if(reaction_type(problem_rc(pr_reac)).EQ.'ionize_suppress')th
     &en
      pr_rc_type(pr_reac)=2
      else if(reaction_type(problem_rc(pr_reac)).EQ.'chargex')then
      pr_rc_type(pr_reac)=3
      else if(reaction_type(problem_rc(pr_reac)).EQ.'elastic')then
      pr_rc_type(pr_reac)=4
      else if(reaction_type(problem_rc(pr_reac)) (1:6).EQ.'dissoc')then
      pr_rc_type(pr_reac)=5
      else if(reaction_type(problem_rc(pr_reac)).EQ.'excitation')then
      pr_rc_type(pr_reac)=1
      else if(reaction_type(problem_rc(pr_reac)).EQ.'deexcitation')then
      pr_rc_type(pr_reac)=1
      else if(reaction_type(problem_rc(pr_reac)).EQ.'ion_conv')then
      pr_rc_type(pr_reac)=6
      else if(reaction_type(problem_rc(pr_reac)).EQ.'recombination')then
      pr_rc_type(pr_reac)=9
      else
      pr_rc_type(pr_reac)=13
      end if
      end do
      do test=1,pr_test_num
      if(species_z(problem_test_sp(test)).NE.0)then
      do i=1,problem_reaction_num(test)
      pr_rc_type(problem_test_reaction(i,test))=6
      end do
      end if
      end do
      end if
      pr_rc_type(pr_reaction_num+1)=7
      pr_rc_type(pr_reaction_num+2)=8
      pr_rc_type(pr_reaction_num+3)=9
      pr_rc_type(pr_reaction_num+4)=10
      pr_rc_type(pr_reaction_num+5)=11
      pr_rc_type(pr_reaction_num+6)=12
      nconversions=0
      do icv=1,5
      conversions(icv)=0
      end do
      call set_conversion_packages(conversion_packages,cv_name)
      call set_var_list(var_num)
      est_name(0)='N'
      est_name(1)='T'
      est_name(2)='C'
      est_name(3)='P'
      est_name(4)='S'
      tally_version='$Id: 59666ba1f75ad467f4020a4e901c52e1ede99c33 $'
      filename=filenames_array(22)
      if(filename.NE.'undefined')continue
      open(unit=20,file=filename,form='formatted',status='old')
      type=0
      sub_section=1
90001 continue
      if(.NOT.read_string(20,line,length))go to 90004
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(line.EQ.'SECTOR TALLIES')then
      type=1
      else if(line.EQ.'TEST TALLIES')then
      type=2
      else if(line.EQ.'REACTION TALLIES')then
      type=3
      else if(line.EQ.'DEPENDENT VARIABLE')then
      if(sub_section.NE.6)sub_section=2
      else if(line.EQ.'GEOMETRY OPERATOR')then
      if(sub_section.NE.6)sub_section=7
      else if(line.EQ.'INDEPENDENT VARIABLES')then
      if(sub_section.NE.6)sub_section=3
      else if(line.EQ.'CONVERSIONS')then
      if(sub_section.NE.6)sub_section=4
      else if(line.EQ.'ESTIMATORS')then
      if(sub_section.NE.6)sub_section=5
      else if(line(1:1).EQ.'-')then
      if(sub_section.NE.6)then
      do i=1,rank
      if(indep_var(i).EQ.5)then
      if(pointer.GT.0.AND.pointer.LE.de_grps)continue
      if(type.EQ.3)continue
      tab_index(i)=detector_num_views(pointer)
      else if(indep_var(i).EQ.17)then
      if(pointer.GT.0.AND.pointer.LE.sc_diagnostic_grps)continue
      if(type.EQ.1)continue
      tab_index(i)=diagnostic_num_sectors(pointer)
      else if(indep_var(i).EQ.14.OR.indep_var(i).EQ.15)then
      if(pointer.GT.0.AND.pointer.LE.sc_diagnostic_grps)continue
      if(type.EQ.1)continue
      tab_index(i)=diagnostic_tab_index(pointer)
      else if(indep_var(i).EQ.16)then
      if(pointer.GT.0.AND.pointer.LE.de_grps)continue
      if(type.EQ.3)continue
      tab_index(i)=detector_tab_index(pointer)
      else
      tab_index(i)=var_num(indep_var(i))
      end if
      if(tab_index(i).LE.0)then
      write(6,*)line(:length)
      if(' Do not have size of this independent variable'.EQ.' ')continu
     &e
      end if
      end do
      if(index(name,'[').EQ.0)then
      call define_tally(name,type,geometry,pointer,dep_var,rank,indep_va
     &r,tab_index,num_conversions,conversions,conversion_packages,estima
     &tor,pr_rc_type)
      else
      b=index(name,'[')+1
      e=index(name,']')-1
      generic=string_lookup(name(b:e),species_sy,sp_num)
      num_equiv=0
      do i=1,pr_test_num
      if(species_generic(problem_test_sp(i)).EQ.generic)then
      num_equiv=num_equiv+1
      equivalents(num_equiv)=i
      end if
      end do
      if(num_equiv.GT.0)continue
      do i=1,num_equiv
      if(b.EQ.2)then
      name_equiv=species_sy(problem_test_sp(equivalents(i))) (1:string_l
     &ength(species_sy(problem_test_sp(equivalents(i)))))//name(e+2:)
      else
      name_equiv=name(:b-2)//species_sy(problem_test_sp(equivalents(i)))
     & (1:string_length(species_sy(problem_test_sp(equivalents(i)))))//n
     &ame(e+2:)
      end if
      if(index(dep_var,'[').GT.0)then
      b=index(dep_var,'[')+1
      e=index(dep_var,']')-1
      if(string_lookup(dep_var(b:e),species_sy,sp_num).NE.generic)then
      if(' Dep. var. generic species must match one in name'.EQ.' ')cont
     &inue
      end if
      if(b.EQ.2)then
      dep_var_equiv=species_sy(problem_test_sp(equivalents(i))) (1:strin
     &g_length(species_sy(problem_test_sp(equivalents(i)))))//dep_var(e+
     &2:)
      else
      dep_var_equiv=dep_var(:b-2)//species_sy(problem_test_sp(equivalent
     &s(i))) (1:string_length(species_sy(problem_test_sp(equivalents(i))
     &)))//dep_var(e+2:)
      end if
      else
      dep_var_equiv=dep_var
      end if
      call define_tally(name_equiv,type,geometry,pointer,dep_var_equiv,r
     &ank,indep_var,tab_index,num_conversions,conversions,conversion_pac
     &kages,estimator,pr_rc_type)
      end do
      end if
      
      end if
      sub_section=1
      else
      if(sub_section.EQ.1)then
      name=line(:length)
      dep_var='UNINITIALIZED'
      rank=0
      num_conversions=0
      pointer=0
      geometry=1
      if(type.EQ.1)then
      estimator(1)=2
      else if(type.EQ.2)then
      estimator(1)=1
      else if(type.EQ.3)then
      do i=1,13
      estimator(i)=2
      end do
      end if
      else if(sub_section.EQ.2)then
      dep_var=line(:length)
      else if(sub_section.EQ.7)then
      if(type.EQ.1)then
      geometry=2
      pointer=string_lookup(line(:length),diagnostic_grp_name(1),sc_diag
     &nostic_grps)
      if(pointer.LE.0.OR.pointer.GT.sc_diagnostic_grps)then
      sub_section=6
      write(6,*)' Tally ',name  (1:string_length(name)),' skipped since 
     &diagnostic ',line  (1:string_length(line)),' does not appear in th
     &is problem.'
      go to 90001
      end if
      else if(type.EQ.3)then
      geometry=3
      pointer=string_lookup(line(:length),detector_name(1),de_grps)
      if(pointer.LE.0.OR.pointer.GT.de_grps)then
      sub_section=6
      write(6,*)' Tally ',name  (1:string_length(name)),' skipped since 
     &detector ',line  (1:string_length(line)),' does not appear in this
     & problem.'
      go to 90001
      end if
      else
      if(' Only volume geometry available for test tallies'.EQ.' ')conti
     &nue
      end if
      else if(sub_section.EQ.3)then
      rank=rank+1
      if(next_token(line(:length),b,e,p))continue
      indep_var(rank)=string_lookup(line(b:e),tally_var_list(1),100)
      if(indep_var(rank).LE.0.OR.indep_var(rank).GT.100)then
      write(6,*)line(b:e)
      if(' Unknown independent variable'.EQ.' ')continue
      end if
      
      else if(sub_section.EQ.4)then
      if(next_token(line(:length),b,e,p))continue
      num_conversions=num_conversions+1
      conversions(num_conversions)=string_lookup(line(b:e),cv_name,9)
      if(conversions(num_conversions).LE.0.OR.conversions(num_conversion
     &s).GT.9)then
      write(6,*)line(b:e)
      if(' Unknown conversion'.EQ.' ')continue
      end if
      if(next_token(line(:length),b,e,p))then
      if(line(b:e).EQ.':')continue
      if(next_token(line(:length),b,e,p))continue
      conversion_packages(6,conversions(num_conversions))=string_lookup(
     &line(b:),tally_name,tl_num)
      if(conversion_packages(6,conversions(num_conversions)).GT.0)contin
     &ue
      end if
      
      else if(sub_section.EQ.5)then
      if(type.EQ.1)then
      if(' Should not be a sector ESTIMATOR section'.EQ.' ')continue
      else if(type.EQ.2)then
      if(next_token(line(:length),b,e,p))continue
      estimator(1)=string_lookup(line(b:e),est_name(1),4)
      if(estimator(1).LE.0.OR.estimator(1).GT.4)then
      write(6,*)line(b:e)
      if(' Unknown estimator'.EQ.' ')continue
      end if
      else if(type.EQ.3)then
      do i=1,13
      if(next_token(line(:length),b,e,p))then
      estimator(i)=string_lookup(line(b:e),est_name(1),4)
      if(estimator(i).LT.0.OR.estimator(i).GT.4)then
      write(6,*)line(b:e)
      if(' Unknown estimator'.EQ.' ')continue
      else if(estimator(i).EQ.4)then
      if(' No snapshot estimators for reactions'.EQ.' ')continue
      else if(estimator(i).EQ.0)then
      if(line(b:e).EQ.'N')continue
      end if
      else
      estimator(i)=2
      end if
      end do
      if(estimator(2).EQ.2)then
      if(' Collision estimator not possible for ionize supress'.EQ.' ')c
     &ontinue
      else if(estimator(9).EQ.1.OR.estimator(7).EQ.1.OR.estimator(8).EQ.
     &1.OR.estimator(10).EQ.1.OR.estimator(11).EQ.1.OR.estimator(12).EQ.
     &1)then
      if(' Track length estimator not possible for sources'.EQ.' ')conti
     &nue
      else if(estimator(6).NE.2)then
      if(' Test ions must use collision estimator'.EQ.' ')continue
      else if(int_lookup(16,indep_var,rank).GT.0)then
      do i=1,13
      if(estimator(i).EQ.3.AND.i.NE.9)then
      if(' Spectrum cannot done with post processing estimator'.EQ.' ')c
     &ontinue
      end if
      end do
      end if
      end if
      
      else if(sub_section.EQ.6)then
      end if
      end if
      go to 90001
90004 continue
      if(mod(((tl_num)-(1)+1),100).NE.0)then
      tally_name =>mem_realloc_c1(tally_name,(80),(1),((int(((((tl_num)-
     &(1)+1))+100-1)/100)*100)+(1)-1),(tl_num),'tally_name')
      end if
      if(mod(((tl_num)-(1)+1),100).NE.0)then
      tally_type =>mem_realloc_i1(tally_type,(1),((int(((((tl_num)-(1)+1
     &))+100-1)/100)*100)+(1)-1),(tl_num),'tally_type')
      end if
      if(mod(((tl_num)-(1)+1),100).NE.0)then
      tally_geometry =>mem_realloc_i1(tally_geometry,(1),((int(((((tl_nu
     &m)-(1)+1))+100-1)/100)*100)+(1)-1),(tl_num),'tally_geometry')
      end if
      if(mod(((tl_num)-(1)+1),100).NE.0)then
      tally_geometry_ptr =>mem_realloc_i1(tally_geometry_ptr,(1),((int((
     &(((tl_num)-(1)+1))+100-1)/100)*100)+(1)-1),(tl_num),'tally_geometr
     &y_ptr')
      end if
      if(mod(((tl_num)-(1)+1),100).NE.0)then
      tally_rank =>mem_realloc_i1(tally_rank,(1),((int(((((tl_num)-(1)+1
     &))+100-1)/100)*100)+(1)-1),(tl_num),'tally_rank')
      end if
      if(mod(((tl_num)-(1)+1),100).NE.0)then
      tally_dep_var_dim =>mem_realloc_i1(tally_dep_var_dim,(1),((int((((
     &(tl_num)-(1)+1))+100-1)/100)*100)+(1)-1),(tl_num),'tally_dep_var_d
     &im')
      end if
      if(mod(((tl_num)-(1)+1),100).NE.0)then
      tally_indep_var =>mem_realloc_i2(tally_indep_var,(1),(5),(1),((int
     &(((((tl_num)-(1)+1))+100-1)/100)*100)+(1)-1),(tl_num),'tally_indep
     &_var')
      end if
      if(mod(((tl_num)-(1)+1),100).NE.0)then
      tally_dep_var =>mem_realloc_i1(tally_dep_var,(1),((int(((((tl_num)
     &-(1)+1))+100-1)/100)*100)+(1)-1),(tl_num),'tally_dep_var')
      end if
      if(mod(((tl_num)-(1)+1),100).NE.0)then
      tally_base =>mem_realloc_i1(tally_base,(1),((int(((((tl_num)-(1)+1
     &))+100-1)/100)*100)+(1)-1),(tl_num),'tally_base')
      end if
      if(mod(((tl_num)-(1)+1),100).NE.0)then
      tally_tab_index =>mem_realloc_i2(tally_tab_index,(1),(5),(1),((int
     &(((((tl_num)-(1)+1))+100-1)/100)*100)+(1)-1),(tl_num),'tally_tab_i
     &ndex')
      end if
      if(mod(((tl_num)-(1)+1),100).NE.0)then
      tally_est_test =>mem_realloc_r2(tally_est_test,(1),(4),(1),((int((
     &(((tl_num)-(1)+1))+100-1)/100)*100)+(1)-1),(tl_num),'tally_est_tes
     &t')
      end if
      if(mod(((tl_num)-(1)+1),100).NE.0)then
      tally_est_reaction =>mem_realloc_r3(tally_est_reaction,(1),(pr_rea
     &ction_dim+6),(1),(4),(1),((int(((((tl_num)-(1)+1))+100-1)/100)*100
     &)+(1)-1),(tl_num),'tally_est_reaction')
      end if
      if(mod(((tl_num)-(1)+1),100).NE.0)then
      tally_num_conversions =>mem_realloc_i1(tally_num_conversions,(1),(
     &(int(((((tl_num)-(1)+1))+100-1)/100)*100)+(1)-1),(tl_num),'tally_n
     &um_conversions')
      end if
      if(mod(((tl_num)-(1)+1),100).NE.0)then
      tally_cv_ptr =>mem_realloc_i2(tally_cv_ptr,(1),(5),(1),((int(((((t
     &l_num)-(1)+1))+100-1)/100)*100)+(1)-1),(tl_num),'tally_cv_ptr')
      end if
      if(mod(((nconversions)-(1)+1),100).NE.0)then
      tally_cv_action =>mem_realloc_i1(tally_cv_action,(1),((int(((((nco
     &nversions)-(1)+1))+100-1)/100)*100)+(1)-1),(nconversions),'tally_c
     &v_action')
      end if
      if(mod(((nconversions)-(1)+1),100).NE.0)then
      tally_cv_type =>mem_realloc_i1(tally_cv_type,(1),((int(((((nconver
     &sions)-(1)+1))+100-1)/100)*100)+(1)-1),(nconversions),'tally_cv_ty
     &pe')
      end if
      if(mod(((nconversions)-(1)+1),100).NE.0)then
      tally_cv_num_partners =>mem_realloc_i1(tally_cv_num_partners,(1),(
     &(int(((((nconversions)-(1)+1))+100-1)/100)*100)+(1)-1),(nconversio
     &ns),'tally_cv_num_partners')
      end if
      if(mod(((nconversions)-(1)+1),100).NE.0)then
      tally_cv_scalers =>mem_realloc_i2(tally_cv_scalers,(1),(3),(1),((i
     &nt(((((nconversions)-(1)+1))+100-1)/100)*100)+(1)-1),(nconversions
     &),'tally_cv_scalers')
      end if
      if(mod(((nconversions)-(1)+1),100).NE.0)then
      tally_cv_partners =>mem_realloc_i2(tally_cv_partners,(1),(3),(1),(
     &(int(((((nconversions)-(1)+1))+100-1)/100)*100)+(1)-1),(nconversio
     &ns),'tally_cv_partners')
      end if
      call mem_free_i1(pr_rc_type,(1),(pr_reaction_dim+6),'pr_rc_type')
      return
      end
      subroutine define_tally(name,type,geometry,pointer,dep_var,rank,in
     &dep_var,tab_index,num_conversions,conversions,conversion_packages,
     &estimator,arg_type)
      
      use pr_mod
      
      use tl_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer type,geometry,pointer,rank,num_conversions
      integer indep_var(5),tab_index(5),conversions(5),conversion_packag
     &es(7,9),estimator(13),arg_type(*)
      character*(*)name,dep_var
      integer i,i_var0,est,pr_reac
      logical check_tally
      integer inc
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
      
      
      i_var0=string_lookup(dep_var,pr_var0_list,pr_var0_num)
      if(i_var0.LE.1)then
      write(6,*)' Tally ',name  (1:string_length(name)),' skipped since 
     &dep. var. ',dep_var  (1:string_length(dep_var)),' does not appear 
     &in this problem.'
      return
      end if
      tl_num=tl_num+1
      if(mod(((tl_num)-(1)+1),100).EQ.1)then
      tally_name =>mem_realloc_c1(tally_name,(80),(1),(((int(((((tl_num)
     &-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((tl_num)-(1)+1))+10
     &0-1)/100)*100)+(1)-1),'tally_name')
      end if
      if(mod(((tl_num)-(1)+1),100).EQ.1)then
      tally_type =>mem_realloc_i1(tally_type,(1),(((int(((((tl_num)-(1)+
     &1))+100-1)/100)*100)-100)+(1)-1),((int(((((tl_num)-(1)+1))+100-1)/
     &100)*100)+(1)-1),'tally_type')
      end if
      if(mod(((tl_num)-(1)+1),100).EQ.1)then
      tally_geometry =>mem_realloc_i1(tally_geometry,(1),(((int(((((tl_n
     &um)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((tl_num)-(1)+1))
     &+100-1)/100)*100)+(1)-1),'tally_geometry')
      end if
      if(mod(((tl_num)-(1)+1),100).EQ.1)then
      tally_geometry_ptr =>mem_realloc_i1(tally_geometry_ptr,(1),(((int(
     &((((tl_num)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((tl_num)
     &-(1)+1))+100-1)/100)*100)+(1)-1),'tally_geometry_ptr')
      end if
      if(mod(((tl_num)-(1)+1),100).EQ.1)then
      tally_rank =>mem_realloc_i1(tally_rank,(1),(((int(((((tl_num)-(1)+
     &1))+100-1)/100)*100)-100)+(1)-1),((int(((((tl_num)-(1)+1))+100-1)/
     &100)*100)+(1)-1),'tally_rank')
      end if
      if(mod(((tl_num)-(1)+1),100).EQ.1)then
      tally_dep_var_dim =>mem_realloc_i1(tally_dep_var_dim,(1),(((int(((
     &((tl_num)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((tl_num)-(
     &1)+1))+100-1)/100)*100)+(1)-1),'tally_dep_var_dim')
      end if
      if(mod(((tl_num)-(1)+1),100).EQ.1)then
      tally_indep_var =>mem_realloc_i2(tally_indep_var,(1),(5),(1),(((in
     &t(((((tl_num)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((tl_nu
     &m)-(1)+1))+100-1)/100)*100)+(1)-1),'tally_indep_var')
      end if
      if(mod(((tl_num)-(1)+1),100).EQ.1)then
      tally_dep_var =>mem_realloc_i1(tally_dep_var,(1),(((int(((((tl_num
     &)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((tl_num)-(1)+1))+1
     &00-1)/100)*100)+(1)-1),'tally_dep_var')
      end if
      if(mod(((tl_num)-(1)+1),100).EQ.1)then
      tally_base =>mem_realloc_i1(tally_base,(1),(((int(((((tl_num)-(1)+
     &1))+100-1)/100)*100)-100)+(1)-1),((int(((((tl_num)-(1)+1))+100-1)/
     &100)*100)+(1)-1),'tally_base')
      end if
      if(mod(((tl_num)-(1)+1),100).EQ.1)then
      tally_tab_index =>mem_realloc_i2(tally_tab_index,(1),(5),(1),(((in
     &t(((((tl_num)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((tl_nu
     &m)-(1)+1))+100-1)/100)*100)+(1)-1),'tally_tab_index')
      end if
      if(mod(((tl_num)-(1)+1),100).EQ.1)then
      tally_est_test =>mem_realloc_r2(tally_est_test,(1),(4),(1),(((int(
     &((((tl_num)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((tl_num)
     &-(1)+1))+100-1)/100)*100)+(1)-1),'tally_est_test')
      end if
      if(mod(((tl_num)-(1)+1),100).EQ.1)then
      tally_est_reaction =>mem_realloc_r3(tally_est_reaction,(1),(pr_rea
     &ction_dim+6),(1),(4),(1),(((int(((((tl_num)-(1)+1))+100-1)/100)*10
     &0)-100)+(1)-1),((int(((((tl_num)-(1)+1))+100-1)/100)*100)+(1)-1),'
     &tally_est_reaction')
      end if
      if(mod(((tl_num)-(1)+1),100).EQ.1)then
      tally_num_conversions =>mem_realloc_i1(tally_num_conversions,(1),(
     &((int(((((tl_num)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((t
     &l_num)-(1)+1))+100-1)/100)*100)+(1)-1),'tally_num_conversions')
      end if
      if(mod(((tl_num)-(1)+1),100).EQ.1)then
      tally_cv_ptr =>mem_realloc_i2(tally_cv_ptr,(1),(5),(1),(((int(((((
     &tl_num)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((tl_num)-(1)
     &+1))+100-1)/100)*100)+(1)-1),'tally_cv_ptr')
      end if
      if(tally_type_num(type).EQ.0)then
      tally_type_base(type)=tl_num
      else
      if(tally_type(tl_num-1).EQ.type)continue
      end if
      tally_name(tl_num)=name
      if(index(name,'vector').GT.0)then
      if(index(dep_var,'vector').GT.0)continue
      tally_dep_var_dim(tl_num)=3
      else
      tally_dep_var_dim(tl_num)=1
      end if
      tally_type(tl_num)=type
      tally_type_num(type)=tally_type_num(type)+1
      tally_geometry(tl_num)=geometry
      tally_geometry_ptr(tl_num)=pointer
      tally_dep_var(tl_num)=i_var0
      tally_rank(tl_num)=rank
      do i=1,rank
      tally_indep_var(i,tl_num)=indep_var(i)
      tally_tab_index(i,tl_num)=tab_index(i)
      end do
      if(rank.LT.5)then
      do i=rank+1,5
      tally_indep_var(i,tl_num)=0
      tally_tab_index(i,tl_num)=1
      end do
      end if
      tally_base(tl_num)=tally_size
      inc=1
      inc=inc*tally_tab_index(1,tl_num)
      inc=inc*tally_tab_index(2,tl_num)
      inc=inc*tally_tab_index(3,tl_num)
      inc=inc*tally_tab_index(4,tl_num)
      inc=inc*tally_tab_index(5,tl_num)
      inc=tally_dep_var_dim(tl_num)*inc
      tally_size=tally_size+inc
      tally_num_conversions(tl_num)=num_conversions
      do i=1,5
      tally_cv_ptr(i,tl_num)=0
      end do
      if(num_conversions.GT.0)then
      if(num_conversions.LE.5)continue
      do i=1,num_conversions
      nconversions=nconversions+1
      if(mod(((nconversions)-(1)+1),100).EQ.1)then
      tally_cv_action =>mem_realloc_i1(tally_cv_action,(1),(((int(((((nc
     &onversions)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((nconver
     &sions)-(1)+1))+100-1)/100)*100)+(1)-1),'tally_cv_action')
      end if
      if(mod(((nconversions)-(1)+1),100).EQ.1)then
      tally_cv_type =>mem_realloc_i1(tally_cv_type,(1),(((int(((((nconve
     &rsions)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((nconversion
     &s)-(1)+1))+100-1)/100)*100)+(1)-1),'tally_cv_type')
      end if
      if(mod(((nconversions)-(1)+1),100).EQ.1)then
      tally_cv_num_partners =>mem_realloc_i1(tally_cv_num_partners,(1),(
     &((int(((((nconversions)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int
     &(((((nconversions)-(1)+1))+100-1)/100)*100)+(1)-1),'tally_cv_num_p
     &artners')
      end if
      if(mod(((nconversions)-(1)+1),100).EQ.1)then
      tally_cv_scalers =>mem_realloc_i2(tally_cv_scalers,(1),(3),(1),(((
     &int(((((nconversions)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int((
     &(((nconversions)-(1)+1))+100-1)/100)*100)+(1)-1),'tally_cv_scalers
     &')
      end if
      if(mod(((nconversions)-(1)+1),100).EQ.1)then
      tally_cv_partners =>mem_realloc_i2(tally_cv_partners,(1),(3),(1),(
     &((int(((((nconversions)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int
     &(((((nconversions)-(1)+1))+100-1)/100)*100)+(1)-1),'tally_cv_partn
     &ers')
      end if
      tally_cv_ptr(i,tl_num)=nconversions
      tally_cv_action(nconversions)=conversion_packages(1,conversions(i)
     &)
      tally_cv_type(nconversions)=conversion_packages(2,conversions(i))
      if(3.EQ.3)continue
      tally_cv_scalers(1,nconversions)=conversion_packages(3,conversions
     &(i))
      tally_cv_scalers(2,nconversions)=conversion_packages(4,conversions
     &(i))
      tally_cv_scalers(3,nconversions)=conversion_packages(5,conversions
     &(i))
      if(3.EQ.3)continue
      tally_cv_partners(1,nconversions)=tl_num
      tally_cv_partners(2,nconversions)=conversion_packages(6,conversion
     &s(i))
      tally_cv_partners(3,nconversions)=conversion_packages(7,conversion
     &s(i))
      tally_cv_num_partners(nconversions)=1
      if(conversion_packages(6,conversions(i)).NE.0)then
      tally_cv_num_partners(nconversions)=2
      if(conversion_packages(7,conversions(i)).NE.0)then
      tally_cv_num_partners(nconversions)=3
      end if
      end if
      end do
      end if
      if(tally_type(tl_num).EQ.3)then
      do est=1,4
      do pr_reac=1,pr_reaction_num+6
      tally_est_reaction(pr_reac,est,tl_num)=(0.0_DOUBLE)
      end do
      tally_est_test(est,tl_num)=(2.0e30_DOUBLE)
      end do
      do pr_reac=1,pr_reaction_num+6
      if(estimator(arg_type(pr_reac)).NE.0)then
      tally_est_reaction(pr_reac,estimator(arg_type(pr_reac)),tl_num)=(1
     &.0_DOUBLE)
      end if
      end do
      else if(tally_type(tl_num).EQ.2)then
      do est=1,4
      tally_est_test(est,tl_num)=(0.0_DOUBLE)
      do pr_reac=1,pr_reaction_num+6
      tally_est_reaction(pr_reac,est,tl_num)=(2.0e30_DOUBLE)
      end do
      end do
      tally_est_test(estimator(1),tl_num)=(1.0_DOUBLE)
      else if(tally_type(tl_num).EQ.1)then
      do est=1,4
      tally_est_test(est,tl_num)=(2.0e30_DOUBLE)
      do pr_reac=1,pr_reaction_num+6
      tally_est_reaction(pr_reac,est,tl_num)=(2.0e30_DOUBLE)
      end do
      end do
      end if
      return
      end
      subroutine nc_write_tally
      
      use rf_mod
      
      use pr_mod
      
      use tl_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical check_tally
      integer inc
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
      integer fileid
      integer tally_type_ind_id
      integer tally_rank_ind_id
      integer tally_est_ind_id
      integer tally_reac_ind_id
      integer tally_name_string_id
      integer tally_tag_string_id
      integer tally_cv_ptr_ind_id
      integer tally_cv_scaler_ind_id
      integer tally_cv_partner_ind_id
      integer tally_index_ind_id
      integer tl_num_id
      integer tally_ind_id
      integer nconversions_id
      integer tally_cv_ind_id
      integer tally_size_id
      integer tally_type_num_id
      integer tally_type_id
      integer tally_geometry_id
      integer tally_geometry_ptr_id
      integer tally_base_id
      integer tally_type_base_id
      integer tally_rank_id
      integer tally_dep_var_dim_id
      integer tally_indep_var_id
      integer tally_tab_index_id
      integer tally_name_id
      integer tally_dep_var_id
      integer tally_est_test_id
      integer tally_est_reaction_id
      integer tally_num_conversions_id
      integer tally_cv_ptr_id
      integer tally_cv_action_id
      integer tally_cv_type_id
      integer tally_cv_num_partners_id
      integer tally_cv_scalers_id
      integer tally_cv_partners_id
      integer tally_var_list_id
      
      character*300 description,program_version
      character*96 tempfile
      program_version='DEGAS 2 Git commit: $Format:%H$, ref names: $Form
     &at:%d$'
      tempfile=filenames_array(19)
      if(tempfile.NE.'undefined')continue
      fileid=nccre(tempfile,0,nc_stat)
      description='Tally description for problem in degas 2'
      call ncaptc(fileid,0,'description',2,string_length(description),de
     &scription,nc_stat)
      call ncaptc(fileid,0,'program_version',2,string_length(program_ver
     &sion),program_version,nc_stat)
      call ncaptc(fileid,0,'tally_version',2,string_length(tally_version
     &),tally_version,nc_stat)
      tally_type_ind_id=ncddef(fileid,'tally_type_ind',((3)-(1)+1),nc_st
     &at)
      tally_rank_ind_id=ncddef(fileid,'tally_rank_ind',((5)-(1)+1),nc_st
     &at)
      tally_est_ind_id=ncddef(fileid,'tally_est_ind',((4)-(1)+1),nc_stat
     &)
      tally_reac_ind_id=ncddef(fileid,'tally_reac_ind',((pr_reaction_dim
     &+6)-(1)+1),nc_stat)
      tally_name_string_id=ncddef(fileid,'tally_name_string',((80)-(1)+1
     &),nc_stat)
      tally_tag_string_id=ncddef(fileid,'tally_tag_string',((40)-(1)+1),
     &nc_stat)
      tally_cv_ptr_ind_id=ncddef(fileid,'tally_cv_ptr_ind',((5)-(1)+1),n
     &c_stat)
      tally_cv_scaler_ind_id=ncddef(fileid,'tally_cv_scaler_ind',((3)-(1
     &)+1),nc_stat)
      tally_cv_partner_ind_id=ncddef(fileid,'tally_cv_partner_ind',((3)-
     &(1)+1),nc_stat)
      tally_index_ind_id=ncddef(fileid,'tally_index_ind',((100)-(0)+1),n
     &c_stat)
      tl_num_id=ncvdef(fileid,'tl_num',4,0,nc_dims,nc_stat)
      tally_ind_id=ncddef(fileid,'tally_ind',((tl_num)-(1)+1),nc_stat)
      nconversions_id=ncvdef(fileid,'nconversions',4,0,nc_dims,nc_stat)
      tally_cv_ind_id=ncddef(fileid,'tally_cv_ind',((nconversions)-(1)+1
     &),nc_stat)
      tally_size_id=ncvdef(fileid,'tally_size',4,0,nc_dims,nc_stat)
      nc_dims(1)=tally_type_ind_id
      tally_type_num_id=ncvdef(fileid,'tally_type_num',4,1,nc_dims,nc_st
     &at)
      nc_dims(1)=tally_ind_id
      tally_type_id=ncvdef(fileid,'tally_type',4,1,nc_dims,nc_stat)
      nc_dims(1)=tally_ind_id
      tally_geometry_id=ncvdef(fileid,'tally_geometry',4,1,nc_dims,nc_st
     &at)
      nc_dims(1)=tally_ind_id
      tally_geometry_ptr_id=ncvdef(fileid,'tally_geometry_ptr',4,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=tally_ind_id
      tally_base_id=ncvdef(fileid,'tally_base',4,1,nc_dims,nc_stat)
      nc_dims(1)=tally_type_ind_id
      tally_type_base_id=ncvdef(fileid,'tally_type_base',4,1,nc_dims,nc_
     &stat)
      nc_dims(1)=tally_ind_id
      tally_rank_id=ncvdef(fileid,'tally_rank',4,1,nc_dims,nc_stat)
      nc_dims(1)=tally_ind_id
      tally_dep_var_dim_id=ncvdef(fileid,'tally_dep_var_dim',4,1,nc_dims
     &,nc_stat)
      nc_dims(1)=tally_rank_ind_id
      nc_dims(2)=tally_ind_id
      tally_indep_var_id=ncvdef(fileid,'tally_indep_var',4,2,nc_dims,nc_
     &stat)
      nc_dims(1)=tally_rank_ind_id
      nc_dims(2)=tally_ind_id
      tally_tab_index_id=ncvdef(fileid,'tally_tab_index',4,2,nc_dims,nc_
     &stat)
      nc_dims(1)=tally_name_string_id
      nc_dims(2)=tally_ind_id
      tally_name_id=ncvdef(fileid,'tally_name',2,2,nc_dims,nc_stat)
      nc_dims(1)=tally_ind_id
      tally_dep_var_id=ncvdef(fileid,'tally_dep_var',4,1,nc_dims,nc_stat
     &)
      nc_dims(1)=tally_est_ind_id
      nc_dims(2)=tally_ind_id
      tally_est_test_id=ncvdef(fileid,'tally_est_test',6,2,nc_dims,nc_st
     &at)
      nc_dims(1)=tally_reac_ind_id
      nc_dims(2)=tally_est_ind_id
      nc_dims(3)=tally_ind_id
      tally_est_reaction_id=ncvdef(fileid,'tally_est_reaction',6,3,nc_di
     &ms,nc_stat)
      nc_dims(1)=tally_ind_id
      tally_num_conversions_id=ncvdef(fileid,'tally_num_conversions',4,1
     &,nc_dims,nc_stat)
      nc_dims(1)=tally_cv_ptr_ind_id
      nc_dims(2)=tally_ind_id
      tally_cv_ptr_id=ncvdef(fileid,'tally_cv_ptr',4,2,nc_dims,nc_stat)
      nc_dims(1)=tally_cv_ind_id
      tally_cv_action_id=ncvdef(fileid,'tally_cv_action',4,1,nc_dims,nc_
     &stat)
      nc_dims(1)=tally_cv_ind_id
      tally_cv_type_id=ncvdef(fileid,'tally_cv_type',4,1,nc_dims,nc_stat
     &)
      nc_dims(1)=tally_cv_ind_id
      tally_cv_num_partners_id=ncvdef(fileid,'tally_cv_num_partners',4,1
     &,nc_dims,nc_stat)
      nc_dims(1)=tally_cv_scaler_ind_id
      nc_dims(2)=tally_cv_ind_id
      tally_cv_scalers_id=ncvdef(fileid,'tally_cv_scalers',4,2,nc_dims,n
     &c_stat)
      nc_dims(1)=tally_cv_partner_ind_id
      nc_dims(2)=tally_cv_ind_id
      tally_cv_partners_id=ncvdef(fileid,'tally_cv_partners',4,2,nc_dims
     &,nc_stat)
      nc_dims(1)=tally_tag_string_id
      nc_dims(2)=tally_index_ind_id
      tally_var_list_id=ncvdef(fileid,'tally_var_list',2,2,nc_dims,nc_st
     &at)
      
      call ncendf(fileid,nc_stat)
      call ncvpt(fileid,tl_num_id,nc_corner,nc_edge,tl_num,nc_stat)
      call ncvpt(fileid,nconversions_id,nc_corner,nc_edge,nconversions,n
     &c_stat)
      call ncvpt(fileid,tally_size_id,nc_corner,nc_edge,tally_size,nc_st
     &at)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      call ncvpt(fileid,tally_type_num_id,nc_corner,nc_edge,tally_type_n
     &um,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((tl_num)-(1)+1)
      call ncvpt(fileid,tally_type_id,nc_corner,nc_edge,tally_type,nc_st
     &at)
      nc_corner(1)=1
      nc_edge(1)=((tl_num)-(1)+1)
      call ncvpt(fileid,tally_geometry_id,nc_corner,nc_edge,tally_geomet
     &ry,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((tl_num)-(1)+1)
      call ncvpt(fileid,tally_geometry_ptr_id,nc_corner,nc_edge,tally_ge
     &ometry_ptr,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((tl_num)-(1)+1)
      call ncvpt(fileid,tally_base_id,nc_corner,nc_edge,tally_base,nc_st
     &at)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      call ncvpt(fileid,tally_type_base_id,nc_corner,nc_edge,tally_type_
     &base,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((tl_num)-(1)+1)
      call ncvpt(fileid,tally_rank_id,nc_corner,nc_edge,tally_rank,nc_st
     &at)
      nc_corner(1)=1
      nc_edge(1)=((tl_num)-(1)+1)
      call ncvpt(fileid,tally_dep_var_dim_id,nc_corner,nc_edge,tally_dep
     &_var_dim,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((tl_num)-(1)+1)
      call ncvpt(fileid,tally_indep_var_id,nc_corner,nc_edge,tally_indep
     &_var,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((tl_num)-(1)+1)
      call ncvpt(fileid,tally_tab_index_id,nc_corner,nc_edge,tally_tab_i
     &ndex,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((80)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((tl_num)-(1)+1)
      call ncvptc(fileid,tally_name_id,nc_corner,nc_edge,tally_name,(((8
     &0)-(1)+1)*((tl_num)-(1)+1)),nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((tl_num)-(1)+1)
      call ncvpt(fileid,tally_dep_var_id,nc_corner,nc_edge,tally_dep_var
     &,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((tl_num)-(1)+1)
      call ncvpt(fileid,tally_est_test_id,nc_corner,nc_edge,tally_est_te
     &st,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_reaction_dim+6)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((tl_num)-(1)+1)
      call ncvpt(fileid,tally_est_reaction_id,nc_corner,nc_edge,tally_es
     &t_reaction,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((tl_num)-(1)+1)
      call ncvpt(fileid,tally_num_conversions_id,nc_corner,nc_edge,tally
     &_num_conversions,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((tl_num)-(1)+1)
      call ncvpt(fileid,tally_cv_ptr_id,nc_corner,nc_edge,tally_cv_ptr,n
     &c_stat)
      nc_corner(1)=1
      nc_edge(1)=((nconversions)-(1)+1)
      call ncvpt(fileid,tally_cv_action_id,nc_corner,nc_edge,tally_cv_ac
     &tion,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((nconversions)-(1)+1)
      call ncvpt(fileid,tally_cv_type_id,nc_corner,nc_edge,tally_cv_type
     &,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((nconversions)-(1)+1)
      call ncvpt(fileid,tally_cv_num_partners_id,nc_corner,nc_edge,tally
     &_cv_num_partners,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((nconversions)-(1)+1)
      call ncvpt(fileid,tally_cv_scalers_id,nc_corner,nc_edge,tally_cv_s
     &calers,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((nconversions)-(1)+1)
      call ncvpt(fileid,tally_cv_partners_id,nc_corner,nc_edge,tally_cv_
     &partners,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((100)-(0)+1)
      call ncvptc(fileid,tally_var_list_id,nc_corner,nc_edge,tally_var_l
     &ist,(((40)-(1)+1)*((100)-(0)+1)),nc_stat)
      
      call ncclos(fileid,nc_stat)
      return
      end
      subroutine clear_tally
      
      use pr_mod
      
      use tl_mod
      
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
      
      
      call mem_free_i1(tally_type_num,(1),(3),'tally_type_num')
      call mem_free_i1(tally_type_base,(1),(3),'tally_type_base')
      call mem_free_c1(tally_name,(80),(1),(tl_num),'tally_name')
      call mem_free_i1(tally_type,(1),(tl_num),'tally_type')
      call mem_free_i1(tally_geometry,(1),(tl_num),'tally_geometry')
      call mem_free_i1(tally_geometry_ptr,(1),(tl_num),'tally_geometry_p
     &tr')
      call mem_free_i1(tally_rank,(1),(tl_num),'tally_rank')
      call mem_free_i2(tally_indep_var,(1),(5),(1),(tl_num),'tally_indep
     &_var')
      call mem_free_i1(tally_dep_var,(1),(tl_num),'tally_dep_var')
      call mem_free_i1(tally_base,(1),(tl_num),'tally_base')
      call mem_free_i2(tally_tab_index,(1),(5),(1),(tl_num),'tally_tab_i
     &ndex')
      call mem_free_r2(tally_est_test,(1),(4),(1),(tl_num),'tally_est_te
     &st')
      call mem_free_r3(tally_est_reaction,(1),(pr_reaction_dim+6),(1),(4
     &),(1),(tl_num),'tally_est_reaction')
      call mem_free_i1(tally_num_conversions,(1),(tl_num),'tally_num_con
     &versions')
      call mem_free_i2(tally_cv_ptr,(1),(5),(1),(tl_num),'tally_cv_ptr')
      call mem_free_i1(tally_cv_action,(1),(nconversions),'tally_cv_acti
     &on')
      call mem_free_i1(tally_cv_type,(1),(nconversions),'tally_cv_type')
      call mem_free_i1(tally_cv_num_partners,(1),(nconversions),'tally_c
     &v_num_partners')
      call mem_free_i2(tally_cv_scalers,(1),(3),(1),(nconversions),'tall
     &y_cv_scalers')
      call mem_free_i2(tally_cv_partners,(1),(3),(1),(nconversions),'tal
     &ly_cv_partners')
      return
      end
      subroutine set_conversion_packages(conversion_packages,cv_name)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer conversion_packages(7,9)
      character*40 cv_name(*)
      integer p
      if(3.EQ.3)continue
      if(3.EQ.3)continue
      do p=1,9
      conversion_packages(6,p)=0
      conversion_packages(7,p)=0
      end do
      p=1
      cv_name(p)='scale_test_mass'
      conversion_packages(1,p)=1
      conversion_packages(2,p)=3
      conversion_packages(3,p)=1
      conversion_packages(4,p)=0
      conversion_packages(5,p)=0
      p=2
      cv_name(p)='scale_problem_sp_mass'
      conversion_packages(1,p)=1
      conversion_packages(2,p)=3
      conversion_packages(3,p)=2
      conversion_packages(4,p)=0
      conversion_packages(5,p)=0
      p=3
      cv_name(p)='scale_volume'
      conversion_packages(1,p)=1
      conversion_packages(2,p)=3
      conversion_packages(3,p)=3
      conversion_packages(4,p)=0
      conversion_packages(5,p)=0
      p=4
      cv_name(p)='Pa_to_mTorr'
      conversion_packages(1,p)=1
      conversion_packages(2,p)=3
      conversion_packages(3,p)=4
      conversion_packages(4,p)=0
      conversion_packages(5,p)=0
      p=5
      cv_name(p)='three_halves'
      conversion_packages(1,p)=1
      conversion_packages(2,p)=3
      conversion_packages(3,p)=11
      conversion_packages(4,p)=0
      conversion_packages(5,p)=0
      p=7
      cv_name(p)='post_scale_test_mass'
      conversion_packages(1,p)=1
      conversion_packages(2,p)=2
      conversion_packages(3,p)=1
      conversion_packages(4,p)=0
      conversion_packages(5,p)=0
      p=6
      cv_name(p)='track_v_to_external'
      conversion_packages(1,p)=2
      conversion_packages(2,p)=1
      conversion_packages(3,p)=5
      conversion_packages(4,p)=6
      conversion_packages(5,p)=7
      p=8
      cv_name(p)='divide_number'
      conversion_packages(1,p)=4
      conversion_packages(2,p)=2
      conversion_packages(3,p)=0
      conversion_packages(4,p)=0
      conversion_packages(5,p)=0
      p=9
      cv_name(p)='post_v_to_internal'
      conversion_packages(1,p)=3
      conversion_packages(2,p)=2
      conversion_packages(3,p)=8
      conversion_packages(4,p)=9
      conversion_packages(5,p)=10
      return
      end
      subroutine set_var_list(var_num)
      
      use pr_mod
      
      use so_mod
      
      use tl_mod
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer var_num(0:*)
      integer i
      tally_var_list(0)='unknown'
      tally_var_list(1)='zone'
      tally_var_list(2)='plasma_zone'
      tally_var_list(3)='test'
      tally_var_list(4)='problem_sp'
      tally_var_list(5)='detector'
      tally_var_list(6)='test_author'
      tally_var_list(7)='reaction'
      tally_var_list(8)='pmi'
      tally_var_list(9)='material'
      tally_var_list(10)='source_group'
      tally_var_list(11)='sector'
      tally_var_list(12)='strata'
      tally_var_list(13)='strata_segment'
      tally_var_list(14)='energy_bin'
      tally_var_list(15)='angle_bin'
      tally_var_list(16)='wavelength_bin'
      tally_var_list(17)='diagnostic'
      tally_var_list(18)='zone_ind_1'
      tally_var_list(19)='zone_ind_2'
      
      do i=19+1,100
      tally_var_list(i)='UNUSED'
      end do
      do i=1,100
      var_num(i)=0
      end do
      var_num(1)=zn_num
      var_num(3)=pr_test_num
      var_num(4)=pr_background_num+pr_test_num
      var_num(6)=6+pr_reaction_num+pr_pmi_num
      var_num(7)=pr_reaction_num+6
      var_num(10)=2000000000
      var_num(18)=zone_index_max(1)
      var_num(19)=zone_index_max(2)
      return
      end
      
      
