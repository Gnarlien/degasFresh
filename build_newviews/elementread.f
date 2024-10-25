      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine nc_read_elements
      
      use el_mod
      
      use rf_mod
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer nc_dims(5),nc_corner(5),nc_edge(5),nc_stat,nc_size,nc_rank
     &,nc_attr,nc_type
      character*128 nc_dummy
      external ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf
     &_inq_format
      integer ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf_
     &inq_format
      integer element_symbol_string_id
      integer element_name_string_id
      integer el_num_id
      integer element_ind_id
      integer element_name_id
      integer element_sy_id
      integer element_z_id
      integer element_m_id
      
      include 'mpif.h'
      integer mpi_err
      integer mpi_status
      dimension mpi_status(MPI_STATUS_SIZE)
      integer fileid
      character*96 tempfile
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
      
      
      if((mpi_rank.EQ.mpi_degas2_root))then
      tempfile=filenames_array(1)
      if(tempfile.NE.'undefined')continue
      fileid=ncopn(tempfile,0,nc_stat)
      call ncagtc(fileid,0,'data_version',element_version,len(element_ve
     &rsion),nc_stat)
      element_symbol_string_id=ncdid(fileid,'element_symbol_string',nc_s
     &tat)
      call ncdinq(fileid,element_symbol_string_id,nc_dummy,nc_size,nc_st
     &at)
      if(nc_size.EQ.((3)-(1)+1))continue
      element_name_string_id=ncdid(fileid,'element_name_string',nc_stat)
      call ncdinq(fileid,element_name_string_id,nc_dummy,nc_size,nc_stat
     &)
      if(nc_size.EQ.((16)-(1)+1))continue
      el_num_id=ncvid(fileid,'el_num',nc_stat)
      call ncvinq(fileid,el_num_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      
      call ncvgt(fileid,el_num_id,nc_corner,nc_edge,el_num,nc_stat)
      element_ind_id=ncdid(fileid,'element_ind',nc_stat)
      call ncdinq(fileid,element_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((el_num)-(1)+1))continue
      element_name_id=ncvid(fileid,'element_name',nc_stat)
      call ncvinq(fileid,element_name_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.element_name_string_id)continue
      if(nc_dims(2).EQ.element_ind_id)continue
      
      element_name =>mem_alloc_c1((16),(1),(el_num),'element_name')
      nc_corner(1)=1
      nc_edge(1)=((16)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((el_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,element_name_id,nc_corner,nc_edge,
     &element_name)
      element_sy_id=ncvid(fileid,'element_sy',nc_stat)
      call ncvinq(fileid,element_sy_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      if(nc_dims(1).EQ.element_symbol_string_id)continue
      if(nc_dims(2).EQ.element_ind_id)continue
      
      element_sy =>mem_alloc_c1((3),(1),(el_num),'element_sy')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((el_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,element_sy_id,nc_corner,nc_edge,el
     &ement_sy)
      element_z_id=ncvid(fileid,'element_z',nc_stat)
      call ncvinq(fileid,element_z_id,nc_dummy,nc_type,nc_rank,nc_dims,n
     &c_attr,nc_stat)
      if(nc_dims(1).EQ.element_ind_id)continue
      
      element_z =>mem_alloc_i1((1),(el_num),'element_z')
      nc_corner(1)=1
      nc_edge(1)=((el_num)-(1)+1)
      call ncvgt(fileid,element_z_id,nc_corner,nc_edge,element_z,nc_stat
     &)
      element_m_id=ncvid(fileid,'element_m',nc_stat)
      call ncvinq(fileid,element_m_id,nc_dummy,nc_type,nc_rank,nc_dims,n
     &c_attr,nc_stat)
      if(nc_dims(1).EQ.element_ind_id)continue
      
      element_m =>mem_alloc_r1((1),(el_num),'element_m')
      nc_corner(1)=1
      nc_edge(1)=((el_num)-(1)+1)
      call ncvgt(fileid,element_m_id,nc_corner,nc_edge,element_m,nc_stat
     &)
      
      call ncclos(fileid,nc_stat)
      endif
      call MPI_bcast(el_num,1,MPI_INTEGER,mpi_degas2_root,comm_world_dup
     &,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      element_name =>mem_alloc_c1((16),(1),(el_num),'element_name')
      endif
      call MPI_bcast(element_name,(((16)-(1)+1)*((el_num)-(1)+1)),MPI_CH
     &ARACTER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      element_sy =>mem_alloc_c1((3),(1),(el_num),'element_sy')
      endif
      call MPI_bcast(element_sy,(((3)-(1)+1)*((el_num)-(1)+1)),MPI_CHARA
     &CTER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      element_z =>mem_alloc_i1((1),(el_num),'element_z')
      endif
      call MPI_bcast(element_z,(((el_num)-(1)+1)),MPI_INTEGER,mpi_degas2
     &_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      element_m =>mem_alloc_r1((1),(el_num),'element_m')
      endif
      call MPI_bcast(element_m,(((el_num)-(1)+1)),MPI_DOUBLE_PRECISION,m
     &pi_degas2_root,comm_world_dup,mpi_err)
      
      return
      end
      subroutine nc_read_species
      
      use sp_mod
      
      use rf_mod
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer fileid
      character*96 tempfile
      integer species_symbol_string_id
      integer species_name_string_id
      integer sp_num_id
      integer species_ind_id
      integer species_comp_ind_id
      integer species_name_id
      integer species_sy_id
      integer species_z_id
      integer species_m_id
      integer species_ncomp_id
      integer species_generic_id
      integer species_multiplicity_id
      integer species_el_id
      integer species_count_id
      
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
      
      
      if((mpi_rank.EQ.mpi_degas2_root))then
      tempfile=filenames_array(7)
      if(tempfile.NE.'undefined')continue
      fileid=ncopn(tempfile,0,nc_stat)
      call ncagtc(fileid,0,'data_version',species_version,len(species_ve
     &rsion),nc_stat)
      species_symbol_string_id=ncdid(fileid,'species_symbol_string',nc_s
     &tat)
      call ncdinq(fileid,species_symbol_string_id,nc_dummy,nc_size,nc_st
     &at)
      if(nc_size.EQ.((8)-(1)+1))continue
      species_name_string_id=ncdid(fileid,'species_name_string',nc_stat)
      call ncdinq(fileid,species_name_string_id,nc_dummy,nc_size,nc_stat
     &)
      if(nc_size.EQ.((32)-(1)+1))continue
      sp_num_id=ncvid(fileid,'sp_num',nc_stat)
      call ncvinq(fileid,sp_num_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      
      call ncvgt(fileid,sp_num_id,nc_corner,nc_edge,sp_num,nc_stat)
      species_ind_id=ncdid(fileid,'species_ind',nc_stat)
      call ncdinq(fileid,species_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((sp_num)-(1)+1))continue
      species_comp_ind_id=ncdid(fileid,'species_comp_ind',nc_stat)
      call ncdinq(fileid,species_comp_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((10)-(1)+1))continue
      species_name_id=ncvid(fileid,'species_name',nc_stat)
      call ncvinq(fileid,species_name_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.species_name_string_id)continue
      if(nc_dims(2).EQ.species_ind_id)continue
      
      species_name =>mem_alloc_c1((32),(1),(sp_num),'species_name')
      nc_corner(1)=1
      nc_edge(1)=((32)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((sp_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,species_name_id,nc_corner,nc_edge,
     &species_name)
      species_sy_id=ncvid(fileid,'species_sy',nc_stat)
      call ncvinq(fileid,species_sy_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      if(nc_dims(1).EQ.species_symbol_string_id)continue
      if(nc_dims(2).EQ.species_ind_id)continue
      
      species_sy =>mem_alloc_c1((8),(1),(sp_num),'species_sy')
      nc_corner(1)=1
      nc_edge(1)=((8)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((sp_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,species_sy_id,nc_corner,nc_edge,sp
     &ecies_sy)
      species_z_id=ncvid(fileid,'species_z',nc_stat)
      call ncvinq(fileid,species_z_id,nc_dummy,nc_type,nc_rank,nc_dims,n
     &c_attr,nc_stat)
      if(nc_dims(1).EQ.species_ind_id)continue
      
      species_z =>mem_alloc_i1((1),(sp_num),'species_z')
      nc_corner(1)=1
      nc_edge(1)=((sp_num)-(1)+1)
      call ncvgt(fileid,species_z_id,nc_corner,nc_edge,species_z,nc_stat
     &)
      species_m_id=ncvid(fileid,'species_m',nc_stat)
      call ncvinq(fileid,species_m_id,nc_dummy,nc_type,nc_rank,nc_dims,n
     &c_attr,nc_stat)
      if(nc_dims(1).EQ.species_ind_id)continue
      
      species_m =>mem_alloc_r1((1),(sp_num),'species_m')
      nc_corner(1)=1
      nc_edge(1)=((sp_num)-(1)+1)
      call ncvgt(fileid,species_m_id,nc_corner,nc_edge,species_m,nc_stat
     &)
      species_ncomp_id=ncvid(fileid,'species_ncomp',nc_stat)
      call ncvinq(fileid,species_ncomp_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.species_ind_id)continue
      
      species_ncomp =>mem_alloc_i1((1),(sp_num),'species_ncomp')
      nc_corner(1)=1
      nc_edge(1)=((sp_num)-(1)+1)
      call ncvgt(fileid,species_ncomp_id,nc_corner,nc_edge,species_ncomp
     &,nc_stat)
      species_generic_id=ncvid(fileid,'species_generic',nc_stat)
      call ncvinq(fileid,species_generic_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.species_ind_id)continue
      
      species_generic =>mem_alloc_i1((1),(sp_num),'species_generic')
      nc_corner(1)=1
      nc_edge(1)=((sp_num)-(1)+1)
      call ncvgt(fileid,species_generic_id,nc_corner,nc_edge,species_gen
     &eric,nc_stat)
      species_multiplicity_id=ncvid(fileid,'species_multiplicity',nc_sta
     &t)
      call ncvinq(fileid,species_multiplicity_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.species_ind_id)continue
      
      species_multiplicity =>mem_alloc_i1((1),(sp_num),'species_multipli
     &city')
      nc_corner(1)=1
      nc_edge(1)=((sp_num)-(1)+1)
      call ncvgt(fileid,species_multiplicity_id,nc_corner,nc_edge,specie
     &s_multiplicity,nc_stat)
      species_el_id=ncvid(fileid,'species_el',nc_stat)
      call ncvinq(fileid,species_el_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      if(nc_dims(1).EQ.species_comp_ind_id)continue
      if(nc_dims(2).EQ.species_ind_id)continue
      
      species_el =>mem_alloc_i2((1),(10),(1),(sp_num),'species_el')
      nc_corner(1)=1
      nc_edge(1)=((10)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((sp_num)-(1)+1)
      call ncvgt(fileid,species_el_id,nc_corner,nc_edge,species_el,nc_st
     &at)
      species_count_id=ncvid(fileid,'species_count',nc_stat)
      call ncvinq(fileid,species_count_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.species_comp_ind_id)continue
      if(nc_dims(2).EQ.species_ind_id)continue
      
      species_count =>mem_alloc_i2((1),(10),(1),(sp_num),'species_count'
     &)
      nc_corner(1)=1
      nc_edge(1)=((10)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((sp_num)-(1)+1)
      call ncvgt(fileid,species_count_id,nc_corner,nc_edge,species_count
     &,nc_stat)
      
      call ncclos(fileid,nc_stat)
      endif
      call MPI_bcast(sp_num,1,MPI_INTEGER,mpi_degas2_root,comm_world_dup
     &,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      species_name =>mem_alloc_c1((32),(1),(sp_num),'species_name')
      endif
      call MPI_bcast(species_name,(((32)-(1)+1)*((sp_num)-(1)+1)),MPI_CH
     &ARACTER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      species_sy =>mem_alloc_c1((8),(1),(sp_num),'species_sy')
      endif
      call MPI_bcast(species_sy,(((8)-(1)+1)*((sp_num)-(1)+1)),MPI_CHARA
     &CTER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      species_z =>mem_alloc_i1((1),(sp_num),'species_z')
      endif
      call MPI_bcast(species_z,(((sp_num)-(1)+1)),MPI_INTEGER,mpi_degas2
     &_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      species_m =>mem_alloc_r1((1),(sp_num),'species_m')
      endif
      call MPI_bcast(species_m,(((sp_num)-(1)+1)),MPI_DOUBLE_PRECISION,m
     &pi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      species_ncomp =>mem_alloc_i1((1),(sp_num),'species_ncomp')
      endif
      call MPI_bcast(species_ncomp,(((sp_num)-(1)+1)),MPI_INTEGER,mpi_de
     &gas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      species_generic =>mem_alloc_i1((1),(sp_num),'species_generic')
      endif
      call MPI_bcast(species_generic,(((sp_num)-(1)+1)),MPI_INTEGER,mpi_
     &degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      species_multiplicity =>mem_alloc_i1((1),(sp_num),'species_multipli
     &city')
      endif
      call MPI_bcast(species_multiplicity,(((sp_num)-(1)+1)),MPI_INTEGER
     &,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      species_el =>mem_alloc_i2((1),(10),(1),(sp_num),'species_el')
      endif
      call MPI_bcast(species_el,(((10)-(1)+1)*((sp_num)-(1)+1)),MPI_INTE
     &GER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      species_count =>mem_alloc_i2((1),(10),(1),(sp_num),'species_count'
     &)
      endif
      call MPI_bcast(species_count,(((10)-(1)+1)*((sp_num)-(1)+1)),MPI_I
     &NTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      
      return
      end
      subroutine nc_read_reactions
      
      use rc_mod
      
      use rf_mod
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer fileid
      character*96 tempfile
      integer reaction_symbol_string_id
      integer reaction_name_string_id
      integer reaction_type_string_id
      integer rc_num_id
      integer reaction_ind_id
      integer reaction_filename_string_id
      integer reagent_ind_id
      integer product_ind_id
      integer reaction_name_id
      integer reaction_type_id
      integer reaction_sy_id
      integer reaction_emitter_id
      integer reaction_reagent_num_id
      integer reaction_generic_id
      integer reaction_product_num_id
      integer reaction_reagent_id
      integer reaction_product_id
      integer reaction_filename_id
      
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
      
      
      if((mpi_rank.EQ.mpi_degas2_root))then
      tempfile=filenames_array(6)
      if(tempfile.NE.'undefined')continue
      fileid=ncopn(tempfile,0,nc_stat)
      call ncagtc(fileid,0,'data_version',reaction_version,len(reaction_
     &version),nc_stat)
      reaction_symbol_string_id=ncdid(fileid,'reaction_symbol_string',nc
     &_stat)
      call ncdinq(fileid,reaction_symbol_string_id,nc_dummy,nc_size,nc_s
     &tat)
      if(nc_size.EQ.((24)-(1)+1))continue
      reaction_name_string_id=ncdid(fileid,'reaction_name_string',nc_sta
     &t)
      call ncdinq(fileid,reaction_name_string_id,nc_dummy,nc_size,nc_sta
     &t)
      if(nc_size.EQ.((80)-(1)+1))continue
      reaction_type_string_id=ncdid(fileid,'reaction_type_string',nc_sta
     &t)
      call ncdinq(fileid,reaction_type_string_id,nc_dummy,nc_size,nc_sta
     &t)
      if(nc_size.EQ.((32)-(1)+1))continue
      rc_num_id=ncvid(fileid,'rc_num',nc_stat)
      call ncvinq(fileid,rc_num_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      
      call ncvgt(fileid,rc_num_id,nc_corner,nc_edge,rc_num,nc_stat)
      reaction_ind_id=ncdid(fileid,'reaction_ind',nc_stat)
      call ncdinq(fileid,reaction_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((rc_num)-(1)+1))continue
      reaction_filename_string_id=ncdid(fileid,'reaction_filename_string
     &',nc_stat)
      call ncdinq(fileid,reaction_filename_string_id,nc_dummy,nc_size,nc
     &_stat)
      if(nc_size.EQ.((96)-(1)+1))continue
      reagent_ind_id=ncdid(fileid,'reagent_ind',nc_stat)
      call ncdinq(fileid,reagent_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((2)-(1)+1))continue
      product_ind_id=ncdid(fileid,'product_ind',nc_stat)
      call ncdinq(fileid,product_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((4)-(1)+1))continue
      reaction_name_id=ncvid(fileid,'reaction_name',nc_stat)
      call ncvinq(fileid,reaction_name_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.reaction_name_string_id)continue
      if(nc_dims(2).EQ.reaction_ind_id)continue
      
      reaction_name =>mem_alloc_c1((80),(1),(rc_num),'reaction_name')
      nc_corner(1)=1
      nc_edge(1)=((80)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((rc_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,reaction_name_id,nc_corner,nc_edge
     &,reaction_name)
      reaction_type_id=ncvid(fileid,'reaction_type',nc_stat)
      call ncvinq(fileid,reaction_type_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.reaction_type_string_id)continue
      if(nc_dims(2).EQ.reaction_ind_id)continue
      
      reaction_type =>mem_alloc_c1((32),(1),(rc_num),'reaction_type')
      nc_corner(1)=1
      nc_edge(1)=((32)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((rc_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,reaction_type_id,nc_corner,nc_edge
     &,reaction_type)
      reaction_sy_id=ncvid(fileid,'reaction_sy',nc_stat)
      call ncvinq(fileid,reaction_sy_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      if(nc_dims(1).EQ.reaction_symbol_string_id)continue
      if(nc_dims(2).EQ.reaction_ind_id)continue
      
      reaction_sy =>mem_alloc_c1((24),(1),(rc_num),'reaction_sy')
      nc_corner(1)=1
      nc_edge(1)=((24)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((rc_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,reaction_sy_id,nc_corner,nc_edge,r
     &eaction_sy)
      reaction_emitter_id=ncvid(fileid,'reaction_emitter',nc_stat)
      call ncvinq(fileid,reaction_emitter_id,nc_dummy,nc_type,nc_rank,nc
     &_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.reaction_ind_id)continue
      
      reaction_emitter =>mem_alloc_i1((1),(rc_num),'reaction_emitter')
      nc_corner(1)=1
      nc_edge(1)=((rc_num)-(1)+1)
      call ncvgt(fileid,reaction_emitter_id,nc_corner,nc_edge,reaction_e
     &mitter,nc_stat)
      reaction_reagent_num_id=ncvid(fileid,'reaction_reagent_num',nc_sta
     &t)
      call ncvinq(fileid,reaction_reagent_num_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.reaction_ind_id)continue
      
      reaction_reagent_num =>mem_alloc_i1((1),(rc_num),'reaction_reagent
     &_num')
      nc_corner(1)=1
      nc_edge(1)=((rc_num)-(1)+1)
      call ncvgt(fileid,reaction_reagent_num_id,nc_corner,nc_edge,reacti
     &on_reagent_num,nc_stat)
      reaction_generic_id=ncvid(fileid,'reaction_generic',nc_stat)
      call ncvinq(fileid,reaction_generic_id,nc_dummy,nc_type,nc_rank,nc
     &_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.reaction_ind_id)continue
      
      reaction_generic =>mem_alloc_i1((1),(rc_num),'reaction_generic')
      nc_corner(1)=1
      nc_edge(1)=((rc_num)-(1)+1)
      call ncvgt(fileid,reaction_generic_id,nc_corner,nc_edge,reaction_g
     &eneric,nc_stat)
      reaction_product_num_id=ncvid(fileid,'reaction_product_num',nc_sta
     &t)
      call ncvinq(fileid,reaction_product_num_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.reaction_ind_id)continue
      
      reaction_product_num =>mem_alloc_i1((1),(rc_num),'reaction_product
     &_num')
      nc_corner(1)=1
      nc_edge(1)=((rc_num)-(1)+1)
      call ncvgt(fileid,reaction_product_num_id,nc_corner,nc_edge,reacti
     &on_product_num,nc_stat)
      reaction_reagent_id=ncvid(fileid,'reaction_reagent',nc_stat)
      call ncvinq(fileid,reaction_reagent_id,nc_dummy,nc_type,nc_rank,nc
     &_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.reagent_ind_id)continue
      if(nc_dims(2).EQ.reaction_ind_id)continue
      
      reaction_reagent =>mem_alloc_i2((1),(2),(1),(rc_num),'reaction_rea
     &gent')
      nc_corner(1)=1
      nc_edge(1)=((2)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((rc_num)-(1)+1)
      call ncvgt(fileid,reaction_reagent_id,nc_corner,nc_edge,reaction_r
     &eagent,nc_stat)
      reaction_product_id=ncvid(fileid,'reaction_product',nc_stat)
      call ncvinq(fileid,reaction_product_id,nc_dummy,nc_type,nc_rank,nc
     &_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.product_ind_id)continue
      if(nc_dims(2).EQ.reaction_ind_id)continue
      
      reaction_product =>mem_alloc_i2((1),(4),(1),(rc_num),'reaction_pro
     &duct')
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((rc_num)-(1)+1)
      call ncvgt(fileid,reaction_product_id,nc_corner,nc_edge,reaction_p
     &roduct,nc_stat)
      reaction_filename_id=ncvid(fileid,'reaction_filename',nc_stat)
      call ncvinq(fileid,reaction_filename_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.reaction_filename_string_id)continue
      if(nc_dims(2).EQ.reaction_ind_id)continue
      
      reaction_filename =>mem_alloc_c1((96),(1),(rc_num),'reaction_filen
     &ame')
      nc_corner(1)=1
      nc_edge(1)=((96)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((rc_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,reaction_filename_id,nc_corner,nc_
     &edge,reaction_filename)
      
      call ncclos(fileid,nc_stat)
      end if
      call MPI_bcast(rc_num,1,MPI_INTEGER,mpi_degas2_root,comm_world_dup
     &,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_name =>mem_alloc_c1((80),(1),(rc_num),'reaction_name')
      endif
      call MPI_bcast(reaction_name,(((80)-(1)+1)*((rc_num)-(1)+1)),MPI_C
     &HARACTER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_type =>mem_alloc_c1((32),(1),(rc_num),'reaction_type')
      endif
      call MPI_bcast(reaction_type,(((32)-(1)+1)*((rc_num)-(1)+1)),MPI_C
     &HARACTER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_sy =>mem_alloc_c1((24),(1),(rc_num),'reaction_sy')
      endif
      call MPI_bcast(reaction_sy,(((24)-(1)+1)*((rc_num)-(1)+1)),MPI_CHA
     &RACTER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_emitter =>mem_alloc_i1((1),(rc_num),'reaction_emitter')
      endif
      call MPI_bcast(reaction_emitter,(((rc_num)-(1)+1)),MPI_INTEGER,mpi
     &_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_reagent_num =>mem_alloc_i1((1),(rc_num),'reaction_reagent
     &_num')
      endif
      call MPI_bcast(reaction_reagent_num,(((rc_num)-(1)+1)),MPI_INTEGER
     &,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_generic =>mem_alloc_i1((1),(rc_num),'reaction_generic')
      endif
      call MPI_bcast(reaction_generic,(((rc_num)-(1)+1)),MPI_INTEGER,mpi
     &_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_product_num =>mem_alloc_i1((1),(rc_num),'reaction_product
     &_num')
      endif
      call MPI_bcast(reaction_product_num,(((rc_num)-(1)+1)),MPI_INTEGER
     &,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_reagent =>mem_alloc_i2((1),(2),(1),(rc_num),'reaction_rea
     &gent')
      endif
      call MPI_bcast(reaction_reagent,(((2)-(1)+1)*((rc_num)-(1)+1)),MPI
     &_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_product =>mem_alloc_i2((1),(4),(1),(rc_num),'reaction_pro
     &duct')
      endif
      call MPI_bcast(reaction_product,(((4)-(1)+1)*((rc_num)-(1)+1)),MPI
     &_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_filename =>mem_alloc_c1((96),(1),(rc_num),'reaction_filen
     &ame')
      endif
      call MPI_bcast(reaction_filename,(((96)-(1)+1)*((rc_num)-(1)+1)),M
     &PI_CHARACTER,mpi_degas2_root,comm_world_dup,mpi_err)
      
      return
      end
      subroutine nc_read_background
      
      use zn_mod
      
      use bk_mod
      
      use so_mod
      
      use rf_mod
      
      use mp_mod
      
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
      
      integer nc_dims(5),nc_corner(5),nc_edge(5),nc_stat,nc_size,nc_rank
     &,nc_attr,nc_type
      character*128 nc_dummy
      external ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf
     &_inq_format
      integer ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf_
     &inq_format
      integer vector_id
      integer string_id
      
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
      
      REAL(kind=DOUBLE)vector_temp(3)
      if((mpi_rank.EQ.mpi_degas2_root))then
      tempfile=filenames_array(2)
      if(tempfile.NE.'undefined')continue
      fileid=ncopn(tempfile,0,nc_stat)
      vector_id=ncdid(fileid,'vector',nc_stat)
      call ncdinq(fileid,vector_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((3)-(1)+1))continue
      string_id=ncdid(fileid,'string',nc_stat)
      call ncdinq(fileid,string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((300)-(1)+1))continue
      
      bk_num_id=ncvid(fileid,'bk_num',nc_stat)
      call ncvinq(fileid,bk_num_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      
      call ncvgt(fileid,bk_num_id,nc_corner,nc_edge,bk_num,nc_stat)
      background_ind_id=ncdid(fileid,'background_ind',nc_stat)
      call ncdinq(fileid,background_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((bk_num)-(1)+1))continue
      bk_plasma_ind_id=ncdid(fileid,'bk_plasma_ind',nc_stat)
      call ncdinq(fileid,bk_plasma_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((zone_type_num(2))-(1)+1))continue
      background_coords_id=ncvid(fileid,'background_coords',nc_stat)
      call ncvinq(fileid,background_coords_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,background_coords_id,nc_corner,nc_edge,backgroun
     &d_coords,nc_stat)
      background_n_id=ncvid(fileid,'background_n',nc_stat)
      call ncvinq(fileid,background_n_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.background_ind_id)continue
      if(nc_dims(2).EQ.bk_plasma_ind_id)continue
      
      background_n =>mem_alloc_r2((1),(bk_num),(1),(zone_type_num(2)),'b
     &ackground_n')
      nc_corner(1)=1
      nc_edge(1)=((bk_num)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((zone_type_num(2))-(1)+1)
      call ncvgt(fileid,background_n_id,nc_corner,nc_edge,background_n,n
     &c_stat)
      background_v_id=ncvid(fileid,'background_v',nc_stat)
      call ncvinq(fileid,background_v_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.vector_id)continue
      if(nc_dims(2).EQ.background_ind_id)continue
      if(nc_dims(3).EQ.bk_plasma_ind_id)continue
      
      background_v =>mem_alloc_r3((1),(3),(1),(bk_num),(1),(zone_type_nu
     &m(2)),'background_v')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((bk_num)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((zone_type_num(2))-(1)+1)
      call ncvgt(fileid,background_v_id,nc_corner,nc_edge,background_v,n
     &c_stat)
      background_temp_id=ncvid(fileid,'background_temp',nc_stat)
      call ncvinq(fileid,background_temp_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.background_ind_id)continue
      if(nc_dims(2).EQ.bk_plasma_ind_id)continue
      
      background_temp =>mem_alloc_r2((1),(bk_num),(1),(zone_type_num(2))
     &,'background_temp')
      nc_corner(1)=1
      nc_edge(1)=((bk_num)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((zone_type_num(2))-(1)+1)
      call ncvgt(fileid,background_temp_id,nc_corner,nc_edge,background_
     &temp,nc_stat)
      
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
      end if
      
      call MPI_bcast(bk_num,1,MPI_INTEGER,mpi_degas2_root,comm_world_dup
     &,mpi_err)
      call MPI_bcast(background_coords,1,MPI_INTEGER,mpi_degas2_root,com
     &m_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      background_n =>mem_alloc_r2((1),(bk_num),(1),(zone_type_num(2)),'b
     &ackground_n')
      endif
      call MPI_bcast(background_n,(((bk_num)-(1)+1)*((zone_type_num(2))-
     &(1)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_er
     &r)
      if(mpi_rank.NE.mpi_degas2_root)then
      background_v =>mem_alloc_r3((1),(3),(1),(bk_num),(1),(zone_type_nu
     &m(2)),'background_v')
      endif
      call MPI_bcast(background_v,(((3)-(1)+1)*((bk_num)-(1)+1)*((zone_t
     &ype_num(2))-(1)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_worl
     &d_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      background_temp =>mem_alloc_r2((1),(bk_num),(1),(zone_type_num(2))
     &,'background_temp')
      endif
      call MPI_bcast(background_temp,(((bk_num)-(1)+1)*((zone_type_num(2
     &))-(1)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi
     &_err)
      
      call MPI_bcast(so_grps,1,MPI_INTEGER,mpi_degas2_root,comm_world_du
     &p,mpi_err)
      call MPI_bcast(so_seg_tot,1,MPI_INTEGER,mpi_degas2_root,comm_world
     &_dup,mpi_err)
      call MPI_bcast(so_restart,1,MPI_INTEGER,mpi_degas2_root,comm_world
     &_dup,mpi_err)
      call MPI_bcast(so_spaced_seeds,1,MPI_INTEGER,mpi_degas2_root,comm_
     &world_dup,mpi_err)
      call MPI_bcast(so_seed_spacing,1,MPI_INTEGER,mpi_degas2_root,comm_
     &world_dup,mpi_err)
      call MPI_bcast(so_sampling,1,MPI_INTEGER,mpi_degas2_root,comm_worl
     &d_dup,mpi_err)
      call MPI_bcast(so_time_dependent,1,MPI_INTEGER,mpi_degas2_root,com
     &m_world_dup,mpi_err)
      call MPI_bcast(so_time_initialization,1,MPI_INTEGER,mpi_degas2_roo
     &t,comm_world_dup,mpi_err)
      call MPI_bcast(so_time_initial,1,MPI_DOUBLE_PRECISION,mpi_degas2_r
     &oot,comm_world_dup,mpi_err)
      call MPI_bcast(so_time_final,1,MPI_DOUBLE_PRECISION,mpi_degas2_roo
     &t,comm_world_dup,mpi_err)
      call MPI_bcast(so_rel_wt_min,1,MPI_DOUBLE_PRECISION,mpi_degas2_roo
     &t,comm_world_dup,mpi_err)
      call MPI_bcast(so_rel_wt_max,1,MPI_DOUBLE_PRECISION,mpi_degas2_roo
     &t,comm_world_dup,mpi_err)
      call MPI_bcast(so_wt_norm_min,1,MPI_DOUBLE_PRECISION,mpi_degas2_ro
     &ot,comm_world_dup,mpi_err)
      call MPI_bcast(so_wt_norm_max,1,MPI_DOUBLE_PRECISION,mpi_degas2_ro
     &ot,comm_world_dup,mpi_err)
      call MPI_bcast(so_gparams_list_size,1,MPI_INTEGER,mpi_degas2_root,
     &comm_world_dup,mpi_err)
      call MPI_bcast(so_gparams_list_dim,1,MPI_INTEGER,mpi_degas2_root,c
     &omm_world_dup,mpi_err)
      call MPI_bcast(so_params_list_size,1,MPI_INTEGER,mpi_degas2_root,c
     &omm_world_dup,mpi_err)
      call MPI_bcast(so_params_list_dim,1,MPI_INTEGER,mpi_degas2_root,co
     &mm_world_dup,mpi_err)
      call MPI_bcast(so_params_data_size,1,MPI_INTEGER,mpi_degas2_root,c
     &omm_world_dup,mpi_err)
      call MPI_bcast(so_params_data_dim,1,MPI_INTEGER,mpi_degas2_root,co
     &mm_world_dup,mpi_err)
      call MPI_bcast(so_giparams_list_size,1,MPI_INTEGER,mpi_degas2_root
     &,comm_world_dup,mpi_err)
      call MPI_bcast(so_giparams_list_dim,1,MPI_INTEGER,mpi_degas2_root,
     &comm_world_dup,mpi_err)
      call MPI_bcast(so_iparams_list_size,1,MPI_INTEGER,mpi_degas2_root,
     &comm_world_dup,mpi_err)
      call MPI_bcast(so_iparams_list_dim,1,MPI_INTEGER,mpi_degas2_root,c
     &omm_world_dup,mpi_err)
      call MPI_bcast(so_iparams_data_size,1,MPI_INTEGER,mpi_degas2_root,
     &comm_world_dup,mpi_err)
      call MPI_bcast(so_iparams_data_dim,1,MPI_INTEGER,mpi_degas2_root,c
     &omm_world_dup,mpi_err)
      call MPI_bcast(so_seed_decimal,(((34)-(1)+1)),MPI_CHARACTER,mpi_de
     &gas2_root,comm_world_dup,mpi_err)
      call MPI_bcast(source_name,(((10)-(1)+1)*((6)-(1)+1)),MPI_CHARACTE
     &R,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_base_ptr =>mem_alloc_i1((1),(so_grps),'source_base_ptr')
      endif
      call MPI_bcast(source_base_ptr,(((so_grps)-(1)+1)),MPI_INTEGER,mpi
     &_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_num_segments =>mem_alloc_i1((1),(so_grps),'source_num_segme
     &nts')
      endif
      call MPI_bcast(source_num_segments,(((so_grps)-(1)+1)),MPI_INTEGER
     &,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_type =>mem_alloc_i1((1),(so_grps),'source_type')
      endif
      call MPI_bcast(source_type,(((so_grps)-(1)+1)),MPI_INTEGER,mpi_deg
     &as2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_geometry =>mem_alloc_i1((1),(so_grps),'source_geometry')
      endif
      call MPI_bcast(source_geometry,(((so_grps)-(1)+1)),MPI_INTEGER,mpi
     &_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_num_flights =>mem_alloc_i1((1),(so_grps),'source_num_flight
     &s')
      endif
      call MPI_bcast(source_num_flights,(((so_grps)-(1)+1)),MPI_INTEGER,
     &mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_num_checkpoints =>mem_alloc_i1((1),(so_grps),'source_num_ch
     &eckpoints')
      endif
      call MPI_bcast(source_num_checkpoints,(((so_grps)-(1)+1)),MPI_INTE
     &GER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_species =>mem_alloc_i1((1),(so_grps),'source_species')
      endif
      call MPI_bcast(source_species,(((so_grps)-(1)+1)),MPI_INTEGER,mpi_
     &degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_root_species =>mem_alloc_i1((1),(so_grps),'source_root_spec
     &ies')
      endif
      call MPI_bcast(source_root_species,(((so_grps)-(1)+1)),MPI_INTEGER
     &,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_time_variation =>mem_alloc_i1((1),(so_grps),'source_time_va
     &riation')
      endif
      call MPI_bcast(source_time_variation,(((so_grps)-(1)+1)),MPI_INTEG
     &ER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_num_gparameters =>mem_alloc_i1((1),(so_grps),'source_num_gp
     &arameters')
      endif
      call MPI_bcast(source_num_gparameters,(((so_grps)-(1)+1)),MPI_INTE
     &GER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_num_parameters =>mem_alloc_i1((1),(so_grps),'source_num_par
     &ameters')
      endif
      call MPI_bcast(source_num_parameters,(((so_grps)-(1)+1)),MPI_INTEG
     &ER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_gparameters_list =>mem_alloc_i1((1),(so_gparams_list_dim),'
     &source_gparameters_list')
      endif
      call MPI_bcast(source_gparameters_list,(((so_gparams_list_dim)-(1)
     &+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_parameters_list =>mem_alloc_i1((1),(so_params_list_dim),'so
     &urce_parameters_list')
      endif
      call MPI_bcast(source_parameters_list,(((so_params_list_dim)-(1)+1
     &)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_gparameters_base =>mem_alloc_i1((1),(so_grps),'source_gpara
     &meters_base')
      endif
      call MPI_bcast(source_gparameters_base,(((so_grps)-(1)+1)),MPI_INT
     &EGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_parameters_base =>mem_alloc_i1((1),(so_grps),'source_parame
     &ters_base')
      endif
      call MPI_bcast(source_parameters_base,(((so_grps)-(1)+1)),MPI_INTE
     &GER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_parameters_data_base =>mem_alloc_i1((1),(so_grps),'source_p
     &arameters_data_base')
      endif
      call MPI_bcast(source_parameters_data_base,(((so_grps)-(1)+1)),MPI
     &_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_gparameters_data =>mem_alloc_r1((1),(so_gparams_list_dim),'
     &source_gparameters_data')
      endif
      call MPI_bcast(source_gparameters_data,(((so_gparams_list_dim)-(1)
     &+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_parameters_data =>mem_alloc_r1((1),(so_params_data_dim),'so
     &urce_parameters_data')
      endif
      call MPI_bcast(source_parameters_data,(((so_params_data_dim)-(1)+1
     &)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_num_giparameters =>mem_alloc_i1((1),(so_grps),'source_num_g
     &iparameters')
      endif
      call MPI_bcast(source_num_giparameters,(((so_grps)-(1)+1)),MPI_INT
     &EGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_num_iparameters =>mem_alloc_i1((1),(so_grps),'source_num_ip
     &arameters')
      endif
      call MPI_bcast(source_num_iparameters,(((so_grps)-(1)+1)),MPI_INTE
     &GER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_giparameters_list =>mem_alloc_i1((1),(so_giparams_list_dim)
     &,'source_giparameters_list')
      endif
      call MPI_bcast(source_giparameters_list,(((so_giparams_list_dim)-(
     &1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_iparameters_list =>mem_alloc_i1((1),(so_iparams_list_dim),'
     &source_iparameters_list')
      endif
      call MPI_bcast(source_iparameters_list,(((so_iparams_list_dim)-(1)
     &+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_giparameters_base =>mem_alloc_i1((1),(so_grps),'source_gipa
     &rameters_base')
      endif
      call MPI_bcast(source_giparameters_base,(((so_grps)-(1)+1)),MPI_IN
     &TEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_iparameters_base =>mem_alloc_i1((1),(so_grps),'source_ipara
     &meters_base')
      endif
      call MPI_bcast(source_iparameters_base,(((so_grps)-(1)+1)),MPI_INT
     &EGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_iparameters_data_base =>mem_alloc_i1((1),(so_grps),'source_
     &iparameters_data_base')
      endif
      call MPI_bcast(source_iparameters_data_base,(((so_grps)-(1)+1)),MP
     &I_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_giparameters_data =>mem_alloc_i1((1),(so_giparams_list_dim)
     &,'source_giparameters_data')
      endif
      call MPI_bcast(source_giparameters_data,(((so_giparams_list_dim)-(
     &1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_iparameters_data =>mem_alloc_i1((1),(so_iparams_data_dim),'
     &source_iparameters_data')
      endif
      call MPI_bcast(source_iparameters_data,(((so_iparams_data_dim)-(1)
     &+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_total_current =>mem_alloc_r1((1),(so_grps),'source_total_cu
     &rrent')
      endif
      call MPI_bcast(source_total_current,(((so_grps)-(1)+1)),MPI_DOUBLE
     &_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_weight_norm =>mem_alloc_r1((1),(so_grps),'source_weight_nor
     &m')
      endif
      call MPI_bcast(source_weight_norm,(((so_grps)-(1)+1)),MPI_DOUBLE_P
     &RECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_scale_factor =>mem_alloc_r1((1),(so_grps),'source_scale_fac
     &tor')
      endif
      call MPI_bcast(source_scale_factor,(((so_grps)-(1)+1)),MPI_DOUBLE_
     &PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_segment_ptr =>mem_alloc_i1((1),(so_seg_tot),'source_segment
     &_ptr')
      endif
      call MPI_bcast(source_segment_ptr,(((so_seg_tot)-(1)+1)),MPI_INTEG
     &ER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_current =>mem_alloc_r1((1),(so_seg_tot),'source_current')
      endif
      call MPI_bcast(source_current,(((so_seg_tot)-(1)+1)),MPI_DOUBLE_PR
     &ECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_segment_rel_wt =>mem_alloc_r1((1),(so_seg_tot),'source_segm
     &ent_rel_wt')
      endif
      call MPI_bcast(source_segment_rel_wt,(((so_seg_tot)-(1)+1)),MPI_DO
     &UBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_segment_prob_alias =>mem_alloc_r1((1),(so_seg_tot),'source_
     &segment_prob_alias')
      endif
      call MPI_bcast(source_segment_prob_alias,(((so_seg_tot)-(1)+1)),MP
     &I_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      source_segment_ptr_alias =>mem_alloc_i1((1),(so_seg_tot),'source_s
     &egment_ptr_alias')
      endif
      call MPI_bcast(source_segment_ptr_alias,(((so_seg_tot)-(1)+1)),MPI
     &_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      
      return
      end
      function species_el_count(s,el)
      
      use sp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer species_el_count
      integer s
      integer el
      integer i,j
      j=0
      do i=1,species_ncomp(s)
      if(species_el(i,s).EQ.el)then
      j=i
      go to 90007
      end if
      end do
      species_el_count=0
      return
90007 continue
      species_el_count=species_count(j,s)
      return
      end
      function species_add_check(n1,list1,n2,list2)
      
      use sp_mod
      
      use el_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical species_add_check
      integer n1,n2
      integer list1(n1)
      integer list2(n2)
      integer z_sum1,z_sum2,i,ic,count_sum1,count_sum2
      REAL(kind=DOUBLE)m_sum1,m_sum2
      external species_el_count
      integer species_el_count
      species_add_check=.FALSE.
      z_sum1=0
      m_sum1=(0.0_DOUBLE)
      do i=1,n1
      z_sum1=z_sum1+species_z(list1(i))
      m_sum1=m_sum1+species_m(list1(i))
      end do
      z_sum2=0
      m_sum2=(0.0_DOUBLE)
      do i=1,n2
      z_sum2=z_sum2+species_z(list2(i))
      m_sum2=m_sum2+species_m(list2(i))
      end do
      if(z_sum1.NE.z_sum2)return
      if(abs(m_sum1-m_sum2).GT.(5*EPSILON((0.0_DOUBLE)))*max(m_sum1,m_su
     &m2))return
      
      do ic=1,el_num
      count_sum1=0
      count_sum2=0
      do i=1,n1
      count_sum1=count_sum1+species_el_count(list1(i),ic)
      end do
      do i=1,n2
      count_sum2=count_sum2+species_el_count(list2(i),ic)
      end do
      if(count_sum1.NE.count_sum2)return
      end do
      
      species_add_check=.TRUE.
      return
      end
      
      
