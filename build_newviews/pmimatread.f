      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine nc_read_materials
      
      use ma_mod
      
      use rf_mod
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer fileid
      character*96 tempfile
      integer materials_symbol_string_id
      integer materials_name_string_id
      integer ma_num_id
      integer materials_ind_id
      integer materials_name_id
      integer materials_sy_id
      
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
      tempfile=filenames_array(15)
      if(tempfile.NE.'undefined')continue
      fileid=ncopn(tempfile,0,nc_stat)
      call ncagtc(fileid,0,'data_version',materials_version,len(material
     &s_version),nc_stat)
      materials_symbol_string_id=ncdid(fileid,'materials_symbol_string',
     &nc_stat)
      call ncdinq(fileid,materials_symbol_string_id,nc_dummy,nc_size,nc_
     &stat)
      if(nc_size.EQ.((8)-(1)+1))continue
      materials_name_string_id=ncdid(fileid,'materials_name_string',nc_s
     &tat)
      call ncdinq(fileid,materials_name_string_id,nc_dummy,nc_size,nc_st
     &at)
      if(nc_size.EQ.((32)-(1)+1))continue
      ma_num_id=ncvid(fileid,'ma_num',nc_stat)
      call ncvinq(fileid,ma_num_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      
      call ncvgt(fileid,ma_num_id,nc_corner,nc_edge,ma_num,nc_stat)
      materials_ind_id=ncdid(fileid,'materials_ind',nc_stat)
      call ncdinq(fileid,materials_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((ma_num)-(1)+1))continue
      materials_name_id=ncvid(fileid,'materials_name',nc_stat)
      call ncvinq(fileid,materials_name_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.materials_name_string_id)continue
      if(nc_dims(2).EQ.materials_ind_id)continue
      
      materials_name =>mem_alloc_c1((32),(1),(ma_num),'materials_name')
      nc_corner(1)=1
      nc_edge(1)=((32)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((ma_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,materials_name_id,nc_corner,nc_edg
     &e,materials_name)
      materials_sy_id=ncvid(fileid,'materials_sy',nc_stat)
      call ncvinq(fileid,materials_sy_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.materials_symbol_string_id)continue
      if(nc_dims(2).EQ.materials_ind_id)continue
      
      materials_sy =>mem_alloc_c1((8),(1),(ma_num),'materials_sy')
      nc_corner(1)=1
      nc_edge(1)=((8)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((ma_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,materials_sy_id,nc_corner,nc_edge,
     &materials_sy)
      
      call ncclos(fileid,nc_stat)
      end if
      call MPI_bcast(ma_num,1,MPI_INTEGER,mpi_degas2_root,comm_world_dup
     &,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      materials_name =>mem_alloc_c1((32),(1),(ma_num),'materials_name')
      endif
      call MPI_bcast(materials_name,(((32)-(1)+1)*((ma_num)-(1)+1)),MPI_
     &CHARACTER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      materials_sy =>mem_alloc_c1((8),(1),(ma_num),'materials_sy')
      endif
      call MPI_bcast(materials_sy,(((8)-(1)+1)*((ma_num)-(1)+1)),MPI_CHA
     &RACTER,mpi_degas2_root,comm_world_dup,mpi_err)
      
      return
      end
      subroutine nc_read_pmi
      
      use pm_mod
      
      use rf_mod
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer fileid
      character*96 tempfile
      integer pmi_symbol_string_id
      integer pmi_name_string_id
      integer pmi_type_string_id
      integer pm_num_id
      integer pm_ignore_id
      integer pmi_ind_id
      integer pmi_filename_string_id
      integer pmi_product_ind_id
      integer pmi_name_id
      integer pmi_type_id
      integer pmi_sy_id
      integer pmi_reagent_id
      integer pmi_materials_id
      integer pmi_generic_id
      integer pmi_product_num_id
      integer pmi_product_id
      integer pmi_filename_id
      
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
      tempfile=filenames_array(17)
      if(tempfile.NE.'undefined')continue
      fileid=ncopn(tempfile,0,nc_stat)
      call ncagtc(fileid,0,'data_version',pmi_version,len(pmi_version),n
     &c_stat)
      pmi_symbol_string_id=ncdid(fileid,'pmi_symbol_string',nc_stat)
      call ncdinq(fileid,pmi_symbol_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((24)-(1)+1))continue
      pmi_name_string_id=ncdid(fileid,'pmi_name_string',nc_stat)
      call ncdinq(fileid,pmi_name_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((80)-(1)+1))continue
      pmi_type_string_id=ncdid(fileid,'pmi_type_string',nc_stat)
      call ncdinq(fileid,pmi_type_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((32)-(1)+1))continue
      pm_num_id=ncvid(fileid,'pm_num',nc_stat)
      call ncvinq(fileid,pm_num_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      
      call ncvgt(fileid,pm_num_id,nc_corner,nc_edge,pm_num,nc_stat)
      pm_ignore_id=ncvid(fileid,'pm_ignore',nc_stat)
      call ncvinq(fileid,pm_ignore_id,nc_dummy,nc_type,nc_rank,nc_dims,n
     &c_attr,nc_stat)
      
      call ncvgt(fileid,pm_ignore_id,nc_corner,nc_edge,pm_ignore,nc_stat
     &)
      pmi_ind_id=ncdid(fileid,'pmi_ind',nc_stat)
      call ncdinq(fileid,pmi_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((pm_num)-(1)+1))continue
      pmi_filename_string_id=ncdid(fileid,'pmi_filename_string',nc_stat)
      call ncdinq(fileid,pmi_filename_string_id,nc_dummy,nc_size,nc_stat
     &)
      if(nc_size.EQ.((96)-(1)+1))continue
      pmi_product_ind_id=ncdid(fileid,'pmi_product_ind',nc_stat)
      call ncdinq(fileid,pmi_product_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((4)-(1)+1))continue
      pmi_name_id=ncvid(fileid,'pmi_name',nc_stat)
      call ncvinq(fileid,pmi_name_id,nc_dummy,nc_type,nc_rank,nc_dims,nc
     &_attr,nc_stat)
      if(nc_dims(1).EQ.pmi_name_string_id)continue
      if(nc_dims(2).EQ.pmi_ind_id)continue
      
      pmi_name =>mem_alloc_c1((80),(1),(pm_num),'pmi_name')
      nc_corner(1)=1
      nc_edge(1)=((80)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pm_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,pmi_name_id,nc_corner,nc_edge,pmi_
     &name)
      pmi_type_id=ncvid(fileid,'pmi_type',nc_stat)
      call ncvinq(fileid,pmi_type_id,nc_dummy,nc_type,nc_rank,nc_dims,nc
     &_attr,nc_stat)
      if(nc_dims(1).EQ.pmi_type_string_id)continue
      if(nc_dims(2).EQ.pmi_ind_id)continue
      
      pmi_type =>mem_alloc_c1((32),(1),(pm_num),'pmi_type')
      nc_corner(1)=1
      nc_edge(1)=((32)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pm_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,pmi_type_id,nc_corner,nc_edge,pmi_
     &type)
      pmi_sy_id=ncvid(fileid,'pmi_sy',nc_stat)
      call ncvinq(fileid,pmi_sy_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      if(nc_dims(1).EQ.pmi_symbol_string_id)continue
      if(nc_dims(2).EQ.pmi_ind_id)continue
      
      pmi_sy =>mem_alloc_c1((24),(1),(pm_num),'pmi_sy')
      nc_corner(1)=1
      nc_edge(1)=((24)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pm_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,pmi_sy_id,nc_corner,nc_edge,pmi_sy
     &)
      pmi_reagent_id=ncvid(fileid,'pmi_reagent',nc_stat)
      call ncvinq(fileid,pmi_reagent_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pmi_ind_id)continue
      
      pmi_reagent =>mem_alloc_i1((1),(pm_num),'pmi_reagent')
      nc_corner(1)=1
      nc_edge(1)=((pm_num)-(1)+1)
      call ncvgt(fileid,pmi_reagent_id,nc_corner,nc_edge,pmi_reagent,nc_
     &stat)
      pmi_materials_id=ncvid(fileid,'pmi_materials',nc_stat)
      call ncvinq(fileid,pmi_materials_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pmi_ind_id)continue
      
      pmi_materials =>mem_alloc_i1((1),(pm_num),'pmi_materials')
      nc_corner(1)=1
      nc_edge(1)=((pm_num)-(1)+1)
      call ncvgt(fileid,pmi_materials_id,nc_corner,nc_edge,pmi_materials
     &,nc_stat)
      pmi_generic_id=ncvid(fileid,'pmi_generic',nc_stat)
      call ncvinq(fileid,pmi_generic_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pmi_ind_id)continue
      
      pmi_generic =>mem_alloc_i1((1),(pm_num),'pmi_generic')
      nc_corner(1)=1
      nc_edge(1)=((pm_num)-(1)+1)
      call ncvgt(fileid,pmi_generic_id,nc_corner,nc_edge,pmi_generic,nc_
     &stat)
      pmi_product_num_id=ncvid(fileid,'pmi_product_num',nc_stat)
      call ncvinq(fileid,pmi_product_num_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pmi_ind_id)continue
      
      pmi_product_num =>mem_alloc_i1((1),(pm_num),'pmi_product_num')
      nc_corner(1)=1
      nc_edge(1)=((pm_num)-(1)+1)
      call ncvgt(fileid,pmi_product_num_id,nc_corner,nc_edge,pmi_product
     &_num,nc_stat)
      pmi_product_id=ncvid(fileid,'pmi_product',nc_stat)
      call ncvinq(fileid,pmi_product_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pmi_product_ind_id)continue
      if(nc_dims(2).EQ.pmi_ind_id)continue
      
      pmi_product =>mem_alloc_i2((1),(4),(1),(pm_num),'pmi_product')
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pm_num)-(1)+1)
      call ncvgt(fileid,pmi_product_id,nc_corner,nc_edge,pmi_product,nc_
     &stat)
      pmi_filename_id=ncvid(fileid,'pmi_filename',nc_stat)
      call ncvinq(fileid,pmi_filename_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pmi_filename_string_id)continue
      if(nc_dims(2).EQ.pmi_ind_id)continue
      
      pmi_filename =>mem_alloc_c1((96),(1),(pm_num),'pmi_filename')
      nc_corner(1)=1
      nc_edge(1)=((96)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pm_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,pmi_filename_id,nc_corner,nc_edge,
     &pmi_filename)
      
      call ncclos(fileid,nc_stat)
      end if
      call MPI_bcast(pm_num,1,MPI_INTEGER,mpi_degas2_root,comm_world_dup
     &,mpi_err)
      call MPI_bcast(pm_ignore,1,MPI_INTEGER,mpi_degas2_root,comm_world_
     &dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_name =>mem_alloc_c1((80),(1),(pm_num),'pmi_name')
      endif
      call MPI_bcast(pmi_name,(((80)-(1)+1)*((pm_num)-(1)+1)),MPI_CHARAC
     &TER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_type =>mem_alloc_c1((32),(1),(pm_num),'pmi_type')
      endif
      call MPI_bcast(pmi_type,(((32)-(1)+1)*((pm_num)-(1)+1)),MPI_CHARAC
     &TER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_sy =>mem_alloc_c1((24),(1),(pm_num),'pmi_sy')
      endif
      call MPI_bcast(pmi_sy,(((24)-(1)+1)*((pm_num)-(1)+1)),MPI_CHARACTE
     &R,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_reagent =>mem_alloc_i1((1),(pm_num),'pmi_reagent')
      endif
      call MPI_bcast(pmi_reagent,(((pm_num)-(1)+1)),MPI_INTEGER,mpi_dega
     &s2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_materials =>mem_alloc_i1((1),(pm_num),'pmi_materials')
      endif
      call MPI_bcast(pmi_materials,(((pm_num)-(1)+1)),MPI_INTEGER,mpi_de
     &gas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_generic =>mem_alloc_i1((1),(pm_num),'pmi_generic')
      endif
      call MPI_bcast(pmi_generic,(((pm_num)-(1)+1)),MPI_INTEGER,mpi_dega
     &s2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_product_num =>mem_alloc_i1((1),(pm_num),'pmi_product_num')
      endif
      call MPI_bcast(pmi_product_num,(((pm_num)-(1)+1)),MPI_INTEGER,mpi_
     &degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_product =>mem_alloc_i2((1),(4),(1),(pm_num),'pmi_product')
      endif
      call MPI_bcast(pmi_product,(((4)-(1)+1)*((pm_num)-(1)+1)),MPI_INTE
     &GER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_filename =>mem_alloc_c1((96),(1),(pm_num),'pmi_filename')
      endif
      call MPI_bcast(pmi_filename,(((96)-(1)+1)*((pm_num)-(1)+1)),MPI_CH
     &ARACTER,mpi_degas2_root,comm_world_dup,mpi_err)
      
      return
      end
      subroutine nc_read_problem
      
      use pr_mod
      
      use rd_mod
      
      use pd_mod
      
      use ma_mod
      
      use pm_mod
      
      use sp_mod
      
      use rf_mod
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      integer fileid
      character*96 tempfile
      integer pr_test_num_id
      integer pr_background_num_id
      integer pr_ex_test_num_id
      integer pr_ex_test_dim_id
      integer pr_reaction_num_id
      integer pr_reaction_dim_id
      integer pr_bkrc_num_id
      integer pr_bkrc_dim_id
      integer pr_exrc_num_id
      integer pr_exrc_dim_id
      integer pr_materials_num_id
      integer pr_pmi_num_id
      integer pr_var0_num_id
      integer problem_test_ind_id
      integer problem_background_ind_id
      integer problem_ex_test_ind_id
      integer problem_reaction_ind_id
      integer problem_reaction_ind0_id
      integer problem_bkrc_ind_id
      integer problem_bkrc_rg_ind_id
      integer problem_exrc_ind_id
      integer problem_species_ind_id
      integer problem_materials_ref_ind_id
      integer problem_materials_sub_ind_id
      integer problem_pmi_ref_ind_id
      integer problem_pmi_sub_ind_id
      integer problem_pmi_ind0_id
      integer problem_product_ind_id
      integer problem_arr_ind_id
      integer pr_var0_list_ind_id
      integer pr_tag_string_id
      integer problem_species_test_id
      integer problem_species_background_id
      integer problem_materials_sub_id
      integer problem_test_sp_id
      integer problem_background_sp_id
      integer problem_ex_test_sp_id
      integer problem_materials_ref_id
      integer problem_rc_id
      integer problem_reaction_num_id
      integer problem_test_reaction_id
      integer problem_test_background_id
      integer problem_num_arrangements_id
      integer problem_test_products_id
      integer problem_prod_mult_id
      integer problem_background_reaction_id
      integer problem_bkrc_reagents_id
      integer problem_bkrc_products_id
      integer problem_external_reaction_id
      integer problem_external_test_reaction_num_id
      integer problem_external_test_reaction_id
      integer problem_external_test_background_id
      integer problem_exrc_products_id
      integer problem_pmi_ref_id
      integer problem_pmi_sub_id
      integer problem_pmi_case_num_id
      integer problem_pmi_cases_id
      integer problem_pmi_num_arrange_id
      integer problem_pmi_products_id
      integer problem_pmi_prod_mult_id
      integer pr_var0_list_id
      
      integer rd_rank_ind_id
      integer rd_dep_var_ind_id
      integer rd_rank_ind0_id
      integer rd_tag_string_id
      integer rd_eval_string_id
      integer reaction_rate_size_id
      integer rd_table_ind_id
      integer reaction_handling_size_id
      integer rd_handling_table_ind_id
      integer reaction_rate_min_id
      integer reaction_rate_delta_id
      integer reaction_rate_rank_id
      integer reaction_rate_spacing_id
      integer reaction_rate_eval_name_id
      integer reaction_rate_tab_index_id
      integer reaction_rate_var_id
      integer reaction_rate_num_rand_id
      integer reaction_rate_base_id
      integer reaction_rate_tab_id
      integer reaction_handling_min_id
      integer reaction_handling_delta_id
      integer reaction_handling_rank_id
      integer reaction_handling_spacing_id
      integer reaction_handling_eval_name_id
      integer reaction_handling_tab_index_id
      integer reaction_handling_var0_id
      integer reaction_handling_var_id
      integer reaction_handling_num_rand_id
      integer reaction_handling_base_id
      integer reaction_handling_tab_id
      
      integer pd_rank_ind_id
      integer pd_dep_var_ind_id
      integer pd_rank_ind0_id
      integer pd_tag_string_id
      integer pd_eval_string_id
      integer pmi_yield_size_id
      integer pd_yield_ind_id
      integer pmi_handling_size_id
      integer pd_handling_ind_id
      integer pmi_yield_min_id
      integer pmi_yield_delta_id
      integer pmi_yield_rank_id
      integer pmi_yield_spacing_id
      integer pmi_yield_eval_name_id
      integer pmi_yield_tab_index_id
      integer pmi_yield_var_id
      integer pmi_yield_num_rand_id
      integer pmi_yield_base_id
      integer pmi_yield_tab_id
      integer pmi_handling_min_id
      integer pmi_handling_delta_id
      integer pmi_handling_rank_id
      integer pmi_handling_spacing_id
      integer pmi_handling_eval_name_id
      integer pmi_handling_tab_index_id
      integer pmi_handling_var0_id
      integer pmi_handling_var_id
      integer pmi_handling_num_rand_id
      integer pmi_handling_base_id
      integer pmi_handling_tab_id
      
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
      tempfile=filenames_array(5)
      if(tempfile.NE.'undefined')continue
      fileid=ncopn(tempfile,0,nc_stat)
      call ncagtc(fileid,0,'data_version',problem_version,len(problem_ve
     &rsion),nc_stat)
      pr_test_num_id=ncvid(fileid,'pr_test_num',nc_stat)
      call ncvinq(fileid,pr_test_num_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      
      call ncvgt(fileid,pr_test_num_id,nc_corner,nc_edge,pr_test_num,nc_
     &stat)
      pr_background_num_id=ncvid(fileid,'pr_background_num',nc_stat)
      call ncvinq(fileid,pr_background_num_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,pr_background_num_id,nc_corner,nc_edge,pr_backgr
     &ound_num,nc_stat)
      pr_ex_test_num_id=ncvid(fileid,'pr_ex_test_num',nc_stat)
      call ncvinq(fileid,pr_ex_test_num_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      
      call ncvgt(fileid,pr_ex_test_num_id,nc_corner,nc_edge,pr_ex_test_n
     &um,nc_stat)
      pr_ex_test_dim_id=ncvid(fileid,'pr_ex_test_dim',nc_stat)
      call ncvinq(fileid,pr_ex_test_dim_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      
      call ncvgt(fileid,pr_ex_test_dim_id,nc_corner,nc_edge,pr_ex_test_d
     &im,nc_stat)
      pr_reaction_num_id=ncvid(fileid,'pr_reaction_num',nc_stat)
      call ncvinq(fileid,pr_reaction_num_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,pr_reaction_num_id,nc_corner,nc_edge,pr_reaction
     &_num,nc_stat)
      pr_reaction_dim_id=ncvid(fileid,'pr_reaction_dim',nc_stat)
      call ncvinq(fileid,pr_reaction_dim_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,pr_reaction_dim_id,nc_corner,nc_edge,pr_reaction
     &_dim,nc_stat)
      pr_bkrc_num_id=ncvid(fileid,'pr_bkrc_num',nc_stat)
      call ncvinq(fileid,pr_bkrc_num_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      
      call ncvgt(fileid,pr_bkrc_num_id,nc_corner,nc_edge,pr_bkrc_num,nc_
     &stat)
      pr_bkrc_dim_id=ncvid(fileid,'pr_bkrc_dim',nc_stat)
      call ncvinq(fileid,pr_bkrc_dim_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      
      call ncvgt(fileid,pr_bkrc_dim_id,nc_corner,nc_edge,pr_bkrc_dim,nc_
     &stat)
      pr_exrc_num_id=ncvid(fileid,'pr_exrc_num',nc_stat)
      call ncvinq(fileid,pr_exrc_num_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      
      call ncvgt(fileid,pr_exrc_num_id,nc_corner,nc_edge,pr_exrc_num,nc_
     &stat)
      pr_exrc_dim_id=ncvid(fileid,'pr_exrc_dim',nc_stat)
      call ncvinq(fileid,pr_exrc_dim_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      
      call ncvgt(fileid,pr_exrc_dim_id,nc_corner,nc_edge,pr_exrc_dim,nc_
     &stat)
      pr_materials_num_id=ncvid(fileid,'pr_materials_num',nc_stat)
      call ncvinq(fileid,pr_materials_num_id,nc_dummy,nc_type,nc_rank,nc
     &_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,pr_materials_num_id,nc_corner,nc_edge,pr_materia
     &ls_num,nc_stat)
      pr_pmi_num_id=ncvid(fileid,'pr_pmi_num',nc_stat)
      call ncvinq(fileid,pr_pmi_num_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      
      call ncvgt(fileid,pr_pmi_num_id,nc_corner,nc_edge,pr_pmi_num,nc_st
     &at)
      pr_var0_num_id=ncvid(fileid,'pr_var0_num',nc_stat)
      call ncvinq(fileid,pr_var0_num_id,nc_dummy,nc_type,nc_rank,nc_dims
     &,nc_attr,nc_stat)
      
      call ncvgt(fileid,pr_var0_num_id,nc_corner,nc_edge,pr_var0_num,nc_
     &stat)
      problem_test_ind_id=ncdid(fileid,'problem_test_ind',nc_stat)
      call ncdinq(fileid,problem_test_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((pr_test_num)-(1)+1))continue
      problem_background_ind_id=ncdid(fileid,'problem_background_ind',nc
     &_stat)
      call ncdinq(fileid,problem_background_ind_id,nc_dummy,nc_size,nc_s
     &tat)
      if(nc_size.EQ.((pr_background_num)-(1)+1))continue
      problem_ex_test_ind_id=ncdid(fileid,'problem_ex_test_ind',nc_stat)
      call ncdinq(fileid,problem_ex_test_ind_id,nc_dummy,nc_size,nc_stat
     &)
      if(nc_size.EQ.((pr_ex_test_dim)-(1)+1))continue
      problem_reaction_ind_id=ncdid(fileid,'problem_reaction_ind',nc_sta
     &t)
      call ncdinq(fileid,problem_reaction_ind_id,nc_dummy,nc_size,nc_sta
     &t)
      if(nc_size.EQ.((pr_reaction_dim)-(1)+1))continue
      problem_reaction_ind0_id=ncdid(fileid,'problem_reaction_ind0',nc_s
     &tat)
      call ncdinq(fileid,problem_reaction_ind0_id,nc_dummy,nc_size,nc_st
     &at)
      if(nc_size.EQ.((15)-(1)+1))continue
      problem_bkrc_ind_id=ncdid(fileid,'problem_bkrc_ind',nc_stat)
      call ncdinq(fileid,problem_bkrc_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((pr_bkrc_dim)-(1)+1))continue
      problem_bkrc_rg_ind_id=ncdid(fileid,'problem_bkrc_rg_ind',nc_stat)
      call ncdinq(fileid,problem_bkrc_rg_ind_id,nc_dummy,nc_size,nc_stat
     &)
      if(nc_size.EQ.((2)-(1)+1))continue
      problem_exrc_ind_id=ncdid(fileid,'problem_exrc_ind',nc_stat)
      call ncdinq(fileid,problem_exrc_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((pr_exrc_dim)-(1)+1))continue
      problem_species_ind_id=ncdid(fileid,'problem_species_ind',nc_stat)
      call ncdinq(fileid,problem_species_ind_id,nc_dummy,nc_size,nc_stat
     &)
      if(nc_size.EQ.((sp_num)-(1)+1))continue
      problem_materials_ref_ind_id=ncdid(fileid,'problem_materials_ref_i
     &nd',nc_stat)
      call ncdinq(fileid,problem_materials_ref_ind_id,nc_dummy,nc_size,n
     &c_stat)
      if(nc_size.EQ.((ma_num)-(1)+1))continue
      problem_materials_sub_ind_id=ncdid(fileid,'problem_materials_sub_i
     &nd',nc_stat)
      call ncdinq(fileid,problem_materials_sub_ind_id,nc_dummy,nc_size,n
     &c_stat)
      if(nc_size.EQ.((pr_materials_num)-(1)+1))continue
      problem_pmi_ref_ind_id=ncdid(fileid,'problem_pmi_ref_ind',nc_stat)
      call ncdinq(fileid,problem_pmi_ref_ind_id,nc_dummy,nc_size,nc_stat
     &)
      if(nc_size.EQ.((pm_num)-(1)+1))continue
      problem_pmi_sub_ind_id=ncdid(fileid,'problem_pmi_sub_ind',nc_stat)
      call ncdinq(fileid,problem_pmi_sub_ind_id,nc_dummy,nc_size,nc_stat
     &)
      if(nc_size.EQ.((pr_pmi_num)-(1)+1))continue
      problem_pmi_ind0_id=ncdid(fileid,'problem_pmi_ind0',nc_stat)
      call ncdinq(fileid,problem_pmi_ind0_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((8)-(1)+1))continue
      problem_product_ind_id=ncdid(fileid,'problem_product_ind',nc_stat)
      call ncdinq(fileid,problem_product_ind_id,nc_dummy,nc_size,nc_stat
     &)
      if(nc_size.EQ.((4)-(1)+1))continue
      problem_arr_ind_id=ncdid(fileid,'problem_arr_ind',nc_stat)
      call ncdinq(fileid,problem_arr_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((4)-(1)+1))continue
      pr_var0_list_ind_id=ncdid(fileid,'pr_var0_list_ind',nc_stat)
      call ncdinq(fileid,pr_var0_list_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((pr_var0_num)-(1)+1))continue
      pr_tag_string_id=ncdid(fileid,'pr_tag_string',nc_stat)
      call ncdinq(fileid,pr_tag_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((40)-(1)+1))continue
      problem_species_test_id=ncvid(fileid,'problem_species_test',nc_sta
     &t)
      call ncvinq(fileid,problem_species_test_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_species_ind_id)continue
      
      problem_species_test =>mem_alloc_i1((1),(sp_num),'problem_species_
     &test')
      nc_corner(1)=1
      nc_edge(1)=((sp_num)-(1)+1)
      call ncvgt(fileid,problem_species_test_id,nc_corner,nc_edge,proble
     &m_species_test,nc_stat)
      problem_species_background_id=ncvid(fileid,'problem_species_backgr
     &ound',nc_stat)
      call ncvinq(fileid,problem_species_background_id,nc_dummy,nc_type,
     &nc_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_species_ind_id)continue
      
      problem_species_background =>mem_alloc_i1((1),(sp_num),'problem_sp
     &ecies_background')
      nc_corner(1)=1
      nc_edge(1)=((sp_num)-(1)+1)
      call ncvgt(fileid,problem_species_background_id,nc_corner,nc_edge,
     &problem_species_background,nc_stat)
      problem_materials_sub_id=ncvid(fileid,'problem_materials_sub',nc_s
     &tat)
      call ncvinq(fileid,problem_materials_sub_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_materials_ref_ind_id)continue
      
      problem_materials_sub =>mem_alloc_i1((1),(ma_num),'problem_materia
     &ls_sub')
      nc_corner(1)=1
      nc_edge(1)=((ma_num)-(1)+1)
      call ncvgt(fileid,problem_materials_sub_id,nc_corner,nc_edge,probl
     &em_materials_sub,nc_stat)
      problem_test_sp_id=ncvid(fileid,'problem_test_sp',nc_stat)
      call ncvinq(fileid,problem_test_sp_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_test_ind_id)continue
      
      problem_test_sp =>mem_alloc_i1((1),(pr_test_num),'problem_test_sp'
     &)
      nc_corner(1)=1
      nc_edge(1)=((pr_test_num)-(1)+1)
      call ncvgt(fileid,problem_test_sp_id,nc_corner,nc_edge,problem_tes
     &t_sp,nc_stat)
      problem_background_sp_id=ncvid(fileid,'problem_background_sp',nc_s
     &tat)
      call ncvinq(fileid,problem_background_sp_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_background_ind_id)continue
      
      problem_background_sp =>mem_alloc_i1((1),(pr_background_num),'prob
     &lem_background_sp')
      nc_corner(1)=1
      nc_edge(1)=((pr_background_num)-(1)+1)
      call ncvgt(fileid,problem_background_sp_id,nc_corner,nc_edge,probl
     &em_background_sp,nc_stat)
      problem_ex_test_sp_id=ncvid(fileid,'problem_ex_test_sp',nc_stat)
      call ncvinq(fileid,problem_ex_test_sp_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_ex_test_ind_id)continue
      
      problem_ex_test_sp =>mem_alloc_i1((1),(pr_ex_test_dim),'problem_ex
     &_test_sp')
      nc_corner(1)=1
      nc_edge(1)=((pr_ex_test_dim)-(1)+1)
      call ncvgt(fileid,problem_ex_test_sp_id,nc_corner,nc_edge,problem_
     &ex_test_sp,nc_stat)
      problem_materials_ref_id=ncvid(fileid,'problem_materials_ref',nc_s
     &tat)
      call ncvinq(fileid,problem_materials_ref_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_materials_sub_ind_id)continue
      
      problem_materials_ref =>mem_alloc_i1((1),(pr_materials_num),'probl
     &em_materials_ref')
      nc_corner(1)=1
      nc_edge(1)=((pr_materials_num)-(1)+1)
      call ncvgt(fileid,problem_materials_ref_id,nc_corner,nc_edge,probl
     &em_materials_ref,nc_stat)
      problem_rc_id=ncvid(fileid,'problem_rc',nc_stat)
      call ncvinq(fileid,problem_rc_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_reaction_ind_id)continue
      
      problem_rc =>mem_alloc_i1((1),(pr_reaction_dim),'problem_rc')
      nc_corner(1)=1
      nc_edge(1)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,problem_rc_id,nc_corner,nc_edge,problem_rc,nc_st
     &at)
      problem_reaction_num_id=ncvid(fileid,'problem_reaction_num',nc_sta
     &t)
      call ncvinq(fileid,problem_reaction_num_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_test_ind_id)continue
      
      problem_reaction_num =>mem_alloc_i1((1),(pr_test_num),'problem_rea
     &ction_num')
      nc_corner(1)=1
      nc_edge(1)=((pr_test_num)-(1)+1)
      call ncvgt(fileid,problem_reaction_num_id,nc_corner,nc_edge,proble
     &m_reaction_num,nc_stat)
      problem_test_reaction_id=ncvid(fileid,'problem_test_reaction',nc_s
     &tat)
      call ncvinq(fileid,problem_test_reaction_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_reaction_ind0_id)continue
      if(nc_dims(2).EQ.problem_test_ind_id)continue
      
      problem_test_reaction =>mem_alloc_i2((1),(15),(1),(pr_test_num),'p
     &roblem_test_reaction')
      nc_corner(1)=1
      nc_edge(1)=((15)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_test_num)-(1)+1)
      call ncvgt(fileid,problem_test_reaction_id,nc_corner,nc_edge,probl
     &em_test_reaction,nc_stat)
      problem_test_background_id=ncvid(fileid,'problem_test_background',
     &nc_stat)
      call ncvinq(fileid,problem_test_background_id,nc_dummy,nc_type,nc_
     &rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_reaction_ind0_id)continue
      if(nc_dims(2).EQ.problem_test_ind_id)continue
      
      problem_test_background =>mem_alloc_i2((1),(15),(1),(pr_test_num),
     &'problem_test_background')
      nc_corner(1)=1
      nc_edge(1)=((15)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_test_num)-(1)+1)
      call ncvgt(fileid,problem_test_background_id,nc_corner,nc_edge,pro
     &blem_test_background,nc_stat)
      problem_num_arrangements_id=ncvid(fileid,'problem_num_arrangements
     &',nc_stat)
      call ncvinq(fileid,problem_num_arrangements_id,nc_dummy,nc_type,nc
     &_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_reaction_ind0_id)continue
      if(nc_dims(2).EQ.problem_test_ind_id)continue
      
      problem_num_arrangements =>mem_alloc_i2((1),(15),(1),(pr_test_num)
     &,'problem_num_arrangements')
      nc_corner(1)=1
      nc_edge(1)=((15)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_test_num)-(1)+1)
      call ncvgt(fileid,problem_num_arrangements_id,nc_corner,nc_edge,pr
     &oblem_num_arrangements,nc_stat)
      problem_test_products_id=ncvid(fileid,'problem_test_products',nc_s
     &tat)
      call ncvinq(fileid,problem_test_products_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_product_ind_id)continue
      if(nc_dims(2).EQ.problem_arr_ind_id)continue
      if(nc_dims(3).EQ.problem_reaction_ind0_id)continue
      if(nc_dims(4).EQ.problem_test_ind_id)continue
      
      problem_test_products =>mem_alloc_i4((1),(4),(1),(4),(1),(15),(1),
     &(pr_test_num),'problem_test_products')
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((15)-(1)+1)
      nc_corner(4)=1
      nc_edge(4)=((pr_test_num)-(1)+1)
      call ncvgt(fileid,problem_test_products_id,nc_corner,nc_edge,probl
     &em_test_products,nc_stat)
      problem_prod_mult_id=ncvid(fileid,'problem_prod_mult',nc_stat)
      call ncvinq(fileid,problem_prod_mult_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_arr_ind_id)continue
      if(nc_dims(2).EQ.problem_reaction_ind0_id)continue
      if(nc_dims(3).EQ.problem_test_ind_id)continue
      
      problem_prod_mult =>mem_alloc_r3((1),(4),(1),(15),(1),(pr_test_num
     &),'problem_prod_mult')
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((15)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_test_num)-(1)+1)
      call ncvgt(fileid,problem_prod_mult_id,nc_corner,nc_edge,problem_p
     &rod_mult,nc_stat)
      problem_background_reaction_id=ncvid(fileid,'problem_background_re
     &action',nc_stat)
      call ncvinq(fileid,problem_background_reaction_id,nc_dummy,nc_type
     &,nc_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_bkrc_ind_id)continue
      
      problem_background_reaction =>mem_alloc_i1((1),(pr_bkrc_dim),'prob
     &lem_background_reaction')
      nc_corner(1)=1
      nc_edge(1)=((pr_bkrc_dim)-(1)+1)
      call ncvgt(fileid,problem_background_reaction_id,nc_corner,nc_edge
     &,problem_background_reaction,nc_stat)
      problem_bkrc_reagents_id=ncvid(fileid,'problem_bkrc_reagents',nc_s
     &tat)
      call ncvinq(fileid,problem_bkrc_reagents_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_bkrc_rg_ind_id)continue
      if(nc_dims(2).EQ.problem_bkrc_ind_id)continue
      
      problem_bkrc_reagents =>mem_alloc_i2((1),(2),(1),(pr_bkrc_dim),'pr
     &oblem_bkrc_reagents')
      nc_corner(1)=1
      nc_edge(1)=((2)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_bkrc_dim)-(1)+1)
      call ncvgt(fileid,problem_bkrc_reagents_id,nc_corner,nc_edge,probl
     &em_bkrc_reagents,nc_stat)
      problem_bkrc_products_id=ncvid(fileid,'problem_bkrc_products',nc_s
     &tat)
      call ncvinq(fileid,problem_bkrc_products_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_product_ind_id)continue
      if(nc_dims(2).EQ.problem_bkrc_ind_id)continue
      
      problem_bkrc_products =>mem_alloc_i2((1),(4),(1),(pr_bkrc_dim),'pr
     &oblem_bkrc_products')
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_bkrc_dim)-(1)+1)
      call ncvgt(fileid,problem_bkrc_products_id,nc_corner,nc_edge,probl
     &em_bkrc_products,nc_stat)
      problem_external_reaction_id=ncvid(fileid,'problem_external_reacti
     &on',nc_stat)
      call ncvinq(fileid,problem_external_reaction_id,nc_dummy,nc_type,n
     &c_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_exrc_ind_id)continue
      
      problem_external_reaction =>mem_alloc_i1((1),(pr_exrc_dim),'proble
     &m_external_reaction')
      nc_corner(1)=1
      nc_edge(1)=((pr_exrc_dim)-(1)+1)
      call ncvgt(fileid,problem_external_reaction_id,nc_corner,nc_edge,p
     &roblem_external_reaction,nc_stat)
      problem_external_test_reaction_num_id=ncvid(fileid,'problem_extern
     &al_test_reaction_num',nc_stat)
      call ncvinq(fileid,problem_external_test_reaction_num_id,nc_dummy,
     &nc_type,nc_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_ex_test_ind_id)continue
      
      problem_external_test_reaction_num =>mem_alloc_i1((1),(pr_ex_test_
     &dim),'problem_external_test_reaction_num')
      nc_corner(1)=1
      nc_edge(1)=((pr_ex_test_dim)-(1)+1)
      call ncvgt(fileid,problem_external_test_reaction_num_id,nc_corner,
     &nc_edge,problem_external_test_reaction_num,nc_stat)
      problem_external_test_reaction_id=ncvid(fileid,'problem_external_t
     &est_reaction',nc_stat)
      call ncvinq(fileid,problem_external_test_reaction_id,nc_dummy,nc_t
     &ype,nc_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_reaction_ind0_id)continue
      if(nc_dims(2).EQ.problem_ex_test_ind_id)continue
      
      problem_external_test_reaction =>mem_alloc_i2((1),(15),(1),(pr_ex_
     &test_dim),'problem_external_test_reaction')
      nc_corner(1)=1
      nc_edge(1)=((15)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_ex_test_dim)-(1)+1)
      call ncvgt(fileid,problem_external_test_reaction_id,nc_corner,nc_e
     &dge,problem_external_test_reaction,nc_stat)
      problem_external_test_background_id=ncvid(fileid,'problem_external
     &_test_background',nc_stat)
      call ncvinq(fileid,problem_external_test_background_id,nc_dummy,nc
     &_type,nc_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_reaction_ind0_id)continue
      if(nc_dims(2).EQ.problem_ex_test_ind_id)continue
      
      problem_external_test_background =>mem_alloc_i2((1),(15),(1),(pr_e
     &x_test_dim),'problem_external_test_background')
      nc_corner(1)=1
      nc_edge(1)=((15)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_ex_test_dim)-(1)+1)
      call ncvgt(fileid,problem_external_test_background_id,nc_corner,nc
     &_edge,problem_external_test_background,nc_stat)
      problem_exrc_products_id=ncvid(fileid,'problem_exrc_products',nc_s
     &tat)
      call ncvinq(fileid,problem_exrc_products_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_product_ind_id)continue
      if(nc_dims(2).EQ.problem_reaction_ind0_id)continue
      if(nc_dims(3).EQ.problem_ex_test_ind_id)continue
      
      problem_exrc_products =>mem_alloc_i3((1),(4),(1),(15),(1),(pr_ex_t
     &est_dim),'problem_exrc_products')
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((15)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_ex_test_dim)-(1)+1)
      call ncvgt(fileid,problem_exrc_products_id,nc_corner,nc_edge,probl
     &em_exrc_products,nc_stat)
      problem_pmi_ref_id=ncvid(fileid,'problem_pmi_ref',nc_stat)
      call ncvinq(fileid,problem_pmi_ref_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_pmi_sub_ind_id)continue
      
      problem_pmi_ref =>mem_alloc_i1((1),(pr_pmi_num),'problem_pmi_ref')
      nc_corner(1)=1
      nc_edge(1)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,problem_pmi_ref_id,nc_corner,nc_edge,problem_pmi
     &_ref,nc_stat)
      problem_pmi_sub_id=ncvid(fileid,'problem_pmi_sub',nc_stat)
      call ncvinq(fileid,problem_pmi_sub_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_pmi_ref_ind_id)continue
      
      problem_pmi_sub =>mem_alloc_i1((1),(pm_num),'problem_pmi_sub')
      nc_corner(1)=1
      nc_edge(1)=((pm_num)-(1)+1)
      call ncvgt(fileid,problem_pmi_sub_id,nc_corner,nc_edge,problem_pmi
     &_sub,nc_stat)
      problem_pmi_case_num_id=ncvid(fileid,'problem_pmi_case_num',nc_sta
     &t)
      call ncvinq(fileid,problem_pmi_case_num_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_test_ind_id)continue
      
      problem_pmi_case_num =>mem_alloc_i1((1),(pr_test_num),'problem_pmi
     &_case_num')
      nc_corner(1)=1
      nc_edge(1)=((pr_test_num)-(1)+1)
      call ncvgt(fileid,problem_pmi_case_num_id,nc_corner,nc_edge,proble
     &m_pmi_case_num,nc_stat)
      problem_pmi_cases_id=ncvid(fileid,'problem_pmi_cases',nc_stat)
      call ncvinq(fileid,problem_pmi_cases_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_pmi_ind0_id)continue
      if(nc_dims(2).EQ.problem_test_ind_id)continue
      
      problem_pmi_cases =>mem_alloc_i2((1),(8),(1),(pr_test_num),'proble
     &m_pmi_cases')
      nc_corner(1)=1
      nc_edge(1)=((8)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_test_num)-(1)+1)
      call ncvgt(fileid,problem_pmi_cases_id,nc_corner,nc_edge,problem_p
     &mi_cases,nc_stat)
      problem_pmi_num_arrange_id=ncvid(fileid,'problem_pmi_num_arrange',
     &nc_stat)
      call ncvinq(fileid,problem_pmi_num_arrange_id,nc_dummy,nc_type,nc_
     &rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_pmi_ind0_id)continue
      if(nc_dims(2).EQ.problem_test_ind_id)continue
      
      problem_pmi_num_arrange =>mem_alloc_i2((1),(8),(1),(pr_test_num),'
     &problem_pmi_num_arrange')
      nc_corner(1)=1
      nc_edge(1)=((8)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_test_num)-(1)+1)
      call ncvgt(fileid,problem_pmi_num_arrange_id,nc_corner,nc_edge,pro
     &blem_pmi_num_arrange,nc_stat)
      problem_pmi_products_id=ncvid(fileid,'problem_pmi_products',nc_sta
     &t)
      call ncvinq(fileid,problem_pmi_products_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_product_ind_id)continue
      if(nc_dims(2).EQ.problem_arr_ind_id)continue
      if(nc_dims(3).EQ.problem_pmi_ind0_id)continue
      if(nc_dims(4).EQ.problem_test_ind_id)continue
      
      problem_pmi_products =>mem_alloc_i4((1),(4),(1),(4),(1),(8),(1),(p
     &r_test_num),'problem_pmi_products')
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((8)-(1)+1)
      nc_corner(4)=1
      nc_edge(4)=((pr_test_num)-(1)+1)
      call ncvgt(fileid,problem_pmi_products_id,nc_corner,nc_edge,proble
     &m_pmi_products,nc_stat)
      problem_pmi_prod_mult_id=ncvid(fileid,'problem_pmi_prod_mult',nc_s
     &tat)
      call ncvinq(fileid,problem_pmi_prod_mult_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_arr_ind_id)continue
      if(nc_dims(2).EQ.problem_pmi_ind0_id)continue
      if(nc_dims(3).EQ.problem_test_ind_id)continue
      
      problem_pmi_prod_mult =>mem_alloc_r3((1),(4),(1),(8),(1),(pr_test_
     &num),'problem_pmi_prod_mult')
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((8)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_test_num)-(1)+1)
      call ncvgt(fileid,problem_pmi_prod_mult_id,nc_corner,nc_edge,probl
     &em_pmi_prod_mult,nc_stat)
      pr_var0_list_id=ncvid(fileid,'pr_var0_list',nc_stat)
      call ncvinq(fileid,pr_var0_list_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pr_tag_string_id)continue
      if(nc_dims(2).EQ.pr_var0_list_ind_id)continue
      
      pr_var0_list =>mem_alloc_c1((40),(1),(pr_var0_num),'pr_var0_list')
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_var0_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,pr_var0_list_id,nc_corner,nc_edge,
     &pr_var0_list)
      
      rd_rank_ind_id=ncdid(fileid,'rd_rank_ind',nc_stat)
      call ncdinq(fileid,rd_rank_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((3)-(1)+1))continue
      rd_dep_var_ind_id=ncdid(fileid,'rd_dep_var_ind',nc_stat)
      call ncdinq(fileid,rd_dep_var_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((19)-(1)+1))continue
      rd_rank_ind0_id=ncdid(fileid,'rd_rank_ind0',nc_stat)
      call ncdinq(fileid,rd_rank_ind0_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((3)-(0)+1))continue
      rd_tag_string_id=ncdid(fileid,'rd_tag_string',nc_stat)
      call ncdinq(fileid,rd_tag_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((40)-(1)+1))continue
      rd_eval_string_id=ncdid(fileid,'rd_eval_string',nc_stat)
      call ncdinq(fileid,rd_eval_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((40)-(1)+1))continue
      reaction_rate_size_id=ncvid(fileid,'reaction_rate_size',nc_stat)
      call ncvinq(fileid,reaction_rate_size_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,reaction_rate_size_id,nc_corner,nc_edge,reaction
     &_rate_size,nc_stat)
      rd_table_ind_id=ncdid(fileid,'rd_table_ind',nc_stat)
      call ncdinq(fileid,rd_table_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((reaction_rate_size-1)-(0)+1))continue
      reaction_handling_size_id=ncvid(fileid,'reaction_handling_size',nc
     &_stat)
      call ncvinq(fileid,reaction_handling_size_id,nc_dummy,nc_type,nc_r
     &ank,nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,reaction_handling_size_id,nc_corner,nc_edge,reac
     &tion_handling_size,nc_stat)
      rd_handling_table_ind_id=ncdid(fileid,'rd_handling_table_ind',nc_s
     &tat)
      call ncdinq(fileid,rd_handling_table_ind_id,nc_dummy,nc_size,nc_st
     &at)
      if(nc_size.EQ.((reaction_handling_size-1)-(0)+1))continue
      reaction_rate_min_id=ncvid(fileid,'reaction_rate_min',nc_stat)
      call ncvinq(fileid,reaction_rate_min_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_rank_ind_id)continue
      if(nc_dims(2).EQ.problem_reaction_ind_id)continue
      
      reaction_rate_min =>mem_alloc_r2((1),(3),(1),(pr_reaction_dim),'re
     &action_rate_min')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_rate_min_id,nc_corner,nc_edge,reaction_
     &rate_min,nc_stat)
      reaction_rate_delta_id=ncvid(fileid,'reaction_rate_delta',nc_stat)
      call ncvinq(fileid,reaction_rate_delta_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_rank_ind_id)continue
      if(nc_dims(2).EQ.problem_reaction_ind_id)continue
      
      reaction_rate_delta =>mem_alloc_r2((1),(3),(1),(pr_reaction_dim),'
     &reaction_rate_delta')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_rate_delta_id,nc_corner,nc_edge,reactio
     &n_rate_delta,nc_stat)
      reaction_rate_rank_id=ncvid(fileid,'reaction_rate_rank',nc_stat)
      call ncvinq(fileid,reaction_rate_rank_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_reaction_ind_id)continue
      
      reaction_rate_rank =>mem_alloc_i1((1),(pr_reaction_dim),'reaction_
     &rate_rank')
      nc_corner(1)=1
      nc_edge(1)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_rate_rank_id,nc_corner,nc_edge,reaction
     &_rate_rank,nc_stat)
      reaction_rate_spacing_id=ncvid(fileid,'reaction_rate_spacing',nc_s
     &tat)
      call ncvinq(fileid,reaction_rate_spacing_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_rank_ind0_id)continue
      if(nc_dims(2).EQ.problem_reaction_ind_id)continue
      
      reaction_rate_spacing =>mem_alloc_i2((0),(3),(1),(pr_reaction_dim)
     &,'reaction_rate_spacing')
      nc_corner(1)=1
      nc_edge(1)=((3)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_rate_spacing_id,nc_corner,nc_edge,react
     &ion_rate_spacing,nc_stat)
      reaction_rate_eval_name_id=ncvid(fileid,'reaction_rate_eval_name',
     &nc_stat)
      call ncvinq(fileid,reaction_rate_eval_name_id,nc_dummy,nc_type,nc_
     &rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_eval_string_id)continue
      if(nc_dims(2).EQ.problem_reaction_ind_id)continue
      
      reaction_rate_eval_name =>mem_alloc_c1((40),(1),(pr_reaction_dim),
     &'reaction_rate_eval_name')
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,reaction_rate_eval_name_id,nc_corn
     &er,nc_edge,reaction_rate_eval_name)
      reaction_rate_tab_index_id=ncvid(fileid,'reaction_rate_tab_index',
     &nc_stat)
      call ncvinq(fileid,reaction_rate_tab_index_id,nc_dummy,nc_type,nc_
     &rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_rank_ind_id)continue
      if(nc_dims(2).EQ.problem_reaction_ind_id)continue
      
      reaction_rate_tab_index =>mem_alloc_i2((1),(3),(1),(pr_reaction_di
     &m),'reaction_rate_tab_index')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_rate_tab_index_id,nc_corner,nc_edge,rea
     &ction_rate_tab_index,nc_stat)
      reaction_rate_var_id=ncvid(fileid,'reaction_rate_var',nc_stat)
      call ncvinq(fileid,reaction_rate_var_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_rank_ind_id)continue
      if(nc_dims(2).EQ.problem_reaction_ind_id)continue
      
      reaction_rate_var =>mem_alloc_i2((1),(3),(1),(pr_reaction_dim),'re
     &action_rate_var')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_rate_var_id,nc_corner,nc_edge,reaction_
     &rate_var,nc_stat)
      reaction_rate_num_rand_id=ncvid(fileid,'reaction_rate_num_rand',nc
     &_stat)
      call ncvinq(fileid,reaction_rate_num_rand_id,nc_dummy,nc_type,nc_r
     &ank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_reaction_ind_id)continue
      
      reaction_rate_num_rand =>mem_alloc_i1((1),(pr_reaction_dim),'react
     &ion_rate_num_rand')
      nc_corner(1)=1
      nc_edge(1)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_rate_num_rand_id,nc_corner,nc_edge,reac
     &tion_rate_num_rand,nc_stat)
      reaction_rate_base_id=ncvid(fileid,'reaction_rate_base',nc_stat)
      call ncvinq(fileid,reaction_rate_base_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_reaction_ind_id)continue
      
      reaction_rate_base =>mem_alloc_i1((1),(pr_reaction_dim),'reaction_
     &rate_base')
      nc_corner(1)=1
      nc_edge(1)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_rate_base_id,nc_corner,nc_edge,reaction
     &_rate_base,nc_stat)
      reaction_rate_tab_id=ncvid(fileid,'reaction_rate_tab',nc_stat)
      call ncvinq(fileid,reaction_rate_tab_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_table_ind_id)continue
      
      reaction_rate_tab =>mem_alloc_r1((0),(reaction_rate_size-1),'react
     &ion_rate_tab')
      nc_corner(1)=1
      nc_edge(1)=((reaction_rate_size-1)-(0)+1)
      call ncvgt(fileid,reaction_rate_tab_id,nc_corner,nc_edge,reaction_
     &rate_tab,nc_stat)
      reaction_handling_min_id=ncvid(fileid,'reaction_handling_min',nc_s
     &tat)
      call ncvinq(fileid,reaction_handling_min_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_rank_ind_id)continue
      if(nc_dims(2).EQ.rd_dep_var_ind_id)continue
      if(nc_dims(3).EQ.problem_reaction_ind_id)continue
      
      reaction_handling_min =>mem_alloc_r3((1),(3),(1),(19),(1),(pr_reac
     &tion_dim),'reaction_handling_min')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((19)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_handling_min_id,nc_corner,nc_edge,react
     &ion_handling_min,nc_stat)
      reaction_handling_delta_id=ncvid(fileid,'reaction_handling_delta',
     &nc_stat)
      call ncvinq(fileid,reaction_handling_delta_id,nc_dummy,nc_type,nc_
     &rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_rank_ind_id)continue
      if(nc_dims(2).EQ.rd_dep_var_ind_id)continue
      if(nc_dims(3).EQ.problem_reaction_ind_id)continue
      
      reaction_handling_delta =>mem_alloc_r3((1),(3),(1),(19),(1),(pr_re
     &action_dim),'reaction_handling_delta')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((19)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_handling_delta_id,nc_corner,nc_edge,rea
     &ction_handling_delta,nc_stat)
      reaction_handling_rank_id=ncvid(fileid,'reaction_handling_rank',nc
     &_stat)
      call ncvinq(fileid,reaction_handling_rank_id,nc_dummy,nc_type,nc_r
     &ank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_dep_var_ind_id)continue
      if(nc_dims(2).EQ.problem_reaction_ind_id)continue
      
      reaction_handling_rank =>mem_alloc_i2((1),(19),(1),(pr_reaction_di
     &m),'reaction_handling_rank')
      nc_corner(1)=1
      nc_edge(1)=((19)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_handling_rank_id,nc_corner,nc_edge,reac
     &tion_handling_rank,nc_stat)
      reaction_handling_spacing_id=ncvid(fileid,'reaction_handling_spaci
     &ng',nc_stat)
      call ncvinq(fileid,reaction_handling_spacing_id,nc_dummy,nc_type,n
     &c_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_rank_ind0_id)continue
      if(nc_dims(2).EQ.rd_dep_var_ind_id)continue
      if(nc_dims(3).EQ.problem_reaction_ind_id)continue
      
      reaction_handling_spacing =>mem_alloc_i3((0),(3),(1),(19),(1),(pr_
     &reaction_dim),'reaction_handling_spacing')
      nc_corner(1)=1
      nc_edge(1)=((3)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((19)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_handling_spacing_id,nc_corner,nc_edge,r
     &eaction_handling_spacing,nc_stat)
      reaction_handling_eval_name_id=ncvid(fileid,'reaction_handling_eva
     &l_name',nc_stat)
      call ncvinq(fileid,reaction_handling_eval_name_id,nc_dummy,nc_type
     &,nc_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_eval_string_id)continue
      if(nc_dims(2).EQ.rd_dep_var_ind_id)continue
      if(nc_dims(3).EQ.problem_reaction_ind_id)continue
      
      reaction_handling_eval_name =>mem_alloc_c2((40),(1),(19),(1),(pr_r
     &eaction_dim),'reaction_handling_eval_name')
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((19)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_reaction_dim)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,reaction_handling_eval_name_id,nc_
     &corner,nc_edge,reaction_handling_eval_name)
      reaction_handling_tab_index_id=ncvid(fileid,'reaction_handling_tab
     &_index',nc_stat)
      call ncvinq(fileid,reaction_handling_tab_index_id,nc_dummy,nc_type
     &,nc_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_rank_ind_id)continue
      if(nc_dims(2).EQ.rd_dep_var_ind_id)continue
      if(nc_dims(3).EQ.problem_reaction_ind_id)continue
      
      reaction_handling_tab_index =>mem_alloc_i3((1),(3),(1),(19),(1),(p
     &r_reaction_dim),'reaction_handling_tab_index')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((19)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_handling_tab_index_id,nc_corner,nc_edge
     &,reaction_handling_tab_index,nc_stat)
      reaction_handling_var0_id=ncvid(fileid,'reaction_handling_var0',nc
     &_stat)
      call ncvinq(fileid,reaction_handling_var0_id,nc_dummy,nc_type,nc_r
     &ank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_dep_var_ind_id)continue
      if(nc_dims(2).EQ.problem_reaction_ind_id)continue
      
      reaction_handling_var0 =>mem_alloc_i2((1),(19),(1),(pr_reaction_di
     &m),'reaction_handling_var0')
      nc_corner(1)=1
      nc_edge(1)=((19)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_handling_var0_id,nc_corner,nc_edge,reac
     &tion_handling_var0,nc_stat)
      reaction_handling_var_id=ncvid(fileid,'reaction_handling_var',nc_s
     &tat)
      call ncvinq(fileid,reaction_handling_var_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_rank_ind_id)continue
      if(nc_dims(2).EQ.rd_dep_var_ind_id)continue
      if(nc_dims(3).EQ.problem_reaction_ind_id)continue
      
      reaction_handling_var =>mem_alloc_i3((1),(3),(1),(19),(1),(pr_reac
     &tion_dim),'reaction_handling_var')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((19)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_handling_var_id,nc_corner,nc_edge,react
     &ion_handling_var,nc_stat)
      reaction_handling_num_rand_id=ncvid(fileid,'reaction_handling_num_
     &rand',nc_stat)
      call ncvinq(fileid,reaction_handling_num_rand_id,nc_dummy,nc_type,
     &nc_rank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_reaction_ind_id)continue
      
      reaction_handling_num_rand =>mem_alloc_i1((1),(pr_reaction_dim),'r
     &eaction_handling_num_rand')
      nc_corner(1)=1
      nc_edge(1)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_handling_num_rand_id,nc_corner,nc_edge,
     &reaction_handling_num_rand,nc_stat)
      reaction_handling_base_id=ncvid(fileid,'reaction_handling_base',nc
     &_stat)
      call ncvinq(fileid,reaction_handling_base_id,nc_dummy,nc_type,nc_r
     &ank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_dep_var_ind_id)continue
      if(nc_dims(2).EQ.problem_reaction_ind_id)continue
      
      reaction_handling_base =>mem_alloc_i2((1),(19),(1),(pr_reaction_di
     &m),'reaction_handling_base')
      nc_corner(1)=1
      nc_edge(1)=((19)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvgt(fileid,reaction_handling_base_id,nc_corner,nc_edge,reac
     &tion_handling_base,nc_stat)
      reaction_handling_tab_id=ncvid(fileid,'reaction_handling_tab',nc_s
     &tat)
      call ncvinq(fileid,reaction_handling_tab_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.rd_handling_table_ind_id)continue
      
      reaction_handling_tab =>mem_alloc_r1((0),(reaction_handling_size-1
     &),'reaction_handling_tab')
      nc_corner(1)=1
      nc_edge(1)=((reaction_handling_size-1)-(0)+1)
      call ncvgt(fileid,reaction_handling_tab_id,nc_corner,nc_edge,react
     &ion_handling_tab,nc_stat)
      
      pd_rank_ind_id=ncdid(fileid,'pd_rank_ind',nc_stat)
      call ncdinq(fileid,pd_rank_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((5)-(1)+1))continue
      pd_dep_var_ind_id=ncdid(fileid,'pd_dep_var_ind',nc_stat)
      call ncdinq(fileid,pd_dep_var_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((4)-(1)+1))continue
      pd_rank_ind0_id=ncdid(fileid,'pd_rank_ind0',nc_stat)
      call ncdinq(fileid,pd_rank_ind0_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((5)-(0)+1))continue
      pd_tag_string_id=ncdid(fileid,'pd_tag_string',nc_stat)
      call ncdinq(fileid,pd_tag_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((20)-(1)+1))continue
      pd_eval_string_id=ncdid(fileid,'pd_eval_string',nc_stat)
      call ncdinq(fileid,pd_eval_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((40)-(1)+1))continue
      pmi_yield_size_id=ncvid(fileid,'pmi_yield_size',nc_stat)
      call ncvinq(fileid,pmi_yield_size_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      
      call ncvgt(fileid,pmi_yield_size_id,nc_corner,nc_edge,pmi_yield_si
     &ze,nc_stat)
      pd_yield_ind_id=ncdid(fileid,'pd_yield_ind',nc_stat)
      call ncdinq(fileid,pd_yield_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((pmi_yield_size-1)-(0)+1))continue
      pmi_handling_size_id=ncvid(fileid,'pmi_handling_size',nc_stat)
      call ncvinq(fileid,pmi_handling_size_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,pmi_handling_size_id,nc_corner,nc_edge,pmi_handl
     &ing_size,nc_stat)
      pd_handling_ind_id=ncdid(fileid,'pd_handling_ind',nc_stat)
      call ncdinq(fileid,pd_handling_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((pmi_handling_size-1)-(0)+1))continue
      pmi_yield_min_id=ncvid(fileid,'pmi_yield_min',nc_stat)
      call ncvinq(fileid,pmi_yield_min_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_rank_ind_id)continue
      if(nc_dims(2).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_yield_min =>mem_alloc_r2((1),(5),(1),(pr_pmi_num),'pmi_yield_m
     &in')
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,pmi_yield_min_id,nc_corner,nc_edge,pmi_yield_min
     &,nc_stat)
      pmi_yield_delta_id=ncvid(fileid,'pmi_yield_delta',nc_stat)
      call ncvinq(fileid,pmi_yield_delta_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_rank_ind_id)continue
      if(nc_dims(2).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_yield_delta =>mem_alloc_r2((1),(5),(1),(pr_pmi_num),'pmi_yield
     &_delta')
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,pmi_yield_delta_id,nc_corner,nc_edge,pmi_yield_d
     &elta,nc_stat)
      pmi_yield_rank_id=ncvid(fileid,'pmi_yield_rank',nc_stat)
      call ncvinq(fileid,pmi_yield_rank_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_yield_rank =>mem_alloc_i1((1),(pr_pmi_num),'pmi_yield_rank')
      nc_corner(1)=1
      nc_edge(1)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,pmi_yield_rank_id,nc_corner,nc_edge,pmi_yield_ra
     &nk,nc_stat)
      pmi_yield_spacing_id=ncvid(fileid,'pmi_yield_spacing',nc_stat)
      call ncvinq(fileid,pmi_yield_spacing_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_rank_ind0_id)continue
      if(nc_dims(2).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_yield_spacing =>mem_alloc_i2((0),(5),(1),(pr_pmi_num),'pmi_yie
     &ld_spacing')
      nc_corner(1)=1
      nc_edge(1)=((5)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,pmi_yield_spacing_id,nc_corner,nc_edge,pmi_yield
     &_spacing,nc_stat)
      pmi_yield_eval_name_id=ncvid(fileid,'pmi_yield_eval_name',nc_stat)
      call ncvinq(fileid,pmi_yield_eval_name_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_eval_string_id)continue
      if(nc_dims(2).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_yield_eval_name =>mem_alloc_c1((40),(1),(pr_pmi_num),'pmi_yiel
     &d_eval_name')
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_pmi_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,pmi_yield_eval_name_id,nc_corner,n
     &c_edge,pmi_yield_eval_name)
      pmi_yield_tab_index_id=ncvid(fileid,'pmi_yield_tab_index',nc_stat)
      call ncvinq(fileid,pmi_yield_tab_index_id,nc_dummy,nc_type,nc_rank
     &,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_rank_ind_id)continue
      if(nc_dims(2).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_yield_tab_index =>mem_alloc_i2((1),(5),(1),(pr_pmi_num),'pmi_y
     &ield_tab_index')
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,pmi_yield_tab_index_id,nc_corner,nc_edge,pmi_yie
     &ld_tab_index,nc_stat)
      pmi_yield_var_id=ncvid(fileid,'pmi_yield_var',nc_stat)
      call ncvinq(fileid,pmi_yield_var_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_rank_ind_id)continue
      if(nc_dims(2).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_yield_var =>mem_alloc_i2((1),(5),(1),(pr_pmi_num),'pmi_yield_v
     &ar')
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,pmi_yield_var_id,nc_corner,nc_edge,pmi_yield_var
     &,nc_stat)
      pmi_yield_num_rand_id=ncvid(fileid,'pmi_yield_num_rand',nc_stat)
      call ncvinq(fileid,pmi_yield_num_rand_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_yield_num_rand =>mem_alloc_i1((1),(pr_pmi_num),'pmi_yield_num_
     &rand')
      nc_corner(1)=1
      nc_edge(1)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,pmi_yield_num_rand_id,nc_corner,nc_edge,pmi_yiel
     &d_num_rand,nc_stat)
      pmi_yield_base_id=ncvid(fileid,'pmi_yield_base',nc_stat)
      call ncvinq(fileid,pmi_yield_base_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_yield_base =>mem_alloc_i1((1),(pr_pmi_num),'pmi_yield_base')
      nc_corner(1)=1
      nc_edge(1)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,pmi_yield_base_id,nc_corner,nc_edge,pmi_yield_ba
     &se,nc_stat)
      pmi_yield_tab_id=ncvid(fileid,'pmi_yield_tab',nc_stat)
      call ncvinq(fileid,pmi_yield_tab_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_yield_ind_id)continue
      
      pmi_yield_tab =>mem_alloc_r1((0),(pmi_yield_size-1),'pmi_yield_tab
     &')
      nc_corner(1)=1
      nc_edge(1)=((pmi_yield_size-1)-(0)+1)
      call ncvgt(fileid,pmi_yield_tab_id,nc_corner,nc_edge,pmi_yield_tab
     &,nc_stat)
      pmi_handling_min_id=ncvid(fileid,'pmi_handling_min',nc_stat)
      call ncvinq(fileid,pmi_handling_min_id,nc_dummy,nc_type,nc_rank,nc
     &_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_rank_ind_id)continue
      if(nc_dims(2).EQ.pd_dep_var_ind_id)continue
      if(nc_dims(3).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_handling_min =>mem_alloc_r3((1),(5),(1),(4),(1),(pr_pmi_num),'
     &pmi_handling_min')
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,pmi_handling_min_id,nc_corner,nc_edge,pmi_handli
     &ng_min,nc_stat)
      pmi_handling_delta_id=ncvid(fileid,'pmi_handling_delta',nc_stat)
      call ncvinq(fileid,pmi_handling_delta_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_rank_ind_id)continue
      if(nc_dims(2).EQ.pd_dep_var_ind_id)continue
      if(nc_dims(3).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_handling_delta =>mem_alloc_r3((1),(5),(1),(4),(1),(pr_pmi_num)
     &,'pmi_handling_delta')
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,pmi_handling_delta_id,nc_corner,nc_edge,pmi_hand
     &ling_delta,nc_stat)
      pmi_handling_rank_id=ncvid(fileid,'pmi_handling_rank',nc_stat)
      call ncvinq(fileid,pmi_handling_rank_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_dep_var_ind_id)continue
      if(nc_dims(2).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_handling_rank =>mem_alloc_i2((1),(4),(1),(pr_pmi_num),'pmi_han
     &dling_rank')
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,pmi_handling_rank_id,nc_corner,nc_edge,pmi_handl
     &ing_rank,nc_stat)
      pmi_handling_spacing_id=ncvid(fileid,'pmi_handling_spacing',nc_sta
     &t)
      call ncvinq(fileid,pmi_handling_spacing_id,nc_dummy,nc_type,nc_ran
     &k,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_rank_ind0_id)continue
      if(nc_dims(2).EQ.pd_dep_var_ind_id)continue
      if(nc_dims(3).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_handling_spacing =>mem_alloc_i3((0),(5),(1),(4),(1),(pr_pmi_nu
     &m),'pmi_handling_spacing')
      nc_corner(1)=1
      nc_edge(1)=((5)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,pmi_handling_spacing_id,nc_corner,nc_edge,pmi_ha
     &ndling_spacing,nc_stat)
      pmi_handling_eval_name_id=ncvid(fileid,'pmi_handling_eval_name',nc
     &_stat)
      call ncvinq(fileid,pmi_handling_eval_name_id,nc_dummy,nc_type,nc_r
     &ank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_eval_string_id)continue
      if(nc_dims(2).EQ.pd_dep_var_ind_id)continue
      if(nc_dims(3).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_handling_eval_name =>mem_alloc_c2((40),(1),(4),(1),(pr_pmi_num
     &),'pmi_handling_eval_name')
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_pmi_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,pmi_handling_eval_name_id,nc_corne
     &r,nc_edge,pmi_handling_eval_name)
      pmi_handling_tab_index_id=ncvid(fileid,'pmi_handling_tab_index',nc
     &_stat)
      call ncvinq(fileid,pmi_handling_tab_index_id,nc_dummy,nc_type,nc_r
     &ank,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_rank_ind_id)continue
      if(nc_dims(2).EQ.pd_dep_var_ind_id)continue
      if(nc_dims(3).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_handling_tab_index =>mem_alloc_i3((1),(5),(1),(4),(1),(pr_pmi_
     &num),'pmi_handling_tab_index')
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,pmi_handling_tab_index_id,nc_corner,nc_edge,pmi_
     &handling_tab_index,nc_stat)
      pmi_handling_var0_id=ncvid(fileid,'pmi_handling_var0',nc_stat)
      call ncvinq(fileid,pmi_handling_var0_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_tag_string_id)continue
      if(nc_dims(2).EQ.pd_dep_var_ind_id)continue
      if(nc_dims(3).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_handling_var0 =>mem_alloc_c2((20),(1),(4),(1),(pr_pmi_num),'pm
     &i_handling_var0')
      nc_corner(1)=1
      nc_edge(1)=((20)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_pmi_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,pmi_handling_var0_id,nc_corner,nc_
     &edge,pmi_handling_var0)
      pmi_handling_var_id=ncvid(fileid,'pmi_handling_var',nc_stat)
      call ncvinq(fileid,pmi_handling_var_id,nc_dummy,nc_type,nc_rank,nc
     &_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_rank_ind_id)continue
      if(nc_dims(2).EQ.pd_dep_var_ind_id)continue
      if(nc_dims(3).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_handling_var =>mem_alloc_i3((1),(5),(1),(4),(1),(pr_pmi_num),'
     &pmi_handling_var')
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,pmi_handling_var_id,nc_corner,nc_edge,pmi_handli
     &ng_var,nc_stat)
      pmi_handling_num_rand_id=ncvid(fileid,'pmi_handling_num_rand',nc_s
     &tat)
      call ncvinq(fileid,pmi_handling_num_rand_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_handling_num_rand =>mem_alloc_i1((1),(pr_pmi_num),'pmi_handlin
     &g_num_rand')
      nc_corner(1)=1
      nc_edge(1)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,pmi_handling_num_rand_id,nc_corner,nc_edge,pmi_h
     &andling_num_rand,nc_stat)
      pmi_handling_base_id=ncvid(fileid,'pmi_handling_base',nc_stat)
      call ncvinq(fileid,pmi_handling_base_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_dep_var_ind_id)continue
      if(nc_dims(2).EQ.problem_pmi_sub_ind_id)continue
      
      pmi_handling_base =>mem_alloc_i2((1),(4),(1),(pr_pmi_num),'pmi_han
     &dling_base')
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_pmi_num)-(1)+1)
      call ncvgt(fileid,pmi_handling_base_id,nc_corner,nc_edge,pmi_handl
     &ing_base,nc_stat)
      pmi_handling_tab_id=ncvid(fileid,'pmi_handling_tab',nc_stat)
      call ncvinq(fileid,pmi_handling_tab_id,nc_dummy,nc_type,nc_rank,nc
     &_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.pd_handling_ind_id)continue
      
      pmi_handling_tab =>mem_alloc_r1((0),(pmi_handling_size-1),'pmi_han
     &dling_tab')
      nc_corner(1)=1
      nc_edge(1)=((pmi_handling_size-1)-(0)+1)
      call ncvgt(fileid,pmi_handling_tab_id,nc_corner,nc_edge,pmi_handli
     &ng_tab,nc_stat)
      
      call ncclos(fileid,nc_stat)
      end if
      call MPI_bcast(pr_test_num,1,MPI_INTEGER,mpi_degas2_root,comm_worl
     &d_dup,mpi_err)
      call MPI_bcast(pr_background_num,1,MPI_INTEGER,mpi_degas2_root,com
     &m_world_dup,mpi_err)
      call MPI_bcast(pr_ex_test_num,1,MPI_INTEGER,mpi_degas2_root,comm_w
     &orld_dup,mpi_err)
      call MPI_bcast(pr_ex_test_dim,1,MPI_INTEGER,mpi_degas2_root,comm_w
     &orld_dup,mpi_err)
      call MPI_bcast(pr_reaction_num,1,MPI_INTEGER,mpi_degas2_root,comm_
     &world_dup,mpi_err)
      call MPI_bcast(pr_reaction_dim,1,MPI_INTEGER,mpi_degas2_root,comm_
     &world_dup,mpi_err)
      call MPI_bcast(pr_bkrc_num,1,MPI_INTEGER,mpi_degas2_root,comm_worl
     &d_dup,mpi_err)
      call MPI_bcast(pr_bkrc_dim,1,MPI_INTEGER,mpi_degas2_root,comm_worl
     &d_dup,mpi_err)
      call MPI_bcast(pr_exrc_num,1,MPI_INTEGER,mpi_degas2_root,comm_worl
     &d_dup,mpi_err)
      call MPI_bcast(pr_exrc_dim,1,MPI_INTEGER,mpi_degas2_root,comm_worl
     &d_dup,mpi_err)
      call MPI_bcast(pr_materials_num,1,MPI_INTEGER,mpi_degas2_root,comm
     &_world_dup,mpi_err)
      call MPI_bcast(pr_pmi_num,1,MPI_INTEGER,mpi_degas2_root,comm_world
     &_dup,mpi_err)
      call MPI_bcast(pr_var0_num,1,MPI_INTEGER,mpi_degas2_root,comm_worl
     &d_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_species_test =>mem_alloc_i1((1),(sp_num),'problem_species_
     &test')
      endif
      call MPI_bcast(problem_species_test,(((sp_num)-(1)+1)),MPI_INTEGER
     &,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_species_background =>mem_alloc_i1((1),(sp_num),'problem_sp
     &ecies_background')
      endif
      call MPI_bcast(problem_species_background,(((sp_num)-(1)+1)),MPI_I
     &NTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_materials_sub =>mem_alloc_i1((1),(ma_num),'problem_materia
     &ls_sub')
      endif
      call MPI_bcast(problem_materials_sub,(((ma_num)-(1)+1)),MPI_INTEGE
     &R,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_test_sp =>mem_alloc_i1((1),(pr_test_num),'problem_test_sp'
     &)
      endif
      call MPI_bcast(problem_test_sp,(((pr_test_num)-(1)+1)),MPI_INTEGER
     &,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_background_sp =>mem_alloc_i1((1),(pr_background_num),'prob
     &lem_background_sp')
      endif
      call MPI_bcast(problem_background_sp,(((pr_background_num)-(1)+1))
     &,MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_ex_test_sp =>mem_alloc_i1((1),(pr_ex_test_dim),'problem_ex
     &_test_sp')
      endif
      call MPI_bcast(problem_ex_test_sp,(((pr_ex_test_dim)-(1)+1)),MPI_I
     &NTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_materials_ref =>mem_alloc_i1((1),(pr_materials_num),'probl
     &em_materials_ref')
      endif
      call MPI_bcast(problem_materials_ref,(((pr_materials_num)-(1)+1)),
     &MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_rc =>mem_alloc_i1((1),(pr_reaction_dim),'problem_rc')
      endif
      call MPI_bcast(problem_rc,(((pr_reaction_dim)-(1)+1)),MPI_INTEGER,
     &mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_reaction_num =>mem_alloc_i1((1),(pr_test_num),'problem_rea
     &ction_num')
      endif
      call MPI_bcast(problem_reaction_num,(((pr_test_num)-(1)+1)),MPI_IN
     &TEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_test_reaction =>mem_alloc_i2((1),(15),(1),(pr_test_num),'p
     &roblem_test_reaction')
      endif
      call MPI_bcast(problem_test_reaction,(((15)-(1)+1)*((pr_test_num)-
     &(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_test_background =>mem_alloc_i2((1),(15),(1),(pr_test_num),
     &'problem_test_background')
      endif
      call MPI_bcast(problem_test_background,(((15)-(1)+1)*((pr_test_num
     &)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_num_arrangements =>mem_alloc_i2((1),(15),(1),(pr_test_num)
     &,'problem_num_arrangements')
      endif
      call MPI_bcast(problem_num_arrangements,(((15)-(1)+1)*((pr_test_nu
     &m)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_test_products =>mem_alloc_i4((1),(4),(1),(4),(1),(15),(1),
     &(pr_test_num),'problem_test_products')
      endif
      call MPI_bcast(problem_test_products,(((4)-(1)+1)*((4)-(1)+1)*((15
     &)-(1)+1)*((pr_test_num)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_w
     &orld_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_prod_mult =>mem_alloc_r3((1),(4),(1),(15),(1),(pr_test_num
     &),'problem_prod_mult')
      endif
      call MPI_bcast(problem_prod_mult,(((4)-(1)+1)*((15)-(1)+1)*((pr_te
     &st_num)-(1)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_du
     &p,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_background_reaction =>mem_alloc_i1((1),(pr_bkrc_dim),'prob
     &lem_background_reaction')
      endif
      call MPI_bcast(problem_background_reaction,(((pr_bkrc_dim)-(1)+1))
     &,MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_bkrc_reagents =>mem_alloc_i2((1),(2),(1),(pr_bkrc_dim),'pr
     &oblem_bkrc_reagents')
      endif
      call MPI_bcast(problem_bkrc_reagents,(((2)-(1)+1)*((pr_bkrc_dim)-(
     &1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_bkrc_products =>mem_alloc_i2((1),(4),(1),(pr_bkrc_dim),'pr
     &oblem_bkrc_products')
      endif
      call MPI_bcast(problem_bkrc_products,(((4)-(1)+1)*((pr_bkrc_dim)-(
     &1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_external_reaction =>mem_alloc_i1((1),(pr_exrc_dim),'proble
     &m_external_reaction')
      endif
      call MPI_bcast(problem_external_reaction,(((pr_exrc_dim)-(1)+1)),M
     &PI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_external_test_reaction_num =>mem_alloc_i1((1),(pr_ex_test_
     &dim),'problem_external_test_reaction_num')
      endif
      call MPI_bcast(problem_external_test_reaction_num,(((pr_ex_test_di
     &m)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_external_test_reaction =>mem_alloc_i2((1),(15),(1),(pr_ex_
     &test_dim),'problem_external_test_reaction')
      endif
      call MPI_bcast(problem_external_test_reaction,(((15)-(1)+1)*((pr_e
     &x_test_dim)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi
     &_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_external_test_background =>mem_alloc_i2((1),(15),(1),(pr_e
     &x_test_dim),'problem_external_test_background')
      endif
      call MPI_bcast(problem_external_test_background,(((15)-(1)+1)*((pr
     &_ex_test_dim)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,m
     &pi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_exrc_products =>mem_alloc_i3((1),(4),(1),(15),(1),(pr_ex_t
     &est_dim),'problem_exrc_products')
      endif
      call MPI_bcast(problem_exrc_products,(((4)-(1)+1)*((15)-(1)+1)*((p
     &r_ex_test_dim)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,
     &mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_pmi_ref =>mem_alloc_i1((1),(pr_pmi_num),'problem_pmi_ref')
      endif
      call MPI_bcast(problem_pmi_ref,(((pr_pmi_num)-(1)+1)),MPI_INTEGER,
     &mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_pmi_sub =>mem_alloc_i1((1),(pm_num),'problem_pmi_sub')
      endif
      call MPI_bcast(problem_pmi_sub,(((pm_num)-(1)+1)),MPI_INTEGER,mpi_
     &degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_pmi_case_num =>mem_alloc_i1((1),(pr_test_num),'problem_pmi
     &_case_num')
      endif
      call MPI_bcast(problem_pmi_case_num,(((pr_test_num)-(1)+1)),MPI_IN
     &TEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_pmi_cases =>mem_alloc_i2((1),(8),(1),(pr_test_num),'proble
     &m_pmi_cases')
      endif
      call MPI_bcast(problem_pmi_cases,(((8)-(1)+1)*((pr_test_num)-(1)+1
     &)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_pmi_num_arrange =>mem_alloc_i2((1),(8),(1),(pr_test_num),'
     &problem_pmi_num_arrange')
      endif
      call MPI_bcast(problem_pmi_num_arrange,(((8)-(1)+1)*((pr_test_num)
     &-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_pmi_products =>mem_alloc_i4((1),(4),(1),(4),(1),(8),(1),(p
     &r_test_num),'problem_pmi_products')
      endif
      call MPI_bcast(problem_pmi_products,(((4)-(1)+1)*((4)-(1)+1)*((8)-
     &(1)+1)*((pr_test_num)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_wor
     &ld_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      problem_pmi_prod_mult =>mem_alloc_r3((1),(4),(1),(8),(1),(pr_test_
     &num),'problem_pmi_prod_mult')
      endif
      call MPI_bcast(problem_pmi_prod_mult,(((4)-(1)+1)*((8)-(1)+1)*((pr
     &_test_num)-(1)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world
     &_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pr_var0_list =>mem_alloc_c1((40),(1),(pr_var0_num),'pr_var0_list')
      endif
      call MPI_bcast(pr_var0_list,(((40)-(1)+1)*((pr_var0_num)-(1)+1)),M
     &PI_CHARACTER,mpi_degas2_root,comm_world_dup,mpi_err)
      
      call MPI_bcast(reaction_rate_size,1,MPI_INTEGER,mpi_degas2_root,co
     &mm_world_dup,mpi_err)
      call MPI_bcast(reaction_handling_size,1,MPI_INTEGER,mpi_degas2_roo
     &t,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_rate_min =>mem_alloc_r2((1),(3),(1),(pr_reaction_dim),'re
     &action_rate_min')
      endif
      call MPI_bcast(reaction_rate_min,(((3)-(1)+1)*((pr_reaction_dim)-(
     &1)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err
     &)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_rate_delta =>mem_alloc_r2((1),(3),(1),(pr_reaction_dim),'
     &reaction_rate_delta')
      endif
      call MPI_bcast(reaction_rate_delta,(((3)-(1)+1)*((pr_reaction_dim)
     &-(1)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_e
     &rr)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_rate_rank =>mem_alloc_i1((1),(pr_reaction_dim),'reaction_
     &rate_rank')
      endif
      call MPI_bcast(reaction_rate_rank,(((pr_reaction_dim)-(1)+1)),MPI_
     &INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_rate_spacing =>mem_alloc_i2((0),(3),(1),(pr_reaction_dim)
     &,'reaction_rate_spacing')
      endif
      call MPI_bcast(reaction_rate_spacing,(((3)-(0)+1)*((pr_reaction_di
     &m)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_rate_eval_name =>mem_alloc_c1((40),(1),(pr_reaction_dim),
     &'reaction_rate_eval_name')
      endif
      call MPI_bcast(reaction_rate_eval_name,(((40)-(1)+1)*((pr_reaction
     &_dim)-(1)+1)),MPI_CHARACTER,mpi_degas2_root,comm_world_dup,mpi_err
     &)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_rate_tab_index =>mem_alloc_i2((1),(3),(1),(pr_reaction_di
     &m),'reaction_rate_tab_index')
      endif
      call MPI_bcast(reaction_rate_tab_index,(((3)-(1)+1)*((pr_reaction_
     &dim)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_rate_var =>mem_alloc_i2((1),(3),(1),(pr_reaction_dim),'re
     &action_rate_var')
      endif
      call MPI_bcast(reaction_rate_var,(((3)-(1)+1)*((pr_reaction_dim)-(
     &1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_rate_num_rand =>mem_alloc_i1((1),(pr_reaction_dim),'react
     &ion_rate_num_rand')
      endif
      call MPI_bcast(reaction_rate_num_rand,(((pr_reaction_dim)-(1)+1)),
     &MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_rate_base =>mem_alloc_i1((1),(pr_reaction_dim),'reaction_
     &rate_base')
      endif
      call MPI_bcast(reaction_rate_base,(((pr_reaction_dim)-(1)+1)),MPI_
     &INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_rate_tab =>mem_alloc_r1((0),(reaction_rate_size-1),'react
     &ion_rate_tab')
      endif
      call MPI_bcast(reaction_rate_tab,(((reaction_rate_size-1)-(0)+1)),
     &MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_handling_min =>mem_alloc_r3((1),(3),(1),(19),(1),(pr_reac
     &tion_dim),'reaction_handling_min')
      endif
      call MPI_bcast(reaction_handling_min,(((3)-(1)+1)*((19)-(1)+1)*((p
     &r_reaction_dim)-(1)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_
     &world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_handling_delta =>mem_alloc_r3((1),(3),(1),(19),(1),(pr_re
     &action_dim),'reaction_handling_delta')
      endif
      call MPI_bcast(reaction_handling_delta,(((3)-(1)+1)*((19)-(1)+1)*(
     &(pr_reaction_dim)-(1)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,com
     &m_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_handling_rank =>mem_alloc_i2((1),(19),(1),(pr_reaction_di
     &m),'reaction_handling_rank')
      endif
      call MPI_bcast(reaction_handling_rank,(((19)-(1)+1)*((pr_reaction_
     &dim)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_handling_spacing =>mem_alloc_i3((0),(3),(1),(19),(1),(pr_
     &reaction_dim),'reaction_handling_spacing')
      endif
      call MPI_bcast(reaction_handling_spacing,(((3)-(0)+1)*((19)-(1)+1)
     &*((pr_reaction_dim)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world
     &_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_handling_eval_name =>mem_alloc_c2((40),(1),(19),(1),(pr_r
     &eaction_dim),'reaction_handling_eval_name')
      endif
      call MPI_bcast(reaction_handling_eval_name,(((40)-(1)+1)*((19)-(1)
     &+1)*((pr_reaction_dim)-(1)+1)),MPI_CHARACTER,mpi_degas2_root,comm_
     &world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_handling_tab_index =>mem_alloc_i3((1),(3),(1),(19),(1),(p
     &r_reaction_dim),'reaction_handling_tab_index')
      endif
      call MPI_bcast(reaction_handling_tab_index,(((3)-(1)+1)*((19)-(1)+
     &1)*((pr_reaction_dim)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_wor
     &ld_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_handling_var0 =>mem_alloc_i2((1),(19),(1),(pr_reaction_di
     &m),'reaction_handling_var0')
      endif
      call MPI_bcast(reaction_handling_var0,(((19)-(1)+1)*((pr_reaction_
     &dim)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_handling_var =>mem_alloc_i3((1),(3),(1),(19),(1),(pr_reac
     &tion_dim),'reaction_handling_var')
      endif
      call MPI_bcast(reaction_handling_var,(((3)-(1)+1)*((19)-(1)+1)*((p
     &r_reaction_dim)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup
     &,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_handling_num_rand =>mem_alloc_i1((1),(pr_reaction_dim),'r
     &eaction_handling_num_rand')
      endif
      call MPI_bcast(reaction_handling_num_rand,(((pr_reaction_dim)-(1)+
     &1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_handling_base =>mem_alloc_i2((1),(19),(1),(pr_reaction_di
     &m),'reaction_handling_base')
      endif
      call MPI_bcast(reaction_handling_base,(((19)-(1)+1)*((pr_reaction_
     &dim)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      reaction_handling_tab =>mem_alloc_r1((0),(reaction_handling_size-1
     &),'reaction_handling_tab')
      endif
      call MPI_bcast(reaction_handling_tab,(((reaction_handling_size-1)-
     &(0)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_er
     &r)
      
      call MPI_bcast(pmi_yield_size,1,MPI_INTEGER,mpi_degas2_root,comm_w
     &orld_dup,mpi_err)
      call MPI_bcast(pmi_handling_size,1,MPI_INTEGER,mpi_degas2_root,com
     &m_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_yield_min =>mem_alloc_r2((1),(5),(1),(pr_pmi_num),'pmi_yield_m
     &in')
      endif
      call MPI_bcast(pmi_yield_min,(((5)-(1)+1)*((pr_pmi_num)-(1)+1)),MP
     &I_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_yield_delta =>mem_alloc_r2((1),(5),(1),(pr_pmi_num),'pmi_yield
     &_delta')
      endif
      call MPI_bcast(pmi_yield_delta,(((5)-(1)+1)*((pr_pmi_num)-(1)+1)),
     &MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_yield_rank =>mem_alloc_i1((1),(pr_pmi_num),'pmi_yield_rank')
      endif
      call MPI_bcast(pmi_yield_rank,(((pr_pmi_num)-(1)+1)),MPI_INTEGER,m
     &pi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_yield_spacing =>mem_alloc_i2((0),(5),(1),(pr_pmi_num),'pmi_yie
     &ld_spacing')
      endif
      call MPI_bcast(pmi_yield_spacing,(((5)-(0)+1)*((pr_pmi_num)-(1)+1)
     &),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_yield_eval_name =>mem_alloc_c1((40),(1),(pr_pmi_num),'pmi_yiel
     &d_eval_name')
      endif
      call MPI_bcast(pmi_yield_eval_name,(((40)-(1)+1)*((pr_pmi_num)-(1)
     &+1)),MPI_CHARACTER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_yield_tab_index =>mem_alloc_i2((1),(5),(1),(pr_pmi_num),'pmi_y
     &ield_tab_index')
      endif
      call MPI_bcast(pmi_yield_tab_index,(((5)-(1)+1)*((pr_pmi_num)-(1)+
     &1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_yield_var =>mem_alloc_i2((1),(5),(1),(pr_pmi_num),'pmi_yield_v
     &ar')
      endif
      call MPI_bcast(pmi_yield_var,(((5)-(1)+1)*((pr_pmi_num)-(1)+1)),MP
     &I_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_yield_num_rand =>mem_alloc_i1((1),(pr_pmi_num),'pmi_yield_num_
     &rand')
      endif
      call MPI_bcast(pmi_yield_num_rand,(((pr_pmi_num)-(1)+1)),MPI_INTEG
     &ER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_yield_base =>mem_alloc_i1((1),(pr_pmi_num),'pmi_yield_base')
      endif
      call MPI_bcast(pmi_yield_base,(((pr_pmi_num)-(1)+1)),MPI_INTEGER,m
     &pi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_yield_tab =>mem_alloc_r1((0),(pmi_yield_size-1),'pmi_yield_tab
     &')
      endif
      call MPI_bcast(pmi_yield_tab,(((pmi_yield_size-1)-(0)+1)),MPI_DOUB
     &LE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_handling_min =>mem_alloc_r3((1),(5),(1),(4),(1),(pr_pmi_num),'
     &pmi_handling_min')
      endif
      call MPI_bcast(pmi_handling_min,(((5)-(1)+1)*((4)-(1)+1)*((pr_pmi_
     &num)-(1)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,m
     &pi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_handling_delta =>mem_alloc_r3((1),(5),(1),(4),(1),(pr_pmi_num)
     &,'pmi_handling_delta')
      endif
      call MPI_bcast(pmi_handling_delta,(((5)-(1)+1)*((4)-(1)+1)*((pr_pm
     &i_num)-(1)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup
     &,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_handling_rank =>mem_alloc_i2((1),(4),(1),(pr_pmi_num),'pmi_han
     &dling_rank')
      endif
      call MPI_bcast(pmi_handling_rank,(((4)-(1)+1)*((pr_pmi_num)-(1)+1)
     &),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_handling_spacing =>mem_alloc_i3((0),(5),(1),(4),(1),(pr_pmi_nu
     &m),'pmi_handling_spacing')
      endif
      call MPI_bcast(pmi_handling_spacing,(((5)-(0)+1)*((4)-(1)+1)*((pr_
     &pmi_num)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_er
     &r)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_handling_eval_name =>mem_alloc_c2((40),(1),(4),(1),(pr_pmi_num
     &),'pmi_handling_eval_name')
      endif
      call MPI_bcast(pmi_handling_eval_name,(((40)-(1)+1)*((4)-(1)+1)*((
     &pr_pmi_num)-(1)+1)),MPI_CHARACTER,mpi_degas2_root,comm_world_dup,m
     &pi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_handling_tab_index =>mem_alloc_i3((1),(5),(1),(4),(1),(pr_pmi_
     &num),'pmi_handling_tab_index')
      endif
      call MPI_bcast(pmi_handling_tab_index,(((5)-(1)+1)*((4)-(1)+1)*((p
     &r_pmi_num)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_
     &err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_handling_var0 =>mem_alloc_c2((20),(1),(4),(1),(pr_pmi_num),'pm
     &i_handling_var0')
      endif
      call MPI_bcast(pmi_handling_var0,(((20)-(1)+1)*((4)-(1)+1)*((pr_pm
     &i_num)-(1)+1)),MPI_CHARACTER,mpi_degas2_root,comm_world_dup,mpi_er
     &r)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_handling_var =>mem_alloc_i3((1),(5),(1),(4),(1),(pr_pmi_num),'
     &pmi_handling_var')
      endif
      call MPI_bcast(pmi_handling_var,(((5)-(1)+1)*((4)-(1)+1)*((pr_pmi_
     &num)-(1)+1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_handling_num_rand =>mem_alloc_i1((1),(pr_pmi_num),'pmi_handlin
     &g_num_rand')
      endif
      call MPI_bcast(pmi_handling_num_rand,(((pr_pmi_num)-(1)+1)),MPI_IN
     &TEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_handling_base =>mem_alloc_i2((1),(4),(1),(pr_pmi_num),'pmi_han
     &dling_base')
      endif
      call MPI_bcast(pmi_handling_base,(((4)-(1)+1)*((pr_pmi_num)-(1)+1)
     &),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      pmi_handling_tab =>mem_alloc_r1((0),(pmi_handling_size-1),'pmi_han
     &dling_tab')
      endif
      call MPI_bcast(pmi_handling_tab,(((pmi_handling_size-1)-(0)+1)),MP
     &I_DOUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      
      return
      end
      subroutine nc_read_tally
      
      use pr_mod
      
      use tl_mod
      
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
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      logical check_tally
      integer inc
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
      
      character*96 tempfile
      if((mpi_rank.EQ.mpi_degas2_root))then
      tempfile=filenames_array(19)
      if(tempfile.NE.'undefined')continue
      fileid=ncopn(tempfile,0,nc_stat)
      call ncagtc(fileid,0,'tally_version',tally_version,len(tally_versi
     &on),nc_stat)
      tally_type_ind_id=ncdid(fileid,'tally_type_ind',nc_stat)
      call ncdinq(fileid,tally_type_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((3)-(1)+1))continue
      tally_rank_ind_id=ncdid(fileid,'tally_rank_ind',nc_stat)
      call ncdinq(fileid,tally_rank_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((5)-(1)+1))continue
      tally_est_ind_id=ncdid(fileid,'tally_est_ind',nc_stat)
      call ncdinq(fileid,tally_est_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((4)-(1)+1))continue
      tally_reac_ind_id=ncdid(fileid,'tally_reac_ind',nc_stat)
      call ncdinq(fileid,tally_reac_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((pr_reaction_dim+6)-(1)+1))continue
      tally_name_string_id=ncdid(fileid,'tally_name_string',nc_stat)
      call ncdinq(fileid,tally_name_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((80)-(1)+1))continue
      tally_tag_string_id=ncdid(fileid,'tally_tag_string',nc_stat)
      call ncdinq(fileid,tally_tag_string_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((40)-(1)+1))continue
      tally_cv_ptr_ind_id=ncdid(fileid,'tally_cv_ptr_ind',nc_stat)
      call ncdinq(fileid,tally_cv_ptr_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((5)-(1)+1))continue
      tally_cv_scaler_ind_id=ncdid(fileid,'tally_cv_scaler_ind',nc_stat)
      call ncdinq(fileid,tally_cv_scaler_ind_id,nc_dummy,nc_size,nc_stat
     &)
      if(nc_size.EQ.((3)-(1)+1))continue
      tally_cv_partner_ind_id=ncdid(fileid,'tally_cv_partner_ind',nc_sta
     &t)
      call ncdinq(fileid,tally_cv_partner_ind_id,nc_dummy,nc_size,nc_sta
     &t)
      if(nc_size.EQ.((3)-(1)+1))continue
      tally_index_ind_id=ncdid(fileid,'tally_index_ind',nc_stat)
      call ncdinq(fileid,tally_index_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((100)-(0)+1))continue
      tl_num_id=ncvid(fileid,'tl_num',nc_stat)
      call ncvinq(fileid,tl_num_id,nc_dummy,nc_type,nc_rank,nc_dims,nc_a
     &ttr,nc_stat)
      
      call ncvgt(fileid,tl_num_id,nc_corner,nc_edge,tl_num,nc_stat)
      tally_ind_id=ncdid(fileid,'tally_ind',nc_stat)
      call ncdinq(fileid,tally_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((tl_num)-(1)+1))continue
      nconversions_id=ncvid(fileid,'nconversions',nc_stat)
      call ncvinq(fileid,nconversions_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      
      call ncvgt(fileid,nconversions_id,nc_corner,nc_edge,nconversions,n
     &c_stat)
      tally_cv_ind_id=ncdid(fileid,'tally_cv_ind',nc_stat)
      call ncdinq(fileid,tally_cv_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((nconversions)-(1)+1))continue
      tally_size_id=ncvid(fileid,'tally_size',nc_stat)
      call ncvinq(fileid,tally_size_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      
      call ncvgt(fileid,tally_size_id,nc_corner,nc_edge,tally_size,nc_st
     &at)
      tally_type_num_id=ncvid(fileid,'tally_type_num',nc_stat)
      call ncvinq(fileid,tally_type_num_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_type_ind_id)continue
      
      tally_type_num =>mem_alloc_i1((1),(3),'tally_type_num')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      call ncvgt(fileid,tally_type_num_id,nc_corner,nc_edge,tally_type_n
     &um,nc_stat)
      tally_type_id=ncvid(fileid,'tally_type',nc_stat)
      call ncvinq(fileid,tally_type_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_ind_id)continue
      
      tally_type =>mem_alloc_i1((1),(tl_num),'tally_type')
      nc_corner(1)=1
      nc_edge(1)=((tl_num)-(1)+1)
      call ncvgt(fileid,tally_type_id,nc_corner,nc_edge,tally_type,nc_st
     &at)
      tally_geometry_id=ncvid(fileid,'tally_geometry',nc_stat)
      call ncvinq(fileid,tally_geometry_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_ind_id)continue
      
      tally_geometry =>mem_alloc_i1((1),(tl_num),'tally_geometry')
      nc_corner(1)=1
      nc_edge(1)=((tl_num)-(1)+1)
      call ncvgt(fileid,tally_geometry_id,nc_corner,nc_edge,tally_geomet
     &ry,nc_stat)
      tally_geometry_ptr_id=ncvid(fileid,'tally_geometry_ptr',nc_stat)
      call ncvinq(fileid,tally_geometry_ptr_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_ind_id)continue
      
      tally_geometry_ptr =>mem_alloc_i1((1),(tl_num),'tally_geometry_ptr
     &')
      nc_corner(1)=1
      nc_edge(1)=((tl_num)-(1)+1)
      call ncvgt(fileid,tally_geometry_ptr_id,nc_corner,nc_edge,tally_ge
     &ometry_ptr,nc_stat)
      tally_base_id=ncvid(fileid,'tally_base',nc_stat)
      call ncvinq(fileid,tally_base_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_ind_id)continue
      
      tally_base =>mem_alloc_i1((1),(tl_num),'tally_base')
      nc_corner(1)=1
      nc_edge(1)=((tl_num)-(1)+1)
      call ncvgt(fileid,tally_base_id,nc_corner,nc_edge,tally_base,nc_st
     &at)
      tally_type_base_id=ncvid(fileid,'tally_type_base',nc_stat)
      call ncvinq(fileid,tally_type_base_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_type_ind_id)continue
      
      tally_type_base =>mem_alloc_i1((1),(3),'tally_type_base')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      call ncvgt(fileid,tally_type_base_id,nc_corner,nc_edge,tally_type_
     &base,nc_stat)
      tally_rank_id=ncvid(fileid,'tally_rank',nc_stat)
      call ncvinq(fileid,tally_rank_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_ind_id)continue
      
      tally_rank =>mem_alloc_i1((1),(tl_num),'tally_rank')
      nc_corner(1)=1
      nc_edge(1)=((tl_num)-(1)+1)
      call ncvgt(fileid,tally_rank_id,nc_corner,nc_edge,tally_rank,nc_st
     &at)
      tally_dep_var_dim_id=ncvid(fileid,'tally_dep_var_dim',nc_stat)
      call ncvinq(fileid,tally_dep_var_dim_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_ind_id)continue
      
      tally_dep_var_dim =>mem_alloc_i1((1),(tl_num),'tally_dep_var_dim')
      nc_corner(1)=1
      nc_edge(1)=((tl_num)-(1)+1)
      call ncvgt(fileid,tally_dep_var_dim_id,nc_corner,nc_edge,tally_dep
     &_var_dim,nc_stat)
      tally_indep_var_id=ncvid(fileid,'tally_indep_var',nc_stat)
      call ncvinq(fileid,tally_indep_var_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_rank_ind_id)continue
      if(nc_dims(2).EQ.tally_ind_id)continue
      
      tally_indep_var =>mem_alloc_i2((1),(5),(1),(tl_num),'tally_indep_v
     &ar')
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((tl_num)-(1)+1)
      call ncvgt(fileid,tally_indep_var_id,nc_corner,nc_edge,tally_indep
     &_var,nc_stat)
      tally_tab_index_id=ncvid(fileid,'tally_tab_index',nc_stat)
      call ncvinq(fileid,tally_tab_index_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_rank_ind_id)continue
      if(nc_dims(2).EQ.tally_ind_id)continue
      
      tally_tab_index =>mem_alloc_i2((1),(5),(1),(tl_num),'tally_tab_ind
     &ex')
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((tl_num)-(1)+1)
      call ncvgt(fileid,tally_tab_index_id,nc_corner,nc_edge,tally_tab_i
     &ndex,nc_stat)
      tally_name_id=ncvid(fileid,'tally_name',nc_stat)
      call ncvinq(fileid,tally_name_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_name_string_id)continue
      if(nc_dims(2).EQ.tally_ind_id)continue
      
      tally_name =>mem_alloc_c1((80),(1),(tl_num),'tally_name')
      nc_corner(1)=1
      nc_edge(1)=((80)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((tl_num)-(1)+1)
      nc_stat=nf_get_vara_text(fileid,tally_name_id,nc_corner,nc_edge,ta
     &lly_name)
      tally_dep_var_id=ncvid(fileid,'tally_dep_var',nc_stat)
      call ncvinq(fileid,tally_dep_var_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_ind_id)continue
      
      tally_dep_var =>mem_alloc_i1((1),(tl_num),'tally_dep_var')
      nc_corner(1)=1
      nc_edge(1)=((tl_num)-(1)+1)
      call ncvgt(fileid,tally_dep_var_id,nc_corner,nc_edge,tally_dep_var
     &,nc_stat)
      tally_est_test_id=ncvid(fileid,'tally_est_test',nc_stat)
      call ncvinq(fileid,tally_est_test_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_est_ind_id)continue
      if(nc_dims(2).EQ.tally_ind_id)continue
      
      tally_est_test =>mem_alloc_r2((1),(4),(1),(tl_num),'tally_est_test
     &')
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((tl_num)-(1)+1)
      call ncvgt(fileid,tally_est_test_id,nc_corner,nc_edge,tally_est_te
     &st,nc_stat)
      tally_est_reaction_id=ncvid(fileid,'tally_est_reaction',nc_stat)
      call ncvinq(fileid,tally_est_reaction_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_reac_ind_id)continue
      if(nc_dims(2).EQ.tally_est_ind_id)continue
      if(nc_dims(3).EQ.tally_ind_id)continue
      
      tally_est_reaction =>mem_alloc_r3((1),(pr_reaction_dim+6),(1),(4),
     &(1),(tl_num),'tally_est_reaction')
      nc_corner(1)=1
      nc_edge(1)=((pr_reaction_dim+6)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((tl_num)-(1)+1)
      call ncvgt(fileid,tally_est_reaction_id,nc_corner,nc_edge,tally_es
     &t_reaction,nc_stat)
      tally_num_conversions_id=ncvid(fileid,'tally_num_conversions',nc_s
     &tat)
      call ncvinq(fileid,tally_num_conversions_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_ind_id)continue
      
      tally_num_conversions =>mem_alloc_i1((1),(tl_num),'tally_num_conve
     &rsions')
      nc_corner(1)=1
      nc_edge(1)=((tl_num)-(1)+1)
      call ncvgt(fileid,tally_num_conversions_id,nc_corner,nc_edge,tally
     &_num_conversions,nc_stat)
      tally_cv_ptr_id=ncvid(fileid,'tally_cv_ptr',nc_stat)
      call ncvinq(fileid,tally_cv_ptr_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_cv_ptr_ind_id)continue
      if(nc_dims(2).EQ.tally_ind_id)continue
      
      tally_cv_ptr =>mem_alloc_i2((1),(5),(1),(tl_num),'tally_cv_ptr')
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((tl_num)-(1)+1)
      call ncvgt(fileid,tally_cv_ptr_id,nc_corner,nc_edge,tally_cv_ptr,n
     &c_stat)
      tally_cv_action_id=ncvid(fileid,'tally_cv_action',nc_stat)
      call ncvinq(fileid,tally_cv_action_id,nc_dummy,nc_type,nc_rank,nc_
     &dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_cv_ind_id)continue
      
      tally_cv_action =>mem_alloc_i1((1),(nconversions),'tally_cv_action
     &')
      nc_corner(1)=1
      nc_edge(1)=((nconversions)-(1)+1)
      call ncvgt(fileid,tally_cv_action_id,nc_corner,nc_edge,tally_cv_ac
     &tion,nc_stat)
      tally_cv_type_id=ncvid(fileid,'tally_cv_type',nc_stat)
      call ncvinq(fileid,tally_cv_type_id,nc_dummy,nc_type,nc_rank,nc_di
     &ms,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_cv_ind_id)continue
      
      tally_cv_type =>mem_alloc_i1((1),(nconversions),'tally_cv_type')
      nc_corner(1)=1
      nc_edge(1)=((nconversions)-(1)+1)
      call ncvgt(fileid,tally_cv_type_id,nc_corner,nc_edge,tally_cv_type
     &,nc_stat)
      tally_cv_num_partners_id=ncvid(fileid,'tally_cv_num_partners',nc_s
     &tat)
      call ncvinq(fileid,tally_cv_num_partners_id,nc_dummy,nc_type,nc_ra
     &nk,nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_cv_ind_id)continue
      
      tally_cv_num_partners =>mem_alloc_i1((1),(nconversions),'tally_cv_
     &num_partners')
      nc_corner(1)=1
      nc_edge(1)=((nconversions)-(1)+1)
      call ncvgt(fileid,tally_cv_num_partners_id,nc_corner,nc_edge,tally
     &_cv_num_partners,nc_stat)
      tally_cv_scalers_id=ncvid(fileid,'tally_cv_scalers',nc_stat)
      call ncvinq(fileid,tally_cv_scalers_id,nc_dummy,nc_type,nc_rank,nc
     &_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_cv_scaler_ind_id)continue
      if(nc_dims(2).EQ.tally_cv_ind_id)continue
      
      tally_cv_scalers =>mem_alloc_i2((1),(3),(1),(nconversions),'tally_
     &cv_scalers')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((nconversions)-(1)+1)
      call ncvgt(fileid,tally_cv_scalers_id,nc_corner,nc_edge,tally_cv_s
     &calers,nc_stat)
      tally_cv_partners_id=ncvid(fileid,'tally_cv_partners',nc_stat)
      call ncvinq(fileid,tally_cv_partners_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_cv_partner_ind_id)continue
      if(nc_dims(2).EQ.tally_cv_ind_id)continue
      
      tally_cv_partners =>mem_alloc_i2((1),(3),(1),(nconversions),'tally
     &_cv_partners')
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((nconversions)-(1)+1)
      call ncvgt(fileid,tally_cv_partners_id,nc_corner,nc_edge,tally_cv_
     &partners,nc_stat)
      tally_var_list_id=ncvid(fileid,'tally_var_list',nc_stat)
      call ncvinq(fileid,tally_var_list_id,nc_dummy,nc_type,nc_rank,nc_d
     &ims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.tally_tag_string_id)continue
      if(nc_dims(2).EQ.tally_index_ind_id)continue
      
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((100)-(0)+1)
      nc_stat=nf_get_vara_text(fileid,tally_var_list_id,nc_corner,nc_edg
     &e,tally_var_list)
      
      call ncclos(fileid,nc_stat)
      end if
      call MPI_bcast(tl_num,1,MPI_INTEGER,mpi_degas2_root,comm_world_dup
     &,mpi_err)
      call MPI_bcast(nconversions,1,MPI_INTEGER,mpi_degas2_root,comm_wor
     &ld_dup,mpi_err)
      call MPI_bcast(tally_size,1,MPI_INTEGER,mpi_degas2_root,comm_world
     &_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_type_num =>mem_alloc_i1((1),(3),'tally_type_num')
      endif
      call MPI_bcast(tally_type_num,(((3)-(1)+1)),MPI_INTEGER,mpi_degas2
     &_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_type =>mem_alloc_i1((1),(tl_num),'tally_type')
      endif
      call MPI_bcast(tally_type,(((tl_num)-(1)+1)),MPI_INTEGER,mpi_degas
     &2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_geometry =>mem_alloc_i1((1),(tl_num),'tally_geometry')
      endif
      call MPI_bcast(tally_geometry,(((tl_num)-(1)+1)),MPI_INTEGER,mpi_d
     &egas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_geometry_ptr =>mem_alloc_i1((1),(tl_num),'tally_geometry_ptr
     &')
      endif
      call MPI_bcast(tally_geometry_ptr,(((tl_num)-(1)+1)),MPI_INTEGER,m
     &pi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_base =>mem_alloc_i1((1),(tl_num),'tally_base')
      endif
      call MPI_bcast(tally_base,(((tl_num)-(1)+1)),MPI_INTEGER,mpi_degas
     &2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_type_base =>mem_alloc_i1((1),(3),'tally_type_base')
      endif
      call MPI_bcast(tally_type_base,(((3)-(1)+1)),MPI_INTEGER,mpi_degas
     &2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_rank =>mem_alloc_i1((1),(tl_num),'tally_rank')
      endif
      call MPI_bcast(tally_rank,(((tl_num)-(1)+1)),MPI_INTEGER,mpi_degas
     &2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_dep_var_dim =>mem_alloc_i1((1),(tl_num),'tally_dep_var_dim')
      endif
      call MPI_bcast(tally_dep_var_dim,(((tl_num)-(1)+1)),MPI_INTEGER,mp
     &i_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_indep_var =>mem_alloc_i2((1),(5),(1),(tl_num),'tally_indep_v
     &ar')
      endif
      call MPI_bcast(tally_indep_var,(((5)-(1)+1)*((tl_num)-(1)+1)),MPI_
     &INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_tab_index =>mem_alloc_i2((1),(5),(1),(tl_num),'tally_tab_ind
     &ex')
      endif
      call MPI_bcast(tally_tab_index,(((5)-(1)+1)*((tl_num)-(1)+1)),MPI_
     &INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_name =>mem_alloc_c1((80),(1),(tl_num),'tally_name')
      endif
      call MPI_bcast(tally_name,(((80)-(1)+1)*((tl_num)-(1)+1)),MPI_CHAR
     &ACTER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_dep_var =>mem_alloc_i1((1),(tl_num),'tally_dep_var')
      endif
      call MPI_bcast(tally_dep_var,(((tl_num)-(1)+1)),MPI_INTEGER,mpi_de
     &gas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_est_test =>mem_alloc_r2((1),(4),(1),(tl_num),'tally_est_test
     &')
      endif
      call MPI_bcast(tally_est_test,(((4)-(1)+1)*((tl_num)-(1)+1)),MPI_D
     &OUBLE_PRECISION,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_est_reaction =>mem_alloc_r3((1),(pr_reaction_dim+6),(1),(4),
     &(1),(tl_num),'tally_est_reaction')
      endif
      call MPI_bcast(tally_est_reaction,(((pr_reaction_dim+6)-(1)+1)*((4
     &)-(1)+1)*((tl_num)-(1)+1)),MPI_DOUBLE_PRECISION,mpi_degas2_root,co
     &mm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_num_conversions =>mem_alloc_i1((1),(tl_num),'tally_num_conve
     &rsions')
      endif
      call MPI_bcast(tally_num_conversions,(((tl_num)-(1)+1)),MPI_INTEGE
     &R,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_cv_ptr =>mem_alloc_i2((1),(5),(1),(tl_num),'tally_cv_ptr')
      endif
      call MPI_bcast(tally_cv_ptr,(((5)-(1)+1)*((tl_num)-(1)+1)),MPI_INT
     &EGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_cv_action =>mem_alloc_i1((1),(nconversions),'tally_cv_action
     &')
      endif
      call MPI_bcast(tally_cv_action,(((nconversions)-(1)+1)),MPI_INTEGE
     &R,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_cv_type =>mem_alloc_i1((1),(nconversions),'tally_cv_type')
      endif
      call MPI_bcast(tally_cv_type,(((nconversions)-(1)+1)),MPI_INTEGER,
     &mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_cv_num_partners =>mem_alloc_i1((1),(nconversions),'tally_cv_
     &num_partners')
      endif
      call MPI_bcast(tally_cv_num_partners,(((nconversions)-(1)+1)),MPI_
     &INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_cv_scalers =>mem_alloc_i2((1),(3),(1),(nconversions),'tally_
     &cv_scalers')
      endif
      call MPI_bcast(tally_cv_scalers,(((3)-(1)+1)*((nconversions)-(1)+1
     &)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      if(mpi_rank.NE.mpi_degas2_root)then
      tally_cv_partners =>mem_alloc_i2((1),(3),(1),(nconversions),'tally
     &_cv_partners')
      endif
      call MPI_bcast(tally_cv_partners,(((3)-(1)+1)*((nconversions)-(1)+
     &1)),MPI_INTEGER,mpi_degas2_root,comm_world_dup,mpi_err)
      call MPI_bcast(tally_var_list,(((40)-(1)+1)*((100)-(0)+1)),MPI_CHA
     &RACTER,mpi_degas2_root,comm_world_dup,mpi_err)
      
      return
      end
      subroutine nc_read_output
      
      use pr_mod
      
      use so_mod
      
      use tl_mod
      
      use ou_mod
      
      use rf_mod
      
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
      
      character*96 tempfile
      tempfile=filenames_array(20)
      if(tempfile.NE.'undefined')continue
      call ncpopt(0)
      fileid=ncopn(tempfile,0,nc_stat)
      if(nc_stat.NE.0)then
      output_old_file=0
      return
      else
      output_old_file=1
      end if
      call ncpopt(2+1)
      output_coupling_ind_id=ncdid(fileid,'output_coupling_ind',nc_stat)
      call ncdinq(fileid,output_coupling_ind_id,nc_dummy,nc_size,nc_stat
     &)
      if(nc_size.EQ.((5)-(1)+1))continue
      output_moments_ind_id=ncdid(fileid,'output_moments_ind',nc_stat)
      call ncdinq(fileid,output_moments_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((1)-(0)+1))continue
      ou_back_ind_id=ncdid(fileid,'ou_back_ind',nc_stat)
      call ncdinq(fileid,ou_back_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((pr_background_num)-(1)+1))continue
      output_tab_ind_id=ncdid(fileid,'output_tab_ind',nc_stat)
      call ncdinq(fileid,output_tab_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((tally_size-1)-(0)+1))continue
      output_grps_ind_id=ncdid(fileid,'output_grps_ind',nc_stat)
      call ncdinq(fileid,output_grps_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((so_grps)-(1)+1))continue
      output_seed_ind_id=ncdid(fileid,'output_seed_ind',nc_stat)
      call ncdinq(fileid,output_seed_ind_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((8-1)-(0)+1))continue
      output_index_1_min_id=ncvid(fileid,'output_index_1_min',nc_stat)
      call ncvinq(fileid,output_index_1_min_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,output_index_1_min_id,nc_corner,nc_edge,output_i
     &ndex_1_min,nc_stat)
      output_index_1_max_id=ncvid(fileid,'output_index_1_max',nc_stat)
      call ncvinq(fileid,output_index_1_max_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,output_index_1_max_id,nc_corner,nc_edge,output_i
     &ndex_1_max,nc_stat)
      output_index_2_min_id=ncvid(fileid,'output_index_2_min',nc_stat)
      call ncvinq(fileid,output_index_2_min_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,output_index_2_min_id,nc_corner,nc_edge,output_i
     &ndex_2_min,nc_stat)
      output_index_2_max_id=ncvid(fileid,'output_index_2_max',nc_stat)
      call ncvinq(fileid,output_index_2_max_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,output_index_2_max_id,nc_corner,nc_edge,output_i
     &ndex_2_max,nc_stat)
      output_ind_1_id=ncdid(fileid,'output_ind_1',nc_stat)
      call ncdinq(fileid,output_ind_1_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((output_index_1_max)-(output_index_1_min)+1))contin
     &ue
      output_ind_2_id=ncdid(fileid,'output_ind_2',nc_stat)
      call ncdinq(fileid,output_ind_2_id,nc_dummy,nc_size,nc_stat)
      if(nc_size.EQ.((output_index_2_max)-(output_index_2_min)+1))contin
     &ue
      output_checkpoint_id=ncvid(fileid,'output_checkpoint',nc_stat)
      call ncvinq(fileid,output_checkpoint_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      
      call ncvgt(fileid,output_checkpoint_id,nc_corner,nc_edge,output_ch
     &eckpoint,nc_stat)
      output_all_id=ncvid(fileid,'output_all',nc_stat)
      call ncvinq(fileid,output_all_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      if(nc_dims(1).EQ.output_moments_ind_id)continue
      if(nc_dims(2).EQ.output_tab_ind_id)continue
      
      output_all =>mem_alloc_r2((0),(1),(0),(tally_size-1),'output_all')
      nc_corner(1)=1
      nc_edge(1)=((1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((tally_size-1)-(0)+1)
      call ncvgt(fileid,output_all_id,nc_corner,nc_edge,output_all,nc_st
     &at)
      output_grp_id=ncvid(fileid,'output_grp',nc_stat)
      call ncvinq(fileid,output_grp_id,nc_dummy,nc_type,nc_rank,nc_dims,
     &nc_attr,nc_stat)
      if(nc_dims(1).EQ.output_moments_ind_id)continue
      if(nc_dims(2).EQ.output_tab_ind_id)continue
      if(nc_dims(3).EQ.output_grps_ind_id)continue
      
      output_grp =>mem_alloc_r3((0),(1),(0),(tally_size-1),(1),(so_grps)
     &,'output_grp')
      nc_corner(1)=1
      nc_edge(1)=((1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((tally_size-1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((so_grps)-(1)+1)
      call ncvgt(fileid,output_grp_id,nc_corner,nc_edge,output_grp,nc_st
     &at)
      out_post_all_id=ncvid(fileid,'out_post_all',nc_stat)
      call ncvinq(fileid,out_post_all_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.output_moments_ind_id)continue
      if(nc_dims(2).EQ.output_tab_ind_id)continue
      
      out_post_all =>mem_alloc_r2((0),(1),(0),(tally_size-1),'out_post_a
     &ll')
      nc_corner(1)=1
      nc_edge(1)=((1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((tally_size-1)-(0)+1)
      call ncvgt(fileid,out_post_all_id,nc_corner,nc_edge,out_post_all,n
     &c_stat)
      out_post_grp_id=ncvid(fileid,'out_post_grp',nc_stat)
      call ncvinq(fileid,out_post_grp_id,nc_dummy,nc_type,nc_rank,nc_dim
     &s,nc_attr,nc_stat)
      if(nc_dims(1).EQ.output_moments_ind_id)continue
      if(nc_dims(2).EQ.output_tab_ind_id)continue
      if(nc_dims(3).EQ.output_grps_ind_id)continue
      
      out_post_grp =>mem_alloc_r3((0),(1),(0),(tally_size-1),(1),(so_grp
     &s),'out_post_grp')
      nc_corner(1)=1
      nc_edge(1)=((1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((tally_size-1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((so_grps)-(1)+1)
      call ncvgt(fileid,out_post_grp_id,nc_corner,nc_edge,out_post_grp,n
     &c_stat)
      output_2D_coupling_id=ncvid(fileid,'output_2D_coupling',nc_stat)
      call ncvinq(fileid,output_2D_coupling_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.output_ind_1_id)continue
      if(nc_dims(2).EQ.output_ind_2_id)continue
      if(nc_dims(3).EQ.ou_back_ind_id)continue
      if(nc_dims(4).EQ.output_coupling_ind_id)continue
      if(nc_dims(5).EQ.output_grps_ind_id)continue
      
      output_2D_coupling =>mem_alloc_r5((output_index_1_min),(output_ind
     &ex_1_max),(output_index_2_min),(output_index_2_max),(1),(pr_backgr
     &ound_num),(1),(5),(1),(so_grps),'output_2D_coupling')
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
      call ncvgt(fileid,output_2D_coupling_id,nc_corner,nc_edge,output_2
     &D_coupling,nc_stat)
      output_weight_grp_id=ncvid(fileid,'output_weight_grp',nc_stat)
      call ncvinq(fileid,output_weight_grp_id,nc_dummy,nc_type,nc_rank,n
     &c_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.output_grps_ind_id)continue
      
      output_weight_grp =>mem_alloc_r1((1),(so_grps),'output_weight_grp'
     &)
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,output_weight_grp_id,nc_corner,nc_edge,output_we
     &ight_grp,nc_stat)
      output_num_flights_id=ncvid(fileid,'output_num_flights',nc_stat)
      call ncvinq(fileid,output_num_flights_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.output_grps_ind_id)continue
      
      output_num_flights =>mem_alloc_i1((1),(so_grps),'output_num_flight
     &s')
      nc_corner(1)=1
      nc_edge(1)=((so_grps)-(1)+1)
      call ncvgt(fileid,output_num_flights_id,nc_corner,nc_edge,output_n
     &um_flights,nc_stat)
      output_random_seed_id=ncvid(fileid,'output_random_seed',nc_stat)
      call ncvinq(fileid,output_random_seed_id,nc_dummy,nc_type,nc_rank,
     &nc_dims,nc_attr,nc_stat)
      if(nc_dims(1).EQ.output_seed_ind_id)continue
      if(nc_dims(2).EQ.output_grps_ind_id)continue
      
      output_random_seed =>mem_alloc_i2((0),(8-1),(1),(so_grps),'output_
     &random_seed')
      nc_corner(1)=1
      nc_edge(1)=((8-1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((so_grps)-(1)+1)
      call ncvgt(fileid,output_random_seed_id,nc_corner,nc_edge,output_r
     &andom_seed,nc_stat)
      
      call ncclos(fileid,nc_stat)
      return
      end
      subroutine read_cramd_data
      
      use pr_mod
      
      use rd_mod
      
      use rf_mod
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      include 'mpif.h'
      integer mpi_err
      integer mpi_status
      dimension mpi_status(MPI_STATUS_SIZE)
      integer pr_reac
      logical have_cramd_data
      character*96 tempfile
      have_cramd_data=.FALSE.
      do pr_reac=1,pr_reaction_num
      if(reaction_rate_eval_name(pr_reac).EQ.'cramd')have_cramd_data=.TR
     &UE.
      end do
      if(.NOT.have_cramd_data)return
      tempfile=filenames_array(5)
      if(tempfile.NE.'undefined')continue
      filenames_array(5)=filenames_array(18)
      call nc_read_problem
      filenames_array(5)=tempfile
      return
      end
      subroutine nc_write_problem
      
      use pr_mod
      
      use rd_mod
      
      use pd_mod
      
      use sp_mod
      
      use ma_mod
      
      use pm_mod
      
      use rf_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer inc
      integer nc_dims(5),nc_corner(5),nc_edge(5),nc_stat,nc_size,nc_rank
     &,nc_attr,nc_type
      character*128 nc_dummy
      external ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf
     &_inq_format
      integer ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf_
     &inq_format
      integer fileid
      integer pr_test_num_id
      integer pr_background_num_id
      integer pr_ex_test_num_id
      integer pr_ex_test_dim_id
      integer pr_reaction_num_id
      integer pr_reaction_dim_id
      integer pr_bkrc_num_id
      integer pr_bkrc_dim_id
      integer pr_exrc_num_id
      integer pr_exrc_dim_id
      integer pr_materials_num_id
      integer pr_pmi_num_id
      integer pr_var0_num_id
      integer problem_test_ind_id
      integer problem_background_ind_id
      integer problem_ex_test_ind_id
      integer problem_reaction_ind_id
      integer problem_reaction_ind0_id
      integer problem_bkrc_ind_id
      integer problem_bkrc_rg_ind_id
      integer problem_exrc_ind_id
      integer problem_species_ind_id
      integer problem_materials_ref_ind_id
      integer problem_materials_sub_ind_id
      integer problem_pmi_ref_ind_id
      integer problem_pmi_sub_ind_id
      integer problem_pmi_ind0_id
      integer problem_product_ind_id
      integer problem_arr_ind_id
      integer pr_var0_list_ind_id
      integer pr_tag_string_id
      integer problem_species_test_id
      integer problem_species_background_id
      integer problem_materials_sub_id
      integer problem_test_sp_id
      integer problem_background_sp_id
      integer problem_ex_test_sp_id
      integer problem_materials_ref_id
      integer problem_rc_id
      integer problem_reaction_num_id
      integer problem_test_reaction_id
      integer problem_test_background_id
      integer problem_num_arrangements_id
      integer problem_test_products_id
      integer problem_prod_mult_id
      integer problem_background_reaction_id
      integer problem_bkrc_reagents_id
      integer problem_bkrc_products_id
      integer problem_external_reaction_id
      integer problem_external_test_reaction_num_id
      integer problem_external_test_reaction_id
      integer problem_external_test_background_id
      integer problem_exrc_products_id
      integer problem_pmi_ref_id
      integer problem_pmi_sub_id
      integer problem_pmi_case_num_id
      integer problem_pmi_cases_id
      integer problem_pmi_num_arrange_id
      integer problem_pmi_products_id
      integer problem_pmi_prod_mult_id
      integer pr_var0_list_id
      
      integer rd_rank_ind_id
      integer rd_dep_var_ind_id
      integer rd_rank_ind0_id
      integer rd_tag_string_id
      integer rd_eval_string_id
      integer reaction_rate_size_id
      integer rd_table_ind_id
      integer reaction_handling_size_id
      integer rd_handling_table_ind_id
      integer reaction_rate_min_id
      integer reaction_rate_delta_id
      integer reaction_rate_rank_id
      integer reaction_rate_spacing_id
      integer reaction_rate_eval_name_id
      integer reaction_rate_tab_index_id
      integer reaction_rate_var_id
      integer reaction_rate_num_rand_id
      integer reaction_rate_base_id
      integer reaction_rate_tab_id
      integer reaction_handling_min_id
      integer reaction_handling_delta_id
      integer reaction_handling_rank_id
      integer reaction_handling_spacing_id
      integer reaction_handling_eval_name_id
      integer reaction_handling_tab_index_id
      integer reaction_handling_var0_id
      integer reaction_handling_var_id
      integer reaction_handling_num_rand_id
      integer reaction_handling_base_id
      integer reaction_handling_tab_id
      
      integer pd_rank_ind_id
      integer pd_dep_var_ind_id
      integer pd_rank_ind0_id
      integer pd_tag_string_id
      integer pd_eval_string_id
      integer pmi_yield_size_id
      integer pd_yield_ind_id
      integer pmi_handling_size_id
      integer pd_handling_ind_id
      integer pmi_yield_min_id
      integer pmi_yield_delta_id
      integer pmi_yield_rank_id
      integer pmi_yield_spacing_id
      integer pmi_yield_eval_name_id
      integer pmi_yield_tab_index_id
      integer pmi_yield_var_id
      integer pmi_yield_num_rand_id
      integer pmi_yield_base_id
      integer pmi_yield_tab_id
      integer pmi_handling_min_id
      integer pmi_handling_delta_id
      integer pmi_handling_rank_id
      integer pmi_handling_spacing_id
      integer pmi_handling_eval_name_id
      integer pmi_handling_tab_index_id
      integer pmi_handling_var0_id
      integer pmi_handling_var_id
      integer pmi_handling_num_rand_id
      integer pmi_handling_base_id
      integer pmi_handling_tab_id
      
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      character*300 description,program_version
      character*96 tempfile
      program_version='DEGAS 2 Git commit: $Format:%H$, ref names: $Form
     &at:%d$'
      tempfile=filenames_array(5)
      if(tempfile.NE.'undefined')continue
      fileid=nccre(tempfile,0,nc_stat)
      description='Data for problem in degas 2'
      call ncaptc(fileid,0,'description',2,string_length(description),de
     &scription,nc_stat)
      call ncaptc(fileid,0,'data_version',2,string_length(problem_versio
     &n),problem_version,nc_stat)
      call ncaptc(fileid,0,'program_version',2,string_length(program_ver
     &sion),program_version,nc_stat)
      pr_test_num_id=ncvdef(fileid,'pr_test_num',4,0,nc_dims,nc_stat)
      pr_background_num_id=ncvdef(fileid,'pr_background_num',4,0,nc_dims
     &,nc_stat)
      pr_ex_test_num_id=ncvdef(fileid,'pr_ex_test_num',4,0,nc_dims,nc_st
     &at)
      pr_ex_test_dim_id=ncvdef(fileid,'pr_ex_test_dim',4,0,nc_dims,nc_st
     &at)
      pr_reaction_num_id=ncvdef(fileid,'pr_reaction_num',4,0,nc_dims,nc_
     &stat)
      pr_reaction_dim_id=ncvdef(fileid,'pr_reaction_dim',4,0,nc_dims,nc_
     &stat)
      pr_bkrc_num_id=ncvdef(fileid,'pr_bkrc_num',4,0,nc_dims,nc_stat)
      pr_bkrc_dim_id=ncvdef(fileid,'pr_bkrc_dim',4,0,nc_dims,nc_stat)
      pr_exrc_num_id=ncvdef(fileid,'pr_exrc_num',4,0,nc_dims,nc_stat)
      pr_exrc_dim_id=ncvdef(fileid,'pr_exrc_dim',4,0,nc_dims,nc_stat)
      pr_materials_num_id=ncvdef(fileid,'pr_materials_num',4,0,nc_dims,n
     &c_stat)
      pr_pmi_num_id=ncvdef(fileid,'pr_pmi_num',4,0,nc_dims,nc_stat)
      pr_var0_num_id=ncvdef(fileid,'pr_var0_num',4,0,nc_dims,nc_stat)
      problem_test_ind_id=ncddef(fileid,'problem_test_ind',((pr_test_num
     &)-(1)+1),nc_stat)
      problem_background_ind_id=ncddef(fileid,'problem_background_ind',(
     &(pr_background_num)-(1)+1),nc_stat)
      problem_ex_test_ind_id=ncddef(fileid,'problem_ex_test_ind',((pr_ex
     &_test_dim)-(1)+1),nc_stat)
      problem_reaction_ind_id=ncddef(fileid,'problem_reaction_ind',((pr_
     &reaction_dim)-(1)+1),nc_stat)
      problem_reaction_ind0_id=ncddef(fileid,'problem_reaction_ind0',((1
     &5)-(1)+1),nc_stat)
      problem_bkrc_ind_id=ncddef(fileid,'problem_bkrc_ind',((pr_bkrc_dim
     &)-(1)+1),nc_stat)
      problem_bkrc_rg_ind_id=ncddef(fileid,'problem_bkrc_rg_ind',((2)-(1
     &)+1),nc_stat)
      problem_exrc_ind_id=ncddef(fileid,'problem_exrc_ind',((pr_exrc_dim
     &)-(1)+1),nc_stat)
      problem_species_ind_id=ncddef(fileid,'problem_species_ind',((sp_nu
     &m)-(1)+1),nc_stat)
      problem_materials_ref_ind_id=ncddef(fileid,'problem_materials_ref_
     &ind',((ma_num)-(1)+1),nc_stat)
      problem_materials_sub_ind_id=ncddef(fileid,'problem_materials_sub_
     &ind',((pr_materials_num)-(1)+1),nc_stat)
      problem_pmi_ref_ind_id=ncddef(fileid,'problem_pmi_ref_ind',((pm_nu
     &m)-(1)+1),nc_stat)
      problem_pmi_sub_ind_id=ncddef(fileid,'problem_pmi_sub_ind',((pr_pm
     &i_num)-(1)+1),nc_stat)
      problem_pmi_ind0_id=ncddef(fileid,'problem_pmi_ind0',((8)-(1)+1),n
     &c_stat)
      problem_product_ind_id=ncddef(fileid,'problem_product_ind',((4)-(1
     &)+1),nc_stat)
      problem_arr_ind_id=ncddef(fileid,'problem_arr_ind',((4)-(1)+1),nc_
     &stat)
      pr_var0_list_ind_id=ncddef(fileid,'pr_var0_list_ind',((pr_var0_num
     &)-(1)+1),nc_stat)
      pr_tag_string_id=ncddef(fileid,'pr_tag_string',((40)-(1)+1),nc_sta
     &t)
      nc_dims(1)=problem_species_ind_id
      problem_species_test_id=ncvdef(fileid,'problem_species_test',4,1,n
     &c_dims,nc_stat)
      nc_dims(1)=problem_species_ind_id
      problem_species_background_id=ncvdef(fileid,'problem_species_backg
     &round',4,1,nc_dims,nc_stat)
      nc_dims(1)=problem_materials_ref_ind_id
      problem_materials_sub_id=ncvdef(fileid,'problem_materials_sub',4,1
     &,nc_dims,nc_stat)
      nc_dims(1)=problem_test_ind_id
      problem_test_sp_id=ncvdef(fileid,'problem_test_sp',4,1,nc_dims,nc_
     &stat)
      nc_dims(1)=problem_background_ind_id
      problem_background_sp_id=ncvdef(fileid,'problem_background_sp',4,1
     &,nc_dims,nc_stat)
      nc_dims(1)=problem_ex_test_ind_id
      problem_ex_test_sp_id=ncvdef(fileid,'problem_ex_test_sp',4,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=problem_materials_sub_ind_id
      problem_materials_ref_id=ncvdef(fileid,'problem_materials_ref',4,1
     &,nc_dims,nc_stat)
      nc_dims(1)=problem_reaction_ind_id
      problem_rc_id=ncvdef(fileid,'problem_rc',4,1,nc_dims,nc_stat)
      nc_dims(1)=problem_test_ind_id
      problem_reaction_num_id=ncvdef(fileid,'problem_reaction_num',4,1,n
     &c_dims,nc_stat)
      nc_dims(1)=problem_reaction_ind0_id
      nc_dims(2)=problem_test_ind_id
      problem_test_reaction_id=ncvdef(fileid,'problem_test_reaction',4,2
     &,nc_dims,nc_stat)
      nc_dims(1)=problem_reaction_ind0_id
      nc_dims(2)=problem_test_ind_id
      problem_test_background_id=ncvdef(fileid,'problem_test_background'
     &,4,2,nc_dims,nc_stat)
      nc_dims(1)=problem_reaction_ind0_id
      nc_dims(2)=problem_test_ind_id
      problem_num_arrangements_id=ncvdef(fileid,'problem_num_arrangement
     &s',4,2,nc_dims,nc_stat)
      nc_dims(1)=problem_product_ind_id
      nc_dims(2)=problem_arr_ind_id
      nc_dims(3)=problem_reaction_ind0_id
      nc_dims(4)=problem_test_ind_id
      problem_test_products_id=ncvdef(fileid,'problem_test_products',4,4
     &,nc_dims,nc_stat)
      nc_dims(1)=problem_arr_ind_id
      nc_dims(2)=problem_reaction_ind0_id
      nc_dims(3)=problem_test_ind_id
      problem_prod_mult_id=ncvdef(fileid,'problem_prod_mult',6,3,nc_dims
     &,nc_stat)
      nc_dims(1)=problem_bkrc_ind_id
      problem_background_reaction_id=ncvdef(fileid,'problem_background_r
     &eaction',4,1,nc_dims,nc_stat)
      nc_dims(1)=problem_bkrc_rg_ind_id
      nc_dims(2)=problem_bkrc_ind_id
      problem_bkrc_reagents_id=ncvdef(fileid,'problem_bkrc_reagents',4,2
     &,nc_dims,nc_stat)
      nc_dims(1)=problem_product_ind_id
      nc_dims(2)=problem_bkrc_ind_id
      problem_bkrc_products_id=ncvdef(fileid,'problem_bkrc_products',4,2
     &,nc_dims,nc_stat)
      nc_dims(1)=problem_exrc_ind_id
      problem_external_reaction_id=ncvdef(fileid,'problem_external_react
     &ion',4,1,nc_dims,nc_stat)
      nc_dims(1)=problem_ex_test_ind_id
      problem_external_test_reaction_num_id=ncvdef(fileid,'problem_exter
     &nal_test_reaction_num',4,1,nc_dims,nc_stat)
      nc_dims(1)=problem_reaction_ind0_id
      nc_dims(2)=problem_ex_test_ind_id
      problem_external_test_reaction_id=ncvdef(fileid,'problem_external_
     &test_reaction',4,2,nc_dims,nc_stat)
      nc_dims(1)=problem_reaction_ind0_id
      nc_dims(2)=problem_ex_test_ind_id
      problem_external_test_background_id=ncvdef(fileid,'problem_externa
     &l_test_background',4,2,nc_dims,nc_stat)
      nc_dims(1)=problem_product_ind_id
      nc_dims(2)=problem_reaction_ind0_id
      nc_dims(3)=problem_ex_test_ind_id
      problem_exrc_products_id=ncvdef(fileid,'problem_exrc_products',4,3
     &,nc_dims,nc_stat)
      nc_dims(1)=problem_pmi_sub_ind_id
      problem_pmi_ref_id=ncvdef(fileid,'problem_pmi_ref',4,1,nc_dims,nc_
     &stat)
      nc_dims(1)=problem_pmi_ref_ind_id
      problem_pmi_sub_id=ncvdef(fileid,'problem_pmi_sub',4,1,nc_dims,nc_
     &stat)
      nc_dims(1)=problem_test_ind_id
      problem_pmi_case_num_id=ncvdef(fileid,'problem_pmi_case_num',4,1,n
     &c_dims,nc_stat)
      nc_dims(1)=problem_pmi_ind0_id
      nc_dims(2)=problem_test_ind_id
      problem_pmi_cases_id=ncvdef(fileid,'problem_pmi_cases',4,2,nc_dims
     &,nc_stat)
      nc_dims(1)=problem_pmi_ind0_id
      nc_dims(2)=problem_test_ind_id
      problem_pmi_num_arrange_id=ncvdef(fileid,'problem_pmi_num_arrange'
     &,4,2,nc_dims,nc_stat)
      nc_dims(1)=problem_product_ind_id
      nc_dims(2)=problem_arr_ind_id
      nc_dims(3)=problem_pmi_ind0_id
      nc_dims(4)=problem_test_ind_id
      problem_pmi_products_id=ncvdef(fileid,'problem_pmi_products',4,4,n
     &c_dims,nc_stat)
      nc_dims(1)=problem_arr_ind_id
      nc_dims(2)=problem_pmi_ind0_id
      nc_dims(3)=problem_test_ind_id
      problem_pmi_prod_mult_id=ncvdef(fileid,'problem_pmi_prod_mult',6,3
     &,nc_dims,nc_stat)
      nc_dims(1)=pr_tag_string_id
      nc_dims(2)=pr_var0_list_ind_id
      pr_var0_list_id=ncvdef(fileid,'pr_var0_list',2,2,nc_dims,nc_stat)
      
      rd_rank_ind_id=ncddef(fileid,'rd_rank_ind',((3)-(1)+1),nc_stat)
      rd_dep_var_ind_id=ncddef(fileid,'rd_dep_var_ind',((19)-(1)+1),nc_s
     &tat)
      rd_rank_ind0_id=ncddef(fileid,'rd_rank_ind0',((3)-(0)+1),nc_stat)
      rd_tag_string_id=ncddef(fileid,'rd_tag_string',((40)-(1)+1),nc_sta
     &t)
      rd_eval_string_id=ncddef(fileid,'rd_eval_string',((40)-(1)+1),nc_s
     &tat)
      reaction_rate_size_id=ncvdef(fileid,'reaction_rate_size',4,0,nc_di
     &ms,nc_stat)
      rd_table_ind_id=ncddef(fileid,'rd_table_ind',((reaction_rate_size-
     &1)-(0)+1),nc_stat)
      reaction_handling_size_id=ncvdef(fileid,'reaction_handling_size',4
     &,0,nc_dims,nc_stat)
      rd_handling_table_ind_id=ncddef(fileid,'rd_handling_table_ind',((r
     &eaction_handling_size-1)-(0)+1),nc_stat)
      nc_dims(1)=rd_rank_ind_id
      nc_dims(2)=problem_reaction_ind_id
      reaction_rate_min_id=ncvdef(fileid,'reaction_rate_min',6,2,nc_dims
     &,nc_stat)
      nc_dims(1)=rd_rank_ind_id
      nc_dims(2)=problem_reaction_ind_id
      reaction_rate_delta_id=ncvdef(fileid,'reaction_rate_delta',6,2,nc_
     &dims,nc_stat)
      nc_dims(1)=problem_reaction_ind_id
      reaction_rate_rank_id=ncvdef(fileid,'reaction_rate_rank',4,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=rd_rank_ind0_id
      nc_dims(2)=problem_reaction_ind_id
      reaction_rate_spacing_id=ncvdef(fileid,'reaction_rate_spacing',4,2
     &,nc_dims,nc_stat)
      nc_dims(1)=rd_eval_string_id
      nc_dims(2)=problem_reaction_ind_id
      reaction_rate_eval_name_id=ncvdef(fileid,'reaction_rate_eval_name'
     &,2,2,nc_dims,nc_stat)
      nc_dims(1)=rd_rank_ind_id
      nc_dims(2)=problem_reaction_ind_id
      reaction_rate_tab_index_id=ncvdef(fileid,'reaction_rate_tab_index'
     &,4,2,nc_dims,nc_stat)
      nc_dims(1)=rd_rank_ind_id
      nc_dims(2)=problem_reaction_ind_id
      reaction_rate_var_id=ncvdef(fileid,'reaction_rate_var',4,2,nc_dims
     &,nc_stat)
      nc_dims(1)=problem_reaction_ind_id
      reaction_rate_num_rand_id=ncvdef(fileid,'reaction_rate_num_rand',4
     &,1,nc_dims,nc_stat)
      nc_dims(1)=problem_reaction_ind_id
      reaction_rate_base_id=ncvdef(fileid,'reaction_rate_base',4,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=rd_table_ind_id
      reaction_rate_tab_id=ncvdef(fileid,'reaction_rate_tab',6,1,nc_dims
     &,nc_stat)
      nc_dims(1)=rd_rank_ind_id
      nc_dims(2)=rd_dep_var_ind_id
      nc_dims(3)=problem_reaction_ind_id
      reaction_handling_min_id=ncvdef(fileid,'reaction_handling_min',6,3
     &,nc_dims,nc_stat)
      nc_dims(1)=rd_rank_ind_id
      nc_dims(2)=rd_dep_var_ind_id
      nc_dims(3)=problem_reaction_ind_id
      reaction_handling_delta_id=ncvdef(fileid,'reaction_handling_delta'
     &,6,3,nc_dims,nc_stat)
      nc_dims(1)=rd_dep_var_ind_id
      nc_dims(2)=problem_reaction_ind_id
      reaction_handling_rank_id=ncvdef(fileid,'reaction_handling_rank',4
     &,2,nc_dims,nc_stat)
      nc_dims(1)=rd_rank_ind0_id
      nc_dims(2)=rd_dep_var_ind_id
      nc_dims(3)=problem_reaction_ind_id
      reaction_handling_spacing_id=ncvdef(fileid,'reaction_handling_spac
     &ing',4,3,nc_dims,nc_stat)
      nc_dims(1)=rd_eval_string_id
      nc_dims(2)=rd_dep_var_ind_id
      nc_dims(3)=problem_reaction_ind_id
      reaction_handling_eval_name_id=ncvdef(fileid,'reaction_handling_ev
     &al_name',2,3,nc_dims,nc_stat)
      nc_dims(1)=rd_rank_ind_id
      nc_dims(2)=rd_dep_var_ind_id
      nc_dims(3)=problem_reaction_ind_id
      reaction_handling_tab_index_id=ncvdef(fileid,'reaction_handling_ta
     &b_index',4,3,nc_dims,nc_stat)
      nc_dims(1)=rd_dep_var_ind_id
      nc_dims(2)=problem_reaction_ind_id
      reaction_handling_var0_id=ncvdef(fileid,'reaction_handling_var0',4
     &,2,nc_dims,nc_stat)
      nc_dims(1)=rd_rank_ind_id
      nc_dims(2)=rd_dep_var_ind_id
      nc_dims(3)=problem_reaction_ind_id
      reaction_handling_var_id=ncvdef(fileid,'reaction_handling_var',4,3
     &,nc_dims,nc_stat)
      nc_dims(1)=problem_reaction_ind_id
      reaction_handling_num_rand_id=ncvdef(fileid,'reaction_handling_num
     &_rand',4,1,nc_dims,nc_stat)
      nc_dims(1)=rd_dep_var_ind_id
      nc_dims(2)=problem_reaction_ind_id
      reaction_handling_base_id=ncvdef(fileid,'reaction_handling_base',4
     &,2,nc_dims,nc_stat)
      nc_dims(1)=rd_handling_table_ind_id
      reaction_handling_tab_id=ncvdef(fileid,'reaction_handling_tab',6,1
     &,nc_dims,nc_stat)
      
      pd_rank_ind_id=ncddef(fileid,'pd_rank_ind',((5)-(1)+1),nc_stat)
      pd_dep_var_ind_id=ncddef(fileid,'pd_dep_var_ind',((4)-(1)+1),nc_st
     &at)
      pd_rank_ind0_id=ncddef(fileid,'pd_rank_ind0',((5)-(0)+1),nc_stat)
      pd_tag_string_id=ncddef(fileid,'pd_tag_string',((20)-(1)+1),nc_sta
     &t)
      pd_eval_string_id=ncddef(fileid,'pd_eval_string',((40)-(1)+1),nc_s
     &tat)
      pmi_yield_size_id=ncvdef(fileid,'pmi_yield_size',4,0,nc_dims,nc_st
     &at)
      pd_yield_ind_id=ncddef(fileid,'pd_yield_ind',((pmi_yield_size-1)-(
     &0)+1),nc_stat)
      pmi_handling_size_id=ncvdef(fileid,'pmi_handling_size',4,0,nc_dims
     &,nc_stat)
      pd_handling_ind_id=ncddef(fileid,'pd_handling_ind',((pmi_handling_
     &size-1)-(0)+1),nc_stat)
      nc_dims(1)=pd_rank_ind_id
      nc_dims(2)=problem_pmi_sub_ind_id
      pmi_yield_min_id=ncvdef(fileid,'pmi_yield_min',6,2,nc_dims,nc_stat
     &)
      nc_dims(1)=pd_rank_ind_id
      nc_dims(2)=problem_pmi_sub_ind_id
      pmi_yield_delta_id=ncvdef(fileid,'pmi_yield_delta',6,2,nc_dims,nc_
     &stat)
      nc_dims(1)=problem_pmi_sub_ind_id
      pmi_yield_rank_id=ncvdef(fileid,'pmi_yield_rank',4,1,nc_dims,nc_st
     &at)
      nc_dims(1)=pd_rank_ind0_id
      nc_dims(2)=problem_pmi_sub_ind_id
      pmi_yield_spacing_id=ncvdef(fileid,'pmi_yield_spacing',4,2,nc_dims
     &,nc_stat)
      nc_dims(1)=pd_eval_string_id
      nc_dims(2)=problem_pmi_sub_ind_id
      pmi_yield_eval_name_id=ncvdef(fileid,'pmi_yield_eval_name',2,2,nc_
     &dims,nc_stat)
      nc_dims(1)=pd_rank_ind_id
      nc_dims(2)=problem_pmi_sub_ind_id
      pmi_yield_tab_index_id=ncvdef(fileid,'pmi_yield_tab_index',4,2,nc_
     &dims,nc_stat)
      nc_dims(1)=pd_rank_ind_id
      nc_dims(2)=problem_pmi_sub_ind_id
      pmi_yield_var_id=ncvdef(fileid,'pmi_yield_var',4,2,nc_dims,nc_stat
     &)
      nc_dims(1)=problem_pmi_sub_ind_id
      pmi_yield_num_rand_id=ncvdef(fileid,'pmi_yield_num_rand',4,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=problem_pmi_sub_ind_id
      pmi_yield_base_id=ncvdef(fileid,'pmi_yield_base',4,1,nc_dims,nc_st
     &at)
      nc_dims(1)=pd_yield_ind_id
      pmi_yield_tab_id=ncvdef(fileid,'pmi_yield_tab',6,1,nc_dims,nc_stat
     &)
      nc_dims(1)=pd_rank_ind_id
      nc_dims(2)=pd_dep_var_ind_id
      nc_dims(3)=problem_pmi_sub_ind_id
      pmi_handling_min_id=ncvdef(fileid,'pmi_handling_min',6,3,nc_dims,n
     &c_stat)
      nc_dims(1)=pd_rank_ind_id
      nc_dims(2)=pd_dep_var_ind_id
      nc_dims(3)=problem_pmi_sub_ind_id
      pmi_handling_delta_id=ncvdef(fileid,'pmi_handling_delta',6,3,nc_di
     &ms,nc_stat)
      nc_dims(1)=pd_dep_var_ind_id
      nc_dims(2)=problem_pmi_sub_ind_id
      pmi_handling_rank_id=ncvdef(fileid,'pmi_handling_rank',4,2,nc_dims
     &,nc_stat)
      nc_dims(1)=pd_rank_ind0_id
      nc_dims(2)=pd_dep_var_ind_id
      nc_dims(3)=problem_pmi_sub_ind_id
      pmi_handling_spacing_id=ncvdef(fileid,'pmi_handling_spacing',4,3,n
     &c_dims,nc_stat)
      nc_dims(1)=pd_eval_string_id
      nc_dims(2)=pd_dep_var_ind_id
      nc_dims(3)=problem_pmi_sub_ind_id
      pmi_handling_eval_name_id=ncvdef(fileid,'pmi_handling_eval_name',2
     &,3,nc_dims,nc_stat)
      nc_dims(1)=pd_rank_ind_id
      nc_dims(2)=pd_dep_var_ind_id
      nc_dims(3)=problem_pmi_sub_ind_id
      pmi_handling_tab_index_id=ncvdef(fileid,'pmi_handling_tab_index',4
     &,3,nc_dims,nc_stat)
      nc_dims(1)=pd_tag_string_id
      nc_dims(2)=pd_dep_var_ind_id
      nc_dims(3)=problem_pmi_sub_ind_id
      pmi_handling_var0_id=ncvdef(fileid,'pmi_handling_var0',2,3,nc_dims
     &,nc_stat)
      nc_dims(1)=pd_rank_ind_id
      nc_dims(2)=pd_dep_var_ind_id
      nc_dims(3)=problem_pmi_sub_ind_id
      pmi_handling_var_id=ncvdef(fileid,'pmi_handling_var',4,3,nc_dims,n
     &c_stat)
      nc_dims(1)=problem_pmi_sub_ind_id
      pmi_handling_num_rand_id=ncvdef(fileid,'pmi_handling_num_rand',4,1
     &,nc_dims,nc_stat)
      nc_dims(1)=pd_dep_var_ind_id
      nc_dims(2)=problem_pmi_sub_ind_id
      pmi_handling_base_id=ncvdef(fileid,'pmi_handling_base',4,2,nc_dims
     &,nc_stat)
      nc_dims(1)=pd_handling_ind_id
      pmi_handling_tab_id=ncvdef(fileid,'pmi_handling_tab',6,1,nc_dims,n
     &c_stat)
      
      call ncendf(fileid,nc_stat)
      call ncvpt(fileid,pr_test_num_id,nc_corner,nc_edge,pr_test_num,nc_
     &stat)
      call ncvpt(fileid,pr_background_num_id,nc_corner,nc_edge,pr_backgr
     &ound_num,nc_stat)
      call ncvpt(fileid,pr_ex_test_num_id,nc_corner,nc_edge,pr_ex_test_n
     &um,nc_stat)
      call ncvpt(fileid,pr_ex_test_dim_id,nc_corner,nc_edge,pr_ex_test_d
     &im,nc_stat)
      call ncvpt(fileid,pr_reaction_num_id,nc_corner,nc_edge,pr_reaction
     &_num,nc_stat)
      call ncvpt(fileid,pr_reaction_dim_id,nc_corner,nc_edge,pr_reaction
     &_dim,nc_stat)
      call ncvpt(fileid,pr_bkrc_num_id,nc_corner,nc_edge,pr_bkrc_num,nc_
     &stat)
      call ncvpt(fileid,pr_bkrc_dim_id,nc_corner,nc_edge,pr_bkrc_dim,nc_
     &stat)
      call ncvpt(fileid,pr_exrc_num_id,nc_corner,nc_edge,pr_exrc_num,nc_
     &stat)
      call ncvpt(fileid,pr_exrc_dim_id,nc_corner,nc_edge,pr_exrc_dim,nc_
     &stat)
      call ncvpt(fileid,pr_materials_num_id,nc_corner,nc_edge,pr_materia
     &ls_num,nc_stat)
      call ncvpt(fileid,pr_pmi_num_id,nc_corner,nc_edge,pr_pmi_num,nc_st
     &at)
      call ncvpt(fileid,pr_var0_num_id,nc_corner,nc_edge,pr_var0_num,nc_
     &stat)
      nc_corner(1)=1
      nc_edge(1)=((sp_num)-(1)+1)
      call ncvpt(fileid,problem_species_test_id,nc_corner,nc_edge,proble
     &m_species_test,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((sp_num)-(1)+1)
      call ncvpt(fileid,problem_species_background_id,nc_corner,nc_edge,
     &problem_species_background,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((ma_num)-(1)+1)
      call ncvpt(fileid,problem_materials_sub_id,nc_corner,nc_edge,probl
     &em_materials_sub,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_test_num)-(1)+1)
      call ncvpt(fileid,problem_test_sp_id,nc_corner,nc_edge,problem_tes
     &t_sp,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_background_num)-(1)+1)
      call ncvpt(fileid,problem_background_sp_id,nc_corner,nc_edge,probl
     &em_background_sp,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_ex_test_dim)-(1)+1)
      call ncvpt(fileid,problem_ex_test_sp_id,nc_corner,nc_edge,problem_
     &ex_test_sp,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_materials_num)-(1)+1)
      call ncvpt(fileid,problem_materials_ref_id,nc_corner,nc_edge,probl
     &em_materials_ref,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,problem_rc_id,nc_corner,nc_edge,problem_rc,nc_st
     &at)
      nc_corner(1)=1
      nc_edge(1)=((pr_test_num)-(1)+1)
      call ncvpt(fileid,problem_reaction_num_id,nc_corner,nc_edge,proble
     &m_reaction_num,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((15)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_test_num)-(1)+1)
      call ncvpt(fileid,problem_test_reaction_id,nc_corner,nc_edge,probl
     &em_test_reaction,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((15)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_test_num)-(1)+1)
      call ncvpt(fileid,problem_test_background_id,nc_corner,nc_edge,pro
     &blem_test_background,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((15)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_test_num)-(1)+1)
      call ncvpt(fileid,problem_num_arrangements_id,nc_corner,nc_edge,pr
     &oblem_num_arrangements,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((15)-(1)+1)
      nc_corner(4)=1
      nc_edge(4)=((pr_test_num)-(1)+1)
      call ncvpt(fileid,problem_test_products_id,nc_corner,nc_edge,probl
     &em_test_products,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((15)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_test_num)-(1)+1)
      call ncvpt(fileid,problem_prod_mult_id,nc_corner,nc_edge,problem_p
     &rod_mult,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_bkrc_dim)-(1)+1)
      call ncvpt(fileid,problem_background_reaction_id,nc_corner,nc_edge
     &,problem_background_reaction,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((2)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_bkrc_dim)-(1)+1)
      call ncvpt(fileid,problem_bkrc_reagents_id,nc_corner,nc_edge,probl
     &em_bkrc_reagents,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_bkrc_dim)-(1)+1)
      call ncvpt(fileid,problem_bkrc_products_id,nc_corner,nc_edge,probl
     &em_bkrc_products,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_exrc_dim)-(1)+1)
      call ncvpt(fileid,problem_external_reaction_id,nc_corner,nc_edge,p
     &roblem_external_reaction,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_ex_test_dim)-(1)+1)
      call ncvpt(fileid,problem_external_test_reaction_num_id,nc_corner,
     &nc_edge,problem_external_test_reaction_num,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((15)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_ex_test_dim)-(1)+1)
      call ncvpt(fileid,problem_external_test_reaction_id,nc_corner,nc_e
     &dge,problem_external_test_reaction,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((15)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_ex_test_dim)-(1)+1)
      call ncvpt(fileid,problem_external_test_background_id,nc_corner,nc
     &_edge,problem_external_test_background,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((15)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_ex_test_dim)-(1)+1)
      call ncvpt(fileid,problem_exrc_products_id,nc_corner,nc_edge,probl
     &em_exrc_products,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,problem_pmi_ref_id,nc_corner,nc_edge,problem_pmi
     &_ref,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pm_num)-(1)+1)
      call ncvpt(fileid,problem_pmi_sub_id,nc_corner,nc_edge,problem_pmi
     &_sub,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_test_num)-(1)+1)
      call ncvpt(fileid,problem_pmi_case_num_id,nc_corner,nc_edge,proble
     &m_pmi_case_num,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((8)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_test_num)-(1)+1)
      call ncvpt(fileid,problem_pmi_cases_id,nc_corner,nc_edge,problem_p
     &mi_cases,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((8)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_test_num)-(1)+1)
      call ncvpt(fileid,problem_pmi_num_arrange_id,nc_corner,nc_edge,pro
     &blem_pmi_num_arrange,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((8)-(1)+1)
      nc_corner(4)=1
      nc_edge(4)=((pr_test_num)-(1)+1)
      call ncvpt(fileid,problem_pmi_products_id,nc_corner,nc_edge,proble
     &m_pmi_products,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((8)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_test_num)-(1)+1)
      call ncvpt(fileid,problem_pmi_prod_mult_id,nc_corner,nc_edge,probl
     &em_pmi_prod_mult,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_var0_num)-(1)+1)
      call ncvptc(fileid,pr_var0_list_id,nc_corner,nc_edge,pr_var0_list,
     &(((40)-(1)+1)*((pr_var0_num)-(1)+1)),nc_stat)
      
      call ncvpt(fileid,reaction_rate_size_id,nc_corner,nc_edge,reaction
     &_rate_size,nc_stat)
      call ncvpt(fileid,reaction_handling_size_id,nc_corner,nc_edge,reac
     &tion_handling_size,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_rate_min_id,nc_corner,nc_edge,reaction_
     &rate_min,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_rate_delta_id,nc_corner,nc_edge,reactio
     &n_rate_delta,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_rate_rank_id,nc_corner,nc_edge,reaction
     &_rate_rank,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_rate_spacing_id,nc_corner,nc_edge,react
     &ion_rate_spacing,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvptc(fileid,reaction_rate_eval_name_id,nc_corner,nc_edge,re
     &action_rate_eval_name,(((40)-(1)+1)*((pr_reaction_dim)-(1)+1)),nc_
     &stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_rate_tab_index_id,nc_corner,nc_edge,rea
     &ction_rate_tab_index,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_rate_var_id,nc_corner,nc_edge,reaction_
     &rate_var,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_rate_num_rand_id,nc_corner,nc_edge,reac
     &tion_rate_num_rand,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_rate_base_id,nc_corner,nc_edge,reaction
     &_rate_base,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((reaction_rate_size-1)-(0)+1)
      call ncvpt(fileid,reaction_rate_tab_id,nc_corner,nc_edge,reaction_
     &rate_tab,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((19)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_handling_min_id,nc_corner,nc_edge,react
     &ion_handling_min,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((19)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_handling_delta_id,nc_corner,nc_edge,rea
     &ction_handling_delta,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((19)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_handling_rank_id,nc_corner,nc_edge,reac
     &tion_handling_rank,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((19)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_handling_spacing_id,nc_corner,nc_edge,r
     &eaction_handling_spacing,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((19)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_reaction_dim)-(1)+1)
      call ncvptc(fileid,reaction_handling_eval_name_id,nc_corner,nc_edg
     &e,reaction_handling_eval_name,(((40)-(1)+1)*((19)-(1)+1)*((pr_reac
     &tion_dim)-(1)+1)),nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((19)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_handling_tab_index_id,nc_corner,nc_edge
     &,reaction_handling_tab_index,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((19)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_handling_var0_id,nc_corner,nc_edge,reac
     &tion_handling_var0,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((19)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_handling_var_id,nc_corner,nc_edge,react
     &ion_handling_var,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_handling_num_rand_id,nc_corner,nc_edge,
     &reaction_handling_num_rand,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((19)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_reaction_dim)-(1)+1)
      call ncvpt(fileid,reaction_handling_base_id,nc_corner,nc_edge,reac
     &tion_handling_base,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((reaction_handling_size-1)-(0)+1)
      call ncvpt(fileid,reaction_handling_tab_id,nc_corner,nc_edge,react
     &ion_handling_tab,nc_stat)
      
      call ncvpt(fileid,pmi_yield_size_id,nc_corner,nc_edge,pmi_yield_si
     &ze,nc_stat)
      call ncvpt(fileid,pmi_handling_size_id,nc_corner,nc_edge,pmi_handl
     &ing_size,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,pmi_yield_min_id,nc_corner,nc_edge,pmi_yield_min
     &,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,pmi_yield_delta_id,nc_corner,nc_edge,pmi_yield_d
     &elta,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,pmi_yield_rank_id,nc_corner,nc_edge,pmi_yield_ra
     &nk,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((5)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,pmi_yield_spacing_id,nc_corner,nc_edge,pmi_yield
     &_spacing,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_pmi_num)-(1)+1)
      call ncvptc(fileid,pmi_yield_eval_name_id,nc_corner,nc_edge,pmi_yi
     &eld_eval_name,(((40)-(1)+1)*((pr_pmi_num)-(1)+1)),nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,pmi_yield_tab_index_id,nc_corner,nc_edge,pmi_yie
     &ld_tab_index,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,pmi_yield_var_id,nc_corner,nc_edge,pmi_yield_var
     &,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,pmi_yield_num_rand_id,nc_corner,nc_edge,pmi_yiel
     &d_num_rand,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,pmi_yield_base_id,nc_corner,nc_edge,pmi_yield_ba
     &se,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pmi_yield_size-1)-(0)+1)
      call ncvpt(fileid,pmi_yield_tab_id,nc_corner,nc_edge,pmi_yield_tab
     &,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,pmi_handling_min_id,nc_corner,nc_edge,pmi_handli
     &ng_min,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,pmi_handling_delta_id,nc_corner,nc_edge,pmi_hand
     &ling_delta,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,pmi_handling_rank_id,nc_corner,nc_edge,pmi_handl
     &ing_rank,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((5)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,pmi_handling_spacing_id,nc_corner,nc_edge,pmi_ha
     &ndling_spacing,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_pmi_num)-(1)+1)
      call ncvptc(fileid,pmi_handling_eval_name_id,nc_corner,nc_edge,pmi
     &_handling_eval_name,(((40)-(1)+1)*((4)-(1)+1)*((pr_pmi_num)-(1)+1)
     &),nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,pmi_handling_tab_index_id,nc_corner,nc_edge,pmi_
     &handling_tab_index,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((20)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_pmi_num)-(1)+1)
      call ncvptc(fileid,pmi_handling_var0_id,nc_corner,nc_edge,pmi_hand
     &ling_var0,(((20)-(1)+1)*((4)-(1)+1)*((pr_pmi_num)-(1)+1)),nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((5)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,pmi_handling_var_id,nc_corner,nc_edge,pmi_handli
     &ng_var,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,pmi_handling_num_rand_id,nc_corner,nc_edge,pmi_h
     &andling_num_rand,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((pr_pmi_num)-(1)+1)
      call ncvpt(fileid,pmi_handling_base_id,nc_corner,nc_edge,pmi_handl
     &ing_base,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((pmi_handling_size-1)-(0)+1)
      call ncvpt(fileid,pmi_handling_tab_id,nc_corner,nc_edge,pmi_handli
     &ng_tab,nc_stat)
      
      call ncclos(fileid,nc_stat)
      return
      end
      
      
