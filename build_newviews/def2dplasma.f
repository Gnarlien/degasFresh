      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine get_n_t(nt_string)
      
      
      
      
      use bk_mod
      
      use zn_mod
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*96 nt_string
      integer p,b,e,length,count,line_num,sub_count,open_stat,diskin3,di
     &m_y,iy,symmetric_plasma
      integer format_code(300)
      character*300 line,plasma_format
      character*96 plasma_file
      integer zone
      integer iy_zone
      integer back
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
      n_iy =>mem_alloc_i1((1),(zn_num),'n_iy')
      dim_y=1
      do zone=1,zn_num
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
      if(read_string(diskin3,line,length))continue
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      plasma_format=line(:length)
      call parse_format(plasma_format,bk_num,format_code,count)
      if(count.GT.0)continue
      line_num=0
90013 continue
      if(.NOT.read_string(diskin3,line,length))goto 90004
      line_num=line_num+1
      zone=line_num
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      sub_count=0
90014 continue
      sub_count=sub_count+1
      if(sub_count.GT.count)goto 90013
      if(next_token(line,b,e,p))continue
      if(format_code(sub_count).EQ.10)then
      zone=read_integer(line(b:e))
      if(check_zone(zone))continue
      if((zone_index(4,zone).EQ.zone).OR.(symmetric_plasma.EQ.0))continu
     &e
      if(zone_type(zone).EQ.2)continue
      else if(format_code(sub_count).EQ.11)then
      sub_count=sub_count+1
      back=format_code(sub_count)
      if((back.GT.0.AND.back.LE.bk_num))continue
      if(check_zone(zone))continue
      if((zone_index(4,zone).EQ.zone).OR.(symmetric_plasma.EQ.0))continu
     &e
      if(zone_type(zone).EQ.2)continue
      background_n(back,zone_pointer(zone))=read_real(line(b:e))
      if(background_n(back,zone_pointer(zone)).GE.(0.0_DOUBLE))continue
      if((dim_y.GT.1).AND.(symmetric_plasma.EQ.1))then
      do iy=1,dim_y
      iy_zone=zone_iy_map((zone-1)*dim_y+iy)
      if((check_zone(iy_zone)).AND.(zone_type(iy_zone).EQ.2))then
      background_n(back,zone_pointer(iy_zone))=background_n(back,zone_po
     &inter(zone))
      end if
      end do
      end if
      else if(format_code(sub_count).EQ.12)then
      sub_count=sub_count+1
      back=format_code(sub_count)
      if((back.GT.0.AND.back.LE.bk_num))continue
      if(check_zone(zone))continue
      if((zone_index(4,zone).EQ.zone).OR.(symmetric_plasma.EQ.0))continu
     &e
      if(zone_type(zone).EQ.2)continue
      background_temp(back,zone_pointer(zone))=read_real(line(b:e))*(1.6
     &0217733e-19_DOUBLE)
      if(background_temp(back,zone_pointer(zone)).GE.(0.0_DOUBLE))contin
     &ue
      if((dim_y.GT.1).AND.(symmetric_plasma.EQ.1))then
      do iy=1,dim_y
      iy_zone=zone_iy_map((zone-1)*dim_y+iy)
      if((check_zone(iy_zone)).AND.(zone_type(iy_zone).EQ.2))then
      background_temp(back,zone_pointer(iy_zone))=background_temp(back,z
     &one_pointer(zone))
      end if
      end do
      end if
      else if(format_code(sub_count).EQ.13)then
      sub_count=sub_count+1
      back=format_code(sub_count)
      if((back.GT.0.AND.back.LE.bk_num))continue
      if(check_zone(zone))continue
      if((zone_index(4,zone).EQ.zone).OR.(symmetric_plasma.EQ.0))continu
     &e
      if(zone_type(zone).EQ.2)continue
      background_v(1,back,zone_pointer(zone))=read_real(line(b:e))
      if((dim_y.GT.1).AND.(symmetric_plasma.EQ.1))then
      do iy=1,dim_y
      iy_zone=zone_iy_map((zone-1)*dim_y+iy)
      if((check_zone(iy_zone)).AND.(zone_type(iy_zone).EQ.2))then
      background_v(1,back,zone_pointer(iy_zone))=background_v(1,back,zon
     &e_pointer(zone))
      end if
      end do
      end if
      else if(format_code(sub_count).EQ.14)then
      sub_count=sub_count+1
      back=format_code(sub_count)
      if((back.GT.0.AND.back.LE.bk_num))continue
      if(check_zone(zone))continue
      if((zone_index(4,zone).EQ.zone).OR.(symmetric_plasma.EQ.0))continu
     &e
      if(zone_type(zone).EQ.2)continue
      background_v(2,back,zone_pointer(zone))=read_real(line(b:e))
      if((dim_y.GT.1).AND.(symmetric_plasma.EQ.1))then
      do iy=1,dim_y
      iy_zone=zone_iy_map((zone-1)*dim_y+iy)
      if((check_zone(iy_zone)).AND.(zone_type(iy_zone).EQ.2))then
      background_v(2,back,zone_pointer(iy_zone))=background_v(2,back,zon
     &e_pointer(zone))
      end if
      end do
      end if
      else if(format_code(sub_count).EQ.15)then
      sub_count=sub_count+1
      back=format_code(sub_count)
      if((back.GT.0.AND.back.LE.bk_num))continue
      if(check_zone(zone))continue
      if((zone_index(4,zone).EQ.zone).OR.(symmetric_plasma.EQ.0))continu
     &e
      if(zone_type(zone).EQ.2)continue
      background_v(3,back,zone_pointer(zone))=read_real(line(b:e))
      if((dim_y.GT.1).AND.(symmetric_plasma.EQ.1))then
      do iy=1,dim_y
      iy_zone=zone_iy_map((zone-1)*dim_y+iy)
      if((check_zone(iy_zone)).AND.(zone_type(iy_zone).EQ.2))then
      background_v(3,back,zone_pointer(iy_zone))=background_v(3,back,zon
     &e_pointer(zone))
      end if
      end do
      end if
      end if
      goto 90014
90004 continue
      close(unit=diskin3)
      return
      end
      
      
