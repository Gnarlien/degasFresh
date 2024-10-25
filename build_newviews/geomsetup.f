      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine init_geometry
      
      use gi_mod
      
      use zn_mod
      
      use sc_mod
      
      use de_mod
      
      use al_mod
      
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
      
      
      logical check_mem
      integer alloc_mem,realloc_mem
      external check_mem,init_mem,alloc_mem,realloc_mem,free_mem,end_mem
      integer i,j
      ncells=0
      if(mod(((ncells)-(0)+1),100).EQ.1)then
      cells =>mem_realloc_i2(cells,(1),(4),(0),(((int(((((ncells)-(0)+1)
     &)+100-1)/100)*100)-100)+(0)-1),((int(((((ncells)-(0)+1))+100-1)/10
     &0)*100)+(0)-1),'cells')
      end if
      cells(1,0)=0
      cells(2,0)=0
      cells(3,0)=0
      cells(4,0)=0
      nsurfaces=0
      nsectors=0
      ntransforms=0
      if(mod(((nsurfaces)-(1)+1),100).EQ.1)then
      surface_sectors =>mem_realloc_i3(surface_sectors,(0),(1),(0),(1),(
     &1),(((int(((((nsurfaces)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((in
     &t(((((nsurfaces)-(1)+1))+100-1)/100)*100)+(1)-1),'surface_sectors'
     &)
      end if
      if(mod(((ntransforms)-(0)+1),100).EQ.1)then
      surfaces_tx_mx =>mem_realloc_r3(surfaces_tx_mx,(1),(3),(1),(4),(0)
     &,(((int(((((ntransforms)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((in
     &t(((((ntransforms)-(0)+1))+100-1)/100)*100)+(0)-1),'surfaces_tx_mx
     &')
      end if
      do j=1,4
      do i=1,3
      surfaces_tx_mx(i,j,ntransforms)=(2.0e30_DOUBLE)
      end do
      end do
      if(mod(((nsectors)-(0)+1),100).EQ.1)then
      strata =>mem_realloc_i1(strata,(0),(((int(((((nsectors)-(0)+1))+10
     &0-1)/100)*100)-100)+(0)-1),((int(((((nsectors)-(0)+1))+100-1)/100)
     &*100)+(0)-1),'strata')
      end if
      if(mod(((nsectors)-(0)+1),100).EQ.1)then
      sector_strata_segment =>mem_realloc_i1(sector_strata_segment,(0),(
     &((int(((((nsectors)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int((((
     &(nsectors)-(0)+1))+100-1)/100)*100)+(0)-1),'sector_strata_segment'
     &)
      end if
      if(mod(((nsectors)-(0)+1),100).EQ.1)then
      sector_zone =>mem_realloc_i1(sector_zone,(0),(((int(((((nsectors)-
     &(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((nsectors)-(0)+1))+1
     &00-1)/100)*100)+(0)-1),'sector_zone')
      end if
      if(mod(((nsectors)-(0)+1),100).EQ.1)then
      sector_surface =>mem_realloc_i1(sector_surface,(0),(((int(((((nsec
     &tors)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((nsectors)-(0)
     &+1))+100-1)/100)*100)+(0)-1),'sector_surface')
      end if
      if(mod(((nsectors)-(0)+1),100).EQ.1)then
      sector_points =>mem_realloc_r3(sector_points,(1),(3),(0),(1),(0),(
     &((int(((((nsectors)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int((((
     &(nsectors)-(0)+1))+100-1)/100)*100)+(0)-1),'sector_points')
      end if
      if(mod(((nsectors)-(0)+1),100).EQ.1)then
      sector_type_pointer =>mem_realloc_i2(sector_type_pointer,(1),(17),
     &(0),(((int(((((nsectors)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((in
     &t(((((nsectors)-(0)+1))+100-1)/100)*100)+(0)-1),'sector_type_point
     &er')
      end if
      strata(nsectors)=2000000000
      sector_strata_segment(nsectors)=2000000000
      sector_zone(nsectors)=2000000000
      sector_surface(nsectors)=2000000000
      sector_points(1,0,nsectors)=(2.0e30_DOUBLE)
      sector_points(2,0,nsectors)=(2.0e30_DOUBLE)
      sector_points(3,0,nsectors)=(2.0e30_DOUBLE)
      sector_points(1,1,nsectors)=(2.0e30_DOUBLE)
      sector_points(2,1,nsectors)=(2.0e30_DOUBLE)
      sector_points(3,1,nsectors)=(2.0e30_DOUBLE)
      do i=1,17
      sector_type_pointer(i,nsectors)=1000000000
      end do
      sc_vacuum_num=0
      if(mod(((sc_vacuum_num)-(0)+1),100).EQ.1)then
      vacuum_sector =>mem_realloc_i1(vacuum_sector,(0),(((int(((((sc_vac
     &uum_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_vacuum_
     &num)-(0)+1))+100-1)/100)*100)+(0)-1),'vacuum_sector')
      end if
      vacuum_sector(sc_vacuum_num)=2000000000
      sc_plasma_num=0
      if(mod(((sc_plasma_num)-(0)+1),100).EQ.1)then
      plasma_sector =>mem_realloc_i1(plasma_sector,(0),(((int(((((sc_pla
     &sma_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_plasma_
     &num)-(0)+1))+100-1)/100)*100)+(0)-1),'plasma_sector')
      end if
      plasma_sector(sc_plasma_num)=2000000000
      sc_target_num=0
      if(mod(((sc_target_num)-(0)+1),100).EQ.1)then
      target_sector =>mem_realloc_i1(target_sector,(0),(((int(((((sc_tar
     &get_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_target_
     &num)-(0)+1))+100-1)/100)*100)+(0)-1),'target_sector')
      end if
      target_sector(sc_target_num)=2000000000
      if(mod(((sc_target_num)-(0)+1),100).EQ.1)then
      target_material =>mem_realloc_i1(target_material,(0),(((int(((((sc
     &_target_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_tar
     &get_num)-(0)+1))+100-1)/100)*100)+(0)-1),'target_material')
      end if
      target_material(sc_target_num)=2000000000
      if(mod(((sc_target_num)-(0)+1),100).EQ.1)then
      target_temperature =>mem_realloc_r1(target_temperature,(0),(((int(
     &((((sc_target_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((
     &sc_target_num)-(0)+1))+100-1)/100)*100)+(0)-1),'target_temperature
     &')
      end if
      target_temperature(sc_target_num)=(2.0e30_DOUBLE)
      if(mod(((sc_target_num)-(0)+1),100).EQ.1)then
      target_recyc_coef =>mem_realloc_r1(target_recyc_coef,(0),(((int(((
     &((sc_target_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc
     &_target_num)-(0)+1))+100-1)/100)*100)+(0)-1),'target_recyc_coef')
      end if
      target_recyc_coef(sc_target_num)=(2.0e30_DOUBLE)
      sc_wall_num=0
      if(mod(((sc_wall_num)-(0)+1),100).EQ.1)then
      wall_sector =>mem_realloc_i1(wall_sector,(0),(((int(((((sc_wall_nu
     &m)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_wall_num)-(0)
     &+1))+100-1)/100)*100)+(0)-1),'wall_sector')
      end if
      wall_sector(sc_wall_num)=2000000000
      if(mod(((sc_wall_num)-(0)+1),100).EQ.1)then
      wall_material =>mem_realloc_i1(wall_material,(0),(((int(((((sc_wal
     &l_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_wall_num)
     &-(0)+1))+100-1)/100)*100)+(0)-1),'wall_material')
      end if
      wall_material(sc_wall_num)=2000000000
      if(mod(((sc_wall_num)-(0)+1),100).EQ.1)then
      wall_temperature =>mem_realloc_r1(wall_temperature,(0),(((int(((((
     &sc_wall_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_wal
     &l_num)-(0)+1))+100-1)/100)*100)+(0)-1),'wall_temperature')
      end if
      wall_temperature(sc_wall_num)=(2.0e30_DOUBLE)
      if(mod(((sc_wall_num)-(0)+1),100).EQ.1)then
      wall_recyc_coef =>mem_realloc_r1(wall_recyc_coef,(0),(((int(((((sc
     &_wall_num)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_wall_
     &num)-(0)+1))+100-1)/100)*100)+(0)-1),'wall_recyc_coef')
      end if
      wall_recyc_coef(sc_wall_num)=(2.0e30_DOUBLE)
      sc_exit_num=0
      if(mod(((sc_exit_num)-(0)+1),100).EQ.1)then
      exit_sector =>mem_realloc_i1(exit_sector,(0),(((int(((((sc_exit_nu
     &m)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_exit_num)-(0)
     &+1))+100-1)/100)*100)+(0)-1),'exit_sector')
      end if
      exit_sector(sc_exit_num)=2000000000
      sc_diagnostic_grps=0
      if(mod(((sc_diagnostic_grps)-(0)+1),100).EQ.1)then
      diagnostic_grp_name =>mem_realloc_c1(diagnostic_grp_name,(40),(0),
     &(((int(((((sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)-100)+(0)-1
     &),((int(((((sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)+(0)-1),'d
     &iagnostic_grp_name')
      end if
      diagnostic_grp_name(sc_diagnostic_grps)='UNUSED'
      if(mod(((sc_diagnostic_grps)-(0)+1),100).EQ.1)then
      diagnostic_num_sectors =>mem_realloc_i1(diagnostic_num_sectors,(0)
     &,(((int(((((sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)-100)+(0)-
     &1),((int(((((sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)+(0)-1),'
     &diagnostic_num_sectors')
      end if
      diagnostic_num_sectors(sc_diagnostic_grps)=2000000000
      if(mod(((sc_diagnostic_grps)-(0)+1),100).EQ.1)then
      diagnostic_var =>mem_realloc_i1(diagnostic_var,(0),(((int(((((sc_d
     &iagnostic_grps)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_
     &diagnostic_grps)-(0)+1))+100-1)/100)*100)+(0)-1),'diagnostic_var')
      end if
      diagnostic_var(sc_diagnostic_grps)=2000000000
      if(mod(((sc_diagnostic_grps)-(0)+1),100).EQ.1)then
      diagnostic_tab_index =>mem_realloc_i1(diagnostic_tab_index,(0),(((
     &int(((((sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)-100)+(0)-1),(
     &(int(((((sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)+(0)-1),'diag
     &nostic_tab_index')
      end if
      diagnostic_tab_index(sc_diagnostic_grps)=2000000000
      if(mod(((sc_diagnostic_grps)-(0)+1),100).EQ.1)then
      diagnostic_min =>mem_realloc_r1(diagnostic_min,(0),(((int(((((sc_d
     &iagnostic_grps)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((sc_
     &diagnostic_grps)-(0)+1))+100-1)/100)*100)+(0)-1),'diagnostic_min')
      end if
      diagnostic_min(sc_diagnostic_grps)=(2.0e30_DOUBLE)
      if(mod(((sc_diagnostic_grps)-(0)+1),100).EQ.1)then
      diagnostic_delta =>mem_realloc_r1(diagnostic_delta,(0),(((int(((((
     &sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int((((
     &(sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)+(0)-1),'diagnostic_d
     &elta')
      end if
      diagnostic_delta(sc_diagnostic_grps)=(2.0e30_DOUBLE)
      if(mod(((sc_diagnostic_grps)-(0)+1),100).EQ.1)then
      diagnostic_spacing =>mem_realloc_i1(diagnostic_spacing,(0),(((int(
     &((((sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int
     &(((((sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)+(0)-1),'diagnost
     &ic_spacing')
      end if
      diagnostic_spacing(sc_diagnostic_grps)=2000000000
      if(mod(((sc_diagnostic_grps)-(0)+1),100).EQ.1)then
      diagnostic_grp_base =>mem_realloc_i1(diagnostic_grp_base,(0),(((in
     &t(((((sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((i
     &nt(((((sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)+(0)-1),'diagno
     &stic_grp_base')
      end if
      diagnostic_grp_base(sc_diagnostic_grps)=2000000000
      de_grps=0
      if(mod(((de_grps)-(0)+1),100).EQ.1)then
      detector_name =>mem_realloc_c1(detector_name,(100),(0),(((int(((((
     &de_grps)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((de_grps)-(
     &0)+1))+100-1)/100)*100)+(0)-1),'detector_name')
      end if
      detector_name(de_grps)='UNUSED'
      if(mod(((de_grps)-(0)+1),100).EQ.1)then
      detector_num_views =>mem_realloc_i1(detector_num_views,(0),(((int(
     &((((de_grps)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((de_grp
     &s)-(0)+1))+100-1)/100)*100)+(0)-1),'detector_num_views')
      end if
      detector_num_views(de_grps)=2000000000
      if(mod(((de_grps)-(0)+1),100).EQ.1)then
      detector_var =>mem_realloc_i1(detector_var,(0),(((int(((((de_grps)
     &-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((de_grps)-(0)+1))+1
     &00-1)/100)*100)+(0)-1),'detector_var')
      end if
      detector_var(de_grps)=2000000000
      if(mod(((de_grps)-(0)+1),100).EQ.1)then
      detector_tab_index =>mem_realloc_i1(detector_tab_index,(0),(((int(
     &((((de_grps)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((de_grp
     &s)-(0)+1))+100-1)/100)*100)+(0)-1),'detector_tab_index')
      end if
      detector_tab_index(de_grps)=2000000000
      if(mod(((de_grps)-(0)+1),100).EQ.1)then
      detector_min =>mem_realloc_r1(detector_min,(0),(((int(((((de_grps)
     &-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((de_grps)-(0)+1))+1
     &00-1)/100)*100)+(0)-1),'detector_min')
      end if
      detector_min(de_grps)=(2.0e30_DOUBLE)
      if(mod(((de_grps)-(0)+1),100).EQ.1)then
      detector_delta =>mem_realloc_r1(detector_delta,(0),(((int(((((de_g
     &rps)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((de_grps)-(0)+1
     &))+100-1)/100)*100)+(0)-1),'detector_delta')
      end if
      detector_delta(de_grps)=(2.0e30_DOUBLE)
      if(mod(((de_grps)-(0)+1),100).EQ.1)then
      detector_spacing =>mem_realloc_i1(detector_spacing,(0),(((int(((((
     &de_grps)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((de_grps)-(
     &0)+1))+100-1)/100)*100)+(0)-1),'detector_spacing')
      end if
      detector_spacing(de_grps)=2000000000
      if(mod(((de_grps)-(0)+1),100).EQ.1)then
      de_view_base =>mem_realloc_i1(de_view_base,(0),(((int(((((de_grps)
     &-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((de_grps)-(0)+1))+1
     &00-1)/100)*100)+(0)-1),'de_view_base')
      end if
      de_view_base(de_grps)=2000000000
      zn_num=0
      
      zone_type_num(1)=0
      
      
      zone_type_num(2)=0
      
      
      zone_type_num(3)=0
      
      
      zone_type_num(4)=0
      
      
      call init_mem
      return
      end
      subroutine universal_cell(symmetry,min_corner,max_corner,vol)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer symmetry
      REAL(kind=DOUBLE)min_corner(3),max_corner(3)
      REAL(kind=DOUBLE)vol
      integer face,face1,face2,surf_type,i
      REAL(kind=DOUBLE)y,cos_y,sin_y
      REAL(kind=DOUBLE)x(3,0:4),apex(3),a(3),b(3),x0(3),x1(3),x2(3),x3(3
     &),coeff(10),tx(3,4)
      external define_surface,define_surface_a,start_cell,polygon_volume
     &,define_sector,lookup_surface
      integer define_surface,define_surface_a,start_cell,define_sector,l
     &ookup_surface
      REAL(kind=DOUBLE)polygon_volume
      
      external vector_compare
      integer vector_compare
      geometry_symmetry=symmetry
      if(symmetry.EQ.0)return
      if(symmetry.EQ.1.OR.symmetry.EQ.3.OR.symmetry.EQ.2.OR.symmetry.EQ.
     &4.OR.symmetry.EQ.5.OR.symmetry.EQ.6)continue
      
      if(min_corner(1).LT.max_corner(1))continue
      
      
      if(min_corner(2).LT.max_corner(2))continue
      
      
      if(min_corner(3).LT.max_corner(3))continue
      
      
      universal_cell_min(1)=min_corner(1)
      universal_cell_min(2)=min_corner(2)
      universal_cell_min(3)=min_corner(3)
      
      universal_cell_max(1)=max_corner(1)
      universal_cell_max(2)=max_corner(2)
      universal_cell_max(3)=max_corner(3)
      
      i=0
      x(1,i)=min_corner(1)
      x(2,i)=(0.0_DOUBLE)
      x(3,i)=min_corner(3)
      i=i+1
      x(1,i)=min_corner(1)
      x(2,i)=(0.0_DOUBLE)
      x(3,i)=max_corner(3)
      i=i+1
      x(1,i)=max_corner(1)
      x(2,i)=(0.0_DOUBLE)
      x(3,i)=max_corner(3)
      i=i+1
      x(1,i)=max_corner(1)
      x(2,i)=(0.0_DOUBLE)
      x(3,i)=min_corner(3)
      i=i+1
      x(1,i)=x(1,0)
      x(2,i)=x(2,0)
      x(3,i)=x(3,0)
      
      if(symmetry.EQ.2.OR.symmetry.EQ.5.OR.symmetry.EQ.6)then
      if(symmetry.EQ.2.OR.symmetry.EQ.5)then
      if(min_corner(2).EQ.(0.0_DOUBLE).AND.(abs(max_corner(2)-(2.0_DOUBL
     &E)*atan2((0.0_DOUBLE),-(1.0_DOUBLE))).LT.(1.0e-8_DOUBLE)))continue
      else
      do i=1,2
      if(i.EQ.1)then
      y=min_corner(2)
      else
      y=max_corner(2)
      end if
      cos_y=cos(y)
      sin_y=sin(y)
      a(1)=-sin_y
      a(2)=cos_y
      a(3)=(0.0_DOUBLE)
      b(1)=max_corner(1)*cos_y
      b(2)=max_corner(1)*sin_y
      b(3)=max_corner(3)
      call plane(b,a,coeff)
      face=define_surface(coeff,.TRUE.)
      call add_surface(face,0,.TRUE.)
      end do
      end if
      do i=0,3
      call conea(x(1,i),x(1,i+1),coeff,surf_type,apex)
      face=define_surface(coeff,.TRUE.)
      if(face.GT.0)then
      surface_points(1,0,face)=x(1,i)
      surface_points(2,0,face)=x(2,i)
      surface_points(3,0,face)=x(3,i)
      
      surface_points(1,1,face)=x(1,i+1)
      surface_points(2,1,face)=x(2,i+1)
      surface_points(3,1,face)=x(3,i+1)
      
      end if
      call add_surface(face,0,.TRUE.)
      end do
      vol=(0.5_DOUBLE)*(max_corner(2)-min_corner(2))*(max_corner(1)**2-m
     &in_corner(1)**2)*(max_corner(3)-min_corner(3))
      else if(symmetry.EQ.1.OR.symmetry.EQ.3.OR.symmetry.EQ.4)then
      a(1)=(0.0_DOUBLE)
      a(2)=(1.0_DOUBLE)
      a(3)=(0.0_DOUBLE)
      call plane(min_corner,a,coeff)
      face1=define_surface(coeff,.TRUE.)
      call add_surface(face1,0,.TRUE.)
      call plane(max_corner,a,coeff)
      face2=define_surface(coeff,.TRUE.)
      call add_surface(-face2,0,.TRUE.)
      if(symmetry.NE.4)then
      call init_identity(tx)
      x0(1)=(0.0_DOUBLE)
      x0(2)=-(max_corner(2)-min_corner(2))
      x0(3)=(0.0_DOUBLE)
      call geom_translate(x0,tx)
      call add_transform(face1,face2,tx)
      call invert(tx,tx)
      call add_transform(-face2,-face1,tx)
      end if
      do i=0,3
      x1(1)=x(1,i)
      x1(2)=x(2,i)
      x1(3)=x(3,i)
      
      x2(1)=x(1,i+1)
      x2(2)=x(2,i+1)
      x2(3)=x(3,i+1)
      
      if(vector_compare(x1,x2).GT.0)then
      x3(1)=x1(1)
      x3(2)=(1.0_DOUBLE)
      x3(3)=x1(3)
      else
      x3(1)=x2(1)
      x3(2)=(1.0_DOUBLE)
      x3(3)=x2(3)
      end if
      call planea(x1,x3,x2,coeff)
      face=define_surface_a(coeff,x1,x2)
      if(i.EQ.3)face1=face
      if(i.EQ.1)face2=-face
      if(face.GT.0)then
      surface_points(1,0,face)=x1(1)
      surface_points(2,0,face)=x1(2)
      surface_points(3,0,face)=x1(3)
      
      surface_points(1,1,face)=x2(1)
      surface_points(2,1,face)=x2(2)
      surface_points(3,1,face)=x2(3)
      
      end if
      call add_surface(face,0,.TRUE.)
      end do
      if(symmetry.EQ.3)then
      call init_identity(tx)
      x0(1)=(0.0_DOUBLE)
      x0(2)=(0.0_DOUBLE)
      x0(3)=-(max_corner(3)-min_corner(3))
      call geom_translate(x0,tx)
      call add_transform(face1,face2,tx)
      call invert(tx,tx)
      call add_transform(-face2,-face1,tx)
      end if
      vol=(max_corner(1)-min_corner(1))*(max_corner(2)-min_corner(2))*(m
     &ax_corner(3)-min_corner(3))
      end if
      geometry_symmetry=symmetry
      universal_cell_vol=vol
      return
      end
      subroutine boundaries_neighbors
      
      use gi_mod
      
      use zn_mod
      
      use al_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer j,k,i,base,sigma,init
      integer ptr_surf_list,ptr_cell_list
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
      
      
      logical check_mem
      integer alloc_mem,realloc_mem
      external check_mem,init_mem,alloc_mem,realloc_mem,free_mem,end_mem
      
      if(mod(((ncells)-(0)+1),100).NE.0)then
      cells =>mem_realloc_i2(cells,(1),(4),(0),((int(((((ncells)-(0)+1))
     &+100-1)/100)*100)+(0)-1),(ncells),'cells')
      end if
      if(mod(((nsurfaces)-(1)+1),100).NE.0)then
      surfaces =>mem_realloc_i3(surfaces,(0),(1),(0),(1),(1),((int(((((n
     &surfaces)-(1)+1))+100-1)/100)*100)+(1)-1),(nsurfaces),'surfaces')
      end if
      if(mod(((nsurfaces)-(1)+1),100).NE.0)then
      surface_sectors =>mem_realloc_i3(surface_sectors,(0),(1),(0),(1),(
     &1),((int(((((nsurfaces)-(1)+1))+100-1)/100)*100)+(1)-1),(nsurfaces
     &),'surface_sectors')
      end if
      if(mod(((nsurfaces)-(1)+1),100).NE.0)then
      surface_coeffs =>mem_realloc_r2(surface_coeffs,(1),(10),(1),((int(
     &((((nsurfaces)-(1)+1))+100-1)/100)*100)+(1)-1),(nsurfaces),'surfac
     &e_coeffs')
      end if
      if(mod(((nsurfaces)-(1)+1),100).NE.0)then
      surface_points =>mem_realloc_r3(surface_points,(1),(3),(0),(1),(1)
     &,((int(((((nsurfaces)-(1)+1))+100-1)/100)*100)+(1)-1),(nsurfaces),
     &'surface_points')
      end if
      if(mod(((zn_num)-(1)+1),100).NE.0)then
      zone_type =>mem_realloc_i1(zone_type,(1),((int(((((zn_num)-(1)+1))
     &+100-1)/100)*100)+(1)-1),(zn_num),'zone_type')
      end if
      if(mod(((zn_num)-(1)+1),100).NE.0)then
      zone_index =>mem_realloc_i2(zone_index,(1),(4),(1),((int(((((zn_nu
     &m)-(1)+1))+100-1)/100)*100)+(1)-1),(zn_num),'zone_index')
      end if
      if(mod(((zn_num)-(1)+1),100).NE.0)then
      zone_center =>mem_realloc_r2(zone_center,(1),(3),(1),((int(((((zn_
     &num)-(1)+1))+100-1)/100)*100)+(1)-1),(zn_num),'zone_center')
      end if
      if(mod(((zn_num)-(1)+1),100).NE.0)then
      zone_pointer =>mem_realloc_i1(zone_pointer,(1),((int(((((zn_num)-(
     &1)+1))+100-1)/100)*100)+(1)-1),(zn_num),'zone_pointer')
      end if
      if(mod(((zn_num)-(1)+1),100).NE.0)then
      zone_volume =>mem_realloc_r1(zone_volume,(1),((int(((((zn_num)-(1)
     &+1))+100-1)/100)*100)+(1)-1),(zn_num),'zone_volume')
      end if
      if(mod(((zn_num)-(1)+1),100).NE.0)then
      zone_min =>mem_realloc_r2(zone_min,(1),(3),(1),((int(((((zn_num)-(
     &1)+1))+100-1)/100)*100)+(1)-1),(zn_num),'zone_min')
      end if
      if(mod(((zn_num)-(1)+1),100).NE.0)then
      zone_max =>mem_realloc_r2(zone_max,(1),(3),(1),((int(((((zn_num)-(
     &1)+1))+100-1)/100)*100)+(1)-1),(zn_num),'zone_max')
      end if
      if(mod(((nsurfaces)-(1)+1),100).NE.0)then
      surfaces_tx_ind =>mem_realloc_i3(surfaces_tx_ind,(0),(1),(0),(1),(
     &1),((int(((((nsurfaces)-(1)+1))+100-1)/100)*100)+(1)-1),(nsurfaces
     &),'surfaces_tx_ind')
      end if
      if(mod(((ntransforms)-(0)+1),100).NE.0)then
      surfaces_tx_mx =>mem_realloc_r3(surfaces_tx_mx,(1),(3),(1),(4),(0)
     &,((int(((((ntransforms)-(0)+1))+100-1)/100)*100)+(0)-1),(ntransfor
     &ms),'surfaces_tx_mx')
      end if
      init=1
      do i=1,zn_num
      if(zone_type(i).EQ.2)then
      if(init.EQ.1)then
      zone_index_min(1)=zone_index(1,i)
      zone_index_max(1)=zone_index(1,i)
      zone_index_min(2)=zone_index(2,i)
      zone_index_max(2)=zone_index(2,i)
      zone_index_min(3)=0
      zone_index_max(3)=0
      zone_index_min(4)=0
      zone_index_max(4)=0
      init=0
      else
      zone_index_min(1)=min(zone_index_min(1),zone_index(1,i))
      zone_index_max(1)=max(zone_index_max(1),zone_index(1,i))
      zone_index_min(2)=min(zone_index_min(2),zone_index(2,i))
      zone_index_max(2)=max(zone_index_max(2),zone_index(2,i))
      zone_index_min(3)=min(zone_index_min(3),zone_index(3,i))
      zone_index_max(3)=max(zone_index_max(3),zone_index(3,i))
      end if
      end if
      end do
      nboundaries=0
      do j=0,ncells
      nboundaries=nboundaries+cells(3,j)
      end do
      boundaries =>mem_alloc_i1((1),(nboundaries),'boundaries')
      base=0
      do j=0,ncells
      ptr_surf_list=cells(1,j)
      do i=1,cells(3,j)
      boundaries(base+i)=memory(ptr_surf_list+(i)-1)
      end do
      if(check_mem())continue
      call free_mem(ptr_surf_list,((((cells(3,j))+10-1)/10)*10))
      cells(1,j)=base+1
      base=base+cells(3,j)
      end do
      if(base.EQ.nboundaries)continue
      nneighbors=ncells
      do k=1,nsurfaces
      do sigma=0,1
      if(surfaces(sigma,1,k).GT.1)nneighbors=nneighbors+surfaces(sigma,1
     &,k)
      end do
      end do
      neighbors =>mem_alloc_i1((0),(nneighbors),'neighbors')
      do i=0,ncells
      neighbors(i)=i
      end do
      base=ncells
      do k=1,nsurfaces
      do sigma=0,1
      ptr_cell_list=surfaces(sigma,0,k)
      if(surfaces(sigma,1,k).EQ.1)then
      surfaces(sigma,0,k)=memory(ptr_cell_list+(1)-1)
      else
      do i=1,surfaces(sigma,1,k)
      neighbors(base+i)=memory(ptr_cell_list+(i)-1)
      end do
      surfaces(sigma,0,k)=base+1
      base=base+surfaces(sigma,1,k)
      end if
      if(check_mem())continue
      call free_mem(ptr_cell_list,((((surfaces(sigma,1,k))+10-1)/10)*10)
     &)
      end do
      end do
      if(base.EQ.nneighbors)continue
      if(check_mem())continue
      return
      end
      subroutine end_sectors
      
      use gi_mod
      
      use sc_mod
      
      use al_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer k,i,base,sigma
      integer ptr_sector_list
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
      
      
      logical check_mem
      integer alloc_mem,realloc_mem
      external check_mem,init_mem,alloc_mem,realloc_mem,free_mem,end_mem
      
      if(mod(((nsectors)-(0)+1),100).NE.0)then
      strata =>mem_realloc_i1(strata,(0),((int(((((nsectors)-(0)+1))+100
     &-1)/100)*100)+(0)-1),(nsectors),'strata')
      end if
      if(mod(((nsectors)-(0)+1),100).NE.0)then
      sector_strata_segment =>mem_realloc_i1(sector_strata_segment,(0),(
     &(int(((((nsectors)-(0)+1))+100-1)/100)*100)+(0)-1),(nsectors),'sec
     &tor_strata_segment')
      end if
      if(mod(((nsectors)-(0)+1),100).NE.0)then
      sector_zone =>mem_realloc_i1(sector_zone,(0),((int(((((nsectors)-(
     &0)+1))+100-1)/100)*100)+(0)-1),(nsectors),'sector_zone')
      end if
      if(mod(((nsectors)-(0)+1),100).NE.0)then
      sector_surface =>mem_realloc_i1(sector_surface,(0),((int(((((nsect
     &ors)-(0)+1))+100-1)/100)*100)+(0)-1),(nsectors),'sector_surface')
      end if
      if(mod(((nsectors)-(0)+1),100).NE.0)then
      sector_points =>mem_realloc_r3(sector_points,(1),(3),(0),(1),(0),(
     &(int(((((nsectors)-(0)+1))+100-1)/100)*100)+(0)-1),(nsectors),'sec
     &tor_points')
      end if
      if(mod(((nsectors)-(0)+1),100).NE.0)then
      sector_type_pointer =>mem_realloc_i2(sector_type_pointer,(1),(17),
     &(0),((int(((((nsectors)-(0)+1))+100-1)/100)*100)+(0)-1),(nsectors)
     &,'sector_type_pointer')
      end if
      if(mod(((sc_vacuum_num)-(0)+1),100).NE.0)then
      vacuum_sector =>mem_realloc_i1(vacuum_sector,(0),((int(((((sc_vacu
     &um_num)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_vacuum_num),'vacuum_se
     &ctor')
      end if
      if(mod(((sc_plasma_num)-(0)+1),100).NE.0)then
      plasma_sector =>mem_realloc_i1(plasma_sector,(0),((int(((((sc_plas
     &ma_num)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_plasma_num),'plasma_se
     &ctor')
      end if
      if(mod(((sc_target_num)-(0)+1),100).NE.0)then
      target_sector =>mem_realloc_i1(target_sector,(0),((int(((((sc_targ
     &et_num)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_target_num),'target_se
     &ctor')
      end if
      if(mod(((sc_target_num)-(0)+1),100).NE.0)then
      target_material =>mem_realloc_i1(target_material,(0),((int(((((sc_
     &target_num)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_target_num),'targe
     &t_material')
      end if
      if(mod(((sc_target_num)-(0)+1),100).NE.0)then
      target_temperature =>mem_realloc_r1(target_temperature,(0),((int((
     &(((sc_target_num)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_target_num),
     &'target_temperature')
      end if
      if(mod(((sc_target_num)-(0)+1),100).NE.0)then
      target_recyc_coef =>mem_realloc_r1(target_recyc_coef,(0),((int((((
     &(sc_target_num)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_target_num),'t
     &arget_recyc_coef')
      end if
      if(mod(((sc_wall_num)-(0)+1),100).NE.0)then
      wall_sector =>mem_realloc_i1(wall_sector,(0),((int(((((sc_wall_num
     &)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_wall_num),'wall_sector')
      end if
      if(mod(((sc_wall_num)-(0)+1),100).NE.0)then
      wall_material =>mem_realloc_i1(wall_material,(0),((int(((((sc_wall
     &_num)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_wall_num),'wall_material
     &')
      end if
      if(mod(((sc_wall_num)-(0)+1),100).NE.0)then
      wall_temperature =>mem_realloc_r1(wall_temperature,(0),((int(((((s
     &c_wall_num)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_wall_num),'wall_te
     &mperature')
      end if
      if(mod(((sc_wall_num)-(0)+1),100).NE.0)then
      wall_recyc_coef =>mem_realloc_r1(wall_recyc_coef,(0),((int(((((sc_
     &wall_num)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_wall_num),'wall_recy
     &c_coef')
      end if
      if(mod(((sc_exit_num)-(0)+1),100).NE.0)then
      exit_sector =>mem_realloc_i1(exit_sector,(0),((int(((((sc_exit_num
     &)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_exit_num),'exit_sector')
      end if
      if(mod(((sc_diagnostic_grps)-(0)+1),100).NE.0)then
      diagnostic_grp_name =>mem_realloc_c1(diagnostic_grp_name,(40),(0),
     &((int(((((sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_
     &diagnostic_grps),'diagnostic_grp_name')
      end if
      if(mod(((sc_diagnostic_grps)-(0)+1),100).NE.0)then
      diagnostic_num_sectors =>mem_realloc_i1(diagnostic_num_sectors,(0)
     &,((int(((((sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)+(0)-1),(sc
     &_diagnostic_grps),'diagnostic_num_sectors')
      end if
      if(mod(((sc_diagnostic_grps)-(0)+1),100).NE.0)then
      diagnostic_var =>mem_realloc_i1(diagnostic_var,(0),((int(((((sc_di
     &agnostic_grps)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_diagnostic_grps
     &),'diagnostic_var')
      end if
      if(mod(((sc_diagnostic_grps)-(0)+1),100).NE.0)then
      diagnostic_tab_index =>mem_realloc_i1(diagnostic_tab_index,(0),((i
     &nt(((((sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_dia
     &gnostic_grps),'diagnostic_tab_index')
      end if
      if(mod(((sc_diagnostic_grps)-(0)+1),100).NE.0)then
      diagnostic_min =>mem_realloc_r1(diagnostic_min,(0),((int(((((sc_di
     &agnostic_grps)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_diagnostic_grps
     &),'diagnostic_min')
      end if
      if(mod(((sc_diagnostic_grps)-(0)+1),100).NE.0)then
      diagnostic_delta =>mem_realloc_r1(diagnostic_delta,(0),((int(((((s
     &c_diagnostic_grps)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_diagnostic_
     &grps),'diagnostic_delta')
      end if
      if(mod(((sc_diagnostic_grps)-(0)+1),100).NE.0)then
      diagnostic_spacing =>mem_realloc_i1(diagnostic_spacing,(0),((int((
     &(((sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_diagnos
     &tic_grps),'diagnostic_spacing')
      end if
      if(mod(((sc_diagnostic_grps)-(0)+1),100).NE.0)then
      diagnostic_grp_base =>mem_realloc_i1(diagnostic_grp_base,(0),((int
     &(((((sc_diagnostic_grps)-(0)+1))+100-1)/100)*100)+(0)-1),(sc_diagn
     &ostic_grps),'diagnostic_grp_base')
      end if
      sectors =>mem_alloc_i1((0),(nsectors),'sectors')
      sectors(0)=2000000000
      base=0
      do k=1,nsurfaces
      do sigma=0,1
      ptr_sector_list=surface_sectors(sigma,0,k)
      do i=1,surface_sectors(sigma,1,k)
      sectors(base+i)=memory(ptr_sector_list+(i)-1)
      end do
      surface_sectors(sigma,0,k)=base+1
      base=base+surface_sectors(sigma,1,k)
      if(check_mem())continue
      call free_mem(ptr_sector_list,((((surface_sectors(sigma,1,k))+10-1
     &)/10)*10))
      end do
      end do
      if(base.EQ.nsectors)continue
      if(check_mem())continue
      return
      end
      function define_surface(coeff,duplicate)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer define_surface
      REAL(kind=DOUBLE)coeff(10)
      logical duplicate
      integer i,k,orientation
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
      
      
      external surface_normalize,surface_equal
      logical surface_equal
      call surface_normalize(coeff)
      if(duplicate)then
      do k=1,nsurfaces
      if(surface_equal(coeff,surface_coeffs(:,k),orientation))then
      define_surface=orientation*k
      return
      end if
      end do
      end if
      nsurfaces=nsurfaces+1
      if(mod(((nsurfaces)-(1)+1),100).EQ.1)then
      surfaces =>mem_realloc_i3(surfaces,(0),(1),(0),(1),(1),(((int(((((
     &nsurfaces)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((nsurface
     &s)-(1)+1))+100-1)/100)*100)+(1)-1),'surfaces')
      end if
      if(mod(((nsurfaces)-(1)+1),100).EQ.1)then
      surface_sectors =>mem_realloc_i3(surface_sectors,(0),(1),(0),(1),(
     &1),(((int(((((nsurfaces)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((in
     &t(((((nsurfaces)-(1)+1))+100-1)/100)*100)+(1)-1),'surface_sectors'
     &)
      end if
      if(mod(((nsurfaces)-(1)+1),100).EQ.1)then
      surface_coeffs =>mem_realloc_r2(surface_coeffs,(1),(10),(1),(((int
     &(((((nsurfaces)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(((((nsu
     &rfaces)-(1)+1))+100-1)/100)*100)+(1)-1),'surface_coeffs')
      end if
      if(mod(((nsurfaces)-(1)+1),100).EQ.1)then
      surface_points =>mem_realloc_r3(surface_points,(1),(3),(0),(1),(1)
     &,(((int(((((nsurfaces)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((int(
     &((((nsurfaces)-(1)+1))+100-1)/100)*100)+(1)-1),'surface_points')
      end if
      if(mod(((nsurfaces)-(1)+1),100).EQ.1)then
      surfaces_tx_ind =>mem_realloc_i3(surfaces_tx_ind,(0),(1),(0),(1),(
     &1),(((int(((((nsurfaces)-(1)+1))+100-1)/100)*100)-100)+(1)-1),((in
     &t(((((nsurfaces)-(1)+1))+100-1)/100)*100)+(1)-1),'surfaces_tx_ind'
     &)
      end if
      do i=1,10
      surface_coeffs(i,nsurfaces)=coeff(i)
      end do
      surfaces(0,0,nsurfaces)=0
      surfaces(0,1,nsurfaces)=0
      surfaces(1,0,nsurfaces)=0
      surfaces(1,1,nsurfaces)=0
      surface_sectors(0,0,nsurfaces)=0
      surface_sectors(0,1,nsurfaces)=0
      surface_sectors(1,0,nsurfaces)=0
      surface_sectors(1,1,nsurfaces)=0
      surfaces_tx_ind(0,0,nsurfaces)=0
      surfaces_tx_ind(0,1,nsurfaces)=0
      surfaces_tx_ind(1,0,nsurfaces)=0
      surfaces_tx_ind(1,1,nsurfaces)=0
      surface_points(1,0,nsurfaces)=(1.0e30_DOUBLE)
      surface_points(2,0,nsurfaces)=(1.0e30_DOUBLE)
      surface_points(3,0,nsurfaces)=(1.0e30_DOUBLE)
      surface_points(1,1,nsurfaces)=(1.0e30_DOUBLE)
      surface_points(2,1,nsurfaces)=(1.0e30_DOUBLE)
      surface_points(3,1,nsurfaces)=(1.0e30_DOUBLE)
      define_surface=nsurfaces
      return
      end
      function define_surface_a(coeff,x1,x2)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer define_surface_a
      REAL(kind=DOUBLE)coeff(10),x1(3),x2(3)
      external define_surface,lookup_surface
      integer define_surface,lookup_surface
      define_surface_a=lookup_surface(x1,x2)
      if(define_surface_a.EQ.0)then
      define_surface_a=define_surface(coeff,.FALSE.)
      end if
      return
      end
      function lookup_surface(x1,x2)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer lookup_surface
      REAL(kind=DOUBLE)x1(3),x2(3)
      integer k
      do k=1,nsurfaces
      if((x1(1).EQ.surface_points(1,0,k).AND.x1(2).EQ.surface_points(2,0
     &,k).AND.x1(3).EQ.surface_points(3,0,k)).AND.(x2(1).EQ.surface_poin
     &ts(1,1,k).AND.x2(2).EQ.surface_points(2,1,k).AND.x2(3).EQ.surface_
     &points(3,1,k)))then
      lookup_surface=k
      return
      else if((x1(1).EQ.surface_points(1,1,k).AND.x1(2).EQ.surface_point
     &s(2,1,k).AND.x1(3).EQ.surface_points(3,1,k)).AND.(x2(1).EQ.surface
     &_points(1,0,k).AND.x2(2).EQ.surface_points(2,0,k).AND.x2(3).EQ.sur
     &face_points(3,0,k)))then
      lookup_surface=-k
      return
      end if
      end do
      lookup_surface=0
      return
      end
      function lookup_sector(x1,x2)
      
      use gi_mod
      
      use sc_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer lookup_sector
      REAL(kind=DOUBLE)x1(3),x2(3)
      integer k
      do k=1,nsectors
      if((x1(1).EQ.sector_points(1,0,k).AND.x1(2).EQ.sector_points(2,0,k
     &).AND.x1(3).EQ.sector_points(3,0,k)).AND.(x2(1).EQ.sector_points(1
     &,1,k).AND.x2(2).EQ.sector_points(2,1,k).AND.x2(3).EQ.sector_points
     &(3,1,k)))then
      lookup_sector=k
      return
      end if
      end do
      lookup_sector=0
      return
      end
      function sloppy_lookup_sector(x1,x2,slop)
      
      use gi_mod
      
      use sc_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer sloppy_lookup_sector
      REAL(kind=DOUBLE)x1(3),x2(3)
      REAL(kind=DOUBLE)slop
      integer k
      do k=1,nsectors
      if(((abs(x1(1)-sector_points(1,0,k)).LT.slop).AND.(abs(x1(2)-secto
     &r_points(2,0,k)).LT.slop).AND.(abs(x1(3)-sector_points(3,0,k)).LT.
     &slop)).AND.((abs(x2(1)-sector_points(1,1,k)).LT.slop).AND.(abs(x2(
     &2)-sector_points(2,1,k)).LT.slop).AND.(abs(x2(3)-sector_points(3,1
     &,k)).LT.slop)))then
      sloppy_lookup_sector=k
      return
      end if
      end do
      sloppy_lookup_sector=0
      return
      end
      function start_cell(zone)
      
      use gi_mod
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer start_cell
      integer zone
      integer z
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
      
      
      ncells=ncells+1
      if(mod(((ncells)-(0)+1),100).EQ.1)then
      cells =>mem_realloc_i2(cells,(1),(4),(0),(((int(((((ncells)-(0)+1)
     &)+100-1)/100)*100)-100)+(0)-1),((int(((((ncells)-(0)+1))+100-1)/10
     &0)*100)+(0)-1),'cells')
      end if
      if(zone.GT.0)continue
      cells(4,ncells)=zone
      cells(1,ncells)=0
      cells(2,ncells)=0
      cells(3,ncells)=0
      start_cell=ncells
      if((int((((zn_num)-(1)+1)+100-1)/100)*100).NE.(int((((max(zn_num,z
     &one))-(1)+1)+100-1)/100)*100))then
      zone_type =>mem_realloc_i1(zone_type,(1),((int((((zn_num)-(1)+1)+1
     &00-1)/100)*100)+(1)-1),((int((((max(zn_num,zone))-(1)+1)+100-1)/10
     &0)*100)+(1)-1),'zone_type')
      end if
      if((int((((zn_num)-(1)+1)+100-1)/100)*100).NE.(int((((max(zn_num,z
     &one))-(1)+1)+100-1)/100)*100))then
      zone_index =>mem_realloc_i2(zone_index,(1),(4),(1),((int((((zn_num
     &)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((max(zn_num,zone))-(1)+1)
     &+100-1)/100)*100)+(1)-1),'zone_index')
      end if
      if((int((((zn_num)-(1)+1)+100-1)/100)*100).NE.(int((((max(zn_num,z
     &one))-(1)+1)+100-1)/100)*100))then
      zone_center =>mem_realloc_r2(zone_center,(1),(3),(1),((int((((zn_n
     &um)-(1)+1)+100-1)/100)*100)+(1)-1),((int((((max(zn_num,zone))-(1)+
     &1)+100-1)/100)*100)+(1)-1),'zone_center')
      end if
      if((int((((zn_num)-(1)+1)+100-1)/100)*100).NE.(int((((max(zn_num,z
     &one))-(1)+1)+100-1)/100)*100))then
      zone_pointer =>mem_realloc_i1(zone_pointer,(1),((int((((zn_num)-(1
     &)+1)+100-1)/100)*100)+(1)-1),((int((((max(zn_num,zone))-(1)+1)+100
     &-1)/100)*100)+(1)-1),'zone_pointer')
      end if
      if((int((((zn_num)-(1)+1)+100-1)/100)*100).NE.(int((((max(zn_num,z
     &one))-(1)+1)+100-1)/100)*100))then
      zone_volume =>mem_realloc_r1(zone_volume,(1),((int((((zn_num)-(1)+
     &1)+100-1)/100)*100)+(1)-1),((int((((max(zn_num,zone))-(1)+1)+100-1
     &)/100)*100)+(1)-1),'zone_volume')
      end if
      if((int((((zn_num)-(1)+1)+100-1)/100)*100).NE.(int((((max(zn_num,z
     &one))-(1)+1)+100-1)/100)*100))then
      zone_min =>mem_realloc_r2(zone_min,(1),(3),(1),((int((((zn_num)-(1
     &)+1)+100-1)/100)*100)+(1)-1),((int((((max(zn_num,zone))-(1)+1)+100
     &-1)/100)*100)+(1)-1),'zone_min')
      end if
      if((int((((zn_num)-(1)+1)+100-1)/100)*100).NE.(int((((max(zn_num,z
     &one))-(1)+1)+100-1)/100)*100))then
      zone_max =>mem_realloc_r2(zone_max,(1),(3),(1),((int((((zn_num)-(1
     &)+1)+100-1)/100)*100)+(1)-1),((int((((max(zn_num,zone))-(1)+1)+100
     &-1)/100)*100)+(1)-1),'zone_max')
      end if
      do z=zn_num+1,max(zn_num,zone)
      zone_type(z)=1000000000
      zone_index(1,z)=1000000000
      zone_index(2,z)=1000000000
      zone_index(3,z)=1000000000
      zone_index(4,z)=1000000000
      zone_center(1,z)=(1.0e30_DOUBLE)
      zone_center(2,z)=(1.0e30_DOUBLE)
      zone_center(3,z)=(1.0e30_DOUBLE)
      zone_volume(z)=(1.0e30_DOUBLE)
      zone_min(1,z)=(1.0e30_DOUBLE)
      zone_min(2,z)=(1.0e30_DOUBLE)
      zone_min(3,z)=(1.0e30_DOUBLE)
      zone_max(1,z)=(1.0e30_DOUBLE)
      zone_max(2,z)=(1.0e30_DOUBLE)
      zone_max(3,z)=(1.0e30_DOUBLE)
      end do
      zn_num=max(zn_num,zone)
      return
      end
      subroutine add_surface(surface,cell,face)
      
      use gi_mod
      
      use al_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer surface,cell
      logical face
      integer i,surface_count,cell_count
      integer ptr_surf_list,ptr_cell_list
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
      
      
      logical check_mem
      integer alloc_mem,realloc_mem
      external check_mem,init_mem,alloc_mem,realloc_mem,free_mem,end_mem
      ptr_surf_list=cells(1,cell)
      surface_count=cells(3,cell)
      if(mod(surface_count,10).EQ.0)then
      if(check_mem())continue
      ptr_surf_list=realloc_mem(ptr_surf_list,surface_count,surface_coun
     &t+10)
      cells(1,cell)=ptr_surf_list
      end if
      cells(3,cell)=surface_count+1
      if(face)then
      cells(2,cell)=cells(2,cell)+1
      do i=cells(3,cell),cells(2,cell)+1,-1
      memory(ptr_surf_list+(i)-1)=memory(ptr_surf_list+(i-1)-1)
      end do
      memory(ptr_surf_list+(cells(2,cell))-1)=surface
      else
      memory(ptr_surf_list+(cells(3,cell))-1)=surface
      end if
      if(face)then
      ptr_cell_list=surfaces((sign(1,surface)+1)/2,0,abs(surface))
      cell_count=surfaces((sign(1,surface)+1)/2,1,abs(surface))
      if(mod(cell_count,10).EQ.0)then
      if(check_mem())continue
      ptr_cell_list=realloc_mem(ptr_cell_list,cell_count,cell_count+10)
      end if
      cell_count=cell_count+1
      memory(ptr_cell_list+(cell_count)-1)=cell
      surfaces((sign(1,surface)+1)/2,0,abs(surface))=ptr_cell_list
      surfaces((sign(1,surface)+1)/2,1,abs(surface))=cell_count
      end if
      return
      end
      function define_sector(stratum,seg,surface,zone1,zone2)
      
      use gi_mod
      
      use sc_mod
      
      use zn_mod
      
      use al_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer define_sector
      integer stratum,seg,surface,zone1,zone2
      integer i,sector_count,base,zone_sign,icell,num_faces,iface
      integer ptr_sector_list
      logical match_plus,match_minus
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
      
      
      logical check_mem
      integer alloc_mem,realloc_mem
      external check_mem,init_mem,alloc_mem,realloc_mem,free_mem,end_mem
      logical check_zone
      if(stratum.GT.0)continue
      if(surface.NE.0)continue
      if(check_zone(zone1))continue
      
      match_plus=.FALSE.
      match_minus=.FALSE.
      zone_sign=0
      do icell=0,ncells
      if(cells(4,icell).EQ.zone1)then
      zone_sign=1
      else if(check_zone(zone2).AND.cells(4,icell).EQ.zone2)then
      zone_sign=-1
      end if
      if(zone_sign.NE.0)then
      num_faces=cells(2,icell)
      if(num_faces.GT.0)continue
      do iface=cells(1,icell),cells(1,icell)+num_faces-1
      if(boundaries(iface).EQ.zone_sign*surface)then
      if(zone_sign.GT.0)then
      match_plus=.TRUE.
      else
      match_minus=.TRUE.
      end if
      end if
      end do
      zone_sign=0
      end if
      end do
      if(match_plus.AND.(.NOT.check_zone(zone2).OR.match_minus))continue
      nsectors=nsectors+1
      if(mod(((nsectors)-(0)+1),100).EQ.1)then
      strata =>mem_realloc_i1(strata,(0),(((int(((((nsectors)-(0)+1))+10
     &0-1)/100)*100)-100)+(0)-1),((int(((((nsectors)-(0)+1))+100-1)/100)
     &*100)+(0)-1),'strata')
      end if
      if(mod(((nsectors)-(0)+1),100).EQ.1)then
      sector_strata_segment =>mem_realloc_i1(sector_strata_segment,(0),(
     &((int(((((nsectors)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int((((
     &(nsectors)-(0)+1))+100-1)/100)*100)+(0)-1),'sector_strata_segment'
     &)
      end if
      if(mod(((nsectors)-(0)+1),100).EQ.1)then
      sector_zone =>mem_realloc_i1(sector_zone,(0),(((int(((((nsectors)-
     &(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((nsectors)-(0)+1))+1
     &00-1)/100)*100)+(0)-1),'sector_zone')
      end if
      if(mod(((nsectors)-(0)+1),100).EQ.1)then
      sector_surface =>mem_realloc_i1(sector_surface,(0),(((int(((((nsec
     &tors)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int(((((nsectors)-(0)
     &+1))+100-1)/100)*100)+(0)-1),'sector_surface')
      end if
      if(mod(((nsectors)-(0)+1),100).EQ.1)then
      sector_points =>mem_realloc_r3(sector_points,(1),(3),(0),(1),(0),(
     &((int(((((nsectors)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((int((((
     &(nsectors)-(0)+1))+100-1)/100)*100)+(0)-1),'sector_points')
      end if
      if(mod(((nsectors)-(0)+1),100).EQ.1)then
      sector_type_pointer =>mem_realloc_i2(sector_type_pointer,(1),(17),
     &(0),(((int(((((nsectors)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((in
     &t(((((nsectors)-(0)+1))+100-1)/100)*100)+(0)-1),'sector_type_point
     &er')
      end if
      strata(nsectors)=stratum
      sector_strata_segment(nsectors)=seg
      sector_zone(nsectors)=zone1
      sector_surface(nsectors)=surface
      if(surface.GT.0)then
      sector_points(1,0,nsectors)=surface_points(1,0,surface)
      sector_points(2,0,nsectors)=surface_points(2,0,surface)
      sector_points(3,0,nsectors)=surface_points(3,0,surface)
      
      sector_points(1,1,nsectors)=surface_points(1,1,surface)
      sector_points(2,1,nsectors)=surface_points(2,1,surface)
      sector_points(3,1,nsectors)=surface_points(3,1,surface)
      
      else
      sector_points(1,1,nsectors)=surface_points(1,0,-surface)
      sector_points(2,1,nsectors)=surface_points(2,0,-surface)
      sector_points(3,1,nsectors)=surface_points(3,0,-surface)
      
      sector_points(1,0,nsectors)=surface_points(1,1,-surface)
      sector_points(2,0,nsectors)=surface_points(2,1,-surface)
      sector_points(3,0,nsectors)=surface_points(3,1,-surface)
      
      end if
      do i=1,17
      sector_type_pointer(i,nsectors)=2000000000
      end do
      ptr_sector_list=surface_sectors((sign(1,surface)+1)/2,0,abs(surfac
     &e))
      sector_count=surface_sectors((sign(1,surface)+1)/2,1,abs(surface))
      if(mod(sector_count,10).EQ.0)then
      if(check_mem())continue
      ptr_sector_list=realloc_mem(ptr_sector_list,sector_count,sector_co
     &unt+10)
      end if
      sector_count=sector_count+1
      memory(ptr_sector_list+(sector_count)-1)=nsectors
      surface_sectors((sign(1,surface)+1)/2,0,abs(surface))=ptr_sector_l
     &ist
      surface_sectors((sign(1,surface)+1)/2,1,abs(surface))=sector_count
      base=surfaces((sign(1,surface)+1)/2,0,abs(surface))-1
      do i=1,surfaces((sign(1,surface)+1)/2,1,abs(surface))
      if(cells(4,neighbors(base+i)).EQ.zone1)goto 90007
      end do
      if(.FALSE.)continue
90007 continue
      define_sector=nsectors
      return
      end
      subroutine add_transform(surface,newsurface,tx)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer surface,newsurface
      REAL(kind=DOUBLE)tx(3,4)
      integer i
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
      
      
      ntransforms=ntransforms+1
      if(mod(((ntransforms)-(0)+1),100).EQ.1)then
      surfaces_tx_mx =>mem_realloc_r3(surfaces_tx_mx,(1),(3),(1),(4),(0)
     &,(((int(((((ntransforms)-(0)+1))+100-1)/100)*100)-100)+(0)-1),((in
     &t(((((ntransforms)-(0)+1))+100-1)/100)*100)+(0)-1),'surfaces_tx_mx
     &')
      end if
      surfaces_tx_ind((sign(1,surface)+1)/2,0,abs(surface))=newsurface
      surfaces_tx_ind((sign(1,surface)+1)/2,1,abs(surface))=ntransforms
      do i=1,4
      surfaces_tx_mx(1,i,ntransforms)=tx(1,i)
      surfaces_tx_mx(2,i,ntransforms)=tx(2,i)
      surfaces_tx_mx(3,i,ntransforms)=tx(3,i)
      
      end do
      return
      end
      subroutine check_geometry
      
      
      
      use gi_mod
      
      use sc_mod
      
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
      
      
      integer n,j,k,sigma,type,real_sector,type_ptr,opp_side,jb,jc,count
     &,start,same_zone,first_zone
      logical outer,opp_outer
      integer,dimension(:),pointer::outer_surfaces
      integer num_outer_surfaces,orientation
      external surface_equal
      logical surface_equal
      if(ncells.GT.0)continue
      if(nboundaries.GT.0)continue
      if(nsurfaces.GT.0)continue
      if(nneighbors.GT.0)continue
      do j=0,ncells
      if(cells(2,j).GT.0)continue
      if(cells(3,j).GE.cells(2,j))continue
      if(cells(1,j).GT.0)continue
      if(cells(1,j)+cells(3,j)-1.LE.nboundaries)continue
      end do
      do k=1,nboundaries
      if(boundaries(k).NE.0.AND.abs(boundaries(k)).LE.nsurfaces)continue
      end do
      do j=0,nneighbors
      if(neighbors(j).GE.0.AND.neighbors(j).LE.ncells)continue
      end do
      num_outer_surfaces=0
      do k=1,nsurfaces
      outer=.FALSE.
      do sigma=0,1
      if(surfaces(sigma,1,k).GE.0)continue
      if(surfaces(sigma,0,k).GE.0)continue
      if(surfaces(sigma,0,k)+surfaces(sigma,1,k)-1.LE.nneighbors)continu
     &e
      if(surfaces(sigma,1,k).GT.0)then
      do j=surfaces(sigma,0,k),surfaces(sigma,0,k)+surfaces(sigma,1,k)-1
      if(neighbors(j).EQ.0)then
      outer=.TRUE.
      goto 90007
      end if
      end do
90007 continue
      else
      if(sigma.EQ.0)then
      opp_side=1
      else
      opp_side=0
      end if
      if(surfaces(opp_side,1,k).GT.0)then
      opp_outer=.FALSE.
      do j=surfaces(opp_side,0,k),surfaces(opp_side,0,k)+surfaces(opp_si
     &de,1,k)-1
      if(neighbors(j).EQ.0)then
      opp_outer=.TRUE.
      goto 90009
      end if
      end do
90009 continue
      if(.NOT.opp_outer)then
      write(0,*)'This surface isn''t an outer surface, but there''s some
     &thing on one side and not on the other (first instance)'
      write(0,*)k
      write(0,*)surface_points(1,0,k),surface_points(2,0,k),surface_poin
     &ts(3,0,k)
      write(0,*)surface_points(1,1,k),surface_points(2,1,k),surface_poin
     &ts(3,1,k)
      if(.FALSE.)continue
      end if
      else
      do j=1,nboundaries
      if(abs(boundaries(j)).EQ.k)then
      jb=j
      goto 90013
      end if
      end do
      if(' No boundary found matching this surface'.EQ.' ')continue
90013 continue
      do j=1,ncells
      if(cells(1,j).GT.jb)then
      jc=j-1
      goto 90014
      end if
      end do
      jc=ncells
90014 continue
      if((jb.GE.cells(1,jc)+cells(2,jc)).AND.(jb.LE.cells(1,jc)+cells(3,
     &jc)-1))continue
      end if
      end if
      if(surface_sectors(sigma,1,k).GT.1)then
      count=surface_sectors(sigma,1,k)
      start=surface_sectors(sigma,0,k)
      same_zone=0
      first_zone=sector_zone(sectors(start))
      do j=1,count-1
      if(sector_zone(sectors(start+j)).EQ.first_zone)same_zone=1
      end do
      if(same_zone.EQ.1)then
      real_sector=0
      do type=1,6+0-1
      type_ptr=sector_type_pointer(type,sectors(start))
      if((type_ptr.NE.2000000000).AND.(type_ptr.NE.1000000000))then
      real_sector=1
      end if
      end do
      if(real_sector.EQ.1)continue
      end if
      end if
      end do
      if(outer)then
      if(surfaces(0,1,k).EQ.0.OR.surfaces(1,1,k).EQ.0)continue
      if(surfaces(0,1,k).GT.0.OR.surfaces(1,1,k).GT.0)continue
      if(mod(num_outer_surfaces,10).EQ.0)outer_surfaces =>mem_realloc_i1
     &(outer_surfaces,(1),num_outer_surfaces,num_outer_surfaces+10,'oute
     &r_surfaces')
      num_outer_surfaces=num_outer_surfaces+1
      outer_surfaces(num_outer_surfaces)=k
      else
      
      if(surfaces(0,1,k).EQ.0)then
      if(surfaces(1,1,k).NE.0)then
      do j=1,num_outer_surfaces
      if(surface_equal(surface_coeffs(:,k),surface_coeffs(:,outer_surfac
     &es(j)),orientation))goto 90008
      end do
      write(0,*)'This surface isn''t an outer surface, but there''s some
     &thing on one side and not on the other (2nd instance)'
      write(0,*)k
      write(0,*)surface_points(1,0,k),surface_points(2,0,k),surface_poin
     &ts(3,0,k)
      write(0,*)surface_points(1,1,k),surface_points(2,1,k),surface_poin
     &ts(3,1,k)
      if(.FALSE.)continue
      end if
      do j=0,ncells
      do n=cells(2,j)+1,cells(3,j)
      if(abs(boundaries(cells(1,j)-1+n)).EQ.k)goto 90008
      end do
      end do
      if(.FALSE.)continue
90008 continue
      else
      if(surfaces(1,1,k).GT.0)continue
      end if
      end if
      end do
      return
      end
      subroutine print_geometry(unit)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer unit
      integer i,j,k
      write(unit,*)ncells
      do i=0,ncells
      write(unit,*)i,(cells(j,i),j=1,4)
      end do
      write(unit,*)nboundaries
      do i=1,nboundaries
      write(unit,*)i,boundaries(i)
      end do
      write(unit,*)nsurfaces
      do i=1,nsurfaces
      write(unit,*)i,((surfaces(j,k,i),j=0,1),k=0,1)
      end do
      do i=1,nsurfaces
      write(unit,'(i4,10f7.4)')i,(surface_coeffs(j,i),j=1,10)
      end do
      write(unit,*)nneighbors
      do i=0,nneighbors
      write(unit,*)i,neighbors(i)
      end do
      return
      end
      subroutine erase_geometry
      
      use gi_mod
      
      use sc_mod
      
      use de_mod
      
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
      
      
      call mem_free_i2(cells,(1),(4),(0),(ncells),'cells')
      call mem_free_i3(surfaces,(0),(1),(0),(1),(1),(nsurfaces),'surface
     &s')
      call mem_free_r2(surface_coeffs,(1),(10),(1),(nsurfaces),'surface_
     &coeffs')
      call mem_free_r3(surface_points,(1),(3),(0),(1),(1),(nsurfaces),'s
     &urface_points')
      call mem_free_i1(boundaries,(1),(nboundaries),'boundaries')
      call mem_free_i1(neighbors,(0),(nneighbors),'neighbors')
      call mem_free_i3(surface_sectors,(0),(1),(0),(1),(1),(nsurfaces),'
     &surface_sectors')
      call mem_free_i3(surfaces_tx_ind,(0),(1),(0),(1),(1),(nsurfaces),'
     &surfaces_tx_ind')
      call mem_free_r3(surfaces_tx_mx,(1),(3),(1),(4),(0),(ntransforms),
     &'surfaces_tx_mx')
      call mem_free_i1(strata,(0),(nsectors),'strata')
      call mem_free_i1(sector_strata_segment,(0),(nsectors),'sector_stra
     &ta_segment')
      call mem_free_i1(sectors,(0),(nsectors),'sectors')
      call mem_free_i1(sector_zone,(0),(nsectors),'sector_zone')
      call mem_free_i1(sector_surface,(0),(nsectors),'sector_surface')
      call mem_free_r3(sector_points,(1),(3),(0),(1),(0),(nsectors),'sec
     &tor_points')
      call mem_free_i2(sector_type_pointer,(1),(17),(0),(nsectors),'sect
     &or_type_pointer')
      call mem_free_i1(vacuum_sector,(0),(sc_vacuum_num),'vacuum_sector'
     &)
      call mem_free_i1(plasma_sector,(0),(sc_plasma_num),'plasma_sector'
     &)
      call mem_free_i1(target_sector,(0),(sc_target_num),'target_sector'
     &)
      call mem_free_i1(target_material,(0),(sc_target_num),'target_mater
     &ial')
      call mem_free_r1(target_temperature,(0),(sc_target_num),'target_te
     &mperature')
      call mem_free_r1(target_recyc_coef,(0),(sc_target_num),'target_rec
     &yc_coef')
      call mem_free_i1(wall_sector,(0),(sc_wall_num),'wall_sector')
      call mem_free_i1(wall_material,(0),(sc_wall_num),'wall_material')
      call mem_free_r1(wall_temperature,(0),(sc_wall_num),'wall_temperat
     &ure')
      call mem_free_r1(wall_recyc_coef,(0),(sc_wall_num),'wall_recyc_coe
     &f')
      call mem_free_i1(exit_sector,(0),(sc_exit_num),'exit_sector')
      call mem_free_c1(diagnostic_grp_name,(40),(0),(sc_diagnostic_grps)
     &,'diagnostic_grp_name')
      call mem_free_i1(diagnostic_num_sectors,(0),(sc_diagnostic_grps),'
     &diagnostic_num_sectors')
      call mem_free_i1(diagnostic_var,(0),(sc_diagnostic_grps),'diagnost
     &ic_var')
      call mem_free_i1(diagnostic_tab_index,(0),(sc_diagnostic_grps),'di
     &agnostic_tab_index')
      call mem_free_r1(diagnostic_min,(0),(sc_diagnostic_grps),'diagnost
     &ic_min')
      call mem_free_r1(diagnostic_delta,(0),(sc_diagnostic_grps),'diagno
     &stic_delta')
      call mem_free_i1(diagnostic_spacing,(0),(sc_diagnostic_grps),'diag
     &nostic_spacing')
      call mem_free_i1(diagnostic_grp_base,(0),(sc_diagnostic_grps),'dia
     &gnostic_grp_base')
      call mem_free_i1(diagnostic_sector_tab,(0),(sc_diag_size-1),'diagn
     &ostic_sector_tab')
      call mem_free_c1(detector_name,(100),(0),(de_grps),'detector_name'
     &)
      call mem_free_i1(detector_num_views,(0),(de_grps),'detector_num_vi
     &ews')
      call mem_free_i1(detector_var,(0),(de_grps),'detector_var')
      call mem_free_i1(detector_tab_index,(0),(de_grps),'detector_tab_in
     &dex')
      call mem_free_r1(detector_min,(0),(de_grps),'detector_min')
      call mem_free_r1(detector_delta,(0),(de_grps),'detector_delta')
      call mem_free_i1(detector_spacing,(0),(de_grps),'detector_spacing'
     &)
      call mem_free_i1(de_view_base,(0),(de_grps),'de_view_base')
      call mem_free_i1(de_view_tab,(0),(de_view_size-1),'de_view_tab')
      call mem_free_r1(de_zone_frags,(1),(de_zone_frags_dim),'de_zone_fr
     &ags')
      call mem_free_i1(de_zone_frags_start,(0),(detector_total_views),'d
     &e_zone_frags_start')
      call mem_free_i1(de_zone_frags_num,(0),(detector_total_views),'de_
     &zone_frags_num')
      call mem_free_i1(de_zone_frags_zones,(1),(de_zone_frags_dim),'de_z
     &one_frags_zones')
      call mem_free_i1(de_zone_frags_min_zn,(0),(detector_total_views),'
     &de_zone_frags_min_zn')
      call mem_free_i1(de_zone_frags_max_zn,(0),(detector_total_views),'
     &de_zone_frags_max_zn')
      ncells=-1
      nsurfaces=0
      nboundaries=0
      nneighbors=-1
      ntransforms=0
      nsectors=0
      return
      end
      subroutine write_geometry
      
      use gi_mod
      
      use zn_mod
      
      use rf_mod
      
      use sc_mod
      
      use de_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer nc_dims(5),nc_corner(5),nc_edge(5),nc_stat,nc_size,nc_rank
     &,nc_attr,nc_type
      character*128 nc_dummy
      external ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf
     &_inq_format
      integer ncopn,ncvid,ncdid,nccre,ncvdef,ncddef,nf_get_vara_text,nf_
     &inq_format
      integer ncells_id
      integer nsurfaces_id
      integer nboundaries_id
      integer nneighbors_id
      integer ntransforms_id
      integer geometry_symmetry_id
      integer cell_info_ind_id
      integer cell_ind_id
      integer surface_ind_id
      integer boundary_ind_id
      integer neighbor_ind_id
      integer neg_pos_id
      integer surface_info_ind_id
      integer surface_tx_ind_id
      integer tx_ind_1_id
      integer tx_ind_2_id
      integer transform_ind_id
      integer coeff_ind_id
      integer universal_cell_min_id
      integer universal_cell_max_id
      integer universal_cell_vol_id
      integer cells_id
      integer surfaces_id
      integer surfaces_tx_ind_id
      integer surfaces_tx_mx_id
      integer surface_sectors_id
      integer boundaries_id
      integer neighbors_id
      integer surface_coeffs_id
      integer surface_points_id
      
      integer zn_num_id
      integer zone_type_ind_id
      integer zone_index_ind_id
      integer zone_ind_id
      integer zone_type_num_id
      integer zone_type_id
      integer zone_index_id
      integer zone_index_min_id
      integer zone_index_max_id
      integer zone_pointer_id
      integer zone_volume_id
      integer zone_center_id
      integer zone_min_id
      integer zone_max_id
      
      integer vector_id
      integer string_id
      
      integer nsectors_id
      integer sector_ind_id
      integer sector_neg_pos_ind_id
      integer sector_type_ind_id
      integer strata_id
      integer sector_strata_segment_id
      integer sectors_id
      integer sector_zone_id
      integer sector_surface_id
      integer sector_points_id
      integer sector_type_pointer_id
      integer sc_vacuum_num_id
      integer vacuum_ind_id
      integer vacuum_sector_id
      integer sc_plasma_num_id
      integer plasma_ind_id
      integer plasma_sector_id
      integer sc_target_num_id
      integer target_ind_id
      integer target_sector_id
      integer target_material_id
      integer target_temperature_id
      integer target_recyc_coef_id
      integer sc_wall_num_id
      integer wall_ind_id
      integer wall_sector_id
      integer wall_material_id
      integer wall_temperature_id
      integer wall_recyc_coef_id
      integer sc_exit_num_id
      integer exit_ind_id
      integer exit_sector_id
      integer sc_diagnostic_grps_id
      integer sc_diag_name_string_id
      integer diag_grp_ind_id
      integer sc_diag_max_bins_id
      integer diagnostic_grp_name_id
      integer diagnostic_num_sectors_id
      integer diagnostic_var_id
      integer diagnostic_tab_index_id
      integer diagnostic_min_id
      integer diagnostic_delta_id
      integer diagnostic_spacing_id
      integer diagnostic_grp_base_id
      integer sc_diag_size_id
      integer sc_diag_ind_id
      integer diagnostic_sector_tab_id
      
      integer de_symbol_string_id
      integer de_name_string_id
      integer de_grps_id
      integer de_grp_ind_id
      integer de_max_bins_id
      integer de_zone_frags_dim_id
      integer de_zone_frags_ind_id
      integer de_zone_frags_size_id
      integer detector_name_id
      integer detector_num_views_id
      integer detector_var_id
      integer detector_tab_index_id
      integer detector_min_id
      integer detector_delta_id
      integer detector_spacing_id
      integer detector_total_views_id
      integer de_tot_view_ind_id
      integer de_start_end_ind_id
      integer de_view_points_id
      integer de_view_algorithm_id
      integer de_view_halfwidth_id
      integer de_zone_frags_id
      integer de_zone_frags_start_id
      integer de_zone_frags_num_id
      integer de_zone_frags_zones_id
      integer de_zone_frags_min_zn_id
      integer de_zone_frags_max_zn_id
      integer de_view_base_id
      integer de_view_size_id
      integer de_view_ind_id
      integer de_view_tab_id
      
      integer fileid
      character*96 tempfile
      tempfile=filenames_array(4)
      if(tempfile.NE.'undefined')continue
      fileid=nccre(tempfile,0,nc_stat)
      vector_id=ncddef(fileid,'vector',((3)-(1)+1),nc_stat)
      string_id=ncddef(fileid,'string',((300)-(1)+1),nc_stat)
      
      ncells_id=ncvdef(fileid,'ncells',4,0,nc_dims,nc_stat)
      nsurfaces_id=ncvdef(fileid,'nsurfaces',4,0,nc_dims,nc_stat)
      nboundaries_id=ncvdef(fileid,'nboundaries',4,0,nc_dims,nc_stat)
      nneighbors_id=ncvdef(fileid,'nneighbors',4,0,nc_dims,nc_stat)
      ntransforms_id=ncvdef(fileid,'ntransforms',4,0,nc_dims,nc_stat)
      geometry_symmetry_id=ncvdef(fileid,'geometry_symmetry',4,0,nc_dims
     &,nc_stat)
      cell_info_ind_id=ncddef(fileid,'cell_info_ind',((4)-(1)+1),nc_stat
     &)
      cell_ind_id=ncddef(fileid,'cell_ind',((ncells)-(0)+1),nc_stat)
      surface_ind_id=ncddef(fileid,'surface_ind',((nsurfaces)-(1)+1),nc_
     &stat)
      boundary_ind_id=ncddef(fileid,'boundary_ind',((nboundaries)-(1)+1)
     &,nc_stat)
      neighbor_ind_id=ncddef(fileid,'neighbor_ind',((nneighbors)-(0)+1),
     &nc_stat)
      neg_pos_id=ncddef(fileid,'neg_pos',((1)-(0)+1),nc_stat)
      surface_info_ind_id=ncddef(fileid,'surface_info_ind',((1)-(0)+1),n
     &c_stat)
      surface_tx_ind_id=ncddef(fileid,'surface_tx_ind',((1)-(0)+1),nc_st
     &at)
      tx_ind_1_id=ncddef(fileid,'tx_ind_1',((3)-(1)+1),nc_stat)
      tx_ind_2_id=ncddef(fileid,'tx_ind_2',((4)-(1)+1),nc_stat)
      transform_ind_id=ncddef(fileid,'transform_ind',((ntransforms)-(0)+
     &1),nc_stat)
      coeff_ind_id=ncddef(fileid,'coeff_ind',((10)-(1)+1),nc_stat)
      nc_dims(1)=vector_id
      universal_cell_min_id=ncvdef(fileid,'universal_cell_min',6,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=vector_id
      universal_cell_max_id=ncvdef(fileid,'universal_cell_max',6,1,nc_di
     &ms,nc_stat)
      universal_cell_vol_id=ncvdef(fileid,'universal_cell_vol',6,0,nc_di
     &ms,nc_stat)
      nc_dims(1)=cell_info_ind_id
      nc_dims(2)=cell_ind_id
      cells_id=ncvdef(fileid,'cells',4,2,nc_dims,nc_stat)
      nc_dims(1)=neg_pos_id
      nc_dims(2)=surface_info_ind_id
      nc_dims(3)=surface_ind_id
      surfaces_id=ncvdef(fileid,'surfaces',4,3,nc_dims,nc_stat)
      nc_dims(1)=neg_pos_id
      nc_dims(2)=surface_tx_ind_id
      nc_dims(3)=surface_ind_id
      surfaces_tx_ind_id=ncvdef(fileid,'surfaces_tx_ind',4,3,nc_dims,nc_
     &stat)
      nc_dims(1)=tx_ind_1_id
      nc_dims(2)=tx_ind_2_id
      nc_dims(3)=transform_ind_id
      surfaces_tx_mx_id=ncvdef(fileid,'surfaces_tx_mx',6,3,nc_dims,nc_st
     &at)
      nc_dims(1)=neg_pos_id
      nc_dims(2)=surface_info_ind_id
      nc_dims(3)=surface_ind_id
      surface_sectors_id=ncvdef(fileid,'surface_sectors',4,3,nc_dims,nc_
     &stat)
      nc_dims(1)=boundary_ind_id
      boundaries_id=ncvdef(fileid,'boundaries',4,1,nc_dims,nc_stat)
      nc_dims(1)=neighbor_ind_id
      neighbors_id=ncvdef(fileid,'neighbors',4,1,nc_dims,nc_stat)
      nc_dims(1)=coeff_ind_id
      nc_dims(2)=surface_ind_id
      surface_coeffs_id=ncvdef(fileid,'surface_coeffs',6,2,nc_dims,nc_st
     &at)
      nc_dims(1)=vector_id
      nc_dims(2)=neg_pos_id
      nc_dims(3)=surface_ind_id
      surface_points_id=ncvdef(fileid,'surface_points',6,3,nc_dims,nc_st
     &at)
      
      zn_num_id=ncvdef(fileid,'zn_num',4,0,nc_dims,nc_stat)
      zone_type_ind_id=ncddef(fileid,'zone_type_ind',((4)-(1)+1),nc_stat
     &)
      zone_index_ind_id=ncddef(fileid,'zone_index_ind',((4)-(1)+1),nc_st
     &at)
      zone_ind_id=ncddef(fileid,'zone_ind',((zn_num)-(1)+1),nc_stat)
      nc_dims(1)=zone_type_ind_id
      zone_type_num_id=ncvdef(fileid,'zone_type_num',4,1,nc_dims,nc_stat
     &)
      nc_dims(1)=zone_ind_id
      zone_type_id=ncvdef(fileid,'zone_type',4,1,nc_dims,nc_stat)
      nc_dims(1)=zone_index_ind_id
      nc_dims(2)=zone_ind_id
      zone_index_id=ncvdef(fileid,'zone_index',4,2,nc_dims,nc_stat)
      nc_dims(1)=zone_index_ind_id
      zone_index_min_id=ncvdef(fileid,'zone_index_min',4,1,nc_dims,nc_st
     &at)
      nc_dims(1)=zone_index_ind_id
      zone_index_max_id=ncvdef(fileid,'zone_index_max',4,1,nc_dims,nc_st
     &at)
      nc_dims(1)=zone_ind_id
      zone_pointer_id=ncvdef(fileid,'zone_pointer',4,1,nc_dims,nc_stat)
      nc_dims(1)=zone_ind_id
      zone_volume_id=ncvdef(fileid,'zone_volume',6,1,nc_dims,nc_stat)
      nc_dims(1)=vector_id
      nc_dims(2)=zone_ind_id
      zone_center_id=ncvdef(fileid,'zone_center',6,2,nc_dims,nc_stat)
      nc_dims(1)=vector_id
      nc_dims(2)=zone_ind_id
      zone_min_id=ncvdef(fileid,'zone_min',6,2,nc_dims,nc_stat)
      nc_dims(1)=vector_id
      nc_dims(2)=zone_ind_id
      zone_max_id=ncvdef(fileid,'zone_max',6,2,nc_dims,nc_stat)
      
      nsectors_id=ncvdef(fileid,'nsectors',4,0,nc_dims,nc_stat)
      sector_ind_id=ncddef(fileid,'sector_ind',((nsectors)-(0)+1),nc_sta
     &t)
      sector_neg_pos_ind_id=ncddef(fileid,'sector_neg_pos_ind',((1)-(0)+
     &1),nc_stat)
      sector_type_ind_id=ncddef(fileid,'sector_type_ind',((17)-(1)+1),nc
     &_stat)
      nc_dims(1)=sector_ind_id
      strata_id=ncvdef(fileid,'strata',4,1,nc_dims,nc_stat)
      nc_dims(1)=sector_ind_id
      sector_strata_segment_id=ncvdef(fileid,'sector_strata_segment',4,1
     &,nc_dims,nc_stat)
      nc_dims(1)=sector_ind_id
      sectors_id=ncvdef(fileid,'sectors',4,1,nc_dims,nc_stat)
      nc_dims(1)=sector_ind_id
      sector_zone_id=ncvdef(fileid,'sector_zone',4,1,nc_dims,nc_stat)
      nc_dims(1)=sector_ind_id
      sector_surface_id=ncvdef(fileid,'sector_surface',4,1,nc_dims,nc_st
     &at)
      nc_dims(1)=vector_id
      nc_dims(2)=sector_neg_pos_ind_id
      nc_dims(3)=sector_ind_id
      sector_points_id=ncvdef(fileid,'sector_points',6,3,nc_dims,nc_stat
     &)
      nc_dims(1)=sector_type_ind_id
      nc_dims(2)=sector_ind_id
      sector_type_pointer_id=ncvdef(fileid,'sector_type_pointer',4,2,nc_
     &dims,nc_stat)
      sc_vacuum_num_id=ncvdef(fileid,'sc_vacuum_num',4,0,nc_dims,nc_stat
     &)
      vacuum_ind_id=ncddef(fileid,'vacuum_ind',((sc_vacuum_num)-(0)+1),n
     &c_stat)
      nc_dims(1)=vacuum_ind_id
      vacuum_sector_id=ncvdef(fileid,'vacuum_sector',4,1,nc_dims,nc_stat
     &)
      sc_plasma_num_id=ncvdef(fileid,'sc_plasma_num',4,0,nc_dims,nc_stat
     &)
      plasma_ind_id=ncddef(fileid,'plasma_ind',((sc_plasma_num)-(0)+1),n
     &c_stat)
      nc_dims(1)=plasma_ind_id
      plasma_sector_id=ncvdef(fileid,'plasma_sector',4,1,nc_dims,nc_stat
     &)
      sc_target_num_id=ncvdef(fileid,'sc_target_num',4,0,nc_dims,nc_stat
     &)
      target_ind_id=ncddef(fileid,'target_ind',((sc_target_num)-(0)+1),n
     &c_stat)
      nc_dims(1)=target_ind_id
      target_sector_id=ncvdef(fileid,'target_sector',4,1,nc_dims,nc_stat
     &)
      nc_dims(1)=target_ind_id
      target_material_id=ncvdef(fileid,'target_material',4,1,nc_dims,nc_
     &stat)
      nc_dims(1)=target_ind_id
      target_temperature_id=ncvdef(fileid,'target_temperature',6,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=target_ind_id
      target_recyc_coef_id=ncvdef(fileid,'target_recyc_coef',6,1,nc_dims
     &,nc_stat)
      sc_wall_num_id=ncvdef(fileid,'sc_wall_num',4,0,nc_dims,nc_stat)
      wall_ind_id=ncddef(fileid,'wall_ind',((sc_wall_num)-(0)+1),nc_stat
     &)
      nc_dims(1)=wall_ind_id
      wall_sector_id=ncvdef(fileid,'wall_sector',4,1,nc_dims,nc_stat)
      nc_dims(1)=wall_ind_id
      wall_material_id=ncvdef(fileid,'wall_material',4,1,nc_dims,nc_stat
     &)
      nc_dims(1)=wall_ind_id
      wall_temperature_id=ncvdef(fileid,'wall_temperature',6,1,nc_dims,n
     &c_stat)
      nc_dims(1)=wall_ind_id
      wall_recyc_coef_id=ncvdef(fileid,'wall_recyc_coef',6,1,nc_dims,nc_
     &stat)
      sc_exit_num_id=ncvdef(fileid,'sc_exit_num',4,0,nc_dims,nc_stat)
      exit_ind_id=ncddef(fileid,'exit_ind',((sc_exit_num)-(0)+1),nc_stat
     &)
      nc_dims(1)=exit_ind_id
      exit_sector_id=ncvdef(fileid,'exit_sector',4,1,nc_dims,nc_stat)
      sc_diagnostic_grps_id=ncvdef(fileid,'sc_diagnostic_grps',4,0,nc_di
     &ms,nc_stat)
      sc_diag_name_string_id=ncddef(fileid,'sc_diag_name_string',((40)-(
     &1)+1),nc_stat)
      diag_grp_ind_id=ncddef(fileid,'diag_grp_ind',((sc_diagnostic_grps)
     &-(0)+1),nc_stat)
      sc_diag_max_bins_id=ncvdef(fileid,'sc_diag_max_bins',4,0,nc_dims,n
     &c_stat)
      nc_dims(1)=sc_diag_name_string_id
      nc_dims(2)=diag_grp_ind_id
      diagnostic_grp_name_id=ncvdef(fileid,'diagnostic_grp_name',2,2,nc_
     &dims,nc_stat)
      nc_dims(1)=diag_grp_ind_id
      diagnostic_num_sectors_id=ncvdef(fileid,'diagnostic_num_sectors',4
     &,1,nc_dims,nc_stat)
      nc_dims(1)=diag_grp_ind_id
      diagnostic_var_id=ncvdef(fileid,'diagnostic_var',4,1,nc_dims,nc_st
     &at)
      nc_dims(1)=diag_grp_ind_id
      diagnostic_tab_index_id=ncvdef(fileid,'diagnostic_tab_index',4,1,n
     &c_dims,nc_stat)
      nc_dims(1)=diag_grp_ind_id
      diagnostic_min_id=ncvdef(fileid,'diagnostic_min',6,1,nc_dims,nc_st
     &at)
      nc_dims(1)=diag_grp_ind_id
      diagnostic_delta_id=ncvdef(fileid,'diagnostic_delta',6,1,nc_dims,n
     &c_stat)
      nc_dims(1)=diag_grp_ind_id
      diagnostic_spacing_id=ncvdef(fileid,'diagnostic_spacing',4,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=diag_grp_ind_id
      diagnostic_grp_base_id=ncvdef(fileid,'diagnostic_grp_base',4,1,nc_
     &dims,nc_stat)
      sc_diag_size_id=ncvdef(fileid,'sc_diag_size',4,0,nc_dims,nc_stat)
      sc_diag_ind_id=ncddef(fileid,'sc_diag_ind',((sc_diag_size-1)-(0)+1
     &),nc_stat)
      nc_dims(1)=sc_diag_ind_id
      diagnostic_sector_tab_id=ncvdef(fileid,'diagnostic_sector_tab',4,1
     &,nc_dims,nc_stat)
      
      de_symbol_string_id=ncddef(fileid,'de_symbol_string',((24)-(1)+1),
     &nc_stat)
      de_name_string_id=ncddef(fileid,'de_name_string',((100)-(1)+1),nc_
     &stat)
      de_grps_id=ncvdef(fileid,'de_grps',4,0,nc_dims,nc_stat)
      de_grp_ind_id=ncddef(fileid,'de_grp_ind',((de_grps)-(0)+1),nc_stat
     &)
      de_max_bins_id=ncvdef(fileid,'de_max_bins',4,0,nc_dims,nc_stat)
      de_zone_frags_dim_id=ncvdef(fileid,'de_zone_frags_dim',4,0,nc_dims
     &,nc_stat)
      de_zone_frags_ind_id=ncddef(fileid,'de_zone_frags_ind',((de_zone_f
     &rags_dim)-(1)+1),nc_stat)
      de_zone_frags_size_id=ncvdef(fileid,'de_zone_frags_size',4,0,nc_di
     &ms,nc_stat)
      nc_dims(1)=de_name_string_id
      nc_dims(2)=de_grp_ind_id
      detector_name_id=ncvdef(fileid,'detector_name',2,2,nc_dims,nc_stat
     &)
      nc_dims(1)=de_grp_ind_id
      detector_num_views_id=ncvdef(fileid,'detector_num_views',4,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=de_grp_ind_id
      detector_var_id=ncvdef(fileid,'detector_var',4,1,nc_dims,nc_stat)
      nc_dims(1)=de_grp_ind_id
      detector_tab_index_id=ncvdef(fileid,'detector_tab_index',4,1,nc_di
     &ms,nc_stat)
      nc_dims(1)=de_grp_ind_id
      detector_min_id=ncvdef(fileid,'detector_min',6,1,nc_dims,nc_stat)
      nc_dims(1)=de_grp_ind_id
      detector_delta_id=ncvdef(fileid,'detector_delta',6,1,nc_dims,nc_st
     &at)
      nc_dims(1)=de_grp_ind_id
      detector_spacing_id=ncvdef(fileid,'detector_spacing',4,1,nc_dims,n
     &c_stat)
      detector_total_views_id=ncvdef(fileid,'detector_total_views',4,0,n
     &c_dims,nc_stat)
      de_tot_view_ind_id=ncddef(fileid,'de_tot_view_ind',((detector_tota
     &l_views)-(0)+1),nc_stat)
      de_start_end_ind_id=ncddef(fileid,'de_start_end_ind',((1)-(0)+1),n
     &c_stat)
      nc_dims(1)=vector_id
      nc_dims(2)=de_start_end_ind_id
      nc_dims(3)=de_tot_view_ind_id
      de_view_points_id=ncvdef(fileid,'de_view_points',6,3,nc_dims,nc_st
     &at)
      nc_dims(1)=de_tot_view_ind_id
      de_view_algorithm_id=ncvdef(fileid,'de_view_algorithm',4,1,nc_dims
     &,nc_stat)
      nc_dims(1)=de_tot_view_ind_id
      de_view_halfwidth_id=ncvdef(fileid,'de_view_halfwidth',6,1,nc_dims
     &,nc_stat)
      nc_dims(1)=de_zone_frags_ind_id
      de_zone_frags_id=ncvdef(fileid,'de_zone_frags',6,1,nc_dims,nc_stat
     &)
      nc_dims(1)=de_tot_view_ind_id
      de_zone_frags_start_id=ncvdef(fileid,'de_zone_frags_start',4,1,nc_
     &dims,nc_stat)
      nc_dims(1)=de_tot_view_ind_id
      de_zone_frags_num_id=ncvdef(fileid,'de_zone_frags_num',4,1,nc_dims
     &,nc_stat)
      nc_dims(1)=de_zone_frags_ind_id
      de_zone_frags_zones_id=ncvdef(fileid,'de_zone_frags_zones',4,1,nc_
     &dims,nc_stat)
      nc_dims(1)=de_tot_view_ind_id
      de_zone_frags_min_zn_id=ncvdef(fileid,'de_zone_frags_min_zn',4,1,n
     &c_dims,nc_stat)
      nc_dims(1)=de_tot_view_ind_id
      de_zone_frags_max_zn_id=ncvdef(fileid,'de_zone_frags_max_zn',4,1,n
     &c_dims,nc_stat)
      nc_dims(1)=de_grp_ind_id
      de_view_base_id=ncvdef(fileid,'de_view_base',4,1,nc_dims,nc_stat)
      de_view_size_id=ncvdef(fileid,'de_view_size',4,0,nc_dims,nc_stat)
      de_view_ind_id=ncddef(fileid,'de_view_ind',((de_view_size-1)-(0)+1
     &),nc_stat)
      nc_dims(1)=de_view_ind_id
      de_view_tab_id=ncvdef(fileid,'de_view_tab',4,1,nc_dims,nc_stat)
      
      call ncendf(fileid,nc_stat)
      
      
      call ncvpt(fileid,ncells_id,nc_corner,nc_edge,ncells,nc_stat)
      call ncvpt(fileid,nsurfaces_id,nc_corner,nc_edge,nsurfaces,nc_stat
     &)
      call ncvpt(fileid,nboundaries_id,nc_corner,nc_edge,nboundaries,nc_
     &stat)
      call ncvpt(fileid,nneighbors_id,nc_corner,nc_edge,nneighbors,nc_st
     &at)
      call ncvpt(fileid,ntransforms_id,nc_corner,nc_edge,ntransforms,nc_
     &stat)
      call ncvpt(fileid,geometry_symmetry_id,nc_corner,nc_edge,geometry_
     &symmetry,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      call ncvpt(fileid,universal_cell_min_id,nc_corner,nc_edge,universa
     &l_cell_min,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      call ncvpt(fileid,universal_cell_max_id,nc_corner,nc_edge,universa
     &l_cell_max,nc_stat)
      call ncvpt(fileid,universal_cell_vol_id,nc_corner,nc_edge,universa
     &l_cell_vol,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((ncells)-(0)+1)
      call ncvpt(fileid,cells_id,nc_corner,nc_edge,cells,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((nsurfaces)-(1)+1)
      call ncvpt(fileid,surfaces_id,nc_corner,nc_edge,surfaces,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((nsurfaces)-(1)+1)
      call ncvpt(fileid,surfaces_tx_ind_id,nc_corner,nc_edge,surfaces_tx
     &_ind,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((4)-(1)+1)
      nc_corner(3)=1
      nc_edge(3)=((ntransforms)-(0)+1)
      call ncvpt(fileid,surfaces_tx_mx_id,nc_corner,nc_edge,surfaces_tx_
     &mx,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((1)-(0)+1)
      nc_corner(2)=1
      nc_edge(2)=((1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((nsurfaces)-(1)+1)
      call ncvpt(fileid,surface_sectors_id,nc_corner,nc_edge,surface_sec
     &tors,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((nboundaries)-(1)+1)
      call ncvpt(fileid,boundaries_id,nc_corner,nc_edge,boundaries,nc_st
     &at)
      nc_corner(1)=1
      nc_edge(1)=((nneighbors)-(0)+1)
      call ncvpt(fileid,neighbors_id,nc_corner,nc_edge,neighbors,nc_stat
     &)
      nc_corner(1)=1
      nc_edge(1)=((10)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((nsurfaces)-(1)+1)
      call ncvpt(fileid,surface_coeffs_id,nc_corner,nc_edge,surface_coef
     &fs,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((nsurfaces)-(1)+1)
      call ncvpt(fileid,surface_points_id,nc_corner,nc_edge,surface_poin
     &ts,nc_stat)
      
      call ncvpt(fileid,zn_num_id,nc_corner,nc_edge,zn_num,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      call ncvpt(fileid,zone_type_num_id,nc_corner,nc_edge,zone_type_num
     &,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((zn_num)-(1)+1)
      call ncvpt(fileid,zone_type_id,nc_corner,nc_edge,zone_type,nc_stat
     &)
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((zn_num)-(1)+1)
      call ncvpt(fileid,zone_index_id,nc_corner,nc_edge,zone_index,nc_st
     &at)
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      call ncvpt(fileid,zone_index_min_id,nc_corner,nc_edge,zone_index_m
     &in,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((4)-(1)+1)
      call ncvpt(fileid,zone_index_max_id,nc_corner,nc_edge,zone_index_m
     &ax,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((zn_num)-(1)+1)
      call ncvpt(fileid,zone_pointer_id,nc_corner,nc_edge,zone_pointer,n
     &c_stat)
      nc_corner(1)=1
      nc_edge(1)=((zn_num)-(1)+1)
      call ncvpt(fileid,zone_volume_id,nc_corner,nc_edge,zone_volume,nc_
     &stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((zn_num)-(1)+1)
      call ncvpt(fileid,zone_center_id,nc_corner,nc_edge,zone_center,nc_
     &stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((zn_num)-(1)+1)
      call ncvpt(fileid,zone_min_id,nc_corner,nc_edge,zone_min,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((zn_num)-(1)+1)
      call ncvpt(fileid,zone_max_id,nc_corner,nc_edge,zone_max,nc_stat)
      
      call ncvpt(fileid,nsectors_id,nc_corner,nc_edge,nsectors,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((nsectors)-(0)+1)
      call ncvpt(fileid,strata_id,nc_corner,nc_edge,strata,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((nsectors)-(0)+1)
      call ncvpt(fileid,sector_strata_segment_id,nc_corner,nc_edge,secto
     &r_strata_segment,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((nsectors)-(0)+1)
      call ncvpt(fileid,sectors_id,nc_corner,nc_edge,sectors,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((nsectors)-(0)+1)
      call ncvpt(fileid,sector_zone_id,nc_corner,nc_edge,sector_zone,nc_
     &stat)
      nc_corner(1)=1
      nc_edge(1)=((nsectors)-(0)+1)
      call ncvpt(fileid,sector_surface_id,nc_corner,nc_edge,sector_surfa
     &ce,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((nsectors)-(0)+1)
      call ncvpt(fileid,sector_points_id,nc_corner,nc_edge,sector_points
     &,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((17)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((nsectors)-(0)+1)
      call ncvpt(fileid,sector_type_pointer_id,nc_corner,nc_edge,sector_
     &type_pointer,nc_stat)
      call ncvpt(fileid,sc_vacuum_num_id,nc_corner,nc_edge,sc_vacuum_num
     &,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_vacuum_num)-(0)+1)
      call ncvpt(fileid,vacuum_sector_id,nc_corner,nc_edge,vacuum_sector
     &,nc_stat)
      call ncvpt(fileid,sc_plasma_num_id,nc_corner,nc_edge,sc_plasma_num
     &,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_plasma_num)-(0)+1)
      call ncvpt(fileid,plasma_sector_id,nc_corner,nc_edge,plasma_sector
     &,nc_stat)
      call ncvpt(fileid,sc_target_num_id,nc_corner,nc_edge,sc_target_num
     &,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_target_num)-(0)+1)
      call ncvpt(fileid,target_sector_id,nc_corner,nc_edge,target_sector
     &,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_target_num)-(0)+1)
      call ncvpt(fileid,target_material_id,nc_corner,nc_edge,target_mate
     &rial,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_target_num)-(0)+1)
      call ncvpt(fileid,target_temperature_id,nc_corner,nc_edge,target_t
     &emperature,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_target_num)-(0)+1)
      call ncvpt(fileid,target_recyc_coef_id,nc_corner,nc_edge,target_re
     &cyc_coef,nc_stat)
      call ncvpt(fileid,sc_wall_num_id,nc_corner,nc_edge,sc_wall_num,nc_
     &stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_wall_num)-(0)+1)
      call ncvpt(fileid,wall_sector_id,nc_corner,nc_edge,wall_sector,nc_
     &stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_wall_num)-(0)+1)
      call ncvpt(fileid,wall_material_id,nc_corner,nc_edge,wall_material
     &,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_wall_num)-(0)+1)
      call ncvpt(fileid,wall_temperature_id,nc_corner,nc_edge,wall_tempe
     &rature,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_wall_num)-(0)+1)
      call ncvpt(fileid,wall_recyc_coef_id,nc_corner,nc_edge,wall_recyc_
     &coef,nc_stat)
      call ncvpt(fileid,sc_exit_num_id,nc_corner,nc_edge,sc_exit_num,nc_
     &stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_exit_num)-(0)+1)
      call ncvpt(fileid,exit_sector_id,nc_corner,nc_edge,exit_sector,nc_
     &stat)
      call ncvpt(fileid,sc_diagnostic_grps_id,nc_corner,nc_edge,sc_diagn
     &ostic_grps,nc_stat)
      call ncvpt(fileid,sc_diag_max_bins_id,nc_corner,nc_edge,sc_diag_ma
     &x_bins,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((40)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((sc_diagnostic_grps)-(0)+1)
      call ncvptc(fileid,diagnostic_grp_name_id,nc_corner,nc_edge,diagno
     &stic_grp_name,(((40)-(1)+1)*((sc_diagnostic_grps)-(0)+1)),nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_diagnostic_grps)-(0)+1)
      call ncvpt(fileid,diagnostic_num_sectors_id,nc_corner,nc_edge,diag
     &nostic_num_sectors,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_diagnostic_grps)-(0)+1)
      call ncvpt(fileid,diagnostic_var_id,nc_corner,nc_edge,diagnostic_v
     &ar,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_diagnostic_grps)-(0)+1)
      call ncvpt(fileid,diagnostic_tab_index_id,nc_corner,nc_edge,diagno
     &stic_tab_index,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_diagnostic_grps)-(0)+1)
      call ncvpt(fileid,diagnostic_min_id,nc_corner,nc_edge,diagnostic_m
     &in,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_diagnostic_grps)-(0)+1)
      call ncvpt(fileid,diagnostic_delta_id,nc_corner,nc_edge,diagnostic
     &_delta,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_diagnostic_grps)-(0)+1)
      call ncvpt(fileid,diagnostic_spacing_id,nc_corner,nc_edge,diagnost
     &ic_spacing,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_diagnostic_grps)-(0)+1)
      call ncvpt(fileid,diagnostic_grp_base_id,nc_corner,nc_edge,diagnos
     &tic_grp_base,nc_stat)
      call ncvpt(fileid,sc_diag_size_id,nc_corner,nc_edge,sc_diag_size,n
     &c_stat)
      nc_corner(1)=1
      nc_edge(1)=((sc_diag_size-1)-(0)+1)
      call ncvpt(fileid,diagnostic_sector_tab_id,nc_corner,nc_edge,diagn
     &ostic_sector_tab,nc_stat)
      
      call ncvpt(fileid,de_grps_id,nc_corner,nc_edge,de_grps,nc_stat)
      call ncvpt(fileid,de_max_bins_id,nc_corner,nc_edge,de_max_bins,nc_
     &stat)
      call ncvpt(fileid,de_zone_frags_dim_id,nc_corner,nc_edge,de_zone_f
     &rags_dim,nc_stat)
      call ncvpt(fileid,de_zone_frags_size_id,nc_corner,nc_edge,de_zone_
     &frags_size,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((100)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((de_grps)-(0)+1)
      call ncvptc(fileid,detector_name_id,nc_corner,nc_edge,detector_nam
     &e,(((100)-(1)+1)*((de_grps)-(0)+1)),nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((de_grps)-(0)+1)
      call ncvpt(fileid,detector_num_views_id,nc_corner,nc_edge,detector
     &_num_views,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((de_grps)-(0)+1)
      call ncvpt(fileid,detector_var_id,nc_corner,nc_edge,detector_var,n
     &c_stat)
      nc_corner(1)=1
      nc_edge(1)=((de_grps)-(0)+1)
      call ncvpt(fileid,detector_tab_index_id,nc_corner,nc_edge,detector
     &_tab_index,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((de_grps)-(0)+1)
      call ncvpt(fileid,detector_min_id,nc_corner,nc_edge,detector_min,n
     &c_stat)
      nc_corner(1)=1
      nc_edge(1)=((de_grps)-(0)+1)
      call ncvpt(fileid,detector_delta_id,nc_corner,nc_edge,detector_del
     &ta,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((de_grps)-(0)+1)
      call ncvpt(fileid,detector_spacing_id,nc_corner,nc_edge,detector_s
     &pacing,nc_stat)
      call ncvpt(fileid,detector_total_views_id,nc_corner,nc_edge,detect
     &or_total_views,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((3)-(1)+1)
      nc_corner(2)=1
      nc_edge(2)=((1)-(0)+1)
      nc_corner(3)=1
      nc_edge(3)=((detector_total_views)-(0)+1)
      call ncvpt(fileid,de_view_points_id,nc_corner,nc_edge,de_view_poin
     &ts,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((detector_total_views)-(0)+1)
      call ncvpt(fileid,de_view_algorithm_id,nc_corner,nc_edge,de_view_a
     &lgorithm,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((detector_total_views)-(0)+1)
      call ncvpt(fileid,de_view_halfwidth_id,nc_corner,nc_edge,de_view_h
     &alfwidth,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((de_zone_frags_dim)-(1)+1)
      call ncvpt(fileid,de_zone_frags_id,nc_corner,nc_edge,de_zone_frags
     &,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((detector_total_views)-(0)+1)
      call ncvpt(fileid,de_zone_frags_start_id,nc_corner,nc_edge,de_zone
     &_frags_start,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((detector_total_views)-(0)+1)
      call ncvpt(fileid,de_zone_frags_num_id,nc_corner,nc_edge,de_zone_f
     &rags_num,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((de_zone_frags_dim)-(1)+1)
      call ncvpt(fileid,de_zone_frags_zones_id,nc_corner,nc_edge,de_zone
     &_frags_zones,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((detector_total_views)-(0)+1)
      call ncvpt(fileid,de_zone_frags_min_zn_id,nc_corner,nc_edge,de_zon
     &e_frags_min_zn,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((detector_total_views)-(0)+1)
      call ncvpt(fileid,de_zone_frags_max_zn_id,nc_corner,nc_edge,de_zon
     &e_frags_max_zn,nc_stat)
      nc_corner(1)=1
      nc_edge(1)=((de_grps)-(0)+1)
      call ncvpt(fileid,de_view_base_id,nc_corner,nc_edge,de_view_base,n
     &c_stat)
      call ncvpt(fileid,de_view_size_id,nc_corner,nc_edge,de_view_size,n
     &c_stat)
      nc_corner(1)=1
      nc_edge(1)=((de_view_size-1)-(0)+1)
      call ncvpt(fileid,de_view_tab_id,nc_corner,nc_edge,de_view_tab,nc_
     &stat)
      
      call ncclos(fileid,nc_stat)
      return
      end
      
      
