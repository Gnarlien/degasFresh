      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine get_pixel_mesh(alen,blen,clen,pixsize,z_scale,na,nb,nc,
     &dx,dz)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)alen,blen,clen,z_scale
      integer pixsize
      REAL(kind=DOUBLE)dx,dz
      integer na,nb,nc
      if(alen.GT.0.AND.blen.GT.0)continue
      dx=sqrt(alen*blen/REAL(pixsize,DOUBLE))
      na=int(alen/dx)
      nb=int(blen/dx)
      if(na*nb.LE.pixsize)continue
      if(clen.GT.(0.0_DOUBLE))then
      dz=z_scale*dx
      nc=int(clen/dz)
      if(nc.GT.1)continue
      else
      nc=1
      dz=(0.0_DOUBLE)
      end if
      return
      end
      subroutine set_pixel_zones(x0,a,b,c,na,nb,nc,dx,dz,xa,xb,xc,pixel_
     &zones)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)dx,dz
      REAL(kind=DOUBLE)x0(3),a(3),b(3),c(3)
      integer na,nb,nc
      REAL(kind=DOUBLE)xa(0:na-1),xb(0:nb-1),xc(0:nc-1)
      integer pixel_zones(0:na*nb*nc-1)
      integer i,j,k,ind,direction,istart,istop,icurrent,inext,uck,outsid
     &e_u_c
      REAL(kind=DOUBLE)vx(3),vy(3),vz(3),x_tmp(3),tmax,t,xpos,tuc
      logical done
      integer zone
      REAL(kind=DOUBLE)pos_xx(3)
      integer cell_xx,zone_xx,surface_xx,cell_next_xx,zone_next_xx,secto
     &r_xx,sector_next_xx
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
      
      
      REAL(kind=DOUBLE)vector_temp(3)
      external locate_point,check_location
      integer locate_point
      logical check_location
      external track,cell_enter
      logical track
      REAL(kind=DOUBLE)cell_enter
      if(nc.GT.1)then
      vector_temp(1)=sqrt((c(1)**2+c(2)**2+c(3)**2))
      if(vector_temp(1).GT.(0.0_DOUBLE))continue
      vz(1)=((1.0_DOUBLE)/vector_temp(1))*c(1)
      vz(2)=((1.0_DOUBLE)/vector_temp(1))*c(2)
      vz(3)=((1.0_DOUBLE)/vector_temp(1))*c(3)
      
      else
      vz(1)=(0.0_DOUBLE)
      vz(2)=(0.0_DOUBLE)
      vz(3)=(0.0_DOUBLE)
      end if
      do j=0,na*nb*nc-1
      pixel_zones(j)=0
      end do
      vector_temp(1)=sqrt((a(1)**2+a(2)**2+a(3)**2))
      if(vector_temp(1).GT.(0.0_DOUBLE))continue
      vx(1)=((1.0_DOUBLE)/vector_temp(1))*a(1)
      vx(2)=((1.0_DOUBLE)/vector_temp(1))*a(2)
      vx(3)=((1.0_DOUBLE)/vector_temp(1))*a(3)
      
      vector_temp(1)=sqrt((b(1)**2+b(2)**2+b(3)**2))
      if(vector_temp(1).GT.(0.0_DOUBLE))continue
      vy(1)=((1.0_DOUBLE)/vector_temp(1))*b(1)
      vy(2)=((1.0_DOUBLE)/vector_temp(1))*b(2)
      vy(3)=((1.0_DOUBLE)/vector_temp(1))*b(3)
      
      do j=0,na-1
      x_tmp(1)=x0(1)+vx(1)*(dx*(float(j)+(0.5_DOUBLE)))
      x_tmp(2)=x0(2)+vx(2)*(dx*(float(j)+(0.5_DOUBLE)))
      x_tmp(3)=x0(3)+vx(3)*(dx*(float(j)+(0.5_DOUBLE)))
      
      xa(j)=(x_tmp(1)*vx(1)+x_tmp(2)*vx(2)+x_tmp(3)*vx(3))
      end do
      do j=0,nb-1
      x_tmp(1)=x0(1)+vy(1)*(dx*(float(j)+(0.5_DOUBLE)))
      x_tmp(2)=x0(2)+vy(2)*(dx*(float(j)+(0.5_DOUBLE)))
      x_tmp(3)=x0(3)+vy(3)*(dx*(float(j)+(0.5_DOUBLE)))
      
      xb(j)=(x_tmp(1)*vy(1)+x_tmp(2)*vy(2)+x_tmp(3)*vy(3))
      end do
      do j=0,nc-1
      x_tmp(1)=x0(1)+vz(1)*(dz*(float(j)+(0.5_DOUBLE)))
      x_tmp(2)=x0(2)+vz(2)*(dz*(float(j)+(0.5_DOUBLE)))
      x_tmp(3)=x0(3)+vz(3)*(dz*(float(j)+(0.5_DOUBLE)))
      
      xc(j)=(x_tmp(1)*vz(1)+x_tmp(2)*vz(2)+x_tmp(3)*vz(3))
      end do
      do k=0,nc-1
      pos_xx(1)=x0(1)+vy(1)*((0.5_DOUBLE)*dx)
      pos_xx(2)=x0(2)+vy(2)*((0.5_DOUBLE)*dx)
      pos_xx(3)=x0(3)+vy(3)*((0.5_DOUBLE)*dx)
      
      if(nc.GT.1)then
      pos_xx(1)=pos_xx(1)+vz(1)*((float(k)+(0.5_DOUBLE))*dz)
      pos_xx(2)=pos_xx(2)+vz(2)*((float(k)+(0.5_DOUBLE))*dz)
      pos_xx(3)=pos_xx(3)+vz(3)*((float(k)+(0.5_DOUBLE))*dz)
      
      end if
      cell_xx=locate_point(pos_xx,zone_xx)
      surface_xx=0
      if(cell_xx.LE.0)then
      outside_u_c=1
      else
      outside_u_c=0
      end if
      vector_temp(1)=sqrt((a(1)**2+a(2)**2+a(3)**2))
      if(vector_temp(1).GT.(0.0_DOUBLE))continue
      vx(1)=((1.0_DOUBLE)/vector_temp(1))*a(1)
      vx(2)=((1.0_DOUBLE)/vector_temp(1))*a(2)
      vx(3)=((1.0_DOUBLE)/vector_temp(1))*a(3)
      
      direction=1
      xpos=(0.0_DOUBLE)
      istop=-1
      do j=0,nb-1
      tmax=REAL(na,DOUBLE)*dx
90000 continue
      if(cell_xx.LE.0)then
      if(outside_u_c.EQ.1)continue
      if(direction.GT.0)then
      icurrent=istop+1
      inext=min(icurrent+1,na)
      else
      icurrent=istop
      inext=max(icurrent-1,0)
      end if
      t=direction*(REAL(inext,DOUBLE)*dx-xpos)
      pos_xx(1)=pos_xx(1)+vx(1)*(t)
      pos_xx(2)=pos_xx(2)+vx(2)*(t)
      pos_xx(3)=pos_xx(3)+vx(3)*(t)
      
      cell_xx=locate_point(pos_xx,zone_xx)
      surface_xx=0
      zone=0
      if(cell_xx.GT.0)then
      tuc=cell_enter(0,pos_xx,vx,uck)
      if((tuc.LT.(0.0_DOUBLE)).AND.(abs(tuc).LT.dx))then
      if(outside_u_c.EQ.1)continue
      outside_u_c=0
      else
      if(' Improper entry into universal cell'.EQ.' ')continue
      end if
      if(abs(tuc)/dx.GT.(0.5_DOUBLE))zone=zone_xx
      end if
      if(abs(tmax-t).LE.(1.0e-8_DOUBLE))then
      done=.TRUE.
      t=tmax
      else
      done=.FALSE.
      endif
      else
      done=track(0,tmax,pos_xx,vx,cell_xx,t,pos_xx,cell_xx,surface_xx,ce
     &ll_next_xx,sector_xx,sector_next_xx)
      if(cell_next_xx.GE.0)continue
      if(cell_next_xx.EQ.0)then
      if(outside_u_c.EQ.0)continue
      if(cell_xx.GT.0)continue
      if(surfaces((sign(1,-surface_xx)+1)/2,1,abs(surface_xx)).EQ.0)cont
     &inue
      if(surfaces((sign(1,surface_xx)+1)/2,1,abs(surface_xx)).GT.0)conti
     &nue
      outside_u_c=1
      end if
      if(surface_xx.NE.0)then
      zone_next_xx=cells(4,cell_next_xx)
      end if
      zone=zone_xx
      if(surface_xx.NE.0)then
      surface_xx=0
      cell_xx=cell_next_xx
      zone_xx=zone_next_xx
      sector_xx=sector_next_xx
      end if
      end if
      istart=int(xpos/dx+(0.5_DOUBLE))
      xpos=xpos+direction*t
      istop=int(xpos/dx+(0.5_DOUBLE))
      if(direction.GT.0)then
      istop=istop-1
      else
      istart=istart-1
      end if
      if(direction*(istop-istart).GE.0)then
      do i=istart,istop,direction
      ind=(k*nb+j)*na+i
      pixel_zones(ind)=zone
      end do
      end if
      tmax=tmax-t
      if(.NOT.done)then
      goto 90000
      end if
      if(tmax.EQ.(0.0_DOUBLE))continue
      tmax=dx
90001 continue
      if(cell_xx.LE.0)then
      if(outside_u_c.EQ.1)continue
      pos_xx(1)=pos_xx(1)+vy(1)*(tmax)
      pos_xx(2)=pos_xx(2)+vy(2)*(tmax)
      pos_xx(3)=pos_xx(3)+vy(3)*(tmax)
      
      tmax=(0.0_DOUBLE)
      cell_xx=locate_point(pos_xx,zone_xx)
      surface_xx=0
      if(cell_xx.GT.0)then
      tuc=cell_enter(0,pos_xx,vy,uck)
      if((tuc.LT.(0.0_DOUBLE)).AND.(abs(tuc).LT.dx))then
      if(outside_u_c.EQ.1)continue
      outside_u_c=0
      else
      if(' Improper entry into universal cell'.EQ.' ')continue
      end if
      end if
      else
      done=track(0,tmax,pos_xx,vy,cell_xx,t,pos_xx,cell_xx,surface_xx,ce
     &ll_next_xx,sector_xx,sector_next_xx)
      if(cell_next_xx.GE.0)continue
      if(cell_next_xx.EQ.0)then
      if(outside_u_c.EQ.0)continue
      if(cell_xx.GT.0)continue
      if(surfaces((sign(1,-surface_xx)+1)/2,1,abs(surface_xx)).EQ.0)cont
     &inue
      if(surfaces((sign(1,surface_xx)+1)/2,1,abs(surface_xx)).GT.0)conti
     &nue
      outside_u_c=1
      end if
      if(surface_xx.NE.0)then
      zone_next_xx=cells(4,cell_next_xx)
      end if
      tmax=tmax-t
      if(surface_xx.NE.0)then
      surface_xx=0
      cell_xx=cell_next_xx
      zone_xx=zone_next_xx
      sector_xx=sector_next_xx
      end if
      if(.NOT.done)then
      goto 90001
      end if
      end if
      if(tmax.EQ.(0.0_DOUBLE))continue
      direction=-direction
      if(direction.GT.0)then
      if(abs(xpos).LE.(1.0e-8_DOUBLE))continue
      xpos=(0.0_DOUBLE)
      istop=-1
      else
      if(abs(xpos-REAL(na,DOUBLE)*dx).LE.(1.0e-8_DOUBLE))continue
      xpos=REAL(na,DOUBLE)*dx
      istop=na
      end if
      vx(1)=(-(1.0_DOUBLE))*vx(1)
      vx(2)=(-(1.0_DOUBLE))*vx(2)
      vx(3)=(-(1.0_DOUBLE))*vx(3)
      
      end do
      end do
      return
      end
      subroutine write_hdf(hdffile,zone_data,clabel,cunits,cformt,na,nb,
     &nc,xa,xb,xc,pixel_zones,pixel_data)
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*(*)hdffile,clabel,cunits,cformt
      integer na,nb,nc
      integer pixel_zones(0:na*nb*nc-1)
      REAL(kind=DOUBLE)zone_data(zn_num),xa(0:na-1),xb(0:nb-1),xc(0:nc-1
     &),pixel_data(0:na*nb*nc-1)
      REAL(kind=DOUBLE)time_slice
      integer i,rank,ret,ret2,var_opts,time_ind,dbfile_local
      integer dims(3)
      integer j,ind
      REAL(kind=DOUBLE)avg
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      include 'silo.inc'
      integer dbfile,dbfile_td
      logical write_mesh
      character*96 mesh_name,var_name
      common/silo_common/dbfile,dbfile_td,write_mesh,mesh_name
      do i=0,na*nb*nc-1
      if(pixel_zones(i).NE.0)then
      pixel_data(i)=zone_data(pixel_zones(i))
      else
      pixel_data(i)=(0.0_DOUBLE)
      end if
      end do
      dims(1)=na
      dims(2)=nb
      if(nc.EQ.1)then
      rank=2
      else
      if(nc.GT.1)continue
      rank=3
      dims(3)=nc
      end if
      ret=dbmkoptlist(1,var_opts)
      if(ret.NE.-1)continue
      if(len(cunits).GT.0)then
      ret=dbaddcopt(var_opts,DBOPT_UNITS,cunits,len(cunits))
      if(ret.NE.-1)continue
      end if
      i=index(hdffile,'.hdf')
      time_ind=index(hdffile,'$')
      if(time_ind.GT.0)then
      dbfile_local=dbfile_td
      time_slice=REAL(read_integer(hdffile(time_ind+1:i-1)),DOUBLE)
      ret=dbadddopt(var_opts,DBOPT_DTIME,time_slice)
      if(ret.NE.-1)continue
      var_name=hdffile(:time_ind-1)
      else
      dbfile_local=dbfile
      var_name=hdffile(:i-1)
      end if
      if(write_mesh)then
      if(nc.EQ.1)then
      ret=dbputqm(dbfile_local,mesh_name  (1:string_length(mesh_name)),s
     &tring_length(mesh_name),'x',1,'y',1,'z',1,xa(0),xb(0),DB_F77NULL,d
     &ims,rank,DB_DOUBLE,DB_COLLINEAR,DB_F77NULL,ret2)
      else
      ret=dbputqm(dbfile_local,mesh_name  (1:string_length(mesh_name)),s
     &tring_length(mesh_name),'x',1,'y',1,'z',1,xa(0),xb(0),xc(0),dims,r
     &ank,DB_DOUBLE,DB_COLLINEAR,DB_F77NULL,ret2)
      end if
      if((ret.NE.-1).AND.(ret2.NE.-1))continue
      write_mesh=.FALSE.
      end if
      ret=dbputqv1(dbfile_local,var_name  (1:string_length(var_name)),st
     &ring_length(var_name),mesh_name  (1:string_length(mesh_name)),stri
     &ng_length(mesh_name),pixel_data(0),dims,rank,DB_F77NULL,0,DB_DOUBL
     &E,DB_NODECENT,var_opts,ret2)
      if((ret.NE.-1).AND.(ret2.NE.-1))continue
      if((var_name  (1:string_length(var_name)).EQ.'spDden').OR.(var_nam
     &e  (1:string_length(var_name)).EQ.'spD2den').OR.((var_name(1:6)).E
     &Q.'halpha'.OR.(var_name(4:6).EQ.'flx').OR.(var_name(5:7).EQ.'flx')
     &.OR.(var_name  (1:string_length(var_name)).EQ.'bk_t2').OR.(var_nam
     &e  (1:string_length(var_name)).EQ.'bk_n1').OR.(var_name  (1:string
     &_length(var_name)).EQ.'bk_t1').OR.(var_name  (1:string_length(var_
     &name)).EQ.'DpionizeV')))then
      open(unit=31,file=var_name  (1:string_length(var_name))//'1D.txt',
     &status='unknown')
      do i=0,na-1
      avg=(0.0_DOUBLE)
      do j=0,nb-1
      ind=j*na+i
      avg=avg+(pixel_data(ind)/REAL(nb,DOUBLE))
      end do
      write(31,'(2x,i3,2x,f8.5,2x,1pe13.5)')i,xa(i),avg
      end do
      close(unit=31)
      end if
      return
      end
      subroutine pixel_map_test
      
      
      
      
      
      
      
      
      
      
      
      use gi_mod
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)alen,blen,clen,z_scale,r_min,r_max,phi_min,phi_ma
     &x,x_min,x_max,y_min,y_max,dx,dz
      integer pixsize,na,nb,nc,i,nq,q_min,iq,q_next,q_max
      REAL(kind=DOUBLE)x(3)
      REAL(kind=DOUBLE)a(3)
      REAL(kind=DOUBLE)b(3)
      REAL(kind=DOUBLE)c(3)
      REAL(kind=DOUBLE),dimension(:),pointer::xa
      REAL(kind=DOUBLE),dimension(:),pointer::xb
      REAL(kind=DOUBLE),dimension(:),pointer::xc
      integer,dimension(:),pointer::pixel_zones
      REAL(kind=DOUBLE),dimension(:),pointer::pixel_data
      REAL(kind=DOUBLE),dimension(:),pointer::zone_data
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
      
      
      REAL(kind=DOUBLE)vector_temp(3)
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      include 'silo.inc'
      integer dbfile,dbfile_td,ret
      logical write_mesh
      character*96 mesh_name,silo_file
      common/silo_common/dbfile,dbfile_td,write_mesh,mesh_name
      silo_file='geomtestc.silo'
      ret=dbcreate(silo_file  (1:string_length(silo_file)),string_length
     &(silo_file),DB_CLOBBER,DB_LOCAL,DB_F77NULL,0,DB_PDB,dbfile)
      if((ret.NE.-1).AND.(dbfile.NE.-1))continue
      if(geometry_symmetry.EQ.3)then
      x(1)=universal_cell_min(1)
      x(2)=universal_cell_min(2)
      x(3)=universal_cell_min(3)
      a(1)=(1.0_DOUBLE)
      a(2)=(0.0_DOUBLE)
      a(3)=(0.0_DOUBLE)
      alen=universal_cell_max(1)-universal_cell_min(1)
      b(1)=(0.0_DOUBLE)
      b(2)=(0.0_DOUBLE)
      b(3)=(1.0_DOUBLE)
      blen=universal_cell_max(3)-universal_cell_min(3)
      c(1)=(0.0_DOUBLE)
      c(2)=(0.0_DOUBLE)
      c(3)=(0.0_DOUBLE)
      clen=(0.0_DOUBLE)
      pixsize=1000
      z_scale=(0.0_DOUBLE)
      else if((geometry_symmetry.EQ.1).OR.(geometry_symmetry.EQ.2))then
      x(1)=universal_cell_min(1)
      x(2)=universal_cell_min(2)
      x(3)=universal_cell_min(3)
      a(1)=(1.0_DOUBLE)
      a(2)=(0.0_DOUBLE)
      a(3)=(0.0_DOUBLE)
      alen=universal_cell_max(1)-universal_cell_min(1)
      b(1)=(0.0_DOUBLE)
      b(2)=(0.0_DOUBLE)
      b(3)=(1.0_DOUBLE)
      blen=universal_cell_max(3)-universal_cell_min(3)
      c(1)=(0.0_DOUBLE)
      c(2)=(0.0_DOUBLE)
      c(3)=(0.0_DOUBLE)
      clen=(0.0_DOUBLE)
      pixsize=40000
      z_scale=(0.0_DOUBLE)
      else if(geometry_symmetry.EQ.4)then
      x(1)=universal_cell_min(1)
      a(1)=(1.0_DOUBLE)
      a(2)=(0.0_DOUBLE)
      a(3)=(0.0_DOUBLE)
      alen=universal_cell_max(1)-universal_cell_min(1)
      x(2)=universal_cell_min(2)
      b(1)=(0.0_DOUBLE)
      b(2)=(1.0_DOUBLE)
      b(3)=(0.0_DOUBLE)
      blen=universal_cell_max(2)-universal_cell_min(2)
      x(3)=universal_cell_min(3)
      c(1)=(0.0_DOUBLE)
      c(2)=(0.0_DOUBLE)
      c(3)=(1.0_DOUBLE)
      clen=universal_cell_max(3)-universal_cell_min(3)
      pixsize=40000
      z_scale=(1.0_DOUBLE)
      else if(geometry_symmetry.EQ.5)then
      x(1)=-universal_cell_max(1)
      a(1)=(1.0_DOUBLE)
      a(2)=(0.0_DOUBLE)
      a(3)=(0.0_DOUBLE)
      alen=(2.0_DOUBLE)*universal_cell_max(1)
      x(2)=x(1)
      b(1)=(0.0_DOUBLE)
      b(2)=(1.0_DOUBLE)
      b(3)=(0.0_DOUBLE)
      blen=alen
      x(3)=universal_cell_min(3)
      c(1)=(0.0_DOUBLE)
      c(2)=(0.0_DOUBLE)
      c(3)=(1.0_DOUBLE)
      clen=universal_cell_max(3)-universal_cell_min(3)
      pixsize=40000
      z_scale=(1.0_DOUBLE)
      else if(geometry_symmetry.EQ.6)then
      r_min=universal_cell_min(1)
      r_max=universal_cell_max(1)
      phi_min=universal_cell_min(2)
      phi_max=universal_cell_max(2)
      x_min=min(r_min*cos(phi_min),r_min*cos(phi_max),r_max*cos(phi_min)
     &,r_max*cos(phi_max))
      x_max=max(r_min*cos(phi_min),r_min*cos(phi_max),r_max*cos(phi_min)
     &,r_max*cos(phi_max))
      y_min=min(r_min*sin(phi_min),r_min*sin(phi_max),r_max*sin(phi_min)
     &,r_max*sin(phi_max))
      y_max=max(r_min*sin(phi_min),r_min*sin(phi_max),r_max*sin(phi_min)
     &,r_max*sin(phi_max))
      if((cos(phi_min).GT.(0.0_DOUBLE)).AND.(sin(phi_min).GE.(0.0_DOUBLE
     &)))then
      q_min=1
      else if((cos(phi_min).LE.(0.0_DOUBLE)).AND.(sin(phi_min).GT.(0.0_D
     &OUBLE)))then
      q_min=2
      else if((cos(phi_min).LT.(0.0_DOUBLE)).AND.(sin(phi_min).LE.(0.0_D
     &OUBLE)))then
      q_min=3
      else if((cos(phi_min).GE.(0.0_DOUBLE)).AND.(sin(phi_min).LT.(0.0_D
     &OUBLE)))then
      q_min=4
      else
      if(' Unable to located quadrant for phi_min'.EQ.' ')continue
      end if
      if((cos(phi_max).GT.(0.0_DOUBLE)).AND.(sin(phi_max).GE.(0.0_DOUBLE
     &)))then
      q_max=1
      else if((cos(phi_max).LE.(0.0_DOUBLE)).AND.(sin(phi_max).GT.(0.0_D
     &OUBLE)))then
      q_max=2
      else if((cos(phi_max).LT.(0.0_DOUBLE)).AND.(sin(phi_max).LE.(0.0_D
     &OUBLE)))then
      q_max=3
      else if((cos(phi_max).GE.(0.0_DOUBLE)).AND.(sin(phi_max).LT.(0.0_D
     &OUBLE)))then
      q_max=4
      else
      if(' Unable to located quadrant for phi_max'.EQ.' ')continue
      end if
      nq=q_max-q_min+1
      if(nq.LE.0)nq=nq+4
      if(nq.GE.1)continue
      if(nq.GT.1)then
      
      do iq=1,nq-1
      q_next=mod(q_min-1+iq,4)+1
      if(q_next.EQ.1)then
      x_max=r_max
      else if(q_next.EQ.2)then
      y_max=r_max
      else if(q_next.EQ.3)then
      x_min=-r_max
      else if(q_next.EQ.4)then
      y_min=-r_max
      else
      if(' Unexpected value of q_next'.EQ.' ')continue
      end if
      end do
      end if
      x(1)=x_min
      a(1)=(1.0_DOUBLE)
      a(2)=(0.0_DOUBLE)
      a(3)=(0.0_DOUBLE)
      alen=x_max-x_min
      x(2)=y_min
      b(1)=(0.0_DOUBLE)
      b(2)=(1.0_DOUBLE)
      b(3)=(0.0_DOUBLE)
      blen=y_max-y_min
      x(3)=universal_cell_min(3)
      c(1)=(0.0_DOUBLE)
      c(2)=(0.0_DOUBLE)
      c(3)=(1.0_DOUBLE)
      clen=universal_cell_max(3)-universal_cell_min(3)
      pixsize=40000
      z_scale=(1.0_DOUBLE)
      else
      if(' Unexpected value of geometry_symmetry'.EQ.' ')continue
      end if
      x(1)=x(1)+((1.0e-8_DOUBLE))
      alen=alen-((2.0_DOUBLE)*(1.0e-8_DOUBLE))
      call get_pixel_mesh(alen,blen,clen,pixsize,z_scale,na,nb,nc,dx,dz)
      xa =>mem_alloc_r1((0),(na-1),'xa')
      xb =>mem_alloc_r1((0),(nb-1),'xb')
      xc =>mem_alloc_r1((0),(nc-1),'xc')
      pixel_zones =>mem_alloc_i1((0),(na*nb*nc-1),'pixel_zones')
      pixel_data =>mem_alloc_r1((0),(na*nb*nc-1),'pixel_data')
      zone_data =>mem_alloc_r1((1),(zn_num),'zone_data')
      call set_pixel_zones(x,a,b,c,na,nb,nc,dx,dz,xa,xb,xc,pixel_zones)
      write_mesh=.TRUE.
      mesh_name='geomtestc2d'
      do i=1,zn_num
      zone_data(i)=REAL(3+mod(97*i,255-3),DOUBLE)
      end do
      call write_hdf('geomtestc.hdf',zone_data,'zone function',' ','E11.
     &3',na,nb,nc,xa,xb,xc,pixel_zones,pixel_data)
      call mem_free_r1(xa,(0),(na-1),'xa')
      call mem_free_r1(xb,(0),(nb-1),'xb')
      call mem_free_r1(xc,(0),(nc-1),'xc')
      call mem_free_i1(pixel_zones,(0),(na*nb*nc-1),'pixel_zones')
      call mem_free_r1(pixel_data,(0),(na*nb*nc-1),'pixel_data')
      call mem_free_r1(zone_data,(1),(zn_num),'zone_data')
      ret=dbclose(dbfile)
      if(ret.NE.-1)continue
      return
      end
      
      
