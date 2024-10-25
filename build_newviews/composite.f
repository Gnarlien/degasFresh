      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      function segment_intersect(x1,x2)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical segment_intersect
      REAL(kind=DOUBLE)x1(2,2),x2(2,2)
      REAL(kind=DOUBLE)d,f11,f12,f21,f22
      d=(x1(1,2)-x1(1,1))*(x2(2,2)-x2(2,1))-(x2(1,2)-x2(1,1))*(x1(2,2)-x
     &1(2,1))
      if(d.EQ.(0.0_DOUBLE))then
      segment_intersect=.FALSE.
      return
      end if
      
      f11=(x2(1,1)-x1(1,1))*x2(2,2)+(x1(1,1)-x2(1,2))*x2(2,1)+(x2(1,2)-x
     &2(1,1))*x1(2,1)
      f12=(x1(1,2)-x2(1,1))*x2(2,2)+(x2(1,1)-x2(1,2))*x1(2,2)+(x2(1,2)-x
     &1(1,2))*x2(2,1)
      f21=(x2(1,1)-x1(1,1))*x1(2,2)+(x1(1,1)-x1(1,2))*x2(2,1)+(x1(1,2)-x
     &2(1,1))*x1(2,1)
      f22=(x1(1,2)-x1(1,1))*x2(2,2)+(x1(1,1)-x2(1,2))*x1(2,2)+(x2(1,2)-x
     &1(1,2))*x1(2,1)
      segment_intersect=(f11.GT.(0.0_DOUBLE)).AND.(f12.GT.(0.0_DOUBLE)).
     &AND.(f21.GT.(0.0_DOUBLE)).AND.(f22.GT.(0.0_DOUBLE))
      return
      end
      function self_intersect(n,x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical self_intersect
      integer n
      REAL(kind=DOUBLE)x(2,0:n)
      integer i,j
      external segment_intersect
      logical segment_intersect
      if(n.GE.3)continue
      if(x(1,0).EQ.x(1,n).AND.x(2,0).EQ.x(2,n))continue
      self_intersect=.TRUE.
      do i=0,n-1
      do j=i+2,min(n-1,n+i-2)
      if(segment_intersect(x(1,i),x(1,j)))then
      return
      end if
      end do
      end do
      
      self_intersect=.FALSE.
      return
      end
      function convex_polygon(n,x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical convex_polygon
      integer n
      REAL(kind=DOUBLE)x(2,0:n)
      integer i
      REAL(kind=DOUBLE)pi,totang,dir0,dir,delta
      external self_intersect
      logical self_intersect
      if(n.GE.3)continue
      if(x(1,0).EQ.x(1,n).AND.x(2,0).EQ.x(2,n))continue
      convex_polygon=.FALSE.
      if(self_intersect(n,x))return
      pi=atan2((0.0_DOUBLE),-(1.0_DOUBLE))
      if(x(2,n).NE.x(2,n-1).OR.x(1,n).NE.x(1,n-1))continue
      dir0=atan2(x(2,n)-x(2,n-1),x(1,n)-x(1,n-1))
      totang=(0.0_DOUBLE)
      do i=0,n-1
      dir=atan2(x(2,i+1)-x(2,i),x(1,i+1)-x(1,i))
      delta=dir-dir0
      if(delta.GT.pi)then
      delta=delta-(2.0_DOUBLE)*pi
      else if(delta.LT.-pi)then
      delta=delta+(2.0_DOUBLE)*pi
      end if
      if(delta.GE.-sqrt((1.0e-10_DOUBLE)))return
      totang=totang+delta
      dir0=dir
      end do
      if(abs(totang+(2.0_DOUBLE)*pi).LE.(1.0e-10_DOUBLE))continue
      convex_polygon=.TRUE.
      return
      end
      function inside_polygon(a,n,x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical inside_polygon
      integer n
      REAL(kind=DOUBLE)a(2),x(2,0:n)
      integer i
      inside_polygon=.FALSE.
      do i=0,n-1
      if((x(2,i+1)-x(2,i))*(a(1)-x(1,i))-(x(1,i+1)-x(1,i))*(a(2)-x(2,i))
     &.LE.(0.0_DOUBLE))return
      end do
      inside_polygon=.TRUE.
      return
      end
      function polygon_volume(n,x)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)polygon_volume
      integer n
      REAL(kind=DOUBLE)x(2,0:n)
      integer i
      polygon_volume=(0.0_DOUBLE)
      if(geometry_symmetry.EQ.1.OR.geometry_symmetry.EQ.3.OR.geometry_sy
     &mmetry.EQ.4)then
      do i=0,n-1
      polygon_volume=polygon_volume+(x(1,i)+x(1,i+1))*(x(2,i)-x(2,i+1))
      end do
      polygon_volume=(universal_cell_max(2)-universal_cell_min(2))*polyg
     &on_volume/(2.0_DOUBLE)
      else if(geometry_symmetry.EQ.2.OR.geometry_symmetry.EQ.5.OR.geomet
     &ry_symmetry.EQ.6)then
      do i=0,n-1
      polygon_volume=polygon_volume+(x(1,i)**2+x(1,i)*x(1,i+1)+x(1,i+1)*
     &*2)*(x(2,i)-x(2,i+1))
      end do
      polygon_volume=(0.5_DOUBLE)*(universal_cell_max(2)-universal_cell_
     &min(2))*polygon_volume/(3.0_DOUBLE)
      end if
      return
      end
      subroutine decompose_polygon(n,x,zonearray,mdiv,facearray)
      
      
      
      
      
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer n,mdiv
      integer zonearray(0:mdiv-1),facearray(0:mdiv)
      REAL(kind=DOUBLE)x(2,0:n)
      REAL(kind=DOUBLE),dimension(:,:),pointer::a
      REAL(kind=DOUBLE),dimension(:,:),pointer::b
      integer i,j,m,k,start,asize
      logical intersect
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
      
      
      external self_intersect,check_split,convex_polygon
      logical self_intersect,check_split,convex_polygon
      if(n.GE.3)continue
      if(x(1,0).EQ.x(1,n).AND.x(2,0).EQ.x(2,n))continue
      
      
      a =>mem_alloc_r2((1),(2),(0),(n),'a')
      b =>mem_alloc_r2((1),(2),(0),(n-1),'b')
      m=0
      a(1,m)=x(1,m)
      a(2,m)=x(2,m)
      do i=1,n
      if(x(1,i).NE.a(1,m).OR.x(2,i).NE.a(2,m))then
      m=m+1
      a(1,m)=x(1,i)
      a(2,m)=x(2,i)
      end if
      end do
      if(m.GE.3)continue
      intersect=self_intersect(m,a)
      if(intersect)then
      do i=0,n
      write(0,*)a(1,i),a(2,i)
      end do
      end if
      if(.NOT.intersect)continue
90000 continue
      if(convex_polygon(m,a))then
      call process_polygon(m,a,zonearray,mdiv,facearray)
      call mem_free_r2(a,(1),(2),(0),(n),'a')
      call mem_free_r2(b,(1),(2),(0),(n-1),'b')
      return
      end if
      k=3
      do i=0,m-1
      if(check_split(m,a,i,k,b))then
      start=i
      asize=k
      goto 90007
      end if
      end do
      write(0,*)'Couldn''t break up the following polygon:'
      do i=0,n
      write(0,*)x(1,i),x(2,i)
      end do
      if(.FALSE.)continue
90007 continue
      do k=asize+1,m-1
      if(.NOT.check_split(m,a,start-(k-asize),k,b))then
      start=start-(k-1-asize)
      asize=k-1
      goto 90008
      end if
      end do
90008 continue
      do k=asize+1,m-1
      if(.NOT.check_split(m,a,start,k,b))then
      asize=k-1
      goto 90009
      end if
      end do
90009 continue
      b(1,asize)=b(1,0)
      b(2,asize)=b(2,0)
      if(check_split(m,a,start,asize,b))continue
      call process_polygon(asize,b,zonearray,mdiv,facearray)
      j=0
      do i=0,m-asize+1
      b(1,j)=a(1,mod(i+start+asize-1,m))
      b(2,j)=a(2,mod(i+start+asize-1,m))
      j=j+1
      end do
      m=m-asize+2
      do i=0,m-1
      a(1,i)=b(1,i)
      a(2,i)=b(2,i)
      end do
      a(1,m)=a(1,0)
      a(2,m)=a(2,0)
      goto 90000
      end
      function check_split(n,a,start,size,b)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical check_split
      integer n,start,size
      REAL(kind=DOUBLE)a(2,0:n)
      REAL(kind=DOUBLE)b(2,0:size)
      integer i,j,l
      REAL(kind=DOUBLE)x(2)
      external convex_polygon,inside_polygon,segment_intersect
      logical convex_polygon,inside_polygon,segment_intersect
      do i=0,size-1
      b(1,i)=a(1,mod(i+start+n,n))
      b(2,i)=a(2,mod(i+start+n,n))
      end do
      b(1,size)=b(1,0)
      b(2,size)=b(2,0)
      check_split=.FALSE.
      if(b(1,size).EQ.b(1,size-1).AND.b(2,size).EQ.b(2,size-1))goto 9000
     &7
      if(.NOT.convex_polygon(size,b))goto 90007
      do l=start+size,start-2+n
      if(inside_polygon(a(1,mod(l,n)),size,b))goto 90007
      x(1)=(0.5_DOUBLE)*(a(1,mod(l,n))+a(1,mod(l+1,n)))
      x(2)=(0.5_DOUBLE)*(a(2,mod(l,n))+a(2,mod(l+1,n)))
      if(inside_polygon(x,size,b))goto 90007
      do j=0,size-1
      if(segment_intersect(b(1,j),a(1,mod(l,n))))goto 90007
      end do
      end do
      if(inside_polygon(a(1,mod(start-1+n,n)),size,b))goto 90007
      check_split=.TRUE.
90007 continue
      return
      end
      subroutine process_polygon(n,x,zonearray,mdiv,facearray)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer n,mdiv
      integer zonearray(0:mdiv-1),facearray(0:mdiv)
      REAL(kind=DOUBLE)x(2,0:n)
      if(geometry_symmetry.EQ.1.OR.geometry_symmetry.EQ.3.OR.geometry_sy
     &mmetry.EQ.2.OR.geometry_symmetry.EQ.4.OR.geometry_symmetry.EQ.5.OR
     &.geometry_symmetry.EQ.6)continue
      if(geometry_symmetry.EQ.3)then
      if(mdiv.EQ.1)continue
      call process_polygon_oned(n,x,zonearray(0))
      else if(geometry_symmetry.EQ.1.OR.geometry_symmetry.EQ.4)then
      call process_polygon_plane(n,x,zonearray,mdiv,facearray)
      else if(geometry_symmetry.EQ.2.OR.geometry_symmetry.EQ.5.OR.geomet
     &ry_symmetry.EQ.6)then
      call process_polygon_cylindrical(n,x,zonearray,mdiv,facearray)
      end if
      return
      end
      subroutine process_polygon_oned(n,x,zone)
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer n,zone
      REAL(kind=DOUBLE)x(2,0:n)
      integer i,cell,face
      REAL(kind=DOUBLE)coeff(10),x1(3),x2(3),x3(3),a(3)
      external define_surface,start_cell,vector_compare,define_surface_a
      integer define_surface,start_cell,vector_compare,define_surface_a
      cell=start_cell(zone)
      a(1)=(0.0_DOUBLE)
      a(2)=(1.0_DOUBLE)
      a(3)=(0.0_DOUBLE)
      call plane(universal_cell_min,a,coeff)
      face=define_surface(coeff,.TRUE.)
      call add_surface(face,cell,.TRUE.)
      call plane(universal_cell_max,a,coeff)
      face=define_surface(coeff,.TRUE.)
      call add_surface(-face,cell,.TRUE.)
      a(1)=(0.0_DOUBLE)
      a(2)=(0.0_DOUBLE)
      a(3)=(1.0_DOUBLE)
      call plane(universal_cell_min,a,coeff)
      face=define_surface(coeff,.TRUE.)
      call add_surface(face,cell,.TRUE.)
      call plane(universal_cell_max,a,coeff)
      face=define_surface(coeff,.TRUE.)
      call add_surface(-face,cell,.TRUE.)
      do i=0,n-1
      x1(1)=x(1,i)
      x1(2)=(0.0_DOUBLE)
      x1(3)=x(2,i)
      x2(1)=x(1,i+1)
      x2(2)=(0.0_DOUBLE)
      x2(3)=x(2,i+1)
      if(vector_compare(x1,x2).GT.0)then
      x3(1)=x1(1)
      x3(2)=(1.0_DOUBLE)
      x3(3)=x1(3)
      else
      x3(1)=x2(1)
      x3(2)=(1.0_DOUBLE)
      x3(3)=x2(3)
      end if
      if(x1(3).NE.x2(3))then
      call planea(x1,x3,x2,coeff)
      face=define_surface_a(coeff,x1,x2)
      call add_surface(face,cell,.TRUE.)
      if(face.GT.0)then
      surface_points(1,0,face)=x1(1)
      surface_points(2,0,face)=x1(2)
      surface_points(3,0,face)=x1(3)
      
      surface_points(1,1,face)=x2(1)
      surface_points(2,1,face)=x2(2)
      surface_points(3,1,face)=x2(3)
      
      end if
      else
      if(x1(3).EQ.x2(3))continue
      if((x1(3).EQ.universal_cell_min(3)).OR.(x1(3).EQ.universal_cell_ma
     &x(3)))continue
      end if
      end do
      return
      end
      subroutine process_polygon_plane(n,x,zonearray,mdiv,facearray)
      
      
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer n,mdiv
      integer zonearray(0:mdiv-1),facearray(0:mdiv)
      REAL(kind=DOUBLE)x(2,0:n)
      integer i,m,face
      REAL(kind=DOUBLE)coeff(10),x1(3),x2(3),x3(3),a(3)
      integer,dimension(:),pointer::cella
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
      
      
      external define_surface,start_cell,vector_compare,define_surface_a
      integer define_surface,start_cell,vector_compare,define_surface_a
      cella =>mem_alloc_i1((0),(mdiv-1),'cella')
      do m=0,mdiv-1
      cella(m)=start_cell(zonearray(m))
      end do
      if(geometry_symmetry.EQ.1)then
      if((mdiv.EQ.1).AND.(facearray(0).EQ.facearray(1)))continue
      a(1)=(0.0_DOUBLE)
      a(2)=(1.0_DOUBLE)
      a(3)=(0.0_DOUBLE)
      call plane(universal_cell_min,a,coeff)
      face=define_surface(coeff,.TRUE.)
      call add_surface(face,cella(0),.TRUE.)
      call plane(universal_cell_max,a,coeff)
      face=define_surface(coeff,.TRUE.)
      call add_surface(-face,cella(0),.TRUE.)
      else
      do m=0,mdiv-1
      call add_surface(facearray(m),cella(m),.TRUE.)
      call add_surface(-facearray(m+1),cella(m),.TRUE.)
      end do
      end if
      do i=0,n-1
      x1(1)=x(1,i)
      x1(2)=(0.0_DOUBLE)
      x1(3)=x(2,i)
      x2(1)=x(1,i+1)
      x2(2)=(0.0_DOUBLE)
      x2(3)=x(2,i+1)
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
      do m=0,mdiv-1
      call add_surface(face,cella(m),.TRUE.)
      end do
      if(face.GT.0)then
      surface_points(1,0,face)=x1(1)
      surface_points(2,0,face)=x1(2)
      surface_points(3,0,face)=x1(3)
      
      surface_points(1,1,face)=x2(1)
      surface_points(2,1,face)=x2(2)
      surface_points(3,1,face)=x2(3)
      
      end if
      end do
      call mem_free_i1(cella,(0),(mdiv-1),'cella')
      return
      end
      subroutine process_polygon_cylindrical(n,x,zonearray,mdiv,facearra
     &y)
      
      
      
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer n,mdiv
      integer zonearray(0:mdiv-1),facearray(0:mdiv)
      REAL(kind=DOUBLE)x(2,0:n)
      integer i,j,ia,ja,m,leftmin,leftmax,rightmin,rightmax,surf_type,kd
     &iv
      REAL(kind=DOUBLE)zmin,zmax,zmina,zmaxa,ztarget,guardmin,guardmax,c
     &utmin,cutmax,coeff(10),x1(3),x2(3),apex(3),a(3),x0(3)
      integer,dimension(:),pointer::k
      integer,dimension(:),pointer::type
      REAL(kind=DOUBLE),dimension(:),pointer::z
      integer,dimension(:),pointer::cella
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
      
      
      external define_surface,start_cell,define_surface_a
      integer define_surface,start_cell,define_surface_a
      leftmin=0
      leftmax=0
      zmin=x(2,0)
      zmax=x(2,0)
      do i=1,n-1
      if(x(2,i).LT.zmin)then
      leftmin=i
      zmin=x(2,i)
      else if(x(2,i).GT.zmax)then
      leftmax=i
      zmax=x(2,i)
      end if
      end do
      if(leftmax.NE.leftmin)continue
      if(zmax.GT.zmin)continue
      rightmin=leftmin
      rightmax=leftmax
      if(leftmax.LT.leftmin)then
      leftmax=leftmax+n
      else
      rightmin=leftmin+n
      end if
      k =>mem_alloc_i1((0),(n-1),'k')
      type =>mem_alloc_i1((0),(n-1),'type')
      z =>mem_alloc_r1((0),(n-1),'z')
      x1(2)=(0.0_DOUBLE)
      x2(2)=(0.0_DOUBLE)
      do i=0,n-1
      x1(1)=x(1,i)
      x1(3)=x(2,i)
      x2(1)=x(1,i+1)
      x2(3)=x(2,i+1)
      call conea(x1,x2,coeff,surf_type,apex)
      k(i)=define_surface_a(coeff,x1,x2)
      if(k(i).GT.0)then
      surface_points(1,0,k(i))=x1(1)
      surface_points(2,0,k(i))=x1(2)
      surface_points(3,0,k(i))=x1(3)
      
      surface_points(1,1,k(i))=x2(1)
      surface_points(2,1,k(i))=x2(2)
      surface_points(3,1,k(i))=x2(3)
      
      end if
      type(i)=surf_type
      z(i)=apex(3)
      end do
      a(1)=(0.0_DOUBLE)
      a(2)=(0.0_DOUBLE)
      a(3)=(1.0_DOUBLE)
      x0(1)=(0.0_DOUBLE)
      x0(2)=(0.0_DOUBLE)
      x0(3)=(0.0_DOUBLE)
      i=leftmin
      kdiv=0
      cella =>mem_alloc_i1((0),(mdiv-1),'cella')
90000 continue
      do m=0,mdiv-1
      cella(m)=start_cell(zonearray(m))
      end do
      if((mdiv.GT.1).OR.((mdiv.EQ.1).AND.(geometry_symmetry.EQ.6)))then
      do m=0,mdiv-1
      if(facearray(m).NE.facearray(m+1))continue
      call add_surface(facearray(m),cella(m),.TRUE.)
      call add_surface(-facearray(m+1),cella(m),.TRUE.)
      end do
      end if
      if(kdiv.NE.0)then
      do m=0,mdiv-1
      call add_surface(kdiv,cella(m),.TRUE.)
      end do
      guardmin=guardmax
      else
      guardmin=-(1.0e16_DOUBLE)
      end if
      ia=mod(i,n)
      zmina=x(2,ia)
      zmaxa=x(2,ia)
      guardmax=(1.0e16_DOUBLE)
      ztarget=(1.0e16_DOUBLE)
90001 continue
      if(i.EQ.leftmax.OR.x(2,ia+1).GT.ztarget.OR.z(ia).GT.zmina.AND.z(ia
     &).LT.zmaxa)then
      if(zmina.GE.zmin.AND.zmaxa.LE.zmax.AND.zmaxa.GT.zmina)continue
      if(i.LT.leftmax)then
      x0(3)=x(2,ia)
      call plane(x0,a,coeff)
      kdiv=define_surface(coeff,.FALSE.)
      do m=0,mdiv-1
      call add_surface(-kdiv,cella(m),.TRUE.)
      end do
      guardmax=x0(3)
      end if
      cutmax=(1.0e16_DOUBLE)
      cutmin=-(1.0e16_DOUBLE)
      do j=rightmax,rightmin-1
      ja=mod(j,n)
      if(.NOT.(x(2,ja).LT.zmina.AND.x(2,ja+1).LE.zmina.OR.x(2,ja).LE.zmi
     &na.AND.x(2,ja+1).LT.zmina.OR.x(2,ja).GT.zmaxa.AND.x(2,ja+1).GE.zma
     &xa.OR.x(2,ja).GE.zmaxa.AND.x(2,ja+1).GT.zmaxa))then
      do m=0,mdiv-1
      call add_surface(k(ja),cella(m),.TRUE.)
      end do
      if(type(ja).EQ.1)then
      if(z(ja).GT.zmaxa.OR.z(ja).LT.zmina)continue
      if(z(ja).GT.zmaxa)then
      cutmax=min(cutmax,z(ja))
      else
      cutmin=max(cutmin,z(ja))
      end if
      else if(type(ja).EQ.0)then
      if(j.EQ.rightmax.OR.j.EQ.rightmin-1)continue
      if(j.EQ.rightmax)then
      guardmax=min(guardmax,z(ja))
      else if(j.EQ.rightmin-1)then
      guardmin=max(guardmin,z(ja))
      end if
      end if
      end if
      end do
      if(cutmin.GT.guardmin)then
      x0(3)=cutmin
      call plane(x0,a,coeff)
      do m=0,mdiv-1
      call add_surface(define_surface(coeff,.FALSE.),cella(m),.FALSE.)
      end do
      end if
      if(cutmax.LT.guardmax)then
      x0(3)=cutmax
      call plane(x0,a,coeff)
      do m=0,mdiv-1
      call add_surface(-define_surface(coeff,.FALSE.),cella(m),.FALSE.)
      end do
      end if
      if(i.EQ.leftmax)then
      if(zmaxa.EQ.zmax)continue
      goto 90007
      else
      goto 90000
      end if
      end if
      zmaxa=x(2,ia+1)
      do m=0,mdiv-1
      call add_surface(k(ia),cella(m),.TRUE.)
      end do
      if(type(ia).EQ.0)then
      if(x(1,ia).GT.x(1,ia+1))then
      guardmin=z(ia)
      else
      guardmax=z(ia)
      end if
      else if(type(ia).EQ.1)then
      if(z(ia).GT.zmaxa)then
      ztarget=min(ztarget,z(ia))
      end if
      end if
      i=i+1
      ia=mod(i,n)
      goto 90001
90007 continue
      call mem_free_i1(k,(0),(n-1),'k')
      call mem_free_i1(type,(0),(n-1),'type')
      call mem_free_r1(z,(0),(n-1),'z')
      call mem_free_i1(cella,(0),(mdiv-1),'cella')
      return
      end
      subroutine set_zn_min_max(n,x,zone,init)
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer n,zone
      REAL(kind=DOUBLE)x(2,0:n)
      logical init
      integer i
      if(init)then
      zone_min(1,zone)=x(1,0)
      zone_min(2,zone)=(0.0_DOUBLE)
      zone_min(3,zone)=x(2,0)
      zone_max(1,zone)=x(1,0)
      zone_max(2,zone)=(0.0_DOUBLE)
      zone_max(3,zone)=x(2,0)
      end if
      do i=0,n
      zone_min(1,zone)=min(zone_min(1,zone),x(1,i))
      zone_min(3,zone)=min(zone_min(3,zone),x(2,i))
      zone_max(1,zone)=max(zone_max(1,zone),x(1,i))
      zone_max(3,zone)=max(zone_max(3,zone),x(2,i))
      end do
      return
      end
      
      
      
