      
      
      
      
      
      
      
      
      
      
      subroutine init_mem
      
      use al_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      memsize=0
      return
      end
      function check_mem()
      
      use al_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical check_mem
      integer ptr,size
      logical lastfree,free
      if(memsize.LT.0)then
      check_mem=.FALSE.
      return
      end if
      check_mem=.FALSE.
      lastfree=.FALSE.
      ptr=0
90000 continue
      if(ptr.EQ.memsize)goto 90007
      size=memory(ptr)
      if(abs(size).LT.0)then
      write(0,*)'Zero length block'
      return
      end if
      if(ptr+abs(size).GT.memsize)then
      write(0,*)'Block too big'
      return
      end if
      free=size.GT.0
      if(lastfree.AND.free)then
      write(0,*)'Two consecutive free blocks'
      return
      end if
      if(memory(ptr+abs(size)-1).NE.size)then
      write(0,*)'Size mismatch'
      return
      end if
      ptr=ptr+abs(size)
      lastfree=free
      goto 90000
90007 continue
      check_mem=.TRUE.
      return
      end
      subroutine enlarge_mem(n)
      
      use al_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer n
      integer new_memsize
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
      
      
      if(n.GE.0)continue
      if(n.EQ.0)return
      new_memsize=max(memsize+n+2,memsize+100,2*memsize)
      memory =>mem_realloc_i1(memory,(0),memsize-1,new_memsize-1,'memory
     &')
      memory(memsize)=new_memsize-memsize
      memory(new_memsize-1)=memory(memsize)
      memsize=new_memsize
      call merge_mem(memsize-memory(memsize-1))
      return
      end
      subroutine merge_mem(ptr)
      
      use al_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer ptr
      integer nptr
      if(memory(ptr).GT.0)continue
      nptr=ptr
      if(nptr.GT.0)then
      if(memory(nptr-1).GT.0)then
      memory(nptr+memory(nptr)-1)=memory(nptr+memory(nptr)-1)+(memory(np
     &tr-1))
      memory(nptr-memory(nptr-1))=memory(nptr+memory(nptr)-1)
      nptr=nptr-(memory(nptr-1))
      end if
      end if
      if(nptr+memory(nptr).LT.memsize)then
      if(memory(nptr+memory(nptr)).GT.0)then
      memory(nptr)=memory(nptr)+(memory(nptr+memory(nptr)))
      memory(nptr+memory(nptr)-1)=memory(nptr)
      end if
      end if
      return
      end
      function alloc_mem(n)
      
      use al_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer n,alloc_mem
      integer size,ptr,i
      external enlarge_mem
      if(n.GE.0)continue
      if(n.EQ.0)then
      alloc_mem=0
      return
      end if
      ptr=0
90000 continue
      if(ptr.EQ.memsize)goto 90007
      size=memory(ptr)
      if(size-2.GE.n)goto 90008
      ptr=ptr+abs(size)
      goto 90000
90007 continue
      call enlarge_mem(n)
      ptr=memsize-memory(memsize-1)
      size=memory(ptr)
90008 continue
      if(size-2.GT.n)then
      memory(ptr+n+2)=size-(n+2)
      memory(ptr+size-1)=size-(n+2)
      end if
      memory(ptr+(n+2)-1)=-(n+2)
      memory(ptr)=-(n+2)
      alloc_mem=ptr+1
      return
      end
      function realloc_mem(p,on,n)
      
      use al_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer p,on,n,realloc_mem
      integer np,i,alloc_mem
      external alloc_mem,free_mem
      if(on.GE.0.AND.n.GT.0)continue
      if(on.EQ.n)then
      realloc_mem=p
      return
      end if
      np=alloc_mem(n)
      do i=0,min(on,n)-1
      memory(np+i)=memory(p+i)
      end do
      call free_mem(p,on)
      realloc_mem=np
      return
      end
      subroutine free_mem(p,n)
      
      use al_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer p,n
      external merge_mem
      if(n.GE.0)continue
      if(n.EQ.0)return
      if(-memory(p-1).EQ.n+2.AND.-memory(p+n).EQ.n+2)continue
      memory(p-1)=n+2
      memory(p+n)=n+2
      call merge_mem(p-1)
      return
      end
      subroutine end_mem
      
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
      
      
      if(memsize.EQ.0)return
      if(memsize.GT.0)continue
      if(memory(0).EQ.memsize.AND.memory(memsize-1).EQ.memsize)continue
      call mem_free_i1(memory,(0),(memsize-1),'memory')
      memsize=0
      return
      end
      
      
