      
      
      
      
      
      function get_env(var,val)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical get_env
      character*(*)var
      character*(*)val
      call getenv(var,val)
      get_env=val.NE.' '
      return
      end
      subroutine date_time(date_array)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer date_array(8)
      character*10 c(3)
      call date_and_time(c(1),c(2),c(3),date_array)
      return
      end
      function wall_time(time)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)wall_time
      integer time(8)
      integer y,m,d
      call date_time(time)
      y=time(1)+int((time(2)+9)/12)-1
      m=mod(time(2)+9,12)
      d=365*y+int(y/4)-int(y/100)+int(y/400)+int((m+4)*153/5)+time(3)-73
     &0548
      wall_time=60*(60*(24*REAL(d,DOUBLE)+time(5))+time(6)-time(4))+time
     &(7)+(0.001_DOUBLE)*time(8)
      return
      end
      subroutine date_string(time,string)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*(*)string
      character*(30)temp
      integer time(8)
      write(temp,'(ss,i4.4,a1,i2.2,a1,i2.2,                a1,i2.2,a1,i2
     &.2,a1,i2.2,a1,i3.3,                a1,sp,i3.2,ss,a1,i2.2)')time(1)
     &,'-',time(2),'-',time(3),' ',time(5),':',time(6),':',time(7),'.',t
     &ime(8),' ',int(time(4)/60),':',abs(time(4)-int(time(4)/60)*60)
      string=temp
      return
      end
      function cpu_time()
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)cpu_time
      integer count,rate
      call system_clock(count,rate)
      cpu_time=REAL(count,kind=DOUBLE)/REAL(max(1,rate),kind=DOUBLE)
      return
      end
      function get_pid()
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer get_pid
      integer getpid
      get_pid=getpid()
      return
      end
      subroutine get_cwd(cwd)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*(*)cwd
      integer getcwd
      if(getcwd(cwd).NE.0)cwd='UNKNOWN'
      if(cwd.EQ.' ')cwd='UNKNOWN'
      return
      end
      function set_cwd(dir)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical set_cwd
      character*(*)dir
      integer chdir
      set_cwd=chdir(dir).EQ.0
      return
      end
      subroutine user_name(user)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*(*)user
      logical get_env
      external get_env
      if(.NOT.get_env('LOGNAME',user))then
      if(.NOT.get_env('USER',user))then
      user='UNKNOWN'
      end if
      end if
      return
      end
      subroutine host_name(host)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*(*)host
      integer hostnm
      if(hostnm(host).NE.0)host='UNKNOWN'
      if(host.EQ.' ')host='UNKNOWN'
      return
      end
      function arg_count()
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer arg_count
      integer iargc
      arg_count=iargc()
      return
      end
      subroutine command_arg(i,arg)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer i
      character*(*)arg
      integer j
      j=i
      call getarg(j,arg)
      return
      end
      block data mem_common_data
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      data mem_array_name/300*' '/
      end
      function mem_alloc_c1(size,l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1
      character(len=size),dimension(:),pointer::p
      character(len=*),intent(in)::name
      logical mem_alloc_check
      external mem_alloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(size.GT.0)continue
      if(u1.EQ.l1-1)then
      nullify(p)
      return
      end if
      if(l1.LE.u1)continue
      if(mem_alloc_check(name,u1))continue
      allocate(p(l1:u1))
      return
      end function
      function mem_alloc_c2(size,l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2
      character(len=size),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_alloc_check
      external mem_alloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(size.GT.0)continue
      if(u2.EQ.l2-1)then
      nullify(p)
      return
      end if
      if(l1.LE.u1.AND.l2.LE.u2)continue
      if(mem_alloc_check(name,u2))continue
      allocate(p(l1:u1,l2:u2))
      return
      end function
      function mem_alloc_c3(size,l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3
      character(len=size),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_alloc_check
      external mem_alloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(size.GT.0)continue
      if(u3.EQ.l3-1)then
      nullify(p)
      return
      end if
      if(l1.LE.u1.AND.l2.LE.u2.AND.l3.LE.u3)continue
      if(mem_alloc_check(name,u3))continue
      allocate(p(l1:u1,l2:u2,l3:u3))
      return
      end function
      function mem_alloc_c4(size,l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4
      character(len=size),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_alloc_check
      external mem_alloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(size.GT.0)continue
      if(u4.EQ.l4-1)then
      nullify(p)
      return
      end if
      if(l1.LE.u1.AND.l2.LE.u2.AND.l3.LE.u3.AND.l4.LE.u4)continue
      if(mem_alloc_check(name,u4))continue
      allocate(p(l1:u1,l2:u2,l3:u3,l4:u4))
      return
      end function
      function mem_alloc_i1(l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      integer,dimension(:),pointer::p
      character(len=*),intent(in)::name
      logical mem_alloc_check
      external mem_alloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u1.EQ.l1-1)then
      nullify(p)
      return
      end if
      if(l1.LE.u1)continue
      if(mem_alloc_check(name,u1))continue
      allocate(p(l1:u1))
      return
      end function
      function mem_alloc_i2(l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      integer,dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_alloc_check
      external mem_alloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u2.EQ.l2-1)then
      nullify(p)
      return
      end if
      if(l1.LE.u1.AND.l2.LE.u2)continue
      if(mem_alloc_check(name,u2))continue
      allocate(p(l1:u1,l2:u2))
      return
      end function
      function mem_alloc_i3(l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      integer,dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_alloc_check
      external mem_alloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u3.EQ.l3-1)then
      nullify(p)
      return
      end if
      if(l1.LE.u1.AND.l2.LE.u2.AND.l3.LE.u3)continue
      if(mem_alloc_check(name,u3))continue
      allocate(p(l1:u1,l2:u2,l3:u3))
      return
      end function
      function mem_alloc_i4(l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      integer,dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_alloc_check
      external mem_alloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u4.EQ.l4-1)then
      nullify(p)
      return
      end if
      if(l1.LE.u1.AND.l2.LE.u2.AND.l3.LE.u3.AND.l4.LE.u4)continue
      if(mem_alloc_check(name,u4))continue
      allocate(p(l1:u1,l2:u2,l3:u3,l4:u4))
      return
      end function
      function mem_alloc_i5(l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      integer,dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_alloc_check
      external mem_alloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u5.EQ.l5-1)then
      nullify(p)
      return
      end if
      if(l1.LE.u1.AND.l2.LE.u2.AND.l3.LE.u3.AND.l4.LE.u4.AND.l5.LE.u5)co
     &ntinue
      if(mem_alloc_check(name,u5))continue
      allocate(p(l1:u1,l2:u2,l3:u3,l4:u4,l5:u5))
      return
      end function
      function mem_alloc_r1(l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      REAL(kind=DOUBLE),dimension(:),pointer::p
      character(len=*),intent(in)::name
      logical mem_alloc_check
      external mem_alloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u1.EQ.l1-1)then
      nullify(p)
      return
      end if
      if(l1.LE.u1)continue
      if(mem_alloc_check(name,u1))continue
      allocate(p(l1:u1))
      return
      end function
      function mem_alloc_r2(l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      REAL(kind=DOUBLE),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_alloc_check
      external mem_alloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u2.EQ.l2-1)then
      nullify(p)
      return
      end if
      if(l1.LE.u1.AND.l2.LE.u2)continue
      if(mem_alloc_check(name,u2))continue
      allocate(p(l1:u1,l2:u2))
      return
      end function
      function mem_alloc_r3(l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_alloc_check
      external mem_alloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u3.EQ.l3-1)then
      nullify(p)
      return
      end if
      if(l1.LE.u1.AND.l2.LE.u2.AND.l3.LE.u3)continue
      if(mem_alloc_check(name,u3))continue
      allocate(p(l1:u1,l2:u2,l3:u3))
      return
      end function
      function mem_alloc_r4(l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_alloc_check
      external mem_alloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u4.EQ.l4-1)then
      nullify(p)
      return
      end if
      if(l1.LE.u1.AND.l2.LE.u2.AND.l3.LE.u3.AND.l4.LE.u4)continue
      if(mem_alloc_check(name,u4))continue
      allocate(p(l1:u1,l2:u2,l3:u3,l4:u4))
      return
      end function
      function mem_alloc_r5(l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_alloc_check
      external mem_alloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u5.EQ.l5-1)then
      nullify(p)
      return
      end if
      if(l1.LE.u1.AND.l2.LE.u2.AND.l3.LE.u3.AND.l4.LE.u4.AND.l5.LE.u5)co
     &ntinue
      if(mem_alloc_check(name,u5))continue
      allocate(p(l1:u1,l2:u2,l3:u3,l4:u4,l5:u5))
      return
      end function
      function mem_realloc_c1(p,size,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,nu
      character(len=size),dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      logical mem_realloc_check
      external mem_realloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      interface
      function mem_alloc_c1(size,l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1
      character(len=size),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      end interface
      if(l1.LE.nu)continue
      if(u1.EQ.l1-1)then
      np =>mem_alloc_c1(size,l1,nu,name)
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1)continue
      if(mem_realloc_check(name,u1,nu))continue
      if(u1.EQ.nu)then
      np =>p
      return
      end if
      allocate(np(l1:nu))
      np(:min(u1,nu))=p(:min(u1,nu))
      deallocate(p)
      return
      end function
      function mem_realloc_c2(p,size,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,nu
      character(len=size),dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      logical mem_realloc_check
      external mem_realloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      interface
      function mem_alloc_c2(size,l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2
      character(len=size),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      end interface
      if(l2.LE.nu)continue
      if(u2.EQ.l2-1)then
      np =>mem_alloc_c2(size,l1,u1,l2,nu,name)
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2)continue
      if(mem_realloc_check(name,u2,nu))continue
      if(u2.EQ.nu)then
      np =>p
      return
      end if
      allocate(np(l1:u1,l2:nu))
      np(:,:min(u2,nu))=p(:,:min(u2,nu))
      deallocate(p)
      return
      end function
      function mem_realloc_c3(p,size,l1,u1,l2,u2,l3,u3,nu,name)result(np
     &)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,nu
      character(len=size),dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      logical mem_realloc_check
      external mem_realloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      interface
      function mem_alloc_c3(size,l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3
      character(len=size),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      end interface
      if(l3.LE.nu)continue
      if(u3.EQ.l3-1)then
      np =>mem_alloc_c3(size,l1,u1,l2,u2,l3,nu,name)
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2.AND.lbound(p,3).EQ.l3.AND.ubound(p,3).EQ.u3)c
     &ontinue
      if(mem_realloc_check(name,u3,nu))continue
      if(u3.EQ.nu)then
      np =>p
      return
      end if
      allocate(np(l1:u1,l2:u2,l3:nu))
      np(:,:,:min(u3,nu))=p(:,:,:min(u3,nu))
      deallocate(p)
      return
      end function
      function mem_realloc_c4(p,size,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)res
     &ult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4,nu
      character(len=size),dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      logical mem_realloc_check
      external mem_realloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      interface
      function mem_alloc_c4(size,l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4
      character(len=size),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      end interface
      if(l4.LE.nu)continue
      if(u4.EQ.l4-1)then
      np =>mem_alloc_c4(size,l1,u1,l2,u2,l3,u3,l4,nu,name)
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2.AND.lbound(p,3).EQ.l3.AND.ubound(p,3).EQ.u3.A
     &ND.lbound(p,4).EQ.l4.AND.ubound(p,4).EQ.u4)continue
      if(mem_realloc_check(name,u4,nu))continue
      if(u4.EQ.nu)then
      np =>p
      return
      end if
      allocate(np(l1:u1,l2:u2,l3:u3,l4:nu))
      np(:,:,:,:min(u4,nu))=p(:,:,:,:min(u4,nu))
      deallocate(p)
      return
      end function
      function mem_realloc_i1(p,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,nu
      integer,dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      logical mem_realloc_check
      external mem_realloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      interface
      function mem_alloc_i1(l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      integer,dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      end interface
      if(l1.LE.nu)continue
      if(u1.EQ.l1-1)then
      np =>mem_alloc_i1(l1,nu,name)
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1)continue
      if(mem_realloc_check(name,u1,nu))continue
      if(u1.EQ.nu)then
      np =>p
      return
      end if
      allocate(np(l1:nu))
      np(:min(u1,nu))=p(:min(u1,nu))
      deallocate(p)
      return
      end function
      function mem_realloc_i2(p,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,nu
      integer,dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      logical mem_realloc_check
      external mem_realloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      interface
      function mem_alloc_i2(l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      integer,dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      end interface
      if(l2.LE.nu)continue
      if(u2.EQ.l2-1)then
      np =>mem_alloc_i2(l1,u1,l2,nu,name)
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2)continue
      if(mem_realloc_check(name,u2,nu))continue
      if(u2.EQ.nu)then
      np =>p
      return
      end if
      allocate(np(l1:u1,l2:nu))
      np(:,:min(u2,nu))=p(:,:min(u2,nu))
      deallocate(p)
      return
      end function
      function mem_realloc_i3(p,l1,u1,l2,u2,l3,u3,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,nu
      integer,dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      logical mem_realloc_check
      external mem_realloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      interface
      function mem_alloc_i3(l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      integer,dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      end interface
      if(l3.LE.nu)continue
      if(u3.EQ.l3-1)then
      np =>mem_alloc_i3(l1,u1,l2,u2,l3,nu,name)
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2.AND.lbound(p,3).EQ.l3.AND.ubound(p,3).EQ.u3)c
     &ontinue
      if(mem_realloc_check(name,u3,nu))continue
      if(u3.EQ.nu)then
      np =>p
      return
      end if
      allocate(np(l1:u1,l2:u2,l3:nu))
      np(:,:,:min(u3,nu))=p(:,:,:min(u3,nu))
      deallocate(p)
      return
      end function
      function mem_realloc_i4(p,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)result(n
     &p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,nu
      integer,dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      logical mem_realloc_check
      external mem_realloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      interface
      function mem_alloc_i4(l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      integer,dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      end interface
      if(l4.LE.nu)continue
      if(u4.EQ.l4-1)then
      np =>mem_alloc_i4(l1,u1,l2,u2,l3,u3,l4,nu,name)
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2.AND.lbound(p,3).EQ.l3.AND.ubound(p,3).EQ.u3.A
     &ND.lbound(p,4).EQ.l4.AND.ubound(p,4).EQ.u4)continue
      if(mem_realloc_check(name,u4,nu))continue
      if(u4.EQ.nu)then
      np =>p
      return
      end if
      allocate(np(l1:u1,l2:u2,l3:u3,l4:nu))
      np(:,:,:,:min(u4,nu))=p(:,:,:,:min(u4,nu))
      deallocate(p)
      return
      end function
      function mem_realloc_i5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu,name)re
     &sult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu
      integer,dimension(:,:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      logical mem_realloc_check
      external mem_realloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      interface
      function mem_alloc_i5(l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      integer,dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      end interface
      if(l5.LE.nu)continue
      if(u5.EQ.l5-1)then
      np =>mem_alloc_i5(l1,u1,l2,u2,l3,u3,l4,u4,l5,nu,name)
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2.AND.lbound(p,3).EQ.l3.AND.ubound(p,3).EQ.u3.A
     &ND.lbound(p,4).EQ.l4.AND.ubound(p,4).EQ.u4.AND.lbound(p,5).EQ.l5.A
     &ND.ubound(p,5).EQ.u5)continue
      if(mem_realloc_check(name,u5,nu))continue
      if(u5.EQ.nu)then
      np =>p
      return
      end if
      allocate(np(l1:u1,l2:u2,l3:u3,l4:u4,l5:nu))
      np(:,:,:,:,:min(u5,nu))=p(:,:,:,:,:min(u5,nu))
      deallocate(p)
      return
      end function
      function mem_realloc_r1(p,l1,u1,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,nu
      REAL(kind=DOUBLE),dimension(:),pointer::p,np
      character(len=*),intent(in)::name
      logical mem_realloc_check
      external mem_realloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      interface
      function mem_alloc_r1(l1,u1,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      REAL(kind=DOUBLE),dimension(:),pointer::p
      character(len=*),intent(in)::name
      end function
      end interface
      if(l1.LE.nu)continue
      if(u1.EQ.l1-1)then
      np =>mem_alloc_r1(l1,nu,name)
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1)continue
      if(mem_realloc_check(name,u1,nu))continue
      if(u1.EQ.nu)then
      np =>p
      return
      end if
      allocate(np(l1:nu))
      np(:min(u1,nu))=p(:min(u1,nu))
      deallocate(p)
      return
      end function
      function mem_realloc_r2(p,l1,u1,l2,u2,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,nu
      REAL(kind=DOUBLE),dimension(:,:),pointer::p,np
      character(len=*),intent(in)::name
      logical mem_realloc_check
      external mem_realloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      interface
      function mem_alloc_r2(l1,u1,l2,u2,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      REAL(kind=DOUBLE),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      end interface
      if(l2.LE.nu)continue
      if(u2.EQ.l2-1)then
      np =>mem_alloc_r2(l1,u1,l2,nu,name)
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2)continue
      if(mem_realloc_check(name,u2,nu))continue
      if(u2.EQ.nu)then
      np =>p
      return
      end if
      allocate(np(l1:u1,l2:nu))
      np(:,:min(u2,nu))=p(:,:min(u2,nu))
      deallocate(p)
      return
      end function
      function mem_realloc_r3(p,l1,u1,l2,u2,l3,u3,nu,name)result(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,nu
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      logical mem_realloc_check
      external mem_realloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      interface
      function mem_alloc_r3(l1,u1,l2,u2,l3,u3,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      end interface
      if(l3.LE.nu)continue
      if(u3.EQ.l3-1)then
      np =>mem_alloc_r3(l1,u1,l2,u2,l3,nu,name)
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2.AND.lbound(p,3).EQ.l3.AND.ubound(p,3).EQ.u3)c
     &ontinue
      if(mem_realloc_check(name,u3,nu))continue
      if(u3.EQ.nu)then
      np =>p
      return
      end if
      allocate(np(l1:u1,l2:u2,l3:nu))
      np(:,:,:min(u3,nu))=p(:,:,:min(u3,nu))
      deallocate(p)
      return
      end function
      function mem_realloc_r4(p,l1,u1,l2,u2,l3,u3,l4,u4,nu,name)result(n
     &p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,nu
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      logical mem_realloc_check
      external mem_realloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      interface
      function mem_alloc_r4(l1,u1,l2,u2,l3,u3,l4,u4,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      end interface
      if(l4.LE.nu)continue
      if(u4.EQ.l4-1)then
      np =>mem_alloc_r4(l1,u1,l2,u2,l3,u3,l4,nu,name)
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2.AND.lbound(p,3).EQ.l3.AND.ubound(p,3).EQ.u3.A
     &ND.lbound(p,4).EQ.l4.AND.ubound(p,4).EQ.u4)continue
      if(mem_realloc_check(name,u4,nu))continue
      if(u4.EQ.nu)then
      np =>p
      return
      end if
      allocate(np(l1:u1,l2:u2,l3:u3,l4:nu))
      np(:,:,:,:min(u4,nu))=p(:,:,:,:min(u4,nu))
      deallocate(p)
      return
      end function
      function mem_realloc_r5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu,name)re
     &sult(np)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,nu
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p,np
      character(len=*),intent(in)::name
      logical mem_realloc_check
      external mem_realloc_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      interface
      function mem_alloc_r5(l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)result(p)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      end function
      end interface
      if(l5.LE.nu)continue
      if(u5.EQ.l5-1)then
      np =>mem_alloc_r5(l1,u1,l2,u2,l3,u3,l4,u4,l5,nu,name)
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2.AND.lbound(p,3).EQ.l3.AND.ubound(p,3).EQ.u3.A
     &ND.lbound(p,4).EQ.l4.AND.ubound(p,4).EQ.u4.AND.lbound(p,5).EQ.l5.A
     &ND.ubound(p,5).EQ.u5)continue
      if(mem_realloc_check(name,u5,nu))continue
      if(u5.EQ.nu)then
      np =>p
      return
      end if
      allocate(np(l1:u1,l2:u2,l3:u3,l4:u4,l5:nu))
      np(:,:,:,:,:min(u5,nu))=p(:,:,:,:,:min(u5,nu))
      deallocate(p)
      return
      end function
      subroutine mem_free_c1(p,size,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1
      character(len=size),dimension(:),pointer::p
      character(len=*),intent(in)::name
      logical mem_free_check
      external mem_free_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u1.EQ.l1-1)then
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1)continue
      if(mem_free_check(name,u1))continue
      deallocate(p)
      return
      end subroutine
      subroutine mem_free_c2(p,size,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2
      character(len=size),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_free_check
      external mem_free_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u2.EQ.l2-1)then
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2)continue
      if(mem_free_check(name,u2))continue
      deallocate(p)
      return
      end subroutine
      subroutine mem_free_c3(p,size,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3
      character(len=size),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_free_check
      external mem_free_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u3.EQ.l3-1)then
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2.AND.lbound(p,3).EQ.l3.AND.ubound(p,3).EQ.u3)c
     &ontinue
      if(mem_free_check(name,u3))continue
      deallocate(p)
      return
      end subroutine
      subroutine mem_free_c4(p,size,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::size,l1,u1,l2,u2,l3,u3,l4,u4
      character(len=size),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_free_check
      external mem_free_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u4.EQ.l4-1)then
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2.AND.lbound(p,3).EQ.l3.AND.ubound(p,3).EQ.u3.A
     &ND.lbound(p,4).EQ.l4.AND.ubound(p,4).EQ.u4)continue
      if(mem_free_check(name,u4))continue
      deallocate(p)
      return
      end subroutine
      subroutine mem_free_i1(p,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      integer,dimension(:),pointer::p
      character(len=*),intent(in)::name
      logical mem_free_check
      external mem_free_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u1.EQ.l1-1)then
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1)continue
      if(mem_free_check(name,u1))continue
      deallocate(p)
      return
      end subroutine
      subroutine mem_free_i2(p,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      integer,dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_free_check
      external mem_free_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u2.EQ.l2-1)then
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2)continue
      if(mem_free_check(name,u2))continue
      deallocate(p)
      return
      end subroutine
      subroutine mem_free_i3(p,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      integer,dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_free_check
      external mem_free_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u3.EQ.l3-1)then
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2.AND.lbound(p,3).EQ.l3.AND.ubound(p,3).EQ.u3)c
     &ontinue
      if(mem_free_check(name,u3))continue
      deallocate(p)
      return
      end subroutine
      subroutine mem_free_i4(p,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      integer,dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_free_check
      external mem_free_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u4.EQ.l4-1)then
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2.AND.lbound(p,3).EQ.l3.AND.ubound(p,3).EQ.u3.A
     &ND.lbound(p,4).EQ.l4.AND.ubound(p,4).EQ.u4)continue
      if(mem_free_check(name,u4))continue
      deallocate(p)
      return
      end subroutine
      subroutine mem_free_i5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      integer,dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_free_check
      external mem_free_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u5.EQ.l5-1)then
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2.AND.lbound(p,3).EQ.l3.AND.ubound(p,3).EQ.u3.A
     &ND.lbound(p,4).EQ.l4.AND.ubound(p,4).EQ.u4.AND.lbound(p,5).EQ.l5.A
     &ND.ubound(p,5).EQ.u5)continue
      if(mem_free_check(name,u5))continue
      deallocate(p)
      return
      end subroutine
      subroutine mem_free_r1(p,l1,u1,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1
      REAL(kind=DOUBLE),dimension(:),pointer::p
      character(len=*),intent(in)::name
      logical mem_free_check
      external mem_free_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u1.EQ.l1-1)then
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1)continue
      if(mem_free_check(name,u1))continue
      deallocate(p)
      return
      end subroutine
      subroutine mem_free_r2(p,l1,u1,l2,u2,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2
      REAL(kind=DOUBLE),dimension(:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_free_check
      external mem_free_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u2.EQ.l2-1)then
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2)continue
      if(mem_free_check(name,u2))continue
      deallocate(p)
      return
      end subroutine
      subroutine mem_free_r3(p,l1,u1,l2,u2,l3,u3,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3
      REAL(kind=DOUBLE),dimension(:,:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_free_check
      external mem_free_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u3.EQ.l3-1)then
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2.AND.lbound(p,3).EQ.l3.AND.ubound(p,3).EQ.u3)c
     &ontinue
      if(mem_free_check(name,u3))continue
      deallocate(p)
      return
      end subroutine
      subroutine mem_free_r4(p,l1,u1,l2,u2,l3,u3,l4,u4,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4
      REAL(kind=DOUBLE),dimension(:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_free_check
      external mem_free_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u4.EQ.l4-1)then
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2.AND.lbound(p,3).EQ.l3.AND.ubound(p,3).EQ.u3.A
     &ND.lbound(p,4).EQ.l4.AND.ubound(p,4).EQ.u4)continue
      if(mem_free_check(name,u4))continue
      deallocate(p)
      return
      end subroutine
      subroutine mem_free_r5(p,l1,u1,l2,u2,l3,u3,l4,u4,l5,u5,name)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer,intent(in)::l1,u1,l2,u2,l3,u3,l4,u4,l5,u5
      REAL(kind=DOUBLE),dimension(:,:,:,:,:),pointer::p
      character(len=*),intent(in)::name
      logical mem_free_check
      external mem_free_check
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      if(u5.EQ.l5-1)then
      return
      end if
      if(lbound(p,1).EQ.l1.AND.ubound(p,1).EQ.u1.AND.lbound(p,2).EQ.l2.A
     &ND.ubound(p,2).EQ.u2.AND.lbound(p,3).EQ.l3.AND.ubound(p,3).EQ.u3.A
     &ND.lbound(p,4).EQ.l4.AND.ubound(p,4).EQ.u4.AND.lbound(p,5).EQ.l5.A
     &ND.ubound(p,5).EQ.u5)continue
      if(mem_free_check(name,u5))continue
      deallocate(p)
      return
      end subroutine
      function mem_alloc_check(name,bound)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical mem_alloc_check
      integer,intent(in)::bound
      character(len=*),intent(in)::name
      character(len=40)::namex
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      integer::i,empty
      namex=name
      if(namex.EQ.' ')then
      mem_alloc_check=.TRUE.
      return
      end if
      empty=0
      mem_alloc_check=.FALSE.
      do i=1,300
      if(mem_array_name(i).EQ.' ')then
      if(empty.EQ.0)empty=i
      else if(namex.EQ.mem_array_name(i))then
      return
      end if
      end do
      if(empty.EQ.0)return
      mem_array_name(empty)=namex
      mem_array_bound(empty)=bound
      mem_alloc_check=.TRUE.
      return
      end
      function mem_realloc_check(name,bound,nbound)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical mem_realloc_check
      integer,intent(in)::bound,nbound
      character(len=*),intent(in)::name
      character(len=40)::namex
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      integer::i
      namex=name
      if(namex.EQ.' ')then
      mem_realloc_check=.TRUE.
      return
      end if
      mem_realloc_check=.FALSE.
      do i=1,300
      if(namex.EQ.mem_array_name(i))then
      if(bound.EQ.mem_array_bound(i))then
      mem_array_bound(i)=nbound
      mem_realloc_check=.TRUE.
      return
      end if
      return
      end if
      end do
      return
      end
      function mem_free_check(name,bound)
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical mem_free_check
      integer,intent(in)::bound
      character(len=*),intent(in)::name
      character(len=40)::namex
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      integer::i
      namex=name
      if(namex.EQ.' ')then
      mem_free_check=.TRUE.
      return
      end if
      mem_free_check=.FALSE.
      do i=1,300
      if(namex.EQ.mem_array_name(i))then
      if(bound.EQ.mem_array_bound(i))then
      mem_array_name(i)=' '
      mem_free_check=.TRUE.
      return
      end if
      return
      end if
      end do
      return
      end
      subroutine mem_check
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character(len=40)mem_array_name(300)
      integer mem_array_bound(300)
      common/mem_common_c/mem_array_name
      common/mem_common_i/mem_array_bound
      save/mem_common_c/
      save/mem_common_i/
      integer::i,count
      write(6,*)'Arrays allocated:'
      count=0
      do i=1,300
      if(mem_array_name(i).NE.' ')then
      write(6,*)mem_array_name(i),mem_array_bound(i)
      count=count+1
      end if
      end do
      write(6,*)'Total number: ',count
      return
      end
      subroutine error_exit(message)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*(*)message
      character*300 response
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      write(0,*)message
      write(0,*)'To resume execution, type:   go'
      write(0,*)'Any other input will terminate the program.'
      read(5,'(a)')response
      if(response  (1:string_length(response)).NE.'go')stop
      return
      end
      
