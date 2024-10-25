      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      program output_browser
      
      use mp_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      include 'mpif.h'
      integer mpi_err
      integer mpi_status
      dimension mpi_status(MPI_STATUS_SIZE)
      integer nargs
      character*96 scriptfile
      external get_env,wall_time,cpu_time,get_pid,arg_count,set_cwd
      logical get_env,set_cwd
      integer get_pid,arg_count
      REAL(kind=DOUBLE)cpu_time,wall_time
      call MPI_init(mpi_err)
      mpi_degas2_root=0
      call MPI_Comm_dup(MPI_COMM_WORLD,comm_world_dup,mpi_err)
      call MPI_comm_rank(comm_world_dup,mpi_rank,mpi_err)
      call MPI_comm_size(comm_world_dup,mpi_size,mpi_err)
      nargs=arg_count()
      if(nargs.GT.0)then
      call command_arg(1,scriptfile)
      if(nargs.GT.1)write(6,*)' Only one script file per run'
      else
      scriptfile=' '
      end if
      call readfilenames
      call degas_init
      call nc_read_output
      call browse(scriptfile)
      call MPI_barrier(comm_world_dup,mpi_err)
      call MPI_finalize(mpi_err)
      stop
      end
      subroutine browse(scriptfile)
      
      
      
      
      use pr_mod
      
      use so_mod
      
      use tl_mod
      
      use ou_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*96 scriptfile
      integer jscore,length,p,b,e,i,j,k,i1,i2,i3,dim,group,screen,nunit,
     &value_size,ii,init,verbose,first_pass
      integer min_slice(5),max_slice(5),index_parameters(100),details(5)
      character*1 choice
      character*2 adddet
      character*300 line
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
      
      
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      logical check_tally
      integer inc
      REAL(kind=DOUBLE)extract_output_datum
      external extract_output_datum
      REAL(kind=DOUBLE),dimension(:),pointer::value
      REAL(kind=DOUBLE),dimension(:),pointer::rsd
      value_size=1000
      value =>mem_alloc_r1((1),(value_size),'value')
      rsd =>mem_alloc_r1((1),(value_size),'rsd')
      init=1
      open(unit=31,file='outputscript',status='unknown')
      if(scriptfile.NE.' ')open(unit=20,file=scriptfile,status='old')
90000 continue
      if(scriptfile.EQ.' ')then
90002 continue
      write(6,*)
      if(init.EQ.1)then
      write(6,*)' Do you want verbose (v) or terse instructions (t)?'
      if(.NOT.read_string(5,line,length))then
      write(6,*)' No, you actually have to type in "v" or "t".'
      go to 90002
      end if
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.'v'.OR.line(b:e).EQ.'V')then
      verbose=1
      else if(line(b:e).EQ.'t'.OR.line(b:e).EQ.'T')then
      verbose=0
      else
      write(6,*)' Type a "v" or a "t"'
      go to 90002
      end if
      end if
      if(verbose.EQ.1.OR.init.EQ.1)then
      write(6,*)'The tallies contained in this file are:'
      write(6,*)
      do jscore=1,tl_num
      write(6,'(i4,a,a)')jscore,' - ',tally_name(jscore) (1:string_lengt
     &h(tally_name(jscore)))
      end do
      write(6,*)
      write(6,*)'Choose one; use 0 to exit'
      else
      write(6,*)'Choose next tally: '
      end if
90003 continue
      if(.NOT.read_string(5,line,length))goto 90017
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      jscore=read_int_soft_fail(line(b:e))
      if(jscore.EQ.0)go to 90017
      if(.NOT.check_tally(jscore))then
      write(6,*)'Tally must be between 1 and ',tl_num
      write(6,*)'Try again, or use 0 to quit:'
      go to 90003
      end if
      write(6,*)
      if(so_grps.GT.1)then
      if(verbose.EQ.1.OR.init.EQ.1)then
      write(6,*)'   Do you want to look at a specific source group?'
      write(6,*)'   Enter 0 for ''no'', or 1 through ',so_grps
      else
      write(6,*)'   Pick source group: '
      end if
90004 continue
      if(.NOT.read_string(5,line,length))goto 90017
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      group=read_int_soft_fail(line(b:e))
      if(.NOT.(group.GT.0.AND.group.LE.so_grps).AND.group.NE.0)then
      write(6,*)'Group must be between 1 and ',so_grps
      write(6,*)'Try again; use 0 to select all:'
      go to 90004
      end if
      else
      group=0
      end if
      
      write(6,*)
      if(tally_rank(jscore).GT.0)then
      if(verbose.EQ.1)then
      write(6,*)'''',tally_name(jscore) (1:string_length(tally_name(jsco
     &re))),''' is of rank: ',tally_rank(jscore)
      write(6,*)
      write(6,*)'   Its independent variables are:'
      write(6,*)
      write(6,*)'   rank  # values    name '
      else
      write(6,*)'   Independent variables are:'
      end if
      do i=1,tally_rank(jscore)
      write(6,'(3x,i4,5x,i6,4x,a)')i,tally_tab_index(i,jscore),tally_var
     &_list(tally_indep_var(i,jscore)) (1:string_length(tally_var_list(t
     &ally_indep_var(i,jscore))))
      end do
      write(6,*)
      do i=1,tally_rank(jscore)
      if(verbose.EQ.1)then
      write(6,*)'   Enter range indices, separated by a space for ''',ta
     &lly_var_list(tally_indep_var(i,jscore)) (1:string_length(tally_var
     &_list(tally_indep_var(i,jscore)))),''''
      write(6,*)'   If you want additional details on this variable, app
     &end a + sign'
      else
      write(6,*)'   Range for ''',tally_var_list(tally_indep_var(i,jscor
     &e)) (1:string_length(tally_var_list(tally_indep_var(i,jscore)))),'
     &'''
      end if
90005 continue
      if(.NOT.read_string(5,line,length))goto 90017
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      min_slice(i)=1000000000
      max_slice(i)=1000000000
      details(i)=0
90011 continue
      if(line(b:e).EQ.'+')then
      details(i)=1
      else if(line(b:e).EQ.'*')then
      max_slice(i)=tally_tab_index(i,jscore)
      if(min_slice(i).EQ.1000000000)min_slice(i)=1
      else
      if(line(e:e).EQ.'+')then
      e=e-(1)
      details(i)=1
      end if
      if(min_slice(i).EQ.1000000000)then
      min_slice(i)=read_int_soft_fail(line(b:e))
      else
      max_slice(i)=read_int_soft_fail(line(b:e))
      end if
      end if
      if(next_token(line,b,e,p))go to 90011
      if(max_slice(i).EQ.1000000000)max_slice(i)=min_slice(i)
      if(min_slice(i).LT.1.OR.min_slice(i).GT.tally_tab_index(i,jscore).
     &OR.max_slice(i).LT.1.OR.max_slice(i).GT.tally_tab_index(i,jscore).
     &OR.min_slice(i).GT.max_slice(i))then
      write(6,*)' Range limits must be between 1 and ',tally_tab_index(i
     &,jscore)
      write(6,*)' Try again: '
      go to 90005
      end if
      end do
      end if
      
      if(tally_rank(jscore).EQ.0.OR.tally_rank(jscore).EQ.1)then
      i1=1
      else
      write(6,*)
      write(6,*)'   Would you like to re-order the independent variables
     & for printing? (y/n)'
      if(.NOT.read_string(5,line,length))go to 90017
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      choice=line(b:e)
      if(choice.NE.'y'.AND.choice.NE.'Y')then
      i1=1
      i2=2
      i3=3
      else
      if(verbose.EQ.1)then
      write(6,*)'   Using the integer index for each variable:'
      do i=1,tally_rank(jscore)
      write(6,*)i,' - ',tally_var_list(tally_indep_var(i,jscore)) (1:str
     &ing_length(tally_var_list(tally_indep_var(i,jscore))))
      end do
      write(6,*)'     The first index will go across the page; second wi
     &ll go down;'
      write(6,*)'     third and more will be on separate pages'
      end if
90006 continue
      write(6,*)'     Enter the desired ordering:'
      if(.NOT.read_string(5,line,length))goto 90017
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      i1=read_int_soft_fail(line(b:e))
      if(tally_rank(jscore).GE.2)then
      if(next_token(line,b,e,p))continue
      i2=read_int_soft_fail(line(b:e))
      end if
      if(tally_rank(jscore).GE.3)then
      if(next_token(line,b,e,p))continue
      i3=read_int_soft_fail(line(b:e))
      end if
      if(i1.LT.1.OR.i1.GT.tally_rank(jscore).OR.(tally_rank(jscore).GE.2
     &.AND.(i2.LT.1.OR.i2.GT.tally_rank(jscore))).OR.(tally_rank(jscore)
     &.GE.3.AND.(i3.LT.1.OR.i3.GT.tally_rank(jscore))))then
      write(6,*)' These integers must all be between 1 and ',tally_rank(
     &jscore),'; Try again'
      go to 90006
      end if
      end if
      end if
90007 continue
      if(tally_dep_var_dim(jscore).EQ.1)then
      dim=1
      else
      write(6,*)' This tally is not a scalar quantity. Enter the compone
     &nt to be printed, 1 to ',tally_dep_var_dim(jscore)
      if(.NOT.read_string(5,line,length))goto 90017
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      dim=read_int_soft_fail(line(b:e))
      if(dim.LT.1.OR.dim.GT.tally_dep_var_dim(jscore))then
      write(6,*)' Try again; pick a number between 1 and ',tally_dep_var
     &_dim(jscore)
      go to 90007
      end if
      end if
90008 continue
      write(6,*)
      write(6,*)'   Display this on screen or file? (s/f)'
      if(.NOT.read_string(5,line,length))goto 90017
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      choice=line(b:e)
      if(choice.EQ.'S'.OR.choice.EQ.'s')then
      screen=1
      nunit=6
      else if(choice.EQ.'F'.OR.choice.EQ.'f')then
      screen=0
      write(6,*)
      write(6,*)'   Enter filename to store information'
      write(6,*)
      if(.NOT.read_string(5,line,length))goto 90017
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      tempfile=line(b:e)
      open(unit=31+1,file=tempfile,status='unknown')
      nunit=31+1
      else
      write(6,*)'No, that will not work, try again.'
      go to 90008
      end if
      init=0
      write(31,*)' Tally ',jscore
      write(31,*)' Group ',group
      if(tally_rank(jscore).GT.0)then
      do i=1,tally_rank(jscore)
      if(details(i).EQ.1)then
      adddet=' +'
      else
      adddet='  '
      end if
      write(31,'(a,i1,a,i6,2x,i6,a)')'  Rank ',i,' : ',min_slice(i),max_
     &slice(i),adddet
      end do
      if(tally_rank(jscore).EQ.2)then
      write(31,'(a,i1,2x,i1)')'  Order ',i1,i2
      else if(tally_rank(jscore).EQ.3)then
      write(31,'(a,i1,2x,i1,2x,i1)')'  Order ',i1,i2,i3
      end if
      end if
      write(31,*)' Component ',dim
      if(screen.EQ.1)then
      write(31,*)' Screen'
      else
      write(31,*)' File ',tempfile  (1:string_length(tempfile))
      end if
      write(31,*)'------------------------------------------------------
     &--'
      
      else
90010 continue
      if(.NOT.read_string(20,line,length))then
      close(unit=20)
      scriptfile=' '
      go to 90001
      else
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.'Tally')then
      if(next_token(line,b,e,p))continue
      jscore=read_int_soft_fail(line(b:e))
      else if(line(b:e).EQ.'Group')then
      if(next_token(line,b,e,p))continue
      group=read_int_soft_fail(line(b:e))
      else if(line(b:e).EQ.'Rank')then
      if(next_token(line,b,e,p))continue
      i=read_int_soft_fail(line(b:e))
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.':')continue
      if(next_token(line,b,e,p))continue
      min_slice(i)=1000000000
      max_slice(i)=1000000000
      details(i)=0
90012 continue
      if(line(b:e).EQ.'+')then
      details(i)=1
      else if(line(b:e).EQ.'*')then
      max_slice(i)=tally_tab_index(i,jscore)
      if(min_slice(i).EQ.1000000000)min_slice(i)=1
      else
      if(line(e:e).EQ.'+')then
      e=e-(1)
      details(i)=1
      end if
      if(min_slice(i).EQ.1000000000)then
      min_slice(i)=read_int_soft_fail(line(b:e))
      else
      max_slice(i)=read_int_soft_fail(line(b:e))
      end if
      end if
      if(next_token(line,b,e,p))go to 90012
      if(max_slice(i).EQ.1000000000)max_slice(i)=min_slice(i)
      else if(line(b:e).EQ.'Component')then
      if(next_token(line,b,e,p))continue
      dim=read_int_soft_fail(line(b:e))
      else if(line(b:e).EQ.'Order')then
      if(next_token(line,b,e,p))continue
      i1=read_int_soft_fail(line(b:e))
      if(next_token(line,b,e,p))continue
      i2=read_int_soft_fail(line(b:e))
      if(next_token(line,b,e,p))then
      i3=read_int_soft_fail(line(b:e))
      else
      i3=3
      end if
      else if(line(b:e).EQ.'Screen')then
      screen=1
      nunit=6
      else if(line(b:e).EQ.'File')then
      screen=0
      if(next_token(line,b,e,p))continue
      tempfile=line(b:e)
      open(unit=31+1,file=tempfile,status='unknown')
      nunit=31+1
      else if(line(1:1).EQ.'-')then
      go to 90009
      else
      write(6,*)' Ignoring unexpected line:',line
      end if
      end if
      go to 90010
90009 continue
      if(jscore.EQ.0)go to 90017
      if(.NOT.check_tally(jscore))then
      write(6,*)
      write(6,*)'Tally ',jscore,' is invalid.'
      write(6,*)'It must be between 1 and ',tl_num
      go to 90010
      end if
      if(.NOT.(group.GT.0.AND.group.LE.so_grps).AND.group.NE.0)then
      write(6,*)
      write(6,*)'Invalid group: ',group,' for tally ',jscore
      write(6,*)'Must be between 1 and ',so_grps
      go to 90010
      end if
      do i=1,tally_rank(jscore)
      if(min_slice(i).LT.1.OR.min_slice(i).GT.tally_tab_index(i,jscore).
     &OR.max_slice(i).LT.1.OR.max_slice(i).GT.tally_tab_index(i,jscore).
     &OR.min_slice(i).GT.max_slice(i))then
      write(6,*)
      write(6,*)'Invalid range for tally ',jscore,', rank ',i
      write(6,*)' Range limits must be between 1 and ',tally_tab_index(i
     &,jscore)
      go to 90010
      end if
      end do
      if(tally_rank(jscore).GE.2)then
      if(i1.LT.1.OR.i1.GT.tally_rank(jscore).OR.(tally_rank(jscore).GE.2
     &.AND.(i2.LT.1.OR.i2.GT.tally_rank(jscore))).OR.(tally_rank(jscore)
     &.GE.3.AND.(i3.LT.1.OR.i3.GT.tally_rank(jscore))))then
      write(6,*)
      write(6,*)'Invalid ordering of independent variables: ',i1,i2,i3,'
     & for tally ',jscore
      write(6,*)' These integers must all be between 1 and ',tally_rank(
     &jscore)
      go to 90010
      end if
      else
      i1=1
      end if
      if(dim.LT.1.OR.dim.GT.tally_dep_var_dim(jscore))then
      if(tally_dep_var_dim(jscore).LE.1)then
      dim=1
      else
      write(6,*)
      write(6,*)'Invalid component: ',dim,' for tally ',jscore
      go to 90010
      end if
      end if
      end if
      
      
      write(nunit,*)
      write(nunit,*)'''',tally_name(jscore) (1:string_length(tally_name(
     &jscore))),''':'
      if(tally_dep_var_dim(jscore).GT.1)then
      write(nunit,*)'  component #',dim
      end if
      write(nunit,*)
      if(max_slice(i1).GT.value_size)then
      value =>mem_realloc_r1(value,(1),value_size,max_slice(i1),'value')
      rsd =>mem_realloc_r1(rsd,(1),value_size,max_slice(i1),'rsd')
      value_size=max_slice(i1)
      end if
      
      if(tally_rank(jscore).EQ.0)then
      i=1
      
      if((group.GT.0.AND.group.LE.so_grps))then
      value(i)=extract_output_datum(index_parameters,dim,out_post_grp(0,
     &0,group),0,tally_name(jscore))
      rsd(i)=extract_output_datum(index_parameters,dim,out_post_grp(0,0,
     &group),1,tally_name(jscore))
      else
      value(i)=extract_output_datum(index_parameters,dim,out_post_all,0,
     &tally_name(jscore))
      rsd(i)=extract_output_datum(index_parameters,dim,out_post_all,1,ta
     &lly_name(jscore))
      end if
      
      write(nunit,*)value(1),rsd(1)
      
      else if(tally_rank(jscore).EQ.1)then
      write(nunit,'(a10,7x,a,5x,a)')tally_var_list(tally_indep_var(1,jsc
     &ore)) (1:string_length(tally_var_list(tally_indep_var(1,jscore))))
     &,' value',' rel. std. dev.'
      do i=min_slice(1),max_slice(1)
      index_parameters(tally_indep_var(1,jscore))=i
      
      if((group.GT.0.AND.group.LE.so_grps))then
      value(i)=extract_output_datum(index_parameters,dim,out_post_grp(0,
     &0,group),0,tally_name(jscore))
      rsd(i)=extract_output_datum(index_parameters,dim,out_post_grp(0,0,
     &group),1,tally_name(jscore))
      else
      value(i)=extract_output_datum(index_parameters,dim,out_post_all,0,
     &tally_name(jscore))
      rsd(i)=extract_output_datum(index_parameters,dim,out_post_all,1,ta
     &lly_name(jscore))
      end if
      
      end do
      do i=min_slice(1),max_slice(1)
      write(nunit,'(5x,i6,4x,1pe13.5,2x,0pf7.4)')i,value(i),rsd(i)
      end do
      
      else if(tally_rank(jscore).EQ.2)then
      first_pass=1
      do j=min_slice(i2),max_slice(i2)
      index_parameters(tally_indep_var(i2,jscore))=j
      do i=min_slice(i1),max_slice(i1)
      index_parameters(tally_indep_var(i1,jscore))=i
      
      if((group.GT.0.AND.group.LE.so_grps))then
      value(i)=extract_output_datum(index_parameters,dim,out_post_grp(0,
     &0,group),0,tally_name(jscore))
      rsd(i)=extract_output_datum(index_parameters,dim,out_post_grp(0,0,
     &group),1,tally_name(jscore))
      else
      value(i)=extract_output_datum(index_parameters,dim,out_post_all,0,
     &tally_name(jscore))
      rsd(i)=extract_output_datum(index_parameters,dim,out_post_all,1,ta
     &lly_name(jscore))
      end if
      
      end do
      if(max_slice(i1)-min_slice(i1)+1.LT.20.AND.max_slice(i1).GT.min_sl
     &ice(i1))then
      if(j.EQ.min_slice(i2))then
      write(nunit,'(12x,a,a3)')tally_var_list(tally_indep_var(i1,jscore)
     &) (1:string_length(tally_var_list(tally_indep_var(i1,jscore)))),' 
     &->'
      write(nunit,'(8x,20(5x,i6,4x))') (ii,ii=min_slice(i1),max_slice(i1
     &))
      write(nunit,'(a)')tally_var_list(tally_indep_var(i2,jscore)) (1:st
     &ring_length(tally_var_list(tally_indep_var(i2,jscore))))
      end if
      write(nunit,'(i6,2x,1p,20(2x,e13.5))')j,(value(ii),ii=min_slice(i1
     &),max_slice(i1))
      else
      if(first_pass.EQ.1)then
      write(nunit,'(a10,2x,a10,5x,a,t39,a)')tally_var_list(tally_indep_v
     &ar(i1,jscore)) (1:string_length(tally_var_list(tally_indep_var(i1,
     &jscore)))),tally_var_list(tally_indep_var(i2,jscore)) (1:string_le
     &ngth(tally_var_list(tally_indep_var(i2,jscore)))),' value',' rel. 
     &std. dev.'
      first_pass=0
      end if
      do i=min_slice(i1),max_slice(i1)
      write(nunit,'(4x,i6,4x,i5,5x,1pe13.5,2x,0pf7.4)')i,j,value(i),rsd(
     &i)
      end do
      end if
      end do
      
      else if(tally_rank(jscore).EQ.3)then
      first_pass=1
      do k=min_slice(i3),max_slice(i3)
      index_parameters(tally_indep_var(i3,jscore))=k
      write(nunit,*)
      write(nunit,'(a,a,i5)')tally_var_list(tally_indep_var(i3,jscore)) 
     &(1:string_length(tally_var_list(tally_indep_var(i3,jscore)))),' : 
     &',k
      do j=min_slice(i2),max_slice(i2)
      index_parameters(tally_indep_var(i2,jscore))=j
      do i=min_slice(i1),max_slice(i1)
      index_parameters(tally_indep_var(i1,jscore))=i
      
      if((group.GT.0.AND.group.LE.so_grps))then
      value(i)=extract_output_datum(index_parameters,dim,out_post_grp(0,
     &0,group),0,tally_name(jscore))
      rsd(i)=extract_output_datum(index_parameters,dim,out_post_grp(0,0,
     &group),1,tally_name(jscore))
      else
      value(i)=extract_output_datum(index_parameters,dim,out_post_all,0,
     &tally_name(jscore))
      rsd(i)=extract_output_datum(index_parameters,dim,out_post_all,1,ta
     &lly_name(jscore))
      end if
      
      end do
      if(max_slice(i1)-min_slice(i1)+1.LT.20.AND.max_slice(i1).GT.min_sl
     &ice(i1))then
      if(j.EQ.min_slice(i2))then
      write(nunit,'(12x,a,a3)')tally_var_list(tally_indep_var(i1,jscore)
     &) (1:string_length(tally_var_list(tally_indep_var(i1,jscore)))),' 
     &->'
      write(nunit,'(8x,20(5x,i6,4x))') (ii,ii=min_slice(i1),max_slice(i1
     &))
      write(nunit,'(a)')tally_var_list(tally_indep_var(i2,jscore)) (1:st
     &ring_length(tally_var_list(tally_indep_var(i2,jscore))))
      end if
      write(nunit,'(i6,2x,1p,20(2x,e13.5))')j,(value(ii),ii=min_slice(i1
     &),max_slice(i1))
      else
      if(first_pass.EQ.1)then
      write(nunit,'(a10,2x,a10,5x,a,t39,a)')tally_var_list(tally_indep_v
     &ar(i1,jscore)) (1:string_length(tally_var_list(tally_indep_var(i1,
     &jscore)))),tally_var_list(tally_indep_var(i2,jscore)) (1:string_le
     &ngth(tally_var_list(tally_indep_var(i2,jscore)))),' value',' rel. 
     &std. dev.'
      first_pass=0
      end if
      do i=min_slice(i1),max_slice(i1)
      write(nunit,'(4x,i6,4x,i5,5x,1pe13.5,2x,0pf7.4)')i,j,value(i),rsd(
     &i)
      end do
      end if
      end do
      
      end do
      else
      write(6,*)' Rank not currently supported here'
      end if
      do i=1,tally_rank(jscore)
      if(details(i).EQ.1)then
      call dump_indep_vars(nunit,jscore,tally_indep_var(i,jscore),min_sl
     &ice(i),max_slice(i))
      end if
      end do
      if(screen.EQ.0)close(unit=nunit)
90001 continue
      if(scriptfile.EQ.' ')then
      write(6,*)
      write(6,*)'Look at another tally? (y/n)'
      if(.NOT.read_string(5,line,length))goto 90017
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      p=0
      if(next_token(line,b,e,p))continue
      if(line(b:e).EQ.'y'.OR.line(b:e).EQ.'Y')go to 90000
      else
      write(nunit,*)'---------------------------------------------------
     &-----'
      go to 90000
      end if
90017 continue
      return
      end
      subroutine dump_indep_vars(nunit,jscore,ivar,imin,imax)
      
      use zn_mod
      
      use sc_mod
      
      use de_mod
      
      use sp_mod
      
      use rc_mod
      
      use pm_mod
      
      use pr_mod
      
      use so_mod
      
      use tl_mod
      
      use gi_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
      logical check_zone
      integer nunit,jscore,ivar,imin,imax
      integer pointer,i,sec,zone
      REAL(kind=DOUBLE)energy,angle,wavelength,circum,area
      character*300 type_string
      character*20 psp_label
      REAL(kind=DOUBLE)xdiff(3)
      REAL(kind=DOUBLE)vector_temp(3)
      write(nunit,*)
      if(ivar.EQ.0)then
      if(' Unknown variable, something is wrong!'.EQ.' ')continue
      else if(ivar.EQ.1)then
      write(nunit,*)'  ZONES:'
      write(nunit,*)
      write(nunit,'(a,3x,a,9x,a,13x,a,11x,a)')' zone  ','center: x1','x2
     &','x3','volume'
      do i=imin,imax
      if(check_zone(i))continue
      write(nunit,'(i6,2x,1p,4(e13.5,2x))')i,zone_center(1,i),zone_cente
     &r(2,i),zone_center(3,i),zone_volume(i)
      end do
      else if(ivar.EQ.2)then
      write(nunit,*)' Nothing written for plasma zone'
      else if(ivar.EQ.3)then
      write(nunit,*)' TEST SPECIES:'
      write(nunit,*)
      write(nunit,'(a,4x,a,3x,a)')'test','species no.','symbol'
      do i=imin,imax
      if((i.GT.0.AND.i.LE.pr_test_num))continue
      write(nunit,'(i4,6x,i4,10x,a)')i,problem_test_sp(i),species_sy(pro
     &blem_test_sp(i))
      end do
      else if(ivar.EQ.4)then
      write(nunit,*)' PROBLEM SPECIES:'
      write(nunit,*)
      write(nunit,'(a,4x,a,5x,a,2x,a,2x,a)')'problem_sp','class','number
     &','species no.','symbol'
      do i=imin,imax
      if(i.EQ.1)then
      psp_label='background'
      else if(i.EQ.pr_background_num+1)then
      write(nunit,*)'---------------------------------------------------
     &-----'
      psp_label='test'
      else
      psp_label=' '
      end if
      if(i.LE.pr_background_num)then
      if((i.GT.0.AND.i.LE.pr_background_num))continue
      write(nunit,'(1x,i4,7x,a,t25,i4,6x,i4,10x,a)')i,psp_label  (1:stri
     &ng_length(psp_label)),i,problem_background_sp(i),species_sy(proble
     &m_background_sp(i))
      else
      if((i-pr_background_num.GT.0.AND.i-pr_background_num.LE.pr_test_nu
     &m))continue
      write(nunit,'(1x,i4,7x,a,t25,i4,6x,i4,10x,a)')i,psp_label  (1:stri
     &ng_length(psp_label)),i-pr_background_num,problem_test_sp(i-pr_bac
     &kground_num),species_sy(problem_test_sp(i-pr_background_num))
      end if
      end do
      else if(ivar.EQ.5)then
      pointer=tally_geometry_ptr(jscore)
      write(nunit,*)' DETECTOR ''',detector_name(pointer) (1:string_leng
     &th(detector_name(pointer))),''':'
      write(nunit,*)
      write(nunit,'(a,2x,a,4x,a,9x,a,13x,a,11x,a,10x,a,13x,a)')'detector
     &','view no.','start: x1','x2','x3','end: x1','x2','x3'
      do i=imin,imax
      write(nunit,'(1x,i4,5x,i4,5x,1p,6(e13.5,2x))')i,de_view_tab(de_vie
     &w_base(pointer)+i-1),de_view_points(1,0,de_view_tab(de_view_base(p
     &ointer)+i-1)),de_view_points(2,0,de_view_tab(de_view_base(pointer)
     &+i-1)),de_view_points(3,0,de_view_tab(de_view_base(pointer)+i-1)),
     &de_view_points(1,1,de_view_tab(de_view_base(pointer)+i-1)),de_view
     &_points(2,1,de_view_tab(de_view_base(pointer)+i-1)),de_view_points
     &(3,1,de_view_tab(de_view_base(pointer)+i-1))
      end do
      else if(ivar.EQ.6)then
      write(nunit,*)' TEST AUTHOR:'
      write(nunit,*)
      write(nunit,'(a,3x,a,3x,a,5x,a)')'author','class','number','name'
      do i=imin,imax
      if(i.EQ.1)then
      psp_label='source'
      else if(i.EQ.6+1)then
      psp_label='pr. reac.'
      write(nunit,*)'---------------------------------------------------
     &-----'
      else if(i.EQ.6+pr_reaction_num+1)then
      psp_label='pr. PMI'
      write(nunit,*)'---------------------------------------------------
     &-----'
      else
      psp_label=' '
      end if
      if(i.LE.6)then
      type_string=source_name(i)
      write(nunit,'(i4,4x,a,t17,i4,4x,a)')i,psp_label  (1:string_length(
     &psp_label)),i,type_string  (1:string_length(type_string))
      else if(i.LE.6+pr_reaction_num)then
      write(nunit,'(i4,4x,a,t17,i4,4x,a)')i,psp_label  (1:string_length(
     &psp_label)),i-6,reaction_name(problem_rc(i-6)) (1:string_length(re
     &action_name(problem_rc(i-6))))
      else
      write(nunit,'(i4,4x,a,t17,i4,4x,a)')i,psp_label  (1:string_length(
     &psp_label)),i-6-pr_reaction_num,pmi_name(problem_pmi_ref(i-6-pr_re
     &action_num)) (1:string_length(pmi_name(problem_pmi_ref(i-6-pr_reac
     &tion_num))))
      end if
      end do
      else if(ivar.EQ.7)then
      write(nunit,*)' REACTIONS:'
      write(nunit,*)
      if(imin.LE.pr_reaction_num)then
      write(nunit,'(a,2x,a,5x,a)')'pr. reac.','reaction','name'
      do i=imin,min(imax,pr_reaction_num)
      write(nunit,'(1x,i4,7x,i4,4x,a)')i,problem_rc(i),reaction_name(pro
     &blem_rc(i)) (1:string_length(reaction_name(problem_rc(i))))
      end do
      if(imax.GT.pr_reaction_num)write(nunit,*)'------------------------
     &--------------------------------'
      end if
      if(imax.GT.pr_reaction_num)then
      write(nunit,'(10x,a,6x,a)')'so. type','name'
      do i=max(imin-pr_reaction_num,1),imax-pr_reaction_num
      write(nunit,'(1x,i4,7x,i4,4x,a)')i+pr_reaction_num,i,source_name(i
     &)
      end do
      end if
      else if(ivar.EQ.8)then
      write(nunit,*)' PMI:'
      write(nunit,*)
      write(nunit,'(a,2x,a,7x,a)')'pr. PMI','PMI','name'
      do i=imin,imax
      write(nunit,'(i4,4x,i4,3x,a)')i,problem_pmi_ref(i),pmi_name(proble
     &m_pmi_ref(i)) (1:string_length(pmi_name(problem_pmi_ref(i))))
      end do
      else if(ivar.EQ.9)then
      write(nunit,*)' Nothing written yet for material'
      else if(ivar.EQ.10)then
      write(nunit,*)' Nothing written yet for source group'
      else if(ivar.EQ.11)then
      write(nunit,*)' Nothing written yet for sector'
      else if(ivar.EQ.12)then
      write(nunit,*)' Nothing written yet for strata'
      else if(ivar.EQ.13)then
      write(nunit,*)' Nothing written yet for strata segment'
      else if(ivar.EQ.14)then
      pointer=tally_geometry_ptr(jscore)
      write(nunit,*)' ENERGY BINS for diagnostic ''',diagnostic_grp_name
     &(pointer) (1:string_length(diagnostic_grp_name(pointer))),''':'
      write(nunit,*)
      write(nunit,'(1x,a,4x,a)')'bin','energy (eV)'
      do i=imin,imax
      energy=diagnostic_min(pointer)+(REAL(i,DOUBLE)-(0.5_DOUBLE))*diagn
     &ostic_delta(pointer)
      if(diagnostic_spacing(pointer).EQ.2)energy=exp(energy)
      energy=energy/((1.60217733e-19_DOUBLE))
      write(nunit,'(i4,2x,1pe13.5)')i,energy
      end do
      else if(ivar.EQ.15)then
      pointer=tally_geometry_ptr(jscore)
      write(nunit,*)' ANGLE BINS for diagnostic ''',diagnostic_grp_name(
     &pointer) (1:string_length(diagnostic_grp_name(pointer))),''':'
      write(nunit,*)
      write(nunit,'(1x,a,3x,a)')'bin','angle (degrees)'
      do i=imin,imax
      angle=diagnostic_min(pointer)+(REAL(i,DOUBLE)-(0.5_DOUBLE))*diagno
     &stic_delta(pointer)
      if(diagnostic_spacing(pointer).EQ.2)angle=exp(angle)
      angle=angle*((1.8e2_DOUBLE)/atan2((0.0_DOUBLE),-(1.0_DOUBLE)))
      write(nunit,'(i4,3x,1pe13.5)')i,angle
      end do
      else if(ivar.EQ.16)then
      pointer=tally_geometry_ptr(jscore)
      write(nunit,*)' WAVELENGTH BINS for diagnostic ''',diagnostic_grp_
     &name(pointer) (1:string_length(diagnostic_grp_name(pointer))),''':
     &'
      write(nunit,*)
      write(nunit,'(1x,a,3x,a)')'bin','wavelength (Angstroms)'
      do i=imin,imax
      wavelength=detector_min(pointer)+(REAL(i,DOUBLE)-0.5)*detector_del
     &ta(pointer)
      if(detector_spacing(pointer).EQ.2)wavelength=exp(wavelength)
      wavelength=wavelength*((1.e10_DOUBLE))
      write(nunit,'(i4,5x,1pe15.7)')i,wavelength
      end do
      else if(ivar.EQ.17)then
      pointer=tally_geometry_ptr(jscore)
      write(nunit,*)' SECTORS for diagnostic ''',diagnostic_grp_name(poi
     &nter) (1:string_length(diagnostic_grp_name(pointer))),''':'
      write(nunit,*)'   Note that in some instances the coordinate and /
     & or area data may not be available.'
      write(nunit,*)
      write(nunit,'(t32,a)')'stratum'
      write(nunit,'(a,3x,a,3x,a,2x,a,2x,a,9x,a,13x,a,10x,a,7x,a)')'diagn
     &ostic','sector','stratum','segment','midpt: x1','x2','x3','iy','ar
     &ea'
      do i=imin,imax
      sec=diagnostic_sector_tab(diagnostic_grp_base(pointer)+i-1)
      xdiff(1)=sector_points(1,0,sec)-sector_points(1,1,sec)
      xdiff(2)=sector_points(2,0,sec)-sector_points(2,1,sec)
      xdiff(3)=sector_points(3,0,sec)-sector_points(3,1,sec)
      
      zone=sector_zone(sec)
      if(geometry_symmetry.EQ.1)then
      circum=universal_cell_max(2)-universal_cell_min(2)
      else if(geometry_symmetry.EQ.4)then
      circum=zone_max(2,zone)-zone_min(2,zone)
      else if(geometry_symmetry.EQ.2)then
      circum=atan2((0.0_DOUBLE),-(1.0_DOUBLE))*(sector_points(1,0,sec)+s
     &ector_points(1,1,sec))
      else if((geometry_symmetry.EQ.5).OR.(geometry_symmetry.EQ.6))then
      circum=(0.5_DOUBLE)*(sector_points(1,0,sec)+sector_points(1,1,sec)
     &)*(zone_max(2,zone)-zone_min(2,zone))
      end if
      area=circum*sqrt((xdiff(1)**2+xdiff(2)**2+xdiff(3)**2))
      write(nunit,'(2x,i5,6x,i5,4x,i4,6x,i4,1x,3(1pe13.5,2x),2x,i3,2x,e1
     &3.5)')i,sec,strata(sec),sector_strata_segment(sec),(0.5_DOUBLE)*(s
     &ector_points(1,0,sec)+sector_points(1,1,sec)),(0.5_DOUBLE)*(sector
     &_points(2,0,sec)+sector_points(2,1,sec)),(0.5_DOUBLE)*(sector_poin
     &ts(3,0,sec)+sector_points(3,1,sec)),zone_index(3,sector_zone(sec))
     &,area
      end do
      else
      write(nunit,*)' Unknown independent variable'
      end if
      return
      end
      
      
