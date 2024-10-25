      
      
      
      
      
      function string_length(string)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer string_length
      character*(*)string
      integer i,k
      k=index(string,char(0))
      if(k.GT.0)then
      k=k-1
      else
      k=len(string)
      end if
      do i=k,1,-1
      if(string(i:i).NE.' ')then
      string_length=i
      return
      end if
      end do
      string_length=0
      return
      end
      subroutine string_pad(string)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*(*)string
      integer k
      k=index(string,char(0))
      if(k.GT.0)then
      string(k:)=' '
      end if
      return
      end
      subroutine null_terminate(string)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*(*)string
      integer k
      integer string_length
      k=string_length(string)
      if(k.LT.len(string))then
      string(k+1:k+1)=char(0)
      end if
      return
      end
      function read_string(unit,string,length)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical read_string
      integer unit
      character*(*)string
      integer length
      integer i,p,l,lb,pb
      character*450 buffer
      logical start,more
      external string_length
      integer string_length
      l=len(string)
      p=1
      start=.TRUE.
      more=.FALSE.
      read_string=.TRUE.
      string=' '
90000 continue
      read(unit,'(a)',end=90004)buffer
      if(start.AND.(buffer(1:1).EQ.'#'.OR.buffer.EQ.' '))goto 90000
      start=.FALSE.
      lb=string_length(buffer)
      if(.NOT.more)then
      pb=1
      else
      do i=1,lb
      if(.NOT.(buffer(i:i).EQ.' '.OR.buffer(i:i).EQ.char(9)))then
      pb=i
      goto 90008
      end if
      end do
      pb=lb+1
90008 continue
      end if
      more=buffer(lb:lb).EQ.char(92)
      if(more)lb=lb-1
      if(pb.LE.lb.AND.p.LE.l)string(p:)=buffer(pb:lb)
      p=p+(lb-pb+1)
      if(more.AND.read_string)goto 90000
      length=p-1
      return
90004 continue
      read_string=.FALSE.
      length=p-1
      return
      end
      function parse_string(string)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer parse_string
      character*(*)string
      integer l,i,k,p,offset
      l=len(string)
      p=1
90000 continue
      k=index(string(p:),char(9))
      if(k.GT.0)then
      string(k+p-1:k+p-1)=' '
      p=k+p
      if(p.LE.l)goto 90000
      end if
      offset=0
      do i=1,l
      if(string(i:i).EQ.' ')then
      offset=offset+1
      else
      goto 90007
      end if
      end do
90007 continue
      p=1+offset
90001 continue
      k=index(string(p:),' ')
      if(k.GT.0)then
      if(offset.GT.0)string(p-offset:p-offset+k-1)=string(p:p+k-1)
      p=p+k
      do i=p,l
      if(string(i:i).EQ.' ')then
      offset=offset+1
      p=p+1
      else
      goto 90008
      end if
      end do
90008 continue
      else
      if(offset.GT.0)string(p-offset:l-offset)=string(p:)
      p=l+1
      end if
      if(p.LE.l)goto 90001
      if(p-offset.LE.l)string(p-offset:)=' '
      parse_string=p-offset-1
      if(parse_string.GT.0.AND.string(parse_string:parse_string).EQ.' ')
     &parse_string=parse_string-1
      return
      end
      function next_token(string,b,e,p)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      logical next_token
      character*(*)string
      integer b,e
      integer p
      integer k,l
      p=p+1
      l=len(string)
      next_token=.FALSE.
      if(p.GT.l)return
      if(string(p:p).EQ.' ')return
      next_token=.TRUE.
      b=p
      k=index(string(p:),' ')
      if(k.GT.0)then
      e=b+k-2
      else
      e=l
      end if
      p=e+1
      return
      end
      function read_integer(string)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer read_integer
      character*(*)string
      character*10 form
      write(form,'(a,i3.3,a)')'(i',len(string),')'
      read(string,form)read_integer
      return
      end
      function read_int_soft_fail(string)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer read_int_soft_fail
      character*(*)string
      character*10 form
      write(form,'(a,i3.3,a)')'(i',len(string),')'
      read(string,form,err=90004)read_int_soft_fail
      return
90004 continue
      read_int_soft_fail=1000000000
      return
      end
      function read_real(string)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)read_real
      character*(*)string
      character*10 form
      write(form,'(a,i3.3,a)')'(f',len(string),'.0)'
      read(string,form)read_real
      return
      end
      function read_real_soft_fail(string)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)read_real_soft_fail
      character*(*)string
      character*10 form
      write(form,'(a,i3.3,a)')'(f',len(string),'.0)'
      read(string,form,err=90004)read_real_soft_fail
      return
90004 continue
      read_real_soft_fail=(1.0e30_DOUBLE)
      return
      end
      function read_real_scaled(string,delta)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)read_real_scaled
      character*(*)string
      integer delta
      integer iexp,exp
      character*3 stexp
      external read_integer,read_real_soft_fail
      integer read_integer
      REAL(kind=DOUBLE)read_real_soft_fail
      iexp=max(index(string,'e'),index(string,'E'),index(string,'d'),ind
     &ex(string,'D'))
      if(iexp.GT.0)continue
      exp=read_integer(string(iexp+1:))
      exp=exp+(delta)
      write(stexp,'(sp,i3.2)')exp
      read_real_scaled=read_real_soft_fail(string(:iexp)//stexp)
      return
      end
      function string_lookup(string,array,num)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer string_lookup
      integer num
      character*(*)string,array(num)
      integer i
      do i=1,num
      if(array(i).EQ.string)then
      string_lookup=i
      return
      end if
      end do
      string_lookup=0
      return
      end
      function int_lookup(n,array,num)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer int_lookup
      integer num
      integer n,array(num)
      integer i
      do i=1,num
      if(array(i).EQ.n)then
      int_lookup=i
      return
      end if
      end do
      int_lookup=0
      return
      end
      
      
