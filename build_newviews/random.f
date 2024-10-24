      
      
      
      
      
      function random(ran_index_x,ran_array_x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)ran_ulp2
      parameter(ran_ulp2=(2.0_DOUBLE)**(-47-1))
      REAL(kind=SINGLE)ran_ulps
      REAL(kind=DOUBLE)ran_mult
      parameter(ran_ulps=2.0**(-23),ran_mult=(2.0_DOUBLE)**23)
      REAL(kind=DOUBLE)random
      REAL(kind=SINGLE)srandom
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      external rand_batch
      if(ran_index_x.GE.100)then
      call rand_batch(ran_index_x,ran_array_x(0))
      end if
      random=ran_array_x(ran_index_x)+ran_ulp2
      ran_index_x=ran_index_x+1
      return
      entry srandom(ran_index_x,ran_array_x)
      if(ran_index_x.GE.100)then
      call rand_batch(ran_index_x,ran_array_x(0))
      end if
      srandom=(int(ran_mult*ran_array_x(ran_index_x))+0.5)*ran_ulps
      ran_index_x=ran_index_x+1
      return
      end
      subroutine random_array(y,n,ran_index_x,ran_array_x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)ran_ulp2
      parameter(ran_ulp2=(2.0_DOUBLE)**(-47-1))
      REAL(kind=SINGLE)ran_ulps
      REAL(kind=DOUBLE)ran_mult
      parameter(ran_ulps=2.0**(-23),ran_mult=(2.0_DOUBLE)**23)
      integer n
      REAL(kind=DOUBLE)y(0:n-1)
      REAL(kind=SINGLE)ys(0:n-1)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer i,k,j
      external rand_batch
      if(n.LE.0)return
      k=min(n,100-ran_index_x)
      do i=0,k-1
      y(i)=ran_array_x(i+ran_index_x)+ran_ulp2
      end do
      ran_index_x=ran_index_x+(k)
      do j=k,n-1,100
      call rand_batch(ran_index_x,ran_array_x(0))
      do i=j,min(j+100,n)-1
      y(i)=ran_array_x(i-j+ran_index_x)+ran_ulp2
      end do
      ran_index_x=ran_index_x+(min(100,n-j))
      end do
      return
      entry srandom_array(ys,n,ran_index_x,ran_array_x)
      if(n.LE.0)return
      k=min(n,100-ran_index_x)
      do i=0,k-1
      ys(i)=(int(ran_mult*ran_array_x(i+ran_index_x))+0.5)*ran_ulps
      end do
      ran_index_x=ran_index_x+(k)
      do j=k,n-1,100
      call rand_batch(ran_index_x,ran_array_x(0))
      do i=j,min(j+100,n)-1
      ys(i)=(int(ran_mult*ran_array_x(i-j+ran_index_x))+0.5)*ran_ulps
      end do
      ran_index_x=ran_index_x+(min(100,n-j))
      end do
      return
      end
      subroutine rand_batch(ran_index_x,ran_array_x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer i
      REAL(kind=DOUBLE)w(0:1009-100-1)
      REAL(kind=DOUBLE)tmp
      
      if(ran_index_x.EQ.100)continue
      do i=0,63-1
      tmp=ran_array_x(i)+ran_array_x(i+100-63)
      if(tmp.GE.(1.0_DOUBLE))then
      w(i)=tmp-(1.0_DOUBLE)
      else
      w(i)=tmp
      end if
      end do
      do i=63,100-1
      tmp=ran_array_x(i)+w(i-63)
      if(tmp.GE.(1.0_DOUBLE))then
      w(i)=tmp-(1.0_DOUBLE)
      else
      w(i)=tmp
      end if
      end do
      do i=100,1009-100-1
      tmp=w(i-100)+w(i-63)
      if(tmp.GE.(1.0_DOUBLE))then
      w(i)=tmp-(1.0_DOUBLE)
      else
      w(i)=tmp
      end if
      end do
      do i=1009-100,1009-100+63-1
      tmp=w(i-100)+w(i-63)
      if(tmp.GE.(1.0_DOUBLE))then
      ran_array_x(i-1009+100)=tmp-(1.0_DOUBLE)
      else
      ran_array_x(i-1009+100)=tmp
      end if
      end do
      do i=1009-100+63,1009-1
      tmp=w(i-100)+ran_array_x(i-1009+100-63)
      if(tmp.GE.(1.0_DOUBLE))then
      ran_array_x(i-1009+100)=tmp-(1.0_DOUBLE)
      else
      ran_array_x(i-1009+100)=tmp
      end if
      end do
      ran_index_x=0
      return
      end
      subroutine random_init_d2(seed,ran_index_x,ran_array_x)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer b
      REAL(kind=DOUBLE)ran_del,ran_ulp
      parameter(b=2**14,ran_del=(2.0_DOUBLE)**(-14),ran_ulp=(2.0_DOUBLE)
     &**(-47))
      integer a0,a1,a2,a3,a4,a5,a6,c0
      parameter(a0=15661,a1=678,a2=724,a3=5245,a4=13656,a5=11852,a6=29)
      parameter(c0=1)
      integer seed(0:8-1)
      integer ran_index_x
      REAL(kind=DOUBLE)ran_array_x(0:100-1)
      integer i,j,s(0:8-1)
      logical odd
      integer z(0:8-1),t
      do i=0,8-1
      if(0.LE.seed(i).AND.seed(i).LT.b)continue
      s(i)=seed(i)
      end do
      odd=mod(s(7),2).NE.0
      ran_array_x(0)=(((s(7)*ran_del+s(6))*ran_del+s(5))*ran_del+int(s(4
     &)/512))*512*ran_del
      do j=1,100-1
      z(0)=c0+a0*s(0)
      z(1)=a0*s(1)+a1*s(0)
      z(2)=a0*s(2)+a1*s(1)+a2*s(0)
      z(3)=a0*s(3)+a1*s(2)+a2*s(1)+a3*s(0)
      z(4)=a0*s(4)+a1*s(3)+a2*s(2)+a3*s(1)+a4*s(0)
      z(5)=a0*s(5)+a1*s(4)+a2*s(3)+a3*s(2)+a4*s(1)+a5*s(0)
      z(6)=a0*s(6)+a1*s(5)+a2*s(4)+a3*s(3)+a4*s(2)+a5*s(1)+a6*s(0)
      z(7)=a0*s(7)+a1*s(6)+a2*s(5)+a3*s(4)+a4*s(3)+a5*s(2)+a6*s(1)
      t=0
      do i=0,8-1
      t=int(t/b)+z(i)
      s(i)=mod(t,b)
      end do
      
      odd=odd.OR.(mod(s(7),2).NE.0)
      
      ran_array_x(j)=(((s(7)*ran_del+s(6))*ran_del+s(5))*ran_del+int(s(4
     &)/512))*512*ran_del
      end do
      ran_index_x=100
      
      if(odd)return
      z(0)=c0+a0*s(0)
      z(1)=a0*s(1)+a1*s(0)
      z(2)=a0*s(2)+a1*s(1)+a2*s(0)
      z(3)=a0*s(3)+a1*s(2)+a2*s(1)+a3*s(0)
      z(4)=a0*s(4)+a1*s(3)+a2*s(2)+a3*s(1)+a4*s(0)
      z(5)=a0*s(5)+a1*s(4)+a2*s(3)+a3*s(2)+a4*s(1)+a5*s(0)
      z(6)=a0*s(6)+a1*s(5)+a2*s(4)+a3*s(3)+a4*s(2)+a5*s(1)+a6*s(0)
      z(7)=a0*s(7)+a1*s(6)+a2*s(5)+a3*s(4)+a4*s(3)+a5*s(2)+a6*s(1)
      t=0
      do i=0,8-1
      t=int(t/b)+z(i)
      s(i)=mod(t,b)
      end do
      
      j=int((s(8-1)*100)/b)
      ran_array_x(j)=ran_array_x(j)+(ran_ulp)
      return
      end
      subroutine decimal_to_seed(decimal,seed)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      character*(*)decimal
      integer seed(0:8-1)
      external rand_axc
      integer i,ten(0:8-1),c(0:8-1),ch
      data ten/10,7*0/
      do i=0,8-1
      seed(i)=0
      c(i)=0
      end do
      do i=1,len(decimal)
      ch=ichar(decimal(i:i))
      if(ch.GE.ichar('0').AND.ch.LE.ichar('9'))then
      c(0)=ch-ichar('0')
      call rand_axc(ten,seed,c)
      end if
      end do
      return
      end
      subroutine string_to_seed(string,seed)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer b
      parameter(b=2**14)
      character*(*)string
      integer seed(0:8-1)
      external rand_axc
      integer t,i,k,unity(0:8-1),c(0:8-1),ch
      data unity/1,7*0/
      do i=0,8-1
      seed(i)=0
      c(i)=0
      end do
      do i=1,len(string)
      ch=ichar(string(i:i))
      if(ch.GT.ichar(' ').AND.ch.LT.127)then
      t=mod(seed(0),2)*(b/2)
      do k=0,8-1
      seed(k)=int(seed(k)/2)
      if(k.LT.8-1)then
      seed(k)=seed(k)+(mod(seed(k+1),2)*(b/2))
      else
      seed(k)=seed(k)+(t)
      end if
      end do
      c(0)=ch
      call rand_axc(unity,seed,c)
      end if
      end do
      return
      end
      subroutine set_random_seed(time,seed)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer time(8)
      integer seed(0:8-1)
      character*26 c
      integer t(8)
      external decimal_to_seed
      t(1)=mod(mod(time(1),1000000000)+1000000000,1000000000)
      t(2)=mod(mod(time(2),100)+100,100)
      t(3)=mod(mod(time(3),100)+100,100)
      t(4)=((1-sign(1,time(4)))/2)*1000+mod(abs(time(4)),1000)
      t(5)=mod(mod(time(5),100)+100,100)
      t(6)=mod(mod(time(6),100)+100,100)
      t(7)=mod(mod(time(7),100)+100,100)
      t(8)=mod(mod(time(7),1000)+1000,1000)
      c=' '
      write(c(1:17),'(i9.9,2i2.2,i4.4)')t(1),t(2),t(3),t(4)
      write(c(18:26),'(3i2.2,i3.3)')t(5),t(6),t(7),t(8)
      call decimal_to_seed(c,seed)
      return
      end
      subroutine seed_to_decimal(seed,decimal)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer pow,decbase,b
      parameter(pow=4,decbase=10**pow,b=2**14)
      character*(*)decimal
      integer seed(0:8-1)
      integer z(0:8-1),i,t,j,k
      character*36 str
      k=-1
      do i=0,8-1
      z(i)=seed(i)
      if(0.LE.z(i).AND.z(i).LT.b)continue
      if(z(i).GT.0)k=i
      end do
      str=' '
      
      i=9
90000 continue
      i=i-1
      t=0
      do j=k,0,-1
      z(j)=z(j)+t*b
      t=mod(z(j),decbase)
      z(j)=int(z(j)/decbase)
      end do
      if(z(max(0,k)).EQ.0)k=k-1
      j=pow*(i+1)
      if(k.GE.0)then
      str(j-(pow-1):j)='0000'
      else
      str(j-(pow-1):j)='   0'
      end if
90001 continue
      if(t.EQ.0)goto 90010
      str(j:j)=char(ichar('0')+mod(t,10))
      j=j-1
      t=int(t/10)
      goto 90001
90010 continue
      if(k.GE.0)goto 90000
      k=min(j+1,len(str))
      if(len(decimal).GE.len(str(k:)))then
      decimal=str(k:)
      else
      decimal=str(len(str(k:))-len(decimal)+1:)
      end if
      return
      end
      subroutine rand_next_seed(n,ax,cx,y)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer n,ax(0:8-1),cx(0:8-1)
      integer y(0:8-1)
      external rand_axc
      integer a(0:8-1),c(0:8-1),z(0:8-1),t(0:8-1),m,i
      data z/8*0/
      if(n.EQ.0)return
      if(n.GT.0)continue
      m=n
      do i=0,8-1
      a(i)=ax(i)
      c(i)=cx(i)
      end do
90000 continue
      if(mod(m,2).GT.0)then
      call rand_axc(a,y,c)
      end if
      m=int(m/2)
      if(m.EQ.0)return
      do i=0,8-1
      t(i)=c(i)
      end do
      call rand_axc(a,c,t)
      do i=0,8-1
      t(i)=a(i)
      end do
      call rand_axc(t,a,z)
      goto 90000
      end
      subroutine next_seed3(n0,n1,n2,seed)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer n0,n1,n2
      integer seed(0:8-1)
      external rand_next_seed
      integer af0(0:8-1),cf0(0:8-1)
      integer ab0(0:8-1),cb0(0:8-1)
      integer af1(0:8-1),cf1(0:8-1)
      integer ab1(0:8-1),cb1(0:8-1)
      integer af2(0:8-1),cf2(0:8-1)
      integer ab2(0:8-1),cb2(0:8-1)
      data af0/15741,8689,9280,4732,12011,7130,6824,12302/
      data cf0/16317,10266,1198,331,10769,8310,2779,13880/
      data ab0/9173,9894,15203,15379,7981,2280,8071,429/
      data cb0/8383,3616,597,12724,15663,9639,187,4866/
      data af1/8405,4808,3603,6718,13766,9243,10375,12108/
      data cf1/13951,7170,9039,11206,8706,14101,1864,15191/
      data ab1/6269,3240,9759,7130,15320,14399,3675,1380/
      data cb1/15357,5843,6205,16275,8838,12132,2198,10330/
      data af2/445,10754,1869,6593,385,12498,14501,7383/
      data cf2/2285,8057,3864,10235,1805,10614,9615,15522/
      data ab2/405,4903,2746,1477,3263,13564,8139,2362/
      data cb2/8463,575,5876,2220,4924,1701,9060,5639/
      if(n2.GT.0)then
      call rand_next_seed(n2,af2,cf2,seed)
      else if(n2.LT.0)then
      call rand_next_seed(-n2,ab2,cb2,seed)
      end if
      if(n1.GT.0)then
      call rand_next_seed(n1,af1,cf1,seed)
      else if(n1.LT.0)then
      call rand_next_seed(-n1,ab1,cb1,seed)
      end if
      entry next_seed(n0,seed)
      if(n0.GT.0)then
      call rand_next_seed(n0,af0,cf0,seed)
      else if(n0.LT.0)then
      call rand_next_seed(-n0,ab0,cb0,seed)
      end if
      return
      end
      subroutine rand_axc(a,x,c)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer b
      parameter(b=2**14)
      integer a(0:8-1),c(0:8-1)
      integer x(0:8-1)
      integer z(0:8-1),i,j,t
      do i=0,8-1
      z(i)=c(i)
      end do
      
      do j=0,8-1
      do i=j,8-1
      z(i)=z(i)+(a(j)*x(i-j))
      end do
      end do
      t=0
      do i=0,8-1
      t=int(t/b)+z(i)
      x(i)=mod(t,b)
      end do
      return
      end
      
      
