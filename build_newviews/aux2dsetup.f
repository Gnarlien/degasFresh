      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine find_poly_zone(face1,type,poly_zone,y_div,sect_zone1,nu
     &m_zone1,sect_zone2,num_zone2)
      
      use gi_mod
      
      use zn_mod
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer type,y_div
      integer poly_zone
      integer face1
      integer num_zone1,num_zone2
      integer sect_zone1(*),sect_zone2(*)
      integer count_pos,count_neg,index_pos,index_neg,temp_zone,i,i_max,
     &match,j
      if(face1.NE.0)continue
      count_pos=surfaces(1,1,abs(face1))
      count_neg=surfaces(0,1,abs(face1))
      index_pos=surfaces(1,0,abs(face1))
      index_neg=surfaces(0,0,abs(face1))
      num_zone2=0
      if(count_pos.GT.0)then
      if(neighbors(index_pos).GT.0)then
      sect_zone1(1)=cells(4,neighbors(index_pos))
      num_zone1=1
      else
      num_zone1=0
      return
      end if
      if(count_pos.GT.1)then
      do i=2,count_pos
      if(neighbors(index_pos+i-1).GT.0)then
      match=0
      do j=1,num_zone1
      if(sect_zone1(j).EQ.cells(4,neighbors(index_pos+i-1)))match=1
      end do
      if(match.EQ.0)then
      num_zone1=num_zone1+1
      if(num_zone1.LE.y_div)continue
      sect_zone1(num_zone1)=cells(4,neighbors(index_pos+i-1))
      end if
      else
      num_zone1=0
      return
      end if
      end do
      end if
      else
      num_zone1=0
      return
      end if
      face1=abs(face1)
      if(count_neg.GT.0)then
      sect_zone2(1)=cells(4,neighbors(index_neg))
      num_zone2=1
      if(count_neg.GT.1)then
      do i=2,count_neg
      match=0
      do j=1,num_zone2
      if(sect_zone2(j).EQ.cells(4,neighbors(index_neg+i-1)))match=1
      end do
      if(match.EQ.0)then
      num_zone2=num_zone2+1
      if(num_zone2.LE.y_div)continue
      sect_zone2(num_zone2)=cells(4,neighbors(index_neg+i-1))
      end if
      end do
      end if
      else
      num_zone2=0
      sect_zone2(1)=-1
      end if
      if(poly_zone.NE.0)then
      if(zone_index(4,sect_zone1(1)).NE.zone_index(4,poly_zone))then
      if(zone_type(zone_index(4,sect_zone2(1))).EQ.type)continue
      face1=-face1
      i_max=max(num_zone1,num_zone2)
      do i=1,i_max
      temp_zone=sect_zone1(i)
      sect_zone1(i)=sect_zone2(i)
      sect_zone2(i)=temp_zone
      end do
      temp_zone=num_zone1
      num_zone1=num_zone2
      num_zone2=temp_zone
      else
      if(zone_index(4,sect_zone1(1)).EQ.zone_index(4,poly_zone))continue
      if((type.EQ.0).OR.(zone_type(zone_index(4,sect_zone1(1))).EQ.type)
     &)continue
      end if
      else
      if((type.EQ.0).OR.(zone_type(zone_index(4,sect_zone1(1))).EQ.type)
     &)continue
      end if
      return
      end
      subroutine read_sonnet_mesh(nunit,nxd_0,nzd_0,ix_min,ix_max,iz_min
     &,iz_max,nxd,nzd,mesh_xz)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer nunit,nxd_0,nzd_0,ix_min,ix_max,iz_min,iz_max,nxd,nzd
      REAL(kind=DOUBLE)mesh_xz(1:2,0:4,nxd,nzd)
      character*300 line
      integer length,ix,iz,ixl,izl,ix_0,iz_0
      REAL(kind=DOUBLE)pitch
      external string_length,read_string,parse_string,next_token,string_
     &lookup,int_lookup,read_real,read_integer,read_int_soft_fail,read_r
     &eal_soft_fail,read_real_scaled
      integer string_length,parse_string,string_lookup,int_lookup,read_i
     &nteger,read_int_soft_fail
      REAL(kind=DOUBLE)read_real,read_real_soft_fail,read_real_scaled
      logical read_string,next_token
90013 continue
      if(read_string(nunit,line,length))continue
      if(length.LE.len(line))continue
      length=parse_string(line(:length))
      if(index(line,'===').EQ.0)go to 90013
      if(iz_min.GT.1)then
      do iz_0=1,iz_min-1
      do ix_0=1,nxd_0
      read(nunit,*,err=90014)
      read(nunit,*,err=90014)
      read(nunit,*,err=90014)
      read(nunit,*,err=90014)
      end do
      end do
      end if
      do iz_0=iz_min,iz_max
      if(ix_min.GT.1)then
      do ix_0=1,ix_min-1
      read(nunit,*,err=90014)
      read(nunit,*,err=90014)
      read(nunit,*,err=90014)
      read(nunit,*,err=90014)
      end do
      end if
      iz=iz_0-iz_min+1
      do ix_0=ix_min,ix_max
      ix=ix_0-ix_min+1
      read(nunit,'(30x,e17.10,1x,e17.10,8x,e17.10,1x,e17.10)')mesh_xz(1,
     &2,ix,iz),mesh_xz(2,2,ix,iz),mesh_xz(1,3,ix,iz),mesh_xz(2,3,ix,iz)
      read(nunit,'(18x,e17.10,14x,e17.10,1x,e17.10)')pitch,mesh_xz(1,0,i
     &x,iz),mesh_xz(2,0,ix,iz)
      read(nunit,'(30x,e17.10,1x,e17.10,8x,e17.10,1x,e17.10)')mesh_xz(1,
     &1,ix,iz),mesh_xz(2,1,ix,iz),mesh_xz(1,4,ix,iz),mesh_xz(2,4,ix,iz)
      read(nunit,*,err=90014)
      end do
      if(ix_max.LT.nxd_0)then
      do ix_0=ix_max+1,nxd_0
      read(nunit,*,err=90014)
      read(nunit,*,err=90014)
      read(nunit,*,err=90014)
      read(nunit,*,err=90014)
      end do
      end if
      end do
      close(unit=nunit)
      return
90014 continue
      if(' Problem reading sonnet mesh; unexpected end of file'.EQ.' ')c
     &ontinue
      write(0,*)' Problem reading sonnet mesh; unexpected end of file'
      return
      end
      subroutine sort2(n,arr,brr)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer n,m,nstack
      REAL(kind=DOUBLE)arr(n)
      integer brr(n)
      parameter(m=7,nstack=50)
      integer i,ir,j,jstack,k,l,istack(nstack)
      REAL(kind=DOUBLE)a,b,temp
      jstack=0
      l=1
      ir=n
1     if(ir-l.LT.m)then
      do j=l+1,ir
      a=arr(j)
      b=brr(j)
      do i=j-1,l,-1
      if(arr(i).LE.a)goto 2
      arr(i+1)=arr(i)
      brr(i+1)=brr(i)
      end do
      i=l-1
2     arr(i+1)=a
      brr(i+1)=b
      end do
      if(jstack.EQ.0)return
      ir=istack(jstack)
      l=istack(jstack-1)
      jstack=jstack-2
      else
      k=(l+ir)/2
      temp=arr(k)
      arr(k)=arr(l+1)
      arr(l+1)=temp
      temp=brr(k)
      brr(k)=brr(l+1)
      brr(l+1)=temp
      if(arr(l).GT.arr(ir))then
      temp=arr(l)
      arr(l)=arr(ir)
      arr(ir)=temp
      temp=brr(l)
      brr(l)=brr(ir)
      brr(ir)=temp
      end if
      if(arr(l+1).GT.arr(ir))then
      temp=arr(l+1)
      arr(l+1)=arr(ir)
      arr(ir)=temp
      temp=brr(l+1)
      brr(l+1)=brr(ir)
      brr(ir)=temp
      end if
      if(arr(l).GT.arr(l+1))then
      temp=arr(l)
      arr(l)=arr(l+1)
      arr(l+1)=temp
      temp=brr(l)
      brr(l)=brr(l+1)
      brr(l+1)=temp
      end if
      i=l+1
      j=ir
      a=arr(l+1)
      b=brr(l+1)
3     continue
      i=i+1
      if(arr(i).LT.a)goto 3
4     continue
      j=j-1
      if(arr(j).GT.a)goto 4
      if(j.LT.i)goto 5
      temp=arr(i)
      arr(i)=arr(j)
      arr(j)=temp
      temp=brr(i)
      brr(i)=brr(j)
      brr(j)=temp
      goto 3
5     arr(l+1)=arr(j)
      arr(j)=a
      brr(l+1)=brr(j)
      brr(j)=b
      jstack=jstack+2
      if(jstack.LE.nstack)continue
      if(ir-i+1.GE.j-l)then
      istack(jstack)=ir
      istack(jstack-1)=i
      ir=j-1
      else
      istack(jstack)=j-1
      istack(jstack-1)=l
      l=i
      end if
      end if
      goto 1
      end
      
      
