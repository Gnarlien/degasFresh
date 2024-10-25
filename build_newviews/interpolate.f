      
      
      
      
      
      function interpolate1(f,array,n)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)interpolate1
      integer n
      REAL(kind=DOUBLE)f,array(0:*)
      integer ia,ib
      REAL(kind=DOUBLE)fx
      ia=int(f)
      if(f.LT.REAL(ia,DOUBLE))ia=ia-1
      fx=f-REAL(ia,DOUBLE)
      ib=ia+1
      ia=min(max(0,ia),n-1)
      ib=min(max(0,ib),n-1)
      interpolate1=array(ia)*((1.0_DOUBLE)-fx)+array(ib)*fx
      return
      end
      function extrapolate1(f,array,n)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)extrapolate1
      integer n
      REAL(kind=DOUBLE)f,array(0:*)
      integer ia,ib
      REAL(kind=DOUBLE)fx
      if(f.LT.(0.0_DOUBLE))then
      ia=0
      ib=1
      fx=f
      else if(f.GT.REAL(n-1,DOUBLE))then
      ia=n-1
      ib=n-2
      fx=REAL(n-1,DOUBLE)-f
      else
      ia=int(f)
      if(f.LT.REAL(ia,DOUBLE))ia=ia-1
      fx=f-REAL(ia,DOUBLE)
      ib=ia+1
      end if
      if((ia.GE.0).AND.(ia.LE.n-1))continue
      if((ib.GE.0).AND.(ib.LE.n-1))continue
      extrapolate1=array(ia)*((1.0_DOUBLE)-fx)+array(ib)*fx
      return
      end
      function interpolate2(f,array,n)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)interpolate2
      integer n(2)
      REAL(kind=DOUBLE)f(2),array(0:*)
      integer i,ia(2),ib(2)
      REAL(kind=DOUBLE)fx(2)
      do i=1,2
      ia(i)=int(f(i))
      if(f(i).LT.REAL(ia(i),DOUBLE))ia(i)=ia(i)-1
      fx(i)=f(i)-REAL(ia(i),DOUBLE)
      ib(i)=ia(i)+1
      ia(i)=min(max(0,ia(i)),n(i)-1)
      ib(i)=min(max(0,ib(i)),n(i)-1)
      end do
      interpolate2=array(ia(1)+n(1)*ia(2))*((1.0_DOUBLE)-fx(1))*((1.0_DO
     &UBLE)-fx(2))+array(ia(1)+n(1)*ib(2))*((1.0_DOUBLE)-fx(1))*fx(2)+ar
     &ray(ib(1)+n(1)*ia(2))*fx(1)*((1.0_DOUBLE)-fx(2))+array(ib(1)+n(1)*
     &ib(2))*fx(1)*fx(2)
      return
      end
      function interpolate3(f,array,n)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)interpolate3
      integer n(3)
      REAL(kind=DOUBLE)f(3),array(0:*)
      integer i,ia(3),ib(3)
      REAL(kind=DOUBLE)fx(3)
      do i=1,3
      ia(i)=int(f(i))
      if(f(i).LT.REAL(ia(i),DOUBLE))ia(i)=ia(i)-1
      fx(i)=f(i)-REAL(ia(i),DOUBLE)
      ib(i)=ia(i)+1
      ia(i)=min(max(0,ia(i)),n(i)-1)
      ib(i)=min(max(0,ib(i)),n(i)-1)
      end do
      interpolate3=array(ia(1)+n(1)*(ia(2)+n(2)*ia(3)))*((1.0_DOUBLE)-fx
     &(1))*((1.0_DOUBLE)-fx(2))*((1.0_DOUBLE)-fx(3))+array(ia(1)+n(1)*(i
     &a(2)+n(2)*ib(3)))*((1.0_DOUBLE)-fx(1))*((1.0_DOUBLE)-fx(2))*fx(3)+
     &array(ia(1)+n(1)*(ib(2)+n(2)*ia(3)))*((1.0_DOUBLE)-fx(1))*fx(2)*((
     &1.0_DOUBLE)-fx(3))+array(ia(1)+n(1)*(ib(2)+n(2)*ib(3)))*((1.0_DOUB
     &LE)-fx(1))*fx(2)*fx(3)+array(ib(1)+n(1)*(ia(2)+n(2)*ia(3)))*fx(1)*
     &((1.0_DOUBLE)-fx(2))*((1.0_DOUBLE)-fx(3))+array(ib(1)+n(1)*(ia(2)+
     &n(2)*ib(3)))*fx(1)*((1.0_DOUBLE)-fx(2))*fx(3)+array(ib(1)+n(1)*(ib
     &(2)+n(2)*ia(3)))*fx(1)*fx(2)*((1.0_DOUBLE)-fx(3))+array(ib(1)+n(1)
     &*(ib(2)+n(2)*ib(3)))*fx(1)*fx(2)*fx(3)
      return
      end
      function interpolate4(f,array,n)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)interpolate4
      integer n(4)
      REAL(kind=DOUBLE)f(4),array(0:*)
      integer i,ia(4),ib(4)
      REAL(kind=DOUBLE)fx(4),ia1,ib1
      do i=1,4
      ia(i)=int(f(i))
      if(f(i).LT.REAL(ia(i),DOUBLE))ia(i)=ia(i)-1
      fx(i)=f(i)-REAL(ia(i),DOUBLE)
      ib(i)=ia(i)+1
      ia(i)=min(max(0,ia(i)),n(i)-1)
      ib(i)=min(max(0,ib(i)),n(i)-1)
      end do
      ia1=array(ia(1)+n(1)*(ia(2)+n(2)*(ia(3)+n(3)*ia(4))))*((1.0_DOUBLE
     &)-fx(1))*((1.0_DOUBLE)-fx(2))*((1.0_DOUBLE)-fx(3))*((1.0_DOUBLE)-f
     &x(4))+array(ia(1)+n(1)*(ia(2)+n(2)*(ia(3)+n(3)*ib(4))))*((1.0_DOUB
     &LE)-fx(1))*((1.0_DOUBLE)-fx(2))*((1.0_DOUBLE)-fx(3))*fx(4)+array(i
     &a(1)+n(1)*(ia(2)+n(2)*(ib(3)+n(3)*ia(4))))*((1.0_DOUBLE)-fx(1))*((
     &1.0_DOUBLE)-fx(2))*fx(3)*((1.0_DOUBLE)-fx(4))+array(ia(1)+n(1)*(ia
     &(2)+n(2)*(ib(3)+n(3)*ib(4))))*((1.0_DOUBLE)-fx(1))*((1.0_DOUBLE)-f
     &x(2))*fx(3)*fx(4)+array(ia(1)+n(1)*(ib(2)+n(2)*(ia(3)+n(3)*ia(4)))
     &)*((1.0_DOUBLE)-fx(1))*fx(2)*((1.0_DOUBLE)-fx(3))*((1.0_DOUBLE)-fx
     &(4))+array(ia(1)+n(1)*(ib(2)+n(2)*(ia(3)+n(3)*ib(4))))*((1.0_DOUBL
     &E)-fx(1))*fx(2)*((1.0_DOUBLE)-fx(3))*fx(4)+array(ia(1)+n(1)*(ib(2)
     &+n(2)*(ib(3)+n(3)*ia(4))))*((1.0_DOUBLE)-fx(1))*fx(2)*fx(3)*((1.0_
     &DOUBLE)-fx(4))+array(ia(1)+n(1)*(ib(2)+n(2)*(ib(3)+n(3)*ib(4))))*(
     &(1.0_DOUBLE)-fx(1))*fx(2)*fx(3)*fx(4)
      ib1=array(ib(1)+n(1)*(ia(2)+n(2)*(ia(3)+n(3)*ia(4))))*fx(1)*((1.0_
     &DOUBLE)-fx(2))*((1.0_DOUBLE)-fx(3))*((1.0_DOUBLE)-fx(4))+array(ib(
     &1)+n(1)*(ia(2)+n(2)*(ia(3)+n(3)*ib(4))))*fx(1)*((1.0_DOUBLE)-fx(2)
     &)*((1.0_DOUBLE)-fx(3))*fx(4)+array(ib(1)+n(1)*(ia(2)+n(2)*(ib(3)+n
     &(3)*ia(4))))*fx(1)*((1.0_DOUBLE)-fx(2))*fx(3)*((1.0_DOUBLE)-fx(4))
     &+array(ib(1)+n(1)*(ia(2)+n(2)*(ib(3)+n(3)*ib(4))))*fx(1)*((1.0_DOU
     &BLE)-fx(2))*fx(3)*fx(4)+array(ib(1)+n(1)*(ib(2)+n(2)*(ia(3)+n(3)*i
     &a(4))))*fx(1)*fx(2)*((1.0_DOUBLE)-fx(3))*((1.0_DOUBLE)-fx(4))+arra
     &y(ib(1)+n(1)*(ib(2)+n(2)*(ia(3)+n(3)*ib(4))))*fx(1)*fx(2)*((1.0_DO
     &UBLE)-fx(3))*fx(4)+array(ib(1)+n(1)*(ib(2)+n(2)*(ib(3)+n(3)*ia(4))
     &))*fx(1)*fx(2)*fx(3)*((1.0_DOUBLE)-fx(4))+array(ib(1)+n(1)*(ib(2)+
     &n(2)*(ib(3)+n(3)*ib(4))))*fx(1)*fx(2)*fx(3)*fx(4)
      interpolate4=ia1+ib1
      return
      end
      function interpolate5(f,array,n)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)interpolate5
      integer n(5)
      REAL(kind=DOUBLE)f(5),array(0:*)
      integer i,ia(5),ib(5)
      REAL(kind=DOUBLE)fx(5),ia1ia2,ia1ib2,ib1ia2,ib1ib2
      do i=1,5
      ia(i)=int(f(i))
      if(f(i).LT.REAL(ia(i),DOUBLE))ia(i)=ia(i)-1
      fx(i)=f(i)-REAL(ia(i),DOUBLE)
      ib(i)=ia(i)+1
      ia(i)=min(max(0,ia(i)),n(i)-1)
      ib(i)=min(max(0,ib(i)),n(i)-1)
      end do
      ia1ia2=array(ia(1)+n(1)*(ia(2)+n(2)*(ia(3)+n(3)*(ia(4)+n(4)*ia(5))
     &)))*((1.0_DOUBLE)-fx(1))*((1.0_DOUBLE)-fx(2))*((1.0_DOUBLE)-fx(3))
     &*((1.0_DOUBLE)-fx(4))*((1.0_DOUBLE)-fx(5))+array(ia(1)+n(1)*(ia(2)
     &+n(2)*(ia(3)+n(3)*(ia(4)+n(4)*ib(5)))))*((1.0_DOUBLE)-fx(1))*((1.0
     &_DOUBLE)-fx(2))*((1.0_DOUBLE)-fx(3))*((1.0_DOUBLE)-fx(4))*fx(5)+ar
     &ray(ia(1)+n(1)*(ia(2)+n(2)*(ia(3)+n(3)*(ib(4)+n(4)*ia(5)))))*((1.0
     &_DOUBLE)-fx(1))*((1.0_DOUBLE)-fx(2))*((1.0_DOUBLE)-fx(3))*fx(4)*((
     &1.0_DOUBLE)-fx(5))+array(ia(1)+n(1)*(ia(2)+n(2)*(ia(3)+n(3)*(ib(4)
     &+n(4)*ib(5)))))*((1.0_DOUBLE)-fx(1))*((1.0_DOUBLE)-fx(2))*((1.0_DO
     &UBLE)-fx(3))*fx(4)*fx(5)+array(ia(1)+n(1)*(ia(2)+n(2)*(ib(3)+n(3)*
     &(ia(4)+n(4)*ia(5)))))*((1.0_DOUBLE)-fx(1))*((1.0_DOUBLE)-fx(2))*fx
     &(3)*((1.0_DOUBLE)-fx(4))*((1.0_DOUBLE)-fx(5))+array(ia(1)+n(1)*(ia
     &(2)+n(2)*(ib(3)+n(3)*(ia(4)+n(4)*ib(5)))))*((1.0_DOUBLE)-fx(1))*((
     &1.0_DOUBLE)-fx(2))*fx(3)*((1.0_DOUBLE)-fx(4))*fx(5)+array(ia(1)+n(
     &1)*(ia(2)+n(2)*(ib(3)+n(3)*(ib(4)+n(4)*ia(5)))))*((1.0_DOUBLE)-fx(
     &1))*((1.0_DOUBLE)-fx(2))*fx(3)*fx(4)*((1.0_DOUBLE)-fx(5))+array(ia
     &(1)+n(1)*(ia(2)+n(2)*(ib(3)+n(3)*(ib(4)+n(4)*ib(5)))))*((1.0_DOUBL
     &E)-fx(1))*((1.0_DOUBLE)-fx(2))*fx(3)*fx(4)*fx(5)
      ia1ib2=array(ia(1)+n(1)*(ib(2)+n(2)*(ia(3)+n(3)*(ia(4)+n(4)*ia(5))
     &)))*((1.0_DOUBLE)-fx(1))*fx(2)*((1.0_DOUBLE)-fx(3))*((1.0_DOUBLE)-
     &fx(4))*((1.0_DOUBLE)-fx(5))+array(ia(1)+n(1)*(ib(2)+n(2)*(ia(3)+n(
     &3)*(ia(4)+n(4)*ib(5)))))*((1.0_DOUBLE)-fx(1))*fx(2)*((1.0_DOUBLE)-
     &fx(3))*((1.0_DOUBLE)-fx(4))*fx(5)+array(ia(1)+n(1)*(ib(2)+n(2)*(ia
     &(3)+n(3)*(ib(4)+n(4)*ia(5)))))*((1.0_DOUBLE)-fx(1))*fx(2)*((1.0_DO
     &UBLE)-fx(3))*fx(4)*((1.0_DOUBLE)-fx(5))+array(ia(1)+n(1)*(ib(2)+n(
     &2)*(ia(3)+n(3)*(ib(4)+n(4)*ib(5)))))*((1.0_DOUBLE)-fx(1))*fx(2)*((
     &1.0_DOUBLE)-fx(3))*fx(4)*fx(5)+array(ia(1)+n(1)*(ib(2)+n(2)*(ib(3)
     &+n(3)*(ia(4)+n(4)*ia(5)))))*((1.0_DOUBLE)-fx(1))*fx(2)*fx(3)*((1.0
     &_DOUBLE)-fx(4))*((1.0_DOUBLE)-fx(5))+array(ia(1)+n(1)*(ib(2)+n(2)*
     &(ib(3)+n(3)*(ia(4)+n(4)*ib(5)))))*((1.0_DOUBLE)-fx(1))*fx(2)*fx(3)
     &*((1.0_DOUBLE)-fx(4))*fx(5)+array(ia(1)+n(1)*(ib(2)+n(2)*(ib(3)+n(
     &3)*(ib(4)+n(4)*ia(5)))))*((1.0_DOUBLE)-fx(1))*fx(2)*fx(3)*fx(4)*((
     &1.0_DOUBLE)-fx(5))+array(ia(1)+n(1)*(ib(2)+n(2)*(ib(3)+n(3)*(ib(4)
     &+n(4)*ib(5)))))*((1.0_DOUBLE)-fx(1))*fx(2)*fx(3)*fx(4)*fx(5)
      ib1ia2=array(ib(1)+n(1)*(ia(2)+n(2)*(ia(3)+n(3)*(ia(4)+n(4)*ia(5))
     &)))*fx(1)*((1.0_DOUBLE)-fx(2))*((1.0_DOUBLE)-fx(3))*((1.0_DOUBLE)-
     &fx(4))*((1.0_DOUBLE)-fx(5))+array(ib(1)+n(1)*(ia(2)+n(2)*(ia(3)+n(
     &3)*(ia(4)+n(4)*ib(5)))))*fx(1)*((1.0_DOUBLE)-fx(2))*((1.0_DOUBLE)-
     &fx(3))*((1.0_DOUBLE)-fx(4))*fx(5)+array(ib(1)+n(1)*(ia(2)+n(2)*(ia
     &(3)+n(3)*(ib(4)+n(4)*ia(5)))))*fx(1)*((1.0_DOUBLE)-fx(2))*((1.0_DO
     &UBLE)-fx(3))*fx(4)*((1.0_DOUBLE)-fx(5))+array(ib(1)+n(1)*(ia(2)+n(
     &2)*(ia(3)+n(3)*(ib(4)+n(4)*ib(5)))))*fx(1)*((1.0_DOUBLE)-fx(2))*((
     &1.0_DOUBLE)-fx(3))*fx(4)*fx(5)+array(ib(1)+n(1)*(ia(2)+n(2)*(ib(3)
     &+n(3)*(ia(4)+n(4)*ia(5)))))*fx(1)*((1.0_DOUBLE)-fx(2))*fx(3)*((1.0
     &_DOUBLE)-fx(4))*((1.0_DOUBLE)-fx(5))+array(ib(1)+n(1)*(ia(2)+n(2)*
     &(ib(3)+n(3)*(ia(4)+n(4)*ib(5)))))*fx(1)*((1.0_DOUBLE)-fx(2))*fx(3)
     &*((1.0_DOUBLE)-fx(4))*fx(5)+array(ib(1)+n(1)*(ia(2)+n(2)*(ib(3)+n(
     &3)*(ib(4)+n(4)*ia(5)))))*fx(1)*((1.0_DOUBLE)-fx(2))*fx(3)*fx(4)*((
     &1.0_DOUBLE)-fx(5))+array(ib(1)+n(1)*(ia(2)+n(2)*(ib(3)+n(3)*(ib(4)
     &+n(4)*ib(5)))))*fx(1)*((1.0_DOUBLE)-fx(2))*fx(3)*fx(4)*fx(5)
      ib1ib2=array(ib(1)+n(1)*(ib(2)+n(2)*(ia(3)+n(3)*(ia(4)+n(4)*ia(5))
     &)))*fx(1)*fx(2)*((1.0_DOUBLE)-fx(3))*((1.0_DOUBLE)-fx(4))*((1.0_DO
     &UBLE)-fx(5))+array(ib(1)+n(1)*(ib(2)+n(2)*(ia(3)+n(3)*(ia(4)+n(4)*
     &ib(5)))))*fx(1)*fx(2)*((1.0_DOUBLE)-fx(3))*((1.0_DOUBLE)-fx(4))*fx
     &(5)+array(ib(1)+n(1)*(ib(2)+n(2)*(ia(3)+n(3)*(ib(4)+n(4)*ia(5)))))
     &*fx(1)*fx(2)*((1.0_DOUBLE)-fx(3))*fx(4)*((1.0_DOUBLE)-fx(5))+array
     &(ib(1)+n(1)*(ib(2)+n(2)*(ia(3)+n(3)*(ib(4)+n(4)*ib(5)))))*fx(1)*fx
     &(2)*((1.0_DOUBLE)-fx(3))*fx(4)*fx(5)+array(ib(1)+n(1)*(ib(2)+n(2)*
     &(ib(3)+n(3)*(ia(4)+n(4)*ia(5)))))*fx(1)*fx(2)*fx(3)*((1.0_DOUBLE)-
     &fx(4))*((1.0_DOUBLE)-fx(5))+array(ib(1)+n(1)*(ib(2)+n(2)*(ib(3)+n(
     &3)*(ia(4)+n(4)*ib(5)))))*fx(1)*fx(2)*fx(3)*((1.0_DOUBLE)-fx(4))*fx
     &(5)+array(ib(1)+n(1)*(ib(2)+n(2)*(ib(3)+n(3)*(ib(4)+n(4)*ia(5)))))
     &*fx(1)*fx(2)*fx(3)*fx(4)*((1.0_DOUBLE)-fx(5))+array(ib(1)+n(1)*(ib
     &(2)+n(2)*(ib(3)+n(3)*(ib(4)+n(4)*ib(5)))))*fx(1)*fx(2)*fx(3)*fx(4)
     &*fx(5)
      interpolate5=ia1ia2+ia1ib2+ib1ia2+ib1ib2
      return
      end
      function find_index(x,array,n)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)find_index
      integer n
      REAL(kind=DOUBLE)x,array(0:*)
      integer jl,jm,ju
      jl=-1
      ju=n
90001 continue
      if(ju-jl.GT.1)then
      jm=(ju+jl)/2
      if((array(n-1).GT.array(0)).EQV.(x.GT.array(jm)))then
      jl=jm
      else
      ju=jm
      end if
      goto 90001
      end if
      jl=min(max(0,jl),n-2)
      
      find_index=(x-array(jl))/(array(jl+1)-array(jl))+jl
      return
      end
      subroutine init_base(base,size)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer size
      integer base(size)
      integer i
      do i=1,size
      base(i)=2000000000
      end do
      return
      end
      subroutine interpolate_grid(new_values,old_values,value_spacing,ne
     &w_grid,n,nold)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      REAL(kind=DOUBLE)new_values(0:*),old_values(0:*),new_grid(0:*)
      character*(*)value_spacing
      integer n,nold
      integer i
      external interpolate1
      REAL(kind=DOUBLE)interpolate1
      if(value_spacing.EQ.'log')then
      do i=0,nold-1
      old_values(i)=log(old_values(i))
      end do
      else
      if(value_spacing.EQ.'linear')continue
      end if
      do i=0,n-1
      new_values(i)=interpolate1(new_grid(i),old_values,nold)
      if(value_spacing.EQ.'log')then
      new_values(i)=exp(new_values(i))
      end if
      end do
      return
      end
      subroutine interpolate_grid2(new_values,old_values,value_spacing,n
     &ew_grid_m,new_grid_n,m,n,mold,nold)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer m,n,mold,nold
      REAL(kind=DOUBLE)new_values(0:n-1,0:*),old_values(0:nold-1,0:*),ne
     &w_grid_m(0:*),new_grid_n(0:*)
      character*(*)value_spacing
      integer i,j
      integer dim(2)
      REAL(kind=DOUBLE)f(2)
      external interpolate2
      REAL(kind=DOUBLE)interpolate2
      if(value_spacing.EQ.'log')then
      do i=0,mold-1
      do j=0,nold-1
      old_values(j,i)=log(old_values(j,i))
      end do
      end do
      else
      if(value_spacing.EQ.'linear')continue
      end if
      dim(1)=nold
      dim(2)=mold
      do i=0,m-1
      f(2)=new_grid_m(i)
      do j=0,n-1
      f(1)=new_grid_n(j)
      new_values(j,i)=interpolate2(f,old_values,dim)
      if(value_spacing.EQ.'log')then
      new_values(j,i)=exp(new_values(j,i))
      end if
      end do
      end do
      return
      end
      subroutine interpolate_grid3(new_values,old_values,value_spacing,n
     &ew_grid_l,new_grid_m,new_grid_n,l,m,n,lold,mold,nold)
      
      implicit none
      integer,parameter::SINGLE=kind(0.0),DOUBLE=selected_real_kind(12)
      integer l,m,n,lold,mold,nold
      REAL(kind=DOUBLE)new_values(0:n-1,0:m-1,0:*),old_values(0:nold-1,0
     &:mold-1,0:*),new_grid_l(0:*),new_grid_m(0:*),new_grid_n(0:*)
      character*(*)value_spacing
      integer i,j,k
      integer dim(3)
      REAL(kind=DOUBLE)f(3)
      external interpolate3
      REAL(kind=DOUBLE)interpolate3
      if(value_spacing.EQ.'log')then
      do k=0,lold-1
      do i=0,mold-1
      do j=0,nold-1
      old_values(j,i,k)=log(old_values(j,i,k))
      end do
      end do
      end do
      else
      if(value_spacing.EQ.'linear')continue
      end if
      dim(1)=nold
      dim(2)=mold
      dim(3)=lold
      do k=0,l-1
      f(3)=new_grid_l(k)
      do i=0,m-1
      f(2)=new_grid_m(i)
      do j=0,n-1
      f(1)=new_grid_n(j)
      new_values(j,i,k)=interpolate3(f,old_values,dim)
      if(value_spacing.EQ.'log')then
      new_values(j,i,k)=exp(new_values(j,i,k))
      end if
      end do
      end do
      end do
      return
      end
      
      
